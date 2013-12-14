#include "settings.h"
#include "parser.h"
#include "track_paths.h"
#include <OakSystem/application.h>
#include <plist/plist.h>
#include <oak/oak.h>
#include <regexp/format_string.h>
#include <regexp/glob.h>
#include <text/parse.h>
#include <text/format.h>
#include <oak/debug.h>
#include <io/io.h>
#include <cf/cf.h>

OAK_DEBUG_VAR(Settings);

namespace
{
	static std::string& default_settings_path ()
	{
		static std::string res = NULL_STR;
		return res;
	}

	static std::string& global_settings_path ()
	{
		static std::string res = NULL_STR;
		return res;
	}

	static std::vector< std::pair<std::string, std::string> > global_variables ()
	{
		std::vector< std::pair<std::string, std::string> > res;
		if(CFPropertyListRef cfPlist = CFPreferencesCopyAppValue(CFSTR("environmentVariables"), kCFPreferencesCurrentApplication))
		{
			if(CFGetTypeID(cfPlist) == CFArrayGetTypeID())
			{
				CFArrayRef cfArray = (CFArrayRef)cfPlist;
				for(CFIndex i = 0; i < CFArrayGetCount(cfArray); ++i)
				{
					std::string name, value; bool enabled;
					plist::dictionary_t const& plist = plist::convert(CFArrayGetValueAtIndex(cfArray, i));
					if(plist::get_key_path(plist, "name", name) && plist::get_key_path(plist, "value", value) && (!plist::get_key_path(plist, "enabled", enabled) || enabled))
						res.emplace_back(name, value);
				}
			}
			CFRelease(cfPlist);
		}
		return res;
	}

	static void expand_variable (std::string const& name, std::string const& value, std::map<std::string, std::string>& environment)
	{
		std::string const expanded = format_string::expand(value, environment);
		environment[name] = expanded;
	}

	static std::vector<std::string> paths (std::string const& directory)
	{
		std::vector<std::string> res;
		for(std::string cwd = path::is_absolute(directory) ? directory : path::home(); true; cwd = cwd == "/" ? path::home() : path::parent(cwd))
		{
			res.push_back(path::join(cwd, ".tm_properties"));
			if(cwd == path::home())
				break;
		}
		if(global_settings_path() != NULL_STR)
			res.push_back(global_settings_path());
		if(default_settings_path() != NULL_STR)
			res.push_back(default_settings_path());
		std::reverse(res.begin(), res.end());
		return res;
	}

	static bool is_scope_selector (std::string const& str)
	{
		static std::string const RootScopes[] = { "text", "source", "attr" };
		for(auto const& scope : RootScopes)
		{
			if(str.find(scope) == 0 && (str.size() == scope.size() || str.find_first_of("., ", scope.size()) == scope.size()))
				return true;
		}
		return str == "";
	}

	struct section_t
	{
		section_t (path::glob_t const& fileGlob, scope::selector_t const& scopeSelector, std::vector< std::pair<std::string, std::string> > const& variables) : file_glob(fileGlob), scope_selector(scopeSelector), variables(variables) { }
		section_t (std::vector< std::pair<std::string, std::string> > const& variables) : section_t("", scope::selector_t(), variables) { }
		section_t (path::glob_t const& fileGlob, std::vector< std::pair<std::string, std::string> > const& variables) : section_t(fileGlob, scope::selector_t(), variables) { has_file_glob = true; }
		section_t (scope::selector_t const& scopeSelector, std::vector< std::pair<std::string, std::string> > const& variables) : section_t("", scopeSelector, variables) { has_scope_selector = true; }

		bool has_file_glob      = false;
		bool has_scope_selector = false;

		path::glob_t                                       file_glob;
		scope::selector_t                                  scope_selector;
		std::vector< std::pair<std::string, std::string> > variables;
	};

	static std::vector<section_t> parse_sections (std::string const& path)
	{
		ini_file_t iniFile(path);
		std::string content = path::content(path);
		if(content != NULL_STR)
			parse_ini(content.data(), content.data() + content.size(), iniFile);

		std::vector<section_t> res;
		for(auto const& section : iniFile.sections)
		{
			std::vector< std::pair<std::string, std::string> > variables;
			if(section.names.empty())
			{
				variables.emplace_back("CWD", path::parent(path));
				variables.emplace_back("TM_PROPERTIES_PATH", text::format("%s${TM_PROPERTIES_PATH:+:$TM_PROPERTIES_PATH}", path.c_str()));
			}

			for(auto const& pair : section.values)
				variables.emplace_back(pair.name, pair.value);

			if(section.names.empty())
			{
				res.emplace_back(variables);
			}
			else
			{
				for(auto const& name : section.names)
				{
					if(is_scope_selector(name))
							res.emplace_back(scope::selector_t(name), variables);
					else	res.emplace_back(path::glob_t(name), variables);
				}
			}
		}
		return res;
	}

	static std::vector<section_t> const& sections (std::string const& path)
	{
		static track_paths_t tracked_paths;
		static std::map<std::string, std::vector<section_t> > cache;

		if(path == NULL_STR)
		{
			if(cache.size() > 64)
			{
				D(DBF_Settings, bug("clear cache\n"););
				for(auto const& pair : cache)
					tracked_paths.remove(pair.first);
				cache.clear();
			}

			static std::vector<section_t> dummy;
			return dummy;
		}
		else
		{
			if(tracked_paths.is_changed(path))
			{
				D(DBF_Settings, if(cache.find(path) != cache.end()) bug("reload %s\n", path.c_str()););
				cache[path] = parse_sections(path);
				D(DBF_Settings, bug("%zu properties files cached\n", cache.size()););
			}
			return cache[path];
		}
	}

	std::map<std::string, std::string> expanded_variables_for (std::string const& directory, std::string const& path, scope::scope_t const& scope, std::map<std::string, std::string> variables)
	{
		D(DBF_Settings, bug("%s, %s, %s\n", directory.c_str(), path.c_str(), to_s(scope).c_str()););
		for(auto const& pair : global_variables())
			expand_variable(pair.first, pair.second, variables);

		static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
		pthread_mutex_lock(&mutex);
		sections(NULL_STR); // clear cache if too big

		std::multimap<double, section_t const*> orderScopeMatches;
		for(auto const& file : paths(directory))
		{
			for(auto const& section : sections(file))
			{
				if(section.has_scope_selector)
				{
					double rank = 0;
					if(section.scope_selector.does_match(scope, &rank))
						orderScopeMatches.emplace(rank, &section);
				}
				else if(!section.has_file_glob)
				{
					for(auto const& pair : section.variables)
						expand_variable(pair.first, pair.second, variables);
				}
			}
		}

		for(auto const& section : orderScopeMatches)
		{
			for(auto const& pair : section.second->variables)
				expand_variable(pair.first, pair.second, variables);
		}

		for(auto const& file : paths(directory))
		{
			for(auto const& section : sections(file))
			{
				if(section.has_file_glob && section.file_glob.does_match(path == NULL_STR ? directory : path))
				{
					for(auto const& pair : section.variables)
						expand_variable(pair.first, pair.second, variables);
				}
			}
		}

		pthread_mutex_unlock(&mutex);

		variables.erase("CWD");
		return variables;
	}
}

void settings_t::set_default_settings_path (std::string const& path)
{
	default_settings_path() = path;
}

void settings_t::set_global_settings_path (std::string const& path)
{
	global_settings_path() = path;
}

settings_t settings_for_path (std::string const& path, scope::scope_t const& scope, std::string const& directory, std::map<std::string, std::string> variables)
{
	for(auto pair : oak::basic_environment())
		variables.insert(pair);
	return expanded_variables_for(directory != NULL_STR ? directory : (path != NULL_STR ? path::parent(path) : path::home()), path, scope, variables);
}

std::map<std::string, std::string> variables_for_path (std::map<std::string, std::string> const& base, std::string const& path, scope::scope_t const& scope, std::string const& directory)
{
	auto variables = expanded_variables_for(directory != NULL_STR ? directory : (path != NULL_STR ? path::parent(path) : path::home()), path, scope, base);

	auto it = variables.begin();
	while(it != variables.end())
	{
		auto prev = it++;
		if(!prev->first.empty() && islower(prev->first[0]))
			variables.erase(prev);
	}

	return variables;
}

// ===================================
// = Helper Funtions for raw get/set =
// ===================================

static std::string quote_string (std::string const& src)
{
	if(src.find_first_of("'\n\\ ") == std::string::npos)
		return src;

	std::string res = "'";
	for(size_t i = 0; i < src.size(); ++i)
	{
		if(strchr("'\n", src[i]) || i+1 != src.size() && src[i] == '\\' && strchr("\\'\n", src[i+1]))
			res.append("\\");
		res += src[i];
	}
	res.append("'");
	return res;
}

static std::map<std::string, std::map<std::string, std::string>> read_file (std::string const& path)
{
	ini_file_t iniFile(path);
	std::string content = path::content(path);
	if(content != NULL_STR)
		parse_ini(content.data(), content.data() + content.size(), iniFile);

	std::map<std::string, std::map<std::string, std::string>> sections;
	for(auto const& section : iniFile.sections)
	{
		std::vector<std::string> names = section.names.empty() ? std::vector<std::string>(1, "") : section.names;
		for(auto const& name : names)
		{
			for(auto const& pair : section.values)
				sections[name].emplace(pair.name, pair.value);
		}
	}
	return sections;
}

// ================================================
// = Get setting without expanding format strings =
// ================================================

std::string settings_t::raw_get (std::string const& key, std::string const& section)
{
	ASSERT_NE(default_settings_path(), NULL_STR);
	ASSERT_NE(global_settings_path(), NULL_STR);

	auto globalSettings = read_file(global_settings_path())[section];
	if(globalSettings.find(key) != globalSettings.end())
		return globalSettings[key];

	auto defaultSettings = read_file(default_settings_path())[section];
	if(defaultSettings.find(key) != defaultSettings.end())
		return defaultSettings[key];

	return NULL_STR;
}

// ===================
// = Saving Settings =
// ===================

void settings_t::set (std::string const& key, std::string const& value, std::string const& fileType, std::string const& path)
{
	ASSERT_NE(default_settings_path(), NULL_STR);
	ASSERT_NE(global_settings_path(), NULL_STR);

	std::vector<std::string> sectionNames(fileType == NULL_STR ? 0 : 1, fileType);
	if(fileType != NULL_STR && fileType.find("attr.") != 0)
	{
		std::vector<std::string> parts = text::split(fileType, ".");
		for(size_t i = 0; i < parts.size(); ++i)
			sectionNames.push_back(text::join(std::vector<std::string>(parts.begin(), parts.begin() + i), "."));
	}

	if(path != NULL_STR)
		sectionNames.push_back(path);

	auto sections = read_file(global_settings_path());
	for(auto const& sectionName : sectionNames)
		sections[sectionName][key] = value;

	auto defaults = read_file(default_settings_path());
	if(FILE* fp = fopen(global_settings_path().c_str(), "w"))
	{
		fprintf(fp, "# Version 1.0 -- Generated content!\n");
		for(auto const& section : sections)
		{
			if(section.second.empty())
				continue;

			if(!section.first.empty())
				fprintf(fp, "\n[ %s ]\n", quote_string(section.first).c_str());

			auto defaultsSection = defaults.find(section.first);
			for(auto const& pair : section.second)
			{
				if(pair.second == NULL_STR)
					continue;

				if(defaultsSection != defaults.end())
				{
					auto it = defaultsSection->second.find(pair.first);
					if(it != defaultsSection->second.end() && it->second == pair.second)
						continue;
				}
				fprintf(fp, "%-16s = %s\n", pair.first.c_str(), quote_string(pair.second).c_str());
			}
		}
		fclose(fp);
	}
}
