#include "wrappers.h"
#include "query.h"
#include <text/case.h>
#include <io/path.h>
#include <regexp/regexp.h>
#include <regexp/format_string.h>
#include <text/tokenize.h>

namespace bundles
{
	// ==============================
	// = Shell Command Requirements =
	// ==============================

	required_command_t::required_command_t (std::string const& command, std::string moreInfoUrl, std::string const& variable, std::vector<std::string> const& locations) : command(command), more_info_url(moreInfoUrl), variable(variable), locations(locations)
	{
	}

	static std::vector<std::string> convert_locations (plist::array_t const& array)
	{
		std::vector<std::string> res;
		iterate(it, array)
		{
			if(std::string const* str = boost::get<std::string>(&*it))
				res.push_back(*str);
		}
		return res;
	}

	static std::vector<required_command_t> requirements (item_ptr const& item)
	{
		std::vector<required_command_t> res;
		if(item->kind() != kItemTypeBundle)
			res = requirements(item->bundle());

		plist::array_t array;
		if(plist::get_key_path(item->plist(), "requiredCommands", array))
		{
			for(auto info : array)
			{
				std::string command, moreInfoURL = NULL_STR, variable = NULL_STR; plist::array_t locations;
				if(plist::get_key_path(info, "command", command))
				{
					plist::get_key_path(info, "moreInfoURL", moreInfoURL);
					plist::get_key_path(info, "variable", variable);
					if(plist::get_key_path(info, "locations", locations))
							res.push_back(required_command_t(command, moreInfoURL, variable, convert_locations(locations)));
					else	res.push_back(required_command_t(command, moreInfoURL, variable));
				}
			}
		}
		return res;
	}

	static std::vector<std::string> search_paths (std::map<std::string, std::string> const& environment)
	{
		std::vector<std::string> res;
		auto searchPath = environment.find("PATH");
		if(searchPath != environment.end())
		{
			for(auto str : text::tokenize(searchPath->second.begin(), searchPath->second.end(), ':'))
			{
				if(str != "")
					res.push_back(str);
			}
		}
		else
		{
			fprintf(stderr, "no PATH!!!\n");
			iterate(pair, environment)
				fprintf(stderr, "%s = %s\n", pair->first.c_str(), pair->second.c_str());
		}
		return res;
	}

	bool missing_requirement (item_ptr const& item, std::map<std::string, std::string>& environment, required_command_t* failedRequirement)
	{
		for(auto requirement : requirements(item))
		{
			std::vector<std::string> candidates;

			if(environment.find(requirement.variable) != environment.end())
				candidates.push_back(environment[requirement.variable]);

			for(auto path : search_paths(environment))
				candidates.push_back(path::join(path, requirement.command));

			for(auto path : requirement.locations)
				candidates.push_back(format_string::expand(path, environment));

			auto exe = std::find_if(candidates.begin(), candidates.end(), [](std::string const& path){ return path::is_executable(path); });
			if(exe != candidates.end())
			{
				if(requirement.variable != NULL_STR)
						environment[requirement.variable] = *exe;
				else	environment["PATH"] += ":" + path::parent(*exe);
			}
			else
			{
				if(failedRequirement)
					*failedRequirement = requirement;
				return true;
			}
		}
		return false;
	}

	// ======================================
	// = Scope Variables (“shellVariables”) =
	// ======================================

	static std::vector< std::pair<std::string, std::string> > parse_variables (item_ptr const& item)
	{
		plist::array_t variables;
		plist::get_key_path(item->plist(), "settings.shellVariables", variables);

		std::vector< std::pair<std::string, std::string> > res;
		iterate(it, variables)
		{
			bool disabled;
			std::string key, value;
			if(!(plist::get_key_path(*it, "disabled", disabled) && disabled) && plist::get_key_path(*it, "name", key) && plist::get_key_path(*it, "value", value))
				res.push_back(std::make_pair(key, value));
		}
		return res;
	}

	std::map<std::string, std::string> scope_variables (std::map<std::string, std::string> const& base, scope::context_t const& scope)
	{
		std::map<std::string, std::string> res = base;
		std::vector<item_ptr> const& items = query(kFieldSettingName, "shellVariables", scope, kItemTypeSettings, oak::uuid_t(), false);

		std::vector< std::set<std::string> > stack;
		riterate(item, items)
		{
			stack.push_back(std::set<std::string>());
			for(auto pair : parse_variables(*item))
			{
				auto tmp = (*item)->bundle_variables();
				res[pair.first] = format_string::expand(pair.second, tmp << res);
				stack.back().insert(pair.first);
			}
		}

		std::set<std::string> didSet, shouldUnset;
		riterate(set, stack)
		{
			std::vector<std::string> tmp;
			std::set_intersection(set->begin(), set->end(), didSet.begin(), didSet.end(), back_inserter(tmp));

			if(tmp.empty())
					didSet.insert(set->begin(), set->end());
			else	shouldUnset.insert(set->begin(), set->end());
		}

		std::vector<std::string> tmp;
		std::set_difference(shouldUnset.begin(), shouldUnset.end(), didSet.begin(), didSet.end(), back_inserter(tmp));
		iterate(key, tmp)
			res.erase(*key);

		return res;
	}

	// ====================
	// = Simpler Wrappers =
	// ====================

	plist::any_t value_for_setting (std::string const& setting, scope::context_t const& scope, item_ptr* match)
	{
		citerate(item, query(kFieldSettingName, setting, scope, kItemTypeSettings))
		{
			plist::any_t res;
			if(plist::get_key_path((*item)->plist(), "settings." + setting, res))
			{
				if(match)
					*match = *item;
				return res;
			}
		}
		return plist::any_t();
	}

	std::vector<item_ptr> drag_commands_for_path (std::string const& path, scope::context_t const& scope)
	{
		std::string ext = text::lowercase(path);
		while(ext != "")
		{
			std::vector<item_ptr> const& res = query(kFieldDropExtension, ext, scope, kItemTypeDragCommand);
			if(!res.empty())
				return res;

			ext = path::extensions(ext);
			if(ext.find('.') == 0)
				ext = ext.substr(1);
		}
		return query(kFieldDropExtension, "*", scope, kItemTypeDragCommand);
	}
	
	std::vector<item_ptr> grammars_for_path (std::string const& path)
	{
		std::multimap<ssize_t, item_ptr> ordering;
		citerate(item, query(kFieldAny, NULL_STR, scope::wildcard, kItemTypeGrammar))
		{
			citerate(ext, (*item)->values_for_field(kFieldGrammarExtension))
			{
				if(ssize_t rank = path::rank(path, *ext))
					ordering.emplace(rank, *item);
			}
		}
		return ordering.empty() ? std::vector<item_ptr>() : std::vector<item_ptr>(1, ordering.begin()->second);
	}

} /* bundles */