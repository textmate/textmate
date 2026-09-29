#include "wrappers.h"
#include "query.h"
#include <text/src/case.h>
#include <io/src/path.h>
#include <regexp/src/regexp.h>
#include <regexp/src/format_string.h>
#include <text/src/tokenize.h>

namespace bundles
{
	// ==============================
	// = Shell Command Requirements =
	// ==============================

	required_command_t::required_command_t (std::string const& command, std::string const& moreInfoUrl, std::string const& variable, std::vector<std::string> const& locations) : command(command), more_info_url(moreInfoUrl), variable(variable), locations(locations)
	{
	}

	static std::vector<std::string> convert_locations (plist::array_t const& array)
	{
		std::vector<std::string> res;
		for(auto const& it : array)
		{
			if(std::string const* str = boost::get<std::string>(&it))
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
			os_log_error(OS_LOG_DEFAULT, "No PATH!!!");
			for(auto const& pair : environment)
				os_log_error(OS_LOG_DEFAULT, "%{public}s = %{public}s", pair.first.c_str(), pair.second.c_str());
		}
		return res;
	}

	bool missing_requirement (item_ptr const& item, std::map<std::string, std::string>& environment, required_command_t* failedRequirement)
	{
		for(auto requirement : requirements(item))
		{
			std::vector<std::string> candidates;

			auto envVariable = environment.find(requirement.variable);
			if(envVariable != environment.end())
			{
				auto value = envVariable->second;
				auto firstSpace = value.find(" ");
				if(firstSpace != std::string::npos)
					candidates.push_back(value.substr(0, firstSpace));
				candidates.push_back(value);

				if(std::find_if(candidates.begin(), candidates.end(), [](std::string const& path){ return path::is_executable(path); }) != candidates.end())
					return false;
			}

			candidates.clear();
			for(auto path : search_paths(environment))
				candidates.push_back(path::join(path, requirement.command));

			auto exe = std::find_if(candidates.begin(), candidates.end(), [](std::string const& path){ return path::is_executable(path); });
			if(exe != candidates.end())
			{
				if(requirement.variable != NULL_STR)
					environment[requirement.variable] = *exe;
				return false;
			}

			candidates.clear();
			for(auto path : requirement.locations)
				candidates.push_back(format_string::expand(path, environment));

			exe = std::find_if(candidates.begin(), candidates.end(), [](std::string const& path){ return path::is_executable(path); });
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

	std::vector< std::pair<std::string, std::string> > shell_variables (item_ptr const& item)
	{
		plist::array_t variables;
		plist::get_key_path(item->plist(), "settings.shellVariables", variables);

		std::vector< std::pair<std::string, std::string> > res;
		for(auto const& it : variables)
		{
			bool disabled;
			std::string key, value;
			if(!(plist::get_key_path(it, "disabled", disabled) && disabled) && plist::get_key_path(it, "name", key) && plist::get_key_path(it, "value", value))
				res.emplace_back(key, value);
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
			for(auto pair : shell_variables(*item))
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
		for(auto const& key : tmp)
			res.erase(key);

		return res;
	}

	// ====================
	// = Simpler Wrappers =
	// ====================

	plist::any_t value_for_setting (std::string const& setting, scope::context_t const& scope, item_ptr* match)
	{
		struct cache_t : bundles::callback_t
		{
			cache_t ()
			{
				bundles::add_callback(this);
			}

			void bundles_did_change ()
			{
				std::lock_guard<std::mutex> lock(mutex);
				map.clear();
			}

			std::map<std::string, item_ptr> map;
			std::mutex mutex;
		};

		static cache_t cache;
		std::lock_guard<std::mutex> lock(cache.mutex);

		std::string const key = setting + "\037" + to_s(scope);
		auto iter = cache.map.find(key);
		if(iter == cache.map.end())
		{
			if(cache.map.size() > 1000)
				cache.map.clear();

			auto items = query(kFieldSettingName, setting, scope, kItemTypeSettings);
			iter = cache.map.emplace(key, items.empty() ? item_ptr() : items.front()).first;
		}

		plist::any_t res;
		if(item_ptr item = iter->second)
		{
			if(plist::get_key_path(item->plist(), "settings." + setting, res))
			{
				if(match)
					*match = item;
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "Missing setting ‘%{public}s’ in %{public}s", setting.c_str(), item->name_with_bundle().c_str());
			}
		}
		return res;
	}

	std::vector<item_ptr> drag_commands_for_path (std::string const& path, scope::context_t const& scope)
	{
		std::string ext = text::lowercase(path);
		while(!ext.empty())
		{
			std::vector<item_ptr> const& res = query(kFieldDropExtension, ext, scope, kItemTypeDragCommand);
			if(!res.empty())
				return res;

			ext = path::extensions(ext);
			if(!ext.empty() && ext.front() == '.')
				ext = ext.substr(1);
		}
		return query(kFieldDropExtension, "*", scope, kItemTypeDragCommand);
	}

} /* bundles */
