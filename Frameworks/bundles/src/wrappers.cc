#include "wrappers.h"
#include "query.h"
#include <text/case.h>
#include <io/path.h>
#include <regexp/regexp.h>
#include <regexp/format_string.h>

namespace bundles
{
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

	std::map<std::string, std::string> scope_variables (scope::context_t const& scope, std::map<std::string, std::string> const& base)
	{
		std::map<std::string, std::string> res = base;
		std::vector<item_ptr> const& items = query(kFieldSettingName, "shellVariables", scope, kItemTypeSettings, oak::uuid_t(), false);

		std::vector< std::set<std::string> > stack;
		riterate(item, items)
		{
			stack.push_back(std::set<std::string>());
			citerate(pair, parse_variables(*item))
			{
				std::string const value = format_string::expand(pair->second, (*item)->bundle_variables(res));
				res[pair->first] = value;
				stack.back().insert(pair->first);
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
					ordering.insert(std::make_pair(rank, *item));
			}
		}
		return ordering.empty() ? std::vector<item_ptr>() : std::vector<item_ptr>(1, ordering.begin()->second);
	}

} /* bundles */