#include "delta.h"
#include <text/format.h>

// ================
// = Create Delta =
// ================

namespace
{
	struct test_equal_helper_t : boost::static_visitor<bool>
	{
		test_equal_helper_t (plist::any_t const& rhs) : rhs(rhs) { }
		plist::any_t const& rhs;

		template <typename T>
		bool operator() (T const& lhs) const
		{
			if(T const* value = boost::get<T>(&rhs))
				return lhs == *value;
			return false;
		}
	};
}

static bool equal (plist::any_t const& lhs, plist::any_t const& rhs)
{
	return boost::apply_visitor(test_equal_helper_t(rhs), lhs);
}

static void delta_plist_helper (plist::dictionary_t const& oldDict, plist::dictionary_t const& newDict, plist::dictionary_t& changed, plist::array_t& deleted, std::vector<std::string> path = std::vector<std::string>())
{
	std::set<std::string> deletedKeys;
	iterate(pair, oldDict)
		deletedKeys.insert(pair->first);
	iterate(pair, newDict)
		deletedKeys.erase(pair->first);

	iterate(key, deletedKeys)
	{
		std::vector<std::string> newPath(path);
		newPath.push_back(*key);
		deleted.push_back(text::join(newPath, "."));
	}

	iterate(newValue, newDict)
	{
		std::vector<std::string> newPath(path);
		newPath.push_back(newValue->first);

		plist::dictionary_t::const_iterator oldValue = oldDict.find(newValue->first);
		if(boost::get<plist::dictionary_t>(&newValue->second) && boost::get<plist::dictionary_t>(&oldValue->second))
			delta_plist_helper(boost::get<plist::dictionary_t>(oldValue->second), boost::get<plist::dictionary_t>(newValue->second), changed, deleted, newPath);
		else if(oldValue == oldDict.end() || !equal(oldValue->second, newValue->second))
			changed.insert(std::make_pair(text::join(newPath, "."), newValue->second));
	}
}

namespace plist
{
	plist::dictionary_t create_delta (plist::dictionary_t const& oldDict, plist::dictionary_t const& newDict)
	{
		plist::dictionary_t changed;
		plist::array_t deleted;
		delta_plist_helper(oldDict, newDict, changed, deleted);

		plist::dictionary_t::const_iterator uuid = oldDict.find("uuid");

		plist::dictionary_t res;
		res["isDelta"] = true;
		if(!changed.empty())
			res["changed"] = changed;
		if(!deleted.empty())
			res["deleted"] = deleted;
		if(uuid != oldDict.end())
			res["uuid"] = uuid->second;
		return res;
	}
	
} /* plist */

// ===============
// = Merge Delta =
// ===============

static void erase_key_path (plist::dictionary_t& plist, std::string const& keyPath)
{
	std::string::size_type sep = keyPath.find('.');
	std::map<std::string, plist::any_t>::iterator it = plist.find(keyPath.substr(0, sep));
	if(it != plist.end())
	{
		if(sep == std::string::npos)
		{
			plist.erase(it);
		}
		else
		{
			if(plist::dictionary_t* dict = boost::get<plist::dictionary_t>(&it->second))
				erase_key_path(*dict, keyPath.substr(sep+1));
		}
	}
}

static void update_key_path (plist::dictionary_t& plist, std::string const& keyPath, plist::any_t const& value)
{
	std::string::size_type sep = keyPath.find('.');
	std::string const& key = keyPath.substr(0, sep);
	std::map<std::string, plist::any_t>::iterator it = plist.find(key);
	if(sep == std::string::npos)
	{
		if(it == plist.end())
				plist.insert(std::make_pair(keyPath, value));
		else	it->second = value;
	}
	else
	{
		if(it == plist.end())
		{
			plist::dictionary_t dict;
			update_key_path(dict, keyPath.substr(sep+1), value);
			plist.insert(std::make_pair(key, dict));
		}
		else
		{
			if(plist::dictionary_t* dict = boost::get<plist::dictionary_t>(&it->second))
				update_key_path(*dict, keyPath.substr(sep+1), value);
			else if(plist::array_t* array = boost::get<plist::array_t>(&it->second))
				array->push_back(keyPath.substr(sep+1));
			else
				fprintf(stderr, "error: unable to update key path ‘%s’ for plist:\n%s\n", keyPath.c_str(), to_s(plist).c_str());
		}
	}
}

namespace plist
{
	dictionary_t merge_delta (std::vector<dictionary_t> const& plists)
	{
		dictionary_t res;
		bool didFindNonDelta = false;

		riterate(it, plists)
		{
			dictionary_t const& plist = *it;
			if(plist.find("isDelta") != plist.end())
			{
				if(plist.find("deleted") != plist.end())
				{
					array_t const& deleted = boost::get<array_t>(plist.find("deleted")->second);
					iterate(it, deleted)
					{
						if(std::string const* str = boost::get<std::string>(&*it))
							erase_key_path(res, *str);
					}
				}

				if(plist.find("changed") != plist.end())
				{
					dictionary_t const& changed = boost::get<dictionary_t>(plist.find("changed")->second);
					iterate(it, changed)
						update_key_path(res, it->first, it->second);
				}
			}
			else
			{
				res = plist;
				didFindNonDelta = true;
			}
		}
		return didFindNonDelta ? res : dictionary_t();
	}

} /* plist */
