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

static std::string encode_key (std::string const& key)
{
	std::string res;
	for(char const& ch : key)
	{
		if(ch == '\\' || ch == '.')
			res.append(1, '\\');
		res.append(1, ch);
	}
	return res;
}

static void delta_plist_helper (plist::dictionary_t const& oldDict, plist::dictionary_t const& newDict, plist::dictionary_t& changed, plist::array_t& deleted, std::vector<std::string> path = std::vector<std::string>())
{
	std::set<std::string> deletedKeys;
	for(auto const& pair : oldDict)
		deletedKeys.insert(pair.first);
	for(auto const& pair : newDict)
		deletedKeys.erase(pair.first);

	for(auto const& key : deletedKeys)
	{
		std::vector<std::string> newPath(path);
		newPath.push_back(key);
		std::transform(newPath.begin(), newPath.end(), newPath.begin(), &encode_key);
		deleted.push_back(text::join(newPath, "."));
	}

	for(auto const& newValue : newDict)
	{
		std::vector<std::string> newPath(path);
		newPath.push_back(newValue.first);

		plist::dictionary_t::const_iterator oldValue = oldDict.find(newValue.first);
		if(boost::get<plist::dictionary_t>(&newValue.second) && oldValue != oldDict.end() && boost::get<plist::dictionary_t>(&oldValue->second))
		{
			delta_plist_helper(boost::get<plist::dictionary_t>(oldValue->second), boost::get<plist::dictionary_t>(newValue.second), changed, deleted, newPath);
		}
		else if(oldValue == oldDict.end() || !equal(oldValue->second, newValue.second))
		{
			std::transform(newPath.begin(), newPath.end(), newPath.begin(), &encode_key);
			changed.emplace(text::join(newPath, "."), newValue.second);
		}
	}
}

static std::vector<std::string> parse_key_path (std::string const& keyPath)
{
	std::vector<std::string> res(1);
	bool escape = false;
	for(char const& ch : keyPath)
	{
		if(!escape && ch == '.')
		{
			res.push_back(std::string());
		}
		else if(!escape && ch == '\\')
		{
			escape = true;
		}
		else
		{
			res.back().append(1, ch);
			escape = false;
		}
	}
	return res;
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
	auto v = parse_key_path(keyPath);
	plist::dictionary_t* current = &plist;

	for(auto key = v.begin(); key != v.end() && current; )
	{
		auto it = current->find(*key);
		if(it == current->end())
			return;

		if(++key != v.end())
				current = boost::get<plist::dictionary_t>(&it->second);
		else	current->erase(it);
	}
}

static void update_key_path (plist::dictionary_t& plist, std::string const& keyPath, plist::any_t const& value)
{
	auto v = parse_key_path(keyPath);
	plist::dictionary_t* current = &plist;

	for(auto key = v.begin(); key != v.end(); )
	{
		auto it = current->find(*key);
		if(it == current->end())
		{
			plist::any_t payload = value;
			for(auto lastKey = v.end(); --lastKey != key; )
			{
				plist::dictionary_t tmp;
				tmp.emplace(*lastKey, payload);
				payload = tmp;
			}
			current->emplace(*key, payload);
		}
		else if(++key == v.end())
		{
			it->second = value;
		}
		else
		{
			if(current = boost::get<plist::dictionary_t>(&it->second))
				continue;
			else if(plist::array_t* array = boost::get<plist::array_t>(&it->second))
				array->push_back(*key);
			else
				fprintf(stderr, "error: unable to update key path ‘%s’ for plist:\n%s\n", keyPath.c_str(), to_s(plist).c_str());
		}
		break;
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
				auto deletedIt = plist.find("deleted");
				if(array_t const* deleted = deletedIt != plist.end() ? boost::get<array_t>(&deletedIt->second) : nullptr)
				{
					for(auto item : *deleted)
					{
						if(std::string const* str = boost::get<std::string>(&item))
							erase_key_path(res, *str);
					}
				}

				auto changedIt = plist.find("changed");
				if(dictionary_t const* changed = changedIt != plist.end() ? boost::get<dictionary_t>(&changedIt->second) : nullptr)
				{
					for(auto pair : *changed)
						update_key_path(res, pair.first, pair.second);
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
