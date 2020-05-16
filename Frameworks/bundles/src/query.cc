#include "query.h"
#include <text/ctype.h>
#include <text/parse.h>
#include <text/trim.h>
#include <regexp/format_string.h>
#include <oak/callbacks.h>

namespace bundles
{
	static std::vector<item_ptr> AllItems;
	static std::map< oak::uuid_t, std::vector<oak::uuid_t> > AllMenus;
	static oak::callbacks_t<callback_t, true> Callbacks;

	void add_callback (callback_t* cb)    { Callbacks.add(cb); }
	void remove_callback (callback_t* cb) { Callbacks.remove(cb); }

	static bool is_deleted (item_ptr item)
	{
		return item->deleted() || item->bundle() && item->bundle()->deleted();
	}

	static bool is_disabled (item_ptr item)
	{
		return item->disabled() || item->bundle() && item->bundle()->disabled();
	}

	namespace
	{
		struct cache_t
		{
			std::multimap<std::string, item_ptr> const& fetch (std::string const& field)
			{
				std::map< std::string, std::multimap<std::string, item_ptr> >::const_iterator it = _cache.find(field);
				if(it != _cache.end())
					return it->second;

				std::multimap<std::string, item_ptr>& res = _cache[field];
				for(auto const& item : AllItems)
				{
					if(is_deleted(item) || is_disabled(item))
						continue;
					for(auto const& value : item->values_for_field(field))
						res.emplace(value, item);
				}
				return res;
			}

			std::vector<item_ptr> const& menu (oak::uuid_t const& uuid)
			{
				if(_menus.empty())
				{
					std::map< item_ptr, std::map<oak::uuid_t, item_ptr> > map; // bundle → { item_uuid → bundle_item }
					for(auto const& item : AllItems)
					{
						if(item->bundle())
							map[item->bundle()].emplace(item->uuid(), item);
					}

					for(auto const& bundle : map)
					{
						std::set<item_ptr> didInclude;
						setup_menu(bundle.first, bundle.second, AllMenus, _menus, didInclude);

						std::multimap<std::string, item_ptr, text::less_t> leftOut;
						for(auto const& pair : bundle.second)
						{
							item_ptr item = pair.second;
							if((item->kind() & kItemTypeMenuTypes) && !item->hidden_from_user() && didInclude.find(item) == didInclude.end())
								leftOut.emplace(item->name(), item);
						}

						if(!leftOut.empty())
							std::transform(leftOut.begin(), leftOut.end(), back_inserter(_menus[bundle.first->uuid()]), [](std::pair<std::string, item_ptr> const& p){ return p.second; });
					}
				}

				static std::vector<item_ptr> const kEmptyMenu;
				std::map< oak::uuid_t, std::vector<item_ptr> >::const_iterator menu = _menus.find(uuid);
				return menu != _menus.end() ? menu->second : kEmptyMenu;
			}

			item_ptr lookup (oak::uuid_t const& uuid)
			{
				std::lock_guard<std::recursive_mutex> lock(_cache_mutex);
				if(_uuids.empty())
				{
					for(auto const& item : AllItems)
						_uuids.emplace(item->uuid(), item);
				}
				auto it = _uuids.find(uuid);
				return it != _uuids.end() ? it->second : item_ptr();
			}

			std::recursive_mutex& mutex ()
			{
				return _cache_mutex;
			}

			void clear ()
			{
				std::lock_guard<std::recursive_mutex> lock(_cache_mutex);
				_cache.clear();
				_menus.clear();
				_uuids.clear();
			}

		private:
			static void setup_menu (item_ptr menuItem, std::map<oak::uuid_t, item_ptr> const& items, std::map< oak::uuid_t, std::vector<oak::uuid_t> > const& menus, std::map< oak::uuid_t, std::vector<item_ptr> >& res, std::set<item_ptr>& didInclude)
			{
				std::map< oak::uuid_t, std::vector<oak::uuid_t> >::const_iterator menu = menus.find(menuItem->uuid());
				if(menu != menus.end())
				{
					std::vector<item_ptr>& resolved = res[menuItem->uuid()];
					for(auto const& itemUUID : menu->second)
					{
						std::map<oak::uuid_t, item_ptr>::const_iterator item = items.find(itemUUID);
						if(item != items.end())
						{
							didInclude.insert(item->second);
							if(item->second->hidden_from_user())
								continue;
							resolved.push_back(item->second);
							if(item->second->kind() == kItemTypeMenu)
								setup_menu(item->second, items, menus, res, didInclude);
						}
						else if(itemUUID == item_t::menu_item_separator()->uuid())
						{
							resolved.push_back(item_t::menu_item_separator());
						}
					}
				}
			}

			std::recursive_mutex _cache_mutex;
			std::map< std::string, std::multimap<std::string, item_ptr> > _cache;
			std::map< oak::uuid_t, std::vector<item_ptr> > _menus;
			std::map< oak::uuid_t, item_ptr > _uuids;
		};

		static cache_t& cache ()
		{
			static cache_t cache;
			return cache;
		}
	}

	bool set_index (std::vector<item_ptr> const& items, std::map< oak::uuid_t, std::vector<oak::uuid_t> > const& menus)
	{
		Callbacks(&callback_t::bundles_will_change);

		AllItems = items;
		AllMenus = menus;
		cache().clear();

		std::map<oak::uuid_t, item_ptr> lookupTable;
		for(auto const& item : items)
		{
			if(item_ptr bundle = item->bundle())
				item->set_parent_menu(bundle->uuid());
			lookupTable.emplace(item->uuid(), item);
		}

		for(auto const& pair : menus)
		{
			for(auto const& itemUUID : pair.second)
			{
				if(auto item = lookupTable[itemUUID])
					item->set_parent_menu(pair.first);
			}
		}

		Callbacks(&callback_t::bundles_did_change);

		bool res = true;
		for(auto const& item : AllItems)
			res = res && item;
		return res;
	}

	void add_item (item_ptr item)
	{
		Callbacks(&callback_t::bundles_will_change);
		AllItems.push_back(item);
		cache().clear();
		Callbacks(&callback_t::bundles_did_change);
	}

	void remove_item (item_ptr item)
	{
		iterate(it, AllItems)
		{
			if((*it)->uuid() != item->uuid())
				continue;

			Callbacks(&callback_t::bundles_will_change);
			AllItems.erase(it);
			cache().clear();
			Callbacks(&callback_t::bundles_did_change);
			break;
		}
	}

	// ===================
	// = Query Functions =
	// ===================

	static void search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, bool resolveProxyItems, std::multimap<double, item_ptr>& ordered);

	static void resolve_proxy (item_ptr item, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered)
	{
		std::string actionClass;
		if(plist::get_key_path(item->plist(), "content", actionClass))
		{
			for(auto const& aClass : text::split(actionClass, "||"))
			{
				size_t oldSize = ordered.size();
				search(kFieldSemanticClass, text::trim(aClass), scope, kind, bundle, includeDisabledItems, true, ordered);
				if(ordered.size() != oldSize)
					break;
			}
		}
	}

	static void cache_search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, bool resolveProxyItems, std::multimap<double, item_ptr>& ordered)
	{
		std::lock_guard<std::recursive_mutex> lock(cache().mutex());
		std::multimap<std::string, item_ptr> const& values = cache().fetch(field);
		foreach(pair, values.lower_bound(value), field == kFieldSemanticClass ? values.lower_bound(value + "/") : values.upper_bound(value)) // Since kFieldSemanticClass is a prefix match we want lower bound of the first item after the last possible prefix (which would be “value.zzzzz…” → “value/”).
		{
			if(auto rank = pair->second->does_match(field, value, scope, kind, bundle))
			{
				if(pair->second->kind() == kItemTypeProxy && resolveProxyItems)
						resolve_proxy(pair->second, scope, kind, bundle, includeDisabledItems, ordered);
				else	ordered.emplace(*rank, pair->second);
			}
		}
	}

	static void linear_search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, bool resolveProxyItems, std::multimap<double, item_ptr>& ordered)
	{
		for(auto const& item : AllItems)
		{
			if(is_deleted(item) || !includeDisabledItems && is_disabled(item))
				continue;

			if(auto rank = item->does_match(field, value, scope, kind, bundle))
			{
				if(item->kind() == kItemTypeProxy && resolveProxyItems)
						resolve_proxy(item, scope, kind, bundle, includeDisabledItems, ordered);
				else	ordered.emplace(*rank, item);
			}
		}
	}

	static void search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, bool resolveProxyItems, std::multimap<double, item_ptr>& ordered)
	{
		static auto const CachedFields = new std::set<std::string>{ kFieldKeyEquivalent, kFieldTabTrigger, kFieldSemanticClass, kFieldGrammarScope, kFieldSettingName };
		if(!includeDisabledItems && !bundle && CachedFields->find(field) != CachedFields->end())
				cache_search(field, value, scope, kind, bundle, includeDisabledItems, resolveProxyItems, ordered);
		else	linear_search(field, value, scope, kind, bundle, includeDisabledItems, resolveProxyItems, ordered);
	}

	std::vector<item_ptr> query (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool filter, bool includeDisabledItems, bool resolveProxyItems)
	{
		std::multimap<double, item_ptr> ordered;
		search(field, value, scope, kind, bundle, includeDisabledItems, resolveProxyItems, ordered);

		std::vector<item_ptr> res;
		for(std::multimap<double, item_ptr>::reverse_iterator it = ordered.rbegin(); it != ordered.rend() && (!filter || it->first == ordered.rbegin()->first); ++it)
			res.push_back(it->second);
		return res;
	}

	std::vector<item_ptr> items_for_proxy (item_ptr proxyItem, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool filter, bool includeDisabledItems)
	{
		std::string actionClass;
		if(plist::get_key_path(proxyItem->plist(), "content", actionClass))
		{
			for(auto const& aClass : text::split(actionClass, "||"))
			{
				auto const res = query(kFieldSemanticClass, text::trim(aClass), scope, kind, bundle, filter, includeDisabledItems);
				if(!res.empty())
					return res;
			}
		}
		return std::vector<item_ptr>();
	}

	item_ptr lookup (oak::uuid_t const& uuid)
	{
		return cache().lookup(uuid);
	}

	std::vector<item_ptr> item_t::menu (bool includeDisabledItems) const
	{
		std::lock_guard<std::recursive_mutex> lock(cache().mutex());
		std::vector<item_ptr> res;
		for(auto const& item : cache().menu(_uuid))
		{
			if(is_deleted(item) || !includeDisabledItems && is_disabled(item))
				continue;
			res.push_back(item);
		}
		return res;
	}

	std::string name_with_selection (item_ptr const& item, bool hasSelection)
	{
		return format_string::replace(item->name(), "\\b(\\w+) / (Selection)\\b", hasSelection ? "$2" : "$1");
	}

	std::string menu_path (item_ptr item)
	{
		std::deque<std::string> path;
		while(item = lookup(item->parent_menu()))
			path.push_front(item->name());
		return text::join(path, " ‣ ");
	}

	std::string key_equivalent (item_ptr const& item)
	{
		std::string const res = item->value_for_field(kFieldKeyEquivalent);
		if(res != NULL_STR || item->kind() == kItemTypeProxy)
			return res;

		std::string const sClass = item->value_for_field(kFieldSemanticClass);
		if(sClass == NULL_STR)
			return res;

		for(auto const& proxyItem : AllItems)
		{
			if(proxyItem->kind() != kItemTypeProxy)
				continue;

			std::string actionClass;
			if(plist::get_key_path(proxyItem->plist(), "content", actionClass))
			{
				if(sClass.compare(0, actionClass.size(), actionClass) == 0)
					return key_equivalent(proxyItem);
			}
		}

		return res;
	}

} /* bundles */
