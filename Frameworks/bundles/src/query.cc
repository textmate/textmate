#include "query.h"
#include <text/ctype.h>
#include <text/parse.h>
#include <text/trim.h>
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

			std::recursive_mutex& mutex ()
			{
				return _cache_mutex;
			}

			void clear ()
			{
				std::lock_guard<std::recursive_mutex> lock(_cache_mutex);
				_cache.clear();
				_menus.clear();
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
		};

		static cache_t& cache ()
		{
			static cache_t cache;
			return cache;
		}
	}

	static void setup_full_name (oak::uuid_t const& menuUUID, std::map< oak::uuid_t, std::vector<oak::uuid_t> > const& menus, std::map<oak::uuid_t, item_ptr> const& items, std::string const& prefix, std::string const& suffix)
	{
		std::map< oak::uuid_t, std::vector<oak::uuid_t> >::const_iterator menu = menus.find(menuUUID);
		if(menu != menus.end())
		{
			for(auto const& uuid : menu->second)
			{
				std::map<oak::uuid_t, item_ptr>::const_iterator item = items.find(uuid);
				if(item == items.end())
					continue;

				if(item->second->kind() == kItemTypeMenu)
						setup_full_name(item->second->uuid(), menus, items, prefix + item->second->name() + " » ", suffix);
				else	item->second->set_full_name(prefix + item->second->name() + suffix);
			}
		}
	}

	bool set_index (std::vector<item_ptr> const& items, std::map< oak::uuid_t, std::vector<oak::uuid_t> > const& menus)
	{
		Callbacks(&callback_t::bundles_will_change);

		AllItems = items;
		AllMenus = menus;
		cache().clear();

		std::map< item_ptr, std::map<oak::uuid_t, item_ptr> > map; // bundle → { item_uuid → bundle_item }
		for(auto const& item : items)
		{
			if(item->bundle())
				map[item->bundle()].emplace(item->uuid(), item);
		}

		for(auto const& bundle : map)
			setup_full_name(bundle.first->uuid(), menus, bundle.second, "", " — " + bundle.first->name());

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

 	static void search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered);

	static void resolve_proxy (item_ptr item, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered)
	{
		std::string actionClass;
		if(plist::get_key_path(item->plist(), "content", actionClass))
		{
			for(auto const& aClass : text::split(actionClass, "||"))
			{
				size_t oldSize = ordered.size();
				search(kFieldSemanticClass, text::trim(aClass), scope, kind, bundle, includeDisabledItems, ordered);
				if(ordered.size() != oldSize)
					break;
			}
		}
	}

 	static void cache_search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered)
	{
		std::lock_guard<std::recursive_mutex> lock(cache().mutex());
		std::multimap<std::string, item_ptr> const& values = cache().fetch(field);
		foreach(pair, values.lower_bound(value), field == kFieldSemanticClass ? values.lower_bound(value + "/") : values.upper_bound(value)) // Since kFieldSemanticClass is a prefix match we want lower bound of the first item after the last possible prefix (which would be “value.zzzzz…” → “value/”).
		{
			double rank = 1.0;
			if(pair->second->does_match(field, value, scope, kind, bundle, &rank))
			{
				if(pair->second->kind() == kItemTypeProxy)
						resolve_proxy(pair->second, scope, kind, bundle, includeDisabledItems, ordered);
				else	ordered.emplace(rank, pair->second);
			}
		}
	}

 	static void linear_search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered)
	{
		for(auto const& item : AllItems)
		{
			if(is_deleted(item) || !includeDisabledItems && is_disabled(item))
				continue;

			double rank = 1.0;
			if(item->does_match(field, value, scope, kind, bundle, &rank))
			{
				if(item->kind() == kItemTypeProxy)
						resolve_proxy(item, scope, kind, bundle, includeDisabledItems, ordered);
				else	ordered.emplace(rank, item);
			}
		}
	}

 	static void search (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool includeDisabledItems, std::multimap<double, item_ptr>& ordered)
	{
		static auto const CachedFields = new std::set<std::string>{ kFieldKeyEquivalent, kFieldTabTrigger, kFieldSemanticClass, kFieldGrammarScope, kFieldSettingName };
		if(!includeDisabledItems && !bundle && CachedFields->find(field) != CachedFields->end())
				cache_search(field, value, scope, kind, bundle, includeDisabledItems, ordered);
		else	linear_search(field, value, scope, kind, bundle, includeDisabledItems, ordered);
	}

	std::vector<item_ptr> query (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, bool filter, bool includeDisabledItems)
	{
		std::multimap<double, item_ptr> ordered;
		search(field, value, scope, kind, bundle, includeDisabledItems, ordered);

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
		for(auto const& item : AllItems)
		{
			if(item->uuid() == uuid)
				return item;
		}
		return item_ptr();
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

	static std::string format_bundle_item_title (std::string title, bool hasSelection)
	{
		static std::string const kSelectionSubString = " / Selection";

		std::string::size_type pos = title.find(kSelectionSubString);
		if(pos == 0 || pos == std::string::npos)
			return title;

		if(hasSelection)
		{
			std::string::size_type from = title.rfind(' ', pos - 1);
			if(from == std::string::npos)
				return title.erase(0, pos + 3);
			return title.erase(from + 1, pos + 3 - from - 1);
		}
		return title.erase(pos, kSelectionSubString.size());
	}

	std::string name_with_selection (item_ptr const& item, bool hasSelection)
	{
		return format_bundle_item_title(item->name(), hasSelection);
	}

	std::string full_name_with_selection (item_ptr const& item, bool hasSelection)
	{
		return format_bundle_item_title(item->full_name(), hasSelection);
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
				if(sClass.find(actionClass) == 0)
					return key_equivalent(proxyItem);
			}
		}

		return res;
	}

} /* bundles */