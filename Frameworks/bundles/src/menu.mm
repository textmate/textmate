#include "menu.h"
#include <text/ctype.h>
#include <cf/cf.h>
#include <oak/oak.h>

namespace bundles
{
	static std::vector<item_ptr> filtered_menu (item_ptr menuItem, std::set<item_ptr>* includedItems)
	{
		std::vector<item_ptr> res;
		citerate(item, menuItem->menu())
		{
			if((*item)->kind() == kItemTypeMenuItemSeparator)
			{
				res.push_back(*item);
			}
			else if((*item)->kind() == kItemTypeMenu)
			{
				res.push_back(item_t::menu_item_separator());
				std::vector<item_ptr> const& tmp = filtered_menu(*item, includedItems);
				res.insert(res.end(), tmp.begin(), tmp.end());
				res.push_back(item_t::menu_item_separator());
			}
			else if(includedItems->find(*item) != includedItems->end())
			{
				res.push_back(*item);
				includedItems->erase(*item);
			}
		}
		return res;
	}

	item_ptr show_menu_for_items (std::vector<item_ptr> const& items, CGPoint const& pos)
	{
		if(items.empty())
			return item_ptr();
		else if(items.size() == 1)
			return items.front();

		MenuRef menuRef;
		CreateNewMenu(0 /* menu id */, kMenuAttrDoNotCacheImage, &menuRef);
		SetMenuFont(menuRef, 0, 12);
		
		int key = 0;
		std::vector<item_ptr> res;

		bool onlyGrammars = true;
		iterate(item, items)
			onlyGrammars = onlyGrammars && (*item)->kind() == kItemTypeGrammar;

		if(onlyGrammars)
		{
			std::multimap<std::string, item_ptr, text::less_t> ordering;
			iterate(item, items)
				ordering.insert(std::make_pair((*item)->name(), *item));

			iterate(pair, ordering)
			{
				MenuItemIndex index;
				AppendMenuItemTextWithCFString(menuRef, cf::wrap(pair->first), 0, res.size(), &index);
				res.push_back(pair->second);
				if(++key <= 10)
				{
					SetMenuItemCommandKey(menuRef, index, false, (key % 10) + '0');
					SetMenuItemModifiers(menuRef, index, kMenuNoCommandModifier);
				}
			}
		}
		else
		{
			std::multimap<item_ptr, item_ptr> byBundle;
			iterate(item, items)
				byBundle.insert(std::make_pair((*item)->bundle(), *item));

			std::multimap<std::string, std::vector<item_ptr>, text::less_t> menus;
			while(!byBundle.empty())
			{
				item_ptr bundle = byBundle.begin()->first;
				std::set<item_ptr> includedItems;
				foreach(pair, byBundle.lower_bound(bundle), byBundle.upper_bound(bundle))
					includedItems.insert(pair->second);
				byBundle.erase(bundle);

				std::vector<item_ptr> menuItems = filtered_menu(bundle, &includedItems);

				std::multimap<std::string, item_ptr, text::less_t> ordering;
				iterate(item, includedItems)
					ordering.insert(std::make_pair((*item)->name(), *item));
				std::transform(ordering.begin(), ordering.end(), back_inserter(menuItems), [](std::pair<std::string, item_ptr> const& p){ return p.second; });

				menus.insert(std::make_pair(bundle->name(), menuItems));
			}

			bool showBundleHeadings = menus.size() > 1;
			iterate(pair, menus)
			{
				if(showBundleHeadings)
					AppendMenuItemTextWithCFString(menuRef, cf::wrap(pair->first), kMenuItemAttrSectionHeader, 0, NULL);

				bool suppressSeparator = true;
				bool pendingSeparator  = false;
				iterate(item, pair->second)
				{
					if((*item)->kind() == kItemTypeMenuItemSeparator)
					{
						if(!suppressSeparator)
							pendingSeparator = true;
					}
					else
					{
						if(pendingSeparator)
							AppendMenuItemTextWithCFString(menuRef, CFSTR(""), kMenuItemAttrSeparator, 0, NULL);
						pendingSeparator = false;

						MenuItemIndex index;
						AppendMenuItemTextWithCFString(menuRef, cf::wrap((*item)->name()), 0, res.size(), &index);
						res.push_back(*item);
						if(++key <= 10)
						{
							SetMenuItemCommandKey(menuRef, index, false, (key % 10) + '0');
							SetMenuItemModifiers(menuRef, index, kMenuNoCommandModifier);
						}
						if(showBundleHeadings)
							SetMenuItemIndent(menuRef, index, 1);

						suppressSeparator = false;
					}
				}
			}
		}

		item_ptr selectedItem;
		SInt32 selectedIndex = PopUpMenuSelect(menuRef, pos.x, pos.y, 0 /* pop-up item */);
		if(selectedIndex)
		{
			MenuCommand cmd = 0;
			GetMenuItemCommandID(menuRef, selectedIndex, &cmd);
			selectedItem = res[cmd];
		}

		DisposeMenu(menuRef);
		return selectedItem;
	}

} /* bundles */
