#import "menu.h"
#import "query.h"
#import <OakFoundation/NSString Additions.h>
#import <text/ctype.h>
#import <cf/cf.h>
#import <oak/oak.h>

@interface BundlePopupMenuTarget : NSObject
{
	NSInteger selectedIndex;
}
@property NSInteger selectedIndex;
@end

@implementation BundlePopupMenuTarget
@synthesize selectedIndex;
- (id)init
{
	if((self = [super init]))
		self.selectedIndex = NSNotFound;
	return self;
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	return [menuItem action] == @selector(takeSelectedItemIndexFrom:);
}

- (void)takeSelectedItemIndexFrom:(id)sender
{
	ASSERT([sender isKindOfClass:[NSMenuItem class]]);
	self.selectedIndex = [(NSMenuItem*)sender tag];
}
@end

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

	item_ptr show_menu_for_items (std::vector<item_ptr> const& items, CGPoint const& pos, bool hasSelection)
	{
		if(items.empty())
			return item_ptr();
		else if(items.size() == 1)
			return items.front();

		NSMenu* menu = [[[NSMenu alloc] init] autorelease];
		[menu setFont:[NSFont menuFontOfSize:[NSFont smallSystemFontSize]]];
		BundlePopupMenuTarget* menuTarget = [[[BundlePopupMenuTarget alloc] init] autorelease];
		
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
				NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(takeSelectedItemIndexFrom:) keyEquivalent:@""];
				[menuItem setTarget:menuTarget];
				[menuItem setTag:key];
				
				res.push_back(pair->second);
				if(++key <= 10)
				{
					[menuItem setKeyEquivalent:[NSString stringWithFormat:@"%d", key % 10]];
					[menuItem setKeyEquivalentModifierMask:0];
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
					ordering.insert(std::make_pair(name_with_selection(*item, hasSelection), *item));
				std::transform(ordering.begin(), ordering.end(), back_inserter(menuItems), [](std::pair<std::string, item_ptr> const& p){ return p.second; });

				menus.insert(std::make_pair(bundle->name(), menuItems));
			}

			bool showBundleHeadings = menus.size() > 1;
			iterate(pair, menus)
			{
				if(showBundleHeadings)
					[menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:NULL keyEquivalent:@""];

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
							[menu addItem:[NSMenuItem separatorItem]];
						pendingSeparator = false;

						NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:name_with_selection(*item, hasSelection)] action:@selector(takeSelectedItemIndexFrom:) keyEquivalent:@""];
						[menuItem setTarget:menuTarget];
						[menuItem setTag:key];

						res.push_back(*item);
						if(++key <= 10)
						{
							[menuItem setKeyEquivalent:[NSString stringWithFormat:@"%d", key % 10]];
							[menuItem setKeyEquivalentModifierMask:0];
						}
						if(showBundleHeadings)
							[menuItem setIndentationLevel:1];

						suppressSeparator = false;
					}
				}
			}
		}

		item_ptr selectedItem;
		if([menu popUpMenuPositioningItem:nil atLocation:NSPointFromCGPoint(pos) inView:nil] && menuTarget.selectedIndex != NSNotFound)
			selectedItem = res[menuTarget.selectedIndex];

		return selectedItem;
	}

} /* bundles */
