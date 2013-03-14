#import "Private.h"
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundleMenu);

@interface NSObject (HasSelection)
- (BOOL)hasSelection;
- (scope::context_t const&)scopeContext;
@end

@implementation BundleMenuDelegate
+ (BundleMenuDelegate*)sharedInstance
{
	static BundleMenuDelegate* instance = [BundleMenuDelegate new];
	return instance;
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_BundleMenu, bug("\n"););
	[aMenu removeAllItems];

	BOOL hasSelection = NO;
	if(id textView = [NSApp targetForAction:@selector(hasSelection)])
		hasSelection = [textView hasSelection];

	scope::context_t scope = "";
	if(id textView = [NSApp targetForAction:@selector(scopeContext)])
		scope = [textView scopeContext];

	bundles::item_ptr umbrellaItem = bundles::lookup(to_s(aMenu.title));
	if(!umbrellaItem)
		return;

	citerate(item, umbrellaItem->menu())
	{
		switch((*item)->kind())
		{
			case bundles::kItemTypeMenu:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:(*item)->name()] action:NULL keyEquivalent:@""];

				menuItem.submenu = [[NSMenu alloc] initWithTitle:[NSString stringWithCxxString:(*item)->uuid()]];
				menuItem.submenu.delegate = [BundleMenuDelegate sharedInstance];
			}
			break;

			case bundles::kItemTypeMenuItemSeparator:
				[aMenu addItem:[NSMenuItem separatorItem]];
			break;

			case bundles::kItemTypeProxy:
			{
				auto const items = bundles::items_for_proxy(*item, scope);
				OakAddBundlesToMenu(items, hasSelection, true, aMenu, @selector(performBundleItemWithUUIDStringFrom:));

				if(items.empty())
				{
					NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:name_with_selection(*item, hasSelection)] action:@selector(nop:) keyEquivalent:@""];
					[menuItem setInactiveKeyEquivalentCxxString:key_equivalent(*item)];
					[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				}
			}
			break;

			default:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:name_with_selection(*item, hasSelection)] action:@selector(performBundleItemWithUUIDStringFrom:) keyEquivalent:@""];
				[menuItem setInactiveKeyEquivalentCxxString:key_equivalent(*item)];
				[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:(*item)->uuid()]];
			}
			break;
		}
	}
}
@end
