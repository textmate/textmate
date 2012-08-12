#import "BundleMenuDelegate.h"
#import <bundles/bundles.h>
#import <command/parser.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>

@implementation BundleMenuDelegate
- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem
{
	if(self = [super init])
		umbrellaItem = aBundleItem;
	return self;
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	[aMenu removeAllItems];

	citerate(item, umbrellaItem->menu())
	{
		switch((*item)->kind())
		{
			case bundles::kItemTypeMenu:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:(*item)->name()] action:NULL keyEquivalent:@""];

				menuItem.submenu = [[NSMenu new] autorelease];
				menuItem.submenu.autoenablesItems = NO;
				BundleMenuDelegate* delegate = [[BundleMenuDelegate alloc] initWithBundleItem:*item];
				menuItem.submenu.delegate = delegate;
			}
			break;

			case bundles::kItemTypeMenuItemSeparator:
				[aMenu addItem:[NSMenuItem separatorItem]];
			break;

			default:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:(*item)->name()] action:@selector(doBundleItem:) keyEquivalent:@""];
				[menuItem setKeyEquivalentCxxString:(*item)->value_for_field(bundles::kFieldKeyEquivalent)];
				[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:(*item)->uuid()]];
			}
			break;
		}
	}
}

- (void)menuWillOpen:(NSMenu*)aMenu
{
	[aMenu enableTabTriggers];
}

- (void)menuDidClose:(NSMenu*)aMenu
{
	// We are not allowed to modify ‘aMenu’ here so we do it “afterDelay” — I really wish we didn’t have to do this at all…
	[self performSelector:@selector(zapMenu:) withObject:aMenu afterDelay:0.0];
}

- (void)zapMenu:(NSMenu*)aMenu
{
	// After a menu has been up, the system will cache all its key equivalents. Even if we set all the key equivalents to the empty string, the system will still remember. The only workaround seems to be to delete all the entries in the menu.
	[aMenu removeAllItems];
}
@end
