#import "Private.h"
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundleMenu);

@interface NSObject (HasSelection)
- (BOOL)hasSelection;
- (scope::context_t const&)scopeContext;
@end

@interface BundleMenuDelegate ()
@property (nonatomic, retain) NSMutableArray* subdelegates;
@end

@implementation BundleMenuDelegate
{
	bundles::item_ptr umbrellaItem;
}

- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem
{
	if(self = [super init])
	{
		umbrellaItem = aBundleItem;
		self.subdelegates = [NSMutableArray new];
	}
	return self;
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_BundleMenu, bug("\n"););
	[aMenu removeAllItems];
	[self.subdelegates removeAllObjects];

	BOOL hasSelection = NO;
	if(id textView = [NSApp targetForAction:@selector(hasSelection)])
		hasSelection = [textView hasSelection];

	scope::context_t scope = "";
	if(id textView = [NSApp targetForAction:@selector(scopeContext)])
		scope = [textView scopeContext];

	citerate(item, umbrellaItem->menu())
	{
		switch((*item)->kind())
		{
			case bundles::kItemTypeMenu:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:(*item)->name()] action:NULL keyEquivalent:@""];

				menuItem.submenu = [NSMenu new];
				BundleMenuDelegate* delegate = [[BundleMenuDelegate alloc] initWithBundleItem:*item];
				menuItem.submenu.delegate = delegate;
				[self.subdelegates addObject:delegate];
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
					[menuItem setKeyEquivalentCxxString:key_equivalent(*item)];
					[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				}
			}
			break;

			default:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:name_with_selection(*item, hasSelection)] action:@selector(performBundleItemWithUUIDStringFrom:) keyEquivalent:@""];
				[menuItem setKeyEquivalentCxxString:key_equivalent(*item)];
				[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:(*item)->uuid()]];
			}
			break;
		}
	}
}
@end
