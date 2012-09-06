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

@implementation BundleMenuDelegate
- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem
{
	if(self = [super init])
	{
		umbrellaItem = aBundleItem;
		subdelegates = [NSMutableArray new];
	}
	return self;
}

- (void)dealloc
{
	[subdelegates release];
	[super dealloc];
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_BundleMenu, bug("\n"););
	[aMenu removeAllItems];
	[subdelegates removeAllObjects];

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

				menuItem.submenu = [[NSMenu new] autorelease];
				BundleMenuDelegate* delegate = [[[BundleMenuDelegate alloc] initWithBundleItem:*item] autorelease];
				menuItem.submenu.delegate = delegate;
				[subdelegates addObject:delegate];
			}
			break;

			case bundles::kItemTypeMenuItemSeparator:
				[aMenu addItem:[NSMenuItem separatorItem]];
			break;

			case bundles::kItemTypeProxy:
			{
				std::string actionClass;
				if(plist::get_key_path((*item)->plist(), "content", actionClass))
					OakAddBundlesToMenu(bundles::query(bundles::kFieldSemanticClass, actionClass, scope), hasSelection, true, aMenu, @selector(doBundleItem:));
			}
			break;

			default:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:name_with_selection(*item, hasSelection)] action:@selector(doBundleItem:) keyEquivalent:@""];
				[menuItem setKeyEquivalentCxxString:key_equivalent(*item)];
				[menuItem setTabTriggerCxxString:(*item)->value_for_field(bundles::kFieldTabTrigger)];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:(*item)->uuid()]];
			}
			break;
		}
	}
}
@end
