#import "Private.h"
#import <OakAppKit/src/NSMenuItem Additions.h>
#import <OakFoundation/src/NSString Additions.h>
#import <ns/src/ns.h>
#import <text/src/parse.h>
#import <oak/debug.h>

@interface NSObject (HasSelection)
- (BOOL)hasSelection;
- (scope::context_t)scopeContext;
@end

// Field: showOnlyIfExecutableMissing = "ruff pyright" — the menu item is only
// shown while at least one of the (space-separated) executables cannot be
// found in PATH or the usual install locations.
static bool AnyExecutableMissing (std::string const& names)
{
	std::vector<std::string> dirs;
	if(char const* path = getenv("PATH"))
	{
		for(auto const& dir : text::split(path, ":"))
			dirs.push_back(dir);
	}
	for(NSString* extra in @[ @"/opt/homebrew/bin", @"/usr/local/bin", [@"~/.local/bin" stringByExpandingTildeInPath] ])
		dirs.push_back(to_s(extra));

	for(auto const& name : text::split(names, " "))
	{
		if(name.empty())
			continue;

		bool found = false;
		for(auto const& dir : dirs)
		{
			if(!dir.empty() && access((dir + "/" + name).c_str(), X_OK) == 0)
			{
				found = true;
				break;
			}
		}
		if(!found)
			return true;
	}
	return false;
}

@implementation BundleMenuDelegate
+ (instancetype)sharedInstance
{
	static BundleMenuDelegate* sharedInstance = [self new];
	return sharedInstance;
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[aMenu removeAllItems];

	scope::context_t scope = "";
	if(id textView = [NSApp targetForAction:@selector(scopeContext)])
		scope = [textView scopeContext];

	bundles::item_ptr umbrellaItem = bundles::lookup(to_s(aMenu.title));
	if(!umbrellaItem)
		return;

	for(auto const& item : umbrellaItem->menu())
	{
		switch(item->kind())
		{
			case bundles::kItemTypeMenu:
			{
				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:item->name()] action:NULL keyEquivalent:@""];

				menuItem.submenu = [[NSMenu alloc] initWithTitle:[NSString stringWithCxxString:item->uuid()]];
				menuItem.submenu.delegate = BundleMenuDelegate.sharedInstance;
			}
			break;

			case bundles::kItemTypeMenuItemSeparator:
				[aMenu addItem:[NSMenuItem separatorItem]];
			break;

			case bundles::kItemTypeProxy:
			{
				auto const items = bundles::items_for_proxy(item, scope);
				OakAddBundlesToMenu(items, true, aMenu, @selector(performBundleItemWithUUIDStringFrom:));

				if(items.empty())
				{
					NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:item->name()] action:@selector(nop:) keyEquivalent:@""];
					[menuItem setInactiveKeyEquivalentCxxString:key_equivalent(item)];
					[menuItem setTabTriggerCxxString:item->value_for_field(bundles::kFieldTabTrigger)];
				}
			}
			break;

			default:
			{
				std::string const& requiredMissing = item->value_for_field(bundles::kFieldShowIfExecutableMissing);
				if(requiredMissing != NULL_STR && !AnyExecutableMissing(requiredMissing))
					break;

				NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:item->name()] action:@selector(performBundleItemWithUUIDStringFrom:) keyEquivalent:@""];
				[menuItem setInactiveKeyEquivalentCxxString:key_equivalent(item)];
				[menuItem setTabTriggerCxxString:item->value_for_field(bundles::kFieldTabTrigger)];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:item->uuid()]];

				// Bind a checkmark to an NSUserDefaults bool key; “!key” inverts,
				// so an unset key can represent the enabled/checked state.
				std::string stateKey = item->value_for_field(bundles::kFieldUserDefaultsState);
				if(stateKey != NULL_STR && !stateKey.empty())
				{
					bool const invert = stateKey.front() == '!';
					if(invert)
						stateKey.erase(0, 1);
					BOOL on = [NSUserDefaults.standardUserDefaults boolForKey:[NSString stringWithCxxString:stateKey]];
					menuItem.state = (invert ? !on : on) ? NSControlStateValueOn : NSControlStateValueOff;
				}
			}
			break;
		}
	}
}
@end
