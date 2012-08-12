#import "AppController.h"
#import <oak/CocoaSTL.h>
#import <oak/oak.h>
#import <text/ctype.h>
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
#import <BundleMenu/BundleMenuDelegate.h>

OAK_DEBUG_VAR(AppController_Menus);

@interface NSObject (BundleMenuDelegate)
- (BOOL)canHandleMenuKeyEquivalent:(NSEvent*)anEvent;
- (void)handleMenuKeyEquivalent:(id)sender;
@end

@implementation AppController (BundlesMenu)
- (void)doBundleItem:(id)anArgument
{
	[NSApp sendAction:@selector(performBundleItemWithUUIDString:) to:nil from:[anArgument representedObject]];
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	D(DBF_AppController_Menus, bug("%s (%s)\n", ns::glyphs_for_event_string(to_s(theEvent)).c_str(), to_s(theEvent).c_str()););
	if(aMenu != bundlesMenu)
		return NO;

	*anAction = @selector(handleMenuKeyEquivalent:);
	*aTarget = self;

	if(id target = [NSApp targetForAction:@selector(canHandleMenuKeyEquivalent:)])
	{
		*aTarget = target;
		return [target canHandleMenuKeyEquivalent:theEvent];
	}
	return NO;
}

- (void)bundlesMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	for(int i = aMenu.numberOfItems; i--; )
	{
		if([[aMenu itemAtIndex:i] isSeparatorItem])
			break;

		NSMenuItem* item = [aMenu itemAtIndex:i];
		if([[[item submenu] delegate] isKindOfClass:[BundleMenuDelegate class]])
		{
			[[[item submenu] delegate] release];
			[[item submenu] setDelegate:nil];
		}
		[aMenu removeItemAtIndex:i];
	}

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.insert(std::make_pair((*item)->name(), *item));

	iterate(pair, ordered)
	{
		if(pair->second->menu().empty())
			continue;

		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:NULL keyEquivalent:@""];
		menuItem.submenu = [NSMenu new];
		menuItem.submenu.autoenablesItems = NO;
		BundleMenuDelegate* delegate = [[BundleMenuDelegate alloc] initWithBundleItem:pair->second];
		menuItem.submenu.delegate = delegate;
	}

	if(ordered.empty())
		[aMenu addItemWithTitle:@"No Bundles Loaded" action:@selector(nop:) keyEquivalent:@""];
}

- (void)themesMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	[aMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme))
		ordered.insert(std::make_pair((*item)->name(), *item));

	iterate(pair, ordered)
	{
		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(takeThemeUUIDFrom:) keyEquivalent:@""];
		[menuItem setKeyEquivalentCxxString:pair->second->value_for_field(bundles::kFieldKeyEquivalent)];
		[menuItem setRepresentedObject:[NSString stringWithCxxString:pair->second->uuid()]];
	}

	if(ordered.empty())
		[aMenu addItemWithTitle:@"No Themes Loaded" action:@selector(nop:) keyEquivalent:@""];
}

- (void)spellingMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););

	for(int i = aMenu.numberOfItems; i--; )
	{
		NSMenuItem* item = [aMenu itemAtIndex:i];
		if([item action] == @selector(takeSpellingLanguageFrom:))
		{
			[[item retain] autorelease];
			[aMenu removeItemAtIndex:i];
		}
	}

	std::multimap<std::string, NSString*, text::less_t> ordered;

	NSSpellChecker* spellChecker = [NSSpellChecker sharedSpellChecker];
	for(NSString* lang in [spellChecker availableLanguages])
	{
		D(DBF_AppController_Menus, bug("%s\n", [lang UTF8String]););
		CFStringRef str = CFLocaleCopyDisplayNameForPropertyValue(CFLocaleGetSystem(), kCFLocaleIdentifier, (CFStringRef)lang);
		D(DBF_AppController_Menus, bug("→ %s\n", cf::to_s(str ?: (CFStringRef)lang).c_str()););
		ordered.insert(std::make_pair(cf::to_s(str ?: (CFStringRef)lang), lang));
		if(str)
			CFRelease(str);
	}

	iterate(it, ordered)
	{
		D(DBF_AppController_Menus, bug("Add Item: %s\n", it->first.c_str()););
		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:it->first] action:@selector(takeSpellingLanguageFrom:) keyEquivalent:@""];
		D(DBF_AppController_Menus, bug("Represented Object: %s\n", [it->second UTF8String]););
		menuItem.representedObject = it->second;
	}
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	if(aMenu == bundlesMenu)
		[self bundlesMenuNeedsUpdate:aMenu];
	else if(aMenu == themesMenu)
		[self themesMenuNeedsUpdate:aMenu];
	else if(aMenu == spellingMenu)
		[self spellingMenuNeedsUpdate:aMenu];
}
@end
