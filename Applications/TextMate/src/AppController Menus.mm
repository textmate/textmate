#import "AppController.h"
#import <oak/oak.h>
#import <text/ctype.h>
#import <bundles/bundles.h>
#import <command/parser.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakTextView.h>
#import <oak/debug.h>
#import <BundleMenu/BundleMenu.h>

OAK_DEBUG_VAR(AppController_Menus);

@implementation AppController (BundlesMenu)
- (void)performBundleItemWithUUIDStringFrom:(id)anArgument
{
	[NSApp sendAction:@selector(performBundleItemWithUUIDString:) to:nil from:[anArgument representedObject]];
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	D(DBF_AppController_Menus, bug("%s (%s)\n", ns::glyphs_for_event_string(to_s(theEvent)).c_str(), to_s(theEvent).c_str()););
	return NO;
}

- (void)bundlesMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	for(int i = aMenu.numberOfItems; i--; )
	{
		if([[aMenu itemAtIndex:i] isSeparatorItem])
			break;
		[aMenu removeItemAtIndex:i];
	}

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.emplace(item->name(), item);

	for(auto const& pair : ordered)
	{
		if(pair.second->menu().empty())
			continue;

		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:NULL keyEquivalent:@""];
		menuItem.submenu = [[NSMenu alloc] initWithTitle:[NSString stringWithCxxString:pair.second->uuid()]];
		menuItem.submenu.delegate = [BundleMenuDelegate sharedInstance];
	}

	if(ordered.empty())
		[aMenu addItemWithTitle:@"No Bundles Loaded" action:@selector(nop:) keyEquivalent:@""];
}

- (void)themesMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	[aMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme))
		ordered.emplace(item->name(), item);

	for(auto const& pair : ordered)
	{
		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(takeThemeUUIDFrom:) keyEquivalent:@""];
		[menuItem setKeyEquivalentCxxString:key_equivalent(pair.second)];
		[menuItem setRepresentedObject:[NSString stringWithCxxString:pair.second->uuid()]];
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
			[aMenu removeItemAtIndex:i];
	}

	std::multimap<std::string, NSString*, text::less_t> ordered;

	NSSpellChecker* spellChecker = [NSSpellChecker sharedSpellChecker];
	for(NSString* lang in [spellChecker availableLanguages])
	{
		D(DBF_AppController_Menus, bug("%s\n", [lang UTF8String]););
		NSString* str = (NSString*)CFBridgingRelease(CFLocaleCopyDisplayNameForPropertyValue(CFLocaleGetSystem(), kCFLocaleIdentifier, (__bridge CFStringRef)lang));
		D(DBF_AppController_Menus, bug("→ %s\n", [(str ?: lang) UTF8String]););
		ordered.emplace(to_s(str ?: lang), lang);
	}

	NSString* systemSpellingLanguage;
	if ([spellChecker automaticallyIdentifiesLanguages])
		systemSpellingLanguage = @"Automatic by Language";
	else
	{
		systemSpellingLanguage = (NSString*)CFBridgingRelease(CFLocaleCopyDisplayNameForPropertyValue(CFLocaleGetSystem(), kCFLocaleIdentifier, (__bridge CFStringRef)[spellChecker language]));
	}
	NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithFormat:@"System (%@)", systemSpellingLanguage] action:@selector(takeSpellingLanguageFrom:) keyEquivalent:@""];
	D(DBF_AppController_Menus, bug("Represented Object: "" (System)\n"););
	menuItem.representedObject = @"";

	for(auto const& it : ordered)
	{
		D(DBF_AppController_Menus, bug("Add Item: %s\n", it.first.c_str()););
		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:it.first] action:@selector(takeSpellingLanguageFrom:) keyEquivalent:@""];
		D(DBF_AppController_Menus, bug("Represented Object: %s\n", [it.second UTF8String]););
		menuItem.representedObject = it.second;
	}
}

- (void)wrapColumnMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	[aMenu removeAllItems];

	SEL action = @selector(takeWrapColumnFrom:);
	NSMenuItem* menuItem;

	menuItem = [aMenu addItemWithTitle:@"Use Window Frame" action:action keyEquivalent:@""];
	menuItem.tag = NSWrapColumnWindowWidth;
	[aMenu addItem:[NSMenuItem separatorItem]];

	NSArray* presets = [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsWrapColumnPresetsKey];
	for(NSNumber* preset in [presets sortedArrayUsingSelector:@selector(compare:)])
	{
		menuItem = [aMenu addItemWithTitle:[NSString stringWithFormat:@"%@", preset] action:action keyEquivalent:@""];
		menuItem.tag = [preset integerValue];
	}

	[aMenu addItem:[NSMenuItem separatorItem]];
	menuItem = [aMenu addItemWithTitle:@"Other…" action:action keyEquivalent:@""];
	menuItem.tag = NSWrapColumnAskUser;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	if(aMenu == bundlesMenu)
		[self bundlesMenuNeedsUpdate:aMenu];
	else if(aMenu == themesMenu)
		[self themesMenuNeedsUpdate:aMenu];
	else if(aMenu == spellingMenu)
		[self spellingMenuNeedsUpdate:aMenu];
	else if(aMenu == wrapColumnMenu)
		[self wrapColumnMenuNeedsUpdate:aMenu];
}
@end
