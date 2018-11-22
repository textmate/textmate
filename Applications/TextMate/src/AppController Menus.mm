#import "AppController.h"
#import <oak/oak.h>
#import <text/ctype.h>
#import <text/parse.h>
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

static NSString* NameForLocaleIdentifier (NSString* languageCode)
{
	NSString* localLanguage = nil;
	if(CFLocaleRef locale = CFLocaleCreate(kCFAllocatorDefault, (__bridge CFStringRef)languageCode))
	{
		localLanguage = [(NSString*)CFBridgingRelease(CFLocaleCopyDisplayNameForPropertyValue(locale, kCFLocaleIdentifier, (__bridge CFStringRef)languageCode)) capitalizedString];
		CFRelease(locale);
	}

	NSString* systemLangauge = [(NSString*)CFBridgingRelease(CFLocaleCopyDisplayNameForPropertyValue(CFLocaleGetSystem(), kCFLocaleIdentifier, (__bridge CFStringRef)languageCode)) capitalizedString];
	return localLanguage ?: systemLangauge ?: languageCode;
}

@implementation AppController (BundlesMenu)
- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	D(DBF_AppController_Menus, bug("%s (%s)\n", ns::glyphs_for_event_string(to_s(theEvent)).c_str(), to_s(theEvent).c_str()););
	return NO;
}

- (void)bundlesMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););
	for(NSInteger i = aMenu.numberOfItems; i--; )
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

	std::map<std::string, std::multimap<std::string, bundles::item_ptr, text::less_t>> ordered;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme))
	{
		if(item->hidden_from_user())
			continue;

		auto semanticClass = text::split(item->value_for_field(bundles::kFieldSemanticClass), ".");
		std::string themeClass = semanticClass.size() > 2 && semanticClass.front() == "theme" ? semanticClass[1] : "unspecified";
		ordered[themeClass].emplace(item->name(), item);
	}

	for(auto const& themeClasses : ordered)
	{
		[aMenu addItemWithTitle:[[NSString stringWithCxxString:themeClasses.first] capitalizedString] action:@selector(nop:) keyEquivalent:@""];
		for(auto const& pair : themeClasses.second)
		{
			NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(takeThemeUUIDFrom:) keyEquivalent:@""];
			[menuItem setKeyEquivalentCxxString:key_equivalent(pair.second)];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:pair.second->uuid()]];
			[menuItem setIndentationLevel:1];
		}
	}

	if(ordered.empty())
		[aMenu addItemWithTitle:@"No Themes Loaded" action:@selector(nop:) keyEquivalent:@""];
}

- (void)spellingMenuNeedsUpdate:(NSMenu*)aMenu
{
	D(DBF_AppController_Menus, bug("\n"););

	for(NSInteger i = aMenu.numberOfItems; i--; )
	{
		NSMenuItem* item = [aMenu itemAtIndex:i];
		if([item action] == @selector(takeSpellingLanguageFrom:))
			[aMenu removeItemAtIndex:i];
	}

	std::multimap<std::string, NSString*, text::less_t> ordered;

	NSSpellChecker* spellChecker = [NSSpellChecker sharedSpellChecker];
	for(NSString* lang in [spellChecker availableLanguages])
		ordered.emplace(to_s(NameForLocaleIdentifier(lang)), lang);

	NSString* systemSpellingLanguage = [spellChecker automaticallyIdentifiesLanguages] ? @"Automatic by Language" : NameForLocaleIdentifier([spellChecker language]);
	NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSString stringWithFormat:@"System (%@)", systemSpellingLanguage] action:@selector(takeSpellingLanguageFrom:) keyEquivalent:@""];
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
