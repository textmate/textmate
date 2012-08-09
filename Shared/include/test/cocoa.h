#ifndef COCOA_P0XQO9KO
#define COCOA_P0XQO9KO

#include <text/tokenize.h>
#include <oak/oak.h>

#if !defined(MAC_OS_X_VERSION_10_6) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6)
enum {
	NSApplicationActivationPolicyRegular,
	NSApplicationActivationPolicyAccessory,
	NSApplicationActivationPolicyProhibited
};
typedef NSInteger NSApplicationActivationPolicy;

@interface NSApplication (SnowLeopard)
- (BOOL)setActivationPolicy:(NSApplicationActivationPolicy)activationPolicy;
@end
#endif

static BOOL IsGUITestsEnabled (std::string const& testName)
{
	std::set<std::string> tests;
	std::string envVar = getenv("GUI_TESTS") ?: "";
	citerate(test, text::tokenize(envVar.begin(), envVar.end(), ' '))
		tests.insert(*test);

	return tests.find("all") != tests.end() || tests.find(testName) != tests.end();
}

static void OakSetupApplicationWithView (NSView* aView, std::string testName = NULL_STR)
{
	NSAutoreleasePool* pool = [NSAutoreleasePool new];

	if(testName == NULL_STR)
	{
		testName = [[[NSProcessInfo processInfo] processName] UTF8String];
		if(testName.find("test_") == 0)
			testName = testName.substr(strlen("test_"));
	}

	if(!IsGUITestsEnabled(testName))
		return;

	[NSApplication sharedApplication];
	if([NSApp respondsToSelector:@selector(setActivationPolicy:)])
		[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
	[NSApp activateIgnoringOtherApps:YES];

	NSString* appName = [[NSProcessInfo processInfo] processName];
	appName = [[[appName componentsSeparatedByString:@"_"] componentsJoinedByString:@" "] capitalizedString];

	NSMenu* appMenu = [[NSMenu new] autorelease];
	[appMenu addItem:[[[NSMenuItem alloc] initWithTitle:[@"Quit " stringByAppendingString:appName] action:@selector(terminate:) keyEquivalent:@"q"] autorelease]];

	NSMenuItem* appMenuItem = [[NSMenuItem new] autorelease];
	[appMenuItem setSubmenu:appMenu];

	NSMenu* mainMenu = [[NSMenu new] autorelease];
	[mainMenu addItem:appMenuItem];
	[NSApp setMainMenu:mainMenu];

	NSRect winRect = [NSWindow frameRectForContentRect:NSInsetRect([aView bounds], -10, -10) styleMask:NSTitledWindowMask|NSResizableWindowMask];
	NSWindow* window = [[NSWindow alloc] initWithContentRect:winRect styleMask:NSTitledWindowMask|NSResizableWindowMask backing:NSBackingStoreBuffered defer:NO];
	[aView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
	[aView setFrame:NSInsetRect([[window contentView] bounds], 10, 10)];
	[[window contentView] addSubview:aView];

	[window cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
	[window setTitle:appName];
	[window setFrameAutosaveName:@"Main Window"];
	[window makeKeyAndOrderFront:nil];

	[NSApp run];
	[pool drain];
}

#endif /* end of include guard: COCOA_P0XQO9KO */
