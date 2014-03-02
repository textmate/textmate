#ifndef COCOA_P0XQO9KO
#define COCOA_P0XQO9KO

#include <text/tokenize.h>
#include <oak/oak.h>

static BOOL IsGUITestsEnabled (std::string const& testName)
{
	std::set<std::string> tests;
	std::string envVar = getenv("GUI_TESTS") ?: "";
	for(auto const& test : text::tokenize(envVar.begin(), envVar.end(), ' '))
		tests.insert(test);

	return tests.find("all") != tests.end() || tests.find(testName) != tests.end();
}

static void OakSetupApplicationWithView (NSResponder* aView, std::string testName = NULL_STR)
{
	if(testName == NULL_STR)
	{
		testName = [[[NSProcessInfo processInfo] processName] UTF8String];
		if(testName.find("cxx_test_") == 0)
			testName = testName.substr(strlen("cxx_test_"));
	}

	if(!IsGUITestsEnabled(testName))
		return;

	[NSApplication sharedApplication];
	[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
	[NSApp activateIgnoringOtherApps:YES];

	NSString* appName = [[NSProcessInfo processInfo] processName];
	appName = [[[appName componentsSeparatedByString:@"_"] componentsJoinedByString:@" "] capitalizedString];

	NSMenu* appMenu = [NSMenu new];
	[appMenu addItem:[[NSMenuItem alloc] initWithTitle:[@"Quit " stringByAppendingString:appName] action:@selector(terminate:) keyEquivalent:@"q"]];

	NSMenuItem* appMenuItem = [NSMenuItem new];
	[appMenuItem setSubmenu:appMenu];

	NSMenu* mainMenu = [NSMenu new];
	[mainMenu addItem:appMenuItem];
	[NSApp setMainMenu:mainMenu];

	NSWindow* window = nil;
	if([aView isKindOfClass:[NSWindow class]])
	{
		window = (NSWindow*)aView;
	}
	else if([aView isKindOfClass:[NSView class]])
	{
		NSView* view = (NSView*)aView;
		NSRect winRect = [NSWindow frameRectForContentRect:NSInsetRect([view bounds], -10, -10) styleMask:NSTitledWindowMask|NSResizableWindowMask];
		window = [[NSWindow alloc] initWithContentRect:winRect styleMask:NSTitledWindowMask|NSResizableWindowMask backing:NSBackingStoreBuffered defer:NO];
		[view setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		[view setFrame:NSInsetRect([[window contentView] bounds], 10, 10)];
		[[window contentView] addSubview:view];
	}

	[window cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
	[window setTitle:appName];
	[window setFrameAutosaveName:@"Main Window"];
	[window makeKeyAndOrderFront:nil];

	[NSApp run];
}

#endif /* end of include guard: COCOA_P0XQO9KO */
