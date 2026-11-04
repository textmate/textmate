#import "AgentStatusTabSource.h"
#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>
#import <OakTabBarView/OakTabBarView.h>
#import "../src/TerminalPanelViewController.h"
#import "../src/DocumentWindowController.h"
#import "../src/ProjectLayoutView.h"
#import <FileBrowser/FileBrowserViewController.h>

// A plain sh without rc files, so tests do not depend on the user's login shell or dotfiles.
static TerminalPanelViewController* MakePanel (NSView* content)
{
	TerminalPanelViewController* res = [[TerminalPanelViewController alloc] initWithContentView:content];
	res.terminalShellPath      = @"/bin/sh";
	res.terminalShellArguments = @[ ];
	return res;
}

static NSWindow* MakeHostWindow (TerminalPanelViewController* controller)
{
	NSWindow* res = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1000, 700) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskResizable backing:NSBackingStoreBuffered defer:NO];
	res.releasedWhenClosed = NO;
	NSView* view = controller.view;
	view.translatesAutoresizingMaskIntoConstraints = NO;
	[res.contentView addSubview:view];
	[NSLayoutConstraint activateConstraints:@[
		[view.leftAnchor constraintEqualToAnchor:res.contentView.leftAnchor], [view.rightAnchor constraintEqualToAnchor:res.contentView.rightAnchor],
		[view.topAnchor constraintEqualToAnchor:res.contentView.topAnchor], [view.bottomAnchor constraintEqualToAnchor:res.contentView.bottomAnchor]
	]];
	return res;
}

static void LayOut (NSWindow* window, NSSize size)
{
	[window setContentSize:size];
	[window.contentView layoutSubtreeIfNeeded];
}

void test_terminal_panel_open_project ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			ProjectLayoutView* content = [[ProjectLayoutView alloc] initWithFrame:NSZeroRect];
			content.documentView = [[NSView alloc] initWithFrame:NSZeroRect];
			[content.documentView.heightAnchor constraintGreaterThanOrEqualToConstant:25].active = YES;
			TerminalPanelViewController* controller = MakePanel(content);
			NSView* split = controller.view;
			NSWindow* window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1000, 700) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskResizable backing:NSBackingStoreBuffered defer:NO];
			window.releasedWhenClosed = NO;
			split.translatesAutoresizingMaskIntoConstraints = NO;
			[window.contentView addSubview:split];
			[NSLayoutConstraint activateConstraints:@[
				[split.leftAnchor constraintEqualToAnchor:window.contentView.leftAnchor],
				[split.rightAnchor constraintEqualToAnchor:window.contentView.rightAnchor],
				[split.topAnchor constraintEqualToAnchor:window.contentView.topAnchor],
				[split.bottomAnchor constraintEqualToAnchor:window.contentView.bottomAnchor],
			]];
			// Project restoration adds the browser before the window's first layout pass.
			[window.contentView updateConstraintsForSubtreeIfNeeded];
			controller.panelHeight = 220;
			FileBrowserViewController* browser = [[FileBrowserViewController alloc] init];
			CGFloat minimumBrowserHeight = browser.view.fittingSize.height;
			content.fileBrowserView = browser.view;
			[window.contentView updateConstraintsForSubtreeIfNeeded];
			[content layoutSubtreeIfNeeded];
			OAK_ASSERT(NSHeight(browser.view.frame) >= minimumBrowserHeight);
			[window setContentSize:NSMakeSize(1100, 700)];
			[window.contentView layoutSubtreeIfNeeded];
			OAK_ASSERT(NSHeight(browser.view.frame) >= minimumBrowserHeight);
			OAK_ASSERT(NSHeight(content.frame) == NSHeight(split.bounds));
			[window close];
		}
	});
}

void test_terminal_panel_layout ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			NSView* content = [[NSView alloc] initWithFrame:NSZeroRect];
			TerminalPanelViewController* controller = MakePanel(content);
			// Exercise a fixed layout regardless of the default placement or saved sizes.
			controller.panelPosition = @"right";
			controller.panelWidth = 320;
			controller.panelHeight = 220;
			NSWindow* window = MakeHostWindow(controller);
			NSView* (^panel)() = ^{ return (NSView*)[controller valueForKey:@"panelView"]; };

			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(!panel().superview && NSWidth(content.frame) == 1000);
			controller.panelVisible = YES;
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(NSWidth(panel().frame) == 320 && NSMinX(panel().frame) > NSMaxX(content.frame));
			controller.panelPosition = @"left";
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(NSMinX(content.frame) > NSMaxX(panel().frame) && NSWidth(panel().frame) == 320);
			controller.panelPosition = @"bottom";
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(NSWidth(content.frame) == 1000 && NSHeight(panel().frame) == 220);
			OAK_ASSERT(NSMinY(content.frame) > NSMaxY(panel().frame)); // not flipped: the panel is below
			controller.panelHeight = 260;
			controller.panelPosition = @"right";
			controller.panelWidth = 380;
			LayOut(window, NSMakeSize(400, 300));
			OAK_ASSERT(NSWidth(content.frame) >= 240);
			OAK_ASSERT(controller.panelWidth == 380);
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(NSWidth(panel().frame) == 380);
			controller.panelVisible = NO;
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(!panel().superview && NSWidth(content.frame) == 1000);
			controller.panelVisible = YES;
			controller.panelPosition = @"bottom";
			LayOut(window, NSMakeSize(1000, 700));
			OAK_ASSERT(NSHeight(panel().frame) == 260);
			controller.panelPosition = @"invalid";
			OAK_ASSERT([controller.panelPosition isEqualToString:@"bottom"]);
		}
	});
}

// Content constraints can change before the window's first layout pass (a project window enables its file browser on creation); the content must not be pinned to a frame then.
void test_terminal_panel_content_is_not_frame_pinned ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			NSView* content = [[NSView alloc] initWithFrame:NSZeroRect];
			TerminalPanelViewController* controller = MakePanel(content);
			NSWindow* window = MakeHostWindow(controller);
			NSView* child = [[NSView alloc] initWithFrame:NSZeroRect];
			child.translatesAutoresizingMaskIntoConstraints = NO;
			[content addSubview:child];
			[NSLayoutConstraint activateConstraints:@[
				[child.heightAnchor constraintGreaterThanOrEqualToConstant:400],
				[child.topAnchor constraintEqualToAnchor:content.topAnchor], [child.bottomAnchor constraintEqualToAnchor:content.bottomAnchor],
				[child.leftAnchor constraintEqualToAnchor:content.leftAnchor], [child.rightAnchor constraintEqualToAnchor:content.rightAnchor]
			]];
			for(NSLayoutConstraintOrientation orientation : { NSLayoutConstraintOrientationHorizontal, NSLayoutConstraintOrientationVertical })
			{
				for(NSLayoutConstraint* constraint in [content constraintsAffectingLayoutForOrientation:orientation])
					OAK_ASSERT(![constraint isKindOfClass:NSClassFromString(@"NSAutoresizingMaskLayoutConstraint")] || (constraint.firstItem != content && constraint.secondItem != content));
			}
			[window setFrame:NSMakeRect(0, 0, 900, 600) display:NO];
			[window.contentView layoutSubtreeIfNeeded];
			OAK_ASSERT(NSHeight(content.frame) >= 400);
		}
	});
}

void test_terminal_panel_constrained_resize ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			NSView* content = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 1000, 700)];
			NSView* child = [[NSView alloc] initWithFrame:NSZeroRect];
			child.translatesAutoresizingMaskIntoConstraints = NO;
			[content addSubview:child];
			[NSLayoutConstraint activateConstraints:@[
				[child.widthAnchor constraintGreaterThanOrEqualToConstant:460], [child.heightAnchor constraintGreaterThanOrEqualToConstant:180],
				[child.leftAnchor constraintEqualToAnchor:content.leftAnchor], [child.rightAnchor constraintEqualToAnchor:content.rightAnchor],
				[child.topAnchor constraintEqualToAnchor:content.topAnchor], [child.bottomAnchor constraintEqualToAnchor:content.bottomAnchor]
			]];
			TerminalPanelViewController* c = MakePanel(content);
			NSView* layout = c.view;
			layout.frame = NSMakeRect(0, 0, 1000, 700);
			NSWindow* window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1000, 700) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskResizable backing:NSBackingStoreBuffered defer:NO];
			window.releasedWhenClosed = NO;
			layout.translatesAutoresizingMaskIntoConstraints = NO;
			[window.contentView addSubview:layout];
			[NSLayoutConstraint activateConstraints:@[
				[layout.leftAnchor constraintEqualToAnchor:window.contentView.leftAnchor], [layout.rightAnchor constraintEqualToAnchor:window.contentView.rightAnchor],
				[layout.topAnchor constraintEqualToAnchor:window.contentView.topAnchor], [layout.bottomAnchor constraintEqualToAnchor:window.contentView.bottomAnchor]
			]];
			c.panelVisible = YES;
			c.terminalDirectoryProvider = ^{ return NSTemporaryDirectory(); };
			[c startTerminal];
			if(!c.hasTerminal) // built without the Swift terminal component
				return;

			OakTabBarView* bar = [[c valueForKey:@"terminalTabBarController"] valueForKey:@"tabBar"];
			OAK_ASSERT([bar isKindOfClass:OakTabBarView.class]);
			OAK_ASSERT([bar.dataSource numberOfRowsInTabBarView:bar] == 1);
			NSUUID* firstSession = [bar.dataSource tabBarView:bar UUIDForIndex:0];
			[bar.delegate tabBarViewDidDoubleClick:bar];
			OAK_ASSERT([bar.dataSource numberOfRowsInTabBarView:bar] == 2);
			OAK_ASSERT([bar.delegate performDropOfTabItem:firstSession fromTabBar:bar index:0 toTabBar:bar index:2 operation:NSDragOperationMove]);
			OAK_ASSERT([[bar.dataSource tabBarView:bar UUIDForIndex:1] isEqual:firstSession]);

			// The content's minimum size must hold at every window width, whichever edge the panel is on.
			for(NSString* position in @[ @"right", @"left", @"bottom" ])
			{
				c.panelPosition = position;
				c.panelVisible = NO;
				c.panelVisible = YES;
				for(int width = 1000; width >= 470; width -= 10)
				{
					[window setContentSize:NSMakeSize(width, 300)];
					[window.contentView layoutSubtreeIfNeeded];
					[layout layoutSubtreeIfNeeded];
					OAK_ASSERT(NSWidth(content.frame) >= 460);
					OAK_ASSERT(NSHeight(content.frame) >= 180);
				}
			}
			[c stopTerminal];
		}
	});
}

void test_terminal_panel_placement_and_last_session ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			TerminalPanelViewController* source   = MakePanel([[NSView alloc] initWithFrame:NSZeroRect]);
			TerminalPanelViewController* detached = MakePanel([[NSView alloc] initWithFrame:NSZeroRect]);
			source.panelPosition = @"left";
			source.panelWidth = 410;
			source.panelHeight = 190;
			[detached takePlacementFromPanel:source];
			OAK_ASSERT([detached.panelPosition isEqualToString:@"left"]);
			OAK_ASSERT(detached.panelWidth == 410 && detached.panelHeight == 190);

			__block int sourceClosed = 0, detachedClosed = 0;
			source.lastSessionClosedHandler   = ^{ ++sourceClosed; };
			detached.lastSessionClosedHandler = ^{ ++detachedClosed; };
			source.terminalDirectoryProvider = ^{ return NSTemporaryDirectory(); };
			[source startTerminal];
			if(!source.hasTerminal) // built without the Swift terminal component
				return;

			NSArray<NSUUID*>* sessions = source.terminalSessionIdentifiers;
			OAK_ASSERT(sessions.count == 1);

			// Moving the only session out must not look like the user closing it.
			[detached takeTerminalSession:sessions.firstObject fromPanel:source];
			OAK_ASSERT(source.terminalSessionIdentifiers.count == 0);
			OAK_ASSERT(sourceClosed == 0);

			[detached closeTerminalSessions:sessions];
			OAK_ASSERT(detached.terminalSessionIdentifiers.count == 0);
			OAK_ASSERT(detachedClosed == 1);

			[source stopTerminal];
			[detached stopTerminal];
		}
	});
}

#if HAVE_OAK_SWIFTUI
static OakTabBarView* FindTabBarView (NSView* view)
{
	if([view isKindOfClass:OakTabBarView.class])
		return (OakTabBarView*)view;
	for(NSView* child in view.subviews)
	{
		if(OakTabBarView* res = FindTabBarView(child))
			return res;
	}
	return nil;
}

// The panel is collapsed by default, so this also covers the state the user cannot see: the window
// subtitle is the only indicator when the terminal panel is hidden or the window is in the background.
void test_terminal_attention_drives_window_subtitle ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			[DocumentWindowController disableSessionSave];
			DocumentWindowController* controller = [[DocumentWindowController alloc] init];
			TerminalPanelViewController* panel = [controller valueForKey:@"terminalPanelController"];
			panel.terminalShellPath      = @"/bin/sh";
			panel.terminalShellArguments = @[ ];
			[panel startTerminal];
			if(panel.hasTerminal) // built without the Swift terminal component, or no shell could start
			{
				NSWindow* window = controller.window;
				OAK_ASSERT(window.subtitle.length == 0);

				[panel simulateSessionAttentionAtIndex:0 state:@"done" kind:nil];
				OAK_ASSERT([window.subtitle isEqualToString:@"terminal finished"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"blocked" kind:@"permission"];
				OAK_ASSERT([window.subtitle isEqualToString:@"terminal needs permission"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"blocked" kind:@"question"];
				OAK_ASSERT([window.subtitle isEqualToString:@"terminal needs your input"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"blocked" kind:nil];
				OAK_ASSERT([window.subtitle isEqualToString:@"terminal needs your input"]);

				// The same fact as a tab tooltip. This reads the data source the tab bar actually installed,
				// so it proves the wiring up to `OakTabItem`; the `NSToolTip` that OakTabView derives from it
				// lives in private internals and is not covered by this assertion.
				// The panel has to be visible for its subviews to be in the layout view's hierarchy.
				panel.panelVisible = YES;
				OakTabBarView* tabBar = FindTabBarView(panel.view);
				OAK_ASSERT(tabBar != nil);
				id <OakTabBarViewDataSource> dataSource = tabBar.dataSource;
				OAK_ASSERT([dataSource respondsToSelector:@selector(tabBarView:toolTipForIndex:)]);
				OAK_ASSERT([[dataSource tabBarView:tabBar toolTipForIndex:0] isEqualToString:@"Terminal: Waiting for input"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"done" kind:nil];
				OAK_ASSERT([[dataSource tabBarView:tabBar toolTipForIndex:0] isEqualToString:@"Terminal: Finished"]);
				OAK_ASSERT([window.subtitle isEqualToString:@"terminal finished"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"blocked" kind:@"permission"];
				OAK_ASSERT([[dataSource tabBarView:tabBar toolTipForIndex:0] isEqualToString:@"Terminal: Needs permission"]);

				[panel simulateSessionAttentionAtIndex:0 state:@"blocked" kind:@"auth"];
				OAK_ASSERT([[dataSource tabBarView:tabBar toolTipForIndex:0] isEqualToString:@"Terminal: Waiting for input"]);

				// No dot, no tooltip: an empty string falls back to the tab's own (also empty) path tooltip.
				[panel simulateSessionAttentionAtIndex:0 state:nil kind:nil];
				OAK_ASSERT([[dataSource tabBarView:tabBar toolTipForIndex:0] isEqualToString:@""]);

				// Closing the only session removes the fact the subtitle was reporting.
				[panel closeTerminalSessions:panel.terminalSessionIdentifiers];
				OAK_ASSERT(window.subtitle.length == 0);

				[panel stopTerminal];
			}
			[DocumentWindowController enableSessionSave];
		}
	});
}
#endif

// `updateStatus` starts the spinner for OakTabStatusWorking and stops it otherwise (OakTabBarView.mm:520-521),
// so counting calls to those public methods observes the contract directly instead of the animation state.
static NSCountedSet* StartedSpinners = nil;
static NSCountedSet* StoppedSpinners = nil;
static IMP OriginalStartAnimation = NULL;
static IMP OriginalStopAnimation  = NULL;

static void CountStartAnimation (id self, SEL _cmd, id sender)
{
	@synchronized(StartedSpinners) { [StartedSpinners addObject:self]; }
	((void(*)(id, SEL, id))OriginalStartAnimation)(self, _cmd, sender);
}

static void CountStopAnimation (id self, SEL _cmd, id sender)
{
	@synchronized(StoppedSpinners) { [StoppedSpinners addObject:self]; }
	((void(*)(id, SEL, id))OriginalStopAnimation)(self, _cmd, sender);
}

static void InstallAnimationCounters ()
{
	StartedSpinners = [NSCountedSet new];
	StoppedSpinners = [NSCountedSet new];
	Method start = class_getInstanceMethod(NSProgressIndicator.class, @selector(startAnimation:));
	Method stop  = class_getInstanceMethod(NSProgressIndicator.class, @selector(stopAnimation:));
	OAK_ASSERT(start != NULL && stop != NULL);
	OriginalStartAnimation = method_setImplementation(start, (IMP)CountStartAnimation);
	OriginalStopAnimation  = method_setImplementation(stop,  (IMP)CountStopAnimation);
}

static void RemoveAnimationCounters ()
{
	method_setImplementation(class_getInstanceMethod(NSProgressIndicator.class, @selector(startAnimation:)), OriginalStartAnimation);
	method_setImplementation(class_getInstanceMethod(NSProgressIndicator.class, @selector(stopAnimation:)),  OriginalStopAnimation);
}

static NSUInteger StartCalls (NSProgressIndicator* spinner)
{
	@synchronized(StartedSpinners) { return [StartedSpinners countForObject:spinner]; }
}

static NSUInteger StopCalls (NSProgressIndicator* spinner)
{
	@synchronized(StoppedSpinners) { return [StoppedSpinners countForObject:spinner]; }
}

void test_terminal_tab_status_presentation ()
{
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        AgentStatusTabSource* source = [AgentStatusTabSource new];
        source.identifier = NSUUID.UUID;
        source.status = OakTabStatusWorking;
        source.statusDescription = @"Working";
        OakTabBarView* bar = [[OakTabBarView alloc] initWithFrame:NSMakeRect(0, 0, 400, 23)];
        bar.dataSource = source;
        InstallAnimationCounters();
        [bar reloadData];
        [bar layoutSubtreeIfNeeded];
        NSView* tab = nil;
        for(NSView* view in bar.subviews)
            if([view.accessibilityLabel isEqualToString:@"Agent, Working"]) tab = view;
        OAK_ASSERT(tab != nil);
        OAK_ASSERT([tab.toolTip isEqualToString:@"Working"]);
        // The spinner is a direct subview of the tab (OakTabBarView.mm:293).
        NSProgressIndicator* spinner = nil;
        for(NSView* view in tab.subviews)
            if([view isKindOfClass:NSProgressIndicator.class]) spinner = (NSProgressIndicator*)view;
        OAK_ASSERT(spinner != nil);
        NSUInteger workingStarts = StartCalls(spinner);
        OAK_ASSERT(workingStarts > 0);
        source.status = OakTabStatusBlocked;
        source.statusDescription = @"Needs approval";
        [bar reloadData];
        OAK_ASSERT([tab.accessibilityLabel isEqualToString:@"Agent, Needs approval"]);
        OAK_ASSERT([tab.toolTip isEqualToString:@"Needs approval"]);
        OAK_ASSERT(StopCalls(spinner) > 0);
        OAK_ASSERT(StartCalls(spinner) == workingStarts);
        NSUInteger stoppedCalls = StopCalls(spinner);
        source.status = OakTabStatusNone;
        source.statusDescription = @"";
        [bar reloadData];
        [bar layoutSubtreeIfNeeded];
        OAK_ASSERT([tab.accessibilityLabel isEqualToString:@"Agent"]);
        OAK_ASSERT(StopCalls(spinner) > stoppedCalls);
        RemoveAnimationCounters();
    });
}
