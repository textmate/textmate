#import "HTMLOutputWindow.h"
#import <OakAppKit/OakAppKit.h>
#import <OakFoundation/NSString Additions.h>
#import <command/runner.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutputWindow);

@interface HTMLOutputWindowController ()
{
	OBJC_WATCH_LEAKS(HTMLOutputWindowController);
}
@property (nonatomic) HTMLOutputWindowController* retainedSelf;
@end

@implementation HTMLOutputWindowController
- (instancetype)init
{
	if(self = [super init])
	{
		NSRect rect = [[NSScreen mainScreen] visibleFrame];
		rect = NSIntegralRect(NSInsetRect(rect, NSWidth(rect) / 3, NSHeight(rect) / 5));

		self.window         = [[NSWindow alloc] initWithContentRect:rect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO];
		self.htmlOutputView = [[OakHTMLOutputView alloc] init];

		[self.window bind:NSTitleBinding toObject:self.htmlOutputView withKeyPath:@"mainFrameTitle" options:nil];
		[self.window bind:NSDocumentEditedBinding toObject:self.htmlOutputView withKeyPath:@"runningCommand" options:nil];
		[self.window setContentView:self.htmlOutputView];
		[self.window setDelegate:self];
		[self.window setReleasedWhenClosed:NO];
		[self.window setCollectionBehavior:NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary];

		// Register to application activation/deactivation notification so we can tweak our collection behavior
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidActivate:) name:NSApplicationDidBecomeActiveNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidDeactivate:) name:NSApplicationDidResignActiveNotification object:nil];
	}
	return self;
}

- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier
{
	if(self = [self init])
		self.window.frameAutosaveName = [NSString stringWithFormat:@"HTML output for %@", anIdentifier.UUIDString];
	return self;
}

- (void)applicationDidActivate:(NSNotification*)notification
{
	// Starting with 10.11 behavior must be changed after current event loop cycle <rdar://23587833>
	dispatch_async(dispatch_get_main_queue(), ^{
		self.window.collectionBehavior |= NSWindowCollectionBehaviorMoveToActiveSpace;
	});
}

- (void)applicationDidDeactivate:(NSNotification*)notification
{
	// Starting with 10.11 behavior must be changed after current event loop cycle <rdar://23587833>
	dispatch_async(dispatch_get_main_queue(), ^{
		self.window.collectionBehavior &= ~NSWindowCollectionBehaviorMoveToActiveSpace;
	});
}

- (void)showWindow:(id)sender
{
	self.retainedSelf = self;
	[self.window makeKeyAndOrderFront:nil];
}

- (void)close
{
	[self.window close];
}

- (BOOL)windowShouldClose:(id)sender
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	if(!_htmlOutputView.isRunningCommand)
		return YES;

	[_htmlOutputView stopLoadingWithUserInteraction:YES completionHandler:^(BOOL didStop){
		if(didStop)
		{
			[self.window orderOut:self];
			[self.window close];
		}
	}];
	return NO;
}

- (void)windowWillClose:(NSNotification*)notification
{
	[self performSelector:@selector(setRetainedSelf:) withObject:nil afterDelay:0];
}

- (void)dealloc
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	self.window.delegate = nil;
}

- (IBAction)toggleHTMLOutput:(id)sender
{
	[self.window performClose:self];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	if([menuItem action] == @selector(toggleHTMLOutput:))
		[menuItem setTitle:@"Hide HTML Output"];
	return YES;
}
@end
