#import "HTMLOutputWindow.h"
#import <OakAppKit/src/OakAppKit.h>
#import <OakFoundation/src/NSString Additions.h>
#import <command/src/runner.h>
#import <ns/src/ns.h>
#import <oak/debug.h>

@interface HTMLOutputWindowController () <NSWindowDelegate>
@property (nonatomic) HTMLOutputWindowController* retainedSelf;
@end

@implementation HTMLOutputWindowController
- (instancetype)init
{
	NSRect rect = [[NSScreen mainScreen] visibleFrame];
	rect = NSIntegralRect(NSInsetRect(rect, NSWidth(rect) / 3, NSHeight(rect) / 5));
	NSWindow* window = [[NSWindow alloc] initWithContentRect:rect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO];

	if(self = [super initWithWindow:window])
	{
		self.window         = window;
		self.htmlOutputView = [[OakHTMLOutputView alloc] init];

		[self.window bind:NSTitleBinding toObject:self.htmlOutputView withKeyPath:@"mainFrameTitle" options:nil];
		[self.window bind:NSDocumentEditedBinding toObject:self.htmlOutputView withKeyPath:@"runningCommand" options:nil];
		[self.window setContentView:self.htmlOutputView];
		[self.window setDelegate:self];
		[self.window setReleasedWhenClosed:NO];
		[self.window setCollectionBehavior:NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary];
	}
	return self;
}

- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier
{
	if(self = [self init])
		self.window.frameAutosaveName = [NSString stringWithFormat:@"HTML output for %@", anIdentifier.UUIDString];
	return self;
}

- (void)showWindow:(id)sender
{
	self.retainedSelf = self;
	[super showWindow:sender];
}

- (void)cancelOperation:(id)sender
{
	[self.window performClose:sender];
}

- (BOOL)windowShouldClose:(id)sender
{
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
	[NSNotificationCenter.defaultCenter removeObserver:self];
	self.window.delegate = nil;
}
@end
