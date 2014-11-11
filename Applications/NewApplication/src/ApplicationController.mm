#import "ApplicationController.h"

@interface ApplicationController () <NSApplicationDelegate, NSWindowDelegate>
@property (nonatomic) NSWindow* window;
@end

@implementation ApplicationController
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	_window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400) styleMask:(NSTitledWindowMask|NSResizableWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO];
	_window.releasedWhenClosed = NO;
	_window.delegate           = self;
	_window.title              = @"New Application";
	[_window center];
	[_window makeKeyAndOrderFront:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[NSApp terminate:self];
}
@end
