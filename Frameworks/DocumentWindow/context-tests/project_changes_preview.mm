// Developer preview of the real Project Changes UI, with an independent app
// identity and no editor-session restoration or saving.
#import <Cocoa/Cocoa.h>
#import "../src/ProjectChangesWindowController.h"

@interface ProjectChangesPreviewDelegate : NSObject <NSApplicationDelegate>
@end
@implementation ProjectChangesPreviewDelegate
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)application { return YES; }
@end

int main(int argc, char const* argv[])
{
	@autoreleasepool {
		NSApplication* app = [NSApplication sharedApplication];
		[app setActivationPolicy:NSApplicationActivationPolicyRegular];
		ProjectChangesPreviewDelegate* delegate = [[ProjectChangesPreviewDelegate alloc] init];
		app.delegate = delegate;
		NSString* directory = @(__FILE__).stringByDeletingLastPathComponent;
		for(int i = 0; i < 3; ++i) directory = directory.stringByDeletingLastPathComponent;
		if(argc > 1) directory = @(argv[1]);
		NSMenu* menu = [[NSMenu alloc] init];
		NSMenuItem* item = [[NSMenuItem alloc] init];
		NSMenu* applicationMenu = [[NSMenu alloc] init];
		[applicationMenu addItemWithTitle:@"Quit Project Changes Preview" action:@selector(terminate:) keyEquivalent:@"q"];
		item.submenu = applicationMenu;
		[menu addItem:item];
		app.mainMenu = menu;
		ProjectChangesWindowController* controller = [[ProjectChangesWindowController alloc] initWithDirectory:directory buffers:^NSDictionary*{ return @{}; } openFile:^(NSString* path, NSUInteger){
			[NSWorkspace.sharedWorkspace openURL:[NSURL fileURLWithPath:path]];
		}];
		[controller showWindow:nil];
		[app activate];
		[app run];
		(void)delegate;
	}
	return 0;
}
