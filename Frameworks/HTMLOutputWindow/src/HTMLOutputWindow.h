#import <HTMLOutput/HTMLOutput.h>
#import <command/runner.h>
#import <oak/debug.h>

PUBLIC @interface HTMLOutputWindowController : NSObject <NSWindowDelegate>
{
	OBJC_WATCH_LEAKS(HTMLOutputWindowController);

	NSWindow* window;
	OakHTMLOutputView* htmlOutputView;
	command::runner_ptr runner;
}
+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner;
@end
