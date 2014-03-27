#import <HTMLOutput/HTMLOutput.h>
#import <oak/misc.h>

PUBLIC @interface HTMLOutputWindowController : NSObject <NSWindowDelegate>
@property (nonatomic) NSWindow* window;
@property (nonatomic) command::runner_ptr commandRunner;
@property (nonatomic, readonly) BOOL running;
@property (nonatomic, readonly) BOOL needsNewWebView;
+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner;

- (void)showWindow:(id)sender;
- (void)close;
@end
