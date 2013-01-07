#import <HTMLOutput/HTMLOutput.h>
#import <oak/misc.h>

PUBLIC @interface HTMLOutputWindowController : NSObject <NSWindowDelegate>
@property (nonatomic) NSWindow* window;
+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner;
- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner;
- (BOOL)running;
@end
