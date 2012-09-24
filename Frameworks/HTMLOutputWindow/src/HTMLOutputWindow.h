#import <HTMLOutput/HTMLOutput.h>
#import <oak/misc.h>

PUBLIC @interface HTMLOutputWindowController : NSObject <NSWindowDelegate>
+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner;
@end
