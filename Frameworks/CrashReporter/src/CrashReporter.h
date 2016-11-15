#include <oak/misc.h>

PUBLIC @interface CrashReporter : NSObject
+ (instancetype)sharedInstance;
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification;
- (void)postNewCrashReportsToURLString:(NSString*)aURL;
@end
