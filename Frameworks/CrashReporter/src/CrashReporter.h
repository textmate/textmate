#include <oak/misc.h>

PUBLIC extern NSString* const kUserDefaultsDisableCrashReportingKey;
PUBLIC extern NSString* const kUserDefaultsCrashReportsContactInfoKey;

PUBLIC @interface CrashReporter : NSObject
+ (CrashReporter*)sharedInstance;
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification;
- (void)postNewCrashReportsToURLString:(NSString*)aURL;
@end
