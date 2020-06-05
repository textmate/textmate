@interface CrashReporter : NSObject
@property (class, readonly) CrashReporter* sharedInstance;
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification;
- (void)postNewCrashReportsToURLString:(NSString*)aURL;
@end
