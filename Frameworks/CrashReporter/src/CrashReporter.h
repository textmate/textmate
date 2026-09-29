@interface CrashReporter : NSObject
@property (class, readonly) CrashReporter* sharedInstance;
- (void)postNewCrashReportsToURLString:(NSString*)aURL;
@end
