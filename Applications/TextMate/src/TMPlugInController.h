@interface TMPlugInController : NSObject
+ (TMPlugInController*)sharedInstance;
- (void)loadAllPlugIns:(id)sender;
- (void)loadPlugIn:(NSString*)aPath;
// - (void)installPlugIn:(NSString*)aPath;
- (float)version;
@end
