@interface TMPlugInController : NSObject
{
	NSMutableArray* loadedPlugIns;
	NSMutableSet* plugInBundleIdentifiers;
	BOOL didLoadAllPlugIns;
}
+ (TMPlugInController*)sharedInstance;
- (void)loadAllPlugIns:(id)sender;
- (void)loadPlugIn:(NSString*)aPath;
// - (void)installPlugIn:(NSString*)aPath;
- (float)version;
@end
