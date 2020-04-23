@protocol TMPlugInController
- (CGFloat)version;
@end

@protocol TMPlugIn
@optional
- (id)initWithPlugInController:(id <TMPlugInController>)aController;
@end

@interface TMPlugInController : NSObject <TMPlugInController>
@property (class, readonly) TMPlugInController* sharedInstance;
- (void)loadAllPlugIns:(id)sender;
- (CGFloat)version;
- (void)installPlugInAtPath:(NSString*)aPath;
@end
