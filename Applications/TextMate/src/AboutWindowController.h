@interface AboutWindowController : NSWindowController
@property (class, readonly) AboutWindowController* sharedInstance;
+ (void)showChangesIfUpdated;
- (void)showAboutWindow:(id)sender;
- (void)showChangesWindow:(id)sender;
@end
