@interface AboutWindowController : NSWindowController
+ (instancetype)sharedInstance;
+ (void)showChangesIfUpdated;
- (void)showAboutWindow:(id)sender;
- (void)showChangesWindow:(id)sender;
@end
