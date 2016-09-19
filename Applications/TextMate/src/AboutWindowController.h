@interface AboutWindowController : NSWindowController
+ (instancetype)sharedInstance;
+ (void)showChangesIfUpdated;
- (void)showAboutWindow:(id)sender;
- (void)showChangesWindow:(id)sender;
@end

@interface RegistrationWindowController : NSWindowController
+ (instancetype)sharedInstance;
@end
