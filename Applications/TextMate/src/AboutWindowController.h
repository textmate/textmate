@interface AboutWindowController : NSWindowController
+ (AboutWindowController*)sharedInstance;
+ (void)showChangesIfUpdated;
- (void)showAboutWindow:(id)sender;
- (void)showChangesWindow:(id)sender;
@end

@interface RegistrationWindowController : NSWindowController
+ (instancetype)sharedInstance;
@end
