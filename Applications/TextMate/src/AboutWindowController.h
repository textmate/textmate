@interface AboutWindowController : NSWindowController <NSWindowDelegate, NSToolbarDelegate>
+ (BOOL)shouldShowChangesWindow;
- (void)showAboutWindow:(id)sender;
- (void)showChangesWindow:(id)sender;
@end
