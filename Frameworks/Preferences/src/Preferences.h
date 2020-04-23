#import <oak/misc.h>

@class MASPreferencesWindowController;

PUBLIC @interface Preferences : NSResponder
@property (class, readonly) Preferences* sharedInstance;
- (void)showWindow:(id)sender;
@end
