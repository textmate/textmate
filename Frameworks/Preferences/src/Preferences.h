#import <oak/misc.h>

@class MASPreferencesWindowController;

PUBLIC @interface Preferences : NSResponder
+ (Preferences*)sharedInstance;
- (void)showWindow:(id)sender;
@end
