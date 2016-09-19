#import <oak/misc.h>

@class MASPreferencesWindowController;

PUBLIC @interface Preferences : NSResponder
+ (instancetype)sharedInstance;
- (void)showWindow:(id)sender;
@end
