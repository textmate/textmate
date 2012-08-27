#import <oak/misc.h>

@class MASPreferencesWindowController;

PUBLIC @interface Preferences : NSResponder
{
	MASPreferencesWindowController* windowController;
	NSArray* viewControllers;
}
+ (Preferences*)sharedInstance;

- (void)showWindow:(id)sender;
@end
