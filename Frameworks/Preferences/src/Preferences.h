@class MASPreferencesWindowController;

@interface Preferences : NSResponder
{
	MASPreferencesWindowController* windowController;
	NSArray* viewControllers;
}
+ (Preferences*)sharedInstance;

- (void)showWindow:(id)sender;
@end
