#import "PreferencesPane.h"

@interface SoftwareUpdatePreferences : PreferencesPane
{
	NSString* lastCheck;
	NSTimer* updateLastCheckTimer;

	BOOL isChecking;
	NSDate* lastPoll;
	NSString* errorString;
}
@property (nonatomic, readonly) NSString* lastCheck;
@property (nonatomic, readonly) BOOL isChecking;

- (IBAction)performSoftwareUpdateCheck:(id)sender;
@end
