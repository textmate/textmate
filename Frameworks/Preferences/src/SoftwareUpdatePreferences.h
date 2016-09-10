#import "PreferencesPane.h"

@interface SoftwareUpdatePreferences : PreferencesPane
@property (nonatomic, readonly) NSString* lastCheck;
@property (nonatomic, readonly) BOOL isChecking;
@end
