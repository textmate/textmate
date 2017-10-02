#import "PreferencesPane.h"

@interface SoftwareUpdatePreferences : PreferencesPane
@property (nonatomic, readonly) NSString* lastCheck;
@property (nonatomic, readonly, getter = isChecking) BOOL checking;
@end
