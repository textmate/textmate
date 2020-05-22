PUBLIC extern NSString* const kUserDefaultsDisableSoftwareUpdateKey;
PUBLIC extern NSString* const kUserDefaultsSoftwareUpdateChannelKey;
PUBLIC extern NSString* const kUserDefaultsAskBeforeUpdatingKey;
PUBLIC extern NSString* const kUserDefaultsLastSoftwareUpdateCheckKey;

PUBLIC extern NSString* const kSoftwareUpdateChannelRelease;
PUBLIC extern NSString* const kSoftwareUpdateChannelPrerelease;
PUBLIC extern NSString* const kSoftwareUpdateChannelCanary;

PUBLIC @interface SoftwareUpdate : NSObject
@property (class, readonly) SoftwareUpdate* sharedInstance;

@property (nonatomic)           NSDictionary* channels;
@property (nonatomic, readonly, getter = isChecking) BOOL checking;
@property (nonatomic, readonly) NSString*     errorString;

- (IBAction)checkForUpdate:(id)sender;
@end
