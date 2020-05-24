extern NSString* const kUserDefaultsDisableSoftwareUpdateKey;
extern NSString* const kUserDefaultsSoftwareUpdateChannelKey;
extern NSString* const kUserDefaultsAskBeforeUpdatingKey;
extern NSString* const kUserDefaultsLastSoftwareUpdateCheckKey;

extern NSString* const kSoftwareUpdateChannelRelease;
extern NSString* const kSoftwareUpdateChannelPrerelease;
extern NSString* const kSoftwareUpdateChannelCanary;

@interface SoftwareUpdate : NSObject
@property (class, readonly) SoftwareUpdate* sharedInstance;

@property (nonatomic) NSDictionary<NSString*, NSURL*>* channels;
@property (nonatomic, readonly, getter = isChecking) BOOL checking;
@property (nonatomic, readonly) NSString* errorString;

- (void)checkForUpdate:(id)sender;
@end

NSComparisonResult OakCompareVersionStrings (NSString* lhsString, NSString* rhsString);
