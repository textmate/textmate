#import "DownloadWindowController.h"
#import <network/key_chain.h>

PUBLIC extern NSString* const kUserDefaultsDisableSoftwareUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsSoftwareUpdateChannelKey;
PUBLIC extern NSString* const kUserDefaultsSubmitUsageInfoKey;
PUBLIC extern NSString* const kUserDefaultsAskBeforeUpdatingKey;

PUBLIC extern NSString* const kSoftwareUpdateChannelRelease;
PUBLIC extern NSString* const kSoftwareUpdateChannelBeta;
PUBLIC extern NSString* const kSoftwareUpdateChannelNightly;

PUBLIC @interface SoftwareUpdate : NSObject <DownloadWindowControllerDelegate>
@property (nonatomic, retain)           NSDictionary* channels;
@property (nonatomic, retain, readonly) NSDate*       lastPoll;
@property (nonatomic, assign, readonly) BOOL          isChecking;
@property (nonatomic, retain, readonly) NSString*     errorString;

- (void)setSignee:(key_chain_t::key_t const&)aSignee;

+ (SoftwareUpdate*)sharedInstance;
- (IBAction)checkForUpdates:(id)sender;
@end
