#import "DownloadWindowController.h"
#import <network/key_chain.h>

PUBLIC extern NSString* const kUserDefaultsDisableSoftwareUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsSoftwareUpdateChannelKey;
PUBLIC extern NSString* const kUserDefaultsAskBeforeUpdatingKey;

PUBLIC extern NSString* const kSoftwareUpdateChannelRelease;
PUBLIC extern NSString* const kSoftwareUpdateChannelPrerelease;
PUBLIC extern NSString* const kSoftwareUpdateChannelCanary;

PUBLIC @interface SoftwareUpdate : NSObject <DownloadWindowControllerDelegate>
@property (class, readonly) SoftwareUpdate* sharedInstance;

@property (nonatomic)           NSDictionary* channels;
@property (nonatomic, readonly) NSDate*       lastPoll;
@property (nonatomic, readonly, getter = isChecking) BOOL checking;
@property (nonatomic, readonly) NSString*     errorString;

- (void)setSignee:(key_chain_t::key_t const&)aSignee;
- (IBAction)checkForUpdates:(id)sender;
@end
