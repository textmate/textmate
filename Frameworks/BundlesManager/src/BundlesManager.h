#import <updater/updater.h>

PUBLIC extern NSString* const kUserDefaultsDisableBundleUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsLastBundleUpdateCheckKey;
PUBLIC extern NSString* const BundlesManagerBundlesDidChangeNotification;

PUBLIC @interface BundlesManager : NSObject
@property (nonatomic) BOOL autoUpdateBundles;

@property (nonatomic, readonly) NSString* activityText;
@property (nonatomic, readonly) BOOL      isBusy;
@property (nonatomic, readonly) BOOL      determinateProgress;
@property (nonatomic, readonly) CGFloat   progress;

- (NSUInteger)numberOfBundles;
- (bundles_db::bundle_ptr const&)bundleAtIndex:(NSUInteger)anIndex;
- (NSCellStateValue)installStateForBundle:(bundles_db::bundle_ptr const&)aBundle;

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle;
- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle;

+ (BundlesManager*)sharedInstance;
- (void)loadBundlesIndex;
@end
