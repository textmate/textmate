#import <updater/updater.h>
#import <bundles/item.h>

PUBLIC extern NSString* const kUserDefaultsDisableBundleUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsLastBundleUpdateCheckKey;
PUBLIC extern NSString* const BundlesManagerBundlesDidChangeNotification;

PUBLIC @interface BundlesManager : NSObject
@property (nonatomic) BOOL autoUpdateBundles;

@property (nonatomic, readonly) NSString* activityText;
@property (nonatomic, readonly) BOOL      isBusy;
@property (nonatomic, readonly) BOOL      determinateProgress;
@property (nonatomic, readonly) CGFloat   progress;
@property (nonatomic, readonly) NSDate*   lastUpdateCheck;

- (NSUInteger)numberOfBundles;
- (bundles_db::bundle_ptr const&)bundleAtIndex:(NSUInteger)anIndex;
- (NSCellStateValue)installStateForBundle:(bundles_db::bundle_ptr const&)aBundle;

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle completionHandler:(void(^)(BOOL))callback;
- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle;

+ (BundlesManager*)sharedInstance;
- (void)loadBundlesIndex;
- (void)installBundleItemsAtPaths:(NSArray*)somePaths;
- (BOOL)findBundleForInstall:(bundles::item_ptr*)res;
- (void)reloadPath:(NSString*)aPath;
@end
