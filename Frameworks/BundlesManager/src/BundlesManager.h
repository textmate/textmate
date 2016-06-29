#import "Bundle.h"
#import <bundles/item.h>

PUBLIC extern NSString* const kUserDefaultsDisableBundleUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsLastBundleUpdateCheckKey;

PUBLIC @interface BundlesManager : NSObject
@property (nonatomic) BOOL autoUpdateBundles;

@property (nonatomic) NSString* activityText;
@property (nonatomic) BOOL      isBusy;
@property (nonatomic, readonly) BOOL      determinateProgress;
@property (nonatomic, readonly) CGFloat   progress;

@property (nonatomic, readonly) NSArray<Bundle*>* bundles;

- (void)installBundles:(NSArray<Bundle*>*)someBundles completionHandler:(void(^)(NSArray<Bundle*>*))callback;
- (void)uninstallBundle:(Bundle*)aBundle;

+ (BundlesManager*)sharedInstance;
- (void)loadBundlesIndex;
- (void)installBundleItemsAtPaths:(NSArray*)somePaths;
- (BOOL)findBundleForInstall:(bundles::item_ptr*)res;
- (void)reloadPath:(NSString*)aPath;
@end
