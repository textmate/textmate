#import "Bundle.h"
#import <bundles/item.h>

PUBLIC extern NSString* const kUserDefaultsDisableBundleUpdatesKey;
PUBLIC extern NSString* const kUserDefaultsLastBundleUpdateCheckKey;

PUBLIC @interface BundlesManager : NSObject
@property (class, readonly) BundlesManager* sharedInstance;

@property (nonatomic, readonly) NSArray<Bundle*>* bundles;

- (NSProgress*)installBundles:(NSArray<Bundle*>*)someBundles completionHandler:(void(^)(NSArray<Bundle*>*))callback;
- (void)uninstallBundle:(Bundle*)aBundle;
- (void)loadBundlesIndex;
- (void)installBundleItemsAtPaths:(NSArray*)somePaths;
- (BOOL)findBundleForInstall:(bundles::item_ptr*)res;
- (void)reloadPath:(NSString*)aPath;
@end
