#import <updater/updater.h>

extern PUBLIC NSString* const BundlesManagerBundlesDidChangeNotification;

PUBLIC @interface BundlesManager : NSObject
{
	std::vector<bundles_db::source_ptr> sourceList;
	std::vector<bundles_db::bundle_ptr> bundlesIndex;

	BOOL isBusy;
	NSString* activityText;
	double progress;

	NSUInteger scheduledTasks;
	NSString* threadActivityText;
	double threadProgress;
	NSTimer* progressTimer;

	std::set<oak::uuid_t> installing;
}
@property (nonatomic, readonly)         BOOL      isBusy;
@property (nonatomic, retain, readonly) NSString* activityText;
@property (nonatomic, readonly)         double    progress;

- (NSUInteger)numberOfBundles;
- (bundles_db::bundle_ptr const&)bundleAtIndex:(NSUInteger)anIndex;
- (NSCellStateValue)installStateForBundle:(bundles_db::bundle_ptr const&)aBundle;

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle;
- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle;

- (void)updateSources:(id)sender;
- (void)updateBundles:(id)sender;

+ (BundlesManager*)sharedInstance;
@end

