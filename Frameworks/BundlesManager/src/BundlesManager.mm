#import "BundlesManager.h"
#import "load_bundles.h"
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundlesManager);

NSString* const kUserDefaultsDisableBundleUpdatesKey       = @"disableBundleUpdates";
NSString* const kUserDefaultsLastBundleUpdateCheckKey      = @"lastBundleUpdateCheck";
NSString* const BundlesManagerBundlesDidChangeNotification = @"BundlesManagerBundlesDidChangeNotification";

static std::string const kInstallDirectory = NULL_STR;
static double const kPollInterval = 3*60*60;

@interface BundlesManager ()
{
	std::vector<bundles_db::source_ptr> sourceList;
	std::vector<bundles_db::bundle_ptr> bundlesIndex;
	std::set<oak::uuid_t> installing;
}
@property (nonatomic) BOOL      isBusy;
@property (nonatomic) NSString* activityText;
@property (nonatomic) double    progress;
@property (nonatomic) NSTimer*  updateTimer;
@end

@implementation BundlesManager
+ (BundlesManager*)sharedInstance
{
	static BundlesManager* instance = [BundlesManager new];
	return instance;
}

- (id)init
{
	if(self = [super init])
	{
		self.progress = 1;

		sourceList   = bundles_db::sources();
		bundlesIndex = bundles_db::index(kInstallDirectory);

		load_bundles(path::join(path::home(), "Library/Caches/com.macromates.TextMate"));

		// remove old cache files
		unlink(path::join(path::home(), "Library/Application Support/TextMate/Cache/FSNodes.plist").c_str());
		unlink(path::join(path::home(), "Library/Application Support/TextMate/Cache/PropertyValues.plist").c_str());
		rmdir(path::join(path::home(), "Library/Application Support/TextMate/Cache").c_str());
	}
	return self;
}

- (void)setAutoUpdateBundles:(BOOL)flag
{
	if(_autoUpdateBundles == flag)
		return;

	if(_autoUpdateBundles = flag)
	{
		[self updateSources:nil];
	}
	else
	{
		[self.updateTimer invalidate];
		self.updateTimer = nil;
	}
}

- (void)installBundles:(std::vector<bundles_db::bundle_ptr> const&)bundlesReference completionHandler:(void(^)(std::vector<bundles_db::bundle_ptr> const& failedBundles))callback
{
	__block std::vector<bundles_db::bundle_ptr> failedBundles;
	if(bundlesReference.empty())
	{
		callback(failedBundles);
		return;
	}

	auto bundles = bundlesReference;
	for(auto bundle : bundles)
		installing.insert(bundle->uuid());

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		dispatch_apply(bundles.size(), dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^(size_t i){
			D(DBF_BundlesManager, bug("Updating ‘%s’…\n", bundles[i]->name().c_str()););
			if(!bundles_db::update(bundles[i]))
				failedBundles.push_back(bundles[i]);
		});

		dispatch_async(dispatch_get_main_queue(), ^{
			for(auto bundle : failedBundles)
				fprintf(stderr, "*** error downloading ‘%s’\n", bundle->url().c_str());
			for(auto bundle : bundles)
				installing.erase(bundle->uuid());
			callback(failedBundles);
			[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];
			bundles_db::save_index(bundlesIndex, kInstallDirectory);
		});
	});
}

- (void)updateInstalledBundles
{
	std::vector<bundles_db::bundle_ptr> bundles;
	for(auto installedBundle : bundlesIndex)
	{
		if(!installedBundle->has_update())
			continue;

		for(auto bundle : bundles_db::dependencies(bundlesIndex, installedBundle, false, false))
		{
			if((bundle->has_update() || !bundle->installed()) && installing.find(bundle->uuid()) == installing.end())
				bundles.push_back(bundle);
		}
	}

	if(!bundles.empty())
	{
		self.activityText = @"Updating bundles…";
		self.isBusy       = YES;
		[self installBundles:bundles completionHandler:^(std::vector<bundles_db::bundle_ptr> const& failedBundles){
			self.activityText = @"Bundles updated";
			self.isBusy       = NO;
		}];
	}
}

- (void)updateSources:(id)sender
{
	D(DBF_BundlesManager, bug("busy %s\n", BSTR(self.isBusy)););
	if(self.isBusy || sourceList.empty())
		return;

	std::vector<bundles_db::source_ptr> sources;
	NSDate* earliest = [NSDate distantFuture];
	for(auto source : sourceList)
	{
		if(!source->disabled() && source->needs_update(kPollInterval))
			sources.push_back(source);
		earliest = [earliest earlierDate:CFBridgingRelease(CFDateCreate(kCFAllocatorDefault, source->last_check().value()))];
	}

	if(sources.empty())
	{
		[self.updateTimer invalidate];
		self.updateTimer = [NSTimer scheduledTimerWithTimeInterval:MIN(kPollInterval + [earliest timeIntervalSinceNow], kPollInterval) target:self selector:@selector(updateSources:) userInfo:nil repeats:NO];
		self.activityText = [NSString stringWithFormat:@"Last check: %@", [earliest humanReadableTimeElapsed]];
		return;
	}

	self.activityText = @"Updating sources…";
	self.isBusy       = YES;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		__block std::vector<bundles_db::source_ptr> failedSources;
		dispatch_apply(sources.size(), dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^(size_t i){
			D(DBF_BundlesManager, bug("Updating ‘%s’…\n", sources[i]->url().c_str()););
			if(!bundles_db::update(sources[i]))
				failedSources.push_back(sources[i]);
		});

		dispatch_async(dispatch_get_main_queue(), ^{
			for(auto source : failedSources)
				fprintf(stderr, "*** error downloading ‘%s’\n", source->url().c_str());

			bundlesIndex = bundles_db::index(kInstallDirectory);
			// trigger reload for table view in Preferences → Bundles
			[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];

			self.activityText = @"Sources updated";
			self.isBusy       = NO;

			if(![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableBundleUpdatesKey])
				[self updateInstalledBundles];

			[self.updateTimer invalidate];
			self.updateTimer = [NSTimer scheduledTimerWithTimeInterval:(failedSources.empty() ? kPollInterval : 20*60*60) target:self selector:@selector(updateSources:) userInfo:nil repeats:NO];
		});
	});
}

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle
{
	std::vector<bundles_db::bundle_ptr> bundles;
	for(auto bundle : bundles_db::dependencies(bundlesIndex, aBundle, true, false))
	{
		if(installing.find(bundle->uuid()) == installing.end())
		{
			bundles.push_back(bundle);
			installing.insert(bundle->uuid());
		}
	}

	NSString* bundleName = [NSString stringWithCxxString:aBundle->name()];
	self.activityText = [NSString stringWithFormat:@"Installing ‘%@’…", bundleName];
	self.isBusy       = YES;
	[self installBundles:bundles completionHandler:^(std::vector<bundles_db::bundle_ptr> const& failedBundles){
		self.activityText = [NSString stringWithFormat:@"Installed ‘%@’.", bundleName];
		self.isBusy       = NO;
	}];
}

- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle
{
	bundles_db::uninstall(aBundle);
	bundles_db::save_index(bundlesIndex, kInstallDirectory);
	self.activityText = [NSString stringWithFormat:@"Uninstalled ‘%@’.", [NSString stringWithCxxString:aBundle->name()]];
}

- (NSCellStateValue)installStateForBundle:(bundles_db::bundle_ptr const&)aBundle
{
	if(aBundle->installed())
		return NSOnState;
	else if(installing.find(aBundle->uuid()) != installing.end())
		return NSMixedState;
	return NSOffState;
}

- (NSUInteger)numberOfBundles
{
	return bundlesIndex.size();
}

- (bundles_db::bundle_ptr const&)bundleAtIndex:(NSUInteger)anIndex
{
	return bundlesIndex[anIndex];
}
@end
