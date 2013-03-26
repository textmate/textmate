#import "BundlesManager.h"
#import "load_bundles.h"
#import "InstallBundleItems.h"
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/locations.h>
#import <bundles/query.h> // set_index
#import <ns/ns.h>
#import <io/path.h>
#import <io/events.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundlesManager);
OAK_DEBUG_VAR(BundlesManager_FSEvents);

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

	std::vector<std::string> bundlesPaths;
	std::string bundlesIndexPath;
	std::set<std::string> watchList;
	fs::cache_t cache;
}
@property (nonatomic) NSString* activityText;
@property (nonatomic) BOOL      isBusy;
@property (nonatomic) BOOL      determinateProgress;
@property (nonatomic) CGFloat   progress;
@property (nonatomic) NSTimer*  updateTimer;

@property (nonatomic) BOOL      needsCreateBundlesIndex;
@property (nonatomic) BOOL      needsSaveBundlesIndex;
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
		sourceList   = bundles_db::sources();
		bundlesIndex = bundles_db::index(kInstallDirectory);

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

- (void)installBundleItemsAtPaths:(NSArray*)somePaths
{
	InstallBundleItems(somePaths);
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

			std::set<std::string> paths;
			for(auto bundle : bundles)
			{
				installing.erase(bundle->uuid());
				paths.insert(path::parent(bundle->path()));
			}

			for(auto path : paths)
				[self reloadPath:[NSString stringWithCxxString:path]];

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
	auto path = path::parent(aBundle->path());
	bundles_db::uninstall(aBundle);
	[self reloadPath:[NSString stringWithCxxString:path]];
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

// ===============================================
// = Creating Bundle Index and Handling FSEvents =
// ===============================================

- (void)createBundlesIndex:(id)sender
{
	D(DBF_BundlesManager, bug("\n"););

	auto pair = create_bundle_index(bundlesPaths, cache);
	bundles::set_index(pair.first, pair.second);

	std::set<std::string> newWatchList;
	for(auto path : bundlesPaths)
		cache.copy_heads_for_path(path, std::inserter(newWatchList, newWatchList.end()));
	[self updateWatchList:newWatchList];

	_needsCreateBundlesIndex = NO;
}

- (void)saveBundlesIndex:(id)sender
{
	D(DBF_BundlesManager, bug("\n"););
	cache.cleanup(bundlesPaths);
	if(cache.dirty())
	{
		cache.save(bundlesIndexPath);
		cache.set_dirty(false);
	}
	_needsSaveBundlesIndex = NO;
}

- (void)setNeedsCreateBundlesIndex:(BOOL)flag
{
	D(DBF_BundlesManager, bug("%s\n", BSTR(flag)););
	if(_needsCreateBundlesIndex != flag && (_needsCreateBundlesIndex = flag))
		[self performSelector:@selector(createBundlesIndex:) withObject:self afterDelay:0];
}

- (void)setNeedsSaveBundlesIndex:(BOOL)flag
{
	D(DBF_BundlesManager, bug("%s\n", BSTR(flag)););
	if(_needsSaveBundlesIndex != flag && (_needsSaveBundlesIndex = flag))
		[self performSelector:@selector(saveBundlesIndex:) withObject:self afterDelay:5];
}

- (void)setEventId:(uint64_t)anEventId forPath:(NSString*)aPath
{
	cache.set_event_id_for_path(anEventId, to_s(aPath));
	self.needsSaveBundlesIndex = YES;
}

- (void)updateWatchList:(std::set<std::string> const&)newWatchList
{
	struct callback_t : fs::event_callback_t
	{
		void set_replaying_history (bool flag, std::string const& observedPath, uint64_t eventId)
		{
			D(DBF_BundlesManager_FSEvents, bug("%s (observing ‘%s’)\n", BSTR(flag), observedPath.c_str()););
			[[BundlesManager sharedInstance] setEventId:eventId forPath:[NSString stringWithCxxString:observedPath]];
		}

		void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive)
		{
			D(DBF_BundlesManager_FSEvents, bug("%s (observing ‘%s’)\n", path.c_str(), observedPath.c_str()););
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:observedPath]];
			[[BundlesManager sharedInstance] setEventId:eventId forPath:[NSString stringWithCxxString:observedPath]];
		}
	};

	static callback_t callback;

	std::vector<std::string> pathsAdded, pathsRemoved;
	std::set_difference(watchList.begin(), watchList.end(), newWatchList.begin(), newWatchList.end(), back_inserter(pathsRemoved));
	std::set_difference(newWatchList.begin(), newWatchList.end(), watchList.begin(), watchList.end(), back_inserter(pathsAdded));

	watchList = newWatchList;

	for(auto path : pathsRemoved)
	{
		D(DBF_BundlesManager_FSEvents, bug("unwatch ‘%s’\n", path.c_str()););
		fs::unwatch(path, &callback);
	}

	for(auto path : pathsAdded)
	{
		D(DBF_BundlesManager_FSEvents, bug("watch ‘%s’\n", path.c_str()););
		fs::watch(path, &callback, cache.event_id_for_path(path) ?: FSEventsGetCurrentEventId(), 1);
	}
}

- (void)reloadPath:(NSString*)aPath
{
	D(DBF_BundlesManager, bug("%s\n", [aPath UTF8String]););
	if(cache.reload(to_s(aPath)))
	{
		self.needsCreateBundlesIndex = YES;
		self.needsSaveBundlesIndex   = YES;
	}
	else
	{
		D(DBF_BundlesManager, bug("no changes\n"););
	}
}

namespace
{
	static std::string const kFieldChangedItems = "changed";
	static std::string const kFieldDeletedItems = "deleted";
	static std::string const kFieldMainMenu     = "mainMenu";

	static plist::dictionary_t prune_dictionary (plist::dictionary_t const& plist)
	{
		static auto const DesiredKeys = new std::set<std::string>{ bundles::kFieldName, bundles::kFieldKeyEquivalent, bundles::kFieldTabTrigger, bundles::kFieldScopeSelector, bundles::kFieldSemanticClass, bundles::kFieldContentMatch, bundles::kFieldGrammarFirstLineMatch, bundles::kFieldGrammarScope, bundles::kFieldGrammarInjectionSelector, bundles::kFieldDropExtension, bundles::kFieldGrammarExtension, bundles::kFieldSettingName, bundles::kFieldHideFromUser, bundles::kFieldIsDeleted, bundles::kFieldIsDisabled, bundles::kFieldRequiredItems, bundles::kFieldUUID, bundles::kFieldIsDelta, kFieldMainMenu, kFieldDeletedItems, kFieldChangedItems };

		plist::dictionary_t res;
		for(auto pair : plist)
		{
			if(DesiredKeys->find(pair.first) == DesiredKeys->end() && pair.first.find(bundles::kFieldSettingName) != 0)
				continue;

			if(pair.first == bundles::kFieldSettingName)
			{
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair.second))
				{
					plist::array_t settings;
					iterate(settingsPair, *dictionary)
						settings.push_back(settingsPair->first);
					res.insert(std::make_pair(pair.first, settings));
				}
			}
			else if(pair.first == kFieldChangedItems)
			{
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair.second))
					res.insert(std::make_pair(pair.first, prune_dictionary(*dictionary)));
			}
			else
			{
				res.insert(pair);
			}
		}
		return res;
	}
}

- (void)loadBundlesIndex
{
	for(auto path : bundles::locations())
		bundlesPaths.push_back(path::join(path, "Bundles"));
	bundlesIndexPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.plist");
	cache.load(bundlesIndexPath, &prune_dictionary);
	[self createBundlesIndex:self];
}
@end
