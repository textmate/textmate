#import "BundlesManager.h"
#import <bundles/load.h>
#import "InstallBundleItems.h"
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/locations.h>
#import <bundles/query.h> // set_index
#import <regexp/format_string.h>
#import <text/ctype.h>
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
	plist::cache_t cache;
}
@property (nonatomic) NSString* activityText;
@property (nonatomic) BOOL      isBusy;
@property (nonatomic) BOOL      determinateProgress;
@property (nonatomic) CGFloat   progress;
@property (nonatomic) NSDate*   lastUpdateCheck;
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

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	D(DBF_BundlesManager, bug("\n"););
	if(self.needsSaveBundlesIndex)
		[self saveBundlesIndex:self];
}

- (void)setAutoUpdateBundles:(BOOL)flag
{
	if(_autoUpdateBundles == flag)
		return;

	_autoUpdateBundles = flag;
	[self setupUpdateTimer];
}

- (void)setupUpdateTimer
{
	[self.updateTimer invalidate];
	self.updateTimer = nil;
	if(!_autoUpdateBundles)
		return;

	NSDate* lastCheck = [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsLastBundleUpdateCheckKey] ?: [NSDate distantPast];
	NSDate* nextCheck = [lastCheck dateByAddingTimeInterval:kPollInterval];
	NSTimeInterval checkAfterSeconds = std::max<NSTimeInterval>(1, [nextCheck timeIntervalSinceNow]);
	D(DBF_BundlesManager, bug("perform next check in %.1f hours\n", checkAfterSeconds/60/60););
	self.updateTimer = [NSTimer scheduledTimerWithTimeInterval:checkAfterSeconds target:self selector:@selector(didFireUpdateTimer:) userInfo:nil repeats:NO];
}

- (void)didFireUpdateTimer:(NSTimer*)aTimer
{
	self.isBusy = YES;

	std::vector<bundles_db::source_ptr> sources;
	for(auto source : sourceList)
	{
		if(!source->disabled())
			sources.push_back(source);
	}

	self.activityText = @"Updating sources…";
	[self updateSources:sources completionHandler:^(std::vector<bundles_db::source_ptr> failedSources){
		std::vector<bundles_db::bundle_ptr> outdatedBundles;
		if(![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableBundleUpdatesKey])
		{
			for(auto installedBundle : bundlesIndex)
			{
				if(!installedBundle->has_update())
					continue;

				for(auto bundle : bundles_db::dependencies(bundlesIndex, installedBundle, false, false))
				{
					if((bundle->has_update() || !bundle->installed()) && installing.find(bundle->uuid()) == installing.end())
						outdatedBundles.push_back(bundle);
				}
			}
		}

		self.activityText = @"Updating bundles…";
		[self installBundles:outdatedBundles completionHandler:^(std::vector<bundles_db::bundle_ptr> failedBundles){
			NSDate* lastCheck = [NSDate date];
			if(failedSources.empty() && failedBundles.empty())
			{
				self.activityText = nil;
			}
			else
			{
				self.activityText = @"Error updating bundles, will retry later.";
				lastCheck = [lastCheck dateByAddingTimeInterval:-(kPollInterval - 30*60)]; // retry in 30 minutes

				for(auto source : failedSources)
					fprintf(stderr, "*** error downloading ‘%s’\n", source->url().c_str());
				for(auto bundle : failedBundles)
					fprintf(stderr, "*** error downloading ‘%s’\n", bundle->url().c_str());
			}

			self.isBusy = NO;

			[[NSUserDefaults standardUserDefaults] setObject:lastCheck forKey:kUserDefaultsLastBundleUpdateCheckKey];
			[self setupUpdateTimer];
		}];
	}];
}

- (void)installBundleItemsAtPaths:(NSArray*)somePaths
{
	InstallBundleItems(somePaths);
}

- (BOOL)findBundleForInstall:(bundles::item_ptr*)res
{
	oak::uuid_t defaultBundle;

	std::string const personalBundleName = format_string::expand("${TM_FULLNAME/^(\\S+).*$/$1/}’s Bundle", std::map<std::string, std::string>{ { "TM_FULLNAME", path::passwd_entry()->pw_gecos ?: "John Doe" } });
	for(auto item : bundles::query(bundles::kFieldName, personalBundleName, scope::wildcard, bundles::kItemTypeBundle))
		defaultBundle = item->uuid();

	NSPopUpButton* bundleChooser = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	[bundleChooser.menu removeAllItems];
	[bundleChooser.menu addItemWithTitle:@"Create new bundle…" action:NULL keyEquivalent:@""];
	[bundleChooser.menu addItem:[NSMenuItem separatorItem]];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.emplace(item->name(), item);

	for(auto pair : ordered)
	{
		NSMenuItem* menuItem = [bundleChooser.menu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:NULL keyEquivalent:@""];
		[menuItem setRepresentedObject:[NSString stringWithCxxString:to_s(pair.second->uuid())]];
		if(defaultBundle && defaultBundle == pair.second->uuid())
			[bundleChooser selectItem:menuItem];
	}

	[bundleChooser sizeToFit];
	NSRect frame = [bundleChooser frame];
	if(NSWidth(frame) > 200)
		[bundleChooser setFrameSize:NSMakeSize(200, NSHeight(frame))];

	NSAlert* alert = [NSAlert alertWithMessageText:@"Select Bundle" defaultButton:@"OK" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Select the bundle which should be used for the new item(s)."];
	[alert setAccessoryView:bundleChooser];
	if([alert runModal] == NSAlertDefaultReturn) // "OK"
	{
		if(NSString* bundleUUID = [[bundleChooser selectedItem] representedObject])
		{
			for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle, to_s(bundleUUID)))
			{
				*res = item;
				return YES;
			}
		}
		else
		{
			NSRunAlertPanel(@"Creating bundles is not yet supported.", @"You can create a new bundle in the bundle editor via File → New (⌘N) and then repeat the previous action.", @"OK", nil, nil);
		}
	}
	return NO;
}

- (void)installBundles:(std::vector<bundles_db::bundle_ptr> const&)bundlesReference completionHandler:(void(^)(std::vector<bundles_db::bundle_ptr> failedBundles))callback
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
			{
				[self reloadPath:[NSString stringWithCxxString:bundle->path()] recursive:YES];
				installing.erase(bundle->uuid());
			}

			callback(failedBundles);
			[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];
			bundles_db::save_index(bundlesIndex, kInstallDirectory);
		});
	});
}

- (void)updateSources:(std::vector<bundles_db::source_ptr> const&)sourcesReference completionHandler:(void(^)(std::vector<bundles_db::source_ptr> failedSources))callback
{
	D(DBF_BundlesManager, bug("\n"););
	__block std::vector<bundles_db::source_ptr> failedSources;
	if(sourcesReference.empty())
	{
		callback(failedSources);
		return;
	}

	auto sources = sourcesReference;
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		dispatch_apply(sources.size(), dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^(size_t i){
			D(DBF_BundlesManager, bug("Updating ‘%s’…\n", sources[i]->url().c_str()););
			bundles_db::update(sources[i]);
		});

		dispatch_async(dispatch_get_main_queue(), ^{
			bundlesIndex = bundles_db::index(kInstallDirectory);
			// trigger reload for table view in Preferences → Bundles
			[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];
			callback(failedSources);
		});
	});
}

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle completionHandler:(void(^)(BOOL))callback
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
	[self installBundles:bundles completionHandler:^(std::vector<bundles_db::bundle_ptr> failedBundles){
		self.activityText = [NSString stringWithFormat:@"Installed ‘%@’.", bundleName];
		self.isBusy       = NO;
		if(callback)
		{
			[self createBundlesIndex:self];
			callback(failedBundles.empty());
		}
	}];
}

- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle
{
	bundles_db::uninstall(aBundle);
	[self erasePath:[NSString stringWithCxxString:aBundle->path()]];
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
	D(DBF_BundlesManager, bug("%s\n", BSTR(_needsCreateBundlesIndex)););
	if(_needsCreateBundlesIndex == NO)
		return;
	_needsCreateBundlesIndex = NO;

	auto pair = create_bundle_index(bundlesPaths, cache);
	bundles::set_index(pair.first, pair.second);

	std::set<std::string> newWatchList;
	for(auto path : bundlesPaths)
		cache.copy_heads_for_path(path, std::inserter(newWatchList, newWatchList.end()));
	[self updateWatchList:newWatchList];
}

- (void)saveBundlesIndex:(id)sender
{
	D(DBF_BundlesManager, bug("\n"););
	cache.cleanup(bundlesPaths);
	if(cache.dirty())
	{
		cache.save_capnp(bundlesIndexPath);
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
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:path] recursive:recursive];
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

- (void)erasePath:(NSString*)aPath
{
	D(DBF_BundlesManager, bug("%s\n", [aPath UTF8String]););
	if(cache.erase(to_s(aPath)))
	{
		self.needsCreateBundlesIndex = YES;
		self.needsSaveBundlesIndex   = YES;
	}
	else
	{
		D(DBF_BundlesManager, bug("no changes\n"););
	}
}

- (void)reloadPath:(NSString*)aPath
{
	[self reloadPath:aPath recursive:NO];
}

- (void)reloadPath:(NSString*)aPath recursive:(BOOL)flag
{
	D(DBF_BundlesManager, bug("%s\n", [aPath UTF8String]););
	if(cache.reload(to_s(aPath), flag))
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
					for(auto const& settingsPair : *dictionary)
						settings.push_back(settingsPair.first);
					res.emplace(pair.first, settings);
				}
			}
			else if(pair.first == kFieldChangedItems)
			{
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair.second))
					res.emplace(pair.first, prune_dictionary(*dictionary));
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
	bundlesIndexPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.binary");
	cache.set_content_filter(&prune_dictionary);

	std::string const oldPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.plist");
	if(access(oldPath.c_str(), R_OK) == 0)
	{
		cache.load(oldPath);
		cache.save_capnp(bundlesIndexPath);
		unlink(oldPath.c_str());
	}
	else
	{
		cache.load_capnp(bundlesIndexPath);
	}

	_needsCreateBundlesIndex = YES;
	[self createBundlesIndex:self];
}
@end
