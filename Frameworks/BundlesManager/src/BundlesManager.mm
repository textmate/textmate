#import "BundlesManager.h"
#import <OakFoundation/NSDate Additions.h>
#import <network/network.h>
#import <oak/server.h>

NSString* const BundlesManagerBundlesDidChangeNotification = @"BundlesManagerBundlesDidChangeNotification";
static std::string const kInstallDirectory = NULL_STR;

@interface BundlesManager ()
@property (nonatomic, assign) BOOL      isBusy;
@property (nonatomic, retain) NSString* activityText;
@property (nonatomic, assign) double    progress;
@property (atomic, retain)    NSString* threadActivityText;
@property (nonatomic, assign) double    threadProgress;
@property (nonatomic, retain) NSTimer*  progressTimer;

- (void)didStartThreadActivity:(id)sender;
- (void)didFinishActivityForSources:(std::vector<bundles_db::source_ptr> const&)someSources bundles:(std::vector<bundles_db::bundle_ptr> const&)someBundles;

- (void)updateSources:(id)sender;
- (void)updateBundles:(id)sender;
@end

namespace
{
	struct background_task_t
	{
		struct request_t
		{
			request_t (std::vector<bundles_db::source_ptr> const& sources, std::vector<bundles_db::bundle_ptr> const& bundles, BundlesManager* bundlesManager, double* threadProgress) : sources(sources), bundles(bundles), bundles_manager(bundlesManager), thread_progress(threadProgress) { }

			std::vector<bundles_db::source_ptr> sources;
			std::vector<bundles_db::bundle_ptr> bundles;
			BundlesManager* bundles_manager;
			double* thread_progress;
		};

		background_task_t (std::vector<bundles_db::source_ptr> const& sources, std::vector<bundles_db::bundle_ptr> const& bundles, BundlesManager* bundlesManager, double* threadProgress);
		~background_task_t ();
		static bool handle_request (request_t const& request);
		void handle_reply (bool result);

	private:
		std::vector<bundles_db::source_ptr> _sources;
		std::vector<bundles_db::bundle_ptr> _bundles;
		BundlesManager* _self;
		size_t _client_key;
	};

	static oak::server_t<background_task_t, background_task_t::request_t, bool>& server ()
	{
		static oak::server_t<background_task_t, background_task_t::request_t, bool> server;
		return server;
	}

	background_task_t::background_task_t (std::vector<bundles_db::source_ptr> const& sources, std::vector<bundles_db::bundle_ptr> const& bundles, BundlesManager* bundlesManager, double* threadProgress) : _sources(sources), _bundles(bundles), _self(bundlesManager)
	{
		_client_key = server().register_client(this);
		[_self didStartThreadActivity:nil];
		server().send_request(_client_key, request_t(sources, bundles, bundlesManager, threadProgress));
	}

	background_task_t::~background_task_t ()
	{
		server().unregister_client(_client_key);
		[_self didFinishActivityForSources:_sources bundles:_bundles];
	}

	bool background_task_t::handle_request (request_t const& request)
	{
		@autoreleasepool {
			std::vector<bundles_db::source_ptr> const& sources = request.sources;
			std::vector<bundles_db::bundle_ptr> const& bundles = request.bundles;
			BundlesManager* self = request.bundles_manager;

			for(size_t i = 0; i < sources.size(); ++i)
			{
				self.threadProgress     = i / (double)sources.size();
				self.threadActivityText = [NSString stringWithFormat:@"Updating ‘%s’…", sources[i]->name().c_str()];
				bundles_db::update(sources[i], request.thread_progress, i / (double)sources.size(), (i + 1) / (double)sources.size());
			}

			double totalSize = 0;
			iterate(bundle, bundles)
				totalSize += (*bundle)->size();

			for(size_t size = 0, i = 0; i < bundles.size(); ++i)
			{
				self.threadProgress     = size / totalSize;
				self.threadActivityText = [NSString stringWithFormat:@"%@Installing ‘%s’…", (bundles.size() > 1 ? [NSString stringWithFormat:@"%zu/%zu: ", i+1, bundles.size()] : @""), bundles[i]->name().c_str()];
				bundles_db::update(bundles[i], kInstallDirectory, request.thread_progress, size / totalSize, (size + bundles[i]->size()) / totalSize);
				size += bundles[i]->size();
			}

			self.threadProgress = 1;
			if(bundles.size() == 0)
				self.threadActivityText = @"";
			else if(bundles.size() == 1)
				self.threadActivityText = [NSString stringWithFormat:@"Installed ‘%s’.", bundles.back()->name().c_str()];
			else
				self.threadActivityText = [NSString stringWithFormat:@"Installed %zu bundles.", bundles.size()];
		}
		return true;
	}

	void background_task_t::handle_reply (bool result)
	{
		delete this;
	}
}

static BundlesManager* SharedInstance;

@implementation BundlesManager
{
	std::vector<bundles_db::source_ptr> sourceList;
	std::vector<bundles_db::bundle_ptr> bundlesIndex;

	NSUInteger scheduledTasks;
	std::set<oak::uuid_t> installing;
}

+ (BundlesManager*)sharedInstance
{
	return SharedInstance ?: [self new];
}

- (id)init
{
	if(SharedInstance)
	{
	}
	else if(self = SharedInstance = [super init])
	{
		sourceList   = bundles_db::sources();
		bundlesIndex = bundles_db::index(kInstallDirectory);

		[self updateSources:nil];
		[NSTimer scheduledTimerWithTimeInterval:4*60*60 target:self selector:@selector(updateSources:) userInfo:nil repeats:YES];
	}
	return SharedInstance;
}

- (void)updateProgress:(NSTimer*)aTimer
{
	self.progress     = self.threadProgress;
	self.activityText = self.threadActivityText;
}

- (void)didStartThreadActivity:(id)sender
{
	if(++scheduledTasks == 1)
	{
		self.progressTimer = [NSTimer scheduledTimerWithTimeInterval:0.04 target:self selector:@selector(updateProgress:) userInfo:nil repeats:YES];
		self.isBusy        = YES;
	}
}

- (void)didFinishActivityForSources:(std::vector<bundles_db::source_ptr> const&)someSources bundles:(std::vector<bundles_db::bundle_ptr> const&)someBundles
{
	if(--scheduledTasks == 0)
	{
		[self.progressTimer invalidate];
		[self updateProgress:nil];
		self.progressTimer = nil;
		self.progress      = 0;
		self.isBusy        = NO;

		[self performSelector:@selector(setActivityText:) withObject:nil afterDelay:5];
	}

	if(!someSources.empty())
	{
		bundlesIndex = bundles_db::index(kInstallDirectory);
		[self updateBundles:nil];
	}
	else if(!someBundles.empty())
	{
		iterate(bundle, someBundles)
			installing.erase((*bundle)->uuid());
		bundles_db::save_index(bundlesIndex, kInstallDirectory);
	}

	[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];
}

- (void)updateSources:(id)sender
{
	std::vector<bundles_db::source_ptr> sources;
	iterate(source, sourceList)
	{
		static double const kPollInterval = 3*60*60;
		if(!(*source)->disabled() && (*source)->needs_update(kPollInterval))
			sources.push_back(*source);
	}

	if(!sources.empty())
	{
		if(network::can_reach_host("api.textmate.org"))
			new background_task_t(sources, std::vector<bundles_db::bundle_ptr>(), self, &_threadProgress);
	}
	else
	{
		NSDate* earliest = [NSDate distantFuture];
		iterate(source, sourceList)
		{
			NSDate* date = (NSDate*)CFBridgingRelease(CFDateCreate(kCFAllocatorDefault, (*source)->last_check().value()));
			earliest = [date earlierDate:earliest];
		}
		self.activityText = [NSString stringWithFormat:@"Last check: %@", [earliest humanReadableTimeElapsed]];
	}
}

- (void)updateBundles:(id)sender
{
	std::set<bundles_db::bundle_ptr> bundles;
	iterate(installedBundle, bundlesIndex)
	{
		if(!(*installedBundle)->has_update())
			continue;

		citerate(bundle, bundles_db::dependencies(bundlesIndex, *installedBundle, false, false))
		{
			if((*bundle)->has_update() || !(*bundle)->installed())
				bundles.insert(*bundle);
		}
	}

	if(!bundles.empty())
		new background_task_t(std::vector<bundles_db::source_ptr>(), std::vector<bundles_db::bundle_ptr>(bundles.begin(), bundles.end()), self, &_threadProgress);
}

- (void)installBundle:(bundles_db::bundle_ptr const&)aBundle
{
	std::vector<bundles_db::bundle_ptr> bundles;
	citerate(bundle, bundles_db::dependencies(bundlesIndex, aBundle, true, false))
	{
		if(installing.find((*bundle)->uuid()) == installing.end())
		{
			bundles.push_back(*bundle);
			installing.insert((*bundle)->uuid());
		}
	}

	if(!bundles.empty())
	{
		[[NSNotificationCenter defaultCenter] postNotificationName:BundlesManagerBundlesDidChangeNotification object:self];
		new background_task_t(std::vector<bundles_db::source_ptr>(), bundles, self, &_threadProgress);
	}
}

- (void)uninstallBundle:(bundles_db::bundle_ptr const&)aBundle
{
	bundles_db::uninstall(aBundle);
	bundles_db::save_index(bundlesIndex, kInstallDirectory);

	self.activityText = [NSString stringWithFormat:@"Uninstalled ‘%s’.", aBundle->name().c_str()];
	[self performSelector:@selector(setActivityText:) withObject:nil afterDelay:5];
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
