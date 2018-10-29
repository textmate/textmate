#import "SCMManager.h"
#import "FSEventsManager.h"
#import "drivers/api.h"
#import <scm/scm.h>
#import <io/path.h>
#import <ns/ns.h>

namespace scm
{
	driver_t* git_driver ();
	driver_t* hg_driver ();
	driver_t* p4_driver ();
	driver_t* svn_driver ();
}

@class SCMRepositoryObserver;

@interface SCMRepository ()
{
	BOOL _needsUpdate;
	BOOL _updating;
	NSDate* _lastUpdate;
	NSTimer* _updateTimer;
}
@property (nonatomic, readwrite) std::map<std::string, scm::status::type> status;
@property (nonatomic, readwrite) std::map<std::string, std::string> variables;
@property (nonatomic, readonly) scm::driver_t const* driver;
@property (nonatomic, readonly) NSMutableArray<SCMRepositoryObserver*>* observers;
@property (nonatomic) id fsEventsObserver;
- (instancetype)initWithURL:(NSURL*)url driver:(scm::driver_t const*)driver;
- (SCMRepositoryObserver*)addObserver:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)removeObserver:(SCMRepositoryObserver*)observer;
@end

@class SCMDirectoryObserver;

@interface SCMDirectory : NSObject
@property (nonatomic, readonly) NSURL* URL;
@property (nonatomic, readonly) SCMRepository* repository;
@property (nonatomic, readonly) SCMRepositoryObserver* repositoryObserver;
@property (nonatomic, readonly) NSMutableArray<SCMDirectoryObserver*>* observers;
- (instancetype)initWithURL:(NSURL*)url;
- (SCMDirectoryObserver*)addObserver:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)removeObserver:(SCMDirectoryObserver*)observer;
@end

@interface SCMManager ()
@property (nonatomic, readonly) NSMapTable<NSURL*, SCMRepository*>* repositories;
@property (nonatomic, readonly) NSMapTable<NSURL*, SCMDirectory*>*  directories;
- (SCMDirectory*)directoryAtURL:(NSURL*)url;
@end

// ===========================================
// = Helper classes for observer identifiers =
// ===========================================

@interface SCMRepositoryObserver : NSObject
@property (nonatomic, readonly) void(^handler)(std::map<std::string, scm::status::type> const&);
@property (nonatomic) SCMRepository* repository;
- (instancetype)initWithBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)remove;
@end

@implementation SCMRepositoryObserver
- (instancetype)initWithBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	if(self = [super init])
		_handler = handler;
	return self;
}

- (void)remove
{
	[self.repository removeObserver:self];
}
@end

@interface SCMDirectoryObserver : NSObject
@property (nonatomic, readonly) void(^handler)(std::map<std::string, scm::status::type> const&);
@property (nonatomic) SCMDirectory* directory;
- (instancetype)initWithBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)remove;
@end

@implementation SCMDirectoryObserver
- (instancetype)initWithBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	if(self = [super init])
		_handler = handler;
	return self;
}

- (void)remove
{
	[self.directory removeObserver:self];
}
@end

// ===========================================

@implementation SCMRepository
- (instancetype)initWithURL:(NSURL*)url driver:(scm::driver_t const*)driver
{
	if(self = [super init])
	{
		_URL               = url;
		_driver            = driver;
		_enabled           = scm::scm_enabled_for_path(url.fileSystemRepresentation);
		_tracksDirectories = driver && driver->tracks_directories();
		_observers         = [NSMutableArray array];

		if(_enabled == YES)
		{
			[self tryUpdateStatusInBackground];

			__weak SCMRepository* weakSelf = self;
			_fsEventsObserver = [FSEventsManager.sharedInstance addObserverToDirectoryAtURL:url observeSubdirectories:YES usingBlock:^{
				[weakSelf tryUpdateStatusInBackground];
			}];
		}
	}
	return self;
}

- (void)dealloc
{
	[FSEventsManager.sharedInstance removeObserver:_fsEventsObserver];
}

- (void)tryUpdateStatusInBackground
{
	if(_updating)
	{
		_needsUpdate = YES;
	}
	else if(_lastUpdate && [[NSDate date] timeIntervalSinceDate:_lastUpdate] < 1.5)
	{
		if(!_updateTimer)
			_updateTimer = [NSTimer scheduledTimerWithTimeInterval:1.5 - [[NSDate date] timeIntervalSinceDate:_lastUpdate] target:self selector:@selector(updateStatusInBackground:) userInfo:nil repeats:NO];
	}
	else
	{
		[self updateStatusInBackground:nil];
	}
}

- (void)updateStatusInBackground:(id)sender
{
	[_updateTimer invalidate];
	_updateTimer = nil;
	_needsUpdate = NO;
	_updating    = YES;

	__weak SCMRepository* weakSelf = self;

	NSURL* url = _URL;
	scm::driver_t const* driver = _driver;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		std::map<std::string, scm::status::type> const status = driver->status(url.fileSystemRepresentation);
		std::map<std::string, std::string> const variables    = driver->variables(url.fileSystemRepresentation);
		dispatch_async(dispatch_get_main_queue(), ^{
			[weakSelf updateStatus:status variables:variables];
		});
	});
}

- (void)updateStatus:(std::map<std::string, scm::status::type> const&)status variables:(std::map<std::string, std::string> const&)variables
{
	_status    = status;
	_variables = variables;

	for(SCMRepositoryObserver* observer in [_observers copy])
		observer.handler(status);

	_updating   = NO;
	_lastUpdate = [NSDate date];
	if(_needsUpdate)
		[self tryUpdateStatusInBackground];
}

- (SCMRepositoryObserver*)addObserver:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	SCMRepositoryObserver* observer = [[SCMRepositoryObserver alloc] initWithBlock:handler];
	observer.repository = self;
	[_observers addObject:observer];

	if(_lastUpdate)
		handler(_status);

	return observer;
}

- (void)removeObserver:(SCMRepositoryObserver*)observer
{
	[_observers removeObject:observer];
	observer.repository = nil;
}
@end

@implementation SCMDirectory
- (instancetype)initWithURL:(NSURL*)url
{
	if(self = [self init])
	{
		_URL        = url;
		_repository = [SCMManager.sharedInstance repositoryAtURL:url];
		_observers  = [NSMutableArray array];

		__weak SCMDirectory* weakSelf = self;
		_repositoryObserver = [_repository addObserver:^(std::map<std::string, scm::status::type> const& status){
			for(SCMDirectoryObserver* observer in [weakSelf.observers copy])
				observer.handler(status);
		}];
	}
	return self;
}

- (void)dealloc
{
	[_repository removeObserver:_repositoryObserver];
}

- (SCMDirectoryObserver*)addObserver:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	SCMDirectoryObserver* observer = [[SCMDirectoryObserver alloc] initWithBlock:handler];
	observer.directory = self;
	[_observers addObject:observer];

	if(_repository)
		handler(_repository.status);

	return observer;
}

- (void)removeObserver:(SCMDirectoryObserver*)observer
{
	[_observers removeObject:observer];
	observer.directory = nil;
}
@end

@implementation SCMManager
+ (instancetype)sharedInstance
{
	static SCMManager* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_directories  = [NSMapTable strongToWeakObjectsMapTable];
		_repositories = [NSMapTable strongToWeakObjectsMapTable];
	}
	return self;
}

- (SCMRepository*)repositoryAtURL:(NSURL*)url
{
	static scm::driver_t* const drivers[] = { scm::git_driver(), scm::hg_driver(), scm::p4_driver(), scm::svn_driver() };

	while(url)
	{
		if(SCMRepository* repository = [_repositories objectForKey:url])
			return repository;

		for(scm::driver_t* driver : drivers)
		{
			if(driver && driver->has_info_for_directory(url.fileSystemRepresentation))
			{
				SCMRepository* repository = [[SCMRepository alloc] initWithURL:url driver:driver];
				[_repositories setObject:repository forKey:url];
				return repository;
			}
		}

		NSNumber* isVolume;
		if([url getResourceValue:&isVolume forKey:NSURLIsVolumeKey error:nil] && [isVolume boolValue])
			break;

		NSURL* parentURL;
		if(![url getResourceValue:&parentURL forKey:NSURLParentDirectoryURLKey error:nil] || [url isEqual:parentURL])
			break;

		url = parentURL;
	}
	return nil;
}

- (SCMDirectory*)directoryAtURL:(NSURL*)url
{
	SCMDirectory* directory = [_directories objectForKey:url];
	if(!directory)
	{
		directory = [[SCMDirectory alloc] initWithURL:url];
		[_directories setObject:directory forKey:url];
	}
	return directory;
}

- (id)addObserverToFileAtURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler
{
	__block scm::status::type oldStatus = scm::status::unknown;
	return [[self directoryAtURL:url.URLByDeletingLastPathComponent] addObserver:^(std::map<std::string, scm::status::type> const& status){
		auto it = status.find(url.fileSystemRepresentation);
		if(it != status.end() && it->second != oldStatus)
			handler(oldStatus = it->second);
	}];
}

- (id)addObserverToRepositoryAtURL:(NSURL*)url usingBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	return [[self repositoryAtURL:url] addObserver:handler];
}

- (void)removeObserver:(id)someObserver
{
	[someObserver remove];
}

- (NSArray<NSURL*>*)urlsWithStatus:(scm::status::type)statusMask inDirectoryAtURL:(NSURL*)directoryURL
{
	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	if(SCMRepository* repository = [self repositoryAtURL:directoryURL])
	{
		std::string const dir = directoryURL.fileSystemRepresentation;
		for(auto pair : repository.status)
		{
			if(!(pair.second & statusMask) || dir != path::parent(pair.first))
				continue;

			[res addObject:[NSURL fileURLWithPath:to_ns(pair.first) isDirectory:NO]];
		}
	}
	return res;
}
@end
