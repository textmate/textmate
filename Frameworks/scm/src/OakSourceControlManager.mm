#import "OakSourceControlManager.h"
#import "drivers/api.h"
#import "snapshot.h"
#import "fs_events.h"
#import <OakFoundation/OakFoundation.h>
#import <ns/ns.h>

namespace scm
{
	driver_t* git_driver ();
	driver_t* hg_driver ();
	driver_t* p4_driver ();
	driver_t* svn_driver ();

} /* scm */

static std::vector<scm::driver_t*> const& drivers ()
{
	static auto const res = new std::vector<scm::driver_t*>{ scm::git_driver(), scm::hg_driver(), scm::p4_driver(), scm::svn_driver() };
	return *res;
}

@class OakRepository;

@interface OakRepositoryObserver : NSObject
@property (nonatomic, weak) OakRepository* repository;
@property (nonatomic) NSString* identifier;
@property (nonatomic) NSString* path;
@property (nonatomic) void(^callback)(NSString* path, scm::status::type status);
@end

@implementation OakRepositoryObserver
@end

@interface OakRepository : NSObject
{
	scm::driver_t const* _driver;
	fs::snapshot_t _fs_snapshot;
	std::unique_ptr<scm::watcher_t> _watcher;

	std::map<std::string, std::string> _variables;
	std::map<std::string, scm::status::type> _status_map;
	NSMutableDictionary<NSString*, OakRepositoryObserver*>* _observers;

	NSUInteger _nextToken;

	BOOL _isFetching;
	BOOL _shouldFetch;
	NSDate* _lastFetch;
}
@property (nonatomic) BOOL disabled;
@property (nonatomic) NSString* path;
@end

@implementation OakRepository
- (id)initWithPath:(NSString*)aPath driver:(scm::driver_t const*)aDriver
{
	if((self = [super init]))
	{
		// TODO Check if SCM support is disabled
		_path      = aPath;
		_driver    = aDriver;
		_observers = [NSMutableDictionary dictionary];
		_lastFetch = [NSDate distantPast];

		__weak OakRepository* weakSelf = self;
		_watcher = std::make_unique<scm::watcher_t>(to_s(_path), [weakSelf](std::set<std::string> const& changes){
			[weakSelf didChangePaths:changes];
		});

		[self didChangePaths:{ }];
	}
	return self;
}

- (void)didChangePaths:(std::set<std::string> const&)changedPaths
{
	if(_isFetching)
	{
		_shouldFetch = YES;
		return;
	}

	NSTimeInterval delayInSeconds = 2+[_lastFetch timeIntervalSinceNow];
	_isFetching = YES;
	[self performSelector:@selector(fetchSCMInfo:) withObject:self afterDelay:MAX(0, delayInSeconds)];
}

- (void)fetchSCMInfo:(id)sender
{
	CFRunLoopRef currentRunLoop = CFRunLoopGetCurrent();
	std::string const path = to_s(_path);
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		// TODO _driver->may_touch_filesystem() || info->_fs_snapshot != fs::snapshot_t(info->_root_path)
		// auto const snapshot  = _driver->may_touch_filesystem() ? fs::snapshot_t(path) : fs::snapshot_t();
		// auto const variables = _driver->variables(path);
		auto const status    = _driver->status(path);
		CFRunLoopPerformBlock(currentRunLoop, kCFRunLoopCommonModes, ^{
			[self setSCMInfo:status];

			_isFetching = NO;
			_lastFetch = [NSDate date];
			if(_shouldFetch)
			{
				_shouldFetch = NO;
				[self didChangePaths:{ }];
			}
		});
		CFRunLoopWakeUp(currentRunLoop);
	});
}

- (void)setSCMInfo:(std::map<std::string, scm::status::type> const&)statusMap
{
	for(OakRepositoryObserver* observer in [_observers allValues])
	{
		auto oldIter = _status_map.find(to_s(observer.path));
		auto newIter = statusMap.find(to_s(observer.path));
		auto oldStatus = oldIter != _status_map.end() ? oldIter->second : scm::status::unknown;
		auto newStatus = newIter != statusMap.end() ? newIter->second : scm::status::unknown;

		if(oldStatus != newStatus)
			observer.callback(observer.path, newStatus);
	}

	_status_map = statusMap;
}

- (scm::status::type)statusForFile:(NSString*)aPath
{
	auto it = _status_map.find(to_s(aPath));
	return it != _status_map.end() ? it->second : scm::status::unknown;
}

- (OakRepositoryObserver*)addObserverForPath:(NSString*)aPath handler:(void(^)(NSString* path, scm::status::type status))handler
{
	OakRepositoryObserver* observer = [[OakRepositoryObserver alloc] init];
	observer.repository = self;
	observer.identifier = [NSString stringWithFormat:@"%ld", _nextToken++];
	observer.path = aPath;
	observer.callback = handler;
	_observers[observer.identifier] = observer;
	return observer;
}

- (void)removeObserver:(NSString*)identifier
{
	[_observers removeObjectForKey:identifier];
}
@end

// ===========================
// = OakSourceControlManager =
// ===========================

@interface OakSourceControlManager ()
{
	NSMutableDictionary<NSString*, OakRepository*>* _repositories;
}
@end

@implementation OakSourceControlManager
+ (instancetype)sharedInstance
{
	static OakSourceControlManager* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		_repositories = [NSMutableDictionary dictionary];
	}
	return self;
}

- (OakRepository*)repositoryForPath:(NSString*)aPath
{
	// TODO Need to account for /path/to/repos/vendor/submodule/file
	for(NSString* cwd = aPath; OakNotEmptyString(cwd) && [cwd isEqualToString:@"/"] == NO; cwd = [cwd stringByDeletingLastPathComponent])
	{
		if(OakRepository* res = _repositories[cwd])
			return res;

		for(scm::driver_t* driver : drivers())
		{
			if(driver && driver->has_info_for_directory(to_s(cwd)))
			{
				OakRepository* res = [[OakRepository alloc] initWithPath:cwd driver:driver];
				_repositories[cwd] = res;
				return res;
			}
		}
	}
	return nil;
}

- (NSString*)rootForPath:(NSString*)aPath
{
	return [self repositoryForPath:aPath].path;
}

- (NSString*)scmNameForPath:(NSString*)aPath
{
	return nil;
}

- (NSString*)branchNameForPath:(NSString*)aPath
{
	return nil;
}

- (scm::status::type)statusForFile:(NSString*)aPath
{
	if(OakRepository* repository = [self repositoryForPath:aPath])
		return [repository statusForFile:aPath];
	return scm::status::unknown;
}

- (OakRepositoryObserver*)addObserverForPath:(NSString*)aPath handler:(void(^)(NSString* path, scm::status::type status))handler
{
	if(OakRepository* repository = [self repositoryForPath:aPath])
		return [repository addObserverForPath:aPath handler:handler];
	return nil;
}

- (void)removeObserver:(OakRepositoryObserver*)observerInfo
{
	[observerInfo.repository removeObserver:observerInfo.identifier];
}
@end
