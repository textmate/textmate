#import "SCMManager.h"
#import "FSEventsManager.h"
#import "drivers/api.h"
#import <scm/src/scm.h>
#import <ns/src/ns.h>
#import <TMFileReference/src/TMFileReference.h>

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
	NSTimer* _updateTimer;
	NSDate* _noUpdateBefore;
	NSMutableSet<TMFileReference*>* _fileReferences;
}
@property (nonatomic, readwrite) std::map<std::string, scm::status::type> status;
@property (nonatomic, readwrite) NSDictionary<NSString*, NSString*>* variables;
@property (nonatomic, readonly) scm::driver_t const* driver;
@property (nonatomic, readonly) NSMutableArray<SCMRepositoryObserver*>* observers;
@property (nonatomic) id fsEventsObserver;
- (instancetype)initWithURL:(NSURL*)url driver:(scm::driver_t const*)driver;
- (scm::status::type)SCMStatusForURL:(NSURL*)url;
- (SCMRepositoryObserver*)addObserver:(void(^)(SCMRepository*))handler;
- (void)removeObserver:(SCMRepositoryObserver*)observer;
@end

@class SCMDirectoryObserver;

@interface SCMDirectory : NSObject
@property (nonatomic, readonly) NSURL* URL;
@property (nonatomic, readonly) SCMRepository* repository;
@property (nonatomic, readonly) SCMRepositoryObserver* repositoryObserver;
@property (nonatomic, readonly) NSMutableArray<SCMDirectoryObserver*>* observers;
- (instancetype)initWithURL:(NSURL*)url;
- (SCMDirectoryObserver*)addObserver:(void(^)(SCMRepository*))handler;
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
@property (nonatomic, readonly) void(^handler)(SCMRepository*);
@property (nonatomic) SCMRepository* repository;
- (instancetype)initWithBlock:(void(^)(SCMRepository*))handler;
- (void)remove;
@end

@implementation SCMRepositoryObserver
- (instancetype)initWithBlock:(void(^)(SCMRepository*))handler
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
@property (nonatomic, readonly) void(^handler)(SCMRepository*);
@property (nonatomic) SCMDirectory* directory;
- (instancetype)initWithBlock:(void(^)(SCMRepository*))handler;
- (void)remove;
@end

@implementation SCMDirectoryObserver
- (instancetype)initWithBlock:(void(^)(SCMRepository*))handler
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
		_noUpdateBefore    = [NSDate distantPast];
		_observers         = [NSMutableArray array];

		if(_enabled == YES)
		{
			[self tryUpdateStatusInBackground];

			__weak SCMRepository* weakSelf = self;
			_fsEventsObserver = [FSEventsManager.sharedInstance addObserverToDirectoryAtURL:url observeSubdirectories:YES usingBlock:^(NSURL* url){
				_noUpdateBefore = [_noUpdateBefore laterDate:[NSDate dateWithTimeIntervalSinceNow:NSApp.isActive ? 0.5 : 3]];
				[weakSelf tryUpdateStatusInBackground];
			}];
		}

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidBecomeActive:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSApplicationDidBecomeActiveNotification object:NSApp];
	[FSEventsManager.sharedInstance removeObserver:_fsEventsObserver];
}

- (void)applicationDidBecomeActive:(NSNotification*)aNotification
{
	if(_updateTimer)
		[self updateStatusInBackground:nil];
}

- (void)tryUpdateStatusInBackground
{
	if(_updating)
	{
		_needsUpdate = YES;
		return;
	}

	NSTimeInterval delayUpdate = [_noUpdateBefore timeIntervalSinceNow];
	if(delayUpdate > 0)
	{
		[_updateTimer invalidate];
		_updateTimer = [NSTimer scheduledTimerWithTimeInterval:delayUpdate target:self selector:@selector(updateStatusInBackground:) userInfo:nil repeats:NO];
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

		NSMutableDictionary* variables = [NSMutableDictionary dictionary];
		for(auto pair : driver->variables(url.fileSystemRepresentation))
			variables[to_ns(pair.first)] = to_ns(pair.second);

		dispatch_async(dispatch_get_main_queue(), ^{
			[weakSelf updateStatus:status variables:variables];
		});
	});
}

- (void)updateStatus:(std::map<std::string, scm::status::type> const&)status variables:(NSDictionary<NSString*, NSString*>*)variables
{
	_status    = status;
	_variables = variables;
	_hasStatus = YES;

	NSMutableSet<TMFileReference*>* fileReferences = [NSMutableSet set];
	for(auto pair : _status)
	{
		if(pair.second != scm::status::none)
		{
			NSString* path = [NSFileManager.defaultManager stringWithFileSystemRepresentation:pair.first.data() length:pair.first.size()];
			TMFileReference* fileReference = [TMFileReference fileReferenceWithURL:[NSURL fileURLWithPath:path]];
			fileReference.SCMStatus = pair.second;
			[fileReferences addObject:fileReference];
			[_fileReferences removeObject:fileReference];
		}
	}

	for(TMFileReference* fileReference in _fileReferences)
		fileReference.SCMStatus = scm::status::none;
	_fileReferences = fileReferences;

	for(SCMRepositoryObserver* observer in [_observers copy])
		observer.handler(self);

	_updating       = NO;
	_noUpdateBefore = [_noUpdateBefore laterDate:[NSDate dateWithTimeIntervalSinceNow:1.5]];
	if(_needsUpdate)
		[self tryUpdateStatusInBackground];
}

- (scm::status::type)SCMStatusForURL:(NSURL*)url
{
	if(_hasStatus)
	{
		auto it = _status.find(url.fileSystemRepresentation);
		if(it != _status.end())
			return it->second;
	}
	return scm::status::unknown;
}

- (SCMRepositoryObserver*)addObserver:(void(^)(SCMRepository*))handler
{
	SCMRepositoryObserver* observer = [[SCMRepositoryObserver alloc] initWithBlock:handler];
	observer.repository = self;
	[_observers addObject:observer];

	if(_hasStatus)
		handler(self);

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
		_repositoryObserver = [_repository addObserver:^(SCMRepository* repository){
			for(SCMDirectoryObserver* observer in [weakSelf.observers copy])
				observer.handler(repository);
		}];
	}
	return self;
}

- (void)dealloc
{
	[_repository removeObserver:_repositoryObserver];
}

- (SCMDirectoryObserver*)addObserver:(void(^)(SCMRepository*))handler
{
	SCMDirectoryObserver* observer = [[SCMDirectoryObserver alloc] initWithBlock:handler];
	observer.directory = self;
	[_observers addObject:observer];

	if(_repository.hasStatus)
		handler(_repository);

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
		if([url getResourceValue:&isVolume forKey:NSURLIsVolumeKey error:nil] && isVolume.boolValue)
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
	return [[self directoryAtURL:url.URLByDeletingLastPathComponent] addObserver:^(SCMRepository* repository){
		scm::status::type newStatus = [repository SCMStatusForURL:url];
		if(oldStatus != newStatus)
			handler(oldStatus = newStatus);
	}];
}

- (id)addObserverToRepositoryAtURL:(NSURL*)url usingBlock:(void(^)(SCMRepository*))handler
{
	return [[self repositoryAtURL:url] addObserver:handler];
}

- (void)removeObserver:(id)someObserver
{
	[someObserver remove];
}
@end
