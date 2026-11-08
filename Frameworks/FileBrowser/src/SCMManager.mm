#import "SCMManager.h"
#import <TMFileReference/TMFileReference.h>
#import <ns/ns.h>
#import <scm/scm.h>

@class SCMRepositoryObserver;

@interface SCMRepository ()
{
	NSMutableSet<TMFileReference *> *_fileReferences;
	scm::info_ptr _info;
}
@property (nonatomic, readwrite) std::map<std::string, scm::status::type> status;
@property (nonatomic, readwrite) NSDictionary<NSString *, NSString *> *variables;
@property (nonatomic, readonly) NSMutableArray<SCMRepositoryObserver *> *observers;
- (instancetype)initWithURL:(NSURL *)url;
- (scm::status::type)SCMStatusForURL:(NSURL *)url;
- (SCMRepositoryObserver *)addObserver:(void (^)(SCMRepository *))handler;
- (void)removeObserver:(SCMRepositoryObserver *)observer;
@end

@class SCMDirectoryObserver;

@interface SCMDirectory : NSObject
@property (nonatomic, readonly) NSURL *URL;
@property (nonatomic, readonly) SCMRepository *repository;
@property (nonatomic, readonly) SCMRepositoryObserver *repositoryObserver;
@property (nonatomic, readonly) NSMutableArray<SCMDirectoryObserver *> *observers;
- (instancetype)initWithURL:(NSURL *)url;
- (SCMDirectoryObserver *)addObserver:(void (^)(SCMRepository *))handler;
- (void)removeObserver:(SCMDirectoryObserver *)observer;
@end

@interface SCMManager ()
@property (nonatomic, readonly) NSMapTable<NSURL *, SCMRepository *> *repositories;
@property (nonatomic, readonly) NSMapTable<NSURL *, SCMDirectory *> *directories;
- (SCMDirectory *)directoryAtURL:(NSURL *)url;
@end

// ===========================================
// = Helper classes for observer identifiers =
// ===========================================

@interface SCMRepositoryObserver : NSObject
@property (nonatomic, readonly) void (^handler)(SCMRepository *);
@property (nonatomic) SCMRepository *repository;
- (instancetype)initWithBlock:(void (^)(SCMRepository *))handler;
- (void)remove;
@end

@implementation SCMRepositoryObserver
- (instancetype)initWithBlock:(void (^)(SCMRepository *))handler
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
@property (nonatomic, readonly) void (^handler)(SCMRepository *);
@property (nonatomic) SCMDirectory *directory;
- (instancetype)initWithBlock:(void (^)(SCMRepository *))handler;
- (void)remove;
@end

@implementation SCMDirectoryObserver
- (instancetype)initWithBlock:(void (^)(SCMRepository *))handler
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
- (instancetype)initWithURL:(NSURL *)url

{
	if(self = [super init])
	{
		_URL		  = url;
		_enabled	  = YES;
		_observers = [NSMutableArray array];
		_info		  = scm::info(url.fileSystemRepresentation);

		__weak SCMRepository *weakSelf = self;
		_info->push_callback(^(scm::info_t const &info) {
		  NSMutableDictionary *variables = [NSMutableDictionary dictionary];
		  for(auto const &pair : info.scm_variables())
			  variables[to_ns(pair.first)] = to_ns(pair.second);

		  if(SCMRepository *repository = weakSelf)
		  {
			  repository->_tracksDirectories = info.tracks_directories();
			  [repository updateStatus:info.status() variables:variables];
		  }
		});
	}
	return self;
}

- (void)dealloc
{
	if(_info)
		_info->pop_callback();
}

- (void)updateStatus:(std::map<std::string, scm::status::type> const &)status variables:(NSDictionary<NSString *, NSString *> *)variables
{
	_status	  = status;
	_variables = variables;
	_hasStatus = YES;

	NSMutableSet<TMFileReference *> *fileReferences = [NSMutableSet set];
	for(auto pair : _status)
	{
		if(pair.second != scm::status::none)
		{
			NSString *path						 = [NSFileManager.defaultManager stringWithFileSystemRepresentation:pair.first.data() length:pair.first.size()];
			TMFileReference *fileReference = [TMFileReference fileReferenceWithURL:[NSURL fileURLWithPath:path]];
			fileReference.SCMStatus			 = pair.second;
			[fileReferences addObject:fileReference];
			[_fileReferences removeObject:fileReference];
		}
	}

	for(TMFileReference *fileReference in _fileReferences)
		fileReference.SCMStatus = scm::status::none;
	_fileReferences = fileReferences;

	for(SCMRepositoryObserver *observer in [_observers copy])
		observer.handler(self);
}

- (scm::status::type)SCMStatusForURL:(NSURL *)url
{
	if(_hasStatus)
	{
		auto it = _status.find(url.fileSystemRepresentation);
		if(it != _status.end())
			return it->second;
	}
	return scm::status::unknown;
}

- (SCMRepositoryObserver *)addObserver:(void (^)(SCMRepository *))handler
{
	SCMRepositoryObserver *observer = [[SCMRepositoryObserver alloc] initWithBlock:handler];
	observer.repository				  = self;
	[_observers addObject:observer];

	if(_hasStatus)
		handler(self);

	return observer;
}

- (void)removeObserver:(SCMRepositoryObserver *)observer
{
	[_observers removeObject:observer];
	observer.repository = nil;
}
@end

@implementation SCMDirectory
- (instancetype)initWithURL:(NSURL *)url
{
	if(self = [self init])
	{
		_URL			= url;
		_repository = [SCMManager.sharedInstance repositoryAtURL:url];
		_observers	= [NSMutableArray array];

		__weak SCMDirectory *weakSelf = self;
		_repositoryObserver				= [_repository addObserver:^(SCMRepository *repository) {
			for(SCMDirectoryObserver *observer in [weakSelf.observers copy])
				observer.handler(repository);
		 }];
	}
	return self;
}

- (void)dealloc
{
	[_repository removeObserver:_repositoryObserver];
}

- (SCMDirectoryObserver *)addObserver:(void (^)(SCMRepository *))handler
{
	SCMDirectoryObserver *observer = [[SCMDirectoryObserver alloc] initWithBlock:handler];
	observer.directory				 = self;
	[_observers addObject:observer];

	if(_repository.hasStatus)
		handler(_repository);

	return observer;
}

- (void)removeObserver:(SCMDirectoryObserver *)observer
{
	[_observers removeObject:observer];
	observer.directory = nil;
}
@end

@implementation SCMManager
+ (instancetype)sharedInstance
{
	static SCMManager *sharedInstance = [self new];
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

- (SCMRepository *)repositoryAtURL:(NSURL *)url
{
	std::string const root = scm::root_for_path(url.fileSystemRepresentation);
	if(root == NULL_STR)
		return nil;

	NSURL *rootURL = [NSURL fileURLWithPath:to_ns(root)];
	if(SCMRepository *repository = [_repositories objectForKey:rootURL])
		return repository;

	SCMRepository *repository = [[SCMRepository alloc] initWithURL:rootURL];
	[_repositories setObject:repository forKey:rootURL];
	return repository;
}

- (SCMDirectory *)directoryAtURL:(NSURL *)url
{
	SCMDirectory *directory = [_directories objectForKey:url];
	if(!directory)
	{
		directory = [[SCMDirectory alloc] initWithURL:url];
		[_directories setObject:directory forKey:url];
	}
	return directory;
}

- (id)addObserverToFileAtURL:(NSURL *)url usingBlock:(void (^)(scm::status::type))handler
{
	__block scm::status::type oldStatus = scm::status::unknown;
	return [[self directoryAtURL:url.URLByDeletingLastPathComponent] addObserver:^(SCMRepository *repository) {
	  scm::status::type newStatus = [repository SCMStatusForURL:url];
	  if(oldStatus != newStatus)
		  handler(oldStatus = newStatus);
	}];
}

- (id)addObserverToRepositoryAtURL:(NSURL *)url usingBlock:(void (^)(SCMRepository *))handler
{
	return [[self repositoryAtURL:url] addObserver:handler];
}

- (void)removeObserver:(id)someObserver
{
	[someObserver remove];
}
@end
