#import "SCMManager.h"
#import <scm/scm.h>
#import <io/path.h>
#import <ns/ns.h>

@class SCMRepository;
@class SCMFileObserver;
@class SCMDirectoryObserver;

@interface SCMManager ()
@property (nonatomic, readonly) NSMutableDictionary<NSURL*, SCMRepository*>* repositories;
@end

@interface SCMRepository : NSObject
@property (nonatomic, readonly) NSURL* URL;
@property (nonatomic, readonly) NSMutableArray<SCMFileObserver*>* fileObservers;
@property (nonatomic, readonly) NSMutableArray<SCMDirectoryObserver*>* directoryObservers;
@property (nonatomic, readonly) scm::info_ptr scmInfo;
- (instancetype)initWithURL:(NSURL*)url;
- (void)addFileObserver:(SCMFileObserver*)observer;
- (void)addDirectoryObserver:(SCMDirectoryObserver*)observer;
@end

@interface SCMFileObserver : NSObject
@property (nonatomic, readonly) NSURL* fileURL;
@property (nonatomic, readonly) void(^handler)(scm::status::type);
@property (nonatomic) scm::status::type SCMStatus;
- (instancetype)initWithURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler;
@end

@interface SCMDirectoryObserver : NSObject
{
	std::map<std::string, scm::status::type> _matchingURLs;
}
@property (nonatomic, readonly) NSURL* directoryURL;
@property (nonatomic, readonly) void(^handler)(std::map<std::string, scm::status::type> const&);
@property (nonatomic, readonly) scm::status::type mask;
- (instancetype)initWithURL:(NSURL*)url mask:(scm::status::type)mask usingBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)updateMatchingURLs:(std::map<std::string, scm::status::type> const&)statusMap;
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
		_repositories = [NSMutableDictionary dictionary];
	}
	return self;
}

- (SCMRepository*)repositoryForDirectoryAtURL:(NSURL*)url
{
	SCMRepository* repository = _repositories[url];
	if(!repository)
	{
		repository = [[SCMRepository alloc] initWithURL:url];
		_repositories[url] = repository;
	}
	return repository;
}

- (id)addObserverToURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler
{
	SCMFileObserver* observer = [[SCMFileObserver alloc] initWithURL:url usingBlock:handler];
	[[self repositoryForDirectoryAtURL:url.URLByDeletingLastPathComponent] addFileObserver:observer];
	return observer;
}

- (id)addObserverForStatus:(scm::status::type)mask inDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	SCMDirectoryObserver* observer = [[SCMDirectoryObserver alloc] initWithURL:url mask:mask usingBlock:handler];
	[[self repositoryForDirectoryAtURL:url] addDirectoryObserver:observer];
	return observer;
}

- (void)removeObserver:(id)someObserver
{
	SCMRepository* repository;

	if([someObserver isKindOfClass:[SCMFileObserver class]])
	{
		SCMFileObserver* observer = someObserver;
		repository = _repositories[observer.fileURL.URLByDeletingLastPathComponent];
		[repository.fileObservers removeObject:observer];
	}
	else if([someObserver isKindOfClass:[SCMDirectoryObserver class]])
	{
		SCMDirectoryObserver* observer = someObserver;
		repository = _repositories[observer.directoryURL];
		[repository.directoryObservers removeObject:observer];
	}

	if(repository && repository.fileObservers.count == 0 && repository.directoryObservers.count == 0)
		_repositories[repository.URL] = nil;
}

- (NSArray<NSURL*>*)urlsWithStatus:(scm::status::type)statusMask inDirectoryAtURL:(NSURL*)directoryURL
{
	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	if(auto scmInfo = scm::info(directoryURL.fileSystemRepresentation))
	{
		std::string const dir = directoryURL.fileSystemRepresentation;
		for(auto pair : scmInfo->status())
		{
			if(!(pair.second & statusMask) || dir != path::parent(pair.first))
				continue;

			[res addObject:[NSURL fileURLWithPath:to_ns(pair.first) isDirectory:NO]];
		}
	}
	return res;
}
@end

@implementation SCMRepository
- (instancetype)initWithURL:(NSURL*)url
{
	if(self = [self init])
	{
		_URL                = url;
		_fileObservers      = [NSMutableArray array];
		_directoryObservers = [NSMutableArray array];

		if(_scmInfo = scm::info(url.fileSystemRepresentation))
		{
			__weak SCMRepository* weakSelf = self;
			_scmInfo->push_callback(^(scm::info_t const& info){
				for(SCMFileObserver* observer in weakSelf.fileObservers)
					observer.SCMStatus = info.status(observer.fileURL.fileSystemRepresentation);
				for(SCMDirectoryObserver* observer in weakSelf.directoryObservers)
					[observer updateMatchingURLs:info.status()];
			});
		}
	}
	return self;
}

- (void)dealloc
{
	if(_scmInfo)
		_scmInfo->pop_callback();
}

- (void)addFileObserver:(SCMFileObserver*)observer
{
	[_fileObservers addObject:observer];
	if(_scmInfo)
		observer.SCMStatus = _scmInfo->status(observer.fileURL.fileSystemRepresentation);
}

- (void)addDirectoryObserver:(SCMDirectoryObserver*)observer
{
	[_directoryObservers addObject:observer];
	if(_scmInfo)
		[observer updateMatchingURLs:_scmInfo->status()];
}
@end

@implementation SCMFileObserver
- (instancetype)initWithURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler
{
	if(self = [self init])
	{
		_fileURL = url;
		_handler = handler;
	}
	return self;
}

- (void)setSCMStatus:(scm::status::type)newStatus
{
	if(_SCMStatus == newStatus)
		return;
	_SCMStatus = newStatus;
	if(_handler)
		_handler(newStatus);
}
@end

@implementation SCMDirectoryObserver
- (instancetype)initWithURL:(NSURL*)url mask:(scm::status::type)mask usingBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler
{
	if(self = [super init])
	{
		_directoryURL = url;
		_mask         = mask;
		_handler      = handler;
	}
	return self;
}

- (void)updateMatchingURLs:(std::map<std::string, scm::status::type> const&)statusMap
{
	std::map<std::string, scm::status::type> matchingURLs;

	for(auto const& pair : statusMap)
	{
		if(pair.second & _mask)
			matchingURLs.insert(pair);
	}

	if(_matchingURLs != matchingURLs)
		_handler(_matchingURLs = matchingURLs);
}
@end
