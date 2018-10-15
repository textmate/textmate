#import <scm/status.h>

@interface SCMManager : NSObject
+ (instancetype)sharedInstance;

- (id)addObserverToURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler;
- (id)addObserverForStatus:(scm::status::type)mask inDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(std::map<std::string, scm::status::type> const&))handler;
- (void)removeObserver:(id)someObserver;

- (NSArray<NSURL*>*)urlsWithStatus:(scm::status::type)statusMask inDirectoryAtURL:(NSURL*)directoryURL;
@end
