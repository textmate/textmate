#import <scm/src/status.h>

@interface SCMRepository : NSObject
@property (nonatomic, readonly) NSURL* URL;
@property (nonatomic, readonly) BOOL enabled;
@property (nonatomic, readonly) BOOL tracksDirectories;
@property (nonatomic, readonly) BOOL hasStatus;
@property (nonatomic, readonly) std::map<std::string, scm::status::type> status;
@property (nonatomic, readonly) NSDictionary<NSString*, NSString*>* variables;
@end

@interface SCMManager : NSObject
@property (class, readonly) SCMManager* sharedInstance;

- (id)addObserverToFileAtURL:(NSURL*)url usingBlock:(void(^)(scm::status::type))handler;
- (id)addObserverToRepositoryAtURL:(NSURL*)url usingBlock:(void(^)(SCMRepository*))handler;
- (void)removeObserver:(id)someObserver;

- (SCMRepository*)repositoryAtURL:(NSURL*)url;
@end
