@interface FSEventsManager : NSObject
+ (instancetype)sharedInstance;

- (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)())handler;
- (void)removeObserver:(id)someObserver;

- (void)reloadDirectoryAtURL:(NSURL*)url;
@end
