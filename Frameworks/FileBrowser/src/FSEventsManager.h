@interface FSEventsManager : NSObject
+ (instancetype)sharedInstance;

- (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(NSURL*))handler;
- (id)addObserverToDirectoryAtURL:(NSURL*)url observeSubdirectories:(BOOL)flag usingBlock:(void(^)(NSURL*))handler;
- (void)removeObserver:(id)someObserver;

- (void)reloadDirectoryAtURL:(NSURL*)url;
@end
