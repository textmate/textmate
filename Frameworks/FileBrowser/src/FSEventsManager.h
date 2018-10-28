@interface FSEventsManager : NSObject
+ (instancetype)sharedInstance;

- (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)())handler;
- (id)addObserverToDirectoryAtURL:(NSURL*)url observeSubdirectories:(BOOL)flag usingBlock:(void(^)())handler;
- (void)removeObserver:(id)someObserver;

- (void)reloadDirectoryAtURL:(NSURL*)url;
@end
