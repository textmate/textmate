@interface FSEventsManager : NSObject
@property (class, readonly) FSEventsManager* sharedInstance;

- (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(NSURL*))handler;
- (id)addObserverToDirectoryAtURL:(NSURL*)url observeSubdirectories:(BOOL)flag usingBlock:(void(^)(NSURL*))handler;
- (void)removeObserver:(id)someObserver;

- (void)reloadDirectoryAtURL:(NSURL*)url;
@end
