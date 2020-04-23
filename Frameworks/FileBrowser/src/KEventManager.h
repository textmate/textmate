@interface KEventManager : NSObject
@property (class, readonly) KEventManager* sharedInstance;

- (id)addObserverToItemAtURL:(NSURL*)url usingBlock:(void(^)(NSURL*, NSUInteger))handler;
- (void)removeObserver:(id)someObserver;

- (void)dumpNodes;
@end
