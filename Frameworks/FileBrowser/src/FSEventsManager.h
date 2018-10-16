@interface FSEventsManager : NSObject
+ (instancetype)sharedInstance;

- (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(NSArray<NSURL*>*))handler;
- (void)removeObserver:(id)someObserver;
@end
