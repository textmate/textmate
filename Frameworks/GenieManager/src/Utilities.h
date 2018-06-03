@interface GenieMediaDuration : NSObject
+ (instancetype)sharedInstance;
- (NSTimeInterval)durationForPath:(NSString*)aPath;
- (void)obtainDurationForPath:(NSString*)aPath andCallback:(void(^)(NSTimeInterval))aCallback;
@end

@interface GenieFavoriteIcon : NSObject
+ (instancetype)sharedInstance;
- (NSImage*)imageForURL:(NSURL*)aURL;
- (void)obtainImageForURL:(NSURL*)aURL andCallback:(void(^)(NSImage*))aCallback;
@end
