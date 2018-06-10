@interface GenieDatabase : NSObject
@end

@interface GenieMediaDuration : GenieDatabase
+ (instancetype)sharedInstance;
- (NSTimeInterval)durationForPath:(NSString*)aPath;
- (void)obtainDurationForPath:(NSString*)aPath andCallback:(void(^)(NSTimeInterval))aCallback;
@end

@interface GenieFavoriteIcon : GenieDatabase
+ (instancetype)sharedInstance;
- (NSImage*)favoriteIconForURL:(NSURL*)aURL;
- (void)obtainFavoriteIconForURL:(NSURL*)aURL andCallback:(void(^)(NSImage*))aCallback;
@end
