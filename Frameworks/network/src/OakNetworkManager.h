NS_ASSUME_NONNULL_BEGIN

@interface OakDownloadManager : NSObject
@property (class, readonly) OakDownloadManager* sharedInstance;
@property (nonatomic) NSString* userAgentString;
- (void)downloadFileAtURL:(NSURL*)serverURL replacingFileAtURL:(NSURL*)localFileURL publicKeys:(NSDictionary<NSString*, NSString*>*)publicKeys completionHandler:(void(^)(BOOL wasUpdated, NSError* error))completionHandler;
- (id <NSProgressReporting>)downloadArchiveAtURL:(NSURL*)serverURL forReplacingURL:(nullable NSURL*)localURL publicKeys:(NSDictionary<NSString*, NSString*>*)publicKeys completionHandler:(void(^)(NSURL* extractedArchiveURL, NSError* error))completionHandler;
@end

NS_ASSUME_NONNULL_END
