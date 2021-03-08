@interface OakOpenWithApplicationInfo : NSObject
@property (nonatomic, readonly) NSURL* URL;
@property (nonatomic, readonly) NSString* bundleIdentifier;
@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) NSString* version;
@property (nonatomic, readonly) NSString* displayName;

@property (nonatomic, readonly, getter = isDefaultApplication) BOOL defaultApplication;
@property (nonatomic, readonly, getter = hasMultipleVersions)  BOOL multipleVersions;
@property (nonatomic, readonly, getter = hasMultipleCopies)    BOOL multipleCopies;
@end

@interface OakOpenWithMenuDelegate : NSObject <NSMenuDelegate>
- (instancetype)initWithDocumentURLs:(NSArray<NSURL*>*)someDocumentURLs;
- (void)openDocumentURLs:(NSArray<NSURL*>*)documentURLs withApplicationURL:(NSURL*)applicationURL;
@property (nonatomic, readonly) NSArray<NSURL*>* documentURLs;
@property (nonatomic, readonly) NSArray<OakOpenWithApplicationInfo*>* applications;
@end
