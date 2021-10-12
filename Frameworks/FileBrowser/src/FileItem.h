extern NSURL* const kURLLocationComputer;
extern NSURL* const kURLLocationFavorites;

@class OakFinderTag;

@interface FileItem : NSObject <QLPreviewItem>
@property (nonatomic) NSURL* URL;

@property (nonatomic, readonly) NSURL* fileReferenceURL;
@property (nonatomic, readonly) NSURL* resolvedURL;
@property (nonatomic, readonly) NSURL* parentURL;
@property (nonatomic, readonly, getter = isDirectory) BOOL directory;

@property (nonatomic, readonly) NSString* displayName;

@property (nonatomic) NSString* localizedName;
@property (nonatomic) NSString* disambiguationSuffix;
@property (nonatomic) NSString* toolTip;

@property (nonatomic, readonly) BOOL canRename;
@property (nonatomic, readonly, getter = isApplication) BOOL application;

@property (nonatomic, getter = isMissing)          BOOL missing;
@property (nonatomic, getter = isHidden)           BOOL hidden;
@property (nonatomic, getter = hasHiddenExtension) BOOL hiddenExtension;
@property (nonatomic, getter = isSymbolicLink)     BOOL symbolicLink;
@property (nonatomic, getter = isPackage)          BOOL package;
@property (nonatomic, getter = isLinkToPackage)    BOOL linkToPackage;
@property (nonatomic, getter = isLinkToDirectory)  BOOL linkToDirectory;

@property (nonatomic) NSArray<OakFinderTag*>* finderTags;

@property (nonatomic) NSArray<FileItem*>* children;
@property (nonatomic) NSMutableArray<FileItem*>* arrangedChildren;

+ (instancetype)fileItemWithURL:(NSURL*)url;

+ (void)registerClass:(Class)klass forURLScheme:(NSString*)urlScheme;
+ (Class)classForURL:(NSURL*)url;

- (instancetype)initWithURL:(NSURL*)url;
- (void)updateFileProperties;
@end

@interface FileItem (Observer)
+ (id)addObserverToDirectoryAtURL:(NSURL*)url usingBlock:(void(^)(NSArray<NSURL*>*))handler;
+ (void)removeObserver:(id)someObserver;
@end
