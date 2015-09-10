extern NSString* const FSItemDidReloadNotification;

@class FSItem;

@interface FSDataSource : NSObject <NSOutlineViewDataSource>
+ (NSArray*)sortArray:(NSArray*)anArray usingOptions:(NSUInteger)someOptions;

@property (nonatomic) FSItem* rootItem;
- (void)reloadItem:(FSItem*)anItem completionHandler:(void(^)(NSArray*))block;
- (BOOL)unloadItem:(FSItem*)anItem;
- (NSArray*)expandedURLs;
@end

static NSUInteger const kFSDataSourceOptionGroupsFirst   = (1 << 0);
static NSUInteger const kFSDataSourceOptionIncludeHidden = (1 << 1);
static NSUInteger const kFSDataSourceOptionShowExtension = (1 << 2);

FSDataSource* DataSourceForURL (NSURL* anURL, NSUInteger someOptions = kFSDataSourceOptionGroupsFirst);
