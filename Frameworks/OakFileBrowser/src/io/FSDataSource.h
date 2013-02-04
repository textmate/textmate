extern NSString* const FSItemDidReloadNotification;

@class FSItem;

@interface FSDataSource : NSObject <NSOutlineViewDataSource>
+ (NSArray*)sortArray:(NSArray*)anArray usingOptions:(NSUInteger)someOptions;

@property (nonatomic, retain) FSItem* rootItem;
- (BOOL)reloadItem:(FSItem*)anItem; // Returns YES if reload happens in the background. An FSItemDidReloadNotification is posted when reload has completed.
- (BOOL)unloadItem:(FSItem*)anItem;
- (NSArray*)expandedURLs;
@end

static NSUInteger const kFSDataSourceOptionGroupsFirst   = (1 << 0);
static NSUInteger const kFSDataSourceOptionIncludeHidden = (1 << 1);
static NSUInteger const kFSDataSourceOptionSortByType    = (1 << 2);
static NSUInteger const kFSDataSourceOptionShowExtension = (1 << 3);

FSDataSource* DataSourceForURL (NSURL* anURL, NSUInteger someOptions = kFSDataSourceOptionGroupsFirst);
