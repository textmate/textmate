#import "FSVolumesDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <oak/oak.h>
#import <oak/debug.h>

@interface FSVolumeListItem : FSItem
@property (nonatomic, weak) FSDataSource* dataSource;
@end

@implementation FSVolumeListItem
- (id)initWithURL:(NSURL*)anURL dataSource:(FSDataSource*)aDataSource
{
	if((self = [super initWithURL:anURL]))
	{
		_dataSource = aDataSource;

		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidMountNotification        object:[NSWorkspace sharedWorkspace]];
		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidUnmountNotification      object:[NSWorkspace sharedWorkspace]];
		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidRenameVolumeNotification object:[NSWorkspace sharedWorkspace]];
	}
	return self;
}

- (void)dealloc
{
	[[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:self];
}

- (void)workspaceDidChangeVolumeList:(NSNotification*)aNotification
{
	[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:_dataSource userInfo:@{ @"item" : self }];
}

- (void)loadChildren:(FSDataSource*)dataSource completionHandler:(void(^)(NSArray*))block
{
	block([self volumeList]);
}

- (NSArray*)volumeList
{
	NSMutableArray* volumes = [NSMutableArray new];
	for(auto const& volume : path::volumes())
	{
		NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:volume] isDirectory:YES];
		FSItem* item = [FSItem itemWithURL:url];
		item.target = url;
		item.leaf   = YES;
		[volumes addObject:item];
	}
	return [FSDataSource sortArray:volumes usingOptions:0];
}
@end

@implementation FSVolumesDataSource { OBJC_WATCH_LEAKS(FSVolumesDataSource); }
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		self.rootItem = [[FSVolumeListItem alloc] initWithURL:anURL dataSource:self];
		self.rootItem.icon        = [NSImage imageNamed:NSImageNameComputer]; // FIXME Assigning to property of type OakFileIconImage
		self.rootItem.displayName = [[NSHost currentHost] localizedName];
	}
	return self;
}

- (void)reloadItem:(FSItem*)anItem completionHandler:(void(^)(NSArray*))block
{
	[(FSVolumeListItem*)anItem loadChildren:self completionHandler:block];
}
@end
