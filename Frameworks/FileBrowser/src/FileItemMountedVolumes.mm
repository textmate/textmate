#import "FileItem.h"

@interface MountedVolumesObserver : NSObject
@property (nonatomic, readonly) void(^handler)(NSArray<NSURL*>*);
- (instancetype)initWithBlock:(void(^)(NSArray<NSURL*>*))handler;
@end

@interface MountedVolumesFileItem : FileItem
@end

@implementation MountedVolumesFileItem
+ (void)load
{
	[self registerClass:self forURLScheme:@"computer"];
}

+ (id)makeObserverForURL:(NSURL*)url usingBlock:(void(^)(NSArray<NSURL*>*))handler
{
	return [[MountedVolumesObserver alloc] initWithBlock:handler];
}

- (NSString*)localizedName
{
	return NSHost.currentHost.localizedName;
}
@end

// ============================
// = Mounted volumes observer =
// ============================

@implementation MountedVolumesObserver
- (instancetype)initWithBlock:(void(^)(NSArray<NSURL*>*))handler
{
	if(self = [super init])
	{
		_handler = handler;

		[NSWorkspace.sharedWorkspace.notificationCenter addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidMountNotification        object:NSWorkspace.sharedWorkspace];
		[NSWorkspace.sharedWorkspace.notificationCenter addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidUnmountNotification      object:NSWorkspace.sharedWorkspace];
		[NSWorkspace.sharedWorkspace.notificationCenter addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidRenameVolumeNotification object:NSWorkspace.sharedWorkspace];

		[self workspaceDidChangeVolumeList:nil];
	}
	return self;
}

- (void)dealloc
{
	[NSWorkspace.sharedWorkspace.notificationCenter removeObserver:self];
}

- (void)workspaceDidChangeVolumeList:(NSNotification*)aNotification
{
	NSArray<NSURL*>* volumeURLs = [NSFileManager.defaultManager mountedVolumeURLsIncludingResourceValuesForKeys:@[ NSURLLocalizedNameKey, NSURLEffectiveIconKey ] options:NSVolumeEnumerationSkipHiddenVolumes];
	_handler(volumeURLs);
}
@end
