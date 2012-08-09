#import "FSVolumesDataSource.h"
#import "FSItem.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <oak/oak.h>

#if !defined(MAC_OS_X_VERSION_10_6) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6)
static NSString* const NSWorkspaceDidRenameVolumeNotification = @"NSWorkspaceDidRenameVolumeNotification";
#endif

@implementation FSVolumesDataSource
- (NSArray*)volumeList
{
	NSMutableArray* volumes = [NSMutableArray new];
	citerate(volume, path::volumes())
	{
		NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:*volume] isDirectory:YES];
		FSItem* item = [FSItem itemWithURL:url];
		item.target = url;
		item.leaf   = YES;
		[volumes addObject:item];
	}
	return [FSDataSource sortArray:volumes usingOptions:0];
}

- (void)workspaceDidChangeVolumeList:(NSNotification*)aNotification
{
	[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:self userInfo:@{ @"item" : self.rootItem, @"children" : [self volumeList], @"recursive" : YES_obj }];
}

- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidMountNotification        object:[NSWorkspace sharedWorkspace]];
		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidUnmountNotification      object:[NSWorkspace sharedWorkspace]];
		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(workspaceDidChangeVolumeList:) name:NSWorkspaceDidRenameVolumeNotification object:[NSWorkspace sharedWorkspace]];

		self.rootItem = [FSItem itemWithURL:anURL];
		self.rootItem.icon     = [NSImage imageNamed:NSImageNameComputer];
		self.rootItem.name     = [(NSString*)SCDynamicStoreCopyComputerName(NULL, NULL) autorelease];
		self.rootItem.children = [self volumeList];
	}
	return self;
}

- (void)dealloc
{
	[[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:self];
	[super dealloc];
}
@end
