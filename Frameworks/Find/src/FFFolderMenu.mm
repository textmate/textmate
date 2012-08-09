#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>
#import <io/io.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import "FFFolderMenu.h"

static bool hide_mnt_point (std::string const& path, dev_t device, std::map<dev_t, bool>& cache)
{
	if(path == "/dev")
		return true;

	std::map<dev_t, bool>::iterator it = cache.find(device);
	if(it != cache.end())
		return it->second;

	struct statfs buf;
	if(statfs(path.c_str(), &buf) == 0)
		return cache.insert(std::make_pair(device, buf.f_flags & MNT_DONTBROWSE)).first->second;

	return false;
}

static NSMutableArray* folders_at_path (NSString* folder)
{
	ASSERT(folder && [folder length] > 0);
	NSMutableArray* res = [NSMutableArray array];

	struct dirent** entries;
	std::string const start_path = [folder UTF8String];
	int num = scandir(start_path.c_str(), &entries, NULL, NULL);
	if(num > 0)
	{
		std::map<dev_t, bool> hidden_devices;
		for(int i = 0; i < num; ++i)
		{
			if(entries[i]->d_type != DT_DIR)
				continue;

			if(entries[i]->d_namlen == 0 || entries[i]->d_name[0] == '.')
				continue;

			std::string const& path = path::join(start_path, entries[i]->d_name);

			struct stat buf;
			if(lstat(path.c_str(), &buf) == 0)
			{
				if((buf.st_flags & UF_HIDDEN) || (S_ISDIR(buf.st_mode) && hide_mnt_point(path, buf.st_dev, hidden_devices)))
					continue;

				if(!S_ISDIR(buf.st_mode)) // TODO we can just use st_dev rather than statfs() for each dir
					continue;

				NSString* folderPath = [NSString stringWithCxxString:path];
				if(![[NSWorkspace sharedWorkspace] isFilePackageAtPath:folderPath])
					[res addObject:folderPath];
			}
		}

		while(num > 0)
			free(entries[--num]);
		free(entries);
	}
	NSSortDescriptor* displayNameSort = [[[NSSortDescriptor alloc] initWithKey:@"stringByDeletingPathExtension" ascending:YES selector:@selector(displayNameCompare:)] autorelease];
	NSSortDescriptor* extensionSort   = [[[NSSortDescriptor alloc] initWithKey:@"pathExtension" ascending:YES selector:@selector(displayNameCompare:)] autorelease];
	[res sortUsingDescriptors:@[ displayNameSort, extensionSort ]];
	return res;
}

@interface FFFolderMenuItemData : NSObject
{
	OBJC_WATCH_LEAKS(FFFolderMenuItemData);

	NSString* folder;
	NSMutableSet* visitedFolders;
	id owner;
}
@property (nonatomic, retain) NSString* folder;
@property (nonatomic, retain) NSMutableSet* visitedFolders;
@property (nonatomic, retain) id owner;

+ (id)dataWithFolder:(NSString*)folder visitedFolders:(NSMutableSet*)visitedFolders owner:(id)owner;
- (id)dataByMovingIntoFolder:(NSString*)folder;
@end

@implementation FFFolderMenuItemData
@synthesize folder, visitedFolders, owner;

+ (id)dataWithFolder:(NSString*)folder visitedFolders:(NSMutableSet*)visitedFolders owner:(id)owner;
{
	FFFolderMenuItemData* data = [[self new] autorelease];
	data.folder              = folder;
	data.visitedFolders      = visitedFolders;
	data.owner               = owner;
	return data;
}

- (id)dataByMovingIntoFolder:(NSString*)aFolder;
{
	FFFolderMenuItemData* data = [[[self class] new] autorelease];
	data.folder              = aFolder;
	data.visitedFolders      = [[visitedFolders mutableCopy] autorelease];
	[data.visitedFolders addObject:self.folder];
	data.owner               = self.owner;
	return data;
}

- (void)dealloc
{
	self.folder         = nil;
	self.visitedFolders = nil;
	self.owner          = nil;
	[super dealloc];
}
@end

@implementation FFFolderMenu
static FFFolderMenu* SharedInstance;

+ (FFFolderMenu*)sharedInstance
{
	return SharedInstance ?: [[FFFolderMenu new] autorelease];
}

- (id)init
{
	if(SharedInstance)
			[self release];
	else	self = SharedInstance = [[super init] retain];
	return SharedInstance;
}

+ (BOOL)hasFoldersAtPath:(NSString*)path
{
	return [folders_at_path(path) count] > 0;
}

+ (void)addFolderMenuAtPath:(NSString*)path toMenuItem:(NSMenuItem*)item withOwner:(id)owner;
{
	ASSERT(path);
	ASSERT(item);
	ASSERT(owner);

	[item setRepresentedObject:[FFFolderMenuItemData dataWithFolder:path visitedFolders:[NSMutableSet set] owner:owner]];

	NSMenu* submenu = [[NSMenu new] autorelease];
	[submenu setAutoenablesItems:NO];
	[submenu setDelegate:[FFFolderMenu sharedInstance]];
	[item setSubmenu:submenu];
}

- (void)selectFolder:(id)sender
{
	FFFolderMenuItemData* data = [sender representedObject];
	[data.owner userDidSelectFolder:data.folder inMenu:[sender menu]];
}

- (NSMenuItem*)menuItemForPath:(NSString*)path parentData:(FFFolderMenuItemData*)data
{
	NSMenuItem* menuItem = [[[NSMenuItem alloc] initWithTitle:[[NSFileManager defaultManager] displayNameAtPath:path] action:@selector(selectFolder:) keyEquivalent:@""] autorelease];
	[menuItem setTarget:self];
	[menuItem setRepresentedObject:[data dataByMovingIntoFolder:path]];
	[menuItem setIconForFile:path];

	if([[self class] hasFoldersAtPath:path])
	{
		NSMenu* submenu = [[NSMenu new] autorelease];
		[submenu setAutoenablesItems:NO];
		[submenu setDelegate:[FFFolderMenu sharedInstance]];
		[menuItem setSubmenu:submenu];
	}
	return menuItem;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	NSMenuItem* superItem = [aMenu parentMenuItem];
	if([aMenu numberOfItems] == 0 && superItem)
	{
		FFFolderMenuItemData* data = [superItem representedObject];
		NSMutableArray* folders = folders_at_path(data.folder);

		for(NSString* path in folders)
			[aMenu addItem:[self menuItemForPath:path parentData:data]];

		NSString* parent = [data.folder stringByDeletingLastPathComponent];
		if(![data.visitedFolders containsObject:parent] && ![data.folder isEqualToString:@"/"] && [[NSFileManager defaultManager] isReadableFileAtPath:parent])
		{
			[aMenu insertItem:[self menuItemForPath:parent parentData:data] atIndex:0];
			[aMenu insertItem:[NSMenuItem separatorItem] atIndex:1];
			if([aMenu numberOfItems] == 2)
			{
				NSMenuItem* noSubfoldersItem = [[[NSMenuItem alloc] initWithTitle:@"No subfolders" action:NULL keyEquivalent:@""] autorelease];
				[noSubfoldersItem setEnabled:NO];
				[aMenu insertItem:noSubfoldersItem atIndex:2];
			}
		}
	}
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)menu forEvent:(NSEvent*)event target:(id*)target action:(SEL*)action
{
	return NO;
}
@end
