#import "FFFolderMenu.h"
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <io/io.h>
#import <io/entries.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

static NSMutableArray* FoldersAtPath (NSString* folder)
{
	ASSERT(folder && [folder length] > 0);
	NSMutableArray* res = [NSMutableArray array];
	BOOL isDirectory = NO;
	if(![[NSFileManager defaultManager] fileExistsAtPath:folder isDirectory:&isDirectory] || isDirectory == NO)
		return res;

	std::string const startPath = to_s(folder);
	for(auto entry : path::entries(startPath, "*"))
	{
		struct stat buf;
		std::string const path = path::join(startPath, entry->d_name);
		if(entry->d_type == DT_DIR && lstat(path.c_str(), &buf) == 0)
		{
			if(!S_ISDIR(buf.st_mode) || (buf.st_flags & UF_HIDDEN))
				continue;

			NSString* folderPath = [NSString stringWithCxxString:path];
			if(![[NSWorkspace sharedWorkspace] isFilePackageAtPath:folderPath])
				[res addObject:folderPath];
		}
	}

	NSSortDescriptor* displayNameSort = [[NSSortDescriptor alloc] initWithKey:@"stringByDeletingPathExtension" ascending:YES selector:@selector(localizedStandardCompare:)];
	NSSortDescriptor* extensionSort   = [[NSSortDescriptor alloc] initWithKey:@"pathExtension" ascending:YES selector:@selector(compare:)];
	[res sortUsingDescriptors:@[ displayNameSort, extensionSort ]];
	return res;
}

@implementation FFFolderMenu
+ (instancetype)sharedInstance
{
	static FFFolderMenu* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem
{
	[[self sharedInstance] addFolderSubmenuToMenuItem:aMenuItem];
}

- (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem
{
	aMenuItem.submenu = [NSMenu new];
	aMenuItem.submenu.delegate = self;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	NSMenuItem* parentItem = [aMenu parentMenuItem];
	if([aMenu numberOfItems] != 0 || !parentItem)
		return;

	NSString* folder = [parentItem representedObject] ?: NSHomeDirectory();
	if([parentItem parentItem] == nil) // root menu, show parent folders
	{
		BOOL hasSubfolders = [FoldersAtPath(folder) count];
		for(NSString* path = folder; OakNotEmptyString(path); path = [path stringByDeletingLastPathComponent])
		{
			NSMenuItem* menuItem = [aMenu addItemWithTitle:[[NSFileManager defaultManager] displayNameAtPath:path] action:parentItem.action keyEquivalent:@""];
			[menuItem setTarget:parentItem.target];
			[menuItem setRepresentedObject:path];
			[menuItem setIconForFile:path];
			if(std::exchange(hasSubfolders, YES))
				[self addFolderSubmenuToMenuItem:menuItem];

			if([path isEqualToString:@"/"])
				break;
		}
	}
	else
	{
		for(NSString* path in FoldersAtPath(folder))
		{
			NSMenuItem* menuItem = [aMenu addItemWithTitle:[[NSFileManager defaultManager] displayNameAtPath:path] action:parentItem.action keyEquivalent:@""];
			[menuItem setTarget:parentItem.target];
			[menuItem setRepresentedObject:path];
			[menuItem setIconForFile:path];

			if([FoldersAtPath(path) count])
				[self addFolderSubmenuToMenuItem:menuItem];
		}
	}
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)menu forEvent:(NSEvent*)event target:(id*)target action:(SEL*)action
{
	return NO;
}
@end
