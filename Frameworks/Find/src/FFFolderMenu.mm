#import "FFFolderMenu.h"
#import <OakAppKit/src/NSMenuItem Additions.h>
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/NSString Additions.h>
#import <io/src/io.h>
#import <io/src/entries.h>
#import <ns/src/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

static NSMutableArray* FoldersAtPath (NSString* folder)
{
	ASSERT(folder && [folder length] > 0);
	NSMutableArray* res = [NSMutableArray array];
	BOOL isDirectory = NO;
	if(![NSFileManager.defaultManager fileExistsAtPath:folder isDirectory:&isDirectory] || isDirectory == NO)
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
			if(![NSWorkspace.sharedWorkspace isFilePackageAtPath:folderPath])
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

+ (void)addSubmenuForDirectoryAtPath:(NSString*)path toMenuItem:(NSMenuItem*)aMenuItem
{
	[FFFolderMenu.sharedInstance addSubmenuForDirectoryAtPath:path toMenuItem:aMenuItem];
}

- (void)addSubmenuForDirectoryAtPath:(NSString*)path toMenuItem:(NSMenuItem*)aMenuItem
{
	aMenuItem.representedObject = path;
	aMenuItem.submenu = [NSMenu new];
	aMenuItem.submenu.delegate = self;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	if(aMenu.numberOfItems > 0)
		return;

	NSMenuItem* parentItem = [aMenu.supermenu itemAtIndex:[aMenu.supermenu indexOfItemWithSubmenu:aMenu]];
	NSString* folder = [parentItem representedObject] ?: NSHomeDirectory();
	for(NSString* path in FoldersAtPath(folder))
	{
		NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSFileManager.defaultManager displayNameAtPath:path] action:parentItem.action keyEquivalent:@""];
		[menuItem setTarget:parentItem.target];
		[menuItem setIconForFile:path];
		[menuItem setRepresentedObject:path];

		if([FoldersAtPath(path) count])
			[self addSubmenuForDirectoryAtPath:path toMenuItem:menuItem];
	}

	if(![parentItem parentItem] && ![folder isEqualToString:@"/"]) // Add enclosing folders to root menu
	{
		if(aMenu.numberOfItems)
			[aMenu addItem:[NSMenuItem separatorItem]];
		[aMenu addItemWithTitle:@"Enclosing Folders" action:@selector(nop:) keyEquivalent:@""];

		BOOL immediateParent = YES;
		for(NSString* path = folder.stringByDeletingLastPathComponent; OakNotEmptyString(path); path = path.stringByDeletingLastPathComponent)
		{
			NSString* shortcut = immediateParent ? @"\uF700" : @"";
			SEL action = immediateParent ? @selector(goToParentFolder:) : parentItem.action;
			id target  = immediateParent ? nil : parentItem.target;

			NSMenuItem* menuItem = [aMenu addItemWithTitle:[NSFileManager.defaultManager displayNameAtPath:path] action:action keyEquivalent:shortcut];
			[menuItem setTarget:target];
			[menuItem setRepresentedObject:path];
			[menuItem setIconForFile:path];

			if([path isEqualToString:@"/"])
				break;

			immediateParent = NO;
		}
	}
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)menu forEvent:(NSEvent*)event target:(id*)target action:(SEL*)action
{
	return NO;
}
@end
