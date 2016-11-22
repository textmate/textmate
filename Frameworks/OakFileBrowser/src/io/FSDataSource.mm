#import "FSDataSource.h"
#import "FSItem.h"
#import "FSDirectoryDataSource.h"
#import "FSVolumesDataSource.h"
// #import "FSBundlesDataSource.h"
#import "FSSCMDataSource.h"
#import "FSSearchDataSource.h"
#import "FSXcodeProjectDataSource.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakFileManager.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <io/path.h>
#import <io/move_path.h>
#import <io/resource.h>
#import <oak/debug.h>

NSString* const FSItemDidReloadNotification = @"FSItemDidReloadNotification";

FSDataSource* DataSourceForURL (NSURL* anURL, NSUInteger someOptions)
{
	FSDataSource* res = nil;
	NSString* scheme = [anURL scheme];
	if([scheme isEqualToString:@"xcodeproj"])
		res = [[FSXcodeProjectDataSource alloc] initWithURL:anURL options:someOptions];
	else if([scheme isEqualToString:@"file"])
		res = [[FSDirectoryDataSource alloc] initWithURL:anURL options:someOptions];
	else if([scheme isEqualToString:@"computer"])
		res = [[FSVolumesDataSource alloc] initWithURL:anURL options:someOptions];
	else if([scheme isEqualToString:@"search"])
		res = [[FSSearchDataSource alloc] initWithURL:anURL options:someOptions];
	// else if([scheme isEqualToString:@"bundles"])
	// 	res = [[FSBundlesDataSource alloc] initWithURL:anURL options:someOptions];
	else if([scheme isEqualToString:@"scm"])
		res = [[FSSCMDataSource alloc] initWithURL:anURL options:someOptions];
	return res;
}

@implementation FSDataSource { OBJC_WATCH_LEAKS(FSDataSource); }
+ (NSArray*)sortArray:(NSArray*)anArray usingOptions:(NSUInteger)someOptions
{
	NSMutableArray* descriptors = [NSMutableArray array];
	if(someOptions & kFSDataSourceOptionGroupsFirst)
		[descriptors addObject:[[NSSortDescriptor alloc] initWithKey:@"sortAsFolder" ascending:NO]];
	[descriptors addObject:[[NSSortDescriptor alloc] initWithKey:@"displayName" ascending:YES selector:@selector(localizedStandardCompare:)]];
	return [anArray sortedArrayUsingDescriptors:descriptors];
}

- (void)reloadItem:(FSItem*)anItem completionHandler:(void(^)(NSArray*))block
{
	block(anItem.children);
}

- (BOOL)unloadItem:(FSItem*)anItem
{
	return NO;
}

- (NSArray*)expandedURLs
{
	return nil;
}

// =====================================
// = NSOutlineView Data Source Methods =
// =====================================

- (NSInteger)outlineView:(NSOutlineView*)anOutlineView numberOfChildrenOfItem:(FSItem*)item
{
	return [(item ?: self.rootItem).children count];
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView isItemExpandable:(FSItem*)item
{
	return !item.leaf;
}

- (id)outlineView:(NSOutlineView*)anOutlineView child:(NSInteger)childIndex ofItem:(FSItem*)item
{
	return [(item ?: self.rootItem).children objectAtIndex:childIndex];
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView writeItems:(NSArray*)items toPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray* urls  = [NSMutableArray array];
	NSMutableArray* names = [NSMutableArray array];
	NSMutableArray* paths = [NSMutableArray array];

	for(FSItem* item in items)
	{
		[urls addObject:item.url];
		[names addObject:item.displayName];
		if([item.url isFileURL])
			[paths addObject:[item.url path]];
	}

	BOOL dragPboard     = [[pboard name] isEqualToString:NSDragPboard];
	BOOL generalPboard  = [[pboard name] isEqualToString:NSGeneralPboard];
	BOOL servicesPboard = !dragPboard && !generalPboard;

	NSMutableArray* types = [NSMutableArray array];
	NSString* string = NULL;

	if(servicesPboard)
	{
		if([paths count])
			[types addObject:NSFilenamesPboardType];
		if(string = [paths lastObject])
			[types addObject:NSStringPboardType];
	}
	else // drag or general pasteboard
	{
		if([paths count])
			[types addObject:NSFilenamesPboardType];
		else if([urls count] == 1)
			[types addObject:NSURLPboardType];

		if(generalPboard)
		{
			string = [names componentsJoinedByString:@"\r"];
			if(OakNotEmptyString(string))
				[types addObject:NSStringPboardType];
		}
		else if(dragPboard)
		{
			for(NSString* path in paths)
			{
				if(path::is_text_clipping([path fileSystemRepresentation]))
				{
					if(string = [NSString stringWithCxxString:path::resource([path fileSystemRepresentation], typeUTF8Text, 256)])
					{
						[types addObject:NSStringPboardType];
						break;
					}
				}
			}
		}
	}

	[pboard declareTypes:types owner:nil];
	if([types containsObject:NSStringPboardType])
		[pboard setString:string forType:NSStringPboardType];
	if([types containsObject:NSFilenamesPboardType])
		[pboard setPropertyList:paths forType:NSFilenamesPboardType];
	if([types containsObject:NSURLPboardType])
		[[urls lastObject] writeToPasteboard:pboard];

	return [types count];
}

// ===================
// = Accepting Drops =
// ===================

- (NSDragOperation)outlineView:(NSOutlineView*)anOutlineView validateDrop:(id <NSDraggingInfo>)info proposedItem:(FSItem*)item proposedChildIndex:(NSInteger)childIndex
{
	if(item.leaf || ![(item ?: self.rootItem).url isFileURL])
		return NSDragOperationNone;

	[anOutlineView setDropItem:item dropChildIndex:NSOutlineViewDropOnItemIndex];

	NSPasteboard* pboard  = [info draggingPasteboard];
	NSArray* draggedPaths = [pboard propertyListForType:NSFilenamesPboardType];
	NSString* dropPath    = [(item ?: self.rootItem).url path];

	dev_t targetDevice = path::device([dropPath fileSystemRepresentation]);
	BOOL linkOperation = ([[NSApp currentEvent] modifierFlags] & NSControlKeyMask) == NSControlKeyMask;
	BOOL toggleOperation = ([[NSApp currentEvent] modifierFlags] & NSAlternateKeyMask) == NSAlternateKeyMask;

	// We accept the drop as long as long as it is valid for at least one of the items
	for(NSString* aPath in draggedPaths)
	{
		BOOL sameSource = (path::device([aPath fileSystemRepresentation]) == targetDevice);
		NSDragOperation operation = linkOperation ? NSDragOperationLink : ((sameSource != toggleOperation) ? NSDragOperationMove : NSDragOperationCopy);

		// Destination path must exist
		if(!path::exists([dropPath fileSystemRepresentation]))
			continue;

		// Can’t move into same location
		NSString* parentPath = [aPath stringByDeletingLastPathComponent];
		if(operation == NSDragOperationMove && [parentPath isEqualToString:dropPath])
			continue;

		return operation;
	}
	return NSDragOperationNone;
}

static NSDragOperation filter (NSDragOperation mask)
{
	return (mask & NSDragOperationMove) ? NSDragOperationMove : ((mask & NSDragOperationCopy) ? NSDragOperationCopy : ((mask & NSDragOperationLink) ? NSDragOperationLink : 0));
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView acceptDrop:(id <NSDraggingInfo>)info item:(FSItem*)item childIndex:(NSInteger)childIndex
{
	if(item.leaf || ![(item ?: self.rootItem).url isFileURL])
		return NO;

	std::string const dropPath = [[(item ?: self.rootItem).url path] fileSystemRepresentation];
	NSDragOperation const op = filter([info draggingSourceOperationMask]);
	if(op == 0)
		return fprintf(stderr, "Unsupported drag operation %02lx for %s\n", [info draggingSourceOperationMask], dropPath.c_str()), NO;

	for(NSString* path in [[info draggingPasteboard] propertyListForType:NSFilenamesPboardType])
	{
		std::string const src = [path fileSystemRepresentation];
		std::string const dst = op == NSDragOperationCopy && dropPath == path::parent(src) ? path::unique(src) : path::join(dropPath, path::name(src));

		if(src == dst)
			continue;

		if(path::exists(dst))
		{
			if(path::is_child(src, dst))
			{
				NSRunCriticalAlertPanel(@"Folder Exists!", @"Cannot replace folder “%@” with an item it contains.", @"OK", nil, nil, [NSString stringWithCxxString:path::display_name(dst)]);
				continue;
			}
			else
			{
				char const* opDescription = (op == NSDragOperationMove) ? "moving" : ((op == NSDragOperationCopy) ? "copying" : "linking to");
				int choice = NSRunAlertPanel(@"File Exists", @"An item named “%@” already exists in this location. Do you want to replace it with the one you’re %s?", @"Replace", @"Cancel", nil, [NSString stringWithCxxString:path::display_name(src)], opDescription);
				if(choice == NSAlertDefaultReturn) // "Replace"
					path::remove(dst);
				else if(choice == NSAlertAlternateReturn) // "Cancel"
					continue;
			}
		}

		OakFileManager* fm = [OakFileManager sharedInstance];
		if(op == NSDragOperationMove)
			[fm moveItemAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:src]] toURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]] view:anOutlineView];
		else if(op == NSDragOperationCopy)
			[fm copyItemAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:src]] toURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]] view:anOutlineView];
		else if(op == NSDragOperationLink)
			[fm createSymbolicLinkAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]] withDestinationURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:src]] view:anOutlineView];
	}
	return YES;
}

- (void)outlineView:(NSOutlineView*)anOutlineView draggedItems:(NSArray*)someItems endedWithOperation:(NSDragOperation)aDragOperation
{
	if(aDragOperation == NSDragOperationDelete)
	{
		for(FSItem* item in someItems)
		{
			if([item.url isFileURL])
				[[OakFileManager sharedInstance] trashItemAtURL:item.url view:anOutlineView];
		}
	}
}
@end
