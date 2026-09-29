#import "FileBrowserViewController.h"
#import "FileBrowserNotifications.h"
#import "FileItem.h"
#import <OakAppKit/src/OakSound.h>
#import <OakAppKit/src/NSAlert Additions.h>
#import <io/src/path.h>
#import <ns/src/ns.h>

@implementation FileBrowserViewController (DiskOperations)
- (NSArray<NSURL*>*)performOperation:(FBOperation)op withURLs:(NSDictionary<NSURL*, NSURL*>*)urls unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs
{
	NSMutableArray* srcURLs  = [NSMutableArray array];
	NSMutableArray* destURLs = [NSMutableArray array];
	[urls enumerateKeysAndObjectsUsingBlock:^(NSURL* srcURL, NSURL* destURL, BOOL* stop){
		[srcURLs addObject:srcURL];
		[destURLs addObject:destURL];
	}];
	return [self performOperation:op sourceURLs:srcURLs destinationURLs:destURLs unique:makeUnique select:selectDestinationURLs];
}

- (NSArray<NSURL*>*)performOperation:(FBOperation)op sourceURLs:(NSArray<NSURL*>*)srcURLs destinationURLs:(NSArray<NSURL*>*)destURLs unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs
{
	if(makeUnique)
		destURLs = [self uniqueDestinationURLs:destURLs];

	NSString* itemDescription = srcURLs.count == 1 ? [NSString stringWithFormat:@"“%@”", [NSFileManager.defaultManager displayNameAtPath:srcURLs.firstObject.path]] : [NSString stringWithFormat:@"%ld Items", srcURLs.count];

	NSMutableArray<NSURL*>* newSrcURLs  = [NSMutableArray array];
	NSMutableArray<NSURL*>* newDestURLs = [NSMutableArray array];

	BOOL forceFlag = NO;

	NSUInteger total = MAX(srcURLs.count, destURLs.count);
	for(NSUInteger i = 0; i < total; ++i)
	{
		NSURL* srcURL  = i < srcURLs.count  ? srcURLs[i].filePathURL  : nil;
		NSURL* destURL = i < destURLs.count ? destURLs[i].filePathURL : nil;

		NSError* error;
		BOOL res = [self performOperation:op sourceURL:srcURL destinationURL:&destURL force:forceFlag error:&error];

		if(!res)
		{
			if((op & (FBOperationLink|FBOperationCopy|FBOperationMove)) && [error.domain isEqualToString:NSCocoaErrorDomain] && error.code == NSFileWriteFileExistsError)
			{
				NSAlert* alert                = [[NSAlert alloc] init];
				alert.alertStyle              = NSAlertStyleCritical;
				alert.messageText             = [NSString stringWithFormat:@"Do you want to replace “%@”?", [NSFileManager.defaultManager displayNameAtPath:destURL.path]];
				alert.informativeText         = [NSString stringWithFormat:@"An item named “%@” already exists in the location that you are moving this item to.", [NSFileManager.defaultManager displayNameAtPath:destURL.path]];
				alert.suppressionButton.title = @"Replace All";
				alert.showsSuppressionButton  = i+1 < total;
				[alert addButtons:@"Replace", @"Stop", @"Skip", nil];

				switch([alert runModal])
				{
					case NSAlertFirstButtonReturn: // Replace
						forceFlag = alert.suppressionButton.state == NSControlStateValueOn;
						res = [self performOperation:op sourceURL:srcURL destinationURL:&destURL force:YES error:&error];
					break;

					case NSAlertSecondButtonReturn: // Stop
						error = nil;
						i = total;
					break;

					case NSAlertThirdButtonReturn: // Skip
						continue;
					break;
				}
			}
			else if(op == FBOperationTrash && [error.domain isEqualToString:NSCocoaErrorDomain] && error.code == NSFeatureUnsupportedError)
			{
				NSAlert* alert                = [[NSAlert alloc] init];
				alert.alertStyle              = NSAlertStyleCritical;
				alert.messageText             = [NSString stringWithFormat:@"Are you sure you want to delete “%@”?", [NSFileManager.defaultManager displayNameAtPath:srcURL.path]];
				alert.informativeText         = @"This item will be deleted immediately. You can’t undo this action.";
				alert.suppressionButton.title = @"Delete All";
				alert.showsSuppressionButton  = i+1 < total;
				[alert addButtons:@"Delete", @"Stop", @"Skip", nil];

				switch([alert runModal])
				{
					case NSAlertFirstButtonReturn: // Delete
						forceFlag = alert.suppressionButton.state == NSControlStateValueOn;
						res = [self performOperation:op sourceURL:srcURL destinationURL:&destURL force:YES error:&error];
					break;

					case NSAlertSecondButtonReturn: // Stop
						error = nil;
						i = total;
					break;

					case NSAlertThirdButtonReturn: // Skip
						continue;
					break;
				}
			}
		}

		if(res)
		{
			if(destURL)
			{
				if(srcURL)
					[newSrcURLs addObject:srcURL];
				[newDestURLs addObject:destURL];
			}
		}
		else if(error)
		{
			[self presentError:error];
		}
	}

	if(!newDestURLs.count)
		return nil;

	NSArray<FileItem*>* newItems;
	if(op & (FBOperationMove|FBOperationRename|FBOperationTrash))
		newItems = [self moveFromURLs:newSrcURLs toURLs:newDestURLs];
	else if(op & (FBOperationLink|FBOperationCopy|FBOperationDuplicate|FBOperationNewFile|FBOperationNewFolder))
		newItems = [self insertURLs:newDestURLs];

	if(newItems.count && selectDestinationURLs)
	{
		NSMutableIndexSet* newIndexes = [NSMutableIndexSet indexSet];
		for(FileItem* item in newItems)
		{
			NSInteger row = [self.outlineView rowForItem:item];
			if(row != -1)
				[newIndexes addIndex:row];
		}
		[self.outlineView selectRowIndexes:newIndexes byExtendingSelection:NO];
	}

	if(op & (FBOperationLink|FBOperationMove|FBOperationCopy|FBOperationDuplicate))
		OakPlayUISound(OakSoundDidMoveItemUISound);
	else if(op & (FBOperationTrash))
		OakPlayUISound(OakSoundDidTrashItemUISound);

	[[self.undoManager prepareWithInvocationTarget:self] undoOperation:op sourceURLs:(newSrcURLs.count ? newSrcURLs : nil) destinationURLs:newDestURLs select:selectDestinationURLs];

	switch(op)
	{
		case FBOperationLink:      self.undoManager.actionName = [NSString stringWithFormat:@"Create Link to %@", itemDescription];   break;
		case FBOperationCopy:      self.undoManager.actionName = [NSString stringWithFormat:@"Copy of %@", itemDescription];          break;
		case FBOperationDuplicate: self.undoManager.actionName = [NSString stringWithFormat:@"Duplicate %@", itemDescription];        break;
		case FBOperationMove:      self.undoManager.actionName = [NSString stringWithFormat:@"Move of %@", itemDescription];          break;
		case FBOperationRename:    self.undoManager.actionName = [NSString stringWithFormat:@"Rename %@", itemDescription];           break;
		case FBOperationTrash:     self.undoManager.actionName = [NSString stringWithFormat:@"Move of %@ to Trash", itemDescription]; break;
		case FBOperationNewFile:   self.undoManager.actionName = @"New File";   break;
		case FBOperationNewFolder: self.undoManager.actionName = @"New Folder"; break;
	}

	return newDestURLs;
}

- (BOOL)performOperation:(FBOperation)op sourceURL:(NSURL*)srcURL destinationURL:(NSURL**)destURL force:(BOOL)force error:(NSError**)error
{
	if(force && (op & (FBOperationLink|FBOperationCopy|FBOperationMove)) && [NSFileManager.defaultManager fileExistsAtPath:(*destURL).path])
	{
		if(![NSFileManager.defaultManager removeItemAtURL:*destURL error:error])
			return NO;
	}

	switch(op)
	{
		case FBOperationLink:
		{
			char const* src = srcURL.fileSystemRepresentation;
			char const* dst = (*destURL).URLByDeletingLastPathComponent.fileSystemRepresentation;
			if(path::device(src) == path::device(dst))
			{
				std::string target = path::relative_to(src, dst);
				return [NSFileManager.defaultManager createSymbolicLinkAtPath:(*destURL).path withDestinationPath:to_ns(target) error:error];
			}
			else
			{
				return [NSFileManager.defaultManager createSymbolicLinkAtURL:*destURL withDestinationURL:srcURL error:error];
			}
		}
		break;

		case FBOperationMove:
		case FBOperationRename:    return [NSFileManager.defaultManager moveItemAtURL:srcURL toURL:*destURL error:error];
		case FBOperationNewFile:   return [NSFileManager.defaultManager createFileAtPath:(*destURL).path contents:nil attributes:nil];
		case FBOperationNewFolder: return [NSFileManager.defaultManager createDirectoryAtURL:*destURL withIntermediateDirectories:NO attributes:nil error:error];

		case FBOperationCopy:
		case FBOperationDuplicate:
		{
			if(path::is_child((*destURL).fileSystemRepresentation, srcURL.fileSystemRepresentation))
			{
				*error = [NSError errorWithDomain:NSPOSIXErrorDomain code:ENOTSUP userInfo:nil];
				return NO;
			}

			if([NSFileManager.defaultManager copyItemAtURL:srcURL toURL:*destURL error:error])
			{
				if(op == FBOperationDuplicate)
					[NSNotificationCenter.defaultCenter postNotificationName:FileBrowserDidDuplicateNotification object:self userInfo:@{ FileBrowserURLDictionaryKey: @{ srcURL: *destURL } }];
				return YES;
			}
		}
		break;

		case FBOperationTrash:
		{
			[NSNotificationCenter.defaultCenter postNotificationName:FileBrowserWillDeleteNotification object:self userInfo:@{ FileBrowserPathKey: srcURL.path }];

			if([NSFileManager.defaultManager trashItemAtURL:srcURL resultingItemURL:destURL error:error])
				return YES;

			if(force && [(*error).domain isEqualToString:NSCocoaErrorDomain] && (*error).code == NSFeatureUnsupportedError)
				return [NSFileManager.defaultManager removeItemAtURL:srcURL error:error];

			return NO;
		}
		break;
	}
	return NO;
}

- (NSArray<NSURL*>*)uniqueDestinationURLs:(NSArray<NSURL*>*)urls
{
	NSMutableArray<NSURL*>* res = [NSMutableArray array];

	NSMutableSet<NSURL*>* existingURLs = [NSMutableSet set];
	for(NSURL* url in urls)
	{
		NSURL* destURL = url;

		NSString* base = destURL.lastPathComponent;

		NSInteger i = 1;
		while([existingURLs containsObject:destURL] || [NSFileManager.defaultManager fileExistsAtPath:destURL.path])
		{
			NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"^(.*?)(?: \\d+)?(\\.\\w+)?$" options:0 error:nil];
			NSString* name = [regex stringByReplacingMatchesInString:base options:0 range:NSMakeRange(0, base.length) withTemplate:[NSString stringWithFormat:@"$1 %ld$2", ++i]];
			destURL = [destURL.URLByDeletingLastPathComponent URLByAppendingPathComponent:name isDirectory:destURL.hasDirectoryPath];
		}
		[existingURLs addObject:destURL];
		[res addObject:destURL];
	}
	return res;
}

- (void)undoOperation:(FBOperation)op sourceURLs:(NSArray<NSURL*>*)srcURLs destinationURLs:(NSArray<NSURL*>*)destURLs select:(BOOL)selectDestinationURLs
{
	NSMutableArray* newSrcURLs  = [NSMutableArray array];
	NSMutableArray* newDestURLs = [NSMutableArray array];

	NSUInteger total = MAX(srcURLs.count, destURLs.count);
	for(NSUInteger i = 0; i < total; ++i)
	{
		NSURL* srcURL  = srcURLs[i];
		NSURL* destURL = destURLs[i];

		NSError* error;
		if([self undoOperation:op sourceURL:srcURL destinationURL:destURL error:&error])
		{
			if(srcURL)
				[newSrcURLs addObject:srcURL];
			if(destURL)
				[newDestURLs addObject:destURL];
		}
		else
		{
			[self presentError:error];
		}
	}

	if(op & (FBOperationMove|FBOperationRename|FBOperationTrash))
		[self moveFromURLs:newDestURLs toURLs:newSrcURLs];
	else if(op & (FBOperationLink|FBOperationCopy|FBOperationDuplicate|FBOperationNewFile|FBOperationNewFolder))
		[self removeURLs:newDestURLs];

	if(op & (FBOperationTrash|FBOperationMove))
		OakPlayUISound(OakSoundDidMoveItemUISound);
	else if(op & (FBOperationLink|FBOperationCopy|FBOperationDuplicate|FBOperationNewFile|FBOperationNewFolder))
		OakPlayUISound(OakSoundDidTrashItemUISound);

	[[self.undoManager prepareWithInvocationTarget:self] performOperation:op sourceURLs:newSrcURLs destinationURLs:newDestURLs unique:NO select:selectDestinationURLs];
}

- (BOOL)undoOperation:(FBOperation)op sourceURL:(NSURL*)srcURL destinationURL:(NSURL*)destURL error:(NSError**)error
{
	if(op & (FBOperationLink|FBOperationCopy|FBOperationDuplicate|FBOperationNewFile|FBOperationNewFolder))
	{
		[NSNotificationCenter.defaultCenter postNotificationName:FileBrowserWillDeleteNotification object:self userInfo:@{ FileBrowserPathKey: destURL.path }];
		return [NSFileManager.defaultManager removeItemAtURL:destURL error:error];
	}
	else if(op & (FBOperationMove|FBOperationRename|FBOperationTrash))
	{
		return [NSFileManager.defaultManager moveItemAtURL:destURL toURL:srcURL error:error];
	}
	return NO;
}

// ========================
// = Update NSOutlineView =
// ========================

- (void)removeURLs:(NSArray<NSURL*>*)urls
{
	[self removeURLs:[NSSet setWithArray:urls] inParent:self.fileItem rearrange:YES removeInChildren:YES];
}

- (void)removeURLs:(NSSet<NSURL*>*)urls inParent:(FileItem*)parent rearrange:(BOOL)rearrangeFlag
{
	[self removeURLs:urls inParent:parent rearrange:rearrangeFlag removeInChildren:NO];
}

- (void)removeURLs:(NSSet<NSURL*>*)urls inParent:(FileItem*)parent rearrange:(BOOL)rearrangeFlag removeInChildren:(BOOL)recursive
{
	NSMutableIndexSet* indexesToRemove = [NSMutableIndexSet indexSet];
	for(NSUInteger i = 0; i < parent.children.count; ++i)
	{
		if([urls containsObject:parent.children[i].URL])
			[indexesToRemove addIndex:i];
	}

	if(indexesToRemove.count)
	{
		NSMutableArray<FileItem*>* children = [parent.children mutableCopy];
		[children removeObjectsAtIndexes:indexesToRemove];
		parent.children = [children copy];

		if(rearrangeFlag)
			[self rearrangeChildrenInParent:parent];
	}

	if(recursive)
	{
		for(FileItem* child in parent.children)
		{
			if(child.children)
				[self removeURLs:urls inParent:child rearrange:rearrangeFlag removeInChildren:recursive];
		}
	}
}

- (NSArray<FileItem*>*)insertURLs:(NSArray<NSURL*>*)urls
{
	NSMutableArray<FileItem*>* newItems = [NSMutableArray array];

	for(FileItem* parent in [self parentsWithFileURL])
	{
		NSURL* parentURL = parent.resolvedURL;

		NSMutableArray<NSURL*>* urlsToInsert = [NSMutableArray array];
		for(NSURL* url in urls)
		{
			if([url.URLByDeletingLastPathComponent isEqual:parentURL])
				[urlsToInsert addObject:url];
		}

		if(urlsToInsert.count)
		{
			[self removeURLs:[NSSet setWithArray:urls] inParent:parent rearrange:NO];
			NSMutableArray<FileItem*>* children = [parent.children mutableCopy];

			for(NSURL* url in urlsToInsert)
			{
				FileItem* newItem = [FileItem fileItemWithURL:url];
				newItem.missing = NO;
				[children addObject:newItem];
				[newItems addObject:newItem];
			}

			parent.children = [children copy];
			[self rearrangeChildrenInParent:parent];
		}
	}

	return newItems;
}

- (NSArray<FileItem*>*)moveFromURLs:(NSArray<NSURL*>*)fromURLs toURLs:(NSArray<NSURL*>*)toURLs
{
	NSMutableArray<FileItem*>* newItems = [NSMutableArray array];

	struct record_t
	{
		NSURL* sourceURL;
		NSURL* destURL;

		FileItem* sourceParent = nil;
		FileItem* sourceItem   = nil;
		FileItem* destParent   = nil;
	};

	std::vector<record_t> v;
	for(NSUInteger i = 0; i < fromURLs.count; ++i)
		v.push_back({ fromURLs[i], toURLs[i] });

	NSMutableArray<NSURL*>* urlsToRemove = [NSMutableArray array];
	NSMutableArray<NSURL*>* urlsToInsert = [NSMutableArray array];

	for(FileItem* parent in [self parentsWithFileURL])
	{
		NSURL* parentURL = parent.resolvedURL;

		for(auto& r : v)
		{
			if([r.sourceURL.URLByDeletingLastPathComponent isEqual:parentURL])
			{
				r.sourceParent = parent;
				for(FileItem* item in parent.arrangedChildren)
				{
					if([item.URL isEqual:r.sourceURL])
					{
						r.sourceItem = item;
						break;
					}
				}
			}

			if([r.destURL.URLByDeletingLastPathComponent isEqual:parentURL])
				r.destParent = parent;
		}
	}

	for(auto& r : v)
	{
		if(!r.sourceParent && !r.destParent && [r.sourceURL.URLByDeletingLastPathComponent isEqual:r.destURL.URLByDeletingLastPathComponent])
		{
			NSMutableArray<FileItem*>* stack = [self.fileItem.children mutableCopy];
			while(FileItem* parent = stack.firstObject)
			{
				[stack removeObjectAtIndex:0];
				for(FileItem* child in parent.children)
				{
					if([child.URL isEqual:r.sourceURL])
					{
						r.sourceItem   = child;
						r.sourceParent = parent;
						r.destParent   = parent;
						break;
					}

					if(child.children)
						[stack addObject:child];
				}

				if(r.sourceItem)
					break;
			}
		}
	}

	NSComparator compare = self.itemComparator;
	for(auto const& r : v)
	{
		if(!r.destParent)
		{
			[urlsToRemove addObject:r.sourceURL];
		}
		else if(!r.sourceItem)
		{
			[urlsToInsert addObject:r.destURL];
		}
		else
		{
			[self removeURLs:[NSSet setWithObject:r.destURL] inParent:r.destParent rearrange:YES];

			NSInteger oldIndex = [r.sourceParent.arrangedChildren indexOfObject:r.sourceItem];
			if(oldIndex == NSNotFound)
			{
				[urlsToInsert addObject:r.destURL];
			}
			else
			{
				NSMutableArray<FileItem*>* children = [r.sourceParent.children mutableCopy];
				[children removeObject:r.sourceItem];
				r.sourceParent.children = [children copy];

				[r.sourceParent.arrangedChildren removeObjectAtIndex:oldIndex];

				r.sourceItem.URL = r.destURL;
				[newItems addObject:r.sourceItem];

				NSInteger newIndex = 0;
				for(; newIndex < r.destParent.arrangedChildren.count; ++newIndex)
				{
					if(compare(r.sourceItem, r.destParent.arrangedChildren[newIndex]) == NSOrderedAscending)
						break;
				}

				[r.destParent.arrangedChildren insertObject:r.sourceItem atIndex:newIndex];
				r.destParent.children = [r.destParent.children arrayByAddingObject:r.sourceItem];

				[self.outlineView moveItemAtIndex:oldIndex inParent:(r.sourceParent != self.fileItem ? r.sourceParent : nil) toIndex:newIndex inParent:(r.destParent != self.fileItem ? r.destParent : nil)];
			}
		}
	}

	if(urlsToRemove.count)
		[self removeURLs:urlsToRemove];

	if(urlsToInsert.count)
		[newItems addObjectsFromArray:[self insertURLs:urlsToInsert]];

	return newItems;
}

- (NSArray<FileItem*>*)parentsWithFileURL
{
	NSMutableArray<FileItem*>* parents = [NSMutableArray arrayWithObject:self.fileItem];

	NSMutableArray<FileItem*>* stack = [self.fileItem.children mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(!item.children)
			continue;
		if(item.URL.isFileURL)
			[parents addObject:item];
		[stack addObjectsFromArray:item.children];
	}

	return parents;
}

- (BOOL)presentError:(NSError*)error
{
	[self presentError:error modalForWindow:self.view.window delegate:nil didPresentSelector:nullptr contextInfo:nullptr];
	return YES;
}
@end
