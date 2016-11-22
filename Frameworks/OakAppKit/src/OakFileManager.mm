#import "OakFileManager.h"
#import "NSAlert Additions.h"
#import "OakSound.h"
#import <OakFoundation/NSFileManager Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>

NSString* const OakFileManagerWillDeleteItemAtPath         = @"OakFileManagerWillDeleteItemAtPath";
NSString* const OakFileManagerDidChangeContentsOfDirectory = @"OakFileManagerDidChangeContentsOfDirectory";
NSString* const OakFileManagerPathKey                      = @"directory";

NSString* OakReplaceDateInString (NSString* srcPath, NSDate* newDate)
{
	NSDateFormatter* formatter = [NSDateFormatter new];
	formatter.dateFormat = @"yyyy-MM-dd";
	NSString* todaysDate = [formatter stringFromDate:newDate];

	NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"(\\b|_)\\d{4}(?:-\\d{2}){2}(\\b|_)" options:0 error:nullptr];
	return [regex stringByReplacingMatchesInString:srcPath options:0 range:NSMakeRange(0, [srcPath length]) withTemplate:[NSString stringWithFormat:@"$1%@$2", todaysDate]];
}

@interface OakFileManager ()
@property (nonatomic) BOOL hasUISoundToPlay;
@end

@implementation OakFileManager
+ (instancetype)sharedInstance
{
	static OakFileManager* sharedInstance = [self new];
	return sharedInstance;
}

- (void)delayedPlaySound:(id)aSound
{
	OakPlayUISound((OakSoundIdentifier)[aSound intValue]);
	self.hasUISoundToPlay = NO;
}

- (void)playSound:(OakSoundIdentifier)aSound
{
	if(!self.hasUISoundToPlay)
	{
		self.hasUISoundToPlay = YES;
		[self performSelector:@selector(delayedPlaySound:) withObject:@(aSound) afterDelay:0];
	}
}

- (void)postDidChangeContentsOfDirectory:(NSString*)aDirectory
{
	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerDidChangeContentsOfDirectory object:self userInfo:@{ OakFileManagerPathKey : aDirectory }];
}

// ===================
// = Stacked Actions =
// ===================

- (BOOL)doCreateDirectory:(NSURL*)dirURL view:(NSView*)view
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] createDirectoryAtURL:dirURL withIntermediateDirectories:NO attributes:nil error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doRemoveDirectory:dirURL view:view];
		[self postDidChangeContentsOfDirectory:[[dirURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
	return res;
}

- (void)doRemoveDirectory:(NSURL*)dirURL view:(NSView*)view
{
	NSError* error;

	NSArray* contents = [[NSFileManager defaultManager] contentsOfDirectoryAtURL:dirURL includingPropertiesForKeys:nil options:NSDirectoryEnumerationSkipsHiddenFiles error:&error];
	if([contents count] != 0)
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.alertStyle      = NSAlertStyleCritical;
		alert.messageText     = @"Folder Not Empty!";
		alert.informativeText = [self expandFormat:@"Do you wish to delete “%@” and all the contained items?" withURL:dirURL];
		[alert addButtons:@"Delete Folder", @"Cancel", nil];

		if([alert runModal] == NSAlertSecondButtonReturn) // "Cancel"
			return;
	}

	if([[NSFileManager defaultManager] removeItemAtURL:dirURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doCreateDirectory:dirURL view:view];
		[self postDidChangeContentsOfDirectory:[[dirURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
}

- (BOOL)doCreateFile:(NSURL*)fileURL view:(NSView*)view
{
	int fd = open([[fileURL path] fileSystemRepresentation], O_CREAT|O_EXCL|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
	if(fd != -1)
	{
		close(fd);
		[[[view undoManager] prepareWithInvocationTarget:self] doRemoveFile:fileURL view:view];
		[self postDidChangeContentsOfDirectory:[[fileURL path] stringByDeletingLastPathComponent]];
	}
	return fd != -1;
}

- (void)doRemoveFile:(NSURL*)fileURL view:(NSView*)view
{
	NSError* error;

	NSNumber* fileSize;
	if([fileURL getResourceValue:&fileSize forKey:NSURLFileSizeKey error:&error])
	{
		if([fileSize unsignedLongLongValue])
		{
			NSAlert* alert        = [[NSAlert alloc] init];
			alert.alertStyle      = NSAlertStyleCritical;
			alert.messageText     = @"Document Not Empty!";
			alert.informativeText = [self expandFormat:@"Do you wish to delete “%@”?" withURL:fileURL];
			[alert addButtons:@"Delete Document", @"Cancel", nil];

			if([alert runModal] == NSAlertSecondButtonReturn) // "Cancel"
				return;
		}
	}
	else if(error)
	{
		[view.window presentError:error];
		return;
	}

	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[fileURL filePathURL] path] }];
	if([[NSFileManager defaultManager] removeItemAtURL:fileURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doCreateFile:fileURL view:view];
		[self postDidChangeContentsOfDirectory:[[fileURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
}

- (BOOL)doCreateCopy:(NSURL*)dstURL ofURL:(NSURL*)srcURL view:(NSView*)view
{
	BOOL res = NO;

	NSError* error = nil;
	if(path::is_child([[[dstURL filePathURL] path] fileSystemRepresentation], [[[srcURL filePathURL] path] fileSystemRepresentation]))
	{
		error = [NSError errorWithDomain:NSPOSIXErrorDomain code:ENOTSUP userInfo:nil];
	}
	else if(res = [[NSFileManager defaultManager] copyItemAtURL:srcURL toURL:dstURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doRemoveCopy:dstURL ofURL:srcURL view:view];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}

	if(!res && error)
		[view.window presentError:error];
	return res;
}

- (void)doRemoveCopy:(NSURL*)dstURL ofURL:(NSURL*)srcURL view:(NSView*)view
{
	NSError* error;
	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[dstURL filePathURL] path] }];
	if([[NSFileManager defaultManager] removeItemAtURL:dstURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doCreateCopy:dstURL ofURL:srcURL view:view];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
}

- (BOOL)doCreateLink:(NSURL*)linkURL withDestinationURL:(NSURL*)contentURL view:(NSView*)view
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] createSymbolicLinkAtURL:linkURL withDestinationURL:contentURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doRemoveLink:linkURL withDestinationURL:contentURL view:view];
		[self postDidChangeContentsOfDirectory:[[linkURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
	return res;
}

- (void)doRemoveLink:(NSURL*)linkURL withDestinationURL:(NSURL*)contentURL view:(NSView*)view
{
	NSError* error;
	if([[NSFileManager defaultManager] removeItemAtURL:linkURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doCreateLink:linkURL withDestinationURL:contentURL view:view];
		[self postDidChangeContentsOfDirectory:[[linkURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
}

- (BOOL)doMove:(NSURL*)srcURL toURL:(NSURL*)dstURL withSound:(BOOL)playSoundFlag view:(NSView*)view
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] tmMoveItemAtURL:srcURL toURL:dstURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doMove:dstURL toURL:srcURL withSound:playSoundFlag view:view];
		if(playSoundFlag)
			[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[srcURL path] stringByDeletingLastPathComponent]];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
	return res;
}

- (BOOL)doTrashItem:(NSURL*)trashURL view:(NSView*)view
{
	NSError* error = nil;
	NSURL* inTrashURL;

	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[trashURL filePathURL] path] }];
	if([[NSFileManager defaultManager] trashItemAtURL:trashURL resultingItemURL:&inTrashURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doRestoreItem:inTrashURL toURL:trashURL view:view];
		[self playSound:OakSoundDidTrashItemUISound];
		[self postDidChangeContentsOfDirectory:[[trashURL path] stringByDeletingLastPathComponent]];
		return YES;
	}

	if([error.domain isEqualToString:NSCocoaErrorDomain] && error.code == NSFeatureUnsupportedError)
	{
		error = nil;

		NSAlert* alert        = [[NSAlert alloc] init];
		alert.alertStyle      = NSAlertStyleCritical;
		alert.messageText     = [NSString stringWithFormat:@"Are you sure you want to delete “%@”?", [[NSFileManager defaultManager] displayNameAtPath:[trashURL path]]];
		alert.informativeText = @"This item will be deleted immediately. You can’t undo this action.";
		[alert addButtons:@"Delete", @"Cancel", nil];

		if([alert runModal] == NSAlertFirstButtonReturn) // "Delete"
		{
			if([[NSFileManager defaultManager] removeItemAtURL:trashURL error:&error])
			{
				[self playSound:OakSoundDidTrashItemUISound];
				[self postDidChangeContentsOfDirectory:[[trashURL path] stringByDeletingLastPathComponent]];
			}
		}
	}

	if(error)
		[view.window presentError:error];

	return NO;
}

- (void)doRestoreItem:(NSURL*)inTrashURL toURL:(NSURL*)orgURL view:(NSView*)view
{
	NSError* error;
	if([[NSFileManager defaultManager] moveItemAtURL:inTrashURL toURL:orgURL error:&error])
	{
		[[[view undoManager] prepareWithInvocationTarget:self] doTrashItem:orgURL view:view];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[orgURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[view.window presentError:error];
	}
}

// ==========
// = Helper =
// ==========

- (NSString*)expandFormat:(NSString*)aFormat withURL:(NSURL*)anURL
{
	return [NSString stringWithFormat:aFormat, [[NSFileManager defaultManager] displayNameAtPath:[anURL path]]];
}

// =======
// = API =
// =======

- (NSURL*)createUntitledDirectoryAtURL:(NSURL*)anURL view:(NSView*)view
{
	NSURL* dst = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::unique(path::join([[anURL path] fileSystemRepresentation], "untitled folder"))] isDirectory:YES];
	if([self doCreateDirectory:dst view:view])
	{
		[[view undoManager] setActionName:@"New Folder"];
		return dst;
	}
	return nil;
}

- (BOOL)createFileAtURL:(NSURL*)anURL view:(NSView*)view
{
	if([self doCreateFile:anURL view:view])
	{
		[[view undoManager] setActionName:@"New Document"];
		return YES;
	}
	return NO;
}

- (NSURL*)createDuplicateOfURL:(NSURL*)srcURL view:(NSView*)view
{
	NSNumber* isDirectory = @NO;
	[srcURL getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
	NSURL* dst = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::unique([[srcURL path] fileSystemRepresentation], " copy")] isDirectory:[isDirectory boolValue]];

	if(![isDirectory boolValue])
	{
		NSString* srcPath = [srcURL path];
		NSString* newPath = OakReplaceDateInString(srcPath);
		if(![srcPath isEqualToString:newPath] && ![[NSFileManager defaultManager] fileExistsAtPath:newPath])
			dst = [NSURL fileURLWithPath:newPath];
	}

	if([self doCreateCopy:dst ofURL:srcURL view:view])
	{
		[[view undoManager] setActionName:[self expandFormat:@"Duplicate of “%@”" withURL:srcURL]];
		return dst;
	}
	return nil;
}

- (void)createSymbolicLinkAtURL:(NSURL*)anURL withDestinationURL:(NSURL*)dstURL view:(NSView*)view
{
	if([self doCreateLink:anURL withDestinationURL:dstURL view:view])
		[[view undoManager] setActionName:[self expandFormat:@"Create Link “%@”" withURL:anURL]];
}

- (BOOL)renameItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view
{
	if([self doMove:srcURL toURL:dstURL withSound:NO view:view])
	{
		[[view undoManager] setActionName:@"Rename"];
		return YES;
	}
	return NO;
}

- (void)copyItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view
{
	if([self doCreateCopy:dstURL ofURL:srcURL view:view])
		[[view undoManager] setActionName:[self expandFormat:@"Copy of “%@”" withURL:srcURL]];
}

- (void)moveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view
{
	if([self doMove:srcURL toURL:dstURL withSound:YES view:view])
		[[view undoManager] setActionName:[self expandFormat:@"Move of “%@”" withURL:srcURL]];
}

- (void)trashItemAtURL:(NSURL*)anURL view:(NSView*)view
{
	if([self doTrashItem:anURL view:view])
		[[view undoManager] setActionName:[self expandFormat:@"Move of “%@”" withURL:anURL]];
}
@end
