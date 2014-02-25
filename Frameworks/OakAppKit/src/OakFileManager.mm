#import "OakFileManager.h"
#import "OakSound.h"
#import <OakFoundation/NSFileManager Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>

NSString* const OakFileManagerWillDeleteItemAtPath         = @"OakFileManagerWillDeleteItemAtPath";
NSString* const OakFileManagerDidChangeContentsOfDirectory = @"OakFileManagerDidChangeContentsOfDirectory";
NSString* const OakFileManagerPathKey                      = @"directory";

@interface OakFileManager ()
@property (nonatomic) BOOL hasUISoundToPlay;
@end

@implementation OakFileManager
+ (OakFileManager*)sharedInstance
{
	static OakFileManager* instance = [OakFileManager new];
	return instance;
}

- (void)delsyedPlaySound:(id)aSound
{
	OakPlayUISound((OakSoundIdentifier)[aSound intValue]);
	self.hasUISoundToPlay = NO;
}

- (void)playSound:(OakSoundIdentifier)aSound
{
	if(!self.hasUISoundToPlay)
	{
		self.hasUISoundToPlay = YES;
		[self performSelector:@selector(delsyedPlaySound:) withObject:@(aSound) afterDelay:0];
	}
}

- (void)postDidChangeContentsOfDirectory:(NSString*)aDirectory
{
	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerDidChangeContentsOfDirectory object:self userInfo:@{ OakFileManagerPathKey : aDirectory }];
}

// ===================
// = Stacked Actions =
// ===================

- (BOOL)doCreateDirectory:(NSURL*)dirURL window:(NSWindow*)window
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] createDirectoryAtURL:dirURL withIntermediateDirectories:NO attributes:nil error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doRemoveDirectory:dirURL window:window];
		[self postDidChangeContentsOfDirectory:[[dirURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
	return res;
}

- (void)doRemoveDirectory:(NSURL*)dirURL window:(NSWindow*)window
{
	NSError* error;

	NSArray* contents = [[NSFileManager defaultManager] contentsOfDirectoryAtURL:dirURL includingPropertiesForKeys:nil options:NSDirectoryEnumerationSkipsHiddenFiles error:&error];
	if([contents count] != 0)
	{
		NSInteger choice = NSRunCriticalAlertPanel(@"Folder Not Empty!", [self expandFormat:@"Do you wish to delete “%@” and all the contained items?" withURL:dirURL], @"Delete Folder", @"Cancel", nil);
		if(choice == NSAlertAlternateReturn) // "Cancel"
			return;
	}

	if([[NSFileManager defaultManager] removeItemAtURL:dirURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doCreateDirectory:dirURL window:window];
		[self postDidChangeContentsOfDirectory:[[dirURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
}

- (BOOL)doCreateFile:(NSURL*)fileURL window:(NSWindow*)window
{
	int fd = open([[fileURL path] fileSystemRepresentation], O_CREAT|O_EXCL|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
	if(fd != -1)
	{
		close(fd);
		[[[window undoManager] prepareWithInvocationTarget:self] doRemoveFile:fileURL window:window];
		[self postDidChangeContentsOfDirectory:[[fileURL path] stringByDeletingLastPathComponent]];
	}
	return fd != -1;
}

- (void)doRemoveFile:(NSURL*)fileURL window:(NSWindow*)window
{
	NSError* error;

	NSNumber* fileSize;
	if([fileURL getResourceValue:&fileSize forKey:NSURLFileSizeKey error:&error])
	{
		if([fileSize unsignedLongLongValue])
		{
			NSInteger choice = NSRunCriticalAlertPanel(@"Document Not Empty!", [self expandFormat:@"Do you wish to delete “%@”?" withURL:fileURL], @"Delete Document", @"Cancel", nil);
			if(choice == NSAlertAlternateReturn) // "Cancel"
				return;
		}
	}
	else if(error)
	{
		[window presentError:error];
		return;
	}

	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[fileURL filePathURL] path] }];
	if([[NSFileManager defaultManager] removeItemAtURL:fileURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doCreateFile:fileURL window:window];
		[self postDidChangeContentsOfDirectory:[[fileURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
}

- (BOOL)doCreateCopy:(NSURL*)dstURL ofURL:(NSURL*)srcURL window:(NSWindow*)window
{
	BOOL res = NO;

	NSError* error = nil;
	if(path::is_child([[[dstURL filePathURL] path] fileSystemRepresentation], [[[srcURL filePathURL] path] fileSystemRepresentation]))
	{
		error = [NSError errorWithDomain:NSPOSIXErrorDomain code:ENOTSUP userInfo:nil];
	}
	else if(res = [[NSFileManager defaultManager] copyItemAtURL:srcURL toURL:dstURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doRemoveCopy:dstURL ofURL:srcURL window:window];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}

	if(!res && error)
		[window presentError:error];
	return res;
}

- (void)doRemoveCopy:(NSURL*)dstURL ofURL:(NSURL*)srcURL window:(NSWindow*)window
{
	NSError* error;
	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[dstURL filePathURL] path] }];
	if([[NSFileManager defaultManager] removeItemAtURL:dstURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doCreateCopy:dstURL ofURL:srcURL window:window];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
}

- (BOOL)doCreateLink:(NSURL*)linkURL withDestinationURL:(NSURL*)contentURL window:(NSWindow*)window
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] createSymbolicLinkAtURL:linkURL withDestinationURL:contentURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doRemoveLink:linkURL withDestinationURL:contentURL window:window];
		[self postDidChangeContentsOfDirectory:[[linkURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
	return res;
}

- (void)doRemoveLink:(NSURL*)linkURL withDestinationURL:(NSURL*)contentURL window:(NSWindow*)window
{
	NSError* error;
	if([[NSFileManager defaultManager] removeItemAtURL:linkURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doCreateLink:linkURL withDestinationURL:contentURL window:window];
		[self postDidChangeContentsOfDirectory:[[linkURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
}

- (BOOL)doMove:(NSURL*)srcURL toURL:(NSURL*)dstURL withSound:(BOOL)playSoundFlag window:(NSWindow*)window
{
	BOOL res;
	NSError* error;
	if(res = [[NSFileManager defaultManager] tmMoveItemAtURL:srcURL toURL:dstURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doMove:dstURL toURL:srcURL withSound:playSoundFlag window:window];
		if(playSoundFlag)
			[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[srcURL path] stringByDeletingLastPathComponent]];
		[self postDidChangeContentsOfDirectory:[[dstURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
	}
	return res;
}

- (BOOL)doTrashItem:(NSURL*)trashURL window:(NSWindow*)window
{
	NSError* error = nil;
	NSURL* inTrashURL;

	[[NSNotificationCenter defaultCenter] postNotificationName:OakFileManagerWillDeleteItemAtPath object:self userInfo:@{ OakFileManagerPathKey : [[trashURL filePathURL] path] }];
	if([[NSFileManager defaultManager] tmTrashItemAtURL:trashURL resultingItemURL:&inTrashURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doRestoreItem:inTrashURL toURL:trashURL window:window];
		[self playSound:OakSoundDidTrashItemUISound];
		[self postDidChangeContentsOfDirectory:[[trashURL path] stringByDeletingLastPathComponent]];
		return YES;
	}

	if([error.domain isEqualToString:NSCocoaErrorDomain] && error.code == NSFeatureUnsupportedError)
	{
		error = nil;

		NSInteger choice = NSRunCriticalAlertPanel([NSString stringWithFormat:@"Are you sure you want to delete “%@”?", [[NSFileManager defaultManager] displayNameAtPath:[trashURL path]]], @"This item will be deleted immediately. You can’t undo this action.", @"Delete", @"Cancel", nil);
		if(choice == NSAlertDefaultReturn) // "Delete"
		{
			if([[NSFileManager defaultManager] removeItemAtURL:trashURL error:&error])
			{
				[self playSound:OakSoundDidTrashItemUISound];
				[self postDidChangeContentsOfDirectory:[[trashURL path] stringByDeletingLastPathComponent]];
			}
		}
	}

	if(error)
		[window presentError:error];

	return NO;
}

- (void)doRestoreItem:(NSURL*)inTrashURL toURL:(NSURL*)orgURL window:(NSWindow*)window
{
	NSError* error;
	if([[NSFileManager defaultManager] moveItemAtURL:inTrashURL toURL:orgURL error:&error])
	{
		[[[window undoManager] prepareWithInvocationTarget:self] doTrashItem:orgURL window:window];
		[self playSound:OakSoundDidMoveItemUISound];
		[self postDidChangeContentsOfDirectory:[[orgURL path] stringByDeletingLastPathComponent]];
	}
	else
	{
		[window presentError:error];
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

- (NSURL*)createUntitledDirectoryAtURL:(NSURL*)anURL window:(NSWindow*)window
{
	NSURL* dst = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::unique(path::join([[anURL path] fileSystemRepresentation], "untitled folder"))] isDirectory:YES];
	if([self doCreateDirectory:dst window:window])
	{
		[[window undoManager] setActionName:@"New Folder"];
		return dst;
	}
	return nil;
}

- (BOOL)createFileAtURL:(NSURL*)anURL window:(NSWindow*)window
{
	if([self doCreateFile:anURL window:window])
	{
		[[window undoManager] setActionName:@"New Document"];
		return YES;
	}
	return NO;
}

- (NSURL*)createDuplicateOfURL:(NSURL*)srcURL window:(NSWindow*)window
{
	NSNumber* isDirectory = @NO;
	[srcURL getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
	NSURL* dst = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::unique([[srcURL path] fileSystemRepresentation], " copy")] isDirectory:[isDirectory boolValue]];
	if([self doCreateCopy:dst ofURL:srcURL window:window])
	{
		[[window undoManager] setActionName:[self expandFormat:@"Duplicate of “%@”" withURL:srcURL]];
		return dst;
	}
	return nil;
}

- (void)createSymbolicLinkAtURL:(NSURL*)anURL withDestinationURL:(NSURL*)dstURL window:(NSWindow*)window
{
	if([self doCreateLink:anURL withDestinationURL:dstURL window:window])
		[[window undoManager] setActionName:[self expandFormat:@"Create Link “%@”" withURL:anURL]];
}

- (BOOL)renameItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window
{
	if([self doMove:srcURL toURL:dstURL withSound:NO window:window])
	{
		[[window undoManager] setActionName:@"Rename"];
		return YES;
	}
	return NO;
}

- (void)copyItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window
{
	if([self doCreateCopy:dstURL ofURL:srcURL window:window])
		[[window undoManager] setActionName:[self expandFormat:@"Copy of “%@”" withURL:srcURL]];
}

- (void)moveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window
{
	if([self doMove:srcURL toURL:dstURL withSound:YES window:window])
		[[window undoManager] setActionName:[self expandFormat:@"Move of “%@”" withURL:srcURL]];
}

- (void)trashItemAtURL:(NSURL*)anURL window:(NSWindow*)window
{
	if([self doTrashItem:anURL window:window])
		[[window undoManager] setActionName:[self expandFormat:@"Move of “%@”" withURL:anURL]];
}
@end
