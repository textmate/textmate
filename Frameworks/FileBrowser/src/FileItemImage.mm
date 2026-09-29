#import "FileItemImage.h"
#import <TMFileReference/src/TMFileReference.h>

NSImage* CreateIconImageForURL (NSURL* url, BOOL isModified, BOOL isMissing, BOOL isDirectory, BOOL isSymbolicLink, scm::status::type scmStatus)
{
	NSImage* res;

	if(isMissing && (scmStatus == scm::status::none || scmStatus == scm::status::unknown))
	{
		res = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];
	}
	else
	{
		TMFileReference* fileReference = [TMFileReference fileReferenceWithURL:url];
		if(scmStatus != scm::status::unknown)
			fileReference.SCMStatus = scmStatus;
		res = fileReference.image;
	}

	res = [res copy];
	res.size = NSMakeSize(16, 16);

	return isModified ? [NSImage imageWithSize:res.size flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		[res drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:0.4];
		return YES;
	}] : res;
}
