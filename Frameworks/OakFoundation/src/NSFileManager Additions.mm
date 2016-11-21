#import "NSFileManager Additions.h"
#import "NSString Additions.h"
#import <io/path.h>
#import <oak/debug.h>

static std::pair<dev_t, ino_t> inode (std::string const& path)
{
	struct stat buf;
	if(lstat(path.c_str(), &buf) == 0)
		return { buf.st_dev, buf.st_ino };
	return std::pair<dev_t, ino_t>();
}

@implementation NSFileManager (TMFileManagerAdditions)
- (BOOL)tmMoveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL error:(NSError**)error
{
	NSError* localError = nil;
	if(![self moveItemAtURL:srcURL toURL:dstURL error:&localError])
	{
		// NSFileManager cannot be used to make case-changes on case-insensitive file systems <rdar://13161552>.
		if([localError.domain isEqualToString:NSCocoaErrorDomain] && localError.code == NSFileWriteFileExistsError)
		{
			std::string const src = [srcURL.path fileSystemRepresentation];
			std::string const dst = [dstURL.path fileSystemRepresentation];
			if(src != dst && inode(src) == inode(dst))
			{
				if(rename(src.c_str(), dst.c_str()) == 0)
					return YES;
			}
		}

		if(error)
			*error = localError;
		return NO;
	}
	return YES;
}
@end
