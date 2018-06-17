#import "OakFoundation.h"
#import "NSString Additions.h"

std::string OakMoveToTrash (std::string const& path)
{
	NSURL* resultingItemURL;
	if([[NSFileManager defaultManager] trashItemAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path]] resultingItemURL:&resultingItemURL error:nil])
			return resultingItemURL.fileSystemRepresentation ?: NULL_STR;
	else	return NULL_STR;
}

BOOL OakIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL OakNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}
