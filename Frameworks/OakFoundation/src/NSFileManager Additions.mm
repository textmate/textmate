#import "NSFileManager Additions.h"
#import "NSString Additions.h"
#import <io/path.h>

@implementation NSFileManager (TMFileManagerAdditions)
- (BOOL)tmTrashItemAtURL:(NSURL*)trashURL resultingItemURL:(NSURL**)resultingURL error:(NSError**)error
{
	if([self respondsToSelector:@selector(trashItemAtURL:resultingItemURL:error:)])
		return [self trashItemAtURL:trashURL resultingItemURL:resultingURL error:error];

	std::string const res = path::move_to_trash([[trashURL path] fileSystemRepresentation]);
	if(res != NULL_STR && resultingURL)
		*resultingURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:res]];
	else if(res == NULL_STR && error)
		*error = [NSError errorWithDomain:NSCocoaErrorDomain code:NSFeatureUnsupportedError userInfo:nil];
	return res != NULL_STR;
}
@end