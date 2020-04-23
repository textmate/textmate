#import "CommonAncestor.h"

static NSString* helper (NSArray<NSString*>* paths)
{
	if(paths.count < 2)
		return paths.firstObject;

	NSUInteger maxLength = NSUIntegerMax;
	for(NSString* path : paths)
		maxLength = MIN(path.length, maxLength);

	NSUInteger pathSeparatorIndex = 0;
	for(NSUInteger i = 0; i < maxLength; ++i)
	{
		unichar ch = [paths.firstObject characterAtIndex:i];
		for(NSUInteger j = 1; j < paths.count; ++j)
		{
			if(ch != [paths[j] characterAtIndex:i])
				return pathSeparatorIndex ? [paths.firstObject substringToIndex:pathSeparatorIndex] : @"/";
		}

		if(ch == '/')
			pathSeparatorIndex = i;
	}

	return pathSeparatorIndex ? [paths.firstObject substringToIndex:pathSeparatorIndex] : @"/";
}

NSString* CommonAncestor (NSArray<NSString*>* paths)
{
	NSString* path = helper(paths);
	BOOL isDirectory = NO;
	if([NSFileManager.defaultManager fileExistsAtPath:path isDirectory:&isDirectory] && !isDirectory)
		path = [path stringByDeletingLastPathComponent];
	return path;
}
