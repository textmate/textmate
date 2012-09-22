#import "NSString Additions.h"
#import "OakFoundation.h"
#import <io/path.h>
#import <text/utf8.h>

@implementation NSString (Path)
+ (NSString*)stringWithUTF8String:(char const*)aString length:(unsigned)aLength
{
	ASSERT(utf8::is_valid(aString, aString + aLength));
	return [[NSString alloc] initWithBytes:aString length:aLength encoding:NSUTF8StringEncoding];
}

+ (NSString*)stringWithCxxString:(std::string const&)aString
{
	ASSERT(utf8::is_valid(aString.begin(), aString.end()));
	return aString == NULL_STR ? nil : [[NSString alloc] initWithBytes:aString.data() length:aString.size() encoding:NSUTF8StringEncoding];
}

- (NSString*)pathExtensions
{
	return [NSString stringWithCxxString:path::extensions(self.UTF8String)];
}

- (NSString*)stringByDeletingPathExtensions
{
	return [NSString stringWithCxxString:path::strip_extensions(self.UTF8String)];
}

- (BOOL)existsAsPath
{
	return [[NSFileManager defaultManager] fileExistsAtPath:self];
}

- (BOOL)isDirectory
{
	BOOL isDir = NO;
	return [[NSFileManager defaultManager] fileExistsAtPath:self isDirectory:&isDir] && isDir;
}
@end

@implementation NSString (FinderSorting)
- (NSComparisonResult)displayNameCompare:(NSString*)otherString
{
	static NSStringCompareOptions comparisonOptions = NSCaseInsensitiveSearch|NSNumericSearch|NSWidthInsensitiveSearch|NSForcedOrderingSearch;
	return [self compare:otherString options:comparisonOptions range:NSMakeRange(0, self.length) locale:[NSLocale currentLocale]];
}
@end
