#import "NSArray Additions.h"
#import "NSString Additions.h"
#import <oak/oak.h>
#import <oak/CocoaSTL.h>

@implementation NSArray (Other)
- (id)firstObject
{
	return [self count] ? [self objectAtIndex:0] : nil;
}

- (NSString*)caseSensitiveMatchForString:(NSString*)otherString
{
	for (NSString *string in self) {
		if (![otherString caseInsensitiveCompare:string])
			return string;
	}
	return nil;
}
@end
