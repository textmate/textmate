#import "NSArray Additions.h"

@implementation NSArray (Other)
- (id)firstObject
{
	return [self count] ? [self objectAtIndex:0] : nil;
}
@end
