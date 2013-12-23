#import "NSArray Additions.h"

@implementation NSArray (Other)
- (id)firstObject
{
	return [self count] ? [self objectAtIndex:0] : nil;
}

- (id)safeObjectAtIndex:(NSUInteger)anIndex
{
	return (anIndex < [self count] && [self objectAtIndex:anIndex] != [NSNull null]) ? [self objectAtIndex:anIndex] : nil;
}
@end
