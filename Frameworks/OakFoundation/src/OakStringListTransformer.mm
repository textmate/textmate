#import "OakStringListTransformer.h"
#import <oak/debug.h>

@interface OakStringListTransformer ()
@property (nonatomic) NSArray* stringList;
@end

@implementation OakStringListTransformer
+ (Class)transformedValueClass         { return [NSNumber class]; }
+ (BOOL)allowsReverseTransformation    { return YES; }

+ (void)createTransformerWithName:(NSString*)aName andObjectsArray:(NSArray*)aList
{
	if([NSValueTransformer valueTransformerForName:aName])
		return;

	OakStringListTransformer* transformer = [OakStringListTransformer new];
	transformer.stringList = aList;
	[NSValueTransformer setValueTransformer:transformer forName:aName];
}

+ (void)createTransformerWithName:(NSString*)aName andObjects:(id)firstObj, ...
{
	ASSERT(firstObj != nil);

	va_list ap;
	va_start(ap, firstObj);
	NSMutableArray* list = [NSMutableArray array];
	do {
		[list addObject:firstObj];
	} while(firstObj = va_arg(ap, id));
	va_end(ap);

	[self createTransformerWithName:aName andObjectsArray:list];
}

- (id)transformedValue:(id)value
{
	NSUInteger i = value ? [self.stringList indexOfObject:value] : NSNotFound;
	return i != NSNotFound ? @(i) : nil;
}

- (id)reverseTransformedValue:(id)value
{
	NSUInteger i = value ? [value unsignedIntValue] : NSNotFound;
	return i < [self.stringList count] ? [self.stringList objectAtIndex:i] : nil;
}
@end
