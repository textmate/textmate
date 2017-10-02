#import "OakStringListTransformer.h"
#import <oak/debug.h>

@interface OakStringListTransformer ()
@property (nonatomic) NSDictionary<NSString*, NSNumber*>* mapping;
@end

@implementation OakStringListTransformer
+ (Class)transformedValueClass         { return [NSNumber class]; }
+ (BOOL)allowsReverseTransformation    { return YES; }

+ (void)createTransformerWithName:(NSString*)aName andObjectsDictionary:(NSDictionary*)mapping
{
	if([NSValueTransformer valueTransformerForName:aName])
		return;

	OakStringListTransformer* transformer = [OakStringListTransformer new];
	transformer.mapping = mapping;
	[NSValueTransformer setValueTransformer:transformer forName:aName];
}

+ (void)createTransformerWithName:(NSString*)aName andObjectsArray:(NSArray*)aList
{
	NSMutableDictionary* dict = [NSMutableDictionary dictionary];
	for(NSUInteger i = 0; i < aList.count; ++i)
		dict[aList[i]] = @(i);
	[self createTransformerWithName:aName andObjectsDictionary:dict];
}

- (NSNumber*)transformedValue:(NSString*)value
{
	return value ? _mapping[value] : nil;
}

- (NSString*)reverseTransformedValue:(NSNumber*)value
{
	for(NSString* key in _mapping)
	{
		if([_mapping[key] isEqual:value])
			return key;
	}
	return nil;
}
@end
