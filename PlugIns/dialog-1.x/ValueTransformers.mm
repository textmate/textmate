#import "ValueTransformers.h"
#import "Dialog.h"

// ===================================================
// = Int Array To Index Path Array Value Transformer =
// ===================================================

@implementation OakIntArrayToIndexPathTransformer
+ (Class)transformedValueClass      { return [NSArray class]; }
+ (BOOL)allowsReverseTransformation { return YES; }

+ (void)load
{
	id transformer = [self new];
	[NSValueTransformer setValueTransformer:transformer forName:@"OakIntArrayToIndexPathTransformer"];
}

- (NSIndexPath*)arrayToIndexPath:(NSArray*)anArray
{
	NSIndexPath* indexPath = [NSIndexPath new];
	for(id index in anArray)
		indexPath = [indexPath indexPathByAddingIndex:[index intValue]];
	return indexPath;
}

- (id)transformedValue:(id)value
{
	NSMutableArray* res = [NSMutableArray array];
	for(NSArray* intArray in value)
		[res addObject:[self arrayToIndexPath:intArray]];
	return res;
}

- (NSArray*)indexPathToArray:(NSIndexPath*)anIndexPath
{
	NSMutableArray* array = [NSMutableArray array];
	for(NSUInteger i = 0; i < [anIndexPath length]; ++i)
		[array addObject:[NSNumber numberWithUnsignedInt:[anIndexPath indexAtPosition:i]]];
	return array;
}

- (id)reverseTransformedValue:(id)value
{
	NSMutableArray* array = [NSMutableArray array];
	for(NSIndexPath* indexPath in value)
		[array addObject:[self indexPathToArray:indexPath]];
	return array;
}
@end

// ============================================
// = Int Array To Index Set Value Transformer =
// ============================================

@implementation OakIntArrayToIndexSetTransformer
+ (Class)transformedValueClass      { return [NSIndexSet class]; }
+ (BOOL)allowsReverseTransformation { return YES; }

+ (void)load
{
	id transformer = [self new];
	[NSValueTransformer setValueTransformer:transformer forName:@"OakIntArrayToIndexSetTransformer"];
}

- (id)transformedValue:(id)value
{
	NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
	for(NSNumber* integer in value)
		[indexSet addIndex:[integer intValue]];
	return indexSet;
}

- (id)reverseTransformedValue:(id)value
{
	NSMutableArray* array = [NSMutableArray array];
	NSUInteger buf[[value count]];
	[(NSIndexSet*)value getIndexes:buf maxCount:[value count] inIndexRange:nil];
	for(NSUInteger i = 0; i != [value count]; i++)
		[array addObject:[NSNumber numberWithUnsignedInt:buf[i]]];
	return array;
}
@end

// =============================================
// = #RRGGBB String To Color Value Transformer =
// =============================================

static NSColor* NSColorFromString (NSString* aColor)
{
	if(!aColor || [aColor isEqualToString:@""])
		return nil;

	CGFloat red = 0, green = 0, blue = 0, alpha = 0xFF;
	if(sscanf([aColor UTF8String], "#%02x%02x%02x%02x", &red, &green, &blue, &alpha) < 3)
		return nil;

	return [NSColor colorWithCalibratedRed:red/255.0 green:green/255.0 blue:blue/255.0 alpha:alpha/255.0];
}

static NSString* NSStringFromColor (NSColor* aColor)
{
	if(aColor == nil)
		return nil;

	aColor = [aColor colorUsingColorSpace:[NSColorSpace sRGBColorSpace]];
	if([aColor alphaComponent] != 1.0)
			return [NSString stringWithFormat:@"#%02lX%02lX%02lX%02lX", lroundf(255.0*[aColor redComponent]), lroundf(255.0*[aColor greenComponent]), lroundf(255.0*[aColor blueComponent]), lroundf(255.0*[aColor alphaComponent])];
	else	return [NSString stringWithFormat:@"#%02lX%02lX%02lX", lroundf(255.0*[aColor redComponent]), lroundf(255.0*[aColor greenComponent]), lroundf(255.0*[aColor blueComponent])];
}

@implementation OakStringToColorTransformer
+ (Class)transformedValueClass      { return [NSColor class]; }
+ (BOOL)allowsReverseTransformation { return YES; }

- (id)transformedValue:(id)value        { return NSColorFromString(value); }
- (id)reverseTransformedValue:(id)value { return NSStringFromColor(value); }

+ (void)load
{
	id transformer = [self new];
	[NSValueTransformer setValueTransformer:transformer forName:@"OakStringToColorTransformer"];
}
@end
