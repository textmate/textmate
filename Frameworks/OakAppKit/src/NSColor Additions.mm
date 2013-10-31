#import "NSColor Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <oak/debug.h>

@implementation NSColor (TMColorAdditions)
+ (NSColor*)colorWithString:(NSString*)aString
{
	if(NSIsEmptyString(aString))
		return nil;

	unsigned int red = 0, green = 0, blue = 0, alpha = 0xFF;
	if(sscanf([aString UTF8String], "#%02x%02x%02x%02x", &red, &green, &blue, &alpha) >= 3)
		return [NSColor colorWithCalibratedRed:red/255.0 green:green/255.0 blue:blue/255.0 alpha:alpha/255.0];

	if([NSColor respondsToSelector:NSSelectorFromString(aString)])
		return [NSColor performSelector:NSSelectorFromString(aString)];

	return nil;
}

+ (NSColor*)tmColorWithCGColor:(CGColorRef)aColor
{
	ASSERT(aColor != nullptr);
	if([self respondsToSelector:@selector(colorWithCGColor:)])
		return [self colorWithCGColor:aColor];
	return [NSColor colorWithColorSpace:[[NSColorSpace alloc] initWithCGColorSpace:CGColorGetColorSpace(aColor)] components:CGColorGetComponents(aColor) count:CGColorGetNumberOfComponents(aColor)];
}

- (CGColorRef)tmCGColor
{
	if([self respondsToSelector:@selector(CGColor)])
		return [self CGColor];

	NSColor* rgbColor = [self colorUsingColorSpaceName:NSCalibratedRGBColorSpace];
	CGFloat rgba[4];
	[rgbColor getRed:&rgba[0] green:&rgba[1] blue:&rgba[2] alpha:&rgba[3]];
	CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
	CGColorRef res = CGColorCreate(colorSpace, rgba);
	CGColorSpaceRelease(colorSpace);
	__autoreleasing __attribute__ ((unused)) id dummy = CFBridgingRelease(res);
	return res;
}
@end
