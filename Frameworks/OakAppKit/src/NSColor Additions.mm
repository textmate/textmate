#import "NSColor Additions.h"
#import <OakFoundation/OakFoundation.h>

@implementation NSColor (Creation)
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

+ (NSColor*)colorWithCGColor:(CGColorRef)aColor
{
	return [NSColor colorWithColorSpace:[[[NSColorSpace alloc] initWithCGColorSpace:CGColorGetColorSpace(aColor)] autorelease] components:CGColorGetComponents(aColor) count:CGColorGetNumberOfComponents(aColor)];
}
@end

@implementation NSColor (OakColor)
- (BOOL)isDark
{
	uint32_t r(lroundf(255 * [self redComponent]));
	uint32_t g(lroundf(255 * [self greenComponent]));
	uint32_t b(lroundf(255 * [self blueComponent])); 

	uint32_t intensity = r*r*30 + g*g*59 + b*b*11;
	return intensity < 50*255*255;
}
@end
