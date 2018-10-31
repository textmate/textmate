#import "NSColor Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <oak/debug.h>

@implementation NSColor (TMColorAdditions)
+ (NSColor*)colorWithString:(NSString*)aString
{
	if(OakIsEmptyString(aString))
		return nil;

	unsigned int red = 0, green = 0, blue = 0, alpha = 0xFF;
	if(sscanf([aString UTF8String], "#%02x%02x%02x%02x", &red, &green, &blue, &alpha) >= 3)
		return [NSColor colorWithCalibratedRed:red/255.0 green:green/255.0 blue:blue/255.0 alpha:alpha/255.0];

	if([NSColor respondsToSelector:NSSelectorFromString(aString)])
		return [NSColor performSelector:NSSelectorFromString(aString)];

	return nil;
}

+ (NSColor*)tmMatchedTextBackgroundColor         { return [NSColor colorWithCalibratedRed:0.92 green:0.86 blue:0.48 alpha:0.5]; }
+ (NSColor*)tmMatchedTextUnderlineColor          { return [NSColor colorWithCalibratedRed:0.89 green:0.72 blue:0.0 alpha:1.0]; }
+ (NSColor*)tmMatchedTextSelectedBackgroundColor { return [NSColor colorWithCalibratedWhite:1.0 alpha:0.30]; }
+ (NSColor*)tmMatchedTextSelectedUnderlineColor  { return [NSColor whiteColor]; }

#define COLOR_ASSET(name, color) + (NSColor*)tm##name { if(@available(macos 10.14, *)) return [NSColor colorNamed:@#name]; return color; }

COLOR_ASSET(DarkDividerColor, [NSColor controlShadowColor])
@end
