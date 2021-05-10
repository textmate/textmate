#import "NSColor Additions.h"

@implementation NSColor (TMColorAdditions)
+ (NSColor*)tmMatchedTextBackgroundColor         { return [NSColor colorWithCalibratedRed:0.92 green:0.86 blue:0.48 alpha:0.5]; }
+ (NSColor*)tmMatchedTextUnderlineColor          { return [NSColor colorWithCalibratedRed:0.89 green:0.72 blue:0.0 alpha:1.0]; }
+ (NSColor*)tmMatchedTextSelectedBackgroundColor { return [NSColor colorWithCalibratedWhite:1.0 alpha:0.30]; }
+ (NSColor*)tmMatchedTextSelectedUnderlineColor  { return [NSColor whiteColor]; }
@end
