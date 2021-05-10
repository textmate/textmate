#import "NSColor Additions.h"

id TMMakeAttributedStringWithBackgroundStyle (id objectValue, NSBackgroundStyle backgroundStyle)
{
	if(![objectValue isKindOfClass:[NSAttributedString class]])
		return objectValue;

	NSColor* backgroundColor = backgroundStyle == NSBackgroundStyleEmphasized ? NSColor.tmMatchedTextSelectedBackgroundColor : NSColor.tmMatchedTextBackgroundColor;
	NSColor* underlineColor  = backgroundStyle == NSBackgroundStyleEmphasized ? NSColor.tmMatchedTextSelectedUnderlineColor  : NSColor.tmMatchedTextUnderlineColor;

	NSMutableAttributedString* res = [objectValue mutableCopy];
	[objectValue enumerateAttributesInRange:NSMakeRange(0, [objectValue length]) options:NSAttributedStringEnumerationLongestEffectiveRangeNotRequired usingBlock:^(NSDictionary* attrs, NSRange range, BOOL* stop){
		if(attrs[NSBackgroundColorAttributeName])
			[res addAttribute:NSBackgroundColorAttributeName value:backgroundColor range:range];
		if(attrs[NSUnderlineColorAttributeName])
			[res addAttribute:NSUnderlineColorAttributeName value:underlineColor range:range];
	}];
	return res;
}

@implementation NSColor (TMColorAdditions)
+ (NSColor*)tmMatchedTextBackgroundColor         { return [NSColor colorWithCalibratedRed:0.92 green:0.86 blue:0.48 alpha:0.5]; }
+ (NSColor*)tmMatchedTextUnderlineColor          { return [NSColor colorWithCalibratedRed:0.89 green:0.72 blue:0.0 alpha:1.0]; }
+ (NSColor*)tmMatchedTextSelectedBackgroundColor { return [NSColor colorWithCalibratedWhite:1.0 alpha:0.30]; }
+ (NSColor*)tmMatchedTextSelectedUnderlineColor  { return [NSColor whiteColor]; }
@end
