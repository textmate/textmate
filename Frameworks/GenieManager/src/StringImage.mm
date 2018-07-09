#import "StringImage.h"

// ===============
// = StringImage =
// ===============

@implementation StringImage
+ (NSImage*)imageWithString:(NSString*)aString size:(NSSize)aSize
{
	return [NSImage imageWithSize:aSize flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		NSDictionary* const attributes = @{
			NSFontAttributeName: [NSFont systemFontOfSize:20],
			NSForegroundColorAttributeName: NSColor.textColor,
		};

		NSAttributedString* str = [[NSAttributedString alloc] initWithString:aString attributes:attributes];

		NSSize size = str.size;
		[str drawInRect:NSIntegralRect(NSInsetRect(dstRect, (NSWidth(dstRect) - size.width)/2, (NSHeight(dstRect) - size.height)/2))];

		return YES;
	}];
}
@end
