#import "OakBundleItemCell.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import "../highlight_ranges.h"

@implementation OakBundleItemCell
@synthesize keyEquivalent, attributedTabTrigger;

- (id)copyWithZone:(NSZone*)zone
{
	OakBundleItemCell* cell    = [super copyWithZone:zone];
	cell->keyEquivalent        = [self.keyEquivalent copy];
	cell->attributedTabTrigger = [self.attributedTabTrigger copy];
	return cell;
}

- (void)dealloc
{
	self.keyEquivalent        = nil;
	self.attributedTabTrigger = nil;
	[super dealloc];
}

- (NSString*)tabTrigger
{
	return attributedTabTrigger.string;
}

- (void)setTabTrigger:(NSString*)tabTrigger
{
	self.attributedTabTrigger = tabTrigger ? [[[NSAttributedString alloc] initWithString:tabTrigger attributes:nil] autorelease] : nil;
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView*)controlView
{
	if(NSNotEmptyString(attributedTabTrigger.string))
	{
		NSMutableAttributedString* attrStr = [[attributedTabTrigger mutableCopy] autorelease];

		NSDictionary* highlightAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
												@1, NSUnderlineStyleAttributeName,
												[NSFont boldSystemFontOfSize:[NSFont systemFontSizeForControlSize:NSSmallControlSize]], NSFontAttributeName,
												nil];
		HighlightRangesWithAttribute(attrStr, FLMatchingTextAttributeName, highlightAttributes);

		[attrStr appendAttributedString:[[[NSAttributedString alloc] initWithString:@"\u21E5" attributes:nil] autorelease]];
		CFTypeRef str = (CFAttributedStringRef)attrStr;

		HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, kThemeSmallSystemFont, kHIThemeTextHorizontalFlushRight, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
		float width = 0.0;
		HIThemeGetTextDimensions(str, 0, &textInfo, &width, NULL, NULL);

		NSRect triggerFrame;
		NSDivideRect(cellFrame, &triggerFrame, &cellFrame, width + 12.0, NSMaxXEdge);
		cellFrame.size.width -= 10.0;
		triggerFrame.size.width -= 2.0;

		[[NSColor colorWithCalibratedRed:0.0 green:0.0 blue:0.0 alpha:0.15] set];
		NSRect podRect = triggerFrame;
		[[NSBezierPath bezierPathWithRoundedRect:podRect xRadius:4 yRadius:4] fill];

		HIRect bounds = { { NSMinX(podRect) - 5.0f, NSMinY(podRect) }, { NSWidth(podRect), NSHeight(podRect) } };
		[[NSColor textColor] set];
		if([self isHighlighted] && ([[controlView window] firstResponder] == controlView || ([[[controlView window] firstResponder] respondsToSelector:@selector(delegate)] && [[[controlView window] firstResponder] performSelector:@selector(delegate)] == controlView)) && [[controlView window] isKeyWindow])
			[[NSColor alternateSelectedControlTextColor] set];

		CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
		HIThemeDrawTextBox(str, &bounds, &textInfo, context, kHIThemeOrientationNormal);
	}
	else if(NSNotEmptyString(keyEquivalent))
	{
		size_t keyStartsAt = 0;
		std::string const glyphString = ns::glyphs_for_event_string(to_s(keyEquivalent), &keyStartsAt);
		NSString* modifiers = [NSString stringWithCxxString:glyphString.substr(0, keyStartsAt)];
		NSString* key       = [NSString stringWithCxxString:glyphString.substr(keyStartsAt)];

		NSDictionary* fontAttr = @{ NSFontAttributeName : [self font] };
		CGFloat width = std::max<CGFloat>(1, [modifiers sizeWithAttributes:fontAttr].width);

		NSMutableAttributedString* aStr = [[[NSMutableAttributedString alloc] initWithString:[NSString stringWithFormat:@"\t%@\t%@", modifiers, key] attributes:fontAttr] autorelease];
		NSMutableParagraphStyle* pStyle = [[NSMutableParagraphStyle new] autorelease];
		[pStyle setTabStops:
			[NSArray arrayWithObjects:
				[[[NSTextTab alloc] initWithType:NSRightTabStopType location:width] autorelease],
				[[[NSTextTab alloc] initWithType:NSLeftTabStopType location:width + 1] autorelease],
				nil]];

		[aStr addAttributes:@{ NSParagraphStyleAttributeName : pStyle } range:NSMakeRange(0, [aStr length])];

		if([self isHighlighted] && ([[controlView window] firstResponder] == controlView || ([[[controlView window] firstResponder] respondsToSelector:@selector(delegate)] && [[[controlView window] firstResponder] performSelector:@selector(delegate)] == controlView)) && [[controlView window] isKeyWindow])
			[aStr addAttributes:@{ NSForegroundColorAttributeName : [NSColor alternateSelectedControlTextColor] } range:NSMakeRange(0, [aStr length])];

		NSRect triggerFrame;
		NSDivideRect(cellFrame, &triggerFrame, &cellFrame, width + 20, NSMaxXEdge);
		cellFrame.size.width -= 10;

		[aStr drawInRect:triggerFrame];
	}

	[super drawWithFrame:cellFrame inView:controlView];
}

- (NSSize)cellSize
{
	NSSize cellSize = [super cellSize];
	cellSize.width += 55.0;
	return cellSize;
}
@end
