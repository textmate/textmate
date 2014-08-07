#import "OakBundleItemCell.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import "../highlight_ranges.h"

@implementation OakBundleItemCell
- (id)copyWithZone:(NSZone*)zone
{
	OakBundleItemCell* cell   = [super copyWithZone:zone];
	cell.keyEquivalentString  = [self.keyEquivalentString copy];
	cell.attributedTabTrigger = [self.attributedTabTrigger copy];
	return cell;
}

- (NSString*)tabTriggerString
{
	return self.attributedTabTrigger.string;
}

- (void)setTabTriggerString:(NSString*)tabTrigger
{
	self.attributedTabTrigger = tabTrigger ? [[NSAttributedString alloc] initWithString:tabTrigger attributes:nil] : nil;
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView*)controlView
{
	if(OakNotEmptyString(self.attributedTabTrigger.string))
	{
		NSMutableAttributedString* attrStr = [self.attributedTabTrigger mutableCopy];

		NSDictionary* highlightAttributes = @{ NSUnderlineStyleAttributeName : @1, NSFontAttributeName : [NSFont boldSystemFontOfSize:[NSFont systemFontSizeForControlSize:NSSmallControlSize]] };
		HighlightRangesWithAttribute(attrStr, FLMatchingTextAttributeName, highlightAttributes);

		[attrStr appendAttributedString:[[NSAttributedString alloc] initWithString:@"⇥" attributes:nil]];
		CFTypeRef str = (CFAttributedStringRef)CFBridgingRetain(attrStr);

		HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, kThemeSmallSystemFont, kHIThemeTextHorizontalFlushRight, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
		CGFloat width = 0;
		HIThemeGetTextDimensions(str, 0, &textInfo, &width, NULL, NULL);

		NSRect triggerFrame;
		NSDivideRect(cellFrame, &triggerFrame, &cellFrame, width + 12, NSMaxXEdge);
		cellFrame.size.width -= 10;
		triggerFrame.size.width -= 2;

		[[NSColor colorWithCalibratedRed:0 green:0 blue:0 alpha:0.15] set];
		NSRect podRect = triggerFrame;
		[[NSBezierPath bezierPathWithRoundedRect:podRect xRadius:4 yRadius:4] fill];

		HIRect bounds = { { NSMinX(podRect) - 5, NSMinY(podRect) }, { NSWidth(podRect), NSHeight(podRect) } };
		[[NSColor textColor] set];
		if([self isHighlighted] && ([[controlView window] firstResponder] == controlView || ([[[controlView window] firstResponder] respondsToSelector:@selector(delegate)] && [[[controlView window] firstResponder] performSelector:@selector(delegate)] == controlView)) && [[controlView window] isKeyWindow])
			[[NSColor alternateSelectedControlTextColor] set];

		CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
		HIThemeDrawTextBox(str, &bounds, &textInfo, context, kHIThemeOrientationNormal);

		CFRelease(str);
	}
	else if(OakNotEmptyString(self.keyEquivalentString))
	{
		size_t keyStartsAt = 0;
		std::string const glyphString = ns::glyphs_for_event_string(to_s(self.keyEquivalentString), &keyStartsAt);
		NSString* modifiers = [NSString stringWithCxxString:glyphString.substr(0, keyStartsAt)];
		NSString* key       = [NSString stringWithCxxString:glyphString.substr(keyStartsAt)];

		NSDictionary* fontAttr = @{ NSFontAttributeName : [self font] };
		CGFloat width = std::max<CGFloat>(1, [modifiers sizeWithAttributes:fontAttr].width);

		NSMutableAttributedString* aStr = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithFormat:@"\t%@\t%@", modifiers, key] attributes:fontAttr];
		NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
		[pStyle setTabStops:@[ [[NSTextTab alloc] initWithType:NSRightTabStopType location:width], [[NSTextTab alloc] initWithType:NSLeftTabStopType location:width + 1] ]];
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
	cellSize.width += 55;
	return cellSize;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityValueAttribute])
	{
		NSMutableString* value = [NSMutableString stringWithString:[super accessibilityAttributeValue:attribute]];

		if(OakNotEmptyString(self.attributedTabTrigger.string))
			[value appendFormat:@", tab trigger is %@", self.attributedTabTrigger.string];
		if(OakNotEmptyString(self.keyEquivalentString))
			[value appendFormat:@", shortcut is %@", [NSString stringWithCxxString:ns::glyphs_for_event_string(to_s(self.keyEquivalentString))]];

		return value;
	}
	return [super accessibilityAttributeValue:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute forParameter:(id)parameter
{
	// Make sure VoiceOver is forced to use our implementation of AXValue above
	if(   [attribute isEqualToString:NSAccessibilityAttributedStringForRangeParameterizedAttribute]
		|| [attribute isEqualToString:NSAccessibilityStringForRangeParameterizedAttribute]
		)
		return nil;

	return [super accessibilityAttributeValue:attribute forParameter:parameter];
}
@end
