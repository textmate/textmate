#import "SearchField.h"
#import <OakAppKit/src/OakAppKit.h>

static id TranslateAXRange(NSRange range, NSUInteger length, id (^process)(NSUInteger left, NSRange range, NSUInteger right), NSUInteger leftMargin = 1, NSUInteger rightMargin = 1)
{
	if(NSMaxRange(range) > leftMargin + length + rightMargin)
		@throw [NSException exceptionWithName:NSAccessibilityException
		                               reason:[NSString stringWithFormat:@"TranslateAXRange: requested range %@ out of bounds for (%@,%@,%@)", NSStringFromRange(range), @(leftMargin), @(length), @(rightMargin)]
		                             userInfo:nil];

	NSRange const leftMarginRange = NSMakeRange(0, leftMargin);
	NSRange const rightMarginRange = NSMakeRange(leftMargin + length, rightMargin);
	NSRange const middleRange = NSMakeRange(leftMargin, length);

	NSRange leftRange  = NSIntersectionRange(range, leftMarginRange);
	NSRange rightRange = NSIntersectionRange(range, rightMarginRange);
	NSRange baseRange  = NSIntersectionRange(range, middleRange);
	if(!baseRange.length)
	{
		if(leftRange.length)
			baseRange.location = leftMargin;
		else if(rightRange.length)
			baseRange.location = NSMaxRange(middleRange);
		else
			baseRange.location = range.location;
	}
	baseRange.location -= leftMargin;
	return process(leftRange.length, baseRange, rightRange.length);
}

static NSString* CreateSpacedString(NSUInteger length)
{
	return [@"" stringByPaddingToLength:length withString:@" " startingAtIndex:0];
}

@interface OakLinkedSearchFieldCell : NSSearchFieldCell
@end

// Workaround <rdar://problem/16271507> by pretending for accessibility
// that the search field has one extra space after and before the actual
// search string.
// See also https://lists.apple.com/archives/accessibility-dev/2014/Feb/msg00019.html
@implementation OakLinkedSearchFieldCell
- (id)accessibilityAttributeValue:(NSString*)attribute
{
	id value = [super accessibilityAttributeValue:attribute];
	if([attribute isEqualToString:NSAccessibilityValueAttribute])
		value = [NSString stringWithFormat:@" %@ ", value];
	else if([attribute isEqualToString:NSAccessibilityNumberOfCharactersAttribute])
		value = @(1 + [value unsignedIntegerValue] + 1);
	else if([attribute isEqualToString:NSAccessibilitySelectedTextRangeAttribute] ||
	         [attribute isEqualToString:NSAccessibilityVisibleCharacterRangeAttribute])
	{
		NSRange range = [value rangeValue];
		++range.location;
		value = [NSValue valueWithRange:range];
	}
	else if([attribute isEqualToString:NSAccessibilityHelpAttribute])
	{
		return @"Type filter string, then hear search results using arrow up/down.";
	}
	return value;
}

- (void)accessibilitySetValue:(id)value forAttribute:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilitySelectedTextRangeAttribute] ||
	    [attribute isEqualToString:NSAccessibilityVisibleCharacterRangeAttribute])
	{
		TranslateAXRange([value rangeValue], [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			[super accessibilitySetValue:[NSValue valueWithRange:range] forAttribute:attribute];
			return nil;
		});
	}
}

- (id)accessibilityAttributeValue:(NSString*)attribute forParameter:(id)parameter
{
	if([attribute isEqualToString:NSAccessibilityAttributedStringForRangeParameterizedAttribute])
	{
		return TranslateAXRange([parameter rangeValue], [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			id value = [super accessibilityAttributeValue:attribute forParameter:[NSValue valueWithRange:range]];
			NSMutableAttributedString* string = [[NSMutableAttributedString alloc] initWithAttributedString:value];
			if(left)
				[string insertAttributedString:[[NSAttributedString alloc] initWithString:CreateSpacedString(left)] atIndex:0];
			if(right)
				[string insertAttributedString:[[NSAttributedString alloc] initWithString:CreateSpacedString(right)] atIndex:string.length];
			return string;
		});
	}
	else if([attribute isEqualToString:NSAccessibilityBoundsForRangeParameterizedAttribute])
	{
		return TranslateAXRange([parameter rangeValue], [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			return [super accessibilityAttributeValue:attribute forParameter:[NSValue valueWithRange:range]];
		});
	}
	else if([attribute isEqualToString:NSAccessibilityLineForIndexParameterizedAttribute])
	{
		return TranslateAXRange(NSMakeRange([parameter unsignedIntegerValue], 0), [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			return [super accessibilityAttributeValue:attribute forParameter:@(range.location)];
		});
	}
	else if([attribute isEqualToString:NSAccessibilityRangeForIndexParameterizedAttribute])
	{
		return TranslateAXRange(NSMakeRange([parameter unsignedIntegerValue], 0), [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			if(left)
				return [NSValue valueWithRange:NSMakeRange(0, 1)];
			if(right)
				return [NSValue valueWithRange:NSMakeRange(NSMaxRange(range), 1)];
			NSRange ret = [[super accessibilityAttributeValue:attribute forParameter:@(range.location)] rangeValue];
			++ret.location;
			return [NSValue valueWithRange:ret];
		});
	}
	else if([attribute isEqualToString:NSAccessibilityRangeForLineParameterizedAttribute] ||
	         [attribute isEqualToString:NSAccessibilityRangeForPositionParameterizedAttribute])
	{
		NSRange ret = [[super accessibilityAttributeValue:attribute forParameter:parameter] rangeValue];
		++ret.location;
		return [NSValue valueWithRange:ret];
	}
	else if([attribute isEqualToString:NSAccessibilityRTFForRangeParameterizedAttribute])
	{
		return TranslateAXRange([parameter rangeValue], [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			NSData* data = [super accessibilityAttributeValue:attribute forParameter:[NSValue valueWithRange:range]];
			NSDictionary* documentAttributes = nil;
			NSMutableAttributedString* string = [[NSMutableAttributedString alloc] initWithRTF:data documentAttributes:&documentAttributes];
			if(left)
				[string insertAttributedString:[[NSAttributedString alloc] initWithString:CreateSpacedString(left)] atIndex:0];
			if(right)
				[string insertAttributedString:[[NSAttributedString alloc] initWithString:CreateSpacedString(right)] atIndex:string.length];
			return [string RTFFromRange:NSMakeRange(0, string.length) documentAttributes:documentAttributes];
		});
	}
	else if([attribute isEqualToString:NSAccessibilityStringForRangeParameterizedAttribute])
	{
		return TranslateAXRange([parameter rangeValue], [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			id value = [super accessibilityAttributeValue:attribute forParameter:[NSValue valueWithRange:range]];
			return [NSString stringWithFormat:@"%@%@%@",
			                 [[NSAttributedString alloc] initWithString:CreateSpacedString(!!left)],
			                 value,
			                 [[NSAttributedString alloc] initWithString:CreateSpacedString(!!right)]
			];
		});
	}
	else if([attribute isEqualToString:NSAccessibilityStyleRangeForIndexParameterizedAttribute])
	{
		return TranslateAXRange(NSMakeRange([parameter unsignedIntegerValue], 0), [[super accessibilityAttributeValue:NSAccessibilityNumberOfCharactersAttribute] unsignedIntegerValue], ^id(NSUInteger left, NSRange range, NSUInteger right) {
			if(left)
				return [NSValue valueWithRange:NSMakeRange(0, 1)];
			if(right)
				return [NSValue valueWithRange:NSMakeRange(NSMaxRange(range), 1)];
			NSRange ret = [[super accessibilityAttributeValue:attribute forParameter:@(range.location)] rangeValue];
			++ret.location;
			return [NSValue valueWithRange:ret];
		});
	}
	else
	{
		return [super accessibilityAttributeValue:attribute forParameter:parameter];
	}
}
@end

@implementation OakLinkedSearchField
+ (void)initialize
{
	[OakLinkedSearchField setCellClass:[OakLinkedSearchFieldCell class]];
}
@end
