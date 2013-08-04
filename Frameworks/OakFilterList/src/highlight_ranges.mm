#import "highlight_ranges.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <oak/oak.h>

NSString* const FLMatchingTextAttributeName = @"FLMatchingTextAttributeName";

NSAttributedString* AttributedStringWithMarkedUpRanges (std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset)
{
	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];
	NSDictionary* highlightAttrs = @{ FLMatchingTextAttributeName : [NSNull null] };
	ASSERT_LE(offset, in.size());

	std::string text(in);
	if(offset > 0)
	{
		[[res mutableString] appendString:[NSString stringWithCxxString:std::string(text.begin(), text.begin() + offset)]];
		text = std::string(text.begin() + offset, text.end());
	}

	size_t from = 0;
	iterate(it, ranges)
	{
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(text.begin() + from, text.begin() + it->first)] attributes:nil]];
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(text.begin() + it->first, text.begin() + it->second)] attributes:highlightAttrs]];
		from = it->second;
	}
	if(from < text.size())
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(text.begin() + from, text.end())] attributes:nil]];

	return res;
}

void HighlightRangesWithAttribute (NSMutableAttributedString* text, NSString* attribute, NSDictionary* highlightAttributes)
{
	NSRange range = {0, 0};
	while(range.location < text.length)
	{
		if([text attribute:attribute atIndex:range.location effectiveRange:&range])
			[text addAttributes:highlightAttributes range:range];
		range.location += range.length;
	}
}
