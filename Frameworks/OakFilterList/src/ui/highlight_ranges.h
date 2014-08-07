extern NSString* const FLMatchingTextAttributeName;

NSAttributedString* AttributedStringWithMarkedUpRanges (std::string const& string, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset = 0);
void HighlightRangesWithAttribute (NSMutableAttributedString* text, NSString* attribute, NSDictionary* highlightAttributes);
