@interface OakInactiveTableView : NSTableView
@property (nonatomic, weak) NSTextField* linkedTextField;
@end

NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (NSFont* baseFont, NSColor* textColor, NSColor* matchedTextColor, std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset = 0);
