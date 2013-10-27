#include <oak/misc.h>

NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (NSFont* baseFont, NSColor* textColor, NSColor* matchedTextColor, std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset = 0);

PUBLIC @interface OakChooser : NSObject
@property (nonatomic) NSWindow* window;

@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;

@property (nonatomic) NSString* filterString;
@property (nonatomic, readonly) NSArray* selectedItems;

- (void)showWindow:(id)sender;
- (void)close;

// For subclasses
@property (nonatomic) NSArray*       items;
@property (nonatomic) NSSearchField* searchField;
@property (nonatomic) NSScrollView*  scrollView;
@property (nonatomic) NSTableView*   tableView;
@property (nonatomic) NSTextField*   statusTextField;
@property (nonatomic) NSTextField*   itemCountTextField;

- (void)windowWillClose:(NSNotification*)aNotification;
- (void)accept:(id)sender;
@end
