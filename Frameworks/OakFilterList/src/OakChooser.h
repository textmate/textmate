#include <oak/misc.h>

PUBLIC NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, NSLineBreakMode lineBreakMode = NSLineBreakByTruncatingMiddle);

PUBLIC @interface OakFileTableCellView : NSTableCellView
- (instancetype)initWithCloseButton:(NSButton*)closeButton;
@end

PUBLIC @interface OakChooser : NSWindowController
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;

@property (nonatomic) NSString* filterString;
@property (nonatomic, readonly) NSArray* selectedItems;

- (void)showWindowRelativeToFrame:(NSRect)parentFrame;

// For subclasses
@property (nonatomic) NSArray*       items;
@property (nonatomic, readonly) NSView* titleBarView;
@property (nonatomic) NSSearchField* searchField;
@property (nonatomic) NSScrollView*  scrollView;
@property (nonatomic) NSTableView*   tableView;
@property (nonatomic) NSTextField*   statusTextField;
@property (nonatomic) NSTextField*   itemCountTextField;

@property (nonatomic) BOOL drawTableViewAsHighlighted;
- (void)updateFilterString:(NSString*)aString;
- (NSUInteger)removeItemsAtIndexes:(NSIndexSet*)anIndexSet;
- (void)performDefaultButtonClick:(id)sender;
- (void)accept:(id)sender;
- (void)cancel:(id)sender;
@end
