#import "OakChooser.h"
#import "ui/TableView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/ranker.h>

NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset)
{
	NSDictionary* baseAttributes      = @{ };
	NSDictionary* highlightAttributes = @{ NSUnderlineStyleAttributeName : @1 };

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	size_t from = 0;
	for(auto range : ranges)
	{
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(in.begin() + from, in.begin() + range.first + offset)] attributes:baseAttributes]];
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(in.begin() + range.first + offset, in.begin() + range.second + offset)] attributes:highlightAttributes]];
		from = range.second + offset;
	}
	if(from < in.size())
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:in.substr(from)] attributes:baseAttributes]];

	return res;
}

@interface OakChooser () <NSWindowDelegate, NSTableViewDataSource, NSTableViewDelegate>
@end

@implementation OakChooser
- (id)init
{
	if((self = [super init]))
	{
		_items = @[ ];

		_searchField = [[NSSearchField alloc] initWithFrame:NSZeroRect];
		[_searchField.cell setScrollable:YES];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(controlTextDidChange:) name:NSControlTextDidChangeNotification object:_searchField];
		if(![NSApp isFullKeyboardAccessEnabled])
			_searchField.focusRingType = NSFocusRingTypeNone;

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"name"];
		tableColumn.dataCell = [[NSTextFieldCell alloc] initTextCell:@""];

		OakInactiveTableView* tableView = [[OakInactiveTableView alloc] initWithFrame:NSZeroRect];
		[tableView addTableColumn:tableColumn];
		tableView.headerView              = nil;
		tableView.focusRingType           = NSFocusRingTypeNone;
		tableView.allowsEmptySelection    = NO;
		tableView.allowsMultipleSelection = NO;
		tableView.refusesFirstResponder   = YES;
		tableView.doubleAction            = @selector(accept:);
		tableView.target                  = self;
		tableView.dataSource              = self;
		tableView.delegate                = self;
		tableView.linkedTextField         = _searchField;
		_tableView = tableView;

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _tableView;

		_statusTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_statusTextField.bezeled         = NO;
		_statusTextField.bordered        = NO;
		_statusTextField.drawsBackground = NO;
		_statusTextField.editable        = NO;
		_statusTextField.font            = OakStatusBarFont();
		_statusTextField.selectable      = NO;
		[[_statusTextField cell] setBackgroundStyle:NSBackgroundStyleRaised];
		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

		_itemCountTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_itemCountTextField.bezeled         = NO;
		_itemCountTextField.bordered        = NO;
		_itemCountTextField.drawsBackground = NO;
		_itemCountTextField.editable        = NO;
		_itemCountTextField.font            = OakStatusBarFont();
		_itemCountTextField.selectable      = NO;
		[[_itemCountTextField cell] setBackgroundStyle:NSBackgroundStyleRaised];
		[_itemCountTextField setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];

		_window = [[NSPanel alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		[_window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[_window setAutorecalculatesContentBorderThickness:NO forEdge:NSMinYEdge];
		[_window setContentBorderThickness:57 forEdge: NSMaxYEdge];
		[_window setContentBorderThickness:23 forEdge: NSMinYEdge];
		[[_window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[_window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		_window.autorecalculatesKeyViewLoop = YES;
		_window.delegate                    = self;
		_window.level                       = NSFloatingWindowLevel;
		_window.releasedWhenClosed          = NO;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSControlTextDidChangeNotification object:_searchField];

	_window.delegate      = nil;
	_tableView.target     = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;
}

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	self.filterString = _searchField.stringValue;
}

- (void)showWindow:(id)sender
{
	[_window makeKeyAndOrderFront:self];
	[_window makeFirstResponder:_searchField];
}

- (void)showWindowRelativeToFrame:(NSRect)parentFrame
{
	if(![_window isVisible])
	{
		[_window layoutIfNeeded];
		NSRect frame  = [_window frame];
		NSRect parent = parentFrame;

		frame.origin.x = NSMinX(parent) + round((NSWidth(parent)  - NSWidth(frame))  * 1 / 4);
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[_window setFrame:frame display:NO];
	}
	[self showWindow:self];
}

- (void)close
{
	[_window performClose:self];
}

// ==============
// = Properties =
// ==============

- (void)setFilterString:(NSString*)aString
{
	if(_filterString == aString || [_filterString isEqualToString:aString])
		return;

	_filterString = [aString copy];
	_searchField.stringValue = aString;

	[self updateItems:self];
}

- (void)setItems:(NSArray*)anArray
{
	_items = anArray;
	[_tableView reloadData];
	[_tableView scrollRowToVisible:_tableView.selectedRow == -1 ? 0 : _tableView.selectedRow];

	[self updateStatusText:self];

	_itemCountTextField.stringValue = [NSString stringWithFormat:@"%@ item%s", [NSNumberFormatter localizedStringFromNumber:@(_items.count) numberStyle:NSNumberFormatterDecimalStyle], _items.count == 1 ? "" : "s"];
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	NSIndexSet* indexes = [_tableView selectedRowIndexes];
	for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
		[res addObject:_items[i]];
	return res;
}

// =================
// = Action Method =
// =================

- (void)accept:(id)sender
{
	[_window orderOut:self];
	if(_action)
		[NSApp sendAction:_action to:_target from:self];
	[_window close];
}

- (void)cancel:(id)sender
{
	[self close];
}

// =========================
// = NSTableViewDataSource =
// =========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return _items.count;
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [_items[rowIndex] objectForKey:aTableColumn.identifier];
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	[self updateStatusText:self];
}

// ========================
// = Overload in subclass =
// ========================

- (void)updateItems:(id)sender
{
}

- (void)updateStatusText:(id)sender
{
	_statusTextField.stringValue = @"";
}
@end
