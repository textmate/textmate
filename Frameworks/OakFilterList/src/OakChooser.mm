#import "OakChooser.h"
#import "ui/TableView.h"
#import "ui/SearchField.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/ranker.h>

@interface OakFileTableCellView ()
@property (nonatomic) NSTextField* folderTextField;
@end

@implementation OakFileTableCellView
- (instancetype)initWithCloseButton:(NSButton*)closeButton
{
	if((self = [super init]))
	{
		NSImageView* imageView = [NSImageView new];
		[imageView setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSTextField* fileTextField = OakCreateLabel(@"", [NSFont boldSystemFontOfSize:14]);
		NSTextField* folderTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:10]);

		NSDictionary* views = @{ @"icon" : imageView, @"file" : fileTextField, @"folder" : folderTextField, @"close" : closeButton };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[file]-(4)-[close(==16)]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file][folder]-(4)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:imageView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:closeButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[imageView bind:NSValueBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[fileTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.name" options:nil];
		[folderTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.folder" options:nil];

		self.imageView       = imageView;
		self.textField       = fileTextField;
		self.folderTextField = folderTextField;
	}
	return self;
}

- (void)setBackgroundStyle:(NSBackgroundStyle)backgroundStyle
{
	[super setBackgroundStyle:backgroundStyle];
	if(backgroundStyle == NSBackgroundStyleDark)
	{
		NSMutableAttributedString* str = [self.textField.attributedStringValue mutableCopy];
		if(!str)
			str = [[NSMutableAttributedString alloc] initWithString:self.textField.stringValue attributes:nil];

		NSShadow* shadow = [NSShadow new];
		[shadow setShadowColor:[NSColor colorWithCalibratedWhite:0 alpha:0.5]];
		[shadow setShadowOffset:NSMakeSize(0, -1)];
		[shadow setShadowBlurRadius:1];

		[str addAttributes:@{ NSShadowAttributeName : shadow } range:NSMakeRange(0, str.string.length)];
		self.textField.attributedStringValue = str;

		self.folderTextField.textColor = [NSColor colorWithCalibratedWhite:0.9 alpha:1];
	}
	else
	{
		if(NSMutableAttributedString* str = [self.textField.attributedStringValue mutableCopy])
		{
			[str removeAttribute:NSShadowAttributeName range:NSMakeRange(0, str.string.length)];
			self.textField.attributedStringValue = str;
		}

		self.folderTextField.textColor = [NSColor colorWithCalibratedWhite:0.5 alpha:1];
	}
}
@end

NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset)
{
	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];

	NSDictionary* baseAttributes      = @{ NSParagraphStyleAttributeName : paragraphStyle };
	NSDictionary* highlightAttributes = @{ NSParagraphStyleAttributeName : paragraphStyle, NSUnderlineStyleAttributeName : @1 };

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

@interface OakChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDataSource, NSTableViewDelegate>
@end

static void* kFirstResponderBinding = &kFirstResponderBinding;

@implementation OakChooser
- (id)init
{
	if((self = [super init]))
	{
		_items = @[ ];

		_searchField = [[OakLinkedSearchField alloc] initWithFrame:NSZeroRect];
		[_searchField.cell setScrollable:YES];
		[_searchField.cell setSendsSearchStringImmediately:YES];
		if(![NSApp isFullKeyboardAccessEnabled])
			_searchField.focusRingType = NSFocusRingTypeNone;
		_searchField.delegate = self;

		NSTableView* tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		[tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"name"]];
		tableView.headerView              = nil;
		tableView.focusRingType           = NSFocusRingTypeNone;
		tableView.allowsEmptySelection    = NO;
		tableView.allowsMultipleSelection = NO;
		tableView.refusesFirstResponder   = YES;
		tableView.doubleAction            = @selector(accept:);
		tableView.target                  = self;
		tableView.dataSource              = self;
		tableView.delegate                = self;
		if(nil != &NSAccessibilitySharedFocusElementsAttribute)
			[_searchField.cell accessibilitySetOverrideValue:@[tableView] forAttribute:NSAccessibilitySharedFocusElementsAttribute];
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
		[[_statusTextField cell] setLineBreakMode:NSLineBreakByTruncatingMiddle];
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
		[_window setContentBorderThickness:32 forEdge:NSMaxYEdge];
		[_window setContentBorderThickness:23 forEdge:NSMinYEdge];
		[[_window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[_window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		_window.delegate           = self;
		_window.nextResponder      = self;
		_window.level              = NSFloatingWindowLevel;
		_window.releasedWhenClosed = NO;

		[_searchField bind:NSValueBinding toObject:self withKeyPath:@"filterString" options:nil];
		[_window addObserver:self forKeyPath:@"firstResponder" options:NSKeyValueObservingOptionOld|NSKeyValueObservingOptionNew context:kFirstResponderBinding];
	}
	return self;
}

- (void)dealloc
{
	_searchField.delegate = nil;
	[_searchField unbind:NSValueBinding];
	[_window removeObserver:self forKeyPath:@"firstResponder" context:kFirstResponderBinding];

	_window.delegate      = nil;
	_tableView.target     = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;
}

- (void)showWindow:(id)sender
{
	[_window layoutIfNeeded];
	[_window recalculateKeyViewLoop];
	[_searchField.window makeFirstResponder:_searchField];
	[_window makeKeyAndOrderFront:self];
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

// ===============================================================================
// = Set wether to render table view as active when search field gain/lose focus =
// ===============================================================================

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kFirstResponderBinding)
	{
		BOOL oldIsSearchField = change[NSKeyValueChangeOldKey] == _searchField || change[NSKeyValueChangeOldKey] == _searchField.currentEditor;
		BOOL newIsSearchField = change[NSKeyValueChangeNewKey] == _searchField || change[NSKeyValueChangeNewKey] == _searchField.currentEditor;
		if(oldIsSearchField != newIsSearchField)
			self.drawTableViewAsHighlighted = newIsSearchField;
	}
}

// ======================================================
// = Forward Search Field Movement Actions to TableView =
// ======================================================

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	if(aCommand == @selector(deleteToBeginningOfLine:) && [aTextView.window tryToPerform:@selector(delete:) with:aTextView])
		return YES;

	NSUInteger res = OakPerformTableViewActionFromSelector(self.tableView, aCommand, aTextView);
	if(res == OakMoveAcceptReturn)
		[self performDefaultButtonClick:self];
	else if(res == OakMoveCancelReturn)
		[self cancel:self];
	return res != OakMoveNoActionReturn;
}

// ==============
// = Properties =
// ==============

- (void)setFilterString:(NSString*)aString
{
	if(_filterString == aString || [_filterString isEqualToString:aString])
		return;

	_filterString = [aString copy];
	_searchField.stringValue = aString ?: @"";

	if([_tableView numberOfRows] != 0)
	{
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
		[_tableView scrollRowToVisible:0];
	}

	[self updateItems:self];

	// see http://lists.apple.com/archives/accessibility-dev/2014/Aug/msg00024.html
	if(nil != &NSAccessibilitySharedFocusElementsAttribute)
		NSAccessibilityPostNotification(_tableView, NSAccessibilitySelectedRowsChangedNotification);
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
	return [_items objectsAtIndexes:[_tableView selectedRowIndexes]];
}

- (void)removeItemsAtIndexes:(NSIndexSet*)anIndexSet
{
	[_tableView removeRowsAtIndexes:anIndexSet withAnimation:NSTableViewAnimationEffectFade];
	NSMutableArray* items = [_items mutableCopy];
	[items removeObjectsAtIndexes:anIndexSet];
	_items = items;

	if([_tableView numberOfRows] && ![[_tableView selectedRowIndexes] count] && [anIndexSet count])
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:MIN([anIndexSet firstIndex], [_tableView numberOfRows]-1)] byExtendingSelection:NO];
}

// =================
// = Action Method =
// =================

- (void)performDefaultButtonClick:(id)sender
{
	if(self.window.defaultButtonCell)
			[self.window.defaultButtonCell performClick:sender];
	else	[self accept:sender];
}

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

- (NSTableRowView*)tableView:(NSTableView*)tableView rowViewForRow:(NSInteger)row
{
	return [OakInactiveTableRowView new];
}

- (void)setDrawTableViewAsHighlighted:(BOOL)flag
{
	_drawTableViewAsHighlighted = flag;
	[_tableView enumerateAvailableRowViewsUsingBlock:^(NSTableRowView* rowView, NSInteger row){
		[(OakInactiveTableRowView*)rowView setDrawAsHighlighted:flag];
	}];
}

- (void)tableView:(NSTableView*)tableView didAddRowView:(NSTableRowView*)rowView forRow:(NSInteger)row
{
	[(OakInactiveTableRowView*)rowView setDrawAsHighlighted:_drawTableViewAsHighlighted];
}

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	NSString* identifier = aTableColumn.identifier;
	NSTextField* res = [aTableView makeViewWithIdentifier:identifier owner:self];
	if(!res)
	{
		res = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		res.identifier = identifier;
	}
	[res setStringValue:[_items[row] objectForKey:identifier]];
	return res;
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
