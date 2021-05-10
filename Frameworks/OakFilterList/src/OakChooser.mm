#import "OakChooser.h"
#import "ui/TableView.h"
#import "ui/SearchField.h"
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/ranker.h>

@interface OakFileTableCellView ()
@property (nonatomic) NSTextField* folderTextField;
@property (nonatomic) NSButton* closeButton;
@end

@implementation OakFileTableCellView
- (instancetype)initWithCloseButton:(NSButton*)closeButton
{
	if((self = [super init]))
	{
		NSImageView* imageView = [NSImageView new];
		[imageView setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[imageView setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSTextField* fileTextField = OakCreateLabel(@"", [NSFont systemFontOfSize:13]);
		NSTextField* folderTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:10]);

		fileTextField.lineBreakMode        = NSLineBreakByTruncatingTail;
		fileTextField.cell.lineBreakMode   = NSLineBreakByTruncatingTail;
		folderTextField.lineBreakMode      = NSLineBreakByTruncatingHead;
		folderTextField.cell.lineBreakMode = NSLineBreakByTruncatingHead;

		NSDictionary* views = @{ @"icon": imageView, @"file": fileTextField, @"folder": folderTextField, @"close": closeButton };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[file]-(4)-[close(==16)]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(2)-[folder]-(5)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[imageView.centerYAnchor constraintEqualToAnchor:self.centerYAnchor].active   = YES;
		[closeButton.centerYAnchor constraintEqualToAnchor:self.centerYAnchor].active = YES;

		[imageView bind:NSValueBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[fileTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.name" options:nil];
		[folderTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.folder" options:nil];

		self.imageView       = imageView;
		self.textField       = fileTextField;
		self.folderTextField = folderTextField;
		self.closeButton     = closeButton;
	}
	return self;
}

- (NSAttributedString*)selectedStringForString:(id)aString
{
	NSMutableAttributedString* str = [aString isKindOfClass:[NSString class]] ? [[NSMutableAttributedString alloc] initWithString:aString attributes:nil] : [aString mutableCopy];
	[str enumerateAttributesInRange:NSMakeRange(0, str.length) options:NSAttributedStringEnumerationLongestEffectiveRangeNotRequired usingBlock:^(NSDictionary* attrs, NSRange range, BOOL *stop){
		if(attrs[NSBackgroundColorAttributeName] != nil)
			[str addAttribute:NSBackgroundColorAttributeName value:[NSColor tmMatchedTextSelectedBackgroundColor] range:range];
		if(attrs[NSUnderlineColorAttributeName] != nil)
			[str addAttribute:NSUnderlineColorAttributeName value:[NSColor tmMatchedTextSelectedUnderlineColor] range:range];
	}];
	return str;
}

- (void)setBackgroundStyle:(NSBackgroundStyle)backgroundStyle
{
	[super setBackgroundStyle:backgroundStyle];
	if(backgroundStyle == NSBackgroundStyleDark)
	{
		self.textField.objectValue       = [self selectedStringForString:[self valueForKeyPath:@"objectValue.name"]];
		self.folderTextField.textColor   = [NSColor colorWithCalibratedWhite:0.9 alpha:1];
		self.folderTextField.objectValue = [self selectedStringForString:[self valueForKeyPath:@"objectValue.folder"]];
	}
	else
	{
		self.textField.objectValue       = [self valueForKeyPath:@"objectValue.name"];
		self.folderTextField.textColor   = [NSColor colorWithCalibratedWhite:0.5 alpha:1];
		self.folderTextField.objectValue = [self valueForKeyPath:@"objectValue.folder"];
	}
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute])
			return @[ self.textField.cell, self.folderTextField.cell, self.closeButton.cell, self.imageView.cell ];
	else	return [super accessibilityAttributeValue:attribute];
}
@end

NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, NSLineBreakMode lineBreakMode)
{
	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:lineBreakMode];

	NSDictionary* baseAttributes      = @{ NSParagraphStyleAttributeName: paragraphStyle };
	NSDictionary* highlightAttributes = @{
		NSParagraphStyleAttributeName:  paragraphStyle,
		NSBackgroundColorAttributeName: [NSColor tmMatchedTextBackgroundColor],
		NSUnderlineStyleAttributeName:  @(NSUnderlineStyleSingle),
		NSUnderlineColorAttributeName:  [NSColor tmMatchedTextUnderlineColor],
	};

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	size_t from = 0;
	for(auto range : ranges)
	{
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:(to_ns(in.substr(from, range.first - from)) ?: @"?") attributes:baseAttributes]];
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:(to_ns(in.substr(range.first, range.second - range.first)) ?: @"?") attributes:highlightAttributes]];
		from = range.second;
	}
	if(from < in.size())
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:(to_ns(in.substr(from)) ?: @"?") attributes:baseAttributes]];

	return res;
}

@interface OakChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDataSource, NSTableViewDelegate, NSSearchFieldDelegate>
{
	NSTitlebarAccessoryViewController* _accessoryViewController;

	NSSearchField*      _searchField;
	NSScrollView*       _scrollView;
	NSTableView*        _tableView;
	NSVisualEffectView* _footerView;
	NSTextField*        _statusTextField;
	NSTextField*        _itemCountTextField;
}
@end

static void* kFirstResponderObserverContext = &kFirstResponderObserverContext;

@implementation OakChooser
- (id)init
{
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable) backing:NSBackingStoreBuffered defer:NO]]))
	{
		_items = @[ ];

		[[self.window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[self.window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		self.window.level             = NSFloatingWindowLevel;
		self.window.frameAutosaveName = NSStringFromClass([self class]);
		self.window.delegate          = self;

		[self.window addObserver:self forKeyPath:@"firstResponder" options:NSKeyValueObservingOptionOld|NSKeyValueObservingOptionNew context:kFirstResponderObserverContext];
	}
	return self;
}

- (void)dealloc
{
	_searchField.delegate = nil;
	[_searchField unbind:NSValueBinding];
	[self.window removeObserver:self forKeyPath:@"firstResponder" context:kFirstResponderObserverContext];

	_tableView.target     = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;
}

// =====================
// = View Construction =
// =====================

- (void)addTitlebarAccessoryView:(NSView*)titlebarView
{
	titlebarView.translatesAutoresizingMaskIntoConstraints = NO;

	_accessoryViewController = [[NSTitlebarAccessoryViewController alloc] init];
	_accessoryViewController.view = titlebarView;
	[_accessoryViewController.view setFrameSize:titlebarView.fittingSize];
	[self.window addTitlebarAccessoryViewController:_accessoryViewController];
}

- (void)updateScrollViewInsets
{
	NSEdgeInsets insets = self.scrollView.contentInsets;
	insets.bottom += self.footerView.fittingSize.height;
	self.scrollView.automaticallyAdjustsContentInsets = NO;
	self.scrollView.contentInsets = insets;
}

- (NSSearchField*)searchField
{
	if(!_searchField)
	{
		_searchField = [[OakLinkedSearchField alloc] initWithFrame:NSZeroRect];
		[_searchField.cell setScrollable:YES];
		[_searchField.cell setSendsSearchStringImmediately:YES];
		_searchField.accessibilitySharedFocusElements = @[ self.tableView ];
		if(!NSApp.isFullKeyboardAccessEnabled)
			_searchField.focusRingType = NSFocusRingTypeNone;
		_searchField.delegate = self;

		[_searchField bind:NSValueBinding toObject:self withKeyPath:@"filterString" options:nil];
	}
	return _searchField;
}

- (NSTableView*)tableView
{
	if(!_tableView)
	{
		_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		_tableView.headerView              = nil;
		_tableView.focusRingType           = NSFocusRingTypeNone;
		_tableView.allowsEmptySelection    = NO;
		_tableView.allowsMultipleSelection = NO;
		_tableView.refusesFirstResponder   = YES;
		_tableView.doubleAction            = @selector(accept:);
		_tableView.target                  = self;
		_tableView.dataSource              = self;
		_tableView.delegate                = self;

		[_tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"name"]];
	}
	return _tableView;
}

- (NSScrollView*)scrollView
{
	if(!_scrollView)
	{
		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = self.tableView;

		NSView* contentView = self.window.contentView;
		_scrollView.translatesAutoresizingMaskIntoConstraints = NO;
		[contentView addSubview:_scrollView positioned:NSWindowBelow relativeTo:nil];

		NSDictionary* views = @{ @"scrollView": _scrollView };
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[scrollView]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView]|" options:0 metrics:nil views:views]];
	}
	return _scrollView;
}

- (NSTextField*)statusTextField
{
	if(!_statusTextField)
	{
		_statusTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_statusTextField.bezeled         = NO;
		_statusTextField.bordered        = NO;
		_statusTextField.drawsBackground = NO;
		_statusTextField.editable        = NO;
		_statusTextField.font            = OakStatusBarFont();
		_statusTextField.selectable      = NO;
		[[_statusTextField cell] setLineBreakMode:NSLineBreakByTruncatingMiddle];
		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	}
	return _statusTextField;
}

- (NSTextField*)itemCountTextField
{
	if(!_itemCountTextField)
	{
		_itemCountTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_itemCountTextField.bezeled         = NO;
		_itemCountTextField.bordered        = NO;
		_itemCountTextField.drawsBackground = NO;
		_itemCountTextField.editable        = NO;
		_itemCountTextField.font            = OakStatusBarFont();
		_itemCountTextField.selectable      = NO;
		[_itemCountTextField setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSFontDescriptor* descriptor = [_itemCountTextField.font.fontDescriptor fontDescriptorByAddingAttributes:@{
			NSFontFeatureSettingsAttribute: @[ @{ NSFontFeatureTypeIdentifierKey: @(kNumberSpacingType), NSFontFeatureSelectorIdentifierKey: @(kMonospacedNumbersSelector) } ]
		}];
		_itemCountTextField.font = [NSFont fontWithDescriptor:descriptor size:0];
	}
	return _itemCountTextField;
}

- (NSVisualEffectView*)footerView
{
	if(!_footerView)
	{
		_footerView = [[NSVisualEffectView alloc] initWithFrame:NSZeroRect];
		_footerView.blendingMode = NSVisualEffectBlendingModeWithinWindow;
		_footerView.material     = NSVisualEffectMaterialTitlebar;
		if(@available(macos 10.14, *))
			_footerView.material = NSVisualEffectMaterialHeaderView;

		NSView* contentView = self.window.contentView;
		contentView.wantsLayer = YES;
		_footerView.translatesAutoresizingMaskIntoConstraints = NO;
		[contentView addSubview:_footerView positioned:NSWindowAbove relativeTo:nil];

		NSDictionary* views = @{ @"footerView": _footerView, };
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(>=77)-[footerView]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[footerView]|" options:0 metrics:nil views:views]];
	}
	return _footerView;
}

// =====================

- (void)showWindow:(id)sender
{
	if(self.isWindowLoaded && self.window.isVisible && self.window.isKeyWindow)
	{
		[self.window close];
		return;
	}

	[self.window makeFirstResponder:self.window.initialFirstResponder];
	[super showWindow:sender];
}

- (void)showWindowRelativeToFrame:(NSRect)parentFrame
{
	if(![self.window isVisible])
	{
		[self.window layoutIfNeeded];
		NSRect frame  = [self.window frame];
		NSRect parent = parentFrame;

		frame.origin.x = NSMinX(parent) + round((NSWidth(parent)  - NSWidth(frame))  * 1 / 4);
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[self.window setFrame:frame display:NO];
	}
	[self showWindow:self];
}

// ================================================================================
// = Set whether to render table view as active when search field gain/lose focus =
// ================================================================================

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kFirstResponderObserverContext)
	{
		BOOL oldIsSearchField = change[NSKeyValueChangeOldKey] == _searchField || change[NSKeyValueChangeOldKey] == _searchField.currentEditor;
		BOOL newIsSearchField = change[NSKeyValueChangeNewKey] == _searchField || change[NSKeyValueChangeNewKey] == _searchField.currentEditor;
		if(oldIsSearchField != newIsSearchField)
			self.drawTableViewAsHighlighted = newIsSearchField && self.tableView.refusesFirstResponder;
	}
}

// ======================================================
// = Forward Search Field Movement Actions to TableView =
// ======================================================

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	NSUInteger res = OakPerformTableViewActionFromSelector(self.tableView, aCommand);
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

	[self updateFilterString:_filterString];

	// see https://lists.apple.com/archives/accessibility-dev/2014/Aug/msg00024.html
	NSAccessibilityPostNotification(_tableView, NSAccessibilitySelectedRowsChangedNotification);
}

- (void)updateFilterString:(NSString*)aString
{
	if([_tableView numberOfRows] != 0)
	{
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
		[_tableView scrollRowToVisible:0];
	}
	[self updateItems:self];
}

- (void)setItems:(NSArray*)anArray
{
	NSIndexSet* oldSelectedRowIndexes = _tableView.selectedRowIndexes;
	NSArray* selectedItems;
	if([oldSelectedRowIndexes count] > 1 || [oldSelectedRowIndexes count] == 1 && [oldSelectedRowIndexes firstIndex] > 0)
		selectedItems = [_items objectsAtIndexes:oldSelectedRowIndexes];

	_items = anArray;
	[_tableView reloadData];

	NSMutableIndexSet* rowIndexes = [NSMutableIndexSet new];
	for(id item in selectedItems)
	{
		NSUInteger row = [_items indexOfObject:item];
		if(row != NSNotFound)
			[rowIndexes addIndex:row];
	}

	if([rowIndexes count] == 0)
		[rowIndexes addIndex:0];

	[_tableView selectRowIndexes:rowIndexes byExtendingSelection:NO];
	if([NSUserDefaults.standardUserDefaults boolForKey:@"disableFilterListAutoScroll"] == NO)
		[_tableView scrollRowToVisible:[rowIndexes firstIndex]];

	[self updateStatusText:self];

	self.itemCountTextField.stringValue = [NSString stringWithFormat:@"%@ item%s", [NSNumberFormatter localizedStringFromNumber:@(_items.count) numberStyle:NSNumberFormatterDecimalStyle], _items.count == 1 ? "" : "s"];
}

- (NSArray*)selectedItems
{
	return [_items objectsAtIndexes:[_tableView selectedRowIndexes]];
}

- (NSUInteger)removeItemsAtIndexes:(NSIndexSet*)anIndexSet
{
	[_tableView removeRowsAtIndexes:anIndexSet withAnimation:NSTableViewAnimationEffectFade];
	NSMutableArray* items = [_items mutableCopy];
	[items removeObjectsAtIndexes:anIndexSet];
	_items = items;
	self.itemCountTextField.stringValue = [NSString stringWithFormat:@"%@ item%s", [NSNumberFormatter localizedStringFromNumber:@(_items.count) numberStyle:NSNumberFormatterDecimalStyle], _items.count == 1 ? "" : "s"];

	if([_tableView numberOfRows] && ![[_tableView selectedRowIndexes] count] && [anIndexSet count])
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:MIN([anIndexSet firstIndex], [_tableView numberOfRows]-1)] byExtendingSelection:NO];

	return anIndexSet.count;
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
	[self.window orderOut:self];
	if(_action)
		[NSApp sendAction:_action to:_target from:self];
	[self.window close];
}

- (void)cancel:(id)sender
{
	[self.window performClose:self];
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
