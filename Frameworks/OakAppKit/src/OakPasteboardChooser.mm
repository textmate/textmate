#import "OakPasteboardChooser.h"
#import "OakPasteboard.h"
#import "OakAppKit.h"
#import "OakScopeBarView.h"
#import "OakUIConstructionFunctions.h"
#import "OakSyntaxFormatter.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>

static NSUserInterfaceItemIdentifier const kTableColumnIdentifierMain = @"main";
static NSUserInterfaceItemIdentifier const kTableColumnIdentifierFlag = @"flag";

@implementation OakPasteboardEntry (DisplayString)
- (NSAttributedString*)displayString
{
	static NSAttributedString* const lineJoiner = [[NSAttributedString alloc] initWithString:@"¬" attributes:@{ NSForegroundColorAttributeName: NSColor.tertiaryLabelColor }];
	static NSAttributedString* const tabJoiner  = [[NSAttributedString alloc] initWithString:@"‣" attributes:@{ NSForegroundColorAttributeName: NSColor.tertiaryLabelColor }];
	static NSAttributedString* const ellipsis   = [[NSAttributedString alloc] initWithString:@"…" attributes:@{ NSForegroundColorAttributeName: NSColor.tertiaryLabelColor }];

	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingTail];
	NSDictionary* defaultAttributes = @{
		NSParagraphStyleAttributeName: paragraphStyle,
		NSFontAttributeName:           [NSFont controlContentFontOfSize:0]
	};

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];
	if([self.options[OakFindRegularExpressionOption] boolValue])
	{
		OakSyntaxFormatter* formatter = [[OakSyntaxFormatter alloc] initWithGrammarName:@"source.regexp.oniguruma"];
		formatter.enabled = YES;
		[res setAttributedString:[[NSMutableAttributedString alloc] initWithString:self.string attributes:defaultAttributes]];
		[formatter addStylesToString:res];
	}
	else
	{
		__block bool firstLine = true;
		[self.string enumerateLinesUsingBlock:^(NSString* line, BOOL* stop){
			if(!std::exchange(firstLine, false))
				[res appendAttributedString:lineJoiner];

			bool firstTab = true;
			for(NSString* str in [line componentsSeparatedByString:@"\t"])
			{
				if([[res string] length] > 1024)
				{
					[res appendAttributedString:ellipsis];
					*stop = YES;
					break;
				}

				if(!std::exchange(firstTab, false))
					[res appendAttributedString:tabJoiner];
				[res appendAttributedString:[[NSAttributedString alloc] initWithString:str]];
			}
		}];
		[res addAttributes:defaultAttributes range:NSMakeRange(0, res.length)];
	}

	return res;
}
@end

@interface OakPasteboardChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDelegate, NSTableViewDataSource, NSSearchFieldDelegate>
{
	NSTitlebarAccessoryViewController* _accessoryViewController;
	OakScopeBarViewController* _scopeBar;
	BOOL _skipUpdatePasteboard;
	NSButton* _actionButton;
	id _eventMonitor;
}
@property (nonatomic) OakPasteboard*        pasteboard;
@property (nonatomic) NSArrayController*    arrayController;
@property (nonatomic) NSSearchField*        searchField;
@property (nonatomic) NSScrollView*         scrollView;
@property (nonatomic) NSTableView*          tableView;
@property (nonatomic) NSVisualEffectView*   footerView;
@property (nonatomic) BOOL                  hasSelection;
@property (nonatomic) NSUInteger            sourceIndex;
@end

static NSMutableDictionary* SharedChoosers;

@implementation OakPasteboardChooser
+ (instancetype)sharedChooserForPasteboard:(OakPasteboard*)pboard
{
	SharedChoosers = SharedChoosers ?: [NSMutableDictionary new];
	return [SharedChoosers objectForKey:pboard.name] ?: [[OakPasteboardChooser alloc] initWithPasteboard:pboard];
}

- (id)initWithPasteboard:(OakPasteboard*)aPasteboard
{
	if(self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskFullSizeContentView) backing:NSBackingStoreBuffered defer:NO]])
	{
		_pasteboard = aPasteboard;

		NSString* windowTitle = @"Clipboard History";
		NSString* actionName  = @"Paste";
		if([_pasteboard isEqual:OakPasteboard.findPasteboard])
		{
			windowTitle = @"Find History";
			actionName  = @"Find Next";
		}

		_arrayController = [[NSArrayController alloc] init];

		_scopeBar = [[OakScopeBarViewController alloc] init];
		_scopeBar.labels = @[ @"All", @"Flagged" ];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:kTableColumnIdentifierMain];
		tableColumn.resizingMask = NSTableColumnAutoresizingMask;
		tableColumn.editable     = NO;
		tableColumn.dataCell     = [[NSTextFieldCell alloc] initTextCell:@""];
		[tableColumn.dataCell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		[self.tableView addTableColumn:tableColumn];

		NSTableColumn* flagColumn = [[NSTableColumn alloc] initWithIdentifier:kTableColumnIdentifierFlag];
		flagColumn.resizingMask = NSTableColumnNoResizing;
		flagColumn.editable     = NO;
		flagColumn.width        = 32;
		[self.tableView addTableColumn:flagColumn];

		[[self.window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[self.window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		self.window.autorecalculatesKeyViewLoop = YES;
		self.window.delegate                    = self;
		self.window.level                       = NSFloatingWindowLevel;
		self.window.title                       = windowTitle;

		NSDictionary* titlebarViews = @{
			@"searchField": self.searchField,
			@"dividerView": OakCreateNSBoxSeparator(),
			@"scopeBar":    _scopeBar.view,
		};

		NSView* titlebarView = [[NSView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview(titlebarViews.allValues, titlebarView);

		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField]-(8)-|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[dividerView]|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(4)-[searchField]-(8)-[dividerView(==1)]-(4)-[scopeBar]-(4)-|" options:0 metrics:nil views:titlebarViews]];

		[self addTitlebarAccessoryView:titlebarView];

		NSButton* flagButton     = OakCreateButton(@"⚑", NSBezelStyleTexturedRounded);
		NSButton* deleteButton   = OakCreateButton(@"Delete", NSBezelStyleTexturedRounded);
		NSButton* clearAllButton = OakCreateButton(@"Clear History", NSBezelStyleTexturedRounded);
		_actionButton            = OakCreateButton(actionName, NSBezelStyleTexturedRounded);

		flagButton.action     = @selector(toggleCurrentBookmark:);
		deleteButton.action   = @selector(deleteForward:);
		clearAllButton.action = @selector(clearAll:);
		_actionButton.action  = @selector(accept:);

		NSDictionary* footerViews = @{
			@"dividerView": OakCreateNSBoxSeparator(),
			@"flag":        flagButton,
			@"delete":      deleteButton,
			@"clearAll":    clearAllButton,
			@"action":      _actionButton,
		};

		NSView* footerView = self.footerView;
		OakAddAutoLayoutViewsToSuperview(footerViews.allValues, footerView);

		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[dividerView]|"                                 options:0 metrics:nil views:footerViews]];
		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[flag]-[delete]-[clearAll]-(>=20)-[action]-(8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:footerViews]];
		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[dividerView(==1)]-(5)-[clearAll]-(6)-|"             options:0 metrics:nil views:footerViews]];

		[self updateScrollViewInsets];

		self.window.defaultButtonCell = _actionButton.cell;

		[flagButton bind:NSEnabledBinding toObject:self withKeyPath:@"hasSelection" options:nil];
		[deleteButton bind:NSEnabledBinding toObject:self withKeyPath:@"hasSelection" options:nil];
		[_actionButton bind:NSEnabledBinding toObject:self withKeyPath:@"hasSelection" options:nil];
		[_scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(clipboardDidChange:) name:OakPasteboardDidChangeNotification object:_pasteboard];

		if([_pasteboard isEqual:OakPasteboard.findPasteboard])
		{
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeKeyStatus:) name:NSWindowDidBecomeKeyNotification object:self.window];
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeKeyStatus:) name:NSWindowDidResignKeyNotification object:self.window];
		}
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];

	self.window.delegate  = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;
	_tableView.target     = nil;
}

- (void)windowDidChangeKeyStatus:(NSNotification*)aNotification
{
	auto updateDefaultButton = ^NSEvent*(NSEvent* event){
		BOOL optionDown = (event.modifierFlags & NSEventModifierFlagDeviceIndependentFlagsMask) == NSEventModifierFlagOption;
		_actionButton.title = optionDown ? @"Find in Project" : @"Find Next";
		return event;
	};

	updateDefaultButton(NSApp.currentEvent);
	if(NSApp.keyWindow == self.window)
	{
		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged handler:updateDefaultButton];
	}
	else if(_eventMonitor)
	{
		[NSEvent removeMonitor:_eventMonitor];
		_eventMonitor = nil;
	}
}

// =====================
// = View Construction =
// =====================

- (void)addTitlebarAccessoryView:(NSView*)titlebarView
{
	titlebarView.translatesAutoresizingMaskIntoConstraints = NO;
	[titlebarView setFrameSize:titlebarView.fittingSize];

	_accessoryViewController = [[NSTitlebarAccessoryViewController alloc] init];
	_accessoryViewController.view = titlebarView;
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
		_searchField = [[NSSearchField alloc] initWithFrame:NSZeroRect];
		[_searchField.cell setScrollable:YES];
		[_searchField.cell setSendsSearchStringImmediately:YES];
		_searchField.delegate = self;
	}
	return _searchField;
}

- (void)refreshTableViewAndSelect:(OakPasteboardEntry*)clipboardEntry
{
	_arrayController.content = [_pasteboard entries];
	[_tableView reloadData];

	NSUInteger row = clipboardEntry ? [self.pasteboardEntries indexOfObject:clipboardEntry] : NSNotFound;
	if(row != NSNotFound)
	{
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[_tableView scrollRowToVisible:_tableView.selectedRow];
	}
	else if(NSArray* historyIds = clipboardEntry.options[@"historyIds"])
	{
		NSSet* set = [NSSet setWithArray:historyIds];
		NSIndexSet* indexSet = [self.pasteboardEntries indexesOfObjectsPassingTest:^BOOL(OakPasteboardEntry* entry, NSUInteger, BOOL*){
			return [set containsObject:@(entry.historyId)];
		}];

		if(indexSet.count)
			[_tableView selectRowIndexes:indexSet byExtendingSelection:NO];
	}
}

- (void)selectCurrentClipboardEntry:(id)sender
{
	_skipUpdatePasteboard = YES;
	[self refreshTableViewAndSelect:_pasteboard.currentEntry];
	_skipUpdatePasteboard = NO;
}

- (void)clipboardDidChange:(NSNotification*)aNotification
{
	[self selectCurrentClipboardEntry:self];
}

- (NSTableView*)tableView
{
	if(!_tableView)
	{
		_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		_tableView.allowsTypeSelect                   = NO;
		_tableView.headerView                         = nil;
		_tableView.focusRingType                      = NSFocusRingTypeNone;
		_tableView.allowsEmptySelection               = YES;
		_tableView.allowsMultipleSelection            = YES;
		_tableView.usesAlternatingRowBackgroundColors = YES;
		_tableView.doubleAction                       = @selector(accept:);
		_tableView.target                             = self;
		_tableView.delegate                           = self;
		_tableView.dataSource                         = self;
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
	[SharedChoosers setObject:self forKey:_pasteboard.name];

	[_searchField bind:NSValueBinding toObject:self withKeyPath:@"filterString" options:nil];
	[self selectCurrentClipboardEntry:self];

	[self.window makeFirstResponder:_tableView];
	[self.window makeKeyAndOrderFront:self];
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

- (void)windowWillClose:(NSNotification*)aNotification
{
	[_searchField unbind:NSValueBinding];
	[SharedChoosers performSelector:@selector(removeObjectForKey:) withObject:_pasteboard.name afterDelay:0];
}

- (IBAction)selectNextTab:(id)sender     { [_scopeBar selectNextButton:sender]; }
- (IBAction)selectPreviousTab:(id)sender { [_scopeBar selectPreviousButton:sender]; }
- (void)updateShowTabMenu:(NSMenu*)aMenu { [_scopeBar updateGoToMenu:aMenu]; }

- (void)setSourceIndex:(NSUInteger)newIndex
{
	if(_sourceIndex != newIndex)
	{
		_sourceIndex = newIndex;
		if(_sourceIndex == 0)
		{
			_arrayController.filterPredicate = nil;
			[self selectCurrentClipboardEntry:self];
		}
		else
		{
			_arrayController.filterPredicate = [NSPredicate predicateWithFormat:@"isFlagged == YES"];
			[_tableView selectRowIndexes:[NSIndexSet indexSet] byExtendingSelection:NO];
			[self refreshTableViewAndSelect:nil];
		}
	}
}

- (void)setFilterString:(NSString*)newString
{
	if(_filterString == newString || [_filterString isEqualToString:newString])
		return;

	NSIndexSet* oldSelectedRowIndexes = _tableView.selectedRowIndexes;
	NSArray<OakPasteboardEntry*>* oldSelectedEntries = [self.pasteboardEntries objectsAtIndexes:oldSelectedRowIndexes];
	NSUInteger oldRowCount = _tableView.numberOfRows;

	_filterString = newString;
	_arrayController.filterPredicate = OakIsEmptyString(_filterString) ? nil : [NSPredicate predicateWithFormat:@"string CONTAINS[cd] %@", _filterString];
	[_tableView reloadData];

	if(_tableView.numberOfRows && _tableView.numberOfRows != oldRowCount)
	{
		NSIndexSet* newSelectedRowIndexes;

		if(oldSelectedRowIndexes.count != 1 || oldSelectedRowIndexes.firstIndex != 0)
		{
			newSelectedRowIndexes = [self.pasteboardEntries indexesOfObjectsPassingTest:^BOOL(OakPasteboardEntry* entry, NSUInteger idx, BOOL* stop){
				return [oldSelectedEntries containsObject:entry];
			}];
		}

		if(newSelectedRowIndexes.count == 0)
			newSelectedRowIndexes = [NSIndexSet indexSetWithIndex:0];

		[self didSelectEntry:[self.pasteboardEntries objectAtIndex:newSelectedRowIndexes.firstIndex] updatePasteboard:YES];
	}
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aCell backgroundStyle] == NSBackgroundStyleDark && [aTableColumn.identifier isEqualToString:kTableColumnIdentifierMain])
	{
		id obj = [aCell objectValue];
		if([obj isKindOfClass:[NSAttributedString class]])
		{
			NSMutableAttributedString* str = [obj mutableCopy];
			[str addAttribute:NSForegroundColorAttributeName value:[NSColor alternateSelectedControlTextColor] range:NSMakeRange(0, [str length])];
			[str removeAttribute:NSBackgroundColorAttributeName range:NSMakeRange(0, [str length])];
			[aCell setAttributedStringValue:str];
		}
	}
}

- (void)didSelectEntry:(OakPasteboardEntry*)clipboardEntry updatePasteboard:(BOOL)flag
{
	NSUInteger row = clipboardEntry ? [self.pasteboardEntries indexOfObject:clipboardEntry] : NSNotFound;
	if(row != NSNotFound)
	{
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[_tableView scrollRowToVisible:_tableView.selectedRow];
	}

	if(flag)
	{
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakPasteboardDidChangeNotification object:_pasteboard];
		[_pasteboard updatePasteboardWithEntry:clipboardEntry];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(clipboardDidChange:) name:OakPasteboardDidChangeNotification object:_pasteboard];
	}
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	NSIndexSet* selectedRowIndexes = _tableView.selectedRowIndexes;
	self.hasSelection = selectedRowIndexes.count != 0;

	if(selectedRowIndexes.count == 1 && !_skipUpdatePasteboard)
	{
		OakPasteboardEntry* clipboardEntry = [self.pasteboardEntries objectAtIndex:selectedRowIndexes.firstIndex];
		[self didSelectEntry:clipboardEntry updatePasteboard:YES];
	}
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSArray<OakPasteboardEntry*>*)pasteboardEntries
{
	return _arrayController.arrangedObjects;
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [self.pasteboardEntries count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	OakPasteboardEntry* entry = [self.pasteboardEntries objectAtIndex:rowIndex];
	if([aTableColumn.identifier isEqualToString:kTableColumnIdentifierMain])
		return entry.displayString;
	else if([aTableColumn.identifier isEqualToString:kTableColumnIdentifierFlag])
		return entry.isFlagged ? @"⚑" : @"";
	return nil;
}

// =================
// = Action Method =
// =================

- (IBAction)orderFrontFindPanel:(id)sender { [self.window makeFirstResponder:_searchField]; }
- (IBAction)findAllInSelection:(id)sender  { [self.window makeFirstResponder:_searchField]; }

- (void)updatePasteboardWithSelectedEntries:(id)sender
{
	NSArray<OakPasteboardEntry*>* selectedEntries = self.selectedEntries;
	if(selectedEntries.count > 1)
		[_pasteboard updatePasteboardWithEntries:selectedEntries];
}

- (void)accept:(id)sender
{
	[self updatePasteboardWithSelectedEntries:self];
	[self.window orderOut:self];

	if(_alternateAction && OakIsAlternateKeyOrMouseEvent() && [NSApp targetForAction:_alternateAction])
		[NSApp sendAction:_alternateAction to:_target from:self];
	else if(_action)
		[NSApp sendAction:_action to:_target from:self];
	[self.window close];
}

- (void)cancel:(id)sender
{
	[self updatePasteboardWithSelectedEntries:self];
	[self.window performClose:self];
}

- (void)clearAll:(id)sender
{
	[_pasteboard removeAllEntries];
	[_pasteboard updatePasteboardWithEntry:nil]; // Triggers OakPasteboardDidChangeNotification
}

- (NSArray<OakPasteboardEntry*>*)selectedEntries
{
	NSArray<OakPasteboardEntry*>* res = [self.pasteboardEntries objectsAtIndexes:_tableView.selectedRowIndexes];
	return res.count ? res : nil;
}

- (void)deleteForward:(id)sender
{
	if(NSArray<OakPasteboardEntry*>* entries = self.selectedEntries)
	{
		NSUInteger row = _tableView.selectedRowIndexes.firstIndex;

		[_pasteboard removeEntries:entries];
		[self refreshTableViewAndSelect:nil];

		if(_tableView.numberOfRows)
				[self didSelectEntry:[self.pasteboardEntries objectAtIndex:MIN(row, _tableView.numberOfRows-1)] updatePasteboard:YES];
		else	[_pasteboard updatePasteboardWithEntry:nil];
	}
}

- (void)deleteBackward:(id)sender
{
	if(NSArray<OakPasteboardEntry*>* entries = self.selectedEntries)
	{
		NSUInteger row = _tableView.selectedRowIndexes.firstIndex;

		[_pasteboard removeEntries:entries];
		[self refreshTableViewAndSelect:nil];

		if(_tableView.numberOfRows)
				[self didSelectEntry:[self.pasteboardEntries objectAtIndex:(row > 0 ? row-1 : row)] updatePasteboard:YES];
		else	[_pasteboard updatePasteboardWithEntry:nil];
	}
}

- (void)toggleCurrentBookmark:(id)sender
{
	if(NSArray<OakPasteboardEntry*>* entries = self.selectedEntries)
	{
		BOOL shouldFlag = [entries indexOfObjectPassingTest:^BOOL(OakPasteboardEntry* entry, NSUInteger, BOOL*){ return !entry.isFlagged; }] != NSNotFound;
		for(OakPasteboardEntry* entry in entries)
			entry.flagged = shouldFlag;

		[_tableView reloadDataForRowIndexes:_tableView.selectedRowIndexes columnIndexes:[NSIndexSet indexSetWithIndex:[_tableView columnWithIdentifier:kTableColumnIdentifierFlag]]];
	}
}

- (void)insertTab:(id)sender
{
	[self.window selectNextKeyView:self];
}

- (void)insertBacktab:(id)sender
{
	[self.window selectPreviousKeyView:self];
}

- (void)insertText:(id)aString
{
	self.filterString = aString;
	[self.window makeFirstResponder:_searchField];
	NSText* fieldEditor = (NSText*)[self.window firstResponder];
	if([fieldEditor isKindOfClass:[NSText class]])
		[fieldEditor setSelectedRange:NSMakeRange([[fieldEditor string] length], 0)];
}

- (void)doCommandBySelector:(SEL)aSelector
{
	if([self respondsToSelector:aSelector])
	{
		[super doCommandBySelector:aSelector];
	}
	else
	{
		NSUInteger res = OakPerformTableViewActionFromSelector(_tableView, aSelector);
		if(res == OakMoveAcceptReturn)
			[self accept:self];
		else if(res == OakMoveCancelReturn)
			[self cancel:self];
	}
}

- (void)keyDown:(NSEvent*)anEvent
{
	[self interpretKeyEvents:@[ anEvent ]];
}

// ========================
// = NSTextField Delegate =
// ========================

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(pageUpAndModifySelection:), @selector(pageDownAndModifySelection:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(scrollToBeginningOfDocument:), @selector(scrollToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward->find(aCommand) == forward->end())
		return NO;

	NSUInteger res = OakPerformTableViewActionFromSelector(_tableView, aCommand);
	if(res == OakMoveAcceptReturn)
		[self accept:aControl];
	else if(res == OakMoveCancelReturn)
		[self cancel:aControl];
	return YES;
}
@end
