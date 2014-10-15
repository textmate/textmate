#import "FFResultsViewController.h"
#import "FFResultNode.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

// =================================
// = OakSearchResultsMatchCellView =
// =================================

@interface OakSearchResultsMatchCellView : NSTableCellView
@property (nonatomic) NSString* replaceString;
@property (nonatomic) BOOL showReplacementPreviews;
@end

@implementation OakSearchResultsMatchCellView
+ (NSSet*)keyPathsForValuesAffectingExcerptString { return [NSSet setWithArray:@[ @"objectValue", @"objectValue.ignored", @"objectValue.excluded", @"objectValue.replaceString", @"replaceString", @"showReplacementPreviews", @"backgroundStyle" ]]; }

- (id)initWithFrame:(NSRect)aFrame
{
	if((self = [super initWithFrame:aFrame]))
	{
		NSTextField* textField = OakCreateLabel(@"");
		[textField setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		[self addSubview:textField];
		[textField bind:NSValueBinding toObject:self withKeyPath:@"excerptString" options:nil];

		self.textField = textField;
	}
	return self;
}

- (NSAttributedString*)excerptString
{
	FFResultNode* item = self.objectValue;
	NSAttributedString* res = [item excerptWithReplacement:item.ignored || item.excluded || !_showReplacementPreviews ? item.replaceString : self.replaceString];
	if(self.backgroundStyle == NSBackgroundStyleDark)
	{
		NSMutableAttributedString* str = [res mutableCopy];
		[str addAttribute:NSForegroundColorAttributeName value:[NSColor alternateSelectedControlTextColor] range:NSMakeRange(0, [str length])];
		res = str;
	}
	return res;
}
@end

// ==================================
// = OakSearchResultsHeaderCellView =
// ==================================

@interface OakSearchResultsHeaderCellView : NSTableCellView
@property (nonatomic) NSString* countOfLeafs;
@property (nonatomic) NSButton* countOfLeafsButton;
@property (nonatomic) NSButton* removeButton;
@end

@implementation OakSearchResultsHeaderCellView
- (id)initWithOutlineView:(NSOutlineView*)anOutlineView
{
	if((self = [super init]))
	{
		NSImageView* imageView = [NSImageView new];
		NSTextField* textField = OakCreateLabel();
		textField.font = [NSFont controlContentFontOfSize:0];

		NSButton* countOfLeafs = [NSButton new];
		[[countOfLeafs cell] setHighlightsBy:NSNoCellMask];
		countOfLeafs.alignment  = NSCenterTextAlignment;
		countOfLeafs.bezelStyle = NSInlineBezelStyle;
		countOfLeafs.font       = [NSFont labelFontOfSize:0];
		countOfLeafs.identifier = @"countOfLeafs";

		NSButton* remove = [NSButton new];
		[[remove cell] setControlSize:NSSmallControlSize];
		remove.bezelStyle = NSRoundRectBezelStyle;
		remove.buttonType = NSMomentaryPushInButton;
		remove.image      = [NSImage imageNamed:NSImageNameRemoveTemplate];

		NSDictionary* views = @{ @"icon" : imageView, @"text" : textField, @"count" : countOfLeafs, @"remove" : remove };
		for(NSView* child in [views allValues])
		{
			[child setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:child];
		}

		[textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[countOfLeafs setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(6)-[icon(==16)]-(3)-[text]-(>=8)-[remove(==16)]-(12)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[text]-(4)-[count]"                                        options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[count]-(>=4)-[remove]"                                    options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[icon(==16,==remove)]-(3)-|"                               options:0 metrics:nil views:views]];

		[imageView bind:NSValueBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[textField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.displayPath" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidExpandNotification object:anOutlineView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidCollapseNotification object:anOutlineView];

		self.imageView          = imageView;
		self.textField          = textField;
		self.countOfLeafsButton = countOfLeafs;
		self.removeButton       = remove;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)setCountOfLeafs:(NSString*)aString
{
	_countOfLeafs = aString;
	_countOfLeafsButton.title  = aString ?: @"0";
	_countOfLeafsButton.hidden = aString == nil;
}

- (void)outlineViewItemDidExpandCollapse:(NSNotification*)aNotification
{
	NSOutlineView* outlineView = [aNotification object];
	NSDictionary* userInfo = [aNotification userInfo];
	FFResultNode* item = userInfo[@"NSObject"];
	if(item == self.objectValue)
		_countOfLeafsButton.hidden = [outlineView isItemExpanded:item];
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification   { [self outlineViewItemDidExpandCollapse:aNotification]; }
- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification { [self outlineViewItemDidExpandCollapse:aNotification]; }
@end

// ===========================
// = FFResultsViewController =
// ===========================

@interface FFResultsViewController () <NSOutlineViewDataSource, NSOutlineViewDelegate>
{
	NSView*        _topDivider;
	NSScrollView*  _scrollView;
	NSView*        _bottomDivider;
}
@end

@implementation FFResultsViewController
- (void)loadView
{
	if(!_scrollView)
	{
		_topDivider    = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1]);
		_bottomDivider = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1]);

		_outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
		OakSetAccessibilityLabel(_outlineView, @"Results");
		_outlineView.focusRingType                      = NSFocusRingTypeNone;
		_outlineView.allowsMultipleSelection            = YES;
		_outlineView.autoresizesOutlineColumn           = NO;
		_outlineView.usesAlternatingRowBackgroundColors = YES;
		_outlineView.headerView                         = nil;
		_outlineView.rowHeight                          = 14;

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"checkbox"];
		tableColumn.width = 50;
		[_outlineView addTableColumn:tableColumn];
		[_outlineView setOutlineTableColumn:tableColumn];

		tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"match"];
		[tableColumn setEditable:NO];
		[_outlineView addTableColumn:tableColumn];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _outlineView;

		NSDictionary* views = @{
			@"topDivider"    : _topDivider,
			@"scrollView"    : _scrollView,
			@"bottomDivider" : _bottomDivider,
		};

		NSView* containerView = [[NSView alloc] initWithFrame:NSZeroRect];
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[containerView addSubview:view];
		}

		[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topDivider][scrollView][bottomDivider]|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView]|" options:0 metrics:nil views:views]];

		self.view = containerView;

		_outlineView.dataSource   = self;
		_outlineView.delegate     = self;
		_outlineView.target       = self;
		_outlineView.action       = @selector(didSingleClick:);
		_outlineView.doubleAction = @selector(didDoubleClick:);
	}
}

- (void)setResults:(FFResultNode*)someResults
{
	_results = someResults;
	[_outlineView reloadData];
}

- (NSArray*)selectedResults
{
	NSMutableArray* res = [NSMutableArray array];

	NSIndexSet* selectedRows = [_outlineView numberOfSelectedRows] == 0 ? [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, [_outlineView numberOfRows])] : [_outlineView selectedRowIndexes];
	for(NSUInteger index = [selectedRows firstIndex]; index != NSNotFound; index = [selectedRows indexGreaterThanIndex:index])
	{
		FFResultNode* item = [_outlineView itemAtRow:index];
		if([item.children count] == 0)
			[res addObject:item];
	}

	return res;
}

- (void)setHideCheckBoxes:(BOOL)flag
{
	if(_hideCheckBoxes == flag)
		return;
	_hideCheckBoxes = flag;

	[[_outlineView tableColumnWithIdentifier:@"checkbox"] setHidden:flag];
	[_outlineView setOutlineTableColumn:[_outlineView tableColumnWithIdentifier:flag ? @"match" : @"checkbox"]];
}

- (void)insertItemsAtIndexes:(NSIndexSet*)anIndexSet
{
	[_outlineView beginUpdates];
	[_outlineView insertItemsAtIndexes:anIndexSet inParent:nil withAnimation:0];
	for(FFResultNode* item in [_results.children objectsAtIndexes:anIndexSet])
		[_outlineView expandItem:item];
	[_outlineView endUpdates];
}

- (void)showResultNode:(FFResultNode*)aResultNode
{
	if(!aResultNode)
		return;

	if(![_outlineView isItemExpanded:aResultNode.parent])
		[_outlineView expandItem:aResultNode.parent];
	[_outlineView scrollRowToVisible:[_outlineView rowForItem:aResultNode.parent]];

	NSInteger row = [_outlineView rowForItem:aResultNode];
	if(row != -1)
	{
		[_outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[_outlineView scrollRowToVisible:row];
		[_outlineView.window makeFirstResponder:_outlineView];
	}
}

// ==================
// = Action Methods =
// ==================

- (void)selectNextResultWrapAround:(BOOL)wrapAround
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];

	item = item ? (item.next ?: item.parent.next.firstResultNode) : _results.firstResultNode.firstResultNode;
	if(!item && wrapAround)
		item = _results.firstResultNode.firstResultNode;

	[self showResultNode:item];
}

- (void)selectPreviousResultWrapAround:(BOOL)wrapAround
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];

	item = item ? (item.previous ?: item.parent.previous.lastResultNode) : _results.lastResultNode.lastResultNode;
	if(!item && wrapAround)
		item = _results.lastResultNode.lastResultNode;

	[self showResultNode:item];
}

- (IBAction)toggleCollapsedState:(id)anArgument
{
	if(self.isCollapsed)
			[_outlineView expandItem:nil expandChildren:YES];
	else	[_outlineView collapseItem:nil collapseChildren:YES];
}

- (IBAction)selectNextDocument:(id)sender
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];
	[self showResultNode:item.parent.next.firstResultNode ?: _results.firstResultNode.firstResultNode];
}

- (IBAction)selectPreviousDocument:(id)sender
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];
	[self showResultNode:item.parent.previous.firstResultNode ?: _results.lastResultNode.firstResultNode];
}

// ==================
// = Helper Methods =
// ==================

- (FFResultNode*)selectedResult
{
	if([_outlineView numberOfSelectedRows] == 1)
		return [_outlineView itemAtRow:[[_outlineView selectedRowIndexes] firstIndex]];
	return nil;
}

- (BOOL)isCollapsed
{
	NSUInteger expanded = 0;
	for(FFResultNode* parent in _results.children)
		expanded += [_outlineView isItemExpanded:parent] ? 1 : 0;
	return [_results.children count] && 2 * expanded <= [_results.children count];
}

// ========================
// = Table (Cell) Actions =
// ========================

- (void)toggleExcludedCheckbox:(NSButton*)sender
{
	NSInteger row = [_outlineView rowForView:sender];
	if(row != -1)
	{
		FFResultNode* item = [_outlineView itemAtRow:row];

		BOOL toggleAllInGroup = OakIsAlternateKeyOrMouseEvent();
		if(toggleAllInGroup)
			item.parent.excluded = item.excluded;

		if(_showReplacementPreviews)
		{
			NSRange range = NSMakeRange(row, 1);
			if(toggleAllInGroup)
				range = NSMakeRange([_outlineView rowForItem:item.parent.firstResultNode], item.parent.countOfLeafs);
			[_outlineView reloadDataForRowIndexes:[NSIndexSet indexSetWithIndexesInRange:range] columnIndexes:[NSIndexSet indexSetWithIndex:1]];
		}
	}
}

- (void)takeSearchResultToRemoveFrom:(NSButton*)sender
{
	NSInteger row = [_outlineView rowForView:sender];
	if(row != -1)
	{
		FFResultNode* item = [_outlineView itemAtRow:row];

		NSUInteger index = [item.parent.children indexOfObject:item];
		if(index != NSNotFound)
			[_outlineView removeItemsAtIndexes:[NSIndexSet indexSetWithIndex:index] inParent:nil withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideDown];

		[item removeFromParent];
		if(_removeResultAction)
			[NSApp sendAction:_removeResultAction to:_target from:item];
	}
}

- (void)didSingleClick:(id)sender
{
	if(_selectResultAction && self.selectedResult)
		[NSApp sendAction:_selectResultAction to:_target from:self.selectedResult];
}

- (void)didDoubleClick:(id)sender
{
	if(_doubleClickResultAction && self.selectedResult)
		[NSApp sendAction:_doubleClickResultAction to:_target from:self.selectedResult];
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	if(_selectResultAction && [[NSApp currentEvent] type] != NSLeftMouseUp && self.selectedResult)
		[NSApp sendAction:_selectResultAction to:_target from:self.selectedResult];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(FFResultNode*)item
{
	return ![self outlineView:outlineView isGroupItem:item];
}

// ============================
// = Outline view data source =
// ============================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(FFResultNode*)item
{
	return [(item ?: _results).children count];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item];
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(FFResultNode*)item
{
	return [(item ?: _results).children objectAtIndex:childIndex];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(FFResultNode*)item
{
	return [outlineView levelForItem:item] == 0;
}

- (CGFloat)outlineView:(NSOutlineView*)outlineView heightOfRowByItem:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item] ? 22 : item.lineSpan * [outlineView rowHeight];
}

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
{
	NSString* identifier = tableColumn.identifier ?: @"group";
	id res = [outlineView makeViewWithIdentifier:identifier owner:self];

	if([identifier isEqualToString:@"checkbox"])
	{
		NSButton* button = res;
		if(!button)
		{
			res = button = OakCreateCheckBox(nil);
			button.identifier = identifier;
			[[button cell] setControlSize:NSSmallControlSize];
			button.action = @selector(toggleExcludedCheckbox:);
			button.target = self;
		}
		else
		{
			[button unbind:NSEnabledBinding];
			[button unbind:NSValueBinding];
		}

		[button bind:NSEnabledBinding toObject:item withKeyPath:@"ignored" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		[button bind:NSValueBinding toObject:item withKeyPath:@"excluded" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		button.state = item.excluded ? NSOffState : NSOnState;
	}
	else if([identifier isEqualToString:@"match"])
	{
		OakSearchResultsMatchCellView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsMatchCellView alloc] initWithFrame:NSZeroRect];
			[cellView bind:@"replaceString" toObject:self withKeyPath:@"replaceString" options:nil];
			[cellView bind:@"showReplacementPreviews" toObject:self withKeyPath:@"showReplacementPreviews" options:nil];
			cellView.identifier = identifier;
		}
		cellView.objectValue = item;
	}
	else
	{
		OakSearchResultsHeaderCellView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsHeaderCellView alloc] initWithOutlineView:outlineView];
			cellView.identifier = identifier;
			cellView.removeButton.action = @selector(takeSearchResultToRemoveFrom:);
			cellView.removeButton.target = self;
		}

		cellView.objectValue = item;
		cellView.countOfLeafsButton.title = [NSString stringWithFormat:@"%lu", item.countOfLeafs];
		cellView.countOfLeafsButton.hidden = [outlineView isItemExpanded:item];
	}
	return res;
}

// ===================
// = Menu Validation =
// ===================

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	BOOL res = YES;
	if(aMenuItem.action == @selector(toggleCollapsedState:))
	{
		[aMenuItem setTitle:self.isCollapsed ? @"Expand Results" : @"Collapse Results"];
		res = _results.countOfLeafs != 0;
	}
	return res;
}
@end
