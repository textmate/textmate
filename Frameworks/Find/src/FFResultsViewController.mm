#import "FFResultsViewController.h"
#import "FFResultNode.h"
#import <document/OakDocument.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSColor Additions.h>

static NSString* const kUserDefaultsSearchResultsFontNameKey = @"searchResultsFontName";
static NSString* const kUserDefaultsSearchResultsFontSizeKey = @"searchResultsFontSize";

static FFResultNode* NextNode (FFResultNode* node)
{
	NSUInteger index = [node.parent.children indexOfObject:node] + 1;
	return index < node.parent.children.count ? node.parent.children[index] : nil;
}

static FFResultNode* PreviousNode (FFResultNode* node)
{
	NSUInteger index = [node.parent.children indexOfObject:node];
	return index > 0 ? node.parent.children[index - 1] : nil;
}

@interface FFResultsViewController () <NSOutlineViewDataSource, NSOutlineViewDelegate>
{
	NSScrollView*  _scrollView;
	NSFont*        _searchResultsFont;

	__weak id      _eventMonitor;
	BOOL           _longPressedCommandModifier;
	CGFloat        _pendingColumnWidth;

	FFResultNode*  _lastSelectedResult;
}
@property (nonatomic) BOOL showKeyEquivalent;
@end

// ================================
// = OakSearchResultsCheckboxView =
// ================================

@interface OakSearchResultsCheckboxView : NSTableCellView
@property (nonatomic) NSButton* button;
@end

@implementation OakSearchResultsCheckboxView
- (id)initWithFrame:(NSRect)aFrame
{
	if((self = [super initWithFrame:aFrame]))
	{
		_button = OakCreateCheckBox(nil);
		_button.controlSize = NSControlSizeSmall;

		_button.translatesAutoresizingMaskIntoConstraints = NO;
		[self addSubview:_button];

		[self addConstraint:[NSLayoutConstraint constraintWithItem:_button attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterX multiplier:1 constant:0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:_button attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[_button bind:NSEnabledBinding toObject:self withKeyPath:@"objectValue.readOnly" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		[_button bind:NSValueBinding toObject:self withKeyPath:@"objectValue.excluded" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	}
	return self;
}
@end

// ====================
// = OakTableCellView =
// ====================

@interface OakTableCellView : NSTableCellView
{
	BOOL _observingKeyPaths;
}
@property (nonatomic) FFResultsViewController* viewController;
@property (nonatomic) NSArray<NSString*>* observeKeyPaths;
@end

@implementation OakTableCellView
- (void)viewWillMoveToSuperview:(NSView*)aView
{
	if(aView && _observingKeyPaths == NO)
	{
		for(NSString* keyPath in self.observeKeyPaths)
			[_viewController addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:nullptr];
		_observingKeyPaths = YES;
	}
	else if(!aView && _observingKeyPaths == YES)
	{
		for(NSString* keyPath in self.observeKeyPaths)
			[_viewController removeObserver:self forKeyPath:keyPath];
		_observingKeyPaths = NO;
	}
	[super viewWillMoveToSuperview:aView];
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)anObject change:(NSDictionary*)someChange context:(void*)context
{
	if([self.observeKeyPaths containsObject:aKeyPath])
			[self setValue:[anObject valueForKey:aKeyPath] forKey:aKeyPath];
	else	[super observeValueForKeyPath:aKeyPath ofObject:anObject change:someChange context:context];
}
@end

// =================================
// = OakSearchResultsMatchCellView =
// =================================

@interface OakSearchResultsMatchCellView : OakTableCellView
@property (nonatomic) NSString* replaceString;
@property (nonatomic) BOOL showReplacementPreviews;
@end

@implementation OakSearchResultsMatchCellView
+ (NSSet*)keyPathsForValuesAffectingExcerptString { return [NSSet setWithArray:@[ @"objectValue", @"objectValue.readOnly", @"objectValue.excluded", @"objectValue.replaceString", @"replaceString", @"showReplacementPreviews", @"backgroundStyle" ]]; }

- (id)initWithViewController:(FFResultsViewController*)viewController font:(NSFont*)font
{
	if((self = [super initWithFrame:NSZeroRect]))
	{
		self.viewController  = viewController;
		self.observeKeyPaths = @[ @"replaceString", @"showReplacementPreviews" ];

		NSTextField* textField = OakCreateLabel(@"", font);
		[textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[textField setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSDictionary* views = @{
			@"textField": textField,
		};
		OakAddAutoLayoutViewsToSuperview(views.allValues, self);
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[textField]" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[textField]|" options:0 metrics:nil views:views]];

		[textField bind:NSValueBinding toObject:self withKeyPath:@"excerptString" options:nil];

		self.textField = textField;
	}
	return self;
}

- (NSAttributedString*)excerptString
{
	FFResultNode* item = self.objectValue;
	NSAttributedString* res = [item excerptWithReplacement:(item.isReadOnly || item.excluded || !_showReplacementPreviews ? item.replaceString : self.replaceString) font:self.textField.font];
	if(self.backgroundStyle == NSBackgroundStyleDark)
	{
		NSMutableAttributedString* str = [res mutableCopy];
		[str enumerateAttributesInRange:NSMakeRange(0, str.length) options:NSAttributedStringEnumerationLongestEffectiveRangeNotRequired usingBlock:^(NSDictionary* attrs, NSRange range, BOOL* stop){
			if(attrs[NSBackgroundColorAttributeName] != nil)
				[str addAttribute:NSBackgroundColorAttributeName value:[NSColor tmMatchedTextSelectedBackgroundColor] range:range];
			if(attrs[NSUnderlineColorAttributeName] != nil)
				[str addAttribute:NSUnderlineColorAttributeName value:[NSColor tmMatchedTextSelectedUnderlineColor] range:range];
		}];
		[str addAttribute:NSForegroundColorAttributeName value:[NSColor alternateSelectedControlTextColor] range:NSMakeRange(0, [str length])];
		res = str;
	}
	return res;
}
@end

// ==================================
// = OakSearchResultsHeaderCellView =
// ==================================

@interface OakSearchResultsHeaderCellView : OakTableCellView
@property (nonatomic) NSButton* countOfLeafsButton;
@property (nonatomic) NSButton* removeButton;
@property (nonatomic) BOOL showKeyEquivalent;
@property (nonatomic) BOOL observingKeyEquivalent;
@end

@implementation OakSearchResultsHeaderCellView
- (id)initWithViewController:(FFResultsViewController*)viewController
{
	if((self = [super init]))
	{
		self.viewController  = viewController;
		self.observeKeyPaths = @[ @"showKeyEquivalent" ];

		NSImageView* imageView = [NSImageView new];
		NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);

		NSButton* countOfLeafs = [NSButton new];
		[[countOfLeafs cell] setHighlightsBy:NSNoCellMask];
		countOfLeafs.alignment  = NSTextAlignmentCenter;
		countOfLeafs.bezelStyle = NSInlineBezelStyle;
		countOfLeafs.font       = [NSFont labelFontOfSize:0];
		countOfLeafs.identifier = @"countOfLeafs";

		NSImage* removeTemplateImage = [NSImage imageWithSize:NSMakeSize(8, 8) flipped:NO drawingHandler:^BOOL(NSRect dstRect){
			[[NSColor blackColor] set];
			NSRectFill(NSInsetRect(dstRect, 0, floor(NSHeight(dstRect)/2)-1));
			return YES;
		}];
		[removeTemplateImage setTemplate:YES];

		NSButton* remove = [NSButton new];
		remove.controlSize = NSControlSizeSmall;
		remove.bezelStyle  = NSRoundRectBezelStyle;
		remove.buttonType  = NSMomentaryPushInButton;
		remove.image       = removeTemplateImage;

		NSDictionary* views = @{ @"icon": imageView, @"text": textField, @"count": countOfLeafs, @"remove": remove };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[countOfLeafs setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(9)-[remove(==16)]-(6)-[icon(==16)]-(3)-[text]"          options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[text]-(4)-[count]-(>=8)-|"                                options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[icon(==16,==remove)]-(3)-|"                               options:0 metrics:nil views:views]];

		[imageView bind:NSValueBinding toObject:self withKeyPath:@"objectValue.document.icon" options:nil];
		[textField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.displayPath" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidExpandNotification object:viewController.outlineView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidCollapseNotification object:viewController.outlineView];

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

- (void)setShowKeyEquivalent:(BOOL)flag
{
	if(_showKeyEquivalent == flag)
		return;
	_showKeyEquivalent = flag;

	FFResultNode* item = self.objectValue;
	if(_showKeyEquivalent)
	{
		NSUInteger index = [item.parent.children indexOfObject:item];
		if(!item || index > 8)
			return;

		NSRect rect = NSUnionRect(self.imageView.bounds, NSMakeRect(0, 0, 16, 16));
		NSImage* image = [NSImage imageWithSize:rect.size flipped:NO drawingHandler:^BOOL(NSRect dstRect){
			NSColor* color = [NSColor secondaryLabelColor];

			CGFloat ptrn[] = { 2, 1 };
			NSBezierPath* path = [NSBezierPath bezierPathWithRoundedRect:NSIntegralRect(NSInsetRect(dstRect, 1, 1)) xRadius:2 yRadius:2];
			[path setLineDash:ptrn count:sizeofA(ptrn) phase:0];
			[path setLineWidth:1];

			[color set];
			[path stroke];

			NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
			[pStyle setAlignment:NSTextAlignmentCenter];
			NSDictionary* attributes = @{
				NSFontAttributeName:            [NSFont boldSystemFontOfSize:0],
				NSForegroundColorAttributeName: color,
				NSParagraphStyleAttributeName:  pStyle,
			};

			NSAttributedString* str = [[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"%lu", (index + 1) % 10] attributes:attributes];
			NSSize size = [str size];
			dstRect.origin.y = 0.5 * (NSHeight(dstRect) - size.height);
			dstRect.size.height = size.height;
			[str drawInRect:NSIntegralRect(dstRect)];

			return YES;
		}];

		[self.imageView setImage:image];
	}
	else
	{
		self.imageView.image = item.document.icon;
	}
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

@implementation FFResultsViewController
- (void)loadView
{
	if(!_scrollView)
	{
		NSString* fontName = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsSearchResultsFontNameKey];
		CGFloat fontSize   = [[NSUserDefaults standardUserDefaults] floatForKey:kUserDefaultsSearchResultsFontSizeKey] ?: 11.0;
		_searchResultsFont = (fontName ? [NSFont fontWithName:fontName size:fontSize] : [NSFont controlContentFontOfSize:fontSize]);

		NSTextField* label = OakCreateLabel(@"m", _searchResultsFont);
		[label sizeToFit];
		CGFloat lineHeight = std::max(NSHeight(label.frame), ceil(_searchResultsFont.ascender) + ceil(fabs(_searchResultsFont.descender)) + ceil(_searchResultsFont.leading));

		_outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
		_outlineView.accessibilityLabel                 = @"Results";
		_outlineView.focusRingType                      = NSFocusRingTypeNone;
		_outlineView.allowsMultipleSelection            = YES;
		_outlineView.autoresizesOutlineColumn           = NO;
		_outlineView.usesAlternatingRowBackgroundColors = YES;
		_outlineView.headerView                         = nil;
		_outlineView.rowHeight                          = std::max(lineHeight, 14.0);
		_outlineView.columnAutoresizingStyle            = NSTableViewNoColumnAutoresizing;

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"checkbox"];
		tableColumn.width = 50;
		[_outlineView addTableColumn:tableColumn];
		[_outlineView setOutlineTableColumn:tableColumn];

		tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"match"];
		[tableColumn setEditable:NO];
		[_outlineView addTableColumn:tableColumn];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = YES;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSLineBorder;
		_scrollView.documentView          = _outlineView;

		NSDictionary* views = @{
			@"scrollView": _scrollView,
		};

		NSView* containerView = [[NSView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview([views allValues], containerView);

		[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[scrollView]|" options:0 metrics:nil views:views]];
		[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(-1)-[scrollView]-(-1)-|" options:0 metrics:nil views:views]];

		self.view = containerView;

		_outlineView.dataSource   = self;
		_outlineView.delegate     = self;
		_outlineView.target       = self;
		_outlineView.action       = @selector(didSingleClick:);
		_outlineView.doubleAction = @selector(didDoubleClick:);

		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged handler:^NSEvent*(NSEvent* event){
			NSUInteger modifierFlags = [_outlineView.window isKeyWindow] ? ([event modifierFlags] & (NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagCommand)) : 0;
			if(_longPressedCommandModifier)
			{
				self.showKeyEquivalent = modifierFlags == NSEventModifierFlagCommand;
				if(modifierFlags == 0)
					_longPressedCommandModifier = NO;
			}
			else
			{
				if(modifierFlags == NSEventModifierFlagCommand)
						[self performSelector:@selector(delayedLongPressedCommandModifier:) withObject:self afterDelay:0.2];
				else	[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(delayedLongPressedCommandModifier:) object:self];
			}
			return event;
		}];
	}
}

- (void)delayedLongPressedCommandModifier:(id)sender
{
	_longPressedCommandModifier = YES;
	self.showKeyEquivalent = YES;
}

- (void)dealloc
{
	[NSEvent removeMonitor:_eventMonitor];
}

- (void)setResults:(FFResultNode*)someResults
{
	[_outlineView sizeLastColumnToFit];

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

	item = item ? (NextNode(item) ?: NextNode(item.parent).firstResultNode) : _results.firstResultNode.firstResultNode;
	if(!item && wrapAround)
		item = _results.firstResultNode.firstResultNode;

	[self showResultNode:item];
}

- (void)selectPreviousResultWrapAround:(BOOL)wrapAround
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];

	item = item ? (PreviousNode(item) ?: PreviousNode(item.parent).lastResultNode) : _results.lastResultNode.lastResultNode;
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
	[self showResultNode:NextNode(item.parent).firstResultNode ?: _results.firstResultNode.firstResultNode];
}

- (IBAction)selectPreviousDocument:(id)sender
{
	NSInteger row = [_outlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_outlineView itemAtRow:row];
	[self showResultNode:PreviousNode(item.parent).firstResultNode ?: _results.lastResultNode.firstResultNode];
}

// ==================
// = Helper Methods =
// ==================

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

- (void)didSelectResult:(FFResultNode*)resultNode
{
	if(_lastSelectedResult == resultNode)
		return;

	// Prevent sending selectResultAction twice since mouse clicks sends both didSingleClick: and outlineViewSelectionDidChange:
	_lastSelectedResult = resultNode;
	dispatch_async(dispatch_get_main_queue(), ^{
		_lastSelectedResult = nil;
	});

	if(_selectResultAction)
		[NSApp sendAction:_selectResultAction to:_target from:resultNode];
}

- (void)didSingleClick:(id)sender
{
	if(_outlineView.clickedRow != -1 && _outlineView.numberOfSelectedRows == 1)
		[self didSelectResult:[_outlineView itemAtRow:_outlineView.clickedRow]];
}

- (void)didDoubleClick:(id)sender
{
	if(_outlineView.clickedRow != -1 && _doubleClickResultAction)
	{
		[self didSelectResult:[_outlineView itemAtRow:_outlineView.clickedRow]];
		[NSApp sendAction:_doubleClickResultAction to:_target from:[_outlineView itemAtRow:_outlineView.clickedRow]];
	}
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	if(_outlineView.numberOfSelectedRows == 1)
		[self didSelectResult:[_outlineView itemAtRow:_outlineView.selectedRowIndexes.firstIndex]];
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

- (void)commitPendingColumnWidth:(NSTableColumn*)tableColumn
{
	if(_pendingColumnWidth)
	{
		tableColumn.minWidth = MIN(_pendingColumnWidth, tableColumn.minWidth);
		tableColumn.maxWidth = MAX(_pendingColumnWidth, tableColumn.maxWidth);
		tableColumn.width    = _pendingColumnWidth;

		_pendingColumnWidth = 0;
	}
}

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
{
	NSString* identifier = tableColumn.identifier ?: @"group";
	id res = [outlineView makeViewWithIdentifier:identifier owner:self];

	if([identifier isEqualToString:@"checkbox"])
	{
		OakSearchResultsCheckboxView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsCheckboxView alloc] initWithFrame:NSZeroRect];
			cellView.identifier = identifier;
			cellView.button.action = @selector(toggleExcludedCheckbox:);
			cellView.button.target = self;
		}
		cellView.objectValue = item;
	}
	else if([identifier isEqualToString:@"match"])
	{
		OakSearchResultsMatchCellView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsMatchCellView alloc] initWithViewController:self font:_searchResultsFont];
			cellView.identifier = identifier;
		}

		cellView.objectValue = item;
		[cellView layoutSubtreeIfNeeded];
		CGFloat width = NSMaxX(cellView.textField.frame) + 32;

		if(tableColumn.width < width && _pendingColumnWidth < width)
		{
			if(_pendingColumnWidth == 0)
				[self performSelector:@selector(commitPendingColumnWidth:) withObject:tableColumn afterDelay:0];
			_pendingColumnWidth = width;
		}
	}
	else
	{
		OakSearchResultsHeaderCellView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsHeaderCellView alloc] initWithViewController:self];
			cellView.identifier = identifier;
			cellView.removeButton.action = @selector(takeSearchResultToRemoveFrom:);
			cellView.removeButton.target = self;
		}

		cellView.objectValue = item;
		cellView.countOfLeafsButton.title = [NSNumberFormatter localizedStringFromNumber:@(item.countOfLeafs) numberStyle:NSNumberFormatterDecimalStyle];
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
