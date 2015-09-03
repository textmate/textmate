#import "FSOutlineViewDelegate.h"
#import "OakFSUtilities.h"
#import "io/FSDataSource.h"
#import "io/FSItem.h"
#import "ui/OFBOutlineView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakRolloverButton.h>
#import <OakAppKit/OakFileIconImage.h>
#import <ns/ns.h>
#import <io/path.h>
#import <text/utf8.h>
#import <oak/oak.h>
#import <crash/info.h>

@interface OakSelectBasenameCell : NSTextFieldCell
@end

@implementation OakSelectBasenameCell
- (void)selectWithFrame:(NSRect)aRect inView:(NSView*)aView editor:(NSText*)aText delegate:(id)someDelegate start:(NSInteger)start length:(NSInteger)length
{
	NSString* basename = [self.stringValue stringByDeletingPathExtension];
	[super selectWithFrame:aRect inView:aView editor:aText delegate:someDelegate start:start length:(start == 0 && basename ? MIN(basename.length, length) : length)];
}
@end

@interface OakLabelSwatchView : NSView
@property (nonatomic) NSInteger labelIndex;
@end

@implementation OakLabelSwatchView
- (void)setLabelIndex:(NSInteger)newLabelIndex
{
	ASSERT_LT(newLabelIndex, 8);
	if(_labelIndex != newLabelIndex)
	{
		_labelIndex = newLabelIndex;
		[self setNeedsDisplay:YES];
	}
}

- (BOOL)isSelected
{
	NSView* view = self;
	while(view && ![view isKindOfClass:[NSTableRowView class]])
		view = [view superview];
	return [view isKindOfClass:[NSTableRowView class]] && ((NSTableRowView*)view).isSelected;
}

- (void)drawRect:(NSRect)aRect
{
	if(_labelIndex == 0)
		return;

	// color names: Gray, Green, Purple, Blue, Yellow, Red, Orange
	static NSString* const labelColor[]  = { @"#A8A8A8", @"#AFDC49", @"#C186D7", @"#5B9CFE", @"#ECDF4A", @"#FC605C", @"#F6AC46" };
	[[NSColor colorWithString:labelColor[_labelIndex - 1]] set];

	NSRect r = NSInsetRect(self.bounds, 1, 1);
	r.size.width = NSHeight(r);
	NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:r];
	[path fill];

	if(self.isSelected)
	{
		[[NSColor whiteColor] set];
		[path stroke];
	}
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(10, 10);
}
@end

@interface OakItemButtonsView : NSView
@property (nonatomic) NSInteger labelIndex;
@property (nonatomic) BOOL open;

@property (nonatomic) SEL closeAction;
@property (nonatomic, weak) id target;

@property (nonatomic) OakLabelSwatchView* labelSwatchView;
@property (nonatomic) OakRolloverButton* closeButton;
@property (nonatomic) NSMutableArray* myConstraints;
@end

@implementation OakItemButtonsView
- (id)initWithCloseAction:(SEL)closeAction target:(id)target
{
	if(self = [super initWithFrame:NSZeroRect])
	{
		_closeAction = closeAction;
		_target      = target;

		[self setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self setContentCompressionResistancePriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
	}
	return self;
}

- (void)updateConstraints
{
	if(_myConstraints)
		[self removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray array];

	if(_labelSwatchView)
	{
		NSDictionary* views = @{ @"labelSwatch" : _labelSwatchView };
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_labelSwatchView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(>=0)-[labelSwatch]-(>=0)-|" options:0 metrics:nil views:views]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[labelSwatch]-(24)-|" options:0 metrics:nil views:views]];
	}

	if(_closeButton)
	{
		NSDictionary* views = @{ @"closeButton" : _closeButton };
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_closeButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(>=0)-[closeButton]-(>=0)-|" options:0 metrics:nil views:views]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[closeButton]-(8)-|" options:0 metrics:nil views:views]];
		if(!_labelSwatchView)
			[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[closeButton]" options:0 metrics:nil views:views]];
	}

	if(!_labelSwatchView && !_closeButton)
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:0]];

	[self addConstraints:_myConstraints];
	[super updateConstraints];
}

- (NSInteger)labelIndex
{
	return _labelSwatchView ? _labelSwatchView.labelIndex : 0;
}

- (void)setLabelIndex:(NSInteger)newLabelIndex
{
	if(self.labelIndex == newLabelIndex)
		return;

	if(newLabelIndex == 0)
	{
		[_labelSwatchView removeFromSuperview];
		_labelSwatchView = nil;
		[self setNeedsUpdateConstraints:YES];
	}
	else if(!_labelSwatchView)
	{
		_labelSwatchView = [[OakLabelSwatchView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview(@[ _labelSwatchView ], self);
		[self setNeedsUpdateConstraints:YES];
	}
	_labelSwatchView.labelIndex = newLabelIndex;
}

- (void)setNilValueForKey:(NSString*)aKey
{
	if([aKey isEqualToString:@"labelIndex"])
		[self setValue:@0 forKey:aKey];
	else if([aKey isEqualToString:@"open"])
		[self setValue:@NO forKey:aKey];
	else
		[super setNilValueForKey:aKey];
}

- (void)setOpen:(BOOL)flag
{
	if(_open == flag)
		return;

	if(!flag)
	{
		[_closeButton removeFromSuperview];
		_closeButton = nil;
	}
	else if(!_closeButton)
	{
		_closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		_closeButton.regularImage  = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:[self class]];
		_closeButton.pressedImage  = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:[self class]];
		_closeButton.rolloverImage = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:[self class]];
		_closeButton.target        = _target;
		_closeButton.action        = _closeAction;
		OakSetAccessibilityLabel(_closeButton, @"Close document");

		OakAddAutoLayoutViewsToSuperview(@[ _closeButton ], self);
	}

	_open = flag;
	[self setNeedsUpdateConstraints:YES];
}
@end

@interface OakFSItemTableCellView : NSTableCellView <NSTextFieldDelegate>
@property (nonatomic) NSButton* openButton;
@property (nonatomic) OakItemButtonsView* itemInfoButtons;
@end

@implementation OakFSItemTableCellView
- (instancetype)initWithOpenAction:(SEL)openAction closeAction:(SEL)closeAction target:(id)target
{
	if((self = [super initWithFrame:NSZeroRect]))
	{
		_openButton = [[NSButton alloc] initWithFrame:NSZeroRect];
		_openButton.refusesFirstResponder = YES;
		_openButton.buttonType            = NSMomentaryChangeButton;
		_openButton.bordered              = NO;
		_openButton.imagePosition         = NSImageOnly;
		_openButton.target                = target;
		_openButton.action                = openAction;

		[_openButton setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_openButton setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		_itemInfoButtons = [[OakItemButtonsView alloc] initWithCloseAction:closeAction target:target];

		NSTextField* fileTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		fileTextField.cell = [[OakSelectBasenameCell alloc] initTextCell:@""];
		[fileTextField.cell setWraps:NO];
		[fileTextField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		fileTextField.editable = YES;
		fileTextField.delegate = self;

		NSDictionary* views = @{ @"icon" : _openButton, @"file" : fileTextField, @"itemInfoButtons" : _itemInfoButtons };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[file]-(4)-[itemInfoButtons]-(0@750)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[itemInfoButtons]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(2)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_openButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[_openButton bind:NSImageBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[fileTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.displayName" options:nil];
		[fileTextField bind:NSToolTipBinding toObject:self withKeyPath:@"objectValue.toolTip" options:nil];
		[_itemInfoButtons bind:@"labelIndex" toObject:self withKeyPath:@"objectValue.labelIndex" options:nil];
		[_itemInfoButtons bind:@"open" toObject:self withKeyPath:@"objectValue.open" options:nil];

		self.textField = fileTextField;
	}
	return self;
}

- (void)dealloc
{
	[_openButton unbind:NSImageBinding];
	[self.textField unbind:NSValueBinding];
	[self.textField unbind:NSToolTipBinding];
	[_itemInfoButtons unbind:@"labelIndex"];
	[_itemInfoButtons unbind:@"open"];
}

- (void)controlTextDidEndEditing:(NSNotification*)aNotification
{
	FSItem* item = self.objectValue;
	if(![item setNewDisplayName:self.textField.stringValue view:self.enclosingScrollView.documentView ?: self])
		item.displayName = [NSString stringWithCxxString:path::display_name([item.url.path fileSystemRepresentation])];
}

- (void)resetCursorRects
{
	[self addCursorRect:self.openButton.frame cursor:[NSCursor pointingHandCursor]];
}
@end

static NSArray* ConvertURLSetToStringArray (NSSet* aSet)
{
	NSMutableArray* res = [NSMutableArray array];
	for(NSURL* url in aSet)
		[res addObject:[url absoluteString]];
	[res sortUsingSelector:@selector(compare:)];
	return res;
}

static NSMutableSet* ConvertStringArrayToURLSet (NSArray* anArray)
{
	NSMutableSet* res = [NSMutableSet set];
	for(NSString* urlString in anArray)
		[res addObject:[NSURL URLWithString:urlString]];
	return res;
}

// ================================
// = OutlineView Helper Functions =
// ================================

static NSSet* VisibleURLs (NSOutlineView* outlineView, FSItem* root, NSMutableSet* res = [NSMutableSet set])
{
	for(FSItem* item in root.children)
	{
		[res addObject:item.url];
		if(!item.leaf && [outlineView isItemExpanded:item])
			VisibleURLs(outlineView, item, res);
	}
	return res;
}

// ================================

struct expansion_state_t
{
	expansion_state_t (std::string const& url, bool recursive = false) : url(url), recursive(recursive) { }

	std::string url;
	bool recursive;
	bool animate = true;
	bool stop = false;
	size_t requests = 0;
};

@interface FSOutlineViewDelegate () <NSOutlineViewDelegate>
{
	IBOutlet OFBOutlineView* _outlineView;
	IBOutlet FSDataSource* _dataSource;

	NSMutableSet* _expandedURLs;
	NSMutableSet* _selectedURLs;

	NSInteger _nestedCollapse;

	std::map<std::string, std::shared_ptr<expansion_state_t>> _expansionRequests;
	std::shared_ptr<expansion_state_t> _expansionState;

	NSURL* _pendingEditURL;
	NSURL* _pendingMakeVisibleURL;
	CGFloat _pendingScrollOffset;
}
@end

@implementation FSOutlineViewDelegate
- (id)init
{
	if((self = [super init]))
	{
		_expandedURLs = ConvertStringArrayToURLSet([[NSUserDefaults standardUserDefaults] arrayForKey:@"ExpandedURLs"]);
		_selectedURLs = [NSMutableSet new];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[self applicationWillTerminate:nil];
	_outlineView.dataSource = nil;
	_outlineView.delegate   = nil;
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	static BOOL mergeWithUserDefaults = NO;
	[_expandedURLs intersectSet:VisibleURLs(_outlineView, _dataSource.rootItem)];
	if(mergeWithUserDefaults)
		[_expandedURLs unionSet:ConvertStringArrayToURLSet([[NSUserDefaults standardUserDefaults] arrayForKey:@"ExpandedURLs"])];
	[[NSUserDefaults standardUserDefaults] setObject:ConvertURLSetToStringArray(_expandedURLs) forKey:@"ExpandedURLs"];
	mergeWithUserDefaults = YES;
}

- (void)setOutlineView:(OFBOutlineView*)anOutlineView
{
	if(_outlineView != anOutlineView)
	{
		[_outlineView setDelegate:nil];
		_outlineView = anOutlineView;
		[_outlineView setDelegate:self];
	}
}

- (void)setDataSource:(FSDataSource*)aDataSource
{
	_pendingEditURL        = nil;
	_pendingMakeVisibleURL = nil;
	_pendingScrollOffset   = 0;

	if(_dataSource)
	{
		if(_outlineView)
		{
			[_selectedURLs minusSet:VisibleURLs(_outlineView, _dataSource.rootItem)];
			NSIndexSet* indexSet = [_outlineView selectedRowIndexes];
			for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
				[_selectedURLs addObject:[[_outlineView itemAtRow:index] url]];

			[_outlineView deselectAll:self];
			[_outlineView setDataSource:nil];
		}
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FSItemDidReloadNotification object:_dataSource];
	}

	if(_dataSource = aDataSource)
	{
		if(NSArray* expandedByDefault = [_dataSource expandedURLs])
			[_expandedURLs addObjectsFromArray:expandedByDefault];
		[_outlineView setDataSource:_dataSource];
		[_outlineView reloadItem:nil reloadChildren:YES];
		[self reloadItem:_dataSource.rootItem usingState:std::shared_ptr<expansion_state_t>()];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(itemDidReload:) name:FSItemDidReloadNotification object:_dataSource];
	}
}

- (void)setModifiedURLs:(NSArray*)newModifiedURLs
{
	_modifiedURLs = newModifiedURLs;
	for(NSInteger i = 0; i < [_outlineView numberOfRows]; ++i)
	{
		FSItem* item = [_outlineView itemAtRow:i];
		item.modified = [_modifiedURLs containsObject:item.url];
	}
}

- (void)setOpenURLs:(NSArray*)newOpenURLs
{
	crash_reporter_info_t crashInfo_1(text::format("%lu open urls, self %p", [newOpenURLs count], self));
	crash_reporter_info_t crashInfo_2(text::format("%lu old open urls", [_openURLs count]));
	_openURLs = newOpenURLs;
	crash_reporter_info_t crashInfo_3(text::format("%ld rows", [_outlineView numberOfRows]));
	for(NSInteger i = 0; i < [_outlineView numberOfRows]; ++i)
	{
		crash_reporter_info_t crashInfo_4(text::format("get item at row %ld", i));
		FSItem* item = [_outlineView itemAtRow:i];
		crash_reporter_info_t crashInfo_5(text::format("assign to item %p", item));
		item.open = [_openURLs containsObject:item.url];
	}
}

- (void)checkPendingSelectAndEditURLs
{
	if(_pendingScrollOffset != 0 && _pendingScrollOffset <= NSHeight([_outlineView frame]) - NSHeight([_outlineView visibleRect]))
	{
		[_outlineView scrollPoint:NSMakePoint(0, _pendingScrollOffset)];
		_pendingScrollOffset = 0;
	}

	for(NSInteger i = 0; i < [_outlineView numberOfRows]; ++i)
	{
		FSItem* item = [_outlineView itemAtRow:i];

		if([_selectedURLs containsObject:item.url])
		{
			[_outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:YES];
			[_selectedURLs removeObject:item.url];
		}

		if([_expandedURLs containsObject:item.url])
			[_outlineView expandItem:item expandChildren:NO];
	}

	if(!_expansionRequests.empty())
		return;

	for(NSInteger i = 0; i < [_outlineView numberOfRows] && _pendingEditURL; ++i)
	{
		if(![_pendingEditURL isEqual:[[_outlineView itemAtRow:i] url]])
			continue;

		[[_outlineView window] makeKeyWindow];
		[_outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
		[_outlineView editColumn:0 row:i withEvent:nil select:YES];
	}
	_pendingEditURL = nil;

	for(NSInteger i = 0; i < [_outlineView numberOfRows] && _pendingMakeVisibleURL; ++i)
	{
		if(![_pendingMakeVisibleURL isEqual:[[_outlineView itemAtRow:i] url]])
			continue;

		NSRect rowRect     = [_outlineView rectOfRow:i];
		NSRect visibleRect = [_outlineView visibleRect];
		if(NSMinY(rowRect) < NSMinY(visibleRect) || NSMaxY(rowRect) > NSMaxY(visibleRect))
			[_outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];

		_pendingMakeVisibleURL = nil;
	}
}

- (void)selectURLs:(NSArray*)someURLs expandChildren:(BOOL)expandAncestors
{
	_selectedURLs = [NSMutableSet setWithArray:someURLs];
	[_outlineView deselectAll:self];

	if([someURLs count] == 1)
		_pendingMakeVisibleURL = [someURLs lastObject];

	if(expandAncestors)
	{
		NSMutableSet* ancestors = [NSMutableSet set];
		NSURL* rootURL = _dataSource.rootItem.url;

		for(NSURL* targetURL in someURLs)
		{
			NSMutableSet* currentAncestors = [NSMutableSet set];
			NSURL* currentURL;

			for(currentURL = ParentForURL(targetURL); currentURL; currentURL = ParentForURL(currentURL))
			{
				if([currentURL isEqual:rootURL])
					break;

				[currentAncestors addObject:currentURL];
			}

			if(currentURL)
				[ancestors unionSet:currentAncestors];
		}

		[_expandedURLs unionSet:ancestors];
	}

	[self checkPendingSelectAndEditURLs];
}

- (void)editURL:(NSURL*)anURL
{
	_selectedURLs   = [NSMutableSet new];
	_pendingEditURL = anURL;
	[self checkPendingSelectAndEditURLs];
}

- (void)scrollToOffset:(CGFloat)anOffset
{
	_pendingScrollOffset = anOffset;
	[self checkPendingSelectAndEditURLs];
}

- (void)itemDidReload:(NSNotification*)aNotification
{
	FSDataSource* aDataSource = [aNotification object];
	if(aDataSource != _dataSource)
		return;

	FSItem* item = [[aNotification userInfo] objectForKey:@"item"];
	[self reloadItem:item usingState:std::shared_ptr<expansion_state_t>()];
}

// =================================
// = Outline view delegate methods =
// =================================

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldSelectItem:(id)item
{
	return [self outlineView:anOutlineView isGroupItem:item] == NO;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView isGroupItem:(id)item
{
	return NO;
}

// ===========================
// = Expand Delegate Methods =
// ===========================

- (void)reloadItem:(FSItem*)item usingState:(std::shared_ptr<expansion_state_t>)state
{
	NSURL* url = item.url;
	if(!url)
		return NSLog(@"%s no url for item %@ using data source %@", sel_getName(_cmd), item, _dataSource);

	if(!state)
	{
		state = std::make_shared<expansion_state_t>([[url absoluteString] fileSystemRepresentation], _outlineView.recursiveRequest);
		state->animate = item != _dataSource.rootItem;
		_expansionRequests.emplace(state->url, state);
	}

	++state->requests;

	[_dataSource reloadItem:item completionHandler:^(NSArray* children){
		BOOL hasChanges = !item.children || ![item.children isEqualToArray:children];
		if(!state->stop && hasChanges)
		{
			for(FSItem* child in children)
			{
				child.modified = [_modifiedURLs containsObject:child.url];
				child.open     = [_openURLs containsObject:child.url];
			}

			NSSet* newItems = [NSSet setWithArray:children];

			id firstResponder = _outlineView.window.firstResponder;
			if([firstResponder isKindOfClass:[NSView class]] && [(NSView*)firstResponder isDescendantOf:_outlineView] && [firstResponder respondsToSelector:@selector(delegate)] && [[firstResponder delegate] respondsToSelector:@selector(abortEditing)])
			{
				NSInteger row = [_outlineView rowForView:firstResponder];
				if(row != -1 && ![newItems containsObject:[_outlineView itemAtRow:row]])
				{
					[[firstResponder delegate] abortEditing];
					[_outlineView.window makeFirstResponder:_outlineView];
				}
			}

			NSIndexSet* removeIndexSet;
			NSIndexSet* insertIndexSet;

			if(children.count < item.children.count)
			{
				NSMutableIndexSet* indexSet = [NSMutableIndexSet new];
				for(NSInteger i = 0, j = 0; i < item.children.count; ++i)
				{
					if(j < children.count && [children[j] isEqual:item.children[i]])
							++j;
					else	[indexSet addIndex:i];
				}

				if(item.children.count - indexSet.count == children.count)
					removeIndexSet = indexSet;
			}
			else if(item.children.count && item.children.count < children.count)
			{
				NSMutableIndexSet* indexSet = [NSMutableIndexSet new];
				for(NSInteger i = 0, j = 0; i < children.count; ++i)
				{
					if(j < item.children.count && [item.children[j] isEqual:children[i]])
							++j;
					else	[indexSet addIndex:i];
				}

				if(item.children.count + indexSet.count == children.count)
					insertIndexSet = indexSet;
			}

			if(!insertIndexSet && !removeIndexSet)
			{
				NSIndexSet* selectedRows = [_outlineView selectedRowIndexes];
				for(NSUInteger row = [selectedRows firstIndex]; row != NSNotFound; row = [selectedRows indexGreaterThanIndex:row])
				{
					FSItem* item = [_outlineView itemAtRow:row];
					if([newItems containsObject:item])
						[_selectedURLs addObject:item.url];
				}
			}

			FSItem* parentItem = item == _dataSource.rootItem ? nil : item;

			item.children = children;
			if(removeIndexSet)
				[_outlineView removeItemsAtIndexes:removeIndexSet inParent:parentItem withAnimation:NSTableViewAnimationSlideDown];
			else if(insertIndexSet)
				[_outlineView insertItemsAtIndexes:insertIndexSet inParent:parentItem withAnimation:parentItem ? NSTableViewAnimationSlideDown : NSTableViewAnimationEffectNone];
			else
				[_outlineView reloadItem:parentItem reloadChildren:YES];

			for(FSItem* child in children)
			{
				if([_selectedURLs containsObject:child.url])
				{
					[_outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:[_outlineView rowForItem:child]] byExtendingSelection:YES];
					[_selectedURLs removeObject:child.url];
				}
			}

			std::shared_ptr<expansion_state_t> oldState = std::exchange(_expansionState, state);
			if(state->recursive)
			{
				[_outlineView expandItem:item expandChildren:YES];
			}
			else
			{
				for(FSItem* child in children)
				{
					if(!child.leaf && [_expandedURLs containsObject:child.url])
						[_outlineView expandItem:child];
				}
			}
			_expansionState = oldState;
		}

		if(--state->requests == 0)
		{
			_expansionRequests.erase(state->url);
			if(_expansionRequests.empty() && (_pendingEditURL || _pendingMakeVisibleURL))
				[self checkPendingSelectAndEditURLs];

			[_outlineView setNeedsDisplay:YES];
		}
	}];
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification
{
	FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
	if(![item isKindOfClass:[FSItem class]])
		return;

	if(item.leaf)
		return;

	[_expandedURLs addObject:item.url];
	if(!item.children)
		[self reloadItem:item usingState:_expansionState];
}

// =============================
// = Collapse Delegate Methods =
// =============================

- (void)outlineViewItemWillCollapse:(NSNotification*)aNotification
{
	FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
	if(![item isKindOfClass:[FSItem class]])
		return;

	if(item.leaf)
		return;

	auto pair = _expansionRequests.find([[item.url absoluteString] fileSystemRepresentation]);
	if(pair != _expansionRequests.end())
		pair->second->stop = true;

	++_nestedCollapse;
}

- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification
{
	FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
	if(![item isKindOfClass:[FSItem class]])
		return;

	if(item.leaf)
		return;

	if(--_nestedCollapse == 0)
	{
		NSMutableSet* toRemove = [NSMutableSet setWithObject:item.url];
		if(_outlineView.recursiveRequest)
		{
			NSString* parentUrlString = [item.url absoluteString];
			for(NSURL* expandedURL in _expandedURLs)
			{
				if([[expandedURL absoluteString] hasPrefix:parentUrlString])
					[toRemove addObject:expandedURL];
			}
		}

		[_expandedURLs minusSet:toRemove];
	}

	if([_dataSource unloadItem:item])
		[_outlineView reloadItem:item reloadChildren:YES];
}

// ===============================
// = Table cell view constructor =
// ===============================

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FSItem*)item
{
	NSTableCellView* res = [outlineView makeViewWithIdentifier:tableColumn.identifier owner:self];
	if(!res)
	{
		res = [[OakFSItemTableCellView alloc] initWithOpenAction:_openItemSelector closeAction:_closeItemSelector target:_target];
		res.identifier = tableColumn.identifier;
	}

	res.objectValue = item;
	return res;
}
@end
