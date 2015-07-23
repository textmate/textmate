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

@interface OakSelectBasenameCell : NSTextFieldCell
@end

@implementation OakSelectBasenameCell
- (void)selectWithFrame:(NSRect)aRect inView:(NSView*)aView editor:(NSText*)aText delegate:(id)someDelegate start:(NSInteger)start length:(NSInteger)length
{
	NSString* basename = [self.stringValue stringByDeletingPathExtension];
	[super selectWithFrame:aRect inView:aView editor:aText delegate:someDelegate start:start length:(start == 0 && basename ? MIN(basename.length, length) : length)];
}
@end

@interface OakFSItemTableCellView : NSTableCellView <NSTextFieldDelegate>
@property (nonatomic) NSButton* openButton;
@property (nonatomic) NSButton* closeButton;
@property (nonatomic) NSArray* openURLs;
@property (nonatomic) NSArray* modifiedURLs;
@property (nonatomic) NSInteger labelIndex;
@end

@implementation OakFSItemTableCellView
- (instancetype)initWithOpenButton:(NSButton*)openButton closeButton:(NSButton*)closeButton
{
	if((self = [super initWithFrame:NSZeroRect]))
	{
		_openButton  = openButton;
		_closeButton = closeButton;

		[openButton setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[openButton setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSTextField* fileTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		fileTextField.cell = [[OakSelectBasenameCell alloc] initTextCell:@""];
		fileTextField.editable = YES;
		fileTextField.delegate = self;

		NSDictionary* views = @{ @"icon" : openButton, @"file" : fileTextField, @"close" : closeButton };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[file]-(4@750)-[close(==16)]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(2)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:openButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:closeButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[openButton bind:NSImageBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[fileTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.displayName" options:nil];
		[fileTextField bind:NSToolTipBinding toObject:self withKeyPath:@"objectValue.toolTip" options:nil];
		[self bind:@"labelIndex" toObject:self withKeyPath:@"objectValue.labelIndex" options:nil];

		self.textField = fileTextField;
	}
	return self;
}

- (void)controlTextDidEndEditing:(NSNotification*)aNotification
{
	FSItem* item = self.objectValue;
	if(![item setNewDisplayName:self.textField.stringValue view:self])
		item.displayName = [NSString stringWithCxxString:path::display_name([item.url.path fileSystemRepresentation])];
}

- (void)setObjectValue:(FSItem*)item
{
	[super setObjectValue:item];
	self.openURLs = _openURLs;
	self.modified = [_modifiedURLs containsObject:item.url];
}

- (void)setLabelIndex:(NSInteger)newLabelIndex
{
	if(_labelIndex != newLabelIndex)
	{
		_labelIndex = newLabelIndex;
		[self setNeedsDisplay:YES];
	}
}

- (void)setModified:(BOOL)flag
{
	FSItem* item  = self.objectValue;
	NSImage* icon = [item.icon copy];

	SEL setModifiedSelector = @selector(setModified:);
	if([icon respondsToSelector:setModifiedSelector])
	{
		auto fn = (void(*)(id, SEL, BOOL))[icon methodForSelector:setModifiedSelector];
		fn(icon, setModifiedSelector, flag);
		item.icon = icon;
	}
}

- (void)setOpenURLs:(NSArray*)someURLs
{
	_openURLs = someURLs;
	FSItem* item = self.objectValue;
	_closeButton.hidden = ![_openURLs containsObject:item.url];
}

- (void)setModifiedURLs:(NSArray*)someURLs
{
	FSItem* item     = self.objectValue;
	BOOL wasModified = [_modifiedURLs containsObject:item.url];
	BOOL isModified  = [someURLs containsObject:item.url];

	_modifiedURLs = someURLs;
	if(wasModified != isModified)
		self.modified = isModified;
}

- (void)resetCursorRects
{
	[self addCursorRect:self.openButton.frame cursor:[NSCursor pointingHandCursor]];
}

- (void)drawLabelIndex:(NSUInteger)labelColorIndex inFrame:(NSRect)cellFrame
{
	if(labelColorIndex == 0)
		return;
	ASSERT(labelColorIndex < 8);

	// color names: Gray, Green, Purple, Blue, Yellow, Red, Orange
	static NSString* const startCol[] = { @"#CFCFCF", @"#D4EE9C", @"#DDBDEA", @"#ACD0FE", @"#F8F79C", @"#FC999A", @"#F9D194" };
	static NSString* const stopCol[]  = { @"#A8A8A8", @"#AFDC49", @"#C186D7", @"#5B9CFE", @"#ECDF4A", @"#FC605C", @"#F6AC46" };

	NSRect r = NSIntegralRect(NSInsetRect(cellFrame, 2, 0)), unused;
	if(self.backgroundStyle == NSBackgroundStyleDark)
		NSDivideRect(r, &r, &unused, 30, NSMaxXEdge);

	NSGradient* gradient = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithString:startCol[labelColorIndex-1]] endingColor:[NSColor colorWithString:stopCol[labelColorIndex-1]]];
	NSBezierPath* path = [NSBezierPath bezierPathWithRoundedRect:r xRadius:8 yRadius:8];
	[gradient drawInBezierPath:path angle:90];
}

- (void)drawRect:(NSRect)aRect
{
	[self drawLabelIndex:_labelIndex inFrame:[self bounds]];
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
		[_selectedURLs minusSet:VisibleURLs(_outlineView, _dataSource.rootItem)];
		NSIndexSet* indexSet = [_outlineView selectedRowIndexes];
		for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
			[_selectedURLs addObject:[[_outlineView itemAtRow:index] url]];

		[_outlineView deselectAll:self];
		[_outlineView setDataSource:nil];
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
	if(item.leaf)
			[_outlineView reloadItem:item reloadChildren:NO];
	else	[self reloadItem:item usingState:std::shared_ptr<expansion_state_t>()];
}

// =================================
// = Outline view delegate methods =
// =================================

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldSelectItem:(id)item
{
	return [self outlineView:anOutlineView isGroupItem:item] == NO;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView isGroupItem:(FSItem*)item
{
	return [item respondsToSelector:@selector(group)] ? item.group : NO;
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
			NSIndexSet* indexSet = [_outlineView selectedRowIndexes];
			for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
				[_selectedURLs addObject:[[_outlineView itemAtRow:index] url]];

			if([_outlineView editedRow] != -1)
			{
				NSLog(@"%s refresh while editing row", sel_getName(_cmd));
				_pendingEditURL = [[_outlineView itemAtRow:[_outlineView editedRow]] url];
				[_outlineView abortEditing];
			}

			[_outlineView beginUpdates];
			item.children = children;
			[_outlineView reloadItem:(item == _dataSource.rootItem ? nil : item) reloadChildren:YES];
			[_outlineView endUpdates];

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
		NSButton* openButton = [[NSButton alloc] initWithFrame:NSZeroRect];
		openButton.refusesFirstResponder = YES;
		openButton.buttonType            = NSMomentaryChangeButton;
		openButton.bordered              = NO;
		openButton.imagePosition         = NSImageOnly;
		openButton.target                = _target;
		openButton.action                = _openItemSelector;

		OakRolloverButton* closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		OakSetAccessibilityLabel(closeButton, @"Close document");

		closeButton.regularImage  = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:[self class]];
		closeButton.pressedImage  = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:[self class]];
		closeButton.rolloverImage = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:[self class]];
		closeButton.target        = _target;
		closeButton.action        = _closeItemSelector;

		res = [[OakFSItemTableCellView alloc] initWithOpenButton:openButton closeButton:closeButton];
		res.identifier = tableColumn.identifier;

		[res bind:@"openURLs" toObject:self withKeyPath:@"openURLs" options:nil];
		[res bind:@"modifiedURLs" toObject:self withKeyPath:@"modifiedURLs" options:nil];
	}

	res.objectValue = item;
	return res;
}
@end
