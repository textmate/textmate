#import "FSOutlineViewDelegate.h"
#import "OakFSUtilities.h"
#import "io/FSDataSource.h"
#import "io/FSItem.h"
#import "ui/OFBOutlineView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFinderTag.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <ns/ns.h>
#import <io/path.h>
#import <text/utf8.h>
#import <oak/oak.h>

@interface FSItemWrapper : NSObject <NSCopying>
@property (nonatomic) NSString* displayName;
@property (nonatomic) NSString* editingName;
@end

@implementation FSItemWrapper
- (id)initWithDisplayName:(NSString*)display editingName:(NSString*)editing
{
	if((self = [super init]))
	{
		_displayName = display;
		_editingName = editing;
	}
	return self;
}

- (id)copyWithZone:(NSZone*)aZone { return [[FSItemWrapper alloc] initWithDisplayName:_displayName editingName:_editingName]; }
- (NSUInteger)hash                { return [_editingName hash]; }
- (BOOL)isEqual:(id)otherObject   { return [otherObject isKindOfClass:[self class]] && [_editingName isEqual:[otherObject editingName]] && [_displayName isEqual:[otherObject displayName]]; }
- (NSString*)description          { return [NSString stringWithFormat:@"<%@: %@ (%@)>", self.class, _displayName, _editingName]; }
@end

@interface FSItem (FSItemWrapper)
- (FSItemWrapper*)wrappedValue;
@end

@implementation FSItem (FSItemWrapper)
+ (NSSet*)keyPathsForValuesAffectingWrappedValue
{
	return [NSSet setWithObjects:@"displayName", @"url", nil];
}

- (FSItemWrapper*)wrappedValue
{
	NSString* editingName = [[self.url.path lastPathComponent] stringByReplacingOccurrencesOfString:@":" withString:@"/"];
	return [[FSItemWrapper alloc] initWithDisplayName:self.displayName editingName:editingName];
}

- (void)setWrappedValue:(FSItemWrapper*)wrappedValue
{
	// Because ‘wrappedValue’ is bound to our text field then we receive updates when user edtis the text field
}
@end

@interface FSItemFormatter : NSFormatter
@property (nonatomic) FSItemWrapper* originalValue;
@end

@implementation FSItemFormatter
- (NSString*)stringForObjectValue:(id)aValue
{
	// If the table view is set to render as source list
	// then we may receive an attributed string as value

	if([aValue isKindOfClass:[FSItemWrapper class]])
		return ((FSItemWrapper*)aValue).displayName;
	else if([aValue isKindOfClass:[NSString class]])
		return aValue;
	else if([aValue isKindOfClass:[NSAttributedString class]])
		return ((NSAttributedString*)aValue).string;
	return [NSString stringWithFormat:@"Unexpected value type: %@", [aValue class]];
}

- (NSString*)editingStringForObjectValue:(id)aValue
{
	if([aValue isKindOfClass:[FSItemWrapper class]])
	{
		_originalValue = [aValue copy];
		return _originalValue.editingName;
	}
	return [super editingStringForObjectValue:aValue];
}

- (BOOL)getObjectValue:(id*)valueRef forString:(NSString*)aString errorDescription:(NSString**)errorRef
{
	if(_originalValue) // This is nil when rendering as source list
	{
		_originalValue.editingName = [aString copy];
		*valueRef = _originalValue;
	}
	else
	{
		*valueRef = aString;
	}
	return YES;
}
@end

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
@property (nonatomic) NSArray<OakFinderTag*>* finderTags;
@end

@implementation OakLabelSwatchView
- (void)setFinderTags:(NSArray<OakFinderTag*>*)newFinderTags
{
	if(![_finderTags isEqual:newFinderTags])
	{
		_finderTags = newFinderTags;
		[self setNeedsDisplay:YES];
	}
}

- (BOOL)isSelectedAndEmphasized
{
	NSView* view = self;
	while(view && ![view isKindOfClass:[NSTableRowView class]])
		view = [view superview];
	return [view isKindOfClass:[NSTableRowView class]] && ((NSTableRowView*)view).isSelected && ((NSTableRowView*)view).isEmphasized;
}

- (void)drawRect:(NSRect)aRect
{
	NSMutableArray* labelColors = [NSMutableArray array];
	for(OakFinderTag* tag in _finderTags)
		if([tag hasLabelColor])
			[labelColors addObject:@(tag.label)];

	auto fillAndStrokePath = ^(NSBezierPath* path, NSNumber* label){
		NSUInteger labelInt = [label unsignedIntegerValue];
		[[OakFinderTagManager backgroundColorForLabel:labelInt] set];
		[path fill];

		self.isSelectedAndEmphasized ? [[NSColor whiteColor] set] : [[OakFinderTagManager foregroundColorForLabel:labelInt] set];
		[path stroke];
	};

	auto drawCrestent = ^(NSPoint center1, NSPoint center2, NSNumber* label){
		[NSGraphicsContext saveGraphicsState];

		NSBezierPath* clippingPath = [NSBezierPath bezierPath];
		[clippingPath appendBezierPathWithArcWithCenter:center2 radius:5.0 startAngle:-100 endAngle:100];
		[clippingPath appendBezierPathWithArcWithCenter:center1 radius:5.5 startAngle:60 endAngle:300 clockwise:YES];
		[clippingPath addClip];

		NSBezierPath* path = [NSBezierPath bezierPath];
		[path appendBezierPathWithArcWithCenter:center2 radius:4.0 startAngle:0 endAngle:360];
		[path closePath];

		fillAndStrokePath(path, label);

      [NSGraphicsContext restoreGraphicsState];
	};

	NSRect r = [self bounds];
	switch([labelColors count])
	{
		case 0: return;
		case 1:
		{
			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:NSMakePoint(NSMidX(r), NSMidY(r)) radius:4.0 startAngle:0 endAngle:360];

			fillAndStrokePath(path, labelColors[0]);
			return;
		}
		case 2:
		{
			NSPoint center = NSMakePoint(NSMidX(r), NSMidY(r));

			NSPoint center1 = NSMakePoint(center.x - 2.0, center.y);
			NSPoint center2 = NSMakePoint(center.x + 2.0, center.y);

			drawCrestent(center1, center2, labelColors[0]);

			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:center1 radius:4.0 startAngle:0 endAngle:360];
			fillAndStrokePath(path, labelColors[1]);
			return;
		}
		default:
		{
			NSPoint center = NSMakePoint(NSMidX(r), NSMidY(r));

			NSPoint center1 = NSMakePoint(center.x - 4.0, center.y);
			NSPoint center2 = NSMakePoint(center.x, center.y);
			NSPoint center3 = NSMakePoint(center.x + 4.0, center.y);

			NSUInteger lastIndex = [labelColors count] - 1;
			drawCrestent(center2, center3, labelColors[lastIndex - 2]);
			drawCrestent(center1, center2, labelColors[lastIndex - 1]);

			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:center1 radius:4.0 startAngle:0 endAngle:360];
			fillAndStrokePath(path, labelColors[lastIndex]);
			return;
		}
	}
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(20, 10);
}
@end

@interface OakItemButtonsView : NSView
@property (nonatomic) NSArray<OakFinderTag*>* finderTags;
@property (nonatomic) BOOL open;

@property (nonatomic) SEL closeAction;
@property (nonatomic, weak) id target;

@property (nonatomic) OakLabelSwatchView* labelSwatchView;
@property (nonatomic) NSButton* closeButton;
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

- (NSArray<OakFinderTag*>*)finderTags
{
	return _labelSwatchView ? _labelSwatchView.finderTags : @[ ];
}

- (void)setFinderTags:(NSArray<OakFinderTag*>*)newFinderTags
{
	if([self.finderTags isEqual:newFinderTags])
		return;

	if(!newFinderTags && [newFinderTags count] == 0)
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
	_labelSwatchView.finderTags = newFinderTags;
}

- (void)setNilValueForKey:(NSString*)aKey
{
	if([aKey isEqualToString:@"finderTags"])
		[self setValue:@[ ] forKey:aKey];
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
		_closeButton = OakCreateCloseButton();
		_closeButton.refusesFirstResponder = YES;
		_closeButton.target = _target;
		_closeButton.action = _closeAction;

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
		fileTextField.formatter = [[FSItemFormatter alloc] init];

		NSDictionary* views = @{ @"icon" : _openButton, @"file" : fileTextField, @"itemInfoButtons" : _itemInfoButtons };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[file]-(4)-[itemInfoButtons]-(0@750)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[itemInfoButtons]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(2)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_openButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[_openButton bind:NSImageBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[fileTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.wrappedValue" options:nil];
		[fileTextField bind:NSToolTipBinding toObject:self withKeyPath:@"objectValue.toolTip" options:nil];
		[_itemInfoButtons bind:@"finderTags" toObject:self withKeyPath:@"objectValue.finderTags" options:nil];
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
	[_itemInfoButtons unbind:@"finderTags"];
	[_itemInfoButtons unbind:@"open"];
}

- (void)controlTextDidEndEditing:(NSNotification*)aNotification
{
	NSString* newName = self.textField.stringValue;
	if([self.textField.objectValue isKindOfClass:[FSItemWrapper class]])
		newName = ((FSItemWrapper*)self.textField.objectValue).editingName;

	FSItem* item = (FSItem*)self.objectValue;
	if(OakNotEmptyString(newName))
		[item renameToName:newName view:self.enclosingScrollView.documentView ?: self];

	// Update NSTextField’s objectValue to new name
	item.wrappedValue = item.wrappedValue;
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
		_expandedURLs = ConvertStringArrayToURLSet([[NSUserDefaults standardUserDefaults] stringArrayForKey:@"ExpandedURLs"]);
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
		[_expandedURLs unionSet:ConvertStringArrayToURLSet([[NSUserDefaults standardUserDefaults] stringArrayForKey:@"ExpandedURLs"])];
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
	_openURLs = newOpenURLs;
	for(NSInteger i = 0; i < [_outlineView numberOfRows]; ++i)
	{
		FSItem* item = [_outlineView itemAtRow:i];
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
		{
			if(NSEqualRects(rowRect, NSIntersectionRect(rowRect, [_outlineView bounds])))
			{
				[_outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];
			}
			else
			{
				NSDate* timeoutDate = [NSDate dateWithTimeIntervalSinceNow:1];
				__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSViewFrameDidChangeNotification object:_outlineView queue:nil usingBlock:^(NSNotification*){
					BOOL canMakeVisible = NSEqualRects(rowRect, NSIntersectionRect(rowRect, [_outlineView bounds]));
					if(canMakeVisible)
						[_outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];
					if(canMakeVisible || [timeoutDate compare:[NSDate date]] == NSOrderedAscending)
						[[NSNotificationCenter defaultCenter] removeObserver:observerId];
				}];
			}
		}

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
				{
					removeIndexSet = indexSet;

					NSMutableArray* newChildren = [item.children mutableCopy];
					[newChildren removeObjectsAtIndexes:removeIndexSet];
					children = newChildren;
				}
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
				{
					insertIndexSet = indexSet;

					NSMutableArray* newChildren = [item.children mutableCopy];
					[newChildren insertObjects:[children objectsAtIndexes:insertIndexSet] atIndexes:insertIndexSet];
					children = newChildren;
				}
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
