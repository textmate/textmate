#import "OakTabBarView.h"
#import "OakTabItemView.h"
#import "OakRolloverButton.h"
#import "OakFileIconImage.h"
#import "OakAppKit.h"
#import "NSImage Additions.h"
#import "NSMenuItem Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <oak/debug.h>

NSString* const kUserDefaultsDisableTabBarCollapsingKey = @"disableTabBarCollapsing";

static NSString* const OakTabItemPasteboardType = @"OakTabItemPasteboardType";

@interface OakTabItem ()
@property (nonatomic) OakTabItemView* tabItemView;
@property (nonatomic) NSRect targetFrame;
@end

@implementation OakTabItem
+ (instancetype)tabItemWithTitle:(NSString*)aTitle path:(NSString*)aPath identifier:(NSString*)anIdentifier modified:(BOOL)flag;
{
	OakTabItem* res = [OakTabItem new];
	res.title      = aTitle;
	res.path       = aPath;
	res.identifier = anIdentifier;
	res.modified   = flag;
	return res;
}

+ (instancetype)tabItemFromPasteboard:(NSPasteboard*)aPasteboard
{
	NSDictionary* plist = [aPasteboard propertyListForType:OakTabItemPasteboardType];
	if(!plist)
		return nil;

	OakTabItem* res = [OakTabItem new];
	res.title      = plist[@"title"];
	res.path       = plist[@"path"];
	res.identifier = plist[@"identifier"];
	res.modified   = [plist[@"modified"] boolValue];
	return res;
}

- (void)writeToPasteboard:(NSPasteboard*)aPasteboard
{
	NSMutableDictionary* dict = [NSMutableDictionary dictionary];
	if(_title)
		dict[@"title"] = _title;
	if(OakNotEmptyString(_path))
		dict[@"path"] = _path;
	if(_identifier)
		dict[@"identifier"] = _identifier;
	if(_modified)
		dict[@"modified"] = @(_modified);

	[aPasteboard declareTypes:@[ OakTabItemPasteboardType ] owner:self];
	[aPasteboard setPropertyList:dict forType:OakTabItemPasteboardType];
}

- (void)setModified:(BOOL)flag
{
	if(_modified == flag)
		return;
	_tabItemView.modified = _modified = flag;
}

- (void)setTitle:(NSString*)aTitle
{
	if(_title == aTitle || [_title isEqualToString:aTitle])
		return;
	_tabItemView.title = _title = aTitle;
}

- (void)setPath:(NSString*)aPath
{
	if(_path == aPath || [_path isEqualToString:aPath])
		return;
	_path = aPath;
	[_tabItemView setToolTip:OakIsEmptyString(_path) ? _title : [_path stringByAbbreviatingWithTildeInPath]];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<OakTabItem: %p title: %@>", self, _title];
}
@end

@interface OakTabBarView () <NSDraggingSource, NSDraggingDestination>
{
	NSMutableArray* _tabItems;

	NSUInteger _draggedTabIndex;
	OakTabItem* _draggedTabItem;
	OakTabItem* _preliminaryTabItem;
	OakTabItem* _overflowTabItem;

	NSInteger _tag;

	NSButton* _addTabButton;
	NSTrackingArea* _trackingArea;
	BOOL _animateLayoutChanges;

	NSUInteger _didCloseTabIndex;
	NSRect _didCloseTabFrame;
}
@property (nonatomic) BOOL expanded;
@property (nonatomic) NSPoint mouseDownPos;
@property (nonatomic) BOOL isMouseInside;
@end

@implementation OakTabBarView
- (id)initWithFrame:(NSRect)aFrame
{
	if(self = [super initWithFrame:aFrame])
	{
		_tabItems = [NSMutableArray new];
		_expanded = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabBarCollapsingKey];
		if(_expanded)
			[self setupAddTabButton];

		OakTabBarStyle* tabStyle = [OakTabBarStyle sharedInstance];
		[tabStyle setupTabBarView:self];

		[self registerForDraggedTypes:@[ OakTabItemPasteboardType ]];
		self.wantsLayer = YES;
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)setupAddTabButton
{
	if(_addTabButton)
		return;

	_addTabButton = [[NSButton alloc] initWithFrame:NSMakeRect(0, 2, 26, 20)];
	OakSetAccessibilityLabel(_addTabButton, @"Create new tab");
	[[_addTabButton cell] setBackgroundStyle:NSBackgroundStyleRaised];
	_addTabButton.image      = [NSImage imageNamed:NSImageNameAddTemplate];
	_addTabButton.bordered   = NO;
	_addTabButton.buttonType = NSMomentaryChangeButton;
	_addTabButton.toolTip    = @"Create new tab";
	_addTabButton.action     = @selector(_newTab:);
	_addTabButton.target     = self;
	[self addSubview:_addTabButton];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.expanded = _expanded || [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabBarCollapsingKey];
}

- (void)expand
{
	self.expanded = YES;
}

- (void)setExpanded:(BOOL)flag
{
	if(_expanded == flag)
		return;
	_expanded = flag;
	[self invalidateIntrinsicContentSize];
	if(_expanded && !_addTabButton)
		[self setupAddTabButton];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(OakTabBarStyle.sharedInstance.minimumTabSize, _expanded ? self.activeBackgroundImage.size.height : 2);
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];
	if(_trackingArea)
		[self removeTrackingArea:_trackingArea];
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways;
	if(self.isMouseInside = NSMouseInRect([self convertPoint:[self.window mouseLocationOutsideOfEventStream] fromView:nil], [self visibleRect], [self isFlipped]))
		options |= NSTrackingAssumeInside;
	_trackingArea = [[NSTrackingArea alloc] initWithRect:[self bounds] options:options owner:self userInfo:nil];
	[self addTrackingArea:_trackingArea];
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.isMouseInside = YES;
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.isMouseInside = NO;
}

- (void)setIsMouseInside:(BOOL)flag
{
	if(_isMouseInside == flag)
		return;
	_isMouseInside = flag;

	if(!flag && _didCloseTabIndex)
	{
		_didCloseTabIndex = 0;
		[self animateLayoutUpdate];
	}
}

// ==========
// = Layout =
// ==========

- (CGFloat)leftPadding
{
	CGFloat res = OakTabBarStyle.sharedInstance.leftPadding;
	return _neverHideLeftBorder ? MAX(0, res) : res;
}

- (void)updateCapImageViews
{
	if(OakTabBarStyle.sharedInstance.tabViewSpacing >= 0)
		return;

	NSUInteger selected = NSNotFound;
	NSUInteger dragged = NSNotFound;
	NSMutableArray* items = [NSMutableArray array];
	for(OakTabItem* tabItem in _tabItems)
	{
		if(!tabItem.tabItemView)
			continue;

		if(tabItem.tabItemView.isHidden)
			dragged = items.count;
		else if(tabItem.tabItemView.selected)
			selected = items.count;

		[items addObject:tabItem];
	}

	for(NSUInteger i = 0; i < items.count; ++i)
	{
		OakTabItem* tabItem = items[i];
		BOOL leftHidden  = i == dragged || (selected < i && dragged+1 != i) || (selected == NSNotFound && i > 0 && i-1 != dragged);
		BOOL rightHidden = i == dragged || (i < selected && i+1 != dragged && selected != NSNotFound);
		if(leftHidden && rightHidden)
			tabItem.tabItemView.visibleCaps = OakTabItemViewVisibleCapsNone;
		else if(leftHidden)
			tabItem.tabItemView.visibleCaps = OakTabItemViewVisibleCapsRight;
		else if(rightHidden)
			tabItem.tabItemView.visibleCaps = OakTabItemViewVisibleCapsLeft;
		else
			tabItem.tabItemView.visibleCaps = OakTabItemViewVisibleCapsBoth;
	}
}

- (NSUInteger)countOfVisibleTabs
{
	CGFloat const leftPadding  = self.leftPadding;
	CGFloat const rightPadding = OakTabBarStyle.sharedInstance.rightPadding;
	CGFloat const tabSpacing   = OakTabBarStyle.sharedInstance.tabViewSpacing;
	CGFloat const tabMinWidth  = OakTabBarStyle.sharedInstance.minimumTabSize;

	CGFloat width = NSWidth(self.bounds) - NSWidth(_addTabButton.frame) - rightPadding - leftPadding;

	BOOL missingDraggedTab =  _draggedTabItem && !_preliminaryTabItem;
	BOOL hasSomeonesTab    = !_draggedTabItem &&  _preliminaryTabItem;

	NSUInteger canShowTabs = floor((width + tabSpacing) / (tabMinWidth + tabSpacing));
	NSUInteger myTabsCount = _tabItems.count + (missingDraggedTab ? 1 : 0) - (hasSomeonesTab ? 1 : 0);

	return MIN(canShowTabs, myTabsCount);
}

- (void)resizeTabIndexes:(NSIndexSet*)anIndexSet inRect:(NSRect)aRect
{
	NSUInteger countOfTabs = [anIndexSet count];

	CGFloat const spacing = OakTabBarStyle.sharedInstance.tabViewSpacing;
	CGFloat width = NSWidth(aRect) - spacing * (countOfTabs-1);

	CGFloat totalSurplus = 0;
	CGFloat totalDeficit = 0;

	for(NSUInteger i = 0, index = [anIndexSet firstIndex]; index != NSNotFound; ++i, (index = [anIndexSet indexGreaterThanIndex:index]))
	{
		OakTabItem* tabItem = _tabItems[index];
		if(!tabItem.tabItemView)
		{
			tabItem.tabItemView = [[OakTabItemView alloc] initWithFrame:NSZeroRect title:tabItem.title modified:tabItem.modified];
			tabItem.tabItemView.selected = tabItem == _selectedTabItem;
			tabItem.tabItemView.hidden   = tabItem == _preliminaryTabItem;
			tabItem.tabItemView.toolTip  = OakIsEmptyString(tabItem.path) ? tabItem.title : [tabItem.path stringByAbbreviatingWithTildeInPath];
			tabItem.tabItemView.showOverflowButton = tabItem == _overflowTabItem;
			tabItem.tabItemView.closeButton.action = @selector(_performCloseTab:);
			tabItem.tabItemView.closeButton.target = self;
			[self addSubview:tabItem.tabItemView];
		}

		CGFloat x1 = round((i+0) * width / countOfTabs);
		CGFloat x2 = round((i+1) * width / countOfTabs);

		CGFloat tabWidth   = x2-x1;
		CGFloat deltaWidth = tabWidth - tabItem.tabItemView.fittingSize.width;

		if(deltaWidth > 0)
			totalSurplus += deltaWidth;
		else if(deltaWidth < 0)
			totalDeficit -= deltaWidth;
	}

	CGFloat redistribution = MIN(totalSurplus, totalDeficit);
	CGFloat currentSurplus = 0;
	CGFloat currentDeficit = 0;

	CGFloat x = NSMinX(aRect);
	for(NSUInteger i = 0, index = [anIndexSet firstIndex]; index != NSNotFound; ++i, (index = [anIndexSet indexGreaterThanIndex:index]))
	{
		OakTabItem* tabItem = _tabItems[index];

		CGFloat x1 = round((i+0) * width / countOfTabs);
		CGFloat x2 = round((i+1) * width / countOfTabs);

		CGFloat tabWidth   = x2-x1;
		CGFloat deltaWidth = tabWidth - tabItem.tabItemView.fittingSize.width;

		if(redistribution > 0)
		{
			if(deltaWidth > 0)
			{
				CGFloat low = round(redistribution * currentSurplus / totalSurplus);
				currentSurplus += deltaWidth;
				CGFloat high = round(redistribution * currentSurplus / totalSurplus);
				tabWidth -= high - low;
			}
			else if(deltaWidth < 0)
			{
				CGFloat low = round(redistribution * currentDeficit / totalDeficit);
				currentDeficit -= deltaWidth;
				CGFloat high = round(redistribution * currentDeficit / totalDeficit);
				tabWidth += high - low;
			}
		}

		NSRect tabFrame = NSMakeRect(x, NSMinY(aRect), tabWidth, NSHeight(aRect));
		x += tabWidth + spacing;

		if(NSIsEmptyRect(tabItem.tabItemView.frame))
		{
			tabItem.tabItemView.frame = tabFrame;
			if(_animateLayoutChanges && tabItem != _preliminaryTabItem)
			{
				tabItem.tabItemView.alphaValue = 0;
				[[tabItem.tabItemView animator] setAlphaValue:1];
			}
		}

		if(NSEqualRects(tabFrame, tabItem.targetFrame))
			continue;

		tabItem.targetFrame = tabFrame;
		[(_animateLayoutChanges ? [tabItem.tabItemView animator] : tabItem.tabItemView) setFrame:tabFrame];
	}
}

- (void)resizeTabItemViewFrames
{
	CGFloat const leftPadding  = self.leftPadding;
	CGFloat const rightPadding = OakTabBarStyle.sharedInstance.rightPadding;
	CGFloat const tabSpacing   = OakTabBarStyle.sharedInstance.tabViewSpacing;
	CGFloat const tabMinWidth  = OakTabBarStyle.sharedInstance.minimumTabSize;
	CGFloat const tabMaxWidth  = OakTabBarStyle.sharedInstance.maximumTabSize;

	NSRect bounds = NSMakeRect(leftPadding, 0, NSWidth(self.bounds) - NSWidth(_addTabButton.frame) - rightPadding - leftPadding, NSHeight(self.bounds));

	if(!_tabItems.count || !_expanded || NSWidth(bounds) < tabMinWidth)
	{
		for(OakTabItem* tabItem in _tabItems)
		{
			if(tabItem.tabItemView)
				[self removeViewForTabItem:tabItem];
		}
		return;
	}

	BOOL missingDraggedTab =  _draggedTabItem && !_preliminaryTabItem;
	BOOL hasSomeonesTab    = !_draggedTabItem &&  _preliminaryTabItem;

	NSUInteger canShowTabs = floor((NSWidth(bounds) + tabSpacing) / (tabMinWidth + tabSpacing));
	NSUInteger myTabsCount = _tabItems.count + (missingDraggedTab ? 1 : 0) - (hasSomeonesTab ? 1 : 0);
	BOOL showOverflowMenu  = canShowTabs < myTabsCount;

	if(showOverflowMenu && hasSomeonesTab)
		++canShowTabs;
	else if(showOverflowMenu && missingDraggedTab)
		--canShowTabs;

	NSUInteger countOfVisibleTabs = MIN(canShowTabs, _tabItems.count);

	OakTabItem* overflowTabItem = nil;
	if(showOverflowMenu)
	{
		overflowTabItem = _overflowTabItem;
		if(!_preliminaryTabItem && !_draggedTabItem)
		{
			if(_selectedTabItem && [_tabItems indexOfObject:_selectedTabItem] >= countOfVisibleTabs-1)
				overflowTabItem = _selectedTabItem;
			else if(!_overflowTabItem || [_tabItems indexOfObject:_overflowTabItem] < countOfVisibleTabs-1)
				overflowTabItem = _tabItems[countOfVisibleTabs-1];
		}
	}

	if(_overflowTabItem && _overflowTabItem != overflowTabItem)
		_overflowTabItem.tabItemView.showOverflowButton = NO;
	_overflowTabItem = overflowTabItem;
	_overflowTabItem.tabItemView.showOverflowButton = YES;

	NSMutableIndexSet* tabIndexes = [NSMutableIndexSet indexSet];
	OakTabItem* mustShow[] = { _selectedTabItem, _overflowTabItem, _preliminaryTabItem };
	for(OakTabItem* tabItem : mustShow)
	{
		NSUInteger tabIndex = [_tabItems indexOfObject:tabItem];
		if(tabIndex != NSNotFound)
			[tabIndexes addIndex:tabIndex];
	}
	for(NSUInteger i = 0; i < _tabItems.count && [tabIndexes count] < countOfVisibleTabs; ++i)
		[tabIndexes addIndex:i];

	for(NSUInteger i = 0; i < _tabItems.count; ++i)
	{
		OakTabItem* tabItem = _tabItems[i];
		if(tabItem.tabItemView && ![tabIndexes containsIndex:i])
			[self removeViewForTabItem:tabItem];
	}

	if(_didCloseTabIndex)
	{
		NSRect leftRect, rightRect;
		NSDivideRect(bounds, &leftRect, &rightRect, NSMinX(_didCloseTabFrame) - leftPadding, NSMinXEdge);

		NSIndexSet* rightSet = [tabIndexes indexesInRange:NSMakeRange(_didCloseTabIndex, [tabIndexes lastIndex]) options:0 passingTest:^(NSUInteger idx, BOOL* stop){ return YES; }];
		rightRect.size.width = MIN((NSWidth(_didCloseTabFrame) + tabSpacing) * [rightSet count] - tabSpacing, NSWidth(rightRect));
		[self resizeTabIndexes:rightSet inRect:rightRect];
	}
	else
	{
		bounds.size.width = MIN((tabMaxWidth + tabSpacing) * [tabIndexes count] - tabSpacing, NSWidth(bounds));
		[self resizeTabIndexes:tabIndexes inRect:bounds];
	}

	if(_overflowTabItem)
	{
		_overflowTabItem.tabItemView.overflowButton.action = @selector(_showOverflowMenu:);
		_overflowTabItem.tabItemView.overflowButton.target = self;
	}

	[self updateCapImageViews];

	NSUInteger lastTabIndex = [tabIndexes lastIndex];
	OakTabItem* lastTab = _tabItems[lastTabIndex];
	NSRect addButtonFrame = NSMakeRect(NSMaxX(lastTab.targetFrame) + rightPadding, 2, NSWidth(_addTabButton.frame), NSHeight(self.bounds)-4);
	[(_animateLayoutChanges ? [_addTabButton animator] : _addTabButton) setFrame:addButtonFrame];
}

- (void)resizeSubviewsWithOldSize:(NSSize)aSize
{
	[super resizeSubviewsWithOldSize:aSize];
	if(!NSEqualSizes(self.bounds.size, aSize))
		[self resizeTabItemViewFrames];
}

- (void)removeViewForTabItem:(OakTabItem*)aTabItem
{
	[aTabItem.tabItemView removeFromSuperview];
	aTabItem.tabItemView = nil;
	aTabItem.targetFrame = NSZeroRect;
}

- (void)animateLayoutUpdate
{
	static CGFloat const kAnimationDuration = 0.25;

	[NSAnimationContext runAnimationGroup:^(NSAnimationContext* context){
		context.duration = kAnimationDuration;
		_animateLayoutChanges = YES;
		[self resizeTabItemViewFrames];
		_animateLayoutChanges = NO;
	} completionHandler:^{
		[self.window recalculateKeyViewLoop];
	}];
}

- (void)setNeverHideLeftBorder:(BOOL)flag
{
	if(_neverHideLeftBorder == flag)
		return;
	_neverHideLeftBorder = flag;
	[self resizeTabItemViewFrames];
}

// =============
// = Selection =
// =============

- (OakTabItem*)tabItemForView:(id)aView
{
	if([aView isKindOfClass:[NSView class]])
	{
		while(aView && ![aView isKindOfClass:[OakTabItemView class]])
			aView = [aView superview];

		if(aView)
		{
			for(OakTabItem* tabItem in _tabItems)
			{
				if(tabItem.tabItemView == aView)
					return tabItem;
			}
		}
	}
	return nil;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	_mouseDownPos = [[self superview] convertPoint:[anEvent locationInWindow] fromView:nil];
	if([anEvent clickCount] == 2)
	{
		if(OakTabItem* tabItem = [self tabItemForView:[self hitTest:[[self superview] convertPoint:[anEvent locationInWindow] fromView:nil]]])
		{
			if([_delegate respondsToSelector:@selector(tabBarView:didDoubleClickIndex:)])
				[_delegate tabBarView:self didDoubleClickIndex:[_tabItems indexOfObject:tabItem]];
		}
		else
		{
			if([_delegate respondsToSelector:@selector(tabBarViewDidDoubleClick:)])
				[_delegate tabBarViewDidDoubleClick:self];
		}
	}
}

- (void)mouseUp:(NSEvent*)anEvent
{
	NSPoint mouseCurrentPos = [[self superview] convertPoint:[anEvent locationInWindow] fromView:nil];
	if(SQ(fabs(_mouseDownPos.x - mouseCurrentPos.x)) + SQ(fabs(_mouseDownPos.y - mouseCurrentPos.y)) >= SQ(2.5))
		return; // mouse was moved
	[self trySelectTabForView:[self hitTest:mouseCurrentPos]];
}

- (void)otherMouseUp:(NSEvent*)anEvent
{
	[self _performCloseTab:[self hitTest:[[self superview] convertPoint:[anEvent locationInWindow] fromView:nil]]];
}

- (void)trySelectTabForView:(NSView*)aView
{
	if(OakTabItem* tabItem = [self tabItemForView:aView])
	{
		if([_delegate respondsToSelector:@selector(tabBarView:shouldSelectIndex:)])
		{
			NSUInteger tabIndex = [_tabItems indexOfObject:tabItem];
			if([_delegate tabBarView:self shouldSelectIndex:tabIndex])
				self.selectedTab = tabIndex;
		}
	}
}

- (void)setSelectedTabItem:(OakTabItem*)aTabItem
{
	if(_selectedTabItem == aTabItem)
		return;

	_selectedTabItem.tabItemView.selected = NO;
	_selectedTabItem = aTabItem;
	_selectedTabItem.tabItemView.selected = YES;

	if(aTabItem.tabItemView)
			[self updateCapImageViews];
	else	[self resizeTabItemViewFrames];

	NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);
}

// ===============
// = Drag’n’drop =
// ===============

- (void)mouseDragged:(NSEvent*)anEvent
{
	NSPoint mouseCurrentPos = [[self superview] convertPoint:[anEvent locationInWindow] fromView:nil];
	if(SQ(fabs(_mouseDownPos.x - mouseCurrentPos.x)) + SQ(fabs(_mouseDownPos.y - mouseCurrentPos.y)) < SQ(2.5))
		return;

	OakTabItem* tabItem = [self tabItemForView:[self hitTest:mouseCurrentPos]];
	if(!tabItem)
		return;

	OakTabItemView* view = tabItem.tabItemView;
	NSRect srcRect = [self convertRect:view.contentFrame fromView:view];

	NSImage* image = [[NSImage alloc] initWithSize:srcRect.size];
	[image lockFocusFlipped:[self isFlipped]];

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextTranslateCTM(context, -NSMinX(srcRect), -NSMinY(srcRect));
	[self displayRectIgnoringOpacity:srcRect inContext:[NSGraphicsContext currentContext]];
	[image unlockFocus];

	NSImage* dragImage = [[NSImage alloc] initWithSize:image.size];
	[dragImage lockFocus];
	[image drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:0.8];
	[dragImage unlockFocus];

	_draggedTabIndex = [_tabItems indexOfObject:tabItem];
	_draggedTabItem  = tabItem;
	[_tabItems removeObject:tabItem];
	tabItem.tabItemView.hidden = YES;

	NSPasteboard* pboard = [NSPasteboard pasteboardWithName:NSDragPboard];
	[tabItem writeToPasteboard:pboard];

	if([_delegate respondsToSelector:@selector(setupPasteboard:forTabAtIndex:)])
		[_delegate setupPasteboard:pboard forTabAtIndex:_draggedTabIndex];

	[self dragImage:dragImage at:srcRect.origin offset:NSZeroSize event:anEvent pasteboard:pboard source:self slideBack:YES];
}

- (NSDragOperation)draggingSession:(NSDraggingSession*)session sourceOperationMaskForDraggingContext:(NSDraggingContext)context
{
	return context == NSDraggingContextOutsideApplication ? (NSDragOperationCopy|NSDragOperationGeneric) : (NSDragOperationCopy|NSDragOperationMove|NSDragOperationLink);
}

- (void)draggingSession:(NSDraggingSession*)session endedAtPoint:(NSPoint)screenPoint operation:(NSDragOperation)operation
{
	if(_draggedTabItem)
	{
		if(operation != NSDragOperationMove)
			[_tabItems insertObject:_draggedTabItem atIndex:_draggedTabIndex];

		_draggedTabItem = nil;
		[self resizeTabItemViewFrames];
	}
}

// ========================
// = Dragging Destination =
// ========================

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
	return [self draggingUpdated:sender];
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)sender
{
	NSDragOperation operation = [sender draggingSourceOperationMask];
	operation = (operation & NSDragOperationMove) ?: (operation & NSDragOperationCopy);
	if(operation == NSDragOperationNone)
		return operation;

	NSUInteger desiredPos = 0;

	NSPoint pos = [self convertPoint:[sender draggingLocation] fromView:nil];
	for(NSUInteger i = 0; i < _tabItems.count; ++i)
	{
		OakTabItem* tabItem = _tabItems[i];
		if(tabItem != _preliminaryTabItem && tabItem.tabItemView && NSMidX(tabItem.targetFrame) <= pos.x)
			desiredPos = i+1;
	}

	NSUInteger currentPos = [_tabItems indexOfObject:_preliminaryTabItem];
	if(currentPos != desiredPos)
	{
		if(currentPos == NSNotFound)
				_preliminaryTabItem = _draggedTabItem ?: [OakTabItem tabItemFromPasteboard:[sender draggingPasteboard]];
		else	[_tabItems removeObjectAtIndex:currentPos];

		[_tabItems insertObject:_preliminaryTabItem atIndex:currentPos < desiredPos ? desiredPos-1 : desiredPos];
		[self animateLayoutUpdate];
	}

	return operation;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
	[self removeViewForTabItem:_preliminaryTabItem];
	[_tabItems removeObject:_preliminaryTabItem];
	_preliminaryTabItem = nil;

	_didCloseTabIndex = 0;
	[self animateLayoutUpdate];
}

- (BOOL)prepareForDragOperation:(id <NSDraggingInfo>)sender
{
	sender.animatesToDestination = YES;
	return YES;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
	[sender enumerateDraggingItemsWithOptions:0 forView:self classes:@[ [NSPasteboardItem class] ] searchOptions:nil usingBlock:^(NSDraggingItem* draggingItem, NSInteger idx, BOOL* stop){
		draggingItem.draggingFrame = [self convertRect:_preliminaryTabItem.tabItemView.contentFrame fromView:_preliminaryTabItem.tabItemView];
	}];

	NSDragOperation mask = [sender draggingSourceOperationMask];
	NSUInteger dropIndex = [_tabItems indexOfObject:_preliminaryTabItem];
	if(_draggedTabItem) // Local drag’n’drop
	{
		if(_draggedTabIndex < dropIndex)
			++dropIndex;
		else if(_draggedTabIndex == dropIndex)
			return YES;
	}

	return [_delegate performTabDropFromTabBar:[sender draggingSource] atIndex:dropIndex fromPasteboard:[sender draggingPasteboard] operation:(mask & NSDragOperationMove) ?: (mask & NSDragOperationCopy)];
}

- (void)concludeDragOperation:(id <NSDraggingInfo>)sender
{
	if(_preliminaryTabItem)
	{
		_preliminaryTabItem.tabItemView.hidden = NO;
		[_preliminaryTabItem.tabItemView updateTrackingAreas];
		_preliminaryTabItem = nil;
	}

	_draggedTabItem = nil;
	[self animateLayoutUpdate];
}

// ==========================
// = Private Action Methods =
// ==========================

- (NSInteger)tag
{
	return _tag;
}

- (void)_newTab:(id)sender
{
	if([_delegate respondsToSelector:@selector(tabBarViewDidDoubleClick:)])
		[_delegate tabBarViewDidDoubleClick:self];
}

- (void)_showOverflowMenu:(id)sender
{
	NSMenu* menu = [NSMenu new];
	for(NSUInteger i = 0; i < _tabItems.count; ++i)
	{
		OakTabItem* tabItem = _tabItems[i];
		if(tabItem.tabItemView && tabItem != _overflowTabItem)
			continue;

		NSMenuItem* item = [menu addItemWithTitle:tabItem.title action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@""];
		item.representedObject = tabItem;
		item.tag = i;

		if(NSString* path = tabItem.path)
		{
			item.image   = [OakFileIconImage fileIconImageWithPath:([path isEqualTo:@""] ? nil : path) isModified:tabItem.modified];
			item.toolTip = [path stringByAbbreviatingWithTildeInPath];
		}

		if(tabItem == _overflowTabItem)
			[item setState:NSOnState];
		else if(tabItem.modified)
			[item setModifiedState:YES];
	}
	[menu popUpMenuPositioningItem:nil atLocation:NSMakePoint(NSWidth([sender frame]), 0) inView:sender];
}

- (void)_performCloseTab:(id)sender
{
	if(OakTabItem* tabItem = [self tabItemForView:sender])
	{
		_tag = [_tabItems indexOfObject:tabItem]; // performCloseTab: asks for [sender tag]

		BOOL closeOther = OakIsAlternateKeyOrMouseEvent();
		if(_isMouseInside && ([[NSApp currentEvent] type] == NSLeftMouseUp || [[NSApp currentEvent] type] == NSOtherMouseUp) && !closeOther && tabItem != _overflowTabItem)
		{
			_didCloseTabIndex = _tag;
			_didCloseTabFrame = tabItem.targetFrame;
		}
		else
		{
			_didCloseTabIndex = 0;
		}

		if(closeOther && [_delegate respondsToSelector:@selector(performCloseOtherTabs:)])
			[_delegate performCloseOtherTabs:self];
		else if([_delegate respondsToSelector:@selector(performCloseTab:)])
			[_delegate performCloseTab:self];
	}
}

// =======
// = API =
// =======

- (void)reloadData
{
	NSUInteger newCount = [_dataSource numberOfRowsInTabBarView:self];
	NSUInteger oldCount = _tabItems.count;

	if(newCount != oldCount && _overflowTabItem)
	{
		_overflowTabItem.tabItemView.showOverflowButton = NO;
		_overflowTabItem = nil;
	}

	NSMutableDictionary* oldTabs = [NSMutableDictionary dictionary];
	for(OakTabItem* tabItem in _tabItems)
		oldTabs[tabItem.identifier] = tabItem;

	NSMutableArray* newTabs = [NSMutableArray array];
	for(NSUInteger i = 0; i < newCount; ++i)
	{
		NSString* title      = [_dataSource tabBarView:self titleForIndex:i];
		NSString* path       = [_dataSource tabBarView:self pathForIndex:i];
		NSString* identifier = [_dataSource tabBarView:self identifierForIndex:i];
		BOOL modified        = [_dataSource tabBarView:self isEditedAtIndex:i];

		OakTabItem* tabItem = oldTabs[identifier];
		if(!tabItem)
		{
			tabItem = [OakTabItem tabItemWithTitle:title path:path identifier:identifier modified:modified];
		}
		else
		{
			tabItem.title    = [_dataSource tabBarView:self titleForIndex:i];
			tabItem.path     = [_dataSource tabBarView:self pathForIndex:i];
			tabItem.modified = [_dataSource tabBarView:self isEditedAtIndex:i];

			[oldTabs removeObjectForKey:identifier];
		}
		[newTabs addObject:tabItem];
	}

	for(NSString* key in oldTabs)
	{
		OakTabItem* tabItem = oldTabs[key];
		if(tabItem == _selectedTabItem)
			_selectedTabItem = nil;
		[self removeViewForTabItem:tabItem];
	}

	_tabItems = newTabs;

	if(!_expanded && _tabItems.count > 1)
	{
		self.expanded = YES;
	}
	else
	{
		if(oldCount && newCount && oldCount != newCount)
				[self animateLayoutUpdate];
		else	[self resizeTabItemViewFrames];
	}

	if(!_selectedTabItem && _tabItems.count)
		self.selectedTabItem = _tabItems.firstObject;
}

- (void)setSelectedTab:(NSUInteger)anIndex
{
	self.selectedTabItem = anIndex < _tabItems.count ? _tabItems[anIndex] : nil;
}

- (void)performClose:(id)sender
{
	_tag = [_tabItems indexOfObject:_selectedTabItem]; // performCloseTab: asks for [sender tag]
	if([_delegate respondsToSelector:@selector(performCloseTab:)])
		[_delegate performCloseTab:self];
}

- (NSMenu*)menuForView:(NSView*)aView
{
	if([_delegate respondsToSelector:@selector(menuForTabBarView:)])
	{
		if(OakTabItem* tabItem = [self tabItemForView:aView])
		{
			_tag = [_tabItems indexOfObject:tabItem];
			return [_delegate menuForTabBarView:self];
		}
	}
	return nil;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	NSMenu* res = [self menuForView:[self hitTest:[[self superview] convertPoint:[anEvent locationInWindow] fromView:nil]]];
	return res ?: [super menuForEvent:anEvent];
}

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		// generic
		NSAccessibilityRoleAttribute,
		// tab group
		NSAccessibilityChildrenAttribute,
		NSAccessibilityContentsAttribute,
		NSAccessibilityFocusedAttribute,
		NSAccessibilityTabsAttribute,
		NSAccessibilityValueAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	// generic attributes
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityTabGroupRole;
	// tab group attributes
	else if([attribute isEqualToString:NSAccessibilityFocusedAttribute])
		return @NO;
	else if([attribute isEqualToString:NSAccessibilityChildrenAttribute] || [attribute isEqualToString:NSAccessibilityContentsAttribute] || [attribute isEqualToString:NSAccessibilityTabsAttribute])
	{
		NSMutableArray* array = [NSMutableArray array];
		for(OakTabItem* tabItem in _tabItems)
		{
			if(tabItem.tabItemView)
				[array addObject:tabItem.tabItemView];
		}
		return array;
	}
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		return _selectedTabItem.tabItemView;
	else
		return [super accessibilityAttributeValue:attribute];
}
@end
