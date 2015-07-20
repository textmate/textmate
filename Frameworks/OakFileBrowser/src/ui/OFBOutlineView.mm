#import "OFBOutlineView.h"
#import "OFBPathInfoCell.h"
#import <OakAppKit/NSEvent Additions.h>
#import <ns/ns.h>
#import <text/utf8.h>
#import <oak/debug.h>
#import <oak/oak.h>

@protocol FSDataSourceDragSource
- (void)outlineView:(NSOutlineView*)anOutlineView draggedItems:(NSArray*)someItems endedWithOperation:(NSDragOperation)aDragOperation;
@end

@interface OFBOutlineView ()
{
	OBJC_WATCH_LEAKS(OFBOutlineView);

	NSRect mouseHoverRect;

	NSTableViewSelectionHighlightStyle defaultSelectionHighlightStyle;
	NSTableViewDraggingDestinationFeedbackStyle defaultDraggingDestinationFeedbackStyle;
	CGFloat defaultRowHeight;
	NSSize defaultIntercellSpacing;
	NSColor* defaultBackgroundColor;
}
@property (nonatomic) NSArray* draggedItems;

- (void)performDoubleClick:(id)sender;
@end

@implementation OFBOutlineView
- (void)expandItem:(id)anItem expandChildren:(BOOL)flag
{
	BOOL oldFlag = std::exchange(_recursiveRequest, flag);
	[super expandItem:anItem expandChildren:flag];
	_recursiveRequest = oldFlag;
}

- (void)collapseItem:(id)anItem collapseChildren:(BOOL)flag
{
	BOOL oldFlag = std::exchange(_recursiveRequest, flag);
	[super collapseItem:anItem collapseChildren:flag];
	_recursiveRequest = oldFlag;
}

- (void)setRenderAsSourceList:(BOOL)value
{
	if(_renderAsSourceList == value)
		return;

	if(_renderAsSourceList = value)
	{
		defaultSelectionHighlightStyle          = [self selectionHighlightStyle];
		defaultDraggingDestinationFeedbackStyle = [self draggingDestinationFeedbackStyle];
		defaultRowHeight                        = [self rowHeight];
		defaultIntercellSpacing                 = [self intercellSpacing];
		defaultBackgroundColor                  = [self backgroundColor];

		[self setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleSourceList];
		[self setRowHeight:16];
		[self setIntercellSpacing:NSMakeSize(3, 2)];
	}
	else
	{
		[self setSelectionHighlightStyle:defaultSelectionHighlightStyle];
		[self setRowHeight:defaultRowHeight];
		[self setIntercellSpacing:defaultIntercellSpacing];

		// setting selectionHighlightStyle to NSTableViewSelectionHighlightStyleSourceList
		// will also change these properties and won't automaticlly be restored
		[self setBackgroundColor:defaultBackgroundColor];
		[self setDraggingDestinationFeedbackStyle:defaultDraggingDestinationFeedbackStyle];
	}
}

/**
 * Fixes the indentation of the row with the given index.
 *
 * When the source list style is used, the second level won't be indented.
 * The reason for this is most likely due to the first level intended to
 * be used as a "group row".
 * But since group rows are not used we need to fix the indentation.
 */
- (NSRect)increaseIndentationAtRow:(NSInteger)row rect:(NSRect)rect adjustWidth:(BOOL)adjustWidth
{
	if(self.renderAsSourceList && [self levelForRow:row] != 0)
	{
		CGFloat indentation = [self indentationPerLevel];
		rect.origin.x += indentation;
		if(adjustWidth)
			rect.size.width -= indentation;
	}
	return rect;
}

// Override to fix indentation
- (NSRect)frameOfOutlineCellAtRow:(NSInteger)row
{
	NSRect rect = [super frameOfOutlineCellAtRow:row];
	return [self increaseIndentationAtRow:row rect:rect adjustWidth:NO];
}

// Override to fix indentation
- (NSRect)frameOfCellAtColumn:(NSInteger)column row:(NSInteger)row
{
	NSRect rect = [super frameOfCellAtColumn:column row:row];
	return [self increaseIndentationAtRow:row rect:rect adjustWidth:YES];
}

- (void)showContextMenu:(id)sender
{
	if(NSMenu* menu = [self.menuDelegate menuForOutlineView:self])
	{
		NSInteger row = [self selectedRow] != -1 ? [self selectedRow] : 0;
		NSRect rect = [self convertRect:[self rectOfRow:row] toView:nil];
		NSPoint pos = rect.origin;
		pos.x += 10;

		NSWindow* win = [self window];
		NSEvent* anEvent = [NSApp currentEvent];
		NSEvent* fakeEvent = [NSEvent
			mouseEventWithType:NSLeftMouseDown
			location:pos
			modifierFlags:0
			timestamp:[anEvent timestamp]
			windowNumber:[win windowNumber]
			context:[anEvent context]
			eventNumber:0
			clickCount:1
			pressure:1];

		[NSMenu popUpContextMenu:menu withEvent:fakeEvent forView:self];
	}
}

- (NSMenu*)menuForEvent:(NSEvent*)theEvent
{
	if(!self.menuDelegate)
		return [super menuForEvent:theEvent];

	NSInteger row = [self rowAtPoint:[self convertPoint:[theEvent locationInWindow] fromView:nil]];
	if(row != -1 && [self.delegate respondsToSelector:@selector(outlineView:shouldSelectItem:)] && ![self.delegate outlineView:self shouldSelectItem:[self itemAtRow:row]])
		row = -1;
	if(row != -1 && ![self.selectedRowIndexes containsIndex:row])
		[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	return [self.menuDelegate menuForOutlineView:self];
}

// =============================
// = Accepting First Responder =
// =============================

- (BOOL)shouldActivate
{
	NSEvent* event = [NSApp currentEvent];
	if([event type] != NSLeftMouseDown)
		return YES;

	id firstResponder = [[self window] firstResponder];
	if(([firstResponder respondsToSelector:@selector(delegate)] && [(NSText*)firstResponder delegate] == self) || firstResponder == self)
		return YES;

	if([event clickCount] != 1 || (event.modifierFlags & NSCommandKeyMask))
		return NO;

	NSInteger row = [self rowAtPoint:[self convertPoint:[event locationInWindow] fromView:nil]];
	NSUInteger hit = row == -1 ? 0 : [[self preparedCellAtColumn:0 row:row] hitTestForEvent:event inRect:[self frameOfCellAtColumn:0 row:row] ofView:self];
	if(hit & (OFBPathInfoCellHitOpenItem | OFBPathInfoCellHitRevealItem | NSCellHitTrackableArea))
		return NO;

	NSPoint p = [self convertPoint:[event locationInWindow] fromView:nil];
	return [self isRowSelected:[self rowAtPoint:p]];
}

- (BOOL)acceptsFirstResponder
{
	return [self shouldActivate] && [super acceptsFirstResponder];
}

// ===========================
// = Key Down Implementation =
// ===========================

- (void)performDoubleClick:(id)sender
{
	[NSApp sendAction:[self doubleAction] to:[self target] from:self];
}

- (void)performEditSelectedRow:(id)sender
{
	if([self numberOfSelectedRows] == 1)
	{
		[[self window] makeKeyWindow];
		[self editColumn:0 row:[self selectedRow] withEvent:nil select:YES];
	}
}

- (void)keyDown:(NSEvent*)theEvent
{
	static struct key_action_t { std::string key; SEL action; } const KeyActions[] =
	{
		{ "@" + utf8::to_s(NSLeftArrowFunctionKey),  @selector(goBack:)                   },
		{ "@" + utf8::to_s(NSRightArrowFunctionKey), @selector(goForward:)                },
		{ "@" + utf8::to_s(NSDownArrowFunctionKey),  @selector(performDoubleClick:)       },
		{ "@o",                                      @selector(performDoubleClick:)       },
		{ "@d",                                      @selector(duplicateSelectedEntries:) },
		{ "@G",                                      @selector(orderFrontGoToFolder:)     },
		{ " ",                                       @selector(toggleQuickLookPreview:)   },
		{ "~\uF705",                                 @selector(showContextMenu:)          },
	};

	std::string const key = to_s(theEvent);
	for(auto const& keyAction : KeyActions)
	{
		if(key == keyAction.key)
			return (void)[NSApp sendAction:keyAction.action to:nil from:self];
	}

	[super keyDown:theEvent];
}

// ===============
// = Drag’n’drop =
// ===============

- (NSImage*)dragImageForRowsWithIndexes:(NSIndexSet*)anIndexSet tableColumns:(NSArray*)anArray event:(NSEvent*)anEvent offset:(NSPointPointer)aPointPointer
{
	NSMutableArray* items = [NSMutableArray array];
	for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
	{
		if(id item = [self itemAtRow:index])
			[items addObject:item];
	}
	self.draggedItems = items;
	return [super dragImageForRowsWithIndexes:anIndexSet tableColumns:anArray event:anEvent offset:aPointPointer];
}

- (void)draggedImage:(NSImage*)anImage endedAt:(NSPoint)aPoint operation:(NSDragOperation)aDragOperation
{
	if(self.draggedItems && [self.dataSource respondsToSelector:@selector(outlineView:draggedItems:endedWithOperation:)])
		[(id <FSDataSourceDragSource>)self.dataSource outlineView:self draggedItems:self.draggedItems endedWithOperation:aDragOperation];
	self.draggedItems = nil;

	if([NSOutlineView respondsToSelector:@selector(draggedImage:endedAt:operation:)])
		[super draggedImage:anImage endedAt:aPoint operation:aDragOperation];
}

// ========================
// = Field Editor Support =
// ========================

- (void)cancelOperation:(id)sender
{
	if([self abortEditing])
		[[self window] makeFirstResponder:self]; // Restore focus
}

// ==================
// = Mouse Tracking =
// ==================

- (void)cursorUpdate:(NSEvent*)event
{
	if(NSMouseInRect([self convertPoint:[event locationInWindow] fromView:nil], [[event trackingArea] rect], self.isFlipped))
			[[NSCursor pointingHandCursor] set];
	else	[super cursorUpdate:event];
}

- (void)updateTrackingAreas
{
	for(NSTrackingArea* trackingArea in self.trackingAreas)
		[self removeTrackingArea:trackingArea];

	[super updateTrackingAreas];

	NSRange rows = [self rowsInRect:[self visibleRect]];
	for(NSUInteger row = rows.location; row < NSMaxRange(rows); ++row)
	{
		NSRect cellFrame  = [self frameOfCellAtColumn:0 row:row];
		NSRect imageFrame = [[[[self tableColumns] lastObject] dataCell] imageFrameWithFrame:cellFrame inControlView:self];
		imageFrame.origin.y    = cellFrame.origin.y;
		imageFrame.size.height = cellFrame.size.height + self.intercellSpacing.height;
		[self addTrackingArea:[[NSTrackingArea alloc] initWithRect:imageFrame options:NSTrackingCursorUpdate|NSTrackingActiveInKeyWindow owner:self userInfo:NULL]];
	}

	[self addTrackingArea:[[NSTrackingArea alloc] initWithRect:[self visibleRect] options:NSTrackingMouseEnteredAndExited|NSTrackingMouseMoved|NSTrackingActiveInKeyWindow owner:self userInfo:NULL]];
}

// ===============
// = Mouse Moved =
// ===============

- (void)mouseMoved:(NSEvent*)theEvent
{
	NSRect newHoverRect = NSZeroRect;

	NSPoint mousePos = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	NSInteger row = [self rowAtPoint:mousePos];
	if(row != -1)
	{
		OFBPathInfoCell* cell = (OFBPathInfoCell*)[self preparedCellAtColumn:0 row:row];
		NSRect closeButtonRect = [cell closeButtonRectInFrame:[self frameOfCellAtColumn:0 row:row]];
		if(NSMouseInRect(mousePos, closeButtonRect, self.isFlipped))
			newHoverRect = closeButtonRect;
	}

	if(!NSEqualRects(mouseHoverRect, newHoverRect))
	{
		[self setNeedsDisplayInRect:mouseHoverRect];
		[self setNeedsDisplayInRect:newHoverRect];
		mouseHoverRect = newHoverRect;
	}
}

- (void)mouseExited:(NSEvent*)anEvent
{
	if(!NSIsEmptyRect(mouseHoverRect))
	{
		[self setNeedsDisplayInRect:mouseHoverRect];
		mouseHoverRect = NSZeroRect;
	}
	[super mouseExited:anEvent];
}
@end
