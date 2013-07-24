#import "OFBOutlineView.h"
#import "OFBPathInfoCell.h"
#import <OakAppKit/NSEvent Additions.h>
#import <ns/ns.h>
#import <text/utf8.h>
#import <oak/debug.h>
#import <oak/oak.h>
#import <settings/settings.h>

@protocol FSDataSourceDragSource
- (void)outlineView:(NSOutlineView*)anOutlineView draggedItems:(NSArray*)someItems endedWithOperation:(NSDragOperation)aDragOperation;
@end

@interface OFBOutlineView ()
{
	NSTableViewSelectionHighlightStyle defaultSelectionHighlightStyle;
	NSTableViewDraggingDestinationFeedbackStyle defaultDraggingDestinationFeedbackStyle;
	CGFloat defaultRowHeight;
	NSSize defaultIntercellSpacing;
	NSColor* defaultBackgroundColor;
}
@property (nonatomic, retain) NSIndexSet* draggedRows;

- (void)performDoubleClick:(id)sender;

- (BOOL)isPointInImage:(NSPoint)point;
- (BOOL)isPointInText:(NSPoint)aPoint;
- (BOOL)isPointInCloseButton:(NSPoint)aPoint;

/**
 * Fixes the indentation of the row with the given index.
 *
 * When the source list style is used, when the setting "fileBrowserSourceList"
 * is enabled, the second level won't be indented. The reason for this is most
 * likely due to the first level is intended to be used as a "group row".
 * But since group rows are not used we need to fix the indentation.
 */
- (NSRect)fixIndentationAtRow:(NSInteger)row rect:(NSRect)rect decreaseWidth:(BOOL)decreaseWidth;	
@end

@implementation OFBOutlineView
@synthesize draggedRows;

- (id)initWithFrame:(NSRect)frameRect
{
	if((self = [super initWithFrame:frameRect]))
	{
		defaultSelectionHighlightStyle = [self selectionHighlightStyle];
		defaultDraggingDestinationFeedbackStyle = [self draggingDestinationFeedbackStyle];
		defaultRowHeight = [self rowHeight];
		defaultIntercellSpacing = [self intercellSpacing];
		defaultBackgroundColor = [self backgroundColor];
	}

	return self;
}

- (void)setRenderAsSourceList:(BOOL)value
{
	_renderAsSourceList = value;

	if(_renderAsSourceList)
	{
		[self setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleSourceList];
		[self setRowHeight:16];
		[self setIntercellSpacing:NSMakeSize(3.0, 2.0)];
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

	int row = [self rowAtPoint:[self convertPoint:[theEvent locationInWindow] fromView:nil]];
	if(row == -1)
		[self selectRowIndexes:[NSIndexSet indexSet] byExtendingSelection:NO];
	else if(![self.selectedRowIndexes containsIndex:row])
		[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	return [self.menuDelegate menuForOutlineView:self];
}

// Override to fix indentation
- (NSRect)frameOfOutlineCellAtRow:(NSInteger)row
{
	auto rect = [super frameOfOutlineCellAtRow:row];
	return [self fixIndentationAtRow:row rect:rect decreaseWidth:NO];
}

// Override to fix indentation
- (NSRect)frameOfCellAtColumn:(NSInteger)column row:(NSInteger)row
{
	auto rect = [super frameOfCellAtColumn:column row:row];
	return [self fixIndentationAtRow:row rect:rect decreaseWidth:YES];
}

- (NSRect)fixIndentationAtRow:(NSInteger)row rect:(NSRect)rect decreaseWidth:(BOOL)decreaseWidth
{
	auto fixIndentation = [self selectionHighlightStyle] == NSTableViewSelectionHighlightStyleSourceList;

	if (fixIndentation && [self levelForRow:row] != 0)
	{
		auto indentation = [self indentationPerLevel];
		rect.origin.x += indentation;

		if (decreaseWidth)
			rect.size.width -= indentation;
	}

	return rect;
}

// =============================
// = Accepting First Responder =
// =============================

- (BOOL)shouldActivate
{
	NSEvent* event = [NSApp currentEvent];
	BOOL res = [event type] != NSLeftMouseDown || ([event modifierFlags] & (NSShiftKeyMask | NSControlKeyMask | NSAlternateKeyMask));

	id firstResponder = [[self window] firstResponder];
	res = res || ([firstResponder respondsToSelector:@selector(delegate)] && [firstResponder delegate] == self);
	res = res || fieldEditorWasUp;
	if(res)
		return YES;

	NSPoint p = [self convertPoint:[event locationInWindow] fromView:nil];
	if([self isPointInImage:p] || [self isPointInCloseButton:p])
		return NO; // Don’t activate when clicking an image to open a document

	return [self isRowSelected:[self rowAtPoint:p]] && (event.modifierFlags & NSCommandKeyMask) == 0;
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
		[self editColumn:0 row:[self selectedRow] withEvent:nil select:YES];
}

- (void)keyDown:(NSEvent*)theEvent
{
	static struct key_action_t { std::string key; SEL action; } const KeyActions[] =
	{
		{ "@" + utf8::to_s(NSLeftArrowFunctionKey),  @selector(goBack:)                   },
		{ "@" + utf8::to_s(NSRightArrowFunctionKey), @selector(goForward:)                },
		{ utf8::to_s(NSCarriageReturnCharacter),     @selector(performEditSelectedRow:)   },
		{ utf8::to_s(NSEnterCharacter),              @selector(performEditSelectedRow:)   },
		{ "@" + utf8::to_s(NSDownArrowFunctionKey),  @selector(performDoubleClick:)       },
		{ "@o",                                      @selector(performDoubleClick:)       },
		{ "@d",                                      @selector(duplicateSelectedEntries:) },
		{ "@G",                                      @selector(orderFrontGoToFolder:)     },
		{ " ",                                       @selector(toggleQuickLookPreview:)   },
		{ "~\uF705",                                 @selector(showContextMenu:)          },
	};

	std::string const key = to_s(theEvent);
	for(size_t i = 0; i < sizeofA(KeyActions); ++i)
	{
		if(key == KeyActions[i].key)
			return (void)[NSApp sendAction:KeyActions[i].action to:nil from:self];
	}

	[super keyDown:theEvent];
}

// ===============
// = Drag’n’drop =
// ===============

- (NSImage*)dragImageForRowsWithIndexes:(NSIndexSet*)anIndexSet tableColumns:(NSArray*)anArray event:(NSEvent*)anEvent offset:(NSPointPointer)aPointPointer
{
	self.draggedRows = anIndexSet;
	return [super dragImageForRowsWithIndexes:anIndexSet tableColumns:anArray event:anEvent offset:aPointPointer];
}

- (void)draggedImage:(NSImage*)anImage endedAt:(NSPoint)aPoint operation:(NSDragOperation)aDragOperation
{
	if(draggedRows && [self.dataSource respondsToSelector:@selector(outlineView:draggedItems:endedWithOperation:)])
	{
		NSMutableArray* items = [NSMutableArray array];
		for(NSUInteger index = [draggedRows firstIndex]; index != NSNotFound; index = [draggedRows indexGreaterThanIndex:index])
			[items addObject:[self itemAtRow:index]];
		[(id <FSDataSourceDragSource>)self.dataSource outlineView:self draggedItems:items endedWithOperation:aDragOperation];
	}
	self.draggedRows = nil;

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

- (void)textDidEndEditing:(NSNotification *)aNotification
{
	int movement = [[[aNotification userInfo] objectForKey:@"NSTextMovement"] intValue];
	[super textDidEndEditing:aNotification];
	NSInteger row = [self selectedRow];
	if(movement == NSReturnTextMovement)
	{
		[self abortEditing];
		[[self window] makeFirstResponder:self];
	}
	// else if(movement == NSTabTextMovement)
	// {
	// 	[self abortEditing];
	// 	++row;
	// 	if(row == [self numberOfRows])
	// 		row = 0;
	// 	[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	// 	[self editColumn:0 row:row withEvent:nil select:YES];
	// }
	else if(movement == NSBacktabTextMovement)
	{
		[self abortEditing];
		if(row == 0)
			row = [self numberOfRows];
		--row;
		[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[self editColumn:0 row:row withEvent:nil select:YES];
	}

	fieldEditorWasUp = YES;
	[self performSelector:@selector(setFieldEditorWasUp:) withObject:0 afterDelay:0.0];
}

- (void)setFieldEditorWasUp:(id)sender
{
	fieldEditorWasUp = NO;
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
	if(!NSEqualRects(mouseHoverRect,  NSZeroRect))
	{
		[self setNeedsDisplayInRect:mouseHoverRect];
		mouseHoverRect =  NSZeroRect;
	}
	[super mouseExited:anEvent];
}

// ========================
// = Hit Testing The Cell =
// ========================

- (NSUInteger)hitTestForPoint:(NSPoint)aPoint
{
	NSInteger row = [self rowAtPoint:aPoint];
	return row == -1 ? 0 : [[self preparedCellAtColumn:0 row:row] hitTestForEvent:[NSApp currentEvent] inRect:[self frameOfCellAtColumn:0 row:row] ofView:self];
}

- (BOOL)isPointInImage:(NSPoint)aPoint       { return ([self hitTestForPoint:aPoint] & OakImageAndTextCellHitImage)   == OakImageAndTextCellHitImage;   }
- (BOOL)isPointInText:(NSPoint)aPoint        { return ([self hitTestForPoint:aPoint] & OakImageAndTextCellHitText)    == OakImageAndTextCellHitText;    }
- (BOOL)isPointInCloseButton:(NSPoint)aPoint { return ([self hitTestForPoint:aPoint] & OFBPathInfoCellHitCloseButton) == OFBPathInfoCellHitCloseButton; }
@end
