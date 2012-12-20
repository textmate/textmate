#import "OFBOutlineView.h"
#import "OFBPathInfoCell.h"
#import <OakAppKit/NSEvent Additions.h>
#import <ns/ns.h>
#import <text/utf8.h>
#import <oak/debug.h>
#import <oak/oak.h>

@protocol OFBOutlineViewMenuDelegate
- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView;
@end

@protocol FSDataSourceDragSource
- (void)outlineView:(NSOutlineView*)anOutlineView draggedItems:(NSArray*)someItems endedWithOperation:(NSDragOperation)aDragOperation;
@end

@interface OFBOutlineView ()
@property (nonatomic, retain) NSIndexSet* draggedRows;

- (void)performDoubleClick:(id)sender;

- (BOOL)isPointInImage:(NSPoint)point;
- (BOOL)isPointInText:(NSPoint)aPoint;
- (BOOL)isPointInCloseButton:(NSPoint)aPoint;
@end

@implementation OFBOutlineView
@synthesize menuDelegate, draggedRows;

- (void)dealloc
{
	self.draggedRows = nil;
	[super dealloc];
}

- (void)showContextMenu:(id)sender
{
	if(NSMenu* menu = [menuDelegate menuForOutlineView:self])
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
	if(!menuDelegate)
		return [super menuForEvent:theEvent];

	int row = [self rowAtPoint:[self convertPoint:[theEvent locationInWindow] fromView:nil]];
	if(row == -1)
		[self selectRowIndexes:[NSIndexSet indexSet] byExtendingSelection:NO];
	else if(![self.selectedRowIndexes containsIndex:row])
		[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	return [menuDelegate menuForOutlineView:self];
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
	[[self target] performSelector:[self doubleAction] withObject:self];
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
		{ "@C",                                      @selector(goToComputer:)             },
		{ "@H",                                      @selector(goToHome:)                 },
		{ "@D",                                      @selector(goToDesktop:)              },
		{ "@[",                                      @selector(goBack:)                   },
		{ "@]",                                      @selector(goForward:)                },
		{ "@" + utf8::to_s(NSLeftArrowFunctionKey),  @selector(goBack:)                   },
		{ "@" + utf8::to_s(NSRightArrowFunctionKey), @selector(goForward:)                },
		{ utf8::to_s(NSCarriageReturnCharacter),     @selector(performEditSelectedRow:)   },
		{ utf8::to_s(NSEnterCharacter),              @selector(performEditSelectedRow:)   },
		{ "@" + utf8::to_s(NSDownArrowFunctionKey),  @selector(performDoubleClick:)       },
		{ "@o",                                      @selector(performDoubleClick:)       },
		{ "@N",                                      @selector(newFolderInSelectedFolder:)},
		{ "@d",                                      @selector(duplicateSelectedEntries:) },
		{ "@G",                                      @selector(orderFrontGoToFolder:)     },
		{ " ",                                       @selector(quickLookSelectedEntries:) },
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
	if([self abortEditing]) {
		[[self window] makeFirstResponder:self]; // Restore focus
    }
}

- (void)editColumn:(NSInteger)columnIndex row:(NSInteger)rowIndex withEvent:(NSEvent *)theEvent select:(BOOL)flag
{
    [super editColumn:columnIndex row:rowIndex withEvent:theEvent select:flag];
    
    //Notify the OakFileBrowser controller that an item is about
    //to be renamed, for undo logic.
    id selectedItem = [self itemAtRow:[self selectedRow]];
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"OFBOutlineViewRenameActionQueued"
     object:selectedItem];
}

- (void)textDidEndEditing:(NSNotification *)aNotification
{
	int movement = [[[aNotification userInfo] objectForKey:@"NSTextMovement"] intValue];
	[super textDidEndEditing:aNotification];
	NSInteger row = [self selectedRow];
    
    //Notify the OakFileBrowser controller that an item was
    //successfully renamed, for undo logic.
    id selectedItem = [self itemAtRow:row];
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"OFBOutlineViewRenameActionFinalized"
     object:selectedItem];
    
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
		NSTrackingArea* cursorRect = [[NSTrackingArea alloc] initWithRect:imageFrame options:NSTrackingCursorUpdate|NSTrackingActiveInKeyWindow owner:self userInfo:NULL];
		[self addTrackingArea:cursorRect];
		[cursorRect release];
	}

	NSTrackingArea* trackingArea = [[NSTrackingArea alloc] initWithRect:[self visibleRect] options:NSTrackingMouseMoved|NSTrackingActiveInKeyWindow owner:self userInfo:NULL];
	[self addTrackingArea:trackingArea];
	[trackingArea release];
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
