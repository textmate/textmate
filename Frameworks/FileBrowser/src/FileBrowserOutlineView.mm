#import "FileBrowserOutlineView.h"
#import <text/utf8.h>
#import <ns/ns.h>

@implementation FileBrowserOutlineView
- (void)expandItem:(id)someItem expandChildren:(BOOL)flag
{
	if([self.delegate respondsToSelector:@selector(outlineView:willExpandItem:expandChildren:)])
		[(id <FileBrowserOutlineViewDelegate>)self.delegate outlineView:self willExpandItem:someItem expandChildren:flag];

	[super expandItem:someItem expandChildren:flag];

	if([self.delegate respondsToSelector:@selector(outlineView:didExpandItem:expandChildren:)])
		[(id <FileBrowserOutlineViewDelegate>)self.delegate outlineView:self didExpandItem:someItem expandChildren:flag];
}

- (void)collapseItem:(id)someItem collapseChildren:(BOOL)flag
{
	if([self.delegate respondsToSelector:@selector(outlineView:willCollapseItem:collapseChildren:)])
		[(id <FileBrowserOutlineViewDelegate>)self.delegate outlineView:self willCollapseItem:someItem collapseChildren:flag];

	[super collapseItem:someItem collapseChildren:flag];

	if([self.delegate respondsToSelector:@selector(outlineView:didCollapseItem:collapseChildren:)])
		[(id <FileBrowserOutlineViewDelegate>)self.delegate outlineView:self didCollapseItem:someItem collapseChildren:flag];
}

- (void)showContextMenu:(id)sender
{
	if(NSMenu* menu = self.menu)
	{
		NSRect rect = [self convertRect:[self rectOfRow:self.selectedRow != -1 ? self.selectedRow : 0] toView:nil];
		NSEvent* fakeEvent = [NSEvent
			mouseEventWithType: NSEventTypeLeftMouseDown
			          location: NSMakePoint(NSMinX(rect) + 10, NSMinY(rect))
			     modifierFlags: 0
			         timestamp: NSApp.currentEvent.timestamp
			      windowNumber: self.window.windowNumber
			           context: nil
			       eventNumber: 0
			        clickCount: 1
			          pressure: 1
		];

		[NSMenu popUpContextMenu:menu withEvent:fakeEvent forView:self];
	}
}

- (void)performDoubleClick:(id)sender
{
	[NSApp sendAction:self.doubleAction to:self.target from:self];
}

- (void)performEditSelectedRow:(id)sender
{
	NSInteger row = self.clickedRow == -1 && self.numberOfSelectedRows == 1 ? self.selectedRow : self.clickedRow;
	if(row != -1)
	{
		[self.window makeKeyWindow];
		[self editColumn:0 row:row withEvent:nil select:YES];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	if(self.window.firstResponder == self)
	{
		static struct key_action_t { std::string key; SEL action; } const KeyActions[] =
		{
			{ "@" + utf8::to_s(NSLeftArrowFunctionKey),  @selector(goBack:)                   },
			{ "@" + utf8::to_s(NSRightArrowFunctionKey), @selector(goForward:)                },
			{ "@" + utf8::to_s(NSDownArrowFunctionKey),  @selector(performDoubleClick:)       },
			{ "~" + utf8::to_s(NSF2FunctionKey),         @selector(showContextMenu:)          },
			{ "@o",                                      @selector(performDoubleClick:)       },
			{ "~@c",                                     @selector(copyAsPathname:)           },
			{ "@d",                                      @selector(duplicateSelectedEntries:) },
			{ "@G",                                      @selector(orderFrontGoToFolder:)     },
			{ " ",                                       @selector(toggleQuickLookPreview:)   },
		};

		std::string const key = to_s(anEvent);
		for(auto const& keyAction : KeyActions)
		{
			if(key == keyAction.key)
				return [NSApp sendAction:keyAction.action to:nil from:self];
		}
	}
	return [super performKeyEquivalent:anEvent];
}

- (void)draggingSession:(NSDraggingSession*)session endedAtPoint:(NSPoint)screenPoint operation:(NSDragOperation)operation
{
	if(operation == NSDragOperationDelete && [self.delegate respondsToSelector:@selector(outlineView:didTrashURLs:)])
	{
		NSMutableArray* urls = [NSMutableArray array];
		NSPasteboard* pboard = session.draggingPasteboard;
		for(NSString* path in [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : @[ ])
			[urls addObject:[NSURL fileURLWithPath:path]];
		[(id <FileBrowserOutlineViewDelegate>)self.delegate outlineView:self didTrashURLs:urls];
	}
	[super draggingSession:session endedAtPoint:screenPoint operation:operation];
}
@end
