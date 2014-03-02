#import "OakControl Private.h"
#import "NSView Additions.h"
#import "NSColor Additions.h"
#import <oak/oak.h>

// The lineBreakMode parameter is here to work around a crash in CoreText <rdar://6940427> — fixed in 10.6
static NSAttributedString* AttributedStringWithOptions (NSString* string, uint32_t options, NSLineBreakMode lineBreakMode = NSLineBreakByTruncatingTail)
{
	NSMutableParagraphStyle* paragraph = [NSMutableParagraphStyle new];
	[paragraph setLineBreakMode:lineBreakMode];

	NSDictionary* attr = @{
		NSParagraphStyleAttributeName : paragraph,
		NSFontAttributeName           : [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]]
	};

	return [[NSAttributedString alloc] initWithString:string attributes:attr];
}

double WidthOfText (NSString* string)
{
	double width = 0;
	if(CFAttributedStringRef attrStr = (CFAttributedStringRef)CFBridgingRetain(AttributedStringWithOptions(string, 0)))
	{
		if(CTLineRef line = CTLineCreateWithAttributedString(attrStr))
		{
			width = CTLineGetTypographicBounds(line, NULL, NULL, NULL);
			CFRelease(line);
		}
		CFRelease(attrStr);
	}
	return ceil(width);
}

static void DrawTextWithOptions (NSString* string, NSRect bounds, uint32_t textOptions)
{
	[NSGraphicsContext saveGraphicsState];

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextSetTextMatrix(context, CGAffineTransformIdentity);

	CGMutablePathRef path = CGPathCreateMutable();
	CGPathAddRect(path, NULL, bounds);

	CFAttributedStringRef attrStr = (CFAttributedStringRef)CFBridgingRetain(AttributedStringWithOptions(string, textOptions, bounds.size.width < 12 ? NSLineBreakByClipping : NSLineBreakByTruncatingTail));
	if(!attrStr)
		return;
	CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(attrStr);
	CFRelease(attrStr);
	if(!framesetter)
		return;
	CTFrameRef frame = CTFramesetterCreateFrame(framesetter, CFRangeMake(0, 0), path, NULL);
	CFRelease(framesetter);
	if(!frame)
		return;

	if(textOptions & layer_t::shadow)
		CGContextSetShadowWithColor(context, NSMakeSize(0, -1), 1, [[NSColor colorWithCalibratedWhite:1 alpha:0.6] tmCGColor]);

	CTFrameDraw(frame, context);

	CFRelease(frame);
	CFRelease(path);

	[NSGraphicsContext restoreGraphicsState];
}

@interface OakControl ()
- (void)setupTrackingRects;
@end

OAK_DEBUG_VAR(OakControl);

@implementation OakControl
{
	NSInteger tag;

	std::vector<layer_t> layout;
	BOOL isTransparent;
	BOOL mouseTrackingDisabled;

	// ===================
	// = MouseDown State =
	// ===================

	BOOL isInMouseDown;
	NSPoint mouseDownPos;
}
@synthesize tag, mouseTrackingDisabled;

- (std::vector<layer_t> const&)layers
{
	return layout;
}

- (void)setLayers:(std::vector<layer_t> const&)aLayout
{
	// Remove views that are no longer in the layout
	for(auto const& oldLayer : layout)
	{
		if(oldLayer.view)
		{
			BOOL found = NO;
			for(auto const& newLayer : aLayout)
			{
				if(newLayer.view == oldLayer.view)
				{
					found = YES;
					break;
				}
			}
			if(!found)
				[oldLayer.view removeFromSuperview];
		}
	}

	// TODO this triggers a redraw — may want to consider if we can delta update…
	layout = aLayout;
	NSRect coveredRect = NSZeroRect;
	for(auto const& layer : layout)
	{
		if(layer.color || layer.image && layer.requisite == layer_t::no_requisite)
			coveredRect = NSUnionRect(coveredRect, layer.rect);
		if(NSView* view = layer.view)
		{
			if([view superview] != self)
				[view removeFromSuperview];
			NSRect viewFrame = layer.rect;
			if(view.frame.size.height > 0)
				viewFrame.size.height = view.frame.size.height;
			viewFrame.origin.x += layer.content_offset.x;
			viewFrame.origin.y += layer.content_offset.y;
			[view setFrame:viewFrame];
			[self addSubview:view];
		}
	}
	isTransparent = !NSContainsRect(coveredRect, [self bounds]);
	[self setupTrackingRects];
}

- (BOOL)isOpaque
{
	return !isTransparent;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)theEvent
{
	return YES;
}

- (NSInteger)tagForLayerContainingPoint:(NSPoint)aPoint
{
	NSInteger res = NSNotFound;
	for(auto const& it : layout)
	{
		if(NSMouseInRect(aPoint, it.rect, [self isFlipped]))
			res = it.tag;
	}
	return res;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)event
{
	// This code is copy/paste from mouseDown: and should ideally be de-duplicated.
	layer_t const* clickLayer = NULL;
	layer_t const* dragLayer  = NULL;
	uint32_t state            = [self currentState] | layer_t::mouse_clicked | layer_t::mouse_dragged | layer_t::mouse_down | layer_t::menu_gesture;
	NSPoint mousePos          = [self convertPoint:[event locationInWindow] fromView:nil];

	for(auto const& it : layout)
	{
		if(!it.action || it.requisite != (state & it.requisite_mask) || !NSMouseInRect(mousePos, it.rect, [self isFlipped]))
			continue;

		if(it.requisite & it.requisite_mask & (layer_t::mouse_clicked | layer_t::mouse_down | layer_t::menu_gesture | layer_t::mouse_double_clicked))
		{
			clickLayer = &it;
			dragLayer  = NULL; // we ignore all drag layers “behind” the click-layer, for example the close button of a tab is a click-layer, behind it is the bigger (draggable) tab, but we want to ignore that when clicking (and dragging) the close button
		}

		if([event clickCount] == 1 && (it.requisite & it.requisite_mask & layer_t::mouse_dragged))
			dragLayer = &it;
	}

	return dragLayer || clickLayer && clickLayer->prevent_window_ordering;
}

- (uint32_t)currentState
{
	NSUInteger modifierFlags = [[NSApp currentEvent] modifierFlags];
	struct { BOOL active; layer_t::requisite_t requisite; } states[] =
	{
		{ [[self window] isKeyWindow],                                 layer_t::window_key  },
		{ [[self window] isMainWindow],                                layer_t::window_main },
		{ [NSApp isActive],                                            layer_t::app_active  },
		{ (modifierFlags & NSAlternateKeyMask) == NSAlternateKeyMask,  layer_t::option      },
		{ (modifierFlags & NSControlKeyMask) == NSControlKeyMask,      layer_t::control     },
		{ (modifierFlags & NSShiftKeyMask) == NSShiftKeyMask,          layer_t::shift       },
		{ (modifierFlags & NSCommandKeyMask) == NSCommandKeyMask,      layer_t::command     },
	};

	uint32_t res = 0;
	for(size_t i = 0; i < sizeofA(states); ++i)
	{
		if(states[i].active)
			res |= states[i].requisite;
	}
	if(res & layer_t::window_main || res & layer_t::window_key)
		res |= layer_t::window_main_or_key;
	return res;
}

- (void)drawLayer:(layer_t const&)aLayer
{
	if(aLayer.color)
	{
		[aLayer.color set];
		NSRectFill(aLayer.rect);
	}

	if(aLayer.image)
	{
		if(aLayer.image_options & layer_t::stretch)
		{
			[aLayer.image drawInRect:aLayer.rect fromRect:NSZeroRect operation:NSCompositeCopy fraction:1.0];
		}
		else
		{
			NSPoint origin = NSMakePoint(aLayer.rect.origin.x + aLayer.content_offset.x, aLayer.rect.origin.y + aLayer.content_offset.y);
			[aLayer.image drawAtPoint:origin fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1.0];
		}
	}

	if(aLayer.text)
		DrawTextWithOptions(aLayer.text, aLayer.rect, aLayer.text_options);
}

- (void)drawRect:(NSRect)aRect
{
	NSPoint mousePos = [self convertPoint:[[self window] mouseLocationOutsideOfEventStream] fromView:nil];
	uint32_t state   = [self currentState];

	for(auto const& it : layout)
	{
		if(NSEqualRects(NSIntersectionRect(aRect, it.rect), NSZeroRect))
			continue;

		// if we are in a mouseDown: then use the location from that first press for all but the clicked layer (so moving mouse while holding left mouse button doesn’t update layer states)
		uint32_t mouseState = 0;
		if(!mouseTrackingDisabled)
		{
			bool isInLayer    = NSMouseInRect(mousePos, it.rect, [self isFlipped]);
			bool clickedLayer = NSMouseInRect(mouseDownPos, it.rect, [self isFlipped]);

			if(isInLayer)
				mouseState |= layer_t::mouse_inside;

			if(isInMouseDown && clickedLayer)
				mouseState |= layer_t::mouse_clicked|layer_t::menu_gesture;
		}

		if(((state | mouseState) & it.requisite_mask) == it.requisite)
			[self drawLayer:it];
	}
}

- (void)sendAction:(SEL)action fromLayer:(layer_t const&)aLayer
{
	ASSERT(action);
	tag = aLayer.tag;

	NSResponder* candidate = self;
	while(candidate)
	{
		if([candidate respondsToSelector:action])
			return (void)[NSApp sendAction:action to:candidate from:self];
		else if([candidate respondsToSelector:@selector(delegate)] && [[candidate performSelector:@selector(delegate)] respondsToSelector:action])
			return (void)[NSApp sendAction:action to:[candidate performSelector:@selector(delegate)] from:self];
		candidate = [candidate nextResponder];
	}
}

- (void)mouseDown:(NSEvent*)event
{
	D(DBF_OakControl, bug("\n"););

	layer_t const* clickLayer = NULL;
	layer_t const* dragLayer  = NULL;
	uint32_t state            = [self currentState] | layer_t::mouse_clicked | layer_t::mouse_dragged | layer_t::mouse_down | layer_t::menu_gesture;
	mouseDownPos              = [self convertPoint:[event locationInWindow] fromView:nil];

	if([event clickCount] == 2)
		state |= layer_t::mouse_double_clicked;

	for(auto const& it : layout)
	{
		if(!it.action || it.requisite != (state & it.requisite_mask) || !NSMouseInRect(mouseDownPos, it.rect, [self isFlipped]))
			continue;

		if(it.requisite & it.requisite_mask & (layer_t::mouse_clicked | layer_t::mouse_down | layer_t::menu_gesture | layer_t::mouse_double_clicked))
		{
			clickLayer = &it;
			dragLayer  = NULL; // we ignore all drag layers “behind” the click-layer, for example the close button of a tab is a click-layer, behind it is the bigger (draggable) tab, but we want to ignore that when clicking (and dragging) the close button
		}

		if([event clickCount] == 1 && (it.requisite & it.requisite_mask & layer_t::mouse_dragged))
			dragLayer = &it;
	}

	if(!clickLayer && !dragLayer)
		return;

	if(clickLayer && clickLayer->prevent_window_ordering)
		[NSApp preventWindowOrdering];

	if(clickLayer && (clickLayer->requisite & clickLayer->requisite_mask & (layer_t::mouse_down|layer_t::mouse_double_clicked)))
		return [self sendAction:clickLayer->action fromLayer:*clickLayer];

	isInMouseDown = YES;
	if(clickLayer)
		[self setNeedsDisplayInRect:clickLayer->rect];

	BOOL isInside = YES;
	NSDate* untilDate = (clickLayer->requisite & clickLayer->requisite_mask & layer_t::menu_gesture) ? [NSDate dateWithTimeIntervalSinceNow:1] : [NSDate distantFuture];
	while(true)
	{
		NSPoint mousePos = [self convertPoint:[event locationInWindow] fromView:nil];

		if((clickLayer->requisite & clickLayer->requisite_mask & layer_t::menu_gesture) && (event == nil || event.type == NSRightMouseDown || (event.type == NSLeftMouseDown && ([event modifierFlags] & NSControlKeyMask) == NSControlKeyMask)))
		{
			ASSERT(clickLayer->menuAction);
			[self sendAction:clickLayer->menuAction fromLayer:*clickLayer];
			break;
		}
		else if([event type] == NSLeftMouseUp && clickLayer)
		{
			if(NSMouseInRect(mousePos, clickLayer->rect, [self isFlipped]))
				[self sendAction:clickLayer->action fromLayer:*clickLayer];
		}
		else if([event type] == NSLeftMouseDragged)
		{
			if(dragLayer)
			{
				if(2.5 <= sqrt(SQ(mouseDownPos.x - mousePos.x) + SQ(mouseDownPos.y - mousePos.y)))
				{
					tag = dragLayer->tag;
					[NSApp sendAction:dragLayer->action to:self from:self];
					break;
				}
			}
			else if(clickLayer && isInside != NSMouseInRect(mousePos, clickLayer->rect, [self isFlipped]))
			{
				[self setNeedsDisplayInRect:clickLayer->rect];
				isInside = !isInside;
			}
		}

		if(event.type == NSLeftMouseUp)
			break;
		event = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSLeftMouseDraggedMask|NSRightMouseDownMask) untilDate:untilDate inMode:NSEventTrackingRunLoopMode dequeue:YES];
	}

	isInMouseDown = NO;
	if(clickLayer)
		[self setNeedsDisplayInRect:clickLayer->rect];
	// ideally we should call [self setNeedsDisplayInRect:clickLayer->rect] but redraw seems to happen anyway (probably due to mouseExited:) — calling it explicitly can cause small render quirks, i.e. redrawing first the close button (in a tab) as “not inside” and then on second redraw, the entire tab (causing the transparent areas of teh close button to momentarily look wrong)
}

// ============
// = Tracking =
// ============

- (void)clearTrackingRects
{
	D(DBF_OakControl, bug("\n"););
	for(NSTrackingArea* trackingArea in self.trackingAreas)
		[self removeTrackingArea:trackingArea];
	[self removeAllToolTips];
}

struct rect_cmp_t
{
	bool operator() (NSRect const& a, NSRect const& b) const
	{
		auto lhs = std::make_tuple(NSMinX(a), NSMinY(a), NSMaxX(a), NSMaxY(a));
		auto rhs = std::make_tuple(NSMinX(b), NSMinY(b), NSMaxX(b), NSMaxY(b));
		return lhs < rhs;
	}
};

- (void)setupTrackingRects
{
	D(DBF_OakControl, bug("\n"););
	[self clearTrackingRects];
	if(self.mouseTrackingDisabled)
		return;

	std::map<NSRect, std::vector<layer_t>, rect_cmp_t> trackedLayers;
	for(auto const& it : layout)
	{
		if(it.requisite & layer_t::mouse_inside || it.requisite_mask & layer_t::mouse_inside)
			trackedLayers[it.rect].push_back(it);

		if(it.tool_tip)
			[self addToolTipRect:it.rect owner:it.tool_tip userData:NULL];
	}

	for(auto const& it : trackedLayers)
	{
		NSTrackingAreaOptions trackingOptions = NSTrackingMouseEnteredAndExited;

		for(auto const& layer : it.second)
		{
			if(!(layer.requisite & layer_t::window_key))
			{
				trackingOptions |= NSTrackingActiveAlways;
				break;
			}
		}

		if(!(trackingOptions & NSTrackingActiveAlways))
			trackingOptions |= NSTrackingActiveInKeyWindow;

		[self addTrackingArea:[[NSTrackingArea alloc] initWithRect:it.first options:trackingOptions owner:self userInfo:nil]];
	}
}

- (void)setMouseTrackingDisabled:(BOOL)flag
{
	mouseTrackingDisabled = flag;
	[self setupTrackingRects];
}

- (void)mouseEntered:(NSEvent*)event
{
	if(!self.mouseTrackingDisabled)
		[self setNeedsDisplayInRect:[[event trackingArea] rect]];
}

- (void)mouseExited:(NSEvent*)event
{
	if(!self.mouseTrackingDisabled)
		[self setNeedsDisplayInRect:[[event trackingArea] rect]];
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	D(DBF_OakControl, bug("%s\n", newWindow.description.UTF8String););
	[self clearTrackingRects];
	if(newWindow)
		[self setupTrackingRects];
	[super viewWillMoveToWindow:newWindow];
}

- (void)setKeyState:(NSUInteger)newState
{
	D(DBF_OakControl, bug("%s\n", BSTR([[self window] isKeyWindow])););
	[super setKeyState:newState];
	// TODO only redraw the layers which has requisite set so that they would change on view/window/app focus changes
	[self setNeedsDisplay:YES];
}
@end
