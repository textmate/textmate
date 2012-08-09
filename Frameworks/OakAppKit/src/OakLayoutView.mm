#import "OakLayoutView.h"
#import <oak/CocoaSTL.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(OakLayoutView);

enum edge_t { kLeftEdge, kTopEdge, kRightEdge, kBottomEdge };

struct OakResizeHandle
{
	box_t* box;
	edge_t edge;
	int distance;

	EXPLICIT operator bool () const { return box; }
};

static void add_cursors (box_t* box, NSView* view)
{
	if(!box || box->children().empty())
		return;

	std::vector<box_t*> const& children = box->children();
	for(size_t i = 0; i < children.size() - 1; ++i)
	{
		rect_t prev = children[i]->bounds(), next = children[i+1]->bounds();
		rect_t r = box->horizontal() ? rect_t(prev.x1 - 2, next.x0 + 2, prev.y0, prev.y1) : rect_t(prev.x0, prev.x1, prev.y1 - 2, next.y0 + 2);
		[view addCursorRect:NSMakeRect(r.x0, r.y0, r.width(), r.height()) cursor:box->horizontal() ? [NSCursor resizeLeftRightCursor] : [NSCursor resizeUpDownCursor]];
	}

	iterate(child, children)
		add_cursors(*child, view);
}

static void draw_thumbs (box_t* box, NSView* view)
{
	if(!box || box->children().empty())
		return;

	std::vector<box_t*> const& children = box->children();
	for(size_t i = 0; i < children.size() - 1; ++i)
	{
		rect_t prev = children[i]->bounds(), next = children[i+1]->bounds();
		rect_t r = box->horizontal() ? rect_t(prev.x1, next.x0, prev.y0, prev.y1) : rect_t(prev.x0, prev.x1, prev.y1, next.y0);
		NSRectFill(NSMakeRect(r.x0, r.y0, r.width(), r.height()));
	}

	iterate(child, children)
		draw_thumbs(*child, view);
}

static OakResizeHandle find_resize_info (box_t* box, int x, int y)
{
	if(!box || box->children().empty())
		return (OakResizeHandle){ NULL };

	std::vector<box_t*> const& children = box->children();
	for(ssize_t i = 0; i < children.size() - 1; ++i)
	{
		rect_t prev = children[i]->bounds(), next = children[i+1]->bounds();
		rect_t r = box->horizontal() ? rect_t(prev.x1 - 2, next.x0 + 2, prev.y0, prev.y1) : rect_t(prev.x0, prev.x1, prev.y1 - 2, next.y0 + 2);
		if(r.contains(x, y))
			return (OakResizeHandle){ children[i], box->horizontal() ? kRightEdge : kBottomEdge, box->horizontal() ? prev.x1 - x : prev.y1 - y };
	}

	iterate(child, children)
	{
		if(OakResizeHandle res = find_resize_info(*child, x, y))
			return res;
	}

	return (OakResizeHandle){ NULL };
}

static bool horizontal_edge (NSRectEdge edge)
{
	return edge == NSMinXEdge || edge == NSMaxXEdge;
}

static box_t* add_box (box_t* newBox, box_t* container, NSRectEdge edge, box_t const* otherBox)
{
	if(!container)
		return newBox;

	if(otherBox && otherBox != container)
	{
		std::vector<box_t*>& children = container->children();
		iterate(child, children)
			*child = add_box(newBox, *child, edge, otherBox);
		return container;
	}
	else
	{
		if(container->children().empty() || container->horizontal() != horizontal_edge(edge))
		{
			box_t* wrapper = new box_t(horizontal_edge(edge) ? 0 : container->width(), horizontal_edge(edge) ? container->height() : 0, 0, 0, false, horizontal_edge(edge));
			wrapper->push_back(container);
			container = wrapper;
		}

		rect_t r = container->bounds();
		switch(edge)
		{
			case NSMinXEdge: r.x0 += newBox->width() + kViewSpacing;  newBox->set_height(container->height()); break;
			case NSMinYEdge: r.y0 += newBox->height() + kViewSpacing; newBox->set_width(container->width());   break;
			case NSMaxXEdge: r.x1 -= newBox->width() + kViewSpacing;  newBox->set_height(container->height()); break;
			case NSMaxYEdge: r.y1 -= newBox->height() + kViewSpacing; newBox->set_width(container->width());   break;
		}

		container->set_size(r);
		if(edge == NSMinXEdge || edge == NSMinYEdge)
				container->push_front(newBox);
		else	container->push_back(newBox);
		container->freeze_layout();
	}

	return container;
}

@implementation OakLayoutView
- (void)repositionSubviews
{
	for(NSView* view in [self subviews])
	{
		std::map<NSView*, box_t*>::const_iterator it = views.find(view);
		ASSERT(it != views.end());
		[view setFrame:NSMakeRect(it->second->x0(), it->second->y0(), it->second->width(), it->second->height())];
	}
}

- (void)setLocked:(BOOL)flag forView:(NSView*)aView
{
	std::map<NSView*, box_t*>::iterator it = views.find(aView);
	ASSERT(it != views.end());
	it->second->set_locked(flag);
}

- (void)addView:(NSView*)aView atEdge:(NSRectEdge)anEdge ofView:(NSView*)otherView
{
	std::map<NSView*, box_t*>::const_iterator it = views.find(otherView);
	box_t const* otherBox = it != views.end() ? it->second : NULL;
	box_t* newBox = new box_t(NSWidth(aView.frame), NSHeight(aView.frame));

	box = add_box(newBox, box, anEdge, otherBox);
	box->set_size(rect_t(0, NSWidth(self.frame), 0, NSHeight(self.frame)));
	box->freeze_layout();

	views.insert(std::make_pair(aView, newBox));

	[self addSubview:aView];
	[self repositionSubviews];
	[[self window] invalidateCursorRectsForView:self];

	// [self setNeedsDisplay:YES];
	// [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewDidResize:) name:NSViewFrameDidChangeNotification object:aView];
}

- (void)removeView:(NSView*)aView
{
	std::map<NSView*, box_t*>::iterator it = views.find(aView);
	ASSERT(it != views.end());

	box = box->erase(it->second);
	box->freeze_layout();
	views.erase(it);
	[self removeResizeInfoForView:aView];
	[aView removeFromSuperview];
	[self repositionSubviews];
	[[self window] invalidateCursorRectsForView:self];
}

- (void)resetCursorRects
{
	add_cursors(box, self);

	iterate(pair, resize_info)
	{
		NSRect r = pair->first.frame;

		int x0, y0;
		switch(pair->second.corner)
		{
			case OakResizeInfo::kTopLeft:     x0 = NSMinX(r); y0 = NSMinY(r); break;
			case OakResizeInfo::kTopRight:    x0 = NSMaxX(r); y0 = NSMinY(r); break;
			case OakResizeInfo::kBottomLeft:  x0 = NSMinX(r); y0 = NSMaxY(r); break;
			case OakResizeInfo::kBottomRight: x0 = NSMaxX(r); y0 = NSMaxY(r); break;
		}

		int x1 = x0 + pair->second.width;
		int y1 = y0 + pair->second.height;
		rect_t rect(std::min(x0, x1), std::max(x0, x1), std::min(y0, y1), std::max(y0, y1));

		[self addCursorRect:NSMakeRect(rect.x0, rect.y0, rect.width(), rect.height()) cursor:pair->second.adjustable == OakResizeInfo::kWidth ? [NSCursor resizeLeftRightCursor] : [NSCursor resizeUpDownCursor]];
	}
}

- (void)resizeSubviewsWithOldSize:(NSSize)oldSize
{
	int w = std::max<int>(NSWidth(self.frame), box->min_width());
	int h = std::max<int>(NSHeight(self.frame), box->min_height());
	box->set_size(rect_t(0, w, 0, h));
	[self repositionSubviews];
}

- (BOOL)isFlipped
{
	return YES;
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)drawRect:(NSRect)aRect
{
	[[NSColor lightGrayColor] set];
	draw_thumbs(box, self);
}

- (OakResizeHandle)findResizeHandle:(NSPoint)pos
{
	int x = (int)round(pos.x), y = (int)round(pos.y);
	if(OakResizeHandle res = find_resize_info(box, x, y))
		return res;

	iterate(pair, resize_info)
	{
		NSRect r = pair->first.frame;

		int x0, y0;
		edge_t edge = kLeftEdge; // avoid compiler warning
		switch(pair->second.corner)
		{
			case OakResizeInfo::kTopLeft:     x0 = NSMinX(r); y0 = NSMinY(r); edge = pair->second.adjustable == OakResizeInfo::kWidth ? kLeftEdge  : kTopEdge;    break;
			case OakResizeInfo::kTopRight:    x0 = NSMaxX(r); y0 = NSMinY(r); edge = pair->second.adjustable == OakResizeInfo::kWidth ? kRightEdge : kTopEdge;    break;
			case OakResizeInfo::kBottomLeft:  x0 = NSMinX(r); y0 = NSMaxY(r); edge = pair->second.adjustable == OakResizeInfo::kWidth ? kLeftEdge  : kBottomEdge; break;
			case OakResizeInfo::kBottomRight: x0 = NSMaxX(r); y0 = NSMaxY(r); edge = pair->second.adjustable == OakResizeInfo::kWidth ? kRightEdge : kBottomEdge; break;
		}

		int x1 = x0 + pair->second.width;
		int y1 = y0 + pair->second.height;

		if(rect_t(std::min(x0, x1), std::max(x0, x1), std::min(y0, y1), std::max(y0, y1)).contains(x, y))
		{
			int distance = 0;
			switch(edge)
			{
				case kLeftEdge:   distance = x - std::min(x0, x1); break;
				case kRightEdge:  distance = std::max(x0, x1) - x; break;
				case kTopEdge:    distance = y - std::min(y0, y1); break;
				case kBottomEdge: distance = std::max(y0, y1) - y; break;
			}
			return (OakResizeHandle){ views[pair->first], edge, distance };
		}
	}

	return (OakResizeHandle){ NULL };
}

- (void)mouseDown:(NSEvent*)anEvent
{
	NSPoint pos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	D(DBF_OakLayoutView, bug("%.0f, %.0f\n", pos.x, pos.y););
	if(OakResizeHandle handle = [self findResizeHandle:pos])
	{
		box_t* clicked = handle.box;
		edge_t edge    = handle.edge;
		int distance   = handle.distance;

		box->freeze_layout();
		D(DBF_OakLayoutView, bug("start resize tracking of box %s\n", to_s(clicked->bounds()).c_str()););
		while((anEvent = [[self window] nextEventMatchingMask:NSLeftMouseDraggedMask | NSLeftMouseUpMask untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES]) && [anEvent type] != NSLeftMouseUp)
		{
			rect_t bounds = clicked->bounds();
			pos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
			switch(edge)
			{
				case kLeftEdge:   bounds.x0 = pos.x - distance; break;
				case kRightEdge:  bounds.x1 = pos.x + distance; break;
				case kTopEdge:    bounds.y0 = pos.y - distance; break;
				case kBottomEdge: bounds.y1 = pos.y + distance; break;
			}
			box->resize(clicked, bounds);
			box->freeze_layout();
			[self repositionSubviews];
			[[self window] invalidateCursorRectsForView:self];
			[self setNeedsDisplay:YES];
		}

		D(DBF_OakLayoutView, bug("stop resize tracking\n"););
		[[self window] discardEventsMatchingMask:NSAnyEventMask beforeEvent:anEvent];
	}
	else
	{
		return [super mouseDown:anEvent];
	}
}

- (void)addView:(NSView*)aView
{
	[self addView:aView atEdge:NSMaxYEdge ofView:nil];
}

- (void)addResizeInfo:(OakResizeInfo)info forView:(NSView*)aView
{
	resize_info.insert(std::make_pair(aView, info));
}

- (void)removeResizeInfoForView:(NSView*)aView
{
	resize_info.erase(resize_info.lower_bound(aView), resize_info.upper_bound(aView));
}

- (void)performClose:(id)sender
{
	NSView* view = (NSView*)[[self window] firstResponder];
	if(![view isKindOfClass:[NSView class]])
		return;

	while(view && [view superview] != self)
		view = [view superview];

	if(view)
			[NSApp sendAction:@selector(performCloseSplit:) to:nil from:view];
	else	[[self nextResponder] tryToPerform:_cmd with:sender];
}

- (NSView*)hitTest:(NSPoint)aPoint
{
	D(DBF_OakLayoutView, bug("%s\n", [NSStringFromPoint(aPoint) UTF8String]););
	if([self findResizeHandle:[self convertPoint:aPoint fromView:[self superview]]])
	{
		D(DBF_OakLayoutView, bug("found resize handle\n"););
		return self;
	}

	D(DBF_OakLayoutView, bug("call super\n"););
	ASSERT([super respondsToSelector:@selector(hitTest:)]);
	return [super hitTest:aPoint];
}
@end
