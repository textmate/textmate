#import "GutterView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/attr_string.h>
#import <text/types.h>
#import <cf/cgrect.h>
#import <oak/CocoaSTL.h>
#import <oak/debug.h>
#import <oak/oak.h>

OAK_DEBUG_VAR(GutterView);

NSString* GVColumnDataSourceDidChange   = @"GVColumnDataSourceDidChange";
NSString* GVLineNumbersColumnIdentifier = @"lineNumbers";

static CGFloat WidthOfLineNumbers (NSUInteger lineNumber, NSFont* font);

struct data_source_t
{
	data_source_t (std::string const& identifier, id datasource, id delegate) : identifier(identifier), datasource(datasource), delegate(delegate) { }

	std::string identifier;
	id datasource;
	id delegate;
	CGFloat x0;
	CGFloat width;
};

@interface GutterView ()
- (void)frameDidChange:(NSNotification*)aNotification;
- (void)updateWidthWithAnimation:(BOOL)animate;
- (void)setGutterWidth:(CGFloat)newWidth animate:(BOOL)animate;
- (CGFloat)widthForColumnWithIdentifier:(std::string const&)identifier;

- (void)clearTrackingRects;
- (void)setupTrackingRects;

- (data_source_t*)columnWithIdentifier:(std::string const&)identifier;
@end

@implementation GutterView
@synthesize partnerView, lineNumberFont, delegate;

// ==================
// = Setup/Teardown =
// ==================

- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		hiddenColumns       = [NSMutableSet new];
		self.lineNumberFont = [NSFont userFixedPitchFontOfSize:12];
		[self insertColumnWithIdentifier:GVLineNumbersColumnIdentifier atPosition:0 dataSource:nil delegate:nil];

		mouseDownAtPoint     = NSMakePoint(-1, -1);
		mouseHoveringAtPoint = NSMakePoint(-1, -1);

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(cursorDidHide:) name:OakCursorDidHideNotification object:nil];
	}
	return self;
}

- (void)updateTrackingAreas
{
	D(DBF_GutterView, bug("\n"););
	[super updateTrackingAreas];
	[self setupTrackingRects];
}

- (void)viewDidMoveToSuperview
{
	[self frameDidChange:nil];
}

- (void)removeFromSuperview
{
	D(DBF_GutterView, bug("\n"););
	self.partnerView = nil;
}

- (void)viewDidMoveToWindow
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignKeyNotification object:nil];
	if(self.window)
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidResignKey:) name:NSWindowDidResignKeyNotification object:self.window];
}

- (void)windowDidResignKey:(NSNotification*)notification
{
	[self mouseExited:nil];
}

- (void)dealloc
{
	D(DBF_GutterView, bug("\n"););
	self.partnerView    = nil;
	self.lineNumberFont = nil;
	iterate(it, columnDataSources)
	{
		if(it->datasource)
			[[NSNotificationCenter defaultCenter] removeObserver:self name:GVColumnDataSourceDidChange object:it->datasource];
	}
	[hiddenColumns release];
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

- (void)setupSelectionRects
{
	backgroundRects.clear();
	borderRects.clear();

	citerate(range, text::selection_t(highlightedRange))
	{
		auto from = range->min(), to = range->max();
		CGFloat firstY = [delegate lineFragmentForLine:from.line column:from.column].firstY;
		auto fragment = [delegate lineFragmentForLine:to.line column:to.column];
		CGFloat lastY = to.column == 0 && from.line != to.line ? fragment.firstY : fragment.lastY;

		backgroundRects.push_back(CGRectMake(0, firstY+1, self.frame.size.width, lastY - firstY - 2));
		borderRects.push_back(CGRectMake(0, firstY, self.frame.size.width, 1));
		borderRects.push_back(CGRectMake(0, lastY-1, self.frame.size.width, 1));
	}
}

// =============
// = Accessors =
// =============

- (void)setHighlightedRange:(std::string const&)str
{
	D(DBF_GutterView, bug("str: %s\n", str.c_str()););

	std::vector<CGRect> oldBackgroundRects, oldBorderRects, refreshRects;
	backgroundRects.swap(oldBackgroundRects);
	borderRects.swap(oldBorderRects);

	highlightedRange = str;
	[self setupSelectionRects];

	OakRectSymmetricDifference(oldBackgroundRects, backgroundRects,    back_inserter(refreshRects));
	OakRectSymmetricDifference(oldBorderRects,     borderRects,        back_inserter(refreshRects));
	iterate(rect, refreshRects)
		[self setNeedsDisplayInRect:*rect];
}

- (void)setLineNumberFont:(NSFont*)font
{
	if(font && font != self.lineNumberFont)
	{
		[lineNumberFont release];
		lineNumberFont = [font retain];
		[self frameDidChange:nil];
	}
}

- (void)setPartnerView:(NSView*)aView
{
	D(DBF_GutterView, bug("%s (%p)\n", [[[aView class] description] UTF8String], aView););

	if(partnerView)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self];
		[partnerView release];
	}

	if(partnerView = [aView retain])
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(frameDidChange:) name:NSViewFrameDidChangeNotification object:partnerView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(boundsDidChange:) name:NSViewBoundsDidChangeNotification object:[[partnerView enclosingScrollView] contentView]];
		[self frameDidChange:nil];
	}
}

- (void)setDelegate:(id <GutterViewDelegate>)aDelegate
{
	delegate = aDelegate;
}

- (void)insertColumnWithIdentifier:(NSString*)columnIdentifier atPosition:(NSUInteger)index dataSource:(id <GutterViewColumnDataSource>)columnDataSource delegate:(id <GutterViewColumnDelegate>)columnDelegate
{
	ASSERT(index <= columnDataSources.size());
	columnDataSources.insert(columnDataSources.begin() + index, data_source_t(columnIdentifier.UTF8String, columnDataSource, columnDelegate));
	if(columnDelegate)
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(columnDataSourceDidChange:) name:GVColumnDataSourceDidChange object:columnDelegate];
	[self reloadData:self];
}

- (BOOL)isFlipped
{
	return YES;
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)setGutterWidth:(CGFloat)newWidth animate:(BOOL)animate
{
	NSRect frame = self.frame;
	CGFloat widthDifference = newWidth - frame.size.width;
	frame.size.width = newWidth;
	[(animate ? self.animator : self) setFrame:frame];

	frame = self.enclosingScrollView.frame;
	frame.size.width += widthDifference;
	[(animate ? self.enclosingScrollView.animator : self.enclosingScrollView) setFrame:frame];

	frame = partnerView.enclosingScrollView.frame;
	frame.size.width -= widthDifference;
	frame.origin.x   += widthDifference;
	[(animate ? partnerView.enclosingScrollView.animator : partnerView.enclosingScrollView) setFrame:frame];
}

- (BOOL)visibilityForColumnWithIdentifier:(NSString*)identifier
{
	return ![hiddenColumns containsObject:identifier];
}

- (CGFloat)widthForColumnWithIdentifier:(std::string const&)identifier
{
	CGFloat width = 0;

	if(identifier == [GVLineNumbersColumnIdentifier UTF8String])
	{
		NSUInteger lastLineNumber = [delegate lineRecordForPosition:NSHeight([partnerView frame])].lineNumber;
		CGFloat newWidth = WidthOfLineNumbers(lastLineNumber == NSNotFound ? 0 : lastLineNumber + 1, self.lineNumberFont);

		width = [self columnWithIdentifier:identifier]->width;
		if(width < newWidth || (newWidth < width && WidthOfLineNumbers(lastLineNumber * 4/3, self.lineNumberFont) < width))
			width = newWidth;
	}
	else
	{
		NSUInteger n = 1;
		while(NSImage* img = [[self columnWithIdentifier:identifier]->datasource imageForState:n++ forColumnWithIdentifier:[NSString stringWithCxxString:identifier]])
			width = std::max(width, [img size].width);
	}

	return ceil(width);
}

- (data_source_t*)columnWithIdentifier:(std::string const&)identifier
{
	iterate(it, columnDataSources)
	{
		if(it->identifier == identifier)
			return &(*it);
	}
	ASSERT(false);
	return NULL;
}

- (std::vector<data_source_t> const&)visibleColumnDataSources
{
	static std::vector<data_source_t> visibleColumnDataSources;
	visibleColumnDataSources.clear();

	iterate(it, columnDataSources)
	{
		if([self visibilityForColumnWithIdentifier:[NSString stringWithCxxString:it->identifier]])
			visibleColumnDataSources.push_back(*it);
	}
	return visibleColumnDataSources;
}

// ================
// = Data sources =
// ================

- (NSImage*)imageForColumn:(std::string const&)identifier atLine:(NSUInteger)lineNumber hovering:(BOOL)hovering pressed:(BOOL)pressed
{
	id datasource    = [self columnWithIdentifier:identifier]->datasource;
	NSUInteger state = [datasource stateForColumnWithIdentifier:[NSString stringWithCxxString:identifier] atLine:lineNumber];
	NSImage* image   = nil;

	if(pressed && [datasource respondsToSelector:@selector(pressedImageForState:forColumnWithIdentifier:)])
		image = [datasource pressedImageForState:state forColumnWithIdentifier:[NSString stringWithCxxString:identifier]];
	else if(hovering && [datasource respondsToSelector:@selector(hoverImageForState:forColumnWithIdentifier:)])
		image = [datasource hoverImageForState:state forColumnWithIdentifier:[NSString stringWithCxxString:identifier]];

	return image ?: [datasource imageForState:state forColumnWithIdentifier:[NSString stringWithCxxString:identifier]];
}

// ==========
// = Events =
// ==========

- (void)boundsDidChange:(NSNotification*)aNotification
{
	[self.enclosingScrollView.contentView scrollToPoint:NSMakePoint(0, NSMinY(partnerView.enclosingScrollView.contentView.bounds))];
}

- (void)frameDidChange:(NSNotification*)aNotification
{
	[self setFrameSize:NSMakeSize(NSWidth(self.enclosingScrollView.documentVisibleRect), NSHeight(partnerView.frame) + NSHeight([self.enclosingScrollView documentVisibleRect]) - NSHeight([partnerView.enclosingScrollView documentVisibleRect]))];
	[self.enclosingScrollView.contentView scrollToPoint:NSMakePoint(0, NSMinY(partnerView.enclosingScrollView.contentView.bounds))];
	[self updateWidthWithAnimation:NO];
}

static CTLineRef CTCreateLineFromText (std::string const& text, NSFont* font)
{
	return CTLineCreateWithAttributedString(ns::attr_string_t(font) << [NSColor grayColor] << text);
}

static CGFloat WidthOfLineNumbers (NSUInteger lineNumber, NSFont* font)
{
	CTLineRef line = CTCreateLineFromText(text::format("%ld", std::max<NSUInteger>(10, lineNumber)), font);
	CGFloat width  = CTLineGetTypographicBounds(line, NULL, NULL, NULL);
	CFRelease(line);
	return ceil(width);
}

static void DrawText (std::string const& text, CGRect const& rect, CGFloat baseline, NSFont* font)
{
	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextSaveGState(context);

	CTLineRef line = CTCreateLineFromText(text, font);
	CGContextSetTextMatrix(context, CGAffineTransformIdentity);
	CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * baseline));
	CGContextSetTextPosition(context, CGRectGetMaxX(rect) - CTLineGetTypographicBounds(line, NULL, NULL, NULL), baseline);
	CTLineDraw(line, context);
	CFRelease(line);

	CGContextRestoreGState(context);
}

- (void)drawRect:(NSRect)aRect
{
	[self.enclosingScrollView.backgroundColor set];
	NSRectFill(NSIntersectionRect(aRect, NSOffsetRect(self.frame, -1, 0)));

	[[NSColor grayColor] set];
	NSRectFill(NSIntersectionRect(aRect, NSOffsetRect(self.frame, NSWidth(self.frame)-1, 0)));

	[self setupSelectionRects];

	[[self.enclosingScrollView.backgroundColor highlightWithLevel:0.5] set];
	iterate(rect, backgroundRects)
		NSRectFill(NSIntersectionRect(*rect, NSIntersectionRect(aRect, NSOffsetRect(self.frame, -1, 0))));

	[[NSColor grayColor] set];
	iterate(rect, borderRects)
		NSRectFill(NSIntersectionRect(*rect, NSIntersectionRect(aRect, NSOffsetRect(self.frame, -1, 0))));

	std::pair<NSUInteger, NSUInteger> prevLine(NSNotFound, 0);
	for(CGFloat y = NSMinY(aRect); y < NSMaxY(aRect); )
	{
		GVLineRecord record = [delegate lineRecordForPosition:y];
		if(prevLine == std::make_pair(record.lineNumber, record.softlineOffset))
			break;
		prevLine = std::make_pair(record.lineNumber, record.softlineOffset);

		citerate(dataSource, [self visibleColumnDataSources])
		{
			NSRect columnRect = NSMakeRect(dataSource->x0, record.firstY, dataSource->width, record.lastY - record.firstY);
			if(dataSource->identifier == GVLineNumbersColumnIdentifier.UTF8String)
			{
				DrawText(record.softlineOffset == 0 ? text::format("%ld", record.lineNumber + 1) : "·", columnRect, NSMinY(columnRect) + record.baseline, self.lineNumberFont);
			}
			else if(record.softlineOffset == 0)
			{
				BOOL isHoveringRect = NSMouseInRect(mouseHoveringAtPoint, columnRect, [self isFlipped]);
				BOOL isDownInRect   = NSMouseInRect(mouseDownAtPoint,     columnRect, [self isFlipped]);

				NSImage* image = [self imageForColumn:dataSource->identifier atLine:record.lineNumber hovering:isHoveringRect && NSEqualPoints(mouseDownAtPoint, NSMakePoint(-1, -1)) pressed:isHoveringRect && isDownInRect];
				CGFloat y = round((NSHeight(columnRect) - [image size].height) / 2);
				CGFloat x = round((NSWidth(columnRect) - [image size].width) / 2);
				[image drawAdjustedAtPoint:NSMakePoint(NSMinX(columnRect) + x, NSMinY(columnRect) + y) fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1.0];
			}
		}

		y = record.lastY;
	}
}

- (void)updateWidthWithAnimation:(BOOL)animate
{
	// Don’t perform any sizing until the view is set up,
	// to avoid the width becoming out of sync with the partnerView
	if(!partnerView || !self.superview)
		return;

	static const CGFloat columnPadding = 1;

	CGFloat currentX = 0, totalWidth = 1; // we start at 1 to account for the right border
	iterate(it, columnDataSources)
	{
		it->x0 = currentX;
		if([self visibilityForColumnWithIdentifier:[NSString stringWithCxxString:it->identifier]])
		{
			it->width   = [self widthForColumnWithIdentifier:it->identifier];
			totalWidth += it->width + columnPadding;
			currentX   += it->width + columnPadding;
		}
		else
		{
			it->width = 0;
		}
	}
	[self setGutterWidth:totalWidth animate:animate];
}

- (void)reloadData:(id)sender
{
	D(DBF_GutterView, bug("\n"););
	[self updateWidthWithAnimation:NO];
	[self setNeedsDisplay:YES];
}

- (NSRect)columnRectForPoint:(NSPoint)aPoint
{
	GVLineRecord record = [delegate lineRecordForPosition:aPoint.y];
	if(record.lineNumber != NSNotFound && record.softlineOffset == 0)
	{
		citerate(dataSource, [self visibleColumnDataSources])
		{
			if(dataSource->identifier == [GVLineNumbersColumnIdentifier UTF8String])
				continue;

			NSRect columnRect = NSMakeRect(dataSource->x0, record.firstY, dataSource->width, record.lastY - record.firstY);
			if(NSPointInRect(aPoint, columnRect))
				return columnRect;
		}
	}
	return NSZeroRect;
}

- (void)mouseDown:(NSEvent*)event
{
	D(DBF_GutterView, bug("\n"););
	NSPoint pos = [self convertPoint:[event locationInWindow] fromView:nil];
	NSRect columnRect = [self columnRectForPoint:pos];
	if(NSMouseInRect(pos, columnRect, [self isFlipped]))
	{
		mouseDownAtPoint = pos;
		[self setNeedsDisplayInRect:columnRect];

		while([event type] != NSLeftMouseUp)
		{
			event = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSMouseMovedMask|NSLeftMouseDraggedMask|NSMouseEnteredMask|NSMouseExitedMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
			if([event type] == NSMouseMoved || [event type] == NSLeftMouseDragged)
				[self mouseMoved:event];
		}

		if(NSEqualRects(columnRect, [self columnRectForPoint:mouseHoveringAtPoint]))
		{
			GVLineRecord record = [delegate lineRecordForPosition:mouseDownAtPoint.y];
			citerate(dataSource, [self visibleColumnDataSources])
			{
				NSRect columnRect = NSMakeRect(dataSource->x0, record.firstY, dataSource->width, record.lastY - record.firstY);
				if(NSPointInRect(mouseDownAtPoint, columnRect))
					[dataSource->delegate userDidClickColumnWithIdentifier:[NSString stringWithCxxString:dataSource->identifier] atLine:record.lineNumber];
			}
		}
		else
		{
			mouseHoveringAtPoint = NSMakePoint(-1, -1);
		}

		mouseDownAtPoint = NSMakePoint(-1, -1);
		[self setNeedsDisplayInRect:columnRect];
	}
	else
	{
		[partnerView mouseDown:event];
		while([event type] != NSLeftMouseUp)
		{
			event = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSMouseMovedMask|NSLeftMouseDraggedMask|NSMouseEnteredMask|NSMouseExitedMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
			if([event type] == NSMouseMoved || [event type] == NSLeftMouseDragged)
				[partnerView mouseDragged:event];
		}
		[partnerView mouseUp:event];
	}
}

- (void)columnDataSourceDidChange:(NSNotification*)notification
{
	[self reloadData:[notification object]];
}

- (void)setVisibility:(BOOL)visible forColumnWithIdentifier:(NSString*)columnIdentifier
{
	if(visible)
			[hiddenColumns removeObject:columnIdentifier];
	else	[hiddenColumns addObject:columnIdentifier];
	[self updateWidthWithAnimation:NO];
}

// ==================
// = Tracking rects =
// ==================

- (void)clearTrackingRects
{
	D(DBF_GutterView, bug("\n"););
	for(NSTrackingArea* trackingArea in self.trackingAreas)
		[self removeTrackingArea:trackingArea];
}

- (void)setupTrackingRects
{
	D(DBF_GutterView, bug("\n"););
	[self clearTrackingRects];

	NSTrackingArea* trackingArea = [[NSTrackingArea alloc] initWithRect:[self visibleRect] options:NSTrackingMouseEnteredAndExited|NSTrackingMouseMoved|NSTrackingActiveInKeyWindow owner:self userInfo:nil];
	[self addTrackingArea:trackingArea];
	[trackingArea release];
}

- (void)scrollWheel:(NSEvent*)event
{
	[partnerView scrollWheel:event];
	[self mouseMoved:event];
}

- (void)mouseMoved:(NSEvent*)event
{
	NSRect beforeRect = [self columnRectForPoint:mouseHoveringAtPoint];
	mouseHoveringAtPoint = [self convertPoint:[event locationInWindow] fromView:nil];
	NSRect afterRect = [self columnRectForPoint:mouseHoveringAtPoint];

	if(!NSEqualRects(beforeRect, afterRect))
	{
		[self setNeedsDisplayInRect:beforeRect];
		[self setNeedsDisplayInRect:afterRect];
	}
}

- (void)mouseExited:(NSEvent*)event
{
	NSRect columnRect = [self columnRectForPoint:mouseHoveringAtPoint];
	mouseHoveringAtPoint = NSMakePoint(-1, -1);
	[self setNeedsDisplayInRect:columnRect];
}

- (void)cursorDidHide:(NSNotification*)aNotification
{
	[self mouseExited:[[self window] currentEvent]];
}
@end
