#import "GutterView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <text/types.h>
#import <cf/cf.h>
#import <cf/cgrect.h>
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
	__weak id datasource;
	__weak id delegate;
	CGFloat x0;
	CGFloat width;
};

@interface GutterView ()
@property (nonatomic) NSSize size;
- (CGFloat)widthForColumnWithIdentifier:(std::string const&)identifier;
- (data_source_t*)columnWithIdentifier:(std::string const&)identifier;

- (void)clearTrackingRects;
- (void)setupTrackingRects;
@end

@implementation GutterView
{
	std::vector<data_source_t> columnDataSources;
	NSMutableSet* hiddenColumns;
	std::string highlightedRange;
	std::vector<CGRect> backgroundRects, borderRects;

	NSPoint mouseDownAtPoint;
	NSPoint mouseHoveringAtPoint;
}

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
	for(auto const& it : columnDataSources)
	{
		if(it.datasource)
			[[NSNotificationCenter defaultCenter] removeObserver:self name:GVColumnDataSourceDidChange object:it.datasource];
	}
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)setupSelectionRects
{
	backgroundRects.clear();
	borderRects.clear();

	for(auto const& range : text::selection_t(highlightedRange))
	{
		auto from = range.min(), to = range.max();
		CGFloat firstY = [self.delegate lineFragmentForLine:from.line column:from.column].firstY;
		auto fragment = [self.delegate lineFragmentForLine:to.line column:to.column];
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
	for(auto const& rect : refreshRects)
		[self setNeedsDisplayInRect:rect];
}

- (void)setPartnerView:(NSView*)aView
{
	D(DBF_GutterView, bug("%s (%p)\n", [[[aView class] description] UTF8String], aView););

	if(_partnerView)
		[[NSNotificationCenter defaultCenter] removeObserver:self];
	if(_partnerView = aView)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(boundsDidChange:) name:NSViewBoundsDidChangeNotification object:[[_partnerView enclosingScrollView] contentView]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(boundsDidChange:) name:NSViewFrameDidChangeNotification object:_partnerView];
	}
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

- (BOOL)visibilityForColumnWithIdentifier:(NSString*)identifier
{
	return ![hiddenColumns containsObject:identifier];
}

- (CGFloat)widthForColumnWithIdentifier:(std::string const&)identifier
{
	CGFloat width = 0;
	if(!self.delegate)
		return 5;

	if(identifier == [GVLineNumbersColumnIdentifier UTF8String])
	{
		NSUInteger lastLineNumber = [self.delegate lineRecordForPosition:NSHeight([_partnerView frame])].lineNumber;
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
	for(auto& it : columnDataSources)
	{
		if(it.identifier == identifier)
			return &it;
	}
	ASSERT(false);
	return NULL;
}

- (std::vector<data_source_t>)visibleColumnDataSources
{
	std::vector<data_source_t> visibleColumnDataSources;
	for(auto const& it : columnDataSources)
	{
		if([self visibilityForColumnWithIdentifier:[NSString stringWithCxxString:it.identifier]])
			visibleColumnDataSources.push_back(it);
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
	[self updateSize];
	[self.enclosingScrollView.contentView scrollToPoint:NSMakePoint(0, NSMinY(_partnerView.enclosingScrollView.contentView.bounds))];
}

- (void)setSize:(NSSize)newSize
{
	if(NSEqualSizes(_size, newSize))
		return;
	_size = newSize;
	[self invalidateIntrinsicContentSize];
}

static CTLineRef CreateCTLineFromText (std::string const& text, NSFont* font, NSColor* color = nil)
{
	CTLineRef res = NULL;
	if(CFMutableDictionaryRef dict = CFDictionaryCreateMutable(kCFAllocatorDefault, 2, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
	{
		if(CGColorRef cgColor = [color tmCGColor])
			CFDictionaryAddValue(dict, kCTForegroundColorAttributeName, cgColor);

		if(CFStringRef fontName = (CFStringRef)CFBridgingRetain([font fontName]))
		{
			if(CTFontRef ctFont = CTFontCreateWithName(fontName, [font pointSize], NULL))
			{
				CFDictionaryAddValue(dict, kCTFontAttributeName, ctFont);
				CFRelease(ctFont);
			}
			CFRelease(fontName);
		}

		if(CFAttributedStringRef str = CFAttributedStringCreate(kCFAllocatorDefault, cf::wrap(text), dict))
		{
			res = CTLineCreateWithAttributedString(str);
			CFRelease(str);
		}

		CFRelease(dict);
	}
	return res;
}

static CGFloat WidthOfLineNumbers (NSUInteger lineNumber, NSFont* font)
{
	CTLineRef line = CreateCTLineFromText(std::to_string(std::max<NSUInteger>(10, lineNumber)), font);
	CGFloat width  = CTLineGetTypographicBounds(line, NULL, NULL, NULL);
	CFRelease(line);
	return ceil(width);
}

static void DrawText (std::string const& text, CGRect const& rect, CGFloat baseline, NSFont* font, NSColor* color)
{
	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextSaveGState(context);

	CTLineRef line = CreateCTLineFromText(text, font, color);
	CGContextSetTextMatrix(context, CGAffineTransformIdentity);
	CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * baseline));
	CGContextSetTextPosition(context, CGRectGetMaxX(rect) - CTLineGetTypographicBounds(line, NULL, NULL, NULL), baseline);
	CTLineDraw(line, context);
	CFRelease(line);

	CGContextRestoreGState(context);
}

- (void)drawRect:(NSRect)aRect
{
	[self.backgroundColor set];
	NSRectFill(NSIntersectionRect(aRect, self.frame));

	[self setupSelectionRects];

	[self.selectionBackgroundColor set];
	for(auto const& rect : backgroundRects)
		NSRectFillUsingOperation(NSIntersectionRect(rect, NSIntersectionRect(aRect, self.frame)), NSCompositeSourceOver);

	[self.selectionBorderColor set];
	for(auto const& rect : borderRects)
		NSRectFillUsingOperation(NSIntersectionRect(rect, NSIntersectionRect(aRect, self.frame)), NSCompositeSourceOver);

	std::pair<NSUInteger, NSUInteger> prevLine(NSNotFound, 0);
	for(CGFloat y = NSMinY(aRect); y < NSMaxY(aRect); )
	{
		GVLineRecord record = [self.delegate lineRecordForPosition:y];
		if(record.lastY <= y || prevLine == std::make_pair(record.lineNumber, record.softlineOffset))
			break;
		prevLine = std::make_pair(record.lineNumber, record.softlineOffset);

		BOOL selectedRow = NO;
		for(auto const& rect : backgroundRects)
			selectedRow = selectedRow || NSIntersectsRect(rect, NSMakeRect(0, record.firstY, CGRectGetWidth(self.frame), record.lastY - record.firstY));

		for(auto const& dataSource : [self visibleColumnDataSources])
		{
			NSRect columnRect = NSMakeRect(dataSource.x0, record.firstY, dataSource.width, record.lastY - record.firstY);
			if(dataSource.identifier == GVLineNumbersColumnIdentifier.UTF8String)
			{
				NSColor* textColor = selectedRow ? self.selectionForegroundColor : self.foregroundColor;
				DrawText(record.softlineOffset == 0 ? std::to_string(record.lineNumber + 1) : "·", columnRect, NSMinY(columnRect) + record.baseline, self.lineNumberFont, textColor);
			}
			else if(record.softlineOffset == 0)
			{
				BOOL isHoveringRect = NSMouseInRect(mouseHoveringAtPoint, columnRect, [self isFlipped]);
				BOOL isDownInRect   = NSMouseInRect(mouseDownAtPoint,     columnRect, [self isFlipped]);

				if(selectedRow && isDownInRect)        [self.selectionIconPressedColor set];
				else if(selectedRow && isHoveringRect) [self.selectionIconHoverColor   set];
				else if(selectedRow)                   [self.selectionIconColor        set];
				else if(isDownInRect)                  [self.iconPressedColor          set];
				else if(isHoveringRect)                [self.iconHoverColor            set];
				else                                   [self.iconColor                 set];

				NSImage* image = [self imageForColumn:dataSource.identifier atLine:record.lineNumber hovering:isHoveringRect && NSEqualPoints(mouseDownAtPoint, NSMakePoint(-1, -1)) pressed:isHoveringRect && isDownInRect];
				if([image size].height > 0 && [image size].width > 0)
				{
					// The placement of the center of image is aligned with the center of the capHeight.
					CGFloat center = record.baseline - ([self.lineNumberFont capHeight] / 2);
					CGFloat x = round((NSWidth(columnRect) - [image size].width) / 2);
					CGFloat y = round(center - ([image size].height / 2));
					NSRect imageRect = NSMakeRect(NSMinX(columnRect) + x, NSMinY(columnRect) + y, [image size].width, [image size].height);

					[NSGraphicsContext saveGraphicsState];

					NSAffineTransform* transform = [NSAffineTransform transform];
					[transform translateXBy:0 yBy:NSMaxY(imageRect)];
					[transform scaleXBy:1 yBy:-1];
					[transform concat];
					imageRect.origin.y = 0;

					CGImageRef cgImage = [image CGImageForProposedRect:NULL context:[NSGraphicsContext currentContext] hints:nil];
					CGImageRef imageMask = CGImageMaskCreate(CGImageGetWidth(cgImage), CGImageGetHeight(cgImage), CGImageGetBitsPerComponent(cgImage), CGImageGetBitsPerPixel(cgImage), CGImageGetBytesPerRow(cgImage), CGImageGetDataProvider(cgImage), NULL, false);
					CGContextClipToMask((CGContextRef)[[NSGraphicsContext currentContext] graphicsPort], imageRect, imageMask);
					CFRelease(imageMask);

					NSRectFillUsingOperation(imageRect, NSCompositeSourceOver);
					[NSGraphicsContext restoreGraphicsState];
				}
			}
		}

		y = record.lastY;
	}
}

- (void)updateSize
{
	static const CGFloat columnPadding = 1;

	CGFloat currentX = 0, totalWidth = 0;
	for(auto& it : columnDataSources)
	{
		it.x0 = currentX;
		if([self visibilityForColumnWithIdentifier:[NSString stringWithCxxString:it.identifier]])
		{
			it.width   = [self widthForColumnWithIdentifier:it.identifier];
			totalWidth += it.width + columnPadding;
			currentX   += it.width + columnPadding;
		}
		else
		{
			it.width = 0;
		}
	}

	NSPoint origin = NSMakePoint(0, NSMinY(_partnerView.enclosingScrollView.contentView.bounds));
	CGFloat height = std::max(NSHeight(_partnerView.frame), origin.y + NSHeight([self visibleRect]));
	[self setSize:NSMakeSize(totalWidth, height)];
}

- (NSSize)intrinsicContentSize
{
	return self.size;
}

- (void)reloadData:(id)sender
{
	D(DBF_GutterView, bug("\n"););
	[self updateSize];
	[self setNeedsDisplay:YES];
}

- (NSRect)columnRectForPoint:(NSPoint)aPoint
{
	GVLineRecord record = [self.delegate lineRecordForPosition:aPoint.y];
	if(record.lineNumber != NSNotFound && record.softlineOffset == 0)
	{
		for(auto const& dataSource : [self visibleColumnDataSources])
		{
			if(dataSource.identifier == [GVLineNumbersColumnIdentifier UTF8String])
				continue;

			NSRect columnRect = NSMakeRect(dataSource.x0, record.firstY, dataSource.width, record.lastY - record.firstY);
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
			GVLineRecord record = [self.delegate lineRecordForPosition:mouseDownAtPoint.y];
			for(auto const& dataSource : [self visibleColumnDataSources])
			{
				NSRect columnRect = NSMakeRect(dataSource.x0, record.firstY, dataSource.width, record.lastY - record.firstY);
				if(NSPointInRect(mouseDownAtPoint, columnRect))
					[dataSource.delegate userDidClickColumnWithIdentifier:[NSString stringWithCxxString:dataSource.identifier] atLine:record.lineNumber];
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
		[_partnerView mouseDown:event];
		while([event type] != NSLeftMouseUp)
		{
			event = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSMouseMovedMask|NSLeftMouseDraggedMask|NSMouseEnteredMask|NSMouseExitedMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
			if([event type] == NSMouseMoved || [event type] == NSLeftMouseDragged)
				[_partnerView mouseDragged:event];
		}
		[_partnerView mouseUp:event];
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
	[self updateSize];
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
	[self addTrackingArea:[[NSTrackingArea alloc] initWithRect:[self visibleRect] options:NSTrackingMouseEnteredAndExited|NSTrackingMouseMoved|NSTrackingActiveInKeyWindow owner:self userInfo:nil]];
}

- (void)scrollWheel:(NSEvent*)event
{
	[_partnerView scrollWheel:event];
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
