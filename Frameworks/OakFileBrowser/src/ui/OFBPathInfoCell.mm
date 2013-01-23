#import "OFBPathInfoCell.h"
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakImage.h>
#import <OakFoundation/OakTimer.h>
#import <oak/debug.h>

static void DrawSpinner (NSRect cellFrame, BOOL isFlipped, NSColor* color, double value)
{
	static const CGFloat deg2rad = 0.017453292519943295;

	CGFloat const cellSize = 16;
	CGFloat const strokeWidth = cellSize*0.08;
	CGFloat const outerRadius = cellSize*0.48;
	CGFloat const innerRadius = cellSize*0.27;
	NSPoint center = cellFrame.origin;
	center.x += cellSize/2.0;
	center.y += cellFrame.size.height/2.0;
	CGFloat a = (270+(round(value/(5.0/60.0)) * 30))*deg2rad;
	for(NSUInteger i = 0; i < 12; i++)
	{
		[[color colorWithAlphaComponent:1.0-sqrt(i)*0.25] set];
		NSBezierPath* path = [NSBezierPath bezierPath];
		[path moveToPoint:NSMakePoint(center.x+cos(a)*outerRadius, center.y+sin(a)*outerRadius)];
		[path lineToPoint:NSMakePoint(center.x+cos(a)*innerRadius, center.y+sin(a)*innerRadius)];
		[path setLineCapStyle:NSRoundLineCapStyle];
		[path setLineWidth:strokeWidth];
		[path stroke];
		a -= 30*deg2rad;
	}
}

@interface OFBPathInfoCell ()
{
	OBJC_WATCH_LEAKS(OFBPathInfoCell);
	double spinnerValue;
}
@property (nonatomic, retain) OakTimer* spinTimer;
@property (nonatomic, assign) BOOL mouseDownInCloseButton;
- (BOOL)isMouseInCloseButtonInFrame:(NSRect)cellFrame controlView:(NSView*)controlView;
@end

@implementation OFBPathInfoCell
- (id)copyWithZone:(NSZone*)aZone
{
	OFBPathInfoCell* res = [super copyWithZone:aZone];
	DB(new(&res->_instance_counter_helper) watch_leaks_OFBPathInfoCell(_instance_counter_helper));
	res.spinTimer = nil;
	return res;
}

- (void)drawLabelIndex:(NSUInteger)labelColorIndex inFrame:(NSRect)cellFrame
{
	if(labelColorIndex == 0)
		return;
	ASSERT(labelColorIndex < 8)

	// color names: Gray, Green, Purple, Blue, Yellow, Red, Orange
	static NSString* const startCol[] = { @"#CFCFCF", @"#D4EE9C", @"#DDBDEA", @"#ACD0FE", @"#F8F79C", @"#B2B2B2", @"#F9D194" };
	static NSString* const stopCol[]  = { @"#A8A8A8", @"#AFDC49", @"#C186D7", @"#5B9CFE", @"#ECDF4A", @"#FC605C", @"#F6AC46" };

	NSRect r = NSIntegralRect(NSInsetRect(cellFrame, 2, 0));
	if([self isHighlighted])
		r.size.height = r.size.width = 16;

	NSGradient* gradient = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithString:startCol[labelColorIndex-1]] endingColor:[NSColor colorWithString:stopCol[labelColorIndex-1]]];
	NSBezierPath* path = [NSBezierPath bezierPathWithRoundedRect:r xRadius:8.0 yRadius:8.0];
	[gradient drawInBezierPath:path angle:90];
}

- (NSImage*)closeIcon
{
	return self.isOpen ? [NSImage imageNamed:@"CloseFile" inSameBundleAsClass:[OFBPathInfoCell class]] : nil;
}

- (NSRect)closeButtonRectInFrame:(NSRect)cellFrame
{
	if(!self.isOpen)
		return NSZeroRect;
	return NSMakeRect(NSMaxX(cellFrame) - self.closeIcon.size.width, NSMaxY(cellFrame) - (cellFrame.size.height + self.closeIcon.size.height) / 2, self.closeIcon.size.width, self.closeIcon.size.height);
}

- (void)redrawFrame:(OakTimer*)timer
{
	[timer.userInfo setNeedsDisplay:YES];
	spinnerValue += 0.1;
	self.spinTimer = nil;
}

- (BOOL)isMouseInCloseButtonInFrame:(NSRect)cellFrame controlView:(NSView*)controlView
{
	NSPoint mousePoint = [controlView convertPoint:[controlView.window mouseLocationOutsideOfEventStream] fromView:nil];
	return NSMouseInRect(mousePoint, [self closeButtonRectInFrame:cellFrame], controlView.isFlipped);
}

- (void)drawInteriorWithFrame:(NSRect)cellFrame inView:(NSView*)controlView
{
	BOOL wasHighlighted = self.isHighlighted;
	if(self.disableHighlight)
		self.highlighted = NO;
	[super drawInteriorWithFrame:cellFrame inView:controlView];
	self.highlighted = wasHighlighted;
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView*)controlView
{
	if([controlView respondsToSelector:@selector(indentationPerLevel)])
	{
		CGFloat const extra = [(NSOutlineView*)controlView indentationPerLevel];
		NSRect labelFrame = cellFrame;
		labelFrame.origin.x -= extra;
		labelFrame.size.width += extra;
		[self drawLabelIndex:_labelIndex inFrame:labelFrame];
	}

	if(self.isLoading)
	{
		if(!self.spinTimer)
			self.spinTimer = [OakTimer scheduledTimerWithTimeInterval:1/12 target:self selector:@selector(redrawFrame:) userInfo:controlView repeats:NO];
		NSRect spinnerFrame = cellFrame;
		spinnerFrame.origin.x += NSWidth(spinnerFrame) - 16;
		spinnerFrame.size = NSMakeSize(16, 16);
		cellFrame.size.width -= spinnerFrame.size.width;
		DrawSpinner(spinnerFrame, controlView.isFlipped, self.isHighlighted ? [NSColor whiteColor] : [NSColor blackColor], spinnerValue);
	}
	else if(self.isOpen)
	{
		NSImage* closeIcon = self.closeIcon;
		if(_mouseDownInCloseButton)
			closeIcon = [NSImage imageNamed:@"CloseFilePressed" inSameBundleAsClass:[OFBPathInfoCell class]];
		else if([self isMouseInCloseButtonInFrame:cellFrame controlView:controlView] && [[controlView window] isKeyWindow])
			closeIcon = [NSImage imageNamed:@"CloseFileOver" inSameBundleAsClass:[OFBPathInfoCell class]];
		[closeIcon drawInRect:[self closeButtonRectInFrame:cellFrame] fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
		cellFrame.size.width -= self.closeIcon.size.width;
	}

	NSFont* unboldFont = self.font;
	if(self.isVisible)
		self.font = [[NSFontManager sharedFontManager] convertFont:self.font toHaveTrait:NSBoldFontMask];

	[super drawWithFrame:cellFrame inView:controlView];

	self.font = unboldFont;
}

- (void)selectWithFrame:(NSRect)aRect inView:(NSView*)aView editor:(NSText*)aText delegate:(id)anId start:(NSInteger)start length:(NSInteger)length
{
	if(NSString* basename = [self.stringValue stringByDeletingPathExtension])
	{
		start  = 0;
		length = basename.length;
	}
	[super selectWithFrame:aRect inView:aView editor:aText delegate:anId start:start length:length];
}

// ============
// = Tracking =
// ============

+ (BOOL)prefersTrackingUntilMouseUp
{
	return YES;
}

- (NSUInteger)hitTestForEvent:(NSEvent*)event inRect:(NSRect)cellFrame ofView:(NSView*)controlView
{
	NSPoint point = [controlView convertPoint:([event window] ? [event locationInWindow] : [[controlView window] convertScreenToBase:[event locationInWindow]]) fromView:nil];

	if(NSMouseInRect(point, [self closeButtonRectInFrame:cellFrame], [controlView isFlipped]))
		return NSCellHitContentArea | NSCellHitTrackableArea | OFBPathInfoCellHitCloseButton;

	return [super hitTestForEvent:event inRect:cellFrame ofView:controlView];
}

- (BOOL)trackMouse:(NSEvent*)theEvent inRect:(NSRect)cellFrame ofView:(NSView*)controlView untilMouseUp:(BOOL)untilMouseUp
{
	NSPoint mousePos = [controlView convertPoint:[theEvent locationInWindow] fromView:nil];
	if(!NSMouseInRect(mousePos, [self closeButtonRectInFrame:cellFrame], [controlView isFlipped]))
		return NO;

	while([theEvent type] != NSLeftMouseUp)
	{
		mousePos = [controlView convertPoint:[theEvent locationInWindow] fromView:nil];
		if(NSMouseInRect(mousePos, [self closeButtonRectInFrame:cellFrame], [controlView isFlipped]) != _mouseDownInCloseButton)
		{
			_mouseDownInCloseButton = !_mouseDownInCloseButton;
			[controlView setNeedsDisplayInRect:[self closeButtonRectInFrame:cellFrame]];
		}
		theEvent = [NSApp nextEventMatchingMask:(NSLeftMouseDraggedMask|NSMouseMovedMask|NSLeftMouseUpMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
	}

	_mouseDownInCloseButton = NO;
	[controlView setNeedsDisplayInRect:[self closeButtonRectInFrame:cellFrame]];
	return YES;
}
@end
