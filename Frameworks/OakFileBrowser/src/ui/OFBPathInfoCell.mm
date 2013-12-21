#import "OFBPathInfoCell.h"
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/OakTimer.h>
#import <Preferences/Keys.h>
#import <oak/debug.h>
#import "../io/FSItem.h"
#import <OakFoundation/NSString Additions.h>

static CGFloat kCloseButtonRightMargin = 5;

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
	ASSERT(labelColorIndex < 8);

	// color names: Gray, Green, Purple, Blue, Yellow, Red, Orange
	static NSString* const startCol[] = { @"#CFCFCF", @"#D4EE9C", @"#DDBDEA", @"#ACD0FE", @"#F8F79C", @"#FC999A", @"#F9D194" };
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
	return NSMakeRect(NSMaxX(cellFrame) - kCloseButtonRightMargin - self.closeIcon.size.width, NSMaxY(cellFrame) - (cellFrame.size.height + self.closeIcon.size.height) / 2, self.closeIcon.size.width, self.closeIcon.size.height);
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

		NSRect closeButtonRect = [self closeButtonRectInFrame:cellFrame];
		[closeIcon drawInRect:closeButtonRect fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
		cellFrame.size.width -= NSMaxX(cellFrame) - NSMinX(closeButtonRect);
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

	NSUInteger res = [super hitTestForEvent:event inRect:cellFrame ofView:controlView];
	if(([event type] == NSLeftMouseDown || [event type] == NSLeftMouseUp) && !([event modifierFlags] & (NSShiftKeyMask | NSControlKeyMask)))
	{
		if([event modifierFlags] & NSCommandKeyMask)
		{
			if(res & OakImageAndTextCellHitImage)
				res |= OFBPathInfoCellHitRevealItem;
		}
		else
		{
			BOOL clickTextToOpen = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFileBrowserSingleClickToOpenKey];
			if((res & OakImageAndTextCellHitImage) && !clickTextToOpen)
				res |= OFBPathInfoCellHitOpenItem;
			else if((res & OakImageAndTextCellHitText) && clickTextToOpen)
				res |= OFBPathInfoCellHitOpenItem;
		}
	}
	return res;
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

// =================
// = Accessibility =
// =================

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityDescriptionAttribute,
		NSAccessibilityHelpAttribute,
		NSAccessibilityURLAttribute,
		NSAccessibilityFilenameAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	if(self.representedObject && [self.representedObject isKindOfClass:[FSItem class]])
	{
		static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
		return attributes;
	}
	return [super accessibilityAttributeNames];
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if(self.representedObject && [self.representedObject isKindOfClass:[FSItem class]])
	{
		FSItem* item = (FSItem*)self.representedObject;
		if([attribute isEqualToString:NSAccessibilityDescriptionAttribute])
		{
			NSMutableArray* descriptions = [NSMutableArray arrayWithCapacity:0];
			NSString* type = [@{
				@(FSItemURLTypeUnknown): @"unknown type",
				@(FSItemURLTypeFile):    @"file",
				@(FSItemURLTypeFolder):  @"folder",
				@(FSItemURLTypePackage): @"package",
				@(FSItemURLTypeAlias):   @"alias",
				@(FSItemURLTypeMissing): @"missing",
			} objectForKey:@(item.urlType)];

			NSString* scmStatus = [NSString stringWithCxxString:scm::status::to_s(item.icon.scmStatus)];
			if(item.icon.scmStatus == scm::status::unknown)
				scmStatus = @"not versioned";

			if(type)
				[descriptions addObject:type];
			if(scmStatus)
				[descriptions addObject:scmStatus];
			if(self.isOpen)
				[descriptions addObject:@"open"];

			return [descriptions componentsJoinedByString:@", "];
		}
		else if([attribute isEqualToString:NSAccessibilityHelpAttribute])
			return item.toolTip;
		else if([attribute isEqualToString:NSAccessibilityURLAttribute])
			return item.url;
		else if([attribute isEqualToString:NSAccessibilityFilenameAttribute])
			return item.name;
	}
	return [super accessibilityAttributeValue:attribute];
}
@end
