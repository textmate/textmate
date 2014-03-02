#import "FFFilePathCell.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <text/tokenize.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import "attr_string.h"
#import <io/path.h>

static NSImage* ImageForBadgeCounter (NSInteger count)
{
	static const NSInteger BADGE_BUFFER_LEFT         = 4;
	static const NSInteger BADGE_BUFFER_SIDE         = 3;
	static const NSInteger BADGE_BUFFER_LEFT_SMALL   = 3;
	static const NSInteger BADGE_CIRCLE_BUFFER_RIGHT = 5;
	static const NSInteger BADGE_TEXT_HEIGHT         = 14;
	static const NSInteger BADGE_X_RADIUS            = 7;
	static const NSInteger BADGE_Y_RADIUS            = 8;
	static const NSInteger BADGE_TEXT_MINI           = 8;
	static const NSInteger BADGE_TEXT_SMALL          = 20;

	if(count == 0)
		return nil;

	static NSDictionary* attrs = [[NSMutableDictionary alloc] initWithObjectsAndKeys:
	    [NSFont fontWithName:@"Helvetica-Bold" size:11], NSFontAttributeName,
	    [NSColor whiteColor], NSForegroundColorAttributeName,
	    nil];

	NSString* badgeText = [NSString stringWithFormat:@"%ld", count];
	NSSize badgeNumSize = [badgeText sizeWithAttributes:nil];

	NSInteger badgeWidth = badgeNumSize.width + BADGE_BUFFER_SIDE * 2;
	if(badgeNumSize.width < BADGE_TEXT_MINI)
		badgeWidth = BADGE_TEXT_SMALL;
	NSInteger badgeX    = BADGE_CIRCLE_BUFFER_RIGHT;
	NSInteger badgeNumX = badgeX + BADGE_BUFFER_LEFT;
	if(badgeNumSize.width < BADGE_TEXT_MINI)
		badgeNumX += BADGE_BUFFER_LEFT_SMALL;
	NSRect badgeRect = NSMakeRect(badgeX, 0, badgeWidth, BADGE_TEXT_HEIGHT);

	NSImage* badge = [[NSImage alloc] initWithSize:NSMakeSize(badgeRect.size.width + BADGE_X_RADIUS, badgeRect.size.height)];

	[badge lockFocus];

	NSBezierPath *badgePath = [NSBezierPath bezierPathWithRoundedRect:badgeRect xRadius:BADGE_X_RADIUS yRadius:BADGE_Y_RADIUS];
	[[NSColor disabledControlTextColor] set];
	[badgePath fill];
	[badgeText drawAtPoint:NSMakePoint(badgeNumX, 0) withAttributes:attrs];

	[badge unlockFocus];

	return badge;
}

static NSAttributedString* PathComponentString (std::string const& path, std::string const& base)
{
	std::vector<std::string> components;
	std::string str = path::relative_to(path, base);
	for(auto const& component : text::tokenize(str.begin(), str.end(), '/'))
		components.push_back(component);
	if(components.front() == "")
		components.front() = path::display_name("/");
	components.back() = "";

	return ns::attr_string_t()
		<< ns::style::line_break(NSLineBreakByTruncatingMiddle)
		<< [NSFont systemFontOfSize:11]
		<< [NSColor darkGrayColor]
		<< text::join(std::vector<std::string>(components.begin(), components.end()), " ▸ ")
		<< [NSFont boldSystemFontOfSize:11]
		<< [NSColor blackColor]
		<< (path::is_absolute(path) ? path::display_name(path) : path);
}

@interface FFFilePathCell ()
{
	BOOL mouseDownInIcon;
}
@end

@implementation FFFilePathCell
- (NSRect)iconFrameInCellFrame:(NSRect)cellFrame
{
	NSRect iconRect, pathRect;
	NSDivideRect(NSInsetRect(cellFrame, 5, 0), &iconRect, &pathRect, NSWidth(cellFrame) / 2, NSMinXEdge);
	iconRect.origin.y = iconRect.origin.y + (iconRect.size.height - 16) / 2;
	iconRect.size = NSMakeSize(16, 16);
	return iconRect;
}

- (void)drawWithFrame:(NSRect)frame inView:(NSView*)view
{
	NSAttributedString* fileString = PathComponentString(to_s(self.path), to_s(self.base));

	NSPoint iconOrigin = frame.origin;
	iconOrigin.x += 5;
	iconOrigin.y  = frame.origin.y + (frame.size.height - self.icon.size.height) / 2;
	[self.icon drawAdjustedAtPoint:iconOrigin fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:mouseDownInIcon ? 0.5 : 1.0];

	NSRect textFrame      = NSInsetRect(frame, 5, 0);
	textFrame.origin.y   += (textFrame.size.height - fileString.size.height) / 2;
	textFrame.origin.x   += 20;
	textFrame.size.width -= 21;

	if(NSImage* badge = ImageForBadgeCounter(self.count))
	{
		NSPoint badgeOrigin = textFrame.origin;
		badgeOrigin.x += (([fileString size].width < textFrame.size.width) ? [fileString size].width : textFrame.size.width) + 5;
		[badge drawAdjustedAtPoint:badgeOrigin fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1.0];
		textFrame.size.width -= badge.size.width + 5;
	}

	[fileString drawInRect:textFrame];
}

- (BOOL)trackMouse:(NSEvent*)theEvent inRect:(NSRect)cellFrame ofView:(NSView*)controlView untilMouseUp:(BOOL)untilMouseUp
{
	NSPoint initialMouseDown = [controlView convertPoint:[theEvent locationInWindow] fromView:nil];
	if(!NSMouseInRect(initialMouseDown, [self iconFrameInCellFrame:cellFrame], [controlView isFlipped]))
		return NO;

	mouseDownInIcon = YES;
	[controlView setNeedsDisplayInRect:cellFrame];

	theEvent = [NSApp nextEventMatchingMask:(NSLeftMouseDraggedMask|NSLeftMouseUpMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];

	mouseDownInIcon = NO;
	[controlView setNeedsDisplayInRect:cellFrame];

	if([theEvent type] == NSLeftMouseDragged)
		[controlView dragFile:self.path fromRect:[self iconFrameInCellFrame:cellFrame] slideBack:YES event:theEvent];

	return YES;
}
@end
