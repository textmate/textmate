#import "OakImageAndTextCell.h"
#import "NSImage Additions.h"

@implementation OakImageAndTextCell
- (id)copyWithZone:(NSZone*)zone
{
	OakImageAndTextCell* cell = [super copyWithZone:zone];
	cell.image = self.image;
	return cell;
}

- (NSRect)imageFrameWithFrame:(NSRect)aRect inControlView:(NSView*)aView
{
	aRect.size = self.image.size;
	aRect.origin.y += 1;
	aRect.origin.x += 8;
	if([aView respondsToSelector:@selector(intercellSpacing)])
		aRect.origin.y -= [(NSOutlineView*)aView intercellSpacing].height / 2;
	return aRect;
}

- (NSRect)textFrameWithFrame:(NSRect)aRect inControlView:(NSView*)aView
{
	NSRect imageFrame = [self imageFrameWithFrame:aRect inControlView:aView];
	NSRect textFrame = aRect;
	textFrame.origin.x = NSMaxX(imageFrame) + 4;
	textFrame.size.width = NSMaxX(aRect) - NSMinX(textFrame);
	return textFrame;
}

- (void)editWithFrame:(NSRect)aRect inView:(NSView*)controlView editor:(NSText*)textObj delegate:(id)anObject event:(NSEvent*)theEvent
{
	[super editWithFrame:[self textFrameWithFrame:aRect inControlView:controlView] inView:controlView editor:textObj delegate:anObject event:theEvent];
}

- (void)selectWithFrame:(NSRect)aRect inView:(NSView*)controlView editor:(NSText*)textObj delegate:(id)anObject start:(NSInteger)selStart length:(NSInteger)selLength
{
	[super selectWithFrame:[self textFrameWithFrame:aRect inControlView:controlView] inView:controlView editor:textObj delegate:anObject start:selStart length:selLength];
}

- (NSRect)expansionFrameWithFrame:(NSRect)cellFrame inView:(NSView*)view
{
	NSRect frame = [super expansionFrameWithFrame:[self textFrameWithFrame:cellFrame inControlView:view] inView:view];
	frame.size.width -= self.image ? [self.image size].width + 3 : 0;
	return frame;
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView*)controlView
{
	if(self.image)
	{
		NSRect imageRect = [self imageFrameWithFrame:cellFrame inControlView:controlView];
		if([self drawsBackground])
		{
			[[self backgroundColor] set];
			NSRectFill(imageRect);
		}
		[self.image drawAdjustedInRect:imageRect fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
	}

	[super drawWithFrame:[self textFrameWithFrame:cellFrame inControlView:controlView] inView:controlView];
}

- (NSSize)cellSize
{
	NSSize cellSize = [super cellSize];
	cellSize.width += self.image ? [self.image size].width + 3 : 0;
	return cellSize;
}

- (NSUInteger)hitTestForEvent:(NSEvent*)event inRect:(NSRect)cellFrame ofView:(NSView*)controlView
{
	NSRect imageRect = [self imageFrameWithFrame:cellFrame inControlView:controlView];
	NSRect textRect  = [self textFrameWithFrame:cellFrame inControlView:controlView];
	NSPoint point    = [controlView convertPoint:([event window] ? [event locationInWindow] : [[controlView window] convertScreenToBase:[event locationInWindow]]) fromView:nil];

	NSUInteger res = NSCellHitContentArea;
	if(NSMouseInRect(point, imageRect, controlView.isFlipped))
		res = NSCellHitContentArea|OakImageAndTextCellHitImage;
	else if(NSMouseInRect(point, textRect, controlView.isFlipped))
		res = NSCellHitContentArea|NSCellHitEditableTextArea|OakImageAndTextCellHitText;
	return res;
}
@end
