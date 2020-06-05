#import "OakZoomingIcon.h"

@implementation OakZoomingIcon
- (id)initWithIcon:(NSImage*)icon rect:(NSRect)aRect
{
	if(self = [super initWithContentRect:NSInsetRect(aRect, -56, -56) styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO])
	{
		self.releasedWhenClosed = NO;
		self.ignoresMouseEvents = YES;
		self.backgroundColor    = [NSColor clearColor];
		self.opaque             = NO;
		self.level              = NSPopUpMenuWindowLevel;

		NSView* view = self.contentView;
		view.wantsLayer = YES;

		CALayer* layer = [CALayer layer];
		[view.layer addSublayer:layer];

		NSImage* image = [icon copy];
		image.size = view.bounds.size;

		layer.bounds   = { NSZeroPoint, aRect.size };
		layer.position = NSMakePoint(NSMidX(view.bounds), NSMidY(view.bounds));
		layer.contents = image;

		[self orderFront:self];

		// Layer properties changed in this run loop cycle will not be animated
		[self performSelector:@selector(runAnimation:) withObject:layer afterDelay:0];
	}
	return self;
}

- (void)runAnimation:(CALayer*)layer
{
	CGFloat duration = 0.25 * ((NSApp.currentEvent.modifierFlags & NSEventModifierFlagShift) ? 10 : 1);

	[CATransaction begin];
	[CATransaction setAnimationDuration:duration];
	[CATransaction setAnimationTimingFunction:[CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn]];

	[CATransaction setCompletionBlock:^{
		[self close];
	}];

	layer.bounds  = self.contentView.bounds;
	layer.opacity = 0;

	[CATransaction commit];
}

+ (OakZoomingIcon*)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect
{
	return [[OakZoomingIcon alloc] initWithIcon:icon rect:aRect];
}
@end
