#import "OakZoomingIcon.h"

@interface OakZoomingIcon ()
{
	OBJC_WATCH_LEAKS(OakZoomingIcon);
}
@end

@implementation OakZoomingIcon
- (id)initWithIcon:(NSImage*)icon rect:(NSRect)aRect
{
	if(self = [super initWithContentRect:aRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO])
	{
		self.releasedWhenClosed = NO;
		self.ignoresMouseEvents = YES;
		self.backgroundColor    = [NSColor clearColor];
		self.opaque             = NO;
		self.level              = NSPopUpMenuWindowLevel;

		icon = [icon copy];
		[icon setSize:NSMakeSize(128, 128)];

		NSImageView* imageView = [[NSImageView alloc] initWithFrame:aRect];
		[imageView setImage:icon];
		[imageView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		self.contentView = imageView;

		[self orderFront:self];

		[NSAnimationContext beginGrouping];

		[NSAnimationContext currentContext].duration = ([[NSApp currentEvent] modifierFlags] & NSShiftKeyMask) ? 2.5 : 0.25;
		[NSAnimationContext currentContext].completionHandler = ^{
			[self orderOut:self];
		};

		[self.animator setAlphaValue:0];
		[self.animator setFrame:NSInsetRect(aRect, -56, -56) display:YES animate:YES];

		[NSAnimationContext endGrouping];
	}
	return self;
}

+ (OakZoomingIcon*)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect
{
	return [[OakZoomingIcon alloc] initWithIcon:icon rect:aRect];
}
@end
