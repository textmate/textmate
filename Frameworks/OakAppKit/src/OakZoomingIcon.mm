#import "OakZoomingIcon.h"

@implementation OakZoomingIcon
- (id)initWithIcon:(NSImage*)icon rect:(NSRect)aRect
{
	if(self = [super initWithContentRect:aRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO])
	{
		NSImageView* imageView = [[[NSImageView alloc] initWithFrame:aRect] autorelease];
		[imageView setImage:icon];
		[imageView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		[self setContentView:imageView];

		startFrame = aRect;
		startTime = [NSDate new];
		duration = ([[NSApp currentEvent] modifierFlags] & NSShiftKeyMask) ? 4.0 : 0.4;
		animationTimer = [[NSTimer scheduledTimerWithTimeInterval:1.0/60.0 target:self selector:@selector(tick:) userInfo:nil repeats:YES] retain];
		[self setOpaque:NO];
		[self setBackgroundColor:[NSColor clearColor]];
		[self setLevel:NSPopUpMenuWindowLevel];
		[self setIgnoresMouseEvents:YES];
		[self orderFront:self];
		[self retain];
	}
	return self;
}

+ (void)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect
{
	[[[OakZoomingIcon alloc] initWithIcon:icon rect:aRect] autorelease];
}

- (void)dealloc
{
	[startTime release];
	[animationTimer release];
	[super dealloc];
}

- (void)tick:(NSTimer*)aTimer
{
	NSTimeInterval delta = -[startTime timeIntervalSinceNow];
	CGFloat t = std::min(1.0, delta / duration);

	t = log2(5.0*(t) + 1.0) / log2(6.0);
	[self setAlphaValue:1.0 - t/1.2];

	CGFloat size = 16 + t * (128-16);
	NSRect r = NSMakeRect(NSMidX(startFrame) - 0.5 * size, NSMidY(startFrame) - 0.5 * size, size, size);
	CGFloat offset = round(0.5 * size);
	r = NSMakeRect(
		round(NSMidX(startFrame) - offset),
		round(NSMidY(startFrame) - offset),
		(2 * offset), (2 * offset));
	[self setFrame:r display:YES];

	if(delta > duration)
	{
		[animationTimer invalidate];
		[self close];
	}
}
@end
