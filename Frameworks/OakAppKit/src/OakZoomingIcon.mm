#import "OakZoomingIcon.h"

@interface OakZoomingIcon ()
{
	OBJC_WATCH_LEAKS(OakZoomingIcon);
}
@property (nonatomic) NSRect         startFrame;
@property (nonatomic) NSDate*        startTime;
@property (nonatomic) NSTimeInterval duration;
@property (nonatomic) NSTimer*       animationTimer;
@end

@implementation OakZoomingIcon
- (id)initWithIcon:(NSImage*)icon rect:(NSRect)aRect
{
	if(self = [super initWithContentRect:aRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO])
	{
		self.startFrame     = aRect;
		self.startTime      = [NSDate new];
		self.duration       = ([[NSApp currentEvent] modifierFlags] & NSShiftKeyMask) ? 4.0 : 0.4;
		self.animationTimer = [NSTimer scheduledTimerWithTimeInterval:1.0/60.0 target:self selector:@selector(tick:) userInfo:nil repeats:YES];

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
	}
	return self;
}

+ (OakZoomingIcon*)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect
{
	return [[OakZoomingIcon alloc] initWithIcon:icon rect:aRect];
}

- (void)tick:(NSTimer*)aTimer
{
	NSTimeInterval delta = [[NSDate date] timeIntervalSinceDate:self.startTime];
	CGFloat t = std::min(1.0, delta / self.duration);

	t = log2(5.0*(t) + 1.0) / log2(6.0);
	[self setAlphaValue:1.0 - t/1.2];

	CGFloat size = 16 + t * (128-16);
	NSRect r = NSMakeRect(NSMidX(self.startFrame) - 0.5 * size, NSMidY(self.startFrame) - 0.5 * size, size, size);
	CGFloat offset = round(0.5 * size);
	r = NSMakeRect(
		round(NSMidX(self.startFrame) - offset),
		round(NSMidY(self.startFrame) - offset),
		(2 * offset), (2 * offset));
	[self setFrame:r display:YES];

	if(delta > self.duration)
	{
		[self close];
		[self.animationTimer invalidate];
		self.animationTimer = nil;
	}
}
@end
