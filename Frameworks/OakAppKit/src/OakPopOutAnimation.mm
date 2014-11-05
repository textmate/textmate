#import "OakPopOutAnimation.h"
#import <oak/algorithm.h>
#import <oak/debug.h>

static CGFloat const kExtendWidth  = 6;
static CGFloat const kExtendHeight = 1;
static CGFloat const kRectXRadius  = 6;
static CGFloat const kRectYRadius  = 6;
static double const  kGrowDuration = 0.10;
static double const  kFadeDuration = 0.60;

@interface OakPopOutView : NSView
{
	OBJC_WATCH_LEAKS(OakPopOutView);

	NSRect baseFrame;
	double growDuration;
	double fadeDuration;
}
@property (nonatomic) NSDate*   animationStartTime;
@property (nonatomic) NSImage*  contentImage;
@property (nonatomic) NSWindow* retainedWindow;
- (void)startAnimation:(id)sender;
@end

void OakShowPopOutAnimation (NSRect aRect, NSImage* anImage)
{
	if(aRect.size.width == 0 || aRect.size.height == 0)
		return;

	aRect = NSInsetRect(aRect, -kExtendWidth, -kExtendHeight);
	NSRect contentRect = NSMakeRect(0, 0, NSWidth(aRect), NSHeight(aRect));
	NSWindow* window = [[NSWindow alloc] initWithContentRect:contentRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO];
	[window setAlphaValue:1];
	[window setBackgroundColor:[NSColor clearColor]];
	[window setExcludedFromWindowsMenu:YES];
	[window setHasShadow:YES];
	[window setIgnoresMouseEvents:YES];
	[window setLevel:NSStatusWindowLevel];
	[window setOpaque:NO];
	[window setReleasedWhenClosed:NO];
	[window useOptimizedDrawing:YES];

	OakPopOutView* aView = [[OakPopOutView alloc] initWithFrame:contentRect];
	[aView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
	aView.contentImage = anImage;
	aView.retainedWindow = window;
	[[window contentView] addSubview:aView];

	[window setFrame:aRect display:YES];
	[window orderFront:nil];

	[aView startAnimation:nil];
}

static double bounce_curve (double t)
{
	return 1 - sqrt( 1 - pow((1 - fabs(2*t - 1)), 2) );
	// return 1 - fabs( 2 * (1 - (1.0 / (1.0 + exp(-12*t + 6)))) - 1);
}

@implementation OakPopOutView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		double const slowDownFactor = 1; // ([[NSApp currentEvent] modifierFlags] & (NSCommandKeyMask|NSAlternateKeyMask|NSControlKeyMask|NSShiftKeyMask)) == NSShiftKeyMask ? 6 : 1;
		growDuration = kGrowDuration * slowDownFactor;
		fadeDuration = kFadeDuration * slowDownFactor;
	}
	return self;
}

- (void)startAnimation:(id)sender
{
	baseFrame = [[self window] frame];
	self.animationStartTime = [NSDate date];
	[NSTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(animationTick:) userInfo:nil repeats:YES];
}

- (void)animationTick:(NSTimer*)aTimer
{
	double const totalDuration = 2*growDuration + fadeDuration;

	CGFloat alpha = 1.0;
	CGFloat grow  = 0.0;

	double t = [[NSDate date] timeIntervalSinceDate:self.animationStartTime];
	if(t > totalDuration)
	{
		[aTimer invalidate];
		[[self window] orderOut:self];
		self.retainedWindow = nil;
		return;
	}
	else if(t > 2*growDuration)
	{
		t = (t - 2*growDuration) * 1.0/fadeDuration;
		alpha = 0.97 * (1 - oak::slow_in_out(t));
	}
	else
	{
		t = t * 1.0/(2*growDuration);
		grow = bounce_curve(t);
	}

	grow = 1 + grow / 2;
	CGFloat w = NSWidth(baseFrame)  * grow;
	CGFloat h = NSHeight(baseFrame) * grow;
	CGFloat x = round(NSMidX(baseFrame) - w/2);
	CGFloat y = round(NSMidY(baseFrame) - h/2);
	[[self window] setFrame:NSMakeRect(x, y, round(w), round(h)) display:YES];
	[[self window] setAlphaValue:alpha];
}

- (BOOL)isFlipped             { return YES; }
- (BOOL)isOpaque              { return NO; }

- (void)drawRect:(NSRect)aRect
{
	[[NSColor clearColor] set];
	NSRectFill(aRect);

	NSBezierPath* roundedRect = [NSBezierPath bezierPathWithRoundedRect:[self bounds] xRadius:kRectXRadius yRadius:kRectYRadius];
	[[NSColor yellowColor] set];
	[roundedRect fill];
	[[NSColor whiteColor] set];
	[roundedRect stroke];

	[self.contentImage drawInRect:NSInsetRect([self bounds], kExtendWidth, kExtendHeight) fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1 respectFlipped:YES hints:nil];

	[super drawRect:aRect];
}
@end
