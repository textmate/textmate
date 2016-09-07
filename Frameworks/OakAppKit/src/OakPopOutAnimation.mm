#import "OakPopOutAnimation.h"
#import <oak/debug.h>

static CGFloat const kExtendWidth  = 4;
static CGFloat const kExtendHeight = 1;
static CGFloat const kRectXRadius  = 2;
static CGFloat const kRectYRadius  = 2;
static CGFloat const kMaxScale     = 1.5;
static CGFloat const kShadowRadius = 2;
static double const  kGrowDuration = 0.05;
static double const  kFadeDuration = 0.50;

#if !defined(MAC_OS_X_VERSION_10_12) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12)
@interface OakPopOutView : NSView
#else
@interface OakPopOutView : NSView <CAAnimationDelegate>
#endif
{
	OBJC_WATCH_LEAKS(OakPopOutView);
	CALayer* imageLayer;
	CAShapeLayer* shapeLayer;
}
@property (nonatomic) NSImage*  contentImage;
@property (nonatomic) NSWindow* retainedWindow;
- (void)startAnimation:(id)sender;
@end

void OakShowPopOutAnimation (NSRect viewRect, NSImage* anImage)
{
	if(viewRect.size.width == 0 || viewRect.size.height == 0)
		return;

	viewRect = NSInsetRect(viewRect, -kExtendWidth, -kExtendHeight);
	NSRect windowRect = viewRect;
	CGFloat extraWidth = ceil((kMaxScale - 1) * (viewRect.size.width + 4 * kShadowRadius)/2);
	CGFloat extraHeight = ceil((kMaxScale - 1) * (viewRect.size.height + 4 * kShadowRadius)/2);
	windowRect.origin.x -= extraWidth; viewRect.origin.x = extraWidth;
	windowRect.origin.y -= extraHeight; viewRect.origin.y = extraHeight;
	windowRect.size.width += 2 * extraWidth;
	windowRect.size.height += 2 * extraHeight;

	NSWindow* window = [[NSWindow alloc] initWithContentRect:windowRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO];
	[window setBackgroundColor:[NSColor clearColor]];
	[window setExcludedFromWindowsMenu:YES];
	[window setIgnoresMouseEvents:YES];
	[window setLevel:NSStatusWindowLevel];
	[window setOpaque:NO];
	[window setReleasedWhenClosed:NO];
	[[window contentView] setWantsLayer:YES];

	OakPopOutView* aView = [[OakPopOutView alloc] initWithFrame:viewRect];
	[aView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
	aView.contentImage = anImage;
	aView.retainedWindow = window;
	[[window contentView] addSubview:aView];

	[window setFrame:[window frameRectForContentRect:windowRect] display:YES];
	[window orderFront:nil];

	[aView startAnimation:nil];
}

@implementation OakPopOutView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		CGPathRef path;
		NSRect rect = CGRectInset([self bounds], 0.25, 0.25);
		BOOL rectToSmallToBeRounded = NSWidth(rect) < 2 * kRectXRadius || NSHeight(rect) < 2 * kRectYRadius;
		if(rectToSmallToBeRounded || CGPathCreateWithRoundedRect == NULL) // MAC_OS_X_VERSION_10_9
				path = CGPathCreateWithRect(rect, NULL);
		else	path = CGPathCreateWithRoundedRect(rect, kRectXRadius, kRectYRadius, NULL);

		[self setWantsLayer:YES];
		self.layer.masksToBounds = NO;

		shapeLayer = [CAShapeLayer layer];
		shapeLayer.frame = self.bounds;
		shapeLayer.fillColor = [[NSColor yellowColor] CGColor];
		shapeLayer.strokeColor = [[NSColor colorWithCalibratedWhite:0 alpha:0.1] CGColor];
		shapeLayer.lineWidth = 0.5;
		shapeLayer.path = path;
		shapeLayer.shadowOpacity = 0.25;
		shapeLayer.shadowRadius = kShadowRadius;
		shapeLayer.shadowOffset = CGSizeMake(0, -1);
		[self.layer addSublayer:shapeLayer];

		imageLayer = [CALayer layer];
		[shapeLayer addSublayer:imageLayer];
	}
	return self;
}

- (void)viewDidMoveToWindow
{
	CGFloat scaleFactor = self.window.screen.backingScaleFactor;
	if(scaleFactor)
	{
		imageLayer.contents = [_contentImage layerContentsForContentsScale:scaleFactor];
		imageLayer.bounds = CGRectMake(0, 0, _contentImage.size.width, _contentImage.size.height);
		imageLayer.position = CGPointMake(CGRectGetMidX(shapeLayer.bounds), CGRectGetMidY(shapeLayer.bounds));
	}
}

- (void)startAnimation:(id)sender
{
	static CAAnimationGroup* animationGroup; // Animations are copied; we'll reuse one
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		CABasicAnimation* grow = [CABasicAnimation animationWithKeyPath:@"transform.scale"];
		grow.duration = kGrowDuration;
		grow.fromValue = @1;
		grow.toValue = @(kMaxScale);
		grow.autoreverses = YES;
		grow.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn];

		CABasicAnimation* fade = [CABasicAnimation animationWithKeyPath:@"opacity"];
		fade.beginTime = 2 * kGrowDuration;
		fade.duration = kFadeDuration;
		fade.fromValue = @1;
		fade.toValue = @0;
		fade.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];

		animationGroup = [CAAnimationGroup new];
		animationGroup.animations = @[grow, fade];
		animationGroup.duration = 2 * kGrowDuration + kFadeDuration;
		animationGroup.fillMode = kCAFillModeForwards;
		animationGroup.removedOnCompletion = NO;
	});

	animationGroup.delegate = self; // Listen for animationDidStop:finished:
	animationGroup.speed = 1;
	[shapeLayer addAnimation:animationGroup forKey:nil];
	animationGroup.delegate = nil;
}

- (void)animationDidStop:(CAAnimation*)theAnimation finished:(BOOL)flag
{
	[[self window] orderOut:self];
	self.retainedWindow = nil;
}
@end
