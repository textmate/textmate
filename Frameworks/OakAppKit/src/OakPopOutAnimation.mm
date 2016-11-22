#import "OakPopOutAnimation.h"
#import <oak/debug.h>

static CGFloat const kExtendWidth   = 4;
static CGFloat const kExtendHeight  = 1;
static CGFloat const kRectXRadius   = 2;
static CGFloat const kRectYRadius   = 2;
static CGFloat const kMaxScale      = 1.3;
static CGFloat const kShadowRadius  = 2;

static double const  kGrowStartTime  = 0.00;
static double const  kGrowFinishTime = 0.10;
static double const  kFadeStartTime  = 0.35;
static double const  kFadeFinishTime = 0.70;

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
@property (nonatomic) NSImage* contentImage;
- (id)initWithFrame:(NSRect)aRect popOutRect:(NSRect)popOutRect;
- (void)startAnimation:(id)sender;
@end

void OakShowPopOutAnimation (NSView* parentView, NSRect popOutRect, NSImage* anImage, BOOL hidePrevious)
{
	if(popOutRect.size.width == 0 || popOutRect.size.height == 0)
		return;

	static NSMutableSet<OakPopOutView*>* previousViews = [NSMutableSet set];

	if(hidePrevious)
	{
		CAAnimation* dummy = nullptr;
		for(OakPopOutView* view in previousViews)
			[view animationDidStop:dummy finished:YES];
		[previousViews removeAllObjects];
	}

	popOutRect = NSInsetRect(popOutRect, -kExtendWidth, -kExtendHeight);
	NSRect windowRect = popOutRect;
	CGFloat extraWidth = ceil((kMaxScale - 1) * (popOutRect.size.width + 4 * kShadowRadius)/2);
	CGFloat extraHeight = ceil((kMaxScale - 1) * (popOutRect.size.height + 4 * kShadowRadius)/2);
	windowRect.origin.x -= extraWidth; popOutRect.origin.x = extraWidth;
	windowRect.origin.y -= extraHeight; popOutRect.origin.y = extraHeight;
	windowRect.size.width += 2 * extraWidth;
	windowRect.size.height += 2 * extraHeight;

	NSWindow* window = [[NSWindow alloc] initWithContentRect:windowRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO];
	CFRetain((CFTypeRef)window); // isReleasedWhenClosed == YES
	[window setBackgroundColor:[NSColor clearColor]];
	[window setExcludedFromWindowsMenu:YES];
	[window setIgnoresMouseEvents:YES];
	[window setOpaque:NO];
	[[window contentView] setWantsLayer:YES];

	OakPopOutView* aView = [[OakPopOutView alloc] initWithFrame:[window contentView].bounds popOutRect:popOutRect];
	[aView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];

	[anImage lockFocus];
	[[NSColor blackColor] set];
	NSRectFillUsingOperation((NSRect){ NSZeroPoint, [anImage size] }, NSCompositeSourceAtop);
	[anImage unlockFocus];

	aView.contentImage = anImage;
	[[window contentView] addSubview:aView];

	if(NSScrollView* scrollView = parentView.enclosingScrollView)
		[[NSNotificationCenter defaultCenter] addObserver:aView selector:@selector(parentViewBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:scrollView.contentView];

	[window setFrame:[window frameRectForContentRect:windowRect] display:YES];
	[parentView.window addChildWindow:window ordered:NSWindowAbove];
	[previousViews addObject:aView];

	[aView startAnimation:nil];
}

@implementation OakPopOutView
- (id)initWithFrame:(NSRect)aRect popOutRect:(NSRect)popOutRect
{
	if(self = [super initWithFrame:aRect])
	{
		CGPathRef path;
		NSRect shapeRect = popOutRect;
		shapeRect.origin = CGPointZero;
		shapeRect = CGRectInset(shapeRect, 0.25, 0.25);
		BOOL rectTooSmallToBeRounded = NSWidth(shapeRect) < 2 * kRectXRadius || NSHeight(shapeRect) < 2 * kRectYRadius;
		if(rectTooSmallToBeRounded)
				path = CGPathCreateWithRect(shapeRect, NULL);
		else	path = CGPathCreateWithRoundedRect(shapeRect, kRectXRadius, kRectYRadius, NULL);

		[self setWantsLayer:YES];

		shapeLayer = [CAShapeLayer layer];
		shapeLayer.frame = popOutRect;
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

		CFRelease(path);
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
		grow.beginTime = kGrowStartTime;
		grow.duration = (kGrowFinishTime-kGrowStartTime)/2;
		grow.fromValue = @1;
		grow.toValue = @(kMaxScale);
		grow.autoreverses = YES;
		grow.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn];

		CABasicAnimation* fade = [CABasicAnimation animationWithKeyPath:@"opacity"];
		fade.beginTime = kFadeStartTime;
		fade.duration = kFadeFinishTime-kFadeStartTime;
		fade.fromValue = @1;
		fade.toValue = @0;
		fade.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];

		animationGroup = [CAAnimationGroup new];
		animationGroup.animations = @[grow, fade];
		animationGroup.duration = kFadeFinishTime;
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
	[shapeLayer removeAllAnimations]; // Releases the animation which holds a strong reference to its delegate (us)
	[[self window] close];
}

- (void)parentViewBoundsDidChange:(NSNotification*)notification
{
	[shapeLayer removeAllAnimations]; // Releases the animation which holds a strong reference to its delegate (us)
	[[self window] close];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}
@end
