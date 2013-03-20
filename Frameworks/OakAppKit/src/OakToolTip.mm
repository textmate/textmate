#import "OakToolTip.h"
#import <oak/debug.h>
#import <oak/oak.h>

OAK_DEBUG_VAR(OakToolTip);

void OakShowToolTip (NSString* msg, NSPoint location)
{
	if(msg)
	{
		OakToolTip* toolTip = [OakToolTip new];
		[toolTip setStringValue:msg];

		// Find the screen which we are displaying on
		NSScreen* screen = nil;
		for(NSScreen* candidate in [NSScreen screens])
		{
			if(NSPointInRect(location, [candidate frame]))
			{
				screen = candidate;
				break;
			}
		}

		[toolTip showAtLocation:location forScreen:screen];
	}
}

@interface OakToolTip ()
{
	OBJC_WATCH_LEAKS(OakToolTip);

	NSTextField* field;

	NSDate* didOpenAtDate; // ignore mouse moves for the next second
	NSPoint mousePositionWhenOpened;
	BOOL enforceMouseThreshold;
}
@property (nonatomic) NSTimer* animationTimer;
@property (nonatomic) NSDate* animationStart;
- (void)stopAnimation:(id)anArgument;
@end

@implementation OakToolTip
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"OakToolTipMouseMoveIgnorePeriod"  : @1,
		@"OakToolTipMouseDistanceThreshold" : @5,
	}];
}

- (id)init
{
	D(DBF_OakToolTip, bug("\n"););
	if(self = [super initWithContentRect:NSZeroRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO])
	{
		[self setReleasedWhenClosed:NO];
		[self setAlphaValue:0.97];
		[self setOpaque:NO];
		[self setBackgroundColor:[NSColor colorWithCalibratedRed:1.0 green:0.96 blue:0.76 alpha:1.0]];
		[self setHasShadow:YES];
		[self setLevel:NSStatusWindowLevel];
		[self setHidesOnDeactivate:YES];
		[self setIgnoresMouseEvents:YES];

		field = [[NSTextField alloc] initWithFrame:NSZeroRect];
		[field setEditable:NO];
		[field setSelectable:NO];
		[field setBezeled:NO];
		[field setBordered:NO];
		[field setDrawsBackground:NO];
		[field setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		[field setStringValue:@"This is a nice little code block"];

		[self setContentView:field];
		[self setFrame:[self frameRectForContentRect:[field frame]] display:NO];

		enforceMouseThreshold = YES;
	}
	return self;
}

- (void)dealloc
{
	D(DBF_OakToolTip, bug("\n"););
}

- (void)setFont:(NSFont*)aFont
{
	ASSERT(aFont != nil);
	[field setFont:aFont];
}

- (void)setStringValue:(NSString*)aString
{
	D(DBF_OakToolTip, bug("%s\n", [aString UTF8String]););
	ASSERT(aString != nil);
	[field setStringValue:aString];
}

- (void)setEnforceMouseThreshold:(BOOL)flag
{
	enforceMouseThreshold = flag;
}

- (BOOL)canBecomeKeyWindow
{
	return YES;
}

- (BOOL)shouldCloseForMousePosition:(NSPoint)aPoint
{
	if(!enforceMouseThreshold)
		return YES;

	CGFloat ignorePeriod = [[NSUserDefaults standardUserDefaults] floatForKey:@"OakToolTipMouseMoveIgnorePeriod"];
	if(-[didOpenAtDate timeIntervalSinceNow] < ignorePeriod)
		return NO;

	if(NSEqualPoints(mousePositionWhenOpened, NSZeroPoint))
	{
		mousePositionWhenOpened = aPoint;
		return NO;
	}

	if(NSPointInRect(aPoint, NSInsetRect([self frame], 5, 5)))
		return NO;

	NSPoint const& p = mousePositionWhenOpened;
	CGFloat dist = sqrt(SQ(p.x - aPoint.x) + SQ(p.y - aPoint.y));

	CGFloat moveThreshold = [[NSUserDefaults standardUserDefaults] floatForKey:@"OakToolTipMouseDistanceThreshold"];
	return dist > moveThreshold;
}

- (void)showUntilUserActivity
{
	// since we run a lcoal event loop we wish to ensure that this is not done in the middle of something that can’t handle such thing, e.g. our call stack could be something like lock_buffer() → do_edit_operation() → show_tool_tip() → [here]. Additionally by using performSelector:withObject:afterDelay: we keep ‘self’ retained while the tool tip is up.
	[self performSelector:@selector(showUntilUserActivityDelayed:) withObject:self afterDelay:0.0];
}

- (void)showUntilUserActivityDelayed:(id)sender
{
	OakToolTip* retainedSelf = self;
	[self orderFront:self];

	didOpenAtDate = [NSDate date];
	mousePositionWhenOpened = NSZeroPoint;

	NSWindow* keyWindow = [NSApp keyWindow];
	if(!keyWindow)
	{
		keyWindow = self;
		[self makeKeyWindow];
	}

	BOOL didAcceptMouseMovedEvents = [keyWindow acceptsMouseMovedEvents];
	[keyWindow setAcceptsMouseMovedEvents:YES];

	while(NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES])
	{
		[NSApp sendEvent:event];

		static std::set<NSEventType> const orderOutEvents = { NSLeftMouseDown, NSRightMouseDown, NSOtherMouseDown, NSKeyDown, NSScrollWheel };
		if(orderOutEvents.find([event type]) != orderOutEvents.end())
		{
			D(DBF_OakToolTip, bug("close because of key/mouse down event\n"););
			break;
		}

		if([event type] == NSMouseMoved && [self shouldCloseForMousePosition:[NSEvent mouseLocation]])
		{
			D(DBF_OakToolTip, bug("close because mouse was moved\n"););
			break;
		}

		if(keyWindow != [NSApp keyWindow] || ![NSApp isActive])
		{
			D(DBF_OakToolTip, bug("close because focus lost (%s → %s) / app gone inactive (%s)\\\\n", [[keyWindow description] UTF8String], [[[NSApp keyWindow] description] UTF8String], BSTR(![NSApp isActive])););
			break;
		}
	}

	[keyWindow setAcceptsMouseMovedEvents:didAcceptMouseMovedEvents];
	[retainedSelf orderOut:nil];
}

- (void)showAtLocation:(NSPoint)aPoint forScreen:(NSScreen*)aScreen
{
	D(DBF_OakToolTip, bug("%s\n", [NSStringFromPoint(aPoint) UTF8String]););
	aScreen = aScreen ?: [NSScreen mainScreen];

	[self stopAnimation:self];

	[field sizeToFit];
	NSRect r = [aScreen visibleFrame];
	NSRect frame = [self frameRectForContentRect:[field frame]];
	frame.size.width = std::min(NSWidth(frame), NSWidth(r));
	frame.size.height = std::min(NSHeight(frame), NSHeight(r));
	[self setFrame:frame display:NO];

	aPoint.x = std::max(NSMinX(r), std::min(aPoint.x, NSMaxX(r)-NSWidth(frame)));
	aPoint.y = std::min(std::max(NSMinY(r)+NSHeight(frame), aPoint.y), NSMaxY(r));

	[self setFrameTopLeftPoint:aPoint];
	[self showUntilUserActivity];
}

- (void)orderOut:(id)sender
{
	if(![self isVisible] || self.animationTimer)
		return;

	[self stopAnimation:self];
	self.animationStart = [NSDate date];
	self.animationTimer = [NSTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(animationTick:) userInfo:nil repeats:YES];
}

- (void)animationTick:(id)sender
{
	CGFloat alpha = 0.97 * (1 - oak::slow_in_out(-1.5 * [self.animationStart timeIntervalSinceNow]));
	if(alpha > 0)
	{
		[self setAlphaValue:alpha];
	}
	else
	{
		[super orderOut:self];
		[self stopAnimation:self];
	}
}

- (void)stopAnimation:(id)sender
{
	if(self.animationTimer)
	{
		OakToolTip* retainedSelf = self;
		[retainedSelf setAlphaValue:0.97];
		[retainedSelf.animationTimer invalidate];
		retainedSelf.animationTimer = nil;
	}
}
@end
