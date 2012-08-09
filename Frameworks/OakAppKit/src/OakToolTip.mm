#import "OakToolTip.h"
#import <oak/debug.h>
#import <oak/oak.h>

OAK_DEBUG_VAR(OakToolTip);

void OakShowToolTip (NSString* msg, NSPoint location)
{
	if(msg)
	{
		OakToolTip* toolTip = [[OakToolTip new] autorelease];
		[toolTip setStringValue:msg];
		[toolTip showAtLocation:location forScreen:nil];
	}
}

@interface OakToolTip (Private)
- (void)stopAnimation:(id)anArgument;
@end

@implementation OakToolTip
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:
		[NSDictionary dictionaryWithObjectsAndKeys:
			@1, @"OakToolTipMouseMoveIgnorePeriod",
			@5, @"OakToolTipMouseDistanceThreshold",
			nil]];
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

		field = [[[NSTextField alloc] initWithFrame:NSZeroRect] autorelease];
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
	[self stopAnimation:self];
	[didOpenAtDate release];
	[super dealloc];
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
	[self orderFront:self];

	[self setValue:[NSDate date] forKey:@"didOpenAtDate"];
	mousePositionWhenOpened = NSZeroPoint;

	NSWindow* keyWindow = [[NSApp keyWindow] retain];
	if(!keyWindow)
	{
		keyWindow = [self retain];
		[self makeKeyWindow];
	}

	BOOL didAcceptMouseMovedEvents = [keyWindow acceptsMouseMovedEvents];
	[keyWindow setAcceptsMouseMovedEvents:YES];

	while(NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES])
	{
		[NSApp sendEvent:event];

		static NSEventType const orderOutEvents[] = { NSLeftMouseDown, NSRightMouseDown, NSOtherMouseDown, NSKeyDown, NSScrollWheel };
		if(oak::contains(beginof(orderOutEvents), endof(orderOutEvents), [event type]))
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
	[keyWindow release];

	[self orderOut:nil];
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
	if(![self isVisible] || animationTimer)
		return;

	[self stopAnimation:self];
	[self setValue:[NSDate date] forKey:@"animationStart"];
	[self setValue:[NSTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(animationTick:) userInfo:nil repeats:YES] forKey:@"animationTimer"];
}

- (void)animationTick:(id)sender
{
	CGFloat alpha = 0.97 * (1 - oak::slow_in_out(-1.5 * [animationStart timeIntervalSinceNow]));
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
	if(animationTimer)
	{
		[[self retain] autorelease];
		[animationTimer invalidate];
		[self setValue:nil forKey:@"animationTimer"];
		[self setValue:nil forKey:@"animationStart"];
		[self setAlphaValue:0.97];
	}
}
@end
