//
//  TMDHTMLTips.mm
//
//  Created by Ciarán Walsh on 2007-08-19.
//

#import "TMDHTMLTips.h"
#import <WebKit/WebKit.h>

/*
"$DIALOG" tooltip --text '‘foobar’'
"$DIALOG" tooltip --html '<h1>‘foobar’</h1>'
*/

NSString* const TMDTooltipPreferencesIdentifier = @"TM Tooltip";

@interface TMDHTMLTip () <WKNavigationDelegate>
{
	WKWebView* webView;

	NSDate* didOpenAtDate; // ignore mouse moves for the next second
	NSPoint mousePositionWhenOpened;
}
- (void)setContent:(NSString*)content transparent:(BOOL)transparent;
- (void)runUntilUserActivity:(id)sender;
@end

@implementation TMDHTMLTip
// ==================
// = Setup/teardown =
// ==================
+ (void)showWithContent:(NSString*)content atLocation:(NSPoint)point transparent:(BOOL)transparent
{
	TMDHTMLTip* tip = [TMDHTMLTip new];
	[tip setFrameTopLeftPoint:point];
	[tip setContent:content transparent:transparent]; // The tooltip will show itself automatically when the HTML is loaded
}

- (id)init;
{
	if(self = [self initWithContentRect:NSMakeRect(0, 0, 1, 1) styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO])
	{
		// Since we are relying on `setReleaseWhenClosed:`, we need to ensure that we are over-retained.
		CFBridgingRetain(self);
		[self setReleasedWhenClosed:YES];
		[self setAlphaValue:0.97];
		[self setOpaque:NO];
		[self setBackgroundColor:[NSColor colorWithDeviceRed:1.0 green:0.96 blue:0.76 alpha:1.0]];
		[self setBackgroundColor:[NSColor clearColor]];
		[self setHasShadow:YES];
		[self setLevel:NSStatusWindowLevel];
		[self setHidesOnDeactivate:YES];
		[self setIgnoresMouseEvents:YES];

		WKWebViewConfiguration* config = [WKWebViewConfiguration new];
		config.websiteDataStore = WKWebsiteDataStore.nonPersistentDataStore;

		webView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:config];
		[webView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		[webView setNavigationDelegate:self];
		[webView setValue:@NO forKey:@"drawsBackground"]; // no public API for a transparent WKWebView

		[self setContentView:webView];
	}
	return self;
}

// ===========
// = Webview =
// ===========
- (void)setContent:(NSString*)content transparent:(BOOL)transparent
{
	NSString* fontName = [NSUserDefaults.standardUserDefaults stringForKey:@"fontName"];
	int fontSize = [NSUserDefaults.standardUserDefaults integerForKey:@"fontSize"] ?: 11;
	NSFont* font = fontName ? [NSFont fontWithName:fontName size:fontSize] : [NSFont userFixedPitchFontOfSize:fontSize];

	NSString* fullContent =	@"<html>"
				@"<head>"
				@"  <style type='text/css' media='screen'>"
				@"      body {"
				@"          background: %@;"
				@"          margin: 0;"
				@"          padding: 2px;"
				@"          overflow: hidden;"
				@"          display: table-cell;"
				@"          max-width: 800px;"
				@"          font-family: '%@';"
				@"          font-size: %dpx;"
				@"      }"
				@"      pre { white-space: pre-wrap; }"
				@"  </style>"
				@"</head>"
				@"<body>%@</body>"
				@"</html>";

	fullContent = [NSString stringWithFormat:fullContent, transparent ? @"transparent" : @"#F6EDC3", [font familyName], fontSize, content];
	[webView loadHTMLString:fullContent baseURL:nil];
}

- (void)sizeToContentAndShow
{
	// Current tooltip position
	NSPoint pos = NSMakePoint([self frame].origin.x, [self frame].origin.y + [self frame].size.height);

	// Find the screen which we are displaying on
	NSRect screenFrame = [[NSScreen mainScreen] visibleFrame];
	for(NSScreen* candidate in [NSScreen screens])
	{
		if(NSPointInRect(pos, [candidate frame]))
		{
			screenFrame = [candidate visibleFrame];
			break;
		}
	}

	// The webview is set to a large initial size and then sized down to fit the content
	[self setContentSize:NSMakeSize(screenFrame.size.width - screenFrame.size.width / 3.0, screenFrame.size.height)];

	[webView evaluateJavaScript:@"[ document.body.getBoundingClientRect().right, document.body.getBoundingClientRect().bottom ]" completionHandler:^(id result, NSError* error){
		if(![result isKindOfClass:[NSArray class]] || [result count] != 2)
			return;

		double width  = ceil([result[0] doubleValue]);
		double height = ceil([result[1] doubleValue]);

		[self->webView setFrameSize:NSMakeSize(width, height)];

		NSRect frame      = [self frameRectForContentRect:[self->webView frame]];
		frame.size.width  = std::min(NSWidth(frame), NSWidth(screenFrame));
		frame.size.height = std::min(NSHeight(frame), NSHeight(screenFrame));
		[self setFrame:frame display:NO];

		NSPoint newPos = pos;
		newPos.x = std::max(NSMinX(screenFrame), std::min(newPos.x, NSMaxX(screenFrame)-NSWidth(frame)));
		newPos.y = std::min(std::max(NSMinY(screenFrame)+NSHeight(frame), newPos.y), NSMaxY(screenFrame));

		[self setFrameTopLeftPoint:newPos];
		[self orderFront:self];
		[self runUntilUserActivity:self];
	}];
}

- (void)webView:(WKWebView*)sender didFinishNavigation:(WKNavigation*)navigation
{
	[self sizeToContentAndShow];
}

// ==================
// = Event handling =
// ==================
- (BOOL)shouldCloseForMousePosition:(NSPoint)aPoint
{
	CGFloat ignorePeriod = [NSUserDefaults.standardUserDefaults floatForKey:@"OakToolTipMouseMoveIgnorePeriod"];
	if(-[didOpenAtDate timeIntervalSinceNow] < ignorePeriod)
		return NO;

	if(NSEqualPoints(mousePositionWhenOpened, NSZeroPoint))
	{
		mousePositionWhenOpened = aPoint;
		return NO;
	}

	NSPoint const& p = mousePositionWhenOpened;
	CGFloat deltaX = p.x - aPoint.x;
	CGFloat deltaY = p.y - aPoint.y;
	CGFloat dist = sqrt(deltaX * deltaX + deltaY * deltaY);

	CGFloat moveThreshold = [NSUserDefaults.standardUserDefaults floatForKey:@"OakToolTipMouseDistanceThreshold"];
	return dist > moveThreshold;
}

- (void)runUntilUserActivity:(id)sender
{
	[self setValue:[NSDate date] forKey:@"didOpenAtDate"];
	mousePositionWhenOpened = NSZeroPoint;

	NSWindow* keyWindow = [NSApp keyWindow];
	BOOL didAcceptMouseMovedEvents = [keyWindow acceptsMouseMovedEvents];
	[keyWindow setAcceptsMouseMovedEvents:YES];

	BOOL slowFadeOut = NO;
	while(NSEvent* event = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES])
	{
		[NSApp sendEvent:event];

		if([event type] == NSEventTypeLeftMouseDown || [event type] == NSEventTypeRightMouseDown || [event type] == NSEventTypeOtherMouseDown || [event type] == NSEventTypeKeyDown || [event type] == NSEventTypeScrollWheel)
			break;

		if([event type] == NSEventTypeMouseMoved && [self shouldCloseForMousePosition:[NSEvent mouseLocation]])
		{
			slowFadeOut = YES;
			break;
		}

		if(keyWindow != [NSApp keyWindow] || ![NSApp isActive])
			break;
	}

	[keyWindow setAcceptsMouseMovedEvents:didAcceptMouseMovedEvents];


	[self fadeOutSlowly:slowFadeOut];
}

// =============
// = Animation =
// =============
- (void)fadeOutSlowly:(BOOL)slowly
{
	[NSAnimationContext beginGrouping];

	[NSAnimationContext currentContext].duration = slowly ? 0.5 : 0.25;
	[NSAnimationContext currentContext].completionHandler = ^{
		[self orderOut:self];
	};

	[self.animator setAlphaValue:0];

	[NSAnimationContext endGrouping];
}
@end
