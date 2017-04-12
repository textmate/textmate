#import "HOBrowserView.h"
#import "HOWebViewDelegateHelper.h"
#import "HOStatusBar.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

static NSString* EscapeHTML (NSString* str)
{
	return [[[str stringByReplacingOccurrencesOfString:@"&" withString:@"&amp;"] stringByReplacingOccurrencesOfString:@"<" withString:@"&lt;"] stringByReplacingOccurrencesOfString:@"\"" withString:@"&quot;"];
}

static void ShowLoadErrorForURL (WebFrame* frame, NSURL* url, NSError* error)
{
	NSString* options  = [[url scheme] isEqualToString:@"file"] ? @" -R" : @"";
	NSString* errorMsg = [NSString stringWithFormat:@"<title>Load Error</title><h1>Load Error</h1><p>WebKit reported <em>%@</em> while loading <tt><a href=\"#\" onClick=\"javascript:TextMate.system('/usr/bin/open%@ &quot;%@&quot;', null)\">%@</a></tt>.</p>", EscapeHTML([error localizedDescription]), options, EscapeHTML([url absoluteString]), EscapeHTML([url absoluteString])];
	[frame loadHTMLString:errorMsg baseURL:[NSURL fileURLWithPath:NSTemporaryDirectory()]];
}

@interface HOBrowserView () <WebPolicyDelegate, WebUIDelegate, WebResourceLoadDelegate>
@property (nonatomic, readwrite) WebView* webView;
@property (nonatomic, readwrite) HOStatusBar* statusBar;
@property (nonatomic) HOWebViewDelegateHelper* webViewDelegateHelper;
@end

@implementation HOBrowserView
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		_webView = [[WebView alloc] initWithFrame:NSZeroRect];

		NSString* const kHTMLOutputPreferencesIdentifier = @"HTML Output Preferences Identifier";
		WebPreferences* webViewPrefs = [[WebPreferences alloc] initWithIdentifier:kHTMLOutputPreferencesIdentifier];
		webViewPrefs.plugInsEnabled = NO;
		self.webView.preferencesIdentifier = kHTMLOutputPreferencesIdentifier;

		_statusBar = [[HOStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = _webView;

		_webViewDelegateHelper          = [HOWebViewDelegateHelper new];
		_webViewDelegateHelper.delegate = _statusBar;
		_webView.policyDelegate         = self;
		_webView.resourceLoadDelegate   = _webViewDelegateHelper;
		_webView.UIDelegate             = _webViewDelegateHelper;
		_webView.frameLoadDelegate      = self;

		NSDictionary* views = @{
			@"webView"   : _webView,
			@"divider"   : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]),
			@"statusBar" : _statusBar
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(==statusBar,==divider)]|"    options:NSLayoutFormatAlignAllTop     metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=10)][divider][statusBar]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
	}
	return self;
}

- (BOOL)needsNewWebView
{
	return _webViewDelegateHelper.needsNewWebView;
}

- (void)webViewProgressEstimateChanged:(NSNotification*)notification
{
	_statusBar.progress = _webView.estimatedProgress;
}

- (void)dealloc
{
	[self setUpdatesProgress:NO];
	_webView.frameLoadDelegate      = nil;
	_webView.UIDelegate             = nil;
	_webView.resourceLoadDelegate   = nil;
	_webView.policyDelegate         = nil;
	[[_webView mainFrame] stopLoading];
}

- (void)setUpdatesProgress:(BOOL)flag
{
	if(flag)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressFinishedNotification object:_webView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressEstimateChangedNotification object:_webView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressStartedNotification object:_webView];
	}
	else
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressStartedNotification object:_webView];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressEstimateChangedNotification object:_webView];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressFinishedNotification object:_webView];
	}
}

// ==============
// = Key Events =
// ==============

/*
Since the webView is typically the first responder, the path for key events is as follows:

For keyDown:
	webView
	HOBrowserView
	OakHTMLOutputView
	NSWindow

For performKeyEquivalent:
	NSWindow
	OakHTMLOutputView
	HOBrowserView
	webView

A webView default implementation passes all key events, including potential key equivalents (except ESC),
to the webpage so that it may have a chance to respond. Unfortunately, we cannot know if these events are
handled so the events are still forwarded down their respective chains as shown above. So to avoid the
NSBeep when hitting the end of the responder chain, we let HOBrowserView swallow all key events. This is
safe since performKeyEquivalent: is called first, which leads to another problem: we can pass
the key event back to the webView (minus the modifier). Therefore, we also terminate the above chain for
performKeyEquivalent: by overriding the method here and returning just NO. Note: that if none of the views
in the hierachy returns YES, the key (equivalent) event is then passed to the menus.
*/

- (BOOL)performKeyEquivalent
{
	return NO;
}

- (void)keyDown:(NSEvent*)anEvent
{

}

// =========
// = Swipe =
// =========

- (BOOL)wantsScrollEventsForSwipeTrackingOnAxis:(NSEventGestureAxis)axis
{
	return axis == NSEventGestureAxisHorizontal;
}

- (void)scrollWheel:(NSEvent*)anEvent
{
	if(![NSEvent isSwipeTrackingFromScrollEventsEnabled] || [anEvent phase] == NSEventPhaseNone || fabs([anEvent scrollingDeltaX]) <= fabs([anEvent scrollingDeltaY]))
		return;

	[anEvent trackSwipeEventWithOptions:0 dampenAmountThresholdMin:(_webView.canGoForward ? -1 : 0) max:(_webView.canGoBack ? +1 : 0) usingHandler:^(CGFloat gestureAmount, NSEventPhase phase, BOOL isComplete, BOOL* stop) {
		if(phase == NSEventPhaseBegan)
		{
			// Setup animation overlay layers
		}

		// Update animation overlay to match gestureAmount

		if(phase == NSEventPhaseEnded)
		{
			if(gestureAmount > 0 && _webView.canGoBack)
				[_webView goBack:self];
			else if(gestureAmount < 0 && _webView.canGoForward)
				[_webView goForward:self];
		}

		if(isComplete)
		{
			// Tear down animation overlay here
		}
	}];
}

// =======================
// = Frame Load Delegate =
// =======================

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	_statusBar.busy = YES;
	[self setUpdatesProgress:YES];
}

- (void)webView:(WebView*)sender didFailProvisionalLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	ShowLoadErrorForURL(frame, [[[frame provisionalDataSource] request] URL], error);
	[self webView:sender didFinishLoadForFrame:frame];
}

- (void)webView:(WebView*)sender didFailLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	ShowLoadErrorForURL(frame, [[[frame provisionalDataSource] request] URL], error);
	[self webView:sender didFinishLoadForFrame:frame];
}

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	_statusBar.canGoBack    = _webView.canGoBack;
	_statusBar.canGoForward = _webView.canGoForward;
	_statusBar.busy         = NO;
	_statusBar.progress     = 0;
}
@end
