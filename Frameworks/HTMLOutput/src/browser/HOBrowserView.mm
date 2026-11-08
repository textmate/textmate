#import "HOBrowserView.h"
#import "HOWebViewDelegateHelper.h"
#import "HOStatusBar.h"
#import "../helpers/HOWKScriptMessageHandler.h"
#import "../helpers/OakFileURLSchemeHandler.h"
#import "../helpers/OakFileHandleURLSchemeHandler.h"
#import "../helpers/OakSystemCommandURLSchemeHandler.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSAlert Additions.h>

static NSString* EscapeHTML (NSString* str)
{
	return [[[str stringByReplacingOccurrencesOfString:@"&" withString:@"&amp;"] stringByReplacingOccurrencesOfString:@"<" withString:@"&lt;"] stringByReplacingOccurrencesOfString:@"\"" withString:@"&quot;"];
}

@interface HOBrowserView ()
@property (nonatomic, readwrite) WKWebView* webView;
@property (nonatomic, readwrite) HOStatusBar* statusBar;
@property (nonatomic, readwrite) HOWKScriptMessageHandler* scriptMessageHandler;
@property (nonatomic, readwrite) OakSystemCommandURLSchemeHandler* systemCommandHandler;
@property (nonatomic) BOOL observingProgress;
@end

@implementation HOBrowserView
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		[self setupWebView];

		_statusBar = [[HOStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = _webView;

		NSDictionary* views = @{
			@"webView":   _webView,
			@"statusBar": _statusBar
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(>=10)]|"            options:0                                                      metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=10)][statusBar]|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:views]];
	}
	return self;
}

- (void)setupWebView
{
	WKWebViewConfiguration* config = [[WKWebViewConfiguration alloc] init];

	// Register scheme handlers
	OakFileHandleURLSchemeHandler* streamHandler = [OakFileHandleURLSchemeHandler new];
	[config setURLSchemeHandler:streamHandler forURLScheme:@"x-txmt-filehandle"];

	OakFileURLSchemeHandler* fileHandler = [OakFileURLSchemeHandler new];
	[config setURLSchemeHandler:fileHandler forURLScheme:@"tm-file"];

	// Register tm-system:// scheme handler for synchronous TextMate.system(cmd, null)
	_systemCommandHandler = [OakSystemCommandURLSchemeHandler new];
	[config setURLSchemeHandler:_systemCommandHandler forURLScheme:@"tm-system"];

	// Set up JS bridge
	_scriptMessageHandler = [HOWKScriptMessageHandler new];
	[config.userContentController addScriptMessageHandler:_scriptMessageHandler name:@"textmate"];

	// Load JS bridge script
	NSURL* jsURL = [[NSBundle bundleForClass:[self class]] URLForResource:@"HTMLOutputWKWebView" withExtension:@"js"];
	if(NSString* jsSource = [NSString stringWithContentsOfURL:jsURL encoding:NSUTF8StringEncoding error:nil])
	{
		WKUserScript* script = [[WKUserScript alloc] initWithSource:jsSource
			injectionTime:WKUserScriptInjectionTimeAtDocumentStart forMainFrameOnly:YES];
		[config.userContentController addUserScript:script];
	}

	// Allow file:// access from custom scheme pages
	[config.preferences setValue:@YES forKey:@"allowFileAccessFromFileURLs"];
	[config setValue:@YES forKey:@"allowUniversalAccessFromFileURLs"];

	_webView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:config];
	_webView.navigationDelegate = self;
	_webView.UIDelegate = self;

	_scriptMessageHandler.webView = _webView;
}

- (BOOL)needsNewWebView
{
	return NO;
}

- (void)dealloc
{
	[self setUpdatesProgress:NO];
	_webView.navigationDelegate = nil;
	_webView.UIDelegate = nil;
	[_webView stopLoading];
}

- (void)setUpdatesProgress:(BOOL)flag
{
	if(flag && !_observingProgress)
	{
		[_webView addObserver:self forKeyPath:@"estimatedProgress" options:NSKeyValueObservingOptionNew context:nil];
		_observingProgress = YES;
	}
	else if(!flag && _observingProgress)
	{
		[_webView removeObserver:self forKeyPath:@"estimatedProgress"];
		_observingProgress = NO;
	}
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"estimatedProgress"])
		_statusBar.progress = _webView.estimatedProgress;
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

// ========================
// = WKNavigationDelegate =
// ========================

- (void)webView:(WKWebView*)webView didStartProvisionalNavigation:(WKNavigation*)navigation
{
	_statusBar.busy = YES;
	[self setUpdatesProgress:YES];
}

- (void)webView:(WKWebView*)webView didFailProvisionalNavigation:(WKNavigation*)navigation withError:(NSError*)error
{
	// "Frame load interrupted" (WebKitErrorFrameLoadInterruptedByPolicyChange = 102) is normal
	// when a new navigation replaces an in-progress one. Don't show an error page for this.
	if([error.domain isEqualToString:@"WebKitErrorDomain"] && error.code == 102)
		return;

	[self showLoadErrorForURL:webView.URL error:error];
	[self webView:webView didFinishNavigation:navigation];
}

- (void)webView:(WKWebView*)webView didFailNavigation:(WKNavigation*)navigation withError:(NSError*)error
{
	[self showLoadErrorForURL:webView.URL error:error];
	[self webView:webView didFinishNavigation:navigation];
}

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
	_statusBar.canGoBack    = webView.canGoBack;
	_statusBar.canGoForward = webView.canGoForward;
	_statusBar.busy         = NO;
	_statusBar.progress     = 0;
}

- (void)showLoadErrorForURL:(NSURL*)url error:(NSError*)error
{
	NSString* errorMsg = [NSString stringWithFormat:
		@"<title>Load Error</title><h1>Load Error</h1>"
		@"<p>WebKit reported <em>%@</em> while loading <tt>%@</tt>.</p>",
		EscapeHTML(error.localizedDescription),
		EscapeHTML(url.absoluteString)];
	[_webView loadHTMLString:errorMsg baseURL:[NSURL fileURLWithPath:NSTemporaryDirectory()]];
}

// =================
// = WKUIDelegate  =
// =================

- (void)webView:(WKWebView*)webView runJavaScriptAlertPanelWithMessage:(NSString*)message initiatedByFrame:(WKFrameInfo*)frame completionHandler:(void(^)(void))completionHandler
{
	NSAlert* alert = [NSAlert tmAlertWithMessageText:NSLocalizedString(@"Script Message", @"JavaScript alert title") informativeText:message buttons:NSLocalizedString(@"OK", @"JavaScript alert confirmation"), nil];
	[alert beginSheetModalForWindow:[webView window] completionHandler:^(NSModalResponse returnCode){
		completionHandler();
	}];
}

- (void)webView:(WKWebView*)webView runJavaScriptConfirmPanelWithMessage:(NSString*)message initiatedByFrame:(WKFrameInfo*)frame completionHandler:(void(^)(BOOL result))completionHandler
{
	NSAlert* alert = [[NSAlert alloc] init];
	alert.messageText     = NSLocalizedString(@"Script Message", @"JavaScript alert title");
	alert.informativeText = message;
	[alert addButtons:NSLocalizedString(@"OK", @"JavaScript alert confirmation"), NSLocalizedString(@"Cancel", @"JavaScript alert cancel"), nil];
	[alert beginSheetModalForWindow:[webView window] completionHandler:^(NSModalResponse returnCode){
		completionHandler(returnCode == NSAlertFirstButtonReturn);
	}];
}

- (void)webView:(WKWebView*)webView runOpenPanelWithParameters:(WKOpenPanelParameters*)parameters initiatedByFrame:(WKFrameInfo*)frame completionHandler:(void(^)(NSArray<NSURL*>* _Nullable URLs))completionHandler
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setDirectoryURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
	panel.allowsMultipleSelection = parameters.allowsMultipleSelection;
	[panel beginSheetModalForWindow:[webView window] completionHandler:^(NSModalResponse result){
		if(result == NSModalResponseOK)
			completionHandler(panel.URLs);
		else
			completionHandler(nil);
	}];
}

- (WKWebView*)webView:(WKWebView*)webView createWebViewWithConfiguration:(WKWebViewConfiguration*)configuration forNavigationAction:(WKNavigationAction*)navigationAction windowFeatures:(WKWindowFeatures*)windowFeatures
{
	NSPoint origin = [webView.window cascadeTopLeftFromPoint:NSMakePoint(NSMinX(webView.window.frame), NSMaxY(webView.window.frame))];
	origin.y -= NSHeight(webView.window.frame);

	WKWebView* newWebView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:configuration];
	newWebView.navigationDelegate = self;
	newWebView.UIDelegate = self;

	NSWindow* window = [[NSWindow alloc] initWithContentRect:(NSRect){origin, NSMakeSize(750, 800)}
		styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable)
		backing:NSBackingStoreBuffered
		defer:NO];
	[window bind:NSTitleBinding toObject:newWebView withKeyPath:@"title" options:nil];
	[window setContentView:newWebView];
	if(navigationAction.request)
		[newWebView loadRequest:navigationAction.request];

	__attribute__ ((unused)) CFTypeRef dummy = CFBridgingRetain(window);
	[window setReleasedWhenClosed:YES];
	[window makeKeyAndOrderFront:self];

	return newWebView;
}

- (void)webViewDidClose:(WKWebView*)webView
{
	if(![webView tryToPerform:@selector(toggleHTMLOutput:) with:self])
		[webView tryToPerform:@selector(performClose:) with:self];
}
@end
