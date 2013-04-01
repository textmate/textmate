#import "HOBrowserView.h"
#import "HOWebViewDelegateHelper.h"
#import "HOStatusBar.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSColor Additions.h>

@interface HOBrowserView ()
@property (nonatomic, readwrite) WebView* webView;
@property (nonatomic, readwrite) HOStatusBar* statusBar;
@property (nonatomic, retain) HOWebViewDelegateHelper* webViewDelegateHelper;
@property (nonatomic, copy) NSEvent* gestureBeginEvent;
@property (nonatomic) NSRect visibleRectBeforeGesture;
@end

@implementation HOBrowserView
+ (BOOL)requiresConstraintBasedLayout
{
	return YES;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, NSViewNoInstrinsicMetric);
}

- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		_webView = [[WebView alloc] initWithFrame:NSZeroRect];

		_statusBar = [[HOStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = _webView;

		_webViewDelegateHelper          = [HOWebViewDelegateHelper new];
		_webViewDelegateHelper.delegate = _statusBar;
		_webView.policyDelegate         = _webViewDelegateHelper;
		_webView.resourceLoadDelegate   = _webViewDelegateHelper;
		_webView.UIDelegate             = _webViewDelegateHelper;
		_webView.frameLoadDelegate      = self;

		NSDictionary* views = @{
			@"webView"   : _webView, 
			@"divider"   : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]),
			@"statusBar" : _statusBar	
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(==statusBar,==divider)]|"    options:NSLayoutFormatAlignAllTop           metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=10)][divider][statusBar]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
	}
	return self;
}

- (NSString*)projectUUID                       { return _webViewDelegateHelper.projectUUID; }
- (void)setProjectUUID:(NSString*)aProjectUUID { _webViewDelegateHelper.projectUUID = aProjectUUID; }

- (void)webViewProgressEstimateChanged:(NSNotification*)notification
{
	_statusBar.progress = _webView.estimatedProgress;
}

- (void)dealloc
{
	[self setUpdatesProgress:NO];
	[_webView setResourceLoadDelegate:nil];
	[_webView setFrameLoadDelegate:nil];
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

// =========
// = Swipe =
// =========

- (void)swipeWithEvent:(NSEvent*)anEvent
{
	if([anEvent deltaX] == +1 && _webView.canGoBack)
		[_webView goBack:self];
	else if([anEvent deltaX] == -1 && _webView.canGoForward)
		[_webView goForward:self];
}

- (NSView*)scrollableViewForPoint:(NSPoint)aPoint inWebFrame:(WebFrame*)parentFrame
{
	for(WebFrame* webFrame in [parentFrame childFrames])
	{
		if(NSView* res = [self scrollableViewForPoint:aPoint inWebFrame:webFrame])
			return res;
	}

	NSView <WebDocumentView>* documentView = [[parentFrame frameView] documentView];
	NSRect visible = [documentView convertRect:[documentView visibleRect] toView:nil];
	NSRect actual = [documentView convertRect:[documentView bounds] toView:nil];
	if(NSMouseInRect(aPoint, visible, [documentView isFlipped]) && !NSEqualRects(visible, actual))
		return documentView;

	return nil;
}

- (void)beginGestureWithEvent:(NSEvent*)anEvent
{
	self.gestureBeginEvent = anEvent;

	NSView* view = [self scrollableViewForPoint:[anEvent locationInWindow] inWebFrame:[_webView mainFrame]];
	self.visibleRectBeforeGesture = view ? [view visibleRect] : NSZeroRect;
}

- (void)endGestureWithEvent:(NSEvent*)anEvent
{
	NSTimeInterval duration = [anEvent timestamp] - [self.gestureBeginEvent timestamp];
	NSMutableDictionary* map = [NSMutableDictionary dictionary];
	for(NSTouch* touch in [self.gestureBeginEvent touchesMatchingPhase:NSTouchPhaseBegan inView:nil])
		map[touch.identity] = touch;
	self.gestureBeginEvent = nil;

	NSView* view = [self scrollableViewForPoint:[anEvent locationInWindow] inWebFrame:[_webView mainFrame]];
	if(duration > 0.2 || view && !NSEqualRects(self.visibleRectBeforeGesture, [view visibleRect]))
		return;

	NSInteger direction = 0;
	for(NSTouch* touch in [anEvent touchesMatchingPhase:NSTouchPhaseAny inView:nil])
	{
		NSTouch* initialTouch = map[touch.identity];
		CGFloat distance = touch.normalizedPosition.x - initialTouch.normalizedPosition.x;
		direction += distance <= -0.1 ? +1 : (distance >= 0.1 ? -1 : 0);
	}

	if(direction == -2 && _webView.canGoBack)
		[_webView goBack:self];
	else if(direction == +2 && _webView.canGoForward)
		[_webView goForward:self];
}

// =======================
// = Frame Load Delegate =
// =======================

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	_statusBar.isBusy = YES;
	[self setUpdatesProgress:YES];
}

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	_statusBar.canGoBack    = _webView.canGoBack;
	_statusBar.canGoForward = _webView.canGoForward;
	_statusBar.isBusy       = NO;
	_statusBar.progress     = 0;
}
@end
