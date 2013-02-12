#import "HOBrowserView.h"
#import "HOWebViewDelegateHelper.h"
#import "HOStatusBar.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSColor Additions.h>

@interface HOBrowserView ()
@property (nonatomic, readwrite) WebView* webView;
@property (nonatomic, readwrite) HOStatusBar* statusBar;
@property (nonatomic, retain) HOWebViewDelegateHelper* webViewDelegateHelper;
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
			@"divider"   : OakCreateViewWithColor([NSColor colorWithString:@"#9d9d9d"]),
			@"statusBar" : _statusBar	
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(==statusBar,==divider)]|"   options:NSLayoutFormatAlignAllTop           metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=10)][divider(==1)][statusBar]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
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

- (void)swipeWithEvent:(NSEvent*)anEvent
{
	if([anEvent deltaX] == +1 && _webView.canGoBack)
		[_webView goBack:self];
	else if([anEvent deltaX] == -1 && _webView.canGoForward)
		[_webView goForward:self];
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
