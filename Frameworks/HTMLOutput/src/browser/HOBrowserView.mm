#import "HOBrowserView.h"
#import "HOWebViewDelegateHelper.h"
#import "HOStatusBar.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSColor Additions.h>

@implementation HOBrowserView
@synthesize webView;

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
		webView = [[WebView alloc] initWithFrame:NSZeroRect];

		statusBar = [[HOStatusBar alloc] initWithFrame:NSZeroRect];
		statusBar.delegate = webView;

		webViewDelegateHelper          = [HOWebViewDelegateHelper new];
		webViewDelegateHelper.delegate = statusBar;
		webView.policyDelegate         = webViewDelegateHelper;
		webView.resourceLoadDelegate   = webViewDelegateHelper;
		webView.UIDelegate             = webViewDelegateHelper;
		webView.frameLoadDelegate      = self;

		NSBox* divider = OakCreateViewWithColor([NSColor colorWithString:@"#9d9d9d"]);

		NSDictionary* views = NSDictionaryOfVariableBindings(webView, divider, statusBar);
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(==statusBar,==divider)]|"   options:NSLayoutFormatAlignAllTop     metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=10)][divider(==1)][statusBar]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
	}
	return self;
}

- (NSString*)projectUUID                       { return webViewDelegateHelper.projectUUID; }
- (void)setProjectUUID:(NSString*)aProjectUUID { webViewDelegateHelper.projectUUID = aProjectUUID; }

- (void)webViewProgressEstimateChanged:(NSNotification*)notification
{
	statusBar.progress = webView.estimatedProgress;
}

- (void)dealloc
{
	[self setUpdatesProgress:NO];
	[webView setResourceLoadDelegate:nil];
	[webView setFrameLoadDelegate:nil];
	[[webView mainFrame] stopLoading];

	[webView release];
	[webViewDelegateHelper release];
	[statusBar release];
	[super dealloc];
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)swipeWithEvent:(NSEvent*)anEvent
{
	if([anEvent deltaX] == +1 && webView.canGoBack)
		[webView goBack:self];
	else if([anEvent deltaX] == -1 && webView.canGoForward)
		[webView goForward:self];
}

- (void)setUpdatesProgress:(BOOL)flag
{
	if(flag)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressFinishedNotification object:webView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressEstimateChangedNotification object:webView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewProgressEstimateChanged:) name:WebViewProgressStartedNotification object:webView];
	}
	else
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressStartedNotification object:webView];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressEstimateChangedNotification object:webView];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:WebViewProgressFinishedNotification object:webView];
	}
}

// =======================
// = Frame Load Delegate =
// =======================

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	statusBar.isBusy = YES;
	[self setUpdatesProgress:YES];
}

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	statusBar.canGoBack    = webView.canGoBack;
	statusBar.canGoForward = webView.canGoForward;
	statusBar.isBusy       = NO;
	statusBar.progress     = 0;
}
@end
