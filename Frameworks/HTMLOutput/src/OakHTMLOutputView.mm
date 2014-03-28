#import "OakHTMLOutputView.h"
#import "browser/HOStatusBar.h"
#import "helpers/HOAutoScroll.h"
#import "helpers/HOJSBridge.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>

extern NSString* const kCommandRunnerURLScheme; // from HTMLOutput.h

@interface OakHTMLOutputView ()
{
	OBJC_WATCH_LEAKS(OakHTMLOutputView);
}
@property (nonatomic) BOOL runningCommand;
@property (nonatomic) HOAutoScroll* autoScrollHelper;
@property (nonatomic) std::map<std::string, std::string> environment;
@end

@implementation OakHTMLOutputView
- (void)loadRequest:(NSURLRequest*)aRequest environment:(std::map<std::string, std::string> const&)anEnvironment autoScrolls:(BOOL)flag
{
	if(flag)
	{
		self.autoScrollHelper = [HOAutoScroll new];
		self.autoScrollHelper.webFrame = self.webView.mainFrame.frameView;
	}

	self.environment = anEnvironment;
	self.runningCommand = [[[aRequest URL] scheme] isEqualToString:kCommandRunnerURLScheme];
	[self.webView.mainFrame loadRequest:aRequest];
}

- (void)stopLoading
{
	[self.webView.mainFrame stopLoading];
}

// =======================
// = Frame Load Delegate =
// =======================

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	self.statusBar.isBusy = YES;
	if(NSString* scheme = [[[[[self.webView mainFrame] provisionalDataSource] request] URL] scheme])
		[self setUpdatesProgress:![scheme isEqualToString:kCommandRunnerURLScheme]];
}

- (void)webView:(WebView*)sender didClearWindowObject:(WebScriptObject*)windowScriptObject forFrame:(WebFrame*)frame
{
	NSString* scheme = [[[[[self.webView mainFrame] dataSource] request] URL] scheme];
	if([@[ kCommandRunnerURLScheme, @"tm-file", @"file" ] containsObject:scheme])
	{
		HOJSBridge* bridge = [HOJSBridge new];
		[bridge setDelegate:self.statusBar];
		[bridge setEnvironment:_environment];
		[windowScriptObject setValue:bridge forKey:@"TextMate"];
	}
}

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;

	// Sending goBack:/goForward: to a WebView does not call this WebFrameLoadDelegate method
	if(frame == [sender mainFrame])
		[self webView:sender didClearWindowObject:[frame windowObject] forFrame:frame];

	[super webView:sender didFinishLoadForFrame:frame];
}

- (void)webView:(WebView*)sender didFailProvisionalLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
}

- (void)webView:(WebView*)sender didFailLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
}

// =========================================
// = WebPolicyDelegate : Intercept txmt:// =
// =========================================

- (void)webView:(WebView*)sender decidePolicyForNavigationAction:(NSDictionary*)actionInformation request:(NSURLRequest*)request frame:(WebFrame*)frame decisionListener:(id <WebPolicyDecisionListener>)listener
{
	if([NSURLConnection canHandleRequest:request])
	{
		[listener use];
	}
	else
	{
		[listener ignore];
		NSURL* url = request.URL;
		if([[url scheme] isEqualToString:@"txmt"])
		{
			auto projectUUID = _environment.find("TM_PROJECT_UUID");
			if(projectUUID != _environment.end())
				url = [NSURL URLWithString:[[url absoluteString] stringByAppendingFormat:@"&project=%@", [NSString stringWithCxxString:projectUUID->second]]];
			[NSApp sendAction:@selector(handleTxMtURL:) to:nil from:url];
		}
		else
		{
			[[NSWorkspace sharedWorkspace] openURL:url];
		}
	}
}
@end
