#import "OakHTMLOutputView.h"
#import "browser/HOStatusBar.h"
#import "helpers/HOAutoScroll.h"
#import "helpers/HOJSBridge.h"
#import "helpers/HOWKScriptMessageHandler.h"
#import "helpers/OakHTMLOutputRequestMetadata.h"
#import "helpers/OakSystemCommandURLSchemeHandler.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <oak/debug.h>

@interface HOStatusBar (BusyAndProgressProperties) <HOJSBridgeDelegate>
@end

@interface OakHTMLOutputView ()
@property (nonatomic, getter = isRunningCommand, readwrite) BOOL runningCommand;
@property (nonatomic) HOAutoScroll* autoScrollHelper;
@property (nonatomic) std::map<std::string, std::string> environment;
@property (nonatomic) NSArray* pendingScrollPosition;
@property (nonatomic, getter = isVisible) BOOL visible;
@end

@implementation OakHTMLOutputView
+ (NSSet*)keyPathsForValuesAffectingMainFrameTitle
{
	return [NSSet setWithObjects:@"webView.title", nil];
}

- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_reusable = YES;
	}
	return self;
}

- (void)loadRequest:(NSURLRequest*)aRequest environment:(std::map<std::string, std::string> const&)anEnvironment autoScrolls:(BOOL)flag
{
	if(flag)
		self.autoScrollHelper = [[HOAutoScroll alloc] initWithWebView:self.webView];

	self.environment = anEnvironment;

	// Pass environment to the script message handler so TextMate.system() uses correct env
	[self.scriptMessageHandler setEnvironment:anEnvironment];
	[self.scriptMessageHandler setDelegate:self.statusBar];

	// Pass environment to the system command handler for synchronous TextMate.system(cmd, null)
	self.systemCommandHandler.environment = anEnvironment;

	// Read commandIdentifier from metadata registry instead of NSURLProtocol property
	OakHTMLOutputRequestMetadata* metadata = [OakHTMLOutputRequestMetadata metadataForURLString:aRequest.URL.absoluteString];
	self.commandIdentifier = metadata.commandIdentifier;
	self.runningCommand = self.commandIdentifier != nil;

	[self willChangeValueForKey:@"mainFrameTitle"];
	[self.webView loadRequest:aRequest];
	[self didChangeValueForKey:@"mainFrameTitle"];
}

- (void)stopLoadingWithUserInteraction:(BOOL)askUserFlag completionHandler:(void(^)(BOOL didStop))handler
{
	OakHTMLOutputRequestMetadata* metadata = [OakHTMLOutputRequestMetadata metadataForURLString:self.webView.URL.absoluteString];
	if(metadata.command)
	{
		NSAlert* alert = askUserFlag ? [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"Stop \u201C%@\u201D?", metadata.processName] informativeText:@"The job that the task is performing will not be completed." buttons:@"Stop", @"Cancel", nil] : nil;

		__weak __block id token = [NSNotificationCenter.defaultCenter addObserverForName:@"OakCommandDidTerminateNotification" object:metadata.command queue:nil usingBlock:^(NSNotification* notification){
			if(alert)
				[self.window endSheet:alert.window returnCode:NSAlertFirstButtonReturn];
			handler(YES);
			[NSNotificationCenter.defaultCenter removeObserver:token];
		}];

		if(alert)
		{
			[alert beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse returnCode){
				if(returnCode == NSAlertFirstButtonReturn) /* "Stop" */
				{
					[self.webView stopLoading];
				}
				else
				{
					handler(NO);
					[NSNotificationCenter.defaultCenter removeObserver:token];
				}
			}];
		}
		else
		{
			[self.webView stopLoading];
		}
	}
	else
	{
		handler(YES);
	}
}

- (void)setContent:(NSString*)someHTML
{
	// Save scroll position via JavaScript
	[self.webView evaluateJavaScript:@"[window.scrollX, window.scrollY]" completionHandler:^(NSArray* result, NSError* error){
		if([result isKindOfClass:[NSArray class]] && result.count == 2)
			self.pendingScrollPosition = result;
	}];

	// Rewrite file:// to tm-file:// in href/src attributes (same as streaming path)
	// but preserve file:// inside txmt:// links
	NSMutableString* rewritten = [someHTML mutableCopy];
	NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:
		@"((?:href|src)\\s*=\\s*[\"'])file://" options:0 error:nil];
	NSArray<NSTextCheckingResult*>* matches = [regex matchesInString:rewritten options:0 range:NSMakeRange(0, rewritten.length)];
	for(NSTextCheckingResult* match in [matches reverseObjectEnumerator])
	{
		NSRange fullRange = match.range;
		NSString* prefix = [rewritten substringWithRange:[match rangeAtIndex:1]];
		NSUInteger searchStart = fullRange.location > 50 ? fullRange.location - 50 : 0;
		NSRange searchRange = NSMakeRange(searchStart, fullRange.location - searchStart);
		if([rewritten rangeOfString:@"txmt://" options:0 range:searchRange].location != NSNotFound)
			continue;
		[rewritten replaceCharactersInRange:fullRange withString:[prefix stringByAppendingString:@"tm-file://"]];
	}

	[self.webView loadHTMLString:rewritten baseURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
}

- (NSString*)mainFrameTitle
{
	if(OakIsEmptyString(self.webView.title))
	{
		OakHTMLOutputRequestMetadata* metadata = [OakHTMLOutputRequestMetadata metadataForURLString:self.webView.URL.absoluteString];
		if(metadata.processName)
			return metadata.processName;
	}
	return self.webView.title;
}

- (void)viewDidMoveToWindow
{
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowWillCloseNotification object:nil];
	if(self.window)
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:self.window];
	self.visible = self.window ? YES : NO;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	self.visible = NO;
}

// ========================
// = WKNavigationDelegate =
// ========================

- (void)webView:(WKWebView*)webView didStartProvisionalNavigation:(WKNavigation*)navigation
{
	self.statusBar.busy = YES;
	[self setUpdatesProgress:!self.isRunningCommand];
}

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;

	// Re-inject environment into the JS bridge after navigation (e.g., goBack/goForward)
	if(!self.disableJavaScriptAPI)
	{
		NSString* scheme = webView.URL.scheme;
		if([@[@"tm-file", @"file", @"x-txmt-filehandle"] containsObject:scheme])
		{
			[self.scriptMessageHandler setEnvironment:_environment];
			[self.scriptMessageHandler setDelegate:self.statusBar];
			self.systemCommandHandler.environment = _environment;
		}
	}

	// Restore scroll position if pending
	if(self.pendingScrollPosition)
	{
		NSString* js = [NSString stringWithFormat:@"window.scrollTo(%@, %@)",
			self.pendingScrollPosition[0], self.pendingScrollPosition[1]];
		[webView evaluateJavaScript:js completionHandler:nil];
		self.pendingScrollPosition = nil;
	}

	[super webView:webView didFinishNavigation:navigation];
}

- (void)webView:(WKWebView*)webView didFailProvisionalNavigation:(WKNavigation*)navigation withError:(NSError*)error
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
	[super webView:webView didFailProvisionalNavigation:navigation withError:error];
}

- (void)webView:(WKWebView*)webView didFailNavigation:(WKNavigation*)navigation withError:(NSError*)error
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
	[super webView:webView didFailNavigation:navigation withError:error];
}

// =========================================
// = Navigation Policy: Intercept txmt:// =
// =========================================

- (void)webView:(WKWebView*)webView decidePolicyForNavigationAction:(WKNavigationAction*)navigationAction decisionHandler:(void(^)(WKNavigationActionPolicy))decisionHandler
{
	NSURL* url = navigationAction.request.URL;
	NSString* scheme = url.scheme;

	// Allow our custom schemes and file://
	if([@[@"x-txmt-filehandle", @"tm-file", @"tm-system", @"file", @"about"] containsObject:scheme])
	{
		decisionHandler(WKNavigationActionPolicyAllow);
		return;
	}

	// Handle txmt:// internally
	if([scheme isEqualToString:@"txmt"])
	{
		auto projectUUID = _environment.find("TM_PROJECT_UUID");
		if(projectUUID != _environment.end())
			url = [NSURL URLWithString:[[url absoluteString] stringByAppendingFormat:@"&project=%@", [NSString stringWithCxxString:projectUUID->second]]];
		[NSApp sendAction:@selector(handleTxMtURL:) to:nil from:url];
		decisionHandler(WKNavigationActionPolicyCancel);
		return;
	}

	// Handle http/https -- allow in-page navigation, open external links externally
	if([@[@"http", @"https"] containsObject:scheme])
	{
		if(navigationAction.navigationType == WKNavigationTypeLinkActivated)
		{
			[NSWorkspace.sharedWorkspace openURL:url];
			decisionHandler(WKNavigationActionPolicyCancel);
		}
		else
		{
			decisionHandler(WKNavigationActionPolicyAllow);
		}
		return;
	}

	// Unknown scheme -- open externally
	[NSWorkspace.sharedWorkspace openURL:url];
	decisionHandler(WKNavigationActionPolicyCancel);
}

// ====================
// = Printing Support =
// ====================

- (IBAction)printDocument:(id)sender
{
	NSPrintInfo* printInfo = [NSPrintInfo sharedPrintInfo];
	NSPrintOperation* printOp = [self.webView printOperationWithPrintInfo:printInfo];
	[[printOp printPanel] setOptions:[[printOp printPanel] options] | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation];
	[printOp runOperationModalForWindow:self.window delegate:nil didRunSelector:NULL contextInfo:nil];
}
@end
