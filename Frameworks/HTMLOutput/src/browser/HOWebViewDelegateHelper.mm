#import "HOWebViewDelegateHelper.h"
#import "HOBrowserView.h"
#import <OakFoundation/NSString Additions.h>

@implementation HOWebViewDelegateHelper
@synthesize delegate, projectUUID;

- (void)dealloc
{
	[projectUUID release];
	[super dealloc];
}

// =====================
// = WebViewUIDelegate =
// =====================

- (void)webView:(WebView*)sender setStatusText:(NSString*)text
{
	[delegate setStatusText:(text ?: @"")];
}

- (NSString*)webViewStatusText:(WebView*)sender
{
	return [delegate statusText];
}

- (void)webView:(WebView*)sender mouseDidMoveOverElement:(NSDictionary*)elementInformation modifierFlags:(NSUInteger)modifierFlags
{
	NSURL* url = [elementInformation objectForKey:@"WebElementLinkURL"];
	[self webView:sender setStatusText:[[url absoluteString] stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
}

- (void)webView:(WebView*)sender runJavaScriptAlertPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	NSAlert* alert = [NSAlert alertWithMessageText:NSLocalizedString(@"Script Message", @"JavaScript alert title")
	                                 defaultButton:NSLocalizedString(@"OK", @"JavaScript alert confirmation")
	                               alternateButton:nil
	                                   otherButton:nil
	                     informativeTextWithFormat:message];
	[alert beginSheetModalForWindow:[sender window] modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
}

- (BOOL)webView:(WebView*)sender runJavaScriptConfirmPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	return NSAlertDefaultReturn == NSRunAlertPanel(NSLocalizedString(@"Script Message", @"JavaScript alert title"), message, NSLocalizedString(@"OK", @"JavaScript alert confirmation"), NSLocalizedString(@"Cancel", @"JavaScript alert cancel"), nil);
}

- (void)webView:(WebView*)sender runOpenPanelForFileButtonWithResultListener:(id <WebOpenPanelResultListener>)resultListener
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setDirectoryURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
	if([panel runModal] == NSOKButton)
		[resultListener chooseFilename:[[[[panel URLs] objectAtIndex:0] fileURL] path]];
}

- (WebView*)webView:(WebView*)sender createWebViewWithRequest:(NSURLRequest*)request
{
	NSPoint origin = [sender.window cascadeTopLeftFromPoint:NSMakePoint(NSMinX(sender.window.frame), NSMaxY(sender.window.frame))];
	origin.y -= NSHeight(sender.window.frame);

	HOBrowserView* view = [[HOBrowserView new] autorelease];
	NSWindow* window = [[NSWindow alloc] initWithContentRect:(NSRect){origin, NSMakeSize(750, 800)}
																  styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask)
																	 backing:NSBackingStoreBuffered
																		defer:NO];
	[window bind:@"title" toObject:view.webView withKeyPath:@"mainFrameTitle" options:nil];
	[window setReleasedWhenClosed:YES];
	[window setContentView:view];
	[[view.webView mainFrame] loadRequest:request];
	return view.webView;
}

- (void)webViewShow:(WebView*)sender
{
	[[sender window] makeKeyAndOrderFront:self];
}

// This is an undocumented WebView delegate method
- (void)webView:(WebView*)webView addMessageToConsole:(NSDictionary*)dictionary;
{
	if([dictionary respondsToSelector:@selector(objectForKey:)])
		fprintf(stderr, "%s: %s on line %d\n", [[[[[[webView mainFrame] dataSource] request] URL] absoluteString] UTF8String], [[dictionary objectForKey:@"message"] UTF8String], [[dictionary objectForKey:@"lineNumber"] intValue]);
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
			if(projectUUID)
				url = [NSURL URLWithString:[[url absoluteString] stringByAppendingFormat:@"&project=%@", projectUUID]];
			[NSApp sendAction:@selector(handleTxMtURL:) to:nil from:url];
		}
		else
		{
			[[NSWorkspace sharedWorkspace] openURL:url];
		}
	}
}

// =====================================================
// = WebResourceLoadDelegate: Redirect tm-file to file =
// =====================================================

- (NSURLRequest*)webView:(WebView*)sender resource:(id)identifier willSendRequest:(NSURLRequest*)request redirectResponse:(NSURLResponse*)redirectResponse fromDataSource:(WebDataSource*)dataSource
{
	if([[[request URL] scheme] isEqualToString:@"tm-file"])
	{
		NSString* fragment = [[request URL] fragment];
		request = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@%s%@", [[[request URL] path] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], fragment ? "#" : "", fragment ?: @""]]];
	}

	if([[request URL] isFileURL] && ![[[request URL] path] existsAsPath])
		request = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@?path=%@&error=1", [[[NSBundle bundleForClass:[self class]] pathForResource:@"error_not_found" ofType:@"html"] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], [[[request URL] path] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]]]];

	return request;
}
@end

@interface HTMLTMFileDummyProtocol : NSURLProtocol { }
@end

@implementation HTMLTMFileDummyProtocol
+ (void)load                                                                                                                                      { [self registerClass:self]; }
+ (BOOL)canInitWithRequest:(NSURLRequest*)request                                                                                                 { return [[[request URL] scheme] isEqualToString:@"tm-file"]; }
+ (NSURLRequest*)canonicalRequestForRequest:(NSURLRequest*)request                                                                                { return request; }
+ (BOOL)requestIsCacheEquivalent:(NSURLRequest*)a toRequest:(NSURLRequest*)b                                                                      { return NO; }
- (id)initWithRequest:(NSURLRequest*)anURLRequest cachedResponse:(NSCachedURLResponse*)aCachedURLResponse client:(id <NSURLProtocolClient>)anId   { return nil; }
@end
