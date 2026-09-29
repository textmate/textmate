#import "HOWebViewDelegateHelper.h"
#import "HOBrowserView.h"
#import <OakAppKit/src/NSAlert Additions.h>
#import <OakFoundation/src/NSString Additions.h>
#import <io/src/path.h>
#import <oak/debug.h>

static NSString* const kUserDefaultsDefaultURLProtocolKey = @"defaultURLProtocol";

static BOOL IsProtocolRelativeURL (NSURL* url)
{
	if([url.scheme hasPrefix:@"x-txmt"] && ![url.host isEqualToString:@"job"])
		return YES;

	if([url.scheme isEqualToString:@"file"] && url.host)
	{
		// If host has a dot and does not exist on disk then treat as protocol-relative URL
		if([url.host containsString:@"."] && ![NSFileManager.defaultManager fileExistsAtPath:[@"/" stringByAppendingPathComponent:url.host]])
			return YES;
	}

	return NO;
}

@implementation HOWebViewDelegateHelper
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		kUserDefaultsDefaultURLProtocolKey: @"https",
	}];
}

// =====================
// = WebViewUIDelegate =
// =====================

- (void)webView:(WebView*)sender setStatusText:(NSString*)text
{
	[_delegate setStatusText:(text ?: @"")];
}

- (NSString*)webViewStatusText:(WebView*)sender
{
	return [_delegate statusText];
}

- (void)webView:(WebView*)sender mouseDidMoveOverElement:(NSDictionary*)elementInformation modifierFlags:(NSUInteger)modifierFlags
{
	NSURL* url = [elementInformation objectForKey:@"WebElementLinkURL"];
	[self webView:sender setStatusText:[[url absoluteString] stringByRemovingPercentEncoding]];
}

- (void)webView:(WebView*)sender runJavaScriptAlertPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	NSAlert* alert = [NSAlert tmAlertWithMessageText:NSLocalizedString(@"Script Message", @"JavaScript alert title") informativeText:message buttons:NSLocalizedString(@"OK", @"JavaScript alert confirmation"), nil];
	[alert beginSheetModalForWindow:[sender window] completionHandler:nil];
}

- (BOOL)webView:(WebView*)sender runJavaScriptConfirmPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	NSAlert* alert        = [[NSAlert alloc] init];
	alert.messageText     = NSLocalizedString(@"Script Message", @"JavaScript alert title");
	alert.informativeText = message;
	[alert addButtons:NSLocalizedString(@"OK", @"JavaScript alert confirmation"), NSLocalizedString(@"Cancel", @"JavaScript alert cancel"), nil];
	return [alert runModal] == NSAlertFirstButtonReturn;
}

- (void)webView:(WebView*)sender runOpenPanelForFileButtonWithResultListener:(id <WebOpenPanelResultListener>)resultListener
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setDirectoryURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
	if([panel runModal] == NSModalResponseOK)
		[resultListener chooseFilename:[[[panel URLs] objectAtIndex:0] path]];
}

- (WebView*)webView:(WebView*)sender createWebViewWithRequest:(NSURLRequest*)request
{
	NSPoint origin = [sender.window cascadeTopLeftFromPoint:NSMakePoint(NSMinX(sender.window.frame), NSMaxY(sender.window.frame))];
	origin.y -= NSHeight(sender.window.frame);

	HOBrowserView* view = [HOBrowserView new];
	NSWindow* window = [[NSWindow alloc] initWithContentRect:(NSRect){origin, NSMakeSize(750, 800)}
																  styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable)
																	 backing:NSBackingStoreBuffered
																		defer:NO];
	[window bind:NSTitleBinding toObject:view.webView withKeyPath:@"mainFrameTitle" options:nil];
	[window setContentView:view];
	[[view.webView mainFrame] loadRequest:request];

	__attribute__ ((unused)) CFTypeRef dummy = CFBridgingRetain(window);
	[window setReleasedWhenClosed:YES];

	return view.webView;
}

- (void)webViewShow:(WebView*)sender
{
	[[sender window] makeKeyAndOrderFront:self];
}

- (void)webViewClose:(WebView*)sender
{
	if(![sender tryToPerform:@selector(toggleHTMLOutput:) with:self])
		[sender tryToPerform:@selector(performClose:) with:self];
	// We cannot re-use WebView objects where window.close() has been executed because of https://bugs.webkit.org/show_bug.cgi?id=121232
	self.needsNewWebView = YES;
}

// This is an undocumented WebView delegate method
- (void)webView:(WebView*)webView addMessageToConsole:(NSDictionary*)dictionary;
{
	if([dictionary respondsToSelector:@selector(objectForKey:)])
		os_log(OS_LOG_DEFAULT, "%{public}@: %{public}@ on line %d\n", webView.mainFrame.dataSource.request.URL.absoluteString, [dictionary objectForKey:@"message"], [[dictionary objectForKey:@"lineNumber"] intValue]);
}

// =====================================================
// = WebResourceLoadDelegate: Redirect tm-file to file =
// =====================================================

- (NSURLRequest*)webView:(WebView*)sender resource:(id)identifier willSendRequest:(NSURLRequest*)request redirectResponse:(NSURLResponse*)redirectResponse fromDataSource:(WebDataSource*)dataSource
{
	if([[[request URL] scheme] isEqualToString:@"tm-file"])
	{
		NSString* fragment = [[request URL] fragment];
		request = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@%s%@", [[[request URL] path] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet], fragment ? "#" : "", fragment ?: @""]]];
	}

	if(IsProtocolRelativeURL([request URL]))
	{
		NSURLComponents* components = [NSURLComponents componentsWithURL:[request URL] resolvingAgainstBaseURL:YES];
		components.scheme = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsDefaultURLProtocolKey];
		request = [NSURLRequest requestWithURL:components.URL];
	}

	if([[request URL] isFileURL])
	{
		NSURL* redirectURL = [NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@?path=%@&error=1", [[[NSBundle bundleForClass:[self class]] pathForResource:@"error_not_found" ofType:@"html"] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet], [[[request URL] path] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLQueryAllowedCharacterSet]]];
		char const* path = [[request URL] fileSystemRepresentation];

		struct stat buf;
		if(path && stat(path, &buf) == 0)
		{
			if(S_ISREG(buf.st_mode) || S_ISLNK(buf.st_mode))
			{
				redirectURL = nil;
			}
			else if(S_ISDIR(buf.st_mode))
			{
				if(path::exists(path::join(path, "index.html")))
				{
					NSString* urlString = [[NSURL URLWithString:@"index.html" relativeToURL:[request URL]] absoluteString];
					if(NSString* query = [[request URL] query])
						urlString = [urlString stringByAppendingFormat:@"?%@", query];
					if(NSString* fragment = [[request URL] fragment])
						urlString = [urlString stringByAppendingFormat:@"#%@", fragment];
					redirectURL = [NSURL URLWithString:urlString];
				}
			}
		}

		if(redirectURL)
			request = [NSURLRequest requestWithURL:redirectURL];
	}

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
@end
