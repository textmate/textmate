#import "HOWebViewDelegateHelper.h"
#import "HOBrowserView.h"
#import <OakAppKit/NSAlert Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutput_WebViewDelegate);

static NSString* const kUserDefaultsDefaultURLProtocolKey = @"defaultURLProtocol";

static BOOL IsProtocolRelativeURL (NSURL* url)
{
	if([url.scheme hasPrefix:@"x-txmt"] && ![url.host isEqualToString:@"job"])
		return YES;

	if([url.scheme isEqualToString:@"file"] && url.host)
	{
		// If host has a dot and does not exist on disk then treat as protocol-relative URL
		if([url.host containsString:@"."] && ![[NSFileManager defaultManager] fileExistsAtPath:[@"/" stringByAppendingPathComponent:url.host]])
			return YES;
	}

	return NO;
}

@implementation HOWebViewDelegateHelper
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsDefaultURLProtocolKey : @"https",
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
	[self webView:sender setStatusText:[[url absoluteString] stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
}

- (void)webView:(WebView*)sender runJavaScriptAlertPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	NSAlert* alert = [NSAlert tmAlertWithMessageText:NSLocalizedString(@"Script Message", @"JavaScript alert title") informativeText:message buttons:NSLocalizedString(@"OK", @"JavaScript alert confirmation"), nil];
	[alert beginSheetModalForWindow:[sender window] modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
}

- (BOOL)webView:(WebView*)sender runJavaScriptConfirmPanelWithMessage:(NSString*)message initiatedByFrame:(WebFrame*)frame
{
	return NSAlertDefaultReturn == NSRunAlertPanel(NSLocalizedString(@"Script Message", @"JavaScript alert title"), @"%@", NSLocalizedString(@"OK", @"JavaScript alert confirmation"), NSLocalizedString(@"Cancel", @"JavaScript alert cancel"), nil, message);
}

- (void)webView:(WebView*)sender runOpenPanelForFileButtonWithResultListener:(id <WebOpenPanelResultListener>)resultListener
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setDirectoryURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
	if([panel runModal] == NSFileHandlingPanelOKButton)
		[resultListener chooseFilename:[[[panel URLs] objectAtIndex:0] path]];
}

- (WebView*)webView:(WebView*)sender createWebViewWithRequest:(NSURLRequest*)request
{
	D(DBF_HTMLOutput_WebViewDelegate, bug("%s\n", [[request description] UTF8String]););
	NSPoint origin = [sender.window cascadeTopLeftFromPoint:NSMakePoint(NSMinX(sender.window.frame), NSMaxY(sender.window.frame))];
	origin.y -= NSHeight(sender.window.frame);

	HOBrowserView* view = [HOBrowserView new];
	NSWindow* window = [[NSWindow alloc] initWithContentRect:(NSRect){origin, NSMakeSize(750, 800)}
																  styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask)
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
	D(DBF_HTMLOutput_WebViewDelegate, bug("%s\n", [[sender description] UTF8String]););
	[[sender window] makeKeyAndOrderFront:self];
}

- (void)webViewClose:(WebView*)sender
{
	D(DBF_HTMLOutput_WebViewDelegate, bug("\n"););
	if(![sender tryToPerform:@selector(toggleHTMLOutput:) with:self])
		[sender tryToPerform:@selector(performClose:) with:self];
	// We cannot re-use WebView objects where window.close() has been executed because of https://bugs.webkit.org/show_bug.cgi?id=121232
	self.needsNewWebView = YES;
}

// This is an undocumented WebView delegate method
- (void)webView:(WebView*)webView addMessageToConsole:(NSDictionary*)dictionary;
{
	if([dictionary respondsToSelector:@selector(objectForKey:)])
		fprintf(stderr, "%s: %s on line %d\n", [[[[[[webView mainFrame] dataSource] request] URL] absoluteString] UTF8String], [[dictionary objectForKey:@"message"] UTF8String], [[dictionary objectForKey:@"lineNumber"] intValue]);
}

// =====================================================
// = WebResourceLoadDelegate: Redirect tm-file to file =
// =====================================================

- (NSURLRequest*)webView:(WebView*)sender resource:(id)identifier willSendRequest:(NSURLRequest*)request redirectResponse:(NSURLResponse*)redirectResponse fromDataSource:(WebDataSource*)dataSource
{
	D(DBF_HTMLOutput_WebViewDelegate, bug("%s\n", [[request description] UTF8String]););
	if([[[request URL] scheme] isEqualToString:@"tm-file"])
	{
		NSString* fragment = [[request URL] fragment];
		request = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@%s%@", [[[request URL] path] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], fragment ? "#" : "", fragment ?: @""]]];
	}

	if(IsProtocolRelativeURL([request URL]))
	{
		NSURLComponents* components = [NSURLComponents componentsWithURL:[request URL] resolvingAgainstBaseURL:YES];
		components.scheme = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsDefaultURLProtocolKey];
		request = [NSURLRequest requestWithURL:components.URL];
	}

	if([[request URL] isFileURL])
	{
		NSURL* redirectURL = [NSURL URLWithString:[NSString stringWithFormat:@"file://localhost%@?path=%@&error=1", [[[NSBundle bundleForClass:[self class]] pathForResource:@"error_not_found" ofType:@"html"] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], [[[request URL] path] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]]];
		char const* path = [[[request URL] path] fileSystemRepresentation];

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
