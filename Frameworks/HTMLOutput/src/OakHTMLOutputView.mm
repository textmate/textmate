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
@property (nonatomic) NSRect pendingVisibleRect;
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

- (void)loadHTMLString:(NSString*)someHTML
{
	self.pendingVisibleRect = [[[[self.webView mainFrame] frameView] documentView] visibleRect];
	[[self.webView mainFrame] loadHTMLString:someHTML baseURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
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
	{
		[self webView:sender didClearWindowObject:[frame windowObject] forFrame:frame];

		// This happens when we redirect to a PDF file
		if(self.window.firstResponder == self.window)
		{
			NSRect rect = [sender frame];
			for(NSView* view = [sender hitTest:NSMakePoint(NSMidX(rect), NSMidY(rect))]; view; view = [view superview])
			{
				if([view acceptsFirstResponder])
				{
					[self.window makeFirstResponder:view];
					break;
				}
			}
		}
	}

	if(!NSEqualRects(self.pendingVisibleRect, NSZeroRect))
		[[[[self.webView mainFrame] frameView] documentView] scrollRectToVisible:self.pendingVisibleRect];
	self.pendingVisibleRect = NSZeroRect;

	[super webView:sender didFinishLoadForFrame:frame];
}

- (void)webView:(WebView*)sender didFailProvisionalLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
	[super webView:sender didFailProvisionalLoadWithError:error forFrame:frame];
}

- (void)webView:(WebView*)sender didFailLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	self.autoScrollHelper = nil;
	[super webView:sender didFailLoadWithError:error forFrame:frame];
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

// ====================
// = Printing Support =
// ====================

- (IBAction)printDocument:(id)sender
{
	NSPrintOperation* printer = [NSPrintOperation printOperationWithView:self.webView.mainFrame.frameView.documentView];
	[[printer printPanel] setOptions:[[printer printPanel] options] | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation];

	NSPrintInfo* info = [printer printInfo];

	NSRect display = NSIntersectionRect(info.imageablePageBounds, (NSRect){ NSZeroPoint, info.paperSize });
	info.leftMargin   = NSMinX(display);
	info.rightMargin  = info.paperSize.width - NSMaxX(display);
	info.topMargin    = info.paperSize.height - NSMaxY(display);
	info.bottomMargin = NSMinY(display);

	[printer runOperationModalForWindow:self.window delegate:nil didRunSelector:NULL contextInfo:nil];
}
@end
