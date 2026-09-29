#import "OakHTMLOutputView.h"
#import "browser/HOStatusBar.h"
#import "helpers/HOAutoScroll.h"
#import "helpers/HOJSBridge.h"
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakAppKit/src/NSAlert Additions.h>
#import <oak/debug.h>

@interface HOStatusBar (BusyAndProgressProperties) <HOJSBridgeDelegate>
@end

@interface OakHTMLOutputView ()
@property (nonatomic, getter = isRunningCommand, readwrite) BOOL runningCommand;
@property (nonatomic) HOAutoScroll* autoScrollHelper;
@property (nonatomic) std::map<std::string, std::string> environment;
@property (nonatomic) NSRect pendingVisibleRect;
@property (nonatomic, getter = isVisible) BOOL visible;
@end

@implementation OakHTMLOutputView
+ (NSSet*)keyPathsForValuesAffectingMainFrameTitle
{
	return [NSSet setWithObjects:@"webView.mainFrameTitle", nil];
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
	{
		self.autoScrollHelper = [HOAutoScroll new];
		self.autoScrollHelper.webFrame = self.webView.mainFrame.frameView;
	}

	self.environment = anEnvironment;
	self.commandIdentifier = [NSURLProtocol propertyForKey:@"commandIdentifier" inRequest:aRequest];
	self.runningCommand = self.commandIdentifier != nil;

	[self willChangeValueForKey:@"mainFrameTitle"];
	[self.webView.mainFrame loadRequest:aRequest];
	[self didChangeValueForKey:@"mainFrameTitle"];
}

- (void)stopLoadingWithUserInteraction:(BOOL)askUserFlag completionHandler:(void(^)(BOOL didStop))handler
{
	NSURLRequest* request = self.webView.mainFrame.dataSource.initialRequest;
	if(id command = [NSURLProtocol propertyForKey:@"command" inRequest:request])
	{
		NSAlert* alert = askUserFlag ? [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"Stop “%@”?", [NSURLProtocol propertyForKey:@"processName" inRequest:request]] informativeText:@"The job that the task is performing will not be completed." buttons:@"Stop", @"Cancel", nil] : nil;

		__weak __block id token = [NSNotificationCenter.defaultCenter addObserverForName:@"OakCommandDidTerminateNotification" object:command queue:nil usingBlock:^(NSNotification* notification){
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
					[self.webView.mainFrame stopLoading];
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
			[self.webView.mainFrame stopLoading];
		}
	}
	else
	{
		handler(YES);
	}
}

- (void)setContent:(NSString*)someHTML
{
	self.pendingVisibleRect = [[[[self.webView mainFrame] frameView] documentView] visibleRect];
	[[self.webView mainFrame] loadHTMLString:someHTML baseURL:[NSURL fileURLWithPath:NSHomeDirectory()]];
}

- (NSString*)mainFrameTitle
{
	if(OakIsEmptyString(self.webView.mainFrameTitle))
	{
		WebFrame* frame = self.webView.mainFrame;
		if(NSURLRequest* request = (frame.provisionalDataSource ?: frame.dataSource).initialRequest)
			return [NSURLProtocol propertyForKey:@"processName" inRequest:request] ?: @"";
	}
	return self.webView.mainFrameTitle;
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

// =======================
// = Frame Load Delegate =
// =======================

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	self.statusBar.busy = YES;
	[self setUpdatesProgress:!self.isRunningCommand];
}

- (void)webView:(WebView*)sender didClearWindowObject:(WebScriptObject*)windowScriptObject forFrame:(WebFrame*)frame
{
	if(self.disableJavaScriptAPI)
		return;

	NSString* scheme = [[[[[self.webView mainFrame] dataSource] request] URL] scheme];
	if(self.isRunningCommand || [@[ @"tm-file", @"file" ] containsObject:scheme])
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

	if(!NSIsEmptyRect(self.pendingVisibleRect))
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
			[NSWorkspace.sharedWorkspace openURL:url];
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
