#import "OakHTMLOutputView.h"
#import "browser/HOStatusBar.h"
#import "helpers/HOAutoScroll.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <oak/debug.h>

extern "C" void* TMCommandOutputCreate(void);
extern "C" void* TMCommandOutputView(void* controller);
extern "C" void TMCommandOutputBegin(void* controller, char const* title);
extern "C" void TMCommandOutputAppend(void* controller, void const* bytes, NSInteger length);
extern "C" void TMCommandOutputFinish(void* controller);
extern "C" void TMCommandOutputRelease(void* controller);

static NSString* const OakUseNativeCommandOutputKey = @"OakUseNativeCommandOutput";

@protocol TMCommandTerminating <NSObject>
@property (nonatomic, readonly) NSUUID* identifier;
- (void)terminate;
@end

@interface OakHTMLOutputView ()
@property (nonatomic, getter = isRunningCommand, readwrite) BOOL runningCommand;
@property (nonatomic) HOAutoScroll* autoScrollHelper;
@property (nonatomic) std::map<std::string, std::string> environment;
@property (nonatomic) NSRect pendingVisibleRect;
@property (nonatomic, getter = isVisible) BOOL visible;
@property (nonatomic) void* nativeOutputController;
@property (nonatomic) NSView* nativeOutputView;
@property (nonatomic) id<TMCommandTerminating> nativeCommand;
@property (nonatomic) NSString* nativeOutputTitle;
@end

@implementation OakHTMLOutputView
+ (void)initialize
{
	if(self == OakHTMLOutputView.class)
		[NSUserDefaults.standardUserDefaults registerDefaults:@{ OakUseNativeCommandOutputKey: @YES }];
}

+ (NSSet*)keyPathsForValuesAffectingMainFrameTitle
{
	return [NSSet setWithObjects:@"webView.mainFrameTitle", nil];
}

- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_reusable = YES;
		if(self.usesNativeOutput)
		{
			self.nativeOutputController = TMCommandOutputCreate();
			self.nativeOutputView = (__bridge NSView*)TMCommandOutputView(self.nativeOutputController);
			self.nativeOutputView.translatesAutoresizingMaskIntoConstraints = NO;
			[self addSubview:self.nativeOutputView];
			[NSLayoutConstraint activateConstraints:@[
				[self.nativeOutputView.leadingAnchor constraintEqualToAnchor:self.leadingAnchor],
				[self.nativeOutputView.trailingAnchor constraintEqualToAnchor:self.trailingAnchor],
				[self.nativeOutputView.topAnchor constraintEqualToAnchor:self.topAnchor],
				[self.nativeOutputView.bottomAnchor constraintEqualToAnchor:self.bottomAnchor],
			]];
			self.webView.hidden = YES;
			self.statusBar.hidden = YES;
		}
	}
	return self;
}

- (void)dealloc
{
	if(self.nativeOutputController)
		TMCommandOutputRelease(self.nativeOutputController);
}

- (BOOL)usesNativeOutput
{
	return [NSUserDefaults.standardUserDefaults boolForKey:OakUseNativeCommandOutputKey];
}

- (void)beginNativeOutputWithTitle:(NSString*)title command:(id<TMCommandTerminating>)command
{
	if(!self.nativeOutputController)
		return;
	self.nativeCommand = command;
	if([command respondsToSelector:@selector(identifier)])
		self.commandIdentifier = [command identifier];
	self.nativeOutputTitle = title ?: @"Command Output";
	self.runningCommand = YES;
	[self willChangeValueForKey:@"mainFrameTitle"];
	TMCommandOutputBegin(self.nativeOutputController, self.nativeOutputTitle.UTF8String);
	[self didChangeValueForKey:@"mainFrameTitle"];
}

- (void)appendNativeOutputBytes:(void const*)bytes length:(size_t)length
{
	if(self.nativeOutputController && bytes && length)
		TMCommandOutputAppend(self.nativeOutputController, bytes, (NSInteger)length);
}

- (void)finishNativeOutput
{
	if(!self.nativeOutputController)
		return;
	TMCommandOutputFinish(self.nativeOutputController);
	self.runningCommand = NO;
	self.nativeCommand = nil;
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
	id<TMCommandTerminating> command = self.nativeCommand ?: [NSURLProtocol propertyForKey:@"command" inRequest:request];
	if(command)
	{
		NSString* processName = self.nativeOutputTitle ?: [NSURLProtocol propertyForKey:@"processName" inRequest:request];
		NSAlert* alert = askUserFlag ? [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"Stop “%@”?", processName] informativeText:@"The job that the task is performing will not be completed." buttons:@"Stop", @"Cancel", nil] : nil;

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
					if(self.usesNativeOutput)
						[command terminate];
					else
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
			if(self.usesNativeOutput)
				[command terminate];
			else
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
	if(self.usesNativeOutput)
		return self.nativeOutputTitle ?: @"Command Output";
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

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	// Sending goBack:/goForward: to a WebView does not call this WebFrameLoadDelegate method
	if(frame == [sender mainFrame])
	{
		self.runningCommand = NO;
		self.autoScrollHelper = nil;

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
	if(frame == sender.mainFrame)
	{
		self.runningCommand = NO;
		self.autoScrollHelper = nil;
	}
	[super webView:sender didFailProvisionalLoadWithError:error forFrame:frame];
}

- (void)webView:(WebView*)sender didFailLoadWithError:(NSError*)error forFrame:(WebFrame*)frame
{
	if(frame == sender.mainFrame)
	{
		self.runningCommand = NO;
		self.autoScrollHelper = nil;
	}
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
