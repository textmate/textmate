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
	std::map<std::string, std::string> environment;
}
@property (nonatomic, assign) BOOL runningCommand;
@property (nonatomic, retain) HOAutoScroll* autoScrollHelper;
@end

@implementation OakHTMLOutputView
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		self.autoScrollHelper = [HOAutoScroll new];
	}
	return self;
}

- (void)setEnvironment:(std::map<std::string, std::string> const&)anEnvironment
{
	environment = anEnvironment;

	if(environment.find("TM_PROJECT_UUID") != environment.end())
			self.projectUUID = [NSString stringWithCxxString:environment["TM_PROJECT_UUID"]];
	else	self.projectUUID = nil;
}

- (void)loadRequest:(NSURLRequest*)aRequest autoScrolls:(BOOL)flag
{
	_autoScrollHelper.webFrame = flag ? self.webView.mainFrame.frameView : nil;
	self.runningCommand = [[[aRequest URL] scheme] isEqualToString:kCommandRunnerURLScheme];
	[self.webView.mainFrame loadRequest:aRequest];
}

- (void)stopLoading
{
	[self.webView.mainFrame stopLoading];
}

- (void)webView:(WebView*)sender didStartProvisionalLoadForFrame:(WebFrame*)frame
{
	self.statusBar.isBusy = YES;
	if(NSString* scheme = [[[[[self.webView mainFrame] provisionalDataSource] request] URL] scheme])
		[self setUpdatesProgress:![scheme isEqualToString:kCommandRunnerURLScheme]];
}

// =================
// = Script object =
// =================

- (void)webView:(WebView*)sender didClearWindowObject:(WebScriptObject*)windowScriptObject forFrame:(WebFrame*)frame
{
	NSString* scheme = [[[[[self.webView mainFrame] dataSource] request] URL] scheme];
	if([@[ kCommandRunnerURLScheme, @"tm-file", @"file" ] containsObject:scheme])
	{
		HOJSBridge* bridge = [HOJSBridge new];
		[bridge setDelegate:self.statusBar];
		[bridge setEnvironment:environment];
		[windowScriptObject setValue:bridge forKey:@"TextMate"];
	}
}

- (void)webView:(WebView*)sender didFinishLoadForFrame:(WebFrame*)frame
{
	self.runningCommand = NO;
	if(frame == [sender mainFrame])
		[self webView:sender didClearWindowObject:[frame windowObject] forFrame:frame];
	[super webView:sender didFinishLoadForFrame:frame];
}
@end
