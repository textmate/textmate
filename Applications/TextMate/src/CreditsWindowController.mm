#import "CreditsWindowController.h"

@interface CreditsWindowController ()
@property (nonatomic, retain) NSURL* creditsURL;
@end

@implementation CreditsWindowController
@synthesize creditsURL;

- (id)initWithURL:(NSURL*)aURL
{
	if(self = [super initWithWindowNibName:@"CreditsWindow"])
	{
		self.creditsURL = aURL;
	}
	return self;
}

+ (void)showPath:(NSString*)aPath
{
	[[[self alloc] initWithURL:[NSURL fileURLWithPath:aPath]] showWindow:nil];
}

- (void)windowDidLoad
{
	[[webView mainFrame] loadRequest:[NSURLRequest requestWithURL:creditsURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60]];
	[webView setPolicyDelegate:self];
}

- (void)webView:(WebView*)sender decidePolicyForNavigationAction:(NSDictionary*)actionInformation request:(NSURLRequest*)request frame:(WebFrame*)frame decisionListener:(id <WebPolicyDecisionListener>)listener
{
	if([[NSWorkspace sharedWorkspace] openURL:request.URL])
		[listener ignore];
	else if([NSURLConnection canHandleRequest:request])
		[listener use];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[webView setPolicyDelegate:nil];
	[self release];
}
@end
