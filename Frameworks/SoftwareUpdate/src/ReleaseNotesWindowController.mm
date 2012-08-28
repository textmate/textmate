#import "ReleaseNotesWindowController.h"

static NSString* const kUserDefaultsReleaseNotesDigestKey = @"releaseNotesDigest";

static NSData* Digest (NSString* someString)
{
	char const* str = [someString UTF8String];
	char md[SHA_DIGEST_LENGTH];
	CC_SHA1((unsigned char*)str, strlen(str), (unsigned char*)md);
	return [NSData dataWithBytes:md length:sizeof(md)];
}

@interface ReleaseNotesWindowController ()
@property (nonatomic, retain) NSURL* releaseNotesURL;
@end

@implementation ReleaseNotesWindowController
@synthesize releaseNotesURL;

- (id)initWithURL:(NSURL*)aURL
{
	if(self = [super initWithWindowNibName:@"ReleaseNotes"])
	{
		self.releaseNotesURL = aURL;
	}
	return self;
}

+ (void)showPath:(NSString*)aPath
{
	[[[self alloc] initWithURL:[NSURL fileURLWithPath:aPath]] showWindow:nil];
}

+ (void)showPathIfUpdated:(NSString*)aPath
{
	if(NSString* releaseNotes = [NSString stringWithContentsOfFile:aPath encoding:NSUTF8StringEncoding error:NULL])
	{
		NSData* lastDigest    = [[NSUserDefaults standardUserDefaults] dataForKey:kUserDefaultsReleaseNotesDigestKey];
		NSData* currentDigest = Digest(releaseNotes);
		if(lastDigest)
		{
			if(![lastDigest isEqualToData:currentDigest])
				[[[self alloc] initWithURL:[NSURL fileURLWithPath:aPath]] showWindow:nil];
		}
		else
		{
			[[NSUserDefaults standardUserDefaults] setObject:currentDigest forKey:kUserDefaultsReleaseNotesDigestKey];
		}
	}
}

- (void)windowDidLoad
{
	[[webView mainFrame] loadRequest:[NSURLRequest requestWithURL:releaseNotesURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60]];
	[webView setPolicyDelegate:self];
	[self.window makeKeyAndOrderFront:self];
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
	if(NSString* releaseNotes = [NSString stringWithContentsOfFile:[releaseNotesURL path] encoding:NSUTF8StringEncoding error:NULL])
		[[NSUserDefaults standardUserDefaults] setObject:Digest(releaseNotes) forKey:kUserDefaultsReleaseNotesDigestKey];
	[webView setPolicyDelegate:nil];
	[self release];
}
@end
