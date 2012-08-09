#include <oak/debug.h>

@interface CreditsWindowController : NSWindowController
{
	OBJC_WATCH_LEAKS(CreditsWindowController);

	IBOutlet WebView* webView;
	NSURL* creditsURL;
}
+ (void)showPath:(NSString*)aPath;
@end
