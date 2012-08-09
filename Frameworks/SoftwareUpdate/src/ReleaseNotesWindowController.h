#include <oak/debug.h>

@interface ReleaseNotesWindowController : NSWindowController
{
	OBJC_WATCH_LEAKS(ReleaseNotesWindowController);

	IBOutlet WebView* webView;
	NSURL* releaseNotesURL;
}
+ (void)showPath:(NSString*)aPath;
+ (void)showPathIfUpdated:(NSString*)aPath;
@end
