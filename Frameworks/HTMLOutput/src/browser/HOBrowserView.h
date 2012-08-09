#import <oak/debug.h>

@class HOStatusBar;
@class HOWebViewDelegateHelper;

@interface HOBrowserView : NSView
{
	OBJC_WATCH_LEAKS(HOBrowserView);
	WebView* webView;
	HOStatusBar* statusBar;
	HOWebViewDelegateHelper* webViewDelegateHelper;
}
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, retain) NSString* projectUUID;
- (void)setUpdatesProgress:(BOOL)flag;
@end
