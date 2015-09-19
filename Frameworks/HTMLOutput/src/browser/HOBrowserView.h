#import <oak/misc.h>

@class HOStatusBar;

#if !defined(MAC_OS_X_VERSION_10_11) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_11)
@interface HOBrowserView : NSView
#else
@interface HOBrowserView : NSView <WebFrameLoadDelegate>
#endif
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@property (nonatomic, readonly) HOStatusBar* statusBar;
- (void)setUpdatesProgress:(BOOL)flag;
@end
