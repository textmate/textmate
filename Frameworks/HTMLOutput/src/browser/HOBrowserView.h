#import <oak/misc.h>

@class HOStatusBar;

@interface HOBrowserView : NSView <WebFrameLoadDelegate>
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@property (nonatomic, readonly) HOStatusBar* statusBar;
- (void)setUpdatesProgress:(BOOL)flag;
@end
