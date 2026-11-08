#import <oak/misc.h>

@class HOStatusBar;
@class HOWKScriptMessageHandler;
@class OakSystemCommandURLSchemeHandler;

@interface HOBrowserView : NSView <WKNavigationDelegate, WKUIDelegate>
@property (nonatomic, readonly) WKWebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@property (nonatomic, readonly) HOStatusBar* statusBar;
@property (nonatomic, readonly) HOWKScriptMessageHandler* scriptMessageHandler;
@property (nonatomic, readonly) OakSystemCommandURLSchemeHandler* systemCommandHandler;
- (void)setUpdatesProgress:(BOOL)flag;
@end
