#import "browser/HOBrowserView.h"
#import <oak/misc.h>

PUBLIC @interface OakHTMLOutputView : HOBrowserView
- (void)loadRequest:(NSURLRequest*)aRequest environment:(std::map<std::string, std::string> const&)anEnvironment autoScrolls:(BOOL)flag;
- (void)stopLoading;

@property (nonatomic, readonly) BOOL runningCommand;

// Read-only access to the webview is given to allow reading page title, etc.
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@end
