#include <oak/misc.h>

extern NSString* const kCommandRunnerURLScheme;

@interface OakHTMLOutputView : NSView
- (void)setEnvironment:(std::map<std::string, std::string> const&)anEnvironment;
- (void)loadRequest:(NSURLRequest*)aRequest autoScrolls:(BOOL)flag;
- (void)stopLoading;

@property (nonatomic, readonly) BOOL runningCommand;

// Read-only access to the webview is given to allow reading page title, etc.
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@end

namespace command { struct runner_t; typedef std::shared_ptr<runner_t> runner_ptr; }
PUBLIC NSURLRequest* URLRequestForCommandRunner (command::runner_ptr aRunner);
