#include <oak/misc.h>

@protocol HOWKScrollSyncDelegate <NSObject>
- (void)scrollSyncDidReceiveRatio:(double)ratio;
@end

@interface OakHTMLOutputView : NSView
- (void)loadRequest:(NSURLRequest*)aRequest environment:(std::map<std::string, std::string> const&)anEnvironment autoScrolls:(BOOL)flag;
- (void)stopLoadingWithUserInteraction:(BOOL)askUserFlag completionHandler:(void(^)(BOOL didStop))handler;
- (void)setContent:(NSString*)someHTML;
- (void)setScrollSyncEnabled:(BOOL)enabled delegate:(id<HOWKScrollSyncDelegate>)delegate;
- (void)scrollPreviewToRatio:(double)ratio;
- (void)setLineSyncEnabled:(BOOL)enabled;
- (void)setMarkdownSource:(NSString*)source;
- (void)highlightPreviewAtLine:(NSInteger)lineNumber lineCount:(NSInteger)lineCount;

@property (nonatomic) NSUUID* commandIdentifier; // UUID from initial load request
@property (nonatomic, getter = isRunningCommand, readonly) BOOL runningCommand;
@property (nonatomic, getter = isVisible, readonly) BOOL visible;
@property (nonatomic, getter = isReusable) BOOL reusable;
@property (nonatomic) BOOL disableJavaScriptAPI;

// Read-only access to the webview is given to allow reading page title, etc.
@property (nonatomic, readonly) WKWebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView; // retained, always NO
@end
