#import <WebKit/WebKit.h>

@protocol HOJSBridgeDelegate;
@protocol HOWKScrollSyncDelegate;

@interface HOWKScriptMessageHandler : NSObject <WKScriptMessageHandler>
@property (nonatomic, weak) id<HOJSBridgeDelegate> delegate;
@property (nonatomic, weak) WKWebView* webView;
@property (nonatomic, weak) id<HOWKScrollSyncDelegate> scrollSyncDelegate;
- (void)setEnvironment:(const std::map<std::string, std::string>&)variables;
- (std::map<std::string, std::string> const&)environment;
- (void)cleanup;
@end
