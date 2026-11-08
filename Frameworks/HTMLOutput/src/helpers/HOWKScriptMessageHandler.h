#import <WebKit/WebKit.h>

@protocol HOJSBridgeDelegate;

@interface HOWKScriptMessageHandler : NSObject <WKScriptMessageHandler>
@property (nonatomic, weak) id<HOJSBridgeDelegate> delegate;
@property (nonatomic, weak) WKWebView* webView;
- (void)setEnvironment:(const std::map<std::string, std::string>&)variables;
- (std::map<std::string, std::string> const&)environment;
- (void)cleanup;
@end
