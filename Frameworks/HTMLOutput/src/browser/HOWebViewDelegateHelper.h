@protocol HOWebViewDelegateHelperProtocol
@property (nonatomic) NSString* statusText;
@end

@interface HOWebViewDelegateHelper : NSObject <WebResourceLoadDelegate, WebUIDelegate>
@property (nonatomic, weak) id /*<HOWebViewDelegateHelperProtocol>*/ delegate;
@property (nonatomic) BOOL needsNewWebView;
@end
