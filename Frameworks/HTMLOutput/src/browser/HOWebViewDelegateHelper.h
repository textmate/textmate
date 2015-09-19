@protocol HOWebViewDelegateHelperProtocol
@property (nonatomic) NSString* statusText;
@end

#if !defined(MAC_OS_X_VERSION_10_11) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_11)
@interface HOWebViewDelegateHelper : NSObject
#else
@interface HOWebViewDelegateHelper : NSObject <WebResourceLoadDelegate, WebUIDelegate>
#endif
@property (nonatomic, weak) id /*<HOWebViewDelegateHelperProtocol>*/ delegate;
@property (nonatomic) BOOL needsNewWebView;
@end
