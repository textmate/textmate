@protocol HOWebViewDelegateHelperProtocol
@property (nonatomic) NSString* statusText;
@end

// HOWebViewDelegateHelper is retained for compatibility but its responsibilities
// (resource interception, UI delegate methods) have moved to HOBrowserView and
// the WKURLSchemeHandler implementations.
@interface HOWebViewDelegateHelper : NSObject
@property (nonatomic, weak) id /*<HOWebViewDelegateHelperProtocol>*/ delegate;
@end
