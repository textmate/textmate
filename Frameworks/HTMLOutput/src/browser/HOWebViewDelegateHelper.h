@protocol HOWebViewDelegateHelperProtocol
@property (nonatomic, retain) NSString* statusText;
@end

@interface HOWebViewDelegateHelper : NSObject
@property (nonatomic, weak) id /*<HOWebViewDelegateHelperProtocol>*/ delegate;
@property (nonatomic, retain) NSString* projectUUID;
@property (nonatomic) BOOL needsNewWebView;
@end
