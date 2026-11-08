#import "HOWebViewDelegateHelper.h"

// All WebResourceLoadDelegate and WebUIDelegate methods have been migrated:
// - tm-file:// rewriting -> OakFileURLSchemeHandler (WKURLSchemeHandler)
// - JavaScript alert/confirm panels -> HOBrowserView (WKUIDelegate)
// - createWebViewWithRequest: -> HOBrowserView (WKUIDelegate)
// - webViewClose: -> HOBrowserView webViewDidClose:
// - Console logging -> HOWKScriptMessageHandler (WKScriptMessageHandler)
// - HTMLTMFileDummyProtocol -> OakFileURLSchemeHandler

@implementation HOWebViewDelegateHelper
@end
