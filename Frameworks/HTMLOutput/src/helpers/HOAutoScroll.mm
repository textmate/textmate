#import "HOAutoScroll.h"
#import <WebKit/WebKit.h>

@interface HOAutoScroll ()
@property (nonatomic, weak) WKWebView* webView;
@end

@implementation HOAutoScroll
- (instancetype)initWithWebView:(WKWebView*)webView
{
	if(self = [super init])
	{
		_webView = webView;
		[self injectAutoScrollScript];
	}
	return self;
}

- (void)injectAutoScrollScript
{
	// Inject at document start, observe document.documentElement which exists before <body>.
	// MutationObserver auto-scrolls to bottom when content is appended,
	// but stops if user scrolls away from bottom.
	NSString* js = @"(function() {"
		"  var _atBottom = true;"
		"  window.addEventListener('scroll', function() {"
		"    _atBottom = (window.innerHeight + window.scrollY) >= (document.body.scrollHeight - 5);"
		"  });"
		"  new MutationObserver(function() {"
		"    if(_atBottom && document.body)"
		"      window.scrollTo(0, document.body.scrollHeight);"
		"  }).observe(document.documentElement, {childList: true, subtree: true, characterData: true});"
		"})();";
	[_webView evaluateJavaScript:js completionHandler:nil];
}

- (void)dealloc
{
	// MutationObserver is automatically cleaned up when the page unloads
}
@end
