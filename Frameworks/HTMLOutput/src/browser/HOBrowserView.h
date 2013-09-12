#import <oak/misc.h>

@class HOStatusBar;

@interface HOBrowserView : NSView
@property (nonatomic, readonly) WebView* webView;
@property (nonatomic, readonly) BOOL needsNewWebView;
@property (nonatomic, readonly) HOStatusBar* statusBar;
@property (nonatomic, retain) NSString* projectUUID;
- (void)setUpdatesProgress:(BOOL)flag;
@end
