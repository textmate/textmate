@protocol HOStatusBarDelegate
- (void)goBack:(id)sender;
- (void)goForward:(id)sender;
@end

@interface HOStatusBar : NSView
@property (nonatomic, assign) BOOL isBusy;
@property (nonatomic, assign) CGFloat progress;
@property (nonatomic, retain) NSString* statusText;
@property (nonatomic, assign) BOOL canGoBack;
@property (nonatomic, assign) BOOL canGoForward;

@property (nonatomic, assign) id delegate;
@end
