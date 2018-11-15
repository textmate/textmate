@protocol HOStatusBarDelegate
- (void)goBack:(id)sender;
- (void)goForward:(id)sender;
@end

@interface HOStatusBar : NSVisualEffectView
@property (nonatomic, weak) id              delegate;

@property (nonatomic) NSString*             statusText;
@property (nonatomic) CGFloat               progress;
@property (nonatomic, getter = isBusy) BOOL busy;
@property (nonatomic) BOOL                  canGoBack;
@property (nonatomic) BOOL                  canGoForward;
@end
