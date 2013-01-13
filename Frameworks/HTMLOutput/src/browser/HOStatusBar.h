#import <OakAppKit/OakStatusBar.h>

@protocol HOStatusBarDelegate
- (void)goBack:(id)sender;
- (void)goForward:(id)sender;
@end

@interface HOStatusBar : OakStatusBar
@property (nonatomic, assign) BOOL isBusy;
@property (nonatomic, assign) CGFloat progress;
@property (nonatomic, copy)   NSString* statusText;
@property (nonatomic, assign) BOOL canGoBack;
@property (nonatomic, assign) BOOL canGoForward;

@property (nonatomic, assign) id delegate;
@end
