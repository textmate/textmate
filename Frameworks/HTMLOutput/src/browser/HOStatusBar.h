#import <OakAppKit/OakStatusBar.h>

@protocol HOStatusBarDelegate
- (void)goBack:(id)sender;
- (void)goForward:(id)sender;
@end

@interface HOStatusBar : OakStatusBar
{
	BOOL isBusy;
	NSString* statusText;
	BOOL canGoBack;
	BOOL canGoForward;

	NSProgressIndicator* spinner;
	NSProgressIndicator* progressIndicator;

	id delegate;
}
@property (nonatomic, assign) BOOL isBusy;
@property (nonatomic, assign) double progress;
@property (nonatomic, copy)   NSString* statusText;
@property (nonatomic, assign) BOOL canGoBack;
@property (nonatomic, assign) BOOL canGoForward;

@property (nonatomic, assign) id delegate;
@end
