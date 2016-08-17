#import "browser/HOBrowserView.h"
#import <oak/misc.h>

PUBLIC @interface OakHTMLOutputView : HOBrowserView
- (void)loadRequest:(NSURLRequest*)aRequest environment:(std::map<std::string, std::string> const&)anEnvironment autoScrolls:(BOOL)flag;
- (void)stopLoading;
- (void)loadHTMLString:(NSString*)someHTML;

@property (nonatomic, getter = isRunningCommand, readonly) BOOL runningCommand;
@end
