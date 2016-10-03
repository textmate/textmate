#import <HTMLOutput/HTMLOutput.h>
#import <oak/misc.h>

PUBLIC @interface HTMLOutputWindowController : NSObject <NSWindowDelegate>
@property (nonatomic) NSWindow* window;
@property (nonatomic) OakHTMLOutputView* htmlOutputView;

- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier;
- (void)showWindow:(id)sender;
- (void)close;
@end
