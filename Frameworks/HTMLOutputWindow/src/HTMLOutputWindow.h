#import <HTMLOutput/HTMLOutput.h>
#import <oak/misc.h>

PUBLIC @interface HTMLOutputWindowController : NSWindowController
@property (nonatomic) OakHTMLOutputView* htmlOutputView;
- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier;
@end
