#import <HTMLOutput/HTMLOutput.h>
@interface HTMLOutputWindowController : NSWindowController
@property (nonatomic) OakHTMLOutputView* htmlOutputView;
- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier;
@end
