@class OakPasteboard;

@interface FFTextFieldViewController : NSViewController
@property (nonatomic) BOOL syntaxHighlightEnabled;
@property (nonatomic) BOOL hasFocus;

- (instancetype)initWithPasteboard:(OakPasteboard*)pasteboard grammarName:(NSString*)grammarName;
- (void)showHistory:(id)sender;
- (void)showPopoverWithString:(NSString*)aString;
@end
