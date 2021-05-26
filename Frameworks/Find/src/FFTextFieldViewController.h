@class OakPasteboard;

@interface FFTextFieldViewController : NSViewController
@property (nonatomic) BOOL syntaxHighlightEnabled;
@property (nonatomic) BOOL hasFocus;
@property (nonatomic) NSString* stringValue;

- (instancetype)initWithPasteboard:(OakPasteboard*)pasteboard grammarName:(NSString*)grammarName;
@end
