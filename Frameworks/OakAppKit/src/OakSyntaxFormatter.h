@class OakTheme;

@interface OakSyntaxFormatter : NSFormatter
- (instancetype)initWithGrammarName:(NSString*)grammarName;
- (void)addStylesToString:(NSMutableAttributedString*)str;
@property (nonatomic) BOOL enabled;
@property (nonatomic, strong) OakTheme* theme;
@end
