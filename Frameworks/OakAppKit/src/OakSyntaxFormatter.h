#include <oak/misc.h>

PUBLIC @interface OakSyntaxFormatter : NSFormatter
- (instancetype)initWithGrammarName:(NSString*)grammarName;
- (void)addStylesToString:(NSMutableAttributedString*)str;
@property (nonatomic) BOOL enabled;
@end
