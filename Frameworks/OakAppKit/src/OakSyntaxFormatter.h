#include <oak/misc.h>

PUBLIC @interface OakSyntaxFormatter : NSFormatter <NSCopying>
- (instancetype)initWithGrammarName:(NSString*)grammarName;
- (void)addStylesToString:(NSMutableAttributedString*)str;
@property (nonatomic) BOOL enabled;
@property (nonatomic, readonly) NSValueTransformerName transformerName;
@property (nonatomic, readonly) NSValueTransformerName pasteboardEntryTransformerName;
@property (nonatomic, readonly) NSValueTransformerName pasteboardEntryRegexOnlyTransformerName;
@end
