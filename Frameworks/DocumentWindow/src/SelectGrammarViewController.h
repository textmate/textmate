@class BundleGrammar;
@class OakDocumentView;

typedef NS_ENUM(NSInteger, SelectGrammarResponse) {
	SelectGrammarResponseInstall = 0,
	SelectGrammarResponseNotNow,
	SelectGrammarResponseNever,
	SelectGrammarResponseCount
};

@interface SelectGrammarViewController : NSViewController
@property (nonatomic) NSString* documentDisplayName;
- (void)showGrammars:(NSArray<BundleGrammar*>*)grammars forView:(OakDocumentView*)documentView completionHandler:(void(^)(SelectGrammarResponse, BundleGrammar*))callback;
- (void)dismiss;
@end
