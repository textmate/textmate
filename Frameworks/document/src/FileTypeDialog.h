enum enabled_grammar_t { kEnabledGrammarsRecommended = 0, kEnabledGrammarsInstalled = 1, kEnabledGrammarsAll = 2 };

@interface FileTypeDialog : NSWindowController
{
	IBOutlet NSTextField* alertTextField;
	IBOutlet NSTextField* infoTextField;
	IBOutlet NSTableView* fileTypesTableView;
	IBOutlet NSButton* useForAllCheckBox;

	IBOutlet NSWindow* installingBundleWindow;
	IBOutlet NSTextField* installingBundleActivityTextField;
	IBOutlet NSProgressIndicator* installingBundleProgressIndicator;
}
@property (nonatomic) NSInteger enabledGrammars;
@property (nonatomic) NSArray* grammars;
@property (nonatomic) NSIndexSet* selectedGrammarIndexes;
@property (nonatomic) BOOL persistentSetting;
@property (nonatomic) BOOL canOpenDocument;

- (id)initWithPath:(NSString*)aPath first:(char const*)firstPointer last:(char const*)lastPointer;
- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSString* fileType))aCompletionHandler;

- (IBAction)performOpenDocument:(id)sender;
- (IBAction)performCancelOperation:(id)sender;
@end
