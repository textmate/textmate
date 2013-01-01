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

	NSString* path;
	std::string firstLine;

	NSInteger enabledGrammars;
	BOOL persistentSetting;
	BOOL canOpenDocument;

	NSArray* recommendedGrammars;
	NSArray* installedGrammars;
	NSArray* allGrammars;

	NSArray* grammars;
	NSIndexSet* selectedGrammarIndexes;

	NSString* alertFormatString;
	NSString* infoFormatString;
	NSString* useForAllFormatString;
}
@property (nonatomic, retain) NSString* path;
@property (nonatomic, assign) NSInteger enabledGrammars;
@property (nonatomic, assign) BOOL persistentSetting;
@property (nonatomic, assign) BOOL canOpenDocument;

@property (nonatomic, retain) NSArray* recommendedGrammars;
@property (nonatomic, retain) NSArray* installedGrammars;
@property (nonatomic, retain) NSArray* allGrammars;

@property (nonatomic, retain) NSArray* grammars;
@property (nonatomic, retain) NSIndexSet* selectedGrammarIndexes;

@property (nonatomic, retain) NSString* alertFormatString;
@property (nonatomic, retain) NSString* infoFormatString;
@property (nonatomic, retain) NSString* useForAllFormatString;

@property (nonatomic, readonly) NSDictionary* grammar;
@property (nonatomic, readonly) NSString* fileType;

- (id)initWithPath:(NSString*)aPath first:(char const*)firstPointer last:(char const*)lastPointer;
- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSString* fileType))aCompletionHandler;

- (IBAction)performOpenDocument:(id)sender;
- (IBAction)performCancelOperation:(id)sender;
@end
