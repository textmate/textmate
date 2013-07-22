// Also defined in Find.h
extern NSString* const FFSearchInDocument;
extern NSString* const FFSearchInSelection;
extern NSString* const FFSearchInOpenFiles;

@interface FindWindowController : NSWindowController
@property (nonatomic, readonly) NSButton* findAllButton;
@property (nonatomic, readonly) NSButton* replaceAllButton;
@property (nonatomic, readonly) NSButton* replaceAndFindButton;
@property (nonatomic, readonly) NSButton* findPreviousButton;
@property (nonatomic, readonly) NSButton* findNextButton;
@property (nonatomic, readonly) NSOutlineView* resultsOutlineView;

@property (nonatomic) BOOL showsResultsOutlineView;
@property (nonatomic) BOOL disableResultsCheckBoxes;
@property (nonatomic) BOOL showResultsCollapsed;
@property (nonatomic) BOOL showReplacementPreviews;

@property (nonatomic) NSString* projectFolder;
@property (nonatomic) NSString* searchIn;
@property (nonatomic, readonly) NSString* searchFolder;

@property (nonatomic) NSString* findString;
@property (nonatomic) NSString* replaceString;
@property (nonatomic, readonly) NSString* globString;

@property (nonatomic) BOOL ignoreCase;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;
@property (nonatomic) BOOL wrapAround;
@property (nonatomic) BOOL fullWords; // not implemented

@property (nonatomic) BOOL followSymbolicLinks;
@property (nonatomic) BOOL searchHiddenFolders;

@property (nonatomic, getter = isBusy) BOOL busy;
@property (nonatomic) NSString* statusString;

@property (nonatomic) NSString* findErrorString;
- (void)updateFindErrorString;

- (IBAction)selectNextResult:(id)sender;
- (IBAction)selectPreviousResult:(id)sender;
@end
