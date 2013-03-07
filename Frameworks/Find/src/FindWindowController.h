extern NSString* const FFSearchInDocument;
extern NSString* const FFSearchInSelection;
extern NSString* const FFSearchInOpenFiles;

@interface FindWindowController : NSWindowController
@property (nonatomic, retain, readonly) NSButton* findAllButton;
@property (nonatomic, retain, readonly) NSButton* replaceAllButton;
@property (nonatomic, retain, readonly) NSButton* replaceAndFindButton;
@property (nonatomic, retain, readonly) NSButton* findPreviousButton;
@property (nonatomic, retain, readonly) NSButton* findNextButton;
@property (nonatomic, retain, readonly) NSOutlineView* resultsOutlineView;

@property (nonatomic) BOOL showsResultsOutlineView;
@property (nonatomic) BOOL disableResultsCheckBoxes;
@property (nonatomic) BOOL showResultsCollapsed;
@property (nonatomic) BOOL showReplacementPreviews;

@property (nonatomic, retain) NSString* projectFolder;
@property (nonatomic, retain) NSString* searchIn;
@property (nonatomic, readonly) NSString* searchFolder;

@property (nonatomic, retain) NSString* findString;
@property (nonatomic, retain) NSString* replaceString;
@property (nonatomic, readonly) NSString* globString;

@property (nonatomic) BOOL ignoreCase;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;
@property (nonatomic) BOOL wrapAround;
@property (nonatomic) BOOL fullWords; // not implemented

@property (nonatomic) BOOL followSymbolicLinks;
@property (nonatomic) BOOL searchHiddenFolders;

@property (nonatomic, getter = isBusy) BOOL busy;
@property (nonatomic, retain) NSString* statusString;

@property (nonatomic, retain) NSString* findErrorString;
- (void)updateFindErrorString;

- (IBAction)selectNextResult:(id)sender;
- (IBAction)selectPreviousResult:(id)sender;

- (void)showSaveChangesAlertAndCallback:(void(^)())callback;
@end
