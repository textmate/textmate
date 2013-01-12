#import <document/document.h>
#import <command/runner.h>

PUBLIC @interface DocumentController : NSObject
@property (nonatomic) NSWindow*                                  window;

@property (nonatomic) NSString*                                  identifier;
@property (nonatomic) NSString*                                  defaultProjectPath;
@property (nonatomic, readonly) NSString*                        projectPath; // effectiveProjectPath

@property (nonatomic) std::vector<document::document_ptr> const& documents;
@property (nonatomic) document::document_ptr              const& selectedDocument;
@property (nonatomic) NSUInteger                                 selectedTabIndex;

@property (nonatomic) BOOL                                       fileBrowserVisible;
@property (nonatomic) NSDictionary*                              fileBrowserHistory;
@property (nonatomic) CGFloat                                    fileBrowserWidth;

@property (nonatomic) BOOL                                       htmlOutputVisible;
@property (nonatomic) NSSize                                     htmlOutputSize;

+ (BOOL)restoreSession;
+ (BOOL)saveSessionIncludingUntitledDocuments:(BOOL)includeUntitled;
+ (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;

- (void)showWindow:(id)sender;
- (void)openAndSelectDocument:(document::document_ptr const&)aDocument;
- (void)close;

- (IBAction)newDocumentInTab:(id)sender;
- (IBAction)moveDocumentToNewWindow:(id)sender; // TODO Move to AppController
- (IBAction)mergeAllWindows:(id)sender;         // TODO Move to AppController

- (IBAction)goToFileCounterpart:(id)sender;
- (IBAction)selectNextTab:(id)sender;
- (IBAction)selectPreviousTab:(id)sender;
- (IBAction)takeSelectedTabIndexFrom:(id)sender;

- (NSPoint)positionForWindowUnderCaret;
- (void)performBundleItem:(bundles::item_ptr const&)anItem;
- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner;
- (IBAction)toggleHTMLOutput:(id)sender;

- (IBAction)performCloseTab:(id)sender;
- (IBAction)performCloseSplit:(id)sender;
- (IBAction)performCloseWindow:(id)sender;
- (IBAction)performCloseOtherTabs:(id)sender;

- (IBAction)saveDocument:(id)sender;
- (IBAction)saveDocumentAs:(id)sender;
- (IBAction)saveAllDocuments:(id)sender;
// - (IBAction)revertDocumentToSaved:(id)sender;

// =============================
// = Opening Auxiliary Windows =
// =============================

- (IBAction)orderFrontFindPanel:(id)sender;
- (IBAction)showSymbolChooser:(id)sender;
- (IBAction)goToFile:(id)sender;

// ==================
// = OakFileBrowser =
// ==================

- (IBAction)toggleFileBrowser:(id)sender;
- (IBAction)revealFileInProject:(id)sender;
- (IBAction)goToProjectFolder:(id)sender;

- (IBAction)goBack:(id)sender;
- (IBAction)goForward:(id)sender;
- (IBAction)goToParentFolder:(id)sender;
- (IBAction)goToComputer:(id)sender;
- (IBAction)goToHome:(id)sender;
- (IBAction)goToDesktop:(id)sender;
- (IBAction)goToFavorites:(id)sender;
- (IBAction)goToSCMDataSource:(id)sender;
- (IBAction)orderFrontGoToFolder:(id)sender;

// ==============
// = Legacy API =
// ==============

+ (instancetype)controllerForDocument:(document::document_ptr const&)aDocument;
- (void)updateVariables:(std::map<std::string, std::string>&)env;

// Private (used by DocumentCommand.mm)
@property (nonatomic, readonly) NSString* scopeAttributes;
- (NSString*)untitledSavePath;
@end
