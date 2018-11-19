#import <bundles/item.h>

@class OakDocument;

PUBLIC @interface DocumentWindowController : NSResponder
@property (nonatomic) NSWindow*                                  window;

@property (nonatomic) NSUUID*                                    identifier;
@property (nonatomic) NSString*                                  defaultProjectPath;
@property (nonatomic, readonly) NSString*                        projectPath; // effectiveProjectPath
@property (nonatomic, readonly) NSString*                        untitledSavePath;

@property (nonatomic, readonly) NSArray<OakDocument*>*           documents;
@property (nonatomic, readonly) OakDocument*                     selectedDocument;
@property (nonatomic) NSUInteger                                 selectedTabIndex;

@property (nonatomic) BOOL                                       fileBrowserVisible;
@property (nonatomic) NSDictionary*                              fileBrowserHistory;
@property (nonatomic) CGFloat                                    fileBrowserWidth;

@property (nonatomic) BOOL                                       htmlOutputVisible;
@property (nonatomic) NSSize                                     htmlOutputSize;

+ (BOOL)restoreSession;
+ (void)disableSessionSave;
+ (void)enableSessionSave;
+ (BOOL)saveSessionIncludingUntitledDocuments:(BOOL)includeUntitled;
+ (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;

- (void)showWindow:(id)sender;
- (void)close;

- (IBAction)newFolder:(id)sender;
- (IBAction)newDocumentInTab:(id)sender;
- (IBAction)newDocumentInDirectory:(id)sender;
- (IBAction)moveDocumentToNewWindow:(id)sender; // TODO Move to AppController
- (IBAction)mergeAllWindows:(id)sender;         // TODO Move to AppController

- (IBAction)goToRelatedFile:(id)sender;
- (IBAction)selectNextTab:(id)sender;
- (IBAction)selectPreviousTab:(id)sender;
- (IBAction)takeSelectedTabIndexFrom:(id)sender;
- (IBAction)toggleSticky:(id)sender;

- (NSPoint)positionForWindowUnderCaret;
- (void)performBundleItem:(bundles::item_ptr)anItem;
- (IBAction)toggleHTMLOutput:(id)sender;

- (IBAction)moveFocus:(id)sender;

- (IBAction)performCloseTab:(id)sender;
- (IBAction)performCloseSplit:(id)sender;
- (IBAction)performCloseWindow:(id)sender;
- (IBAction)performCloseAllTabs:(id)sender;
- (IBAction)performCloseOtherTabsXYZ:(id)sender;
- (IBAction)performCloseTabsToTheRight:(id)sender;

- (IBAction)saveDocument:(id)sender;
- (IBAction)saveDocumentAs:(id)sender;
- (IBAction)saveAllDocuments:(id)sender;
// - (IBAction)revertDocumentToSaved:(id)sender;

// =============================
// = Opening Auxiliary Windows =
// =============================

- (IBAction)orderFrontFindPanel:(id)sender;
- (IBAction)orderFrontRunCommandWindow:(id)sender;
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

// Used by AppController
+ (instancetype)controllerForDocument:(OakDocument*)aDocument;
@end
