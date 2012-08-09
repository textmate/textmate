#import "DocumentOpenHelper.h"
#import <OakFileBrowser/OakFileBrowser.h>
#import <OakAppKit/OakTabBarView.h>
#import <oak/debug.h>
#import <plist/uuid.h>
#import <command/runner.h>
#import <scm/scm.h>

extern NSString* const kUserDefaultsHTMLOutputPlacementKey;
extern NSString* const kUserDefaultsFileBrowserPlacementKey;

@class OakLayoutView;
@class OakDocumentView;
@class OakTextView;
@class OakFilterWindowController;
@class OakHTMLOutputView;

struct document_tab_t;
typedef std::tr1::shared_ptr<document_tab_t> document_tab_ptr;

namespace bundles { struct item_t; typedef std::tr1::shared_ptr<item_t> item_ptr; }

@interface DocumentController : NSWindowController <OakFileBrowserDelegate, OakTabBarViewDelegate, OakTabBarViewDataSource, DocumentOpenHelperDelegate>
{
	OBJC_WATCH_LEAKS(DocumentController);

	IBOutlet OakTabBarView* tabBarView;
	IBOutlet OakLayoutView* layoutView;

	OakFileBrowser* fileBrowser;
	OakDocumentView* documentView;
	OakTextView* textView;

	OakHTMLOutputView* htmlOutputView;
	command::runner_ptr runner;

	BOOL windowHasLoaded;

	BOOL fileBrowserHidden;
	NSDictionary* fileBrowserState;

	int32_t fileBrowserWidth;
	int32_t htmlOutputHeight;

	OakFilterWindowController* filterWindowController;
	NSUInteger fileChooserSourceIndex;

	// =====================
	// = Document Bindings =
	// =====================

	NSString* windowTitle;
	NSString* representedFile;
	BOOL isDocumentEdited;

	scm::info_ptr scmInfo;
	scm::callback_t* scmCallback;

	// =================
	// = Document Tabs =
	// =================

	oak::uuid_t identifier;
@public // FIXME
	std::vector<document_tab_ptr> documentTabs;
@protected
	size_t selectedTabIndex;

	oak::uuid_t scratchDocument;
}
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, assign) BOOL fileBrowserHidden;
@property (nonatomic, readonly) NSString* documentPath;
@property (nonatomic, readonly) NSString* documentFilePath;
@property (nonatomic, readonly) NSString* fileBrowserPath;
@property (nonatomic, readonly) NSString* projectPath;
@property (nonatomic, readonly) NSString* untitledSavePath;

+ (DocumentController*)controllerForDocument:(document::document_ptr const&)aDocument;
+ (DocumentController*)controllerForPath:(std::string const&)aPath;
+ (DocumentController*)controllerForUUID:(oak::uuid_t const&)aUUID;

- (id)init;

- (IBAction)goToFileCounterpart:(id)sender;
- (IBAction)selectNextTab:(id)sender;
- (IBAction)selectPreviousTab:(id)sender;
- (IBAction)takeSelectedTabIndexFrom:(id)sender;

- (IBAction)revealFileInProject:(id)sender;
- (IBAction)toggleFileBrowser:(id)sender;

- (void)performBundleItem:(bundles::item_ptr const&)anItem;
- (NSPoint)positionForWindowUnderCaret;

- (void)performCloseWindow:(id)sender;
- (void)performCloseOtherTabs:(id)sender;

- (void)makeTextViewFirstResponder:(id)sender;

- (void)closeTabAtIndex:(NSUInteger)tabIndex;
- (void)closeDocumentWithPath:(NSString*)aPath;

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner;

- (IBAction)saveDocument:(id)sender;
- (IBAction)saveDocumentAs:(id)sender;
- (IBAction)saveAllDocuments:(id)sender;
@end

@interface DocumentController (ApplicationTermination)
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;
@end
