#import <OakFoundation/OakHistoryList.h>
#import "FFDocumentSearch.h"

@class OakStatusBar;

extern NSString* const FFSearchInDocument;
extern NSString* const FFSearchInSelection;
extern NSString* const FFSearchInProjectFolder;
extern NSString* const FFSearchInOtherFolder; // pops up a sheet requesting the folder
extern NSString* const FFSearchInOpenFiles;

@interface FFWindowController : NSWindowController
{
	OBJC_WATCH_LEAKS(FFWindowController);

	// The find/replace fields are bound through this controller so we can commitEditing
	IBOutlet NSObjectController* findController;

	IBOutlet NSPopUpButton* findInPopUp;

	IBOutlet NSTextField* findStringField;
	IBOutlet NSTextField* replaceStringField;

	IBOutlet NSButton* findNextButton;
	IBOutlet NSButton* findAllButton;
	IBOutlet NSButton* replaceAllButton;

	IBOutlet NSOutlineView* findAllResultsOutlineView;
	IBOutlet OakStatusBar* findAllResultsHeaderView;
	NSString* resultsHeaderText;
	BOOL expandCollapseAllIsExpanding;
	BOOL resultsHeaderEnabled;
	BOOL enableReplacementSelectionCheckboxes;

	IBOutlet NSTextField* statusTextField;

	NSString* searchIn;
	NSString* searchFolder;
	NSString* projectFolder;

	NSString* findString;
	NSString* replaceString;

	BOOL isBusy;
	NSString* statusMessage;

	OakHistoryList* recentFolders;

	OakHistoryList* recentGlobs;
	IBOutlet NSComboBox* globField;
	IBOutlet NSTextField* globFieldLabel;
	IBOutlet NSButton* wrapAroundField;

	id delegate;

	// ==============================
	// = For the benefit of binding =
	// ==============================
	
	BOOL canSetFileTypes;
	BOOL canSetWrapAround;

	BOOL findRegularExpression;
	BOOL findFullWords;
	BOOL findIgnoreWhitespace;

	BOOL followLinks;
	BOOL searchHiddenFolders;

	FFDocumentSearch* searcher;
	BOOL previewReplacements;
}

@property (nonatomic, retain) FFDocumentSearch* searcher;

@property (nonatomic, assign) id delegate;

@property (nonatomic, retain) NSString* findString;
@property (nonatomic, retain) NSString* replaceString;
@property (nonatomic, retain) OakHistoryList* recentGlobs;

@property (nonatomic, assign) BOOL findRegularExpression;
@property (nonatomic, assign) BOOL findIgnoreWhitespace;
@property (nonatomic, assign) BOOL findFullWords;

@property (nonatomic, assign) BOOL followLinks;
@property (nonatomic, assign) BOOL searchHiddenFolders;

@property (nonatomic, readonly) NSString* globString;
@property (nonatomic, retain, readonly) NSString* searchFolder;
@property (nonatomic, assign, readonly) BOOL isSearchingFolders;
@property (nonatomic, retain) NSString* searchIn;
@property (nonatomic, retain) NSString* projectFolder;

@property (nonatomic, assign) BOOL isShowingFindAllResults;

@property (nonatomic, copy) NSString* statusMessage;

- (IBAction)showFolderSelectionPanel:(id)sender;

- (IBAction)searchInPopUpChanged:(id)sender;

- (IBAction)performFindAction:(id)sender;
- (IBAction)saveAll:(id)sender;

- (void)invalidate;
@end

enum FindActionTag
{
	FindActionFindNext = 1,
	FindActionFindPrevious,
	FindActionCountMatches,
	FindActionFindAll,
	FindActionReplaceAll,
	FindActionReplaceSelected,
};

@protocol FFWindowControllerDelegate
- (void)performFindAction:(FindActionTag)action withWindowController:(FFWindowController*)controller;
@end
