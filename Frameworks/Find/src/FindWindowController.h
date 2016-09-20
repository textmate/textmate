#import "Find.h"

@class FFResultsViewController;

@interface FindWindowController : NSWindowController
@property (nonatomic) FFResultsViewController* resultsViewController;

@property (nonatomic, readonly) NSButton* findAllButton;
@property (nonatomic, readonly) NSButton* replaceAllButton;
@property (nonatomic, readonly) NSButton* replaceAndFindButton;
@property (nonatomic, readonly) NSButton* findPreviousButton;
@property (nonatomic, readonly) NSButton* findNextButton;

@property (nonatomic) BOOL showsResultsOutlineView;

@property (nonatomic) FFSearchTarget searchTarget;
@property (nonatomic) NSString* projectFolder;
@property (nonatomic) NSArray*  fileBrowserItems;
@property (nonatomic) NSString* otherFolder;

@property (nonatomic, readonly) NSString* searchFolder;

@property (nonatomic) NSString* findString;
@property (nonatomic) NSString* replaceString;
@property (nonatomic) NSString* globString;

@property (nonatomic) BOOL ignoreCase;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;
@property (nonatomic) BOOL wrapAround;
@property (nonatomic) BOOL fullWords; // not implemented

@property (nonatomic) BOOL searchHiddenFolders;
@property (nonatomic) BOOL searchFolderLinks;
@property (nonatomic) BOOL searchFileLinks;
@property (nonatomic) BOOL searchBinaryFiles;

@property (nonatomic, getter = isBusy) BOOL busy;
@property (nonatomic) NSString* statusString;
@property (nonatomic) NSString* alternateStatusString;

@property (nonatomic) NSString* findErrorString;
- (void)updateFindErrorString;

- (IBAction)selectNextResult:(id)sender;
- (IBAction)selectPreviousResult:(id)sender;
@end
