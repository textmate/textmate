#import "FileBrowserNotifications.h"

@class FileBrowserViewController;
@class FileItem;

@protocol FileBrowserDelegate
- (void)fileBrowser:(FileBrowserViewController*)fileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(FileBrowserViewController*)fileBrowser closeURL:(NSURL*)anURL;
@end

@interface FileBrowserViewController : NSViewController
@property (nonatomic, weak) id <FileBrowserDelegate> delegate;

@property (nonatomic, readonly) NSURL*           URL;
@property (nonatomic, readonly) NSString*        path; // Returns self.URL.filePathURL.path
@property (nonatomic, readonly) NSURL*           directoryURLForNewItems;
@property (nonatomic, readonly) NSArray<NSURL*>* selectedFileURLs;

@property (nonatomic, readonly) NSView*          headerView;
@property (nonatomic, readonly) NSOutlineView*   outlineView;
@property (nonatomic, readonly) id               sessionState;

- (void)setupViewWithState:(id)state;
- (std::map<std::string, std::string>)variables;

- (void)goToURL:(NSURL*)url;
- (void)selectURL:(NSURL*)url withParentURL:(NSURL*)parentURL;
- (NSURL*)newFile:(id)sender;
- (NSURL*)newFolder:(id)sender;

- (void)reload:(id)sender;
- (void)deselectAll:(id)sender;
- (void)toggleShowInvisibles:(id)sender;

- (BOOL)canGoBack;
- (BOOL)canGoForward;

- (void)goBack:(id)sender;
- (void)goForward:(id)sender;
- (void)goToParentFolder:(id)sender;

- (void)goToComputer:(id)sender;
- (void)goToHome:(id)sender;
- (void)goToDesktop:(id)sender;
- (void)goToFavorites:(id)sender;
- (void)goToSCMDataSource:(id)sender;
- (void)orderFrontGoToFolder:(id)sender;

// ======================================================
// = Private (FileBrowserViewController DiskOperations) =
// ======================================================
@property (nonatomic) FileItem* fileItem;
- (NSComparator)itemComparator;
- (NSArray<FileItem*>*)arrangeChildren:(NSArray<FileItem*>*)children inParent:(FileItem*)parentOrNil;
- (void)rearrangeChildrenInParent:(FileItem*)item;
@end

typedef NS_OPTIONS(NSUInteger, FBOperation) {
	FBOperationLink      = 0x0001,
	FBOperationCopy      = 0x0002,
	FBOperationDuplicate = 0x0004,
	FBOperationMove      = 0x0008,
	FBOperationRename    = 0x0010,
	FBOperationTrash     = 0x0020,
	FBOperationNewFile   = 0x0040,
	FBOperationNewFolder = 0x0080,
};

@interface FileBrowserViewController (DiskOperations)
- (NSArray<NSURL*>*)performOperation:(FBOperation)op withURLs:(NSDictionary<NSURL*, NSURL*>*)urls unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs;
- (NSArray<NSURL*>*)performOperation:(FBOperation)op sourceURLs:(NSArray<NSURL*>*)srcURLs destinationURLs:(NSArray<NSURL*>*)destURLs unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs;
@end
