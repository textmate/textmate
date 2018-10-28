#import "FileBrowserNotifications.h"

@class FileBrowserViewController;

@protocol FileBrowserDelegate
- (void)fileBrowser:(FileBrowserViewController*)fileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(FileBrowserViewController*)fileBrowser closeURL:(NSURL*)anURL;
@end

@interface FileBrowserViewController : NSViewController
@property (nonatomic, weak) id <FileBrowserDelegate> delegate;

@property (nonatomic, readonly) NSString*        path;
@property (nonatomic, readonly) NSURL*           directoryURLForNewItems;
@property (nonatomic, readonly) NSArray<NSURL*>* selectedFileURLs;

@property (nonatomic, readonly) NSView*          headerView;
@property (nonatomic, readonly) NSOutlineView*   outlineView;
@property (nonatomic, readonly) NSDictionary*    sessionState;

@property (nonatomic) NSArray<NSURL*>* openURLs;
@property (nonatomic) NSArray<NSURL*>* modifiedURLs;

- (void)setupViewWithState:(NSDictionary*)fileBrowserState;
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
@end
