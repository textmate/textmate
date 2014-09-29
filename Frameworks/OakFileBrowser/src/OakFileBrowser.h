#import <oak/debug.h>

@class OakFileBrowser;
@class OakBackgroundFillView;

@protocol OakFileBrowserDelegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;
@end

PUBLIC @interface OakFileBrowser : NSResponder
@property (nonatomic, weak) id <OakFileBrowserDelegate> delegate;

@property (nonatomic)           NSURL*        url;
@property (nonatomic, readonly) NSString*     path;
@property (nonatomic, readonly) NSString*     directoryForNewItems;

@property (nonatomic)           NSArray*      openURLs;
@property (nonatomic)           NSArray*      modifiedURLs;
@property (nonatomic, readonly) NSArray*      selectedURLs;

@property (nonatomic, readonly) OakBackgroundFillView* headerView;
@property (nonatomic, readonly) NSView*       view;
@property (nonatomic)           NSDictionary* sessionState;

- (void)setupViewWithState:(NSDictionary*)fileBrowserState;
- (std::map<std::string, std::string>)variables;

- (void)goToURL:(NSURL*)aURL;
- (void)selectURL:(NSURL*)aURL withParentURL:(NSURL*)parentURL;
- (void)editURL:(NSURL*)aURL;

- (NSRect)iconFrameForURL:(NSURL*)aURL;

- (IBAction)reload:(id)sender;
- (IBAction)deselectAll:(id)sender;
- (IBAction)toggleShowInvisibles:(id)sender;

- (BOOL)canGoBack;
- (BOOL)canGoForward;

- (IBAction)goBack:(id)sender;
- (IBAction)goForward:(id)sender;
- (IBAction)goToParentFolder:(id)sender;

- (IBAction)goToComputer:(id)sender;
- (IBAction)goToHome:(id)sender;
- (IBAction)goToDesktop:(id)sender;
- (IBAction)goToFavorites:(id)sender;
- (IBAction)goToSCMDataSource:(id)sender;
- (IBAction)orderFrontGoToFolder:(id)sender;
@end

@interface OakFileBrowser (ForQuickLookKeyEventForwardingAndMoveFocus)
@property (nonatomic, readonly) NSOutlineView* outlineView;
@end
