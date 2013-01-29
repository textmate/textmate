#import <oak/debug.h>

@class OakFileBrowser;

@protocol OakFileBrowserDelegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;
@end

PUBLIC @interface OakFileBrowser : NSResponder
@property (nonatomic, weak) id <OakFileBrowserDelegate> delegate;

@property (nonatomic)           NSURL*        url;
@property (nonatomic, readonly) NSString*     path;

@property (nonatomic)           NSArray*      openURLs;
@property (nonatomic)           NSArray*      modifiedURLs;
@property (nonatomic, readonly) NSArray*      selectedURLs;

@property (nonatomic, readonly) NSView*       view;
@property (nonatomic)           NSDictionary* sessionState;

- (void)setupViewWithState:(NSDictionary*)fileBrowserState;
- (void)updateVariables:(std::map<std::string, std::string>&)env;

- (void)goToURL:(NSURL*)aURL;
- (void)selectURL:(NSURL*)aURL withParentURL:(NSURL*)parentURL;

- (NSRect)iconFrameForURL:(NSURL*)aURL;

- (IBAction)reload:(id)sender;
- (IBAction)deselectAll:(id)sender;
- (IBAction)toggleShowInvisibles:(id)sender;

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
@property (nonatomic, readonly) NSView* outlineView;
@end
