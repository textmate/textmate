#import <oak/debug.h>

@class OakFileBrowser;

@protocol OakFileBrowserDelegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;
@end

PUBLIC @interface OakFileBrowser : NSResponder
@property (nonatomic, weak) id <OakFileBrowserDelegate> delegate;
@property (nonatomic, readonly) NSView*       view;
@property (nonatomic, readonly) NSString*     path;
@property (nonatomic, readonly) NSArray*      selectedURLs;
@property (nonatomic)           NSArray*      openURLs;
@property (nonatomic)           NSArray*      modifiedURLs;
@property (nonatomic, readonly) NSDictionary* sessionState;

- (void)setupViewWithState:(NSDictionary*)fileBrowserState;
- (void)showURL:(NSURL*)aPath;
- (void)revealURL:(NSURL*)aURL;
- (void)deselectAll:(id)sender;
- (void)updateVariables:(std::map<std::string, std::string>&)env;

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
