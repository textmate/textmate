#import <oak/debug.h>

@class OakFileBrowser;
@class OakFileBrowserView;
@class OakHistoryController;
@class FSOutlineViewDelegate;

@protocol OakFileBrowserDelegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;
@end

@protocol OFBOutlineViewMenuDelegate // private/internal -- from ui/OFBOutlineView.h
- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView;
@end

@interface OakFileBrowser : NSResponder <OFBOutlineViewMenuDelegate>
{
	OBJC_WATCH_LEAKS(OakFileBrowser);

	NSURL* url; // Currently viewed root url
	NSUInteger dataSourceOptions;
	OakHistoryController* historyController;

	id <OakFileBrowserDelegate> delegate;
	OakFileBrowserView* view;
	FSOutlineViewDelegate* outlineViewDelegate;
}
@property (nonatomic, assign) id <OakFileBrowserDelegate> delegate;
@property (nonatomic, retain, readonly) NSView* view;

@property (nonatomic, readonly) NSString*     location;
@property (nonatomic, readonly) NSArray*      selectedURLs;
@property (nonatomic, retain)   NSArray*      openURLs;
@property (nonatomic, retain)   NSArray*      modifiedURLs;
@property (nonatomic, readonly) NSDictionary* sessionState;

- (void)setupViewWithSize:(NSSize)viewSize resizeIndicatorOnRight:(BOOL)flag state:(NSDictionary*)fileBrowserState;
- (void)showURL:(NSURL*)aPath;
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
