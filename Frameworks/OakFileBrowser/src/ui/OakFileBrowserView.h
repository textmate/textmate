#import <oak/debug.h>

@class OFBOutlineView;

namespace fb
{
	enum header_column { goBack = 1, goForward, title, options, scmDataSource, favorites, home, computer };
}

@protocol OakFileBrowserUIDelegate
- (void)didClickHeaderColumn:(id)sender;
- (void)didTriggerMenuForHeaderColumn:(id)sender;
- (void)didClickCloseIconForEntry:(id)anEntry;
- (void)renameEntry:(id)anEntry to:(NSString*)name;
@end

@interface OakFileBrowserView : NSView
// Initial setup
@property (nonatomic, assign) id           delegate;
@property (nonatomic, assign) NSResponder* persistentNextResponder;

// Header info
@property (nonatomic, assign) BOOL      canGoBackward;
@property (nonatomic, assign) BOOL      canGoForward;
@property (nonatomic, retain) NSString* titleText;
@property (nonatomic, retain) NSImage*  titleImage;

// Outline view
@property (nonatomic, readonly) OFBOutlineView* outlineView;

- (void)displayMenu:(NSMenu*)aMenu fromHeaderColumn:(fb::header_column)columnTag selectedIndex:(NSUInteger)index popup:(BOOL)popup;
- (NSRect)iconFrameForEntry:(id)anEntry;
@end
