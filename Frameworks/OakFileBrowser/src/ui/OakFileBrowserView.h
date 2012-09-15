#import <oak/debug.h>

@class OakStatusBar;
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
{
	OBJC_WATCH_LEAKS(OakFileBrowserView);

	// These two properties are retained only as subviews
	OFBOutlineView* outlineView;
	OakStatusBar* headerView;

	id delegate;
	NSResponder* persistentNextResponder;

	// Header view
	BOOL canGoBackward;
	BOOL canGoForward;

	NSString* titleText;
	NSImage*  titleImage;
}
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
