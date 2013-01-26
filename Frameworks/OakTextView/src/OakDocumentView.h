#import "GutterView.h"
#import "OakTextView.h"
#import <document/document.h>
#import <oak/debug.h>

@class OTVStatusBar;

PUBLIC @interface OakDocumentView : NSView <GutterViewDelegate, GutterViewColumnDataSource, GutterViewColumnDelegate>
{
	OBJC_WATCH_LEAKS(OakDocumentView);

	NSScrollView* gutterScrollView;
	GutterView* gutterView;
	NSColor* gutterDividerColor;
	NSDictionary* gutterImages;
	NSDictionary* gutterHoverImages;
	NSDictionary* gutterPressedImages;

	NSBox* gutterDividerView;
	NSBox* statusDividerView;

	NSScrollView* textScrollView;
	OakTextView* textView;
	OTVStatusBar* statusBar;
	document::document_ptr document;
	document::document_t::callback_t* callback;

	NSMutableArray* topAuxiliaryViews;
	NSMutableArray* bottomAuxiliaryViews;

	IBOutlet NSPanel* tabSizeSelectorPanel;
}
@property (nonatomic, readonly) OakTextView* textView;
@property (nonatomic, assign) document::document_ptr const& document;
- (IBAction)toggleLineNumbers:(id)sender;
- (IBAction)takeThemeUUIDFrom:(id)sender;

- (void)setThemeWithUUID:(NSString*)themeUUID;

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge;
- (void)removeAuxiliaryView:(NSView*)aView;

- (IBAction)showSymbolChooser:(id)sender;
@end
