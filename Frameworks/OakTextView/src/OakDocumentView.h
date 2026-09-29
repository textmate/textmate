#import "OakTextView.h"
#import <oak/debug.h>

@class OakDocument;
@class GutterView;

@interface OakDocumentView : NSView
@property (nonatomic, readonly) OakTextView* textView;
@property (nonatomic, readonly) GutterView* gutterView;
@property (nonatomic) OakDocument* document;
@property (nonatomic) BOOL hideStatusBar;
@property (nonatomic) BOOL showsScopeBar;
@property (nonatomic, weak) id navigationTarget;
@property (nonatomic) BOOL canNavigateBack;
@property (nonatomic) BOOL canNavigateForward;
- (IBAction)toggleLineNumbers:(id)sender;
- (IBAction)takeTabSizeFrom:(id)sender;
- (IBAction)showTabSizeSelectorPanel:(id)sender;

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge;
- (void)removeAuxiliaryView:(NSView*)aView;

- (IBAction)showSymbolChooser:(id)sender;
@end
