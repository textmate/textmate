#import "OakTextView.h"
#import <oak/debug.h>

@class OakDocument;

@interface OakDocumentView : NSView
@property (nonatomic, readonly) OakTextView* textView;
@property (nonatomic) OakDocument* document;
@property (nonatomic) BOOL hideStatusBar;
- (IBAction)toggleLineNumbers:(id)sender;

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge;
- (void)removeAuxiliaryView:(NSView*)aView;

- (IBAction)showSymbolChooser:(id)sender;
@end
