#import "OakTextView.h"
#import <oak/debug.h>

@class OakDocument;

PUBLIC @interface OakDocumentView : NSView
@property (nonatomic, readonly) OakTextView* textView;
@property (nonatomic) OakDocument* document;
@property (nonatomic) BOOL hideStatusBar;
- (IBAction)toggleLineNumbers:(id)sender;
- (IBAction)takeThemeUUIDFrom:(id)sender;

- (void)setThemeWithUUID:(NSString*)themeUUID;

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge;
- (void)removeAuxiliaryView:(NSView*)aView;

- (IBAction)showSymbolChooser:(id)sender;
@end
