@class OakDocument;

@interface OakDocumentPrintOptionsViewController : NSViewController <NSPrintPanelAccessorizing>
@end

@interface OakDocumentPrintableView : NSView
- (id)initWithDocument:(OakDocument*)aDocument fontName:(NSString*)aFontName;
@end
