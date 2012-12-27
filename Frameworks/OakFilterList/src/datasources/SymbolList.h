#import <oak/misc.h>
#import <document/document.h>

@interface FileChooserSymbolItem : NSObject
@property (nonatomic, retain) NSString* path;
@property (nonatomic, retain) NSString* selectionString;
@property (nonatomic, retain) NSString* identifier;
@property (nonatomic, retain) NSAttributedString* displayString;
@property (nonatomic, retain) NSAttributedString* infoString;

- (id)initWithPath:(NSString*)aPath selectionString:(NSString*)aSelectionString identifier:(NSString*)anIdentifier displayString:(NSAttributedString*)aDisplayString infoString:(NSAttributedString*)anInfoString;
@end

PUBLIC NSArray* SymbolListForDocument (document::document_ptr const& document, std::string const& filter);
