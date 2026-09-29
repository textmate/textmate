#import "OakChooser.h"
#import <document/src/OakDocument.h>

@interface SymbolChooser : OakChooser
@property (class, readonly) SymbolChooser* sharedInstance;

@property (nonatomic) OakDocument* TMDocument;
@property (nonatomic) NSString* selectionString;@end
