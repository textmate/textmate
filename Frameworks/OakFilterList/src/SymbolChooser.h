#import "OakChooser.h"
#import <document/document.h>

PUBLIC @interface SymbolChooser : OakChooser
@property (nonatomic) document::document_ptr document;
@property (nonatomic) NSString* selectionString;
+ (instancetype)sharedInstance;
@end
