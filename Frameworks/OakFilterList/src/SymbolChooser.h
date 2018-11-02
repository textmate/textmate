#import "OakChooser.h"
#import <document/OakDocument.h>

PUBLIC @interface SymbolChooser : OakChooser
@property (nonatomic) OakDocument* TMDocument;
@property (nonatomic) NSString* selectionString;
+ (instancetype)sharedInstance;
@end
