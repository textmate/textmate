#import "OakChooser.h"
#import <document/OakDocument.h>

PUBLIC @interface SymbolChooser : OakChooser
@property (nonatomic) OakDocument* document;
@property (nonatomic) NSString* selectionString;
+ (instancetype)sharedInstance;
@end
