#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <document/document.h>
#import <oak/misc.h>

PUBLIC @interface SymbolChooser : NSObject <FilterListDataSource>
@property (nonatomic) document::document_ptr const& document;
@property (nonatomic) NSString* selectionString;
+ (id)symbolChooserForDocument:(document::document_ptr)aDocument;
@end
