#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <document/document.h>
#import <oak/misc.h>

@class SymbolChooserViewController;

PUBLIC @interface SymbolChooser : NSObject <FilterListDataSource>
{
	OBJC_WATCH_LEAKS(SymbolChooser);
	document::document_ptr document;
	std::string filterString;
	SymbolChooserViewController* viewController;
}
+ (id)symbolChooserForDocument:(document::document_ptr)aDocument;
@property (nonatomic, readonly) NSString* filterString;
@end
