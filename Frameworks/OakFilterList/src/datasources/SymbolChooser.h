#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <OakTextView/OakDocumentView.h>
#import <document/document.h>
#import <oak/misc.h>

@class SymbolChooserViewController;

PUBLIC @interface SymbolChooser : NSObject <FilterListDataSource>
{
	OBJC_WATCH_LEAKS(SymbolChooser);
	document::document_ptr document;
	OakDocumentView* documentView;
	std::string filterString;
	SymbolChooserViewController* viewController;
}
+ (id)symbolChooserForDocumentView:(OakDocumentView *)aDocumentView;
@property (nonatomic, readonly) NSString* filterString;
@end
