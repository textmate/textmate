#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <scope/scope.h>
#import <oak/misc.h>

namespace search
{
	enum type
	{
		actions,
		grammars,
		themes,
	};
}

PUBLIC @interface BundleItemChooser : NSObject <FilterListDataSource>
+ (id)bundleItemChooserForScope:(scope::context_t const&)aScope;
@property (nonatomic) NSString*    filterString;
@property (nonatomic) BOOL         keyEquivalentSearch;
@property (nonatomic) BOOL         textViewHasSelection;
@property (nonatomic) BOOL         searchAllScopes;
@property (nonatomic) search::type searchType;
@end
