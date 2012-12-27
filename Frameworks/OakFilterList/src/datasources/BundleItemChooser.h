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
@property (nonatomic, retain) NSString*    filterString;
@property (nonatomic, assign) BOOL         keyEquivalentSearch;
@property (nonatomic, assign) BOOL         textViewHasSelection;
@property (nonatomic, assign) BOOL         searchAllScopes;
@property (nonatomic, assign) search::type searchType;
@end
