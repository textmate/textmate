#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <plist/uuid.h>
#import <scope/scope.h>
#import <bundles/bundles.h>
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
{
	OBJC_WATCH_LEAKS(BundleItemChooser);
	scope::context_t scope;
	BOOL hasSelection;
	std::vector<bundles::item_ptr> all_items;
	std::set<oak::uuid_t> items_filtered_by_scope;
	BOOL searchAllScopes;
	std::string originalFilterString;
	std::string filterString;
	NSViewController* viewController;
	BOOL keyEquivalentSearch;
	search::type searchType;
}
+ (id)bundleItemChooserForScope:(scope::context_t const&)aScope;
@property (nonatomic, retain) NSString*    filterString;
@property (nonatomic, assign) BOOL         keyEquivalentSearch;
@property (nonatomic, assign) BOOL         textViewHasSelection;
@property (nonatomic, assign) BOOL         searchAllScopes;
@property (nonatomic, assign) search::type searchType;
@end
