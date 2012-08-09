#import <OakFilterList/OakFilterList.h>
#import <text/ctype.h>

@class FavoritesViewController;

@interface FavoritesDataSource : NSObject <FilterListDataSource>
{
	OBJC_WATCH_LEAKS(FavoritesDataSource);
	std::string favoritesPath;
	std::multimap<std::string, std::string, text::less_t> favorites;
	std::string filterString;
	FavoritesViewController* viewController;
}
+ (FavoritesDataSource*)favoritesDataSource;
@property (nonatomic, readonly) NSString* filterString;
@end
