#import <OakFilterList/OakFilterList.h>
#import <text/ctype.h>

@interface FavoritesDataSource : NSObject <FilterListDataSource>
+ (FavoritesDataSource*)favoritesDataSource;
@property (nonatomic, readonly) NSString* filterString;
@end
