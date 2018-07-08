#import <oak/misc.h>

NS_ASSUME_NONNULL_BEGIN

PUBLIC @interface OakFinderTag : NSObject
@property (nonatomic) NSString* displayName;
@property (nonatomic, getter=isMarkedFavorite) BOOL markedFavorite;
@property (nonatomic, readonly) NSColor* backgroundColor;
@property (nonatomic, readonly) NSColor* foregroundColor;
- (BOOL)hasLabelColor;
@end

PUBLIC @interface OakFinderTagManager : NSObject
+ (NSArray<OakFinderTag*>*)finderTagsForURL:(NSURL*)aURL;
+ (NSArray<OakFinderTag*>*)favoriteFinderTags;
@end

NS_ASSUME_NONNULL_END
