#import <oak/misc.h>

NS_ASSUME_NONNULL_BEGIN

PUBLIC @interface OakFinderTag : NSObject
@property (nonatomic) NSString* displayName;
@property (nonatomic) NSUInteger label;
@property (nonatomic, getter=isMarkedFavorite) BOOL markedFavorite;
@property (nonatomic, readonly) NSColor* backgroundColor;
@property (nonatomic, readonly) NSColor* foregroundColor;
- (instancetype)initWithDisplayName:(NSString*)name label:(NSUInteger)label markedFavorite:(BOOL)markedFavorite;
+ (instancetype)tagWithDisplayName:(NSString*)name label:(NSUInteger)label markedFavorite:(BOOL)markedFavorite;
- (BOOL)hasLabelColor;
@end

PUBLIC @interface OakFinderTagManager : NSObject
+ (NSArray<OakFinderTag*>*)finderTagsForURL:(NSURL*)aURL;
@property (nonatomic, class, readonly) NSArray<OakFinderTag*>* favoriteFinderTags;
@end

NS_ASSUME_NONNULL_END
