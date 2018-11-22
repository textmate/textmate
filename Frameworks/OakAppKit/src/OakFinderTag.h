#import <oak/misc.h>

NS_ASSUME_NONNULL_BEGIN

PUBLIC @interface OakFinderTag : NSObject
@property (nonatomic) NSString* displayName;
@property (nonatomic, readonly) NSColor* labelColor;
- (BOOL)hasLabelColor;
@end

PUBLIC @interface OakFinderTagManager : NSObject
+ (NSArray<OakFinderTag*>*)finderTagsForURL:(NSURL*)aURL;
+ (NSArray<OakFinderTag*>*)favoriteFinderTags;
@end

NS_ASSUME_NONNULL_END
