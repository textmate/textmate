NS_ASSUME_NONNULL_BEGIN

@interface OakFinderTag : NSObject
@property (nonatomic) NSString* displayName;
@property (nonatomic, readonly) NSColor* labelColor;
- (BOOL)hasLabelColor;
@end

@interface OakFinderTagManager : NSObject
+ (NSArray<OakFinderTag*>*)finderTagsForURL:(NSURL*)aURL;
+ (NSArray<OakFinderTag*>*)favoriteFinderTags;
@end

NS_ASSUME_NONNULL_END
