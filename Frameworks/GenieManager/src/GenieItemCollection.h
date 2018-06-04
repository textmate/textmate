@class GenieItem;
@class GenieFilter;

@interface GenieItemCollection : NSObject
@property (nonatomic) NSString* queryString;
@property (nonatomic, readonly) GenieFilter* filter;
@property (nonatomic) BOOL live;

@property (nonatomic, copy) NSIndexSet* selectionIndexes;
@property (nonatomic, readonly) NSArray<GenieItem*>* arrangedObjects;
@property (nonatomic, readonly) NSArray<GenieItem*>* selectedObjects;
@property (nonatomic, getter = isBusy) BOOL busy;

+ (instancetype)defaultCollection;
- (instancetype)initWithItems:(NSArray<GenieItem*>*)items;
@end
