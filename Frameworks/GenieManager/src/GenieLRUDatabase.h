@class GenieItem;

@interface GenieLRUDatabase : NSObject
+ (instancetype)sharedInstance;

- (NSDate*)lruForItem:(GenieItem*)item withFilter:(NSString*)filter;
- (NSDate*)lruForItem:(GenieItem*)item withFilterPrefix:(NSString*)filterPrefix;
- (NSString*)lastQueryStringForItem:(GenieItem*)item;

- (void)addEvent:(NSDictionary*)event;
- (void)synchronize:(id)sender;
@end
