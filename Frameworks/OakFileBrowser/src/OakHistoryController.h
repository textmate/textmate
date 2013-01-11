@interface OakHistoryController : NSObject
@property (nonatomic) NSDictionary* state;

@property (nonatomic, readonly) NSURL* previousURL;
@property (nonatomic, readonly) NSURL* nextURL;
@property (nonatomic, readonly) NSURL* currentURL;

@property (nonatomic) CGFloat currentURLScrollOffset;

- (void)addURLToHistory:(NSURL*)path;
- (BOOL)advance:(id)sender;
- (BOOL)retreat:(id)sender;

@property (nonatomic)           NSInteger historyIndex;
@property (nonatomic, readonly) NSInteger historyCount;
- (NSURL*)urlAtIndex:(NSInteger)index;
@end

@interface OakHistoryController (WorkaroundForBeingBackedByNSMutableArray)
@property (nonatomic, readonly) NSArray* recentLocations;
@end
