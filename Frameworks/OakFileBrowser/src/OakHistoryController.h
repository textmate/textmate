@interface OakHistoryController : NSObject
{
	NSMutableArray* historyArray;
	NSMutableArray* recentLocationsArray;
	NSInteger historyIndex;
}
@property (nonatomic, retain) NSDictionary* state;

@property (nonatomic, readonly) NSURL* previousURL;
@property (nonatomic, readonly) NSURL* nextURL;
@property (nonatomic, readonly) NSURL* currentURL;

@property (nonatomic, assign) CGFloat currentURLScrollOffset;

- (void)addURLToHistory:(NSURL*)path;
- (BOOL)advance:(id)sender;
- (BOOL)retreat:(id)sender;

@property (nonatomic, readonly) NSArray* recentLocations;

@property (nonatomic, assign)   NSInteger historyIndex;
@property (nonatomic, readonly) NSInteger historyCount;
- (NSURL*)urlAtIndex:(NSInteger)index;
@end
