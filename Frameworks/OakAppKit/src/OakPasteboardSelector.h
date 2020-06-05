@interface OakPasteboardSelector : NSWindowController
{
	IBOutlet NSTableView* tableView;
}
@property (class, readonly) OakPasteboardSelector* sharedInstance;

- (void)setIndex:(NSUInteger)index;
- (void)setEntries:(NSArray*)entries;

- (NSInteger)showAtLocation:(NSPoint)aLocation;
- (void)setWidth:(CGFloat)width;
- (void)setPerformsActionOnSingleClick;
- (NSArray*)entries;
@end
