#import <oak/misc.h>

PUBLIC @interface OakPasteboardSelector : NSWindowController
{
	IBOutlet NSTableView* tableView;
}
+ (instancetype)sharedInstance;
- (void)setIndex:(NSUInteger)index;
- (void)setEntries:(NSArray*)entries;

- (NSInteger)showAtLocation:(NSPoint)aLocation;
- (void)setWidth:(CGFloat)width;
- (void)setPerformsActionOnSingleClick;
- (NSArray*)entries;
@end
