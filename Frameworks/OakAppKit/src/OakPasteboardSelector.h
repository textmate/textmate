#import <oak/misc.h>

@class OakPasteboardSelectorTableViewHelper;

PUBLIC @interface OakPasteboardSelector : NSWindowController
{
@private
	IBOutlet NSTableView* tableView;
	OakPasteboardSelectorTableViewHelper* tableViewHelper;
}
+ (instancetype)sharedInstance;
- (void)setIndex:(NSUInteger)index;
- (void)setEntries:(NSArray*)entries;

- (NSInteger)showAtLocation:(NSPoint)aLocation;
- (void)setWidth:(CGFloat)width;
- (void)setPerformsActionOnSingleClick;
- (NSArray*)entries;
@end
