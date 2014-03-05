#import <oak/debug.h>

@interface OakFilterListView : NSTableView <NSTableViewDataSource, NSTableViewDelegate>
@property (nonatomic) id <FilterListDataSource> filterDataSource;
@property (nonatomic, readonly) NSAttributedString* infoString;
@property (nonatomic, readonly) NSArray* selectedItems;
@property (nonatomic) NSUInteger sourceIndex;
@property (nonatomic) NSButtonCell* accessoryButton;

- (void)waitForAllItems;
- (void)makeSelectedItemsBestMatch;
@end
