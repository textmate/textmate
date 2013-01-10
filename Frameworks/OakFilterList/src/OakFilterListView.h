#import <oak/debug.h>

@interface OakFilterListView : NSTableView <NSTableViewDataSource, NSTableViewDelegate>
@property (nonatomic, retain) id <FilterListDataSource> filterDataSource;
@property (nonatomic, retain, readonly) NSAttributedString* infoString;
@property (nonatomic, readonly) NSArray* selectedItems;
@property (nonatomic, assign) NSUInteger sourceIndex;
@property (nonatomic, retain) NSButtonCell* accessoryButton;

- (void)waitForAllItems;
- (void)makeSelectedItemsBestMatch;
@end
