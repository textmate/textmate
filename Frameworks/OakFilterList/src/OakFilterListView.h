#import <oak/debug.h>
#import <text/ranker.h>

@interface OakFilterListView : NSTableView <NSTableViewDataSource, NSTableViewDelegate>
{
	OBJC_WATCH_LEAKS(OakFilterListView)
	NSArray* items;
	NSAttributedString* infoString;
	id <FilterListDataSource> filterDataSource;
	BOOL isWaitingForItems;
	NSUInteger sourceIndex;
}
@property (nonatomic, retain) id <FilterListDataSource> filterDataSource;
@property (nonatomic, retain, readonly) NSAttributedString* infoString;
@property (nonatomic, readonly) NSArray* selectedItems;
@property (nonatomic, assign) NSUInteger sourceIndex;
@property (nonatomic, retain) NSButtonCell* accessoryButton;

- (void)waitForAllItems;
- (void)makeSelectedItemsBestMatch;
@end
