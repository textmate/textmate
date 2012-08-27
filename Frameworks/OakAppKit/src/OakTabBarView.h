#import "OakControl.h"
#import <OakFoundation/OakTimer.h>
#import <oak/debug.h>

PUBLIC extern NSString* const kUserDefaultsDisableTabBarCollapsingKey;

struct binding_info_t;
struct value_t;

struct layout_metrics_t;
typedef std::shared_ptr<layout_metrics_t> layout_metrics_ptr;

@protocol OakTabBarViewDelegate, OakTabBarViewDataSource;

@interface OakTabBarView : OakControl
{
	OBJC_WATCH_LEAKS(OakTabBarView);

	NSMutableArray* tabTitles;         // bindable as ‘value’
	NSMutableArray* tabToolTips;       // bindable as ‘toolTip’
	NSMutableArray* tabModifiedStates; // bindable as ‘isEdited’

	BOOL layoutNeedsUpdate;
	NSUInteger selectedTab;            // bindable as ‘selectionIndexes’ (NSIndexSet)
	NSUInteger hiddenTab;

	BOOL isExpanded;

	layout_metrics_ptr metrics;
	std::vector<NSRect> tabRects;
	std::map<NSUInteger, value_t> tabDropSpacing;
	OakTimer* slideAroundAnimationTimer;

	id <OakTabBarViewDelegate> delegate;
	id <OakTabBarViewDataSource> dataSource;
	std::vector<binding_info_t> bindings;
}
@property (nonatomic, assign) BOOL isExpanded;
@property (nonatomic, assign) id <OakTabBarViewDelegate> delegate;
@property (nonatomic, assign) id <OakTabBarViewDataSource> dataSource;
@property (nonatomic, readonly) NSUInteger countOfVisibleTabs;
- (void)reloadData;
- (void)setSelectedTab:(NSUInteger)anIndex;
@end

@protocol OakTabBarViewDelegate <NSObject>
@optional
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView shouldSelectIndex:(NSUInteger)anIndex;
- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex;
- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView;
- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView;

// Methods sent to the delegate which the tab was dragged from
// When called, the delegate should set up the drag pasteboard using -addTypes:owner: with the data it requires
- (void)setupPasteboard:(NSPasteboard*)aPasteboard forTabAtIndex:(NSUInteger)draggedTabIndex;

// Methods sent to the delegate which the tab was dragged to
- (BOOL)performTabDropFromTabBar:(OakTabBarView*)tabBar atIndex:(NSUInteger)droppedIndex fromPasteboard:(NSPasteboard*)aPasteboard operation:(NSDragOperation)operation;
@end

@protocol OakTabBarViewDataSource <NSObject>
- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView;

- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex;
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView toolTipForIndex:(NSUInteger)anIndex;
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex;
@end
