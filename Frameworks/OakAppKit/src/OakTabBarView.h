#import "OakControl.h"
#import <OakFoundation/OakTimer.h>
#import <oak/debug.h>

PUBLIC extern NSString* const kUserDefaultsDisableTabBarCollapsingKey;

@protocol OakTabBarViewDelegate, OakTabBarViewDataSource;

PUBLIC @interface OakTabBarView : OakControl
@property (nonatomic, weak) id <OakTabBarViewDelegate> delegate;
@property (nonatomic, weak) id <OakTabBarViewDataSource> dataSource;
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
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView pathForIndex:(NSUInteger)anIndex;
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex;
@end
