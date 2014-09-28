#import "OakUIConstructionFunctions.h"
#import <oak/debug.h>

PUBLIC extern NSString* const kUserDefaultsDisableTabBarCollapsingKey;

PUBLIC @interface OakTabItem : NSObject
+ (instancetype)tabItemWithTitle:(NSString*)aTitle path:(NSString*)aPath identifier:(NSString*)anIdentifier modified:(BOOL)flag;
@property (nonatomic) NSString* title;
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* identifier;
@property (nonatomic) BOOL modified;
@property (nonatomic) BOOL sticky;
@end

@protocol OakTabBarViewDelegate, OakTabBarViewDataSource;

PUBLIC @interface OakTabBarView : OakBackgroundFillView
@property (nonatomic, weak) id <OakTabBarViewDelegate> delegate;
@property (nonatomic, weak) id <OakTabBarViewDataSource> dataSource;
@property (nonatomic, readonly) NSUInteger countOfVisibleTabs;
- (void)expand;
- (void)reloadData;
- (void)setSelectedTab:(NSUInteger)anIndex;

@property (nonatomic, readonly) NSArray* tabItems;
@property (nonatomic) OakTabItem* selectedTabItem;
- (OakTabItem*)tabItemForView:(id)aView;
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
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView identifierForIndex:(NSUInteger)anIndex;
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex;
@end
