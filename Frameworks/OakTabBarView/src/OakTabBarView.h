#import <oak/misc.h>

@interface OakTabItem : NSObject
@property (nonatomic) NSString* title;
@property (nonatomic) NSString* path;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, getter = isModified) BOOL modified;
@end

@protocol OakTabBarViewDelegate, OakTabBarViewDataSource;

@interface OakTabBarView : NSView
@property (nonatomic, weak) id <OakTabBarViewDelegate> delegate;
@property (nonatomic, weak) id <OakTabBarViewDataSource> dataSource;
@property (nonatomic, readonly) NSInteger countOfVisibleTabs;
- (void)reloadData;
- (void)setSelectedTabIndex:(NSInteger)anIndex;
- (void)performClose:(id)sender;

@property (nonatomic, readonly) NSArray<OakTabItem*>* tabItems;
@property (nonatomic, readonly) OakTabItem* selectedTabItem;

@property (nonatomic) BOOL neverHideLeftBorder;
@end

@protocol OakTabBarViewDelegate <NSObject>
@optional
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView shouldSelectIndex:(NSUInteger)anIndex;
- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex;
- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView;
- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView;

// Methods sent to the delegate which the tab was dragged to
- (BOOL)performDropOfTabItem:(OakTabItem*)tabItem fromTabBar:(OakTabBarView*)sourceTabBar index:(NSUInteger)dragIndex toTabBar:(OakTabBarView*)destTabBar index:(NSUInteger)droppedIndex operation:(NSDragOperation)operation;

- (void)performCloseTab:(OakTabBarView*)sender;
- (void)performCloseOtherTabsXYZ:(OakTabBarView*)sender;
@end

@protocol OakTabBarViewDataSource <NSObject>
- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView;

- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex;
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView pathForIndex:(NSUInteger)anIndex;
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView identifierForIndex:(NSUInteger)anIndex;
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex;
@end
