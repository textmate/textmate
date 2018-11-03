#import <oak/misc.h>

@class OakBackgroundFillView;
@class OakRolloverButton;

PUBLIC @interface OakTabBarStyle : NSObject
+ (instancetype)sharedInstance;

@property (nonatomic, readonly) CGFloat leftPadding;
@property (nonatomic, readonly) CGFloat rightPadding;
@property (nonatomic, readonly) CGFloat tabViewSpacing;
@property (nonatomic, readonly) CGFloat minimumTabSize;
@property (nonatomic, readonly) CGFloat maximumTabSize;

@property (nonatomic, readonly) NSDictionary* activeTabTextStyles;
@property (nonatomic, readonly) NSDictionary* selectedTabTextStyles;
@property (nonatomic, readonly) NSDictionary* inactiveTabTextStyles;

- (void)setupTabBarView:(OakBackgroundFillView*)aView;

- (void)updateLeftCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateRightCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateTabItemView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateCloseButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected modified:(BOOL)modified;
- (void)updateOverflowButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected;
@end
