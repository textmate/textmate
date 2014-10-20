#import "OakUIConstructionFunctions.h"
#import <oak/misc.h>

@class OakRolloverButton;

PUBLIC @interface OakTabBarStyle : NSObject
+ (instancetype)sharedInstance;

@property (nonatomic) CGFloat leftPadding;
@property (nonatomic) CGFloat rightPadding;
@property (nonatomic) CGFloat tabViewSpacing;
@property (nonatomic) CGFloat minimumTabSize;
@property (nonatomic) CGFloat maximumTabSize;

@property (nonatomic) NSDictionary* activeTabTextStyles;
@property (nonatomic) NSDictionary* inactiveTabTextStyles;

- (void)setupTabBarView:(OakBackgroundFillView*)aView;

- (void)updateLeftCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateRightCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateTabItemView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected;
- (void)updateCloseButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected modified:(BOOL)modified;
- (void)updateOverflowButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected;
@end

typedef NS_ENUM(NSUInteger, OakTabItemViewVisibleCaps) {
	OakTabItemViewVisibleCapsBoth,
	OakTabItemViewVisibleCapsNone,
	OakTabItemViewVisibleCapsLeft,
	OakTabItemViewVisibleCapsRight
};

PUBLIC @interface OakTabItemView : OakBackgroundFillView
@property (nonatomic, readonly) NSRect contentFrame;
@property (nonatomic) OakRolloverButton* closeButton;
@property (nonatomic) OakRolloverButton* overflowButton;
@property (nonatomic) OakTabItemViewVisibleCaps visibleCaps;
@property (nonatomic) NSString* title;
@property (nonatomic) BOOL modified;
@property (nonatomic) BOOL selected;
@property (nonatomic) BOOL showOverflowButton;
- (id)initWithFrame:(NSRect)aRect title:(NSString*)aTitle modified:(BOOL)modified;
@end
