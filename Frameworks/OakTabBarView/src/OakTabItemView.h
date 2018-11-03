#import <OakAppKit/OakUIConstructionFunctions.h>
#import <oak/misc.h>

@class OakRolloverButton;

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
@property (nonatomic, getter = isModified) BOOL modified;
@property (nonatomic, getter = isSelected) BOOL selected;
@property (nonatomic) BOOL showOverflowButton;
- (id)initWithFrame:(NSRect)aRect title:(NSString*)aTitle modified:(BOOL)modified;
@end
