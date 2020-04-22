#import "OakTabBarView.h"

@interface OakTabBarViewController : NSTitlebarAccessoryViewController
@property (nonatomic) NSArray<NSUUID*>*   identifiers;
@property (nonatomic) NSArray<NSString*>* titles;
@property (nonatomic) NSArray<NSNumber*>* modifiedStates;
@property (nonatomic) NSArray<NSURL*>*    URLs;
@property (nonatomic) NSArray<NSString*>* toolTips;
@property (nonatomic) NSArray<NSImage*>*  images;
@property (nonatomic) NSUInteger          selectedIndex;

@property (nonatomic, weak) id <OakTabBarViewDelegate> delegate;

- (void)updateGoToMenu:(NSMenu*)aMenu;
@end
