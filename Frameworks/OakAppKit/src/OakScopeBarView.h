#import <oak/misc.h>

PUBLIC @interface OakScopeBarView : NSView
@property (nonatomic) NSInteger selectedIndex;
@property (nonatomic) NSArray* labels;
@property (nonatomic, readonly) NSArray* buttons;
- (void)selectNextButton:(id)sender;
- (void)selectPreviousButton:(id)sender;
- (void)updateGoToMenu:(NSMenu*)aMenu;
@end
