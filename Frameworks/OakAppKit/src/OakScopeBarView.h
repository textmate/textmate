@interface OakScopeBarViewController : NSViewController
@property (nonatomic) NSArray* labels;
@property (nonatomic) NSInteger selectedIndex;
- (void)selectNextButton:(id)sender;
- (void)selectPreviousButton:(id)sender;
- (void)updateGoToMenu:(NSMenu*)aMenu;
@end
