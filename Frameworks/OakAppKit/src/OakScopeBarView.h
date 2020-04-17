@interface OakScopeBarViewController : NSViewController
@property (nonatomic) NSArray* labels;
@property (nonatomic) BOOL allowsEmptySelection;
@property (nonatomic) NSUInteger selectedIndex;
@property (nonatomic) NSControlSize controlSize;
- (void)selectNextButton:(id)sender;
- (void)selectPreviousButton:(id)sender;
- (void)updateGoToMenu:(NSMenu*)aMenu;
@end
