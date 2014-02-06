#import <oak/misc.h>

PUBLIC @interface OakScopeBarView : NSView
@property (nonatomic) NSInteger selectedIndex;
@property (nonatomic) NSArray* labels;
@property (nonatomic, readonly) NSArray* buttons;
@end
