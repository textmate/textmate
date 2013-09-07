#import <oak/debug.h>

PUBLIC @interface OakFinderLabelChooser : NSView
@property (nonatomic) BOOL enabled;
@property (nonatomic) NSInteger selectedIndex;
@property (nonatomic, weak) id target;
@property (nonatomic) SEL action;
@property (nonatomic) NSFont* font;
@end
