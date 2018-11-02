#include <oak/misc.h>

PUBLIC @interface OakPasteboardChooser : NSWindowController
@property (nonatomic) NSString* filterString;
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;

+ (instancetype)sharedChooserForName:(NSString*)aName;
- (void)showWindowRelativeToFrame:(NSRect)parentFrame;
@end
