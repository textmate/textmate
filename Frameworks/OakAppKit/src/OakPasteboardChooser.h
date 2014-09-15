#include <oak/misc.h>

PUBLIC @interface OakPasteboardChooser : NSResponder
@property (nonatomic) NSString* filterString;
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;

+ (instancetype)sharedChooserForName:(NSString*)aName;
- (void)showWindow:(id)sender;
- (void)showWindowRelativeToFrame:(NSRect)parentFrame;
- (void)close;
@end
