#include <oak/misc.h>

@class OakPasteboard;

PUBLIC @interface OakPasteboardChooser : NSResponder
@property (nonatomic) NSString* filterString;
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;

- (id)initWithPasteboard:(OakPasteboard*)aPasteboard;
- (void)showWindow:(id)sender;
- (void)showWindowRelativeToFrame:(NSRect)parentFrame;
- (void)close;
@end
