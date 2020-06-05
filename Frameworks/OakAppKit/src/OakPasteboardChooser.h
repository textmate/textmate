@class OakPasteboard;

@interface OakPasteboardChooser : NSWindowController
@property (nonatomic) NSString* filterString;
@property (nonatomic) SEL action;
@property (nonatomic) SEL alternateAction;
@property (nonatomic, weak) id target;

+ (instancetype)sharedChooserForPasteboard:(OakPasteboard*)pboard;
- (void)showWindowRelativeToFrame:(NSRect)parentFrame;
@end
