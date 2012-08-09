extern NSUInteger const OakChoiceMenuKeyUnused;
extern NSUInteger const OakChoiceMenuKeyReturn;
extern NSUInteger const OakChoiceMenuKeyTab;
extern NSUInteger const OakChoiceMenuKeyCancel;
extern NSUInteger const OakChoiceMenuKeyMovement;

@interface OakChoiceMenu : NSResponder <NSTableViewDataSource>
{
	NSWindow* window;
	NSTableView* tableView;
	NSArray* choices;
	NSUInteger choiceIndex;
	NSUInteger keyAction;
	NSPoint topLeftPosition;
}
@property (nonatomic, retain) NSArray* choices;
@property (nonatomic, assign) NSUInteger choiceIndex;
@property (nonatomic, readonly) NSString* selectedChoice;
- (void)showAtTopLeftPoint:(NSPoint)aPoint forView:(NSView*)aView;
- (BOOL)isVisible;
- (NSUInteger)didHandleKeyEvent:(NSEvent*)anEvent;
@end
