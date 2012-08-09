@interface FFFindWindow : NSPanel
{
	IBOutlet NSView* collapsibleView;
	IBOutlet NSView* collapsibleViewPlaceholder;

	// Window resizing state
	BOOL isExpanded;
	NSSize shrinkedFindPanelSize;
	NSSize expandedFindPanelSize;
	CGFloat maxContentHeight;

	NSViewAnimation* resizeAnimation;
}
@property (nonatomic, assign) BOOL isExpanded;
@end
