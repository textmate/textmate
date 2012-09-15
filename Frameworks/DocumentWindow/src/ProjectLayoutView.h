@interface ProjectLayoutView : NSView
{
	NSView* tabBarView;
	NSView* documentView;
	NSView* fileBrowserView;
	NSView* htmlOutputView;

	CGFloat fileBrowserWidth;
	BOOL fileBrowserOnRight;

	CGFloat htmlOutputHeight;
	BOOL htmlOutputOnRight;

	NSLayoutConstraint* fileBrowserWidthConstraint;
	NSLayoutConstraint* htmlOutputHeightConstraint;
}
@property (nonatomic, retain) NSView* tabBarView;
@property (nonatomic, retain) NSView* documentView;
@property (nonatomic, retain) NSView* fileBrowserView;
@property (nonatomic, retain) NSView* htmlOutputView;

@property (nonatomic, assign) CGFloat fileBrowserWidth;
@property (nonatomic, assign) BOOL fileBrowserOnRight;

@property (nonatomic, assign) CGFloat htmlOutputHeight;
@property (nonatomic, assign) BOOL htmlOutputOnRight;
@end
