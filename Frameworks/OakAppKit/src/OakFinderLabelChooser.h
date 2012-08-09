#import <oak/debug.h>

@interface OakFinderLabelChooser : NSView
{
	OBJC_WATCH_LEAKS(OakFinderLabelChooser);

	NSInteger selectedIndex;
	NSInteger highlightedIndex;
	NSMutableArray* labelNames;

	BOOL enabled;
	id target;
	SEL action;
}
@property (nonatomic, assign) BOOL enabled;
@property (nonatomic, assign) id target;
@property (nonatomic, assign) SEL action;
@property (nonatomic, assign) NSInteger selectedIndex;
@end
