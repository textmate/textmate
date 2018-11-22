#import "ViewController.h"

@interface MyView : NSView
@end

@implementation MyView
- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(100, 100);
}

- (void)drawRect:(NSRect)aRect
{
	NSEraseRect(aRect);
}
@end

@implementation ViewController
- (void)loadView
{
	self.view = [[MyView alloc] initWithFrame:NSZeroRect];
}
@end
