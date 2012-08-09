#import "NSWindow Additions.h"

@interface NSWindow (Private)
- (void)setBottomCornerRounded:(BOOL)flag;
@end

@implementation NSWindow (Other)
- (void)toggleVisibility
{
	if([self isVisible])
	{
		[self makeFirstResponder:nil];
		[self selectNextKeyView:self];
		[self makeKeyAndOrderFront:self];
	}
	else
	{
		[self makeFirstResponder:nil];
		[self makeKeyAndOrderFront:self];
	}
}

- (void)setPrivateBottomCornerRounded:(BOOL)flag
{
	if([self respondsToSelector:@selector(setBottomCornerRounded:)])
		[self setBottomCornerRounded:flag];
}
@end
