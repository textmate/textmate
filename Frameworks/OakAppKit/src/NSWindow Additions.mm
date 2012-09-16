#import "NSWindow Additions.h"

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
@end
