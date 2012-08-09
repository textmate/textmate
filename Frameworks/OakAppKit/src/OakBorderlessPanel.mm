#import "OakBorderlessPanel.h"

@implementation OakBorderlessPanel
- (id)initWithContentRect:(NSRect)contentRect styleMask:(NSUInteger)styleMask backing:(NSBackingStoreType)backingType defer:(BOOL)flag
{
	styleMask |= NSBorderlessWindowMask;
	styleMask &= ~NSTitledWindowMask;
	return [super initWithContentRect:contentRect styleMask:styleMask backing:backingType defer:flag];
}

- (BOOL)isKeyWindow
{
	return YES;
}
@end
