#import "NSView Additions.h"

@implementation NSView (PopupAddition)
- (void)showMenu:(NSMenu*)menu inRect:(NSRect)rect withSelectedIndex:(NSInteger)index font:(NSFont*)font popup:(BOOL)isPopup
{
	NSPopUpButtonCell* cell = [[[NSPopUpButtonCell alloc] init] autorelease];
	[cell setPullsDown:!isPopup];
	if(!isPopup)
		[menu insertItemWithTitle:@"dummy title item" action:NULL keyEquivalent:@"" atIndex:0];
	[cell setFont:font];
	[cell setAltersStateOfSelectedItem:NO];
	[cell setMenu:menu];
	[cell selectItemAtIndex:index];
	[cell performClickWithFrame:rect inView:self];
}
@end
