#import "NSMenu Additions.h"

@implementation NSMenu (Additions)
- (NSMenuItem*)parentMenuItem
{
	NSInteger i = [self.supermenu indexOfItemWithSubmenu:self];
	return i != -1 ? self.supermenu.itemArray[i] : nil;
}
@end
