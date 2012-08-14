#import "NSMenu Additions.h"

@implementation NSMenu (Additions)
- (NSMenuItem*)parentMenuItem;
{
	NSMenuItem* superItem = nil;
	for(NSMenuItem* item in [[self supermenu] itemArray])
	{
		if([item submenu] == self)
		{
			superItem = item;
			break;
		}
	}
	return superItem;
}
@end
