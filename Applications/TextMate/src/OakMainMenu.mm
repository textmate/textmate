#include <ns/ns.h>

@interface OakMainMenu : NSMenu
{
}
@end

@implementation OakMainMenu
- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	std::string const keyString = to_s(anEvent);
	for(NSMenuItem* menuItem in [self itemArray])
	{
		if([[menuItem title] isEqualToString:@"Bundles"])
		{
			for(NSMenuItem* subMenuItem in [[menuItem submenu] itemArray])
			{
				static struct { NSUInteger mask; std::string symbol; } const EventFlags[] =
				{
					{ NSNumericPadKeyMask, "#" },
					{ NSControlKeyMask,    "^" },
					{ NSAlternateKeyMask,  "~" },
					{ NSShiftKeyMask,      "$" },
					{ NSCommandKeyMask,    "@" }
				};

				NSUInteger flags = [subMenuItem keyEquivalentModifierMask];

				std::string res = "";
				iterate(flag, EventFlags)
					res += (flags & flag->mask) ? flag->symbol : "";
				res += to_s([subMenuItem keyEquivalent]);

				if(keyString == res)
				{
					[NSApp sendAction:[subMenuItem action] to:[subMenuItem target] from:subMenuItem];
					return YES;
				}
			}
		}
		else if([[menuItem submenu] performKeyEquivalent:anEvent])
		{
			return YES;
		}
	}
	return NO;
}
@end
