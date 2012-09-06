#include <ns/ns.h>
#include <bundles/bundles.h>
#include <OakFoundation/NSString Additions.h>
#include <BundleMenu/BundleMenu.h>

/*

The route of an event seems to be:

 - OakTextView performKeyEquivalent:
 - OakMainMenu performKeyEquivalent:
 - OakTextView keyDown:

Handling keys in keyDown: (and trusting NSMneu to handle menu items) leads to a
few problems:

 - If multiple menu items share key, NSMenu will pick one at random. We can
   make a better choice since we know about scope selectors.
 - If user started a multi-stroke key sequence, NSMenu will not know about it
   and may disrupt it (by firing a menu item for one of the keys involved).
 - Some “special keys” do not make it to OakTextView’s keyDown: (e.g. control
   left/right).

For this reason we:

 - Handle bundle items, “special keys”, and multi-stroke sequences in
   OakTextView performKeyEquivalent:
 - Bypass NSMenu’s performKeyEquivalent: for the bundles menu.
 - Handle bundle items in OakMainMenu performKeyEquivalent: — this is incase
   there are no windows open.

One downside is that we do not get the Bundles menu flashing when the user
picks from that menu (via a key equivalent). To achieve this, I am thinking it
might be possible to replace the Bundles menu with one that has just one item
(with the key equivalent pressed) and then call performKeyEquivalent: on this
submenu.

*/

static CGPoint MenuPosition ()
{
	NSPoint pos = [NSEvent mouseLocation];
	pos.y -= 16;

	return NSPointToCGPoint(pos);
}

@interface OakMainMenu : NSMenu
{
	IBOutlet NSMenuItem* bundlesMenuItem;
}
@end

@implementation OakMainMenu
- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	std::string const keyString = to_s(anEvent);

	auto const bundleItems = bundles::query(bundles::kFieldKeyEquivalent, keyString, "", bundles::kItemTypeCommand|bundles::kItemTypeGrammar|bundles::kItemTypeSnippet);
	if(!bundleItems.empty())
	{
		if(bundles::item_ptr item = OakShowMenuForBundleItems(bundleItems, MenuPosition()))
			[NSApp sendAction:@selector(performBundleItemWithUUIDString:) to:nil from:[NSString stringWithCxxString:item->uuid()]];
		return YES;
	}

	for(NSMenuItem* menuItem in [[self itemArray] reverseObjectEnumerator])
	{
		if(menuItem == bundlesMenuItem)
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
