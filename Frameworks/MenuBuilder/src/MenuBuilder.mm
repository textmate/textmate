#import "MenuBuilder.h"

NSMenuItem* MBCreateMenuItem (MBMenuItem const& item)
{
	if(!item.title || item.separator)
		return [NSMenuItem separatorItem];

	NSMenuItem* menuItem = [[NSMenuItem alloc] initWithTitle:item.title action:item.action keyEquivalent:item.keyEquivalent];
	menuItem.keyEquivalentModifierMask = item.modifierFlags;
	menuItem.tag                       = item.tag;
	menuItem.target                    = item.target;
	menuItem.alternate                 = item.alternate;
	menuItem.enabled                   = item.enabled;
	menuItem.hidden                    = item.hidden;
	menuItem.indentationLevel          = item.indent;
	menuItem.representedObject         = item.representedObject;

	if(item.key)
		menuItem.keyEquivalent = [NSString stringWithFormat:@"%C", item.key];

	if(item.submenu.size() > 0 || item.systemMenu != MBMenuTypeRegular || item.delegate || item.submenuRef)
	{
		NSMenu* submenu = MBCreateMenu(item.submenu, item.title);
		submenu.delegate = item.delegate;
		menuItem.submenu = submenu;

		switch(item.systemMenu)
		{
			case MBMenuTypeServices:   NSApp.servicesMenu                       = submenu; break;
			case MBMenuTypeFont:       NSFontManager.sharedFontManager.fontMenu = submenu; break;
			case MBMenuTypeWindows:    NSApp.windowsMenu                        = submenu; break;
			case MBMenuTypeHelp:       NSApp.helpMenu                           = submenu; break;

			case MBMenuTypeOpenRecent:
				if([submenu respondsToSelector:@selector(_setMenuName:)])
					[submenu performSelector:@selector(_setMenuName:) withObject:@"NSRecentDocumentsMenu"];
			break;
		}

		if(item.submenuRef)
			*item.submenuRef = submenu;
	}

	if(item.ref)
		*item.ref = menuItem;

	return menuItem;
}

PUBLIC NSMenu* MBCreateMenu (MBMenu const& items, NSString* title, NSMenu* existingMenu)
{
	NSMenu* menu = existingMenu ?: [[NSMenu alloc] initWithTitle:title];
	for(auto const& item : items)
		[menu addItem:MBCreateMenuItem(item)];
	return menu;
}
