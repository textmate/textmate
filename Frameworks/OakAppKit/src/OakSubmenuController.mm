#import "OakSubmenuController.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(OakSubmenuController);

@interface OakProxyMenuItem : NSMenuItem
@end

@implementation OakProxyMenuItem
- (void)tmSendAction:(id)sender
{
	[NSApp sendAction:self.action to:self.target from:self];
	self.target = nil;
	self.representedObject = nil;
}
@end

@interface OakSubmenuController ()
@property (nonatomic) OakProxyMenuItem* proxyMenuItem;
@end

@implementation OakSubmenuController
- (void)awakeFromNib
{
	[selectTabMenu setDelegate:self];
	[marksMenu setDelegate:self];
}

- (void)updateMenu:(NSMenu*)aMenu withSelector:(SEL)aSelector
{
	[aMenu removeAllItems];
	if(id delegate = [NSApp targetForAction:aSelector])
			[NSApp sendAction:aSelector to:delegate from:aMenu];
	else	[aMenu addItemWithTitle:@"no items" action:NULL keyEquivalent:@""];
	D(DBF_OakSubmenuController, bug("%s\n", [[aMenu description] UTF8String]););
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[self updateMenu:aMenu withSelector:aMenu == selectTabMenu ? @selector(updateSelectTabMenu:) : @selector(updateBookmarksMenu:)];
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)anEvent target:(id*)anId action:(SEL*)aSEL
{
	D(DBF_OakSubmenuController, bug("%s %s\n", to_s(anEvent).c_str(), [[aMenu description] UTF8String]););

	if(aMenu != selectTabMenu)
		return NO;

	std::string const eventString = to_s(anEvent);
	if(eventString < "@1" || "@9" < eventString)
		return NO;

	NSMenu* dummy = [NSMenu new];
	[self updateMenu:dummy withSelector:@selector(updateSelectTabMenu:)];
	for(NSMenuItem* item in [dummy itemArray])
	{
		if(eventString == ns::create_event_string(item.keyEquivalent, item.keyEquivalentModifierMask))
		{
			D(DBF_OakSubmenuController, bug("%s%ld\n", sel_getName(item.action), item.tag););
			if(!self.proxyMenuItem)
				self.proxyMenuItem = [OakProxyMenuItem new];

			self.proxyMenuItem.action            = item.action;
			self.proxyMenuItem.target            = item.target;
			self.proxyMenuItem.tag               = item.tag;
			self.proxyMenuItem.representedObject = item.representedObject;

			*anId = self.proxyMenuItem;
			*aSEL = @selector(tmSendAction:);

			return YES;
		}
	}
	return NO;
}
@end
