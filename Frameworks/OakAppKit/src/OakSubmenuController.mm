#import "OakSubmenuController.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(OakSubmenuController);

@implementation OakSubmenuController
- (void)awakeFromNib
{
	[goToMenu setDelegate:self];
	[marksMenu setDelegate:self];
}

- (void)updateMenu:(NSMenu*)aMenu withSelector:(SEL)aSelector
{
	[aMenu removeAllItems];
	if(id delegate = [NSApp targetForAction:aSelector])
			[delegate performSelector:aSelector withObject:aMenu];
	else	[aMenu addItemWithTitle:@"no items" action:NULL keyEquivalent:@""];
	D(DBF_OakSubmenuController, bug("%s\n", [[aMenu description] UTF8String]););
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[self updateMenu:aMenu withSelector:aMenu == goToMenu ? @selector(updateGoToMenu:) : @selector(updateBookmarksMenu:)];
}
@end
