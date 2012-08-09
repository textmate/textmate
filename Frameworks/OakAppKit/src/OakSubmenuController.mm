#import "OakSubmenuController.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <ns/ns.h>
#import <oak/CocoaSTL.h>

OAK_DEBUG_VAR(OakSubmenuController);

static OakSubmenuController* SharedInstance = nil;

@interface OakSubmenuController ()
@property (nonatomic, retain) id representedObject;
@end

@implementation OakSubmenuController
@synthesize representedObject;

+ (OakSubmenuController*)sharedInstance
{
	return SharedInstance ?: [[OakSubmenuController new] autorelease];
}

- (id)init
{
	if(SharedInstance)
			[self release];
	else	self = SharedInstance = [[super init] retain];
	return SharedInstance;
}

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

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)anEvent target:(id*)anId action:(SEL*)aSEL
{
	D(DBF_OakSubmenuController, bug("%s %s\n", to_s(anEvent).c_str(), [[aMenu description] UTF8String]););	

	if(aMenu != goToMenu)
		return NO;

	self.representedObject = nil;
	std::string const eventString = to_s(anEvent);

	NSMenu* dummy = [[NSMenu new] autorelease];
	[self updateMenu:dummy withSelector:@selector(updateGoToMenu:)];
	for(NSMenuItem* item in [dummy itemArray])
	{
		if(eventString == ns::create_event_string(item.keyEquivalent, item.keyEquivalentModifierMask))
		{
			D(DBF_OakSubmenuController, bug("%s%ld\n", SELNAME(item.action), item.tag););
			*anId                  = item.target;
			*aSEL                  = item.action;
			tag                    = item.tag;
			self.representedObject = item.representedObject;
			return YES;
		}
	}
	return NO;
}

- (NSInteger)tagForSender:(id)aSender
{
	if([aSender respondsToSelector:@selector(tag)])
		return [aSender tag];
	ASSERT([aSender isKindOfClass:[NSMenu class]]);
	return tag;
}

- (id)representedObjectForSender:(id)aSender
{
	if([aSender respondsToSelector:@selector(representedObject)])
		return [aSender representedObject];
	ASSERT([aSender isKindOfClass:[NSMenu class]]);
	return representedObject;
}
@end
