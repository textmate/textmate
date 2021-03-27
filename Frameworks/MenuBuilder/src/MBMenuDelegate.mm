#import "MBMenuDelegate.h"

@interface MBProxyMenuItem : NSMenuItem
@end

@implementation MBProxyMenuItem
- (void)tmSendAction:(id)sender
{
	[NSApp sendAction:self.action to:self.target from:self];
	self.target = nil;
	self.representedObject = nil;
}
@end

@interface MBKeyEquivalentMenu : NSMenu
@end

@implementation MBKeyEquivalentMenu
- (NSMenuProperties)propertiesToUpdate
{
	return NSMenuPropertyItemKeyEquivalent;
}
@end

@interface MBMenuDelegate ()
@property (nonatomic) SEL selector;
@property (nonatomic) MBProxyMenuItem* proxyMenuItem;
@end

@implementation MBMenuDelegate
+ (MBMenuDelegate*)delegateUsingSelector:(SEL)selector
{
	static std::map<SEL, MBMenuDelegate*> retainedDelegates;
	auto it = retainedDelegates.find(selector);
	if(it == retainedDelegates.end())
		it = retainedDelegates.emplace(selector, [[MBMenuDelegate alloc] initWithSelector:selector]).first;
	return it->second;
}

- (instancetype)initWithSelector:(SEL)selector
{
	if(self = [super init])
		_selector = selector;
	return self;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[aMenu removeAllItems];
	if(id delegate = [NSApp targetForAction:_selector])
			[NSApp sendAction:_selector to:delegate from:aMenu];
	else	[aMenu addItemWithTitle:@"no items" action:NULL keyEquivalent:@""];
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)anEvent target:(id*)anId action:(SEL*)aSEL
{
	NSUInteger flags     = anEvent.modifierFlags & (NSEventModifierFlagCommand|NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption);
	NSString* characters = anEvent.characters;
	if(flags != NSEventModifierFlagCommand || characters.length != 1 || ![NSCharacterSet.decimalDigitCharacterSet characterIsMember:[characters characterAtIndex:0]])
		return NO;

	NSMenu* dummy = [MBKeyEquivalentMenu new];
	[self menuNeedsUpdate:dummy];
	for(NSMenuItem* item in dummy.itemArray)
	{
		if(item.keyEquivalentModifierMask == flags && [item.keyEquivalent isEqualToString:characters])
		{
			if(!self.proxyMenuItem)
				self.proxyMenuItem = [MBProxyMenuItem new];

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
