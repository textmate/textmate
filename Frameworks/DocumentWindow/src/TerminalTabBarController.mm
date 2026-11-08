#import "TerminalTabBarController.h"
#if HAVE_OAK_SWIFTUI
#import "OakSwiftUI-Swift.h"
#import <OakTabBarView/OakTabBarView.h>

@interface TerminalTabBarController () <OakTabBarViewDelegate, OakTabBarViewDataSource>
@property (nonatomic) OakTabBarView* tabBar;
@property (nonatomic, weak) OakTerminalPanelController* controller;
@end

@implementation TerminalTabBarController
- (instancetype)initWithTerminalController:(OakTerminalPanelController*)controller
{
	if(self = [super init])
	{
		_controller = controller;
		_tabBar = [[OakTabBarView alloc] initWithFrame:NSZeroRect];
		_tabBar.delegate = self;
		_tabBar.dataSource = self;
		_tabBar.neverHideLeftBorder = YES;
		__weak TerminalTabBarController* weakSelf = self;
		controller.tabsDidChange = ^{ [weakSelf reloadTabs]; };
		[controller installTabBar:_tabBar];
	}
	return self;
}

- (void)reloadTabs
{
	[self.tabBar reloadData];
	self.tabBar.selectedTabIndex = self.controller.selectedSessionIndex;
	// Single fan-out point: consumers that only need the resulting state hook here instead of
	// fighting over the controller's one `tabsDidChange` block.
	if(self.tabsDidReloadHandler)
		self.tabsDidReloadHandler();
}

- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)bar { return self.controller.sessionIdentifiers.count; }
- (NSString*)tabBarView:(OakTabBarView*)bar titleForIndex:(NSUInteger)index { return self.controller.sessionTitles[index]; }
- (NSString*)tabBarView:(OakTabBarView*)bar pathForIndex:(NSUInteger)index { return @""; }
- (NSUUID*)tabBarView:(OakTabBarView*)bar UUIDForIndex:(NSUInteger)index { return self.controller.sessionIdentifiers[index]; }
- (BOOL)tabBarView:(OakTabBarView*)bar isEditedAtIndex:(NSUInteger)index
{
	NSArray<NSNumber*>* attention = self.controller.sessionNeedsAttention;
	return index < attention.count && attention[index].boolValue;
}
- (NSString*)tabBarView:(OakTabBarView*)bar toolTipForIndex:(NSUInteger)index
{
	// A terminal tab has no path to show, so the attention dot is the only thing worth describing.
	NSArray<NSString*>* toolTips = self.controller.sessionToolTips;
	return index < toolTips.count ? toolTips[index] : @"";
}
- (OakTabStatus)tabBarView:(OakTabBarView*)bar statusForIndex:(NSUInteger)index
{
	NSArray<NSNumber*>* values = self.controller.sessionStatusValues;
	return index < values.count ? (OakTabStatus)values[index].integerValue : OakTabStatusNone;
}
- (NSString*)tabBarView:(OakTabBarView*)bar statusDescriptionForIndex:(NSUInteger)index
{
	NSArray<NSString*>* descriptions = self.controller.sessionStatusDescriptions;
	return index < descriptions.count ? descriptions[index] : @"";
}
- (BOOL)tabBarView:(OakTabBarView*)bar shouldSelectIndex:(NSUInteger)index
{
	[self.controller selectSessionAt:index];
	return YES;
}
- (void)tabBarView:(OakTabBarView*)bar didDoubleClickIndex:(NSUInteger)index
{
	if(self.controller.sessionIdentifiers.count > 1 && self.detachSessionHandler)
		self.detachSessionHandler(self.controller.sessionIdentifiers[index]);
}
- (void)tabBarViewDidDoubleClick:(OakTabBarView*)bar { [self.controller newTerminal]; }

- (void)performCloseTab:(OakTabBarView*)sender
{
	NSArray* identifiers = self.controller.sessionIdentifiers;
	if(sender.tag >= 0 && sender.tag < identifiers.count)
		[self.controller closeSessionsWithIdentifiers:@[ identifiers[sender.tag] ]];
}
- (void)performCloseOtherTabsXYZ:(OakTabBarView*)sender
{
	NSMutableArray* identifiers = [self.controller.sessionIdentifiers mutableCopy];
	if(sender.tag >= 0 && sender.tag < identifiers.count)
	{
		[identifiers removeObjectAtIndex:sender.tag];
		[self.controller closeSessionsWithIdentifiers:identifiers];
	}
}

- (BOOL)performDropOfTabItem:(NSUUID*)identifier fromTabBar:(OakTabBarView*)source index:(NSUInteger)sourceIndex toTabBar:(OakTabBarView*)destination index:(NSUInteger)destinationIndex operation:(NSDragOperation)operation
{
	if(destination != self.tabBar || operation != NSDragOperationMove || ![source.delegate isKindOfClass:TerminalTabBarController.class])
		return NO;
	TerminalTabBarController* sourceController = (TerminalTabBarController*)source.delegate;
	NSUInteger index = [sourceController.controller.sessionIdentifiers indexOfObject:identifier];
	if(index == NSNotFound)
		return NO;
	if(source == destination)
		[self.controller moveSessionFrom:index to:destinationIndex];
	else
		[sourceController.controller transferSessionWithIdentifier:identifier to:self.controller at:destinationIndex];
	return YES;
}

- (NSMenu*)menuForTabBarView:(OakTabBarView*)bar
{
	NSArray* identifiers = self.controller.sessionIdentifiers;
	NSInteger index = bar.tag;
	NSMenu* menu = [[NSMenu alloc] initWithTitle:@"Terminal"];
	NSMenuItem* newTab = [menu addItemWithTitle:@"New Tab" action:@selector(newTerminal) keyEquivalent:@""];
	newTab.target = self.controller;
	if(index < 0 || index >= identifiers.count)
		return menu;
	NSMenuItem* move = [menu addItemWithTitle:@"Move Tab to New Window" action:@selector(detachTabFromMenu:) keyEquivalent:@""];
	move.target = self;
	move.representedObject = identifiers[index];
	[menu addItem:NSMenuItem.separatorItem];
	NSArray* groups = @[
		@[ identifiers[index] ],
		[identifiers filteredArrayUsingPredicate:[NSPredicate predicateWithBlock:^BOOL(NSUUID* value, NSDictionary* bindings){ return ![value isEqual:identifiers[index]]; }]],
		[identifiers subarrayWithRange:NSMakeRange(index + 1, identifiers.count - index - 1)],
		[identifiers subarrayWithRange:NSMakeRange(0, index)]
	];
	NSArray* titles = @[ @"Close Tab", @"Close Other Tabs", @"Close Tabs to the Right", @"Close Tabs to the Left" ];
	for(NSUInteger i = 0; i < groups.count; ++i)
	{
		NSMenuItem* item = [menu addItemWithTitle:titles[i] action:@selector(closeTabsFromMenu:) keyEquivalent:@""];
		item.target = self;
		item.representedObject = groups[i];
	}
	return menu;
}
- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if(item.action == @selector(detachTabFromMenu:))
		return self.detachSessionHandler != nil && self.controller.sessionIdentifiers.count > 1;
	return [item.representedObject count] > 0;
}
- (void)detachTabFromMenu:(NSMenuItem*)sender
{
	if(self.detachSessionHandler)
		self.detachSessionHandler(sender.representedObject);
}
- (void)closeTabsFromMenu:(NSMenuItem*)sender { [self.controller closeSessionsWithIdentifiers:sender.representedObject]; }
@end
#endif
