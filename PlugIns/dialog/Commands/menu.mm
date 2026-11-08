#import "../Dialog2.h"
#import "../TMDCommand.h"
#import "Utilities/TextMate.h" // -positionForWindowUnderCaret

// ========
// = Menu =
// ========

/*
echo '{ items = ({title = "foo"; header = 1;},{title = "bar";}); }' | "$DIALOG" menu
"$DIALOG" menu --items '({title = "foo"; header = 1;},{title = "bar";})'
*/

@interface DialogPopupMenuTarget : NSObject
@property (nonatomic, retain) NSDictionary* selectedItem;
@end

@implementation DialogPopupMenuTarget
- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	return [menuItem action] == @selector(takeRepresentedObjectFrom:);
}

- (void)takeRepresentedObjectFrom:(NSMenuItem*)sender
{
	NSAssert([sender isKindOfClass:[NSMenuItem class]], @"Unexpected sender for menu target");
	self.selectedItem = [sender representedObject];
}
@end

@interface TMDMenuCommand : TMDCommand
@end

@implementation TMDMenuCommand
+ (void)load
{
	[TMDCommand registerObject:[self new] forCommand:@"menu"];
}

- (NSString*)commandDescription
{
	return @"Presents a menu using the given structure and returns the option chosen by the user";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"\t%1$@ --items '({title = foo;}, {separator = 1;}, {header=1; title = bar;}, {title = baz;})'\n", invocation];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];
	NSArray* menuItems = [args objectForKey:@"items"];

	// FIXME this is needed only because we presently can’t express argument constraints (CLIProxy would otherwise correctly validate/convert CLI arguments)
	if([menuItems isKindOfClass:[NSString class]])
		menuItems = [NSPropertyListSerialization propertyListWithData:[(NSString*)menuItems dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];

	NSMenu* menu = [NSMenu new];
	[menu setFont:[NSFont menuFontOfSize:([NSUserDefaults.standardUserDefaults integerForKey:@"OakBundleManagerDisambiguateMenuFontSize"] ?: 11)]];
	DialogPopupMenuTarget* menuTarget = [[DialogPopupMenuTarget alloc] init];

	NSInteger itemId = 0;
	BOOL inSection = NO;
	for(NSDictionary* menuItem : menuItems)
	{
		if([[menuItem objectForKey:@"separator"] intValue])
		{
			[menu addItem:[NSMenuItem separatorItem]];
		}
		else if([[menuItem objectForKey:@"header"] intValue])
		{
			[menu addItemWithTitle:[menuItem objectForKey:@"title"] action:NULL keyEquivalent:@""];
			inSection = YES;
		}
		else
		{
			NSMenuItem* theItem = [menu addItemWithTitle:[menuItem objectForKey:@"title"] action:@selector(takeRepresentedObjectFrom:) keyEquivalent:@""];
			[theItem setTarget:menuTarget];
			[theItem setRepresentedObject:menuItem];
			if(++itemId <= 10)
			{
				[theItem setKeyEquivalent:[NSString stringWithFormat:@"%ld", itemId % 10]];
				[theItem setKeyEquivalentModifierMask:0];
			}
			if(inSection)
				[theItem setIndentationLevel:1];
		}
	}

	NSPoint pos = [NSEvent mouseLocation];
	if(id textView = [NSApp targetForAction:@selector(positionForWindowUnderCaret)])
		pos = [textView positionForWindowUnderCaret];


	if([menu popUpMenuPositioningItem:nil atLocation:pos inView:nil] && menuTarget.selectedItem)
		[TMDCommand writePropertyList:menuTarget.selectedItem toFileHandle:[proxy outputHandle]];
}
@end
