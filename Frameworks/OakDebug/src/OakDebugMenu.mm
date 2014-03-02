#import "OakDebug.h"
#import "OakDebugMenu.h"
#import <oak/oak.h>
#import <malloc/malloc.h>

namespace
{
	struct MiB_t
	{
		MiB_t (size_t inBytes) : unit(bytes)
		{
			size = (double)inBytes;
			if(inBytes > 1000 * 1024*1024)
			{
				size /= 1024*1024*1024;
				unit  = GiB;
			}
			else if(inBytes > 1000 * 1024)
			{
				size /= 1024*1024;
				unit  = MiB;
			}
			else if(inBytes > 1000)
			{
				size /= 1024;
				unit  = KiB;
			}
		}

		NSString* to_s () const
		{
			char const* unitStr = NULL;
			switch(unit)
			{
				case GiB:   unitStr = "GB"; break;
				case MiB:   unitStr = "MB"; break;
				case KiB:   unitStr = "KB"; break;
				case bytes: unitStr = "bytes"; break;
			}

			std::string pad = "";
			for(size_t i = 1000; i > 1 && size < i; i /= 10)
				pad += "\u2007"; // Figure Space
			return [NSString stringWithFormat:@"%@%.1f %s", @(pad.c_str()), size, unitStr];
		}

		double size;
		enum unit_t { GiB, MiB, KiB, bytes } unit;
	};
}

@implementation OakDebugMenu
+ (void)load
{
	@autoreleasepool {
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(installDebugMenu:) name:NSApplicationDidFinishLaunchingNotification object:NSApp];

		if(NSArray* debugEnabled = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakDebug Enabled"])
		{
			for(NSString* flag in debugEnabled)
				OakDebugBaseClass::registry()[[flag UTF8String]] = true;
		}
	}
}

+ (void)toggleDebugOption:(id)menuItem
{
	OakDebugBaseClass::registry()[[[menuItem representedObject] UTF8String]] ^= true;

	NSMutableArray* debugEnabled = [NSMutableArray array];
	for(auto const& it : OakDebugBaseClass::registry())
	{
		if(it.second)
			[debugEnabled addObject:@(it.first.c_str())];
	}
	[[NSUserDefaults standardUserDefaults] setObject:debugEnabled forKey:@"OakDebug Enabled"];
}

+ (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	bool enabled = OakDebugBaseClass::registry()[[[menuItem representedObject] UTF8String]];
	[menuItem setState:enabled ? NSOnState : NSOffState];
	return YES;
}

+ (BOOL)menuHasKeyEquivalent:(NSMenu*)menu forEvent:(NSEvent*)event target:(id*)target action:(SEL*)action
{
	return NO;
}

+ (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	for(int i = [aMenu numberOfItems]; i-- > 0; )
		[aMenu removeItemAtIndex:i];

	// ===================
	// = Add Memory Info =
	// ===================

	struct mstats m = mstats();

	NSString* usedMemory  = [NSString stringWithFormat:@"%@ Used", MiB_t(m.bytes_used).to_s()];
	NSString* freeMemory  = [NSString stringWithFormat:@"%@ Free", MiB_t(m.bytes_free).to_s()];
	NSString* totalMemory = [NSString stringWithFormat:@"%@ Total", MiB_t(m.bytes_total).to_s()];

	NSString* titles[] = { usedMemory, freeMemory, totalMemory };
	for(size_t i = 0; i < sizeofA(titles); ++i)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:titles[i] action:NULL keyEquivalent:@""];
		[item setEnabled:NO];
	}

	[aMenu addItem:[NSMenuItem separatorItem]];

	// ===================

	std::vector<std::string> sectionNames = OakDebugBaseClass::sectionNames();;
	NSMenu* submenu = nil;
	for(auto const& it : OakDebugBaseClass::registry())
	{
		std::string const& sectionName = OakDebugBaseClass::sectionName(it.first);
		NSMenuItem* item;
		if(oak::contains(sectionNames.begin(), sectionNames.end(), sectionName))
		{
			NSString* section = @(sectionName.c_str());
			NSString* title   = sectionName.size() == it.first.size() ? @"Base" : @(it.first.substr(sectionName.size() + 1).c_str());
			if(![[submenu title] isEqualToString:section])
			{
				submenu = [[NSMenu alloc] initWithTitle:section];
				[[aMenu addItemWithTitle:section action:NULL keyEquivalent:@""] setSubmenu:submenu];
			}
			item = [submenu addItemWithTitle:title action:@selector(toggleDebugOption:) keyEquivalent:@""];
		}
		else
		{
			item = [aMenu addItemWithTitle:@(it.first.c_str()) action:@selector(toggleDebugOption:) keyEquivalent:@""];
		}
		[item setRepresentedObject:@(it.first.c_str())];
		[item setTarget:self];
		[item setState:it.second ? NSOnState : NSOffState];
	}
}

+ (void)installDebugMenu:(id)sender
{
	NSMenuItem* menuItem = [[NSMenuItem alloc] init];
	[menuItem setTitle:@"Debug"];

	NSMenu* debugMenu = [[NSMenu alloc] initWithTitle:@"Debug"];
	[debugMenu setDelegate:self];
	[menuItem setSubmenu:debugMenu];

	[[NSApp mainMenu] addItem:menuItem];
}
@end
