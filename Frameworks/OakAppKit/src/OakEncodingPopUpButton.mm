#import "OakEncodingPopUpButton.h"
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/NSString Additions.h>
#import <io/src/path.h>
#import <ns/src/ns.h>
#import <text/src/parse.h>
#import <oak/oak.h>

static NSString* const kUserDefaultsAvailableEncodingsKey = @"availableEncodings";

namespace // encoding_list
{
	struct charset_t
	{
		charset_t (std::string const& name, std::string const& code) : _name(name), _code(code) { }

		std::string const& name () const { return _name; };
		std::string const& code () const { return _code; };

	private:
		std::string _name;
		std::string _code;
	};

	static std::vector<charset_t> encoding_list ()
	{
		std::vector<charset_t> res;

		std::string path = path::join(path::home(), "Library/Application Support/TextMate/Charsets.plist");
		if(!path::exists(path))
			path = to_s([[NSBundle bundleForClass:[OakEncodingPopUpButton class]] pathForResource:@"Charsets" ofType:@"plist"]);

		plist::array_t encodings;
		if(plist::get_key_path(plist::load(path), "encodings", encodings))
		{
			for(auto const& item : encodings)
			{
				std::string name, code;
				if(plist::get_key_path(item, "name", name) && plist::get_key_path(item, "code", code))
					res.emplace_back(name, code);
			}
		}

		return res;
	}
}

namespace // PopulateMenu{Flat,Hierarchical}
{
	struct menu_item_t
	{
		menu_item_t (std::string const& group, std::string const& title, std::string const& represented_object) : group(group), title(title), represented_object(represented_object) { }

		std::string group;
		std::string title;
		std::string represented_object;
	};

	static NSMenuItem* PopulateMenuFlat (NSMenu* menu, std::vector<menu_item_t> const& items, id target, SEL action, std::string const& selected)
	{
		NSMenuItem* res = nil;
		for(auto const& item : items)
		{
			NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:item.group + " – " + item.title] action:action keyEquivalent:@""];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:item.represented_object]];
			[menuItem setTarget:target];

			if(item.represented_object == selected)
				res = menuItem;
		}
		return res;
	}

	static void PopulateMenuHierarchical (NSMenu* containingMenu, std::vector<menu_item_t> const& items, id target, SEL action, std::string const& selected)
	{
		std::string groupName = NULL_STR;
		NSMenu* menu = nil;
		for(auto const& item : items)
		{
			if(groupName != item.group)
			{
				groupName = item.group;

				menu = [NSMenu new];
				[menu setAutoenablesItems:NO];
				[[containingMenu addItemWithTitle:[NSString stringWithCxxString:groupName] action:NULL keyEquivalent:@""] setSubmenu:menu];
			}

			NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:item.title] action:action keyEquivalent:@""];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:item.represented_object]];
			[menuItem setTarget:target];
			if(selected == item.represented_object)
				[menuItem setState:NSControlStateValueOn];
		}
	}
}

@interface OakCustomizeEncodingsWindowController : NSWindowController
{
	NSMutableArray* encodings;
}
@property (class, readonly) OakCustomizeEncodingsWindowController* sharedInstance;
@end

@interface OakEncodingPopUpButton () <OakUserDefaultsObserver>
@property (nonatomic) NSArray*    availableEncodings;
@property (nonatomic) NSMenuItem* firstMenuItem;
@end

@implementation OakEncodingPopUpButton
+ (void)initialize
{
	NSArray* encodings = @[ @"WINDOWS-1252", @"MACROMAN", @"ISO-8859-1", @"UTF-8", @"UTF-16LE//BOM", @"UTF-16BE//BOM", @"SHIFT_JIS", @"GB18030" ];
	[NSUserDefaults.standardUserDefaults registerDefaults:@{ kUserDefaultsAvailableEncodingsKey: encodings }];

	// LEGACY format used prior to 2.0-beta.10
	NSArray* legacy = [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsAvailableEncodingsKey];
	if([legacy containsObject:@"UTF-16BE"] && ![legacy containsObject:@"UTF-16BE//BOM"])
	{
		NSMutableArray* updatedList = [NSMutableArray array];
		for(NSString* charset in legacy)
		{
			BOOL legacyName = ([charset hasPrefix:@"UTF-16"] || [charset hasPrefix:@"UTF-32"]) && ![charset hasSuffix:@"//BOM"];
			[updatedList addObject:legacyName ? [charset stringByAppendingString:@"//BOM"] : charset];
		}
		[NSUserDefaults.standardUserDefaults setObject:updatedList forKey:kUserDefaultsAvailableEncodingsKey];
	}
}

- (void)updateAvailableEncodings
{
	NSMutableArray* encodings = [NSMutableArray array];
	for(NSString* str in [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsAvailableEncodingsKey])
		[encodings addObject:str];

	if(self.encoding && ![encodings containsObject:self.encoding])
		[encodings addObject:self.encoding];

	self.availableEncodings = encodings;
}

- (void)updateMenu
{
	NSString* currentEncodingsTitle = self.encoding;

	std::vector<menu_item_t> items;
	for(auto const& charset : encoding_list())
	{
		if([self.availableEncodings containsObject:[NSString stringWithCxxString:charset.code()]])
		{
			auto v = text::split(charset.name(), " – ");
			if(v.size() == 2)
			{
				items.push_back(menu_item_t(v.front(), v.back(), charset.code()));
				if(to_s(self.encoding) == charset.code())
					currentEncodingsTitle = to_ns(charset.name());
			}
		}
	}

	[self.menu removeAllItems];
	self.firstMenuItem = nil;

	if(items.size() < 10)
	{
		if(NSMenuItem* currentItem = PopulateMenuFlat(self.menu, items, self, @selector(selectEncoding:), to_s(self.encoding)))
			[self selectItem:currentItem];
	}
	else
	{
		if(currentEncodingsTitle)
		{
			self.firstMenuItem = [self.menu addItemWithTitle:currentEncodingsTitle action:NULL keyEquivalent:@""];
			[self.menu addItem:[NSMenuItem separatorItem]];
			[self selectItem:self.firstMenuItem];
		}
		PopulateMenuHierarchical(self.menu, items, self, @selector(selectEncoding:), to_s(self.encoding));
	}

	[self.menu addItem:[NSMenuItem separatorItem]];
	[[self.menu addItemWithTitle:@"Customize List…" action:@selector(customizeAvailableEncodings:) keyEquivalent:@""] setTarget:self];
}

- (id)initWithCoder:(NSCoder*)aCoder
{
	if(self = [super initWithCoder:aCoder])
	{
		self.encoding = @"UTF-8";
		[self updateAvailableEncodings];
		[self updateMenu];
		OakObserveUserDefaults(self);
	}
	return self;
}

- (id)initWithFrame:(NSRect)aRect pullsDown:(BOOL)flag
{
	if(self = [super initWithFrame:aRect pullsDown:flag])
	{
		self.encoding = @"UTF-8";
		[self updateAvailableEncodings];
		[self updateMenu];
		OakObserveUserDefaults(self);
	}
	return self;
}

- (id)init
{
	if(self = [self initWithFrame:NSZeroRect pullsDown:NO])
	{
		[self sizeToFit];
		if(NSWidth([self frame]) > 200)
			[self setFrameSize:NSMakeSize(200, NSHeight([self frame]))];
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (void)selectEncoding:(NSMenuItem*)sender
{
	self.encoding = [sender representedObject];
}

- (void)setEncoding:(NSString*)newEncoding
{
	if(_encoding == newEncoding || [_encoding isEqualToString:newEncoding])
		return;

	_encoding = newEncoding;
	if(_encoding && ![self.availableEncodings containsObject:_encoding])
		[self updateAvailableEncodings];
	[self updateMenu];

	if(NSDictionary* info = [self infoForBinding:@"encoding"])
	{
		id controller     = info[NSObservedObjectKey];
		NSString* keyPath = info[NSObservedKeyPathKey];
		if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
		{
			NSString* newValue = _encoding;

			NSString* oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqualToString:newValue])
				[controller setValue:newValue forKeyPath:keyPath];
		}
	}
}

- (void)setAvailableEncodings:(NSArray*)newEncodings
{
	if(_availableEncodings == newEncodings || [_availableEncodings isEqualToArray:newEncodings])
		return;

	_availableEncodings = newEncodings;
	[self updateMenu];
}

- (void)customizeAvailableEncodings:(id)sender
{
	[OakCustomizeEncodingsWindowController.sharedInstance showWindow:self];
	[self updateMenu];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	[self updateAvailableEncodings];
}
@end

// =========================================
// = Customize Encodings Window Controller =
// =========================================

@implementation OakCustomizeEncodingsWindowController
+ (instancetype)sharedInstance
{
	static OakCustomizeEncodingsWindowController* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if(self = [super initWithWindowNibName:@"CustomizeEncodings"])
	{
		std::set<std::string> enabledEncodings;
		for(NSString* encoding in [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsAvailableEncodingsKey])
			enabledEncodings.insert(to_s(encoding));

		encodings = [NSMutableArray new];
		for(auto const& charset : encoding_list())
		{
			bool enabled = enabledEncodings.find(charset.code()) != enabledEncodings.end();
			id item = [NSMutableDictionary dictionaryWithObjectsAndKeys:
				@(enabled), @"enabled",
				[NSString stringWithCxxString:charset.name()], @"name",
				[NSString stringWithCxxString:charset.code()], @"charset",
				nil];
			[encodings addObject:item];
		}
	}
	return self;
}

// ========================
// = NSTableView Delegate =
// ========================

- (BOOL)tableView:(NSTableView*)aTableView shouldEditTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [[aTableColumn identifier] isEqualToString:@"enabled"];
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [encodings count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [[encodings objectAtIndex:rowIndex] objectForKey:[aTableColumn identifier]];
}

- (void)tableView:(NSTableView*)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	[[encodings objectAtIndex:rowIndex] setObject:anObject forKey:[aTableColumn identifier]];

	NSMutableArray* newEncodings = [NSMutableArray array];
	for(NSDictionary* encoding in encodings)
	{
		if([[encoding objectForKey:@"enabled"] boolValue])
			[newEncodings addObject:[encoding objectForKey:@"charset"]];
	}

	[NSUserDefaults.standardUserDefaults setObject:newEncodings forKey:kUserDefaultsAvailableEncodingsKey];
}
@end
