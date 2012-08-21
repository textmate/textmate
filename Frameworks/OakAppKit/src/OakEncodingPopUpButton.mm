#import "OakEncodingPopUpButton.h"
#import "NSMenu Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <ns/ns.h>
#import <text/parse.h>
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
			iterate(item, encodings)
			{
				std::string name, code;
				if(plist::get_key_path(*item, "name", name) && plist::get_key_path(*item, "code", code))
					res.push_back(charset_t(name, code));
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
		iterate(item, items)
		{
			NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:item->group + " – " + item->title] action:action keyEquivalent:@""];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:item->represented_object]];
			[menuItem setTarget:target];

			if(item->represented_object == selected)
				res = menuItem;
		}
		return res;
	}

	static void PopulateMenuHierarchical (NSMenu* containingMenu, std::vector<menu_item_t> const& items, id target, SEL action, std::string const& selected)
	{
		std::string groupName = NULL_STR;
		NSMenu* menu = nil;
		iterate(item, items)
		{
			if(groupName != item->group)
			{
				groupName = item->group;

				menu = [[NSMenu new] autorelease];
				[menu setAutoenablesItems:NO];
				[[containingMenu addItemWithTitle:[NSString stringWithCxxString:groupName] action:NULL keyEquivalent:@""] setSubmenu:menu];
			}

			NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:item->title] action:action keyEquivalent:@""];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:item->represented_object]];
			[menuItem setTarget:target];
			if(selected == item->represented_object)
				[menuItem setState:NSOnState];
		}
	}
}

@interface OakCustomizeEncodingsWindowController : NSWindowController
{
	NSMutableArray* encodings;
}
+ (OakCustomizeEncodingsWindowController*)sharedInstance;
@end

@interface OakEncodingPopUpButton ()
@property (nonatomic, retain) NSArray* availableEncodings;
@end

@implementation OakEncodingPopUpButton
@synthesize encoding, availableEncodings;

+ (void)initialize
{
	NSArray* encodings = @[ @"WINDOWS-1252", @"MACROMAN", @"ISO-8859-1", @"UTF-8", @"UTF-16LE", @"UTF-16BE", @"SHIFT_JIS", @"GB18030" ];
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ kUserDefaultsAvailableEncodingsKey : encodings }];
}

- (void)updateAvailableEncodings
{
	NSMutableArray* encodings = [NSMutableArray array];
	for(NSString* str in [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsAvailableEncodingsKey])
		[encodings addObject:str];

	if(encoding && ![encodings containsObject:encoding])
		[encodings addObject:encoding];

	self.availableEncodings = encodings;
}

- (void)updateMenu
{
	std::vector<menu_item_t> items;
	std::string currentEncodingsTitle = to_s(encoding);
	citerate(charset, encoding_list())
	{
		if([availableEncodings containsObject:[NSString stringWithCxxString:charset->code()]])
		{
			auto v = text::split(charset->name(), " – ");
			if(v.size() == 2)
			{
				items.push_back(menu_item_t(v.front(), v.back(), charset->code()));
				if(to_s(encoding) == charset->code())
					currentEncodingsTitle = charset->name();
			}
		}
	}

	[self.menu removeAllItems];
	firstMenuItem = nil;
	if(items.size() >= 10)
	{
		firstMenuItem = [self.menu addItemWithTitle:[NSString stringWithCxxString:currentEncodingsTitle] action:NULL keyEquivalent:@""];
		[self.menu addItem:[NSMenuItem separatorItem]];
		[self selectItem:firstMenuItem];
	}

	if(items.size() < 10)
			[self selectItem:PopulateMenuFlat(self.menu, items, self, @selector(selectEncoding:), to_s(encoding))];
	else	PopulateMenuHierarchical(self.menu, items, self, @selector(selectEncoding:), to_s(encoding));

	[self.menu addItem:[NSMenuItem separatorItem]];
	[[self.menu addItemWithTitle:@"Customize Encodings List…" action:@selector(customizeAvailableEncodings:) keyEquivalent:@""] setTarget:self];
}

- (id)initWithCoder:(NSCoder*)aCoder
{
	if(self = [super initWithCoder:aCoder])
	{
		encoding = [@"UTF-8" retain];
		[self updateAvailableEncodings];
		[self updateMenu];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (id)initWithFrame:(NSRect)aRect pullsDown:(BOOL)flag
{
	if(self = [super initWithFrame:aRect pullsDown:flag])
	{
		encoding = [@"UTF-8" retain];
		[self updateAvailableEncodings];
		[self updateMenu];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
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
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[encoding release];
	[availableEncodings release];
	[super dealloc];
}

- (void)selectEncoding:(NSMenuItem*)sender
{
	self.encoding = [sender representedObject];
}

- (void)setEncoding:(NSString*)newEncoding
{
	if(encoding == newEncoding || [encoding isEqualToString:newEncoding])
		return;

	[encoding release];
	encoding = [newEncoding retain];
	if(encoding && ![availableEncodings containsObject:encoding])
		[self updateAvailableEncodings];
	[self updateMenu];
}

- (void)setAvailableEncodings:(NSArray*)newEncodings
{
	if(availableEncodings == newEncodings || [availableEncodings isEqualToArray:newEncodings])
		return;

	[availableEncodings release];
	availableEncodings = [newEncodings retain];
	[self updateMenu];
}

- (void)customizeAvailableEncodings:(id)sender
{
	[[OakCustomizeEncodingsWindowController sharedInstance] showWindow:self];
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

static OakCustomizeEncodingsWindowController* SharedInstance;

@implementation OakCustomizeEncodingsWindowController
+ (OakCustomizeEncodingsWindowController*)sharedInstance
{
	return SharedInstance ?: [[OakCustomizeEncodingsWindowController new] autorelease];
}

- (id)init
{
	if(SharedInstance)
	{
		[self release];
	}
	else if(self = SharedInstance = [[super initWithWindowNibName:@"CustomizeEncodings"] retain])
	{
		std::set<std::string> enabledEncodings;
		for(NSString* encoding in [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsAvailableEncodingsKey])
			enabledEncodings.insert(to_s(encoding));

		encodings = [NSMutableArray new];
		citerate(charset, encoding_list())
		{
			bool enabled = enabledEncodings.find(charset->code()) != enabledEncodings.end();
			id item = [NSMutableDictionary dictionaryWithObjectsAndKeys:
				@(enabled), @"enabled",
				[NSString stringWithCxxString:charset->name()], @"name",
				[NSString stringWithCxxString:charset->code()], @"charset",
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

	[[NSUserDefaults standardUserDefaults] setObject:newEncodings forKey:kUserDefaultsAvailableEncodingsKey];
}
@end
