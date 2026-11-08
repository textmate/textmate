# Formatter Auto-Detection & Preferences Pane

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Auto-detect installed formatters (prettier, swiftformat, clang-format, etc.) by file type and expose a Preferences pane where users can view detected formatters, toggle them on/off, and edit commands.

**Architecture:** A `FormatterRegistry` singleton (Obj-C++) holds a table of known formatters per file-type glob. On first access per file type, it searches PATH for the executable using `path::is_executable()` (same pattern as `scm/src/drivers/api.cc`). The Preferences pane (`FormattersPreferences`) displays the registry as an NSTableView. The existing `validateMenuItem:` and `lspFormatDocument:` code queries the registry as a fallback when no `formatCommand` is set in `.tm_properties`.

**Tech Stack:** Objective-C++, AppKit (NSTableView, NSGridView), `path::is_executable()`, NSUserDefaults for persistence, existing Preferences framework patterns.

---

## File Structure

| File | Responsibility |
|------|---------------|
| `Frameworks/Preferences/src/FormatterRegistry.h` | Singleton interface: known formatter table, detection, lookup |
| `Frameworks/Preferences/src/FormatterRegistry.mm` | Implementation: PATH search, caching, defaults persistence |
| `Frameworks/Preferences/src/FormattersPreferences.h` | Preference pane header |
| `Frameworks/Preferences/src/FormattersPreferences.mm` | Preference pane UI: table view of formatters per file type |
| `Frameworks/Preferences/src/Keys.h` | New `kUserDefaultsFormatterConfigurationsKey` constant |
| `Frameworks/Preferences/src/Keys.mm` | Register default value |
| `Frameworks/Preferences/src/Preferences.mm` | Add FormattersPreferences to pane array |
| `Frameworks/OakTextView/src/OakTextView.mm` | Query FormatterRegistry as fallback in validation + action |

---

## Chunk 1: FormatterRegistry

### Task 1: Create FormatterRegistry with known formatters table

**Files:**
- Create: `Frameworks/Preferences/src/FormatterRegistry.h`
- Create: `Frameworks/Preferences/src/FormatterRegistry.mm`

- [ ] **Step 1: Create FormatterRegistry.h**

```objc
#import <Foundation/Foundation.h>

@interface FormatterEntry : NSObject
@property (nonatomic, copy) NSString* fileTypeGlob;    // e.g. "*.swift", "*.{js,ts,jsx,tsx}"
@property (nonatomic, copy) NSString* formatterName;   // e.g. "swiftformat"
@property (nonatomic, copy) NSString* command;          // e.g. "swiftformat --stdinpath \"$TM_FILEPATH\""
@property (nonatomic, copy) NSString* executableName;   // e.g. "swiftformat" (binary to search in PATH)
@property (nonatomic, copy) NSString* detectedPath;     // e.g. "/opt/homebrew/bin/swiftformat" or nil
@property (nonatomic) BOOL enabled;
@end

@interface FormatterRegistry : NSObject
+ (instancetype)sharedInstance;

// Returns the format command for a file path, or nil if none detected/enabled
- (NSString*)formatCommandForPath:(NSString*)filePath;

// Returns all known formatter entries (for Preferences UI)
- (NSArray<FormatterEntry*>*)allEntries;

// Update an entry's enabled state or command (from Preferences UI)
- (void)setEnabled:(BOOL)enabled forEntryAtIndex:(NSUInteger)index;
- (void)setCommand:(NSString*)command forEntryAtIndex:(NSUInteger)index;

// Re-detect all executables in PATH
- (void)detectAll;
@end
```

- [ ] **Step 2: Create FormatterRegistry.mm with known formatters table**

```objc
#import "FormatterRegistry.h"
#import "Keys.h"
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <text/tokenize.h>

@implementation FormatterEntry
@end

static NSArray<FormatterEntry*>* DefaultFormatterTable ()
{
	struct { NSString* glob; NSString* name; NSString* cmd; NSString* exe; } const defaults[] = {
		{ @"*.swift",                    @"swiftformat",   @"swiftformat --stdinpath \"$TM_FILEPATH\"",           @"swiftformat"   },
		{ @"*.{js,ts,jsx,tsx,css,json}", @"prettier",      @"prettier --stdin-filepath \"$TM_FILEPATH\"",         @"prettier"      },
		{ @"*.{html,vue,svelte}",        @"prettier",      @"prettier --stdin-filepath \"$TM_FILEPATH\"",         @"prettier"      },
		{ @"*.php",                      @"prettier",      @"prettier --stdin-filepath \"$TM_FILEPATH\"",         @"prettier"      },
		{ @"*.py",                       @"black",          @"black -q -",                                         @"black"         },
		{ @"*.go",                       @"gofmt",          @"gofmt",                                              @"gofmt"         },
		{ @"*.rs",                       @"rustfmt",        @"rustfmt --edition 2021",                             @"rustfmt"       },
		{ @"*.{c,cc,cpp,h,hpp,m,mm}",   @"clang-format",   @"clang-format",                                       @"clang-format"  },
		{ @"*.rb",                       @"rubocop",        @"rubocop -A --stderr --stdin \"$TM_FILEPATH\"",       @"rubocop"       },
	};

	NSMutableArray* entries = [NSMutableArray array];
	for(auto const& d : defaults)
	{
		FormatterEntry* e = [[FormatterEntry alloc] init];
		e.fileTypeGlob    = d.glob;
		e.formatterName   = d.name;
		e.command          = d.cmd;
		e.executableName   = d.exe;
		e.enabled          = YES;
		[entries addObject:e];
	}
	return entries;
}

@interface FormatterRegistry ()
@property (nonatomic) NSMutableArray<FormatterEntry*>* entries;
@end

@implementation FormatterRegistry
+ (instancetype)sharedInstance
{
	static FormatterRegistry* instance = [self new];
	return instance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		[self loadFromDefaults];
		[self detectAll];
	}
	return self;
}

- (void)loadFromDefaults
{
	NSArray* saved = [NSUserDefaults.standardUserDefaults arrayForKey:kUserDefaultsFormatterConfigurationsKey];
	if(saved.count)
	{
		NSMutableArray* entries = [NSMutableArray array];
		for(NSDictionary* dict in saved)
		{
			FormatterEntry* e = [[FormatterEntry alloc] init];
			e.fileTypeGlob    = dict[@"glob"] ?: @"";
			e.formatterName   = dict[@"name"] ?: @"";
			e.command          = dict[@"command"] ?: @"";
			e.executableName   = dict[@"executable"] ?: @"";
			e.enabled          = [dict[@"enabled"] boolValue];
			[entries addObject:e];
		}
		_entries = entries;
	}
	else
	{
		_entries = [DefaultFormatterTable() mutableCopy];
	}
}

- (void)saveToDefaults
{
	NSMutableArray* array = [NSMutableArray array];
	for(FormatterEntry* e in _entries)
	{
		[array addObject:@{
			@"glob":        e.fileTypeGlob ?: @"",
			@"name":        e.formatterName ?: @"",
			@"command":     e.command ?: @"",
			@"executable":  e.executableName ?: @"",
			@"enabled":     @(e.enabled),
		}];
	}
	[NSUserDefaults.standardUserDefaults setObject:array forKey:kUserDefaultsFormatterConfigurationsKey];
}

- (void)detectAll
{
	std::vector<std::string> searchPaths;

	// Build search PATH matching the formatter execution environment
	std::string homePath = to_s(NSHomeDirectory());
	searchPaths.push_back("/opt/homebrew/bin");
	searchPaths.push_back("/usr/local/bin");
	searchPaths.push_back(homePath + "/.local/bin");

	if(char const* envPath = getenv("PATH"))
	{
		for(auto const& p : text::tokenize(envPath, envPath + strlen(envPath), ':'))
		{
			if(!p.empty())
				searchPaths.push_back(p);
		}
	}

	for(FormatterEntry* entry in _entries)
	{
		std::string exe = to_s(entry.executableName);
		entry.detectedPath = nil;

		for(auto const& dir : searchPaths)
		{
			std::string candidate = path::join(dir, exe);
			if(path::is_executable(candidate))
			{
				entry.detectedPath = [NSString stringWithCxxString:candidate];
				break;
			}
		}
	}
}

- (NSString*)formatCommandForPath:(NSString*)filePath
{
	if(!filePath)
		return nil;

	NSString* fileName = filePath.lastPathComponent;
	for(FormatterEntry* entry in _entries)
	{
		if(!entry.enabled || !entry.detectedPath)
			continue;

		if([self fileName:fileName matchesGlob:entry.fileTypeGlob])
			return entry.command;
	}
	return nil;
}

- (BOOL)fileName:(NSString*)fileName matchesGlob:(NSString*)glob
{
	// Handle brace expansion: "*.{js,ts,jsx}" → check each extension
	if([glob containsString:@"{"])
	{
		NSRange braceOpen = [glob rangeOfString:@"{"];
		NSRange braceClose = [glob rangeOfString:@"}" options:0 range:NSMakeRange(braceOpen.location, glob.length - braceOpen.location)];
		if(braceClose.location != NSNotFound)
		{
			NSString* prefix = [glob substringToIndex:braceOpen.location];
			NSString* suffix = [glob substringFromIndex:braceClose.location + 1];
			NSString* alternatives = [glob substringWithRange:NSMakeRange(braceOpen.location + 1, braceClose.location - braceOpen.location - 1)];
			for(NSString* alt in [alternatives componentsSeparatedByString:@","])
			{
				NSString* expandedGlob = [NSString stringWithFormat:@"%@%@%@", prefix, alt, suffix];
				if([self fileName:fileName matchesGlob:expandedGlob])
					return YES;
			}
			return NO;
		}
	}

	// Simple "*.ext" matching
	if([glob hasPrefix:@"*."])
	{
		NSString* ext = [glob substringFromIndex:2];
		return [[fileName pathExtension] caseInsensitiveCompare:ext] == NSOrderedSame;
	}

	return NO;
}

- (NSArray<FormatterEntry*>*)allEntries
{
	return [_entries copy];
}

- (void)setEnabled:(BOOL)enabled forEntryAtIndex:(NSUInteger)index
{
	if(index < _entries.count)
	{
		_entries[index].enabled = enabled;
		[self saveToDefaults];
	}
}

- (void)setCommand:(NSString*)command forEntryAtIndex:(NSUInteger)index
{
	if(index < _entries.count)
	{
		_entries[index].command = command;
		[self saveToDefaults];
	}
}
@end
```

- [ ] **Step 3: Build and verify**

Run: `make 2>&1 | tail -5`
Expected: Build succeeds (CMake glob picks up new .mm files automatically)

- [ ] **Step 4: Commit**

```bash
git add Frameworks/Preferences/src/FormatterRegistry.h Frameworks/Preferences/src/FormatterRegistry.mm
git commit -m "Add FormatterRegistry with known formatters table and PATH detection"
```

---

### Task 2: Add NSUserDefaults key for formatter configurations

**Files:**
- Modify: `Frameworks/Preferences/src/Keys.h:75` (before closing comment)
- Modify: `Frameworks/Preferences/src/Keys.mm` (add key definition + register default)

- [ ] **Step 1: Add key declaration to Keys.h**

Add after the `kUserDefaultsFolderSearchFollowLinksKey` line (before end of file):

```objc
// ==============
// = Formatters =
// ==============

extern NSString* const kUserDefaultsFormatterConfigurationsKey;
```

- [ ] **Step 2: Add key definition to Keys.mm**

Find the other key definitions and add:

```objc
NSString* const kUserDefaultsFormatterConfigurationsKey = @"formatters";
```

No need to register a default value — FormatterRegistry handles the empty case by loading DefaultFormatterTable().

- [ ] **Step 3: Build and verify**

Run: `make 2>&1 | tail -5`
Expected: Build succeeds

- [ ] **Step 4: Commit**

```bash
git add Frameworks/Preferences/src/Keys.h Frameworks/Preferences/src/Keys.mm
git commit -m "Add NSUserDefaults key for formatter configurations"
```

---

## Chunk 2: Preferences Pane & Integration

### Task 3: Create FormattersPreferences pane

**Files:**
- Create: `Frameworks/Preferences/src/FormattersPreferences.h`
- Create: `Frameworks/Preferences/src/FormattersPreferences.mm`

The pane follows the VariablesPreferences pattern: an NSTableView with columns for enabled (checkbox), file type glob, formatter name, command, and detected status.

- [ ] **Step 1: Create FormattersPreferences.h**

```objc
#import "Preferences.h"

@interface FormattersPreferences : NSViewController <PreferencesPaneProtocol>
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@end
```

- [ ] **Step 2: Create FormattersPreferences.mm**

```objc
#import "FormattersPreferences.h"
#import "FormatterRegistry.h"
#import "Keys.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

static NSString* const kColumnEnabled   = @"enabled";
static NSString* const kColumnGlob      = @"glob";
static NSString* const kColumnName      = @"name";
static NSString* const kColumnCommand   = @"command";
static NSString* const kColumnStatus    = @"status";

@interface FormattersPreferences () <NSTableViewDelegate, NSTableViewDataSource>
{
	NSTableView* _tableView;
}
@end

@implementation FormattersPreferences
- (NSImage*)toolbarItemImage { return [NSImage imageWithSystemSymbolName:@"hammer" accessibilityDescription:@"Formatters"]; }

- (id)init
{
	if(self = [self initWithNibName:nil bundle:nil])
	{
		self.identifier = @"Formatters";
		self.title      = @"Formatters";
	}
	return self;
}

- (NSTableColumn*)columnWithIdentifier:(NSUserInterfaceItemIdentifier)identifier title:(NSString*)title editable:(BOOL)editable width:(CGFloat)width resizingMask:(NSTableColumnResizingOptions)resizingMask
{
	NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier:identifier];
	col.title        = title;
	col.editable     = editable;
	col.width        = width;
	col.resizingMask = resizingMask;
	if(resizingMask == NSTableColumnNoResizing)
	{
		col.minWidth = width;
		col.maxWidth = width;
	}
	return col;
}

- (void)loadView
{
	NSTableColumn* enabledCol = [self columnWithIdentifier:kColumnEnabled title:@""          editable:YES width:20  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* globCol    = [self columnWithIdentifier:kColumnGlob    title:@"File Type" editable:NO  width:120 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* nameCol    = [self columnWithIdentifier:kColumnName    title:@"Formatter" editable:NO  width:100 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* commandCol = [self columnWithIdentifier:kColumnCommand title:@"Command"   editable:YES width:260 resizingMask:NSTableColumnAutoresizingMask];
	NSTableColumn* statusCol  = [self columnWithIdentifier:kColumnStatus  title:@"Status"    editable:NO  width:60  resizingMask:NSTableColumnNoResizing];

	NSButtonCell* checkboxCell = [[NSButtonCell alloc] init];
	checkboxCell.buttonType  = NSButtonTypeSwitch;
	checkboxCell.controlSize = NSControlSizeSmall;
	checkboxCell.title       = @"";
	enabledCol.dataCell = checkboxCell;

	_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	_tableView.allowsColumnReordering  = NO;
	_tableView.columnAutoresizingStyle = NSTableViewLastColumnOnlyAutoresizingStyle;
	_tableView.delegate                = self;
	_tableView.dataSource              = self;
	_tableView.usesAlternatingRowBackgroundColors = YES;

	for(NSTableColumn* col in @[ enabledCol, globCol, nameCol, commandCol, statusCol ])
		[_tableView addTableColumn:col];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSBezelBorder;
	scrollView.documentView          = _tableView;

	NSButton* detectButton = OakCreateButton(@"Re-detect");
	detectButton.target = self;
	detectButton.action = @selector(redetect:);

	NSTextField* infoLabel = OakCreateLabel(@"Formatters are used when no formatCommand is set in .tm_properties. Configure formatCommand there to override.");
	infoLabel.textColor = NSColor.secondaryLabelColor;
	[infoLabel setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSView* view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 622, 454)];

	NSDictionary* views = @{
		@"scrollView": scrollView,
		@"detect":     detectButton,
		@"info":       infoLabel,
	};

	OakAddAutoLayoutViewsToSuperview(views.allValues, view);

	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView(>=50)]-|" options:0 metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[info]-[detect]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView(>=50)]-8-[detect]-|" options:0 metrics:nil views:views]];

	self.view = view;
}

- (IBAction)redetect:(id)sender
{
	[[FormatterRegistry sharedInstance] detectAll];
	[_tableView reloadData];
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableView:(NSTableView*)tableView willDisplayCell:(id)cell forTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	FormatterEntry* entry = [FormatterRegistry sharedInstance].allEntries[row];
	if([col.identifier isEqualToString:kColumnStatus])
	{
		NSTextFieldCell* textCell = cell;
		textCell.textColor = entry.detectedPath ? NSColor.systemGreenColor : NSColor.secondaryLabelColor;
	}
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return [FormatterRegistry sharedInstance].allEntries.count;
}

- (id)tableView:(NSTableView*)tableView objectValueForTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	FormatterEntry* entry = [FormatterRegistry sharedInstance].allEntries[row];
	if([col.identifier isEqualToString:kColumnEnabled])
		return @(entry.enabled);
	else if([col.identifier isEqualToString:kColumnGlob])
		return entry.fileTypeGlob;
	else if([col.identifier isEqualToString:kColumnName])
		return entry.formatterName;
	else if([col.identifier isEqualToString:kColumnCommand])
		return entry.command;
	else if([col.identifier isEqualToString:kColumnStatus])
		return entry.detectedPath ? @"Found" : @"Not found";
	return nil;
}

- (void)tableView:(NSTableView*)tableView setObjectValue:(id)value forTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	if([col.identifier isEqualToString:kColumnEnabled])
		[[FormatterRegistry sharedInstance] setEnabled:[value boolValue] forEntryAtIndex:row];
	else if([col.identifier isEqualToString:kColumnCommand])
		[[FormatterRegistry sharedInstance] setCommand:value forEntryAtIndex:row];
}
@end
```

- [ ] **Step 3: Build and verify**

Run: `make 2>&1 | tail -5`
Expected: Build succeeds

- [ ] **Step 4: Commit**

```bash
git add Frameworks/Preferences/src/FormattersPreferences.h Frameworks/Preferences/src/FormattersPreferences.mm
git commit -m "Add Formatters preferences pane with table view"
```

---

### Task 4: Register the pane in Preferences.mm

**Files:**
- Modify: `Frameworks/Preferences/src/Preferences.mm:5` (add import)
- Modify: `Frameworks/Preferences/src/Preferences.mm:96-102` (add to pane array)

- [ ] **Step 1: Add import**

After the existing imports (line 6, after `#import "TerminalPreferences.h"`), add:

```objc
#import "FormattersPreferences.h"
```

- [ ] **Step 2: Add to viewControllers array**

In the `init` method (line 96-102), add `FormattersPreferences` to the array, before Terminal:

```objc
NSArray<NSViewController <PreferencesPaneProtocol>*>* viewControllers = @[
	[[FilesPreferences alloc] init],
	[[ProjectsPreferences alloc] init],
	[[BundlesPreferences alloc] init],
	[[VariablesPreferences alloc] init],
	[[FormattersPreferences alloc] init],
	[[TerminalPreferences alloc] init]
];
```

- [ ] **Step 3: Build and verify**

Run: `make 2>&1 | tail -5`
Expected: Build succeeds

- [ ] **Step 4: Manual test**

1. Launch TextMate
2. Open Preferences (Cmd+,)
3. Verify "Formatters" tab appears with hammer icon
4. Verify table shows all known formatters with correct detection status
5. Verify checkboxes toggle and changes persist after restarting Preferences

- [ ] **Step 5: Commit**

```bash
git add Frameworks/Preferences/src/Preferences.mm
git commit -m "Register Formatters pane in Preferences window"
```

---

### Task 5: Integrate FormatterRegistry as fallback in menu validation and format action

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:3341-3357` (validateMenuItem for lspFormatDocument)
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:5859-5900` (lspFormatDocument action)

The logic: if `formatCommand` from `.tm_properties` is empty AND no LSP formatting support, check `FormatterRegistry` for an auto-detected formatter. `.tm_properties` always takes priority.

- [ ] **Step 1: Add import at top of OakTextView.mm**

Add with the other Preferences imports (search for existing `#import` block):

```objc
#import <Preferences/FormatterRegistry.h>
```

- [ ] **Step 2: Update validateMenuItem for lspFormatDocument**

Replace the existing `lspFormatDocument:` validation block (lines 3341-3357) with:

```objc
else if([aMenuItem action] == @selector(lspFormatDocument:))
{
	OakDocument* doc = self.document;
	std::string filePath  = to_s(doc.path ?: @"");
	std::string fileType  = to_s(doc.fileType ?: @"");
	std::string directory = to_s(doc.directory ?: [doc.path stringByDeletingLastPathComponent] ?: @"");

	settings_t const settings = settings_for_path(filePath, fileType, directory);
	std::string formatCommand = settings.get(kSettingsFormatCommandKey, "");

	BOOL hasCustomFormatter = !formatCommand.empty();
	BOOL hasAutoFormatter   = !hasCustomFormatter && [[FormatterRegistry sharedInstance] formatCommandForPath:doc.path] != nil;
	BOOL hasRange = doc && [[LSPManager sharedManager] serverSupportsRangeFormattingForDocument:doc];
	BOOL hasDoc   = doc && [[LSPManager sharedManager] serverSupportsFormattingForDocument:doc];
	BOOL showSelection = !hasCustomFormatter && !hasAutoFormatter && [self hasSelection] && hasRange;
	[aMenuItem updateTitle:[NSString stringWithCxxString:format_string::replace(to_s(aMenuItem.title), "\\b(\\w+) / (Selection)\\b", showSelection ? "$2" : "$1")]];
	return hasCustomFormatter || hasAutoFormatter || showSelection || hasDoc;
}
```

- [ ] **Step 3: Update lspFormatDocument action**

In the `lspFormatDocument:` method (around line 5859), after the existing custom formatter check and before the LSP fallback, add the auto-detected formatter fallback. Find the block that checks `formatCommand.empty()` and runs `runCustomFormatter()`. After that block, before the LSP section, add:

```objc
// Auto-detected formatter fallback (when no formatCommand in .tm_properties)
if(formatCommand.empty())
{
	NSString* autoCommand = [[FormatterRegistry sharedInstance] formatCommandForPath:self.document.path];
	if(autoCommand)
		formatCommand = to_s(autoCommand);
}
```

This goes right after `formatCommand` is read from settings and before the `if(!formatCommand.empty())` block that calls `runCustomFormatter()`.

- [ ] **Step 4: Update format-on-save similarly**

In `documentWillSave:` (around line 1090), add the same fallback after reading `formatCommand` from settings:

```objc
if(formatCommand.empty())
{
	NSString* autoCommand = [[FormatterRegistry sharedInstance] formatCommandForPath:self.document.path];
	if(autoCommand)
		formatCommand = to_s(autoCommand);
}
```

Note: format-on-save still requires `formatOnSave = true` in `.tm_properties`. Auto-detected formatters only enable manual format by default. Users must opt into format-on-save explicitly.

- [ ] **Step 5: Build and verify**

Run: `make 2>&1 | tail -5`
Expected: Build succeeds

- [ ] **Step 6: Manual test**

1. Open a .swift file with NO `formatCommand` in `.tm_properties` and NO LSP
2. Verify Text → "Format Code" is enabled (if swiftformat is installed)
3. Invoke Format Code → file should be formatted
4. Add `formatCommand = "some-other-formatter"` to `.tm_properties`
5. Verify the `.tm_properties` formatter takes priority
6. Open Preferences → Formatters → disable swiftformat
7. Re-open the .swift file (no .tm_properties override) → Format Code should be greyed out

- [ ] **Step 7: Commit**

```bash
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Use auto-detected formatters as fallback when no formatCommand is configured"
```
