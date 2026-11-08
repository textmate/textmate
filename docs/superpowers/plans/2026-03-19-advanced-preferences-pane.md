# Advanced Preferences Pane Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an "Advanced" preferences pane that exposes all hidden `NSUserDefaults` settings with descriptive hints.

**Architecture:** New `AdvancedPreferences` class extending `PreferencesPane` (same pattern as FilesPreferences/ProjectsPreferences). Uses `defaultsProperties` KVO magic for automatic NSUserDefaults binding. Some keys currently defined as `static` in other frameworks need to be moved to `Keys.h/Keys.mm` as extern constants.

**Tech Stack:** Objective-C++, AppKit (NSGridView, Cocoa bindings), PreferencesPane KVO infrastructure

---

### Task 0: Declare missing keys in Keys.h / Keys.mm

**Files:**
- Modify: `Frameworks/Preferences/src/Keys.h`
- Modify: `Frameworks/Preferences/src/Keys.mm`
- Modify: `Frameworks/OakTextView/src/OakTextView.mm` (remove local defs — already imports `<Preferences/Keys.h>`)
- Modify: `Frameworks/OakTextView/src/OakDocumentView.mm` (remove static defs, add `#import <Preferences/Keys.h>`)
- Modify: `Frameworks/OakTabBarView/src/OakTabBarView.mm` (remove static defs, add `#import <Preferences/Keys.h>`)
- Modify: `Frameworks/OakTabBarView/CMakeLists.txt` (add Preferences header include path)
- Modify: `Frameworks/DocumentWindow/src/DocumentWindowController.mm` (remove static defs — already imports `<Preferences/Keys.h>`)
- Modify: `Frameworks/Find/src/Find.mm` (remove static def — already imports `<Preferences/Keys.h>`)
- Modify: `Frameworks/OakAppKit/src/OakPasteboard.mm` (remove defs, add `#import <Preferences/Keys.h>`)
- Modify: `Frameworks/OakAppKit/CMakeLists.txt` (add Preferences header include path)

Several keys are currently defined as `static` locals in their respective frameworks. They need to become `extern` in `Keys.h` and non-static in `Keys.mm` so the Preferences framework can reference them.

- [ ] **Step 1: Add new extern declarations to Keys.h**

Add under the existing `// = Appearance =` section and a new `// = Advanced =` section:

```objc
// ==============
// = Appearance =
// ==============

extern NSString* const kUserDefaultsDisableAntiAliasKey;
extern NSString* const kUserDefaultsLineNumbersKey;
extern NSString* const kUserDefaultsLineNumberScaleFactorKey;
extern NSString* const kUserDefaultsLineNumberFontNameKey;

// ============
// = Advanced =
// ============

// Editor
extern NSString* const kUserDefaultsDisableTypingPairsKey;
extern NSString* const kUserDefaultsFontSmoothingKey;
extern NSString* const kUserDefaultsHideStatusBarKey;

// Tabs
extern NSString* const kUserDefaultsTabItemMinWidthKey;
extern NSString* const kUserDefaultsTabItemMaxWidthKey;

// Clipboard
extern NSString* const kUserDefaultsDisablePersistentClipboardHistory;
extern NSString* const kUserDefaultsClipboardHistoryKeepAtLeast;
extern NSString* const kUserDefaultsClipboardHistoryKeepAtMost;
extern NSString* const kUserDefaultsClipboardHistoryDaysToKeep;

// Find
extern NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick;
extern NSString* const kUserDefaultsAlwaysFindInDocument;

// File Browser
extern NSString* const kUserDefaultsDisableFolderStateRestore;

// Bundles
extern NSString* const kUserDefaultsDisableBundleSuggestionsKey;
extern NSString* const kUserDefaultsGrammarsToNeverSuggestKey;
```

Note: `kUserDefaultsScrollPastEndKey` is NOT included — it's already toggled via View menu and doesn't need to be in the Advanced pane. It stays as a local in `OakTextView.mm`. Similarly, `kUserDefaultsShowFavoritesInsteadOfUntitledKey` and `kUserDefaultsFileBrowserOpenAnimationDisabled` are already in Keys.h.

- [ ] **Step 2: Add definitions to Keys.mm**

Add corresponding `NSString* const` definitions under a new `// = Advanced =` section:

```objc
// ============
// = Advanced =
// ============

// Editor
NSString* const kUserDefaultsDisableTypingPairsKey              = @"disableTypingPairs";
NSString* const kUserDefaultsFontSmoothingKey                   = @"fontSmoothing";
NSString* const kUserDefaultsHideStatusBarKey                   = @"hideStatusBar";

// Appearance
NSString* const kUserDefaultsLineNumberScaleFactorKey           = @"lineNumberScaleFactor";
NSString* const kUserDefaultsLineNumberFontNameKey              = @"lineNumberFontName";

// Tabs
NSString* const kUserDefaultsTabItemMinWidthKey                 = @"tabItemMinWidth";
NSString* const kUserDefaultsTabItemMaxWidthKey                 = @"tabItemMaxWidth";

// Clipboard
NSString* const kUserDefaultsDisablePersistentClipboardHistory  = @"disablePersistentClipboardHistory";
NSString* const kUserDefaultsClipboardHistoryKeepAtLeast        = @"clipboardHistoryKeepAtLeast";
NSString* const kUserDefaultsClipboardHistoryKeepAtMost         = @"clipboardHistoryKeepAtMost";
NSString* const kUserDefaultsClipboardHistoryDaysToKeep         = @"clipboardHistoryDaysToKeep";

// Find
NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick     = @"keepSearchResultsOnDoubleClick";
NSString* const kUserDefaultsAlwaysFindInDocument               = @"alwaysFindInDocument";

// File Browser
NSString* const kUserDefaultsDisableFolderStateRestore          = @"disableFolderStateRestore";

// Bundles
NSString* const kUserDefaultsDisableBundleSuggestionsKey        = @"disableBundleSuggestions";
NSString* const kUserDefaultsGrammarsToNeverSuggestKey          = @"grammarsToNeverSuggest";
```

- [ ] **Step 3: Remove duplicate definitions from source frameworks**

Each file below already has a local definition. Remove it. Add `#import <Preferences/Keys.h>` where it's missing.

Files that **already import** `<Preferences/Keys.h>` (just remove the local defs):
- `Frameworks/OakTextView/src/OakTextView.mm`: Remove `NSString* const kUserDefaultsFontSmoothingKey` and `kUserDefaultsDisableTypingPairsKey` (lines 59-60). Keep `kUserDefaultsScrollPastEndKey` (line 61) — it stays local.
- `Frameworks/DocumentWindow/src/DocumentWindowController.mm`: Remove `static NSString* const` for `kUserDefaultsAlwaysFindInDocument`, `kUserDefaultsDisableFolderStateRestore`, `kUserDefaultsHideStatusBarKey`, `kUserDefaultsDisableBundleSuggestionsKey`, `kUserDefaultsGrammarsToNeverSuggestKey` (lines 39-43)
- `Frameworks/Find/src/Find.mm`: Remove `static NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick` (line 34)

Files that **need `#import <Preferences/Keys.h>` added**:
- `Frameworks/OakTextView/src/OakDocumentView.mm`: Add import, remove `static NSString* const kUserDefaultsLineNumberScaleFactorKey` and `kUserDefaultsLineNumberFontNameKey` (lines 24-25)
- `Frameworks/OakTabBarView/src/OakTabBarView.mm`: Add import, remove `static NSString* kUserDefaultsTabItemMinWidthKey` and `kUserDefaultsTabItemMaxWidthKey` (lines 11-12). Note: these lacked `const` — the centralized version adds it.
- `Frameworks/OakAppKit/src/OakPasteboard.mm`: Add import, remove the 4 `NSString* const` clipboard history definitions (lines 24-27)

- [ ] **Step 4: Add CMake header include paths for OakTabBarView and OakAppKit**

These frameworks don't link Preferences but need to see `<Preferences/Keys.h>`. Add header-only dependency (same pattern used in `Preferences/CMakeLists.txt` for OakTabBarView).

In `Frameworks/OakTabBarView/CMakeLists.txt`, add:
```cmake
target_include_directories(OakTabBarView PRIVATE
  $<TARGET_PROPERTY:Preferences,INTERFACE_INCLUDE_DIRECTORIES>)
```

In `Frameworks/OakAppKit/CMakeLists.txt`, add:
```cmake
target_include_directories(OakAppKit PRIVATE
  $<TARGET_PROPERTY:Preferences,INTERFACE_INCLUDE_DIRECTORIES>)
```

OakDocumentView.mm is in the OakTextView framework which already links Preferences — no CMake change needed.

- [ ] **Step 5: Register defaults for keys that don't have them in `default_settings()`**

In `Keys.mm`, update `default_settings()` to include defaults for newly-centralized keys:

```objc
kUserDefaultsFontSmoothingKey:                    @(3), // OTVFontSmoothingDisabledForDarkHiDPI
kUserDefaultsLineNumberScaleFactorKey:            @(0.8),
kUserDefaultsTabItemMinWidthKey:                  @(120),
kUserDefaultsTabItemMaxWidthKey:                  @(250),
kUserDefaultsClipboardHistoryKeepAtLeast:         @(25),
kUserDefaultsClipboardHistoryKeepAtMost:          @(500),
kUserDefaultsClipboardHistoryDaysToKeep:          @(30),
```

These values match the existing hardcoded defaults in:
- `OakTextView.mm:2194` — fontSmoothing = 3
- `OakDocumentView.mm:216` — lineNumberScaleFactor fallback = 0.8
- `OakTabBarView.mm:688-689` — tabItemMinWidth = 120, tabItemMaxWidth = 250
- `OakPasteboard.mm:289-291` — clipboard history limits

Remove the now-duplicate `registerDefaults:` calls from:
- `OakPasteboard.mm` `+initialize` method (lines 288-292)
- `OakTabBarView.mm` constructor (lines 687-690)
- `OakTextView.mm` `+initialize` — remove only the `kUserDefaultsFontSmoothingKey` default registration (line 2194). Keep `kUserDefaultsWrapColumnPresetsKey`.

- [ ] **Step 6: Build and verify no duplicate symbol or linker errors**

Run: `make`

Expected: Clean build, no duplicate symbol errors, no unresolved symbol errors.

- [ ] **Step 7: Commit**

```
git add Frameworks/Preferences/src/Keys.h Frameworks/Preferences/src/Keys.mm Frameworks/OakTextView/src/OakTextView.mm Frameworks/OakTextView/src/OakDocumentView.mm Frameworks/OakTabBarView/src/OakTabBarView.mm Frameworks/OakTabBarView/CMakeLists.txt Frameworks/DocumentWindow/src/DocumentWindowController.mm Frameworks/Find/src/Find.mm Frameworks/OakAppKit/src/OakPasteboard.mm Frameworks/OakAppKit/CMakeLists.txt
git commit -m "Move hidden user defaults keys to centralized Keys.h/Keys.mm"
```

---

### Task 1: Create AdvancedPreferences header

**Files:**
- Create: `Frameworks/Preferences/src/AdvancedPreferences.h`

- [ ] **Step 1: Create the header file**

```objc
#import "PreferencesPane.h"

@interface AdvancedPreferences : PreferencesPane
@end
```

- [ ] **Step 2: Commit**

```
git add Frameworks/Preferences/src/AdvancedPreferences.h
git commit -m "Add AdvancedPreferences header"
```

---

### Task 2: Implement AdvancedPreferences pane

**Files:**
- Create: `Frameworks/Preferences/src/AdvancedPreferences.mm`

Uses the `PreferencesPane` pattern with `defaultsProperties` for KVO binding. Sections separated by `OakSetupGridViewWithSeparators`. Each setting has a hint label below it in small font.

Grid row count (for separator indices):
- Rows 0-9: Editor (5 controls + 5 hints = 10 rows)
- Row 10: separator
- Rows 11-18: Appearance (4 controls + 4 hints = 8 rows)
- Row 19: separator
- Rows 20-27: Clipboard (4 controls + 4 hints = 8 rows)
- Row 28: separator
- Rows 29-32: Find (2 controls + 2 hints = 4 rows)
- Row 33: separator
- Rows 34-37: File Browser (2 controls + 2 hints = 4 rows)
- Row 38: separator
- Rows 39-42: Bundles (2 controls + 2 hints = 4 rows)

Separator indices: `{ 10, 19, 28, 33, 38 }`

- [ ] **Step 1: Create AdvancedPreferences.mm**

```objc
#import "AdvancedPreferences.h"
#import "Keys.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <MenuBuilder/MenuBuilder.h>

@implementation AdvancedPreferences
- (id)init
{
	if(self = [super initWithNibName:nil label:@"Advanced" image:[NSImage imageWithSystemSymbolName:@"gearshape.2" accessibilityDescription:@"Advanced"]])
	{
		self.defaultsProperties = @{
			// Editor
			@"disableTypingPairs":              kUserDefaultsDisableTypingPairsKey,
			@"disableAntiAlias":                kUserDefaultsDisableAntiAliasKey,
			@"fontSmoothing":                   kUserDefaultsFontSmoothingKey,
			@"hideStatusBar":                   kUserDefaultsHideStatusBarKey,
			@"showFavoritesInsteadOfUntitled":  kUserDefaultsShowFavoritesInsteadOfUntitledKey,

			// Appearance
			@"lineNumberFontName":              kUserDefaultsLineNumberFontNameKey,
			@"lineNumberScaleFactor":           kUserDefaultsLineNumberScaleFactorKey,
			@"tabItemMinWidth":                 kUserDefaultsTabItemMinWidthKey,
			@"tabItemMaxWidth":                 kUserDefaultsTabItemMaxWidthKey,

			// Clipboard
			@"disablePersistentClipboardHistory": kUserDefaultsDisablePersistentClipboardHistory,
			@"clipboardHistoryKeepAtLeast":     kUserDefaultsClipboardHistoryKeepAtLeast,
			@"clipboardHistoryKeepAtMost":      kUserDefaultsClipboardHistoryKeepAtMost,
			@"clipboardHistoryDaysToKeep":      kUserDefaultsClipboardHistoryDaysToKeep,

			// Find
			@"keepSearchResultsOnDoubleClick":  kUserDefaultsKeepSearchResultsOnDoubleClick,
			@"alwaysFindInDocument":            kUserDefaultsAlwaysFindInDocument,

			// File Browser
			@"fileBrowserOpenAnimationDisabled": kUserDefaultsFileBrowserOpenAnimationDisabled,
			@"disableFolderStateRestore":       kUserDefaultsDisableFolderStateRestore,

			// Bundles
			@"disableBundleSuggestions":        kUserDefaultsDisableBundleSuggestionsKey,
		};
	}
	return self;
}

- (NSString*)grammarsToNeverSuggest
{
	NSArray* arr = [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsGrammarsToNeverSuggestKey];
	return [arr componentsJoinedByString:@", "];
}

- (void)setGrammarsToNeverSuggest:(NSString*)value
{
	NSArray* components = [value componentsSeparatedByString:@","];
	NSMutableArray* trimmed = [NSMutableArray array];
	for(NSString* s in components)
	{
		NSString* t = [s stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceCharacterSet];
		if(t.length)
			[trimmed addObject:t];
	}
	[NSUserDefaults.standardUserDefaults setObject:trimmed forKey:kUserDefaultsGrammarsToNeverSuggestKey];
}

- (void)loadView
{
	NSFont* hintFont = [NSFont systemFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	NSColor* hintColor = NSColor.secondaryLabelColor;

	// === Editor ===
	NSButton* disableTypingPairsCheckBox = OakCreateCheckBox(@"Disable typing pairs");
	NSButton* disableAntiAliasCheckBox   = OakCreateCheckBox(@"Disable text anti-aliasing");
	NSPopUpButton* fontSmoothingPopUp    = OakCreatePopUpButton();
	NSButton* hideStatusBarCheckBox      = OakCreateCheckBox(@"Hide status bar");
	NSButton* showFavoritesCheckBox      = OakCreateCheckBox(@"Show favorites instead of untitled");

	MBMenu const fontSmoothingItems = {
		{ @"Disabled",                        .tag = 0 },
		{ @"Enabled",                         .tag = 1 },
		{ @"Disabled for dark themes",        .tag = 2 },
		{ @"Disabled for dark themes on HiDPI (default)", .tag = 3 },
	};
	MBCreateMenu(fontSmoothingItems, fontSmoothingPopUp.menu);

	// === Appearance ===
	NSTextField* lineNumberFontNameField   = [NSTextField textFieldWithString:@""];
	NSTextField* lineNumberScaleField      = [NSTextField textFieldWithString:@""];
	NSTextField* tabMinWidthField          = [NSTextField textFieldWithString:@""];
	NSTextField* tabMaxWidthField          = [NSTextField textFieldWithString:@""];

	lineNumberFontNameField.placeholderString = @"Uses editor font when blank";
	lineNumberScaleField.placeholderString    = @"0.8";
	tabMinWidthField.placeholderString        = @"120";
	tabMaxWidthField.placeholderString        = @"250";

	// === Clipboard ===
	NSButton* disableClipboardPersistenceCheckBox = OakCreateCheckBox(@"Disable persistent clipboard history");
	NSTextField* keepAtLeastField  = [NSTextField textFieldWithString:@""];
	NSTextField* keepAtMostField   = [NSTextField textFieldWithString:@""];
	NSTextField* daysToKeepField   = [NSTextField textFieldWithString:@""];

	keepAtLeastField.placeholderString = @"25";
	keepAtMostField.placeholderString  = @"500";
	daysToKeepField.placeholderString  = @"30";

	// === Find ===
	NSButton* keepSearchResultsCheckBox    = OakCreateCheckBox(@"Keep search results on double-click");
	NSButton* alwaysFindInDocumentCheckBox = OakCreateCheckBox(@"Always find in document");

	// === File Browser ===
	NSButton* disableOpenAnimationCheckBox      = OakCreateCheckBox(@"Disable open animation");
	NSButton* disableFolderStateRestoreCheckBox = OakCreateCheckBox(@"Disable folder state restore");

	// === Bundles ===
	NSButton* disableBundleSuggestionsCheckBox = OakCreateCheckBox(@"Disable bundle suggestions");
	NSTextField* grammarsToNeverSuggestField   = [NSTextField textFieldWithString:@""];
	grammarsToNeverSuggestField.placeholderString = @"Comma-separated grammar UUIDs";

	// === Hint labels ===
	NSTextField* (^makeHint)(NSString*) = ^NSTextField* (NSString* text) {
		NSTextField* label = OakCreateLabel(text, hintFont);
		label.textColor = hintColor;
		label.lineBreakMode = NSLineBreakByWordWrapping;
		label.maximumNumberOfLines = 2;
		return label;
	};

	// === Grid Layout ===
	// Row indices:
	// 0-9: Editor (5 controls + 5 hints)
	// 10: separator
	// 11-18: Appearance (4 controls + 4 hints)
	// 19: separator
	// 20-27: Clipboard (4 controls + 4 hints)
	// 28: separator
	// 29-32: Find (2 controls + 2 hints)
	// 33: separator
	// 34-37: File Browser (2 controls + 2 hints)
	// 38: separator
	// 39-42: Bundles (2 controls + 2 hints)
	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		// Editor section (rows 0-9)
		@[ OakCreateLabel(@"Editor:"),              disableTypingPairsCheckBox               ],  // 0
		@[ NSGridCell.emptyContentView,             makeHint(@"Stops auto-closing of brackets, quotes, and other paired characters") ],  // 1
		@[ NSGridCell.emptyContentView,             disableAntiAliasCheckBox                 ],  // 2
		@[ NSGridCell.emptyContentView,             makeHint(@"Disables text anti-aliasing for a sharper, aliased look") ],  // 3
		@[ OakCreateLabel(@"Font smoothing:"),      fontSmoothingPopUp                       ],  // 4
		@[ NSGridCell.emptyContentView,             makeHint(@"Controls subpixel font smoothing behavior by theme and display type") ],  // 5
		@[ NSGridCell.emptyContentView,             hideStatusBarCheckBox                    ],  // 6
		@[ NSGridCell.emptyContentView,             makeHint(@"Hides the status bar at the bottom of the editor window") ],  // 7
		@[ NSGridCell.emptyContentView,             showFavoritesCheckBox                    ],  // 8
		@[ NSGridCell.emptyContentView,             makeHint(@"Shows the favorites dialog instead of an empty document at startup") ],  // 9

		@[ ], // separator (row 10)

		// Appearance section (rows 11-18)
		@[ OakCreateLabel(@"Line number font:"),    lineNumberFontNameField                  ],  // 11
		@[ NSGridCell.emptyContentView,             makeHint(@"PostScript font name for the gutter (e.g. Menlo-Regular)") ],  // 12
		@[ OakCreateLabel(@"Line number scale:"),   lineNumberScaleField                     ],  // 13
		@[ NSGridCell.emptyContentView,             makeHint(@"Scale factor relative to editor font size (default 0.8)") ],  // 14
		@[ OakCreateLabel(@"Min tab width:"),       tabMinWidthField                         ],  // 15
		@[ NSGridCell.emptyContentView,             makeHint(@"Minimum pixel width for document tabs (default 120)") ],  // 16
		@[ OakCreateLabel(@"Max tab width:"),       tabMaxWidthField                         ],  // 17
		@[ NSGridCell.emptyContentView,             makeHint(@"Maximum pixel width for document tabs (default 250)") ],  // 18

		@[ ], // separator (row 19)

		// Clipboard section (rows 20-27)
		@[ OakCreateLabel(@"Clipboard:"),           disableClipboardPersistenceCheckBox      ],  // 20
		@[ NSGridCell.emptyContentView,             makeHint(@"Uses in-memory database only; clipboard history is lost on quit") ],  // 21
		@[ OakCreateLabel(@"Keep at least:"),       keepAtLeastField                         ],  // 22
		@[ NSGridCell.emptyContentView,             makeHint(@"Minimum number of clipboard entries to retain (default 25)") ],  // 23
		@[ OakCreateLabel(@"Keep at most:"),        keepAtMostField                          ],  // 24
		@[ NSGridCell.emptyContentView,             makeHint(@"Maximum clipboard entries before pruning (default 500)") ],  // 25
		@[ OakCreateLabel(@"Days to keep:"),        daysToKeepField                          ],  // 26
		@[ NSGridCell.emptyContentView,             makeHint(@"Entries older than this are pruned (default 30)") ],  // 27

		@[ ], // separator (row 28)

		// Find section (rows 29-32)
		@[ OakCreateLabel(@"Find:"),                keepSearchResultsCheckBox                ],  // 29
		@[ NSGridCell.emptyContentView,             makeHint(@"Keeps the Find in Folder results window open after double-clicking a match") ],  // 30
		@[ NSGridCell.emptyContentView,             alwaysFindInDocumentCheckBox             ],  // 31
		@[ NSGridCell.emptyContentView,             makeHint(@"Find always searches the full document, even when text is selected") ],  // 32

		@[ ], // separator (row 33)

		// File Browser section (rows 34-37)
		@[ OakCreateLabel(@"File browser:"),        disableOpenAnimationCheckBox             ],  // 34
		@[ NSGridCell.emptyContentView,             makeHint(@"Disables the expand/collapse animation in the file browser") ],  // 35
		@[ NSGridCell.emptyContentView,             disableFolderStateRestoreCheckBox        ],  // 36
		@[ NSGridCell.emptyContentView,             makeHint(@"Stops restoring expanded/collapsed folder state when reopening projects") ],  // 37

		@[ ], // separator (row 38)

		// Bundles section (rows 39-42)
		@[ OakCreateLabel(@"Bundles:"),             disableBundleSuggestionsCheckBox         ],  // 39
		@[ NSGridCell.emptyContentView,             makeHint(@"Stops suggesting bundle installation for unrecognized file types") ],  // 40
		@[ OakCreateLabel(@"Never suggest for:"),   grammarsToNeverSuggestField              ],  // 41
		@[ NSGridCell.emptyContentView,             makeHint(@"Comma-separated list of grammar UUIDs to exclude from suggestions") ],  // 42
	]];

	// Constrain text fields to consistent width
	for(NSTextField* field in @[ lineNumberFontNameField, lineNumberScaleField, tabMinWidthField, tabMaxWidthField, keepAtLeastField, keepAtMostField, daysToKeepField, grammarsToNeverSuggestField ])
		[field.widthAnchor constraintEqualToConstant:360].active = YES;

	self.view = OakSetupGridViewWithSeparators(gridView, { 10, 19, 28, 33, 38 });

	// === Bindings ===

	// Editor
	[disableTypingPairsCheckBox bind:NSValueBinding       toObject:self withKeyPath:@"disableTypingPairs"             options:nil];
	[disableAntiAliasCheckBox   bind:NSValueBinding       toObject:self withKeyPath:@"disableAntiAlias"               options:nil];
	[fontSmoothingPopUp         bind:NSSelectedTagBinding toObject:self withKeyPath:@"fontSmoothing"                  options:nil];
	[hideStatusBarCheckBox      bind:NSValueBinding       toObject:self withKeyPath:@"hideStatusBar"                  options:nil];
	[showFavoritesCheckBox      bind:NSValueBinding       toObject:self withKeyPath:@"showFavoritesInsteadOfUntitled" options:nil];

	// Appearance
	[lineNumberFontNameField bind:NSValueBinding toObject:self withKeyPath:@"lineNumberFontName"    options:@{ NSNullPlaceholderBindingOption: @"" }];
	[lineNumberScaleField    bind:NSValueBinding toObject:self withKeyPath:@"lineNumberScaleFactor" options:@{ NSNullPlaceholderBindingOption: @"" }];
	[tabMinWidthField        bind:NSValueBinding toObject:self withKeyPath:@"tabItemMinWidth"       options:@{ NSNullPlaceholderBindingOption: @"" }];
	[tabMaxWidthField        bind:NSValueBinding toObject:self withKeyPath:@"tabItemMaxWidth"       options:@{ NSNullPlaceholderBindingOption: @"" }];

	// Clipboard
	[disableClipboardPersistenceCheckBox bind:NSValueBinding toObject:self withKeyPath:@"disablePersistentClipboardHistory" options:nil];
	[keepAtLeastField bind:NSValueBinding toObject:self withKeyPath:@"clipboardHistoryKeepAtLeast" options:@{ NSNullPlaceholderBindingOption: @"" }];
	[keepAtMostField  bind:NSValueBinding toObject:self withKeyPath:@"clipboardHistoryKeepAtMost"  options:@{ NSNullPlaceholderBindingOption: @"" }];
	[daysToKeepField  bind:NSValueBinding toObject:self withKeyPath:@"clipboardHistoryDaysToKeep"  options:@{ NSNullPlaceholderBindingOption: @"" }];

	// Find
	[keepSearchResultsCheckBox    bind:NSValueBinding toObject:self withKeyPath:@"keepSearchResultsOnDoubleClick" options:nil];
	[alwaysFindInDocumentCheckBox bind:NSValueBinding toObject:self withKeyPath:@"alwaysFindInDocument"           options:nil];

	// File Browser
	[disableOpenAnimationCheckBox       bind:NSValueBinding toObject:self withKeyPath:@"fileBrowserOpenAnimationDisabled" options:nil];
	[disableFolderStateRestoreCheckBox  bind:NSValueBinding toObject:self withKeyPath:@"disableFolderStateRestore"        options:nil];

	// Bundles
	[disableBundleSuggestionsCheckBox bind:NSValueBinding toObject:self withKeyPath:@"disableBundleSuggestions"   options:nil];
	[grammarsToNeverSuggestField      bind:NSValueBinding toObject:self withKeyPath:@"grammarsToNeverSuggest"    options:@{ NSNullPlaceholderBindingOption: @"" }];
}
@end
```

- [ ] **Step 2: Commit**

```
git add Frameworks/Preferences/src/AdvancedPreferences.mm
git commit -m "Add Advanced preferences pane with hints"
```

---

### Task 3: Register the pane in Preferences.mm

**Files:**
- Modify: `Frameworks/Preferences/src/Preferences.mm`

- [ ] **Step 1: Add import and register the pane**

Add `#import "AdvancedPreferences.h"` to the imports, and add `[[AdvancedPreferences alloc] init]` to the viewControllers array after TerminalPreferences:

```objc
#import "AdvancedPreferences.h"

// In -[Preferences init]:
NSArray<NSViewController <PreferencesPaneProtocol>*>* viewControllers = @[
	[[FilesPreferences alloc] init],
	[[ProjectsPreferences alloc] init],
	[[BundlesPreferences alloc] init],
	[[VariablesPreferences alloc] init],
	[[FormattersPreferences alloc] init],
	[[TerminalPreferences alloc] init],
	[[AdvancedPreferences alloc] init]
];
```

- [ ] **Step 2: Commit**

```
git add Frameworks/Preferences/src/Preferences.mm
git commit -m "Register Advanced pane in preferences window"
```

---

### Task 4: Build and verify

**Files:** None (testing only)

- [ ] **Step 1: Build**

Run: `make`

Expected: Clean compile, no warnings from new code, no duplicate symbols.

- [ ] **Step 2: Launch and verify**

Run: `make run`

Open Preferences (Cmd+,). Verify:
1. "Advanced" tab appears as the last item with a gear icon
2. All 6 sections render with proper separators
3. Checkboxes toggle correctly (verify with `defaults read com.macromates.TextMate-dev`)
4. Text fields accept input and persist across relaunch
5. Font smoothing popup shows all 4 options and persists selection
6. Hint text appears below each control in small gray font
7. Changing clipboard history limits takes effect after restart
8. Grammar UUIDs field round-trips correctly (array ↔ comma-separated string)
