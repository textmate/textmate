# Projects Preferences Pane Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a new "Projects" preference pane with per-project `.tm_properties` management and dedicated LSP configuration UI.

**Architecture:** Hybrid AppKit shell (Obj-C++ NSSplitView + NSTableView sidebar) with SwiftUI content panel (via OakSwiftUI bridge). Data layer uses a new `tm_properties_editor_t` C++ class for per-file `.tm_properties` I/O, plus NSUserDefaults fallback.

**Tech Stack:** Objective-C++, Swift/SwiftUI, C++20, AppKit, KVDB, settings framework

**Spec:** `docs/superpowers/specs/2026-03-19-projects-preferences-pane-design.md`

---

## File Structure

```
Frameworks/settings/src/
├── tm_properties_editor.h        # NEW — per-file .tm_properties read/write
└── tm_properties_editor.cc       # NEW — implementation using ini_file_t parser

Frameworks/settings/tests/
└── t_tm_properties_editor.cc     # NEW — tests for editor class

Frameworks/Preferences/src/
├── ProjectsPreferencesV2.h       # NEW — pane header
└── ProjectsPreferencesV2.mm      # NEW — AppKit shell: NSSplitView, project list, bridge

Frameworks/OakSwiftUI/Sources/OakSwiftUI/
├── Bridge/
│   ├── OakPropertyEntry.swift            # NEW — bridge data type
│   ├── OakLSPStatus.swift                # NEW — LSP status bridge type
│   ├── ProjectSettingsBridge.swift        # NEW — @objc bridge controller
│   └── ProjectSettingsBridgeDelegate.swift # NEW — delegate protocol
└── ProjectSettings/
    ├── ProjectSettingsViewModel.swift     # NEW — ObservableObject + property catalog
    ├── ProjectSettingsView.swift          # NEW — root view with TabView + scope selector
    ├── LSPSettingsTab.swift               # NEW — LSP form UI
    ├── EditorSettingsTab.swift            # NEW — editor grid
    ├── FilesSettingsTab.swift             # NEW — files grid
    ├── AllPropertiesTab.swift             # NEW — full table with source
    └── PropertyRowView.swift             # NEW — reusable typed row (toggle/text/number)
```

**Modified files:**
- `Frameworks/settings/CMakeLists.txt` — new .cc sources auto-discovered by glob
- `Frameworks/Preferences/src/Preferences.mm:98-106` — replace `ProjectsPreferences` with `ProjectsPreferencesV2`
- `Frameworks/Preferences/src/AdvancedPreferences.mm` — absorb 12 NSUserDefaults settings from old ProjectsPreferences
- `Frameworks/lsp/src/LSPManager.h/.mm` — add `statusForScope:projectPath:` method

---

### Task 0: Migrate old ProjectsPreferences settings to Advanced pane

**Files:**
- Modify: `Frameworks/Preferences/src/AdvancedPreferences.mm`
- Modify: `Frameworks/Preferences/src/Preferences.mm:98-106`
- Reference: `Frameworks/Preferences/src/ProjectsPreferences.mm` (read-only, for settings list)
- Reference: `Frameworks/Preferences/src/Keys.h` (verify keys exist)

This task is independent. The old `ProjectsPreferences` pane has 12 NSUserDefaults settings (file browser, document tabs, HTML output) that move to the Advanced pane. The 3 `.tm_properties` bindings (exclude, include, binary) will be handled by the new Projects pane later.

- [ ] **Step 1: Read ProjectsPreferences.mm** — identify all 12 NSUserDefaults bindings and their keys. Verify every key already exists in `Keys.h`/`Keys.mm`.

- [ ] **Step 2: Add settings to AdvancedPreferences.mm** — add a new "File Browser" section at the end of `loadView` with these controls:

```objc
// File Browser section
NSPopUpButton* fileBrowserPositionPopUp = OakCreatePopUpButton();
NSButton* foldersOnTopCheckBox = OakCreateCheckBox(@"Folders on top");
NSButton* showLinksAsExpandableCheckBox = OakCreateCheckBox(@"Show links as expandable");
NSButton* openFilesOnSingleClickCheckBox = OakCreateCheckBox(@"Open files on single click");
NSButton* keepCurrentDocSelectedCheckBox = OakCreateCheckBox(@"Keep current document selected");
NSButton* adjustWindowCheckBox = OakCreateCheckBox(@"Adjust window when toggling display");

// Document Tabs section
NSButton* showForSingleDocCheckBox = OakCreateCheckBox(@"Show for single document");
NSButton* reOrderOnOpenCheckBox = OakCreateCheckBox(@"Re-order when opening a file");
NSButton* autoCloseUnusedCheckBox = OakCreateCheckBox(@"Automatically close unused tabs");

// HTML Output section
NSPopUpButton* htmlOutputPopUp = OakCreatePopUpButton();
```

Add to `defaultsProperties` dict:
```objc
@"fileBrowserPlacement":         kUserDefaultsFileBrowserPlacementKey,
@"foldersOnTop":                 kUserDefaultsFoldersOnTopKey,
@"showFileExtensions":           kUserDefaultsShowFileExtensionsKey,
@"allowExpandingLinks":          kUserDefaultsAllowExpandingLinksKey,
@"fileBrowserSingleClickToOpen": kUserDefaultsFileBrowserSingleClickToOpenKey,
@"autoRevealFile":               kUserDefaultsAutoRevealFileKey,
@"disableAutoResize":            kUserDefaultsDisableFileBrowserWindowResizeKey,
@"disableTabBarCollapsing":      kUserDefaultsDisableTabBarCollapsingKey,
@"disableTabReordering":         kUserDefaultsDisableTabReorderingKey,
@"disableTabAutoClose":          kUserDefaultsDisableTabAutoCloseKey,
@"htmlOutputPlacement":          kUserDefaultsHTMLOutputPlacementKey,
```

Add `OakStringListTransformer` registrations in `init` (copy from ProjectsPreferences.mm lines 22-23):
```objc
[OakStringListTransformer createTransformerWithName:@"OakFileBrowserPlacementSettingsTransformer" andObjectsArray:@[ @"left", @"right" ]];
[OakStringListTransformer createTransformerWithName:@"OakHTMLOutputPlacementSettingsTransformer" andObjectsArray:@[ @"bottom", @"right", @"window" ]];
```

Also add a checkbox for "Show file extensions" (`kUserDefaultsShowFileExtensionsKey`).

Add popup menus, grid rows, bindings following the existing pattern in AdvancedPreferences. Use the registered transformers for placement popup bindings. Add hint labels for each setting.

- [ ] **Step 3: Remove ProjectsPreferences from Preferences.mm** — in the `viewControllers` array (~line 100), replace `[[ProjectsPreferences alloc] init]` with a placeholder comment. Don't add ProjectsPreferencesV2 yet (that's Task 7). Remove the `#import "ProjectsPreferences.h"` at the top.

- [ ] **Step 4: Build and verify** — Run `make` to ensure it compiles. The old Projects pane should be gone from the toolbar, and the new settings should appear at the bottom of the Advanced pane.

- [ ] **Step 5: Commit**
```
git add Frameworks/Preferences/src/AdvancedPreferences.mm Frameworks/Preferences/src/Preferences.mm
git commit -m "Migrate ProjectsPreferences settings to Advanced pane"
```

---

### Task 1: Create tm_properties_editor_t (C++ data layer)

**Files:**
- Create: `Frameworks/settings/src/tm_properties_editor.h`
- Create: `Frameworks/settings/src/tm_properties_editor.cc`
- Create: `Frameworks/settings/tests/t_tm_properties_editor.cc`
- Reference: `Frameworks/settings/src/parser.h` (ini_file_t struct)
- Reference: `Frameworks/settings/src/settings.cc:359-475` (serialization logic to replicate)

- [ ] **Step 1: Write the test file** `Frameworks/settings/tests/t_tm_properties_editor.cc`:

```cpp
#include <settings/tm_properties_editor.h>
#include <test/jail.h>

void test_editor_read_empty ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	OAK_ASSERT_EQ(editor.sections().size(), 0);
}

void test_editor_set_unscoped ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	editor.set("tabSize", "4");
	editor.set("softTabs", "true");
	editor.save();

	// Re-read and verify
	tm_properties_editor_t editor2(path);
	auto sections = editor2.sections();
	OAK_ASSERT_EQ(sections.size(), 1);  // one section: top-level (empty key)
	OAK_ASSERT_EQ(sections[""]["tabSize"], "4");
	OAK_ASSERT_EQ(sections[""]["softTabs"], "true");
}

void test_editor_set_scoped ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	editor.set("tabSize", "4", "*.py");
	editor.set("softTabs", "true", "*.py");
	editor.set("tabSize", "2", "*.rb");
	editor.save();

	tm_properties_editor_t editor2(path);
	auto sections = editor2.sections();
	OAK_ASSERT_EQ(sections["*.py"]["tabSize"], "4");
	OAK_ASSERT_EQ(sections["*.py"]["softTabs"], "true");
	OAK_ASSERT_EQ(sections["*.rb"]["tabSize"], "2");
}

void test_editor_unset ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	editor.set("tabSize", "4");
	editor.set("softTabs", "true");
	editor.save();

	tm_properties_editor_t editor2(path);
	editor2.unset("softTabs");
	editor2.save();

	tm_properties_editor_t editor3(path);
	auto sections = editor3.sections();
	OAK_ASSERT_EQ(sections[""]["tabSize"], "4");
	OAK_ASSERT(sections[""].find("softTabs") == sections[""].end());
}

void test_editor_add_remove_section ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	editor.set("tabSize", "4", "*.py");
	editor.set("tabSize", "2", "*.rb");
	editor.save();

	tm_properties_editor_t editor2(path);
	editor2.remove_section("*.rb");
	editor2.save();

	tm_properties_editor_t editor3(path);
	auto sections = editor3.sections();
	OAK_ASSERT_EQ(sections.count("*.py"), (size_t)1);
	OAK_ASSERT_EQ(sections.count("*.rb"), (size_t)0);
}

void test_editor_read_existing_file ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	// Write a .tm_properties file manually
	if(FILE* fp = fopen(path.c_str(), "w"))
	{
		fprintf(fp, "tabSize = 3\nsoftTabs = false\n\n[ *.py ]\ntabSize = 4\nsoftTabs = true\n\n[ source.ruby ]\ntabSize = 2\n");
		fclose(fp);
	}

	tm_properties_editor_t editor(path);
	auto sections = editor.sections();
	OAK_ASSERT_EQ(sections[""]["tabSize"], "3");
	OAK_ASSERT_EQ(sections[""]["softTabs"], "false");
	OAK_ASSERT_EQ(sections["*.py"]["tabSize"], "4");
	OAK_ASSERT_EQ(sections["*.py"]["softTabs"], "true");
	OAK_ASSERT_EQ(sections["source.ruby"]["tabSize"], "2");
}

void test_editor_preserves_order ()
{
	test::jail_t jail;
	std::string path = jail.path("test.tm_properties");

	tm_properties_editor_t editor(path);
	editor.set("encoding", "UTF-8");
	editor.set("tabSize", "4", "source.python");
	editor.set("tabSize", "2", "*.rb");
	editor.set("lspCommand", "clangd", "*.cpp");
	editor.save();

	// Read raw file and check ordering: top-level first, then scope selectors, then globs
	std::string content = path::content(path);
	size_t pos_encoding = content.find("encoding");
	size_t pos_source   = content.find("source.python");
	size_t pos_rb       = content.find("*.rb");
	size_t pos_cpp      = content.find("*.cpp");

	OAK_ASSERT_LT(pos_encoding, pos_source);  // top-level before scope
	OAK_ASSERT_LT(pos_source, pos_rb);        // scope before glob
	OAK_ASSERT_LT(pos_rb, pos_cpp);           // shorter glob before longer
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd build-debug && ctest --output-on-failure -R settings`
Expected: compilation errors — `tm_properties_editor.h` not found.

- [ ] **Step 3: Write tm_properties_editor.h**

Create `Frameworks/settings/src/tm_properties_editor.h`:

```cpp
#ifndef TM_PROPERTIES_EDITOR_H_ABCDEF
#define TM_PROPERTIES_EDITOR_H_ABCDEF

#include <oak/oak.h>
#include <map>
#include <string>
#include <vector>

struct tm_properties_editor_t
{
	tm_properties_editor_t (std::string const& path);

	std::map<std::string, std::map<std::string, std::string>> sections () const;

	void set (std::string const& key, std::string const& value, std::string const& section = "");
	void unset (std::string const& key, std::string const& section = "");

	void add_section (std::string const& section);
	void remove_section (std::string const& section);

	void save () const;

	std::vector<std::string> section_names () const;

private:
	std::string _path;
	std::map<std::string, std::map<std::string, std::string>> _sections;
};

#endif
```

- [ ] **Step 4: Write tm_properties_editor.cc**

Create `Frameworks/settings/src/tm_properties_editor.cc`:

```cpp
#include "tm_properties_editor.h"
#include "parser.h"
#include <io/path.h>
#include <text/text.h>
#include <scope/scope.h>

static bool is_scope_selector (std::string const& str)
{
	// Scope selectors don't start with * or / and contain dots or single words
	if(str.empty() || str.front() == '*' || str.front() == '/' || str.front() == '"')
		return false;
	return str.find('.') != std::string::npos || (str.find('*') == std::string::npos && str.find('/') == std::string::npos);
}

tm_properties_editor_t::tm_properties_editor_t (std::string const& path) : _path(path)
{
	std::string content = path::content(_path);
	if(content == NULL_STR)
		return;

	ini_file_t iniFile(_path);
	parse_ini(content.data(), content.data() + content.size(), iniFile);

	for(auto const& section : iniFile.sections)
	{
		// Section names: empty vector = top-level, otherwise joined by " ; "
		std::string sectionName;
		if(!section.names.empty())
			sectionName = text::join(section.names, " ; ");

		for(auto const& value : section.values)
			_sections[sectionName][value.name] = value.value;
	}
}

std::map<std::string, std::map<std::string, std::string>> tm_properties_editor_t::sections () const
{
	return _sections;
}

std::vector<std::string> tm_properties_editor_t::section_names () const
{
	std::vector<std::string> names;
	for(auto const& pair : _sections)
		names.push_back(pair.first);
	return names;
}

void tm_properties_editor_t::set (std::string const& key, std::string const& value, std::string const& section)
{
	_sections[section][key] = value;
}

void tm_properties_editor_t::unset (std::string const& key, std::string const& section)
{
	auto it = _sections.find(section);
	if(it != _sections.end())
	{
		it->second.erase(key);
		if(it->second.empty())
			_sections.erase(it);
	}
}

void tm_properties_editor_t::add_section (std::string const& section)
{
	_sections[section]; // creates empty map if not exists
}

void tm_properties_editor_t::remove_section (std::string const& section)
{
	_sections.erase(section);
}

void tm_properties_editor_t::save () const
{
	// Replicate ordering logic from settings_t::set() in settings.cc
	struct ordered_section_t
	{
		ordered_section_t (std::string const& title) : title(title)
		{
			_is_top_level      = title.empty();
			_is_scope_selector = !_is_top_level && is_scope_selector(title);
			_is_wildcard       = !_is_top_level && !title.empty() && title.front() == '*';
		}

		bool operator< (ordered_section_t const& rhs) const
		{
			if((_is_top_level && rhs._is_top_level) || (_is_scope_selector && rhs._is_scope_selector))
				return title < rhs.title;
			else if(_is_top_level || rhs._is_scope_selector)
				return true;
			else if(rhs._is_top_level || _is_scope_selector)
				return false;

			size_t lhsSize = title.size() - (_is_wildcard ? 1 : 0);
			size_t rhsSize = rhs.title.size() - (rhs._is_wildcard ? 1 : 0);
			return lhsSize == rhsSize ? title < rhs.title : lhsSize < rhsSize;
		}

		std::string title;
		std::vector<std::pair<std::string, std::string>> assignments;

	private:
		bool _is_scope_selector;
		bool _is_wildcard;
		bool _is_top_level;
	};

	std::set<ordered_section_t> ordered_sections;
	for(auto const& section : _sections)
	{
		ordered_section_t tmp(section.first);
		for(auto const& pair : section.second)
			tmp.assignments.emplace_back(pair.first, pair.second);
		if(!tmp.assignments.empty())
			ordered_sections.insert(tmp);
	}

	if(FILE* fp = fopen(_path.c_str(), "w"))
	{
		bool firstSection = true;
		for(auto const& section : ordered_sections)
		{
			if(!firstSection)
				fprintf(fp, "\n");
			firstSection = false;

			if(!section.title.empty())
				fprintf(fp, "[ %s ]\n", section.title.c_str());

			for(auto const& assignment : section.assignments)
			{
				// Pad key to align = signs (use max 24 char width)
				int padding = std::max(1, 25 - (int)assignment.first.size());
				fprintf(fp, "%s%*s= %s\n", assignment.first.c_str(), padding, " ", assignment.second.c_str());
			}
		}
		fclose(fp);
	}
}
```

- [ ] **Step 5: Run tests**

Run: `cd build-debug && cmake .. -G Ninja && ninja && ctest --output-on-failure -R settings`
Expected: All `t_tm_properties_editor` tests pass.

- [ ] **Step 6: Commit**
```
git add Frameworks/settings/src/tm_properties_editor.h Frameworks/settings/src/tm_properties_editor.cc Frameworks/settings/tests/t_tm_properties_editor.cc
git commit -m "Add tm_properties_editor_t for per-file .tm_properties I/O"
```

---

### Task 2: Add LSPManager scope-based status query

**Files:**
- Modify: `Frameworks/lsp/src/LSPManager.h`
- Modify: `Frameworks/lsp/src/LSPManager.mm`
- Modify: `Frameworks/lsp/src/LSPClient.h` (add `processIdentifier` readonly property)
- Modify: `Frameworks/lsp/src/LSPClient.mm` (implement `processIdentifier`)

Currently LSPManager only queries by OakDocument. The preferences pane needs to query by scope/path without having an open document.

**Link dependency note:** Preferences is a static lib linked into the TextMate app, which also links lsp. So `#import <lsp/LSPManager.h>` from ProjectsPreferencesV2.mm resolves at app link time. No need to add lsp to the Preferences CMakeLists.

**LSPClient PID access:** `_task` is a private ivar on LSPClient. Add a readonly property `processIdentifier` that returns `_task.isRunning ? _task.processIdentifier : 0`.

- [ ] **Step 1: Add method declaration to LSPManager.h**

After the existing `hasClientForDocument:` method, add:

```objc
- (NSDictionary*)lspStatusForFileType:(NSString*)fileType projectPath:(NSString*)projectPath;
```

Returns a dictionary with keys: `@"isRunning"` (NSNumber bool), `@"pid"` (NSNumber), `@"serverName"` (NSString), `@"capabilities"` (NSArray of NSString), `@"error"` (NSString or nil).

- [ ] **Step 2: Implement in LSPManager.mm**

Add the implementation. It needs to:
1. Call `settings_for_path()` with a dummy path matching the file type to get `lspCommand`
2. Check if any existing client matches that command + project root
3. Return status dict

```objc
- (NSDictionary*)lspStatusForFileType:(NSString*)fileType projectPath:(NSString*)projectPath
{
	if(!fileType || !projectPath)
		return nil;

	// Build a dummy path to look up settings
	std::string dummyPath = to_s(projectPath) + "/dummy." + to_s(fileType);
	settings_t settings = settings_for_path(dummyPath, "", to_s(projectPath));
	std::string lspCommand = settings.get(kSettingsLSPCommandKey, "");
	if(lspCommand.empty())
		return @{ @"isRunning": @NO };

	// Check running clients for matching workspace root
	for(NSString* rootPath in _clients)
	{
		if([rootPath hasPrefix:projectPath])
		{
			LSPClient* client = _clients[rootPath];
			NSMutableArray* caps = [NSMutableArray array];
			if(client.completionResolveProvider) [caps addObject:@"completion"];
			if(client.documentFormattingProvider) [caps addObject:@"formatting"];
			if(client.documentRangeFormattingProvider) [caps addObject:@"rangeFormatting"];
			if(client.renameProvider) [caps addObject:@"rename"];
			if(client.codeActionProvider) [caps addObject:@"codeAction"];
			// hover and definition are always available if server initialized

			return @{
				@"isRunning": @(client.initialized),
				@"pid": @(client.task.processIdentifier),
				@"serverName": [NSString stringWithFormat:@"%s", lspCommand.c_str()],
				@"capabilities": caps,
			};
		}
	}

	return @{ @"isRunning": @NO, @"serverName": [NSString stringWithUTF8String:lspCommand.c_str()] };
}
```

Note: The actual implementation will need adjustment based on exact LSPClient properties — read `LSPClient.h` to get the correct property names. The `lspCommand` key may be a raw string not in `keys.h`; check `LSPManager.mm:193` where it uses `settings.get("lspCommand", "")`.

- [ ] **Step 3: Build and verify**

Run: `make`
Expected: Compiles without errors.

- [ ] **Step 4: Commit**
```
git add Frameworks/lsp/src/LSPManager.h Frameworks/lsp/src/LSPManager.mm
git commit -m "Add LSPManager scope-based status query for preferences pane"
```

---

### Task 3: Create OakSwiftUI bridge types

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakPropertyEntry.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakLSPStatus.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/ProjectSettingsBridgeDelegate.swift`

These are pure data types with no dependencies — they can be written and compiled first.

- [ ] **Step 1: Create OakPropertyEntry.swift**

```swift
import AppKit

@MainActor @objc public class OakPropertyEntry: NSObject, Identifiable {
	@objc public let key: String
	@objc public dynamic var value: String
	@objc public let defaultValue: String
	@objc public let source: String         // "project", "global", "defaults", "userDefaults"
	@objc public let category: String       // "lsp", "editor", "files", "browser", "formatting", "project"
	@objc public let propertyType: String   // "bool", "int", "string", "pattern", "json"
	@objc public dynamic var isModified: Bool

	public var id: String { key }

	@objc public init(key: String, value: String, defaultValue: String, source: String, category: String, propertyType: String, isModified: Bool) {
		self.key = key
		self.value = value
		self.defaultValue = defaultValue
		self.source = source
		self.category = category
		self.propertyType = propertyType
		self.isModified = isModified
		super.init()
	}
}
```

- [ ] **Step 2: Create OakLSPStatus.swift**

```swift
import AppKit

@MainActor @objc public class OakLSPStatus: NSObject {
	@objc public let isRunning: Bool
	@objc public let pid: Int
	@objc public let serverName: String
	@objc public let errorMessage: String?
	@objc public let capabilities: [String]

	@objc public init(isRunning: Bool, pid: Int, serverName: String, errorMessage: String?, capabilities: [String]) {
		self.isRunning = isRunning
		self.pid = pid
		self.serverName = serverName
		self.errorMessage = errorMessage
		self.capabilities = capabilities
		super.init()
	}
}
```

- [ ] **Step 3: Create ProjectSettingsBridgeDelegate.swift**

```swift
import AppKit

@MainActor @objc public protocol ProjectSettingsBridgeDelegate: AnyObject {
	func settingsBridge(_ bridge: ProjectSettingsBridge, didChangeProperty key: String, value: String, scope: String)
	func settingsBridge(_ bridge: ProjectSettingsBridge, didUnsetProperty key: String, scope: String)
	func settingsBridge(_ bridge: ProjectSettingsBridge, didAddScope scope: String)
	func settingsBridge(_ bridge: ProjectSettingsBridge, didRemoveScope scope: String)
	func settingsBridgeDidRequestScopeList(_ bridge: ProjectSettingsBridge) -> [String]
	func settingsBridge(_ bridge: ProjectSettingsBridge, lspStatusForScope scope: String) -> OakLSPStatus?
}
```

- [ ] **Step 4: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && ./build.sh`
Expected: Compiles. The `@objc` declarations become available in the generated Swift header.

- [ ] **Step 5: Commit**
```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakPropertyEntry.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakLSPStatus.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/ProjectSettingsBridgeDelegate.swift
git commit -m "Add OakSwiftUI bridge types for project settings"
```

---

### Task 4: Create ProjectSettingsViewModel and property catalog

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/ProjectSettingsViewModel.swift`

The ViewModel holds all state for the SwiftUI side: selected tab, selected scope, property list, filter text, and the "show modified only" toggle.

- [ ] **Step 1: Create ProjectSettingsViewModel.swift**

```swift
import SwiftUI
import Combine

public enum SettingsTab: Int, CaseIterable {
	case lsp, editor, files, all

	public var title: String {
		switch self {
		case .lsp:    return "LSP"
		case .editor: return "Editor"
		case .files:  return "Files"
		case .all:    return "All"
		}
	}
}

@MainActor
public class ProjectSettingsViewModel: ObservableObject {
	@Published public var selectedTab: SettingsTab = .lsp
	@Published public var selectedScope: String = ""  // empty = unscoped
	@Published public var availableScopes: [String] = []
	@Published public var properties: [OakPropertyEntry] = []
	@Published public var filterText: String = ""
	@Published public var showModifiedOnly: Bool = false
	@Published public var lspStatus: OakLSPStatus?

	public weak var delegate: ProjectSettingsBridgeDelegate?
	public weak var bridge: ProjectSettingsBridge?

	// Property catalog — defines categories, types, defaults
	public static let propertyCatalog: [(key: String, category: String, type: String, defaultValue: String, label: String)] = [
		// LSP
		("lspCommand",     "lsp",    "string", "",     "Server command"),
		("lspEnabled",     "lsp",    "bool",   "true", "Enabled"),
		("lspRootPath",    "lsp",    "string", "",     "Root path"),
		("lspInitOptions", "lsp",    "json",   "",     "Init options"),
		// Editor
		("tabSize",          "editor", "int",    "4",     "Tab size"),
		("softTabs",         "editor", "bool",   "false", "Soft tabs"),
		("softWrap",         "editor", "bool",   "false", "Soft wrap"),
		("wrapColumn",       "editor", "int",    "80",    "Wrap column"),
		("showWrapColumn",   "editor", "bool",   "false", "Show wrap column"),
		("showIndentGuides", "editor", "bool",   "false", "Show indent guides"),
		("showInvisibles",   "editor", "bool",   "false", "Show invisibles"),
		("invisiblesMap",    "editor", "string", "",      "Invisibles map"),
		("fontName",         "editor", "string", "",      "Font name"),
		("fontSize",         "editor", "int",    "12",    "Font size"),
		("theme",            "editor", "string", "",      "Theme"),
		("spellChecking",    "editor", "bool",   "false", "Spell checking"),
		("spellingLanguage", "editor", "string", "",      "Spelling language"),
		// Files
		("encoding",                  "files", "string",  "UTF-8",  "Encoding"),
		("lineEndings",               "files", "string",  "\\n",    "Line endings"),
		("saveOnBlur",                "files", "bool",    "false",  "Save on blur"),
		("atomicSave",                "files", "bool",    "true",   "Atomic save"),
		("storeEncodingPerFile",      "files", "bool",    "false",  "Store encoding per file"),
		("disableExtendedAttributes", "files", "bool",    "false",  "Disable extended attributes"),
		("formatCommand",             "files", "string",  "",       "Format command"),
		("formatOnSave",              "files", "bool",    "false",  "Format on save"),
		("fileType",                  "files", "string",  "",       "File type"),
		("binary",                    "files", "pattern", "",       "Binary pattern"),
		// Browser
		("exclude",                      "browser", "pattern", "", "Exclude"),
		("include",                      "browser", "pattern", "", "Include"),
		("excludeInBrowser",             "browser", "pattern", "", "Exclude in browser"),
		("includeInBrowser",             "browser", "pattern", "", "Include in browser"),
		("excludeInFileChooser",         "browser", "pattern", "", "Exclude in file chooser"),
		("includeInFileChooser",         "browser", "pattern", "", "Include in file chooser"),
		("excludeInFolderSearch",        "browser", "pattern", "", "Exclude in folder search"),
		("excludeDirectories",           "browser", "pattern", "", "Exclude directories"),
		("excludeDirectoriesInBrowser",  "browser", "pattern", "", "Exclude dirs in browser"),
		("excludeFiles",                 "browser", "pattern", "", "Exclude files"),
		("excludeFilesInBrowser",        "browser", "pattern", "", "Exclude files in browser"),
		("followSymbolicLinks",          "browser", "bool",    "true", "Follow symbolic links"),
		("excludeSCMDeleted",            "browser", "bool",    "false", "Exclude SCM deleted"),
		// Project
		("projectDirectory", "project", "string", "", "Project directory"),
		("windowTitle",      "project", "string", "", "Window title"),
		("tabTitle",         "project", "string", "", "Tab title"),
		("scopeAttributes",  "project", "string", "", "Scope attributes"),
		("relatedFilePath",  "project", "string", "", "Related file path"),
	]

	public var filteredProperties: [OakPropertyEntry] {
		var result = properties
		if showModifiedOnly {
			result = result.filter { $0.isModified }
		}
		if !filterText.isEmpty {
			let filter = filterText.lowercased()
			result = result.filter { $0.key.lowercased().contains(filter) }
		}
		return result
	}

	public func propertiesForCategory(_ category: String) -> [OakPropertyEntry] {
		properties.filter { $0.category == category }
	}

	public func updateProperty(key: String, value: String) {
		if let entry = properties.first(where: { $0.key == key }) {
			entry.value = value
			entry.isModified = true
		}
		if let bridge = bridge {
			delegate?.settingsBridge(bridge, didChangeProperty: key, value: value, scope: selectedScope)
		}
	}

	public func unsetProperty(key: String) {
		if let entry = properties.first(where: { $0.key == key }) {
			entry.value = entry.defaultValue
			entry.isModified = false
		}
		if let bridge = bridge {
			delegate?.settingsBridge(bridge, didUnsetProperty: key, scope: selectedScope)
		}
	}

	public func addScope(_ scope: String) {
		if !availableScopes.contains(scope) {
			availableScopes.append(scope)
		}
		selectedScope = scope
		if let bridge = bridge {
			delegate?.settingsBridge(bridge, didAddScope: scope)
		}
	}

	public func removeScope(_ scope: String) {
		availableScopes.removeAll { $0 == scope }
		selectedScope = ""
		if let bridge = bridge {
			delegate?.settingsBridge(bridge, didRemoveScope: scope)
		}
	}

	public func refreshLSPStatus() {
		if let bridge = bridge {
			lspStatus = delegate?.settingsBridge(bridge, lspStatusForScope: selectedScope)
		}
	}
}
```

- [ ] **Step 2: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && ./build.sh`
Expected: Compiles.

- [ ] **Step 3: Commit**
```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/ProjectSettingsViewModel.swift
git commit -m "Add ProjectSettingsViewModel with property catalog"
```

---

### Task 5: Create SwiftUI tab views

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/PropertyRowView.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/LSPSettingsTab.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/EditorSettingsTab.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/FilesSettingsTab.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/AllPropertiesTab.swift`

- [ ] **Step 1: Create PropertyRowView.swift** — reusable row component for typed property editing:

```swift
import SwiftUI

struct PropertyRowView: View {
	let entry: OakPropertyEntry
	let onChanged: (String) -> Void
	let onUnset: () -> Void

	var body: some View {
		HStack(spacing: 8) {
			Text(entry.key)
				.frame(width: 140, alignment: .trailing)
				.foregroundColor(entry.isModified ? .primary : .secondary)
				.font(.system(size: 12))

			Group {
				switch entry.propertyType {
				case "bool":
					Toggle("", isOn: Binding(
						get: { entry.value == "true" },
						set: { onChanged($0 ? "true" : "false") }
					))
					.labelsHidden()
					.toggleStyle(.switch)
					.controlSize(.small)
				case "int":
					TextField("", text: Binding(
						get: { entry.value },
						set: { onChanged($0) }
					))
					.textFieldStyle(.roundedBorder)
					.frame(width: 60)
					.font(.system(size: 11, design: .monospaced))
				case "pattern", "json":
					TextField(entry.defaultValue.isEmpty ? "—" : entry.defaultValue, text: Binding(
						get: { entry.value },
						set: { onChanged($0) }
					))
					.textFieldStyle(.roundedBorder)
					.font(.system(size: 11, design: .monospaced))
				default: // "string"
					TextField(entry.defaultValue.isEmpty ? "—" : entry.defaultValue, text: Binding(
						get: { entry.value },
						set: { onChanged($0) }
					))
					.textFieldStyle(.roundedBorder)
					.font(.system(size: 11))
				}
			}
			.frame(maxWidth: .infinity, alignment: .leading)

			if entry.isModified {
				Button(action: onUnset) {
					Image(systemName: "arrow.uturn.backward.circle")
						.foregroundColor(.secondary)
				}
				.buttonStyle(.plain)
				.help("Reset to default")
			}
		}
		.padding(.vertical, 2)
	}
}
```

- [ ] **Step 2: Create LSPSettingsTab.swift**

```swift
import SwiftUI

struct LSPSettingsTab: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel

	var body: some View {
		VStack(alignment: .leading, spacing: 12) {
			ForEach(viewModel.propertiesForCategory("lsp")) { entry in
				PropertyRowView(
					entry: entry,
					onChanged: { viewModel.updateProperty(key: entry.key, value: $0) },
					onUnset: { viewModel.unsetProperty(key: entry.key) }
				)
			}

			Divider()

			// LSP Server Status
			VStack(alignment: .leading, spacing: 6) {
				Text("Server Status")
					.font(.system(size: 11, weight: .semibold))
					.foregroundColor(.secondary)

				if let status = viewModel.lspStatus {
					HStack(spacing: 6) {
						Circle()
							.fill(status.isRunning ? Color.green : Color.red)
							.frame(width: 8, height: 8)
						Text(status.isRunning ? "Running" : "Stopped")
							.font(.system(size: 11))
						if status.pid > 0 {
							Text("PID \(status.pid)")
								.font(.system(size: 10))
								.foregroundColor(.secondary)
						}
					}

					if !status.serverName.isEmpty {
						Text(status.serverName)
							.font(.system(size: 10, design: .monospaced))
							.foregroundColor(.secondary)
					}

					if !status.capabilities.isEmpty {
						Text("Capabilities: \(status.capabilities.joined(separator: ", "))")
							.font(.system(size: 10))
							.foregroundColor(.secondary)
					}

					if let error = status.errorMessage {
						Text(error)
							.font(.system(size: 10))
							.foregroundColor(.red)
					}
				} else {
					Text("No server configured for this scope")
						.font(.system(size: 11))
						.foregroundColor(.secondary)
				}
			}
		}
		.onAppear { viewModel.refreshLSPStatus() }
		.onChange(of: viewModel.selectedScope) { _, _ in viewModel.refreshLSPStatus() }
	}
}
```

- [ ] **Step 3: Create EditorSettingsTab.swift**

```swift
import SwiftUI

struct EditorSettingsTab: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel

	var body: some View {
		VStack(alignment: .leading, spacing: 4) {
			ForEach(viewModel.propertiesForCategory("editor")) { entry in
				PropertyRowView(
					entry: entry,
					onChanged: { viewModel.updateProperty(key: entry.key, value: $0) },
					onUnset: { viewModel.unsetProperty(key: entry.key) }
				)
			}
		}
	}
}
```

- [ ] **Step 4: Create FilesSettingsTab.swift**

```swift
import SwiftUI

struct FilesSettingsTab: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel

	var body: some View {
		VStack(alignment: .leading, spacing: 4) {
			ForEach(viewModel.propertiesForCategory("files")) { entry in
				PropertyRowView(
					entry: entry,
					onChanged: { viewModel.updateProperty(key: entry.key, value: $0) },
					onUnset: { viewModel.unsetProperty(key: entry.key) }
				)
			}

			Divider()

			Text("FILE BROWSER FILTERS")
				.font(.system(size: 9, weight: .semibold))
				.foregroundColor(.secondary)
				.tracking(1)

			ForEach(viewModel.propertiesForCategory("browser")) { entry in
				PropertyRowView(
					entry: entry,
					onChanged: { viewModel.updateProperty(key: entry.key, value: $0) },
					onUnset: { viewModel.unsetProperty(key: entry.key) }
				)
			}
		}
	}
}
```

- [ ] **Step 5: Create AllPropertiesTab.swift**

```swift
import SwiftUI

struct AllPropertiesTab: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel

	private let categories = ["lsp", "editor", "files", "browser", "project"]

	var body: some View {
		VStack(alignment: .leading, spacing: 0) {
			// Controls bar
			HStack {
				TextField("Filter properties...", text: $viewModel.filterText)
					.textFieldStyle(.roundedBorder)
					.font(.system(size: 11))
					.frame(maxWidth: 200)

				Toggle("Modified only", isOn: $viewModel.showModifiedOnly)
					.toggleStyle(.checkbox)
					.font(.system(size: 11))

				Spacer()
			}
			.padding(.bottom, 8)

			// Table header
			HStack(spacing: 0) {
				Text("Property")
					.frame(width: 160, alignment: .leading)
				Text("Value")
					.frame(maxWidth: .infinity, alignment: .leading)
				Text("Source")
					.frame(width: 120, alignment: .leading)
			}
			.font(.system(size: 10, weight: .medium))
			.foregroundColor(.secondary)
			.padding(.vertical, 4)
			.padding(.horizontal, 8)

			Divider()

			// Grouped properties
			ScrollView {
				LazyVStack(alignment: .leading, spacing: 0) {
					ForEach(categories, id: \.self) { category in
						let items = viewModel.filteredProperties.filter { $0.category == category }
						if !items.isEmpty {
							// Category header
							Text(category.uppercased())
								.font(.system(size: 9, weight: .bold))
								.foregroundColor(.accentColor)
								.tracking(1)
								.padding(.top, 8)
								.padding(.bottom, 2)
								.padding(.horizontal, 8)

							ForEach(items) { entry in
								AllPropertiesRow(entry: entry, viewModel: viewModel)
							}
						}
					}
				}
			}
		}
	}
}

private struct AllPropertiesRow: View {
	let entry: OakPropertyEntry
	@ObservedObject var viewModel: ProjectSettingsViewModel

	var body: some View {
		HStack(spacing: 0) {
			Text(entry.key)
				.font(.system(size: 11, design: .monospaced))
				.foregroundColor(entry.isModified ? .primary : .secondary)
				.italic(!entry.isModified)
				.frame(width: 160, alignment: .leading)

			TextField(entry.defaultValue.isEmpty ? "—" : entry.defaultValue, text: Binding(
				get: { entry.value },
				set: { viewModel.updateProperty(key: entry.key, value: $0) }
			))
			.textFieldStyle(.plain)
			.font(.system(size: 11, design: .monospaced))
			.foregroundColor(entry.isModified ? .primary : .secondary)
			.italic(!entry.isModified)
			.frame(maxWidth: .infinity, alignment: .leading)

			sourceLabel(for: entry)
				.frame(width: 120, alignment: .leading)

			if entry.isModified {
				Button(action: { viewModel.unsetProperty(key: entry.key) }) {
					Image(systemName: "arrow.uturn.backward.circle")
						.foregroundColor(.secondary)
						.font(.system(size: 10))
				}
				.buttonStyle(.plain)
			}
		}
		.padding(.vertical, 2)
		.padding(.horizontal, 8)
	}

	@ViewBuilder
	private func sourceLabel(for entry: OakPropertyEntry) -> some View {
		switch entry.source {
		case "project":
			Label(".tm_properties", systemImage: "doc.text")
				.font(.system(size: 9))
				.foregroundColor(.green)
		case "global":
			Label("~/.tm_properties", systemImage: "house")
				.font(.system(size: 9))
				.foregroundColor(.purple)
		case "userDefaults":
			Label("Preferences", systemImage: "gearshape")
				.font(.system(size: 9))
				.foregroundColor(.yellow)
		default:
			Text("Default")
				.font(.system(size: 9))
				.foregroundColor(.secondary)
				.italic()
		}
	}
}
```

- [ ] **Step 6: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && ./build.sh`
Expected: Compiles.

- [ ] **Step 7: Commit**
```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/
git commit -m "Add SwiftUI tab views for project settings"
```

---

### Task 6: Create ProjectSettingsBridge and root view

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/ProjectSettingsBridge.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/ProjectSettingsView.swift`

The bridge is the @objc controller that Obj-C++ creates and owns. It creates the NSHostingView containing the root SwiftUI view.

- [ ] **Step 1: Create ProjectSettingsView.swift** — root SwiftUI view with scope selector and tabs:

```swift
import SwiftUI

struct ProjectSettingsView: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel

	var body: some View {
		VStack(spacing: 0) {
			// Scope selector
			HStack(spacing: 8) {
				Text("File type:")
					.font(.system(size: 11))
					.foregroundColor(.secondary)

				Picker("", selection: $viewModel.selectedScope) {
					Text("(All / Unscoped)").tag("")
					ForEach(viewModel.availableScopes, id: \.self) { scope in
						Text("[ \(scope) ]").tag(scope)
					}
				}
				.labelsHidden()
				.frame(maxWidth: .infinity)

				Button("+") {
					showAddScopeSheet = true
				}
				.controlSize(.small)

				Button("−") {
					if !viewModel.selectedScope.isEmpty {
						viewModel.removeScope(viewModel.selectedScope)
					}
				}
				.controlSize(.small)
				.disabled(viewModel.selectedScope.isEmpty)
			}
			.padding(8)

			Divider()

			// Tab view
			TabView(selection: $viewModel.selectedTab) {
				ScrollView {
					LSPSettingsTab(viewModel: viewModel)
						.padding(12)
				}
				.tabItem { Text("LSP") }
				.tag(SettingsTab.lsp)

				ScrollView {
					EditorSettingsTab(viewModel: viewModel)
						.padding(12)
				}
				.tabItem { Text("Editor") }
				.tag(SettingsTab.editor)

				ScrollView {
					FilesSettingsTab(viewModel: viewModel)
						.padding(12)
				}
				.tabItem { Text("Files") }
				.tag(SettingsTab.files)

				AllPropertiesTab(viewModel: viewModel)
					.padding(12)
					.tabItem { Text("All") }
					.tag(SettingsTab.all)
			}
		}
		.sheet(isPresented: $showAddScopeSheet) {
			AddScopeSheet(viewModel: viewModel, isPresented: $showAddScopeSheet)
		}
	}

	@State private var showAddScopeSheet = false
}

private struct AddScopeSheet: View {
	@ObservedObject var viewModel: ProjectSettingsViewModel
	@Binding var isPresented: Bool
	@State private var scopeText = ""

	var body: some View {
		VStack(spacing: 12) {
			Text("Add Scope")
				.font(.headline)

			TextField("e.g. *.py or source.python", text: $scopeText)
				.textFieldStyle(.roundedBorder)
				.frame(width: 250)

			HStack {
				Button("Cancel") { isPresented = false }
					.keyboardShortcut(.cancelAction)
				Button("Add") {
					if !scopeText.isEmpty {
						viewModel.addScope(scopeText)
						isPresented = false
					}
				}
				.keyboardShortcut(.defaultAction)
				.disabled(scopeText.isEmpty)
			}
		}
		.padding(20)
	}
}
```

- [ ] **Step 2: Create ProjectSettingsBridge.swift**

```swift
import SwiftUI
import AppKit

@MainActor @objc public class ProjectSettingsBridge: NSObject {
	@objc public weak var delegate: ProjectSettingsBridgeDelegate? {
		didSet { viewModel.delegate = delegate }
	}

	private let viewModel = ProjectSettingsViewModel()
	private var hostingView: NSHostingView<ProjectSettingsView>?

	@objc public override init() {
		super.init()
		viewModel.bridge = self
	}

	@objc public func makeView() -> NSView {
		let rootView = ProjectSettingsView(viewModel: viewModel)
		let hosting = NSHostingView(rootView: rootView)
		self.hostingView = hosting
		return hosting
	}

	@objc public func updateProperties(_ properties: [OakPropertyEntry]) {
		viewModel.properties = properties
	}

	@objc public func updateScopes(_ scopes: [String]) {
		viewModel.availableScopes = scopes
		if !scopes.contains(viewModel.selectedScope) {
			viewModel.selectedScope = ""
		}
	}

	@objc public var selectedScope: String {
		viewModel.selectedScope
	}

	@objc public func setSelectedScope(_ scope: String) {
		viewModel.selectedScope = scope
	}
}
```

- [ ] **Step 3: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && ./build.sh`
Expected: Compiles.

- [ ] **Step 4: Commit**
```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/ProjectSettingsBridge.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/ProjectSettings/ProjectSettingsView.swift
git commit -m "Add ProjectSettingsBridge and root SwiftUI view"
```

---

### Task 7: Create ProjectsPreferencesV2 AppKit shell

**Files:**
- Create: `Frameworks/Preferences/src/ProjectsPreferencesV2.h`
- Create: `Frameworks/Preferences/src/ProjectsPreferencesV2.mm`
- Modify: `Frameworks/Preferences/src/Preferences.mm`

This is the main Obj-C++ pane that ties everything together: project list sidebar (NSTableView), SwiftUI bridge (right panel), and data layer orchestration.

- [ ] **Step 1: Create ProjectsPreferencesV2.h**

```objc
#import "Preferences.h"

@interface ProjectsPreferencesV2 : NSViewController <PreferencesPaneProtocol>
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@end
```

- [ ] **Step 2: Create ProjectsPreferencesV2.mm**

This is a large file (~400 lines). Key components:

```objc
#import "ProjectsPreferencesV2.h"
#import "Keys.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSImage Additions.h>
#import <settings/settings.h>
#import <settings/tm_properties_editor.h>
#import <kvdb/kvdb.h>
#import <lsp/LSPManager.h>
#import <io/path.h>
#import <oak/oak.h>
#import <OakSwiftUI/OakSwiftUI-Swift.h>

// KVDB path helper (duplicated from DocumentWindowController)
// Note: The KVDB path string must match exactly what DocumentWindowController uses
// to avoid opening a second SQLite connection.
static KVDB* sharedProjectStateDB ()
{
	static KVDB* db;
	if(!db)
	{
		NSString* appSupport = [NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) firstObject];
		NSString* path = [appSupport stringByAppendingPathComponent:@"TextMate/RecentProjects.db"];
		db = [KVDB sharedDBUsingFile:path];
	}
	return db;
}

@interface ProjectsPreferencesV2 () <NSTableViewDataSource, NSTableViewDelegate, ProjectSettingsBridgeDelegate>
{
	NSTableView* _projectListTable;
	ProjectSettingsBridge* _bridge;
	NSMutableArray<NSDictionary*>* _projects; // [{path, name, lastUsed}]
	NSString* _selectedProjectPath;
	NSTextField* _filterField;
	BOOL _suppressReload;
}
@end

@implementation ProjectsPreferencesV2

- (NSImage*)toolbarItemImage
{
	return [NSImage imageWithSystemSymbolName:@"folder.badge.gearshape" accessibilityDescription:@"Projects"];
}

- (id)init
{
	if(self = [super initWithNibName:nil bundle:nil])
	{
		self.identifier = @"projectsV2";
		self.title = @"Projects";
		_projects = [NSMutableArray array];
	}
	return self;
}

- (void)loadView
{
	// 1. Build project list sidebar
	_projectListTable = [[NSTableView alloc] init];
	NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"project"];
	column.title = @"Projects";
	[_projectListTable addTableColumn:column];
	_projectListTable.headerView = nil;
	_projectListTable.dataSource = self;
	_projectListTable.delegate = self;
	_projectListTable.rowHeight = 44;
	_projectListTable.usesAlternatingRowBackgroundColors = YES;

	NSScrollView* tableScroll = [[NSScrollView alloc] init];
	tableScroll.documentView = _projectListTable;
	tableScroll.hasVerticalScroller = YES;

	// Filter field
	_filterField = [NSTextField textFieldWithString:@""];
	_filterField.placeholderString = @"Filter projects…";
	_filterField.bezelStyle = NSTextFieldRoundedBezel;
	_filterField.target = self;
	_filterField.action = @selector(filterDidChange:);

	// Add/Remove buttons
	NSButton* addButton = [NSButton buttonWithTitle:@"+" target:self action:@selector(addProject:)];
	NSButton* removeButton = [NSButton buttonWithTitle:@"−" target:self action:@selector(removeProject:)];
	addButton.bezelStyle = NSBezelStyleSmallSquare;
	removeButton.bezelStyle = NSBezelStyleSmallSquare;

	NSStackView* buttonBar = [NSStackView stackViewWithViews:@[ addButton, removeButton ]];
	buttonBar.spacing = 4;

	NSStackView* sidebar = [NSStackView stackViewWithViews:@[ _filterField, tableScroll, buttonBar ]];
	sidebar.orientation = NSUserInterfaceLayoutOrientationVertical;
	sidebar.spacing = 4;
	sidebar.edgeInsets = NSEdgeInsetsMake(8, 8, 8, 8);
	[sidebar.widthAnchor constraintGreaterThanOrEqualToConstant:200].active = YES;
	[sidebar.widthAnchor constraintLessThanOrEqualToConstant:280].active = YES;
	[tableScroll setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];

	// 2. Build SwiftUI right panel
	_bridge = [[ProjectSettingsBridge alloc] init];
	_bridge.delegate = self;
	NSView* swiftUIView = [_bridge makeView];

	// 3. NSSplitView
	NSSplitView* splitView = [[NSSplitView alloc] init];
	splitView.dividerStyle = NSSplitViewDividerStyleThin;
	splitView.vertical = YES;
	[splitView addSubview:sidebar];
	[splitView addSubview:swiftUIView];
	[splitView setHoldingPriority:NSLayoutPriorityDefaultHigh forSubviewAtIndex:0];

	[splitView setFrameSize:NSMakeSize(700, 500)];
	self.view = splitView;

	// 4. Load project list
	[self reloadProjects];

	// Auto-select first project
	if(_projects.count > 0)
	{
		[_projectListTable selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
		[self tableViewSelectionDidChange:[NSNotification notificationWithName:NSTableViewSelectionDidChangeNotification object:_projectListTable]];
	}
}

// === Data Loading ===

- (void)reloadProjects
{
	[_projects removeAllObjects];

	// Add Global Defaults entry (always first)
	NSString* globalPath = [NSHomeDirectory() stringByAppendingPathComponent:@".tm_properties"];
	[_projects addObject:@{
		@"path": @"~/.tm_properties",
		@"name": @"Global Defaults",
		@"lastUsed": [NSDate date],
		@"isGlobal": @YES,
	}];

	// Load from RecentProjects.db
	KVDB* db = sharedProjectStateDB();
	NSArray* allEntries = [db allObjects];
	NSMutableArray* sortedEntries = [NSMutableArray arrayWithArray:allEntries];
	[sortedEntries sortUsingComparator:^NSComparisonResult(NSDictionary* a, NSDictionary* b) {
		NSDictionary* aVal = a[@"value"];
		NSDictionary* bVal = b[@"value"];
		NSDate* aDate = aVal[@"lastRecentlyUsed"] ?: [NSDate distantPast];
		NSDate* bDate = bVal[@"lastRecentlyUsed"] ?: [NSDate distantPast];
		return [bDate compare:aDate]; // descending
	}];

	for(NSDictionary* entry in sortedEntries)
	{
		NSString* path = entry[@"key"];
		if(!path) continue;

		[_projects addObject:@{
			@"path": path,
			@"name": [path lastPathComponent],
			@"lastUsed": entry[@"value"][@"lastRecentlyUsed"] ?: [NSDate distantPast],
			@"isGlobal": @NO,
		}];
	}

	[_projectListTable reloadData];
}

- (void)loadSettingsForProject:(NSString*)projectPath
{
	_selectedProjectPath = projectPath;
	BOOL isGlobal = [projectPath isEqualToString:@"~/.tm_properties"];
	std::string cppPath = isGlobal ? path::home() : to_s(projectPath);

	// Collect scopes from .tm_properties if it exists
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: cppPath + "/.tm_properties";

	NSMutableArray* scopes = [NSMutableArray array];
	NSMutableArray<OakPropertyEntry*>* entries = [NSMutableArray array];

	tm_properties_editor_t editor(tmPropsPath);
	auto sections = editor.sections();
	for(auto const& pair : sections)
	{
		if(!pair.first.empty())
			[scopes addObject:[NSString stringWithUTF8String:pair.first.c_str()]];
	}

	// Build property entries from catalog
	std::string selectedScope = to_s(_bridge.selectedScope);
	auto settingsInfo = settings_info_for_path(
		cppPath + "/dummy",
		selectedScope.empty() ? "" : scope::scope_t(),
		cppPath
	);

	// Map settings_info to source lookup
	std::map<std::string, std::string> infoSources;
	std::map<std::string, std::string> infoValues;
	for(auto const& info : settingsInfo)
	{
		infoValues[info.variable] = info.value;
		if(info.path.find("/.tm_properties") != std::string::npos)
		{
			if(info.path.find(path::home()) == 0 && info.path.find(cppPath) == std::string::npos)
				infoSources[info.variable] = "global";
			else
				infoSources[info.variable] = "project";
		}
		else
		{
			infoSources[info.variable] = "defaults";
		}
	}

	// Also check per-scope values from the editor
	auto scopeValues = sections.count(to_s(_bridge.selectedScope)) > 0
		? sections[to_s(_bridge.selectedScope)]
		: std::map<std::string, std::string>();
	auto unscopedValues = sections.count("") > 0
		? sections[""]
		: std::map<std::string, std::string>();

	// Check NSUserDefaults fallback
	// (Implementation details: read project:{path}:{scope}:{key} pattern)

	// Property catalog — hardcoded keys matching ProjectSettingsViewModel.propertyCatalog
	struct prop_meta { const char* key; const char* category; const char* type; const char* defaultVal; };
	static prop_meta const catalog[] = {
		{"lspCommand", "lsp", "string", ""}, {"lspEnabled", "lsp", "bool", "true"},
		{"lspRootPath", "lsp", "string", ""}, {"lspInitOptions", "lsp", "json", ""},
		{"tabSize", "editor", "int", "4"}, {"softTabs", "editor", "bool", "false"},
		{"softWrap", "editor", "bool", "false"}, {"wrapColumn", "editor", "int", "80"},
		{"showWrapColumn", "editor", "bool", "false"}, {"showIndentGuides", "editor", "bool", "false"},
		{"showInvisibles", "editor", "bool", "false"}, {"invisiblesMap", "editor", "string", ""},
		{"fontName", "editor", "string", ""}, {"fontSize", "editor", "int", "12"},
		{"theme", "editor", "string", ""}, {"spellChecking", "editor", "bool", "false"},
		{"spellingLanguage", "editor", "string", ""},
		{"encoding", "files", "string", "UTF-8"}, {"lineEndings", "files", "string", "\\n"},
		{"saveOnBlur", "files", "bool", "false"}, {"atomicSave", "files", "bool", "true"},
		{"formatCommand", "files", "string", ""}, {"formatOnSave", "files", "bool", "false"},
		{"fileType", "files", "string", ""}, {"binary", "files", "pattern", ""},
		{"exclude", "browser", "pattern", ""}, {"include", "browser", "pattern", ""},
		{"excludeInBrowser", "browser", "pattern", ""}, {"includeInBrowser", "browser", "pattern", ""},
		{"excludeInFileChooser", "browser", "pattern", ""}, {"includeInFileChooser", "browser", "pattern", ""},
		{"excludeInFolderSearch", "browser", "pattern", ""},
		{"followSymbolicLinks", "browser", "bool", "true"}, {"excludeSCMDeleted", "browser", "bool", "false"},
		{"projectDirectory", "project", "string", ""}, {"windowTitle", "project", "string", ""},
		{"tabTitle", "project", "string", ""}, {"scopeAttributes", "project", "string", ""},
		{"relatedFilePath", "project", "string", ""},
	};

	for(auto const& meta : catalog)
	{
		std::string key(meta.key);
		std::string value(meta.defaultVal);
		NSString* source = @"defaults";
		BOOL modified = NO;

		// Check per-scope values from .tm_properties
		if(scopeValues.count(key))
		{
			value = scopeValues[key];
			source = @"project";
			modified = YES;
		}
		else if(unscopedValues.count(key))
		{
			value = unscopedValues[key];
			source = @"project";
			modified = YES;
		}
		// Check settings_info cascade (picks up global ~/.tm_properties values)
		else if(infoValues.count(key))
		{
			value = infoValues[key];
			source = [NSString stringWithUTF8String:infoSources[key].c_str()];
			modified = (infoSources[key] != "defaults");
		}
		// Check NSUserDefaults fallback
		else
		{
			NSString* prefix = isGlobal ? @"global" : @"project";
			NSString* uKey = [NSString stringWithFormat:@"%@:{%@}:{%@}:%s", prefix, _selectedProjectPath, _bridge.selectedScope, meta.key];
			NSString* uVal = [NSUserDefaults.standardUserDefaults stringForKey:uKey];
			if(uVal)
			{
				value = to_s(uVal);
				source = @"userDefaults";
				modified = YES;
			}
		}

		OakPropertyEntry* entry = [[OakPropertyEntry alloc]
			initWithKey:[NSString stringWithUTF8String:meta.key]
			      value:[NSString stringWithUTF8String:value.c_str()]
			defaultValue:[NSString stringWithUTF8String:meta.defaultVal]
			     source:source
			   category:[NSString stringWithUTF8String:meta.category]
			propertyType:[NSString stringWithUTF8String:meta.type]
			 isModified:modified];
		[entries addObject:entry];
	}

	[_bridge updateScopes:scopes];
	[_bridge updateProperties:entries];
}

// === NSTableViewDataSource ===

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return _projects.count;
}

- (NSView*)tableView:(NSTableView*)tableView viewForTableColumn:(NSTableColumn*)column row:(NSInteger)row
{
	NSDictionary* project = _projects[row];
	NSTableCellView* cell = [tableView makeViewWithIdentifier:@"ProjectCell" owner:self];
	if(!cell)
	{
		cell = [[NSTableCellView alloc] init];
		cell.identifier = @"ProjectCell";

		NSTextField* nameField = [NSTextField labelWithString:@""];
		nameField.font = [NSFont systemFontOfSize:12 weight:NSFontWeightMedium];
		nameField.translatesAutoresizingMaskIntoConstraints = NO;

		NSTextField* pathField = [NSTextField labelWithString:@""];
		pathField.font = [NSFont systemFontOfSize:10];
		pathField.textColor = NSColor.secondaryLabelColor;
		pathField.translatesAutoresizingMaskIntoConstraints = NO;

		NSImageView* icon = [NSImageView imageViewWithImage:[NSImage imageWithSystemSymbolName:@"folder" accessibilityDescription:nil]];
		icon.translatesAutoresizingMaskIntoConstraints = NO;
		[icon.widthAnchor constraintEqualToConstant:20].active = YES;
		[icon.heightAnchor constraintEqualToConstant:20].active = YES;

		[cell addSubview:icon];
		[cell addSubview:nameField];
		[cell addSubview:pathField];
		cell.textField = nameField;

		[NSLayoutConstraint activateConstraints:@[
			[icon.leadingAnchor constraintEqualToAnchor:cell.leadingAnchor constant:4],
			[icon.centerYAnchor constraintEqualToAnchor:cell.centerYAnchor],
			[nameField.leadingAnchor constraintEqualToAnchor:icon.trailingAnchor constant:6],
			[nameField.topAnchor constraintEqualToAnchor:cell.topAnchor constant:4],
			[nameField.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor constant:-4],
			[pathField.leadingAnchor constraintEqualToAnchor:nameField.leadingAnchor],
			[pathField.topAnchor constraintEqualToAnchor:nameField.bottomAnchor constant:1],
			[pathField.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor constant:-4],
		]];

		nameField.tag = 100;
		pathField.tag = 101;
	}

	NSTextField* nameField = [cell viewWithTag:100];
	NSTextField* pathField = [cell viewWithTag:101];
	nameField.stringValue = project[@"name"];
	pathField.stringValue = [project[@"isGlobal"] boolValue] ? @"~/.tm_properties" : project[@"path"];

	if([project[@"isGlobal"] boolValue])
		nameField.textColor = NSColor.systemBlueColor;
	else
		nameField.textColor = NSColor.labelColor;

	return cell;
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	NSInteger row = _projectListTable.selectedRow;
	if(row >= 0 && row < (NSInteger)_projects.count)
	{
		[self loadSettingsForProject:_projects[row][@"path"]];
	}
}

// === Actions ===

- (void)addProject:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	panel.canChooseFiles = NO;
	panel.canChooseDirectories = YES;
	[panel beginSheetModalForWindow:self.view.window completionHandler:^(NSModalResponse result) {
		if(result == NSModalResponseOK)
		{
			NSString* path = panel.URL.path;
			KVDB* db = sharedProjectStateDB();
			NSDictionary* existing = db[path];
			if(!existing)
			{
				db[path] = @{ @"projectPath": path, @"lastRecentlyUsed": [NSDate date] };
			}
			[self reloadProjects];

			// Find and select the new project
			for(NSUInteger i = 0; i < _projects.count; i++)
			{
				if([_projects[i][@"path"] isEqualToString:path])
				{
					[_projectListTable selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
					break;
				}
			}
		}
	}];
}

- (void)removeProject:(id)sender
{
	NSInteger row = _projectListTable.selectedRow;
	if(row <= 0) return; // Can't remove Global Defaults (row 0)

	NSString* path = _projects[row][@"path"];
	KVDB* db = sharedProjectStateDB();
	[db removeObjectForKey:path];
	[self reloadProjects];
}

- (void)filterDidChange:(id)sender
{
	// TODO: implement filtering of _projects based on _filterField.stringValue
	[_projectListTable reloadData];
}

// === ProjectSettingsBridgeDelegate ===

- (void)settingsBridge:(ProjectSettingsBridge*)bridge didChangeProperty:(NSString*)key value:(NSString*)value scope:(NSString*)scope
{
	if(!_selectedProjectPath) return;
	_suppressReload = YES;

	BOOL isGlobal = [_selectedProjectPath isEqualToString:@"~/.tm_properties"];
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: to_s(_selectedProjectPath) + "/.tm_properties";

	if(path::exists(tmPropsPath))
	{
		tm_properties_editor_t editor(tmPropsPath);
		editor.set(to_s(key), to_s(value), to_s(scope));
		editor.save();
	}
	else
	{
		// NSUserDefaults fallback
		NSString* prefix = isGlobal ? @"global" : @"project";
		NSString* uKey = [NSString stringWithFormat:@"%@:{%@}:{%@}:%@", prefix, _selectedProjectPath, scope, key];
		[NSUserDefaults.standardUserDefaults setObject:value forKey:uKey];
	}

	_suppressReload = NO;
}

- (void)settingsBridge:(ProjectSettingsBridge*)bridge didUnsetProperty:(NSString*)key scope:(NSString*)scope
{
	if(!_selectedProjectPath) return;
	_suppressReload = YES;

	BOOL isGlobal = [_selectedProjectPath isEqualToString:@"~/.tm_properties"];
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: to_s(_selectedProjectPath) + "/.tm_properties";

	if(path::exists(tmPropsPath))
	{
		tm_properties_editor_t editor(tmPropsPath);
		editor.unset(to_s(key), to_s(scope));
		editor.save();
	}
	else
	{
		NSString* prefix = isGlobal ? @"global" : @"project";
		NSString* uKey = [NSString stringWithFormat:@"%@:{%@}:{%@}:%@", prefix, _selectedProjectPath, scope, key];
		[NSUserDefaults.standardUserDefaults removeObjectForKey:uKey];
	}

	// Reload settings to show resolved value
	[self loadSettingsForProject:_selectedProjectPath];
	_suppressReload = NO;
}

- (void)settingsBridge:(ProjectSettingsBridge*)bridge didAddScope:(NSString*)scope
{
	if(!_selectedProjectPath) return;
	BOOL isGlobal = [_selectedProjectPath isEqualToString:@"~/.tm_properties"];
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: to_s(_selectedProjectPath) + "/.tm_properties";

	if(path::exists(tmPropsPath))
	{
		tm_properties_editor_t editor(tmPropsPath);
		editor.add_section(to_s(scope));
		editor.save();
	}
}

- (void)settingsBridge:(ProjectSettingsBridge*)bridge didRemoveScope:(NSString*)scope
{
	if(!_selectedProjectPath) return;
	BOOL isGlobal = [_selectedProjectPath isEqualToString:@"~/.tm_properties"];
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: to_s(_selectedProjectPath) + "/.tm_properties";

	if(path::exists(tmPropsPath))
	{
		tm_properties_editor_t editor(tmPropsPath);
		editor.remove_section(to_s(scope));
		editor.save();
	}

	[self loadSettingsForProject:_selectedProjectPath];
}

- (NSArray<NSString*>*)settingsBridgeDidRequestScopeList:(ProjectSettingsBridge*)bridge
{
	if(!_selectedProjectPath) return @[];
	BOOL isGlobal = [_selectedProjectPath isEqualToString:@"~/.tm_properties"];
	std::string tmPropsPath = isGlobal
		? path::home() + "/.tm_properties"
		: to_s(_selectedProjectPath) + "/.tm_properties";

	tm_properties_editor_t editor(tmPropsPath);
	auto names = editor.section_names();
	NSMutableArray* result = [NSMutableArray array];
	for(auto const& name : names)
	{
		if(!name.empty())
			[result addObject:[NSString stringWithUTF8String:name.c_str()]];
	}
	return result;
}

- (OakLSPStatus*)settingsBridge:(ProjectSettingsBridge*)bridge lspStatusForScope:(NSString*)scope
{
	if(!_selectedProjectPath || [_selectedProjectPath isEqualToString:@"~/.tm_properties"])
		return nil;

	NSDictionary* status = [[LSPManager sharedManager] lspStatusForFileType:scope projectPath:_selectedProjectPath];
	if(!status)
		return nil;

	return [[OakLSPStatus alloc] initWithIsRunning:[status[@"isRunning"] boolValue]
	                                           pid:[status[@"pid"] integerValue] ?: 0
	                                    serverName:status[@"serverName"] ?: @""
	                                  errorMessage:status[@"error"]
	                                  capabilities:status[@"capabilities"] ?: @[]];
}

@end
```

Note: The `loadSettingsForProject:` method above has placeholders (marked with comments) for the property entry building logic. During implementation, this will need to be fleshed out with the full property catalog iteration — duplicating the catalog keys from the Swift ViewModel since C++ can't call Swift statics. Consider extracting the catalog keys to a shared header or plist.

- [ ] **Step 3: Register the pane in Preferences.mm**

In `Frameworks/Preferences/src/Preferences.mm`, add import and replace in array:

```objc
#import "ProjectsPreferencesV2.h"
```

In the `viewControllers` array (~line 98-106), the `ProjectsPreferences` line should already be removed (Task 0). Add ProjectsPreferencesV2:

```objc
NSArray<NSViewController <PreferencesPaneProtocol>*>* viewControllers = @[
	[[FilesPreferences alloc] init],
	[[ProjectsPreferencesV2 alloc] init],
	[[BundlesPreferences alloc] init],
	[[VariablesPreferences alloc] init],
	[[FormattersPreferences alloc] init],
	[[TerminalPreferences alloc] init],
	[[AdvancedPreferences alloc] init]
];
```

- [ ] **Step 4: Add framework dependencies and include paths**

The Preferences framework needs to link against `kvdb` (for KVDB access). Add to `Frameworks/Preferences/CMakeLists.txt`:

```cmake
target_link_libraries(Preferences PUBLIC BundlesManager OakAppKit OakFoundation MenuBuilder bundles io ns regexp settings text kvdb)
```

Do NOT add `lsp` to Preferences — it will be resolved at app link time since Preferences is a static lib.

For OakSwiftUI headers: the Swift-generated header `OakSwiftUI-Swift.h` is already available via the include path set in `Applications/TextMate/CMakeLists.txt`. Since ProjectsPreferencesV2.mm compiles as part of the TextMate app target (via static lib), this include path is inherited. Verify by checking that `#import <OakSwiftUI/OakSwiftUI-Swift.h>` resolves during build. If not, add to `Applications/TextMate/CMakeLists.txt`:

```cmake
target_include_directories(TextMate PRIVATE ${CMAKE_BINARY_DIR}/lib/OakSwiftUI.build/include)
```

- [ ] **Step 5: Build and test**

Run: `make`
Expected: Compiles. The Projects pane should appear in Preferences toolbar.

Run: `make run` — open Preferences, click Projects tab. Verify:
- Project list loads with Global Defaults at top + recent projects
- Selecting a project loads the SwiftUI content on the right
- Tab switching works (LSP/Editor/Files/All)

- [ ] **Step 6: Commit**
```
git add Frameworks/Preferences/src/ProjectsPreferencesV2.h Frameworks/Preferences/src/ProjectsPreferencesV2.mm Frameworks/Preferences/src/Preferences.mm Frameworks/Preferences/CMakeLists.txt
git commit -m "Add ProjectsPreferencesV2 AppKit shell with project list and SwiftUI bridge"
```

---

### Task 8: Integration testing and polish

**Files:**
- Modify: various files as needed for bug fixes

- [ ] **Step 1: Manual integration test checklist**

Run: `make run` and test each scenario:

1. Open Preferences → Projects tab appears with correct icon
2. Global Defaults is pinned at top, blue text, shows "~/.tm_properties"
3. Recent projects load sorted by last used, show folder name + path
4. Click "+ Add" → folder picker → new project appears in list
5. Click "− Remove" → project removed (not for Global Defaults)
6. Select a project → right panel populates with settings
7. LSP tab: shows form fields, scope selector works, server status displays
8. Editor tab: toggle and text field controls work
9. Files tab: all file/browser properties display
10. All tab: shows all 45+ properties with source column
11. All tab: "Modified only" toggle filters correctly
12. All tab: filter text field narrows properties
13. Change a setting → verify `.tm_properties` file updates (or NSUserDefaults if no file)
14. Reset button (undo arrow) → reverts property to default
15. Add scope → new scope appears in dropdown
16. Remove scope → scope and its properties removed
17. Scope selection persists across tab switches

- [ ] **Step 2: Fix any compilation or runtime issues found**

- [ ] **Step 3: Build release to verify no warnings**

Run: `make release`
Expected: Clean build.

- [ ] **Step 4: Run settings framework tests**

Run: `cd build-debug && ctest --output-on-failure -R settings`
Expected: All pass including new `t_tm_properties_editor` tests.

- [ ] **Step 5: Final commit**
```
git add -u
git commit -m "Polish Projects preferences pane integration"
```

---

## Dependencies

```
Task 0 (migrate old pane)     ← independent
Task 1 (tm_properties_editor) ← independent
Task 2 (LSP status query)     ← independent
Task 3 (bridge types)         ← independent
Task 4 (ViewModel)            ← depends on Task 3
Task 5 (tab views)            ← depends on Task 4
Task 6 (bridge + root view)   ← depends on Task 5
Task 7 (AppKit shell)         ← depends on Tasks 0, 1, 2, 6
Task 8 (integration)          ← depends on Task 7
```

Tasks 0, 1, 2, 3 can run in parallel. Tasks 4-6 are sequential (Swift build chain). Task 7 integrates everything. Task 8 is final polish.
