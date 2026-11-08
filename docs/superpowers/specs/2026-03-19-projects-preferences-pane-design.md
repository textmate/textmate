# Projects Preferences Pane Design

**Date:** 2026-03-19
**Status:** Draft
**Scope:** New preference pane for managing per-project and global `.tm_properties` settings with a dedicated LSP configuration UI

## Overview

A new "Projects" preference pane that provides a GUI for managing `.tm_properties` settings. The pane uses a split-panel layout with a project list sidebar (AppKit) and a tabbed settings panel (SwiftUI via OakSwiftUI). LSP configuration is the primary tab with a dedicated form UI; all other tm_properties are accessible through categorized tabs and a full "All" table view.

## Architecture

**Hybrid AppKit + SwiftUI approach:**

```
┌─────────────────────────────────────────────────────────┐
│ Preferences Framework (Obj-C++)                         │
│                                                         │
│  ProjectsPreferencesV2.mm                               │
│  ├─ NSViewController + PreferencesPaneProtocol          │
│  ├─ NSSplitView                                         │
│  │   ├─ Left: NSTableView (project list)                │
│  │   └─ Right: NSHostingView (SwiftUI content)          │
│  └─ Owns ProjectSettingsBridge (Swift @objc bridge)     │
│                                                         │
├─────────── @objc bridge ────────────────────────────────┤
│                                                         │
│  OakSwiftUI Framework (Swift)                           │
│                                                         │
│  ProjectSettingsBridge.swift                             │
│  ├─ @objc public class, creates NSHostingView           │
│  ├─ ProjectSettingsBridgeDelegate protocol              │
│  └─ Passes data as [OakPropertyEntry] arrays            │
│                                                         │
│  ProjectSettingsViewModel.swift                          │
│  ├─ @MainActor ObservableObject                         │
│  ├─ @Published: properties, scopes, selectedTab, etc.   │
│  └─ Property catalog with defaults and type metadata    │
│                                                         │
│  Tab Views:                                             │
│  ├─ LSPSettingsTab.swift      (dedicated form)          │
│  ├─ EditorSettingsTab.swift   (key-value grid)          │
│  ├─ FilesSettingsTab.swift    (key-value grid)          │
│  └─ AllPropertiesTab.swift    (full table + source)     │
│                                                         │
├─────────── delegate callbacks ──────────────────────────┤
│                                                         │
│  Data Layer (C++ / Obj-C++)                             │
│  ├─ tm_properties_editor_t    → per-file .tm_properties │
│  │   (new class: parse via ini_file_t, serialize back)  │
│  ├─ settings_for_path()       → cascade resolution      │
│  ├─ settings_info_for_path()  → source attribution      │
│  ├─ KVDB (RecentProjects.db)  → project list source     │
│  ├─ NSUserDefaults            → fallback storage         │
│  └─ track_paths_t             → file change watching     │
└─────────────────────────────────────────────────────────┘
```

**Data flow:**
1. Obj-C++ pane queries project list from KVDB, creates bridge, passes project path + settings
2. Bridge populates ViewModel with `[OakPropertyEntry]` arrays
3. SwiftUI tabs display settings with two-way binding
4. On edit → ViewModel calls delegate → Obj-C++ writes to `.tm_properties` or NSUserDefaults
5. `track_paths_t` detects `.tm_properties` file changes → settings reload → ViewModel refreshes

### Critical: Writing to .tm_properties Files

**`settings_t::set()` only writes to the global `~/.tm_properties` file.** It cannot target per-project `.tm_properties` files. A new helper class `tm_properties_editor_t` is needed in `Frameworks/settings/`:

```cpp
// Frameworks/settings/src/tm_properties_editor.h
struct tm_properties_editor_t
{
    tm_properties_editor_t (std::string const& path);

    // Read all sections and their key-value pairs
    std::map<std::string, std::map<std::string, std::string>> sections () const;

    // Set a property in a specific section (empty section = top-level)
    void set (std::string const& key, std::string const& value,
              std::string const& section = "");

    // Remove a property from a section
    void unset (std::string const& key, std::string const& section = "");

    // Add/remove entire sections
    void add_section (std::string const& section);
    void remove_section (std::string const& section);

    // Write back to file (sorted: top-level → scope selectors → globs)
    void save () const;

private:
    std::string _path;
    std::map<std::string, std::map<std::string, std::string>> _sections;
};
```

This class uses `ini_file_t` for parsing and replicates the serialization logic from `settings_t::set()` (ordered sections: top-level first, then scope selectors, then file globs sorted by specificity). It targets a specific file path rather than always writing to `global_settings_path()`.

For the global level, `settings_t::set()` can still be used since it targets `~/.tm_properties`.

## Component 1: Project List Sidebar (AppKit)

**Implementation:** NSTableView in the left pane of an NSSplitView, managed by `ProjectsPreferencesV2.mm`.

### Data Source

- **Primary:** `[DocumentWindowController sharedProjectStateDB]` — KVDB SQLite database at `~/Library/Application Support/TextMate/RecentProjects.db`
- Each entry contains: `projectPath`, `lastRecentlyUsed` (NSDate), `documents`, window state
- **Sort:** By `lastRecentlyUsed` descending (most recent first)

### UI Elements

| Element | Behavior |
|---------|----------|
| **Global Defaults** (pinned) | Always at top, visually distinct. Represents `~/.tm_properties` or NSUserDefaults. Cannot be removed. |
| **Project rows** | Folder name (bold), full path (secondary), relative timestamp. Folder icon. |
| **Filter field** | Above project list. Instant text filter by folder name or path. |
| **+ Add button** | Opens NSOpenPanel (folder picker). Creates entry in RecentProjects.db. Auto-selects new project. |
| **− Remove button** | Removes selected project from RecentProjects.db. Does NOT delete `.tm_properties` file. Disabled for Global Defaults. |

### Selection Behavior

Selecting a project triggers `projectDidChange:` on the delegate, which:
1. Reads settings for the project path via `settings_for_path()` / `settings_info_for_path()`
2. Collects available scopes from the project's `.tm_properties` (if exists)
3. Passes property entries and scope list to the SwiftUI bridge
4. ViewModel updates, SwiftUI tabs refresh

## Component 2: Legacy ProjectsPreferences Migration

The existing `ProjectsPreferences` pane is **removed**. Its settings are redistributed:

**Moved to Advanced preferences pane** (global NSUserDefaults, not per-project):
- File browser: location, folders on top, show links as expandable, single-click to open, auto-reveal, position (left/right), window resize toggle
- Document tabs: show for single document, re-order on open, auto-close unused
- HTML output placement: bottom/right/window

These are app-wide UI preferences that don't vary per-project, so they belong with other global settings in the Advanced pane.

**Already covered by the new Projects pane** (Files tab, Browser Filters section):
- Exclude pattern, include pattern, binary pattern (`.tm_properties` keys)

## Component 3: Scope Selector (SwiftUI, shared across tabs)

A dropdown at the top of the settings area, consistent across all 4 tabs. Selecting a scope in one tab carries to all tabs.

### Scope Dropdown

- Lists all sections found in the project's `.tm_properties` file
- Plus scopes from `Default.tmProperties` that have settings
- Special entry: **"(All / Unscoped)"** — top-level settings with no section qualifier
- Format examples: `[ *.py ]`, `[ source.python ]`, `[ *.{c,cc,cpp,h} ]`

### + Add Scope

- Text field with autocomplete for known file extensions and scope names
- Supports glob patterns: `*.{c,cc,cpp,h}`
- Supports TextMate scopes: `source.python`
- Creates a new empty section in `.tm_properties` or NSUserDefaults

### − Delete Scope

- Removes the scope section and all its properties
- Confirmation alert if the scope has values set

## Component 4: LSP Settings Tab (SwiftUI — Dedicated Form)

The primary tab. Shows a form with labeled fields for the 4 LSP configuration keys, plus a read-only server status section.

### Form Fields

| Field | Property Key | Control Type | Placeholder/Hint |
|-------|-------------|-------------|-----------------|
| **Enabled** | `lspEnabled` | Toggle switch | ON/OFF |
| **Server command** | `lspCommand` | Monospace text field | `"clangd --background-index"` |
| **Root path** | `lspRootPath` | Text field | "Auto-detected" (italic placeholder) |
| **Init options** | `lspInitOptions` | Multi-line monospace text area | `{ }` — JSON object |

Each field shows hint text below explaining usage. Fields display per the currently selected scope.

### Server Status Section (read-only)

Below a divider, queries `LSPManager` for the running client matching the current scope:

- **Status indicator:** Green dot + "Running" / Red dot + "Stopped" / Yellow dot + "Error"
- **PID:** Process ID when running
- **Capabilities:** Lists detected server capabilities (completion, hover, definition, rename, codeAction, formatting)

Helps users debug "why isn't LSP working?" without leaving preferences.

## Component 5: Editor Settings Tab (SwiftUI — Key-Value Grid)

Displays editor-related properties in a labeled grid with typed controls.

### Properties

| Property | Key | Control |
|----------|-----|---------|
| Tab size | `tabSize` | Number field |
| Soft tabs | `softTabs` | Toggle |
| Soft wrap | `softWrap` | Toggle |
| Wrap column | `wrapColumn` | Number field |
| Show wrap column | `showWrapColumn` | Toggle |
| Show indent guides | `showIndentGuides` | Toggle |
| Show invisibles | `showInvisibles` | Toggle |
| Invisibles map | `invisiblesMap` | Text field |
| Font name | `fontName` | Text field |
| Font size | `fontSize` | Number field |
| Theme | `theme` | Text field |
| Spell checking | `spellChecking` | Toggle |
| Spelling language | `spellingLanguage` | Text field |

### Value Display

- **Explicitly set values:** Normal weight, full color
- **Inherited/default values:** Italic, gray — indicates the value comes from a parent scope, global settings, or hardcoded default

## Component 6: Files Settings Tab (SwiftUI — Key-Value Grid)

Displays file I/O properties and file browser filter patterns.

### Properties

| Property | Key | Control |
|----------|-----|---------|
| Encoding | `encoding` | Dropdown (known encodings) |
| Line endings | `lineEndings` | Dropdown (LF/CRLF/CR) |
| Save on blur | `saveOnBlur` | Toggle |
| Atomic save | `atomicSave` | Toggle |
| Store encoding per file | `storeEncodingPerFile` | Toggle |
| Disable extended attributes | `disableExtendedAttributes` | Toggle |
| Format command | `formatCommand` | Monospace text field |
| Format on save | `formatOnSave` | Toggle |
| Binary pattern | `binary` | Wide monospace text field |
| File type | `fileType` | Text field |

### File Browser Filters Section

Separated by a divider with a "FILE BROWSER FILTERS" label.

| Property | Key | Control |
|----------|-----|---------|
| Exclude | `exclude` | Wide monospace text field |
| Include | `include` | Wide monospace text field |
| Exclude in browser | `excludeInBrowser` | Wide monospace text field |
| Include in browser | `includeInBrowser` | Wide monospace text field |
| Exclude in file chooser | `excludeInFileChooser` | Wide monospace text field |
| Include in file chooser | `includeInFileChooser` | Wide monospace text field |
| Exclude in folder search | `excludeInFolderSearch` | Wide monospace text field |
| Follow symbolic links | `followSymbolicLinks` | Toggle |
| Exclude SCM deleted | `excludeSCMDeleted` | Toggle |

## Component 7: All Properties Tab (SwiftUI — Full Table)

A flat table showing every known property with its resolved value and source attribution.

### Table Columns

| Column | Width | Content |
|--------|-------|---------|
| **Property** | 30% | Key name in monospace |
| **Value** | 40% | Resolved value, click to edit inline |
| **Source** | 30% | Where the value comes from (color-coded) |

### Source Color Coding

| Source | Color | Icon | Meaning |
|--------|-------|------|---------|
| Project `.tm_properties` | Green | 📄 | Set explicitly in project file |
| Global `~/.tm_properties` | Purple | 🏠 | Set in user's global file |
| NSUserDefaults | Yellow | ⚙️ | Stored in app preferences |
| Default | Gray italic | — | From Default.tmProperties or hardcoded |

### Grouping

Properties are grouped under visual section headers: **LSP**, **Editor**, **Files**, **Browser**, **Formatting**, **Project**. Headers are non-collapsible visual separators.

### Controls

- **Filter field:** Narrows visible properties by name (instant, as-you-type)
- **"Show modified only" toggle:** Hides default/inherited rows, showing only properties with an explicit source. Keeps the table practical when only reviewing customizations.
- **Inline editing:** Click any value cell to edit. Editing a default value promotes it to an explicit setting (writes to `.tm_properties` or NSUserDefaults).

## Storage Strategy

### Read Path

1. User selects project + scope
2. Obj-C++ delegate receives `projectDidChange:` / `scopeDidChange:`
3. **If `.tm_properties` exists:** `ini_file_t` parses the file, `settings_for_path()` resolves the cascade (`~/.tm_properties` → project `.tm_properties`), `settings_info_for_path()` provides per-property source metadata
4. **If no `.tm_properties`:** Check NSUserDefaults for per-project keys, merge with global defaults
5. Fill remaining properties from `Default.tmProperties` hardcoded defaults
6. Build `[OakPropertyEntry]` array and pass to ViewModel

### Write Path

1. User edits a property value in any tab
2. ViewModel notifies delegate via `didChangeProperty:value:scope:`
3. **If `.tm_properties` exists for this level:** `tm_properties_editor_t` writes to the correct section in the target file. For global `~/.tm_properties`, `settings_t::set()` can also be used. `track_paths_t` detects the change and triggers auto-reload.
4. **If no `.tm_properties`:** Write to NSUserDefaults with structured key pattern
5. **Side effects:** Open editors refresh settings via `track_paths_t` → `OakDocument` → `OakTextView`. If `lspCommand` changed, `LSPManager` re-evaluates clients.

### NSUserDefaults Key Schema

When no `.tm_properties` file exists, settings are stored with structured keys:

```
project:{/path/to/project}:{scope}:{property}
project:{/Users/user/code/myapp}:{*.py}:tabSize = 4
project:{/Users/user/code/myapp}::encoding = UTF-8        (unscoped)
global::{*.go}:lspCommand = gopls                          (global scope)
global:::fontName = Menlo-Regular                          (global unscoped)
```

### Precedence (highest to lowest)

1. Project `.tm_properties` file-type scoped section (`[ *.py ]`)
2. Project `.tm_properties` unscoped
3. Global `~/.tm_properties` file-type scoped section
4. Global `~/.tm_properties` unscoped
5. Per-project NSUserDefaults (when no `.tm_properties`)
6. Global NSUserDefaults (when no `~/.tm_properties`)
7. `Default.tmProperties` hardcoded defaults

## Bridge Types (OakSwiftUI)

### OakPropertyEntry

```swift
@objc public class OakPropertyEntry: NSObject, Identifiable {
    @objc public let key: String           // e.g. "tabSize"
    @objc public var value: String          // e.g. "4"
    @objc public let defaultValue: String   // e.g. "4"
    @objc public let source: String         // "project", "global", "defaults", "userDefaults"
    @objc public let category: String       // "lsp", "editor", "files", "browser", "formatting", "project"
    @objc public let propertyType: String   // "bool", "int", "string", "pattern", "json"
    @objc public var isModified: Bool       // true if explicitly set (not default)
}
```

### ProjectSettingsBridgeDelegate

```objc
@objc public protocol ProjectSettingsBridgeDelegate: AnyObject {
    func settingsBridge(_ bridge: ProjectSettingsBridge, didChangeProperty key: String, value: String, scope: String)
    func settingsBridge(_ bridge: ProjectSettingsBridge, didUnsetProperty key: String, scope: String)
    func settingsBridge(_ bridge: ProjectSettingsBridge, didAddScope scope: String)
    func settingsBridge(_ bridge: ProjectSettingsBridge, didRemoveScope scope: String)
    func settingsBridgeDidRequestScopeList(_ bridge: ProjectSettingsBridge) -> [String]
    func settingsBridge(_ bridge: ProjectSettingsBridge, lspStatusForScope scope: String) -> OakLSPStatus?
}
```

The `didUnsetProperty:scope:` method removes a property from `.tm_properties` (via `tm_properties_editor_t::unset()`) or NSUserDefaults, reverting it to the inherited/default value. This is triggered when the user clears a value or explicitly clicks "Reset to default" on a property.

### OakLSPStatus

```swift
@objc public class OakLSPStatus: NSObject {
    @objc public let isRunning: Bool
    @objc public let pid: Int
    @objc public let errorMessage: String?
    @objc public let capabilities: [String]  // ["completion", "hover", "definition", ...]
}
```

## Property Catalog

Complete list of all known `.tm_properties` keys, organized by tab category:

### LSP (4 properties)
- `lspCommand` (string) — Server executable + arguments
- `lspEnabled` (bool) — Enable/disable LSP for scope
- `lspRootPath` (string) — Override workspace root
- `lspInitOptions` (json) — Server initialization options JSON

### Editor (13 properties)
- `tabSize` (int) — Tab width in spaces
- `softTabs` (bool) — Use spaces instead of tabs
- `softWrap` (bool) — Wrap long lines visually
- `wrapColumn` (int) — Column to wrap at
- `showWrapColumn` (bool) — Show wrap column indicator
- `showIndentGuides` (bool) — Show indent guide lines
- `showInvisibles` (bool) — Show whitespace characters
- `invisiblesMap` (string) — Map of invisible character replacements
- `fontName` (string) — Editor font PostScript name
- `fontSize` (int) — Editor font size in points
- `theme` (string) — Theme UUID or name
- `spellChecking` (bool) — Enable spell checking
- `spellingLanguage` (string) — Spell check language code

### Files (10 properties)
- `encoding` (string) — File encoding (UTF-8, etc.)
- `lineEndings` (string) — Line ending style (\n, \r\n, \r)
- `saveOnBlur` (bool) — Auto-save when editor loses focus
- `atomicSave` (bool) — Use atomic file writes
- `storeEncodingPerFile` (bool) — Remember encoding per file
- `disableExtendedAttributes` (bool) — Skip xattr on save
- `formatCommand` (string) — External format command
- `formatOnSave` (bool) — Run format command on save
- `fileType` (string) — Override file type/grammar
- `binary` (pattern) — Binary file glob pattern

### Browser (13 properties)
- `exclude` (pattern) — Global exclude pattern
- `include` (pattern) — Global include pattern
- `excludeInBrowser` (pattern) — Exclude in file browser
- `includeInBrowser` (pattern) — Include in file browser
- `excludeInFileChooser` (pattern) — Exclude in file chooser
- `includeInFileChooser` (pattern) — Include in file chooser
- `excludeInFolderSearch` (pattern) — Exclude in folder search
- `excludeDirectories` (pattern) — Exclude directories globally
- `excludeDirectoriesInBrowser` (pattern) — Exclude dirs in browser
- `excludeFiles` (pattern) — Exclude files globally
- `excludeFilesInBrowser` (pattern) — Exclude files in browser
- `followSymbolicLinks` (bool) — Follow symlinks in browser
- `excludeSCMDeleted` (bool) — Hide SCM-deleted files

### Project (5 properties)
- `projectDirectory` (string) — Project root override
- `windowTitle` (string) — Window title format string
- `tabTitle` (string) — Tab title format string
- `scopeAttributes` (string) — Additional scope attributes
- `relatedFilePath` (string) — Related file path pattern

## File Inventory

### New Files

| File | Framework | Purpose |
|------|-----------|---------|
| `tm_properties_editor.h` | settings | Per-file .tm_properties read/write helper |
| `tm_properties_editor.cc` | settings | Implementation: parse via ini_file_t, serialize back |
| `ProjectsPreferencesV2.h` | Preferences | Header for new pane |
| `ProjectsPreferencesV2.mm` | Preferences | AppKit shell: NSSplitView, NSTableView, bridge ownership |
| `ProjectSettingsBridge.swift` | OakSwiftUI | @objc bridge controller, NSHostingView lifecycle |
| `ProjectSettingsViewModel.swift` | OakSwiftUI | @MainActor ObservableObject, property catalog |
| `ProjectSettingsView.swift` | OakSwiftUI | Root SwiftUI view with TabView + scope selector |
| `LSPSettingsTab.swift` | OakSwiftUI | LSP dedicated form UI |
| `EditorSettingsTab.swift` | OakSwiftUI | Editor properties grid |
| `FilesSettingsTab.swift` | OakSwiftUI | Files properties grid |
| `AllPropertiesTab.swift` | OakSwiftUI | Full property table with source |
| `OakPropertyEntry.swift` | OakSwiftUI | Bridge data type |
| `OakLSPStatus.swift` | OakSwiftUI | LSP status bridge type |

### Modified Files

| File | Change |
|------|--------|
| `Preferences/src/Preferences.mm` | Replace old ProjectsPreferences with ProjectsPreferencesV2 in pane array |
| `Preferences/src/AdvancedPreferences.mm` | Add 12 settings migrated from old ProjectsPreferences (file browser, tabs, HTML output) |
| `Preferences/src/Keys.h` / `Keys.mm` | Verify all migrated keys are declared (most already are) |
| `settings/CMakeLists.txt` | Add tm_properties_editor source files |
| `OakSwiftUI/Sources/OakSwiftUI/` | Add new Bridge/ and ProjectSettings/ subdirectories |
| `OakSwiftUI/Package.swift` | No change needed (sources auto-discovered) |

## Constraints and Risks

1. **OakSwiftUI build integration:** The Swift package builds separately via `build.sh`. New files in `Sources/OakSwiftUI/` are auto-discovered by SPM, but the generated header must include new `@objc` declarations for Obj-C++ to see them.

2. **C++ ↔ Swift boundary:** `settings_t`, `ini_file_t`, and `settings_for_path()` are C++ APIs. They cannot be called directly from Swift. All data must be marshaled through the Obj-C++ pane (`ProjectsPreferencesV2.mm`) and passed to Swift as `[OakPropertyEntry]` arrays via the bridge delegate.

3. **Existing ProjectsPreferences:** The old `ProjectsPreferences` pane is removed. Its 12 global NSUserDefaults settings migrate to the Advanced pane. The 3 `.tm_properties` bindings (exclude, include, binary) are covered by the new pane's Files tab.

4. **NSUserDefaults ↔ settings cascade gap:** `settings_for_path()` does NOT read the `project:{path}:{scope}:{prop}` NSUserDefaults keys. The preferences pane reads these keys directly and presents them to the user, but they are invisible to the editor's settings resolution engine. This means: **NSUserDefaults is a UI-only storage layer for the preferences pane.** For settings to actually take effect in the editor, the pane should write to `.tm_properties` whenever possible. NSUserDefaults is purely a persistence mechanism for the pane's own state when no `.tm_properties` file exists — the pane itself injects these values into the settings resolution by creating a `.tm_properties` file on first edit, or the values remain pane-local until the user creates one. Alternatively, `settings_for_path()` could be extended to consult NSUserDefaults as a final fallback, but this is a larger change that should be evaluated during implementation.

5. **KVDB access from Preferences framework:** `sharedProjectStateDB` is defined in `DocumentWindowController.mm` and `Favorites.mm`. The Preferences framework does not currently link against these. **Solution:** Extract `sharedProjectStateDB` into a shared utility (e.g., a class method on a new lightweight `ProjectState` class in a framework both can access, or simply duplicate the KVDB path lookup in the pane since KVDB is already linked).

6. **Performance:** Loading 60+ properties with source attribution for each scope requires calling `settings_info_for_path()`. This should be fast (in-memory after first parse) but should be done off-main-thread if any I/O is involved, delivering results to ViewModel on main thread.

7. **track_paths_t reactivity:** When the UI writes to `.tm_properties` via `tm_properties_editor_t::save()`, the file watcher will fire and trigger a reload. The pane must handle this without creating an edit → reload → edit loop. Use a flag to suppress reload when the pane itself initiated the write.

8. **`settings_t::set()` rewrites the entire `~/.tm_properties` on every call.** For the global level, rapid edits in the All tab could cause performance/integrity issues. Consider debouncing writes (batch changes, write on focus loss or after a short delay) rather than writing on every keystroke.
