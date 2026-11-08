# Command Palette Design

## Overview

A keyboard-driven command palette (Cmd+Shift+P) providing unified fuzzy-search access to menu actions, bundle commands, recent projects, symbols, bundle editor, go-to-line, find-in-project, and settings. Built as a pure SwiftUI component in OakSwiftUI with an ObjC++ bridge class following the `OakRenameField` pattern (key-stealing panel with text input).

## Prefix Modes

| Prefix | Mode | Data Source |
|--------|------|-------------|
| (none) | Recent Projects | KVDB `RecentProjects.db` |
| `>` | Commands & Actions | `NSApp.mainMenu` walk + `bundles::query()` |
| `@` | Symbols | Current document symbol list |
| `#` | Bundle Editor | `bundles::query()` for grammars/snippets/commands |
| `:` | Go to Line | Parses number, validates against document line count |
| `/` | Find in Project | Delegates to existing Find in Project |
| `~` | Settings | Curated toggleable settings with current values |

Typing a prefix character switches mode and strips it from the filter query. Backspacing past the prefix returns to default (Recent Projects) mode.

## Architecture

### Component Overview

```
AppController (ObjC++)
  └─ OakCommandPalette (Swift bridge, @MainActor @objc)
       ├─ KeyablePanel (NSPanel subclass, canBecomeKey=true)
       │   └─ NSHostingView<CommandPaletteView>
       ├─ CommandPaletteViewModel (@MainActor ObservableObject)
       └─ OakCommandPaletteDelegate (weak, @objc protocol)
```

### File Layout

```
Frameworks/OakSwiftUI/Sources/OakSwiftUI/
├── Bridge/
│   ├── OakCommandPalette.swift          # Bridge class
│   ├── OakCommandPaletteDelegate.swift  # Delegate protocol
│   └── OakCommandPaletteItem.swift      # Item model
└── CommandPalette/
    ├── CommandPaletteViewModel.swift     # State + filtering + frecency
    ├── CommandPaletteView.swift          # Root SwiftUI view
    └── CommandPaletteRowView.swift       # Result row
```

### Bridge Class: OakCommandPalette

Follows the `OakRenameField` pattern — a key-stealing panel that captures keyboard focus for its embedded text field. Unlike `OakCompletionPopup` (which is non-key and relies on the parent text view forwarding events), the command palette owns keyboard focus directly.

```swift
@MainActor @objc public class OakCommandPalette: NSObject {
    @objc public weak var delegate: OakCommandPaletteDelegate?

    private let theme: OakThemeEnvironment
    private var panel: KeyablePanel?
    private var viewModel: CommandPaletteViewModel?
    private var cancellables = Set<AnyCancellable>()

    @objc public init(theme: OakThemeEnvironment) { ... }
    @objc public func show(in parentWindow: NSWindow, items: [OakCommandPaletteItem]) { ... }
    @objc public func dismiss() { ... }
    @objc public var isVisible: Bool { ... }
}

private class KeyablePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override func resignKey() {
        super.resignKey()
        // Notify bridge to dismiss
        (delegate as? OakCommandPalette)?.dismiss()
    }
    override func cancelOperation(_ sender: Any?) {
        // Escape key
        (delegate as? OakCommandPalette)?.dismiss()
    }
}
```

**NSPanel configuration:**
- `styleMask: [.borderless, .nonactivatingPanel]`
- `level: .floating`
- `isOpaque: false`, `backgroundColor: .clear`, `hasShadow: true`
- Added as child window: `parentWindow.addChildWindow(panel, ordered: .above)`
- `makeKeyAndOrderFront(nil)` — panel steals key status so the SwiftUI TextField gets first responder
- Dismiss on `resignKey` (click outside) or Escape via `cancelOperation:`

**Panel teardown on each dismiss** (not recycled — follows `OakCompletionPopup.show()` which recreates the panel each time, avoiding stale SwiftUI state and ObservableObject subscription issues):
- `dismiss()`: remove child window, `orderOut(nil)`, nil out panel/viewModel, cancel subscriptions
- `show()`: create fresh panel, viewModel, NSHostingView each time

**Positioning:**
- Horizontally centered in parent window
- Vertically at ~25% from top of parent
- Width: 50% of parent window width, clamped to 400–700pt
- Height: dynamic based on result count, max 10 rows visible

### Keyboard Event Routing

Since the `KeyablePanel` becomes key and the SwiftUI `TextField` becomes first responder inside it, the standard AppKit responder chain handles text input naturally — keystrokes flow to the TextField without custom forwarding.

For navigation keys (arrows, Return, Escape), the ViewModel handles them via SwiftUI's `.onKeyPress` modifier on the root `CommandPaletteView`:

```swift
.onKeyPress(.downArrow) { viewModel.selectNext(); return .handled }
.onKeyPress(.upArrow) { viewModel.selectPrevious(); return .handled }
.onKeyPress(.return) { viewModel.acceptSelection(); return .handled }
.onKeyPress(.escape) { viewModel.requestDismiss(); return .handled }
```

This avoids the `handleKeyEvent:` + keyCode pattern used by `OakCompletionPopup` (which is needed there because the popup doesn't own keyboard focus). Here the panel is key, so SwiftUI's own event system works.

### Item Model: OakCommandPaletteItem

```swift
@MainActor @objc public class OakCommandPaletteItem: NSObject, Identifiable, @unchecked Sendable {
    public let id = UUID()
    @objc public let title: String
    @objc public let subtitle: String           // menu path or project path
    @objc public let keyEquivalent: String       // formatted shortcut (empty if none)
    @objc public let category: Int               // OakCommandPaletteCategory raw value
    @objc public let actionIdentifier: String    // unique ID for frecency + execution dispatch
    @objc public var icon: NSImage?
    @objc public var enabled: Bool = true

    // For menuAction items: store the NSMenuItem reference to extract action/target at execution time
    @objc public weak var sourceMenuItem: NSMenuItem?
}
```

The `actionIdentifier` is a structured string:
- Menu actions: `"menu:<selector_name>"` (e.g. `"menu:toggleSoftWrap:"`)
- Bundle commands: `"bundle:<UUID>"` (e.g. `"bundle:A8F4C3B2-..."`)
- Recent projects: `"project:<path>"` (e.g. `"project:/Users/foo/code/bar"`)
- Symbols: `"symbol:<name>:<line>"` (e.g. `"symbol:viewDidLoad:42"`)
- Settings: `"setting:<key>"` (e.g. `"setting:softWrap"`)

The `sourceMenuItem` weak reference allows `AppController` to call `NSApp.sendAction(item.sourceMenuItem.action, to: item.sourceMenuItem.target, from: self)` without needing to encode selectors as strings.

**Categories** (NS_ENUM exposed to ObjC++):
- `menuAction` (0) — menu items
- `bundleCommand` (1) — bundle commands not already in menus
- `recentProject` (2) — from RecentProjects.db
- `symbol` (3) — document symbols
- `bundleEditor` (4) — grammars/snippets for bundle editor
- `goToLine` (5) — line number target
- `setting` (6) — toggleable setting

### Delegate Protocol

```swift
@MainActor @objc public protocol OakCommandPaletteDelegate: AnyObject {
    func commandPalette(_ palette: OakCommandPalette,
                        didSelectItem item: OakCommandPaletteItem)
    func commandPaletteDidDismiss(_ palette: OakCommandPalette)
    func commandPalette(_ palette: OakCommandPalette,
                        requestItemsForMode mode: Int) -> [OakCommandPaletteItem]
}
```

The `requestItemsForMode:` callback allows lazy data loading. When the user types a prefix to switch modes, the ViewModel calls through the bridge to the delegate, which returns the appropriate items:
- `@` mode → delegate queries the key window's document for symbols
- `~` mode → delegate returns the curated settings list with current values
- `#` mode → delegate queries `bundles::query()` for all bundle items
- Other modes use items already provided at `show:` time

### ViewModel: CommandPaletteViewModel

```swift
@MainActor
class CommandPaletteViewModel: ObservableObject {
    @Published var filterText: String = ""
    @Published private(set) var activeMode: PaletteMode = .recentProjects
    @Published private(set) var filteredItems: [RankedItem] = []
    @Published var selectedIndex: Int = 0

    var onItemSelected: ((OakCommandPaletteItem) -> Void)?
    var onDismiss: (() -> Void)?
    var onModeSwitch: ((PaletteMode) -> [OakCommandPaletteItem])?

    private var itemsByMode: [PaletteMode: [OakCommandPaletteItem]] = [:]
    private var frecencyData: [String: FrecencyEntry] = [:]
}

struct RankedItem: Identifiable {
    let item: OakCommandPaletteItem
    let matchedIndices: [Int]  // character positions that matched the query
    let score: Double          // combined fuzzy + frecency score
    var id: UUID { item.id }
}
```

**PaletteMode** (internal enum):
```swift
enum PaletteMode: String, CaseIterable {
    case recentProjects  // no prefix
    case commands        // >
    case symbols         // @
    case bundleEditor    // #
    case goToLine        // :
    case findInProject   // /
    case settings        // ~

    var prefix: Character? { ... }
    var label: String { ... }       // "Commands", "Symbols", etc.
    var placeholder: String { ... } // search field placeholder

    init?(prefix: Character) { ... }
}
```

**Filter text handling:**
- On `filterText` change, check first character for prefix match via `PaletteMode(prefix:)`
- If prefix found: set `activeMode`, call `onModeSwitch` if items for that mode aren't cached, filter remaining text
- If no prefix: default to `recentProjects` mode
- Backspace past prefix → revert to `recentProjects`

## Fuzzy Matching & Ranking

**Matching:** Use `FuzzyMatcher.score()` (not `.filter()`) from `OakSwiftUI/CompletionPopup/FuzzyMatcher.swift` per item. This returns `FuzzyMatchResult` which includes both the score and `matchedIndices` array. The `RankedItem` wrapper stores these indices for bold rendering in the row view.

**Match highlighting:** The row view builds an `AttributedString` from the title, applying `.bold` weight to characters at `matchedIndices`. This uses character-by-character `Text` concatenation:

```swift
func highlightedTitle(_ title: String, matches: [Int]) -> Text {
    var result = Text("")
    for (i, char) in title.enumerated() {
        let t = Text(String(char))
        result = result + (matches.contains(i) ? t.bold() : t)
    }
    return result
}
```

**Frecency boost:**
- Storage: KVDB at `~/Library/Application Support/TextMate/CommandPalette.db`
- Key: `actionIdentifier` from the item
- Value: serialized via `NSKeyedArchiver` to `Data` (KVDB stores raw bytes), containing `{ count: Int, lastUsed: TimeInterval }`
- On execution: increment count, update lastUsed timestamp
- Score formula: `fuzzyScore * (1.0 + frecencyBoost(count, lastUsed))`
- `frecencyBoost` decays exponentially: `min(count, 20) * exp(-hoursSinceLastUse / 168.0)` (half-life ~1 week)
- The bridge class owns the KVDB handle and updates frecency when `onItemSelected` fires

## Mode-Specific UX Details

### Go to Line (`:`)

When the user types `:`, the results list shows a single live-updating row:

```
┌──────────────────────────────────────────┐
│  [≡]  Go to line 42          (of 1337)  │
└──────────────────────────────────────────┘
```

- As the user types digits after `:`, the row updates to show the target line and total line count
- Non-numeric input after `:` is ignored (only digits are meaningful)
- If the number exceeds document line count, the row shows in a warning style (dimmed)
- Return executes immediately — no fuzzy matching involved

### Find in Project (`/`)

This mode acts as a quick launcher, not a search results viewer:

```
┌──────────────────────────────────────────┐
│  [🔍]  Find "auth" in Project    ⇧⌘F   │
└──────────────────────────────────────────┘
```

- Shows a single row: "Find `<query>` in Project"
- The row updates live as the user types after `/`
- Return dismisses the palette and opens the Find in Project panel (`orderFrontFindPanel:`) with the query string pre-filled in the search field
- No fuzzy matching — the query IS the search term

### Settings (`~`)

Curated hardcoded list of toggleable settings. Initial set:

| Setting | Key | Type | Source |
|---------|-----|------|--------|
| Soft Wrap | `softWrap` | bool | `.tm_properties` |
| Show Invisibles | `showInvisibles` | bool | `.tm_properties` |
| Soft Tabs | `softTabs` | bool | `.tm_properties` |
| Tab Size | `tabSize` | int (2/3/4/8) | `.tm_properties` |
| Spell Checking | `spellChecking` | bool | `.tm_properties` |
| Line Numbers | `showLineNumbers` | bool | NSUserDefaults |
| Font Size | `fontSize` | int | NSUserDefaults |
| Theme | n/a | special | Opens Preferences |

Row display includes current value:

```
┌──────────────────────────────────────────┐
│  [⚙]  Soft Wrap                    ON   │
│  [⚙]  Tab Size                      3   │
│  [⚙]  Show Invisibles             OFF   │
└──────────────────────────────────────────┘
```

**Execution:** Boolean settings toggle immediately. Multi-value settings (Tab Size) cycle through options. The delegate writes to `.tm_properties` (per-project) or NSUserDefaults (global) based on which key it is. Settings that affect the current document use `.tm_properties` via `tm_properties_editor_t`. Global UI settings use NSUserDefaults.

## Visual Design

### Panel Appearance

- `.ultraThinMaterial` background via SwiftUI (consistent with `CompletionListView`)
- `RoundedRectangle(cornerRadius: 6)` clip shape (same as completion popup)
- Shadow from NSPanel (`hasShadow: true`)
- Rounded mask on `NSVisualEffectView` with `.hudWindow` material (same as `OakRenameField`)

### Search Field

```
┌──────────────────────────────────────┐
│  [Commands >] Search commands...     │
└──────────────────────────────────────┘
```

- SwiftUI `TextField` with `.textFieldStyle(.plain)`
- Mode pill: small rounded rectangle with mode label, accent background, shown left of text when a prefix mode is active. Hidden in default mode.
- Placeholder text changes per mode (e.g. "Search commands...", "Go to line...", "Search settings...")
- Font: `theme.fontSize` in monospaced design (matching completion popup)
- Padding: 8pt vertical, 12pt horizontal

### Result Row

```
┌──────────────────────────────────────────┐
│ [icon]  Title Text              ⌘⇧F     │
│         Edit › Format › Format File      │
└──────────────────────────────────────────┘
```

- Row height: `max(theme.fontSize * 2.4, 32)` — taller than completion rows to fit two lines
- Left: SF Symbol per category (`terminal.fill` for commands, `folder.fill` for projects, `number` for symbols, `gearshape` for settings, `text.cursor` for go-to-line, `magnifyingglass` for find, `puzzlepiece.fill` for bundles)
- Primary text: `theme.foregroundColor`, `theme.fontSize`, monospaced design
- Matched characters: `.fontWeight(.bold)` on matching ranges
- Subtitle: `theme.foregroundColor` at 60% opacity, `max(theme.fontSize - 2, 9)`, monospaced design
- Key equivalent (right-aligned): `theme.foregroundColor` at 50% opacity, same size as subtitle
- Selected row: `theme.selectionColor` background with `Color(nsColor: .alternateSelectedControlTextColor)` text
- Disabled items: 40% opacity, still visible but not selectable

### Scrolling

`ScrollViewReader` with `.onChange(of: viewModel.selectedIndex)` to auto-scroll selected item into view, using `.easeOut(duration: 0.1)` animation — identical to completion popup pattern.

## Data Collection (ObjC++ Side)

### Theme Environment

`AppController` creates and owns a shared `OakThemeEnvironment` instance. It listens for theme change notifications and calls `applyTheme:` on changes. This instance is passed to the `OakCommandPalette` at init. (If an `OakThemeEnvironment` already exists on OakTextView, AppController can reuse the one from the key window's text view instead of creating its own.)

### Menu Actions

Recursive walk of `NSApp.mainMenu`. Each `NSMenuItem` with an `action` selector becomes an `OakCommandPaletteItem` with:
- `title`: the menu item's `title`
- `subtitle`: full menu path (e.g. "Edit › Find › Find Next")
- `keyEquivalent`: formatted from `keyEquivalent` + `keyEquivalentModifierMask`
- `sourceMenuItem`: weak reference to the `NSMenuItem` for action/target extraction at execution time
- `actionIdentifier`: `"menu:<selectorName>"` for frecency

Bundle menu items carry a `representedObject` containing the bundle UUID. During the walk, these UUIDs are collected into a set for deduplication against the separate bundle query.

```objcpp
- (NSArray<OakCommandPaletteItem*>*)menuItemsForCommandPalette
{
    NSMutableArray* result = [NSMutableArray array];
    NSMutableSet* bundleUUIDs = [NSMutableSet set];
    [self collectMenuItems:NSApp.mainMenu path:@"" into:result bundleUUIDs:bundleUUIDs];
    return result;
}
```

### Bundle Commands

Query all commands not already in menus:

```objcpp
auto const items = bundles::query(bundles::kFieldAny, "",
    scope::context_t(), bundles::kItemTypeCommand);
// Filter out items whose UUID is already in bundleUUIDs set from menu walk
```

### Recent Projects

Read from KVDB `RecentProjects.db` (this DB already exists — used by `FavoriteChooser` in `Applications/TextMate/src/Favorites.mm`). Sort by `lastRecentlyUsed` descending. Each entry becomes an item with the folder name as title and full path as subtitle.

### Symbols (on-demand via delegate)

When `@` mode activates, `requestItemsForMode:` is called. The delegate obtains the symbol list from the key window's `DocumentWindowController` → document's symbols (same source as `SymbolChooser`).

### Settings (on-demand via delegate)

When `~` mode activates, the delegate returns the hardcoded settings list with current values read from NSUserDefaults and the active document's `.tm_properties`.

## Action Execution (Delegate)

`AppController` implements `OakCommandPaletteDelegate` and dispatches based on `item.category`:

| Category | Execution |
|----------|-----------|
| `menuAction` | `NSApp.sendAction(item.sourceMenuItem.action, to: item.sourceMenuItem.target, from: self)` |
| `bundleCommand` | `performBundleItemWithUUIDStringFrom:` with UUID from `actionIdentifier` |
| `recentProject` | Open project at path extracted from `actionIdentifier` |
| `symbol` | Set document selection to symbol range via key window's DocumentWindowController |
| `bundleEditor` | Open Bundle Editor filtered to item UUID from `actionIdentifier` |
| `goToLine` | `goToLine:` on key window's DocumentWindowController with parsed line number |
| `findInProject` | `orderFrontFindPanel:` + pre-fill search field with query text |
| `setting` | Toggle via NSUserDefaults or `tm_properties_editor_t` depending on the key |

After execution, bridge class updates frecency in `CommandPalette.db`.

## Integration Point

The palette is triggered from `AppController`:

```objcpp
- (IBAction)showCommandPalette:(id)sender
{
#if HAVE_OAK_SWIFTUI
    if(!_commandPalette)
    {
        OakThemeEnvironment* theme = /* obtain from key window's text view or create shared */;
        _commandPalette = [[OakCommandPalette alloc] initWithTheme:theme];
        _commandPalette.delegate = self;
    }

    NSMutableArray* items = [NSMutableArray array];
    NSMutableSet* bundleUUIDs = [NSMutableSet set];
    [items addObjectsFromArray:[self menuItemsForCommandPalette]];
    [items addObjectsFromArray:[self bundleCommandsExcluding:bundleUUIDs]];
    [items addObjectsFromArray:[self recentProjectsForCommandPalette]];

    [_commandPalette show:self.keyWindow items:items];
#endif
}
```

Menu entry in `AppController Menus.mm` under the Navigate menu:

```objcpp
{ "Command Palette", @selector(showCommandPalette:), "P", NSEventModifierFlagCommand|NSEventModifierFlagShift }
```

## Lifecycle

- Singleton `OakCommandPalette` instance on `AppController`, created lazily on first invoke
- Panel is torn down on dismiss (not recycled) — fresh panel + viewModel + NSHostingView each `show:` call. Follows `OakCompletionPopup` pattern to avoid stale SwiftUI state.
- Item data for default modes (menu actions, bundles, projects) collected on each `show:`
- Mode-specific data (symbols, settings) loaded lazily via `requestItemsForMode:` delegate callback
- Frecency KVDB opened once on bridge init, kept open for the app lifetime

## Out of Scope

- Custom user-defined commands in the palette
- Plugin/extension API for third-party palette providers
- Palette customization preferences
- Multi-select / batch execution of palette items
