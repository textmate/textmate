# Command Palette Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Cmd+Shift+P command palette with prefix-based modes for commands, projects, symbols, bundles, go-to-line, find-in-project, and settings.

**Architecture:** SwiftUI command palette in OakSwiftUI (following OakRenameField's KeyablePanel pattern), bridged to ObjC++ via `OakCommandPalette`. AppController owns the singleton, collects data sources, and dispatches actions via delegate callback. Frecency ranking stored in KVDB.

**Tech Stack:** Swift 6.0 / SwiftUI (macOS 14+), Objective-C++, KVDB (SQLite), OakSwiftUI framework

**Spec:** `docs/superpowers/specs/2026-03-21-command-palette-design.md`

---

### Task 0: Item Model + Delegate Protocol

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteItem.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteDelegate.swift`

- [ ] **Step 1: Create OakCommandPaletteItem**

```swift
import AppKit

@objc public enum OakCommandPaletteCategory: Int, Sendable {
	case menuAction = 0
	case bundleCommand
	case recentProject
	case symbol
	case bundleEditor
	case goToLine
	case findInProject
	case setting
}

@MainActor @objc public class OakCommandPaletteItem: NSObject, Identifiable {
	public let id = UUID()
	@objc public let title: String
	@objc public let subtitle: String
	@objc public let keyEquivalent: String
	@objc public let category: OakCommandPaletteCategory
	@objc public let actionIdentifier: String
	@objc public var icon: NSImage?
	@objc public var enabled: Bool = true
	@objc public weak var sourceMenuItem: NSMenuItem?

	@objc public init(title: String, subtitle: String, keyEquivalent: String,
	                  category: OakCommandPaletteCategory, actionIdentifier: String) {
		self.title = title
		self.subtitle = subtitle
		self.keyEquivalent = keyEquivalent
		self.category = category
		self.actionIdentifier = actionIdentifier
		super.init()
	}

	public var categorySymbolName: String {
		switch category {
		case .menuAction:     return "terminal.fill"
		case .bundleCommand:  return "terminal.fill"
		case .recentProject:  return "folder.fill"
		case .symbol:         return "number"
		case .bundleEditor:   return "puzzlepiece.fill"
		case .goToLine:       return "text.cursor"
		case .findInProject:  return "magnifyingglass"
		case .setting:        return "gearshape"
		@unknown default:     return "questionmark.square"
		}
	}
}
```

Write this to `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteItem.swift`.

- [ ] **Step 2: Create OakCommandPaletteDelegate**

```swift
import AppKit

@MainActor @objc public protocol OakCommandPaletteDelegate: AnyObject {
	func commandPalette(_ palette: OakCommandPalette, didSelectItem item: OakCommandPaletteItem)
	func commandPaletteDidDismiss(_ palette: OakCommandPalette)
	func commandPalette(_ palette: OakCommandPalette, requestItemsForMode mode: Int) -> [OakCommandPaletteItem]
}
```

Write this to `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteDelegate.swift`.

- [ ] **Step 3: Build OakSwiftUI to verify**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -20`

Expected: Build succeeds (OakCommandPalette doesn't exist yet so the delegate reference won't resolve — that's fine, comment out the palette type reference temporarily or use `AnyObject` in the protocol parameter and fix in Task 2).

**Note:** The delegate protocol references `OakCommandPalette` which doesn't exist yet. Use a forward-compatible approach: define the delegate with `NSObject` parameter initially:

```swift
@MainActor @objc public protocol OakCommandPaletteDelegate: AnyObject {
	func commandPaletteDidSelectItem(_ item: OakCommandPaletteItem)
	func commandPaletteDidDismiss()
	func commandPaletteRequestItems(forMode mode: Int) -> [OakCommandPaletteItem]
}
```

This avoids the circular reference. The bridge class will call these directly.

- [ ] **Step 4: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteItem.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteDelegate.swift
git commit -m "Add OakCommandPaletteItem model and delegate protocol"
```

---

### Task 1: ViewModel with Prefix Mode Parsing and Fuzzy Filtering

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift`
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/FuzzyMatcher.swift`
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift`

- [ ] **Step 1: Write test for prefix mode parsing**

Create `Frameworks/OakSwiftUI/Tests/OakSwiftUITests/CommandPaletteViewModelTests.swift`:

```swift
import Testing
@testable import OakSwiftUI

@MainActor
@Suite("CommandPaletteViewModel")
struct CommandPaletteViewModelTests {
	@Test func defaultModeIsRecentProjects() {
		let vm = CommandPaletteViewModel()
		#expect(vm.activeMode == .recentProjects)
	}

	@Test func typingGreaterThanSwitchesToCommandsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ">format"
		#expect(vm.activeMode == .commands)
		#expect(vm.queryText == "format")
	}

	@Test func typingAtSwitchesToSymbolsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "@viewDidLoad"
		#expect(vm.activeMode == .symbols)
		#expect(vm.queryText == "viewDidLoad")
	}

	@Test func typingColonSwitchesToGoToLineMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ":42"
		#expect(vm.activeMode == .goToLine)
		#expect(vm.queryText == "42")
	}

	@Test func typingTildeSwitchesToSettingsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "~wrap"
		#expect(vm.activeMode == .settings)
		#expect(vm.queryText == "wrap")
	}

	@Test func typingSlashSwitchesToFindMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "/auth"
		#expect(vm.activeMode == .findInProject)
		#expect(vm.queryText == "auth")
	}

	@Test func typingHashSwitchesToBundleEditorMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "#ruby"
		#expect(vm.activeMode == .bundleEditor)
		#expect(vm.queryText == "ruby")
	}

	@Test func emptyFilterDefaultsToRecentProjects() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ">test"
		vm.filterText = ""
		#expect(vm.activeMode == .recentProjects)
	}

	@Test func fuzzyFilteringWorks() {
		let vm = CommandPaletteViewModel()
		let items = [
			OakCommandPaletteItem(title: "Format File", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:formatFile:"),
			OakCommandPaletteItem(title: "Find in Project", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:findInProject:"),
			OakCommandPaletteItem(title: "Open Recent", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:openRecent:"),
		]
		vm.setItems(items, forMode: .commands)
		vm.filterText = ">fmt"
		#expect(vm.filteredItems.count == 1)
		#expect(vm.filteredItems.first?.item.title == "Format File")
	}

	@Test func selectionNavigationWorks() {
		let vm = CommandPaletteViewModel()
		let items = [
			OakCommandPaletteItem(title: "Aa", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "a"),
			OakCommandPaletteItem(title: "Bb", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "b"),
			OakCommandPaletteItem(title: "Cc", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "c"),
		]
		vm.setItems(items, forMode: .commands)
		vm.filterText = ">"
		#expect(vm.selectedIndex == 0)
		vm.selectNext()
		#expect(vm.selectedIndex == 1)
		vm.selectNext()
		#expect(vm.selectedIndex == 2)
		vm.selectNext()
		#expect(vm.selectedIndex == 2) // stays at end
		vm.selectPrevious()
		#expect(vm.selectedIndex == 1)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd Frameworks/OakSwiftUI && swift test --filter CommandPaletteViewModelTests 2>&1 | tail -20`
Expected: Compilation error — `CommandPaletteViewModel` not found.

- [ ] **Step 3: Implement CommandPaletteViewModel**

Create `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift`:

```swift
import AppKit
import Combine

enum PaletteMode: String, CaseIterable, Sendable {
	case recentProjects
	case commands
	case symbols
	case bundleEditor
	case goToLine
	case findInProject
	case settings

	var prefix: Character? {
		switch self {
		case .recentProjects: return nil
		case .commands:       return ">"
		case .symbols:        return "@"
		case .bundleEditor:   return "#"
		case .goToLine:       return ":"
		case .findInProject:  return "/"
		case .settings:       return "~"
		}
	}

	var label: String {
		switch self {
		case .recentProjects: return "Recent Projects"
		case .commands:       return "Commands"
		case .symbols:        return "Symbols"
		case .bundleEditor:   return "Bundles"
		case .goToLine:       return "Go to Line"
		case .findInProject:  return "Find"
		case .settings:       return "Settings"
		}
	}

	var placeholder: String {
		switch self {
		case .recentProjects: return "Search recent projects..."
		case .commands:       return "Search commands..."
		case .symbols:        return "Search symbols..."
		case .bundleEditor:   return "Search bundles..."
		case .goToLine:       return "Type a line number..."
		case .findInProject:  return "Search in project..."
		case .settings:       return "Search settings..."
		}
	}

	/// Stable integer for ObjC++ interop (matches OakCommandPaletteCategory ordering)
	var intValue: Int {
		switch self {
		case .recentProjects: return 0
		case .commands:       return 1
		case .symbols:        return 2
		case .bundleEditor:   return 3
		case .goToLine:       return 4
		case .findInProject:  return 5
		case .settings:       return 6
		}
	}

	init?(prefix: Character) {
		guard let mode = PaletteMode.allCases.first(where: { $0.prefix == prefix }) else {
			return nil
		}
		self = mode
	}
}

struct RankedItem: Identifiable {
	let item: OakCommandPaletteItem
	let matchedIndices: [Int]
	let score: Double
	var id: UUID { item.id }
}

@MainActor
class CommandPaletteViewModel: ObservableObject {
	@Published var filterText: String = "" {
		didSet { processFilterChange() }
	}
	@Published private(set) var activeMode: PaletteMode = .recentProjects
	@Published private(set) var filteredItems: [RankedItem] = []
	@Published var selectedIndex: Int = 0

	var onItemSelected: ((OakCommandPaletteItem) -> Void)?
	var onDismiss: (() -> Void)?
	var onModeSwitch: ((PaletteMode) -> [OakCommandPaletteItem])?

	private var itemsByMode: [PaletteMode: [OakCommandPaletteItem]] = [:]

	/// The query text without the prefix character
	var queryText: String {
		guard let prefix = activeMode.prefix, filterText.first == prefix else {
			return filterText
		}
		return String(filterText.dropFirst())
	}

	func setItems(_ items: [OakCommandPaletteItem], forMode mode: PaletteMode) {
		itemsByMode[mode] = items
		if mode == activeMode {
			applyFilter()
		}
	}

	func selectNext() {
		if selectedIndex < filteredItems.count - 1 {
			selectedIndex += 1
		}
	}

	func selectPrevious() {
		if selectedIndex > 0 {
			selectedIndex -= 1
		}
	}

	func acceptSelection() {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return }
		let item = filteredItems[selectedIndex].item
		onItemSelected?(item)
	}

	func requestDismiss() {
		onDismiss?()
	}

	var selectedItem: OakCommandPaletteItem? {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return nil }
		return filteredItems[selectedIndex].item
	}

	private func processFilterChange() {
		let newMode: PaletteMode
		if let first = filterText.first, let mode = PaletteMode(prefix: first) {
			newMode = mode
		} else {
			newMode = .recentProjects
		}

		if newMode != activeMode {
			activeMode = newMode
			if itemsByMode[newMode] == nil, let items = onModeSwitch?(newMode) {
				itemsByMode[newMode] = items
			}
		}

		applyFilter()
		selectedIndex = 0
	}

	private func applyFilter() {
		let query = queryText
		guard let items = itemsByMode[activeMode] else {
			filteredItems = []
			return
		}

		if query.isEmpty {
			filteredItems = items.map { RankedItem(item: $0, matchedIndices: [], score: 0) }
			return
		}

		filteredItems = items
			.compactMap { item -> RankedItem? in
				guard let result = FuzzyMatcher.score(item.title, query: query) else { return nil }
				return RankedItem(item: item, matchedIndices: result.matchedIndices, score: Double(result.score))
			}
			.sorted { $0.score > $1.score }
	}
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd Frameworks/OakSwiftUI && swift test --filter CommandPaletteViewModelTests 2>&1 | tail -20`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift Frameworks/OakSwiftUI/Tests/OakSwiftUITests/CommandPaletteViewModelTests.swift
git commit -m "Add CommandPaletteViewModel with prefix mode parsing and fuzzy filtering"
```

---

### Task 2: SwiftUI Views (Row + List + Root)

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteRowView.swift`
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteView.swift`
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionRowView.swift`
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionListView.swift`

- [ ] **Step 1: Create CommandPaletteRowView**

Create `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteRowView.swift`:

```swift
import SwiftUI

struct CommandPaletteRowView: View {
	let item: OakCommandPaletteItem
	let matchedIndices: [Int]
	let isSelected: Bool
	@EnvironmentObject var theme: OakThemeEnvironment

	var body: some View {
		HStack(spacing: 8) {
			Image(systemName: item.categorySymbolName)
				.font(.system(size: max(theme.fontSize - 2, 9)))
				.foregroundStyle(isSelected ? Color(nsColor: .alternateSelectedControlTextColor) : .secondary)
				.frame(width: 20, alignment: .center)

			VStack(alignment: .leading, spacing: 1) {
				highlightedTitle(item.title, matches: matchedIndices)
					.font(.system(size: theme.fontSize, design: .monospaced))
					.foregroundStyle(isSelected ? Color(nsColor: .alternateSelectedControlTextColor) : .primary)
					.lineLimit(1)

				if !item.subtitle.isEmpty {
					Text(item.subtitle)
						.font(.system(size: max(theme.fontSize - 2, 9), design: .monospaced))
						.foregroundStyle(isSelected
							? Color(nsColor: .alternateSelectedControlTextColor).opacity(0.7)
							: .secondary)
						.lineLimit(1)
				}
			}

			Spacer(minLength: 4)

			if !item.keyEquivalent.isEmpty {
				Text(item.keyEquivalent)
					.font(.system(size: max(theme.fontSize - 2, 9)))
					.foregroundStyle(isSelected
						? Color(nsColor: .alternateSelectedControlTextColor).opacity(0.7)
						: Color(nsColor: theme.foregroundColor).opacity(0.5))
					.lineLimit(1)
			}
		}
		.padding(.horizontal, 8)
		.padding(.vertical, 4)
		.frame(height: max(theme.fontSize * 2.4, 32))
		.background(isSelected ? Color.accentColor : Color.clear)
		.cornerRadius(4)
		.opacity(item.enabled ? 1.0 : 0.4)
	}

	private func highlightedTitle(_ title: String, matches: [Int]) -> Text {
		guard !matches.isEmpty else { return Text(title) }
		var result = Text("")
		for (i, char) in title.enumerated() {
			let t = Text(String(char))
			result = result + (matches.contains(i) ? t.bold() : t)
		}
		return result
	}
}
```

- [ ] **Step 2: Create CommandPaletteView**

Create `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteView.swift`:

```swift
import SwiftUI

struct CommandPaletteView: View {
	@ObservedObject var viewModel: CommandPaletteViewModel
	@EnvironmentObject var theme: OakThemeEnvironment
	@FocusState private var isSearchFieldFocused: Bool

	var body: some View {
		VStack(spacing: 0) {
			searchField
			Divider()
			resultsList
		}
		.background(.ultraThinMaterial)
		.clipShape(RoundedRectangle(cornerRadius: 6))
		.onKeyPress(.downArrow) { viewModel.selectNext(); return .handled }
		.onKeyPress(.upArrow) { viewModel.selectPrevious(); return .handled }
		.onKeyPress(.return) { viewModel.acceptSelection(); return .handled }
		.onKeyPress(.escape) { viewModel.requestDismiss(); return .handled }
		.onAppear { isSearchFieldFocused = true }
	}

	private var searchField: some View {
		HStack(spacing: 6) {
			if viewModel.activeMode != .recentProjects {
				modePill
			}
			TextField(viewModel.activeMode.placeholder, text: $viewModel.filterText)
				.textFieldStyle(.plain)
				.font(.system(size: theme.fontSize, design: .monospaced))
				.focused($isSearchFieldFocused)
		}
		.padding(.horizontal, 12)
		.padding(.vertical, 8)
	}

	private var modePill: some View {
		Text(viewModel.activeMode.label)
			.font(.system(size: max(theme.fontSize - 2, 9), weight: .medium))
			.padding(.horizontal, 6)
			.padding(.vertical, 2)
			.background(Color.accentColor.opacity(0.2))
			.foregroundStyle(Color.accentColor)
			.clipShape(RoundedRectangle(cornerRadius: 4))
	}

	private var resultsList: some View {
		ScrollViewReader { proxy in
			ScrollView(.vertical) {
				LazyVStack(spacing: 0) {
					ForEach(Array(viewModel.filteredItems.enumerated()), id: \.element.id) { index, ranked in
						CommandPaletteRowView(
							item: ranked.item,
							matchedIndices: ranked.matchedIndices,
							isSelected: index == viewModel.selectedIndex
						)
						.id(ranked.id)
						.contentShape(Rectangle())
						.onTapGesture {
							viewModel.selectedIndex = index
							viewModel.acceptSelection()
						}
					}
				}
				.padding(.vertical, 4)
				.padding(.horizontal, 4)
			}
			.frame(maxHeight: max(theme.fontSize * 2.4, 32) * 10 + 8)
			.onChange(of: viewModel.selectedIndex) { _, newValue in
				guard newValue < viewModel.filteredItems.count else { return }
				withAnimation(.easeOut(duration: 0.1)) {
					proxy.scrollTo(viewModel.filteredItems[newValue].id, anchor: .center)
				}
			}
		}
	}
}
```

- [ ] **Step 3: Build OakSwiftUI to verify**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -20`
Expected: Build succeeds.

- [ ] **Step 4: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteRowView.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteView.swift
git commit -m "Add CommandPaletteView and CommandPaletteRowView SwiftUI views"
```

---

### Task 3: Bridge Class (OakCommandPalette)

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPalette.swift`
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakRenameField.swift` (KeyablePanel pattern)
- Reference: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopup.swift` (show/dismiss lifecycle)

- [ ] **Step 1: Create OakCommandPalette bridge class**

Create `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPalette.swift`:

```swift
import AppKit
import SwiftUI
import Combine

private class KeyablePanel: NSPanel {
	override var canBecomeKey: Bool { true }
}

@MainActor @objc public class OakCommandPalette: NSObject, NSWindowDelegate {
	@objc public weak var delegate: OakCommandPaletteDelegate?

	private let theme: OakThemeEnvironment
	private var panel: KeyablePanel?
	private var viewModel: CommandPaletteViewModel?
	private var cancellables = Set<AnyCancellable>()

	@objc public init(theme: OakThemeEnvironment) {
		self.theme = theme
		super.init()
	}

	@objc public func show(in parentWindow: NSWindow, items: [OakCommandPaletteItem]) {
		dismiss()

		let vm = CommandPaletteViewModel()
		vm.setItems(items, forMode: .recentProjects)

		// Separate commands and projects from the flat items array
		let commands = items.filter {
			$0.category == .menuAction || $0.category == .bundleCommand
		}
		vm.setItems(commands, forMode: .commands)

		let projects = items.filter { $0.category == .recentProject }
		vm.setItems(projects, forMode: .recentProjects)

		vm.onItemSelected = { [weak self] item in
			self?.dismiss()
			self?.delegate?.commandPaletteDidSelectItem(item)
		}
		vm.onDismiss = { [weak self] in
			self?.dismiss()
		}
		vm.onModeSwitch = { [weak self] mode in
			self?.delegate?.commandPaletteRequestItems(forMode: mode.intValue) ?? []
		}

		self.viewModel = vm

		let rootView = CommandPaletteView(viewModel: vm)
			.environmentObject(theme)

		let hostingView = NSHostingView(rootView: rootView)

		let parentFrame = parentWindow.frame
		let panelWidth = min(max(parentFrame.width * 0.5, 400), 700)
		let panelHeight: CGFloat = 400

		let panelX = parentFrame.origin.x + (parentFrame.width - panelWidth) / 2
		let panelY = parentFrame.origin.y + parentFrame.height * 0.75 - panelHeight / 2

		let panelFrame = NSRect(x: panelX, y: panelY, width: panelWidth, height: panelHeight)

		let p = KeyablePanel(
			contentRect: panelFrame,
			styleMask: [.borderless, .nonactivatingPanel],
			backing: .buffered,
			defer: false
		)
		p.level = .floating
		p.isOpaque = false
		p.backgroundColor = .clear
		p.hasShadow = true
		p.contentView = hostingView
		p.delegate = self

		parentWindow.addChildWindow(p, ordered: .above)
		p.makeKeyAndOrderFront(nil)

		self.panel = p
	}

	@objc public func dismiss() {
		guard let p = panel else { return }
		p.parent?.removeChildWindow(p)
		p.orderOut(nil)
		panel = nil
		viewModel = nil
		cancellables.removeAll()
		delegate?.commandPaletteDidDismiss()
	}

	@objc public var isVisible: Bool {
		panel?.isVisible ?? false
	}

	// MARK: - NSWindowDelegate

	nonisolated public func windowDidResignKey(_ notification: Notification) {
		MainActor.assumeIsolated {
			dismiss()
		}
	}
}
```

- [ ] **Step 2: Update delegate protocol to remove circular reference**

Update `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteDelegate.swift` — now that `OakCommandPalette` exists, we can reference it if desired, but the simplified protocol without palette parameter is cleaner:

```swift
import AppKit

@MainActor @objc public protocol OakCommandPaletteDelegate: AnyObject {
	func commandPaletteDidSelectItem(_ item: OakCommandPaletteItem)
	func commandPaletteDidDismiss()
	func commandPaletteRequestItems(forMode mode: Int) -> [OakCommandPaletteItem]
}
```

- [ ] **Step 3: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -20`
Expected: Build succeeds.

- [ ] **Step 4: Run all tests**

Run: `cd Frameworks/OakSwiftUI && swift test 2>&1 | tail -20`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPalette.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPaletteDelegate.swift
git commit -m "Add OakCommandPalette bridge class with KeyablePanel"
```

---

### Task 4: AppController Integration — Menu Entry + Data Collection

**Files:**
- Modify: `Applications/TextMate/src/AppController.h` (add `showCommandPalette:` declaration)
- Modify: `Applications/TextMate/src/AppController.mm` (add menu entry at line ~301, implement show + data collection + delegate)

- [ ] **Step 1: Add IBAction declaration to AppController.h**

In `Applications/TextMate/src/AppController.h`, after line 25 (`showBundleItemChooser:`), add:

```objcpp
- (IBAction)showCommandPalette:(id)sender;
```

- [ ] **Step 2: Add menu entry to Navigate menu**

In `Applications/TextMate/src/AppController.mm`, in the Navigate menu submenu (around line 301), add a new entry before "Jump to Line...":

```objcpp
{ @"Command Palette…", @selector(showCommandPalette:), @"P", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagShift },
{ /* -------- */ },
```

This goes as the first item in the Navigate submenu, before "Jump to Line…".

- [ ] **Step 3: Add OakSwiftUI imports and ivar**

At the top of `AppController.mm`, add the conditional import (find the existing `#if HAVE_OAK_SWIFTUI` import pattern used in OakTextView.mm and replicate):

```objcpp
#if HAVE_OAK_SWIFTUI
#import <OakSwiftUI/OakSwiftUI-Swift.h>
#endif
```

In the `@implementation AppController` section, add an ivar or use a static variable:

```objcpp
#if HAVE_OAK_SWIFTUI
static OakCommandPalette* sharedCommandPalette;
static OakThemeEnvironment* sharedThemeEnvironment;
#endif
```

- [ ] **Step 4: Implement showCommandPalette: and data collection**

Add to `AppController.mm`:

```objcpp
// ========================
// = Command Palette      =
// ========================

#if HAVE_OAK_SWIFTUI
static NSString* formattedKeyEquivalent (NSMenuItem* item)
{
	NSString* key = item.keyEquivalent;
	if(key.length == 0)
		return @"";

	NSMutableString* result = [NSMutableString string];
	NSEventModifierFlags flags = item.keyEquivalentModifierMask;
	if(flags & NSEventModifierFlagControl)  [result appendString:@"\u2303"];
	if(flags & NSEventModifierFlagOption)   [result appendString:@"\u2325"];
	if(flags & NSEventModifierFlagShift)    [result appendString:@"\u21E7"];
	if(flags & NSEventModifierFlagCommand)  [result appendString:@"\u2318"];
	[result appendString:key.uppercaseString];
	return result;
}

- (void)collectMenuItems:(NSMenu*)menu path:(NSString*)path into:(NSMutableArray<OakCommandPaletteItem*>*)result bundleUUIDs:(NSMutableSet<NSString*>*)uuids
{
	for(NSMenuItem* item in menu.itemArray)
	{
		if(item.isSeparatorItem || item.isHidden || item.title.length == 0)
			continue;

		NSString* itemPath = path.length
			? [NSString stringWithFormat:@"%@ \u203A %@", path, item.title]
			: item.title;

		if(item.hasSubmenu)
		{
			[self collectMenuItems:item.submenu path:itemPath into:result bundleUUIDs:uuids];
		}
		else if(item.action)
		{
			OakCommandPaletteItem* paletteItem = [[OakCommandPaletteItem alloc]
				initWithTitle:item.title
				     subtitle:path
				keyEquivalent:formattedKeyEquivalent(item)
				     category:OakCommandPaletteCategoryMenuAction
			     actionIdentifier:[NSString stringWithFormat:@"menu:%@", NSStringFromSelector(item.action)]];
			paletteItem.sourceMenuItem = item;
			paletteItem.enabled = item.isEnabled;
			[result addObject:paletteItem];

			if(item.representedObject && [item.representedObject isKindOfClass:[NSString class]])
				[uuids addObject:item.representedObject];
		}
	}
}

- (NSArray<OakCommandPaletteItem*>*)recentProjectsForCommandPalette
{
	NSMutableArray* result = [NSMutableArray array];
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	KVDB* db = [KVDB sharedDBUsingFile:@"RecentProjects.db" inDirectory:appSupport];

	for(NSDictionary* pair in db.allObjects)
	{
		NSString* path = pair[@"key"];
		if(!path || ![NSFileManager.defaultManager fileExistsAtPath:path])
			continue;

		OakCommandPaletteItem* item = [[OakCommandPaletteItem alloc]
			initWithTitle:path.lastPathComponent
			     subtitle:[path stringByAbbreviatingWithTildeInPath]
			keyEquivalent:@""
			     category:OakCommandPaletteCategoryRecentProject
		     actionIdentifier:[NSString stringWithFormat:@"project:%@", path]];
		item.icon = [NSWorkspace.sharedWorkspace iconForFile:path];
		[result addObject:item];
	}
	return result;
}
#endif

- (IBAction)showCommandPalette:(id)sender
{
#if HAVE_OAK_SWIFTUI
	NSWindow* keyWindow = NSApp.keyWindow;
	if(!keyWindow)
		return;

	if(!sharedThemeEnvironment)
		sharedThemeEnvironment = [[OakThemeEnvironment alloc] init];

	if(!sharedCommandPalette)
	{
		sharedCommandPalette = [[OakCommandPalette alloc] initWithTheme:sharedThemeEnvironment];
		sharedCommandPalette.delegate = (id<OakCommandPaletteDelegate>)self;
	}

	NSMutableArray<OakCommandPaletteItem*>* items = [NSMutableArray array];
	NSMutableSet<NSString*>* bundleUUIDs = [NSMutableSet set];
	[self collectMenuItems:NSApp.mainMenu path:@"" into:items bundleUUIDs:bundleUUIDs];
	[items addObjectsFromArray:[self recentProjectsForCommandPalette]];

	[sharedCommandPalette showIn:keyWindow items:items];
#endif
}
```

- [ ] **Step 5: Implement OakCommandPaletteDelegate methods**

Add to `AppController.mm`:

```objcpp
#if HAVE_OAK_SWIFTUI
- (void)commandPaletteDidSelectItem:(OakCommandPaletteItem*)item
{
	switch(item.category)
	{
		case OakCommandPaletteCategoryMenuAction:
		{
			NSMenuItem* menuItem = item.sourceMenuItem;
			if(menuItem && menuItem.action)
				[NSApp sendAction:menuItem.action to:menuItem.target from:self];
			break;
		}
		case OakCommandPaletteCategoryBundleCommand:
		{
			NSString* uuid = [item.actionIdentifier stringByReplacingOccurrencesOfString:@"bundle:" withString:@""];
			NSMenuItem* fakeItem = [[NSMenuItem alloc] init];
			fakeItem.representedObject = uuid;
			[self performBundleItemWithUUIDStringFrom:fakeItem];
			break;
		}
		case OakCommandPaletteCategoryRecentProject:
		{
			NSString* path = [item.actionIdentifier stringByReplacingOccurrencesOfString:@"project:" withString:@""];
			OakOpenDocuments(@[path]);
			break;
		}
		case OakCommandPaletteCategoryGoToLine:
		{
			NSString* lineStr = [item.actionIdentifier stringByReplacingOccurrencesOfString:@"line:" withString:@""];
			NSInteger lineNumber = lineStr.integerValue;
			if(lineNumber > 0)
			{
				NSString* selStr = [NSString stringWithFormat:@"%ld", (long)lineNumber];
				id target = [NSApp targetForAction:@selector(setSelectionString:)];
				if([target respondsToSelector:@selector(setSelectionString:)])
					[target performSelector:@selector(setSelectionString:) withObject:selStr];
			}
			break;
		}
		case OakCommandPaletteCategoryFindInProject:
		{
			NSString* query = [item.actionIdentifier stringByReplacingOccurrencesOfString:@"find:" withString:@""];
			// Set the find pasteboard so the Find panel picks it up
			[[NSPasteboard pasteboardWithName:NSPasteboardNameFind] clearContents];
			[[NSPasteboard pasteboardWithName:NSPasteboardNameFind] setString:query forType:NSPasteboardTypeString];
			[self orderFrontFindPanel:self];
			break;
		}
		default:
			break;
	}
}

- (void)commandPaletteDidDismiss
{
}

- (NSArray<OakCommandPaletteItem*>*)commandPaletteRequestItemsForMode:(NSInteger)mode
{
	return @[];
}
#endif
```

- [ ] **Step 6: Add KVDB import**

Ensure the KVDB header is imported at the top of AppController.mm:

```objcpp
#import <kvdb/kvdb.h>
```

Check if it's already imported; if not, add it.

- [ ] **Step 7: Build the full project**

Run: `make 2>&1 | tail -30`
Expected: Build succeeds. The command palette should be accessible via Navigate > Command Palette (Cmd+Shift+P).

- [ ] **Step 8: Commit**

```
git add Applications/TextMate/src/AppController.h Applications/TextMate/src/AppController.mm
git commit -m "Integrate command palette into AppController with menu entry and data collection"
```

---

### Task 5: Mode-Specific Providers (Symbols, Settings, GoToLine, FindInProject, BundleEditor)

**Files:**
- Modify: `Applications/TextMate/src/AppController.mm` (expand `commandPaletteRequestItemsForMode:`)
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift` (add GoToLine + FindInProject synthetic items)

- [ ] **Step 1: Add GoToLine synthetic item generation to ViewModel**

In `CommandPaletteViewModel.swift`, add a method for generating go-to-line items:

```swift
private func applyFilter() {
	let query = queryText
	guard let items = itemsByMode[activeMode] else {
		// For goToLine and findInProject, generate synthetic items
		if activeMode == .goToLine {
			filteredItems = goToLineItems(query: query)
			return
		}
		if activeMode == .findInProject {
			filteredItems = findInProjectItems(query: query)
			return
		}
		filteredItems = []
		return
	}

	if query.isEmpty {
		filteredItems = items.map { RankedItem(item: $0, matchedIndices: [], score: 0) }
		return
	}

	filteredItems = items
		.compactMap { item -> RankedItem? in
			guard let result = FuzzyMatcher.score(item.title, query: query) else { return nil }
			return RankedItem(item: item, matchedIndices: result.matchedIndices, score: Double(result.score))
		}
		.sorted { $0.score > $1.score }
}

private func goToLineItems(query: String) -> [RankedItem] {
	let lineStr = query.isEmpty ? "..." : query
	let title = "Go to line \(lineStr)"
	let item = OakCommandPaletteItem(
		title: title, subtitle: "", keyEquivalent: "",
		category: .goToLine, actionIdentifier: "line:\(query)")
	return [RankedItem(item: item, matchedIndices: [], score: 0)]
}

private func findInProjectItems(query: String) -> [RankedItem] {
	let searchTerm = query.isEmpty ? "..." : "\"\(query)\""
	let title = "Find \(searchTerm) in Project"
	let item = OakCommandPaletteItem(
		title: title, subtitle: "", keyEquivalent: "\u{21E7}\u{2318}F",
		category: .findInProject, actionIdentifier: "find:\(query)")
	return [RankedItem(item: item, matchedIndices: [], score: 0)]
}
```

- [ ] **Step 2: Add Settings items to delegate**

In `AppController.mm`, expand `commandPaletteRequestItemsForMode:`:

```objcpp
- (NSArray<OakCommandPaletteItem*>*)commandPaletteRequestItemsForMode:(NSInteger)mode
{
	// Mode values correspond to PaletteMode enum
	// settings = 6 (from PaletteMode.settings.rawValue hash)
	// For now, return a hardcoded settings list
	NSMutableArray* result = [NSMutableArray array];

	// Ordered settings list
	NSArray<NSArray<NSString*>*>* settings = @[
		@[@"Soft Wrap",         @"softWrap"],
		@[@"Show Invisibles",   @"showInvisibles"],
		@[@"Soft Tabs",         @"softTabs"],
		@[@"Spell Checking",    @"spellChecking"],
		@[@"Show Line Numbers", @"showLineNumbers"],
	];

	for(NSArray<NSString*>* pair in settings)
	{
		NSString* title = pair[0];
		NSString* key = pair[1];
		OakCommandPaletteItem* item = [[OakCommandPaletteItem alloc]
			initWithTitle:title
			     subtitle:@""
			keyEquivalent:@""
			     category:OakCommandPaletteCategorySetting
		     actionIdentifier:[NSString stringWithFormat:@"setting:%@", key]];
		[result addObject:item];
	}
	return result;
}
```

- [ ] **Step 3: Build and test**

Run: `make 2>&1 | tail -30`
Expected: Build succeeds.

- [ ] **Step 4: Commit**

```
git add Applications/TextMate/src/AppController.mm Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift
git commit -m "Add mode-specific providers for GoToLine, FindInProject, and Settings"
```

---

### Task 6: Frecency Ranking

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift` (add frecency scoring)
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPalette.swift` (add KVDB frecency storage)

- [ ] **Step 1: Write frecency tests**

Add to `Frameworks/OakSwiftUI/Tests/OakSwiftUITests/CommandPaletteViewModelTests.swift`:

```swift
@Test func frecencyBoostIncreasesScoreForRecentItems() {
	let vm = CommandPaletteViewModel()
	let now = Date().timeIntervalSinceReferenceDate

	vm.updateFrecency(for: "menu:formatFile:", at: now)
	vm.updateFrecency(for: "menu:formatFile:", at: now)
	vm.updateFrecency(for: "menu:formatFile:", at: now)

	let boost = vm.frecencyBoost(for: "menu:formatFile:")
	#expect(boost > 0)
}

@Test func frecencyBoostDecaysOverTime() {
	let vm = CommandPaletteViewModel()
	let now = Date().timeIntervalSinceReferenceDate
	let oneWeekAgo = now - 168 * 3600

	vm.updateFrecency(for: "old", at: oneWeekAgo)
	vm.updateFrecency(for: "recent", at: now)

	let oldBoost = vm.frecencyBoost(for: "old")
	let recentBoost = vm.frecencyBoost(for: "recent")
	#expect(recentBoost > oldBoost)
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd Frameworks/OakSwiftUI && swift test --filter CommandPaletteViewModelTests 2>&1 | tail -20`
Expected: Compilation error — `updateFrecency` and `frecencyBoost` not found.

- [ ] **Step 3: Implement frecency in ViewModel**

Add to `CommandPaletteViewModel.swift`:

```swift
struct FrecencyEntry {
	var count: Int
	var lastUsed: TimeInterval
}

// Add to CommandPaletteViewModel class:
private(set) var frecencyData: [String: FrecencyEntry] = [:]

func updateFrecency(for identifier: String, at timestamp: TimeInterval? = nil) {
	let ts = timestamp ?? Date().timeIntervalSinceReferenceDate
	var entry = frecencyData[identifier] ?? FrecencyEntry(count: 0, lastUsed: ts)
	entry.count += 1
	entry.lastUsed = ts
	frecencyData[identifier] = entry
}

func frecencyBoost(for identifier: String) -> Double {
	guard let entry = frecencyData[identifier] else { return 0 }
	let now = Date().timeIntervalSinceReferenceDate
	let hoursSinceLastUse = (now - entry.lastUsed) / 3600.0
	return Double(min(entry.count, 20)) * exp(-hoursSinceLastUse / 168.0)
}

func loadFrecency(_ data: [String: FrecencyEntry]) {
	frecencyData = data
}
```

Update `applyFilter()` to incorporate frecency in scoring:

```swift
// In the compactMap block, change score calculation:
let frecency = frecencyBoost(for: item.actionIdentifier)
let combinedScore = Double(result.score) * (1.0 + frecency)
return RankedItem(item: item, matchedIndices: result.matchedIndices, score: combinedScore)
```

- [ ] **Step 4: Run tests**

Run: `cd Frameworks/OakSwiftUI && swift test --filter CommandPaletteViewModelTests 2>&1 | tail -20`
Expected: All tests pass.

- [ ] **Step 5: Add KVDB frecency persistence to bridge class**

In `OakCommandPalette.swift`, add frecency load/save using the delegate's KVDB (or a new one). Since KVDB is an ObjC class and we're in Swift, add helper methods:

```swift
// Add to OakCommandPalette:
@objc public func recordUsage(forIdentifier identifier: String) {
	viewModel?.updateFrecency(for: identifier)
	// Persist via delegate or direct KVDB call
}
```

The actual KVDB persistence will be handled on the ObjC++ side in `AppController.mm`:

```objcpp
static KVDB* commandPaletteFrecencyDB ()
{
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	return [KVDB sharedDBUsingFile:@"CommandPalette.db" inDirectory:appSupport];
}

- (void)commandPaletteDidSelectItem:(OakCommandPaletteItem*)item
{
	// ... existing dispatch code ...

	// Update frecency
	[self updateCommandPaletteFrecency:item.actionIdentifier];
}

- (void)updateCommandPaletteFrecency:(NSString*)identifier
{
	KVDB* db = commandPaletteFrecencyDB();
	NSDictionary* existing = [db objectForKey:identifier];
	NSInteger count = [existing[@"count"] integerValue] + 1;
	NSTimeInterval now = [NSDate date].timeIntervalSinceReferenceDate;
	[db setObject:@{ @"count": @(count), @"lastUsed": @(now) } forKey:identifier];
}
```

Also update `showCommandPalette:` to load frecency data into the ViewModel. Add this to the bridge class API:

```swift
// In OakCommandPalette, add:
@objc public func loadFrecencyData(_ data: NSDictionary) {
	var entries: [String: FrecencyEntry] = [:]
	for case let (key as String, value as NSDictionary) in data {
		let count = (value["count"] as? Int) ?? 0
		let lastUsed = (value["lastUsed"] as? TimeInterval) ?? 0
		entries[key] = FrecencyEntry(count: count, lastUsed: lastUsed)
	}
	viewModel?.loadFrecency(entries)
}
```

And in `showCommandPalette:` in AppController.mm, after `[sharedCommandPalette showIn:...]`:

```objcpp
// Load frecency data
KVDB* frecencyDB = commandPaletteFrecencyDB();
NSMutableDictionary* frecencyData = [NSMutableDictionary dictionary];
for(NSDictionary* pair in frecencyDB.allObjects)
{
	NSString* key = pair[@"key"];
	id value = pair[@"value"];
	if(key && value)
		frecencyData[key] = value;
}
[sharedCommandPalette loadFrecencyData:frecencyData];
```

- [ ] **Step 6: Build full project**

Run: `make 2>&1 | tail -30`
Expected: Build succeeds.

- [ ] **Step 7: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CommandPalette/CommandPaletteViewModel.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCommandPalette.swift Frameworks/OakSwiftUI/Tests/OakSwiftUITests/CommandPaletteViewModelTests.swift Applications/TextMate/src/AppController.mm
git commit -m "Add frecency ranking with KVDB persistence"
```

---

### Task 7: Polish and Manual Testing

**Files:**
- Possibly modify any files from previous tasks for bug fixes

- [ ] **Step 1: Build release and launch**

Run: `make run`
Expected: TextMate launches.

- [ ] **Step 2: Manual test — open Command Palette**

Press Cmd+Shift+P. Verify:
- Floating panel appears centered in window
- Search field has focus
- Menu actions are listed
- Recent projects are listed

- [ ] **Step 3: Manual test — prefix modes**

Type `>` — verify mode pill shows "Commands", results filter to menu actions.
Type `@` — verify mode pill shows "Symbols" (may be empty if no document open).
Type `:42` — verify "Go to line 42" synthetic item appears.
Type `/test` — verify "Find "test" in Project" synthetic item appears.
Type `~` — verify settings items appear.
Type `#` — verify bundle items appear.

- [ ] **Step 4: Manual test — execution**

Select a menu action (e.g. "Show Preferences") — verify it executes.
Select a recent project — verify it opens.
Select "Go to line 42" — verify cursor moves.

- [ ] **Step 5: Manual test — dismiss**

Press Escape — verify palette dismisses.
Click outside palette — verify it dismisses.

- [ ] **Step 6: Fix any issues found, build, commit**

```
git add -u
git commit -m "Fix command palette issues found during manual testing"
```

- [ ] **Step 7: Run all OakSwiftUI tests**

Run: `cd Frameworks/OakSwiftUI && swift test 2>&1 | tail -20`
Expected: All tests pass.

- [ ] **Step 8: Final commit if needed**

Only if fixes were made in step 6.
