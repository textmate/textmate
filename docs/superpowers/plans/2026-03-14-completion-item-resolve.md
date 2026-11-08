# completionItem/resolve Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `completionItem/resolve` support so the completion popup shows a documentation side panel with lazily-loaded docs for the highlighted item, and consumes richer insertText/additionalTextEdits from the server.

**Architecture:** On selection change in the completion list (150ms debounce), OakTextView asks LSPManager → LSPClient to send `completionItem/resolve` with the original LSP item dictionary. The response updates `OakCompletionItem.documentation`, which the SwiftUI ViewModel publishes to a new `DocDetailView` rendered beside the completion list inside the same NSPanel.

**Tech Stack:** Objective-C++ (LSPClient, LSPManager, OakTextView), Swift/SwiftUI (OakSwiftUI framework)

---

## File Map

| Action | File | Responsibility |
|--------|------|----------------|
| Modify | `Frameworks/lsp/src/LSPClient.h` | Add `resolveCompletionItem:completion:` declaration, `completionResolveProvider` property |
| Modify | `Frameworks/lsp/src/LSPClient.mm` | Implement resolve request, parse server capability, add `resolveSupport` to client capabilities |
| Modify | `Frameworks/lsp/src/LSPManager.h` | Add `resolveCompletionItem:forDocument:completion:` declaration, capability query |
| Modify | `Frameworks/lsp/src/LSPManager.mm` | Route resolve request to correct LSPClient |
| Modify | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionItem.swift` | Add `documentation`, `originalItem`, `isResolved` properties |
| Create | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/DocDetailView.swift` | SwiftUI view rendering markdown documentation |
| Modify | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionListView.swift` | Wrap in HStack with DocDetailView |
| Modify | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift` | Add selectedItem publisher, debounced resolve trigger, resolve callback |
| Modify | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopup.swift` | Wire resolve callback, resize panel for docs, expose resolve API |
| Modify | `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopupDelegate.swift` | Add resolve request delegate method |
| Modify | `Frameworks/OakTextView/src/OakTextView.mm` | Preserve original LSP item dict, implement resolve delegate, cancel pending resolve on dismiss |

---

## Chunk 1: LSP Protocol Layer (LSPClient + LSPManager)

### Task 1: Add completionItem/resolve to LSPClient

**Files:**
- Modify: `Frameworks/lsp/src/LSPClient.h`
- Modify: `Frameworks/lsp/src/LSPClient.mm`

- [ ] **Step 1: Add property and method declaration to LSPClient.h**

In `LSPClient.h`, add after line 22 (`documentRangeFormattingProvider`):

```objc
@property (nonatomic, readonly) BOOL completionResolveProvider;
```

Add after line 34 (`requestRangeFormattingForURI:...`):

```objc
- (void)resolveCompletionItem:(NSDictionary*)item completion:(void(^)(NSDictionary*))callback;
```

- [ ] **Step 2: Add ivar in LSPClient.mm**

In the `@implementation LSPClient` ivar block (after `_documentRangeFormattingProvider`), add:

```objc
BOOL _completionResolveProvider;
```

- [ ] **Step 3: Parse server capability in initialize response**

In `LSPClient.mm`, inside the capabilities parsing block (after the `_documentRangeFormattingProvider` assignment around line 387), add:

```objc
if(caps.contains("completionProvider") && caps["completionProvider"].contains("resolveProvider"))
	_completionResolveProvider = caps["completionProvider"]["resolveProvider"].get<bool>();
```

Update the log line to include the new capability:

```objc
[self logLSP:@"Capabilities: formatting=%d rangeFormatting=%d completionResolve=%d", _documentFormattingProvider, _documentRangeFormattingProvider, _completionResolveProvider];
```

- [ ] **Step 4: Add resolveSupport to client capabilities**

In `sendInitialize`, inside the `"completionItem"` object (after `"snippetSupport": true`), add:

```cpp
{"resolveSupport", {
	{"properties", {"documentation", "detail", "additionalTextEdits"}}
}}
```

- [ ] **Step 5: Implement resolveCompletionItem:completion:**

Add after the existing `requestCompletionForURI:...` method:

```objc
- (void)resolveCompletionItem:(NSDictionary*)item completion:(void(^)(NSDictionary*))callback
{
	if(!_initialized || !_completionResolveProvider)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = json::parse([NSJSONSerialization dataWithJSONObject:item options:0 error:nil].bytes, nullptr, false);

	[self sendRequest:@"completionItem/resolve" params:params callback:^(id result) {
		if([result isKindOfClass:[NSDictionary class]])
		{
			if(callback)
				callback(result);
		}
		else
		{
			if(callback)
				callback(nil);
		}
	}];
}
```

Note: `json::parse` on the NSJSONSerialization output converts the NSDictionary back to nlohmann::json for the wire format. The `sendRequest:params:callback:` method handles JSON-RPC framing and response deserialization.

- [ ] **Step 6: Preserve full LSP item in completion response**

In `requestCompletionForURI:line:character:completion:`, the current code builds a stripped `suggestion` dictionary. Add the original item so it can be sent back for resolve. After the line `suggestion[@"insertTextFormat"] = insertTextFormat;` add:

```objc
suggestion[@"_originalItem"] = item;
```

This preserves the full server response item (including `data` field that servers use for resolve) in a private key.

- [ ] **Step 7: Build and verify**

Run: `make debug 2>&1 | tail -20`
Expected: Build succeeds, no new errors in lsp framework.

- [ ] **Step 8: Commit**

```
git add Frameworks/lsp/src/LSPClient.h Frameworks/lsp/src/LSPClient.mm
git commit -m "Add completionItem/resolve protocol support to LSPClient"
```

---

### Task 2: Route resolve through LSPManager

**Files:**
- Modify: `Frameworks/lsp/src/LSPManager.h`
- Modify: `Frameworks/lsp/src/LSPManager.mm`

- [ ] **Step 1: Add declarations to LSPManager.h**

After the `requestFormattingForDocument:...` declaration, add:

```objc
- (void)resolveCompletionItem:(NSDictionary*)item forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback;
- (BOOL)serverSupportsCompletionResolveForDocument:(OakDocument*)document;
```

- [ ] **Step 2: Implement routing in LSPManager.mm**

Add after the existing completion routing method:

```objc
- (void)resolveCompletionItem:(NSDictionary*)item forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(nil);
		return;
	}

	[client resolveCompletionItem:item completion:^(NSDictionary* resolved) {
		if(callback)
			callback(resolved);
	}];
}

- (BOOL)serverSupportsCompletionResolveForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.completionResolveProvider;
}
```

- [ ] **Step 3: Build and verify**

Run: `make debug 2>&1 | tail -20`
Expected: Build succeeds.

- [ ] **Step 4: Commit**

```
git add Frameworks/lsp/src/LSPManager.h Frameworks/lsp/src/LSPManager.mm
git commit -m "Add completionItem/resolve routing to LSPManager"
```

---

## Chunk 2: OakSwiftUI Data Layer (OakCompletionItem + ViewModel)

### Task 3: Extend OakCompletionItem with resolve fields

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionItem.swift`

- [ ] **Step 1: Add mutable properties for resolved data**

Add after `@objc public var isSnippet: Bool = false`:

```swift
@objc public var documentation: String?
@objc public var originalItem: NSDictionary?
@objc public var isResolved: Bool = false
```

`documentation` holds the markdown/plaintext docs from resolve. `originalItem` holds the full LSP item dictionary for sending back in the resolve request. `isResolved` prevents re-requesting.

- [ ] **Step 2: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10`
Expected: Build succeeds.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionItem.swift
git commit -m "Add documentation and resolve fields to OakCompletionItem"
```

---

### Task 4: Add resolve trigger to CompletionViewModel

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift`

- [ ] **Step 1: Add resolve callback and debounce timer**

Replace the entire file content:

```swift
import AppKit
import Combine

@MainActor
public class CompletionViewModel: ObservableObject {
	@Published public private(set) var filteredItems: [OakCompletionItem] = []
	@Published public var selectedIndex: Int = 0
	@Published public private(set) var resolvedDocumentation: String?

	private var allItems: [OakCompletionItem] = []
	private var currentFilter: String = ""
	private var resolveTimer: Timer?

	public var onResolveNeeded: ((OakCompletionItem) -> Void)?

	public init() {}

	public func setItems(_ items: [OakCompletionItem]) {
		allItems = items
		applyFilter()
	}

	public func updateFilter(_ text: String) {
		currentFilter = text
		applyFilter()
		selectedIndex = 0
		scheduleResolve()
	}

	public func selectNext() {
		if selectedIndex < filteredItems.count - 1 {
			selectedIndex += 1
			scheduleResolve()
		}
	}

	public func selectPrevious() {
		if selectedIndex > 0 {
			selectedIndex -= 1
			scheduleResolve()
		}
	}

	public var selectedItem: OakCompletionItem? {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return nil }
		return filteredItems[selectedIndex]
	}

	public func resolveCompleted(for item: OakCompletionItem, documentation: String?) {
		item.documentation = documentation
		item.isResolved = true
		if item === selectedItem {
			resolvedDocumentation = documentation
		}
	}

	private func scheduleResolve() {
		resolveTimer?.invalidate()
		resolvedDocumentation = nil

		guard let item = selectedItem else { return }

		if item.isResolved {
			resolvedDocumentation = item.documentation
			return
		}

		resolveTimer = Timer.scheduledTimer(withTimeInterval: 0.15, repeats: false) { [weak self] _ in
			Task { @MainActor in
				guard let self, let current = self.selectedItem, current === item else { return }
				self.onResolveNeeded?(item)
			}
		}
	}

	public func cancelResolve() {
		resolveTimer?.invalidate()
		resolveTimer = nil
	}

	private func applyFilter() {
		filteredItems = FuzzyMatcher.filter(allItems, query: currentFilter, keyPath: \.label)
	}
}
```

Key changes from original:
- `resolvedDocumentation` published property drives the doc panel visibility
- `onResolveNeeded` callback wired by OakCompletionPopup bridge
- `scheduleResolve()` called on every selection change with 150ms debounce
- `resolveCompleted(for:documentation:)` called by bridge when LSP responds
- Already-resolved items show docs immediately (no re-request)

- [ ] **Step 2: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10`
Expected: Build succeeds.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift
git commit -m "Add debounced resolve trigger to CompletionViewModel"
```

---

## Chunk 3: OakSwiftUI UI Layer (DocDetailView + Layout)

### Task 5: Create DocDetailView

**Files:**
- Create: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/DocDetailView.swift`

- [ ] **Step 1: Create the documentation panel view**

```swift
import SwiftUI

struct DocDetailView: View {
	let documentation: String
	@EnvironmentObject var theme: OakThemeEnvironment

	var body: some View {
		ScrollView {
			Text(attributedDocumentation)
				.font(.system(size: max(theme.fontSize - 1, 10)))
				.lineLimit(nil)
				.frame(maxWidth: .infinity, alignment: .leading)
				.padding(10)
		}
		.frame(width: 260)
		.background(.ultraThinMaterial)
	}

	private var attributedDocumentation: AttributedString {
		var result = AttributedString()
		let lines = documentation.components(separatedBy: "\n")
		var inCodeBlock = false

		for (index, line) in lines.enumerated() {
			if index > 0 {
				result.append(AttributedString("\n"))
			}

			if line.hasPrefix("```") {
				inCodeBlock.toggle()
				continue
			}

			if inCodeBlock {
				var codeLine = AttributedString(line)
				codeLine.font = .system(size: theme.fontSize, design: .monospaced)
				codeLine.foregroundColor = .primary
				result.append(codeLine)
			} else {
				result.append(parseInlineMarkdown(line))
			}
		}
		return result
	}

	private func parseInlineMarkdown(_ text: String) -> AttributedString {
		var result = AttributedString()
		var i = text.startIndex

		while i < text.endIndex {
			if text[i] == "`" {
				let codeStart = text.index(after: i)
				if let codeEnd = text[codeStart...].firstIndex(of: "`") {
					var code = AttributedString(String(text[codeStart..<codeEnd]))
					code.font = .system(size: max(theme.fontSize - 1, 10), design: .monospaced)
					code.foregroundColor = .primary
					result.append(code)
					i = text.index(after: codeEnd)
					continue
				}
			}

			if text[i] == "*" && text.index(after: i) < text.endIndex && text[text.index(after: i)] == "*" {
				let boldStart = text.index(i, offsetBy: 2)
				if let range = text[boldStart...].range(of: "**") {
					var bold = AttributedString(String(text[boldStart..<range.lowerBound]))
					bold.font = .system(size: max(theme.fontSize - 1, 10), weight: .bold)
					result.append(bold)
					i = range.upperBound
					continue
				}
			}

			var plain = AttributedString(String(text[i]))
			plain.foregroundColor = .secondary
			result.append(plain)
			i = text.index(after: i)
		}
		return result
	}
}
```

This is a lightweight markdown renderer covering code blocks, inline code, and bold — the formats most LSP servers return in documentation. Matches the style of the existing hover tooltip.

- [ ] **Step 2: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10`
Expected: Build succeeds.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/DocDetailView.swift
git commit -m "Add DocDetailView for completion resolve documentation"
```

---

### Task 6: Integrate DocDetailView into CompletionListView

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionListView.swift`

- [ ] **Step 1: Add HStack layout with conditional doc panel**

Replace the entire file content:

```swift
import SwiftUI

struct CompletionListView: View {
	@ObservedObject var viewModel: CompletionViewModel
	@EnvironmentObject var theme: OakThemeEnvironment

	var body: some View {
		HStack(spacing: 0) {
			itemsList
			if let docs = viewModel.resolvedDocumentation, !docs.isEmpty {
				Divider()
				DocDetailView(documentation: docs)
					.transition(.move(edge: .trailing).combined(with: .opacity))
			}
		}
		.animation(.easeInOut(duration: 0.15), value: viewModel.resolvedDocumentation != nil)
	}

	private var itemsList: some View {
		ScrollViewReader { proxy in
			ScrollView(.vertical) {
				LazyVStack(spacing: 0) {
					ForEach(Array(viewModel.filteredItems.enumerated()), id: \.element.id) { index, item in
						CompletionRowView(
							item: item,
							isSelected: index == viewModel.selectedIndex
						)
						.id(item.id)
						.accessibilityElement(children: .ignore)
						.accessibilityLabel(item.label)
						.accessibilityHint(item.detail)
						.accessibilityAddTraits(index == viewModel.selectedIndex ? .isSelected : [])
					}
				}
				.padding(.vertical, 4)
			}
			.accessibilityElement(children: .contain)
			.onChange(of: viewModel.selectedIndex) { _, newValue in
				guard newValue < viewModel.filteredItems.count else { return }
				withAnimation(.easeOut(duration: 0.1)) {
					proxy.scrollTo(viewModel.filteredItems[newValue].id, anchor: .center)
				}
			}
		}
		.background(.ultraThinMaterial)
		.clipShape(RoundedRectangle(cornerRadius: 6))
	}
}
```

Changes from original: wraps the existing scroll view in an HStack. When `resolvedDocumentation` is non-nil, a `Divider` + `DocDetailView` appear to the right with a slide+fade animation.

- [ ] **Step 2: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10`
Expected: Build succeeds.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionListView.swift
git commit -m "Integrate DocDetailView beside completion list"
```

---

### Task 7: Resize NSPanel when docs appear and wire resolve callback

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopup.swift`
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopupDelegate.swift`

- [ ] **Step 1: Add resolve delegate method**

In `OakCompletionPopupDelegate.swift`, add after `completionPopupDidDismiss`:

```swift
@objc optional func completionPopup(_ popup: OakCompletionPopup, resolveItem item: OakCompletionItem)
```

- [ ] **Step 2: Update OakCompletionPopup to wire resolve and resize**

Replace the entire file content of `OakCompletionPopup.swift`:

```swift
import AppKit
import SwiftUI
import Combine

@MainActor @objc public class OakCompletionPopup: NSObject {
	@objc public weak var delegate: OakCompletionPopupDelegate?

	private var window: NSWindow?
	private var viewModel: CompletionViewModel?
	private let theme: OakThemeEnvironment
	private var baseWidth: CGFloat = 280
	private static let docPanelWidth: CGFloat = 262 // 260 + 2 for divider
	private var cancellables = Set<AnyCancellable>()

	@objc public init(theme: OakThemeEnvironment) {
		self.theme = theme
		super.init()
	}

	@objc public func show(in parentView: NSView, at point: NSPoint, items: [OakCompletionItem]) {
		if let w = window {
			w.parent?.removeChildWindow(w)
			w.orderOut(nil)
			window = nil
			viewModel = nil
			cancellables.removeAll()
		}

		let vm = CompletionViewModel()
		vm.setItems(items)
		vm.onResolveNeeded = { [weak self] item in
			self?.delegate?.completionPopup?(self!, resolveItem: item)
		}
		self.viewModel = vm

		vm.$resolvedDocumentation
			.receive(on: RunLoop.main)
			.sink { [weak self] docs in
				self?.resizePanelForDocs(docs != nil && !(docs?.isEmpty ?? true))
			}
			.store(in: &cancellables)

		let listView = CompletionListView(viewModel: vm)
			.environmentObject(theme)

		let hostingView = NSHostingView(rootView: listView)

		let rowHeight = max(theme.fontSize * 1.8, 22)
		let itemCount = min(items.count, 12)
		let height = CGFloat(itemCount) * rowHeight + 8
		let detailFont = NSFont.systemFont(ofSize: max(theme.fontSize - 2, 9))
		let maxLabelWidth = items.prefix(50).map { ($0.label as NSString).size(withAttributes: [.font: theme.font]).width }.max() ?? 200
		let maxDetailWidth = items.prefix(50).map { ($0.detail as NSString).size(withAttributes: [.font: detailFont]).width }.max() ?? 0
		let width = min(max(maxLabelWidth + maxDetailWidth + 60, 280), 650)
		self.baseWidth = width

		let screenPoint = parentView.window?.convertPoint(toScreen:
			parentView.convert(point, to: nil)) ?? point

		var origin = NSPoint(x: screenPoint.x, y: screenPoint.y - height)
		if let screen = NSScreen.main, origin.y < screen.visibleFrame.minY {
			origin.y = screenPoint.y + 20
		}

		let panel = NSPanel(
			contentRect: NSRect(origin: origin, size: NSSize(width: width, height: height)),
			styleMask: [.borderless, .nonactivatingPanel],
			backing: .buffered,
			defer: false
		)
		panel.level = .floating
		panel.isOpaque = false
		panel.backgroundColor = .clear
		panel.hasShadow = true
		panel.contentView = hostingView

		panel.orderFront(nil)
		parentView.window?.addChildWindow(panel, ordered: .above)
		self.window = panel

		vm.scheduleResolve()
	}

	@objc public func updateFilter(_ text: String) {
		viewModel?.updateFilter(text)
		resizePanelToFit()
	}

	@objc public func resolveCompleted(for item: OakCompletionItem, documentation: String?, insertText: String?) {
		if let newInsert = insertText, !newInsert.isEmpty {
			item.setValue(newInsert, forKey: "insertText")
		}
		viewModel?.resolveCompleted(for: item, documentation: documentation)
	}

	private func resizePanelForDocs(_ hasDocs: Bool) {
		guard let w = window else { return }
		var frame = w.frame
		let targetWidth = hasDocs ? baseWidth + Self.docPanelWidth : baseWidth
		frame.size.width = targetWidth
		w.setFrame(frame, display: true, animate: true)
	}

	private func resizePanelToFit() {
		guard let w = window, let vm = viewModel else { return }
		let rowHeight = max(theme.fontSize * 1.8, 22)
		let itemCount = min(vm.filteredItems.count, 12)
		let newHeight = CGFloat(itemCount) * rowHeight + 8
		var frame = w.frame
		let delta = newHeight - frame.height
		frame.origin.y -= delta
		frame.size.height = newHeight
		w.setFrame(frame, display: true, animate: false)
	}

	@objc public func handleKeyEvent(_ event: NSEvent) -> Bool {
		guard let vm = viewModel else { return false }

		switch event.keyCode {
		case 125: // down arrow
			vm.selectNext()
			return true
		case 126: // up arrow
			vm.selectPrevious()
			return true
		case 36: // return
			if let item = vm.selectedItem {
				delegate?.completionPopup(self, didSelectItem: item)
				dismiss()
			}
			return true
		case 48: // tab
			if let item = vm.selectedItem {
				delegate?.completionPopup(self, didSelectItem: item)
				dismiss()
			}
			return true
		case 53: // escape
			dismiss()
			return true
		default:
			return false
		}
	}

	@objc public func dismiss() {
		if let w = window {
			viewModel?.cancelResolve()
			cancellables.removeAll()
			w.parent?.removeChildWindow(w)
			w.orderOut(nil)
			window = nil
			viewModel = nil
			delegate?.completionPopupDidDismiss(self)
		}
	}

	@objc public var isVisible: Bool {
		window?.isVisible ?? false
	}
}
```

Key changes from original:
- Subscribes to `resolvedDocumentation` to resize panel width when docs arrive
- `onResolveNeeded` wired to delegate call
- `resolveCompleted(for:documentation:insertText:)` public method for bridge to call
- `scheduleResolve()` called on initial show so first selected item triggers resolve
- `cancelResolve()` called on dismiss
- `baseWidth` stored so panel can expand/contract relative to list-only width
- `scheduleResolve()` needs to be made `public` in CompletionViewModel (noted in Task 4 already)

- [ ] **Step 3: Build OakSwiftUI**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10`
Expected: Build succeeds.

- [ ] **Step 4: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopup.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopupDelegate.swift
git commit -m "Wire resolve callback and panel resizing in OakCompletionPopup"
```

---

## Chunk 4: OakTextView Integration

### Task 8: Bridge resolve through OakTextView

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm`

- [ ] **Step 1: Store original LSP item in OakCompletionItem during popup creation**

In `showLSPCompletionPopupWithSuggestions:prefixLength:`, after `item.isSnippet = isSnippet;` (around line 5222), add:

```objc
item.originalItem = s[@"_originalItem"];
```

This passes through the full LSP item dictionary that was preserved in Task 1 Step 6.

- [ ] **Step 2: Add pending resolve request ID ivar**

In the LSP completion ivar block (after `NSString* _lspFilterPrefix;`), add:

```objc
int _lspPendingResolveRequestId;
```

- [ ] **Step 3: Implement the resolve delegate method**

Add after the existing `completionPopupDidDismiss:` method:

```objc
- (void)completionPopup:(OakCompletionPopup*)popup resolveItem:(OakCompletionItem*)item
{
	if(!item.originalItem)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	if(![[LSPManager sharedManager] serverSupportsCompletionResolveForDocument:doc])
		return;

	__weak OakTextView* weakSelf = self;
	[[LSPManager sharedManager] resolveCompletionItem:item.originalItem forDocument:doc completion:^(NSDictionary* resolved) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !resolved)
			return;

		NSString* documentation = nil;
		id docValue = resolved[@"documentation"];
		if([docValue isKindOfClass:[NSString class]])
		{
			documentation = docValue;
		}
		else if([docValue isKindOfClass:[NSDictionary class]])
		{
			documentation = docValue[@"value"];
		}

		NSString* newInsertText = nil;
		if(resolved[@"insertText"])
			newInsertText = resolved[@"insertText"];
		else if(resolved[@"textEdit"] && resolved[@"textEdit"][@"newText"])
			newInsertText = resolved[@"textEdit"][@"newText"];

		dispatch_async(dispatch_get_main_queue(), ^{
			[strongSelf->_lspCompletionPopup resolveCompletedFor:item documentation:documentation insertText:newInsertText];
		});
	}];
}
```

The documentation field in LSP can be either a plain string or a `MarkupContent` object with `kind` and `value` fields. We handle both.

- [ ] **Step 4: Cancel pending resolve on popup dismiss**

The popup's `dismiss` method already calls `viewModel.cancelResolve()` which invalidates the timer. No additional work needed on the OakTextView side since the weak reference pattern handles the callback safely.

- [ ] **Step 5: Build full project**

Run: `make debug 2>&1 | tail -20`
Expected: Build succeeds with no new warnings.

- [ ] **Step 6: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Bridge completionItem/resolve from OakTextView to LSP"
```

---

## Chunk 5: Fix-ups and Visibility

### Task 9: Make scheduleResolve public and fix insertText mutability

**Files:**
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift`
- Modify: `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionItem.swift`

- [ ] **Step 1: Make scheduleResolve public in CompletionViewModel**

Change `private func scheduleResolve()` to `public func scheduleResolve()`.

- [ ] **Step 2: Make insertText mutable for resolve updates**

In `OakCompletionItem.swift`, change:

```swift
@objc public let insertText: String?
```

to:

```swift
@objc public private(set) var insertText: String?
```

And add a setter method:

```swift
@objc public func updateInsertText(_ text: String) {
	insertText = text
}
```

- [ ] **Step 3: Update OakCompletionPopup to use the setter**

In `OakCompletionPopup.swift`, in `resolveCompleted(for:documentation:insertText:)`, replace:

```swift
item.setValue(newInsert, forKey: "insertText")
```

with:

```swift
item.updateInsertText(newInsert)
```

- [ ] **Step 4: Build OakSwiftUI and full project**

Run: `cd Frameworks/OakSwiftUI && swift build 2>&1 | tail -10 && cd ../.. && make debug 2>&1 | tail -20`
Expected: Both builds succeed.

- [ ] **Step 5: Commit**

```
git add Frameworks/OakSwiftUI/Sources/OakSwiftUI/CompletionPopup/CompletionViewModel.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionItem.swift Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/OakCompletionPopup.swift
git commit -m "Fix visibility: public scheduleResolve, mutable insertText"
```

---

### Task 10: Manual testing

- [ ] **Step 1: Build and launch**

Run: `make run`

- [ ] **Step 2: Test with an LSP server that supports resolve**

Open a PHP file (Intelephense supports resolve) or a TypeScript file (tsserver supports resolve).

1. Trigger completion with Opt+Tab
2. Arrow down through items — after ~150ms pause, documentation panel should slide in from the right
3. Arrow to a different item — docs should update (may briefly disappear during debounce)
4. Arrow to a previously-resolved item — docs should appear immediately (cached)
5. Press Escape — popup and docs dismiss cleanly
6. Trigger completion again — fresh state, no stale docs

- [ ] **Step 3: Test edge cases**

1. Server without resolve support (e.g., a basic LSP) — no doc panel should appear, no errors
2. Rapid arrow key scrolling — should not flood server with resolve requests (debounce working)
3. Type characters while docs are visible — re-query should dismiss docs, show new list
4. Select item with resolved richer insertText — verify the updated text is inserted

- [ ] **Step 4: Commit any fixes from testing**

If any issues found during testing, fix and commit with descriptive message.
