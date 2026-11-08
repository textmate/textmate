# Split Window Support — Design Spec

**Date:** 2026-03-14
**Target:** TextMate (macOS, Objective-C++)
**Branch:** TBD (feature/split-windows)

---

## 1. Overview

Add split window support to TextMate, allowing users to view and edit multiple documents (or the same document) side-by-side within a single window. The implementation uses a flat grid model (up to 3 columns OR up to 3 rows) with per-pane tab bars and independent document tracking per pane.

---

## 2. Current Architecture

### View Hierarchy

```
NSWindow (DocumentWindowController.window)
├── contentView
│   └── ProjectLayoutView
│       ├── OakDocumentView (single, always)
│       │   ├── gutterScrollView (GutterView)
│       │   ├── textScrollView (containing OakTextView)
│       │   └── OTVStatusBar
│       ├── fileBrowserDivider
│       ├── fileBrowserView (optional)
│       ├── htmlOutputDivider
│       └── htmlOutputView (optional)
├── TitlebarAccessoryViewController
│   └── OakTabBarView (in titlebar)
```

### Document Model

- `DocumentWindowController` owns a single `OakDocumentView` and a flat list of open documents (`_documents`).
- Switching tabs reassigns the document on the existing view: `self.documentView.document = _selectedDocument`.
- `OakTabBarView` lives in the titlebar via `NSTitlebarAccessoryViewController`.
- The controller stores `self.textView = self.documentView.textView` as a persistent property and references it 14 times.
- `DocumentWindowController` conforms to `OakTabBarViewDelegate` and `OakTabBarViewDataSource`, with all data source methods referencing `_documents` directly.
- `_stickyDocumentIdentifiers` tracks pinned/sticky tabs as a flat `NSMutableSet<NSUUID*>` shared across the window.

### Buffer Layer

The buffer layer (`ng::buffer_t`) already supports multiple simultaneous views. Each `OakTextView` creates its own `OakDocumentEditor` with independent selection, undo stack, and viewport, all sharing the same underlying `buffer_t`. No buffer-layer changes are needed.

### Layout

`ProjectLayoutView` uses Auto Layout constraints with custom divider drag handling to position the document view, file browser, and HTML output panel. The key view loop (`updateKeyViewLoop`) currently wires `{_documentView, _htmlOutputView, _fileBrowserView}`.

### SCM Integration

`DocumentWindowController.setDocumentPath:` sets up a single `scm::info` callback for the currently selected document, writing to `_documentSCMVariables`. These feed into window title, scope attributes (`scopeAttributes`), and bundle command variables (`variables`). There is only one `_documentSCMInfo` and one `_documentSCMVariables` set per window.

### LSP

LSP client code already has deduplication designed for the case where the same document appears in multiple views.

### Existing `performCloseSplit:`

`DocumentWindowController` already has a `performCloseSplit:` method (line 561) that **closes the HTML output panel**, not editor panes. This method is triggered via `performClose:` in `ProjectLayoutView` when the first responder is inside `htmlOutputView`. The new pane-close action must use a different selector.

### `performClose:` Chain

`ProjectLayoutView.performClose:` checks if the first responder is inside `_htmlOutputView` and routes to `performCloseSplit:`. Otherwise it calls `self.window.delegate.performClose:` → `DocumentWindowController.performClose:` → `self.tabBarView.performClose:sender`. This chain references the titlebar tab bar directly.

---

## 3. Design Decisions

| # | Decision | Rationale |
|---|----------|-----------|
| 1 | **Per-pane tab bars** | Each split pane gets its own `OakTabBarView`. The titlebar tab bar hides when splits are active; single pane preserves today's behavior unchanged. |
| 2 | **Flat grid only** | Up to 3 columns OR up to 3 rows, never mixed/nested. Simpler implementation, covers the vast majority of use cases. State machine: single → vertical-2 → vertical-3 (or single → horizontal-2 → horizontal-3). |
| 3 | **Same document in multiple panes** | Allowed with independent cursors and selections. Already supported by the buffer architecture. |
| 4 | **Pane lifecycle** | Closing the last tab in a pane auto-closes that pane. The last remaining pane always stays open (matches current behavior). |
| 5 | **Keyboard shortcuts only** | No drag-to-split gesture. Splits created and managed entirely via keyboard shortcuts and menu items. |
| 6 | **Menu items under View** | All split operations accessible from the View menu for discoverability. |
| 7 | **SCM tracking is focused-pane only** | Only the focused pane's document has live SCM state. Non-focused panes get stale SCM data until they gain focus. This avoids multiple SCM watchers and keeps the existing single-watcher architecture intact. |
| 8 | **Sticky tabs are document-global** | `_stickyDocumentIdentifiers` remains a window-level set. A document pinned in one pane is considered sticky everywhere. Simplest model, matches user intent ("don't close this file"). |

---

## 4. Proposed Architecture

### New Classes Location

All new classes (`OakSplitContainerView`, `OakPaneView`, `OakPaneDividerView`, `OakPaneController`) live in `Frameworks/DocumentWindow`. This framework already depends on `OakTextView` (which contains `OakDocumentView`), so no new dependency edges are introduced.

### New Class: `OakSplitContainerView`

Custom `NSView` managing 1–3 panes in a flat grid.

**Properties:**
- `NSArray<OakPaneView*>* panes` — ordered array of pane views
- `OakPaneView* focusedPane` — the pane currently receiving input
- `OakSplitOrientation orientation` — `OakSplitOrientationVertical` or `OakSplitOrientationHorizontal`
- `NSArray<NSNumber*>* dividerPositions` — fractional positions (0.0–1.0) for each divider

**Methods:**
- `-splitPane:(OakPaneView*)pane orientation:(OakSplitOrientation)orientation` — split the focused pane, inserting a new pane after it
- `-closeSplitPane:(OakPaneView*)pane` — remove pane and its divider, redistribute space
- `-paneAtIndex:(NSUInteger)index` — accessor
- `-moveFocusInDirection:(OakPaneDirection)direction` — internal method called by the controller's action methods (`moveFocusToNextPane:` / `moveFocusToPreviousPane:`)

**Layout approach:**
- Constraint-based layout matching `ProjectLayoutView`'s existing pattern.
- Custom divider views (`OakPaneDividerView`) with mouse-drag resize handling. Divider width: 1pt visual, 5pt hit target.
- Focus indicated by a subtle border or highlighted divider on the active pane.
- **Minimum pane size: 200pt** — enforced during divider drag and programmatic split. Splitting is refused if adding a pane would push any pane below minimum.

### New Class: `OakPaneView`

Lightweight container holding a tab bar and document view for one pane.

**Properties:**
- `OakTabBarView* tabBarView` — per-pane tab bar (hidden when this is the sole pane)
- `OakDocumentView* documentView` — the editor view for this pane

### New Class: `OakPaneController`

Per-pane controller that owns the document list and serves as delegate/dataSource for its tab bar.

**Properties:**
- `NSArray<OakDocument*>* documents` — document list for this pane
- `NSUInteger selectedIndex` — index of the active document
- `OakPaneView* paneView` — the view this controller manages (weak reference)
- `DocumentWindowController* windowController` — back-reference for window-level queries (weak reference)

**Conformances:**
- `OakTabBarViewDelegate` — handles tab selection, close, reorder within this pane. For `performDropOfTabItem:fromTabBar:index:toTabBar:index:operation:`, the pane controller forwards to `DocumentWindowController` since cross-pane and cross-window drag requires window-level coordination.
- `OakTabBarViewDataSource` — provides document count, titles, modified state for this pane's tab bar

**Note:** `OakPaneController` does **not** conform to `OakTextViewDelegate`. See "SCM & OakTextViewDelegate" section below.

**Responsibilities:**
- Creates `OakDocumentView` via `[[OakDocumentView alloc] init]`. Theme and font are global settings (from user defaults), not per-pane — no copying needed.
- Manages per-pane document list independently
- Owns `closeTabsAtIndexes:askToSaveChanges:createDocumentIfEmpty:activate:` — moved from `DocumentWindowController`. Save-dialog logic that blocks the run loop stays in this method; it calls back to the window controller only for window-level side effects (session backup, window close decision).
- Forwards cross-pane/cross-window tab drag operations to `DocumentWindowController`

### SCM & `OakTextViewDelegate`

`DocumentWindowController` **remains** the `OakTextViewDelegate` for all `OakTextView` instances. It implements `scopeAttributes` and `variables` using window-level state.

SCM tracking follows a focused-pane-only model:
- `setDocumentPath:` is called whenever the focused pane changes or the focused pane's selected document changes.
- This updates `_documentSCMInfo` and `_documentSCMVariables` for the focused document.
- Non-focused panes' documents do not have live SCM tracking. Their scope attributes use whatever `_documentSCMVariables` was last set (stale but acceptable — SCM state only matters for the document being actively edited).
- When a pane gains focus, `setDocumentPath:` is called for its selected document, refreshing SCM state.

### Modified: `ProjectLayoutView`

- Replace the single `documentView` property with a `splitContainerView` property.
- `OakSplitContainerView` occupies the same constraint slot the old `documentView` used.
- File browser and HTML output layout are unchanged — they remain siblings of the split container.
- `updateKeyViewLoop` updated to iterate all pane views instead of a single `_documentView`.

### Modified: `DocumentWindowController`

This is a **major refactor**. The controller currently assumes a single document list, single tab bar, and single text view throughout. Key changes:

**Delegate/DataSource delegation:**
- The controller no longer directly conforms to `OakTabBarViewDelegate`/`OakTabBarViewDataSource`. These responsibilities move to `OakPaneController`.
- The controller maintains an array of `OakPaneController` objects, one per pane.
- The controller retains its role as the window-level coordinator: handling menu actions, window title, proxy icon, file browser interaction, session save/restore, and cross-pane tab drag.

**`self.textView` becomes dynamic:**
- Replace the stored `self.textView` property with a computed accessor: `self.splitContainerView.focusedPane.documentView.textView`.
- The 14 references to `self.textView` fall into three categories:
  1. **Focused-pane** (most): actions targeting the active editor — route through focused pane's text view.
  2. **Any-pane** (few): checks like `validateMenuItem:` that test if *any* text view is first responder — iterate all pane controllers.
  3. **All-pane** (rare): operations affecting all views (e.g., theme change notification) — iterate and apply to each.

**`self.documents` becomes an aggregate:**
- `self.documents` returns a computed union of all pane controllers' document lists (for `treatAsProjectWindow`, session save, and the `SortedControllers()` lookup).
- Direct mutation of `_documents` is removed; all document list changes go through `OakPaneController`.

**Focus tracking:**
- `focusedPane` is updated whenever a pane's `OakTextView` becomes first responder. Each `OakTextView` notifies its pane controller on `becomeFirstResponder`, which in turn notifies `DocumentWindowController`.
- Clicking in a pane's text view automatically makes it the focused pane.
- Focus change triggers `setDocumentPath:` for the newly focused document (refreshing SCM state).

**Window title and proxy icon:**
- Always reflect the focused pane's selected document. Updated whenever `focusedPane` changes or the focused pane's selected document changes.

**`performClose:` chain update:**
- `DocumentWindowController.performClose:` routes to the focused pane's `OakPaneController` for tab close logic, not to the titlebar tab bar. The pane controller's tab bar handles the close internally.

**Touch Bar:**
- `updateTouchBarButtons` reads from the focused pane's state. Updated on pane focus change and document selection change.

### Per-Pane `OakTabBarView`

- Each `OakPaneView` embeds an `OakTabBarView` above its `OakDocumentView`.
- When pane count = 1, the per-pane tab bar is hidden and the titlebar tab bar is used instead (backward compatibility). The existing titlebar tab bar collapsing logic (`hidden = !disableTabBarCollapsingKey && self.documents.count <= 1`) is preserved and applied to the titlebar bar only.
- When pane count > 1, the titlebar tab bar is hidden and each pane's embedded tab bar is shown. Each per-pane tab bar uses its own pane controller's document count for auto-hide decisions (but per-pane bars should always be visible when splits are active, even with 1 document, to maintain visual pane identity).
- Tab drag between panes: drag from one pane's tab bar, drop on another pane's tab bar or editor area to move the tab.

**Embedding considerations:**
- `OakTabBarView` was designed for titlebar rendering. When embedded as a regular subview, it will need visual adjustments:
  - Background drawing: needs a non-titlebar background style (no vibrancy/material compositing)
  - Height/intrinsic content size: verify compatibility with Auto Layout outside titlebar context
  - These adjustments are addressed in Phase 3.

### Responder Chain & First Responder

With multiple `OakTextView` instances, the responder chain needs explicit management:

- **Focus detection:** Each `OakTextView` overrides `becomeFirstResponder` to notify its `OakPaneController`, which in turn notifies `DocumentWindowController` to update `focusedPane`.
- **Menu validation:** `validateMenuItem:` checks like `[self.window firstResponder] == self.textView` must be updated to check whether *any* pane's text view is first responder (for enabling editor-related menu items), or specifically the focused pane's (for actions that target the focused editor).
- **`moveFocus:` (existing):** The current `moveFocus:` toggles between text view and file browser. With splits, it continues to toggle between the *focused pane's* text view and the file browser. Pane-to-pane navigation uses separate shortcuts (`Cmd+Ctrl+Right` / `Cmd+Ctrl+Left`).

### Bundle Commands

Bundle commands always operate on the first responder's view and selection. `TM_SELECTED_TEXT` and other environment variables are sourced from the focused pane's `OakTextView`. No special handling needed — the existing `textView` accessor becomes dynamic and always returns the focused pane's view.

---

## 5. Proposed View Hierarchy

```
NSWindow
├── contentView
│   └── ProjectLayoutView
│       ├── OakSplitContainerView
│       │   ├── OakPaneView (pane 0)
│       │   │   ├── OakTabBarView (per-pane, hidden when single pane)
│       │   │   └── OakDocumentView
│       │   │       ├── gutterScrollView (GutterView)
│       │   │       ├── textScrollView (OakTextView)
│       │   │       └── OTVStatusBar
│       │   ├── OakPaneDividerView (if 2+ panes)
│       │   ├── OakPaneView (pane 1)
│       │   │   ├── OakTabBarView
│       │   │   └── OakDocumentView
│       │   ├── OakPaneDividerView (if 3 panes)
│       │   └── OakPaneView (pane 2)
│       │       ├── OakTabBarView
│       │       └── OakDocumentView
│       ├── fileBrowserDivider
│       ├── fileBrowserView
│       ├── htmlOutputDivider
│       └── htmlOutputView
├── TitlebarAccessoryViewController
│   └── OakTabBarView (titlebar — visible only when single pane)
```

Each `OakPaneView` is managed by an `OakPaneController` (not shown in view hierarchy).

---

## 6. Keyboard Shortcuts & Menu Items

### Shortcuts

| Shortcut | Action | Selector |
|----------|--------|----------|
| `Cmd+\` | Split vertically (add column right of focused pane) | `performSplitVertically:` |
| `Cmd+Opt+\` | Split horizontally (add row below focused pane) | `performSplitHorizontally:` |
| `Cmd+Ctrl+W` | Close focused pane (all its tabs) | `performCloseSplitPane:` |
| `Cmd+Ctrl+Right` | Move focus to next pane (right / below) | `moveFocusToNextPane:` |
| `Cmd+Ctrl+Left` | Move focus to previous pane (left / above) | `moveFocusToPreviousPane:` |

Note: `Cmd+Opt+Left/Right` is already bound to `selectPreviousTab:`/`selectNextTab:`. `Cmd+Ctrl+Right`/`Cmd+Ctrl+Left` is already bound to `shiftRight:`/`shiftLeft:` (indent/outdent). `Cmd+Ctrl+Left/Right` is unbound and forms a consistent modifier pattern with `Cmd+Ctrl+W` (close pane).

### `Cmd+W` Behavior in Multi-Pane Mode

`performCloseTab:` closes the selected tab in the focused pane (routed through the focused pane's `OakPaneController`, not the titlebar tab bar). If it was the last tab:
- If there are other panes: the focused pane auto-closes, focus moves to the adjacent pane.
- If it is the sole pane: existing behavior (close window if no file browser, otherwise show untitled document).

### Menu Items (View menu)

```
View
├── ...
├── ─────────────
├── Split Vertically          Cmd+\
├── Split Horizontally        Cmd+Opt+\
├── Close Split Pane          Cmd+Ctrl+W
├── ─────────────
├── Focus Next Pane           Cmd+Ctrl+Right
├── Focus Previous Pane       Cmd+Ctrl+Left
├── ...
```

**Enable/disable rules:**
- `Close Split Pane`: disabled when only one pane is open.
- `Split Vertically`: disabled when 3 panes exist, or when horizontal splits are active.
- `Split Horizontally`: disabled when 3 panes exist, or when vertical splits are active.
- `Focus Next/Previous Pane`: disabled when only one pane is open.

---

## 7. Session Persistence

### Saved State

Window-level:
- `splitPaneCount` — number of panes (1–3)
- `splitOrientation` — `"vertical"` or `"horizontal"` (omitted when count = 1)
- `splitDividerPositions` — array of fractional divider positions
- `splitFocusedPaneIndex` — which pane had focus (0-based)
- `splitPanes` — array of per-pane state, each containing:
  - `documents` — array of document identifiers (paths or UUIDs)
  - `selectedIndex` — index of the active document in this pane

### Restore Behavior

1. Read pane count and orientation from session.
2. Create the `OakSplitContainerView` with the correct number of panes and orientation.
3. Set divider positions.
4. For each pane, create an `OakPaneController`, reopen its documents, and select the previously active one.
5. If a document fails to reopen (file deleted, etc.), skip it. If a pane ends up empty, close it.
6. Restore focus to the pane at `splitFocusedPaneIndex`.

### Backward Compatibility

Sessions saved without split state load as a single pane (current behavior). No migration needed.

---

## 8. Edge Cases

| Scenario | Behavior |
|----------|----------|
| Open file from file browser | Targets the focused pane's document list |
| `Cmd+T` (Go to File) | Opens result in the focused pane |
| Find in Files result click | Opens in the last keyboard-focused pane (tracked via `focusedPane`, which persists even when focus is in a dialog) |
| Window resize | Panes resize proportionally based on divider fractions |
| Minimum pane size | 200pt width (vertical) or height (horizontal), enforced during divider drag |
| Split when at max panes (3) | Menu item disabled, shortcut is a no-op |
| Split with conflicting orientation | Disabled (cannot mix vertical and horizontal) |
| Split with no document / untitled | Allowed — new pane opens with the same document (shared view on the untitled buffer) |
| Same document in 2 panes, file modified externally | Both views refresh (existing document reload mechanism handles this) |
| Undo in one pane of a shared document | Both views update (undo operates on the shared `buffer_t`) |
| `Cmd+W` closes last tab in a non-last pane | Pane auto-closes, focus moves to adjacent pane |
| HTML output panel + 3 vertical panes | HTML output positioned below spans all panes. If positioned right, the minimum window width is enforced: 3×200pt + file browser (~250pt) + HTML output (~300pt) ≈ 1150pt. If the window is too narrow, the split is refused. |
| Full-screen mode | Splits function identically. Native macOS window tabs (full-screen tab grouping) are orthogonal — each native tab is a separate `DocumentWindowController`. |
| Window title / proxy icon | Reflects the focused pane's selected document. Updates on pane focus change and document selection change. |
| Bundle commands (`TM_SELECTED_TEXT`, etc.) | Always source from the focused pane's `OakTextView` (via dynamic `self.textView` accessor) |
| `moveFocus:` (existing Ctrl+Tab) | Toggles between the focused pane's text view and the file browser, same as today. Does not cycle through panes. |
| SCM state in non-focused panes | Stale until pane gains focus. `setDocumentPath:` is called on focus change to refresh. Acceptable because SCM state only matters for active editing. |
| `treatAsProjectWindow` | Checks aggregate document count across all panes (via computed `self.documents`). |
| Touch Bar | Reflects focused pane's state. Updated on focus change. |

---

## 9. Implementation Phases

### Phase 1: `OakSplitContainerView` + Focus Infrastructure

- Implement `OakSplitContainerView` with constraint-based layout (all new classes in `Frameworks/DocumentWindow`).
- Implement `OakPaneView` as a container for `OakDocumentView`.
- Implement `OakPaneDividerView` with drag-to-resize and minimum size enforcement (200pt).
- Implement focus tracking: `focusedPane` updated on `becomeFirstResponder` via notification from `OakTextView` → `OakPaneController` → `DocumentWindowController`.
- Support 2-pane vertical split only for initial validation.
- No tab bar changes yet — titlebar tab bar drives the focused pane only.

### Phase 2: `OakPaneController` + Per-Pane Document Tracking

- Introduce `OakPaneController` conforming to `OakTabBarViewDelegate` and `OakTabBarViewDataSource`.
- Move `closeTabsAtIndexes:askToSaveChanges:createDocumentIfEmpty:activate:` into `OakPaneController`, with callbacks to `DocumentWindowController` for window-level side effects.
- Extract document list management from `DocumentWindowController` into `OakPaneController`.
- Each pane controller independently manages its own document list and selected index.
- Refactor `DocumentWindowController` to delegate tab operations through `OakPaneController`.
- Audit and categorize all 14 `self.textView` references:
  - Focused-pane (most): route through focused pane's text view
  - Any-pane (few): iterate all pane controllers for `validateMenuItem:` checks
  - All-pane (rare): iterate and apply to each for global operations
- Replace `_documents` direct access with computed aggregate property.
- Update `performClose:` chain to route through focused pane's `OakPaneController` instead of titlebar tab bar.
- Update `setDocumentPath:` to trigger on focused pane change (SCM refresh).

### Phase 3: Per-Pane `OakTabBarView` + Titlebar Logic

- Embed `OakTabBarView` inside each `OakPaneView`, wired to that pane's `OakPaneController`.
- Investigate and fix `OakTabBarView` rendering outside titlebar context (background drawing without vibrancy, height/intrinsic content size in Auto Layout).
- Hide/show titlebar tab bar based on pane count. Preserve existing collapsing logic (`documents.count <= 1`) for titlebar bar in single-pane mode.
- Per-pane tab bars always visible when splits are active (even with 1 document per pane).
- Ensure tab switching works independently per pane.

### Phase 4: Keyboard Shortcuts + Menu Items

- Wire `Cmd+\` (`performSplitVertically:`), `Cmd+Ctrl+W` (`performCloseSplitPane:`).
- Wire `Cmd+Ctrl+Right`/`Cmd+Ctrl+Left` for pane focus navigation (verify no existing binding conflict first).
- Add View menu items with proper enable/disable validation.
- Implement `Cmd+W` / `performCloseTab:` pane-aware behavior (routed through focused `OakPaneController`).

### Phase 5: Horizontal Splits + 3-Pane Support

- Add `OakSplitOrientationHorizontal` support.
- Allow up to 3 panes.
- Wire `Cmd+Opt+\` for horizontal split.
- Enforce orientation constraint (no mixing).
- Enforce minimum window size constraints when combined with file browser / HTML output.

### Phase 6: Session Save/Restore

- Extend session serialization to include split state (window-level `splitFocusedPaneIndex`, per-pane document lists).
- Implement restore logic with fallback for missing documents and empty panes.
- Verify backward compatibility with pre-split sessions.

### Phase 7: Tab Drag Between Panes

- Extend `OakTabBarView` drag-and-drop to support cross-pane tab moves.
- Handle drop on editor area (not just tab bar).
- Implement intra-window cross-pane drag path in `DocumentWindowController` (distinct from existing cross-window drag in `performDropOfTabItem:fromTabBar:index:toTabBar:index:operation:`). The `OakPaneController` forwards this call to the window controller.

### Phase 8: Polish

- Focus indicator visual refinement (subtle border, active pane highlight).
- Keyboard accessibility audit.
- Full-screen mode testing and edge case fixes.
- Touch Bar updates for pane-aware state.

---

## 10. Out of Scope

- **Nested/recursive splits** — future enhancement if flat grid proves insufficient.
- **Drag-to-split gesture** — future enhancement; keyboard-only for initial release.
- **Per-pane file browsers** — file browser remains a single shared panel.
- **Minimap per pane** — not planned.
- **Different split orientations in the same window** — by design, a window is either all-vertical or all-horizontal.
- **Per-pane SCM tracking** — only the focused pane has live SCM state. Non-focused panes get stale data until they gain focus.
