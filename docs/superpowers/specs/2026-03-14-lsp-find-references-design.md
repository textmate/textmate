# LSP Find References Design

## Overview

Add `textDocument/references` support to TextMate's LSP integration. Shows all references to the symbol under the caret in a SwiftUI panel (OakSwiftUI), with single-result optimization that navigates directly.

## Keybinding & Menu

- **Cmd+Opt+R** → `lspFindReferences:`
- Menu: Text → Find References (after Show Hover Info)

## Architecture

### Data Flow

```
Cmd+Opt+R → OakTextView.lspFindReferences:
  → LSPManager.requestReferencesForDocument:line:character:completion:
  → LSPClient (JSON-RPC textDocument/references)
  → Response: Location[]
  → 0 results: NSBeep()
  → 1 result: navigate directly (same as Go to Definition)
  → N results: build OakReferenceItem[], show OakReferencesPanel
  → user double-clicks row → delegate → navigate to location
```

### LSP Protocol

Request: `textDocument/references`
```json
{
  "textDocument": { "uri": "file:///path" },
  "position": { "line": N, "character": N },
  "context": { "includeDeclaration": true }
}
```

Response: `Location[]`
```json
[
  { "uri": "file:///path", "range": { "start": { "line": N, "character": N }, "end": { "line": N, "character": N } } }
]
```

## New Components

### OakSwiftUI (Swift)

#### OakReferenceItem (@objc, @MainActor)
Model for a single reference location.

Properties:
- `filePath: String` — absolute file path
- `displayPath: String` — relative/shortened for display
- `line: Int` — 0-based line number
- `column: Int` — 0-based column
- `content: String` — trimmed line content at that location

#### OakReferencesPanelDelegate (@objc protocol)
```swift
func referencesPanel(_ panel: OakReferencesPanel, didSelectItem item: OakReferenceItem)
func referencesPanelDidClose(_ panel: OakReferencesPanel)
```

#### ReferencesViewModel (internal)
Groups `[OakReferenceItem]` by file path into sections. Tracks selected index.

#### ReferencesListView (internal SwiftUI)
- List with sections grouped by file
- Section header: file icon + display path + count badge
- Row: line number (dimmed) + content (monospace)
- Single-click selects, double-click triggers delegate
- Uses OakThemeEnvironment for styling

#### OakReferencesPanel (@objc, @MainActor)
Bridge class. Owns an NSPanel (.titled, .closable, .resizable, .utilityWindow).

API:
- `init(theme: OakThemeEnvironment)`
- `show(in view: NSView, items: [OakReferenceItem], symbol: String)` — title becomes "N References to 'symbol'"
- `close()`
- `isVisible: Bool`
- `delegate: OakReferencesPanelDelegate?`

Panel default size: 500x350, positioned near parent window.

### LSPClient (Obj-C++)

Add `requestReferencesForDocument:line:character:completion:`:
- Check initialized
- Build params: textDocument.uri, position, context.includeDeclaration=true
- Send `textDocument/references` request
- Response handler: normalize Location[] to array of {uri, line, character}
- Same error handling pattern as definition

### LSPManager (Obj-C++)

Add `requestReferencesForDocument:line:character:completion:`:
- Look up client for document
- Convert file path to URI
- Forward to LSPClient
- Same pattern as `requestDefinitionForDocument:`

### OakTextView (Obj-C++)

Add `lspFindReferences:` action:
1. Check `LSPManager.hasClientForDocument:`
2. Get caret position, convert to line/column
3. Flush pending changes
4. Call LSPManager.requestReferences
5. In callback:
   - 0 results → `NSBeep()`
   - 1 result → navigate directly via `OakDocumentController showDocument:andSelect:inProject:bringToFront:`
   - N results → create `OakReferenceItem` array, show `OakReferencesPanel`

For each Location, load line content: open OakDocument for the URI, read the line at the given position for the content snippet.

Add `_lspReferencesPanel` ivar (lazy init like completion popup).
Implement `OakReferencesPanelDelegate`.

### AppController (Obj-C++)

Add menu item in Text menu (after Show Hover Info, line ~357):
```objc
{ @"Find References", @selector(lspFindReferences:), @"r", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
```

## Implementation Order

1. LSPClient: add references request method
2. LSPManager: add routing method
3. OakSwiftUI: add OakReferenceItem, delegate, view model, list view, panel
4. OakTextView: add action, delegate impl, panel management
5. AppController: add menu item
6. Build & test
