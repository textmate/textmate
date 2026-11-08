# LSP Rename Design

**Date:** 2026-03-15
**Feature:** `textDocument/rename` + `textDocument/prepareRename`
**Keybinding:** F2
**Branch:** feature/lsp-rename (from develop)

## Overview

Add LSP rename support to TextMate. The user presses F2 on a symbol, an inline text field appears pre-filled with the current name, the user types the new name, a preview panel shows all affected locations grouped by file, and on confirmation the edits are applied across all files.

## Request Flow

```
F2 → OakTextView.lspRename:
  → LSPManager.requestPrepareRenameForDocument:...
    → LSPClient.prepareRenameForURI:... (textDocument/prepareRename)
      → Server returns range + placeholder (or error if not renameable)
  → Show OakRenameField at caret (pre-filled with placeholder)
  → User types new name, presses Enter
  → LSPManager.requestRenameForDocument:...newName:...
    → LSPClient.requestRenameForURI:... (textDocument/rename)
      → Server returns WorkspaceEdit
  → Show OakRenamePreviewPanel with grouped edits
  → User clicks "Apply"
  → Apply edits to all files (reverse order per file)
  → Toast: "Renamed foo → bar in N files"
```

## Components

### 1. LSPClient (Frameworks/lsp/src/LSPClient.{h,mm})

**New properties:**
- `@property (readonly) BOOL renameProvider` — extracted from server capabilities

**Client capability declaration** (in sendInitialize):
```json
{
  "rename": {
    "dynamicRegistration": false,
    "prepareSupport": true
  }
}
```

**Server capability extraction** (in handleMessage for initialize response):
```objc
if(caps.contains("renameProvider") && !caps["renameProvider"].is_null())
    _renameProvider = caps["renameProvider"].is_boolean()
        ? caps["renameProvider"].get<bool>() : true;
```
Note: `renameProvider` can be a boolean or a `RenameOptions` object — treat non-null non-false as true.

**New methods:**

```objc
- (void)prepareRenameForURI:(NSString*)uri
                       line:(NSUInteger)line
                  character:(NSUInteger)character
                 completion:(void(^)(NSDictionary* _Nullable result))completion;
```
Sends `textDocument/prepareRename` with `{textDocument: {uri}, position: {line, character}}`.

Three possible response shapes (all must be handled):
1. **Range only** (clangd): `{"start": {"line": 5, "character": 10}, "end": {"line": 5, "character": 15}}` — extract word from buffer at range for placeholder
2. **Range + placeholder** (most servers): `{"range": {...}, "placeholder": "symbolName"}` — use placeholder directly
3. **defaultBehavior** (LSP 3.16+): `{"defaultBehavior": true}` — extract word under cursor locally

If response is `null` or error → symbol is not renameable.

```objc
- (void)requestRenameForURI:(NSString*)uri
                       line:(NSUInteger)line
                  character:(NSUInteger)character
                    newName:(NSString*)newName
                 completion:(void(^)(NSDictionary* _Nullable workspaceEdit))completion;
```
Sends `textDocument/rename` with `{textDocument: {uri}, position: {line, character}, newName}`.
Response: WorkspaceEdit `{changes: {uri: [TextEdit]}}` or `{documentChanges: [...]}`.

### 2. LSPManager (Frameworks/lsp/src/LSPManager.{h,mm})

**New methods:**

```objc
- (BOOL)serverSupportsRenameForDocument:(OakDocument*)document;

- (void)requestPrepareRenameForDocument:(OakDocument*)document
                                   line:(NSUInteger)line
                              character:(NSUInteger)character
                             completion:(void(^)(NSDictionary* _Nullable))completion;

- (void)requestRenameForDocument:(OakDocument*)document
                            line:(NSUInteger)line
                       character:(NSUInteger)character
                         newName:(NSString*)newName
                      completion:(void(^)(NSDictionary* _Nullable))completion;
```

Both methods flush pending changes before sending the request (via `flushPendingChangesForDocument:`). Follow the same document-to-client lookup pattern as `requestDefinitionForDocument:`.

### 3. OakRenameField (OakSwiftUI)

**SwiftUI view:** `RenameFieldView`
- TextField pre-filled with placeholder text, all text selected
- Monospace font from OakThemeEnvironment
- Enter key → confirm, Esc → dismiss
- Minimal chrome: border, slight shadow, theme background

**NSPanel host:** `OakRenameFieldPanel` (borderless, non-activating)
- Positioned below the caret rect (same approach as CompletionPopup)
- Contains NSHostingView with RenameFieldView

**ObjC bridge:** `OakRenameField`
```objc
@interface OakRenameField : NSObject
- (instancetype)initWithTheme:(OakThemeEnvironment*)theme;
@property (weak) id<OakRenameFieldDelegate> delegate;
- (void)showIn:(NSView*)parentView at:(NSPoint)point placeholder:(NSString*)placeholder;
- (void)dismiss;
- (BOOL)isVisible;
@end

@protocol OakRenameFieldDelegate
- (void)renameField:(OakRenameField*)field didConfirmWithName:(NSString*)newName;
- (void)renameFieldDidDismiss:(OakRenameField*)field;
@end
```

### 4. OakRenamePreviewPanel (OakSwiftUI)

**Data model:**
```swift
struct RenameFileEdit: Identifiable {
    let filePath: String
    let fileName: String
    let edits: [RenameLineEdit]
}

struct RenameLineEdit: Identifiable {
    let line: Int
    let oldText: String
    let newText: String
}
```

**SwiftUI view:** `RenamePreviewListView`
- Grouped by file (DisclosureGroup or Section with file name header)
- Each row: line number + old text (strikethrough/red) → new text (green)
- Summary at top: "N changes in M files"
- "Apply" button (prominent) and "Cancel" button at bottom
- Theme-aware via OakThemeEnvironment

**NSPanel host:** `OakRenamePreviewNSPanel`
- Floating panel, child of parent window (same pattern as OakReferencesPanel)
- Reasonable default size (400x300), resizable

**ObjC bridge:** `OakRenamePreviewPanel`
```objc
@interface OakRenamePreviewPanel : NSObject
- (instancetype)initWithTheme:(OakThemeEnvironment*)theme;
@property (weak) id<OakRenamePreviewPanelDelegate> delegate;
- (void)showWithEdits:(NSArray<NSDictionary*>*)edits
              oldName:(NSString*)oldName
              newName:(NSString*)newName
         parentWindow:(NSWindow*)window;
- (void)dismiss;
@end

@protocol OakRenamePreviewPanelDelegate
- (void)renamePreviewPanelDidConfirm:(OakRenamePreviewPanel*)panel;
- (void)renamePreviewPanelDidCancel:(OakRenamePreviewPanel*)panel;
@end
```

### 5. OakTextView.lspRename: (Frameworks/OakTextView/src/OakTextView.mm)

**Action method:**
```objc
- (void)lspRename:(id)sender
{
    // 1. Get caret position, convert to line/character
    // 2. Flush pending changes
    // 3. Call prepareRename
    //    - Success: show OakRenameField with placeholder
    //    - Failure (null/error): show toast "Symbol cannot be renamed"
    //    - Server doesn't support prepareRename: extract word under cursor, show field
    // 4. On rename field confirm (delegate callback):
    //    - Call textDocument/rename with new name
    // 5. On WorkspaceEdit response:
    //    - Parse edits, load line content for preview
    //    - Show OakRenamePreviewPanel
    // 6. On preview confirm (delegate callback):
    //    - Apply edits per file (reverse order within each file)
    //    - For open documents: edit buffer in-place, mark modified
    //    - For closed files: load, edit, save
    //    - Show toast "Renamed oldName → newName in N files"
    // 7. On preview cancel: no-op, dismiss
}
```

**validateMenuItem:** Return NO for rename menu item when `![[LSPManager sharedManager] serverSupportsRenameForDocument:self.document]`.

### 6. Menu Item (Applications/TextMate/src/AppController.mm)

Add after "Find References" in the Text menu:
```objc
{ @"Rename Symbol", @selector(lspRename:), .key = NSF2FunctionKey, .modifierFlags = 0 },
```

## WorkspaceEdit Handling

The rename response is a WorkspaceEdit. Two formats to handle:

**Format 1: `changes`** (simpler, more common)
```json
{
  "changes": {
    "file:///path/to/file.php": [
      {"range": {"start": {"line": 5, "character": 10}, "end": {"line": 5, "character": 15}}, "newText": "newName"}
    ]
  }
}
```

**Format 2: `documentChanges`** (richer, supports create/rename/delete)
```json
{
  "documentChanges": [
    {
      "textDocument": {"uri": "file:///path/to/file.php", "version": 3},
      "edits": [{"range": {...}, "newText": "newName"}]
    }
  ]
}
```

Implementation handles both: prefer `documentChanges` if present, fall back to `changes`. Only TextEdit operations are supported (no CreateFile/RenameFile/DeleteFile for now).

## Staleness Guard

Capture the document revision (version counter) when the user confirms the new name. When the WorkspaceEdit response arrives, compare against the current revision. If the buffer has changed (user typed, undo, external reload), discard the response and show a toast: "Buffer changed, rename cancelled." This prevents applying edits to stale positions. Same pattern as lspFormatDocument:.

## Undo Strategy

Each file's edits are applied as a single undo group. For open documents, wrap all edits for that file in a single undo transaction so that Cmd+Z reverts the entire rename in that file in one step. For files not open in the editor (loaded from disk), apply edits and save — these are not undoable via the editor but the user saw the preview and confirmed.

## OakSwiftUI Guards

All OakSwiftUI usage in OakTextView.mm is guarded:
- Compile time: `#if HAVE_OAK_SWIFTUI`
- Runtime: `NSClassFromString(@"OakRenameField")` / `NSClassFromString(@"OakRenamePreviewPanel")`
- If OakSwiftUI is not available, `lspRename:` is a no-op and validateMenuItem returns NO.

## Preview Data Loading

To populate `oldText` for each edit in the preview panel:
- For open documents: read line content from the in-memory buffer
- For closed files: load file from disk, split into lines, extract the relevant line
- Cache file contents during a single rename operation to avoid re-reading
- `newText` is constructed by replacing the edit range within `oldText`

The WorkspaceEdit is stored as an ivar (`_pendingRenameEdits`) on OakTextView between showing the preview and applying on confirm.

## Edge Cases

| Scenario | Behavior |
|----------|----------|
| prepareRename not supported by server | Extract word under cursor locally, show rename field |
| prepareRename returns null/error | Toast: "Symbol cannot be renamed" |
| Empty WorkspaceEdit | Toast: "No changes needed" |
| User presses Esc in rename field | Dismiss, no action |
| User presses Esc in preview panel | Dismiss, no action |
| Server returns error on rename | Toast with error message |
| Edits affect unsaved files | Apply to in-memory buffer, mark as modified (don't auto-save) |
| Edits affect files not open in editor | Load from disk, apply edits, save |
| No LSP server running | Menu item grayed out (validateMenuItem) |
| Buffer changed between confirm and response | Discard WorkspaceEdit, show toast (staleness guard) |
| Read-only file in edit set | Skip that file, show warning in toast listing skipped files |

## Files Modified

| File | Changes |
|------|---------|
| `Frameworks/lsp/src/LSPClient.h` | Add `renameProvider` property, two new method declarations |
| `Frameworks/lsp/src/LSPClient.mm` | Capability extraction, prepareRename + rename request methods, client capabilities |
| `Frameworks/lsp/src/LSPManager.h` | Three new method declarations |
| `Frameworks/lsp/src/LSPManager.mm` | Capability check + two request forwarding methods |
| `Frameworks/OakTextView/src/OakTextView.mm` | `lspRename:` action, delegate callbacks, validateMenuItem update |
| `Applications/TextMate/src/AppController.mm` | Menu item for Rename |
| `Frameworks/OakSwiftUI/Sources/OakSwiftUI/` | New: RenameFieldView, OakRenameField bridge, RenamePreviewListView, OakRenamePreviewPanel bridge |
| `Frameworks/OakSwiftUI/Package.swift` | New source files |

## Implementation Order

1. **LSPClient** — capability property, prepareRename + rename request methods, client capability declaration
2. **LSPManager** — capability check, two forwarding methods
3. **OakSwiftUI: OakRenameField** — SwiftUI view + NSPanel + ObjC bridge
4. **OakSwiftUI: OakRenamePreviewPanel** — SwiftUI view + NSPanel + ObjC bridge
5. **OakTextView.lspRename:** — orchestration action, delegate callbacks, staleness guard, undo grouping
6. **AppController menu item** — F2 keybinding
7. **Build + manual test**

## Testing

- OakSwiftUI unit tests for RenameFieldView and RenamePreviewListView
- Manual testing with Intelephense (PHP) and clangd (C++) LSP servers
- Verify: prepareRename fallback, multi-file edits, cancel flows, empty edits
