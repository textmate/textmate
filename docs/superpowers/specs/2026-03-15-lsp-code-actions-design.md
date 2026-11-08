# LSP Code Actions Design Spec

## Overview

Add `textDocument/codeAction` support to TextMate's LSP integration. Code actions surface quick fixes, refactorings, and source actions (e.g. organize imports) from the language server. Triggered via Cmd+. or a lightbulb gutter icon on the current cursor line when diagnostics are present.

## Keybinding

Cmd+. is rebound from its current role (`cancelOperation:` → `complete:`, which cycles Esc-pipeline word completions) to `lspCodeActions:`. The Esc key itself still triggers the word completion pipeline. LSP completion remains on Opt+Tab.

**KeyBindings.dict change:** Remove the `cancelOperation:` → `complete:` override in OakTextView. The menu item binding for Cmd+. takes over.

**Live search compatibility:** `cancelOperation:` is also used to dismiss the incremental search bar. Since the menu item `lspCodeActions:` captures Cmd+. at the menu level before it reaches the responder chain, we must handle this: the `validateMenuItem:` for `lspCodeActions:` returns `NO` when the search bar is active (first responder is the search field). This lets Cmd+. fall through to `cancelOperation:` for search dismissal.

## Configuration

**`.tm_properties` key:** `lspCodeActions` (boolean, default `true`)

```
lspCodeActions = false

[ *.md ]
lspCodeActions = false
```

When `false`: no lightbulb gutter icon, `lspCodeActions:` menu item greyed out, no codeAction requests sent. The diagnostics cache still populates since it's cheap and may serve future features.

Checked via `settingsForPath:` in `validateMenuItem:` and at the top of `lspCodeActions:`.

## Diagnostics Cache

### Problem

Current `publishDiagnostics` handler strips diagnostics down to simple fields (line, character, severity, message) in LSPClient.mm before they reach LSPManager, which further reduces them to document marks. The full range (start+end), code, source, and data fields are lost — all needed for the codeAction `context.diagnostics` parameter.

### Solution

Two changes:

1. **LSPClient.mm** — In the `publishDiagnostics` handler, preserve the full original diagnostic dictionaries (range with start+end, severity, message, code, source, data) when calling the delegate. This requires modifying how diagnostics are parsed and forwarded.

2. **LSPManager.mm** — Add `NSMutableDictionary<NSString*, NSArray<NSDictionary*>*>* _diagnosticsByURI`. When `publishDiagnostics` arrives, cache the full diagnostic array alongside existing mark creation. The mark creation continues using only the fields it needs.

New method on LSPManager:

```objc
- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document
                                           atLine:(NSUInteger)line
                                        character:(NSUInteger)character
                                          endLine:(NSUInteger)endLine
                                     endCharacter:(NSUInteger)endCharacter;
```

Filters cached diagnostics to those overlapping the given range. Called internally when building the codeAction request.

## Lightbulb Gutter Icon

**Reactive, cursor-line only.** No proactive codeAction requests — the lightbulb appears based on existing diagnostic marks.

### Display Logic

In `OakDocumentView.imageForLine:inColumnWithIdentifier:state:` (bookmarks column):
- If `line == currentCursorLine` AND line has diagnostic marks (error/warning/note) AND server supports code actions AND `lspCodeActions` setting is enabled → return lightbulb image
- Otherwise fall through to existing bookmark/diagnostic icon logic

### Cursor Tracking

When the cursor line changes (in `setSelectedRanges:` or equivalent), invalidate the gutter for the old and new cursor lines to trigger redraw.

### Click Handling

In `GutterViewColumnDelegate.userDidClickColumnWithIdentifier:atLine:`, if the lightbulb is showing on that line, trigger `lspCodeActions:`.

### Image

SF Symbol `lightbulb.fill` rendered as a template image at gutter size, or a small custom PNG asset. Colored via the gutter's `iconColor` theme property.

## LSP Protocol Layer

### Client Capabilities (initialize request)

Added to `textDocument` capabilities in LSPClient `sendInitialize`:

```json
{
  "codeAction": {
    "codeActionLiteralSupport": {
      "codeActionKind": {
        "valueSet": [
          "quickfix",
          "refactor",
          "refactor.extract",
          "refactor.inline",
          "refactor.rewrite",
          "source",
          "source.organizeImports"
        ]
      }
    },
    "dynamicRegistration": false,
    "resolveSupport": {
      "properties": ["edit", "command"]
    },
    "dataSupport": true,
    "isPreferredSupport": true
  }
}
```

### Server Capability Extraction

In the initialize response handler, parse `capabilities.codeActionProvider`:
- If boolean `true` → set `_codeActionProvider = YES`
- If object → set `_codeActionProvider = YES`, extract `resolveProvider` bool

New properties on LSPClient:
- `@property (readonly) BOOL codeActionProvider;`
- `@property (readonly) BOOL codeActionResolveProvider;`

### LSPClient Methods

```objc
- (void)requestCodeActionsForURI:(NSString*)uri
                            line:(NSUInteger)line
                       character:(NSUInteger)character
                         endLine:(NSUInteger)endLine
                    endCharacter:(NSUInteger)endCharacter
                     diagnostics:(NSArray<NSDictionary*>*)diagnostics
                      completion:(void(^)(NSArray<NSDictionary*>*))callback;

- (void)resolveCodeAction:(NSDictionary*)codeAction
               completion:(void(^)(NSDictionary*))callback;

- (void)executeCommand:(NSString*)command
             arguments:(NSArray*)arguments
            completion:(void(^)(id))callback;
```

**Request params structure:**
```json
{
  "textDocument": { "uri": "file:///path" },
  "range": {
    "start": { "line": N, "character": M },
    "end": { "line": N, "character": M }
  },
  "context": {
    "diagnostics": [ ... ],
    "triggerKind": 1
  }
}
```

`triggerKind`: 1 = invoked (Cmd+. or lightbulb click), 2 = automatic.

### LSPManager Methods

```objc
- (BOOL)serverSupportsCodeActionsForDocument:(OakDocument*)document;

- (void)requestCodeActionsForDocument:(OakDocument*)document
                                 line:(NSUInteger)line
                            character:(NSUInteger)character
                              endLine:(NSUInteger)endLine
                         endCharacter:(NSUInteger)endCharacter
                           completion:(void(^)(NSArray<NSDictionary*>*))callback;

- (void)resolveCodeAction:(NSDictionary*)codeAction
              forDocument:(OakDocument*)document
               completion:(void(^)(NSDictionary*))callback;

- (void)executeCommand:(NSString*)command
             arguments:(NSArray*)arguments
           forDocument:(OakDocument*)document
            completion:(void(^)(id))callback;
```

The `requestCodeActionsForDocument:` method auto-attaches cached diagnostics for the range internally — callers don't need to pass them.

## OakTextView Action

### `lspCodeActions:` Action

1. Guard: check `lspCodeActions` setting via `settingsForPath:`. If disabled, return.
2. Guard: check `serverSupportsCodeActionsForDocument:`. If not, NSBeep + return.
3. Get caret position (or full selection range if selection is active).
4. Flush pending changes: `[lsp flushPendingChangesForDocument:doc]`.
5. Call `requestCodeActionsForDocument:...`.
6. On response:
   - Empty array → NSBeep, return.
   - Build and show NSMenu at caret screen position.

### NSMenu Construction

- Response may contain a mix of `CodeAction` and bare `Command` objects. Bare `Command` objects (identified by having `command` string + `arguments` but no `title`/`kind`/`edit`) are wrapped into a CodeAction with the command's `title` for display.
- Each `CodeAction` → NSMenuItem with `title` as the menu item title.
- `isPreferred` items: bold via `NSAttributedString` on `attributedTitle`.
- `disabled` items: greyed out, `disabled.reason` as tooltip.
- Grouped by kind with separators:
  1. Quick Fixes (`quickfix`)
  2. Refactor (`refactor.*`)
  3. Source (`source.*`)
  4. Ungrouped (no kind)
- Section headers as disabled menu items with small grey text (e.g. "Quick Fix", "Refactor").
- Each item stores its CodeAction dict as `representedObject`.
- Action selector: `performCodeAction:`.

### `performCodeAction:` Apply Flow

1. Extract CodeAction dict from `[sender representedObject]`.
2. If action has `edit` (WorkspaceEdit) → call `applyWorkspaceEdit:` (reuse from rename).
3. If action has no `edit` but server has `resolveProvider` → call `resolveCodeAction:forDocument:completion:`, then apply returned `edit`.
4. If action has `command` → call `executeCommand:arguments:forDocument:completion:`.
5. If action has both `edit` and `command` → apply edit first, then execute command.

### Menu Validation

In `validateMenuItem:` for `lspCodeActions:`:
- Return `YES` only when document exists AND `serverSupportsCodeActionsForDocument:` AND `lspCodeActions` setting is enabled.

## Menu Item

In `AppController.mm` Text menu, after "Rename Symbol (F2)":

```objc
{ @"Code Actions", @selector(lspCodeActions:), .key = @".", .modifierFlags = NSEventModifierFlagCommand },
```

## workspace/executeCommand

Some code actions return a `Command` instead of (or in addition to) a `WorkspaceEdit`. Add to LSPClient:

```objc
- (void)executeCommand:(NSString*)command
             arguments:(NSArray*)arguments
            completion:(void(^)(id))callback;
```

Sends `workspace/executeCommand` request.

## workspace/applyEdit (server→client)

Some servers respond to `executeCommand` by sending a `workspace/applyEdit` request back to the client with the actual edits. This is a server-initiated request that LSPClient must handle.

In LSPClient's message handler, add a case for `"workspace/applyEdit"`:
1. Extract the `WorkspaceEdit` from `params.edit`
2. Forward to the delegate (LSPManager) via a new delegate method: `client:didReceiveApplyEditRequest:requestId:`
3. LSPManager routes to OakTextView's `applyWorkspaceEdit:` (same method used by rename and direct code action edits)
4. Respond to the server with `{"applied": true}` or `{"applied": false, "failureReason": "..."}` using the request ID

Without this, code actions that use the Command pattern (server does the work, sends edits back) would silently do nothing.

## Files Modified

| File | Changes |
|---|---|
| `Frameworks/lsp/src/LSPClient.h` | Add codeActionProvider, codeActionResolveProvider properties; add requestCodeActions, resolveCodeAction, executeCommand methods |
| `Frameworks/lsp/src/LSPClient.mm` | Implement new methods; add client capabilities; parse server capabilities; preserve full diagnostic dicts in publishDiagnostics handler; handle workspace/applyEdit server→client request |
| `Frameworks/lsp/src/LSPManager.h` | Add diagnostics cache; add wrapper methods; add capability check |
| `Frameworks/lsp/src/LSPManager.mm` | Cache raw diagnostics in publishDiagnostics handler; implement wrapper methods |
| `Frameworks/OakTextView/src/OakTextView.mm` | Add lspCodeActions: action, performCodeAction:, menu validation, cursor-line tracking for lightbulb |
| `Frameworks/OakTextView/src/OakDocumentView.mm` | Lightbulb icon in gutter for cursor line with diagnostics |
| `Applications/TextMate/src/AppController.mm` | Add "Code Actions" menu item with Cmd+. |
| `Applications/TextMate/resources/KeyBindings.dict` | Remove cancelOperation: override (no longer needed) |
| `Frameworks/settings/src/settings.cc` (or equivalent) | Add `lspCodeActions` setting with default `true` |

## Out of Scope (Future)

- Proactive lightbulb (fire codeAction on cursor movement to check availability)
- SwiftUI panel replacement for NSMenu (upgrade if NSMenu proves insufficient)
- Code action on save (auto-apply isPreferred on file save)
- Source actions submenu (organize imports, fix all)
