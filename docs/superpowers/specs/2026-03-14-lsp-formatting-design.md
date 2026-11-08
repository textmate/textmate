# LSP Formatting — Design Spec

## Overview

Add LSP `textDocument/formatting` and `textDocument/rangeFormatting` support to TextMate. Formatting can be triggered manually (menu item, no default shortcut) or automatically on save via `.tm_properties` setting. Cursor position is preserved using TextMate's existing line interpolation mechanism.

## Scope

- **In scope:** Full document formatting, range (selection) formatting, format-on-save, manual trigger via menu item, server capability parsing
- **Out of scope:** `textDocument/onTypeFormatting` (future work), no menu toggle for format-on-save

## Protocol

### textDocument/formatting

Request:
```json
{
  "method": "textDocument/formatting",
  "params": {
    "textDocument": { "uri": "file:///path/to/file" },
    "options": {
      "tabSize": 3,
      "insertSpaces": false
    }
  }
}
```

Response: `TextEdit[]` or `null`. Each TextEdit has a `range` (start/end line:character) and `newText`.

### textDocument/rangeFormatting

Same as above but with an additional `range` field in params specifying the selection to format.

### FormattingOptions

Built from the document's existing properties:
- `tabSize` → `document.tabSize`
- `insertSpaces` → `document.softTabs`

### Server Capabilities

Formatting support is advertised in the initialize response:
- `documentFormattingProvider: true|false`
- `documentRangeFormattingProvider: true|false`

These are parsed from the initialize response and stored as properties on LSPClient. This is a new pattern — existing features (completion, hover, references) do not check capabilities. Formatting introduces capability parsing because the menu item needs to be grayed out when unsupported. This begins a pattern that should eventually be applied to all LSP features.

## Architecture

### Layer 1: LSPClient

**File:** `Frameworks/lsp/src/LSPClient.mm`

New methods:
- `requestFormattingForURI:tabSize:insertSpaces:callback:` — sends `textDocument/formatting`
- `requestRangeFormattingForURI:range:tabSize:insertSpaces:callback:` — sends `textDocument/rangeFormatting`

Callback receives `NSArray<NSDictionary*>*` of TextEdits, or `nil`.

New properties (parsed from initialize response):
- `documentFormattingProvider` (BOOL, readonly)
- `documentRangeFormattingProvider` (BOOL, readonly)

In the initialize response handler (after setting `_initialized = YES`), extract `capabilities.documentFormattingProvider` and `capabilities.documentRangeFormattingProvider` from the result dictionary and store them.

Capability declaration: No client-side capabilities needed for formatting (unlike completion which declares `snippetSupport`).

### Layer 2: LSPManager

**File:** `Frameworks/lsp/src/LSPManager.mm`

New methods:
- `requestFormattingForDocument:callback:` — resolves LSPClient, checks capability, flushes pending changes via `flushPendingChangesForDocument:`, builds FormattingOptions from document, forwards request
- `requestRangeFormattingForDocument:range:callback:` — same with range
- `serverSupportsFormattingForDocument:` → `BOOL`
- `serverSupportsRangeFormattingForDocument:` → `BOOL`

If capability is unsupported, calls back with `nil` immediately.

**Prerequisite:** Always call `flushPendingChangesForDocument:` before sending a formatting request to ensure the server has the latest document content via `textDocument/didChange`. Note: `didChange` is a notification with no response, so there is no guarantee the server has processed it before receiving the formatting request. In practice, servers process messages sequentially and this is not an issue.

### Layer 3: OakTextView

**File:** `Frameworks/OakTextView/src/OakTextView.mm`

#### Manual Formatting

Action method: `lspFormatDocument:`
- Capture current selection at request time (for range formatting)
- If non-empty selection exists and server supports range formatting → send `textDocument/rangeFormatting` for captured selection
- Otherwise if server supports document formatting → send `textDocument/formatting`
- Apply result via `editor_t::handle_result` with `output_format::text`:
  - Full document: `output::replace_document` + `output_caret::interpolate_by_line`
  - Range: `output::replace_input` with captured `inputRanges` + `output_caret::interpolate_by_line`

Using `replace_input` with captured `inputRanges` (not `replace_selection`) for range formatting ensures correctness even if the user moves the cursor between request and response.

Menu item: **Text → Format Code / Selection** (no default key equivalent)
- Title dynamically updates: "Format Code" when no selection, "Format Selection" when there is one (using existing `updateTitle:` pattern with `"\\b(\\w+) / (Selection)\\b"` regex)
- Grayed out via `validateMenuItem:` when server doesn't advertise formatting support or no LSP client is connected
- Users can bind their own shortcut via macOS System Settings or `.tm_properties`

**Staleness guard:** Track a revision counter (incrementing on each buffer modification). Capture it at request time. When the response arrives, compare against current revision — if they differ, discard the response (the buffer has changed during the round-trip).

#### Format-on-Save

In `documentWillSave:`, after existing bundle callbacks:
1. Read `lspFormatOnSave` from `.tm_properties` via `settings.get("lspFormatOnSave", false)`
2. If enabled and server supports formatting:
   - Flush pending changes via `flushPendingChangesForDocument:`
   - Send `textDocument/formatting` request
   - Spin run loop until response arrives (timeout: 3 seconds)
   - Apply result via `handle_result` with `output_format::text`, `output::replace_document` + `output_caret::interpolate_by_line`
   - On timeout: proceed with save silently (log warning to console)
   - On nil/error response: proceed with save silently
3. Save proceeds with formatted buffer

The run loop spin ensures the save blocks until formatting completes. This matches the behavior of existing bundle-based formatters — `executeBundleCommand:` uses `CFRunLoopRunInMode` in a modal event loop (OakTextView.mm:5129-5156) that blocks until the command terminates.

**Interaction with bundle will-save callbacks:** LSP formatting runs *after* bundle `callback.document.will-save` items. Users should not enable both a bundle-based formatter (e.g., Prettier) and `lspFormatOnSave` for the same file type — the LSP formatter would re-format already-formatted output. This is documented but not enforced.

No staleness guard needed for format-on-save since the run loop spin blocks user input.

## Settings

### .tm_properties

| Setting | Type | Default | Description |
|---|---|---|---|
| `lspFormatOnSave` | bool | `false` | Format document via LSP before saving |

Example:
```
[ *.php ]
lspFormatOnSave = true

[ *.js ]
lspFormatOnSave = true
```

Follows existing pattern: `lspEnabled`, `lspCommand`, `lspRootPath`, `lspInitOptions`.

## Cursor Preservation

Uses TextMate's `output_caret::interpolate_by_line`:
1. Before replacement: save cursor's line number and visual column
2. Apply the formatted text
3. After replacement: restore cursor to the same line:column position

This is the same mechanism used by bundle commands with "Caret Placement: Line Interpolation".

## TextEdit Application

The LSP server returns an array of TextEdits (non-overlapping ranges + replacement text). The implementation:

1. **Sort** TextEdits in reverse document order (bottom-to-top, right-to-left) to avoid offset invalidation
2. **Convert** LSP line:character positions to buffer byte offsets using `buffer_t::convert(text::pos_t(line, column))`. Note: this treats `column` as a byte offset from line start, not a UTF-16 code unit offset. This is a known limitation inherited from all existing LSP features (completion, hover, go-to-def, references). Documents with characters where UTF-8 byte count differs from UTF-16 code unit count (emoji, some CJK) may produce incorrect offsets. Fixing this requires a UTF-16-to-UTF-8 conversion layer that is out of scope for this feature.
3. **Apply** all edits to the original document text to produce the formatted result
4. **Replace** the entire document through `handle_result` with `output_format::text`, `output::replace_document`

This creates a single undo step, enables line interpolation for cursor preservation, and matches the behavior of existing bundle-based formatters.

For range formatting with `output::replace_input`, only the captured selection range is replaced.

**Edge cases:**
- Empty TextEdit array or null response: no-op
- TextEdit range beyond document bounds: clamp to document end
- Server error response: no-op, log warning
- Buffer modified between request and response (manual trigger only): discard response

## Menu Structure

```
Text
  ...
  Show Hover Info          ⌃⌘I
  Find References          ⌃⌘R
  ──────────────────────────────
  Format Code / Selection            ← new (no default shortcut)
```
