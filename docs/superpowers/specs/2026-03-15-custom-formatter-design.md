# Custom Formatter Support

**Date:** 2026-03-15
**Status:** Draft

## Overview

Add first-class support for external formatters (prettier, black, rustfmt, etc.) configured via `.tm_properties`. When a custom formatter is set for a file type, it takes priority over LSP formatting for that type.

## Configuration

New `.tm_properties` keys:

```
[ *.js ]
formatCommand     = prettier --stdin-filepath "$TM_FILEPATH"
formatOnSave      = true

[ *.py ]
formatCommand     = black -q -
formatOnSave      = true

[ *.rs ]
formatCommand     = rustfmt
formatOnSave      = true
```

### Priority Logic

1. `formatCommand` is set -> use custom formatter
2. LSP server has `documentFormattingProvider` -> use LSP formatting
3. Neither -> formatting unavailable (menu item grayed out)

### `formatOnSave` Semantics

- `formatOnSave = true` triggers whichever formatter is active (custom or LSP)
- Existing `lspFormatOnSave` continues to work as alias for backward compatibility
- `formatOnSave` takes precedence over `lspFormatOnSave` when both are set

## Execution Model

### I/O

- **Input:** Entire document content piped to formatter's stdin
- **Output:** Formatted document text read from stdout
- **Error:** stderr captured; shown in status bar on failure
- **Exit code:** 0 = success (apply output), non-zero = skip formatting silently

### Environment Variables

Standard TextMate bundle variables available to the process:
- `TM_FILEPATH` - full path to the file
- `TM_FILENAME` - filename only
- `TM_DIRECTORY` - parent directory
- `TM_SOFT_TABS` - "YES" or "NO"
- `TM_TAB_SIZE` - tab width as string
- `TM_SCOPE` - current scope at caret

### PATH Resolution

The formatter command runs via `/bin/sh -c`, inheriting the user's shell PATH. To handle GUI app launch contexts where PATH may be limited, prepend common tool locations (`/usr/local/bin`, `/opt/homebrew/bin`, `~/.local/bin`) to PATH before execution.

### Apply Strategy

Whole-buffer replacement — no diffing needed:

1. Capture formatter stdout as the formatted text
2. If output is identical to input, skip (no-op)
3. Otherwise, call `perform_replacements()` with a single replacement spanning the entire buffer: range `(0, buffer_length)` -> formatted text
4. Entire operation is a single undo step

### Cursor Preservation

After whole-buffer replacement, clamp the previous caret byte offset to the new buffer length. This is imperfect but simple and consistent with how other editors handle external formatters.

### Timeout

3-second timeout, matching the existing LSP format-on-save timeout. If the formatter process exceeds this, it is killed and formatting is skipped. Prevents blocking the save pipeline.

### Unsaved Buffers

If the document has no file path (unsaved buffer), `TM_FILEPATH` is not set. Formatters that require `--stdin-filepath` for language detection will receive an empty value. This is acceptable — users can adjust their formatter command or save the file first.

## Integration Points

### Format Action (`lspFormatDocument:` in OakTextView.mm)

The existing format action is extended:
1. Read `formatCommand` setting for current document
2. If set: run custom formatter synchronously via NSTask
3. If not set: fall back to existing LSP formatting code
4. If neither available: no-op

### Format on Save (`documentWillSave:` in OakTextView.mm)

The existing save handler is extended:
1. Read `formatOnSave` (falling back to `lspFormatOnSave`) setting
2. If enabled and `formatCommand` is set: run custom formatter, block save with `CFRunLoopRunInMode` (3s timeout) — same pattern as existing LSP format-on-save
3. If enabled and no `formatCommand`: use LSP formatting (existing behavior)
4. Proceed with save

### Menu Validation

`validateMenuItem:` for the Format Code item:
- Enabled if `formatCommand` is set OR LSP `documentFormattingProvider` is available
- Title remains "Format Code" (always full document)

### Status Feedback

- On success: no message (silent)
- On non-zero exit: show first line of stderr in status bar for 3 seconds
- On timeout: show "Formatter timed out" in status bar

## New Helper Method

```objc
- (NSString*)runCustomFormatter:(NSString*)command
                         onText:(NSString*)text
                          error:(NSString**)outError;
```

Synchronous method that:
- Creates NSTask with `/bin/sh -c` execution
- Populates TextMate environment variables (TM_FILEPATH, etc.)
- Prepends common bin paths to PATH for GUI context
- Pipes document text to stdin
- Reads stdout/stderr
- Enforces 3-second timeout with process termination
- Returns formatted text on success, nil on failure (error in outError)

## Files to Modify

| File | Change |
|------|--------|
| `Frameworks/settings/src/keys.cc` | Add `formatCommand` and `formatOnSave` setting keys |
| `Frameworks/OakTextView/src/OakTextView.mm` | Format action routing, save handler extension, new `runCustomFormatter:onText:error:` helper |

Total: 2 files modified, no new files or frameworks.

## Non-Goals (v1)

- Selection/range formatting (most formatters require complete files)
- Formatter auto-detection (user must configure explicitly)
- Formatter installation/management
- Per-project formatter version pinning
- On-type formatting via custom formatters (LSP only)
- Formatter configuration UI in Preferences (`.tm_properties` only)
- Minimal diff computation (whole-buffer replacement is sufficient)
