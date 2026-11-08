# LSP Phase 1: Foundation + Diagnostics — Design Spec

## Goal

Extract the PoC LSP client into a proper framework, add `.tm_properties` configuration, document sync, and diagnostics displayed in the gutter — making LSP a first-class, visible feature in TextMate.

## Architecture

Three components in a new `Frameworks/lsp/` framework:

### LSPClient

Manages one language server process. Refined from the PoC (`OakTextView/src/LSPClient.mm`).

**Responsibilities:**
- Spawn server subprocess via NSTask with stdio pipes
- JSON-RPC 2.0 framing (Content-Length header, nlohmann/json)
- Background dispatch queue for reading, main thread for callbacks
- `initialize` / `initialized` handshake
- Send requests (with ID tracking) and notifications
- Dispatch incoming responses and notifications to registered handlers
- Respond to server-initiated requests (e.g. `client/registerCapability`) with empty success responses to prevent server hangs
- PATH setup for node-based servers (homebrew)

**Key improvement over PoC:** request/response tracking via ID → callback map, so callers can handle responses to specific requests.

### LSPManager

Singleton. Maps project root + server command → LSPClient instance. All document views go through this instead of creating their own client.

**Responsibilities:**
- Read `lspCommand` from `.tm_properties` via `settings_for_path()`
- Parse command string (split on whitespace: first token = executable, rest = args)
- Determine workspace root:
  1. Project folder if open → `rootUri`
  2. Otherwise walk upward for markers (`.git`, `composer.json`, `package.json`, `CMakeLists.txt`, `go.mod`, `Cargo.toml`, `pyproject.toml`)
  3. `lspRootPath` from `.tm_properties` overrides both
- Spawn LSPClient on first matching document open; reuse for same root
- Track open documents per client (set of document identifiers — prevents duplicate `didOpen` from split views)
- Maintain document → client reverse lookup for efficient `didChange`/`didSave` dispatch
- Shut down client when last document for that root closes
- Crash restart with backoff (1s → 2s → 4s → 8s, max 5 attempts, reset counter after 60s stable)
- Graceful shutdown all clients on app quit

**API:**
```objc
+ (instancetype)sharedManager;
- (void)documentDidOpen:(OakDocument*)document;
- (void)documentDidChange:(OakDocument*)document;
- (void)documentDidSave:(OakDocument*)document;
- (void)documentWillClose:(OakDocument*)document;
```

### LSPDiagnosticsManager

Receives `publishDiagnostics` notifications and stores them as buffer marks.

**Responsibilities:**
- Parse diagnostic JSON (uri, range, severity, message)
- Convert LSP positions (0-indexed line + UTF-16 column) to `text::pos_t`
  - Use `Frameworks/text/src/utf16.h` for UTF-16 → UTF-8 conversion
  - Negotiate `positionEncoding` capability with server (prefer UTF-8 if supported)
- Clear previous LSP diagnostics for the file (remove all `diagnostic/*` mark types)
- Set new marks on the buffer using OakDocument's mark API
- Batch mark updates: clear all old marks first, set all new marks, then post a single `OakDocumentMarksDidChangeNotification` manually (or accept multiple notifications from the mark API — acceptable for Phase 1)

## Diagnostics & Gutter Display

**No new gutter column.** Diagnostics reuse the existing bookmarks column.

### Mark Types

LSP diagnostics reuse the same mark types as `mate --set-mark` — the existing images already work:

| LSP Severity | Mark Type | Existing Gutter Image |
|---|---|---|
| 1 (Error) | `error` | `error Template.pdf` |
| 2 (Warning) | `warning` | `warning Template.pdf` |
| 3 (Information) | `note` | `note Template.pdf` |
| 4 (Hint) | `note` | `note Template.pdf` |

No new image resources needed. Mark payload contains the diagnostic message text.

**Removal strategy:** On new `publishDiagnostics`, call `removeAllMarksOfType:` for `error`, `warning`, `note` on the document, then set new marks. This also clears any `mate --set-mark` marks of the same type — acceptable since they represent the same concept.

### How It Works

`enumerateBookmarksAtLine:` returns all mark types at a line. When a diagnostic mark has a non-empty payload, it gets priority 0 (highest) in the existing image selection logic. The mark type string is used directly as the image name by `gutterImage:`.

**Click behavior:** Lines with diagnostic marks show a popover with the message on click (same mechanism as bookmark payload display — already implemented in the existing click handler).

**Coexistence with bookmarks:** Diagnostic marks take visual priority (priority 0 due to non-empty payload) over bookmarks (priority 1). Bookmark toggling is blocked on lines with diagnostics — acceptable for Phase 1.

### Scope-to-LanguageId Mapping

`textDocument/didOpen` requires LSP language IDs. Mapping from TextMate scopes:

```
source.php         → php
source.c           → c
source.c++         → cpp
source.objc        → objective-c
source.objc++      → objective-cpp
source.js          → javascript
source.ts          → typescript
source.python      → python
source.go          → go
source.rust        → rust
source.ruby        → ruby
source.java        → java
source.json        → json
source.css         → css
source.html        → html
source.shell       → shellscript
source.yaml        → yaml
source.xml         → xml
source.sql         → sql
source.lua         → lua
source.swift       → swift
text.html.markdown → markdown
```

Stored as a static map in LSPManager. Fallback: strip `source.` prefix and use as-is.

## Configuration

Using `.tm_properties`:

```properties
[ source.php ]
lspCommand = "intelephense --stdio"

[ source.c, source.c++, source.objc, source.objc++ ]
lspCommand = "clangd --background-index --clang-tidy"

[ source.js, source.ts ]
lspCommand = "typescript-language-server --stdio"

[ source.go ]
lspCommand = "gopls"

# Optional overrides
lspRootPath  = "/path/to/monorepo/backend"
lspEnabled   = true      # default: true when lspCommand is set
lspLogLevel  = "warning"  # off, error, warning, info, debug
```

Read via `settings_for_path()` with the document's path, scope, and directory. `lspCommand` is the trigger — if absent, no LSP for that file type.

## Document Sync

**Full sync** (`TextDocumentSyncKind.Full`): on content change, send entire buffer content.

**Lifecycle notifications:**
- `textDocument/didOpen` — when LSPManager first sees a document for an active client. Tracked by document identifier to prevent duplicates from split views.
- `textDocument/didChange` — on content change, debounced ~300ms. Observe `OakDocumentContentDidChangeNotification`.
- `textDocument/didSave` — observe `OakDocumentDidSaveNotification`
- `textDocument/didClose` — observe `OakDocumentWillCloseNotification`

**Version tracking:** Maintain a per-document version counter, incremented on each `didChange` sent.

## Server Resilience

**Phase 1 scope:** only the `initialize` request needs a timeout (10s). Other request types don't exist yet.

**Malformed responses:** log and discard. Validate `id` matches pending request.

**Server crashes:** restart with backoff (1s → 2s → 4s → 8s, max 5 attempts, reset counter after 60s stable).

**Server stderr:** capture, route through NSLog with `[LSP][stderr]` prefix, filtered by `lspLogLevel`.

## File Structure

| File | Responsibility |
|------|---------------|
| `Frameworks/lsp/CMakeLists.txt` | Framework build config |
| `Frameworks/lsp/src/LSPClient.h` | Single server connection (JSON-RPC, process lifecycle) |
| `Frameworks/lsp/src/LSPClient.mm` | LSPClient implementation |
| `Frameworks/lsp/src/LSPManager.h` | Singleton: config → client mapping, document lifecycle |
| `Frameworks/lsp/src/LSPManager.mm` | LSPManager implementation |
| `Frameworks/lsp/src/LSPDiagnosticsManager.h` | Diagnostics → buffer marks |
| `Frameworks/lsp/src/LSPDiagnosticsManager.mm` | LSPDiagnosticsManager implementation |
| `Frameworks/OakTextView/src/OakDocumentView.mm` | Wire LSPManager calls |

**Removed:** `Frameworks/OakTextView/src/LSPClient.{h,mm}` (PoC code moves to `Frameworks/lsp/`)

## Integration Points

### OakDocumentView

- Remove PoC LSPClient code
- In `setDocument:`: call `[LSPManager.sharedManager documentDidOpen:aDocument]`
- Observe `OakDocumentContentDidChangeNotification` → `[LSPManager.sharedManager documentDidChange:]`
- Already observes `OakDocumentMarksDidChangeNotification` → gutter refresh works automatically

### OakTextView

- Already observes `OakDocumentWillSaveNotification` / `OakDocumentDidSaveNotification`
- Wire `documentDidSave:` through to LSPManager

### CMake

- New `Frameworks/lsp/CMakeLists.txt` with `textmate_framework(lsp)`
- Dependencies: `document`, `settings`, `text`, `ns`, `oak`
- `target_include_directories` for `vendor/nlohmann`
- Add `lsp` to OakTextView's `target_link_libraries`

## Scope Exclusions

- No completion integration (Phase 2)
- No hover tooltips (Phase 2)
- No go-to-definition or navigation (Phase 3)
- No incremental document sync (optimization, later)
- No multi-root workspace support
- No UI beyond gutter icons and click-to-show-message
