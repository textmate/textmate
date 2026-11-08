# LSP 3.17 Capabilities Gap Analysis

Audit date: 2026-03-24. Based on TextMate's LSP client implementation in `Frameworks/lsp/`.

## Currently Implemented

| Capability | Status |
|---|---|
| textDocument/publishDiagnostics | Done |
| textDocument/completion + completionItem/resolve | Done |
| textDocument/definition | Done |
| textDocument/hover | Done |
| textDocument/references | Done |
| textDocument/rename + prepareRename | Done |
| textDocument/codeAction + codeAction/resolve | Done |
| textDocument/formatting + rangeFormatting | Done |
| textDocument/didOpen/didChange/didSave/didClose | Done |
| workspace/applyEdit | Done |
| workspace/executeCommand | Done (client→server) |
| workspace/didChangeWatchedFiles | Done (dynamic registration + FSEvents) |
| window/showMessage | Done |
| window/logMessage | Done |
| $/progress | Done |
| client/registerCapability + unregisterCapability | Done (file watching only) |

## Quick Wins — Trivial Fixes

### window/workDoneProgress/create — return null

Server request asking permission to start progress reporting. Currently falls through to generic handler returning `{}`. Should return `null`. ~2 lines.

**Status**: Fixed ✓

### workspace/didChangeConfiguration — send after initialized

Client notification signaling "settings ready." Many servers wait for this before full analysis. Send `{settings:{}}` immediately after `initialized`. ~1 line.

**Status**: Fixed ✓

### window/showMessageRequest — return null

Server request with action buttons (e.g., ESLint "Allow?", rust-analyzer "Download?"). Should return selected action or `null` (dismissed). Returning `{}` is a protocol violation. Minimal fix: return `null`. Full fix: show NSAlert with action buttons.

**Status**: Fixed (returns null) ✓ — NSAlert UI deferred to future work

## Small Effort, High Value

### workspace/configuration — return correctly-shaped array

Server pulls config from client. Expects `[{}, {}, ...]` array matching requested items. Currently gets `{}` — protocol violation that can crash strict servers. Intelephense, pyright, gopls all use this.

**Status**: Fixed (returns empty config array matching items length) ✓

### publishDiagnostics relatedInformation — stop declaring or implement

We declare `relatedInformation: true` but discard the data in `handleDiagnostics`. False capability claim.

**Status**: Fixed (removed false declaration) ✓

## Medium Effort — Future Work

| Capability | Importance | Effort | Notes |
|---|---|---|---|
| workspace/symbol | Important | Medium | Needs fuzzy search UI (Cmd+T equivalent) |
| window/showDocument | Nice-to-have | Small | Open file/URL via NSWorkspace |
| Full workspace/configuration | Important | Medium | Read settings from .tm_properties, pass to server |
| window/showMessageRequest with NSAlert | Important | Medium | Show dialog with action buttons, return selection |
| File operation notifications (willRename/didRename etc.) | Nice-to-have | Medium | Auto-update imports on file rename |
| workspace/didChangeWorkspaceFolders | Nice-to-have | Medium | Multi-root workspace support |

## Not Needed Yet

| Capability | Reason |
|---|---|
| workspace/semanticTokens/refresh | Semantic tokens not implemented |
| workspace/codeLens/refresh | CodeLens not implemented |
| workspace/inlayHint/refresh | Inlay hints not implemented |
| workspace/diagnostic/refresh | Using push diagnostics model |
| workspace/workspaceFolders (request) | Single-root model works fine |

## Root Cause: Generic Request Handler

`handleMessage` returns `json::object()` (`{}`) for any unrecognized server request. This is wrong for:
- `workspace/configuration` — needs array
- `window/workDoneProgress/create` — needs null
- `window/showMessageRequest` — needs null or action item

All fixed by adding named branches in `handleMessage`.
