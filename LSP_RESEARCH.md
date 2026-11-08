# LSP Support for TextMate — Research & Implementation Options

## Executive Summary

This document explores approaches for adding Language Server Protocol (LSP) support to TextMate. LSP would bring modern IDE features — code completion, diagnostics, hover info, go-to-definition, and more — powered by the same language servers used by VS Code, Neovim, and other editors.

Three implementation approaches are evaluated: **native C++ integration**, **Swift/Objective-C integration via ChimeHQ libraries**, and **external proxy architecture**. A phased rollout plan is recommended.

---

## Current State

### What TextMate Already Has

| Feature | Current Mechanism | Location |
|---------|------------------|----------|
| **Completion** | `completionCommand` bundle setting runs shell commands; falls back to word-scanning the buffer | `Frameworks/editor/src/completion.cc` |
| **Tooltips** | `OakToolTip` NSPanel, triggered by commands with `output::tool_tip` | `Frameworks/OakAppKit/src/OakToolTip.mm` |
| **Gutter** | Extensible via `GutterViewColumnDataSource` protocol; currently shows bookmarks & foldings | `Frameworks/OakTextView/src/GutterView.h` |
| **Marks** | Buffer mark system — associates metadata with positions, survives edits | `Frameworks/buffer/src/marks.cc` |
| **Document events** | Notifications for content change, save, close | `Frameworks/document/src/OakDocument.h` |
| **Process management** | `command::runner_t` spawns processes with piped stdin/stdout/stderr | `Frameworks/command/src/runner.mm` |
| **Scope system** | Hierarchical scope selectors per file type | `Frameworks/scope/src/` |

### Key Architectural Constraints

1. **Synchronous commands**: Bundle commands block until completion — LSP requires persistent async communication.
2. **No background processes**: The bundle system is request/response with no long-lived process support.
3. **Limited output types**: Commands can produce `text`, `snippet`, `html`, `completion_list`, or `tool_tip` — no inline diagnostics or gutter annotations from commands.

---

## Existing Community Work

### tectiv3/lsp-client (Go proxy)
- **URL**: https://github.com/tectiv3/lsp-client
- **Architecture**: Go HTTP server acts as proxy between TextMate and language servers
- **Supports**: Intelephense (PHP), Volar (Vue), GitHub Copilot
- **How it works**: TextMate sends HTTP POST → proxy translates to JSON-RPC → language server responds → proxy returns result
- **Companion bundle**: https://github.com/tectiv3/PHP-LSP.tmbundle
- **Limitation**: Still constrained by TextMate's synchronous command model; no push-based diagnostics

### Mailing List Consensus
Multiple TextMate mailing list threads discuss LSP. The community consensus: **proper LSP needs native editor support**, not bolt-on bundles, due to the synchronous command limitation.

---

## LSP Protocol Overview

LSP uses **JSON-RPC 2.0** over stdio or sockets. Current stable spec: [LSP 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).

### Priority Features for TextMate

| Priority | Feature | LSP Method | TextMate Integration Point |
|----------|---------|-----------|---------------------------|
| **P0** | Diagnostics | `textDocument/publishDiagnostics` | Gutter marks + inline annotations |
| **P0** | Completion | `textDocument/completion` | Replace/augment `completionCommand` |
| **P0** | Document sync | `didOpen/didChange/didSave/didClose` | `OakDocumentContentDidChangeNotification` + buffer callbacks |
| **P1** | Hover | `textDocument/hover` | `OakToolTip` on mouse hover |
| **P1** | Go to Definition | `textDocument/definition` | Open file at location |
| **P1** | Document Symbols | `textDocument/documentSymbol` | Symbol chooser / navigation |
| **P2** | Find References | `textDocument/references` | Results in Find window |
| **P2** | Formatting | `textDocument/formatting` | Buffer replacement |
| **P2** | Code Actions | `textDocument/codeAction` | Quick-fix menu |
| **P2** | Rename | `textDocument/rename` | Multi-file edits |
| **P5** | Signature Help | `textDocument/signatureHelp` | Parameter hint tooltip |
| **P5** | Inlay Hints | `textDocument/inlayHint` | Inline virtual text (requires layout changes) |

### Protocol Mechanics
- Client and server exchange **capabilities** during `initialize`
- Document synchronization: full or incremental text changes
- Positions use **0-indexed line + UTF-16 code unit offset** (requires conversion from TextMate's UTF-8)
- Server-initiated notifications (e.g., diagnostics) require async handling

---

## Implementation Options

### Option A: Native C++ Integration

**Approach**: Build an LSP client directly into TextMate's C++ framework layer.

**Libraries**:
| Library | Stars | Dependencies | Notes |
|---------|-------|-------------|-------|
| [lsp-framework](https://github.com/leon-bckl/lsp-framework) | ~30 | minimal | Lightweight, supports stdio+socket |
| ~~[LspCpp](https://github.com/kuafuwang/LspCpp)~~ | ~~~200~~ | ~~boost, rapidjson, utfcpp~~ | ~~Ruled out: boost removed from TextMate~~ |

**Architecture**:
```
┌─────────────────────────────────────────────────┐
│ TextMate Application                            │
│  ┌──────────────┐  ┌────────────────────────┐   │
│  │ OakTextView  │  │  OakDocumentView       │   │
│  │  (edits)     │  │  (gutter, diagnostics) │   │
│  └──────┬───────┘  └────────────┬───────────┘   │
│         │                       │               │
│  ┌──────▼───────────────────────▼───────────┐   │
│  │        LSPManager (new framework)        │   │
│  │  - Server lifecycle (spawn/init/shut)    │   │
│  │  - JSON-RPC over stdio                   │   │
│  │  - Document sync                         │   │
│  │  - Request/response dispatch             │   │
│  │  - Capability negotiation                │   │
│  └──────────────────┬───────────────────────┘   │
│                     │ stdio pipes                │
└─────────────────────┼───────────────────────────┘
                      │
              ┌───────▼───────┐
              │ Language Server│  (clangd, pyright, etc.)
              │   (subprocess) │
              └───────────────┘
```

**New framework**: `Frameworks/lsp/`
- `lsp_client_t` — manages a single server connection (spawn, JSON-RPC I/O, request queuing)
- `lsp_manager_t` — maps file types → server configs, manages client lifecycles
- `lsp_types.h` — LSP message types (Position, Range, Diagnostic, CompletionItem, etc.)
- `lsp_json.h` — JSON serialization (could use rapidjson or nlohmann/json)

**Pros**:
- Best performance — no inter-process overhead beyond the language server itself
- Full access to TextMate internals (buffer, marks, gutter, layout)
- Can leverage existing buffer callback system for efficient document sync
- Single binary — no external dependencies at runtime

**Cons**:
- Largest implementation effort (~3-5K lines of new C++)
- Must implement JSON-RPC from scratch or pull in a dependency
- C++ JSON handling is more verbose than Swift/Go

---

### Option B: Swift/Objective-C Integration via ChimeHQ

**Approach**: Use ChimeHQ's Swift LSP libraries, integrated via Objective-C++ bridging.

**Libraries**:
| Library | Description |
|---------|-------------|
| [LanguageServerProtocol](https://github.com/ChimeHQ/LanguageServerProtocol) | Swift types for the full LSP spec |
| [LanguageClient](https://github.com/ChimeHQ/LanguageClient) | Swift LSP client with server lifecycle management |

**Architecture**: A Swift framework wrapping ChimeHQ libraries, exposed to TextMate's Obj-C++ via `@objc` bridging headers.

**Pros**:
- Least code to write — ChimeHQ handles JSON-RPC, types, and server management
- Actively maintained by the Chime editor team
- Swift's `async/await` + `Codable` make LSP message handling clean
- Good macOS citizen — uses Foundation/Dispatch natively

**Cons**:
- Adds Swift to a pure Obj-C++/C++ codebase — build system complexity
- Bridging overhead between Swift ↔ Obj-C++ ↔ C++ layers
- ChimeHQ libraries are SPM-based; TextMate uses Ninja — integration friction
- Less control over internals if ChimeHQ's abstractions don't fit

---

### Option C: External Proxy with Native Bridge (Enhanced tectiv3/lsp-client)

**Approach**: Enhance the existing Go-based `tectiv3/lsp-client` proxy and add a thin native layer in TextMate for async communication.

**Architecture**:
```
┌──────────────────────────────┐
│ TextMate                     │
│  ┌────────────────────────┐  │
│  │ LSPBridge (thin native │  │
│  │ layer, Unix socket)    │  │
│  └───────────┬────────────┘  │
└──────────────┼───────────────┘
               │ Unix domain socket
       ┌───────▼───────┐
       │  lsp-client   │  (Go binary, already exists)
       │  (proxy)      │
       └───────┬───────┘
               │ stdio
       ┌───────▼───────┐
       │ Language Server│
       └───────────────┘
```

**Pros**:
- Builds on existing, working code (tectiv3/lsp-client)
- Minimal changes to TextMate core
- Proxy can be updated independently of the editor
- Go is excellent for concurrent I/O

**Cons**:
- Extra process + IPC hop adds latency
- Two codebases to maintain (Go proxy + native bridge)
- Still need native UI changes for diagnostics/gutter/hover
- Deployment complexity — must bundle or install the Go binary

---

## Comparison Matrix

| Criterion | A: Native C++ | B: Swift/ChimeHQ | C: Go Proxy |
|-----------|:------------:|:----------------:|:-----------:|
| Performance | Best | Good | Acceptable |
| Implementation effort | High | Medium | Low–Medium |
| Codebase consistency | Best (C++) | Mixed (Swift+C++) | Mixed (Go+C++) |
| Dependency burden | Low–Medium | Medium | Low |
| Async capability | Manual (dispatch/threads) | Native (async/await) | Native (goroutines) |
| Maintenance | Self-contained | Depends on ChimeHQ | Two codebases |
| Build system impact | Minimal | Significant (SPM) | Minimal |
| Diagnostic/gutter UI | Full control | Full control | Full control |

---

## Recommended Approach: Option A (Native C++) with Phased Rollout

Given TextMate's pure C++/Obj-C++ codebase and Ninja build system, **native C++ integration** is the most architecturally consistent choice. The `lsp-framework` library (minimal deps, supports stdio+socket) is the best fit.

### Phase 1: Foundation
- New `Frameworks/lsp/` framework
- `lsp_client_t`: JSON-RPC over stdio, process lifecycle
- Document sync (`didOpen`, `didChange`, `didSave`, `didClose`) driven by `OakDocumentContentDidChangeNotification`
- Configuration via `.tm_properties` (e.g., `lspCommand = "clangd"` scoped by file type)

### Phase 2: Core Features
- **Diagnostics**: Store as buffer marks (`diagnostic/error`, `diagnostic/warning`), new gutter column via `GutterViewColumnDataSource`
- **Completion**: Hook into `completionCommand` path — if an LSP server is active, use `textDocument/completion` instead of spawning a shell command
- **Hover**: Track mouse position in `OakTextView`, send `textDocument/hover`, display via `OakToolTip`

### Phase 3: Navigation
- **Go to Definition**: `textDocument/definition` → open file at position
- **Document Symbols**: `textDocument/documentSymbol` → feed symbol chooser
- **Find References**: `textDocument/references` → display in Find results

### Phase 4: Advanced
- Code actions, formatting, rename, signature help
- Inlay hints (requires layout engine changes)
- Multi-root workspace support

---

## Configuration Design

Using TextMate's existing `.tm_properties` system:

```properties
# Global or per-project .tm_properties

# Per file type — uses scope selectors
[ source.python ]
lspCommand   = "pyright-langserver --stdio"

[ source.c, source.c++, source.objc, source.objc++ ]
lspCommand   = "clangd"
lspArgs      = "--background-index --clang-tidy"

[ source.js, source.ts ]
lspCommand   = "typescript-language-server --stdio"

[ source.go ]
lspCommand   = "gopls"

# Global LSP settings
lspEnabled   = true
lspLogLevel  = "warning"    # off, error, warning, info, debug
```

---

## Workspace Root Detection

### Strategy

1. **Project folder open → that's `rootUri`.** Simple, predictable, matches Sublime/Zed approach.

2. **Single file open (no project) → walk upward for markers.** Language-specific markers first, `.git` as fallback:
   - C/C++: `CMakeLists.txt`, `compile_commands.json`, `.clangd`
   - Python: `pyproject.toml`, `setup.py`, `setup.cfg`
   - JS/TS: `tsconfig.json`, `package.json`
   - Go: `go.mod`
   - Rust: `Cargo.toml`
   - Generic: `.git`

3. **`.tm_properties` override** for monorepos and special cases:
   ```properties
   [ source.python ]
   lspCommand   = "pyright-langserver --stdio"
   lspRootPath  = "/path/to/monorepo/backend"
   ```

4. **Multi-root workspaces** — deferred. Most servers work fine with single root.

### How Other Editors Do It

| Editor | Strategy | Monorepo handling |
|--------|----------|-------------------|
| **Sublime Text** | Sidebar folders = workspace roots | Add subproject folders explicitly |
| **Neovim** | Walk upward, check markers, first match wins | Poor — requires custom functions |
| **Zed** | Open folder = root, period | Acknowledged limitation |
| **Kakoune** | Marker walk + env var overrides per filetype | Elegant but niche |

---

## JSON Library

**Decision: [nlohmann/json](https://github.com/nlohmann/json)** (header-only, single file, vendor into repo).

The plist framework cannot parse JSON (it's CFPropertyList — XML/binary plist only). `NSJSONSerialization` exists in one place (SoftwareUpdate) but is awkward from C++. nlohmann/json provides clean ergonomics (`json["method"].get<std::string>()`), and the performance difference vs rapidjson is irrelevant at LSP message sizes (<10KB typical).

---

## Key Technical Considerations

### UTF-16 Position Encoding
LSP uses UTF-16 code unit offsets. TextMate uses UTF-8 internally. The `Frameworks/text/src/utf16.h` header already provides conversion utilities. Position negotiation via `positionEncoding` capability (LSP 3.17+) can request UTF-8 if the server supports it.

### Incremental Sync
For large files, full document sync on every keystroke is expensive. LSP supports incremental sync (`TextDocumentSyncKind.Incremental`) — the buffer's `will_replace`/`did_replace` callbacks provide exactly the byte range needed.

### Server Lifecycle
- Spawn on first file open matching a scope with `lspCommand`
- Keep alive for the session (or until last matching document closes)
- Graceful shutdown on app quit (`shutdown` → `exit`)
- Crash recovery with exponential backoff restart (1s → 2s → 4s → 8s → give up after 5 attempts)
- Reset attempt counter after 60s of stable operation

### Server Resilience

**Per-request timeouts** (tiered by expected latency):
- `completion`, `hover`, `signatureHelp`: **2s** — user is typing, stale results are worthless
- `definition`, `references`, `documentSymbol`: **5s** — user initiated, willing to wait briefly
- `initialize`: **10s** — servers can be slow to start (clangd indexing, etc.)
- `formatting`, `rename`: **10s** — explicit action, user expects a pause

**On timeout**: cancel via `$/cancelRequest`, drop silently for implicit requests (hover/completion), show status bar message for explicit ones (go-to-def, rename).

**Malformed responses**: log and discard. Validate `id` matches a pending request; drop orphan responses.

**Hung server**: if >5 timed-out requests in 30s window, kill and restart.

**Server stderr**: capture and route to an LSP Log output panel, filterable by `lspLogLevel`.

### Thread Safety
- Language server I/O on a background dispatch queue
- Results dispatched to main thread for UI updates
- Buffer access synchronized via existing `ng::buffer_t` callback mechanism

---

## References

- [LSP Specification 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [LSP SDKs](https://microsoft.github.io/language-server-protocol/implementors/sdks/)
- [LspCpp](https://github.com/kuafuwang/LspCpp) — C++ LSP library
- [lsp-framework](https://github.com/leon-bckl/lsp-framework) — Lightweight C++ LSP framework
- [ChimeHQ/LanguageClient](https://github.com/ChimeHQ/LanguageClient) — Swift LSP client
- [ChimeHQ/LanguageServerProtocol](https://github.com/ChimeHQ/LanguageServerProtocol) — Swift LSP types
- [tectiv3/lsp-client](https://github.com/tectiv3/lsp-client) — Existing Go proxy for TextMate
- [f1nnix/LSP.tmbundle](https://github.com/f1nnix/LSP.tmbundle) — Experimental TextMate bundle
- [BBEdit LSP Documentation](https://www.barebones.com/support/bbedit/lsp-notes.html) — Reference native implementation
- [sublimelsp/LSP](https://github.com/sublimelsp/LSP) — Sublime Text's LSP plugin
- [NSHipster: Language Server Protocol](https://nshipster.com/language-server-protocol/) — macOS/Swift LSP overview
- [TextMate Mailing List LSP Discussion](https://lists.macromates.com/hyperkitty/list/textmate@lists.macromates.com/thread/UV2UPBTNHSNMJAXHXGNB6TC5KLAHSJQ2/)
