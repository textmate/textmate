# LSP Phase 2: Completion — Design Spec

## Goal

Add native LSP completion to TextMate. When the user triggers completion (Esc key) and an LSP server is active, use `textDocument/completion` results in the existing `OakChoiceMenu` popup. Fall back to existing sources (word scanning, completionCommand, static completions) if LSP returns nothing or is unavailable.

## Common Design (All Strategies)

### LSPClient Changes

Add a completion request method:

```objc
- (void)requestCompletionForDocument:(OakDocument*)document
                                line:(NSUInteger)line
                           character:(NSUInteger)character
                          completion:(void(^)(NSArray<NSString*>* items))callback;
```

This sends `textDocument/completion` and parses the response into `CompletionItem.label` strings. The callback is called on the main thread.

Implementation: track request ID → callback block in a dictionary. When response arrives in `handleMessage:`, look up the callback by ID and invoke it.

### LSPManager Changes

Expose a synchronous or async completion API for the editor layer:

```objc
// For Strategy A (synchronous)
- (NSArray<NSString*>*)completionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character;

// For Strategy B & C (async)
- (void)requestCompletionsForDocument:(OakDocument*)document
                                 line:(NSUInteger)line
                            character:(NSUInteger)character
                           completion:(void(^)(NSArray<NSString*>*))callback;
```

### Integration Point

`editor_t::completions()` in `Frameworks/editor/src/completion.cc` — before the `completionCommand` block. The editor needs a way to call into LSPManager. Since `editor_t` is C++ and LSPManager is ObjC, bridge via `editor_delegate_t` or a free function that the OakTextView layer provides.

### Completion Item Mapping

Use `CompletionItem.label` as the completion string. `filterText` (if present) for matching, `sortText` for ordering. For Phase 2, just use `label` for everything.

### Position Conversion

The editor provides byte offsets (`bow`, `eow`, `from`, `to`). LSP needs line + character (0-indexed). Convert using `buffer.convert(index)` which returns `text::pos_t(line, column)`.

---

## Strategy A: Synchronous with Timeout

### How It Works

In `completions()`, before running `completionCommand`:
1. Check if LSP is available for this scope
2. Send `textDocument/completion` via LSPManager
3. Block on a dispatch semaphore for up to 2s
4. If response arrives, return those results (skip word scanning and completionCommand)
5. If timeout, fall through to existing pipeline

### Files Modified
- `Frameworks/lsp/src/LSPClient.{h,mm}` — add completion request with callback
- `Frameworks/lsp/src/LSPManager.{h,mm}` — add synchronous `completionsForDocument:` (semaphore-based)
- `Frameworks/editor/src/completion.cc` — call LSPManager before completionCommand
- `Frameworks/OakTextView/src/OakTextView.mm` — provide bridge from editor to LSPManager

### Trade-offs
- **Pro:** ~50 lines of new code, minimal architecture change
- **Con:** Blocks main thread, UI freezes during wait

---

## Strategy B: Async Completion

### How It Works

1. User triggers completion (Esc)
2. `completions()` checks if LSP is available. If so, returns empty (or word-scan results as placeholder)
3. Simultaneously fires async `textDocument/completion` request
4. When response arrives on main thread, populates `completion_info_t.suggestions` and triggers `updateChoiceMenu` on OakTextView
5. Choice menu appears (or updates) with LSP results

### Files Modified
- `Frameworks/lsp/src/LSPClient.{h,mm}` — add completion request with callback
- `Frameworks/lsp/src/LSPManager.{h,mm}` — add async `requestCompletionsForDocument:completion:`
- `Frameworks/editor/src/editor.h` — add method to set suggestions externally
- `Frameworks/editor/src/completion.cc` — trigger async request when LSP available
- `Frameworks/OakTextView/src/OakTextView.mm` — handle async result callback, update choice menu

### Trade-offs
- **Pro:** No UI freeze, responsive
- **Con:** More files touched, needs callback path from LSPManager → OakTextView → editor

---

## Strategy C: Background Prefetch

### How It Works

1. On every `didChange` (already debounced 300ms), also send `textDocument/completion` for the current cursor position
2. Cache results keyed by (document URI, line, prefix)
3. When user triggers completion (Esc), check cache first
4. If cache hit with fresh data, return immediately
5. If cache miss, fall back to Strategy A or B behavior

### Files Modified
- `Frameworks/lsp/src/LSPClient.{h,mm}` — add completion request with callback
- `Frameworks/lsp/src/LSPManager.{h,mm}` — add prefetch logic, cache, cursor tracking
- `Frameworks/editor/src/completion.cc` — check cache before other sources
- `Frameworks/OakTextView/src/OakTextView.mm` — report cursor position to LSPManager on changes

### Trade-offs
- **Pro:** Zero-latency popup, best UX
- **Con:** High server load (many requests), cache invalidation complexity, needs cursor position tracking

---

## Scope Exclusions (All Strategies)

- No rich popup UI (icons, detail) — Phase 2+
- No `completionItem/resolve` for additional detail
- No snippet expansion from `CompletionItem.insertText` with snippet format
- No `textEdit` support (just insert label at cursor)
