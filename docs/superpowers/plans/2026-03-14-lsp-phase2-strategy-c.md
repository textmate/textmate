# LSP Phase 2 Strategy C: Background Prefetch

> **For agentic workers:** REQUIRED: Use superpowers:executing-plans to implement this plan.

**Goal:** Prefetch LSP completions in the background on every document change. When user triggers completion, serve from cache for zero-latency popup.

**Spec:** `docs/superpowers/specs/2026-03-14-lsp-phase2-completion-design.md`

---

## File Structure

| File | Change |
|------|--------|
| `Frameworks/lsp/src/LSPClient.h` | Add completion request method with callback |
| `Frameworks/lsp/src/LSPClient.mm` | Implement textDocument/completion request, response callback dispatch |
| `Frameworks/lsp/src/LSPManager.h` | Add prefetch API, cache query, cursor tracking |
| `Frameworks/lsp/src/LSPManager.mm` | Implement prefetch on didChange, completion cache, cache query |
| `Frameworks/editor/src/editor.h` | Extend editor_delegate_t with cache query and cursor report |
| `Frameworks/editor/src/completion.cc` | Query cache before other sources |
| `Frameworks/OakTextView/src/OakTextView.mm` | Report cursor position, implement delegate methods |

---

### Task 1: Add completion request to LSPClient

Same as Strategy A/B Task 1:

- [ ] Add response callback system (id → block dictionary).
- [ ] Add `requestCompletionForURI:line:character:completion:` method.
- [ ] Build and commit.

---

### Task 2: Add prefetch and cache to LSPManager

**Files:** `Frameworks/lsp/src/LSPManager.{h,mm}`

- [ ] Add a completion cache: dictionary keyed by document URI, storing `{items: NSArray<NSString*>*, line: NSUInteger, prefix: NSString*, timestamp: NSDate*}`.

- [ ] Modify `documentDidChange:` (or add a new method `documentDidChangeAtLine:character:`): after sending `textDocument/didChange`, also send `textDocument/completion` for the current cursor position. Store results in cache when they arrive.

- [ ] Add method: `- (NSArray<NSString*>*)cachedCompletionsForDocument:(OakDocument*)document line:(NSUInteger)line prefix:(NSString*)prefix` — returns cached results if they match the document, are on the same line, and the prefix matches. Returns nil on cache miss.

- [ ] Add method: `- (void)updateCursorPosition:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character` — called by OakTextView when cursor moves, used to know where to prefetch.

- [ ] Cache invalidation: clear cache entry when document changes (new prefetch will replace it). Expire entries older than 5s.

- [ ] Build and commit.

---

### Task 3: Bridge editor to cache

**Files:** `Frameworks/editor/src/editor.h`, `Frameworks/editor/src/completion.cc`, `Frameworks/OakTextView/src/OakTextView.mm`

- [ ] In `editor.h`, extend `editor_delegate_t` with:
  ```cpp
  virtual std::vector<std::string> lsp_cached_completions (size_t bow, std::string const& prefix) { return {}; }
  ```

- [ ] In `completion.cc`, at the top of `completions()`: call `_delegate->lsp_cached_completions(bow, prefix)`. If non-empty, use as `commandResult` and skip completionCommand. If empty (cache miss), fall through to existing pipeline.

- [ ] In `OakTextView.mm`:
  - Implement `lsp_cached_completions`: convert position, call `[LSPManager.sharedManager cachedCompletionsForDocument:...]`, convert to std::vector.
  - On cursor movement or text change, call `[LSPManager.sharedManager updateCursorPosition:...]`.

- [ ] Build and test. Commit.

---

### Task 4: Test and verify

- [ ] Open PHP file, type `Auth::`, wait 1s for prefetch, press Esc. Should see instant LSP completions.
- [ ] Type quickly — completions should appear with minimal delay.
- [ ] Verify cache miss falls back to word scanning.
- [ ] Verify no excessive server load (check LSP logs for completion request frequency).
- [ ] Commit any fixes.
