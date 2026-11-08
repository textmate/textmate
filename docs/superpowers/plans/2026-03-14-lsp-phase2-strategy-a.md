# LSP Phase 2 Strategy A: Synchronous Completion with Timeout

> **For agentic workers:** REQUIRED: Use superpowers:executing-plans to implement this plan.

**Goal:** When user triggers completion and LSP is active, send `textDocument/completion`, block up to 2s, use results in OakChoiceMenu. Fall back to existing pipeline on timeout.

**Spec:** `docs/superpowers/specs/2026-03-14-lsp-phase2-completion-design.md`

---

## File Structure

| File | Change |
|------|--------|
| `Frameworks/lsp/src/LSPClient.h` | Add completion request method with callback |
| `Frameworks/lsp/src/LSPClient.mm` | Implement textDocument/completion request, response callback dispatch |
| `Frameworks/lsp/src/LSPManager.h` | Add synchronous completions API |
| `Frameworks/lsp/src/LSPManager.mm` | Implement semaphore-based sync wrapper, position conversion |
| `Frameworks/editor/src/editor.h` | Extend editor_delegate_t with LSP completion method |
| `Frameworks/editor/src/completion.cc` | Call LSP before completionCommand |
| `Frameworks/OakTextView/src/OakTextView.mm` | Implement delegate method bridging to LSPManager |

---

### Task 1: Add completion request to LSPClient

**Files:** `Frameworks/lsp/src/LSPClient.{h,mm}`

- [ ] Add a response callback system: `NSMutableDictionary<NSNumber*, void(^)(json const&)>* _responseCallbacks` ivar. In `handleMessage:` when a response arrives with an id, look up and invoke the callback, then remove it.

- [ ] Add method: `- (void)requestCompletionForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSString*>*))callback` — sends `textDocument/completion` request, registers callback that parses CompletionItem array and extracts labels.

- [ ] Build and commit.

---

### Task 2: Add synchronous completion to LSPManager

**Files:** `Frameworks/lsp/src/LSPManager.{h,mm}`

- [ ] Add method: `- (NSArray<NSString*>*)completionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character` — finds client for document, calls LSPClient's async completion, blocks on `dispatch_semaphore_t` with 2s timeout. Returns results or empty array on timeout.

- [ ] Build and commit.

---

### Task 3: Bridge editor to LSPManager

**Files:** `Frameworks/editor/src/editor.h`, `Frameworks/editor/src/completion.cc`, `Frameworks/OakTextView/src/OakTextView.mm`

- [ ] In `editor.h`, extend `editor_delegate_t` with a virtual method:
  ```cpp
  virtual std::vector<std::string> lsp_completions (size_t bow, std::string const& prefix) { return {}; }
  ```

- [ ] In `completion.cc`, at the top of `completions()` (before line 67 — the completionCommand block), add: call `_delegate->lsp_completions(bow, prefix)`. If non-empty, populate `commandResult` and skip the completionCommand block.

- [ ] In `OakTextView.mm`, find where the editor delegate is implemented (search for `variables_for_bundle_item`). Add the `lsp_completions` override: convert `bow` to `text::pos_t` via `_buffer.convert(bow)`, call `[LSPManager.sharedManager completionsForDocument:... line:pos.line character:pos.column]`, convert NSArray to std::vector.

- [ ] Build and test: `make debug`, launch, trigger completion in a PHP file. Should see LSP results in popup.

- [ ] Commit.

---

### Task 4: Test and verify

- [ ] Open a PHP file, type `Auth::` and press Esc. Should see LSP completions.
- [ ] Open a file with no LSP server. Completion should still work (word scanning fallback).
- [ ] Open a file where LSP is slow — completion should fall back after 2s.
- [ ] Commit any fixes.
