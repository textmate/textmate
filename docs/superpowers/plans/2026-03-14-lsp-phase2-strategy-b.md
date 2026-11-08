# LSP Phase 2 Strategy B: Async Completion

> **For agentic workers:** REQUIRED: Use superpowers:executing-plans to implement this plan.

**Goal:** When user triggers completion and LSP is active, fire async `textDocument/completion` request. When response arrives, populate the choice menu without blocking the UI.

**Spec:** `docs/superpowers/specs/2026-03-14-lsp-phase2-completion-design.md`

---

## File Structure

| File | Change |
|------|--------|
| `Frameworks/lsp/src/LSPClient.h` | Add completion request method with callback |
| `Frameworks/lsp/src/LSPClient.mm` | Implement textDocument/completion request, response callback dispatch |
| `Frameworks/lsp/src/LSPManager.h` | Add async completions API and notification |
| `Frameworks/lsp/src/LSPManager.mm` | Implement async completion, post notification with results |
| `Frameworks/editor/src/editor.h` | Add method to externally set suggestions and a delegate callback for async completions |
| `Frameworks/editor/src/completion.cc` | Trigger async request, return word-scan results as placeholder |
| `Frameworks/OakTextView/src/OakTextView.mm` | Observe completion notification, update choice menu |

---

### Task 1: Add completion request to LSPClient

Same as Strategy A Task 1:

- [ ] Add response callback system (id → block dictionary).
- [ ] Add `requestCompletionForURI:line:character:completion:` method.
- [ ] Build and commit.

---

### Task 2: Add async completion to LSPManager

**Files:** `Frameworks/lsp/src/LSPManager.{h,mm}`

- [ ] Add an NSNotificationName: `LSPCompletionsDidArrive` (or similar).

- [ ] Add method: `- (void)requestCompletionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character` — finds client, calls async completion. When callback fires on main thread, post `LSPCompletionsDidArrive` notification with results in userInfo.

- [ ] Build and commit.

---

### Task 3: Trigger async request from editor

**Files:** `Frameworks/editor/src/editor.h`, `Frameworks/editor/src/completion.cc`, `Frameworks/OakTextView/src/OakTextView.mm`

- [ ] In `editor.h`, extend `editor_delegate_t` with:
  ```cpp
  virtual void request_lsp_completions (size_t index, std::string const& prefix) { }
  ```
  Also add a public method on `editor_t`:
  ```cpp
  void set_lsp_completions (std::vector<std::string> const& completions, std::string const& prefix);
  ```
  This method should populate `_completion_info` with the provided suggestions and notify that choices changed.

- [ ] In `completion.cc`, in `completions()`: before the completionCommand block, call `_delegate->request_lsp_completions(bow, prefix)`. Don't wait — continue to word scanning so the user gets immediate (if less precise) results. The LSP results will replace them when they arrive.

- [ ] In `OakTextView.mm`:
  - Implement `request_lsp_completions` in the delegate: convert position, call `[LSPManager.sharedManager requestCompletionsForDocument:...]`.
  - Observe `LSPCompletionsDidArrive` notification. When received, call `editor.set_lsp_completions(...)` then `[self updateChoiceMenu:self]`.

- [ ] Build and test. Commit.

---

### Task 4: Test and verify

- [ ] Open PHP file, type `Auth::`, press Esc. Should see word-scan results immediately, then LSP results replace them.
- [ ] Verify no UI freeze during completion.
- [ ] Verify fallback works when no LSP server.
- [ ] Commit any fixes.
