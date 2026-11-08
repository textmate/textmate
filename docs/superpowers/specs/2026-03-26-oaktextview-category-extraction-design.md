# OakTextView Category Extraction

**Date:** 2026-03-26
**Status:** Approved (rev 2)

## Problem

OakTextView.mm is 7498 lines. LSP, Copilot, completion, hover, and formatting code was added incrementally and is now entangled with core text editing. This makes the file hard to navigate, review, and maintain.

## Design

Split OakTextView.mm into 7 files using Objective-C categories, a private header for shared ivar access, and an internal C++ utilities header.

### File Structure

```
Frameworks/OakTextView/src/
├── OakTextView.mm                    (~4900 lines) Core text editing, input, drawing, drag/drop
├── OakTextView_Private.h             (~100 lines)  Private interface extension with ivars + internal method declarations
├── OakTextView_LSPUtilities.h        (~80 lines)   Shared C++ structs/functions used across categories
├── OakTextView+LSP.mm               (~1100 lines)  Go-to-definition, find references, rename, code actions, workspace edit
├── OakTextView+Copilot.mm           (~400 lines)   Ghost text drawing/scheduling/accepting, copilot completion
├── OakTextView+Completion.mm        (~350 lines)   LSP completion popup, delegate methods
├── OakTextView+Hover.mm             (~500 lines)   Hover tooltip, markdown parsing, hover cache
└── OakTextView+Formatting.mm        (~250 lines)   lspFormatDocument:, format-on-save, custom formatter
```

### OakTextView_Private.h

Contains:
1. The `@interface OakTextView ()` class extension with ALL ivars (lines 474-576)
2. Internal method declarations that categories call on core or other categories

**Declared internal methods (exhaustive list):**
- Core → categories call: `updateSelection`, `redisplayFrom:to:`, `showToolTip:`, `variables`, `scopeContext`, `ensureSelectionIsInVisibleArea:`, `resetBlinkCaretTimer`, `updateSymbol`
- Hover category: `cancelLSPHoverRequest`
- Copilot category: `drawGhostText:inRect:`, `scheduleCopilotGhostText`, `clearGhostText`, `hasGhostText`, `ghostTextExtraHeight`, `acceptGhostText`
- LSP category: `lspTheme` (lazy accessor)
- Formatting category: `performFormatOnSave` (called from core's `documentWillSave:`)

### OakTextView_LSPUtilities.h

Shared C++ constructs used across multiple category files. Contains:

1. **`refresh_helper_t` struct and `AUTO_REFRESH` macro** (currently lines 652-742) — used by core, LSP, Copilot, Completion, and Formatting categories
2. **`lspPositionToOffset()`** (line 5959) — used by LSP and Formatting
3. **`replacementsFromTextEdits()`** (line 5968) — used by LSP and Formatting
4. **`runCustomFormatter()`** (line 5846) — used by Formatting (and previously by core's documentWillSave:, but that path moves to Formatting)

All functions are `inline` or in an anonymous namespace to avoid duplicate symbol errors.

### documentWillSave: Extraction Boundary

`documentWillSave:` (line 1109) has three interleaved concerns:
1. Bundle save callbacks (stays in core)
2. Custom formatter format-on-save (moves to Formatting)
3. LSP format-on-save (moves to Formatting)

**Extraction approach:** Core's `documentWillSave:` handles bundle callbacks, then calls `[self performFormatOnSave]` which lives in OakTextView+Formatting.mm and contains both the custom formatter and LSP formatting paths. This keeps all formatting logic together in one category.

### validateMenuItem: Stays in Core

`validateMenuItem:` (line 3432) contains LSP selector checks (`lspFormatDocument:`, `lspRename:`, `lspCodeActions:`, etc.). These stay in core because:
- ObjC categories cannot chain `validateMenuItem:` implementations
- The validation logic is simple boolean checks (LSP connected? has selection?)
- Adding a new LSP action requires editing both the category and this method — acceptable tradeoff

### Ghost Text in drawRect:

`drawRect:` (core, line 1354) directly reads `_ghostText`, `_ghostTextCaret`, `_ghostTextExtraHeight` ivars for clipping setup (lines 1397-1422) before calling `[self drawGhostText:ctx inRect:aRect]` (in Copilot category). This split is acceptable — the clipping/layout orchestration stays in the rendering pipeline, the actual ghost text drawing is in Copilot. All ivars are accessible via the private header.

### Method Mapping

**OakTextView+LSP.mm:**
- `lspGoToDefinition:` — go to definition with configurable filtering
- `lspFindReferences:` — find all references
- `lspRename:` and rename field/preview methods — inline rename UI
- `lspCodeActions:`, `showCodeActionsMenu:`, `performCodeAction:` — code action menu
- `applyWorkspaceEdit:` and workspace edit handler — apply edits from server
- `bestDefinitionLocation:` — shared filtering helper (replaces duplicated inline code)
- `lspTheme` — lazy accessor for shared theme environment

**OakTextView+Copilot.mm:**
- Ghost text lifecycle: `scheduleCopilotGhostText`, `requestCopilotGhostText`, `cancelCopilotGhostTextRequest`, `showGhostText:`, `clearGhostText`, `hasGhostText`, `ghostTextExtraHeight`, `acceptGhostText`
- Ghost text rendering: `drawGhostText:inRect:`
- Copilot completion: `lspCopilotComplete:`, `insertCopilotCompletion:`, `showCopilotCompletionPopup:cursorCharacter:`

**OakTextView+Completion.mm:**
- `lspComplete:` — trigger LSP completion
- `ensureCompletionPopup` / `caretPointForCompletionPopup`
- `showLSPCompletionPopupWithSuggestions:prefixLength:autoInsertSingle:`
- Delegate: `completionPopup:didSelectItem:`, `completionPopupDidDismiss:`, `completionPopup:resolveItem:`

**OakTextView+Hover.mm:**
- `lspShowHoverInfo:` / `lspRequestHoverAtIndex:`
- Markdown: `parseMarkdownDocumentation:`, `parseMarkdownToAttributedString:`, `parseInlineMarkdown:`
- Tooltip: `createTooltipContentFromHover:`, `showLSPHoverTooltip:atRect:`
- Deprecated: `showLSPHoverTooltipWithContent:atIndex:` (with deprecation attribute)
- `cancelLSPHoverRequest`

**OakTextView+Formatting.mm:**
- `lspFormatDocument:` — manual format command
- `performFormatOnSave` — called from core's `documentWillSave:`, contains both custom formatter and LSP format-on-save paths
- Uses `runCustomFormatter()`, `replacementsFromTextEdits()`, `lspPositionToOffset()` from LSPUtilities header

### Bug Fixes Included

**R1 — Configurable definition filtering:**
Register `kSettingsLSPDefinitionExcludePatternKey` in `Frameworks/settings/src/keys.cc` and `keys.h`. Extract duplicated `_ide_helper`/`vendor/` filtering into `bestDefinitionLocation:` in OakTextView+LSP.mm that reads the setting. Default: empty (no filtering).

**R2 — Line splitting normalization:**
Change `newlineCharacterSet` at line 5136 to `componentsSeparatedByString:@"\n"`.

**R3 — Workspace edit error handling:**
Add `os_log_error` and post `OakShowNotificationNotification` toast on file write failure in `applyWorkspaceEdit:`.

**R4 — Unified _lspTheme:**
Single `- (OakThemeEnvironment*)lspTheme` lazy accessor in OakTextView+LSP.mm using editor's current font. All categories call `[self lspTheme]`.

**R5 — Deprecated method:**
Add `__attribute__((deprecated("Use showLSPHoverTooltip:atRect: instead")))` to declaration in private header.

**S2 — Replace _didApplyCodeActionEdit:**
Replace boolean flag with a completion handler block `_codeActionEditCompletion` in the private header. `performCodeAction:` sets it; workspace edit handler invokes and nils it.

**S3 — Instance-scope lastHandledRequestId:**
Move from static local to ivar `_lastHandledWorkspaceEditRequestId` in private header.

### CMakeLists.txt

Verified: `file(GLOB _src src/*.cc src/*.mm)` picks up new `.mm` files. No changes needed. Headers are not listed (they're found via include paths).

### Build Verification

After extraction, verify:
1. `make` compiles without errors or warnings
2. `cd build-debug && ctest --output-on-failure` passes
3. Launch TextMate and verify: LSP hover, completion, rename, code actions, ghost text, format-on-save, custom formatter
