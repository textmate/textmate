# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TextMate is a macOS-native text editor written in **Objective-C++**. Low-level data structures and algorithms are C++20; GUI code uses Objective-C++ with AppKit/Cocoa. Licensed under GPL v3. Minimum deployment target: macOS 14.0.

This is the `tectiv3/textmate` fork, based on the (unmaintained) `textmate/textmate` upstream. Active community fork: `tagliala/textmate`.

## Build Commands

```bash
make debug           # Incremental debug build (ASan enabled)
make release         # Incremental release build (LTO, no ASan)
make run             # Build debug and launch
make clean           # Remove all build dirs
```

**Requirements:** Xcode (full install, not just CLT — `ibtool` and `actool` required), `ninja`, `cmake` ≥ 3.21. Swift is optional (only needed for `OakSwiftUI`).

**First-time setup:** submodules must be initialized before the first build:
```bash
git submodule update --init --recursive
```

If the active developer directory isn't pointing to Xcode: `sudo xcode-select -s /Applications/Xcode.app/Contents/Developer`

## Testing

```bash
cd build-debug && ctest --output-on-failure          # run all tests
cd build-debug && ctest -R <pattern> --output-on-failure  # run matching tests
```

Test files live at `Frameworks/<name>/tests/t_*.cc` or `t_*.mm`. The `gen_test` runner (at `bin/`) inlines test source bodies — don't compile test files separately.

## Repository Structure

```
Applications/   - 9 apps: TextMate (main), mate, commit, gtm, indent, etc.
Frameworks/     - Self-contained frameworks (core logic + GUI)
PlugIns/        - Plugin system (dialog, dialog-1.x)
Shared/         - Shared PCH headers, oak utility includes
vendor/         - External deps (Onigmo regex, kvdb)
bin/            - Build tools (gen_test, CxxTest) and release scripts
cmake/          - CMake helper functions (TextMateHelpers.cmake)
```

## Architecture

### Core Data Structures

The text storage stack, from bottom to top:

1. **`oak::basic_tree_t`** (`Shared/`) — Balanced AA-tree / binary indexed tree. O(log N) insert/delete, O(1) prefix accumulation. Used as the backbone for all indexed storage.
2. **`ng::detail::storage_t`** (`Frameworks/buffer/`) — Chunked byte storage built on `oak::basic_tree_t`. Efficient O(log N) insert/delete for large strings.
3. **`ng::buffer_t`** (`Frameworks/buffer/`) — Adds semantics: line/column tracking, spelling, scope assignment (syntax highlighting), marks. This is the central document model.
4. **`ng::indexed_map_t`** — Segment tree built on `oak::basic_tree_t`; maps `ssize_t` ranges to values. Used internally by buffer and layout.
5. **`ng::layout_t`** (`Frameworks/layout/`) — Holds a `ng::buffer_t` plus viewport dimensions; calculates visual line wrapping, folding, geometry, and renders into a `CGContext`.

### GUI Layer

- **`OakTextView`** (`Frameworks/OakTextView/`) — Main text view. Wires `ng::buffer_t` + `ng::layout_t` together. Handles Cocoa key binding input. Contains `GutterView` (line numbers, folds, bookmarks) and `OTVStatusBar`.
- **`OakDocumentView`** — Composes `OakTextView`, `GutterView`, and `OTVStatusBar` into a single view.
- **`DocumentWindow`** (`Frameworks/DocumentWindow/`) — Document/session management, window controller.
- **`OakAppKit`** (`Frameworks/OakAppKit/`) — AppKit utilities and extensions used throughout.

### Notable Frameworks

- **`lsp/`** (`Frameworks/lsp/`) — Native LSP client. `LSPManager` manages server lifecycle; `LSPClient` handles JSON-RPC over stdio. `CopilotManager` handles GitHub Copilot ghost-text/completions as a special LSP case.
- **`command/`** — Bundle command execution via `command::runner_t` (spawns processes with piped stdio).
- **`parse/`** — Grammar/syntax parsing engine (TextMate grammar format).
- **`scope/`** — Scope selectors and hierarchy for syntax highlighting and bundle dispatch.
- **`settings/`** — `.tm_properties` file loading and scope-based settings resolution.
- **`scm/`** — Source control integration (git, svn, hg).
- **`bundles/`** — Bundle loading and index; `BundleEditor`/`BundlesManager` for the GUI.
- **`find/`** — Find/replace, including find-in-project.

### Entry Point

`Applications/TextMate/src/main.mm` — Sets app support dir, registers signal handlers, calls `NSApplicationMain()`.

Use `oak::application_t::support("relative/path")` (from `<OakSystem/application.h>`) instead of hardcoding `~/Library/Application Support/TextMate/`.

## Code Conventions

- **Indentation:** Hard tabs, width 3
- **Namespaces:** `oak::` (utilities), `ng::` (text/buffer engine)
- **Type names:** `_t` suffix for types, `_ptr` for smart pointers
- **Pointer style:** `type* var` (not `type *var`)
- **Visibility:** `-fvisibility=hidden` by default
- **Memory:** `std::shared_ptr` for C++; ARC for Objective-C
- **Header guards:** `#ifndef SOMETHING_H_RANDOMHASH`
- **No `@available` checks** for APIs available since macOS 14

## Build Quirks

- `-ObjC` linker flag required to load ObjC categories from static libraries.
- Framework resources (icons, images, plists) must be added to the TextMate app `CMakeLists.txt` — static libs can't carry resources.
- Plugin `.tmplugin` bundles need ad-hoc codesigning before embedding.
- Bundle ID is `com.macromates.TextMate-dev` to coexist with production TextMate.

## xib Files

When editing a `.xib`, check `git diff` before committing — Xcode sometimes writes many unrelated changes. If the diff is noisy, revert and re-open the pristine file in Xcode, save it (without changes) as a separate commit: `Save xib file with Xcode <version>`, then re-apply your change.
