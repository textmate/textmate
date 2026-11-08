# CLAUDE.md - TextMate Development Guide

### Workflow
- always activate serena project first (with full cwd path)
- always list serena memories and always read task completion guideline


## Project Overview

TextMate is a macOS-native text editor written in **Objective-C++**. Low-level data structures and algorithms are C++20; GUI code uses Objective-C++ with AppKit/Cocoa. Licensed under GPL v3. Minimum deployment target: macOS 14.0.

## Repository Structure

```
Applications/   - 9 apps (TextMate, mate, commit, gtm, indent, etc.)
Frameworks/     - 42 self-contained frameworks (core logic + GUI)
PlugIns/        - Plugin system (dialog, dialog-1.x) — incorporated into repo
Shared/         - Shared PCH headers, oak utility includes
vendor/         - External deps (Onigmo, kvdb)
bin/            - Build tools (gen_test, CxxTest) and release scripts
cmake/          - CMake helper functions (TextMateHelpers.cmake)
```

## Build System

- **Build tool:** CMake + Ninja
- **Bundle ID:** `com.macromates.TextMate-dev` (coexists with production TextMate)

### Key Build Commands

```bash
make                 # Incremental debug build (ASan enabled)
make release         # Incremental release build (LTO, no ASan)
make run             # Build debug and launch
make clean           # Remove all build dirs
make package         # Prepare release tag, then ship it end-to-end
make publish         # Push tag, create GitHub release, update Homebrew tap
make ship            # Build/sign/notarize HEAD's tag and publish it, no version bump
```

### Dependencies

ninja, cmake, swift (optional, for OakSwiftUI framework)

### Build Quirks

- `-ObjC` linker flag required to load ObjC categories from static libraries
- OakDebug linked unconditionally (symbols used in all build configs)
- Plugin `.tmplugin` bundles need ad-hoc codesigning before embedding in app
- gen_test runner inlines test source bodies — don't compile test files separately
- Framework resources (icons, images, plists) must be added to TextMate app CMakeLists since static libs can't carry resources

## Architecture

### Core Data Structures (Frameworks/)

- **buffer/** - Text buffer: `ng::buffer_t` built on `ng::detail::storage_t` (chunked text via `oak::basic_tree_t`)
- **editor/** - Editing operations, clipboard, snippets, 40+ editor actions
- **layout/** - Text layout/rendering: `ng::layout_t` wraps buffer + viewport
- **parse/** - Grammar/syntax parsing engine
- **scope/** - Scope management for syntax highlighting
- **text/** - String/text utilities

### GUI Frameworks

- **OakTextView** - Main text editor view + GutterView + OTVStatusBar
- **OakAppKit** - macOS AppKit utilities/extensions
- **DocumentWindow** - Document/session management
- **Preferences** - Settings UI
- **BundleEditor** / **BundlesManager** - Bundle editing and management

### Support Frameworks

- **command/** - Command execution
- **plist/** - Property list I/O
- **regexp/** - Regex (Onigmo-based)
- **scm/** - Source control (git, svn, hg)
- **theme/** - Theme loading
- **file/** - File I/O
- **io/** - I/O abstractions
- **undo/** - Undo/redo stack

### Entry Point

`Applications/TextMate/src/main.mm` - Sets app support dir via `oak::application_t::set_support()`, registers signal handlers, calls `NSApplicationMain()`.

### App Support Path

Use `oak::application_t::support("relative/path")` (from `<OakSystem/application.h>`) instead of hardcoding `~/Library/Application Support/TextMate/`. Set once in `main.mm`.

## Code Conventions

- **Tab size:** 3 (hard tabs, no soft tabs)
- **Namespaces:** `oak::` (utilities), `ng::` (text/buffer engine)
- **Type suffix:** `_t` for type definitions, `_ptr` for smart pointers
- **Pointer style:** `type* var`
- **Visibility:** hidden by default (`-fvisibility=hidden`)
- **Memory:** std::shared_ptr, ARC for Objective-C
- **Header guards:** `#ifndef SOMETHING_H_RANDOMHASH`
- **Commit messages:** Summary < 70 chars, blank line, then reasoning
- **No `@available` checks** for APIs available since macOS 14 (our minimum target)

## Testing

- **Framework:** CxxTest (at `bin/CxxTest`)
- **Test files:** `Frameworks/<name>/tests/t_*.cc` or `t_*.mm`
- **Run tests:** `cd build-debug && ctest --output-on-failure`

### Reading results — `ctest` is the only oracle

The test binaries are not an oracle. Follow this and nothing else.

- **Build first.** `ctest` does not rebuild; a stale binary makes every result meaningless.
  `ninja <target> && ctest -R '^<Suite>$' --output-on-failure 2>&1 | tail -20`
- **Green** is the line `100% tests passed, 0 tests failed out of N`. **Red** is one or more
  `path:line: Assertion failed:` lines plus a `FAILED:` list. Nothing else is a verdict.
- **Never invoke the test binaries directly.** The `bin/gen_test` runners print
  `N of M tests failed` and still exit 0 (`bin/gen_test:143`), so exit codes are not a signal.
  Do not grep `gen_test` or reverse-engineer the harness to interpret a run.
- **Suite names are not framework names.** `EditorContext` is registered from
  `Frameworks/DocumentWindow/context-tests/`; `textmate-mcp` is a Python suite. `-R '^Name$'`
  needs the registered name — list them with `ctest -N` instead of guessing.
- **Run a check once.** A second run only distinguishes flakiness. If two identical runs
  disagree, report it as flaky and stop; a third run is not information.
- **State the expected result** (suite, count, outcome) when asking for verification. When
  actual differs from expected, report the assertion sites and stop — do not iterate until green.


## Upstream Status

This fork (`tectiv3/textmate`) is based on `textmate/textmate` at the latest upstream commit. Upstream is unmaintained. Active community fork: `tagliala/textmate`.
