# TextMate Build System Migration: rave → CMake

**Date:** 2026-03-13
**Status:** Proposed (rev 3 — minimal-first approach)

## Summary

Replace TextMate's custom Ruby-based build system (`bin/rave` + `.rave` DSL) with CMake. This is a focused migration of the build orchestration layer only — no dependency changes, no code changes, no deployment target changes.

**In scope:** Replace `bin/rave`, `configure`, and 63 `.rave` files with CMake.
**Out of scope (follow-up PRs):** Cap'n Proto removal, PCH, deployment target bump, notarization.

## Motivation

- **Ruby dependency for build orchestration** — `bin/rave` is 1,579 lines of Ruby; Ruby no longer ships with Xcode/macOS
- **Broken on ARM Macs** — `configure` hardcodes `/usr/local` paths; Homebrew uses `/opt/homebrew` on Apple Silicon
- **Custom undocumented DSL** — the `.rave` format is understood only by reading `bin/rave` source
- **No IDE integration** — no Xcode project, no CLion support
- **No CI/CD** — GitHub Actions workflow was removed from this fork

## Scope Decisions

| Item | Decision | Rationale |
|------|----------|-----------|
| PCH (precompiled headers) | **Skip** | ~500 source files build in minutes on M-series. Add later if needed. |
| DefaultBundles archive | **External script** | Self-referential build pipeline (build bl → download → archive) is fragile. Separate from build system. |
| Cap'n Proto | **Keep** | `find_package(CapnProto)` + `capnp_generate_cpp()` just works. Removal is independent work. |
| Notarization | **Standalone script** | Async polling workflow, release-only. Wrong abstraction level for CMake. |
| Deployment target | **Keep 10.12** | Orthogonal to build system. Separate PR. |
| Ruby helper scripts | **Keep** | `bin/gen_html`, `bin/gen_test`, `bin/expand_variables` stay. Called via `add_custom_command`. Elimination is follow-up work. |
| UTF-16 .strings | **Always convert** | Require UTF-8 in repo, always `iconv`. No BOM detection. |
| Info.plist syntax | **Use ${VAR} directly** | `configure_file` supports `${VAR}` natively. No rename to `.in`, no `@ONLY`. |

## Architecture

### Project Structure

```
CMakeLists.txt                        # Root: project config, global flags, subdirectories
cmake/
  TextMateHelpers.cmake               # Custom functions: ragel, capnp, xib, assets, codesign
CMakePresets.json                     # Build presets (debug/release)
Frameworks/<name>/CMakeLists.txt      # ~45 files, one per framework
Applications/<name>/CMakeLists.txt    # ~11 files, one per app/tool/service
vendor/<name>/CMakeLists.txt          # Onigmo + kvdb
PlugIns/<name>/CMakeLists.txt         # Dialog plugins
```

### Root CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.21)
project(TextMate LANGUAGES C CXX OBJC OBJCXX)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.12")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 99)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)

# ObjC flags — each flag gets its own generator expression to avoid
# space-in-argument bugs
add_compile_options(
  $<$<COMPILE_LANGUAGE:OBJC>:-fobjc-abi-version=3>
  $<$<COMPILE_LANGUAGE:OBJC>:-fobjc-arc>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-abi-version=3>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-arc>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-call-cxx-cdtors>
)

# Common flags (all languages)
add_compile_options(
  -funsigned-char -Wall -Wwrite-strings -Wformat -Winit-self
  -Wmissing-include-dirs -Wno-parentheses -Wno-sign-compare
  -Wno-switch -Wno-c99-designator
)
add_compile_definitions(
  NULL_STR="\uFFFF"
  REST_API="https://api.textmate.org"
)

# Debug config: -Os + ASan (rave uses -Os even in debug)
set(CMAKE_C_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer")
set(CMAKE_CXX_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer")
set(CMAKE_OBJC_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer")
set(CMAKE_OBJCXX_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer")
set(CMAKE_EXE_LINKER_FLAGS_DEBUG "-fsanitize=address")

# Release config: LTO + dead stripping
set(CMAKE_C_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin")
set(CMAKE_CXX_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin")
set(CMAKE_OBJC_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin")
set(CMAKE_OBJCXX_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin")
set(CMAKE_EXE_LINKER_FLAGS_RELEASE
  "-flto=thin -Wl,-dead_strip -Wl,-dead_strip_dylibs -Wl,-cache_path_lto,${CMAKE_BINARY_DIR}/.lto-cache")

# Linker: ObjC runtime
add_link_options(-fobjc-link-runtime)

# Dependencies
find_package(Boost REQUIRED)
find_package(CapnProto REQUIRED)
find_program(RAGEL_EXECUTABLE ragel REQUIRED)

# Version extraction (replaces rave `capture` command)
file(STRINGS "${CMAKE_SOURCE_DIR}/Applications/TextMate/about/Changes.md"
  _version_line REGEX "^## [0-9]" LIMIT_COUNT 1)
string(REGEX MATCH "[0-9]+\\.[0-9]+(\\.[0-9]+)?" TEXTMATE_VERSION "${_version_line}")

# Shared include path
include_directories(Shared/include)

# Custom build helpers
include(cmake/TextMateHelpers.cmake)

# Code signing identity
set(CS_IDENTITY "-" CACHE STRING "Code signing identity (- for ad-hoc)")

# Conditional OakDebug in debug builds
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(TEXTMATE_DEBUG_LIBS OakDebug)
endif()

# Subdirectories (order doesn't matter, CMake resolves deps)
add_subdirectory(vendor/Onigmo)
add_subdirectory(vendor/kvdb)
# ... all Frameworks
# ... all Applications
# ... PlugIns
```

### Custom Build Functions (cmake/TextMateHelpers.cmake)

Six functions replacing `bin/rave`'s compiler classes:

#### 1. Framework include path setup
```cmake
# Source tree uses flat layout (src/buffer.h), but consumers include
# <buffer/buffer.h>. Create symlink: build/include/buffer/ → src/
function(textmate_framework TARGET)
  set(link "${CMAKE_CURRENT_BINARY_DIR}/include/${TARGET}")
  if(NOT EXISTS "${link}")
    file(CREATE_LINK
      "${CMAKE_CURRENT_SOURCE_DIR}/src"
      "${link}"
      SYMBOLIC)
  endif()
  target_include_directories(${TARGET} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include")
endfunction()
```

#### 2. Ragel (.rl → .cc/.mm)
```cmake
function(target_ragel_sources TARGET)
  foreach(rl_file ${ARGN})
    get_filename_component(name ${rl_file} NAME)
    string(REGEX REPLACE "\\.rl$" "" out_name "${name}")
    if(NOT out_name MATCHES "\\.(cc|mm)$")
      set(out_name "${out_name}.cc")
    endif()
    set(out_file "${CMAKE_CURRENT_BINARY_DIR}/${out_name}")
    add_custom_command(
      OUTPUT ${out_file}
      COMMAND ${RAGEL_EXECUTABLE} -o ${out_file}
        ${CMAKE_CURRENT_SOURCE_DIR}/${rl_file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${rl_file}
      COMMENT "Ragel: ${rl_file}")
    target_sources(${TARGET} PRIVATE ${out_file})
  endforeach()
endfunction()
```

#### 3. Xib compilation (.xib → .nib)
```cmake
function(target_xib_sources TARGET RESOURCE_LOCATION)
  foreach(xib ${ARGN})
    get_filename_component(name ${xib} NAME_WE)
    set(nib "${CMAKE_CURRENT_BINARY_DIR}/${name}.nib")
    add_custom_command(
      OUTPUT ${nib}
      COMMAND xcrun ibtool --compile ${nib}
        --errors --warnings --notices
        --output-format human-readable-text
        ${CMAKE_CURRENT_SOURCE_DIR}/${xib}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${xib}
      COMMENT "Xib: ${xib}")
    target_sources(${TARGET} PRIVATE ${nib})
    set_source_files_properties(${nib} PROPERTIES
      MACOSX_PACKAGE_LOCATION "Resources/${RESOURCE_LOCATION}")
  endforeach()
endfunction()
```

#### 4. Asset catalog (.xcassets → .car)
```cmake
function(target_asset_catalog TARGET XCASSETS_DIR)
  file(GLOB_RECURSE _assets "${CMAKE_CURRENT_SOURCE_DIR}/${XCASSETS_DIR}/*")
  set(car "${CMAKE_CURRENT_BINARY_DIR}/Assets.car")
  add_custom_command(
    OUTPUT ${car}
    COMMAND xcrun actool --compile ${CMAKE_CURRENT_BINARY_DIR}
      --errors --warnings --notices
      --output-format human-readable-text
      --minimum-deployment-target=${CMAKE_OSX_DEPLOYMENT_TARGET}
      --platform=macosx
      ${CMAKE_CURRENT_SOURCE_DIR}/${XCASSETS_DIR}
    DEPENDS ${_assets}
    COMMENT "AssetCatalog: ${XCASSETS_DIR}")
  target_sources(${TARGET} PRIVATE ${car})
  set_source_files_properties(${car} PROPERTIES
    MACOSX_PACKAGE_LOCATION Resources)
endfunction()
```

#### 5. Code signing (with optional entitlements)
```cmake
function(textmate_codesign TARGET IDENTITY)
  cmake_parse_arguments(CS "" "ENTITLEMENTS" "" ${ARGN})
  set(FLAGS --force --options runtime)
  if(CMAKE_BUILD_TYPE STREQUAL "Release")
    list(APPEND FLAGS --timestamp)
  else()
    list(APPEND FLAGS --timestamp=none)
  endif()
  if(CS_ENTITLEMENTS)
    list(APPEND FLAGS --entitlements "${CS_ENTITLEMENTS}")
  endif()
  add_custom_command(TARGET ${TARGET} POST_BUILD
    COMMAND xcrun codesign --sign "${IDENTITY}" ${FLAGS}
      "$<TARGET_BUNDLE_DIR:${TARGET}>"
    COMMENT "Codesign: ${TARGET}")
endfunction()
```

#### 6. Embed target into app bundle
```cmake
function(textmate_embed APP_TARGET DEP_TARGET LOCATION)
  cmake_parse_arguments(EMB "DIRECTORY" "" "" ${ARGN})
  add_dependencies(${APP_TARGET} ${DEP_TARGET})
  if(EMB_DIRECTORY)
    add_custom_command(TARGET ${APP_TARGET} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_directory
        "$<TARGET_BUNDLE_DIR:${DEP_TARGET}>"
        "$<TARGET_BUNDLE_DIR:${APP_TARGET}>/Contents/${LOCATION}")
  else()
    add_custom_command(TARGET ${APP_TARGET} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy
        "$<TARGET_FILE:${DEP_TARGET}>"
        "$<TARGET_BUNDLE_DIR:${APP_TARGET}>/Contents/${LOCATION}/$<TARGET_FILE_NAME:${DEP_TARGET}>")
  endif()
endfunction()
```

### Framework Pattern

**Simple framework (buffer):**
```cmake
add_library(buffer STATIC)
file(GLOB SOURCES src/*.cc src/*.mm)
target_sources(buffer PRIVATE ${SOURCES})
textmate_framework(buffer)
target_link_libraries(buffer PUBLIC bundles io ns parse regexp scope text)
```

**Framework with Ragel (plist):**
```cmake
add_library(plist STATIC)
file(GLOB SOURCES src/*.cc src/*.mm)
target_sources(plist PRIVATE ${SOURCES})
textmate_framework(plist)
target_link_libraries(plist PUBLIC text cf io)
target_ragel_sources(plist src/ascii.rl)
capnp_generate_cpp(CAPNP_SRCS CAPNP_HDRS src/cache.capnp)
target_sources(plist PRIVATE ${CAPNP_SRCS} ${CAPNP_HDRS})
target_link_libraries(plist PRIVATE CapnProto::capnp)
```

**Framework with system deps (OakAppKit):**
```cmake
add_library(OakAppKit STATIC)
file(GLOB SOURCES src/*.cc src/*.mm)
target_sources(OakAppKit PRIVATE ${SOURCES})
textmate_framework(OakAppKit)
target_link_libraries(OakAppKit PUBLIC
  OakFoundation bundles crash file io ns parse regexp settings text theme)
target_link_libraries(OakAppKit PRIVATE
  "-framework Cocoa" "-framework Carbon" "-framework AudioToolbox" "-framework Quartz"
  sqlite3)
```

**Header-only dependency (require_headers):**
```cmake
target_include_directories(Preferences PRIVATE
  $<TARGET_PROPERTY:OakDebug,INTERFACE_INCLUDE_DIRECTORIES>)
```

### Application Target Types

#### Standard app bundle (TextMate)
```cmake
add_executable(TextMate MACOSX_BUNDLE)
```

#### CLI tool (mate, bl, etc.)
```cmake
add_executable(mate)
```

#### QuickLook generator (TextMateQL)
```cmake
add_library(TextMateQL MODULE)
set_target_properties(TextMateQL PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "qlgenerator"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")
target_link_options(TextMateQL PRIVATE -bundle)
```

#### XPC service (SyntaxMate)
```cmake
add_executable(SyntaxMate)
set_target_properties(SyntaxMate PROPERTIES
  MACOSX_BUNDLE TRUE
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist"
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/SyntaxMate.xpc/Contents/MacOS")
```

### TextMate Main App

```cmake
add_executable(TextMate MACOSX_BUNDLE)
file(GLOB SOURCES src/*.cc src/*.mm)
target_sources(TextMate PRIVATE ${SOURCES})

target_link_libraries(TextMate PRIVATE
  DocumentWindow BundleEditor BundleMenu BundlesManager CommitWindow
  CrashReporter Find HTMLOutputWindow MenuBuilder OakAppKit OakCommand
  OakFilterList OakFoundation OakSystem OakTextView Preferences
  SoftwareUpdate authorization bundles cf command crash document io
  kvdb license network ns plist regexp scm settings text theme
  ${TEXTMATE_DEBUG_LIBS}
)

target_link_libraries(TextMate PRIVATE
  "-framework Cocoa" "-framework Carbon" "-framework AppKit"
  "-framework WebKit" "-framework Security"
)

# Info.plist — configure_file handles ${VAR} natively, no rename needed
set(TARGET_NAME "TextMate")
set(APP_MIN_OS "${CMAKE_OSX_DEPLOYMENT_TARGET}")
set(APP_VERSION "${TEXTMATE_VERSION}")
# CS_GET_TASK_ALLOW: can't use generator expressions in configure_file,
# so use if() at configure time
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CS_GET_TASK_ALLOW "true")
else()
  set(CS_GET_TASK_ALLOW "false")
endif()
configure_file(Info.plist ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)
set_target_properties(TextMate PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")

# Resources
file(GLOB RESOURCES resources/*.icns resources/*.png)
target_sources(TextMate PRIVATE ${RESOURCES})
set_source_files_properties(${RESOURCES} PROPERTIES
  MACOSX_PACKAGE_LOCATION Resources)

# Xib, assets
target_xib_sources(TextMate "English.lproj" resources/MainMenu.xib)
target_asset_catalog(TextMate resources/Assets.xcassets)

# Markdown → HTML via existing Ruby script
foreach(md About Changes Contributions Legal Registration)
  set(_in "${CMAKE_CURRENT_SOURCE_DIR}/about/${md}.md")
  set(_out "${CMAKE_CURRENT_BINARY_DIR}/${md}.html")
  add_custom_command(OUTPUT ${_out}
    COMMAND ${CMAKE_SOURCE_DIR}/bin/gen_html ${_in} > ${_out}
    DEPENDS ${_in} ${CMAKE_SOURCE_DIR}/bin/gen_html
    COMMENT "Markdown: ${md}.md")
  target_sources(TextMate PRIVATE ${_out})
  set_source_files_properties(${_out} PROPERTIES
    MACOSX_PACKAGE_LOCATION "Resources/About")
endforeach()

# Embedded tools
textmate_embed(TextMate mate "SharedSupport/bin")
textmate_embed(TextMate commit "SharedSupport/bin")

# Embedded plugins
textmate_embed(TextMate Dialog "PlugIns" DIRECTORY)
textmate_embed(TextMate Dialog2 "PlugIns" DIRECTORY)

# QuickLook + XPC
textmate_embed(TextMate TextMateQL "Library/QuickLook" DIRECTORY)
textmate_embed(TextMate SyntaxMate "XPCServices" DIRECTORY)

# Entitlements + code signing
configure_file(Entitlements.plist ${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist)
textmate_codesign(TextMate "${CS_IDENTITY}"
  ENTITLEMENTS ${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist)
```

### Testing

Tests use `bin/gen_test` (Ruby script that generates runners from `void test_*()` signatures). Keep it as-is:

```cmake
function(textmate_add_tests FRAMEWORK_TARGET)
  file(GLOB TEST_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/tests/t_*.cc
    ${CMAKE_CURRENT_SOURCE_DIR}/tests/t_*.mm)
  if(NOT TEST_SOURCES)
    return()
  endif()

  set(test_target ${FRAMEWORK_TARGET}_test)
  set(runner "${CMAKE_CURRENT_BINARY_DIR}/test_runner.cc")

  add_custom_command(
    OUTPUT ${runner}
    COMMAND ${CMAKE_SOURCE_DIR}/bin/gen_test ${TEST_SOURCES} > ${runner}
    DEPENDS ${TEST_SOURCES} ${CMAKE_SOURCE_DIR}/bin/gen_test
    COMMENT "gen_test: ${FRAMEWORK_TARGET}")

  add_executable(${test_target} ${runner} ${TEST_SOURCES})
  target_link_libraries(${test_target} PRIVATE ${FRAMEWORK_TARGET})
  add_test(NAME ${FRAMEWORK_TARGET} COMMAND ${test_target})
endfunction()
```

### System Libraries & Frameworks Catalog

| Framework | System Libraries | System Frameworks |
|-----------|-----------------|-------------------|
| text | iconv | CoreFoundation |
| cf | | CoreFoundation |
| ns | | Foundation, Cocoa |
| crash | | Foundation |
| OakFoundation | | Foundation, Cocoa |
| OakSystem | | Cocoa, Security |
| OakDebug | | Cocoa, ExceptionHandling |
| OakAppKit | sqlite3 | Cocoa, Carbon, AudioToolbox, Quartz |
| network | curl | SystemConfiguration, Security |
| CrashReporter | z | Foundation, UserNotifications |
| document | | ApplicationServices |
| kvdb | sqlite3 | |
| authorization | | Security |
| mate (app) | | ApplicationServices, Security |
| TextMateQL | | CoreFoundation, QuickLook, AppKit, OSAKit |

### CMake Presets

```json
{
  "version": 6,
  "configurePresets": [
    {
      "name": "debug",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build-debug",
      "cacheVariables": { "CMAKE_BUILD_TYPE": "Debug" }
    },
    {
      "name": "release",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "CS_IDENTITY": "Developer ID Application: ..."
      }
    }
  ]
}
```

User overrides go in `CMakeUserPresets.json` (gitignored).

### Build Commands

```bash
cmake -B build -G Ninja              # Configure (replaces ./configure)
ninja -C build TextMate              # Build
ninja -C build TextMate && open build/TextMate.app  # Build + run
cd build && ctest -R buffer           # Test one framework
cd build && ctest                     # All tests
cmake --preset release                # Release configure
cmake -B xcode -G Xcode              # Xcode project (free)
cmake -B build -G Ninja              # Re-run after adding/removing files
```

## Migration Order

Bottom-up through the dependency tree for incremental validation:

**Phase 1: Infrastructure**
- Root `CMakeLists.txt`, `cmake/TextMateHelpers.cmake`, `CMakePresets.json`

**Phase 2: Leaf frameworks** (no internal deps)
- text, scope, cf, ns, crash, OakFoundation, OakSystem, OakDebug, authorization, undo

**Phase 3: Vendors**
- vendor/Onigmo, vendor/kvdb

**Phase 4: Core engine**
- regexp → parse → buffer → selection → editor
- encoding (with capnp), io

**Phase 5: Service frameworks**
- plist (ragel + capnp), settings (ragel), bundles
- scm, theme, file, command, network, document

**Phase 6: GUI frameworks**
- OakAppKit, OakTabBarView, OakFilterList, OakCommand
- TMFileReference, FileBrowser, MenuBuilder
- layout, HTMLOutput, HTMLOutputWindow, Find
- BundlesManager, BundleEditor, BundleMenu
- OakTextView, DocumentWindow
- Preferences, SoftwareUpdate, CrashReporter, CommitWindow

**Phase 7: Applications**
- CLI tools: mate, bl, gtm, indent, tm_query, pretty_plist
- PrivilegedTool
- TextMateQL (QuickLook), SyntaxMate (XPC)
- TextMate main app
- Plugins: Dialog, Dialog2

**Phase 8: Cleanup**
- Remove: `bin/rave`, `bin/gen_build`, `configure`, `*.rave`, `local-orig.rave`
- Update: `.gitignore`, README

## Files Changed

**Removed:**

| File | Lines | Replaced By |
|------|-------|-------------|
| `bin/rave` | 1,579 | CMake |
| `bin/gen_build` | 33 | (dead code) |
| `configure` | 36 | `cmake -B build` + `find_package()` |
| `local.rave` | varies | CMakeUserPresets.json |
| `local-orig.rave` | ~30 | CMakePresets.json |
| 63 `default.rave` | ~507 | CMakeLists.txt files |

**Kept (follow-up work to replace):**

| File | Reason |
|------|--------|
| `bin/expand_variables` | Used by Info.plist. Partially replaced by `configure_file`. |
| `bin/gen_html` | Used for About window markdown. Called from CMake. |
| `bin/gen_test` | Test runner generator. Called from CMake. |
| `*.capnp` | Kept; uses `find_package(CapnProto)` + `capnp_generate_cpp()`. |

**Added:**

| File | Purpose |
|------|---------|
| `CMakeLists.txt` (root) | Project configuration |
| `cmake/TextMateHelpers.cmake` | 6 custom functions (~150 lines) |
| `CMakePresets.json` | Debug/release presets |
| ~58 `CMakeLists.txt` | One per target |
| `.gitignore` update | `build*/`, `CMakeUserPresets.json` |

## Dependencies After Migration

**Required (unchanged):**
CMake >= 3.21, Ninja, ragel, Boost, Cap'n Proto, multimarkdown, google-sparsehash, Ruby (for helper scripts)

**No longer required for build orchestration:**
Ruby is still needed for `bin/gen_html`, `bin/gen_test`, `bin/expand_variables` but NOT for `bin/rave` (the main build generator). Full Ruby elimination is follow-up work.

## Risks

| Risk | Mitigation |
|------|------------|
| Header include paths don't match after symlink setup | Phase 2 validates leaf frameworks first; fix paths before proceeding |
| GLOB misses files after add/remove | Document re-run requirement; compare file lists against rave |
| CMake ObjC++ bugs | Minimum 3.21; generator expressions per-flag |
| QuickLook/XPC bundle structure wrong | `diff -r` against rave-built bundle |
| `configure_file` breaks existing plist variables | Test plist output; fall back to `bin/expand_variables` if needed |

## Follow-up PRs

1. **Cap'n Proto removal** — replace with NSKeyedArchiver bridge + compiled-in C++ data
2. **Ruby elimination** — replace `bin/gen_html`, `bin/gen_test`, `bin/expand_variables`
3. **PCH** — add if build times warrant it
4. **Deployment target bump** — 10.12 → 12.0+
5. **CI/CD** — GitHub Actions with CMake
6. **DefaultBundles** — decide on shipping strategy (prebuilt tarball vs download script)
