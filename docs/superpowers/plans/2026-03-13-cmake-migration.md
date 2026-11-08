# CMake Migration Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace TextMate's custom Ruby build system (bin/rave + .rave files) with CMake while keeping all existing source code, dependencies, and helper scripts unchanged.

**Architecture:** Root CMakeLists.txt sets global compiler/linker flags matching default.rave. cmake/TextMateHelpers.cmake provides 6 custom functions (framework includes, ragel, xib, asset catalog, codesign, embed). Each of the 58 targets gets its own CMakeLists.txt translated mechanically from its default.rave. Migration proceeds bottom-up through the dependency tree.

**Tech Stack:** CMake >= 3.21, Ninja, Clang (Objective-C++/C++20), Cap'n Proto (via find_package), Ragel, Boost

**Spec:** `docs/superpowers/specs/2026-03-13-cmake-migration-design.md`

---

## Chunk 1: Infrastructure

### Task 1: Root CMakeLists.txt

**Files:**
- Create: `CMakeLists.txt`

- [ ] **Step 1: Create root CMakeLists.txt with project definition and global flags**

```cmake
cmake_minimum_required(VERSION 3.21)
project(TextMate LANGUAGES C CXX OBJC OBJCXX)

set(CMAKE_OSX_DEPLOYMENT_TARGET "10.12")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 99)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)

# ObjC flags — one flag per generator expression
add_compile_options(
  $<$<COMPILE_LANGUAGE:OBJC>:-fobjc-abi-version=3>
  $<$<COMPILE_LANGUAGE:OBJC>:-fobjc-arc>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-abi-version=3>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-arc>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-call-cxx-cdtors>
)

# Common flags (all languages) — from default.rave FLAGS
add_compile_options(
  -funsigned-char
  -Wall -Wwrite-strings -Wformat -Winit-self -Wmissing-include-dirs
  -Wno-parentheses -Wno-sign-compare -Wno-switch -Wno-c99-designator
)
add_compile_definitions(
  "NULL_STR=\"\\uFFFF\""
  "REST_API=\"https://api.textmate.org\""
)

# Debug: -Os + ASan (rave debug config uses -Os)
set(CMAKE_C_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer" CACHE STRING "" FORCE)
set(CMAKE_OBJC_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer" CACHE STRING "" FORCE)
set(CMAKE_OBJCXX_FLAGS_DEBUG "-Os -g -fsanitize=address -fno-omit-frame-pointer" CACHE STRING "" FORCE)
set(CMAKE_EXE_LINKER_FLAGS_DEBUG "-fsanitize=address" CACHE STRING "" FORCE)

# Release: LTO + dead stripping
set(CMAKE_C_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_OBJC_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_OBJCXX_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_EXE_LINKER_FLAGS_RELEASE
  "-flto=thin -Wl,-dead_strip -Wl,-dead_strip_dylibs -Wl,-cache_path_lto,${CMAKE_BINARY_DIR}/.lto-cache"
  CACHE STRING "" FORCE)

# ObjC runtime linking
add_link_options(-fobjc-link-runtime)

# Dependencies
find_package(Boost REQUIRED)
find_package(CapnProto REQUIRED)
find_program(RAGEL_EXECUTABLE ragel REQUIRED)

# Version extraction (replaces rave `capture` command)
file(STRINGS "${CMAKE_SOURCE_DIR}/Applications/TextMate/about/Changes.md"
  _version_line REGEX "^## [0-9]" LIMIT_COUNT 1)
string(REGEX MATCH "[0-9]+\\.[0-9]+(\\.[0-9]+)?" TEXTMATE_VERSION "${_version_line}")

# Shared includes
include_directories(Shared/include)

# Helpers
include(cmake/TextMateHelpers.cmake)

# Code signing identity
set(CS_IDENTITY "-" CACHE STRING "Code signing identity (- for ad-hoc)")

# Debug-only OakDebug dependency (rave: `require OakDebug` in debug config)
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(TEXTMATE_DEBUG_LIBS OakDebug)
endif()

# === Vendor ===
add_subdirectory(vendor/Onigmo)
add_subdirectory(vendor/kvdb)

# === Leaf Frameworks (no internal deps or only text) ===
add_subdirectory(Frameworks/text)
add_subdirectory(Frameworks/scope)
add_subdirectory(Frameworks/crash)
add_subdirectory(Frameworks/OakFoundation)
add_subdirectory(Frameworks/cf)
add_subdirectory(Frameworks/MenuBuilder)

# === Low-level frameworks ===
add_subdirectory(Frameworks/ns)
add_subdirectory(Frameworks/OakSystem)
add_subdirectory(Frameworks/OakDebug)
add_subdirectory(Frameworks/io)
add_subdirectory(Frameworks/authorization)
add_subdirectory(Frameworks/regexp)
add_subdirectory(Frameworks/encoding)

# === Core engine ===
add_subdirectory(Frameworks/parse)
add_subdirectory(Frameworks/bundles)
add_subdirectory(Frameworks/plist)
add_subdirectory(Frameworks/selection)
add_subdirectory(Frameworks/buffer)
add_subdirectory(Frameworks/undo)
add_subdirectory(Frameworks/editor)
add_subdirectory(Frameworks/settings)
add_subdirectory(Frameworks/theme)

# === Services ===
add_subdirectory(Frameworks/scm)
add_subdirectory(Frameworks/network)
add_subdirectory(Frameworks/updater)
add_subdirectory(Frameworks/file)
add_subdirectory(Frameworks/command)
add_subdirectory(Frameworks/layout)
add_subdirectory(Frameworks/license)

# === GUI frameworks ===
add_subdirectory(Frameworks/OakAppKit)
add_subdirectory(Frameworks/TMFileReference)
add_subdirectory(Frameworks/SoftwareUpdate)
add_subdirectory(Frameworks/Preferences)
add_subdirectory(Frameworks/OakTabBarView)
add_subdirectory(Frameworks/OakFilterList)
add_subdirectory(Frameworks/HTMLOutput)
add_subdirectory(Frameworks/HTMLOutputWindow)
add_subdirectory(Frameworks/OakCommand)
add_subdirectory(Frameworks/BundlesManager)
add_subdirectory(Frameworks/BundleMenu)
add_subdirectory(Frameworks/BundleEditor)
add_subdirectory(Frameworks/Find)
add_subdirectory(Frameworks/FileBrowser)
add_subdirectory(Frameworks/OakTextView)
add_subdirectory(Frameworks/CrashReporter)
add_subdirectory(Frameworks/document)
add_subdirectory(Frameworks/DocumentWindow)
add_subdirectory(Frameworks/CommitWindow)

# === Applications ===
add_subdirectory(Applications/mate)
add_subdirectory(Applications/bl)
add_subdirectory(Applications/gtm)
add_subdirectory(Applications/indent)
add_subdirectory(Applications/pretty_plist)
add_subdirectory(Applications/tm_query)
add_subdirectory(Applications/PrivilegedTool)
add_subdirectory(Applications/QuickLookGenerator)
add_subdirectory(Applications/SyntaxMate)
add_subdirectory(Applications/NewApplication)
add_subdirectory(Applications/TextMate)

# === PlugIns (submodules) ===
add_subdirectory(PlugIns/dialog)
add_subdirectory(PlugIns/dialog-1.x)
```

- [ ] **Step 2: Verify the file parses**

Run: `cmake -B build -G Ninja 2>&1 | head -5`
Expected: Errors about missing subdirectory CMakeLists.txt files (that's fine — we'll create them next)

- [ ] **Step 3: Commit**

```
git add CMakeLists.txt
git commit -m "Add root CMakeLists.txt with global build configuration"
```

---

### Task 2: TextMateHelpers.cmake

**Files:**
- Create: `cmake/TextMateHelpers.cmake`

- [ ] **Step 1: Create cmake directory and TextMateHelpers.cmake**

```cmake
# cmake/TextMateHelpers.cmake
# Custom build functions for TextMate's CMake build system.

# Framework include path setup.
# Source tree uses flat layout (src/buffer.h) but consumers
# include <buffer/buffer.h>. Symlink: build/include/<target>/ → src/
function(textmate_framework TARGET)
  set(_link "${CMAKE_CURRENT_BINARY_DIR}/include/${TARGET}")
  if(NOT EXISTS "${_link}")
    file(CREATE_LINK
      "${CMAKE_CURRENT_SOURCE_DIR}/src"
      "${_link}"
      SYMBOLIC)
  endif()
  target_include_directories(${TARGET} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include")
endfunction()

# Ragel state machine compilation (.rl → .cc or .mm)
function(target_ragel_sources TARGET)
  foreach(_rl ${ARGN})
    get_filename_component(_name "${_rl}" NAME)
    string(REGEX REPLACE "\\.rl$" "" _out_name "${_name}")
    if(NOT _out_name MATCHES "\\.(cc|mm)$")
      set(_out_name "${_out_name}.cc")
    endif()
    set(_out "${CMAKE_CURRENT_BINARY_DIR}/${_out_name}")
    add_custom_command(
      OUTPUT "${_out}"
      COMMAND "${RAGEL_EXECUTABLE}" -o "${_out}" "${CMAKE_CURRENT_SOURCE_DIR}/${_rl}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${_rl}"
      COMMENT "Ragel: ${_rl}")
    target_sources(${TARGET} PRIVATE "${_out}")
  endforeach()
endfunction()

# Xib compilation (.xib → .nib via ibtool)
function(target_xib_sources TARGET RESOURCE_LOCATION)
  foreach(_xib ${ARGN})
    get_filename_component(_name "${_xib}" NAME_WE)
    set(_nib "${CMAKE_CURRENT_BINARY_DIR}/${_name}.nib")
    add_custom_command(
      OUTPUT "${_nib}"
      COMMAND xcrun ibtool --compile "${_nib}"
        --errors --warnings --notices
        --output-format human-readable-text
        "${CMAKE_CURRENT_SOURCE_DIR}/${_xib}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${_xib}"
      COMMENT "Xib: ${_xib}")
    target_sources(${TARGET} PRIVATE "${_nib}")
    set_source_files_properties("${_nib}" PROPERTIES
      MACOSX_PACKAGE_LOCATION "Resources/${RESOURCE_LOCATION}")
  endforeach()
endfunction()

# Asset catalog compilation (.xcassets → .car via actool)
function(target_asset_catalog TARGET XCASSETS_DIR)
  file(GLOB_RECURSE _assets "${CMAKE_CURRENT_SOURCE_DIR}/${XCASSETS_DIR}/*")
  set(_car "${CMAKE_CURRENT_BINARY_DIR}/Assets.car")
  add_custom_command(
    OUTPUT "${_car}"
    COMMAND xcrun actool --compile "${CMAKE_CURRENT_BINARY_DIR}"
      --errors --warnings --notices
      --output-format human-readable-text
      --minimum-deployment-target=${CMAKE_OSX_DEPLOYMENT_TARGET}
      --platform=macosx
      "${CMAKE_CURRENT_SOURCE_DIR}/${XCASSETS_DIR}"
    DEPENDS ${_assets}
    COMMENT "AssetCatalog: ${XCASSETS_DIR}")
  target_sources(${TARGET} PRIVATE "${_car}")
  set_source_files_properties("${_car}" PROPERTIES
    MACOSX_PACKAGE_LOCATION Resources)
endfunction()

# Code signing with optional entitlements
function(textmate_codesign TARGET IDENTITY)
  cmake_parse_arguments(_CS "" "ENTITLEMENTS" "" ${ARGN})
  set(_flags --force --options runtime)
  if(CMAKE_BUILD_TYPE STREQUAL "Release")
    list(APPEND _flags --timestamp)
  else()
    list(APPEND _flags --timestamp=none)
  endif()
  if(_CS_ENTITLEMENTS)
    list(APPEND _flags --entitlements "${_CS_ENTITLEMENTS}")
  endif()
  add_custom_command(TARGET ${TARGET} POST_BUILD
    COMMAND xcrun codesign --sign "${IDENTITY}" ${_flags}
      "$<TARGET_BUNDLE_DIR:${TARGET}>"
    COMMENT "Codesign: ${TARGET}")
endfunction()

# Embed a target into an app bundle.
# Usage: textmate_embed(AppTarget DepTarget "Location/In/Bundle" [DIRECTORY])
# Without DIRECTORY: copies the single executable file.
# With DIRECTORY: copies the entire bundle directory.
function(textmate_embed APP_TARGET DEP_TARGET LOCATION)
  cmake_parse_arguments(_EMB "DIRECTORY" "" "" ${ARGN})
  add_dependencies(${APP_TARGET} ${DEP_TARGET})
  if(_EMB_DIRECTORY)
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

# Test generation using bin/gen_test (Ruby script that generates runners
# from void test_*() and void benchmark_*() signatures)
function(textmate_add_tests FRAMEWORK_TARGET)
  file(GLOB _test_sources
    "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_*.cc"
    "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_*.mm")
  if(NOT _test_sources)
    return()
  endif()

  set(_test_target "${FRAMEWORK_TARGET}_tests")
  set(_runner "${CMAKE_CURRENT_BINARY_DIR}/test_runner.cc")

  add_custom_command(
    OUTPUT "${_runner}"
    COMMAND "${CMAKE_SOURCE_DIR}/bin/gen_test" ${_test_sources} > "${_runner}"
    DEPENDS ${_test_sources} "${CMAKE_SOURCE_DIR}/bin/gen_test"
    COMMENT "gen_test: ${FRAMEWORK_TARGET}")

  add_executable(${_test_target} "${_runner}" ${_test_sources})
  target_link_libraries(${_test_target} PRIVATE ${FRAMEWORK_TARGET} ${TEXTMATE_DEBUG_LIBS})
  target_include_directories(${_test_target} PRIVATE "${CMAKE_SOURCE_DIR}/Shared/include")
  add_test(NAME ${FRAMEWORK_TARGET} COMMAND ${_test_target})
endfunction()
```

- [ ] **Step 2: Commit**

```
git add cmake/TextMateHelpers.cmake
git commit -m "Add CMake helper functions for TextMate build"
```

---

### Task 3: CMakePresets.json and .gitignore update

**Files:**
- Create: `CMakePresets.json`
- Modify: `.gitignore`

- [ ] **Step 1: Create CMakePresets.json**

```json
{
  "version": 6,
  "configurePresets": [
    {
      "name": "debug",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug"
      }
    },
    {
      "name": "release",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release"
      }
    }
  ]
}
```

- [ ] **Step 2: Update .gitignore**

Append to `.gitignore`:
```
build*/
CMakeUserPresets.json
```

- [ ] **Step 3: Commit**

```
git add CMakePresets.json .gitignore
git commit -m "Add CMake presets and update .gitignore"
```

---

## Chunk 2: Vendor Libraries

### Task 4: vendor/Onigmo

**Files:**
- Create: `vendor/Onigmo/CMakeLists.txt`

Source rave:
```
target "Onigmo" {
  headers     vendor/oniguruma.h
  add FLAGS   "-I${dir} -I${dir}/vendor"
  add C_FLAGS "-Wno-incompatible-pointer-types -Wno-char-subscripts"
  sources     src/*.c vendor/{enc/{ascii,euc_jp,iso8859_1,sjis,unicode,utf*},{reg*,st}}.c
  tests       tests/t_*.cc
}
```

- [ ] **Step 1: Create CMakeLists.txt**

```cmake
add_library(Onigmo STATIC)

file(GLOB _src src/*.c)
file(GLOB _enc
  vendor/enc/ascii.c
  vendor/enc/euc_jp.c
  vendor/enc/iso8859_1.c
  vendor/enc/sjis.c
  vendor/enc/unicode.c
  vendor/enc/utf*.c)
file(GLOB _vendor vendor/reg*.c vendor/st.c)

target_sources(Onigmo PRIVATE ${_src} ${_enc} ${_vendor})

# Onigmo exports vendor/oniguruma.h, not from src/
target_include_directories(Onigmo PUBLIC
  "${CMAKE_CURRENT_SOURCE_DIR}/vendor")
target_include_directories(Onigmo PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}"
  "${CMAKE_CURRENT_SOURCE_DIR}/vendor")

target_compile_options(Onigmo PRIVATE
  $<$<COMPILE_LANGUAGE:C>:-Wno-incompatible-pointer-types>
  $<$<COMPILE_LANGUAGE:C>:-Wno-char-subscripts>)

textmate_add_tests(Onigmo)
```

- [ ] **Step 2: Commit**

```
git add vendor/Onigmo/CMakeLists.txt
git commit -m "Add CMakeLists.txt for vendor/Onigmo"
```

---

### Task 5: vendor/kvdb

**Files:**
- Create: `vendor/kvdb/CMakeLists.txt`

Source rave:
```
target "kvdb" {
  headers    vendor/kvdb/kvdb.h
  sources    vendor/kvdb/*.m
  frameworks Cocoa
  libraries  sqlite3
}
```

- [ ] **Step 1: Create CMakeLists.txt**

```cmake
add_library(kvdb STATIC)

file(GLOB _src vendor/kvdb/*.m)
target_sources(kvdb PRIVATE ${_src})

# kvdb exports vendor/kvdb/kvdb.h — include parent so <kvdb/kvdb.h> works
target_include_directories(kvdb PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/vendor")

target_link_libraries(kvdb PRIVATE "-framework Cocoa" sqlite3)
```

- [ ] **Step 2: Commit**

```
git add vendor/kvdb/CMakeLists.txt
git commit -m "Add CMakeLists.txt for vendor/kvdb"
```

---

## Chunk 3: Leaf Frameworks

These have zero or minimal internal dependencies.

### Task 6: Leaf frameworks batch (text, scope, crash, OakFoundation, cf, MenuBuilder)

**Files:**
- Create: `Frameworks/text/CMakeLists.txt`
- Create: `Frameworks/scope/CMakeLists.txt`
- Create: `Frameworks/crash/CMakeLists.txt`
- Create: `Frameworks/OakFoundation/CMakeLists.txt`
- Create: `Frameworks/cf/CMakeLists.txt`
- Create: `Frameworks/MenuBuilder/CMakeLists.txt`

- [ ] **Step 1: Create Frameworks/text/CMakeLists.txt**

```cmake
# rave: require (none), headers src/*.h, sources src/*.cc, tests, frameworks CoreFoundation, libraries iconv
add_library(text STATIC)
file(GLOB _src src/*.cc)
target_sources(text PRIVATE ${_src})
textmate_framework(text)
target_link_libraries(text PUBLIC "-framework CoreFoundation")
find_library(ICONV_LIB iconv REQUIRED)
target_link_libraries(text PUBLIC ${ICONV_LIB})

textmate_add_tests(text)
```

- [ ] **Step 2: Create Frameworks/scope/CMakeLists.txt**

```cmake
# rave: require text
add_library(scope STATIC)
file(GLOB _src src/*.cc)
target_sources(scope PRIVATE ${_src})
textmate_framework(scope)
target_link_libraries(scope PUBLIC text)

textmate_add_tests(scope)
```

- [ ] **Step 3: Create Frameworks/crash/CMakeLists.txt**

```cmake
# rave: no deps
add_library(crash STATIC)
file(GLOB _src src/*.cc)
target_sources(crash PRIVATE ${_src})
textmate_framework(crash)
```

- [ ] **Step 4: Create Frameworks/OakFoundation/CMakeLists.txt**

```cmake
# rave: require text
add_library(OakFoundation STATIC)
file(GLOB _src src/*.mm)
target_sources(OakFoundation PRIVATE ${_src})
textmate_framework(OakFoundation)
target_link_libraries(OakFoundation PUBLIC text "-framework Cocoa")
```

- [ ] **Step 5: Create Frameworks/cf/CMakeLists.txt**

```cmake
# rave: require text, frameworks CoreFoundation ApplicationServices
add_library(cf STATIC)
file(GLOB _src src/*.cc)
target_sources(cf PRIVATE ${_src})
textmate_framework(cf)
target_link_libraries(cf PUBLIC text "-framework CoreFoundation" "-framework ApplicationServices")

textmate_add_tests(cf)
```

- [ ] **Step 6: Create Frameworks/MenuBuilder/CMakeLists.txt**

```cmake
# rave: no require, frameworks Cocoa
add_library(MenuBuilder STATIC)
file(GLOB _src src/*.mm)
target_sources(MenuBuilder PRIVATE ${_src})
textmate_framework(MenuBuilder)
target_link_libraries(MenuBuilder PUBLIC "-framework Cocoa")
```

- [ ] **Step 7: Commit**

```
git add Frameworks/text/CMakeLists.txt Frameworks/scope/CMakeLists.txt Frameworks/crash/CMakeLists.txt Frameworks/OakFoundation/CMakeLists.txt Frameworks/cf/CMakeLists.txt Frameworks/MenuBuilder/CMakeLists.txt
git commit -m "Add CMakeLists.txt for leaf frameworks"
```

---

## Chunk 4: Low-Level Frameworks

### Task 7: ns, OakSystem, OakDebug, io, authorization, regexp, encoding

**Files:**
- Create: `Frameworks/ns/CMakeLists.txt`
- Create: `Frameworks/OakSystem/CMakeLists.txt`
- Create: `Frameworks/OakDebug/CMakeLists.txt`
- Create: `Frameworks/io/CMakeLists.txt`
- Create: `Frameworks/authorization/CMakeLists.txt`
- Create: `Frameworks/regexp/CMakeLists.txt`
- Create: `Frameworks/encoding/CMakeLists.txt`

- [ ] **Step 1: Create Frameworks/ns/CMakeLists.txt**

```cmake
# rave: require text OakFoundation plist
add_library(ns STATIC)
file(GLOB _src src/*.mm)
target_sources(ns PRIVATE ${_src})
textmate_framework(ns)
target_link_libraries(ns PUBLIC text OakFoundation plist "-framework Cocoa")

textmate_add_tests(ns)
```

Note: `ns` depends on `plist` which is defined later, but CMake handles forward references within the same project via target names.

- [ ] **Step 2: Create Frameworks/OakSystem/CMakeLists.txt**

```cmake
# rave: require cf io text
add_library(OakSystem STATIC)
file(GLOB _src src/*.cc)
target_sources(OakSystem PRIVATE ${_src})
textmate_framework(OakSystem)
target_link_libraries(OakSystem PUBLIC cf io text)
```

- [ ] **Step 3: Create Frameworks/OakDebug/CMakeLists.txt**

```cmake
# rave: require_headers text, frameworks Cocoa ExceptionHandling
add_library(OakDebug STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(OakDebug PRIVATE ${_src})
textmate_framework(OakDebug)
# require_headers = include dirs only, no linking
target_include_directories(OakDebug PRIVATE
  $<TARGET_PROPERTY:text,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(OakDebug PUBLIC "-framework Cocoa" "-framework ExceptionHandling")
```

- [ ] **Step 4: Create Frameworks/io/CMakeLists.txt**

```cmake
# rave: require text cf ns regexp crash OakFoundation, frameworks Carbon Security
add_library(io STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(io PRIVATE ${_src})
textmate_framework(io)
target_link_libraries(io PUBLIC text cf ns regexp crash OakFoundation
  "-framework Carbon" "-framework Security")

textmate_add_tests(io)
```

- [ ] **Step 5: Create Frameworks/authorization/CMakeLists.txt**

```cmake
# rave: require io text regexp OakSystem
add_library(authorization STATIC)
file(GLOB _src src/*.cc)
target_sources(authorization PRIVATE ${_src})
textmate_framework(authorization)
target_link_libraries(authorization PUBLIC io text regexp OakSystem)

textmate_add_tests(authorization)
```

- [ ] **Step 6: Create Frameworks/regexp/CMakeLists.txt**

```cmake
# rave: require Onigmo text cf
add_library(regexp STATIC)
file(GLOB _src src/*.cc)
target_sources(regexp PRIVATE ${_src})
textmate_framework(regexp)
target_link_libraries(regexp PUBLIC Onigmo text cf)

textmate_add_tests(regexp)
```

- [ ] **Step 7: Create Frameworks/encoding/CMakeLists.txt**

```cmake
# rave: sources src/*.{mm,capnp}, libraries capnp kj
add_library(encoding STATIC)
file(GLOB _src src/*.mm)
target_sources(encoding PRIVATE ${_src})
textmate_framework(encoding)

capnp_generate_cpp(CAPNP_SRCS CAPNP_HDRS "${CMAKE_CURRENT_SOURCE_DIR}/src/frequencies.capnp")
target_sources(encoding PRIVATE ${CAPNP_SRCS} ${CAPNP_HDRS})
target_link_libraries(encoding PRIVATE CapnProto::capnp)
```

- [ ] **Step 8: Commit**

```
git add Frameworks/ns/CMakeLists.txt Frameworks/OakSystem/CMakeLists.txt Frameworks/OakDebug/CMakeLists.txt Frameworks/io/CMakeLists.txt Frameworks/authorization/CMakeLists.txt Frameworks/regexp/CMakeLists.txt Frameworks/encoding/CMakeLists.txt
git commit -m "Add CMakeLists.txt for low-level frameworks"
```

---

## Chunk 5: Core Engine Frameworks

### Task 8: parse, bundles, plist, selection, buffer, undo, editor, settings, theme

**Files:**
- Create: `Frameworks/parse/CMakeLists.txt`
- Create: `Frameworks/bundles/CMakeLists.txt`
- Create: `Frameworks/plist/CMakeLists.txt`
- Create: `Frameworks/selection/CMakeLists.txt`
- Create: `Frameworks/buffer/CMakeLists.txt`
- Create: `Frameworks/undo/CMakeLists.txt`
- Create: `Frameworks/editor/CMakeLists.txt`
- Create: `Frameworks/settings/CMakeLists.txt`
- Create: `Frameworks/theme/CMakeLists.txt`

- [ ] **Step 1: Create Frameworks/parse/CMakeLists.txt**

```cmake
# rave: require text bundles plist regexp scope
add_library(parse STATIC)
file(GLOB _src src/*.cc)
target_sources(parse PRIVATE ${_src})
textmate_framework(parse)
target_link_libraries(parse PUBLIC text bundles plist regexp scope)

textmate_add_tests(parse)
```

- [ ] **Step 2: Create Frameworks/bundles/CMakeLists.txt**

```cmake
# rave: require OakSystem io plist regexp scope text, frameworks CoreFoundation
add_library(bundles STATIC)
file(GLOB _src src/*.cc)
target_sources(bundles PRIVATE ${_src})
textmate_framework(bundles)
target_link_libraries(bundles PUBLIC OakSystem io plist regexp scope text
  "-framework CoreFoundation")

textmate_add_tests(bundles)
```

- [ ] **Step 3: Create Frameworks/plist/CMakeLists.txt**

```cmake
# rave: require text cf io, sources src/*.{cc,rl,capnp}, libraries capnp kj
add_library(plist STATIC)
file(GLOB _src src/*.cc)
target_sources(plist PRIVATE ${_src})
textmate_framework(plist)
target_link_libraries(plist PUBLIC text cf io "-framework CoreFoundation")

# Ragel sources
target_ragel_sources(plist src/ascii.rl)

# Cap'n Proto
capnp_generate_cpp(CAPNP_SRCS CAPNP_HDRS "${CMAKE_CURRENT_SOURCE_DIR}/src/cache.capnp")
target_sources(plist PRIVATE ${CAPNP_SRCS} ${CAPNP_HDRS})
target_link_libraries(plist PRIVATE CapnProto::capnp)

textmate_add_tests(plist)
```

- [ ] **Step 4: Create Frameworks/selection/CMakeLists.txt**

```cmake
# rave: require text buffer bundles crash regexp
add_library(selection STATIC)
file(GLOB _src src/*.cc)
target_sources(selection PRIVATE ${_src})
textmate_framework(selection)
target_link_libraries(selection PUBLIC text buffer bundles crash regexp)

textmate_add_tests(selection)
```

- [ ] **Step 5: Create Frameworks/buffer/CMakeLists.txt**

```cmake
# rave: require bundles io ns parse regexp scope text
add_library(buffer STATIC)
file(GLOB _src src/*.cc)
target_sources(buffer PRIVATE ${_src})
textmate_framework(buffer)
target_link_libraries(buffer PUBLIC bundles io ns parse regexp scope text)

textmate_add_tests(buffer)
```

- [ ] **Step 6: Create Frameworks/undo/CMakeLists.txt**

```cmake
# rave: require buffer selection
add_library(undo STATIC)
file(GLOB _src src/*.cc)
target_sources(undo PRIVATE ${_src})
textmate_framework(undo)
target_link_libraries(undo PUBLIC buffer selection)
```

- [ ] **Step 7: Create Frameworks/editor/CMakeLists.txt**

```cmake
# rave: require buffer bundles cf command io regexp scope selection settings text
add_library(editor STATIC)
file(GLOB _src src/*.cc)
target_sources(editor PRIVATE ${_src})
textmate_framework(editor)
target_link_libraries(editor PUBLIC buffer bundles cf command io regexp scope selection settings text)

textmate_add_tests(editor)
```

- [ ] **Step 8: Create Frameworks/settings/CMakeLists.txt**

```cmake
# rave: require cf io plist regexp scope text, sources src/*.{cc,rl}
add_library(settings STATIC)
file(GLOB _src src/*.cc)
target_sources(settings PRIVATE ${_src})
textmate_framework(settings)
target_link_libraries(settings PUBLIC cf io plist regexp scope text)

# Ragel
file(GLOB _rl_files src/*.rl)
foreach(_rl ${_rl_files})
  get_filename_component(_name "${_rl}" NAME)
  target_ragel_sources(settings "src/${_name}")
endforeach()

textmate_add_tests(settings)
```

- [ ] **Step 9: Create Frameworks/theme/CMakeLists.txt**

```cmake
# rave: require bundles cf ns scope
add_library(theme STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(theme PRIVATE ${_src})
textmate_framework(theme)
target_link_libraries(theme PUBLIC bundles cf ns scope)

textmate_add_tests(theme)
```

- [ ] **Step 10: Commit**

```
git add Frameworks/parse/CMakeLists.txt Frameworks/bundles/CMakeLists.txt Frameworks/plist/CMakeLists.txt Frameworks/selection/CMakeLists.txt Frameworks/buffer/CMakeLists.txt Frameworks/undo/CMakeLists.txt Frameworks/editor/CMakeLists.txt Frameworks/settings/CMakeLists.txt Frameworks/theme/CMakeLists.txt
git commit -m "Add CMakeLists.txt for core engine frameworks"
```

---

## Chunk 6: Service Frameworks

### Task 9: scm, network, updater, file, command, layout, license

**Files:**
- Create one CMakeLists.txt per framework listed

- [ ] **Step 1: Create Frameworks/scm/CMakeLists.txt**

```cmake
# rave: require text cf io settings regexp, sources src/**/*.cc, frameworks Carbon Security
add_library(scm STATIC)
file(GLOB_RECURSE _src src/*.cc)
target_sources(scm PRIVATE ${_src})
textmate_framework(scm)
target_link_libraries(scm PUBLIC text cf io settings regexp
  "-framework Carbon" "-framework Security")

# Resources
file(GLOB _resources resources/*)
if(_resources)
  target_sources(scm PRIVATE ${_resources})
endif()

textmate_add_tests(scm)
```

- [ ] **Step 2: Create Frameworks/network/CMakeLists.txt**

```cmake
# rave: require text cf io plist OakSystem regexp, libraries curl, frameworks SystemConfiguration Security
add_library(network STATIC)
file(GLOB _src src/*.cc)
target_sources(network PRIVATE ${_src})
textmate_framework(network)
target_link_libraries(network PUBLIC text cf io plist OakSystem regexp)
find_library(CURL_LIB curl REQUIRED)
target_link_libraries(network PRIVATE ${CURL_LIB}
  "-framework SystemConfiguration" "-framework Security")

textmate_add_tests(network)
```

- [ ] **Step 3: Create Frameworks/updater/CMakeLists.txt**

```cmake
# rave: require text io network plist
add_library(updater STATIC)
file(GLOB _src src/*.cc)
target_sources(updater PRIVATE ${_src})
textmate_framework(updater)
target_link_libraries(updater PUBLIC text io network plist)
```

- [ ] **Step 4: Create Frameworks/file/CMakeLists.txt**

```cmake
# rave: require authorization bundles cf command encoding io plist regexp scm settings text
add_library(file STATIC)
file(GLOB _src src/*.cc)
target_sources(file PRIVATE ${_src})
textmate_framework(file)
target_link_libraries(file PUBLIC
  authorization bundles cf command encoding io plist regexp scm settings text)

textmate_add_tests(file)
```

- [ ] **Step 5: Create Frameworks/command/CMakeLists.txt**

```cmake
# rave: require OakAppKit OakFoundation OakSystem buffer bundles cf io plist regexp scope selection settings text
add_library(command STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(command PRIVATE ${_src})
textmate_framework(command)
target_link_libraries(command PUBLIC
  OakAppKit OakFoundation OakSystem buffer bundles cf io plist regexp scope selection settings text)

textmate_add_tests(command)
```

- [ ] **Step 6: Create Frameworks/layout/CMakeLists.txt**

```cmake
# rave: require OakFoundation buffer bundles cf crash io ns plist regexp selection text theme
add_library(layout STATIC)
file(GLOB _src src/*.cc)
target_sources(layout PRIVATE ${_src})
textmate_framework(layout)
target_link_libraries(layout PUBLIC
  OakFoundation buffer bundles cf crash io ns plist regexp selection text theme)

textmate_add_tests(layout)
```

- [ ] **Step 7: Create Frameworks/license/CMakeLists.txt**

```cmake
# rave: require crash text cf ns OakAppKit OakFoundation, frameworks Security
# Special: -Wl,-U,__Z15revoked_serialsv (weak undefined symbol)
add_library(license STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(license PRIVATE ${_src})
textmate_framework(license)
target_link_libraries(license PUBLIC crash text cf ns OakAppKit OakFoundation
  "-framework Security")
target_link_options(license PUBLIC "-Wl,-U,__Z15revoked_serialsv")
```

- [ ] **Step 8: Commit**

```
git add Frameworks/scm/CMakeLists.txt Frameworks/network/CMakeLists.txt Frameworks/updater/CMakeLists.txt Frameworks/file/CMakeLists.txt Frameworks/command/CMakeLists.txt Frameworks/layout/CMakeLists.txt Frameworks/license/CMakeLists.txt
git commit -m "Add CMakeLists.txt for service frameworks"
```

---

## Chunk 7: GUI Frameworks (Part 1)

### Task 10: OakAppKit, TMFileReference, SoftwareUpdate, Preferences, OakTabBarView

- [ ] **Step 1: Create Frameworks/OakAppKit/CMakeLists.txt**

```cmake
# rave: require OakFoundation bundles crash file io ns parse regexp settings text theme
# files resources/* gfx/CloseButton/*.png "Resources"
# cxx_tests tests/gui_*.mm
# frameworks Carbon Cocoa AudioToolbox Quartz, libraries sqlite3
add_library(OakAppKit STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(OakAppKit PRIVATE ${_src})
textmate_framework(OakAppKit)
target_link_libraries(OakAppKit PUBLIC
  OakFoundation bundles crash file io ns parse regexp settings text theme)
target_link_libraries(OakAppKit PRIVATE
  "-framework Carbon" "-framework Cocoa" "-framework AudioToolbox" "-framework Quartz"
  sqlite3)
```

- [ ] **Step 2: Create Frameworks/TMFileReference/CMakeLists.txt**

```cmake
# rave: require_headers scm, frameworks Cocoa
add_library(TMFileReference STATIC)
file(GLOB _src src/*.mm)
target_sources(TMFileReference PRIVATE ${_src})
textmate_framework(TMFileReference)
target_include_directories(TMFileReference PRIVATE
  $<TARGET_PROPERTY:scm,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(TMFileReference PUBLIC "-framework Cocoa")
```

- [ ] **Step 3: Create Frameworks/SoftwareUpdate/CMakeLists.txt**

```cmake
# rave: require OakAppKit, frameworks Cocoa WebKit
add_library(SoftwareUpdate STATIC)
file(GLOB _src src/*.mm)
target_sources(SoftwareUpdate PRIVATE ${_src})
textmate_framework(SoftwareUpdate)
target_link_libraries(SoftwareUpdate PUBLIC OakAppKit "-framework Cocoa" "-framework WebKit")

textmate_add_tests(SoftwareUpdate)
```

- [ ] **Step 4: Create Frameworks/Preferences/CMakeLists.txt**

```cmake
# rave: require BundlesManager OakAppKit OakFoundation MenuBuilder SoftwareUpdate bundles io ns regexp settings text
# require_headers OakTabBarView
add_library(Preferences STATIC)
file(GLOB _src src/*.mm)
target_sources(Preferences PRIVATE ${_src})
textmate_framework(Preferences)
target_link_libraries(Preferences PUBLIC
  BundlesManager OakAppKit OakFoundation MenuBuilder SoftwareUpdate
  bundles io ns regexp settings text)
target_include_directories(Preferences PRIVATE
  $<TARGET_PROPERTY:OakTabBarView,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(Preferences PRIVATE "-framework Cocoa")
```

- [ ] **Step 5: Create Frameworks/OakTabBarView/CMakeLists.txt**

```cmake
# rave: require OakFoundation OakAppKit TMFileReference, frameworks Cocoa
add_library(OakTabBarView STATIC)
file(GLOB _src src/*.mm)
target_sources(OakTabBarView PRIVATE ${_src})
textmate_framework(OakTabBarView)
target_link_libraries(OakTabBarView PUBLIC OakFoundation OakAppKit TMFileReference
  "-framework Cocoa")
```

- [ ] **Step 6: Commit**

```
git add Frameworks/OakAppKit/CMakeLists.txt Frameworks/TMFileReference/CMakeLists.txt Frameworks/SoftwareUpdate/CMakeLists.txt Frameworks/Preferences/CMakeLists.txt Frameworks/OakTabBarView/CMakeLists.txt
git commit -m "Add CMakeLists.txt for GUI frameworks (part 1)"
```

---

## Chunk 8: GUI Frameworks (Part 2)

### Task 11: OakFilterList, HTMLOutput, HTMLOutputWindow, OakCommand, BundlesManager, BundleMenu, BundleEditor, Find, FileBrowser

- [ ] **Step 1: Create Frameworks/OakFilterList/CMakeLists.txt**

```cmake
add_library(OakFilterList STATIC)
file(GLOB _src src/*.mm src/ui/*.mm)
target_sources(OakFilterList PRIVATE ${_src})
textmate_framework(OakFilterList)
target_link_libraries(OakFilterList PUBLIC
  OakAppKit OakFoundation OakSystem TMFileReference bundles document
  ns regexp scm scope settings text "-framework Cocoa" "-framework Carbon")
```

- [ ] **Step 2: Create Frameworks/HTMLOutput/CMakeLists.txt**

```cmake
add_library(HTMLOutput STATIC)
file(GLOB_RECURSE _src src/*.mm)
target_sources(HTMLOutput PRIVATE ${_src})
textmate_framework(HTMLOutput)
target_link_libraries(HTMLOutput PUBLIC
  OakAppKit OakFoundation cf document io ns text "-framework Cocoa" "-framework WebKit")

textmate_add_tests(HTMLOutput)
```

- [ ] **Step 3: Create Frameworks/HTMLOutputWindow/CMakeLists.txt**

```cmake
add_library(HTMLOutputWindow STATIC)
file(GLOB _src src/*.mm)
target_sources(HTMLOutputWindow PRIVATE ${_src})
textmate_framework(HTMLOutputWindow)
target_link_libraries(HTMLOutputWindow PUBLIC HTMLOutput OakAppKit OakFoundation command ns
  "-framework Cocoa")
```

- [ ] **Step 4: Create Frameworks/OakCommand/CMakeLists.txt**

```cmake
add_library(OakCommand STATIC)
file(GLOB _src src/*.mm)
target_sources(OakCommand PRIVATE ${_src})
textmate_framework(OakCommand)
target_link_libraries(OakCommand PUBLIC
  BundleEditor HTMLOutput HTMLOutputWindow OakAppKit OakSystem bundles cf command
  document io ns regexp settings text)
```

- [ ] **Step 5: Create Frameworks/BundlesManager/CMakeLists.txt**

```cmake
add_library(BundlesManager STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(BundlesManager PRIVATE ${_src})
textmate_framework(BundlesManager)
target_link_libraries(BundlesManager PUBLIC
  OakAppKit OakFoundation SoftwareUpdate bundles io ns regexp text "-framework Foundation")
```

- [ ] **Step 6: Create Frameworks/BundleMenu/CMakeLists.txt**

```cmake
add_library(BundleMenu STATIC)
file(GLOB _src src/*.mm)
target_sources(BundleMenu PRIVATE ${_src})
textmate_framework(BundleMenu)
target_link_libraries(BundleMenu PUBLIC OakAppKit OakFoundation bundles text cf ns
  "-framework AppKit")
```

- [ ] **Step 7: Create Frameworks/BundleEditor/CMakeLists.txt**

```cmake
add_library(BundleEditor STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(BundleEditor PRIVATE ${_src})
textmate_framework(BundleEditor)
target_link_libraries(BundleEditor PUBLIC
  BundlesManager OakAppKit OakFoundation OakTextView TMFileReference bundles cf
  command document io ns plist regexp settings text "-framework Cocoa" "-framework AddressBook")
```

- [ ] **Step 8: Create Frameworks/Find/CMakeLists.txt**

```cmake
add_library(Find STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(Find PRIVATE ${_src})
textmate_framework(Find)
target_link_libraries(Find PUBLIC
  MenuBuilder OakAppKit OakFoundation Preferences document io ns regexp settings text
  "-framework Cocoa")
```

- [ ] **Step 9: Create Frameworks/FileBrowser/CMakeLists.txt**

```cmake
add_library(FileBrowser STATIC)
file(GLOB _src src/*.mm src/OFB/*.mm)
target_sources(FileBrowser PRIVATE ${_src})
textmate_framework(FileBrowser)
target_link_libraries(FileBrowser PUBLIC
  MenuBuilder OakAppKit OakCommand OakFoundation TMFileReference Preferences
  bundles io ns regexp scm settings text "-framework Cocoa")
```

- [ ] **Step 10: Commit**

```
git add Frameworks/OakFilterList/CMakeLists.txt Frameworks/HTMLOutput/CMakeLists.txt Frameworks/HTMLOutputWindow/CMakeLists.txt Frameworks/OakCommand/CMakeLists.txt Frameworks/BundlesManager/CMakeLists.txt Frameworks/BundleMenu/CMakeLists.txt Frameworks/BundleEditor/CMakeLists.txt Frameworks/Find/CMakeLists.txt Frameworks/FileBrowser/CMakeLists.txt
git commit -m "Add CMakeLists.txt for GUI frameworks (part 2)"
```

---

## Chunk 9: Top-Level Frameworks

### Task 12: OakTextView, CrashReporter, document, DocumentWindow, CommitWindow

- [ ] **Step 1: Create Frameworks/OakTextView/CMakeLists.txt**

```cmake
add_library(OakTextView STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(OakTextView PRIVATE ${_src})
textmate_framework(OakTextView)
target_link_libraries(OakTextView PUBLIC
  BundleMenu BundlesManager Find HTMLOutput MenuBuilder OakAppKit OakCommand
  OakFilterList OakFoundation OakSystem Preferences buffer bundles cf command
  crash document editor file io layout ns settings text theme
  "-framework Cocoa")
```

- [ ] **Step 2: Create Frameworks/CrashReporter/CMakeLists.txt**

```cmake
add_library(CrashReporter STATIC)
file(GLOB _src src/*.mm)
target_sources(CrashReporter PRIVATE ${_src})
textmate_framework(CrashReporter)
target_link_libraries(CrashReporter PUBLIC Preferences
  "-framework Foundation" "-framework UserNotifications")
find_library(ZLIB z REQUIRED)
target_link_libraries(CrashReporter PRIVATE ${ZLIB})
```

- [ ] **Step 3: Create Frameworks/document/CMakeLists.txt**

```cmake
add_library(document STATIC)
file(GLOB _src src/*.mm src/*.cc)
target_sources(document PRIVATE ${_src})
textmate_framework(document)
target_link_libraries(document PUBLIC
  BundlesManager FileBrowser OakAppKit OakFoundation TMFileReference authorization
  buffer cf command editor encoding file io layout ns plist regexp scm selection
  settings text theme undo "-framework ApplicationServices")

textmate_add_tests(document)
```

- [ ] **Step 4: Create Frameworks/DocumentWindow/CMakeLists.txt**

```cmake
add_library(DocumentWindow STATIC)
file(GLOB _src src/*.mm)
target_sources(DocumentWindow PRIVATE ${_src})
textmate_framework(DocumentWindow)
target_link_libraries(DocumentWindow PUBLIC
  BundleEditor BundlesManager FileBrowser Find HTMLOutputWindow MenuBuilder OakAppKit
  OakTabBarView OakCommand OakFilterList OakFoundation OakSystem OakTextView Preferences
  bundles crash document file io kvdb ns regexp scm settings text
  "-framework Cocoa")
```

- [ ] **Step 5: Create Frameworks/CommitWindow/CMakeLists.txt**

CommitWindow has TWO targets: the framework and a CLI tool.

```cmake
# Framework
add_library(CommitWindow STATIC)
file(GLOB _src src/*.mm)
target_sources(CommitWindow PRIVATE ${_src})
textmate_framework(CommitWindow)
target_link_libraries(CommitWindow PUBLIC
  OakAppKit OakFoundation OakTextView bundles document io ns plist regexp text
  "-framework Cocoa")

# CLI tool: CommitWindowTool (embedded into CommitWindow as "commit")
add_executable(CommitWindowTool tool/commit.mm)
target_include_directories(CommitWindowTool PRIVATE
  $<TARGET_PROPERTY:CommitWindow,INTERFACE_INCLUDE_DIRECTORIES>)
```

- [ ] **Step 6: Commit**

```
git add Frameworks/OakTextView/CMakeLists.txt Frameworks/CrashReporter/CMakeLists.txt Frameworks/document/CMakeLists.txt Frameworks/DocumentWindow/CMakeLists.txt Frameworks/CommitWindow/CMakeLists.txt
git commit -m "Add CMakeLists.txt for top-level frameworks"
```

---

## Chunk 10: CLI Applications

### Task 13: mate, bl, gtm, indent, pretty_plist, tm_query, PrivilegedTool

- [ ] **Step 1: Create Applications/mate/CMakeLists.txt**

```cmake
# rave: require authorization io plist text, frameworks ApplicationServices Security
add_executable(mate)
file(GLOB _src src/*.mm)
target_sources(mate PRIVATE ${_src})
target_link_libraries(mate PRIVATE authorization io plist text
  "-framework ApplicationServices" "-framework Security")
```

- [ ] **Step 2: Create Applications/bl/CMakeLists.txt**

```cmake
# rave: require OakSystem io regexp text updater
add_executable(bl)
file(GLOB _src src/*.cc)
target_sources(bl PRIVATE ${_src})
target_link_libraries(bl PRIVATE OakSystem io regexp text updater)
```

- [ ] **Step 3: Create Applications/gtm/CMakeLists.txt**

```cmake
add_executable(gtm)
file(GLOB _src src/*.cc)
target_sources(gtm PRIVATE ${_src})
target_link_libraries(gtm PRIVATE parse)
```

- [ ] **Step 4: Create Applications/indent/CMakeLists.txt**

```cmake
add_executable(indent)
file(GLOB _src src/*.cc)
target_sources(indent PRIVATE ${_src})
target_link_libraries(indent PRIVATE text io regexp plist)
```

- [ ] **Step 5: Create Applications/pretty_plist/CMakeLists.txt**

```cmake
add_executable(pretty_plist)
file(GLOB _src src/*.cc)
target_sources(pretty_plist PRIVATE ${_src})
target_link_libraries(pretty_plist PRIVATE plist)
```

- [ ] **Step 6: Create Applications/tm_query/CMakeLists.txt**

```cmake
add_executable(tm_query)
file(GLOB _src src/*.cc)
target_sources(tm_query PRIVATE ${_src})
target_link_libraries(tm_query PRIVATE settings)
```

- [ ] **Step 7: Create Applications/PrivilegedTool/CMakeLists.txt**

```cmake
add_executable(PrivilegedTool)
file(GLOB _src src/*.cc)
target_sources(PrivilegedTool PRIVATE ${_src})
target_link_libraries(PrivilegedTool PRIVATE authorization io text)
```

- [ ] **Step 8: Commit**

```
git add Applications/mate/CMakeLists.txt Applications/bl/CMakeLists.txt Applications/gtm/CMakeLists.txt Applications/indent/CMakeLists.txt Applications/pretty_plist/CMakeLists.txt Applications/tm_query/CMakeLists.txt Applications/PrivilegedTool/CMakeLists.txt
git commit -m "Add CMakeLists.txt for CLI applications"
```

---

## Chunk 11: Special Bundle Applications

### Task 14: QuickLookGenerator, SyntaxMate, NewApplication

- [ ] **Step 1: Create Applications/QuickLookGenerator/CMakeLists.txt**

```cmake
# .qlgenerator bundle, not a regular app
add_library(TextMateQL MODULE)
file(GLOB _src src/*.c src/*.mm)
target_sources(TextMateQL PRIVATE ${_src})

set_target_properties(TextMateQL PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "qlgenerator"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")

target_link_options(TextMateQL PRIVATE -bundle)
target_link_libraries(TextMateQL PRIVATE
  OakFoundation buffer bundles cf file io ns plist scope settings theme
  "-framework CoreFoundation" "-framework QuickLook" "-framework AppKit" "-framework OSAKit")
```

- [ ] **Step 2: Create Applications/SyntaxMate/CMakeLists.txt**

```cmake
# XPC service bundle
add_executable(SyntaxMate)
file(GLOB _src src/*.mm)
target_sources(SyntaxMate PRIVATE ${_src})
target_link_libraries(SyntaxMate PRIVATE file theme)

set_target_properties(SyntaxMate PROPERTIES
  MACOSX_BUNDLE TRUE
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist"
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/SyntaxMate.xpc/Contents/MacOS")
```

- [ ] **Step 3: Create Applications/NewApplication/CMakeLists.txt**

```cmake
add_executable(NewApplication MACOSX_BUNDLE)
file(GLOB _src src/*.cc src/*.mm)
target_sources(NewApplication PRIVATE ${_src})
target_link_libraries(NewApplication PRIVATE
  OakFoundation OakAppKit MenuBuilder "-framework Cocoa")

set_target_properties(NewApplication PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")
```

- [ ] **Step 4: Commit**

```
git add Applications/QuickLookGenerator/CMakeLists.txt Applications/SyntaxMate/CMakeLists.txt Applications/NewApplication/CMakeLists.txt
git commit -m "Add CMakeLists.txt for QuickLook, XPC, and NewApplication"
```

---

## Chunk 12: PlugIns (Submodules)

### Task 15: Dialog and Dialog-1.x plugins

Each plugin has a CLI helper tool + a `.tmplugin` bundle (like a `.bundle`, uses `-bundle` linker flag).

**Files:**
- Create: `PlugIns/dialog/CMakeLists.txt`
- Create: `PlugIns/dialog-1.x/CMakeLists.txt`

- [ ] **Step 1: Create PlugIns/dialog/CMakeLists.txt**

```cmake
# CLI tool: tm_dialog2
add_executable(tm_dialog2 tm_dialog2.mm)
target_link_libraries(tm_dialog2 PRIVATE "-framework Foundation")

# Plugin bundle: Dialog2.tmplugin
add_library(Dialog2 MODULE)
file(GLOB _cmd_src Commands/**/*.mm)
target_sources(Dialog2 PRIVATE
  CLIProxy.mm Dialog2.mm TMDCommand.mm ${_cmd_src})

set_target_properties(Dialog2 PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "tmplugin"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")

target_link_options(Dialog2 PRIVATE -bundle)
target_link_libraries(Dialog2 PRIVATE
  "-framework Cocoa" "-framework WebKit" "-framework Quartz")

# Embed tm_dialog2 into plugin Resources
add_dependencies(Dialog2 tm_dialog2)
add_custom_command(TARGET Dialog2 POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
    "$<TARGET_FILE:tm_dialog2>"
    "$<TARGET_BUNDLE_DIR:Dialog2>/Contents/Resources/tm_dialog2")
```

- [ ] **Step 2: Create PlugIns/dialog-1.x/CMakeLists.txt**

```cmake
# CLI tool: tm_dialog
add_executable(tm_dialog tm_dialog.mm)
target_link_libraries(tm_dialog PRIVATE "-framework Foundation")

# Plugin bundle: Dialog.tmplugin
add_library(Dialog MODULE)
target_sources(Dialog PRIVATE
  Dialog.mm TMDChameleon.mm TMDSemaphore.mm)

set_target_properties(Dialog PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "tmplugin"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")

target_link_options(Dialog PRIVATE -bundle)
target_link_libraries(Dialog PRIVATE "-framework Cocoa")

# Embed tm_dialog into plugin Resources
add_dependencies(Dialog tm_dialog)
add_custom_command(TARGET Dialog POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
    "$<TARGET_FILE:tm_dialog>"
    "$<TARGET_BUNDLE_DIR:Dialog>/Contents/Resources/tm_dialog")
```

- [ ] **Step 3: Commit**

```
git add PlugIns/dialog/CMakeLists.txt PlugIns/dialog-1.x/CMakeLists.txt
git commit -m "Add CMakeLists.txt for Dialog plugins"
```

---

## Chunk 13: TextMate Main Application

### Task 16: Applications/TextMate/CMakeLists.txt

**Files:**
- Create: `Applications/TextMate/CMakeLists.txt`

- [ ] **Step 1: Create TextMate app CMakeLists.txt**

```cmake
add_executable(TextMate MACOSX_BUNDLE)
file(GLOB _src src/*.cc src/*.mm)
target_sources(TextMate PRIVATE ${_src})

# Internal framework dependencies
target_link_libraries(TextMate PRIVATE
  BundleEditor BundleMenu BundlesManager CommitWindow CrashReporter DocumentWindow
  Find HTMLOutputWindow MenuBuilder OakAppKit OakCommand OakFilterList OakFoundation
  OakSystem OakTextView Preferences SoftwareUpdate authorization bundles cf command
  crash document io kvdb license network ns plist regexp scm settings text theme
  ${TEXTMATE_DEBUG_LIBS}
)

target_link_libraries(TextMate PRIVATE "-framework Cocoa")

# Info.plist variable expansion
set(TARGET_NAME "TextMate")
set(APP_MIN_OS "${CMAKE_OSX_DEPLOYMENT_TARGET}")
set(APP_VERSION "${TEXTMATE_VERSION}")
string(TIMESTAMP YEAR "%Y")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CS_GET_TASK_ALLOW "true")
else()
  set(CS_GET_TASK_ALLOW "false")
endif()
configure_file(Info.plist ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)
set_target_properties(TextMate PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")

# Icon and image resources
file(GLOB _icons icons/*.icns)
file(GLOB _resources resources/*)
set(_all_resources ${_icons} ${_resources})
target_sources(TextMate PRIVATE ${_all_resources})
set_source_files_properties(${_all_resources} PROPERTIES
  MACOSX_PACKAGE_LOCATION Resources)

# Markdown → HTML for About window (uses existing bin/gen_html Ruby script)
file(GLOB _about_md about/*.md)
foreach(_md ${_about_md})
  get_filename_component(_name "${_md}" NAME_WE)
  set(_html "${CMAKE_CURRENT_BINARY_DIR}/${_name}.html")
  add_custom_command(
    OUTPUT "${_html}"
    COMMAND "${CMAKE_SOURCE_DIR}/bin/gen_html"
      -h "${CMAKE_CURRENT_SOURCE_DIR}/templates/header.html"
      -f "${CMAKE_CURRENT_SOURCE_DIR}/templates/footer.html"
      "${_md}" > "${_html}"
    DEPENDS "${_md}" "${CMAKE_SOURCE_DIR}/bin/gen_html"
    COMMENT "gen_html: ${_name}.md")
  target_sources(TextMate PRIVATE "${_html}")
  set_source_files_properties("${_html}" PROPERTIES
    MACOSX_PACKAGE_LOCATION "Resources/About")
endforeach()

# Embedded CLI tools
textmate_embed(TextMate mate "MacOS")
textmate_embed(TextMate tm_query "MacOS")
textmate_embed(TextMate PrivilegedTool "Resources")

# Embedded plugins (.tmplugin bundles from submodules)
textmate_embed(TextMate Dialog "PlugIns" DIRECTORY)
textmate_embed(TextMate Dialog2 "PlugIns" DIRECTORY)

# Embedded QuickLook plugin
textmate_embed(TextMate TextMateQL "Library/QuickLook" DIRECTORY)

# Entitlements + code signing
configure_file(Entitlements.plist ${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist)
textmate_codesign(TextMate "${CS_IDENTITY}"
  ENTITLEMENTS "${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist")
```

- [ ] **Step 2: Commit**

```
git add Applications/TextMate/CMakeLists.txt
git commit -m "Add CMakeLists.txt for TextMate main application"
```

---

## Chunk 14: Build Verification

### Task 17: First build attempt and fix-up

- [ ] **Step 1: Run CMake configure**

Run: `cmake -B build -G Ninja 2>&1 | tee cmake-output.log`
Expected: May produce errors about missing dependencies or syntax issues. Fix iteratively.

- [ ] **Step 2: Fix any configure errors**

Common issues to expect:
- Missing `find_package` for system libraries
- Incorrect framework names
- Source glob patterns not matching actual files
- Forward dependency issues

Fix each error, re-run cmake until configure succeeds.

- [ ] **Step 3: Run ninja build**

Run: `ninja -C build 2>&1 | tee build-output.log`
Expected: Compilation errors from missing includes (due to the header export mechanism needing verification). Fix iteratively.

- [ ] **Step 4: Verify the symlink include mechanism works**

Check: `ls -la build/Frameworks/buffer/include/buffer/` should show symlink to `src/`
Check: A source file doing `#include <buffer/buffer.h>` should resolve correctly.
If not, adjust `textmate_framework()`.

- [ ] **Step 5: Fix compilation errors iteratively**

Work through errors bottom-up (leaf frameworks first). Common fixes:
- Add missing `target_link_libraries` for system frameworks
- Add missing source file patterns (some frameworks use `.c` files too)
- Fix include path issues

- [ ] **Step 6: Commit all fixes**

```
git add -A
git commit -m "Fix build errors from initial CMake migration"
```

---

### Task 18: Test verification

- [ ] **Step 1: Run tests**

Run: `cd build && ctest --output-on-failure 2>&1 | tee test-output.log`

- [ ] **Step 2: Fix any test build/run failures**

- [ ] **Step 3: Compare app bundle structure**

Run against a rave-built version (if available) or verify manually:
```
find build/TextMate.app -type f | sort > cmake-bundle.txt
```

Verify it contains:
- `Contents/MacOS/TextMate`
- `Contents/MacOS/mate`
- `Contents/MacOS/tm_query`
- `Contents/Resources/` (icons, about HTML, etc.)
- `Contents/Library/QuickLook/TextMateQL.qlgenerator/`

- [ ] **Step 4: Verify code signing**

Run: `codesign -vvv build/TextMate.app`
Expected: "valid on disk" (or "satisfies its Designated Requirement" for ad-hoc)

- [ ] **Step 5: Smoke test**

Run: `open build/TextMate.app`
- App should launch
- Open a file, edit, save
- Check About window (should show HTML content)

- [ ] **Step 6: Commit any remaining fixes**

```
git add -A
git commit -m "Fix remaining build and test issues"
```

---

## Chunk 15: Cleanup

### Task 19: Remove old build system files

- [ ] **Step 1: Remove old build system files**

Delete:
- `bin/rave`
- `bin/gen_build`
- `configure`
- `local-orig.rave`
- `default.rave`
- All `*/default.rave` files (63 total)
- `local.rave` (if exists)

Keep:
- `bin/gen_test` (Ruby, used by CMake)
- `bin/gen_html` (Ruby, used by CMake)
- `bin/expand_variables` (Ruby, may still be needed)
- `bin/CxxTest/` (test framework headers)

- [ ] **Step 2: Verify build still works after deletion**

Run: `cmake -B build -G Ninja && ninja -C build TextMate`

- [ ] **Step 3: Commit**

```
git add -A
git commit -m "Remove old rave build system"
```

---

### Task 20: Update documentation

- [ ] **Step 1: Update CLAUDE.md build instructions**

Replace the Build System section with CMake commands:
```
## Build System
- **Build tool:** CMake + Ninja
- **Bootstrap:** `cmake -B build -G Ninja && ninja -C build TextMate`
- **Build dir:** `build/` (or use presets: `cmake --preset debug`)

### Key Build Commands
cmake -B build -G Ninja    # Configure
ninja -C build TextMate    # Build main app
ninja -C build TextMate && open build/TextMate.app  # Build + run
cd build && ctest -R io    # Run tests for a framework
cmake -B xcode -G Xcode   # Generate Xcode project
```

- [ ] **Step 2: Commit**

```
git add CLAUDE.md
git commit -m "Update CLAUDE.md with CMake build instructions"
```
