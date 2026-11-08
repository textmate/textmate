cmake_minimum_required(VERSION 3.21)
project(TextMate LANGUAGES C CXX OBJC OBJCXX)

set(CMAKE_OSX_DEPLOYMENT_TARGET "14.0")
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

# ObjC runtime linking + force-load ObjC categories from static libraries
add_link_options(-fobjc-link-runtime -ObjC)

# Dependencies — none required (all external deps removed)

# Derive version from git tags (e.g. v3.0 → "3.0", v3.0-5-gabcdef → "3.0.5")
execute_process(
  COMMAND git describe --tags --match "v*"
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE _git_describe
  OUTPUT_STRIP_TRAILING_WHITESPACE
  RESULT_VARIABLE _git_result
)
if(_git_result EQUAL 0 AND _git_describe MATCHES "^v([0-9]+\\.[0-9]+[0-9.]*)(-([0-9]+))?")
  set(TEXTMATE_VERSION "${CMAKE_MATCH_1}")
  if(CMAKE_MATCH_3)
    string(APPEND TEXTMATE_VERSION ".${CMAKE_MATCH_3}")
  endif()
else()
  set(TEXTMATE_VERSION "2.5")
endif()

# Shared includes
include_directories(Shared/include)

# Precompiled headers — force-include per language (matches rave's PCH behavior)
include_directories(Shared/PCH)
add_compile_options(
  $<$<COMPILE_LANGUAGE:C>:-include${CMAKE_SOURCE_DIR}/Shared/PCH/prelude.c>
  $<$<COMPILE_LANGUAGE:CXX>:-include${CMAKE_SOURCE_DIR}/Shared/PCH/prelude.cc>
  $<$<COMPILE_LANGUAGE:OBJC>:-include${CMAKE_SOURCE_DIR}/Shared/PCH/prelude.m>
  $<$<COMPILE_LANGUAGE:OBJCXX>:-include${CMAKE_SOURCE_DIR}/Shared/PCH/prelude.mm>
)

# Helpers
include(CTest)
include(cmake/TextMateHelpers.cmake)

# Code signing identity
set(CS_IDENTITY "-" CACHE STRING "Code signing identity (- for ad-hoc)")

# OakDebug provides oak::to_s and OakBadAssertion symbols used pervasively
# through header templates — needed in all build configs, not just debug
set(TEXTMATE_DEBUG_LIBS OakDebug)

# === OakSwiftUI (optional Swift framework) ===
find_library(OakSwiftUI_LIB NAMES OakSwiftUI
  PATHS "${CMAKE_BINARY_DIR}/lib" NO_DEFAULT_PATH)
if(OakSwiftUI_LIB)
  message(STATUS "Found OakSwiftUI: ${OakSwiftUI_LIB}")

  # Frameworks/OakSwiftUI/build.sh only produces debug and release builds.
  execute_process(COMMAND swift -print-target-info
    OUTPUT_VARIABLE _swift_target_info OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(JSON _swift_triple GET "${_swift_target_info}" "target" "unversionedTriple")
  if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(_swift_config "release")
  else()
    set(_swift_config "debug")
  endif()
  # Directory holding the generated OakSwiftUI-Swift.h
  set(OakSwiftUI_INCLUDE_DIR
    "${CMAKE_SOURCE_DIR}/Frameworks/OakSwiftUI/.build/${_swift_triple}/${_swift_config}/OakSwiftUI.build/include")
else()
  message(STATUS "OakSwiftUI not found — SwiftUI features disabled")
endif()

# === Vendor ===
add_subdirectory(vendor/Onigmo)
add_subdirectory(vendor/kvdb)
add_subdirectory(vendor/xdiff)

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
add_subdirectory(Frameworks/file)
add_subdirectory(Frameworks/command)
add_subdirectory(Frameworks/layout)
add_subdirectory(Frameworks/lsp)

# === GUI frameworks ===
add_subdirectory(Frameworks/OakAppKit)
add_subdirectory(Frameworks/TMFileReference)
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
add_subdirectory(Frameworks/document)
add_subdirectory(Frameworks/DocumentWindow)
add_subdirectory(Frameworks/CommitWindow)

# === Applications ===
add_subdirectory(Applications/textmate-mcp)
add_subdirectory(Applications/mate)
add_subdirectory(Applications/gtm)
add_subdirectory(Applications/indent)
add_subdirectory(Applications/pretty_plist)
add_subdirectory(Applications/tm_query)
add_subdirectory(Applications/PrivilegedTool)
add_subdirectory(Applications/QuickLookGenerator)
add_subdirectory(Applications/NewApplication)
add_subdirectory(Applications/ThemeEditor)
add_subdirectory(Applications/TextMate)

# === PlugIns (submodules) ===
add_subdirectory(PlugIns/dialog)
add_subdirectory(PlugIns/dialog-1.x)
