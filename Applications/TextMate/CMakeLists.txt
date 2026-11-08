add_executable(TextMate MACOSX_BUNDLE)
file(GLOB _src src/*.cc src/*.mm)
target_sources(TextMate PRIVATE ${_src})

# Internal framework dependencies
target_link_libraries(TextMate PRIVATE
  BundleEditor BundleMenu BundlesManager CommitWindow DocumentWindow
  Find HTMLOutputWindow MenuBuilder OakAppKit OakCommand OakFilterList OakFoundation
  OakSystem OakTextView Preferences authorization bundles cf command
  crash document io kvdb ns plist regexp scm settings text theme
  ${TEXTMATE_DEBUG_LIBS}
)

target_link_libraries(TextMate PRIVATE "-framework Cocoa" "-framework UniformTypeIdentifiers")

# OakSwiftUI (optional SwiftUI framework)
if(OakSwiftUI_LIB)
  target_link_libraries(TextMate PRIVATE ${OakSwiftUI_LIB})
  target_compile_definitions(TextMate PRIVATE HAVE_OAK_SWIFTUI=1)
  target_include_directories(TextMate PRIVATE "${OakSwiftUI_INCLUDE_DIR}")

  # Copy dylib into app bundle and set rpath
  add_custom_command(TARGET TextMate POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory
      "$<TARGET_BUNDLE_DIR:TextMate>/Contents/Frameworks"
    COMMAND ${CMAKE_COMMAND} -E copy
      "${CMAKE_BINARY_DIR}/lib/libOakSwiftUI.dylib"
      "$<TARGET_BUNDLE_DIR:TextMate>/Contents/Frameworks/libOakSwiftUI.dylib"
  )
  set_target_properties(TextMate PROPERTIES
    BUILD_RPATH "@executable_path/../Frameworks"
    INSTALL_RPATH "@executable_path/../Frameworks"
  )
endif()

# Info.plist variable expansion
set(TARGET_NAME "TextMate")
if(CMAKE_BUILD_TYPE STREQUAL "Release")
  set(BUNDLE_IDENTIFIER "com.macromates.TextMate")
  set(BUNDLE_NAME "TextMate")
else()
  set(BUNDLE_IDENTIFIER "com.macromates.TextMate-dev")
  set(BUNDLE_NAME "TextMate-dev")
endif()
set(APP_MIN_OS "${CMAKE_OSX_DEPLOYMENT_TARGET}")
set(APP_VERSION "${TEXTMATE_VERSION}")
string(TIMESTAMP YEAR "%Y")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CS_GET_TASK_ALLOW "true")
else()
  set(CS_GET_TASK_ALLOW "false")
endif()
configure_file(Info.plist ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)

# Expand ${YEAR} in InfoPlist.strings — overwrite source copy in bundle
configure_file(
  resources/English.lproj/InfoPlist.strings
  ${CMAKE_CURRENT_BINARY_DIR}/English.lproj/InfoPlist.strings)
add_custom_command(TARGET TextMate POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
    "${CMAKE_CURRENT_BINARY_DIR}/English.lproj/InfoPlist.strings"
    "$<TARGET_BUNDLE_DIR:TextMate>/Contents/Resources/English.lproj/InfoPlist.strings")
set_target_properties(TextMate PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")

# Compile xib → nib (all xibs from TextMate and frameworks go into the app bundle)
target_xib_sources(TextMate "English.lproj"
  resources/English.lproj/MainMenu.xib
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/English.lproj/TabSizeSetting.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/Preferences/resources/English.lproj/TerminalPreferences.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakAppKit/resources/English.lproj/CustomizeEncodings.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakAppKit/resources/English.lproj/Pasteboard Selector.xib")
target_xib_sources(TextMate "English.lproj"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/BundleProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/FileDropProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/CommandProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/SharedProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/SnippetProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/ThemeProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/MacroProperties.xib"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/resources/English.lproj/GrammarProperties.xib")

# Icon and image resources
file(GLOB _icons icons/*.icns)
file(GLOB _resources resources/*)
set(_all_resources ${_icons} ${_resources})
target_sources(TextMate PRIVATE ${_all_resources})
set_source_files_properties(${_all_resources} PROPERTIES
  MACOSX_PACKAGE_LOCATION Resources)

# Framework resources (static libs can't carry resources, so they go in the app bundle)
file(GLOB _fw_res
  "${CMAKE_SOURCE_DIR}/Frameworks/Preferences/icons/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.pdf"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.tiff"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakAppKit/gfx/CloseButton/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakAppKit/resources/Charsets.plist"
  "${CMAKE_SOURCE_DIR}/Frameworks/BundleEditor/gfx/Bundle Item Icons/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTabBarView/gfx/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/FileBrowser/resources/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/DocumentWindow/resources/*.png"
  "${CMAKE_SOURCE_DIR}/Frameworks/HTMLOutput/resources/*.html"
  "${CMAKE_SOURCE_DIR}/Frameworks/HTMLOutput/resources/*.js"
  "${CMAKE_SOURCE_DIR}/Applications/textmate-mcp/resources/*.js"
  "${CMAKE_SOURCE_DIR}/Frameworks/scm/resources/*.xslt"
)
target_sources(TextMate PRIVATE ${_fw_res})
set_source_files_properties(${_fw_res} PROPERTIES
  MACOSX_PACKAGE_LOCATION Resources)

# TMFileReference resources go in Resources/DocumentTypes/ subdirectory
file(GLOB _doctype_res
  "${CMAKE_SOURCE_DIR}/Frameworks/TMFileReference/resources/*.icns"
  "${CMAKE_SOURCE_DIR}/Frameworks/TMFileReference/resources/bindings.plist")
target_sources(TextMate PRIVATE ${_doctype_res})
set_source_files_properties(${_doctype_res} PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources/DocumentTypes")

# Pre-converted HTML for About window
file(GLOB _about_html about/*.html)
file(GLOB _about_css about/css/*)
file(GLOB _about_js about/js/*)
target_sources(TextMate PRIVATE ${_about_html} ${_about_css} ${_about_js})
set_source_files_properties(${_about_html} PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources/About")
set_source_files_properties(${_about_css} PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources/About/css")
set_source_files_properties(${_about_js} PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources/About/js")

# Embedded CLI tools
textmate_embed(TextMate mate "MacOS")
textmate_embed(TextMate tm-theme-editor "MacOS")
textmate_embed(TextMate ThemeEditor "Helpers" DIRECTORY)
# Refresh the embedded app when only the theme editor or launcher changes.
set_property(TARGET TextMate APPEND PROPERTY LINK_DEPENDS
  "$<TARGET_FILE:ThemeEditor>" "$<TARGET_FILE:tm-theme-editor>")
textmate_embed(TextMate textmate-mcp "MacOS")
textmate_embed(TextMate tm-codex "MacOS")
textmate_embed(TextMate tm-claude "MacOS")
textmate_embed(TextMate tm-opencode "MacOS")
textmate_embed(TextMate tm-pi "MacOS")
textmate_embed(TextMate tm_query "MacOS")
textmate_embed(TextMate PrivilegedTool "Resources")

# Embedded plugins (.tmplugin bundles from submodules)
textmate_embed(TextMate Dialog "PlugIns" DIRECTORY)
textmate_embed(TextMate Dialog2 "PlugIns" DIRECTORY)

# Embedded QuickLook plugin
textmate_embed(TextMate TextMateQL "Library/QuickLook" DIRECTORY)

# Sign embedded binaries (inner-to-outer, before app signature)
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm-pi")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm-theme-editor")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm-opencode")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm-claude")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm-codex")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/textmate-mcp")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/mate")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/MacOS/tm_query")
textmate_codesign_file(TextMate "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:TextMate>/Contents/Resources/PrivilegedTool")
if(OakSwiftUI_LIB)
  textmate_codesign_file(TextMate "${CS_IDENTITY}"
    "$<TARGET_BUNDLE_DIR:TextMate>/Contents/Frameworks/libOakSwiftUI.dylib")
endif()

# Entitlements + code signing (outer app — must be last)
configure_file(Entitlements.plist ${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist)
textmate_codesign(TextMate "${CS_IDENTITY}"
  ENTITLEMENTS "${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist")
