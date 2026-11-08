add_executable(ThemeEditor MACOSX_BUNDLE src/main.mm)
set(_icon "${CMAKE_CURRENT_SOURCE_DIR}/resources/ThemeEditor.icns")
target_sources(ThemeEditor PRIVATE "${_icon}")
set_source_files_properties("${_icon}" PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources")
# Icon changes must refresh signing and the helper embedded in TextMate.
set_property(TARGET ThemeEditor APPEND PROPERTY LINK_DEPENDS "${_icon}")
file(GLOB _samples CONFIGURE_DEPENDS samples/*)
target_sources(ThemeEditor PRIVATE ${_samples})
set_source_files_properties(${_samples} PROPERTIES
  HEADER_FILE_ONLY TRUE MACOSX_PACKAGE_LOCATION "Resources/Samples")
# Sample edits must refresh both the helper signature and the embedded app.
set_property(TARGET ThemeEditor APPEND PROPERTY LINK_DEPENDS ${_samples})
# OakTextView is static, so its preview images belong in this app's bundle.
# Source bundle locations must be set in the directory defining these targets.
file(GLOB _preview_resources CONFIGURE_DEPENDS
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.pdf"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.tiff"
  "${CMAKE_SOURCE_DIR}/Frameworks/OakTextView/resources/*.png")
target_sources(ThemeEditor PRIVATE ${_preview_resources})
set_source_files_properties(${_preview_resources} PROPERTIES
  MACOSX_PACKAGE_LOCATION "Resources")
set_property(TARGET ThemeEditor APPEND PROPERTY LINK_DEPENDS ${_preview_resources})
target_link_libraries(ThemeEditor PRIVATE
  OakTextView OakSystem Preferences bundles document ns plist settings theme
  ${TEXTMATE_DEBUG_LIBS} "-framework Cocoa" "-framework UniformTypeIdentifiers")
set_target_properties(ThemeEditor PROPERTIES
  OUTPUT_NAME "ThemeEditor"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist"
  BUILD_RPATH "@executable_path/../../../../Frameworks;${CMAKE_BINARY_DIR}/lib"
  INSTALL_RPATH "@executable_path/../../../../Frameworks")
if(CS_IDENTITY STREQUAL "-")
  set(THEME_EDITOR_DISABLE_LIBRARY_VALIDATION true)
else()
  set(THEME_EDITOR_DISABLE_LIBRARY_VALIDATION false)
endif()
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CS_GET_TASK_ALLOW true)
else()
  set(CS_GET_TASK_ALLOW false)
endif()
configure_file(Entitlements.plist ${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist)
textmate_codesign(ThemeEditor "${CS_IDENTITY}"
  ENTITLEMENTS "${CMAKE_CURRENT_BINARY_DIR}/Entitlements.plist")

add_executable(tm-theme-editor src/launcher.mm)
target_link_libraries(tm-theme-editor PRIVATE "-framework AppKit")

if(BUILD_TESTING)
  set(_runner "${CMAKE_CURRENT_BINARY_DIR}/test_runner.mm")
  add_custom_command(OUTPUT "${_runner}"
    COMMAND "${CMAKE_SOURCE_DIR}/bin/gen_test"
      "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_theme_editor.cc"
      "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_theme_colors.mm"
      "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_ase.mm"
      "${CMAKE_CURRENT_SOURCE_DIR}/tests/t_dtcg.mm" > "${_runner}"
    DEPENDS tests/t_theme_editor.cc tests/t_theme_colors.mm tests/t_ase.mm tests/t_dtcg.mm src/ase.h src/dtcg.h src/appearance.h src/bundle_index.h src/colors.h src/samples.h src/rule_matching.h "${CMAKE_SOURCE_DIR}/bin/gen_test")
  add_executable(theme_editor_tests "${_runner}")
  target_link_libraries(theme_editor_tests PRIVATE theme bundles ${TEXTMATE_DEBUG_LIBS} "-framework Cocoa")
  target_compile_definitions(theme_editor_tests PRIVATE
    THEME_EDITOR_SAMPLE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/samples"
    THEME_EDITOR_FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures")
  add_test(NAME theme-editor COMMAND theme_editor_tests --no-parallel)

  add_executable(theme_editor_selection_tests MACOSX_BUNDLE
    src/main.mm tests/save_selection.mm ${_samples} ${_preview_resources} "${_icon}")
  set_target_properties(theme_editor_selection_tests PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER "com.macromates.TextMate.ThemeEditor.Tests"
    MACOSX_BUNDLE_ICON_FILE "ThemeEditor.icns"
    BUILD_RPATH "${CMAKE_BINARY_DIR}/lib")
  target_compile_definitions(theme_editor_selection_tests PRIVATE THEME_EDITOR_TESTING=1
    THEME_EDITOR_FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures")
  target_link_libraries(theme_editor_selection_tests PRIVATE
    OakTextView OakSystem Preferences bundles document ns plist settings theme
    ${TEXTMATE_DEBUG_LIBS} "-framework Cocoa" "-framework UniformTypeIdentifiers")
  add_test(NAME theme-editor-selection COMMAND $<TARGET_FILE:theme_editor_selection_tests>)
  set_tests_properties(theme-editor-selection PROPERTIES RUN_SERIAL TRUE TIMEOUT 30)
endif()
