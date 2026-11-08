# CLI tool: tm_dialog2
add_executable(tm_dialog2 tm_dialog2.mm)
target_link_libraries(tm_dialog2 PRIVATE "-framework Foundation")

# Plugin bundle: Dialog2.tmplugin
add_library(Dialog2 MODULE)
file(GLOB _cmd_src Commands/**/*.mm)
target_sources(Dialog2 PRIVATE
  CLIProxy.mm Dialog2.mm TMDCommand.mm ${_cmd_src})

# Expand ${TARGET_NAME} in Info.plist
set(TARGET_NAME "Dialog2")
configure_file(Info.plist "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")
set_target_properties(Dialog2 PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "tmplugin"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")

target_link_options(Dialog2 PRIVATE -bundle)
target_link_libraries(Dialog2 PRIVATE
  "-framework Cocoa" "-framework WebKit" "-framework Quartz")

# Embed tm_dialog2 into plugin Resources and sign inner-to-outer
add_dependencies(Dialog2 tm_dialog2)
add_custom_command(TARGET Dialog2 POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
    "$<TARGET_FILE:tm_dialog2>"
    "$<TARGET_BUNDLE_DIR:Dialog2>/Contents/Resources/tm_dialog2")
textmate_codesign_file(Dialog2 "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:Dialog2>/Contents/Resources/tm_dialog2")
textmate_codesign(Dialog2 "${CS_IDENTITY}")
