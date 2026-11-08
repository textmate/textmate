# CLI tool: tm_dialog
add_executable(tm_dialog tm_dialog.mm)
target_link_libraries(tm_dialog PRIVATE "-framework Foundation")

# Plugin bundle: Dialog.tmplugin
add_library(Dialog MODULE)
target_sources(Dialog PRIVATE
  Dialog.mm TMDChameleon.mm TMDSemaphore.mm)

# Expand ${TARGET_NAME} in Info.plist
set(TARGET_NAME "Dialog")
configure_file(Info.plist "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")
set_target_properties(Dialog PROPERTIES
  BUNDLE TRUE
  BUNDLE_EXTENSION "tmplugin"
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")

target_link_options(Dialog PRIVATE -bundle)
target_link_libraries(Dialog PRIVATE "-framework Cocoa")

# Embed tm_dialog into plugin Resources and sign inner-to-outer
add_dependencies(Dialog tm_dialog)
add_custom_command(TARGET Dialog POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
    "$<TARGET_FILE:tm_dialog>"
    "$<TARGET_BUNDLE_DIR:Dialog>/Contents/Resources/tm_dialog")
textmate_codesign_file(Dialog "${CS_IDENTITY}"
  "$<TARGET_BUNDLE_DIR:Dialog>/Contents/Resources/tm_dialog")
textmate_codesign(Dialog "${CS_IDENTITY}")
