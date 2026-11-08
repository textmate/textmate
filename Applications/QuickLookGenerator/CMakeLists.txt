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
  OakFoundation buffer bundles cf file io ns plist scope settings theme OakDebug
  "-framework CoreFoundation" "-framework QuickLook" "-framework AppKit" "-framework OSAKit" "-framework UniformTypeIdentifiers")

textmate_codesign(TextMateQL "${CS_IDENTITY}")
