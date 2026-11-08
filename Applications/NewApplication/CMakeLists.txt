add_executable(NewApplication MACOSX_BUNDLE)
file(GLOB _src src/*.cc src/*.mm)
target_sources(NewApplication PRIVATE ${_src})
target_link_libraries(NewApplication PRIVATE
  OakFoundation OakAppKit MenuBuilder OakDebug "-framework Cocoa")

set_target_properties(NewApplication PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")
