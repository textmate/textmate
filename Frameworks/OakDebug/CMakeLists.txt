# rave: require_headers text, frameworks Cocoa ExceptionHandling
add_library(OakDebug STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(OakDebug PRIVATE ${_src})
textmate_framework(OakDebug)
# require_headers = include dirs only, no linking
target_include_directories(OakDebug PRIVATE
  $<TARGET_PROPERTY:text,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(OakDebug PUBLIC "-framework Cocoa" "-framework ExceptionHandling")
