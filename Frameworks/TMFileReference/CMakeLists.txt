# rave: require_headers scm, frameworks Cocoa
add_library(TMFileReference STATIC)
file(GLOB _src src/*.mm)
target_sources(TMFileReference PRIVATE ${_src})
textmate_framework(TMFileReference)
target_include_directories(TMFileReference PRIVATE
  $<TARGET_PROPERTY:scm,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(TMFileReference PUBLIC "-framework Cocoa" "-framework UniformTypeIdentifiers")
