add_library(HTMLOutput STATIC)
file(GLOB_RECURSE _src src/*.mm)
target_sources(HTMLOutput PRIVATE ${_src})
textmate_framework(HTMLOutput)
target_link_libraries(HTMLOutput PUBLIC
  OakAppKit OakFoundation OakSystem cf document io ns text "-framework Cocoa" "-framework WebKit" "-framework UniformTypeIdentifiers")

textmate_add_tests(HTMLOutput)
