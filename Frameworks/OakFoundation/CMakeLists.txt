# rave: require text
add_library(OakFoundation STATIC)
file(GLOB _src src/*.mm)
target_sources(OakFoundation PRIVATE ${_src})
textmate_framework(OakFoundation)
target_link_libraries(OakFoundation PUBLIC text "-framework Cocoa")
textmate_add_tests(OakFoundation)
