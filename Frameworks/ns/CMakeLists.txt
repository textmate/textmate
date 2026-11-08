# rave: require text OakFoundation plist
add_library(ns STATIC)
file(GLOB _src src/*.mm)
target_sources(ns PRIVATE ${_src})
textmate_framework(ns)
target_link_libraries(ns PUBLIC text OakFoundation plist "-framework Cocoa")

textmate_add_tests(ns)
