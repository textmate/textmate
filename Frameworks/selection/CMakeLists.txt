# rave: require text buffer bundles crash regexp
add_library(selection STATIC)
file(GLOB _src src/*.cc)
target_sources(selection PRIVATE ${_src})
textmate_framework(selection)
target_link_libraries(selection PUBLIC text buffer bundles crash regexp)

textmate_add_tests(selection)
