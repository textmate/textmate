# rave: require text bundles plist regexp scope
add_library(parse STATIC)
file(GLOB _src src/*.cc)
target_sources(parse PRIVATE ${_src})
textmate_framework(parse)
target_link_libraries(parse PUBLIC text bundles plist regexp scope)

textmate_add_tests(parse)
