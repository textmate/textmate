# rave: require text
add_library(scope STATIC)
file(GLOB _src src/*.cc)
target_sources(scope PRIVATE ${_src})
textmate_framework(scope)
target_link_libraries(scope PUBLIC text)

textmate_add_tests(scope)
