# rave: require bundles io ns parse regexp scope text
add_library(buffer STATIC)
file(GLOB _src src/*.cc)
target_sources(buffer PRIVATE ${_src})
textmate_framework(buffer)
target_link_libraries(buffer PUBLIC bundles io ns parse regexp scope text)

textmate_add_tests(buffer)
