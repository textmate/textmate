# rave: require buffer bundles cf command io regexp scope selection settings text
add_library(editor STATIC)
file(GLOB _src src/*.cc)
target_sources(editor PRIVATE ${_src})
textmate_framework(editor)
target_link_libraries(editor PUBLIC buffer bundles cf command io regexp scope selection settings text)

textmate_add_tests(editor)
