# rave: require bundles cf ns scope
add_library(theme STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(theme PRIVATE ${_src})
textmate_framework(theme)
target_link_libraries(theme PUBLIC bundles cf ns scope)

textmate_add_tests(theme)
