# rave: require OakAppKit OakFoundation OakSystem buffer bundles cf io plist regexp scope selection settings text
add_library(command STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(command PRIVATE ${_src})
textmate_framework(command)
target_link_libraries(command PUBLIC
  OakAppKit OakFoundation OakSystem buffer bundles cf io plist regexp scope selection settings text)

textmate_add_tests(command)
