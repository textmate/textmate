# rave: require OakFoundation buffer bundles cf crash io ns plist regexp selection text theme
add_library(layout STATIC)
file(GLOB _src src/*.cc)
target_sources(layout PRIVATE ${_src})
textmate_framework(layout)
target_link_libraries(layout PUBLIC
  OakFoundation buffer bundles cf crash io ns plist regexp selection text theme)

textmate_add_tests(layout)
