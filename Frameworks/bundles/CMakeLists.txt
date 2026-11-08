# rave: require OakSystem io plist regexp scope text, frameworks CoreFoundation
add_library(bundles STATIC)
file(GLOB _src src/*.cc)
target_sources(bundles PRIVATE ${_src})
textmate_framework(bundles)
target_link_libraries(bundles PUBLIC OakSystem io plist regexp scope text
  "-framework CoreFoundation")

textmate_add_tests(bundles)
