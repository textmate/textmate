# rave: require cf io plist regexp scope text
add_library(settings STATIC)
file(GLOB _src src/*.cc)
target_sources(settings PRIVATE ${_src})
textmate_framework(settings)
target_link_libraries(settings PUBLIC cf io plist regexp scope text)

textmate_add_tests(settings)
