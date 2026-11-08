add_library(plist STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(plist PRIVATE ${_src})
textmate_framework(plist)
target_include_directories(plist PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/src")
target_link_libraries(plist PUBLIC text cf io "-framework CoreFoundation" "-framework Foundation")

textmate_add_tests(plist)
