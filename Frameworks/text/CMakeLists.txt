# rave: require (none), headers src/*.h, sources src/*.cc, tests, frameworks CoreFoundation, libraries iconv
add_library(text STATIC)
file(GLOB _src src/*.cc)
target_sources(text PRIVATE ${_src})
textmate_framework(text)
target_link_libraries(text PUBLIC "-framework CoreFoundation")
find_library(ICONV_LIB iconv REQUIRED)
target_link_libraries(text PUBLIC ${ICONV_LIB})

textmate_add_tests(text)
