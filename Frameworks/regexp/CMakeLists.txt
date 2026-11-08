# rave: require Onigmo text cf
add_library(regexp STATIC)
file(GLOB _src src/*.cc)
target_sources(regexp PRIVATE ${_src})
textmate_framework(regexp)
target_link_libraries(regexp PUBLIC Onigmo text cf)

textmate_add_tests(regexp)
