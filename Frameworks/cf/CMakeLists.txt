# rave: require text, frameworks CoreFoundation ApplicationServices
add_library(cf STATIC)
file(GLOB _src src/*.cc)
target_sources(cf PRIVATE ${_src})
textmate_framework(cf)
target_link_libraries(cf PUBLIC text "-framework CoreFoundation" "-framework ApplicationServices")

textmate_add_tests(cf)
