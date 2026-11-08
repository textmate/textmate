# rave: require io text regexp OakSystem
add_library(authorization STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(authorization PRIVATE ${_src})
textmate_framework(authorization)
target_link_libraries(authorization PUBLIC io text regexp OakSystem)

textmate_add_tests(authorization)
