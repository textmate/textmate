# rave: require text cf ns regexp crash OakFoundation, frameworks Carbon Security
add_library(io STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(io PRIVATE ${_src})
textmate_framework(io)
target_link_libraries(io PUBLIC text cf ns regexp crash OakFoundation
  "-framework Carbon" "-framework Security")

textmate_add_tests(io)
