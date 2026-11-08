# rave: require text cf io settings regexp, sources src/**/*.cc, frameworks Carbon Security
add_library(scm STATIC)
file(GLOB_RECURSE _src CONFIGURE_DEPENDS src/*.cc)
target_sources(scm PRIVATE ${_src})
textmate_framework(scm)
target_link_libraries(scm PUBLIC text cf io settings regexp xdiff
  "-framework Carbon" "-framework Security")

# Resources
file(GLOB _resources resources/*)
if(_resources)
  target_sources(scm PRIVATE ${_resources})
endif()

textmate_add_tests(scm)
