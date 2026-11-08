# rave: require authorization bundles cf command encoding io plist regexp scm settings text
add_library(file STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(file PRIVATE ${_src})
textmate_framework(file)
target_link_libraries(file PUBLIC
  authorization bundles cf command encoding io plist regexp scm settings text -lz)

textmate_add_tests(file)
