add_library(document STATIC)
file(GLOB _src src/*.mm src/*.cc)
target_sources(document PRIVATE ${_src})
textmate_framework(document)
target_link_libraries(document PUBLIC
  BundlesManager FileBrowser OakAppKit OakFoundation TMFileReference authorization
  buffer cf command editor encoding file io layout ns plist regexp scm selection
  settings text theme undo "-framework ApplicationServices" -lz)

textmate_add_tests(document)
