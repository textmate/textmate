add_library(OakCommand STATIC)
file(GLOB _src src/*.mm)
target_sources(OakCommand PRIVATE ${_src})
textmate_framework(OakCommand)
target_link_libraries(OakCommand PUBLIC
  BundleEditor HTMLOutput HTMLOutputWindow OakAppKit OakSystem bundles cf command
  document io ns regexp settings text)
