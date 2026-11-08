add_library(OakFilterList STATIC)
file(GLOB _src src/*.mm src/ui/*.mm)
target_sources(OakFilterList PRIVATE ${_src})
textmate_framework(OakFilterList)
target_link_libraries(OakFilterList PUBLIC
  OakAppKit OakFoundation OakSystem TMFileReference bundles document
  ns regexp scm scope settings text "-framework Cocoa" "-framework Carbon")
