add_library(FileBrowser STATIC)
file(GLOB _src src/*.mm src/OFB/*.mm)
target_sources(FileBrowser PRIVATE ${_src})
textmate_framework(FileBrowser)
target_link_libraries(FileBrowser PUBLIC
  MenuBuilder OakAppKit OakCommand OakFoundation TMFileReference Preferences
  bundles io ns regexp scm settings text "-framework Cocoa")
