add_library(Find STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(Find PRIVATE ${_src})
textmate_framework(Find)
target_link_libraries(Find PUBLIC
  MenuBuilder OakAppKit OakFoundation Preferences document io ns regexp settings text
  "-framework Cocoa")
