# rave: no require, frameworks Cocoa
add_library(MenuBuilder STATIC)
file(GLOB _src src/*.mm)
target_sources(MenuBuilder PRIVATE ${_src})
textmate_framework(MenuBuilder)
target_link_libraries(MenuBuilder PUBLIC "-framework Cocoa")
