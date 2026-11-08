# rave: require buffer selection
add_library(undo STATIC)
file(GLOB _src src/*.cc)
target_sources(undo PRIVATE ${_src})
textmate_framework(undo)
target_link_libraries(undo PUBLIC buffer selection)
