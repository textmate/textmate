# rave: require OakFoundation OakAppKit TMFileReference, frameworks Cocoa
add_library(OakTabBarView STATIC)
file(GLOB _src src/*.mm)
target_sources(OakTabBarView PRIVATE ${_src})
textmate_framework(OakTabBarView)
target_link_libraries(OakTabBarView PUBLIC OakFoundation OakAppKit TMFileReference
  "-framework Cocoa")
target_include_directories(OakTabBarView PRIVATE
  $<TARGET_PROPERTY:Preferences,INTERFACE_INCLUDE_DIRECTORIES>)
