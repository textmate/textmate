# rave: require BundlesManager OakAppKit OakFoundation MenuBuilder bundles io ns regexp settings text
# require_headers OakTabBarView
add_library(Preferences STATIC)
file(GLOB _src src/*.mm)
target_sources(Preferences PRIVATE ${_src})
textmate_framework(Preferences)
target_link_libraries(Preferences PUBLIC
  BundlesManager OakAppKit OakFoundation MenuBuilder
  bundles io ns regexp settings text)
target_include_directories(Preferences PRIVATE
  $<TARGET_PROPERTY:OakTabBarView,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(Preferences PRIVATE "-framework Cocoa")
