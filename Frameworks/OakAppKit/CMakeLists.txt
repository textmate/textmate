# rave: require OakFoundation bundles crash file io ns parse regexp settings text theme
# frameworks Carbon Cocoa AudioToolbox Quartz, libraries sqlite3
add_library(OakAppKit STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(OakAppKit PRIVATE ${_src})
textmate_framework(OakAppKit)
target_link_libraries(OakAppKit PUBLIC
  OakFoundation bundles crash file io ns parse regexp settings text theme)
target_link_libraries(OakAppKit PRIVATE
  "-framework Carbon" "-framework Cocoa" "-framework AudioToolbox" "-framework Quartz"
  "-framework UniformTypeIdentifiers" sqlite3)
