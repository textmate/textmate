add_library(BundleMenu STATIC)
file(GLOB _src src/*.mm)
target_sources(BundleMenu PRIVATE ${_src})
textmate_framework(BundleMenu)
target_link_libraries(BundleMenu PUBLIC OakAppKit OakFoundation bundles text cf ns
  "-framework AppKit")
