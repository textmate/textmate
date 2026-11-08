add_library(BundleEditor STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(BundleEditor PRIVATE ${_src})
textmate_framework(BundleEditor)
target_link_libraries(BundleEditor PUBLIC
  BundlesManager OakAppKit OakFoundation OakTextView TMFileReference bundles cf
  command document io ns plist regexp settings text "-framework Cocoa" "-framework AddressBook")
