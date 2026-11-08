# Framework
add_library(CommitWindow STATIC)
file(GLOB _src src/*.mm)
target_sources(CommitWindow PRIVATE ${_src})
textmate_framework(CommitWindow)
target_link_libraries(CommitWindow PUBLIC
  OakAppKit OakFoundation OakTextView bundles document io ns plist regexp text
  "-framework Cocoa")

# CLI tool: CommitWindowTool (embedded into CommitWindow as "commit")
add_executable(CommitWindowTool tool/commit.mm)
target_include_directories(CommitWindowTool PRIVATE
  $<TARGET_PROPERTY:CommitWindow,INTERFACE_INCLUDE_DIRECTORIES>)
