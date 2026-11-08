# rave: require authorization io plist text, frameworks ApplicationServices Security
add_executable(mate)
file(GLOB _src src/*.mm)
target_sources(mate PRIVATE ${_src})
target_link_libraries(mate PRIVATE authorization io plist text ${TEXTMATE_DEBUG_LIBS}
  "-framework ApplicationServices" "-framework Security")
target_compile_definitions(mate PRIVATE "TEXTMATE_VERSION_STRING=\"${TEXTMATE_VERSION}\"")
