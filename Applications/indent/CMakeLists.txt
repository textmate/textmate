add_executable(indent)
file(GLOB _src src/*.cc)
target_sources(indent PRIVATE ${_src})
target_link_libraries(indent PRIVATE text io regexp plist ${TEXTMATE_DEBUG_LIBS})
