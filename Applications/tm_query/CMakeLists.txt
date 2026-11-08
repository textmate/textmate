add_executable(tm_query)
file(GLOB _src src/*.cc)
target_sources(tm_query PRIVATE ${_src})
target_link_libraries(tm_query PRIVATE settings OakSystem ${TEXTMATE_DEBUG_LIBS})
