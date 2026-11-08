add_executable(PrivilegedTool)
file(GLOB _src src/*.cc src/*.mm)
target_sources(PrivilegedTool PRIVATE ${_src})
target_link_libraries(PrivilegedTool PRIVATE authorization io text ${TEXTMATE_DEBUG_LIBS})
