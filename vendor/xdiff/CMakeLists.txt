add_library(xdiff STATIC)

file(GLOB _src src/*.c)
target_sources(xdiff PRIVATE ${_src})

target_include_directories(xdiff PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")

target_compile_options(xdiff PRIVATE
  -Wno-sign-compare
  -Wno-shorten-64-to-32
  -Wno-unused-function)
