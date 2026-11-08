add_library(encoding STATIC)
file(GLOB _src src/*.mm)
target_sources(encoding PRIVATE ${_src})
textmate_framework(encoding)
