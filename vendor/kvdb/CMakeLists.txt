add_library(kvdb STATIC)

file(GLOB _src vendor/kvdb/*.m)
target_sources(kvdb PRIVATE ${_src})

# kvdb exports vendor/kvdb/kvdb.h — include parent so <kvdb/kvdb.h> works
target_include_directories(kvdb PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/vendor")

target_link_libraries(kvdb PRIVATE "-framework Cocoa" sqlite3)
