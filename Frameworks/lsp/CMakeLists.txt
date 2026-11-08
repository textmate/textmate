add_library(lsp STATIC)
file(GLOB _src src/*.mm src/*.cc)
target_sources(lsp PRIVATE ${_src})
textmate_framework(lsp)
target_link_libraries(lsp PUBLIC
  document settings text ns io "-framework Cocoa")
target_include_directories(lsp PRIVATE "${CMAKE_SOURCE_DIR}/vendor")

# Headless integration test. Not a textmate_add_tests target: the generic runner
# executes test_* on a background queue, but LSPClient requires the main thread.
if(BUILD_TESTING)
  add_executable(lsp_integration tests/lsp_integration.mm)
  target_link_libraries(lsp_integration PRIVATE lsp ${TEXTMATE_DEBUG_LIBS})
  target_include_directories(lsp_integration PRIVATE "${CMAKE_SOURCE_DIR}/Shared/include")
  target_compile_definitions(lsp_integration PRIVATE
    LSP_TEST_FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures")
  add_test(NAME lsp_integration COMMAND lsp_integration)
endif()
