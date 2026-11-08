add_library(DocumentWindow STATIC)
file(GLOB _src src/*.mm)
target_sources(DocumentWindow PRIVATE ${_src})
textmate_framework(DocumentWindow)
target_link_libraries(DocumentWindow PUBLIC
  BundleEditor BundlesManager FileBrowser Find HTMLOutputWindow MenuBuilder OakAppKit
  OakTabBarView OakCommand OakFilterList OakFoundation OakSystem OakTextView Preferences
  bundles crash document file io kvdb ns regexp scm settings text
  "-framework Cocoa" "-framework SystemConfiguration")

textmate_add_tests(DocumentWindow)

if(BUILD_TESTING)
  set(_context_test_source "${CMAKE_CURRENT_SOURCE_DIR}/context-tests/t_editor_context.mm")
  set(_context_test_runner "${CMAKE_CURRENT_BINARY_DIR}/editor_context_test_runner.mm")
  add_custom_command(OUTPUT "${_context_test_runner}"
    COMMAND "${CMAKE_SOURCE_DIR}/bin/gen_test" "${_context_test_source}" > "${_context_test_runner}"
    DEPENDS "${_context_test_source}" "${CMAKE_SOURCE_DIR}/bin/gen_test")
  add_executable(EditorContext_tests "${_context_test_runner}")
  target_link_libraries(EditorContext_tests PRIVATE DocumentWindow ${TEXTMATE_DEBUG_LIBS})
  # Fixtures share AppKit and the singleton service. Running all fixtures in
  # parallel can starve nested blocking socket calls in the global thread pool;
  # concurrency is exercised explicitly inside the routing/transfer tests.
  add_test(NAME EditorContext COMMAND EditorContext_tests --no-parallel)

  # Optional standalone preview: the production view without restoring or
  # saving a user's running TextMate session.
  add_executable(ProjectChangesPreview MACOSX_BUNDLE EXCLUDE_FROM_ALL context-tests/project_changes_preview.mm)
  target_link_libraries(ProjectChangesPreview PRIVATE DocumentWindow ${TEXTMATE_DEBUG_LIBS})
  set_target_properties(ProjectChangesPreview PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER "org.textmate.ProjectChangesPreview"
    MACOSX_BUNDLE_BUNDLE_NAME "Project Changes Preview")
endif()

# Bridge the native terminal panel to the Swift component.
if(OakSwiftUI_LIB)
  # PUBLIC so dependents (including the test runner) compile the same conditional code.
  target_compile_definitions(DocumentWindow PUBLIC HAVE_OAK_SWIFTUI=1)
  target_include_directories(DocumentWindow PRIVATE "${OakSwiftUI_INCLUDE_DIR}")
  target_link_libraries(DocumentWindow PUBLIC ${OakSwiftUI_LIB})
endif()
