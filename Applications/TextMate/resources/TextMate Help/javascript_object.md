title: Commands: HTML Output Security and Compatibility

# HTML Output Security and Compatibility

TextMate 3 displays ordinary command output in a native SwiftUI view. Output is treated as text and does not execute scripts.

Commands that explicitly request HTML output may use the legacy compatibility renderer. Treat HTML produced by bundles or external tools as untrusted content.

## JavaScript API Removal

The former `TextMate` JavaScript object and its `system`, `open`, `log`, progress, and process-control APIs are no longer exposed. This prevents rendered content from invoking native application operations or launching commands through a privileged JavaScript bridge.

Bundle commands should perform required work in the command itself and write their final result to standard output. Use a text output mode whenever interactive HTML is unnecessary.

## Compatibility Guidance

* Prefer plain-text command output for logs, diagnostics, and generated content.
* Keep HTML self-contained and avoid depending on application-injected objects.
* Do not place secrets in HTML output.
* Escape untrusted values before including them in markup.
* Migrate interactive tools to native bundle commands or dedicated plug-in UI.
