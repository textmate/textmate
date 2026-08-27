# SwiftUI Migration

TextMate is an AppKit and Objective-C++ application built with Rave and Ninja. The
migration keeps the editor core in place while replacing UI boundaries with native
SwiftUI modules. The modernization baseline is macOS 14 so the new UI can use
selectable text, native progress indicators, and current styling without carrying
legacy SwiftUI fallbacks.

## Security boundary

Legacy HTML output injected a `TextMate` JavaScript object whose `system` method
executed arbitrary shell commands. Phase one removes that injection entirely. HTML
may still render during the transition, but it no longer receives a native shell or
file-access capability.

## Phases

1. **Native command-output model and view.** `TextMateUI` provides a compiled,
   independently tested SwiftUI command-output surface with no web runtime.
2. **AppKit host integration (implemented).** A narrow C ABI owns an `NSHostingView`,
   Rave compiles Swift sources alongside Objective-C++, and command bytes stream to
   `CommandOutputModel` as inert UTF-8. The legacy renderer remains behind the
   `OakUseNativeCommandOutput` user default as a temporary compatibility switch.
3. **Structured output actions.** Replace JavaScript callbacks with explicit native
   commands for opening files, updating progress, cancellation, and diagnostics.
   No API accepts shell source from rendered content.
4. **Remove legacy WebView.** The unused `HOJSBridge` implementation is deleted.
   Migrate remaining HTML-only presentation to native SwiftUI views or tightly
   sandboxed `WKWebView` content without native bridges, then remove the WebKit
   framework dependency and the compatibility switch.
5. **Incremental application shell migration.** Move preferences, bundle management,
   and auxiliary windows to SwiftUI while wrapping the existing editor views with
   `NSViewRepresentable` where replacement would risk editing behavior.

## Compatibility rule

Existing bundles that call `TextMate.system`, `TextMate.open`, or mutate the HTML
output status object must move to structured command metadata. Restoring the bridge,
including for `file:` or `tm-file:` pages, is not an accepted compatibility fallback.

Native output is enabled by default. During the transition it can be disabled with
`defaults write com.macromates.TextMate OakUseNativeCommandOutput -bool NO`; this
restores legacy HTML rendering only, never the removed privileged JavaScript bridge.
