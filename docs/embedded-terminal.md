# Embedded terminal

Show the panel using View → Show Terminal Panel or Control–`. Choose its edge under View → Terminal Panel Position. The panel now contains a SwiftTerm terminal. The edge and size last chosen seed new windows. A restored window reopens with its own saved edge and size, which take precedence over those defaults, and a window created by detaching a tab copies the placement of the window it came from.

Each window starts one login shell on first use, using the account's configured shell. It starts in the project folder, the file browser's root, the current document's folder, or the home folder when none is available. Opening another document does not change an existing shell's directory.

Hiding or repositioning the panel preserves the running session and scrollback. Showing it focuses the terminal; hiding it returns focus to the editor. Normal terminal controls and copy/paste are provided by SwiftTerm, and ⌘K clears the display and scrollback and redraws the shell prompt while the terminal has focus. Use the + button in the tab bar to start additional shells in the current project directory, click a tab or press ⌘⌥← / ⌘⌥→ while the terminal pane has focus to switch sessions (wrapping at either end), and use its × button to close it. Other tabs keep running while inactive. When a shell exits, its tab closes automatically. Closing the last tab leaves the pane open so you can start another with +.

Terminal tabs use the same OakTabBarView as editor tabs, including sizing, overflow, hover close buttons, and drag reordering. Drag a session to another terminal pane to move it without restarting the shell. Double-click empty tab-bar space to create a session; double-click a tab (when more than one exists), or choose Move Tab to New Window, to detach it. A detached session opens in a new window whose editor holds only an untitled placeholder document; that window closes when its last session ends, unless the placeholder has been edited or other files were opened in it. The context menu supports closing a tab, other tabs, or tabs on either side. Option-clicking a close button closes the other tabs.

Closing a window or quitting asks before ending a session that is running a foreground program. Background jobs and helper processes started by the shell or its prompt do not trigger the question. Sessions end when TextMate exits; reopening a saved window starts a new shell, not the previous processes or scrollback.

## Building

SwiftTerm 1.99.1 is pinned in Frameworks/OakSwiftUI/Package.swift and Package.resolved. It requires Swift 6.2 or later. `make debug` (or `make release`) builds the Swift component before compiling the terminal bridge, and embeds and signs `libOakSwiftUI.dylib` in the application’s `Contents/Frameworks` directory. SwiftTerm is linked into that library. The application requires macOS 14 or later, matching the Swift package’s deployment target. The first build downloads Swift package dependencies. SwiftTerm ships a build-tool plugin, and `build.sh` passes `--disable-sandbox`, so that plugin runs unsandboxed at build time; the exact version pin in Package.swift and Package.resolved is what limits that exposure, so review the plugin before bumping it. TerminalThirdPartyLicenses.txt is included in the application resources.

The terminal bridge is OakTerminalPanelController. TerminalPanelViewController owns the bridge independently of its position/visibility, and DocumentWindowController supplies the initial directory and handles close/quit checks.

Terminal sessions inherit the editor’s background, foreground, caret, and selection colours. The sixteen ANSI colours are derived from common syntax roles (strings, numbers, functions, keywords, and types), with tinted fallbacks for unstyled roles and contrast adjustments for light and dark backgrounds. Applications that emit explicit RGB colours still choose their own colours.

Terminal sessions follow the active editor’s font family, size, and zoom level. Font changes update all tabs, including hidden sessions; new and transferred sessions use the destination editor’s font.

## Agent status on tabs

Programs that support the Program Status Protocol (OSC 7501) can report their state directly to TextMate. Terminal tabs show a spinner while working, an attention symbol when approval, an answer, or sign-in is needed, a checkmark when a result is ready, and an error symbol after failure. Hover over the tab for the reason and any reported progress. Status is also included in the tab's accessibility label and overflow menu.

Status follows the terminal when you reorder tabs or move them between windows. Custom tab names remain intact. Selecting a tab, switching to it with the terminal tab shortcuts, or typing in the focused terminal acknowledges completed and failed results. Pending approvals and questions remain visible until the program reports that it has resumed. A blocked worker takes priority over working or completed records; work still running takes priority over completed results.

This uses reports from the running program, without reading its displayed text or inferring activity from silence. Claude Code 2.1.295 and later supports the protocol. Other programs and agent versions need to emit OSC 7501 themselves; the TextMate launchers do not install status hooks or plugins. Unsupported programs show no status indicator. “Result ready” describes the reported work, rather than guaranteeing every background task has finished.

## Opening files from terminal output

Hold Command to underline a detected local file, then Command-click to open it in the same TextMate window. Existing editor tabs are reused. References such as `src/main.swift:42:7` jump to the indicated line and column. Absolute paths, `~/` paths, quoted paths with spaces, and explicit `file://` terminal hyperlinks are supported. Ordinary clicks still select terminal text, and web hyperlinks retain their normal behaviour.

Relative paths use the session's shell working directory (queried from macOS, with OSC 7 directory reports as a fallback). Recent output retains its observed directory when the shell changes directories. This is best-effort for old output: resizing/reflow, cleared history, or multiple directory changes within a single output batch can lose that context. Explicit absolute file hyperlinks are unambiguous. Remote files are not downloaded, and plain-path detection is disabled in alternate-screen applications.

## Editor context for terminal agents

The bundled MCP server lets an agent query the current file, selected text, open documents, and unsaved buffer contents in its terminal's window. In a new terminal tab, run `tm-codex`, `tm-claude`, `tm-opencode`, or `tm-pi` to start the corresponding agent with editor context. Each launcher accepts `--check` to test the connection. Other MCP clients can obtain configuration with `"$TM_MCP_SERVER" --print-config`. See [Editor context MCP](editor-context-mcp.md) for setup and tool details.

## Diagnostics for terminal agents

With an agent running through `tm-codex`, `tm-claude`, `tm-opencode`, or `tm-pi`, ask it to **“Read TextMate's diagnostics and explain the errors and warnings in this file.”** The `get_diagnostics` tool reads the language-server reports behind the editor's inline diagnostics, including messages, severity, source, codes, and source ranges. Ask for file locations in the response, then Command-click a location to jump to that line.

The tool reports whether diagnostics have been received and whether a server is available, and supports other loaded documents in the same window. Reports are cached and can lag unsaved edits. Terminal build logs are not collected by this tool. See [Using diagnostics](editor-context-mcp.md#using-diagnostics) for paging and status details.

## Navigate from the conversation

Ask an agent **“Show me that function in TextMate”**, **“Open this file at line 42”**, or **“Select the code for that diagnostic.”** Its `navigate_editor` tool opens or reuses an editor tab in the terminal's window, moves to a line, or selects a passage. It works with unsaved and untitled documents already in that window, as well as existing local files supplied by absolute path.

Keyboard focus stays in the terminal so you can keep chatting. Ask the agent to focus the editor if you want to start editing. Navigation preserves document contents and existing tabs. The agent can use current document revisions to avoid stale locations, and coordinates handle Unicode correctly. See [Navigate from the conversation](editor-context-mcp.md#navigate-from-the-conversation) for the tool arguments and file-loading behavior.

## Follow editor changes

Agents can follow file switches, selections, unsaved edits, open tabs, metadata, and diagnostics. Clients supporting MCP resource subscriptions can subscribe to `textmate://editor/context` and receive coalesced change notifications. The client decides whether to refresh context or wake the model when a notification arrives.

For clients that use tools, ask **“Follow my current file and selection, using get_editor_changes to refresh when they change.”** This tool returns a cursor with the current context. Subsequent calls with that cursor omit unchanged text and return a fresh snapshot when something changes. Following stays within the terminal's owning window, including after moving the terminal to another window.

Subscriptions are optional, and there are no background editor queries without a subscriber. The feature does not automatically start model turns or recurring calls in tool-only clients. See [Follow editor changes](editor-context-mcp.md#follow-editor-changes) for the subscription protocol and cursor behavior.

## Agent-proposed buffer edits

While chatting with an agent started through `tm-codex`, `tm-claude`, `tm-opencode`, or `tm-pi`, ask it to propose changes to an open buffer. For example: **“Simplify this function and propose the edits in TextMate for review.”**

The agent reads the current live text and submits `propose_document_edits`. Review its diff in the **Review Buffer Edits** window and choose **Apply Edits** or **Discard**. This supports unsaved and untitled documents, insertions, replacements, and deletions. Apply groups the changes into one undo action; save the document when ready. If you change the target buffer while the agent works, the stale proposal cannot overwrite your newer edits.

See [Propose edits to live buffers](editor-context-mcp.md#propose-edits-to-live-buffers) for coordinates, limits, and proposal status.

Buffer edit review windows can be moved and resized independently of the editor. The diff expands with the window. Closing a review window discards the proposal; Apply still checks that the buffer has not changed.
