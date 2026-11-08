# Querying TextMate from a terminal agent

TextMate bundles an editor MCP server, `textmate-mcp`, and launchers for Codex (`tm-codex`), Claude Code (`tm-claude`), OpenCode (`tm-opencode`), and pi (`tm-pi`). Agents can query live editor buffers, including unsaved changes and untitled documents. Queries stay within the window owning the terminal session, even while the terminal has keyboard focus. Moving a terminal to another window updates its context without restarting the agent.

## Start Codex

Launch the updated TextMate app and open a **new terminal tab**, then run:

```sh
tm-codex
```

The launcher checks the editor connection, finds Codex CLI, and starts it with temporary MCP settings for this terminal. It preserves the current directory and forwards arguments, including prompts, model options, `exec`, and `resume`. For example:

```sh
tm-codex "Tell me which file is open and explain my selection"
```

The bundled command directory is appended to new terminals' PATH, so it never shadows a command you installed. If a shell startup file replaces PATH entirely, run the launcher by its full path:

```sh
"${TM_MCP_SERVER%/*}/tm-codex"
```

Install and sign in to [Codex CLI](https://learn.chatgpt.com/docs/cli) first if necessary. The launcher checks PATH, the ChatGPT application's bundled CLI, and common Homebrew locations. Set `TM_CODEX_COMMAND` to the absolute executable path for another installation. Codex must support `--no-daemon`; this ensures a terminal uses its own connection environment rather than a previously started shared server. An older CLI reporting an unknown option needs updating.

The launcher neither writes Codex configuration nor places session credentials in command arguments. It passes the names of the environment variables Codex must forward, as described in [Codex MCP configuration](https://learn.chatgpt.com/docs/extend/mcp). User-supplied Codex configuration options still work and can override the launcher's settings.

## Start Claude Code

Install and sign in to [Claude Code](https://code.claude.com/docs/en/setup), then open a new terminal in the updated TextMate app and run:

```sh
tm-claude
# Or start with a prompt:
tm-claude "Explain the selected text in TextMate"
```

The launcher checks the editor connection and supplies a temporary `textmate` server using Claude's [`--mcp-config` option](https://code.claude.com/docs/en/cli-reference). Claude's [environment variable expansion](https://code.claude.com/docs/en/mcp#environment-variable-expansion-in-mcpjson) forwards the terminal credentials by reference, so their values do not appear in arguments or configuration files.

Claude's existing MCP servers and permission settings stay enabled. Session arguments, such as `--model`, `--continue`, `--resume`, and print mode, are forwarded. Administrative subcommands should be run directly with `claude`.

The launcher searches PATH, `~/.local/bin/claude`, the older `~/.claude/local/claude` location, and common Homebrew locations. Set `TM_CLAUDE_COMMAND` to an explicit executable path for another installation. To launch by full path when shell startup files replace PATH:

```sh
"${TM_MCP_SERVER%/*}/tm-claude"
```

## Start OpenCode

Install and configure [OpenCode](https://opencode.ai/docs/), then open a new TextMate terminal and run:

```sh
tm-opencode
# Or run a prompt without the terminal UI:
tm-opencode run "Explain the selected text in TextMate"
```

The launcher adds a local `textmate` MCP server through OpenCode's [`OPENCODE_CONFIG_CONTENT` runtime configuration](https://opencode.ai/docs/config/#precedence-order). Existing inline JSON/JSONC settings, other MCP servers, provider settings, permissions, and arguments are preserved. Only the inline `mcp.textmate` entry is replaced for the current terminal. Malformed inline configuration produces an error without echoing or replacing its contents.

The server's `environment` fields use OpenCode's [`{env:VARIABLE}` references](https://opencode.ai/docs/config/#env-vars), so connection credentials remain in environment variables rather than arguments or configuration files. Global and project settings continue to load normally, and administrator-managed settings retain OpenCode's usual precedence.

The launcher searches PATH, `~/.opencode/bin/opencode`, `~/.local/bin/opencode`, and common Homebrew locations. Set `TM_OPENCODE_COMMAND` to another absolute executable path if needed. To launch by full path when shell startup files replace PATH:

```sh
"${TM_MCP_SERVER%/*}/tm-opencode"
```

Use `tm-opencode --check` to test the editor connection without starting OpenCode. Commands that attach to an already running remote OpenCode server use that server's configuration; start a local OpenCode session for this terminal's editor context.

## Start pi

Install [pi](https://pi.dev), open a new TextMate terminal tab, and run:

```sh
tm-pi
# Or run a prompt without the terminal UI:
tm-pi -p "Explain the selected text in TextMate"
```

pi has no inline MCP configuration, so the launcher loads a bundled extension (`pi --extension`) that registers the `textmate` server for this session only. Your `mcp.json`, settings, and other extensions are untouched, and the connection credentials stay in environment variables. The tools are declared to the model directly as `mcp__textmate__*` rather than hidden behind codemode. Arguments are forwarded. pi's subcommands (`install`, `remove`, `uninstall`, `update`, `list`, `config`, `auth`, `mcp`) run unchanged, without the extension.

A server named `textmate` in your own `mcp.json` takes precedence over the registration, and an installed extension that replaces pi's built-in MCP support, such as `pi-mcp-adapter`, stops it from connecting.

The launcher searches PATH, `~/.local/bin/pi`, and common Homebrew locations. Set `TM_PI_COMMAND` to an explicit executable path for another installation. To launch by full path when shell startup files replace PATH:

```sh
"${TM_MCP_SERVER%/*}/tm-pi"
```

Use `tm-pi --check` to test the editor connection without starting pi.

## Check the connection

```sh
tm-codex --check
tm-claude --check
tm-opencode --check
tm-pi --check
# or:
"$TM_MCP_SERVER" --check
```

A successful check identifies the connected window and current document without reading document content. Errors distinguish missing environment variables, an unavailable service, permission denial, a closed or unauthorized session, an invalid response, and a timeout.

Open a new terminal if its variables are missing. Restarting TextMate ends existing sessions; old credentials cannot reconnect. After rebuilding, restart the app to load the updated service. A new terminal tab alone does not update an already running application binary.

## Other MCP clients

Inside a TextMate terminal, run:

```sh
"$TM_MCP_SERVER" --print-config
```

This prints an `mcpServers` JSON entry with the helper's absolute path and explicit `TM_CONTEXT_SOCKET`, `TM_CONTEXT_SESSION`, and `TM_CONTEXT_TOKEN` **values**. The printed entry therefore contains a live session token, not a `${TM_CONTEXT_TOKEN}` reference — unlike the three launchers, which pass environment references and leave the value in the terminal's environment. Register that entry using the client's configuration mechanism and treat the printed JSON as a credential: these values belong to this terminal, they stop working when the session is revoked, and they must not be committed to a project. Generate separate configuration for separate tabs. Emitting references here instead is a known inconsistency that is deliberately deferred.

Clients supporting environment forwarding can pass those three variables from the launching shell instead. Explicit forwarding matters because MCP clients may inherit only a restricted environment. `TM_MCP_SERVER` holds the helper's absolute path.

The helper runs without arguments as an MCP stdio server. Standard output contains protocol messages only. No Python, Node, TCP listener, or external service is needed by the TextMate integration at runtime. The chosen agent CLI is installed separately.

## What the capability token does not protect

The three `TM_CONTEXT_*` variables are the connection's credentials, and any process running as your user can read them. Measured on a Release build: `ps -E -p <pid of a shell started in the embedded terminal>` printed the shell's full environment, including `TM_CONTEXT_SESSION`, `TM_CONTEXT_SOCKET`, and `TM_CONTEXT_TOKEN` (a 72-character random value). Descendants inherit all three, across `sh -c`, `python3 -c`, and a grandchild of that shell; `env -i sh -c` starts with none of them. `ps -E -p 1`, a root-owned process, printed no environment entries at all, so the exposure comes from same-user process inspection rather than from a world-readable store.

Stated plainly: **“the user approved this terminal session” is equivalent to “any code running as the same user holds a live token.”** The boundary is the user account, not the token. Moving the token from the environment into a mode-0600 file would not change that, because a same-user process can read the file too.

The token is an approval marker, not a secret capability. It identifies which terminal session is asking, so closing or transferring a terminal ends that grant and two terminals in one window stay separate. It does not exclude code that already runs as you: such code can also open the socket directly, read your files, or drive the editor through other means. What limits it is the application's per-request re-authorisation against the terminal registration, and revocation on terminal closure — not secrecy of the value.

## Tools

- `get_editor_context`: current document metadata, cursor, selections and selected text, plus roughly 2,048 UTF-16 code units around the cursor. Selected text has an 8,192-unit combined budget; at most 128 ranges are returned. Column selections expand into rows with the limit applied during expansion. Truncation is explicit.
- `list_open_documents`: up to 256 document IDs and metadata in the associated window. Includes path (null for untitled documents), name, language, modified state, loaded state, and an opaque content revision.
- `read_document`: reads a loaded document by `documentId`, with optional `offset` (default 0) and `length` (default 16,384; maximum 32,768). Returns text, total length, next offset, revision, and truncation status. It does not open arbitrary paths or load inactive buffers.
- `get_diagnostics`: cached language-server diagnostics for the current document, or an optional `documentId` from `list_open_documents`. The document must be loaded in the terminal's window. Returns document metadata, messages, severity (`error`, `warning`, `information`, `hint`, or `unknown`), optional source and code, and one-based UTF-16 ranges with exclusive ends. A `location` such as `/path/to/file:12` can be repeated in the agent's reply and Command-clicked in the terminal to jump to the source line. The link intentionally targets the line: terminal links use native byte columns, while diagnostic ranges retain UTF-16 columns. Invalid server ranges are explicitly marked instead of inventing a location.
- `navigate_editor`: switches to an open `documentId` in the terminal's window, or opens an existing local file by absolute `path`. A `path` must name a file inside the window's project directory, or a file already open in that window; anything else is refused with `path_outside_project` before a tab is created. A window without a project directory only navigates to documents already open in it. Optional line/column and range coordinates position the cursor or select text. This tool changes editor UI state, is annotated `readOnlyHint: false`, and does not edit or save document text.
- `get_editor_changes`: initial context and an opaque cursor, or changes since an optional previous `cursor`. Changed responses include a fresh context snapshot; unchanged responses omit document text. This is the tool-based fallback for clients that do not subscribe to MCP resources.

Offsets and lengths count **UTF-16 code units**, starting at zero. Line and column numbers start at one; lines split on LF. Ranges are half-open. Text boundaries preserve surrogate pairs, and reads reject offsets inside a pair. Continue with the returned `nextOffset` and compare document ID and revision between requests, since the user can edit between calls. A requested length of one at a supplementary character may return no text; request at least two units to advance.

No-document context returns a null document. Closed windows, revoked sessions, foreign document IDs, malformed arguments, and unavailable services produce errors rather than falling back to another window or disk content.

### Using diagnostics

Ask a connected agent: **“Read TextMate's diagnostics for the current file, explain the errors and warnings, and include source locations.”** To inspect other files, the agent can list open documents and call `get_diagnostics` for each loaded document ID.

`get_diagnostics` accepts `offset` (default 0) and `limit` (default 50, maximum 100). These offsets count diagnostic entries, not text characters. Follow `nextOffset` while `truncated` is true. Pass the returned `reportId` with subsequent pages; an updated or cleared report produces `diagnostics_changed`, and the agent should restart at offset 0 without that ID. Capture the returned `documentId` when paging the current file so switching editor tabs cannot change the target.

`status` is `reported` when a report has been received, including a report with zero diagnostics; `pending` when a language server is starting or running without a report; and `unavailable` when no report or active server exists. `serverStatus` independently reports `starting`, `running`, `indexing`, or `unavailable`, including when an older report is cached after a server stops. `pendingChanges` indicates edits still waiting to be sent to the server. `freshness` is `unknown`: a cached diagnostic report is not guaranteed to match the current buffer revision, even when there are no pending changes. The document revision describes the buffer at query time, not the version used to produce diagnostics.

Messages are capped at 4,096 UTF-16 units, source/code strings at 256, and location strings at 2,048, preserving surrogate pairs. Each page has a combined 32,768-unit string budget and may contain fewer entries than requested. `textTruncated` on an entry and on the response flags shortened fields; it is independent of page `truncated`. Opaque code-action data is not exported.

This reads the same language-server reports used by TextMate's inline diagnostics. It does not launch a server, trigger analysis, run a build, or parse output from terminal commands or bundle build actions. No diagnostics available is not evidence that a build succeeds. The five query tools remain read-only; `navigate_editor` changes editor UI state.

### Navigate from the conversation

Ask the agent **“Open the file containing that error and select the affected code in TextMate”**, **“Show line 42 of this file”**, or **“Switch to my untitled document.”** The agent uses `navigate_editor` with exactly one of `documentId` or `path`.

For an open document, including an unsaved or untitled buffer:

```json
{"documentId":"ID from get_editor_context or list_open_documents","line":12,"column":4,"endLine":14,"endColumn":1}
```

For a file not yet open:

```json
{"path":"/absolute/project/src/example.py","line":42}
```

Coordinates are one-based UTF-16, matching the query tools and diagnostic ranges. `column` defaults to 1 when `line` is given. Supplying both `endLine` and `endColumn` selects a half-open range; omitting them moves the cursor. Omitting all coordinates retains the document's selection. Out-of-range coordinates, reversed ranges, and positions inside a surrogate pair fail without changing the visible tab or selection. Tabs and multibyte characters are converted to native editor coordinates without treating UTF-16 columns as byte columns.

The agent can pass the document's opaque `revision` from a query to reject stale coordinates. Navigation also detects edits between locating and applying a range. `focus` defaults to false so the terminal retains keyboard focus; setting it to true raises the window and focuses the editor. Existing tabs are reused, and opening a file appends a tab without closing existing tabs or discarding unsaved work. The result contains `status: "navigated"`, window/document metadata, `opened`, `focused`, and the applied `range` (null when no coordinates were supplied).

Document IDs are accepted only from the terminal's own window. An explicit path can open an existing regular local file inside the window's project directory, making that file accessible to subsequent context queries in this window. Anything else is refused with `path_outside_project` before a tab is created, unless the file is already open in the window; a window without a project directory only navigates to documents already open in it. Relative paths, URLs, missing files, directories, devices, and FIFOs are rejected. File loading does not request encoding or authorization dialogs; files needing such interaction should be opened manually first.

Navigation follows the same three-second deadline as queries. Deferred work rechecks the terminal registration, owning window, document revision, and request generation before changing the UI. Closing or transferring the terminal, closing the window, expiration, or a newer navigation request prevents an older pending request from changing its former target. Cancellation suppresses the MCP reply; it does not undo a navigation that already completed.

### Follow editor changes

The server advertises MCP resource subscriptions and one JSON resource, `textmate://editor/context`. `resources/read` returns the same current-file, selection, cursor, and surrounding-text snapshot as `get_editor_context`, including `changeCursor`. `resources/list` discovers it; `resources/templates/list` returns an empty list. A client can subscribe with `resources/subscribe`, receive `notifications/resources/updated` with this URI, and then reread the resource when needed. `resources/unsubscribe` stops updates.

Events cover active-document switches, selection/cursor changes, unsaved content edits (including inactive documents open in the same window), tab-list changes, document metadata, and diagnostics. Notifications contain only the resource URI, not document text or credentials. Changes are coalesced by observing an inexpensive opaque token at most every 500 ms while subscribed; context is fetched only when requested. An initial update after subscription establishes a fresh baseline. Unchanged tokens emit no further notifications. Without a subscriber, the helper performs no background editor queries. EOF stops the subscription worker, and unsubscribing prevents further updates after its acknowledgement.

For a client that only uses tools, ask **“Follow the file and selection as I work; use get_editor_changes to refresh only when something changes.”** Call `get_editor_changes` without arguments for an initial `context` and `cursor`. Pass the returned cursor, or `changeCursor` from an ordinary context/resource read, on later calls:

```json
{"cursor":"opaque cursor returned by TextMate"}
```

When `changed` is false, the response contains no context text. When true, it includes recent `events`, a fresh `context`, and the next `cursor`. Each event has a type and optional document ID, never the edited text. The per-window journal retains at most 128 events. `reset: true` means an initial, invalid, foreign-window, or expired-history cursor; use the complete included context rather than reconstructing old state. Rapid edits may be coalesced for notification delivery, and this is not a lossless keystroke history. A terminal transfer switches to the destination window's cursor and forces a refresh; closing a terminal revokes its access.

MCP resources are [application-driven](https://modelcontextprotocol.io/specification/2025-11-25/server/resources): the client decides whether to subscribe, reread changed resources, add them to model context, or start another model turn. Advertising subscriptions does not force Codex, Claude Code, or OpenCode to wake an idle model. The tool fallback is available to all three, but requires the agent to call it; it is not itself a background subscription. No agent-specific channel flags, hidden prompts, or recurring model calls are installed.

## Implementation

`EditorContextService` owns one private Unix socket per application instance, in a mode-0700 temporary directory; the socket has mode 0600. Each terminal receives a separate random capability token. Every request validates credentials and resolves the terminal's current owning window. Terminal closure revokes its registration. Normal application termination removes the socket; an abrupt process kill may leave an inaccessible directory in the system temporary area. The token identifies the session; it is not a boundary against same-user code (see [What the capability token does not protect](#what-the-capability-token-does-not-protect)).

The main thread captures metadata, bounded raw selection ranges, and an immutable snapshot sharing the buffer's existing storage. Background workers build a sparse UTF-16/line index and extract only requested text. The index is reused for an unchanged buffer, and edits invalidate the cached snapshot. In-flight snapshots remain consistent even when the user edits or closes the document.

Local requests have a three-second overall deadline, covering connection, I/O, main-thread scheduling, and rendering. Reads and writes use deadline-aware polling; a peer sending a slow trickle of bytes cannot extend the deadline. The service admits at most eight concurrent connections, and messages are limited to 1 MiB.

The native helper implements MCP's stdio tools capability for protocol revisions 2024-11-05, 2025-03-26, 2025-06-18, and 2025-11-25. It validates initialization, waits for `notifications/initialized`, rejects invalid tool arguments and duplicate in-flight IDs, and remains able to process pings and cancellation while tool calls run. Cancellation suppresses the result; an outstanding editor request is allowed to finish within its deadline. Editor text is untrusted content for the agent to interpret.

## Verification

Build with `make debug` or `make release`. The MCP helper and all three launchers are embedded and signed inside the application bundle. Run the native and executable regression tests with:

```sh
ctest --test-dir build-debug -R '^(EditorContext|DocumentWindow|selection|textmate-mcp)$' --output-on-failure
```

The tests cover live and immutable buffers, Unicode and pagination, multiple and rectangular selections, large documents, concurrent contexts in real windows, moving a live terminal, closure and revocation, launcher argument and path handling, initialization, cancellation, diagnostics, and slow-peer deadlines. If Codex is installed, an additional executable test checks the generated configuration with the actual CLI without invoking a model.

An optional interoperability test uses the official MCP TypeScript SDK installed separately for development:

```sh
npm install --prefix /tmp/textmate-mcp-sdk --ignore-scripts @modelcontextprotocol/sdk
node Applications/textmate-mcp/tests/test_sdk.mjs \
  /tmp/textmate-mcp-sdk/node_modules/@modelcontextprotocol/sdk/dist/esm \
  build-debug/Applications/textmate-mcp/textmate-mcp
```

Alternatively, set CMake's `TEXTMATE_MCP_SDK_DIR` to that SDK `dist/esm` directory to add the `textmate-mcp-sdk` CTest target. Tests need permission to create local Unix sockets, and AppKit tests require access to a graphical login session.

For an optional real-Claude startup check, supply an installed Claude executable:

```sh
python3 Applications/textmate-mcp/tests/test_claude_cli.py \
  build-debug/Applications/textmate-mcp/textmate-mcp /absolute/path/to/claude
```

This check uses isolated temporary settings and a local API stub that rejects inference. It verifies the actual launcher's inline configuration, environment expansion, and MCP startup without logging in or sending a request to a real model.

An optional real-OpenCode check verifies the launcher, configuration references, and MCP discovery using isolated temporary settings, with no login or model inference:

```sh
python3 Applications/textmate-mcp/tests/test_opencode_cli.py \
  build-debug/Applications/textmate-mcp/textmate-mcp /absolute/path/to/opencode
```

## Language intelligence

Terminal agents can ask TextMate's existing language server for semantic code information. These tools are read-only: they do not move the cursor, open files, apply edits, or save documents. Targets must be loaded, named documents in the terminal's owning window. Configure and start LSP through normal editing first; queries do not launch or restart a server.

- `get_definition`: definition locations for the current cursor, or explicit `documentId`, `line`, and `column`.
- `find_references`: reference locations at the same kinds of positions. `includeDeclaration` defaults to `false`; set it to `true` to include declarations.
- `get_document_symbols`: symbols in the current or specified document. Nested symbols are flattened in source order with `name`, numeric LSP `kind`, and `container`.
- `search_workspace_symbols`: requires a nonempty `query` of up to 256 characters. Uses the server attached to the current or specified `documentId`. It searches that server's workspace, rather than combining every language server in a mixed-language project.
- `get_hover`: bounded type information/documentation at the current cursor or an explicit position. Returns `hover.text`, an optional range, and `textTruncated`; a successful `hover: null` means the server has no hover information at that position.

For example, ask an agent: **“Use TextMate's language tools to find the definition of the symbol under my cursor and explain its callers.”** Or: **“List this document's symbols, then search the workspace for related implementations.”**

Positions use one-based lines and UTF-16 columns, with half-open ranges. `column` defaults to 1 when `line` is given. Omit both to use the current cursor; an inactive document requires an explicit position. Invalid positions, including the middle of a surrogate pair, are rejected. Optional `revision` rejects a query against an outdated source buffer. Pending edits to the source document are sent before the request. If its content, path, or terminal session changes before completion, the result is rejected. Language-server indexes can still be incomplete or indexing other project files.

Location entries include the original `uri`, a local `path` where available, and a one-based `range`. `location` is a source-line link such as `/path/file.swift:42`, intentionally omitting the UTF-16 column so it can be Command-clicked in the terminal. Definition `LocationLink` responses use the target selection range. Nonlocal URIs retain their URI but have no local path or terminal link. Workspace symbols without a resolved range explicitly have `unresolved: true` and `range: null`; coordinates are never invented. Returned hover text, symbol names, and paths are untrusted content for the agent to interpret.

### Stable result pages

Location/symbol results use `items`. A query returns up to 50 entries by default; optional `limit` accepts 1–100. If `nextOffset` is not null, call the same tool with its `resultId`, `offset: nextOffset`, and an optional `limit`. Other query arguments can be omitted when paging: the result identifies the original document even if the active tab has changed. Supplied arguments must match the original query.

Pages read a cached result rather than rerunning the language server. The window retains at most eight results, and document edits invalidate access to an old result. An unavailable result returns `result_expired`; start again without `resultId`. A new query cannot specify a nonzero offset. `availableCount` counts cached entries; `truncated` indicates more pages or a capped result. Each result retains at most 1,000 entries and a 131,072-unit combined string budget, with at most 5,000 symbol nodes and 32 nested levels inspected. `resultsTruncated: true` means some server results were not retained; narrow the query where possible. Symbol names and container names are capped at 256 UTF-16 units and indicate shortened text with `textTruncated`. Hover text is capped at 8,192 units. Malformed location/symbol entries are skipped and counted in `invalidCount`.

### Availability and errors

A successful empty `items` array means the supported request returned no matches. `server_unavailable`, `server_starting`, and `unsupported_capability` are distinct errors. The tools support servers using UTF-16 positions and statically advertised capabilities. A rejected server request returns `language_request_failed`, rather than being interpreted as no matches. Requests share the normal three-second editor deadline; overdue language requests are cancelled on their original server and return `timeout`. Retry once the server has finished indexing, or consult **View → LSP Log** for a server error. MCP cancellation suppresses the reply; an outstanding editor operation remains bounded by the deadline.

## Propose edits to live buffers

Agents can propose changes to any loaded document in their terminal's editor window with `propose_document_edits`. This works with unsaved and untitled buffers, including inactive editor tabs.

First use `get_editor_context`, `list_open_documents`, or `read_document` to identify the buffer and its opaque `revision`. Read the text being edited from the live buffer rather than a disk copy. Then provide `documentId`, the exact `revision`, a short `summary`, and an `edits` array. Each edit contains:

- `offset`: zero-based UTF-16 offset into the original buffer revision.
- `length`: number of original UTF-16 units to replace; zero inserts text.
- `text`: complete replacement text; an empty string deletes the original range.

All offsets refer to the same original revision, even when multiple edits are supplied. Edits can be supplied in any order. Ranges must not overlap or share a start offset; combine multiple insertions at one position into one edit. UTF-16 surrogate pairs cannot be split. The maximum is 128 edits, replacing up to 131,072 original UTF-16 units with up to 32,768 replacement units combined. `summary` accepts up to 2,048 units. A proposal containing no text changes is rejected.

For example, to replace the first three units of a buffer and insert text at offset 10:

```json
{
  "documentId": "ID returned by TextMate",
  "revision": "revision returned with the live text",
  "summary": "Correct the value and add an explanation",
  "edits": [
    {"offset": 0, "length": 3, "text": "new"},
    {"offset": 10, "length": 0, "text": " explanation"}
  ]
}
```

TextMate shows a **Review Buffer Edits** window with a diff and **Apply Edits** / **Discard** buttons. The tool returns promptly with `proposalId`, `status: "pending_review"`, and `applied: false`. Only the user can apply the changes; there is no MCP tool that accepts the review. Applying all edits forms one undo action in the target buffer. It neither saves the file nor switches the current editor tab. Discard leaves the buffer unchanged. Apply requires an explicit click or Command–Return, so a plain Return typed in the terminal cannot accept a newly arrived proposal.

Use `get_edit_proposal_status` with `proposalId` when the user has reviewed it. Status is `pending_review`, `applied`, `discarded`, or `stale`; the current document metadata and revision are included if it remains open. Do not poll repeatedly or claim that pending changes are already applied. Each window retains up to eight proposal records. An unknown, evicted, or foreign-window identifier returns `proposal_unavailable`.

Both preparation and application check the target identity, path, and content revision. Editing or closing the target invalidates a pending proposal. Source coordinates are never silently rebased over newer typing. Other open buffers and the live cursor do not retarget it. A busy or hidden editor window returns `review_unavailable`; close its current dialog or show the window before retrying. Terminal closure or transfer cancels work that has not yet reached review. Once a review is displayed, the user can still make their explicit Apply/Discard choice if the agent exits. Proposals remain in their original window when a terminal moves.

A useful request is: **“Refactor this open buffer and propose the edits in TextMate for me to review.”** The agent's own CLI permissions still apply; this tool supplies a buffer-aware review path rather than restricting every external action that agent can perform.

Buffer edit review windows can be moved and resized independently of the editor. The diff expands with the window. Closing a review window discards the proposal; Apply still checks that the buffer has not changed.
