# Copilot Integration Design

Native GitHub Copilot integration for TextMate via the `copilot-language-server` native binary.

## Phases

- **Phase 1**: Copilot completions through existing OakCompletionPopup (Opt+Esc trigger)
- **Phase 2**: Inline ghost text suggestions (auto-triggered, accept with Tab)

This spec covers Phase 1 in full detail. Phase 2 is outlined for architectural awareness.

---

## Architecture

### CopilotManager — Thin Wrapper Over LSPClient

```
copilot-language-server (native arm64 binary, --stdio)
        ↕ JSON-RPC 2.0
    LSPClient (existing — process mgmt, message framing, request/response routing)
        ↕ delegate callbacks
    CopilotManager (new — singleton, Copilot-specific protocol layer)
        ↕ OakCompletionItem[] + notifications
    OakTextView (lspCopilotComplete: → existing OakCompletionPopup)
```

CopilotManager is a singleton that owns an LSPClient instance configured for the copilot-language-server. It adds:

- **Initialization protocol**: `setEditorInfo` after LSP `initialize`/`initialized`
- **Auth lifecycle**: device flow via `workspace/executeCommand` → `github.copilot.signIn`
- **Inline completions**: `textDocument/inlineCompletion` request/response
- **Telemetry**: `didShowCompletion`, `acceptCompletionItem`, `didPartiallyAcceptCompletion`
- **Document focus**: `textDocument/didFocus` on active document change
- **Configuration**: responds to `workspace/configuration` requests from server

### Server Target

`copilot-language-server` native arm64 binary from `@github/copilot` npm package. No Node.js required. Uses standard LSP `textDocument/inlineCompletion` (not legacy `getCompletions`).

Known binary location: `@github/copilot/node_modules/@github/copilot-darwin-arm64/copilot`

### Auto-Detection

Search order for the copilot binary:
1. `.tm_properties` → `copilotCommand` (user override)
2. Homebrew npm globals: `/opt/homebrew/lib/node_modules/@github/copilot/node_modules/@github/copilot-darwin-arm64/copilot`
3. Yarn globals: `~/.config/yarn/global/node_modules/@github/copilot/...`
4. npm prefix globals: `/usr/local/lib/node_modules/@github/copilot/...`

If not found, Copilot features are silently unavailable. Status bar icon stays dimmed.

### Configuration via .tm_properties

```properties
copilotEnabled = true            # default: true globally
copilotCommand = /path/to/copilot  # override auto-detect (--stdio always appended)
copilotLogging = true            # verbose protocol logging (default: false)
```

`copilotCommand` is the path to the binary only. CopilotManager always appends `--stdio`. If the value contains spaces, it is split into command + arguments (same as `lspCommand`).

---

## Phase 1: Inline Completions

### Trigger

`lspCopilotComplete:` action bound to Opt+Esc (`~\033`) in KeyBindings.dict.

### Completion Flow

`textDocument/inlineCompletion` typically returns a single item. The common case is immediate insertion with no UI.

1. User presses Opt+Esc
2. OakTextView `lspCopilotComplete:` extracts cursor position (line/character)
3. Calls `[CopilotManager.sharedManager requestCompletionForDocument:line:character:completion:]`
4. CopilotManager sends `textDocument/inlineCompletion`:
   ```json
   {
     "method": "textDocument/inlineCompletion",
     "params": {
       "textDocument": { "uri": "file:///...", "version": 5 },
       "position": { "line": 10, "character": 15 },
       "context": { "triggerKind": 1 }
     }
   }
   ```
5. **Single result (common case)**: Insert immediately
   - Delete text in the response `range` (if any)
   - Insert `insertText` at cursor position
   - Move cursor to end of inserted text
   - Send telemetry (didShowCompletion + acceptCompletionItem)
   - Toast "Copilot: No suggestions" if empty response
6. **Multiple results (rare)**: Show in OakCompletionPopup for user to pick
   - Convert items to `OakCompletionItem[]` with Copilot icon
   - Show popup at caret via existing `[_lspCompletionPopup show:in:at:items:]`
   - User selects → insert + telemetry
   - User dismisses → no-op

### Insertion

Copilot responses include an explicit `range` field specifying what text to replace:
```json
"range": { "start": {"line": 10, "character": 5}, "end": {"line": 10, "character": 15} }
```

Insertion logic:
1. If `range` is present: delete text in range, insert `insertText` at range start
2. If `range` is absent or zero-width: insert `insertText` at current cursor position
3. Move cursor to end of inserted text

### Telemetry

- `textDocument/didShowCompletion`: Sent when suggestion is displayed to user (immediately for single-result insertion, on popup display for multi-result)
- `workspace/executeCommand` with item's `command`: Sent on acceptance (carries UUID for tracking)

### Multi-Result Popup (Fallback)

When the server returns multiple items, they are shown in the existing OakCompletionPopup:
- `label` = first line of insertText
- `detail` = "Copilot" + line count if multi-line (e.g., "Copilot · 8 lines")
- `icon` = bundled Copilot NSImage
- `originalItem` = full response item (for range + command)
- Filter-as-you-type works normally (local fuzzy filtering on label, no server re-query)

---

## Document Sync

### Notification Mechanism

OakTextView already calls LSPManager methods for document lifecycle. For CopilotManager, we add parallel calls at the same call sites:

```objc
// In OakTextView document lifecycle methods:
[[LSPManager sharedManager] documentDidOpen:doc];
[[CopilotManager sharedManager] documentDidOpen:doc];  // NEW — parallel call
```

This is explicit and avoids observer/notification indirection. The call sites are:
- `setDocument:` → `documentDidOpen:`
- `textDidChange:` (or change debounce timer) → `documentDidChange:version:`
- `dealloc` / document close → `documentWillClose:`

### Sync Details

- **didOpen**: Full document text. CopilotManager sends `textDocument/didOpen` to its LSPClient.
- **didChange**: Full sync initially (matching existing LSPClient behavior). CopilotManager's LSPClient debounces at 300ms independently.
- **didClose**: CopilotManager sends `textDocument/didClose`.
- **didFocus** (Copilot-specific): CopilotManager sends `textDocument/didFocus` when active document changes. Triggered by adding a call in OakTextView's `viewDidMoveToWindow` / `becomeFirstResponder` or by observing `NSWindowDidBecomeKeyNotification`.

### Version Tracking

CopilotManager maintains its own version counter per document URI, independent of LSPManager's versions. Both start at 1 and increment on each change notification.

---

## Authentication

### Flow

1. CopilotManager sends `workspace/executeCommand` → `github.copilot.signIn` on first use
2. Server responds with `userCode` + `verificationUri`
3. CopilotAuthPanel (OakSwiftUI floating panel) displays:
   - User code prominently
   - "Copy Code & Open Browser" button (copies code to clipboard, opens github.com/login/device)
   - "Cancel" button
4. Server polls GitHub internally; returns success
5. Toast: "Copilot: Signed in as @username"
6. Status bar icon becomes active

### Token Storage

Handled entirely by copilot-language-server. Tokens stored in `~/.config/github-copilot/`. TextMate does not manage tokens directly.

### Environment Variable Fallback

Server checks `COPILOT_GITHUB_TOKEN`, `GITHUB_TOKEN`, `GH_TOKEN` automatically. No TextMate code needed.

### Re-authentication

If a completion request fails with auth error, CopilotManager detects it and:
1. Shows toast: "Copilot: Authentication required"
2. Updates status bar icon to dimmed state
3. User clicks icon → triggers sign-in flow again

---

## Status Bar

### Icon-Only Indicator in OTVStatusBar

| State | Icon Appearance |
|-------|----------------|
| Ready | Copilot icon, normal opacity |
| Disabled / Not authenticated | Copilot icon, dimmed/grayed |
| Error | Copilot icon, red tint or warning badge |
| Loading/Connecting | Copilot icon, pulsing or spinner |

Click action: if not authenticated → trigger sign-in. If authenticated → no-op (or show status toast).

### Icon Asset

Bundled as `copilot-icon.pdf` (template image) in TextMate app resources. Rendered at status bar size (~16pt).

---

## Notifications (Toasts)

All notifications via existing `OakNotificationManager.shared.show(message:type:)`.

| Event | Message | Type |
|-------|---------|------|
| Sign-in success | "Copilot: Signed in as @username" | success |
| Auth required | "Copilot: Authentication required" | warning |
| Server error | "Copilot: Server error — restarting" | error |
| Server not found | "Copilot: Server not found" | warning |
| No suggestions (explicit trigger) | "Copilot: No suggestions" | info |
| Server started | "Copilot: Connected" | info |

---

## Logging

Extensive logging under `[Copilot]` prefix via `NSLog`, matching existing `[LSP]` pattern.

Log categories:
- **Protocol**: All JSON-RPC messages sent/received (when `copilotLogging = true`)
- **Lifecycle**: Server start/stop, initialization, auth state changes
- **Completions**: Request timing, response item count, acceptance/rejection
- **Errors**: Parse failures, server crashes, auth errors

All logging enabled by default during development. Production: lifecycle + errors always logged, protocol logging gated by `copilotLogging` setting.

Also posts `NSNotification` with name `CopilotLogNotification` (mirroring `LSPLogNotification`) for potential future log panel integration.

---

## Required LSPClient Extensions

The existing LSPClient needs these additions for CopilotManager:

### 1. Generic Request Method

LSPClient currently has specific methods like `requestCompletionForURI:`. CopilotManager needs to send non-standard methods (`textDocument/inlineCompletion`, `setEditorInfo`, etc.). Add:

```objc
- (void)sendRequest:(NSString*)method params:(NSDictionary*)params completion:(void(^)(id response))callback;
- (void)sendNotification:(NSString*)method params:(NSDictionary*)params;
```

These use the existing JSON-RPC framing and callback infrastructure — just expose them with arbitrary method names instead of hardcoded ones.

### 2. Server-to-Client Request Routing

LSPClient's `handleMessage:` currently handles server requests for `workspace/applyEdit` only. Add a generic delegate method or block-based registration for arbitrary server-initiated requests:

```objc
// LSPClientDelegate addition:
- (id)lspClient:(LSPClient*)client handleServerRequest:(NSString*)method params:(NSDictionary*)params;
```

CopilotManager implements this to handle `workspace/configuration` requests.

### 3. Post-Initialization Hook

LSPClient sends `initialize` → `initialized` internally. CopilotManager needs to inject `setEditorInfo` immediately after `initialized`. Options:
- **Delegate callback**: `- (void)lspClientDidInitialize:(LSPClient*)client` called after `initialized` is sent
- CopilotManager uses this hook to send `setEditorInfo` via the generic request method

---

## Initialization Sequence

1. **App launch**: CopilotManager initializes lazily on first document open
2. **Server discovery**: Auto-detect binary path
3. **Process spawn**: LSPClient creates NSTask with `copilot --stdio`
4. **LSP handshake**: `initialize` → response → `initialized` notification
5. **Post-init hook**: LSPClient calls `lspClientDidInitialize:` delegate
6. **Editor info**: CopilotManager sends `setEditorInfo` via generic request: `{editorInfo: {name: "TextMate", version: "2.1"}, editorPluginInfo: {name: "textmate-copilot", version: "1.0"}}`
7. **Auth check**: CopilotManager sends `workspace/executeCommand` → `checkStatus`
8. **If authenticated**: Status bar icon → ready, begin accepting completion requests
9. **If not authenticated**: Status bar icon → dimmed, show toast with sign-in prompt

### Server Crash Recovery

If the copilot-language-server process terminates:
1. LSPClient calls `lspClientDidTerminate:` delegate on CopilotManager
2. CopilotManager shows toast: "Copilot: Server error — restarting"
3. Status bar icon → error state
4. After 5s delay, CopilotManager re-creates LSPClient and restarts the initialization sequence
5. Max 3 restart attempts, then stays in error state until user manually triggers restart (via status bar click)

---

## File Layout

### New Files

```
Frameworks/lsp/src/
  CopilotManager.h              Singleton interface
  CopilotManager.mm             Implementation (wraps LSPClient, auth, completions, telemetry)

Frameworks/OakSwiftUI/Sources/OakSwiftUI/
  Bridge/CopilotAuthPanel.swift Device flow sign-in floating panel

Applications/TextMate/resources/
  copilot-icon.pdf              Template image for status bar
```

### Modified Files

```
Frameworks/OakTextView/src/OakTextView.mm
  + lspCopilotComplete: action
  + CopilotManager integration in completion delegate callbacks (telemetry)
  + Copilot status icon in OTVStatusBar

Frameworks/OakTextView/src/OTVStatusBar.mm (or .swift if SwiftUI)
  + Copilot icon indicator

Frameworks/lsp/CMakeLists.txt
  + CopilotManager source files

Applications/TextMate/resources/KeyBindings.dict
  + "~\033" = "lspCopilotComplete:"
```

---

## Phase 2: Ghost Text (Future — Outline Only)

Phase 2 adds inline ghost text rendering, triggered automatically on typing pause.

### Trigger
- Auto-trigger: ~500ms debounce after typing stops
- Manual: Opt+Esc still available for popup mode

### Rendering
- Ghost text drawn inline in OakTextView at cursor position
- Dimmed/italic styling (using theme's comment color at ~40% opacity)
- Multi-line: first line inline, additional lines rendered below
- Implemented as a new rendering pass in OakTextView's drawing code, or as a SwiftUI overlay (CopilotGhostText.swift)

### Keybindings
- **Tab**: Accept full suggestion
- **Cmd+Right**: Accept next word (partial acceptance)
- **Esc**: Reject / dismiss
- **Any other keystroke**: Dismiss and process keystroke normally
- **Opt+]** / **Opt+[**: Cycle through alternative suggestions

### Telemetry
- `textDocument/didShowCompletion` when ghost text appears
- `workspace/executeCommand` → `acceptCompletionItem` on Tab
- `textDocument/didPartiallyAcceptCompletion` on word-by-word accept (UTF-16 code units)

### Architectural Impact
- New ghost text rendering layer in OakTextView (or SwiftUI overlay)
- CopilotManager tracks "active suggestion" state
- Tab key interception when ghost text is visible (careful not to conflict with snippet navigation)

---

## Copilot Protocol Reference

### Custom Methods (Beyond Standard LSP)

| Method | Direction | Purpose |
|--------|-----------|---------|
| `textDocument/inlineCompletion` | Client→Server | Request inline suggestions |
| `textDocument/didFocus` | Client→Server | Active document changed |
| `textDocument/didShowCompletion` | Client→Server | Suggestion was displayed |
| `textDocument/didPartiallyAcceptCompletion` | Client→Server | Partial word acceptance |
| `workspace/executeCommand` | Client→Server | Auth commands + acceptance telemetry |
| `workspace/configuration` | Server→Client | Server requests editor settings |

### Inline Completion Response Shape

```json
{
  "items": [{
    "insertText": "def hello():\n    print('world')",
    "range": {
      "start": { "line": 10, "character": 15 },
      "end": { "line": 10, "character": 15 }
    },
    "command": {
      "title": "Accept",
      "command": "github.copilot.acceptCompletionItem",
      "arguments": ["<uuid>", "<telemetry-data>"]
    }
  }]
}
```

### Auth Commands

- `github.copilot.signIn` → returns `{userCode, verificationUri}`
- `github.copilot.checkStatus` → returns `{status, user}`

---

## Testing Strategy

### Unit Tests (CxxTest)

```
Frameworks/lsp/tests/
  t_copilot_manager.cc    CopilotManager initialization, auto-detect, config parsing
```

### Integration Tests

- Mock copilot-language-server responses for completion lifecycle
- Auth flow with mock server
- Document sync (open/change/close/focus)

### Manual Testing

- Real copilot-language-server with multiple file types
- Tab switching (verify didFocus)
- Auth flow end-to-end
- Toast notifications for all states
- Status bar icon transitions

---

## Dependencies

### Required
- `@github/copilot` npm package (for the native binary)
- Existing: LSPClient, LSPManager, OakCompletionPopup, OakNotificationManager, OakThemeEnvironment

### No New External Dependencies
- JSON parsing: nlohmann/json (already in vendor/)
- Process management: NSTask (already used by LSPClient)
- UI: OakSwiftUI (already linked to OakTextView)
