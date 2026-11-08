# LSP Server Request UI — `window/showMessageRequest` and `window/showDocument`

## Problem

LSP servers send interactive requests to the client that require user action:

- **`window/showMessageRequest`** — asks the user a question with action buttons (e.g., ESLint "Allow validation?", rust-analyzer "Download server binary?", Intelephense "Enter license key?"). Currently returns `null` (user dismissed) without showing anything.
- **`window/showDocument`** — asks the client to open a file or URL. Currently returns `{}` (wrong shape — should be `{success: bool}`).

Both are server-to-client requests (have an `id`, require a response).

## Approach

Extend the existing toast notification system (`OakNotificationManager` / `NotificationView`) with an interactive variant that shows action buttons and returns the selected action. This keeps a consistent visual language — the toast slides up from the bottom, but with clickable buttons instead of auto-dismissing.

For `window/showDocument`, handle in `LSPClient` directly — no UI needed, just open the file/URL.

## Design

### 1. `window/showMessageRequest` — Interactive Toast

#### LSP Request Structure
```json
{
  "type": 3,         // 1=Error, 2=Warning, 3=Info, 4=Log
  "message": "ESLint wants to validate files in this workspace",
  "actions": [
    {"title": "Allow"},
    {"title": "Deny"}
  ]
}
```

#### LSP Response
```json
{"title": "Allow"}   // user selected an action
null                  // user dismissed without selecting
```

**Important**: Servers like vscode-eslint send custom properties on `MessageActionItem` (e.g., `{"title": "Fix", "id": "eslint:fix:123"}`) and expect the **full object** back in the response, not just the title. The implementation must preserve and return all properties from the original action object, not reconstruct it from the title string alone.

#### UI: Interactive Toast with Action Buttons

Extend the existing `NotificationView` to support an optional actions array. When actions are present:
- Toast does NOT auto-dismiss (stays until user acts)
- Action buttons appear as pill-shaped buttons to the right of the message
- Clicking a button dismisses the toast and triggers the callback
- Clicking the toast background or pressing Escape dismisses with `null`
- Only one interactive toast at a time — new ones queue behind the current one

Visual layout:
```
┌──────────────────────────────────────────────────────────┐
│  ⚠  ESLint wants to validate files    [Allow]  [Deny]   │
└──────────────────────────────────────────────────────────┘
```

Same styling as existing toasts: `.ultraThinMaterial` background, rounded corners, dark color scheme, slides up from bottom.

### 2. Data Model Changes

#### Toast (extended)
```swift
public struct Toast: Identifiable, Equatable {
    public let id = UUID()
    public let message: String
    public let type: ToastType
    public let duration: TimeInterval
    public let actions: [String]?            // nil = auto-dismiss toast, non-nil = interactive
    public let onAction: ((String?) -> Void)? // called with action title or nil on dismiss
}
```

`Equatable` conformance should ignore the closure (compare by `id` only).

#### ToastViewModel (extended)
```swift
public func showInteractive(message: String, type: ToastType, actions: [String], onAction: @escaping (String?) -> Void) {
    // Same as show() but no auto-dismiss timer
    self.currentToast = Toast(message: message, type: type, duration: 0, actions: actions, onAction: onAction)
}
```

If an interactive toast is already showing and a new one arrives, queue it. Non-interactive toasts (auto-dismiss) can replace each other as they do now.

### 3. NotificationView Changes

```swift
if let actions = toast.actions, !actions.isEmpty {
    ForEach(actions, id: \.self) { action in
        Button(action) {
            toast.onAction?(action)
            withAnimation { model.currentToast = nil }
            model.showNextQueued()
        }
        .buttonStyle(.bordered)
        .tint(color(for: toast.type))
    }
}
```

Add Escape key handling via `.onKeyPress(.escape)` (macOS 14+) to dismiss with `nil`.

### 4. OakNotificationManager Bridge

Add a new `@objc` method:

```swift
@objc public func showInteractive(message: String, type: Int, actions: [String], callback: @escaping (String?) -> Void) {
    let toastType: ToastType = ...  // same mapping as show()
    self.ensureWindow()
    self.model.showInteractive(message: message, type: toastType, actions: actions, onAction: callback)
}
```

### 5. LSPClient Integration

In `handleMessage`, replace the current `window/showMessageRequest` handler:

```objc
else if(method == "window/showMessageRequest")
{
    int requestId = msg["id"].get<int>();
    std::string message = msg["params"].contains("message") ? msg["params"]["message"].get<std::string>() : "";
    int type = msg["params"].contains("type") ? msg["params"]["type"].get<int>() : 3;

    // Preserve full action objects (servers like eslint include custom properties)
    NSMutableArray<NSDictionary*>* actions = [NSMutableArray new];
    NSMutableArray<NSString*>* actionTitles = [NSMutableArray new];
    if(msg["params"].contains("actions"))
    {
        for(auto const& action : msg["params"]["actions"])
        {
            [actions addObject:[self convertJSON:action]];
            [actionTitles addObject:to_ns(action["title"].get<std::string>())];
        }
    }

    if(actions.count == 0)
    {
        // No actions — just dismiss immediately
        json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
        [self sendMessage:response];
        return;
    }

    // Post notification with callback for LSPBridge to show interactive toast
    [[NSNotificationCenter defaultCenter] postNotificationName:LSPShowMessageRequestNotification
        object:self
        userInfo:@{
            @"type": @(type),
            @"message": to_ns(message),
            @"actions": actions,           // full action objects for response
            @"actionTitles": actionTitles,  // display titles for UI
            @"requestId": @(requestId)
        }];
}
```

LSPBridge observes `LSPShowMessageRequestNotification` and calls `OakNotificationManager.showInteractive`. The callback sends the response back via a new `LSPClient` method:

```objc
- (void)respondToShowMessageRequest:(int)requestId action:(NSDictionary*)action
{
    json result = action ? [self convertToJSON:action] : json(nullptr);
    json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", result}};
    [self sendMessage:response];
}
```

LSPBridge gets the LSPClient from `note.object` and calls this with the full action dictionary (preserving custom properties like `id`). The UI shows `actionTitles` for display; when user clicks, LSPBridge looks up the corresponding full action object from the `actions` array and passes it back.

### 6. `window/showDocument` — No UI

#### LSP Request
```json
{
  "uri": "file:///path/to/file.php",
  "external": false,       // true = open in browser
  "takeFocus": true,
  "selection": {"start": {"line": 10, "character": 0}, "end": {"line": 10, "character": 0}}
}
```

#### LSP Response
```json
{"success": true}
```

#### Implementation

Named branch in `handleMessage`:

```objc
else if(method == "window/showDocument")
{
    NSString* uri = to_ns(msg["params"]["uri"].get<std::string>());
    bool external = msg["params"].value("external", false);
    bool takeFocus = msg["params"].value("takeFocus", true);

    bool success = false;
    NSURL* url = [NSURL URLWithString:uri];

    if(external || !url.isFileURL)
    {
        success = [[NSWorkspace sharedWorkspace] openURL:url];
    }
    else
    {
        // Open file in TextMate via delegate
        if([_delegate respondsToSelector:@selector(lspClient:didRequestShowDocument:takeFocus:)])
        {
            [_delegate lspClient:self didRequestShowDocument:url.path takeFocus:takeFocus];
            success = true;
        }
    }

    json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", {{"success", success}}}};
    [self sendMessage:response];
}
```

The delegate method opens the document in TextMate. Selection handling (jumping to a specific line) can be deferred — the basic "open file" functionality covers the main use case.

### 7. New Notifications and Delegate Methods

#### LSPClient.h additions
```objc
extern NSString* const LSPShowMessageRequestNotification;

@protocol LSPClientDelegate
// ... existing ...
- (void)lspClient:(LSPClient*)client didRequestShowDocument:(NSString*)path takeFocus:(BOOL)takeFocus;
@end
```

#### LSPClient.mm additions
```objc
- (void)respondToShowMessageRequest:(int)requestId action:(NSString*)action;
```

### 8. Interactive Toast Queue

When multiple `showMessageRequest` arrive simultaneously (rare but possible):
- Store pending requests in an `NSMutableArray` on `ToastViewModel`
- Show one at a time
- When the current interactive toast is resolved, show the next queued one
- Non-interactive toasts (regular `showMessage`) still replace each other as before

### 9. Window Management

The notification window currently uses `ignoresMouseEvents = true` for non-interactive toasts. When showing an interactive toast:
- Set `ignoresMouseEvents = false` so buttons are clickable
- The window is already `level: .floating` and `nonactivatingPanel` — the parent window stays active
- Set `ignoresMouseEvents` back to `true` after dismissal

This is already partially implemented — the `model.$currentToast` sink toggles `ignoresMouseEvents`.

## Files to Modify

**OakSwiftUI (Swift):**
- `Notifications/Toast.swift` or `ToastViewModel.swift` — add `actions` and `onAction` to Toast, add `showInteractive` method, add queue for interactive toasts
- `Notifications/NotificationView.swift` — render action buttons, Escape to dismiss
- `Notifications/OakNotificationManager.swift` — add `showInteractive(message:type:actions:callback:)` bridge method

**lsp framework (ObjC++):**
- `LSPClient.h` — add `LSPShowMessageRequestNotification`, `respondToShowMessageRequest:action:`, `didRequestShowDocument:takeFocus:` delegate method
- `LSPClient.mm` — handle `window/showMessageRequest` (extract actions, post notification) and `window/showDocument` (open file/URL)

**TextMate app (ObjC++):**
- `LSPBridge.mm` — observe `LSPShowMessageRequestNotification`, call `OakNotificationManager.showInteractive`, send response back via `respondToShowMessageRequest:action:`

## Known Limitations

- No `selection` handling in `showDocument` (deferred — opens file but doesn't jump to line)
- Interactive toast doesn't steal keyboard focus from the editor — user must click. This is intentional to avoid interrupting typing.
- Multiple simultaneous message requests are queued, not shown in parallel

## Testing

- Manual: configure eslint with `lspCommand` and verify "Allow validation?" prompt appears with buttons
- Manual: test with Intelephense — check if any showMessageRequest is sent on startup
- Manual: test `window/showDocument` with rust-analyzer or a custom LSP server
- Manual: dismiss interactive toast via Escape — verify `null` response sent
- Manual: click action button — verify correct `{title: "..."}` response sent
- Manual: multiple rapid showMessageRequest — verify queuing works
