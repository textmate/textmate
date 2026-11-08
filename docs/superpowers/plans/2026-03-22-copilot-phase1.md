# Copilot Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Native Copilot integration into TextMate — Opt+Esc triggers inline completion from copilot-language-server, single result inserts immediately, multiple results show in existing popup.

**Architecture:** CopilotManager singleton wraps an LSPClient configured for the copilot-language-server native arm64 binary. It adds Copilot-specific protocol (auth, setEditorInfo, inlineCompletion, telemetry) on top of the existing JSON-RPC infrastructure. OakTextView gets a new `lspCopilotComplete:` action. Status bar gets a Copilot icon.

**Tech Stack:** Objective-C++, SwiftUI (OakSwiftUI), nlohmann/json, JSON-RPC 2.0, LSP protocol

**Spec:** `docs/superpowers/specs/2026-03-22-copilot-integration-design.md`

---

## File Map

### New Files

| File | Responsibility |
|------|---------------|
| `Frameworks/lsp/src/CopilotManager.h` | Singleton interface: init, auth, completions, doc sync, telemetry |
| `Frameworks/lsp/src/CopilotManager.mm` | Implementation: wraps LSPClient, auto-detects binary, manages lifecycle |
| `Frameworks/OakSwiftUI/Sources/OakSwiftUI/Bridge/CopilotAuthPanel.swift` | Device flow sign-in floating panel (SwiftUI) |

### Modified Files

| File | Changes |
|------|---------|
| `Frameworks/lsp/src/LSPClient.h` | Add `lspClientDidInitialize:` and `handleServerRequest:params:` to delegate protocol |
| `Frameworks/lsp/src/LSPClient.mm` | Post-init delegate callback after `initialized`; route unknown server requests to delegate |
| `Frameworks/OakTextView/src/OakDocumentView.mm` | Add parallel CopilotManager calls at document lifecycle points (lines 330, 907, 912, 917) |
| `Frameworks/OakTextView/src/OakTextView.mm` | Add `lspCopilotComplete:` action |
| `Frameworks/OakTextView/src/OTVStatusBar.h` | Add `setCopilotStatus:` method |
| `Frameworks/OakTextView/src/OTVStatusBar.mm` | Add Copilot icon button with state management |
| `Applications/TextMate/resources/KeyBindings.dict` | Add `"~\033" = "lspCopilotComplete:"` |

### No CMakeLists Changes Needed

The lsp framework uses `file(GLOB _src src/*.mm src/*.cc)` — new `.mm` files in `src/` are auto-discovered.

---

## Reference: Key Existing APIs

```objc
// LSPClient — generic request/notification (already exist)
- (int)sendRequest:(NSString*)method params:(json)params callback:(void(^)(id))callback;
- (void)sendNotification:(NSString*)method params:(json)params;

// OakCompletionPopup — show with pre-built items
@objc public func show(in parentView: NSView, at point: NSPoint, items: [OakCompletionItem])

// OakCompletionItem — data model
@objc public init(label: String, insertText: String?, detail: String, kind: Int)
@objc public var icon: NSImage?
@objc public var originalItem: NSDictionary?

// OakNotificationManager — toasts
[OakNotificationManager.shared showWithMessage:@"text" type:N];
// type: 1=error, 2=warning, 3=info, 4=success

// Settings
settings_t settings = settings_for_path(filePath, fileType, directory);
std::string val = settings.get("copilotCommand", "");
```

---

### Task 1: LSPClient Delegate Extensions

Add post-initialization hook and generic server request routing to LSPClient.

**Files:**
- Modify: `Frameworks/lsp/src/LSPClient.h:8-13` (delegate protocol)
- Modify: `Frameworks/lsp/src/LSPClient.mm:450-471` (initialize response handler)
- Modify: `Frameworks/lsp/src/LSPClient.mm:349-420` (handleMessage server request dispatch)

- [ ] **Step 1: Add delegate methods to LSPClient.h**

In the `LSPClientDelegate` protocol, add two optional methods:

```objc
@optional
- (void)lspClientDidTerminate:(LSPClient*)client;
- (void)lspClient:(LSPClient*)client didReceiveApplyEditRequest:(NSDictionary*)workspaceEdit requestId:(int)requestId;
- (void)lspClientDidInitialize:(LSPClient*)client;  // NEW
- (id)lspClient:(LSPClient*)client handleServerRequest:(NSString*)method params:(NSDictionary*)params;  // NEW
```

- [ ] **Step 2: Add post-init callback in LSPClient.mm**

In the initialize response handler (after line ~455 where `[self sendNotification:@"initialized" params:json::object()]` is called), add:

```objc
[self sendNotification:@"initialized" params:json::object()];

// Post-initialization hook for delegates (e.g., CopilotManager sends setEditorInfo)
if([_delegate respondsToSelector:@selector(lspClientDidInitialize:)])
	[_delegate lspClientDidInitialize:self];
```

- [ ] **Step 3: Route unknown server requests to delegate in handleMessage:**

In `handleMessage:`, in the server request branch where `msg.contains("id")` (the else clause that currently returns empty result for unknown methods), replace with:

```objc
else
{
	// Route to delegate for handling
	if([_delegate respondsToSelector:@selector(lspClient:handleServerRequest:params:)])
	{
		NSDictionary* params = msg.contains("params") ? [self convertJSON:msg["params"]] : @{};
		id result = [_delegate lspClient:self handleServerRequest:to_ns(method) params:params];
		json response = {
			{"jsonrpc", "2.0"},
			{"id",      requestId},
			{"result",  result ? [self convertToJSON:result] : json::object()}
		};
		[self sendMessage:response];
	}
	else
	{
		json response = {
			{"jsonrpc", "2.0"},
			{"id",      requestId},
			{"result",  json::object()}
		};
		[self sendMessage:response];
	}
}
```

- [ ] **Step 4: Build and verify**

Run: `make`
Expected: Clean build, no warnings. Existing LSP functionality unaffected (LSPManager doesn't implement the new optional delegate methods, so behavior is identical).

- [ ] **Step 5: Commit**

```
git add Frameworks/lsp/src/LSPClient.h Frameworks/lsp/src/LSPClient.mm
git commit -m "Add post-init hook and generic server request routing to LSPClient delegate"
```

---

### Task 2: CopilotManager — Binary Detection & Lifecycle

Create the CopilotManager singleton with server auto-detection and LSPClient lifecycle.

**Files:**
- Create: `Frameworks/lsp/src/CopilotManager.h`
- Create: `Frameworks/lsp/src/CopilotManager.mm`

- [ ] **Step 1: Create CopilotManager.h**

```objc
#import <Foundation/Foundation.h>

@class OakDocument;
@class OakCompletionItem;

extern NSNotificationName const CopilotStatusDidChangeNotification;
extern NSNotificationName const CopilotLogNotification;

typedef NS_ENUM(NSInteger, CopilotStatus) {
	CopilotStatusDisabled,
	CopilotStatusConnecting,
	CopilotStatusReady,
	CopilotStatusAuthRequired,
	CopilotStatusError,
};

@interface CopilotManager : NSObject
@property (nonatomic, readonly) CopilotStatus status;
@property (nonatomic, readonly) NSString* username;

+ (instancetype)sharedManager;

- (void)documentDidOpen:(OakDocument*)document;
- (void)documentDidChange:(OakDocument*)document;
- (void)documentDidSave:(OakDocument*)document;
- (void)documentWillClose:(OakDocument*)document;
- (void)documentDidFocus:(OakDocument*)document;

- (void)requestCompletionForDocument:(OakDocument*)document
                                line:(NSUInteger)line
                           character:(NSUInteger)character
                          completion:(void(^)(NSArray<NSDictionary*>* items))callback;

- (void)sendAcceptanceTelemetry:(NSDictionary*)command;
- (void)sendDidShowCompletion:(NSDictionary*)item;

- (void)signIn;
- (void)shutdown;
@end
```

- [ ] **Step 2: Create CopilotManager.mm — binary detection and singleton**

```objc
#import "CopilotManager.h"
#import "LSPClient.h"
#import <OakFoundation/OakFoundation.h>
#import <document/OakDocument.h>
#import <settings/settings.h>
#import <text/utf8.h>
#import <ns/ns.h>
#if HAVE_OAK_SWIFTUI
#import <OakSwiftUI/OakSwiftUI-Swift.h>
#endif

NSNotificationName const CopilotStatusDidChangeNotification = @"CopilotStatusDidChangeNotification";
NSNotificationName const CopilotLogNotification = @"CopilotLogNotification";

static NSString* const kCopilotBinarySubpath = @"node_modules/@github/copilot-darwin-arm64/copilot";

@interface CopilotManager () <LSPClientDelegate>
{
	LSPClient* _client;
	NSMutableDictionary<NSString*, NSNumber*>* _documentVersions;
	NSMutableSet<NSString*>* _openURIs;
	NSMutableDictionary<NSString*, NSTimer*>* _changeTimers;
	NSInteger _restartCount;
}
@end

@implementation CopilotManager

+ (instancetype)sharedManager
{
	static CopilotManager* instance;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{ instance = [CopilotManager new]; });
	return instance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_documentVersions = [NSMutableDictionary new];
		_openURIs = [NSMutableSet new];
		_changeTimers = [NSMutableDictionary new];
		_status = CopilotStatusDisabled;
		_restartCount = 0;
	}
	return self;
}

#pragma mark - Binary Detection

- (NSString*)detectBinaryPath
{
	NSArray<NSString*>* searchPaths = @[
		[@"/opt/homebrew/lib/node_modules/@github/copilot/" stringByAppendingString:kCopilotBinarySubpath],
		[NSString stringWithFormat:@"%@/.config/yarn/global/%@", NSHomeDirectory(), kCopilotBinarySubpath],
		[NSString stringWithFormat:@"/usr/local/lib/node_modules/@github/copilot/%@", kCopilotBinarySubpath],
	];

	for(NSString* path in searchPaths)
	{
		if([[NSFileManager defaultManager] isExecutableFileAtPath:path])
		{
			[self log:[NSString stringWithFormat:@"Found binary at %@", path]];
			return path;
		}
	}
	return nil;
}

- (NSString*)binaryPathForDocument:(OakDocument*)document
{
	if(!document.path)
		return [self detectBinaryPath];

	settings_t settings = settings_for_path(to_s(document.path), to_s(document.fileType), to_s(document.directory));
	std::string cmd = settings.get("copilotCommand", "");
	if(!cmd.empty())
	{
		NSString* path = to_ns(cmd);
		if([[NSFileManager defaultManager] isExecutableFileAtPath:path])
			return path;
		[self log:[NSString stringWithFormat:@"copilotCommand path not found: %@", path]];
	}

	bool enabled = settings.get("copilotEnabled", true);
	if(!enabled)
		return nil;

	return [self detectBinaryPath];
}

#pragma mark - Client Lifecycle

- (void)ensureClientForDocument:(OakDocument*)document
{
	if(_client && _client.running)
		return;

	NSString* binaryPath = [self binaryPathForDocument:document];
	if(!binaryPath)
	{
		if(_status != CopilotStatusDisabled)
		{
			_status = CopilotStatusDisabled;
			[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
		}
		return;
	}

	_status = CopilotStatusConnecting;
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];

	NSString* workDir = document.directory ?: NSHomeDirectory();
	_client = [[LSPClient alloc] initWithCommand:binaryPath
	                                   arguments:@[@"--stdio"]
	                            workingDirectory:workDir
	                                 initOptions:nil];
	_client.delegate = self;
	[self log:[NSString stringWithFormat:@"Started server: %@ --stdio", binaryPath]];
}

- (void)shutdown
{
	[self log:@"Shutting down"];
	_client = nil;
	_status = CopilotStatusDisabled;
	[_documentVersions removeAllObjects];
	[_openURIs removeAllObjects];
	for(NSTimer* timer in _changeTimers.allValues)
		[timer invalidate];
	[_changeTimers removeAllObjects];
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
}

#pragma mark - Logging

- (void)log:(NSString*)message
{
	NSLog(@"[Copilot] %@", message);
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotLogNotification
	                                                  object:self
	                                                userInfo:@{@"message": message}];
}

@end
```

- [ ] **Step 3: Build and verify**

Run: `make`
Expected: Clean build. CopilotManager compiles and links with lsp framework (auto-discovered by GLOB).

- [ ] **Step 4: Commit**

```
git add Frameworks/lsp/src/CopilotManager.h Frameworks/lsp/src/CopilotManager.mm
git commit -m "Add CopilotManager skeleton with binary detection and client lifecycle"
```

---

### Task 3: CopilotManager — Initialization Protocol & Auth

Add the Copilot-specific handshake (setEditorInfo), auth check, and sign-in flow.

**Files:**
- Modify: `Frameworks/lsp/src/CopilotManager.mm`

- [ ] **Step 1: Implement LSPClientDelegate methods**

Add to CopilotManager.mm:

```objc
#pragma mark - LSPClientDelegate

- (void)lspClientDidInitialize:(LSPClient*)client
{
	[self log:@"Server initialized, sending setEditorInfo"];

	json editorInfo = {
		{"editorInfo", {{"name", "TextMate"}, {"version", "2.1"}}},
		{"editorPluginInfo", {{"name", "textmate-copilot"}, {"version", "1.0"}}}
	};
	[_client sendRequest:@"setEditorInfo" params:editorInfo callback:^(id response) {
		[self log:@"setEditorInfo acknowledged"];
		[self checkAuthStatus];
	}];
}

- (void)lspClientDidTerminate:(LSPClient*)client
{
	[self log:@"Server terminated"];
	_client = nil;
	[_openURIs removeAllObjects];
	[_documentVersions removeAllObjects];
	for(NSTimer* t in _changeTimers.allValues)
		[t invalidate];
	[_changeTimers removeAllObjects];
	_status = CopilotStatusError;
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];

	if(++_restartCount <= 3)
	{
		[self log:[NSString stringWithFormat:@"Scheduling restart (%ld/3)", (long)_restartCount]];
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC), dispatch_get_main_queue(), ^{
			// Re-open documents on next ensureClient call
		});
	}
	else
	{
		[self log:@"Max restarts exceeded, staying in error state"];
	}
}

- (void)lspClient:(LSPClient*)client didReceiveDiagnostics:(NSArray<NSDictionary*>*)diagnostics forDocumentURI:(NSString*)uri
{
	// Copilot server does not send diagnostics — ignore
}

- (id)lspClient:(LSPClient*)client handleServerRequest:(NSString*)method params:(NSDictionary*)params
{
	if([method isEqualToString:@"workspace/configuration"])
	{
		[self log:@"Responding to workspace/configuration"];
		// Return array of config objects matching requested sections
		NSArray* items = params[@"items"];
		NSMutableArray* results = [NSMutableArray new];
		for(NSDictionary* item in items)
		{
			NSString* section = item[@"section"];
			if([section hasPrefix:@"github.copilot"])
				[results addObject:@{@"enable": @YES}];
			else
				[results addObject:@{}];
		}
		return results;
	}
	return nil;
}
```

- [ ] **Step 2: Add auth check and sign-in**

```objc
#pragma mark - Authentication

- (void)checkAuthStatus
{
	[self log:@"Checking auth status"];
	json params = {
		{"command", "github.copilot.checkStatus"},
		{"arguments", json::array()}
	};
	[_client sendRequest:@"workspace/executeCommand" params:params callback:^(id response) {
		NSDictionary* result = response;
		NSString* status = result[@"status"];
		[self log:[NSString stringWithFormat:@"Auth status: %@", status ?: @"(nil)"]];

		if([status isEqualToString:@"OK"] || [status isEqualToString:@"AlreadySignedIn"])
		{
			self->_username = result[@"user"];
			self->_status = CopilotStatusReady;
			self->_restartCount = 0;
			[OakNotificationManager.shared showWithMessage:
				[NSString stringWithFormat:@"Copilot: Connected as %@", self->_username ?: @"unknown"]
				type:4];
		}
		else
		{
			self->_status = CopilotStatusAuthRequired;
			[OakNotificationManager.shared showWithMessage:@"Copilot: Authentication required" type:2];
		}
		[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
	}];
}

- (void)signIn
{
	[self log:@"Initiating sign-in"];
	json params = {
		{"command", "github.copilot.signIn"},
		{"arguments", json::array()}
	};
	[_client sendRequest:@"workspace/executeCommand" params:params callback:^(id response) {
		NSDictionary* result = response;
		NSString* userCode = result[@"userCode"];
		NSString* verificationUri = result[@"verificationUri"];

		if(userCode && verificationUri)
		{
			[self log:[NSString stringWithFormat:@"Sign-in code: %@", userCode]];
			dispatch_async(dispatch_get_main_queue(), ^{
				[self showAuthPanelWithCode:userCode uri:verificationUri];
			});
		}
		else if([result[@"status"] isEqualToString:@"AlreadySignedIn"])
		{
			[self checkAuthStatus];
		}
	}];
}

- (void)showAuthPanelWithCode:(NSString*)code uri:(NSString*)uri
{
	// Phase 1: simple alert. CopilotAuthPanel (SwiftUI) comes later.
	NSAlert* alert = [NSAlert new];
	alert.messageText = @"GitHub Copilot";
	alert.informativeText = [NSString stringWithFormat:
		@"Enter this code at github.com/login/device:\n\n%@\n\nThe code has been copied to your clipboard.", code];
	[alert addButtonWithTitle:@"Open Browser"];
	[alert addButtonWithTitle:@"Cancel"];

	[[NSPasteboard generalPasteboard] clearContents];
	[[NSPasteboard generalPasteboard] setString:code forType:NSPasteboardTypeString];

	if([alert runModal] == NSAlertFirstButtonReturn)
	{
		[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:uri]];
		// Poll for completion after user returns
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC), dispatch_get_main_queue(), ^{
			[self checkAuthStatus];
		});
	}
}
```

- [ ] **Step 3: Build and verify**

Run: `make`
Expected: Clean build. Add `#import <OakSwiftUI/OakSwiftUI-Swift.h>` at top of CopilotManager.mm if OakNotificationManager is used (guarded by `#if HAVE_OAK_SWIFTUI`).

- [ ] **Step 4: Commit**

```
git add Frameworks/lsp/src/CopilotManager.mm
git commit -m "Add Copilot initialization protocol, auth check, and sign-in flow"
```

---

### Task 4: CopilotManager — Document Sync

Wire CopilotManager into the document lifecycle.

**Files:**
- Modify: `Frameworks/lsp/src/CopilotManager.mm`
- Modify: `Frameworks/OakTextView/src/OakDocumentView.mm:330,907,912,917`

- [ ] **Step 1: Implement document lifecycle in CopilotManager.mm**

```objc
#pragma mark - Document Sync

static NSString* uriForDocument(OakDocument* doc)
{
	if(!doc.path)
		return nil;
	return [NSString stringWithFormat:@"file://%@", doc.path];
}

static NSString* languageIdForDocument(OakDocument* doc)
{
	// Reuse the same mapping as LSPManager
	NSString* ext = doc.path.pathExtension.lowercaseString;
	static NSDictionary* map;
	static dispatch_once_t once;
	dispatch_once(&once, ^{
		map = @{
			@"py": @"python", @"js": @"javascript", @"ts": @"typescript",
			@"jsx": @"javascriptreact", @"tsx": @"typescriptreact",
			@"rb": @"ruby", @"go": @"go", @"rs": @"rust",
			@"c": @"c", @"cc": @"cpp", @"cpp": @"cpp", @"h": @"c",
			@"m": @"objective-c", @"mm": @"objective-cpp",
			@"swift": @"swift", @"java": @"java", @"php": @"php",
			@"sh": @"shellscript", @"bash": @"shellscript",
			@"json": @"json", @"yaml": @"yaml", @"yml": @"yaml",
			@"xml": @"xml", @"html": @"html", @"css": @"css",
			@"md": @"markdown", @"sql": @"sql", @"lua": @"lua",
		};
	});
	return map[ext] ?: @"plaintext";
}

- (void)documentDidOpen:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri)
		return;

	[self ensureClientForDocument:document];
	if(!_client || !_client.initialized)
		return;

	if([_openURIs containsObject:uri])
		return;

	[_openURIs addObject:uri];
	_documentVersions[uri] = @1;

	NSString* text = document.content ?: @"";
	NSString* langId = languageIdForDocument(document);

	json params = {
		{"textDocument", {
			{"uri", uri.UTF8String},
			{"languageId", langId.UTF8String},
			{"version", 1},
			{"text", text.UTF8String}
		}}
	};
	[_client sendNotification:@"textDocument/didOpen" params:params];
	[self log:[NSString stringWithFormat:@"didOpen %@ (%@)", uri.lastPathComponent, langId]];
}

- (void)documentDidChange:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || ![_openURIs containsObject:uri])
		return;

	[_changeTimers[uri] invalidate];
	__weak CopilotManager* weakSelf = self;
	_changeTimers[uri] = [NSTimer scheduledTimerWithTimeInterval:0.3 repeats:NO block:^(NSTimer* timer) {
		[weakSelf flushChangeForDocument:document uri:uri];
	}];
}

- (void)flushChangeForDocument:(OakDocument*)document uri:(NSString*)uri
{
	if(!_client || !_client.initialized)
		return;

	int version = [_documentVersions[uri] intValue] + 1;
	_documentVersions[uri] = @(version);

	NSString* text = document.content ?: @"";
	json params = {
		{"textDocument", {{"uri", uri.UTF8String}, {"version", version}}},
		{"contentChanges", json::array({
			{{"text", text.UTF8String}}
		})}
	};
	[_client sendNotification:@"textDocument/didChange" params:params];
}

- (void)documentDidSave:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || ![_openURIs containsObject:uri])
		return;

	// Flush any pending change first
	[_changeTimers[uri] invalidate];
	[_changeTimers removeObjectForKey:uri];
	[self flushChangeForDocument:document uri:uri];

	json params = {{"textDocument", {{"uri", uri.UTF8String}}}};
	[_client sendNotification:@"textDocument/didSave" params:params];
}

- (void)documentWillClose:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || ![_openURIs containsObject:uri])
		return;

	[_changeTimers[uri] invalidate];
	[_changeTimers removeObjectForKey:uri];
	[_openURIs removeObject:uri];
	[_documentVersions removeObjectForKey:uri];

	json params = {{"textDocument", {{"uri", uri.UTF8String}}}};
	[_client sendNotification:@"textDocument/didClose" params:params];
	[self log:[NSString stringWithFormat:@"didClose %@", uri.lastPathComponent]];
}

- (void)documentDidFocus:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || !_client || !_client.initialized)
		return;

	json params = {{"textDocument", {{"uri", uri.UTF8String}}}};
	[_client sendNotification:@"textDocument/didFocus" params:params];
}
```

- [ ] **Step 2: Wire into OakDocumentView.mm**

Add `#import <lsp/CopilotManager.h>` at top. Then add parallel calls at each lifecycle point:

**Line ~330 (setDocument:, after LSPManager documentDidOpen):**
```objc
[LSPManager.sharedManager documentDidOpen:aDocument];
[[CopilotManager sharedManager] documentDidOpen:aDocument];  // NEW
```

**Line ~907 (documentContentDidChange:):**
```objc
- (void)documentContentDidChange:(NSNotification*)notification
{
	[LSPManager.sharedManager documentDidChange:notification.object];
	[[CopilotManager sharedManager] documentDidChange:notification.object];  // NEW
}
```

**Line ~912 (documentDidSave:):**
```objc
- (void)documentDidSave:(NSNotification*)notification
{
	[LSPManager.sharedManager documentDidSave:notification.object];
	[[CopilotManager sharedManager] documentDidSave:notification.object];  // NEW
}
```

**Line ~917 (documentWillClose:):**
```objc
- (void)documentWillClose:(NSNotification*)notification
{
	[LSPManager.sharedManager documentWillClose:notification.object];
	[[CopilotManager sharedManager] documentWillClose:notification.object];  // NEW
}
```

Also add `documentDidFocus:` call in setDocument after the open call:
```objc
[[CopilotManager sharedManager] documentDidOpen:aDocument];
[[CopilotManager sharedManager] documentDidFocus:aDocument];
```

- [ ] **Step 3: Build and verify**

Run: `make`
Expected: Clean build. Opening a file should trigger Copilot server launch (check Console.app for `[Copilot]` logs).

- [ ] **Step 4: Commit**

```
git add Frameworks/lsp/src/CopilotManager.mm Frameworks/OakTextView/src/OakDocumentView.mm
git commit -m "Wire CopilotManager document sync into OakDocumentView lifecycle"
```

---

### Task 5: CopilotManager — Inline Completion Request

Implement `requestCompletionForDocument:` and telemetry methods.

**Files:**
- Modify: `Frameworks/lsp/src/CopilotManager.mm`

- [ ] **Step 1: Add completion request and telemetry methods**

```objc
#pragma mark - Completions

- (void)requestCompletionForDocument:(OakDocument*)document
                                line:(NSUInteger)line
                           character:(NSUInteger)character
                          completion:(void(^)(NSArray<NSDictionary*>* items))callback
{
	NSString* uri = uriForDocument(document);
	if(!uri || !_client || !_client.initialized)
	{
		[self log:@"Cannot request completion — client not ready"];
		if(callback) callback(nil);
		return;
	}

	// Flush any pending change
	[_changeTimers[uri] invalidate];
	[_changeTimers removeObjectForKey:uri];
	[self flushChangeForDocument:document uri:uri];

	int version = [_documentVersions[uri] intValue];

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}, {"version", version}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}},
		{"context", {{"triggerKind", 1}}}  // 1 = Invoked (explicit trigger)
	};

	[self log:[NSString stringWithFormat:@"Requesting completion at %lu:%lu", (unsigned long)line, (unsigned long)character]];

	[_client sendRequest:@"textDocument/inlineCompletion" params:params callback:^(id response) {
		if(!response)
		{
			[self log:@"Completion response: nil (error or cancelled)"];
			if(callback) callback(nil);
			return;
		}

		// Response shape: {items: [{insertText, range, command}]}
		NSArray* items = nil;
		if([response isKindOfClass:[NSDictionary class]])
			items = response[@"items"];
		else if([response isKindOfClass:[NSArray class]])
			items = response;

		[self log:[NSString stringWithFormat:@"Completion response: %lu items", (unsigned long)items.count]];
		if(callback) callback(items);
	}];
}

#pragma mark - Telemetry

- (void)sendDidShowCompletion:(NSDictionary*)item
{
	if(!_client || !_client.initialized)
		return;

	// The item's command contains the UUID for telemetry
	NSDictionary* command = item[@"command"];
	if(!command)
		return;

	json params = [_client convertToJSON:@{
		@"completionItem": item
	}];
	[_client sendNotification:@"textDocument/didShowCompletion" params:params];
	[self log:@"Sent didShowCompletion"];
}

- (void)sendAcceptanceTelemetry:(NSDictionary*)command
{
	if(!_client || !_client.initialized || !command)
		return;

	json params = {
		{"command", [command[@"command"] UTF8String]},
		{"arguments", [_client convertToJSON:command[@"arguments"] ?: @[]]}
	};
	[_client sendRequest:@"workspace/executeCommand" params:params callback:^(id response) {
		[self log:@"Acceptance telemetry sent"];
	}];
}
```

Note: `convertToJSON:` is a private method on LSPClient. Either make it public (add to .h) or reimplement the conversion in CopilotManager. The simpler approach is to expose it:

Add to LSPClient.h:
```objc
- (id)convertJSON:(json const&)value;   // json → NSObject
- (json)convertToJSON:(id)obj;           // NSObject → json
```

These are `json` (nlohmann) typed — they need the nlohmann header. Since CopilotManager.mm is in the same framework and already has access, just declare them in the header.

- [ ] **Step 2: Build and verify**

Run: `make`
Expected: Clean build.

- [ ] **Step 3: Commit**

```
git add Frameworks/lsp/src/CopilotManager.mm Frameworks/lsp/src/LSPClient.h
git commit -m "Add inline completion request and telemetry to CopilotManager"
```

---

### Task 6: OakTextView — lspCopilotComplete: Action

Add the Opt+Esc keybinding and the action that triggers Copilot completion.

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:5981+`
- Modify: `Applications/TextMate/resources/KeyBindings.dict`

- [ ] **Step 1: Add keybinding**

In `KeyBindings.dict`, add after the existing `"~\t"` line:

```
"~\033" = "lspCopilotComplete:";
```

(`\033` = Escape, so `~\033` = Opt+Esc)

- [ ] **Step 2: Add lspCopilotComplete: action to OakTextView.mm**

Add `#import <lsp/CopilotManager.h>` at top. Then add the action method near `lspComplete:`:

```objc
- (void)lspCopilotComplete:(id)sender
{
	if(!documentView)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	CopilotManager* copilot = [CopilotManager sharedManager];
	if(copilot.status != CopilotStatusReady)
	{
		if(copilot.status == CopilotStatusAuthRequired)
			[copilot signIn];
		else if(copilot.status == CopilotStatusDisabled)
			[OakNotificationManager.shared showWithMessage:@"Copilot: Server not found" type:2];
		else
			[OakNotificationManager.shared showWithMessage:@"Copilot: Not ready" type:3];
		return;
	}

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	__weak OakTextView* weakSelf = self;
	[copilot requestCompletionForDocument:doc
		line:pos.line
		character:pos.column
		completion:^(NSArray<NSDictionary*>* items) {
			OakTextView* strongSelf = weakSelf;
			if(!strongSelf || !strongSelf->documentView)
				return;

			if(!items || items.count == 0)
			{
				[OakNotificationManager.shared showWithMessage:@"Copilot: No suggestions" type:3];
				return;
			}

			if(items.count == 1)
			{
				// Single result: insert immediately
				[strongSelf insertCopilotCompletion:items[0]];
			}
			else
			{
				// Multiple results: show in completion popup
				[strongSelf showCopilotCompletionPopup:items];
			}
		}];
}
```

- [ ] **Step 3: Add single-result insertion method**

```objc
- (void)insertCopilotCompletion:(NSDictionary*)item
{
	AUTO_REFRESH;

	NSString* insertText = item[@"insertText"];
	if(!insertText.length)
		return;

	NSDictionary* range = item[@"range"];
	if(range)
	{
		// Range-based replacement
		int startLine = [range[@"start"][@"line"] intValue];
		int startChar = [range[@"start"][@"character"] intValue];
		int endLine   = [range[@"end"][@"line"] intValue];
		int endChar   = [range[@"end"][@"character"] intValue];

		size_t from = documentView->convert(text::pos_t(startLine, startChar));
		size_t to   = documentView->convert(text::pos_t(endLine, endChar));
		documentView->set_ranges(ng::range_t(from, to));
	}

	documentView->insert(to_s(insertText));

	// Telemetry
	CopilotManager* copilot = [CopilotManager sharedManager];
	[copilot sendDidShowCompletion:item];
	[copilot sendAcceptanceTelemetry:item[@"command"]];
}
```

- [ ] **Step 4: Add multi-result popup method**

```objc
- (void)showCopilotCompletionPopup:(NSArray<NSDictionary*>*)items
{
#if HAVE_OAK_SWIFTUI
	if(!_lspTheme)
	{
		_lspTheme = [[OakThemeEnvironment alloc] init];
		NSFont* f = self.font ?: [NSFont userFixedPitchFontOfSize:12];
		[_lspTheme applyTheme:@{
			@"fontName": f.fontName,
			@"fontSize": @(f.pointSize),
			@"backgroundColor": [NSColor textBackgroundColor],
			@"foregroundColor": [NSColor textColor],
		}];
	}

	if(!_lspCompletionPopup)
	{
		_lspCompletionPopup = [[OakCompletionPopup alloc] initWithTheme:_lspTheme];
		_lspCompletionPopup.delegate = self;
	}

	// Build OakCompletionItem array
	NSMutableArray<OakCompletionItem*>* completionItems = [NSMutableArray new];
	for(NSDictionary* item in items)
	{
		NSString* insertText = item[@"insertText"] ?: @"";
		NSString* firstLine = [insertText componentsSeparatedByString:@"\n"].firstObject;
		NSArray* lines = [insertText componentsSeparatedByString:@"\n"];
		NSString* detail = lines.count > 1
			? [NSString stringWithFormat:@"Copilot · %lu lines", (unsigned long)lines.count]
			: @"Copilot";

		OakCompletionItem* ci = [[OakCompletionItem alloc] initWithLabel:firstLine
		                                                      insertText:insertText
		                                                          detail:detail
		                                                            kind:15]; // 15 = Snippet kind
		ci.originalItem = (NSDictionary*)item;
		// ci.icon = copilotIcon;  // TODO: add bundled Copilot icon
		[completionItems addObject:ci];
	}

	_lspInitialPrefixLength = 0;
	_lspFilterPrefix = @"";
	_copilotCompletionActive = YES;  // Flag to distinguish from LSP completions

	NSPoint caretPoint = [self positionForWindowUnderCaret];
	[_lspCompletionPopup show:self at:caretPoint items:completionItems];

	// Telemetry: shown
	CopilotManager* copilot = [CopilotManager sharedManager];
	for(NSDictionary* item in items)
		[copilot sendDidShowCompletion:item];
#endif
}
```

Add ivar `BOOL _copilotCompletionActive;` to OakTextView's ivar block.

- [ ] **Step 5: Modify completionPopup:didSelectItem: for Copilot items**

In the existing `completionPopup:didSelectItem:` method, add a branch for Copilot items:

```objc
- (void)completionPopup:(OakCompletionPopup*)popup didSelectItem:(OakCompletionItem*)item
{
	if(!documentView)
		return;

	AUTO_REFRESH;

	if(_copilotCompletionActive)
	{
		// Copilot: insert using range from originalItem
		[self insertCopilotCompletion:(NSDictionary*)item.originalItem];
		_copilotCompletionActive = NO;
		_lspFilterPrefix = nil;
		return;
	}

	// Existing LSP completion insertion logic (unchanged)
	size_t caret = documentView->ranges().last().last.index;
	// ... rest of existing code ...
}
```

Also reset the flag in `completionPopupDidDismiss:`:
```objc
- (void)completionPopupDidDismiss:(OakCompletionPopup*)popup
{
	_copilotCompletionActive = NO;
	_lspFilterPrefix = nil;
}
```

- [ ] **Step 6: Build and verify**

Run: `make`
Expected: Clean build. Opt+Esc should trigger Copilot completion (will show "Not ready" toast until server connects).

- [ ] **Step 7: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm Applications/TextMate/resources/KeyBindings.dict
git commit -m "Add lspCopilotComplete: action with Opt+Esc binding"
```

---

### Task 7: Status Bar — Copilot Icon

Add Copilot icon indicator to OTVStatusBar.

**Files:**
- Modify: `Frameworks/OakTextView/src/OTVStatusBar.h`
- Modify: `Frameworks/OakTextView/src/OTVStatusBar.mm`
- Modify: `Frameworks/OakTextView/src/OakDocumentView.mm`

- [ ] **Step 1: Add Copilot status method to OTVStatusBar.h**

```objc
- (void)setCopilotStatus:(NSInteger)status;  // CopilotStatus enum value
```

- [ ] **Step 2: Add Copilot icon button in OTVStatusBar.mm**

Add ivar/property:
```objc
@property (nonatomic) NSButton* copilotButton;
```

In init, create the button (after lspPopUp setup):
```objc
// Copilot status icon
self.copilotButton = [NSButton buttonWithImage:[NSImage imageWithSystemSymbolName:@"sparkle" accessibilityDescription:@"Copilot"] target:nil action:nil];
self.copilotButton.bordered = NO;
self.copilotButton.imagePosition = NSImageOnly;
[self.copilotButton.widthAnchor constraintEqualToConstant:20].active = YES;
self.copilotButton.toolTip = @"GitHub Copilot";
self.copilotButton.alphaValue = 0.3;  // Dimmed by default
[self addSubview:self.copilotButton];
```

Add layout constraints to place it in the status bar (after the LSP section).

- [ ] **Step 3: Implement setCopilotStatus:**

```objc
- (void)setCopilotStatus:(NSInteger)status
{
	switch(status)
	{
		case 0: // Disabled
			self.copilotButton.alphaValue = 0.3;
			self.copilotButton.contentTintColor = NSColor.secondaryLabelColor;
			self.copilotButton.toolTip = @"Copilot: Disabled";
			break;
		case 1: // Connecting
			self.copilotButton.alphaValue = 0.5;
			self.copilotButton.contentTintColor = NSColor.systemYellowColor;
			self.copilotButton.toolTip = @"Copilot: Connecting…";
			break;
		case 2: // Ready
			self.copilotButton.alphaValue = 1.0;
			self.copilotButton.contentTintColor = NSColor.systemGreenColor;
			self.copilotButton.toolTip = @"Copilot: Ready";
			break;
		case 3: // Auth required
			self.copilotButton.alphaValue = 0.7;
			self.copilotButton.contentTintColor = NSColor.systemOrangeColor;
			self.copilotButton.toolTip = @"Copilot: Sign-in required (click)";
			break;
		case 4: // Error
			self.copilotButton.alphaValue = 0.7;
			self.copilotButton.contentTintColor = NSColor.systemRedColor;
			self.copilotButton.toolTip = @"Copilot: Error";
			break;
	}
}
```

- [ ] **Step 4: Wire up status updates in OakDocumentView.mm**

Add observer for `CopilotStatusDidChangeNotification` in setDocument:, and update status bar:

```objc
[NSNotificationCenter.defaultCenter addObserver:self
    selector:@selector(copilotStatusDidChange:)
    name:CopilotStatusDidChangeNotification
    object:nil];
```

```objc
- (void)copilotStatusDidChange:(NSNotification*)notification
{
	[_statusBar setCopilotStatus:[CopilotManager sharedManager].status];
}
```

- [ ] **Step 5: Add click action to trigger sign-in**

Set button target/action in init:
```objc
self.copilotButton.target = self;
self.copilotButton.action = @selector(copilotButtonClicked:);
```

In OTVStatusBar or via delegate:
```objc
- (void)copilotButtonClicked:(id)sender
{
	CopilotManager* copilot = [CopilotManager sharedManager];
	if(copilot.status == CopilotStatusAuthRequired)
		[copilot signIn];
}
```

- [ ] **Step 6: Build and verify**

Run: `make`
Expected: Clean build. Status bar shows dimmed sparkle icon. Changes color on Copilot status changes.

- [ ] **Step 7: Commit**

```
git add Frameworks/OakTextView/src/OTVStatusBar.h Frameworks/OakTextView/src/OTVStatusBar.mm Frameworks/OakTextView/src/OakDocumentView.mm
git commit -m "Add Copilot status icon to OTVStatusBar"
```

---

### Task 8: Integration Test — End-to-End Manual Verification

Verify the full flow works with the real copilot-language-server.

**Files:** None (manual testing only)

- [ ] **Step 1: Build and run**

Run: `make run`

- [ ] **Step 2: Verify server launch**

Open any source file. Check Console.app for:
```
[Copilot] Found binary at /opt/homebrew/lib/node_modules/@github/copilot/...
[Copilot] Started server: ... --stdio
[Copilot] Server initialized, sending setEditorInfo
[Copilot] Auth status: OK
```

Expected: Toast "Copilot: Connected as tectiv3", status bar icon turns green.

- [ ] **Step 3: Test completion**

In a Python or JavaScript file, type some code and press Opt+Esc.
Expected: Either text inserted (single result) or popup shown (multiple results). Toast "No suggestions" if server returns empty.

- [ ] **Step 4: Test auth flow (if needed)**

If auth status is not OK, click the orange status bar icon.
Expected: Alert with user code, "Open Browser" opens github.com/login/device.

- [ ] **Step 5: Verify logging**

Check Console.app for `[Copilot]` prefix messages showing:
- didOpen notifications for opened files
- Completion request/response timing
- didShowCompletion telemetry
- Acceptance telemetry

- [ ] **Step 6: Commit any fixes**

```
git add -u
git commit -m "Fix integration issues found during manual testing"
```

---

## Implementation Notes

### Threading
LSPClient dispatches response callbacks on the main queue (via `dispatch_async(dispatch_get_main_queue(), ...)`  in `handleMessage:`). All CopilotManager callback code runs on main thread. Verified in LSPClient.mm.

### Column Offset
`text::pos_t.column` is a byte offset within the line. For ASCII text this matches LSP's UTF-16 character offset. For non-ASCII (emoji, CJK), a conversion is needed. The existing `lspComplete:` passes `pos.column` directly — same limitation. For Phase 1 this is acceptable; UTF-16 conversion can be added as a follow-up.

### Undo Grouping
`documentView->insert(...)` inside `AUTO_REFRESH` creates a single undo step. Multi-line Copilot insertions will undo atomically. Verified by existing LSP completion insertion pattern.

---

## Task Dependencies

```
Task 1 (LSPClient extensions)
  ↓
Task 2 (CopilotManager skeleton)
  ↓
Task 3 (Init protocol & auth)
  ↓
Task 4 (Document sync)     → Task 5 (Completions)
  ↓                              ↓
Task 7 (Status bar)         Task 6 (OakTextView action)
  ↓                              ↓
  └──────────────────────────────→ Task 8 (Integration test)
```
