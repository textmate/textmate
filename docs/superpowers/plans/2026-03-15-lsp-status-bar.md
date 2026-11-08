# LSP Status Bar Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an LSP status + diagnostic counts section to the OTVStatusBar, showing server state and error/warning/info counts with a click menu for controls.

**Architecture:** A new popup button in OTVStatusBar displays a colored status dot + diagnostic counts (e.g., "● ✕2 ▲1"). LSPManager exposes diagnostic counts and server state per-document via new API methods and posts notifications on changes. OakDocumentView bridges the data to the status bar.

**Tech Stack:** Objective-C++, AppKit (NSPopUpButton, NSMenu, NSAttributedString)

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `Frameworks/lsp/src/LSPManager.h` | Modify | Add notification constants, diagnostic counts API, server status/name, restart |
| `Frameworks/lsp/src/LSPManager.mm` | Modify | Implement counts from `_diagnosticsByURI`, server state, restart with deferred re-open, notification posting |
| `Frameworks/OakTextView/src/OTVStatusBar.h` | Modify | Add `@optional` delegate method, batch update method for LSP state |
| `Frameworks/OakTextView/src/OTVStatusBar.mm` | Modify | Add LSP popup button + divider ivar, layout, attributed string display, `setUsesItemFromMenu:NO` pattern |
| `Frameworks/OakTextView/src/OakDocumentView.mm` | Modify | Observe LSP notifications, filter by document, update status bar, handle menu actions |

No new files.

---

## Chunk 1: LSPManager diagnostic counts + server state API

### Task 1: Add diagnostic count and server state API to LSPManager

**Files:**
- Modify: `Frameworks/lsp/src/LSPManager.h`
- Modify: `Frameworks/lsp/src/LSPManager.mm`

- [ ] **Step 1: Add notification constants and method declarations to LSPManager.h**

Before `@interface`, add:

```objc
extern NSString* const LSPDiagnosticsDidChangeNotification;
extern NSString* const LSPServerStatusDidChangeNotification;
```

After `hasClientForDocument:`, add:

```objc
- (NSDictionary<NSString*, NSNumber*>*)diagnosticCountsForDocument:(OakDocument*)document;
- (NSString*)serverStatusForDocument:(OakDocument*)document;
- (NSString*)serverNameForDocument:(OakDocument*)document;
- (void)restartServerForDocument:(OakDocument*)document;
```

- [ ] **Step 2: Implement notification constants in LSPManager.mm**

Near the top:

```objc
NSString* const LSPDiagnosticsDidChangeNotification = @"LSPDiagnosticsDidChange";
NSString* const LSPServerStatusDidChangeNotification = @"LSPServerStatusDidChange";
```

- [ ] **Step 3: Implement diagnosticCountsForDocument:**

Counts only valid LSP severities (1=error, 2=warning, 3=info, 4=hint). Ignores nil/0 severity.

```objc
- (NSDictionary<NSString*, NSNumber*>*)diagnosticCountsForDocument:(OakDocument*)document
{
	NSUInteger errors = 0, warnings = 0, info = 0;
	NSString* path = document.path;
	if(path)
	{
		NSURL* fileURL = [NSURL fileURLWithPath:path];
		NSArray<NSDictionary*>* diags = _diagnosticsByURI[fileURL.absoluteString];
		for(NSDictionary* diag in diags)
		{
			switch([diag[@"severity"] intValue])
			{
				case 1:  errors++;   break;
				case 2:  warnings++; break;
				case 3:
				case 4:  info++;     break;
				default: break;
			}
		}
	}
	return @{ @"errors": @(errors), @"warnings": @(warnings), @"info": @(info) };
}
```

- [ ] **Step 4: Implement serverStatusForDocument: and serverNameForDocument:**

```objc
- (NSString*)serverStatusForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return nil;
	if(client.initialized)
		return @"running";
	if(client.running)
		return @"starting";
	return nil;
}

- (NSString*)serverNameForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return nil;

	NSString* path = document.path;
	if(!path)
		return nil;

	std::string filePath  = to_s(path);
	std::string fileType  = to_s(document.fileType);
	std::string directory = to_s(document.directory ?: [path stringByDeletingLastPathComponent]);

	settings_t settings = settings_for_path(filePath, fileType, directory);
	std::string lspCommand = settings.get("lspCommand", "");
	if(lspCommand.empty())
		return nil;

	std::vector<std::string> parts = path::unescape(lspCommand);
	if(parts.empty())
		return nil;

	// Return just the executable name
	return [[NSString stringWithCxxString:parts[0]] lastPathComponent];
}
```

- [ ] **Step 5: Implement restartServerForDocument:**

Uses `dispatch_async` to defer re-open until after `lspClientDidTerminate:` cleanup runs:

```objc
- (void)restartServerForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return;

	// Collect actual OakDocument objects before shutdown
	NSMutableArray<OakDocument*>* affectedDocs = [NSMutableArray new];
	for(NSUUID* docId in _documentClients)
	{
		if(_documentClients[docId] == client)
		{
			OakDocument* doc = [OakDocument documentWithIdentifier:docId];
			if(doc && doc.isLoaded)
				[affectedDocs addObject:doc];
		}
	}

	[client shutdown];

	// Defer re-open to next run loop so lspClientDidTerminate: cleanup completes first
	dispatch_async(dispatch_get_main_queue(), ^{
		for(OakDocument* doc in affectedDocs)
			[self documentDidOpen:doc];

		[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
	});
}
```

Note: Verify `[OakDocument documentWithIdentifier:]` exists. If not, build a map from `_openDocuments` UUIDs to OakDocument objects by iterating loaded documents.

- [ ] **Step 6: Post notifications on diagnostic and status changes**

In `lspClient:didReceiveDiagnostics:forDocumentURI:`, after setting marks on the document, add:

```objc
[NSNotificationCenter.defaultCenter postNotificationName:LSPDiagnosticsDidChangeNotification object:self userInfo:@{ @"uri": uri }];
```

In `lspClientDidTerminate:`, at the end:

```objc
[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
```

In `clientForDocument:`, after `_clients[root] = client;`:

```objc
[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
```

- [ ] **Step 7: Build and verify**

Run: `make`
Expected: Clean build, no new warnings.

- [ ] **Step 8: Commit**

```
git add Frameworks/lsp/src/LSPManager.h Frameworks/lsp/src/LSPManager.mm
git commit -m "Add LSP diagnostic counts, server status API, and restart to LSPManager"
```

---

## Chunk 2: Status bar UI — LSP popup button

### Task 2: Add LSP status popup to OTVStatusBar

**Files:**
- Modify: `Frameworks/OakTextView/src/OTVStatusBar.h`
- Modify: `Frameworks/OakTextView/src/OTVStatusBar.mm`

- [ ] **Step 1: Update OTVStatusBar.h — add delegate method and update method**

Add `@optional` section to delegate protocol:

```objc
@protocol OTVStatusBarDelegate <NSObject>
- (void)showBundleItemSelector:(NSPopUpButton*)popUpButton;
- (void)showSymbolSelector:(NSPopUpButton*)popUpButton;
@optional
- (void)showLSPStatusMenu:(NSPopUpButton*)popUpButton;
@end
```

Add to `@interface OTVStatusBar`:

```objc
- (void)setLspStatus:(NSString*)status errors:(NSUInteger)errors warnings:(NSUInteger)warnings info:(NSUInteger)info;
```

- [ ] **Step 2: Add LSP popup + divider ivars in OTVStatusBar.mm**

In `@interface OTVStatusBar ()`:

```objc
@property (nonatomic) NSPopUpButton* lspPopUp;
@property (nonatomic) NSView* lspDivider;
```

- [ ] **Step 3: Create LSP popup in initWithFrame:**

After `self.macroRecordingButton.toolTip = ...;`:

```objc
self.lspPopUp = OakCreateStatusBarPopUpButton(nil, @"LSP Status");
[[self.lspPopUp cell] setUsesItemFromMenu:NO];
self.lspPopUp.hidden = YES;
```

Create `lspDivider`:

```objc
NSView* dividerSix = OakCreateNSBoxSeparator();
self.lspDivider = dividerSix;
self.lspDivider.hidden = YES;
```

- [ ] **Step 4: Update layout constraints**

Add to views dictionary:

```objc
@"dividerSix": dividerSix,
@"lsp":        self.lspPopUp,
```

Replace the main horizontal constraint (line 146) with:

```objc
@"H:|-10-[line]-[selection(>=50,<=225)]-8-[dividerOne(==1)]-2-[grammar(>=125@400,>=50,<=225)]-5-[dividerTwo(==1)]-2-[tabSize]-4-[dividerThree(==1)]-5-[items(==31)]-4-[dividerFour(==1)]-2-[symbol(>=125@450,>=50)]-5-[dividerSix(==1)]-2-[lsp]-5-[dividerFive(==1)]-6-[recording]-7-|"
```

Update baseline alignment constraint (line 151) to include `lsp`:

```objc
@"H:[line]-[selection]-(>=1)-[grammar]-(>=1)-[tabSize]-(>=1)-[symbol]-(>=1)-[lsp]"
```

Update center-alignment constraint (line 154) to include `dividerSix`:

```objc
@"H:[selection]-(>=1)-[dividerOne]-(>=1)-[dividerTwo]-(>=1)-[dividerThree]-(>=1)-[items]-(>=1)-[dividerFour]-(>=1)-[dividerSix]-(>=1)-[dividerFive]-(>=1)-[recording]"
```

Update divider equal-height constraint (line 155):

```objc
@"V:|-5-[dividerOne(==15,==dividerTwo,==dividerThree,==dividerFour,==dividerFive,==dividerSix)]-5-|"
```

- [ ] **Step 5: Implement setLspStatus:errors:warnings:info:**

Single batch update method — builds an attributed string with colored dot + counts, uses `setUsesItemFromMenu:NO` pattern:

```objc
- (void)setLspStatus:(NSString*)status errors:(NSUInteger)errors warnings:(NSUInteger)warnings info:(NSUInteger)info
{
	BOOL visible = status != nil;
	self.lspPopUp.hidden = !visible;
	self.lspDivider.hidden = !visible;

	if(!visible)
		return;

	// Build attributed string incrementally with colors
	NSMutableAttributedString* attrTitle = [NSMutableAttributedString new];
	NSDictionary* baseAttrs = @{
		NSFontAttributeName: OakStatusBarFont(),
		NSForegroundColorAttributeName: NSColor.secondaryLabelColor,
	};

	// Status dot
	NSColor* dotColor;
	if([status isEqualToString:@"running"])
		dotColor = [NSColor systemGreenColor];
	else if([status isEqualToString:@"starting"])
		dotColor = [NSColor systemYellowColor];
	else
		dotColor = [NSColor tertiaryLabelColor];

	NSString* dot = [status isEqualToString:@"starting"] ? @"◉ " : ([status isEqualToString:@"running"] ? @"● " : @"○ ");
	[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:dot attributes:@{
		NSFontAttributeName: OakStatusBarFont(),
		NSForegroundColorAttributeName: dotColor,
	}]];

	// Diagnostic counts — colored per type
	BOOL hasCounts = NO;
	if(errors > 0)
	{
		[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"✕%lu", (unsigned long)errors] attributes:@{
			NSFontAttributeName: OakStatusBarFont(),
			NSForegroundColorAttributeName: [NSColor systemRedColor],
		}]];
		hasCounts = YES;
	}
	if(warnings > 0)
	{
		if(hasCounts) [attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:@" " attributes:baseAttrs]];
		[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"▲%lu", (unsigned long)warnings] attributes:@{
			NSFontAttributeName: OakStatusBarFont(),
			NSForegroundColorAttributeName: [NSColor systemYellowColor],
		}]];
		hasCounts = YES;
	}
	if(info > 0)
	{
		if(hasCounts) [attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:@" " attributes:baseAttrs]];
		[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"ℹ%lu", (unsigned long)info] attributes:baseAttrs]];
	}

	if(!hasCounts && [status isEqualToString:@"running"])
		[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:@"LSP" attributes:baseAttrs]];
	else if(!hasCounts)
		[attrTitle appendAttributedString:[[NSAttributedString alloc] initWithString:@"LSP" attributes:baseAttrs]];

	// Apply via cell menuItem (decoupled from popup menu contents)
	NSMenuItem* displayItem = [[NSMenuItem alloc] initWithTitle:@"" action:NULL keyEquivalent:@""];
	displayItem.attributedTitle = attrTitle;
	[[self.lspPopUp cell] setMenuItem:displayItem];
}
```

- [ ] **Step 6: Register for popup notification with respondsToSelector guard**

In `initWithFrame:`, add:

```objc
[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.lspPopUp];
```

Implement:

```objc
- (void)lspPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	if([self.delegate respondsToSelector:@selector(showLSPStatusMenu:)])
		[self.delegate showLSPStatusMenu:self.lspPopUp];
}
```

- [ ] **Step 7: Add lsp to key view loop**

Update `OakSetupKeyViewLoop` call:

```objc
OakSetupKeyViewLoop(@[ self, _grammarPopUp, _tabSizePopUp, _bundleItemsPopUp, _symbolPopUp, _lspPopUp, _macroRecordingButton ]);
```

- [ ] **Step 8: Build and verify**

Run: `make`
Expected: Clean build.

- [ ] **Step 9: Commit**

```
git add Frameworks/OakTextView/src/OTVStatusBar.h Frameworks/OakTextView/src/OTVStatusBar.mm
git commit -m "Add LSP status popup button to OTVStatusBar with diagnostic counts display"
```

---

## Chunk 3: OakDocumentView integration

### Task 3: Wire LSP notifications to status bar in OakDocumentView

**Files:**
- Modify: `Frameworks/OakTextView/src/OakDocumentView.mm`

- [ ] **Step 1: Add notification observers in initWithFrame:**

After the existing KVO setup (after line 107):

```objc
[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspDiagnosticsDidChange:) name:LSPDiagnosticsDidChangeNotification object:nil];
[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspServerStatusDidChange:) name:LSPServerStatusDidChangeNotification object:nil];
```

Add the `#import` for LSPManager.h if not already present (it is — line 3).

- [ ] **Step 2: Implement updateLSPStatusBar helper**

```objc
- (void)updateLSPStatusBar
{
	if(!_statusBar)
		return;

	LSPManager* lsp = [LSPManager sharedManager];
	OakDocument* doc = self.document;

	NSString* status = [lsp serverStatusForDocument:doc];
	NSDictionary<NSString*, NSNumber*>* counts = [lsp diagnosticCountsForDocument:doc];

	[_statusBar setLspStatus:status
	                  errors:[counts[@"errors"] unsignedIntegerValue]
	                warnings:[counts[@"warnings"] unsignedIntegerValue]
	                    info:[counts[@"info"] unsignedIntegerValue]];
}
```

- [ ] **Step 3: Implement notification handlers**

```objc
- (void)lspDiagnosticsDidChange:(NSNotification*)notification
{
	if(!_statusBar || !self.document)
		return;

	NSString* uri = notification.userInfo[@"uri"];
	if(uri)
	{
		NSURL* fileURL = self.document.path ? [NSURL fileURLWithPath:self.document.path] : nil;
		if(fileURL && ![uri isEqualToString:fileURL.absoluteString])
			return;
	}

	[self updateLSPStatusBar];
}

- (void)lspServerStatusDidChange:(NSNotification*)notification
{
	[self updateLSPStatusBar];
}
```

- [ ] **Step 4: Update status bar on document switch**

Find the `setDocument:` method in OakDocumentView.mm. At the end of it (after the document is assigned and KVO is set up), add:

```objc
dispatch_async(dispatch_get_main_queue(), ^{
	[self updateLSPStatusBar];
});
```

The async dispatch ensures the LSPManager has had time to register the document's client after `documentDidOpen:` is called.

- [ ] **Step 5: Implement showLSPStatusMenu: delegate method**

```objc
- (void)showLSPStatusMenu:(NSPopUpButton*)popUpButton
{
	LSPManager* lsp = [LSPManager sharedManager];
	OakDocument* doc = self.document;
	NSMenu* menu = popUpButton.menu;
	[menu removeAllItems];

	NSString* serverName = [lsp serverNameForDocument:doc];
	NSString* status = [lsp serverStatusForDocument:doc];

	// Server info header
	NSString* headerText;
	if(serverName && status)
		headerText = [NSString stringWithFormat:@"%@ — %@", serverName, status];
	else
		headerText = @"No LSP Server";

	NSMenuItem* header = [[NSMenuItem alloc] initWithTitle:headerText action:nil keyEquivalent:@""];
	header.enabled = NO;
	[menu addItem:header];

	if(status)
	{
		[menu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* restart = [[NSMenuItem alloc] initWithTitle:@"Restart Server" action:@selector(lspRestartServer:) keyEquivalent:@""];
		restart.target = self;
		[menu addItem:restart];
	}

	// Diagnostic summary
	NSDictionary* counts = [lsp diagnosticCountsForDocument:doc];
	NSUInteger errors   = [counts[@"errors"] unsignedIntegerValue];
	NSUInteger warnings = [counts[@"warnings"] unsignedIntegerValue];
	NSUInteger info     = [counts[@"info"] unsignedIntegerValue];

	if(errors + warnings + info > 0)
	{
		[menu addItem:[NSMenuItem separatorItem]];
		NSString* summary = [NSString stringWithFormat:@"Errors: %lu  Warnings: %lu  Info: %lu",
			(unsigned long)errors, (unsigned long)warnings, (unsigned long)info];
		NSMenuItem* diagHeader = [[NSMenuItem alloc] initWithTitle:summary action:nil keyEquivalent:@""];
		diagHeader.enabled = NO;
		[menu addItem:diagHeader];
	}
}
```

- [ ] **Step 6: Implement lspRestartServer: action**

```objc
- (void)lspRestartServer:(id)sender
{
	[[LSPManager sharedManager] restartServerForDocument:self.document];
}
```

- [ ] **Step 7: Build, test end-to-end**

Run: `make run`

Test scenarios:
1. Open a file with no LSP config → LSP section hidden
2. Open a PHP/JS file with LSP → green dot appears
3. Introduce a syntax error → error count updates in status bar
4. Click the LSP popup → menu shows server name, restart option, counts
5. Click "Restart Server" → server restarts, dot goes yellow then green
6. Switch between tabs → counts update per document

- [ ] **Step 8: Commit**

```
git add Frameworks/OakTextView/src/OakDocumentView.mm
git commit -m "Wire LSP diagnostics and server status to status bar via notifications"
```
