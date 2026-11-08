# Custom Formatter Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `.tm_properties`-based external formatter support that takes priority over LSP formatting.

**Architecture:** Two new settings keys (`formatCommand`, `formatOnSave`) in the settings framework. A synchronous helper method in OakTextView runs the formatter via NSTask. The existing `lspFormatDocument:` action and `documentWillSave:` handler are extended to check for a custom formatter first, falling back to LSP.

**Tech Stack:** Objective-C++, NSTask, settings framework, existing `perform_replacements()` infrastructure.

**Spec:** `docs/superpowers/specs/2026-03-15-custom-formatter-design.md`

---

## Chunk 1: Settings Keys & Custom Formatter Helper

### Task 1: Add formatCommand and formatOnSave settings keys

**Files:**
- Modify: `Frameworks/settings/src/keys.h:29` (after `kSettingsAtomicSaveKey`)
- Modify: `Frameworks/settings/src/keys.cc:29` (after `kSettingsAtomicSaveKey`)

- [ ] **Step 1: Add extern declarations to keys.h**

After line 30 (`extern std::string const kSettingsAtomicSaveKey;`), add:

```cpp
extern std::string const kSettingsFormatCommandKey;
extern std::string const kSettingsFormatOnSaveKey;
```

- [ ] **Step 2: Add definitions to keys.cc**

After line 29 (`std::string const kSettingsAtomicSaveKey = "atomicSave";`), add:

```cpp
std::string const kSettingsFormatCommandKey               = "formatCommand";
std::string const kSettingsFormatOnSaveKey                 = "formatOnSave";
```

- [ ] **Step 3: Build to verify no compile errors**

Run: `make`
Expected: Clean build, no errors related to settings keys.

- [ ] **Step 4: Commit**

```
git add Frameworks/settings/src/keys.h Frameworks/settings/src/keys.cc
git commit -m "Add formatCommand and formatOnSave settings keys"
```

---

### Task 2: Add runCustomFormatter helper to OakTextView

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm` (add helper before `lspFormatDocument:` at line ~5706)

- [ ] **Step 1: Add the helper method**

Insert before the `// = LSP Formatting =` section (before `replacementsFromTextEdits` at line ~5683), add a new section:

```objc
// ========================
// = Custom Formatter     =
// ========================

static NSString* runCustomFormatter (std::string const& command, NSString* inputText, std::map<std::string, std::string> const& variables, NSString** outError)
{
	NSTask* task = [[NSTask alloc] init];
	task.launchPath = @"/bin/sh";
	task.arguments = @[@"-c", [NSString stringWithCxxString:command]];

	// Build environment from TM variables
	NSMutableDictionary* env = [NSMutableDictionary dictionaryWithDictionary:[[NSProcessInfo processInfo] environment]];

	// Prepend common tool paths for GUI launch contexts
	NSString* existingPath = env[@"PATH"] ?: @"/usr/bin:/bin";
	NSString* localBin = [NSHomeDirectory() stringByAppendingPathComponent:@".local/bin"];
	env[@"PATH"] = [NSString stringWithFormat:@"/opt/homebrew/bin:/usr/local/bin:%@:%@", localBin, existingPath];

	for(auto const& [key, value] : variables)
		env[[NSString stringWithCxxString:key]] = [NSString stringWithCxxString:value];

	task.environment = env;

	NSPipe* stdinPipe  = [NSPipe pipe];
	NSPipe* stdoutPipe = [NSPipe pipe];
	NSPipe* stderrPipe = [NSPipe pipe];

	task.standardInput  = stdinPipe;
	task.standardOutput = stdoutPipe;
	task.standardError  = stderrPipe;

	@try {
		[task launch];
	}
	@catch(NSException* e) {
		if(outError)
			*outError = [NSString stringWithFormat:@"Failed to launch formatter: %@", e.reason];
		return nil;
	}

	// Write stdin and drain stdout/stderr concurrently to avoid pipe buffer deadlock
	__block NSData* outputData = nil;
	__block NSData* errorData = nil;

	dispatch_group_t group = dispatch_group_create();
	dispatch_queue_t bgQueue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

	dispatch_group_async(group, bgQueue, ^{
		NSData* inputData = [inputText dataUsingEncoding:NSUTF8StringEncoding];
		[stdinPipe.fileHandleForWriting writeData:inputData];
		[stdinPipe.fileHandleForWriting closeFile];
	});

	dispatch_group_async(group, bgQueue, ^{
		outputData = [stdoutPipe.fileHandleForReading readDataToEndOfFile];
	});

	dispatch_group_async(group, bgQueue, ^{
		errorData = [stderrPipe.fileHandleForReading readDataToEndOfFile];
	});

	// Block with 3-second timeout
	NSDate* deadline = [NSDate dateWithTimeIntervalSinceNow:3.0];
	while(task.isRunning && [deadline timeIntervalSinceNow] > 0)
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.05, true);

	if(task.isRunning)
	{
		[task terminate];
		[task waitUntilExit];
		dispatch_group_wait(group, dispatch_time(DISPATCH_TIME_NOW, 500 * NSEC_PER_MSEC));
		if(outError)
			*outError = @"Formatter timed out";
		return nil;
	}

	// Wait for I/O threads to finish reading
	dispatch_group_wait(group, dispatch_time(DISPATCH_TIME_NOW, 500 * NSEC_PER_MSEC));

	if(task.terminationStatus != 0)
	{
		if(outError)
		{
			NSString* errStr = [[NSString alloc] initWithData:errorData encoding:NSUTF8StringEncoding];
			*outError = [errStr componentsSeparatedByString:@"\n"].firstObject ?: @"Formatter failed";
		}
		return nil;
	}

	NSString* output = [[NSString alloc] initWithData:outputData encoding:NSUTF8StringEncoding];

	// Guard against empty output from broken formatters
	if(!output || output.length == 0)
	{
		if(outError)
			*outError = @"Formatter returned empty output";
		return nil;
	}

	return output;
}
```

- [ ] **Step 2: Build to verify no compile errors**

Run: `make`
Expected: Clean build. The function is static so no linker issues even though it's not called yet.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Add runCustomFormatter static helper for external formatters"
```

---

## Chunk 2: Integration — Format Action, Save Handler, Menu Validation

### Task 3: Extend lspFormatDocument: to check custom formatter first

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:5706` (`lspFormatDocument:` method)

- [ ] **Step 1: Add custom formatter check at the top of lspFormatDocument:**

Replace the current `lspFormatDocument:` method (lines 5706-5771) with:

```objc
- (void)lspFormatDocument:(id)sender
{
	if(!documentView)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	std::string filePath  = to_s(doc.path ?: @"");
	std::string fileType  = to_s(doc.fileType ?: @"");
	std::string directory = to_s(doc.directory ?: [doc.path stringByDeletingLastPathComponent] ?: @"");

	settings_t const settings = settings_for_path(filePath, fileType, directory);
	std::string formatCommand = settings.get(kSettingsFormatCommandKey, "");

	if(!formatCommand.empty())
	{
		NSString* inputText = [NSString stringWithCxxString:documentView->substr()];
		std::map<std::string, std::string> variables = [self variables];

		NSString* error = nil;
		NSString* output = runCustomFormatter(formatCommand, inputText, variables, &error);

		if(output && ![output isEqualToString:inputText])
		{
			// Clamp previous caret offset to new buffer length
			size_t caretOffset = documentView->ranges().last().last.index;
			size_t newLength = to_s(output).size();

			AUTO_REFRESH;
			std::multimap<std::pair<size_t, size_t>, std::string> replacements;
			replacements.emplace(std::make_pair((size_t)0, documentView->size()), to_s(output));
			documentView->perform_replacements(replacements);
			documentView->set_ranges(ng::range_t(std::min(caretOffset, newLength)));
		}
		else if(error)
		{
			[self showToolTip:error];
		}
		return;
	}

	// Fall back to LSP formatting
	LSPManager* lsp = [LSPManager sharedManager];

	bool hasSelection = documentView->has_selection();
	ng::ranges_t capturedRanges = documentView->ranges();
	size_t revision = documentView->revision();
	NSUInteger tabSize = doc.tabSize;
	BOOL insertSpaces = doc.softTabs;

	[lsp flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;

	if(hasSelection && [lsp serverSupportsRangeFormattingForDocument:doc])
	{
		ng::range_t sel = capturedRanges.last();
		text::pos_t startPos = documentView->convert(sel.min().index);
		text::pos_t endPos   = documentView->convert(sel.max().index);

		[lsp requestRangeFormattingForDocument:doc
			startLine:startPos.line startCharacter:startPos.column
			endLine:endPos.line endCharacter:endPos.column
			tabSize:tabSize insertSpaces:insertSpaces
			completion:^(NSArray<NSDictionary*>* edits) {
				OakTextView* strongSelf = weakSelf;
				if(!strongSelf || !strongSelf->documentView)
					return;
				if(!edits || edits.count == 0)
					return;
				if(strongSelf->documentView->revision() != revision)
					return;

				AUTO_REFRESH;
				strongSelf->documentView->perform_replacements(replacementsFromTextEdits(*strongSelf->documentView, edits));
			}];
	}
	else if([lsp serverSupportsFormattingForDocument:doc])
	{
		[lsp requestFormattingForDocument:doc
			tabSize:tabSize insertSpaces:insertSpaces
			completion:^(NSArray<NSDictionary*>* edits) {
				OakTextView* strongSelf = weakSelf;
				if(!strongSelf || !strongSelf->documentView)
					return;
				if(!edits || edits.count == 0)
					return;
				if(strongSelf->documentView->revision() != revision)
					return;

				AUTO_REFRESH;
				strongSelf->documentView->perform_replacements(replacementsFromTextEdits(*strongSelf->documentView, edits));
			}];
	}
	else
	{
		NSBeep();
	}
}
```

- [ ] **Step 2: Build to verify**

Run: `make`
Expected: Clean build.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Route format action through custom formatter when configured"
```

---

### Task 4: Extend documentWillSave: for custom formatter format-on-save

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:1076` (`documentWillSave:` method)

- [ ] **Step 1: Replace the save handler**

Replace the current `documentWillSave:` method (lines 1076-1124) with:

```objc
- (void)documentWillSave:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.will-save", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];

	if(documentView)
	{
		OakDocument* doc = self.document;
		if(doc)
		{
			std::string filePath  = to_s(doc.path ?: @"");
			std::string fileType  = to_s(doc.fileType ?: @"");
			std::string directory = to_s(doc.directory ?: [doc.path stringByDeletingLastPathComponent] ?: @"");

			settings_t const settings = settings_for_path(filePath, fileType, directory);
			bool formatOnSave = settings.get(kSettingsFormatOnSaveKey, settings.get("lspFormatOnSave", false));
			std::string formatCommand = settings.get(kSettingsFormatCommandKey, "");

			if(formatOnSave && !formatCommand.empty())
			{
				NSString* inputText = [NSString stringWithCxxString:documentView->substr()];
				std::map<std::string, std::string> variables = [self variables];

				NSString* error = nil;
				NSString* output = runCustomFormatter(formatCommand, inputText, variables, &error);

				if(output && ![output isEqualToString:inputText])
				{
					size_t caretOffset = documentView->ranges().last().last.index;
					size_t newLength = to_s(output).size();

					AUTO_REFRESH;
					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					replacements.emplace(std::make_pair((size_t)0, documentView->size()), to_s(output));
					documentView->perform_replacements(replacements);
					documentView->set_ranges(ng::range_t(std::min(caretOffset, newLength)));
				}
				else if(error)
				{
					NSLog(@"[Formatter] Format-on-save failed: %@", error);
				}
			}
			else if(formatOnSave && [[LSPManager sharedManager] serverSupportsFormattingForDocument:doc])
			{
				[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

				__block BOOL done = NO;
				__block NSArray<NSDictionary*>* receivedEdits = nil;

				[[LSPManager sharedManager] requestFormattingForDocument:doc
					tabSize:doc.tabSize insertSpaces:doc.softTabs
					completion:^(NSArray<NSDictionary*>* edits) {
						receivedEdits = edits;
						done = YES;
					}];

				NSDate* timeout = [NSDate dateWithTimeIntervalSinceNow:3.0];
				while(!done && [timeout timeIntervalSinceNow] > 0)
					CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.05, true);

				if(!done)
					NSLog(@"[LSP] Format-on-save timed out after 3 seconds");

				if(receivedEdits.count > 0)
				{
					AUTO_REFRESH;
					documentView->perform_replacements(replacementsFromTextEdits(*documentView, receivedEdits));
				}
			}
		}
	}

	[self updateDocumentMetadata];
}
```

- [ ] **Step 2: Build to verify**

Run: `make`
Expected: Clean build.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Extend format-on-save to support custom formatters with LSP fallback"
```

---

### Task 5: Update menu validation for custom formatter

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:3305` (`validateMenuItem:` for `lspFormatDocument:`)

- [ ] **Step 1: Update the validation block**

Replace the `lspFormatDocument:` validation block (lines 3305-3313) with:

```objc
else if([aMenuItem action] == @selector(lspFormatDocument:))
{
	OakDocument* doc = self.document;
	std::string filePath  = to_s(doc.path ?: @"");
	std::string fileType  = to_s(doc.fileType ?: @"");
	std::string directory = to_s(doc.directory ?: [doc.path stringByDeletingLastPathComponent] ?: @"");

	settings_t const settings = settings_for_path(filePath, fileType, directory);
	std::string formatCommand = settings.get(kSettingsFormatCommandKey, "");

	BOOL hasCustomFormatter = !formatCommand.empty();
	BOOL hasRange = doc && [[LSPManager sharedManager] serverSupportsRangeFormattingForDocument:doc];
	BOOL hasDoc   = doc && [[LSPManager sharedManager] serverSupportsFormattingForDocument:doc];
	BOOL showSelection = !hasCustomFormatter && [self hasSelection] && hasRange;
	[aMenuItem updateTitle:[NSString stringWithCxxString:format_string::replace(to_s(aMenuItem.title), "\\b(\\w+) / (Selection)\\b", showSelection ? "$2" : "$1")]];
	return hasCustomFormatter || showSelection || hasDoc;
}
```

Note: When a custom formatter is set, selection formatting is not available (v1), so the title always shows "Code" not "Selection".

- [ ] **Step 2: Build to verify**

Run: `make`
Expected: Clean build.

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Enable format menu item when custom formatter is configured"
```

---

## Chunk 3: Manual Testing & Final Verification

### Task 6: Build, run, and manually test

- [ ] **Step 1: Full debug build**

Run: `make`
Expected: Clean build, no new warnings.

- [ ] **Step 2: Test with a custom formatter**

Create a test `.tm_properties` in a project directory:
```
[ *.js ]
formatCommand = prettier --stdin-filepath "$TM_FILEPATH"
formatOnSave  = true
```

Open a `.js` file in that project. Verify:
1. Text > Format Code menu item is enabled
2. Invoking it formats the document via prettier
3. Saving the file triggers format-on-save via prettier
4. Undo reverses the formatting in one step

- [ ] **Step 3: Test LSP fallback**

Open a file type with LSP configured but no `formatCommand`. Verify:
1. Format Code still works via LSP
2. LSP format-on-save still works when `lspFormatOnSave = true`

- [ ] **Step 4: Test error handling**

Set `formatCommand = nonexistent_command`. Verify:
1. Format Code shows tooltip with error
2. Format-on-save logs error but doesn't block save

- [ ] **Step 5: Test timeout**

Set `formatCommand = sleep 10`. Verify:
1. Format action returns after ~3 seconds
2. Save is not blocked for more than ~3 seconds

- [ ] **Step 6: Final commit if any fixes were needed**

```
git add -u
git commit -m "Fix issues found during manual testing"
```
