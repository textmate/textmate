# LSP Completion Popup — OakSwiftUI Integration

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the NSMenu-based LSP completion with OakSwiftUI's CompletionPopup, adding live fuzzy filtering and keyboard navigation.

**Architecture:** OakTextView lazily creates an `OakCompletionPopup` (guarded by `HAVE_OAK_SWIFTUI`), shows it on LSP response, intercepts keystrokes in `realKeyDown:` while visible, and inserts the selected completion via the delegate callback. Falls back to current NSMenu path when OakSwiftUI is not linked.

**Tech Stack:** Objective-C++, OakSwiftUI (Swift/SwiftUI via SPM), AppKit

---

## File Map

| File | Action | Responsibility |
|------|--------|---------------|
| `Frameworks/OakTextView/src/OakTextView.mm` | Modify | Add import, ivars, popup lifecycle, key interception, delegate methods |
| `Frameworks/OakTextView/src/OakTextView.h` | Modify | Forward-declare OakSwiftUI types (conditional) |

No new files needed. All OakSwiftUI components already exist.

---

## Chunk 1: Wire Up OakCompletionPopup

### Task 1: Add conditional import and ivars

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:1-49` (imports)
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:512-516` (ivars)

- [ ] **Step 1: Add OakSwiftUI import (guarded)**

After line 49 (`#import <Find/Find.h>`), add:

```objcpp
#if HAVE_OAK_SWIFTUI
#import <OakSwiftUI/OakSwiftUI-Swift.h>
#endif
```

- [ ] **Step 2: Replace LSP completion ivars**

Replace the ivar block at lines 512-515:

```objcpp
// ==================
// = LSP Completion =
// ==================

NSArray<NSDictionary*>* _lspSuggestions;
```

With:

```objcpp
// ==================
// = LSP Completion =
// ==================

#if HAVE_OAK_SWIFTUI
OakCompletionPopup* _lspCompletionPopup;
OakThemeEnvironment* _lspTheme;
NSUInteger _lspInitialPrefixLength;
NSString* _lspFilterPrefix;
#else
NSArray<NSDictionary*>* _lspSuggestions;
#endif
```

- [ ] **Step 3: Build to verify compilation**

Run: `make debug`
Expected: Clean build (or existing warnings only)

- [ ] **Step 4: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Add OakSwiftUI import and completion popup ivars"
```

---

### Task 2: Replace lspComplete: to use OakCompletionPopup

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:4642-4683` (`lspComplete:`)

- [ ] **Step 1: Rewrite lspComplete: with OakSwiftUI path**

Replace the entire `lspComplete:` method (lines 4642-4683) with:

```objcpp
- (void)lspComplete:(id)sender
{
	if(!documentView)
		return;

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	// Walk back from caret to find word prefix
	size_t bol = documentView->begin(pos.line);
	std::string lineText = documentView->substr(bol, caret);
	size_t prefixStart = lineText.size();
	while(prefixStart > 0 && (isalnum(lineText[prefixStart-1]) || lineText[prefixStart-1] == '_'))
		--prefixStart;
	NSString* prefix = to_ns(lineText.substr(prefixStart));

	OakDocument* doc = self.document;
	if(!doc)
		return;

	[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;
	NSUInteger prefixLen = prefix.length;
	[[LSPManager sharedManager] requestCompletionsForDocument:doc
		line:pos.line
		character:pos.column
		prefix:prefix
		completion:^(NSArray<NSDictionary*>* suggestions) {
			OakTextView* strongSelf = weakSelf;
			if(!strongSelf || suggestions.count == 0)
				return;

#if HAVE_OAK_SWIFTUI
			[strongSelf showLSPCompletionPopupWithSuggestions:suggestions prefixLength:prefixLen];
#else
			strongSelf->_lspSuggestions = suggestions;
			[strongSelf performSelector:@selector(showLSPCompletionMenu) withObject:nil afterDelay:0];
#endif
		}];
}
```

- [ ] **Step 2: Add showLSPCompletionPopupWithSuggestions:prefixLength: method**

Add this new method right after `lspComplete:`:

```objcpp
#if HAVE_OAK_SWIFTUI
- (void)showLSPCompletionPopupWithSuggestions:(NSArray<NSDictionary*>*)suggestions prefixLength:(NSUInteger)prefixLen
{
	if(!documentView)
		return;

	// Lazy-init theme
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

	// Lazy-init popup
	if(!_lspCompletionPopup)
	{
		_lspCompletionPopup = [[OakCompletionPopup alloc] initWithTheme:_lspTheme];
		_lspCompletionPopup.delegate = (id<OakCompletionPopupDelegate>)self;
	}

	// Convert suggestions to OakCompletionItem
	NSMutableArray<OakCompletionItem*>* items = [NSMutableArray arrayWithCapacity:suggestions.count];
	for(NSDictionary* s in suggestions)
	{
		OakCompletionItem* item = [[OakCompletionItem alloc]
			initWithLabel:s[@"label"] ?: s[@"display"] ?: @""
			   insertText:s[@"insert"]
			       detail:s[@"detail"] ?: @""
			         kind:[s[@"kind"] intValue]];
		[items addObject:item];
	}

	_lspInitialPrefixLength = prefixLen;
	_lspFilterPrefix = @"";

	// Get caret position in view-local coordinates
	CGRect caretRect = documentView->rect_at_index(documentView->ranges().last().last.index);
	NSPoint caretPoint = NSMakePoint(NSMinX(caretRect), NSMaxY(caretRect) + 4);

	[_lspCompletionPopup showIn:self at:caretPoint items:items];
}
#endif
```

- [ ] **Step 3: Build to verify**

Run: `make debug`
Expected: Clean build

- [ ] **Step 4: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Show OakSwiftUI completion popup on LSP response"
```

---

### Task 3: Intercept keystrokes while popup is visible

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:2237-2306` (`realKeyDown:`)

- [ ] **Step 1: Add LSP popup interception in realKeyDown:**

At the top of `realKeyDown:` (line 2238, after `AUTO_REFRESH;`), before the `_choiceMenu` check, insert:

```objcpp
#if HAVE_OAK_SWIFTUI
	if([_lspCompletionPopup isVisible])
	{
		if([_lspCompletionPopup handleKeyEvent:anEvent])
			return; // popup consumed it (arrows, return, tab, esc)

		// Popup didn't consume — check what kind of key it is
		NSString* chars = [anEvent characters];
		if(chars.length > 0)
		{
			unichar ch = [chars characterAtIndex:0];
			BOOL isWordChar = isalnum(ch) || ch == '_';
			BOOL isBackspace = [anEvent keyCode] == 51;

			if(isWordChar)
			{
				[self oldKeyDown:anEvent];
				_lspFilterPrefix = [_lspFilterPrefix stringByAppendingString:chars];
				[_lspCompletionPopup updateFilter:_lspFilterPrefix];
				return;
			}
			else if(isBackspace)
			{
				if(_lspFilterPrefix.length > 0)
				{
					[self oldKeyDown:anEvent];
					_lspFilterPrefix = [_lspFilterPrefix substringToIndex:_lspFilterPrefix.length - 1];
					[_lspCompletionPopup updateFilter:_lspFilterPrefix];
					return;
				}
				// Filter empty — dismiss and let backspace proceed normally
				[_lspCompletionPopup dismiss];
				return [self oldKeyDown:anEvent];
			}
			else
			{
				// Non-word char (period, space, etc.) — dismiss, process keystroke
				[_lspCompletionPopup dismiss];
				return [self oldKeyDown:anEvent];
			}
		}
	}
#endif
```

- [ ] **Step 2: Build to verify**

Run: `make debug`
Expected: Clean build

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Intercept keystrokes for LSP completion popup filtering"
```

---

### Task 4: Implement delegate methods

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm` (before `@end`, around line 4890)

- [ ] **Step 1: Add OakCompletionPopupDelegate methods**

Before the final `@end` of OakTextView, add:

```objcpp
#if HAVE_OAK_SWIFTUI
// ===================================
// = OakCompletionPopupDelegate =
// ===================================

- (void)completionPopup:(OakCompletionPopup*)popup didSelectItem:(OakCompletionItem*)item
{
	if(!documentView)
		return;

	AUTO_REFRESH;

	// Delete the prefix (initial + filter) then insert completion text
	size_t caret = documentView->ranges().last().last.index;
	NSUInteger deleteCount = _lspInitialPrefixLength + _lspFilterPrefix.length;
	size_t from = caret - deleteCount;
	documentView->set_ranges(ng::range_t(from, caret));
	documentView->insert(to_s(item.effectiveInsertText));

	_lspFilterPrefix = nil;
}

- (void)completionPopupDidDismiss:(OakCompletionPopup*)popup
{
	_lspFilterPrefix = nil;
}
#endif
```

- [ ] **Step 2: Build to verify**

Run: `make debug`
Expected: Clean build

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Implement completion popup delegate for item insertion"
```

---

### Task 5: Remove old NSMenu code (guarded)

**Files:**
- Modify: `Frameworks/OakTextView/src/OakTextView.mm:4685-4731` (`showLSPCompletionMenu`, `lspInsertCompletion:`)

- [ ] **Step 1: Wrap old methods in #if !HAVE_OAK_SWIFTUI**

Wrap the existing `showLSPCompletionMenu` and `lspInsertCompletion:` methods:

```objcpp
#if !HAVE_OAK_SWIFTUI
- (void)showLSPCompletionMenu
{
	// ... existing code unchanged ...
}

- (void)lspInsertCompletion:(NSMenuItem*)sender
{
	// ... existing code unchanged ...
}
#endif
```

- [ ] **Step 2: Build both paths**

Run: `make debug`
Expected: Clean build with OakSwiftUI path active

- [ ] **Step 3: Commit**

```
git add Frameworks/OakTextView/src/OakTextView.mm
git commit -m "Guard old NSMenu completion behind !HAVE_OAK_SWIFTUI"
```

---

### Task 6: Manual smoke test

- [ ] **Step 1: Launch TextMate**

Run: `make run`

- [ ] **Step 2: Test completion flow**

1. Open a file with an LSP server configured (e.g., a `.ts` or `.py` file)
2. Type a partial identifier, press Opt+Tab
3. Verify: SwiftUI popup appears below caret with completions
4. Type more characters → popup filters live
5. Press backspace → filter un-narrows
6. Arrow keys → selection moves
7. Return/Tab → selected item inserted, prefix replaced
8. Esc → popup dismissed without insertion
9. Type non-word char while popup visible → popup dismissed, char inserted

- [ ] **Step 3: Verify fallback**

Build without OakSwiftUI to confirm the `#else` path still compiles (optional — mainly for CI).
