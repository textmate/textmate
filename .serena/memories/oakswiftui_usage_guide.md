# OakSwiftUI Usage Guide for ObjC++ Consumers

## Prerequisites

OakSwiftUI is an optional dependency. Guard all usage:
- Compile time: `#if HAVE_OAK_SWIFTUI`
- Runtime: `NSClassFromString(@"OakCompletionPopup")` returns non-nil
- Import: `#import <OakSwiftUI/OakSwiftUI-Swift.h>` (auto-generated header)

All bridge classes are `@MainActor` — call them only from the main thread.

---

## 1. CompletionPopup (LSP autocomplete)

### Setup (once per OakTextView instance)
```objcpp
#if HAVE_OAK_SWIFTUI
OakThemeEnvironment* theme = [[OakThemeEnvironment alloc] init];
[theme applyTheme:@{
   @"fontName": @"Menlo",
   @"fontSize": @(12),
   @"backgroundColor": [NSColor textBackgroundColor],
   @"foregroundColor": [NSColor textColor],
}];
OakCompletionPopup* completionPopup = [[OakCompletionPopup alloc] initWithTheme:theme];
completionPopup.delegate = self; // adopt OakCompletionPopupDelegate
#endif
```

### Showing completions (on LSP response)
```objcpp
NSMutableArray<OakCompletionItem*>* items = [NSMutableArray array];
for (auto const& lspItem : lspResponse.items) {
   OakCompletionItem* item = [[OakCompletionItem alloc]
      initWithLabel:[NSString stringWithUTF8String:lspItem.label.c_str()]
         insertText:[NSString stringWithUTF8String:lspItem.insertText.c_str()]
             detail:[NSString stringWithUTF8String:lspItem.detail.c_str()]
               kind:lspItem.kind];
   [items addObject:item];
}
NSPoint caretPoint = [self positionForWindowUnderCaret]; // OakTextView method
[completionPopup showIn:self at:caretPoint items:items];
```

### Keyboard forwarding (in keyDown: or similar)
```objcpp
if ([completionPopup isVisible]) {
   if ([completionPopup handleKeyEvent:event])
      return; // popup consumed the event
   // Popup didn't handle it — process keystroke normally, then update filter
   [self insertText:...];
   [completionPopup updateFilter:currentWordAtCaret];
}
```

### Delegate callbacks
```objcpp
- (void)completionPopup:(OakCompletionPopup*)popup didSelectItem:(OakCompletionItem*)item {
   // Insert item.effectiveInsertText into the buffer at cursor
   [self insertSnippetWithOptions:@{ @"content": item.effectiveInsertText }];
}

- (void)completionPopupDidDismiss:(OakCompletionPopup*)popup {
   // Cleanup if needed (e.g., clear LSP state)
}
```

### Dismissal
- Escape, Tab, Return handled automatically by `handleKeyEvent:`
- Call `[completionPopup dismiss]` manually on focus loss, document switch, etc.
- `show:` with new items auto-dismisses previous popup (no spurious delegate callback)

---

## 2. InfoTooltip (LSP hover/definition)

### Setup
```objcpp
OakInfoTooltip* tooltip = [[OakInfoTooltip alloc] initWithTheme:theme];
tooltip.delegate = self; // adopt OakInfoTooltipDelegate
```

### Showing hover info
```objcpp
NSAttributedString* body = [[NSAttributedString alloc]
   initWithString:@"Returns the current user's name"
       attributes:@{NSFontAttributeName: [NSFont systemFontOfSize:12]}];

OakTooltipContent* content = [[OakTooltipContent alloc]
   initWithTitle:@"func userName() -> String"
            body:body
     codeSnippet:@"let name = userName()"
        language:@"swift"];

NSRect charRect = [self rectForCharacterAtIndex:hoverPosition];
[tooltip showIn:self at:charRect content:content];
```

### Scroll handling
OakTextView must notify the tooltip when the view scrolls:
```objcpp
- (void)scrollWheel:(NSEvent*)event {
   [super scrollWheel:event];
   if ([tooltip isVisible]) {
      NSRect newRect = [self rectForCharacterAtIndex:hoverPosition];
      [tooltip repositionTo:newRect];
   }
}
```

### Dismissal
- Auto-dismisses when mouse leaves tooltip bounds or on any keystroke
- `popover.behavior = .semitransient` handles this
- Delegate notified via `infoTooltipDidDismiss:`

---

## 3. FloatingPanel (persistent reference)

### Showing a panel
```objcpp
OakFloatingPanel* panel = [[OakFloatingPanel alloc] init];
panel.delegate = self; // adopt OakFloatingPanelDelegate

// Content can be any NSView — use NSHostingView for SwiftUI content
NSView* contentView = ...; // e.g., symbol outline, docs panel
[panel showWithContent:contentView
                 title:@"Symbol Outline"
          parentWindow:self.window];
```

### Behavior
- Floats above parent window only (child window relationship)
- Follows parent window when moved
- User can move/resize/close the panel
- `floatingPanelDidClose:` called when user closes

---

## Theme Updates

When TextMate theme changes, update the shared `OakThemeEnvironment`:
```objcpp
[theme applyTheme:@{
   @"fontName": newTheme.fontName,
   @"fontSize": @(newTheme.fontSize),
   @"backgroundColor": newTheme.backgroundColor,
   // ... all color keys
}];
// All visible SwiftUI views update automatically via @Published
```

Supported keys: `fontName`, `fontSize`, `backgroundColor`, `foregroundColor`,
`selectionColor`, `keywordColor`, `commentColor`, `stringColor`.

---

## Key Integration Points in TextMate

- **OakTextView** (`Frameworks/OakTextView/src/OakTextView.mm`): owns popup instances, forwards key events, provides caret position via `positionForWindowUnderCaret`
- **LSPManager** (`Frameworks/lsp/src/LSPManager.mm`): receives LSP responses, converts to OakCompletionItem/OakTooltipContent, calls OakTextView to show UI
- **Theme changes**: listen for theme change notification, call `applyTheme:` on shared OakThemeEnvironment
