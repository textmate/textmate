# LSP Inline Diagnostics — Design Spec (v4)

## Goal

Add rich visual diagnostic rendering to TextMate's LSP integration: line background tinting, inline diagnostic messages at the right edge of the viewport, and severity-specific coloring. Currently we show gutter mark icons only.

## Reference

Inspired by [schriftgestalt/textmate](https://github.com/schriftgestalt/textmate) fork, which implements similar visuals via a bundle + `mate --set-mark` approach (~400 lines in OakTextView). Our implementation differs: we use LSP as the diagnostic source and query LSPManager directly instead of encoding data in mark strings.

## Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Diagnostic source | LSP only | We already have real-time LSP diagnostics. Bundle commands keep basic gutter icons. |
| Visual elements | Line tint + inline messages + existing gutter icons | Full visual feedback. Squiggly underlines only if trivially portable from fork. |
| Inline message placement | Right edge of viewport | Always visible regardless of horizontal scroll. Repositioned on scroll. |
| Multi-diagnostic lines | Highest severity in the pill, all of them in the box | The pill carries the primary message plus a `+N` count; expanding it lists every diagnostic on the line, sorted by severity. |
| Rendering scope | Visible viewport only | Natural `drawRect:` behavior. No explicit cap needed. |
| Toggle granularity | Global preference only | `lspShowInlineDiagnostics` user default, reachable from Preferences → Advanced and Edit → LSP. A `.tm_properties` override was implemented and then removed: it left the checkbox looking broken whenever a project overrode it, and it put a `settings_for_path` directory walk in the draw path. |
| Colors | NSColor semantic system colors | systemRed/systemYellow/systemBlue. Alpha tuned during implementation (0.08 may be too faint for yellow on light backgrounds). |
| Click interaction | Expands the banner into a selectable box below the line | Code actions keep the two affordances they already had, Cmd+. and the gutter lightbulb. Expansion cannot edit anything, so it needs no buffer-revision guard against a stale click. |
| Code location | OakTextView+Diagnostics.mm category | Follows existing pattern (OakTextView+Formatting.mm, OakTextView+Copilot.mm). |
| Data model | Query LSPManager directly, no parallel store | `_diagnosticsByURI` already holds full diagnostic dicts. Group by line on demand during draw. |
| Message format | Code + message | "F401: \`sys\` imported but unused" |
| LSP fields preserved | Full diagnostic object in LSPManager | Already stored in `_diagnosticsByURI`. OakTextView reads what it needs. |
| Gutter icons | Keep existing mark icons | Line tint + inline messages already communicate severity. |
| Animation | Instant (try fade-in later) | Start simple, iterate. |

## Architecture

### Data Flow

```
LSP Server
  → LSPClient (JSON-RPC, dispatched to main queue)
    → LSPManager.didReceiveDiagnostics:forDocumentURI:
      → Cache full diagnostics in _diagnosticsByURI (existing)
      → Set buffer marks for gutter icons (existing)
      → Post LSPDiagnosticsDidChangeNotification with userInfo: { uri }
        → OakTextView receives notification
          → Filters by matching URI against self.document.path
          → Invalidates cached banner rects
          → Calls setNeedsDisplay:YES

drawRect: (existing in OakTextView.mm)
  → documentView->draw(...)         // existing: text, selection, folding (fills background first)
  → [self drawGhostText:...]        // existing: Copilot ghost text
  → [self drawDiagnosticTints:context inRect:aRect]    // NEW: line tints (AFTER text)
  → [self drawDiagnosticBanners:context inRect:aRect]  // NEW: right-edge message pills
  → definition highlight, hover highlight (existing)
```

### Threading

All safe on main thread. `LSPClient.handleMessage` dispatches to `dispatch_get_main_queue()` before calling delegate methods. The `NSNotificationCenter` post and all rendering happen on main.

### Data Model — No Parallel Store

Instead of building a redundant `diagnostic_entry_t` struct store, OakTextView queries LSPManager directly:

```objc
// New method on LSPManager:
- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document;
// Returns the cached _diagnosticsByURI[fileURL] array — already stores full LSP diagnostic objects.

// In the Diagnostics category, group by line on demand:
- (NSDictionary<NSNumber*, NSArray<NSDictionary*>*>*)groupedDiagnosticsForVisibleRect:(NSRect)visibleRect;
// Iterates diagnosticsForDocument:, filters to lines visible in rect, groups by line,
// returns @{ @(lineNumber): @[diagnostic, ...] }. <100 items, negligible cost.
```

This eliminates: ivar storage concerns, ARC-in-C++-struct bugs, and sync issues between two stores.

**What IS stored as ivars** (in `OakTextView_Private.h`, following existing pattern):

```objc
// In @interface OakTextView () { ... } block in OakTextView_Private.h:

struct diagnostic_banner_t {
    NSRect rect;
    size_t line;               // 0-indexed logical line
    NSDictionary* diagnostic;  // the LSP diagnostic dict (ARC-managed in ObjC++ struct)
};

std::vector<diagnostic_banner_t> _diagnosticBannerRects;  // cached for hit testing, rebuilt each draw
BOOL _diagnosticBannersVisible;                             // toggle state (from settings)
```

This follows existing patterns in `OakTextView_Private.h` which already mixes C++ types (`ng::range_t`, `std::vector<std::string>`) with ARC-managed ObjC pointers as ivars.

### Rendering

All drawing happens in the category methods called from the existing `drawRect:` — NOT by overriding `drawRect:` in the category.

**Draw sequence in OakTextView.mm `drawRect:`** (showing where new calls go):

```
// Inside the ghost text conditional block (both branches):
documentView->draw(...)                          // existing: text, selection, folding (paints opaque background first)
[self drawGhostText:context inRect:aRect]        // existing: Copilot

// AFTER the ghost text if/else block (lines ~939+), before definition/hover highlights:
[self drawDiagnosticTints:context inRect:aRect]  // NEW — translucent line background wash
[self drawDiagnosticBanners:context inRect:aRect] // NEW — right-edge message pills
// existing: definition underline, hover highlight
```

**Z-order rationale**: Both tints and banners draw AFTER `documentView->draw()`. This is necessary because `layout_t::draw()` fills the entire visible rect with the theme's opaque background color (`layout.cc:812-814`, `drawBackground` defaults to `true`) — any drawing before it is obliterated. Drawing tints after text means the translucent color washes over both background and glyphs together. At alpha 0.08-0.12, the effect on glyph readability is negligible — this is standard practice in VS Code, IntelliJ, and Xcode. Banners are opaque UI elements and naturally belong on top.

**Ghost text branch structure**: `documentView->draw()` is called inside an if/else block (`OakTextView.mm:908-938`) — once in the multi-line ghost text path (Pass 1 + Pass 2) and once in the else branch. The diagnostic draw calls go AFTER this entire block (after line ~939), not inside either branch. This means they execute in the restored (unshifted) coordinate space.

**1. Line Tint** (`drawDiagnosticTints:inRect:`)

For each visible line with diagnostics, fill the full line rect with a translucent severity color:
- Error (severity 1): `[NSColor.systemRedColor colorWithAlphaComponent:α]`
- Warning (severity 2): `[NSColor.systemYellowColor colorWithAlphaComponent:α]`
- Info (severity 3): `[NSColor.systemBlueColor colorWithAlphaComponent:α]`
- Hint (severity 4): no tint

Alpha value TBD during implementation (start at 0.12, test in both light and dark themes — 0.08 may be invisible for yellow on light backgrounds).

When multiple diagnostics exist on one line, use the lowest severity number (highest priority): error > warning > info.

Line rects obtained by iterating visible lines using the C++ layout API directly (not the ObjC `GutterViewDelegate` method, which returns a different type):

```cpp
// Walk visible lines by y-position, same pattern as GutterView but using ng::line_record_t
auto prevLine = std::make_pair<size_t, size_t>(-1, 0);
for(CGFloat y = NSMinY(aRect); y < NSMaxY(aRect); )
{
    auto record = documentView->line_record_for(y);
    if(record.bottom <= y || prevLine == std::make_pair(record.line, record.softline))
        break;
    prevLine = std::make_pair(record.line, record.softline);

    if(record.softline != 0) // only draw on first visual line of logical line
    {
        y = record.bottom;
        continue;
    }

    // record.line is 0-indexed logical line number
    // record.top / record.bottom define the visual line rect
    // record.baseline is the text baseline offset within the line
    NSRect lineRect = NSMakeRect(NSMinX(aRect), record.top, NSWidth(aRect), record.bottom - record.top);
    // ... draw tint for this line if it has diagnostics ...

    y = record.bottom;
}
```

`ng::line_record_t` fields: `line` (0-indexed logical line), `softline` (soft-wrap offset), `top`, `bottom`, `baseline`. This handles soft-wrapped and folded lines correctly.

**2. Diagnostic Banners** (`drawDiagnosticBanners:inRect:`)

For each visible line with diagnostics (highest severity drives the pill; a `+N` count stands for the rest):

- Compute banner position: `x = NSMaxX([self visibleRect]) - bannerWidth - padding`, `y = lineRecord.firstY + baseline offset`
- Draw a rounded rect pill with severity background color (higher alpha than tint, ~0.15-0.25)
- Draw text: `"CODE: message"` truncated with ellipsis if longer than max banner width (~40% of visible width)
- Cache each banner's rect + associated diagnostic + line number in `_diagnosticBannerRects` for hit testing

**Coordinate system**: Banners are pinned to the viewport's right edge, not the document's. Since `drawRect:` operates in document coordinates, the x-position is computed from `[self visibleRect]` which gives the clip view's visible area in document coordinates. This naturally handles horizontal scroll.

**Soft-wrap**: A logical line may span multiple visual lines. Place the banner at the **first visual line** of the logical line (where `softlineOffset == 0`), using the same `lineRecordForPosition:` iteration as GutterView. If the banner would overlap wrapped text on the first visual line, it still draws at the right edge — wrapping is the user's choice and the banner is semi-transparent.

### Scroll Handling

**Vertical scroll**: Handled automatically. The scroll machinery calls `setNeedsDisplay:` for newly exposed areas. Banner x-positions are recomputed from `[self visibleRect]` on every draw, so they reposition naturally.

**Horizontal scroll**: Requires explicit invalidation. When `NSScrollView` scrolls horizontally with `copiesOnScroll`, it blits existing pixels sideways — banners drawn at the old right edge are baked into wrong positions in the blitted content. The fix:

Register for `NSViewBoundsDidChangeNotification` on `self.enclosingScrollView.contentView` (the clip view). In the handler, call `[self setNeedsDisplay:YES]` to redraw the full visible area. This is acceptable because:
1. Horizontal scroll is rare in a code editor (most code fits the viewport width).
2. Full redraw on horizontal scroll matches what vertical scroll already does for newly exposed areas.
3. Attempting surgical invalidation (tracking old vs new banner positions to avoid full redraw) adds complexity for a case that rarely fires.

**Registration timing** (important — two different lifecycle points):

```objc
// 1. LSPDiagnosticsDidChangeNotification — register ONCE in initWithFrame:
//    (NOT in setDocument:, which is called on every document switch and would accumulate observers)
//    The handler already filters by URI, so it naturally handles document switches.
- (instancetype)initWithFrame:(NSRect)frameRect
{
    // ... existing init code ...
    [NSNotificationCenter.defaultCenter addObserver:self
        selector:@selector(lspDiagnosticsDidChange:)
        name:LSPDiagnosticsDidChangeNotification
        object:nil];
}

// 2. NSViewBoundsDidChangeNotification — register in viewDidMoveToWindow
//    (enclosingScrollView is nil in initWithFrame: — the view isn't in the hierarchy yet)
- (void)viewDidMoveToWindow
{
    // ... existing code ...
    if(self.window)
    {
        [NSNotificationCenter.defaultCenter addObserver:self
            selector:@selector(diagnosticScrollBoundsDidChange:)
            name:NSViewBoundsDidChangeNotification
            object:self.enclosingScrollView.contentView];
    }
    else
    {
        [NSNotificationCenter.defaultCenter removeObserver:self
            name:NSViewBoundsDidChangeNotification
            object:nil];
    }
}

- (void)diagnosticScrollBoundsDidChange:(NSNotification*)notification
{
    if(_diagnosticBannersVisible)
        [self setNeedsDisplay:YES];
}
```

Both observers are cleaned up by the existing blanket `removeObserver:self` in `dealloc` (OakTextView.mm:688).

### Hit Testing — mouseDown Integration

At the **top of `mouseDown:`** in OakTextView.mm (after the `ignoreMouseDown` check, before fold/macro handling):

```objc
// In mouseDown: (OakTextView.mm), early exit for diagnostic banner clicks
if([self handleDiagnosticBannerClickAtPoint:[self convertPoint:[anEvent locationInWindow] fromView:nil]])
    return;
```

The category method `handleDiagnosticBannerClickAtPoint:`:
1. Iterates `_diagnosticBannerRects` (cached from last draw)
2. If point hits a banner rect, extract the diagnostic's range (startLine:col → endLine:col)
3. Call `requestCodeActionsForDocument:line:character:endLine:endCharacter:completion:` with the **diagnostic's range**, not the current selection
4. In the completion block (dispatched to main queue): if `actions.count == 0`, return silently. Otherwise, call `[self showCodeActionsMenu:actions]` (existing method, reused from `lspCodeActions:`). Position the menu at the banner rect's origin.
5. Return `YES` to consume the click

```objc
- (BOOL)handleDiagnosticBannerClickAtPoint:(NSPoint)point
{
    for(auto const& banner : _diagnosticBannerRects)
    {
        if(!NSPointInRect(point, banner.rect))
            continue;

        NSDictionary* diag = banner.diagnostic;
        NSUInteger line = [diag[@"line"] unsignedIntegerValue];
        NSUInteger col  = [diag[@"character"] unsignedIntegerValue];
        NSUInteger endLine = [diag[@"endLine"] unsignedIntegerValue];
        NSUInteger endCol  = [diag[@"endCharacter"] unsignedIntegerValue];

        __weak OakTextView* weakSelf = self;
        [[LSPManager sharedManager]
            requestCodeActionsForDocument:self.document
            line:line character:col endLine:endLine endCharacter:endCol
            completion:^(NSArray<NSDictionary*>* actions) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    OakTextView* strongSelf = weakSelf;
                    if(!strongSelf || !actions.count)
                        return;
                    [strongSelf showCodeActionsMenu:actions];
                });
            }];
        return YES;
    }
    return NO;
}
```

This differs from `lspCodeActions:` which uses `documentView->ranges().last()` (current selection). The banner click bypasses that and passes the diagnostic's own range directly to LSPManager.

### Ghost Text Interaction

When Copilot ghost text is active (`_ghostText != nil`):
- **Lines above cursor line**: Unaffected. Tints and banners draw correctly.
- **Lines below cursor line**: These are visually shifted down by `_ghostTextExtraHeight` during Pass 2 of `drawRect:` via `CGContextTranslateCTM`. However, that transform is inside a `CGContextSaveGState`/`RestoreGState` block — by the time our diagnostic draw calls execute (after the ghost text if/else block), the context is restored to its original unshifted state. This means tints and banners for lines below the ghost text will be positioned at their **pre-shift** coordinates, misaligned with the shifted text.
- **This is a pre-existing limitation.** The existing definition highlight (line 941) and hover highlight (line 958) in `drawRect:` have the same issue — they also draw in unshifted coordinates after the ghost text block. Our diagnostic rendering matches this existing behavior. Fix deferred — if ghost text positioning is corrected for highlights generally, diagnostics benefit automatically.
- **Ghost text line itself**: If a diagnostic exists on the cursor line, the banner draws at the correct position since the cursor line itself is not shifted (only lines below are).

### Document Lifecycle

- **Document switch** (user opens different file in same tab): `setDocument:` is called. The diagnostic notification handler filters by URI, so the old document's diagnostics stop rendering immediately. Next `drawRect:` queries LSPManager for the new document's diagnostics.
- **Document close**: LSPManager sends `textDocument/didClose`, server stops sending diagnostics. `_diagnosticsByURI` entry removed. Nothing to clean up in OakTextView — no stored state to invalidate.
- **Notification unregistration**: Add `removeObserver:` for `LSPDiagnosticsDidChangeNotification` in `dealloc`. (Follow existing pattern — check how other notification observers are handled in OakTextView.)

### Notification Filtering

Every OakTextView instance receives every `LSPDiagnosticsDidChangeNotification`. The handler must filter:

```objc
- (void)lspDiagnosticsDidChange:(NSNotification*)notification
{
    NSString* uri = notification.userInfo[@"uri"];
    OakDocument* doc = self.document;
    if(!doc || !doc.path)
        return;
    NSString* myURI = [NSURL fileURLWithPath:doc.path].absoluteString;
    if(![uri isEqualToString:myURI])
        return;

    _diagnosticBannerRects.clear(); // invalidate hit-test cache
    [self setNeedsDisplay:YES];
}
```

### Line Number Indexing

LSP uses **0-indexed** lines. The existing `didReceiveDiagnostics:` in LSPManager extracts `diag[@"line"]` which is already 0-indexed (set by LSPClient at line ~985). Buffer operations like `_buffer->begin(line)` also use 0-indexed lines. All internal handling uses 0-indexed lines. Display conversion to 1-indexed only happens in GutterView's `DrawText` call (adds 1 for display). No conversion needed in our rendering code.

### Banner Text Truncation

- Maximum banner width: 40% of `NSWidth([self visibleRect])`, capped at 400pt
- Text that exceeds this width is truncated with `NSLineBreakByTruncatingTail`
- Use `NSAttributedString` with `NSParagraphStyle` for truncation, drawn with `drawInRect:`
- Font: system font at editor font size minus 1pt (smaller than code, clearly distinct)

### Files to Modify

| File | Change |
|---|---|
| `Frameworks/OakTextView/src/OakTextView+Diagnostics.mm` | **New.** Category with `drawDiagnosticTints:inRect:`, `drawDiagnosticBanners:inRect:`, `handleDiagnosticBannerClickAtPoint:`, `lspDiagnosticsDidChange:`, `groupedDiagnosticsForVisibleRect:`. |
| `Frameworks/OakTextView/src/OakTextView+Diagnostics.h` | **New.** Category header declaring public methods. |
| `Frameworks/OakTextView/src/OakTextView_Private.h` | Add ivars: `_diagnosticBannerRects`, `_diagnosticBannersVisible`. |
| `Frameworks/OakTextView/src/OakTextView.mm` | (1) `#import "OakTextView+Diagnostics.h"`. (2) In `drawRect:`, add `[self drawDiagnosticTints:context inRect:aRect]` and `[self drawDiagnosticBanners:context inRect:aRect]` AFTER the ghost text if/else block (~line 939), BEFORE definition/hover highlights. (3) In `mouseDown:`, add early-exit call to `handleDiagnosticBannerClickAtPoint:` after the `ignoreMouseDown` check. (4) Register `LSPDiagnosticsDidChangeNotification` in `initWithFrame:`. (5) Register `NSViewBoundsDidChangeNotification` (clip view) in `viewDidMoveToWindow`. Both cleaned up by existing blanket `removeObserver:self` in `dealloc`. |
| `Frameworks/lsp/src/LSPManager.mm` | Add `diagnosticsForDocument:` method returning cached `_diagnosticsByURI` array. Ensure `code` and `range` fields from LSP response are preserved in the cached dicts (verify they already are). |
| `Frameworks/lsp/src/LSPManager.h` | Declare `diagnosticsForDocument:`. |
| `Frameworks/OakTextView/CMakeLists.txt` | Add `OakTextView+Diagnostics.mm` to sources. |
| `Frameworks/Preferences/` | Add the toggle checkbox to the Advanced pane, under Editor. |

### Configuration

One global user default, `lspShowInlineDiagnostics`, read straight from `NSUserDefaults` — no `.tm_properties` override, and so no `settings_for_path` call in the draw path. It is exposed twice: a "Show inline diagnostics" checkbox in Preferences → Advanced, alongside the other text rendering toggles, and a Show/Hide item in Edit → LSP.

### Performance Considerations

- Only render diagnostics for lines intersecting `aRect` (the dirty rect).
- No parallel data store — query LSPManager and group ~100 items on the fly. Negligible cost.
- Horizontal scroll triggers full `setNeedsDisplay:YES`, acceptable since horizontal scroll is rare in code editors.
- `_diagnosticBannerRects` cached per draw cycle — hit testing is a simple array scan (~20 items max visible).
- `setNeedsDisplay:YES` on diagnostic notification is acceptable — it redraws the visible area, same cost as any edit.

### Open Questions

- Exact pill visual design (corner radius, padding, icon glyph) — decide during implementation.
- Whether squiggly underlines can be trivially lifted from the fork — research during implementation.
- Alpha values for tint and banner backgrounds — tune during implementation with both light and dark themes.
- Fade-in animation — try after instant rendering works.

## Estimated Scope

~400-600 lines of new code across the new category file, plus ~20 lines of integration in OakTextView.mm and ~10 lines in LSPManager. Medium complexity. Primary risk is coordinate math for right-edge banners with soft-wrap. Known limitation: diagnostic overlays on lines below active ghost text will be vertically misaligned (pre-existing issue shared with definition/hover highlights).
