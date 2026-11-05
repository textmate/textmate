# TextMate Bundle System

## Bundle Loading Order (locations.cc)

Bundles are loaded from these paths in order (lower index = higher priority):

0. `~/Library/Application Support/TextMate/` (user customizations)
1. `~/Library/Application Support/TextMate/Pristine Copy/`
2. `~/Library/Application Support/TextMate/Managed/` (downloaded bundles from bundle server)
3. `/Library/Application Support/TextMate/`
4. `/Library/Application Support/TextMate/Pristine Copy/`
5. `<app>/Contents/SharedSupport/` (bundled with app)

## Bundle Conflict Resolution (load.cc)

- If a bundle UUID is already loaded from a higher-priority location, the same UUID at a lower-priority location is **skipped** ("eclipsed").
- Delta bundles (`isDelta: true`) at higher priority are stored, then merged onto the base bundle found at a lower-priority location.
- A patched bundle in SharedSupport (index 5) will NOT override the managed copy (index 2). A delta at index 5 is also too late.
- To override a managed bundle, the patched version must be at index 0 or 1.

## URL/Link Detection in OakTextView

- `OakTextView.mm` line ~1525: `-[OakTextView links]` builds clickable links by scanning for `markup.underline.link` scope from the grammar/syntax parser.
- The scope is applied by the **Hyperlink Helper** bundle grammar (`Hyperlink.tmLanguage`).
- The grammar regex uses Onigmo with `ONIG_ENCODING_UTF8`, so Unicode-aware patterns work fine.

## Key Files

- `Frameworks/bundles/src/locations.cc` — bundle search paths
- `Frameworks/bundles/src/load.cc` — bundle loading, conflict resolution, delta merging
- `Frameworks/OakTextView/src/OakTextView.mm` — link detection via grammar scopes
