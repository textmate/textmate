# Theme Editor icon

The flower comes directly from `Applications/TextMate/icons/TextMate.icns`.
It is not regenerated, resized, recolored, or redrawn. `Pencil.png` is a separate
transparent overlay, composited over the top-right corner at each original
TextMate icon size. The flower remains pixel-for-pixel identical everywhere
outside that overlay.

At runtime, the app uses `NSWorkspace`'s rendered bundle icon, matching TextMate's
system-provided framing and padding. Assigning the raw `.icns` image directly to
`NSApp.applicationIconImage` bypasses that presentation and makes the flower look
larger in the Dock, even when the source pixels match.
The rendered image is requested at 512 points before assignment so AppKit keeps
a full 1024-pixel Retina representation instead of snapshotting a 64-pixel thumbnail.

`ThemeEditor.png` is the full-resolution preview; `ThemeEditor.icns` is bundled
with the application. Rebuild both from the repository root with:

```sh
swift Applications/ThemeEditor/resources/build-icon.swift
```

The script extracts the original icon representations, composites the pencil,
and verifies every decoded pixel outside the overlay against the original
before packaging. A successful run reports the number of unchanged pixels.

## Pencil generation

Only the pencil was created with the built-in image-generation tool. Prompt:

Create a single isolated realistic yellow wooden pencil on a genuinely transparent RGBA background. This is an overlay asset for an existing macOS app icon; do not include any flower, app tile, badge, scenery, text, or other objects. Square canvas. Pencil runs diagonally at 45 degrees, graphite tip at (12%, 88%), pink eraser at (88%, 12%). Entire pencil inside canvas, no cropping. Slim but clearly readable yellow hexagonal shaft, exposed pale wood and sharp dark graphite tip, silver ferrule and pink eraser. Width of pencil approximately 13% of canvas. Clean crisp anti-aliased edges, subtle realistic volume, no cast shadow outside the pencil. Everything except the pencil is fully transparent. Do not include any colored flecks or remnants in the transparent space.
