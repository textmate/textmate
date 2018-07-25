# Changes

## Genie 0.15-beta (2018-07-25)

* Genie now includes the default set of items. Any changes you make to items are saved as `~/Library/Application Support/Genie/Custom.genieItems` (saving only the changes).
* If there is no `Custom.genieItems` then Genie will still load the old `Items.plist`, this is incase you have changes that you wish to keep. If this is the case, do the following to make sure `Custom.genieItems` becomes a delta against the latest version if `Items.plist`:

    1. Quit Genie
    2. Delete any existing `Custom.genieItems`
    3. Git update `~/Library/Application Support/Genie`
    4. Launch Genie
    5. Open Genie Preferences
    6. Close Preferences → This should save a new `Custom.genieItems`
    7. You can now delete all files from `~/Library/Application Support/Genie` except `Custom.genieItems` and possibly `ClipboardHistory.db`

    If you do not have customizations (that you wish to keep), just do:

    1. Quit Genie
    2. Delete `~/Library/Application Support/Genie`
    3. Launch Genie

* Fixed issue with stale results from script data source that had no expiration or dependency (these should expire when closing the window, but did not).

* Fixed exception if using malformed predicate strings for Spotlight Query or Predicate Group items.

* I forgot to include changes in the last release: Major change is that there is now documentation (in the Preferences window), which you may have noticed. In the documentation you can read about how JavaScript can now run shell commands (new in last build). This is currently used by _Set Timer_ where it is now possible to click a timer to cancel it.

* Also added in last build are CSS variables for `textColor`, `labelColor`, `secondaryLabelColor`, `tertiaryLabelColor`, and `quaternaryLabelColor`. These should be used instead of hardcoded colors in anticipation of darkmode support. The _Show Calendar_ makes use of these (and now includes week numbers).

## Genie 0.12-beta (2018-06-24)

* Improved preferences window, you can now:

	- Set the shortcut to bring up Genie.
	- Set for how long clipboard history should be kept. If you want a duration that is not in the pop-up then you can use `defaults write com.macromates.Genie clipboardHistoryExpireAfter «value»` where `«value»` should be a number followed by `hours`, `days`, `months`, or `years`, e.g. `12 hours`.
	- Set if applications should opt-out of participating in clipboard history. Defaults to Keychain Access.

	There are still some minor quirks to work out. For example recording a new shortcut key may bring up Genie if you pick the same key and checking for updates will bring up a non-focused dialog. Both issues are because Genie and the preferences window are separate applications.

* The Dry Run feature in preferences (⌘R) now show console output and potential error.

* You can now bind parameters to sqlite3 queries. Use `?` in the query itself and then provide the value via the “Bindings” table below the query.

* Items with (inline) HTML output can now access the query string as `genie.query` (from JavaScript) and a `querychange` event is fired when the query string is changed (though the event target is currently `window`). It is also possible to write to the `genie.query` property, although currently the filter string (shown to the user) is not updated (but may be in the future). The “Show Calendar” item has been updated to make use of this, so it is pure HTML/JavaScript. There may still be some flashing when the filter string changes, this is due to how view-based table views do not always re-use same views for same rows when content changes, so a new `WKWebView` is created. It should however be possible to address.

* Items can now set `acceptsQuery = :true;` (using the “Advanced…” button). Normally Genie detects this automatically by looking if the title, script arguments, etc. reference the `${query}` variable, but items with inline HTML can access this via JavaScript, which Genie won’t (currently) detect.

## Genie 0.11-beta (2018-06-04)

This version is a major refactoring/rewrite: Be sure to update `Application Support/Genie` from GitHub.

### Improvements

- You can use ⌃↑ or ⌃↓ to scroll through filter string history. This is mainly if you do some complex calculation, close Genie, and wants to get back to the calculation and augment it.

- There is a spinner to show when data sources are active. Currently it shows immediately, so it may cause a bit of flicker for short-lived data sources. I have kept it like this for debugging/diagnostics (being sure something is running or *not* running). Eventually it’ll be made to start after a short delay.

- The Show Calendar item has arrows to go to previous/next month.

- Performance for Dry Run in Genie Preferences should be much better, although currently there is no console output for script data sources.

### Regressions

- Alternate actions are currently not supported (these appeared when holding down option), I plan to bring them back but in a slightly different form, as I want this to work with predicate groups.

- Variables are no longer recursively expanded. E.g. before you could set an item’s title to `${foo}` then set the variable `foo` to `${bar}` etc. and Genie would follow it until it reached a literal value (or detected a loop). There are two reasons I changed this:

	1. Values coming from sqlite3 or spotlight should *not* be further expanded, and with the previous system there was no way to indicate that a value is a literal string rather than a format string.
	2. Values are transformed based on the field being expanded, e.g. when an item’s URL property uses variables, the value is URL-encoded, and if title or subtitle references `${file}` then the path will be abbreviated with a tilde (when appropriate), etc. When supporting recursive expansion, these transformations became a little opaque, because some you’d want inherited, others not.

## Genie 0.10-beta (2018-05-08)

* It’s now possible to debug data sources: Open preferences and select a data source (spotlight, sqlite3, or custom script), now choose Data Source → Dry Run (⌘R) from the menu.

	The Results tab will show the parsed output as an outline where the Console tab will show the raw output (from custom script data sources only).

	There is currently no way to provide arguments (like `${query}`) when doing a dry run, and variables are not expanded, e.g. a database path using `$HOME` will fail when executed from the preferences window.

## Genie 0.9 (2018-04-29)

* Fix issue with rendering UI in standard resolution (and scaled) on retina screens.

## Genie 0.8 (2018-04-28)

* Fix crash on macOS 10.13.

## Genie 0.7 (2018-04-28)

* Selecting multiple items and pressing return will now look at the item’s type and arguments:

    1. Non-script items are executed one-by-one, e.g. selecting multiple URLs and pressing ↩ will open them all.

	2. For items using the same script with identical arguments, only the first item is executed, but the other items are available (as JSON) via STDIN. An example of this is iTunes → All Artists where the user can select multiple songs, and only one instance of the script is executed, which creates a playlist using all selected items (read from STDIN).

	3. Script items with different arguments (but possibly same script) will be executed one-by-one, an example of this would be «Application» → Recent Documents. Here each document item uses the same script (`/usr/bin/open -b "$0" "$1"`) but the second argument (the file to open) is different for each item, therefor Genie will run `/usr/bin/open` for each selected document.

* Changed the main UI: This is still work in progress, as we may need a more customized text field plus a visual effects view to get blurred background, custom drawn icons instead of the non-scalable gear and magnifying glass, etc.
