# Genie 0.10-beta (2018-05-08)

* It’s now possible to debug data sources: Open preferences and select a data source (spotlight, sqlite3, or custom script), now choose Data Source → Dry Run (⌘R) from the menu.

	The Results tab will show the parsed output as an outline where the Console tab will show the raw output (from custom script data sources only).

	There is currently no way to provide arguments (like `${query}`) when doing a dry run, and variables are not expanded, e.g. a database path using `$HOME` will fail when executed from the preferences window.

# Genie 0.9 (2018-04-29)

* Fix issue with rendering UI in standard resolution (and scaled) on retina screens.

# Genie 0.8 (2018-04-28)

* Fix crash on macOS 10.13.

# Genie 0.7 (2018-04-28)

* Selecting multiple items and pressing return will now look at the item’s type and arguments:

    1. Non-script items are executed one-by-one, e.g. selecting multiple URLs and pressing ↩ will open them all.

	2. For items using the same script with identical arguments, only the first item is executed, but the other items are available (as JSON) via STDIN. An example of this is iTunes → All Artists where the user can select multiple songs, and only one instance of the script is executed, which creates a playlist using all selected items (read from STDIN).

	3. Script items with different arguments (but possibly same script) will be executed one-by-one, an example of this would be «Application» → Recent Documents. Here each document item uses the same script (`/usr/bin/open -b "$0" "$1"`) but the second argument (the file to open) is different for each item, therefor Genie will run `/usr/bin/open` for each selected document.

* Changed the main UI: This is still work in progress, as we may need a more customized text field plus a visual effects view to get blurred background, custom drawn icons instead of the non-scalable gear and magnifying glass, etc.
