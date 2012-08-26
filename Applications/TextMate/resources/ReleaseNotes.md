# Release Notes

## News

Not only are we back to frequent updates, but I have also started blogging about TextMate features again, so if you aren’t watching [the blog](http://blog.macromates.com/) then I recommend you do.

## 2012-08-26

* Add context menu to tab bar. Actions include creating new tab (can also be done by double-clicking empty space in a tab bar), tearing off tabs (can also be done by double-clicking a tab), closing other tabs (can be done by option-clicking the tab’s close button), and closing tabs to the right.

* Added badges for Subversion status. This is based on the `svn` executable rather than `libsvn`. Set `TM_SVN` to a version of `svn` compatible with your repository format. Note that if your Mac username is “long” then the driver currently fails to parse the output from `svn`. *[Jeremy Whitlock]*

* Add a few new options to Project Preferences (previously these could only be enabled via `defaults write`).

* All items in the Preferences now work, well, except for _Projects → Display → All filename extensions_.

* When changing settings, we now save them to `~/Library/Application Support/TextMate/Global.tmProperties`. This is a regular properties file (like `~/.tm_properties`) and it will try to be “smart” about how to record your preferences:

	 - If you change theme, font, wrap column, or whether or not to show the wrap column, then we store it at the root of this properties file, so it applies to all files.
	 - If you change tab size, soft tabs, soft wrap, or show invisibles, then we save it for the current file type, potential parent types (e.g. `text` is a parent type of `text.plain`), and globally. This makes it easy to set different tab sizes for different languages, or enable soft wrap for text files.
	 - If you change spell checking or spelling language, we store this for the current file’s path, its type, parent types, and globally.

	Some of the settings in the Preferences window are also stored in `Global.tmProperties` and if you switch file type for an open file, we also record that in this file (by adding a section for a file glob matching the current file’s extension, and setting the file type in this section).

	We encourage you to tweak your settings and then inspect this file, as it’ll give you a feel for the possibilities.

* Theme colors are now transparent against the background. Previously colors with alpha would be blended with a previous color setting for the same element, which was rarely desired. This mainly applies to color of invisibles and selection (which now should match the rendering done by TextMate 1.x).

* Further improvements to theming the gutter. There are now the following keys which can be set to change the color of gutter elements: *[Dennis Vennink]*

	- `gutterDivider`: Border between text view and gutter.
	- `gutterForeground`: Text color.
	- `gutterBackground`: Background color.
	- `gutterIcons`: Color of the images in the gutter.
	- `gutterSelectionForeground`: Text color for lines containing caret / part of a selection.
	- `gutterSelectionBackground`: Background color for lines containing caret / part of a selection.
	- `gutterSelectionIcons`: Color of images on lines containing caret / part of a selection.
	- `gutterSelectionBorder`: Border between selected and non-selected lines.

* Changed default font to Menlo, 12 pt. Although this has been on the system for quite some time, applications which don’t use the 10.8 SDK will still receive Monaco as the default monospaced font. Unfortunately this means by default users wont see bold and italic text, as Monaco has no such variants, and CoreText will not synthesize these styles.

* If shrinking HTML output view to (below) zero points, the view is now back again after relaunch (temporary fix for the lack of constraints on sizing views in the main window) *[Lukasz Czekaj]*

* Add scope attributes for Ant, CMake, Maven and Scons projects. *[Michael Sheets]*

## 2012-08-22

* You can now ⌘-click icons in the file browser for “Show in Finder” *[Jacob Bandes-Storch]*
* Bundle editor now remember column widths and item titles are truncated with ellipsis *[Gerd Knops]*
* File browser now remember it’s width *[Gerd Knops]*
* You can now also set `gutterSelectionBackground` in a theme (or as a global style) to affect gutter rendering *[David Howden]*
* Suppress borders and disallow horizontal scroll for Find in Folder results *[Jacob Bandes-Storch]*
* Add missing retina images *[Paul Wilde]*
* Fix issue with gutter’s border being drawn behind incremental search (and disappearing).
* Yesterday’s build was missing “double-click bundles and bundle items to install” functionality.

## 2012-08-21

* Save dialogs now have controls for setting encoding, line endings, and whether or not to use a byte order mark (for unicode encodings).

* When the find window is active (with find in folder results) one can use one of the View → Toggle Foldings At Level actions to collapse/expand the results. This was done to allow collapsing using the keyboard (e.g. use ⌥⌘1).

* When in a snippet placeholder field, deleting the content and pressing tab will advance to next field even if the word to the left of the caret is a tab trigger. An example of this is `div⇥` in HTML mode, if pressing ⌫ after expanding it, pressing ⇥ would expand another `div` snippet. When at the end of a snippet, it will allow the word to the left of the caret to trigger a tab expansion, as this is used to chain snippets.

*	Colors used in the gutter can now be set in the theme using the following 3 keys:

	* `gutterBackground`
	* `gutterForeground`
	* `gutterDivider`

	Default colors are calculated, but might not be ideal for all themes (e.g. solarized) *[Jacob Bandes-Storch]*

* Icons in the gutter has been updated for retina and images changed so the bookmark indicator is a bookmark and the “found a match here” (for search in folder) is now a magnify glass. *[Dennis Vennink]*

* Last version of `rmate` didn’t preserve file permissions when editing existing files. This has now been addressed.

* Several users reported a socket error during startup. This happened after we moved the socket into `TMPDIR` so it has now been moved back to `/tmp` but the name include the user ID to avoid clashes on multi-user systems.

* TextMate will now update the installed version of `mate` if TextMate.app includes a newer version. This might result in prompting for admin password after updating TextMate.

## 2012-08-19

* Various improvements in paragraph definition and wrapping:

    - A paragraph (the input for reformat/justify and select paragraph) is now extended up and down until there are blank lines (previously only the current (hard) line was considered part of the paragraph).
    - Paragraphs can now be selected with ⌃⌥P
    - Reformat/justify a paragraph (⌃Q or ⌃J) no longer leave trailing whitespace after the lines.
    - Wrap column is no longer hardcoded to 80 characters.
    - Characters with the _East Asian Width_ (unicode) property set now count as two columns wide.

* Selecting _Use Selection for Find/Replace_ with a discontinuous or column selection now use just the first range/row.
* Go to Current Document (⌃⌘R) is now a toggle.
* Go to SCM Status (⇧⌘Y) is now a toggle.
* When showing SCM Status (⇧⌘Y), we no longer show folders in the _Uncommitted Changes_ if items from these are also shown. In the _Untracked Items_ we don’t show items as untracked if an ancestor folder is also included.
* Find in Folder: Pressing ⌘1-⌘9 will select first match of the n’th file in the results list. Previously it only selected the n’th file (and you had to use arrow down to actually get to the match).
* Favorites prefixed with ‘[DIR]’ show folders within.

	For example if you have all your projects under `~/Projects` then you can do:
    
	    mkdir -p ~/Library/Application\ Support/TextMate/Favorites
	    cd ~/Library/Application\ Support/TextMate/Favorites
	    ln -s ~/Projects "[DIR] My Projects"
    
	This will then have all the folders inside `~/Projects` show when you choose Open Favorites… (⇧⌘O).

* When TextMate launches it creates a socket for `mate` to connect to. There have been [a few reports][1] of TextMate reporting a problem creating this socket. We now also show an error if an old socket exist and we are unable to delete this old socket. If you are seeing an issue, please don’t just add “it fails for me too”. Instead provide as much information as possible, and if e.g. it says it failed to delete the socket, fire up Terminal and check the file flags (`ls -l`) and try to delete it from Terminal.
* Fixed: Keys on the numeric keypad can be used to trigger bundle actions (not explicitly bound to the numeric keypad). This fix removes the distinction between regular keys and the numeric keypad so it is no longer possible to bind (only) to numeric keypad keys.
* Include high resolution (white) I-Beam cursor. *[Jacob Bandes-Storch]*
* Several HiDPI assets added. *[Paul Wilde]*
* Border between text view and gutter is now “fixed” (when invoking the elastic “scroll beyond the document bounds”). *[Jacob Bandes-Storch]*
* Fix bundle menu items' appearance in the Help menu. *[Jacob Bandes-Storch]*
* Bundle Editor: If you disable an item, you can now actually enable it again. Disabled items are rendered in grey. *[Gerd Knops]*
* Bundle Editor: Columns are now resizable. *[Elia Schito]*

[1]: https://github.com/textmate/textmate/issues/183

## 2012-08-15

* Minor tweak to how pasting works. Previously if you did a multiline selection and on the last line selected to the end of the line but excluded the actual newline, copy and pasted that somewhere else, it would treat it as you had also copied the newline. This is now only the case when you are pasting on a line that is not empty.

* Unless you disable session restore, relaunching as part of software update no longer ask you about unsaved documents.

* Preferences icons updated for HiDPI displays. *[Paul Wilde]*

* rmate: Better file saving strategy. *[OZAWA Sakuro]*

* rmate: You can now store options in `/etc/rmate.rc` or `~/.rmate.rc`. Example below. *[Nicolas Ledez]*

		host: localhost
		port: 52698

* Improve tab trigger boundary checks. Previously a tab trigger had to be preceded by a non-word character (where underscore is considered a word character). Now we instead require that the previous character belong to a different character class (as defined by scoped settings) or has another “is alpha numeric”-state.

* Split `disableIndentCorrections` into two settings:

	The `disableIndentCorrections` setting disables the (aggressive) indent corrections that TextMate does while you type.
    
	An additional `indentOnPaste` setting controls how to indent when pasting and can be set to:
    
	1. `simple` — this is the indent behavior which was previously implied when setting `disableIndentCorrections`. It indents the paste to the position of the caret and works well for Python.
	2. `disable` — the text is inserted as-is without indenting it.
	3. «unset» — indent the paste based on the indent patterns of the current scope.

## 2012-08-14

* Pressing ⌥F2 with focus in the file browser now show the context menu.
* The `TM_DROPPED_FILE` variable had a path relative to project directory instead of current file. This would make some drop commands insert wrong path.
* Dropping binary files onto the text view for which there is no drop command will now insert the path for the file dropped. Hint: You can hold down control (⌃) when dropping text files to get the “insert path” behavior.
* The `--host=auto` option to `rmate` didn’t work. *[Timothy Andrew]*
* Added simplified Chinese localization for interface files. *[Bo Xiao]*
* Bundle items with both a tab trigger and key equivalent now show both. *[Adam Strzelecki]*
* The bundle menu in the status bar can now be opened via ⌥F1 and will default to select the bundle for the current language. *[Adam Strzelecki]*
* Further fixes to handling of txmt: URLs without a file argument.

## 2012-08-13

* Consecutive deletes extend the yank clipboard.
* Tab triggers are once again rendered in the menu (though presently without the rounded rectangle) and all key equivalents are now shown menu (e.g. `⌃!` would previously not show). Also several legacy APis have been updated to the latest from Apple *[Jacob Bandes-Storch]*
* When clearing the “CJK edit buffer” (e.g. via escape) then the following key press would be treated literally, e.g. pressing escape or delete would insert the character code for this key.
* Fixed crash when setting a non-path for `projectDirectory` and then doing a project folder search.
* For users of proxy auto-configuration (PAC) scripts, the actual URL TextMate needs to connect to (for software and bundle updates plus crash reporting) is now given to the PAC script.
* Fix crash for proxy users (introduced in last build).

## 2012-08-12

* Fix problem with pressing return, tab, or escape after inserting CJK.
* Add bundle menu to the status bar. *[Elia Schito]*
* Changed Find/Replace combobox height to 21, to be aligned with other controls. *[Bo Xiao]*

## 2012-08-11

* Implement ⌃Y for yanking last deletion. *[Nathaniel Tagg]*
* Added fullscreen behavior to document window and View → Enter/Exit Full Screen menu item. *[Jesse B. Hannah]*
* When closing last document (except empty untitled ones) for a window with a visible file browser, we now keep the window around *[Mads Hartmann Jensen]*
* If TextMate was launched via `git commit` then the Git bundle would have issues (since it would inherit a wrong `GIT_DIR` environment variables)
* Preliminary support for installing bundles and bundle items (including themes) via double-click (from Finder). You can hold down option (⌥) if you wish to open them as folders/property lists.

## 2012-08-10

The source for TextMate 2 is now [available at GitHub][1] under a GPL 3 license. There is an [interview at Ars Technica][2] that gives some background about what motivated this decision.

[1]: https://github.com/textmate/textmate
[2]: http://arstechnica.com/apple/2012/08/odgaard-i-will-continue-working-on-textmate-as-long-as-i-am-a-mac-user/

* TextMate 2 now require 10.7
* When opening an already open folder (e.g. `mate .`) the window with that folder is brought to front *[Adam Strzelecki]*
* When no URL is given in a `txmt` URL then current document is used (this affected e.g. clicking errors in a ruby stack dump when running untitled files) *[Gerd Knops]*
* Opening `txmt` URLs with escaped spaces no longer fail *[Gerd Knops]*
* git: If a folder contains files with mixed status, the folder itself now gets a badge *[Gerd Knops]*
* SCM badges are now enabled for disk images *[Gerd Knops]*
* Scope selectors now support anchoring matches by using `^`, `$`, and `>` to anchor to either the first, last, or previous scope element *[Joachim Mårtensson]*
* Subversion support (badges) has been disabled. The `libsvn.a` used was an older version which didn’t support the most recent repository format, and building the latest version gave rise to a few problems. Moving forward, I think subversion support should be re-added by calling the `svn` shell command to obtain status (this is the approach taken for both git and mercurial).
* Absolute paths in the “Find in Folder” results now use the name of the disk instead of `/` as first element
* git: Updated driver to work with the new way submodules have their metadata in the super project.
* Tab triggers are no longer rendered in the menu. This is caused by the switch to the 10.7 SDK which no longer allow the old way of rendering these items. Replacement code is of course planned.
* Projects with a `*.ninja` file now get the `attr.project.ninja` scope.
* Sections in `.tm_properties` files can now target multiple globs/scopes by separating them with a semi-colon, e.g.:
	
		[ source.ruby; *.rb ]
		tabSize = 2

## 2012-07-09

* Signed for Gatekeeper.
* Using ⌃K at the end of a line now deletes the newline.
* Implement `moveToBeginning/EndOfParagraphAndModifySelection:` selectors. The default key bindings have ⌃⇧E extend selection to end of paragraph (so was previously a no-op in TM).
* The `.tm_properties` parser no longer require spaces around the equal character nor is there a need to quote section names when they contain special characters, etc. The full grammar can be found in the help book.
* Disable font ligatures. Using the Mensch font on Lion (but not Snow Leopard) would previously render ‘fi’ using the ‘fi’ ligature.
* Fix drawing issues for Retina Mac Books (horizontal lines would appear while scrolling).
* The Select Bundle Item window now use the same key equivalent control as the bundle editor.
* Improve keyboard usage with key equivalent control. You can tab to/from the control, when the control has focus you can use forward/backward delete to remove the current key equivalent or space to record a new one. Clicking the control using the mouse will go directly to record mode.
* When closing the preferences window pending edits of the variables will be committed.
* If a bundle item contained an empty string for key equivalent, the bundle editor would crash when showing that item.
* When recording a key equivalent in the bundle editor, global hotkeys are disabled. This means you can bind to keys that may not actually work on your system (because another application or system service is swallowing the event).
* Commands which use ssh should now work even if the private key has a passphrase (e.g. the Subversion commands).
* Improve clicking various UI elements (e.g. the close button in tabs) for inactive windows.
* Add clear button to key equivalent recorder.
* Fix file browser crash when duplicating files which had a suffix one character longer than the base name.
* Support services.
* Request administration rights when required by the software update process.
* Fix missing repeat events (on Lion) for characters with no potential diacritics.
* Fixed crash resulting from clicking and holding mouse exactly on a caret, starting a drag operation and dropping the (empty) selection.
* Show dialog for (some) keychain failures. The license key is stored in and retrieved from the keychain, so errors will cause TextMate to not run. I suspect there is an issue where TextMate “loses” its signed state and is denied access to the keychain, as a few users have reported sudden problems which were resolved simply by moving TextMate on disk.
* When switching tabs, selection was drawn as if in a background window.

## 2012-02-27

* Restore UI sounds for 10.7.
* Bundle editor now use a key equivalent recorder (instead of text field).
* Fixed issue with involuntary folder expansion when using ⌥⌘N with focus in file browser (missed by previous “fix”).
* Dropped support for Mac OS X 10.5.

## 2012-02-18

* Improved SCM workaround for Mercurial issue — previous workaround could leave SCM status outdated, this should no longer be the case.
* Fix problem with items that differed only in capitalization. These were collapsed into a single item in several UI menus/dialogs, e.g. Open Favorites.
* Fix bad encoding when copying Find in Folder / Find All results.
* Using Go to Counterpart no longer include files matched by the effective `binary` or `exclude` glob from `.tm_properties`.
* Changed `binary` setting in `.tm_properties` from a boolean to a glob. This allows to set a global binary glob instead of needing to set the boolean for specific settings (which is more complex to manage via a GUI).
* Introduce `attr.file.unknown-type` and `attr.file.unknown-encoding`. You can use this in `.tm_properties` files to set `fileType` and `encoding` to provide default values only for files where TextMate was unable to find the proper type/encoding (which normally results in a dialog asking the user).
* Cached `.tm_properties` files are now observed via `kevent` so changes invalidate the cache immediately (previously it could take up to 30 seconds before the updated file was read). On file systems without `kevent` support you need to relaunch TextMate to flush the cache.
* The sections in `.tm_properties` files are now considered real scope selectors. As the syntax for globs and scope selectors is the same, the latter must start with `attr`, `source`, or `text` to indicate it is a scope selector. If your desired scope selector does not, you can always prefix it (with the always present `attr`) e.g.: `[ "attr - attr.scm" ]` to have settings for files not under version control. This should just be temporary while exact requirements are being fleshed out.
* Introduce `TM_SCM_BRANCH` variable (mainly useful for setting `windowTitle` in `.tm_properties`).
* You can augment the rules for scope attributes by saving a property list to `~/Library/Application Support/TextMate/ScopeAttributes.plist`. Your rules will be added to the default list which is:

		{
		    rules = (
		        { attribute = 'attr.scm.svn';       glob = '.svn';        group = 'scm';   },
		        { attribute = 'attr.scm.hg';        glob = '.hg';         group = 'scm';   },
		        { attribute = 'attr.scm.git';       glob = '.git';        group = 'scm';   },
		        { attribute = 'attr.scm.p4';        glob = '.p4config';   group = 'scm';   },
		        { attribute = 'attr.project.make';  glob = 'Makefile';    group = 'build'; },
		        { attribute = 'attr.project.rake';  glob = 'Rakefile';    group = 'build'; },
		        { attribute = 'attr.project.xcode'; glob = '*.xcodeproj'; group = 'build'; },
		    );
		}

	The `group` is to allow mutually exclusive attributes. E.g. if a folder contains both a `Rakefile` and a `Makefile` entry, files in that folder only get the `attr.project.make` attribute set since the two tests are in same group (and the `Makefile` test is listed first).

* Added _View → Show Wrap Column_ setting and corresponding `showWrapColumn` `.tm_properties` setting.
* Introduced `fontLeadingDelta` and `fontAscentDelta` defaults keys. These must be set as floats or integers (not strings), e.g.: `defaults write com.macromates.TextMate.preview fontLeadingDelta -float 0`. The default value for both keys is `1`.
* Use of extended attributes can now be disabled. The defaults key is `volumeSettings` and the value is an associative array with path prefix and another associative array with settings for that path (presently only `extendedAttributes` is supported, but SCM badges, display names, and more is likely to appear). So if for example we wish to disable extended attributes for files under `/net/` we can do: `defaults write com.macromates.TextMate.preview volumeSettings '{ "/net/" = { extendedAttributes = 0; }; }'`.
* Some improvements to file chooser (⌘T):
	- Current document is now included in list, and if typed exactly, will rank at top.
	- Filter string is now down-cased with spaces removed before matching to cater to the habits of some users.
	<!-- - Typing `:` or `@` without a filter string will go to line/symbol in current file (rather than currently selected file). -->
	- Raw paths are shown instead of display names, which should be faster (especially on network file systems).
	- Subtle ranking improvements.
* The font size unit parser (for themes) is no longer locale sensitive (so period is always `.`).
* The collapse/expand all button for the Find in Folder results did not properly refresh the outline view on Lion.
* Implement option page up/down (for moving caret).
* Set `initialFileBrowserURL` defaults key (to a URL) to change the initial location of the file browser (for new windows). E.g.: `defaults write com.macromates.TextMate.preview initialFileBrowserURL "file://$HOME/Source/"`.
* Default location for `mate` is now `/usr/local/bin` (if it exists) as this is included in the `PATH` for most users.
* Fix failure to install `mate` when the install path existed as a dead symbolic link.
* Fix issues with involuntary (recursive) expansion of file browser.
* Support transparent backgrounds in themes.
* Exclude binary matches for folder searching.
* It’s now possible to change case in the file browser (previously a “file exists” error was shown for case-insensitive systems).
* Fix issue with the `txmt:` URL scheme (though omitting the `file` parameter is still not supported).

## 2011-12-20

* Rename _Beta Builds_ to _Nightly Builds_ (Preferences → Software Update). This will be semi-daily builds that might have minor regressions (like e.g. the out-of-date SCM badges mentioned below), as I plan to do these frequently (but at most once per day). With Christmas coming up in four days (and me being in charge this year) don’t expect too many updates in the coming days though; I’m still working my way through reported issues.
* Don’t add documents to recent menu when opened via file browser.
* Fix the previously introduced `callback.application.did-(de)activate` semantic classes.
* Asking for SCM status now use selected folder over file browser root.
* Provide document name in Save As dialog.
* Fix issue with proxy auto-configuration scripts.
* The `htmlOutputPlacement` defaults value can now be set to `right` to get the split to the right of the document. The width is presently not persisted across relaunches (and defaults to 100px).
* Throttle SCM status polls. This is a provisional fix for mercurial issues which can cause SCM badges to be out of date.

## 2011-12-18

* Introduced `callback.application.did-activate` and `callback.application.did-deactivate` as two new semantic classes. A command with this class will be executed when the application gain/lose focus. The scope selector is matched against the “current scope” of the visible document (in each window). This allows creating a command with “Save” set to “Modified Documents” and thereby recreating TextMate 1.x’s ability to save modified documents on lost focus.

	The approach in 2.0 also allows to run some code, for example reloading the currently open browsers, and it can be scoped e.g. to `text.html` to only have the “save on lost focus” enabled when editing HTML files — alternatively one can introduce a custom scope like `attr.save-on-deactivate` and set that for specific projects via a `scopeAttributes` setting in `.tm_properties`.

* Tab bar can be made always visible: `defaults write com.macromates.TextMate.preview disableTabBarCollapsing -bool YES`.
* Fix issue with indented soft wrap having the prefix string wider than the wrap column.
* Fix issue with selection sent to commands needing entire document (e.g. ⌘R for Ruby, Python, etc.).
* Fix issue with ⌃⇥ not always working to move focus to file browser.
* Remove folding patterns from grammar template.

## 2011-12-16

* HTML output can open in its own window: `defaults write com.macromates.TextMate.preview htmlOutputPlacement window`.
* Anti-alias can be disabled: `defaults write com.macromates.TextMate.preview disableAntiAlias -bool YES`.
* File browser can be placed on the right side: `defaults write com.macromates.TextMate.preview fileBrowserPlacement right`.
* With multiple carets, ⌘-clicking one of them removes it (i.e. it’s a toggle).
* Carriage returns (`<CR>`) on the general clipboard are converted to regular newlines when pasting.
* Rename help book to “TextMate 2” which fixes issues where _Help → TextMate Help_ showed TextMate 1.5’s help book.
* TextMate 1.x and 2.0 can now both run at the same time.
* Setting `TM_HG` and `TM_GIT` in Preferences is no longer eclipsed by the default properties (which set them to point at `/opt/local/bin`).
* Fix potential crash when deleting bundles on disk (`rm -rf`).

- - -

## 2011-12-13

Notable changes since to TextMate 1.5.10:

* **Multiple carets / discontinuous selection**

	The easiest way to get multiple carets is by doing a left or right movement command (like arrow left/right) with a column selection. You can also ⌘-click to place new carets or use _Edit → Find All_ (⌥⌘F).

* **Snippets**

	New syntax for pop-up menus: `${1|first,second,…,last|}`. In a Git commit message try `fixup⇥` to get a pop-up of your last 10 commit messages (for a `fixup!`-commit) or between `@interface`…`@end` in Objective-C you can try `prop⇥` to get a `@property`-declaration with pop-up for its access type.

	Snippets can be nested, a frequent request but it does cause a few false firings of snippets which we haven’t yet found a good solution for.

* **Movement / Selection**

	Text is classified into units which you can change with the `characterClass` (scope) setting. For example a C string with `%1$ld\n` has only two units (`%1$ld` and `\n`) for the purpose of selection (_Select → Word_ or double click), word movement (⌥⇠/⌥⇢), and other functions that rely on character types.

	TextMate 1.x has _Select → Block_ (⇧⌘B) where a block is found using scope-defined paired characters such as braces, brackets, etc. In 2.0 you can move to begin/end of such block with ⌃⇡/⌃⇣ and the paired characters can be strings (planned to be patterns).

	Whenever you select a unit (like anything in the _Edit → Select_ submenu) the selection is “unanchored”, meaning it can be extended to either the left or right side. A minor thing but I find it surprisingly useful, e.g. to select `foo(bar‸)` we first select the parenthesis with ⇧⌘B and then extend left by one unit with ⌥⇧⇠, previously you couldn’t be sure the last step would extend or simply move the selection end point.

* **Themes**

	There now is the ability to change font and font size per scope. So you’ll (by default) see that headings in markup are shown with a larger (non-fixed width) font.

	You can also change soft wrap on a per-line basis, so soft wrap is e.g. disabled (by default) for diffs embedded in Git commit messages, raw (code) blocks embedded in markup, and enabled for line comments in source.

	Soft wrap can be indented. This is also based on scoped settings so list items in markup are indented differently than line comments in source.

* **Foldings**

	Foldings have been taken out of grammars and are now per-line (via scoped settings). In addition there are two new patterns to allow folding indented blocks.

* **Indent**

	TextMate is more aggressive about doing indent corrections. This works great when the patterns are well-calibrated but can be disabled with the `disableIndentCorrections` scope-setting. This setting also change the behavior of re-indented paste to a heuristic that works much better with Python (for which indent corrections are disabled by default).

* **Folder specific settings**

	Variables and some settings can be set on a folder / file-type basis.

* **Project drawer**

	The project drawer has been replaced with a file browser sidebar. This file browser has most Finder functions (labels, duplicate, etc.) and does SCM-badging, presently only for Subversion, Git, and Mercurial. In addition it has support for running commands (although somewhat proof-of-concept ATM) and can also do custom data sources, the API is not yet public but an SCM Status data source is included (the smart folder icon) which show SCM status for the current project. This data source works great together with the various SCM bundles.

* **Buffer completion**

	This has been improved to work with the existing word suffix (in addition to prefix), use the new character class system, etc.

* **Shell invocation**

	_Preferences → Terminal_ allows you to install `mate` which has a few new options and work when called as `sudo mate`. In addition you’ll find `rmate` which is a ruby implementation that you can use on a server over ssh.

* **Search in project / folder**

	Revamped the old “Find in Folder” and merged it with the regular find dialog. It’s now asynchronous, a lot faster, and more flexible.

* **Format strings**

	Format strings are ubiquitous in TextMate 2. Even scopes in grammars are format strings (allowing you to capture document content for use in scopes) and format strings are more powerful than the 1.x variant.

* **Scopes**

	Scopes now contain info beyond document context. For example the `attr.scm` scopes give info about the current file’s SCM status, allowing using same key equivalent for different SCM systems.

	The `dyn` scopes give info such as “is there a selection” (allowing to change key bindings only when there is or isn’t a selection), an example of this is overloading `{` to “wrap selection” (nicely), but leave the key alone when there is no selection. Another use-case could be to change tab to shift the text right (indent) only when there is a selection (common user request that I don’t plan to make native behavior).

	You can also add your own stuff to the scope via the file and folder specific settings which allows e.g. adding `attr.test` to your unit tests and overload ⌘R for that scope or have certain snippets only fire in that scope (e.g. we have a `main⇥` snippet for C which inserts the venerable main function — makes sense to overload this for (C) unit tests, which generally would be regular C files).

* **Scope selectors**

	The caret is actually between two scopes, the left and right characters scope. In 2.0 we finally allow you to explicitly target either side (or both), making the firing of certain overloads more correct. There are also a few other new things related to scope selectors, but a lot of it is still only implemented in the parser, so will delay the thorough explanation of this.

* **Commands**

	Commands can require other bundles which allow them to reference support files in the required bundle. They can also declare which shell commands they need which means better error handling and also having TextMate try to find the required commands. Failing commands result in more graceful error dialogs. The output options for a command has been split into location and format. Commands can run without a document.

* **Grammars**

	Grammars can be injected into existing scopes, for example there are now grammars for marking URLs, `TODO`, and similar which are injected into comments and other appropriate places.

	Various other things related to parsing which require its own post.

* **Semantic classes**

	Bundle items can now be assigned a semantic class. This allows us to query for e.g. `action.build || action.run` (for current scope) to find an appropriate build command (with a run command as fallback). The advantage of this is manyfold: ability to do tool bars, palettes, and unified key bindings, for example we have a single _proxy item_ bound to ⌘Y which does a query for `action.scm` finding all the SCM actions relevant for the current scope (remember scope now include SCM info, making it pick the proper SCM system).

	Proxy items are a new construct but still at the proof-of-concept level, so more about this later.

	TextMate itself will also do queries at various times, for example when opening a file it queries for import commands, where we include several to decompile AppleScript, pretty-print binary property lists, and similar. It also uses this system on save, so by default we now make new scripts with a shebang executable after save.

	Like proxy items, this system is in its infancy.

* **Managed bundles**

	When you open a file type for which you have no bundle, you’ll be asked to install one (if TextMate knows of one). Bundles are automatically kept up to date and you can see installed bundles (and install more) in _Preferences → Bundles_.

* **Session restore**

	TextMate restores the full session which includes unsaved changes incase of abnormal exit. Hold shift (⇧) during launch to bypass.

* **Foreign Input Modes**

	Display of CJK and “advanced” input modes is now be supported (although only limited testing has been done).


