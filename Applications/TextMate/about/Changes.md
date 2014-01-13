# Changes

## 2014-01-13 ([v2.0-alpha.9503](https://github.com/textmate/textmate/compare/v2.0-alpha.9495...v2.0-alpha.9503))

* Various changes, click link above for details.

## 2013-11-03 ([v2.0-alpha.9495](https://github.com/textmate/textmate/compare/v2.0-alpha.9489...v2.0-alpha.9495))

* If the _Go to File_ filter string contains an asterisk (`*`) it will be considered a file glob. Support for extension matching (by using a period in the filter string) has been removed. It did more harm than good, and the same can now be achieved by using an explicit glob string.
* The zoom animation shown when opening items can now be disabled by running this in a terminal: `defaults write com.macromates.TextMate.preview fileBrowserOpenAnimationDisabled -bool YES`
* You can now define word characters similar to 1.x by creating a new setting in the bundle editor with this content:

		{ wordCharacters = «value»; }

    The `«value»` is a string of which characters should be considered word characters. You can set the scope selector if you wish to limit the scope in which the characters should be considered a word character.

    Units for word movement/selection, completion, etc. are defined using a different system, see [this FAQ item](https://github.com/textmate/textmate/wiki/FAQ#completion-considers-var-var-and-var-as-different).

* Opening files in the file browser can be done by single-clicking the icon. If you think the click-target is too small, you can make it open by clicking the text instead, this is activated by running the following in a terminal:

        defaults write com.macromates.TextMate.preview fileBrowserSingleClickToOpen -bool true

    If you wish to select items you either need to click to the left of the text, or hold down command (⌘) when clicking the item’s text.

* Fix missing transparent backgrounds on 10.9.

## 2013-10-23 ([v2.0-alpha.9489](https://github.com/textmate/textmate/compare/v2.0-alpha.9487...v2.0-alpha.9489))

* Using a “filtering” command with a column selection now has defined behavior: The command’s output will replace the subset selected on each line in the selection. The output is truncated if it has more lines than what’s selected and otherwise padded with blank lines. This isn’t identical to how TextMate 1.x dealt with this, but I find the new behavior slightly more useful, for example make a (zero-width) column selection and use _Text → Filter Through Command…_ (⌘|) to insert sequence numbers by running `seq 100` (no need to count how many lines are actually selected).
* Add _Go → Go to Tab → Last Tab_ bound to ⌘0 (this is similar to iTerm and Chrome, although they use ⌘9, so perhaps TM should do the same).
* Allow sorting by the “installed” state in Preferences → Bundles. *[Ryan Goulden]*
* Show Invisibles will no longer treat space as an invisible (which was added in previous build) as it was causing issues with right-to-left rendering and combining marks used after spaces. The feature might be back, but needs to be implemented differently.

## 2013-10-08 ([v2.0-alpha.9487](https://github.com/textmate/textmate/compare/v2.0-alpha.9479...v2.0-alpha.9487))

* Add `invisiblesMap` option to the `.tm_properties` file. This can be set to a string which is used to control which glyphs are used for invisible characters. Add either `\n`, `\t`, or a space to the string, followed by the glyph that should be used to render the character, or prefix it with `~` to disable rendering of that character. For example to disable rendering of spaces and render tabs as `┊` add this to `.tm_properties`: `invisiblesMap = "~ \t┊"`. *[Steven Clukey]*

## 2013-09-13 ([v2.0-alpha.9479](https://github.com/textmate/textmate/compare/v2.0-alpha.9477...v2.0-alpha.9479))

* Support text attributes with VoiceOver. See [commit for details](https://github.com/textmate/textmate/commit/21088105a8646db664f5fcfccc18f3d612931694). *[Boris Dušek]*

## 2013-09-06 ([v2.0-alpha.9477](https://github.com/textmate/textmate/compare/v2.0-alpha.9475...v2.0-alpha.9477))

* Edit → Spelling → Check Spelling (⌘;) has been implemented. This will select the next misspelled word and, if the spelling panel is not showing, open a context menu with guesses, otherwise, it’ll update the spelling panel. *[Adam Strzelecki]*
* When all of the open documents don’t fit in the tab bar, the last visible tab is used as a proxy tab with an attached menu to select between non-visible documents. *[Ronald Wampler]*
* The format string syntax now supports `\x{HHHH}` for unicode code points and `\xHH` for raw byte values (normally you would only use the former syntax).

## 2013-08-26 ([v2.0-alpha.9475](https://github.com/textmate/textmate/compare/v2.0-alpha.9459...v2.0-alpha.9475))

* Support `${var}` in regexp part of format string transformations. For example if you wish the file name in the window title to be relative to `$CWD` (current directory of the `.tm_properties` file) then you can now add:

        windowTitle = '${TM_FILEPATH:?${TM_FILEPATH/${CWD}.//}:$TM_DISPLAYNAME}$windowTitleProject$windowTitleSCM'

    The example makes use of two constructs, the outer being `${var:?«if set»:«if not set»}` and the inner being the regexp substitution. I used `.` to match the trailing slash that we also want to remove, mainly to avoid having to use escapes (if we wanted to use a literal slash).

* Using ⌃T has been improved for the case where two “things” are selected delimited by some operator. It will now swap the two “things” rather than reverse the characters. You can see [examples in the test file](https://github.com/textmate/textmate/blob/master/Frameworks/editor/tests/t_transform.cc).
* Miscellaneous fixes and improvements — as usual, click the link above for full commit history.

## 2013-08-02 ([v2.0-alpha.9459](https://github.com/textmate/textmate/compare/a9455...v2.0-alpha.9459))

* TextMate now listens to `rmate` connections via IPv6 meaning that if you are using an ssh tunnel, you should use `localhost` instead of `127.0.0.1`. Also worth mentioning that `rmate` is once again a [standalone script](https://github.com/textmate/rmate) that can be installed without needing rubygems.

## 2013-07-31 ([a9455](https://github.com/textmate/textmate/compare/a9449...a9455))

* Add Quick Look generator for file types conforming to `public.source-code`. *[Jacob Bandes-Storch]*
* Folder search now supports UTF-16/32 encoding. If document is not in UTF, TextMate will try a potential `encoding` set in `.tm_properties` (in a `attr.file.unknown-encoding` section). *[Jasmin Lapalme]*
* Add source list style setting for the file browser. Run `defaults write com.macromates.TextMate.preview fileBrowserStyle SourceList` and relaunch TextMate to enable. *[Jacob Carlborg]*

## 2013-07-23 ([a9449](https://github.com/textmate/textmate/compare/a9443...a9449))

* If a tab is sticky (settable via its context menu) then it will not close when using the batch close actions like holding option when opening a file via file chooser or file browser, etc.

* The `highlightPairs` setting now support regular expressions. To indicate a value is a regular expression, surround it with slashes, e.g. `/<\w+.*?>/`. This setting is used for highlighting counterparts when caret moves over characters but you can also jump to the next/previous paired character (with same nesting level) via ⌃↓/⌃↑ and you can select the content with ⇧⌘B. The HTML bundle has been updated to declare start/end tags as pairs (_HTML → Settings → Miscellaneous_).

* The file used for _Go → Go to Related File_ (⌥⌘↑) can be set using a `relatedFilePath` setting in `.tm_properties`. If the file does not exist, TextMate will try its built-in search (which looks for files with same base name) and if nothing is found, it will create a new tab using the path specified via `relatedFilePath`. The default properties have added the following settings to allow switching between test and regular `*.go` files:

		[ *.go ]
		relatedFilePath = "${TM_FILEPATH/(?=\.go$)/_test/}"
	
		[ *_test.go ]
		relatedFilePath = "${TM_FILEPATH/_test(?=\.go$)//}"

* A folder search can now be stopped via the stop button next to the status text or by pressing ⌘.. *[David Howden]*

## 2013-06-28 ([a9439](https://github.com/textmate/textmate/compare/a9437...a9439))

* Add support for printing. You can set the theme to use in the print dialog. The font used for printing is the document font settable via _View → Font_ though hardcoded to 11 points. If you want to increase the size then adjust the scale factor.
* If you want the file chooser (⌘T) to follow symbolic links then you can add `followSymbolicLinks = true` to `.tm_properties`.
* Miscellaneous fixes.

## 2013-06-24 ([a9437](https://github.com/textmate/textmate/compare/a9435...a9437))

* With the find dialog showing for a document, it is now possible to use ⌥⌘F to activate “Find All”. Be aware that Edit → Find → Find All without the dialog showing is really “select all matches” (possibly limited to a selection).
* To aid in discovery, I’ve added Edit → Select → Toggle Column Selection which shows just the option modifier key (⌥) as key equivalent, since a single-tap of option is how you switch from stream selection to column selection. This is the same as TextMate 1.x, but many still seem unfamiliar with just how easy it is to create column selections with TextMate, and subsequently, break it into multiple carets. For the latter there now is a [short screencast](http://www.youtube.com/watch?v=Jqc5EdZmpmQ "YouTube » Multiple Carets with TextMate 2") in addition to the [blog post about multiple carets](http://blog.macromates.com/2011/multiple-carets/ "TextMate Blog » Multiple Carets").

## 2013-06-21 ([a9435](https://github.com/textmate/textmate/compare/a9433...a9435))

* You can now [add your TextMate (1.x) license](javascript:TextMate.addLicense()) in About → Registration.
* Using arrow up/down with multiple carets all on same line will no longer leave the multiple carets editing mode. Instead use ⌘←/⌘→ or ⌃A/⌃E to move all carets to the beginning or end of the line (which collapses them into a single caret).
* Previously using ⌘F with a multiline selection would automatically set the “in” pop-up to “selection”. This is no longer the case, but can be brought back by running: `defaults write findInSelectionByDefault -bool YES`
* Miscellaneous fixes.

## 2013-06-15 ([a9433](https://github.com/textmate/textmate/compare/a9427...a9433))

* Don't add files from find window to recent list. *[Jakub Suder]*
* Enable "Inspect Element" in web preview. *[Zete Lui]*
* Miscellaneous fixes.

## 2013-05-26 ([a9427](https://github.com/textmate/textmate/compare/a9423...a9427))

* A bundle’s `info.plist` can now use the `requiredCommands` key. All items in the bundle will inherit the requirements, though they are presently only checked for commands and drag commands.
* Failing to set/remove extended attributes is no longer treated as an error (to improve compatibility with various file systems).
* Add SOCKS proxy support for updating and crash report submissions.

## 2013-05-17 ([a9423](https://github.com/textmate/textmate/compare/a9419...a9423))

* TextMate should now work with _Proxy Auto Discovery_ (for updating itself and the managed bundles).
* A bunch of (work in progress) refactoring has caused some changes to executing commands:
	- Scoped variables (which are set via bundle settings) are now setup before the variables in Preferences → Variables, which in turn are setup before path specific variables (`.tm_properties`). This matters when you want to override or augment variables. For example, if you want to setup commenting style variables, you previously had to edit the existing variables found in the language’s bundle. You can now place the variables in `.tm_properties` in a group with the proper scope selector — although presently scope selectors in `.tm_properties` are matched against the file’s scope, not that of the caret (so for languages that are likely embedded, it’s still best to set the variables via a bundle item).
	- The `requiredCommands` bundle item key (to specify required shell commands) can now be used for completion commands as well. Long-term this will be usable with all types of bundle items (e.g. a snippet or macro may also require the presence of certain shell commands).
	- The working directory of the command being executed is unspecified except for drop commands (where it is the parent of the current file). We have never documented what the working directory is set to, and if you need it to be set to something specific (e.g. `TM_PROJECT_DIRECTORY`) then you should set it yourself.
	- Commands being run via `TextMate.system` (JavaScript in HTML output) no longer support being an inline shebang-script.
	- Shell code embedded in snippets and run via `TextMate.system` no longer source `TM_SUPPORT_PATH/lib/bash_init.sh`.

## 2013-05-02 ([a9419](https://github.com/textmate/textmate/compare/a9417...a9419))

* _Full Screen_ is now assigned to ⌃⌘F to follow Apple’s recommendations. Previously this key was used for _Replace All_ which is now instead ⌃⌘G and _Replace All in Selection_ is ⌃⇧⌘G. This is somewhat consistent with _Replace & Find_ bound to ⌥⌘G.
* The scope selector used to inject grammar rules can now match against the left scope to make the injected rule rank higher than the rules from the context it gets injected into. *[Joachim Mårtensson]*
* Swipe gestures (in file browser and HTML windows) to go back/forward in history is now detected differently. Some users have reported that scrolling up/down in the file browser was sometimes triggering an undesired swipe action, hopefully the new code works better. If not, you can let me know via IRC or the mailing list.
* The bundles in Preferences → Bundles now show link arrows to go to the bundle’s (GitHub) page where you can find its issue tracker, detailed version history, instructions on how to clone the bundle, and it’s the origin for which you can submit pull requests.

## 2013-04-25 ([a9417](https://github.com/textmate/textmate/compare/a9415...a9417))

* By default, font smoothing is now only disabled (for dark themes) on high-DPI displays. See [wiki for how to control font smoothing](https://github.com/textmate/textmate/wiki/Hidden-Settings#controlling-font-smoothing).

## 2013-04-23 ([a9415](https://github.com/textmate/textmate/compare/a9411...a9415))

* By default “font smoothing” is now disabled for dark themes. Font smoothing often makes light text on dark backgrounds look bolder than desired, should you however wish to have font smoothing enabled (for dark themes) you can run:

		defaults write com.macromates.TextMate.preview fontSmoothing 1

* The table showing bundles in preferences now use sortable columns. *[Steven Clukey]*
* Added API to OakTextView to support the [Emmet plug-in](https://github.com/emmetio/Emmet.tmplugin). *[Sergey Chikuyonok]*

## 2013-04-10 ([a9411](https://github.com/textmate/textmate/compare/a9407...a9411))

* Improve accessibility of various aspects of TextMate. *[Boris Dušek]*
* Setup document scope `attr.project.lein` for [Leiningen](https://github.com/technomancy/leiningen) projects. *[Dirk Geurs]*
* Auto-paired characters can now be globally disabled using:

		defaults write com.macromates.TextMate.preview disableTypingPairs -bool YES

## 2013-03-27 ([a9407](https://github.com/textmate/textmate/compare/a9405...a9407))

* Macros can now be saved via Edit → Macros → Save Macro… (⌃⌘M). Actions invoked via Find dialog are also correctly recorded, though replaying “Replace and Find” is still unimplemented (although the action itself works when called outside macros).
* Revert "Space can be used instead of slash in file chooser (⌘T)". See [issue #893](https://github.com/textmate/textmate/issues/893) for discussion.

## 2013-03-26 ([a9405](https://github.com/textmate/textmate/compare/a9403...a9405))

* When using `$` in a scope selector we anchor the match to the content scope’s end. For example `string $` will match `source string attr.scm.git` because the last part (`attr.scm.git`) is not part of the content scope. *[Joachim Mårtensson]*
* Installing bundles from within TextMate should no longer stall/fail on volumes where file system events are not generated. Installing from outside TextMate still require proper events to be generated. If your bundles are stored on a volume that does not generate these events then you will need to delete `~/Library/Caches/com.macromates.TextMate/BundlesIndex.plist` and relaunch TextMate to have it re-index your `Bundles` folders. Normally local file systems do generate file system events but [problems appear to exist](https://github.com/andreyvit/find-fsevents-bugs).

## 2013-03-21 ([a9403](https://github.com/textmate/textmate/compare/a9401...a9403))

  * Add _Filter Through Command_ bound to ⌘|. I am aware that this keybinding might not work for all users. For now you will have to rebind manually via _System Preferences → Keyboard → Keyboard Shortcuts_.

    The dialog doesn’t include all the options of 1.x, but I think most of that wasn’t being used, and by keeping the choices limited, we can improve keyboard usability. A few options are however likely going to appear, like indication of the input unit (which presently is selection and falling back on document) and outputting to a new window. *[Jacob Bandes-Storch + Allan Odgaard]*

## 2013-03-20 ([a9401](https://github.com/textmate/textmate/compare/a9399...a9401))

* Tweaks and improvements.

## 2013-03-18 ([a9399](https://github.com/textmate/textmate/compare/a9397...a9399))

* Fix (cache) issue with locally deleted bundle item fields not actually being deleted.

## 2013-03-17 ([a9397](https://github.com/textmate/textmate/compare/r9395...a9397))

* You can now set the current window’s project folder via the file browser. Click the folder in the top bar and there should be an option to use the current folder as project folder. Project folder is the default location for project searches (⇧⌘F), file chooser (⌘T), and many commands will fallback on project folder (e.g. build commands).
* The file chooser (⌘T) will now treat space the same as slash, i.e. enable full path filtering.
* `rmate` has been turned into a ruby gem and can be installed with `gem install rmate` on servers that have `gem` available.
* The bundle history in this window (which you can get to by pressing ⌘3, ⌘}, or ⌥⌘→) now only list history for 2012 and 2013, which should make showing the history much faster.
* Don’t use `POSIX_SPAWN_CLOEXEC_DEFAULT` on 10.7 to avoid kernel panic. Thanks to [squatch](https://github.com/squatch) for debugging this.
* Fix type of `rmate` listen port in _Preferences → Terminal_. If this value shows with a thousand separator you need to manually remove the separator character and press return to get rid of the old (bad) value.
* I changed the `r` prefix on the build number to `a`. Previously it was short for “revision”, but since I may reset the counter once we go from alpha → beta → release, it should use the build’s status as prefix.

## 2013-03-13 ([a9395](https://github.com/textmate/textmate/compare/r9393...r9395))

* When estimating indent we now use per-line indent patterns to avoid having multi-line block comments above the caret affect the estimated indent level.
* New scoped setting: `zeroIndentPattern`. Lines matches by this pattern get zero indent but does not affect the indent of following lines. Probably only useful for C preprocessor lines, maybe also code lines in template languages that support a “begin of line” character as an alternative to wrapping the code in special syntax.
* Window resize when toggling file browser can now be disabled in _Preferences → Projects_. The behavior has however seen a few improvements (accounting for browser on left side of window and reusing old frame on second toggle, instead of calculating a new frame).
* Introduce `TM_MATE` environment variable which bundle commands should use when they wish to call `mate` (previously the convention was to use `"$TM_SUPPORT_PATH/bin/mate"`).
* The _Edit → Select → None_ (⇧⌘A) menu item has been moved to the file browser’s action menu, as the key is always sent to the file browser.

## 2013-03-07 ([a9393](https://github.com/textmate/textmate/compare/r9391...r9393))

* Fix potential deadlock when opening find dialog (introduced in r9391).

## 2013-03-07 ([a9391](https://github.com/textmate/textmate/compare/r9389...r9391))

  * Assign ⌃⌘↩ to View → Enter / Exit Full Screen.

  * Rework find dialog: This is work in progress but I don’t think any functionality is missing compared to previous build, although some functionality might be less polished, e.g. the action buttons don’t properly enable/disable and the height of the results list gets lost when hiding it.

    Some of the stuff that has changed / improved:

      * The find/replace text fields adjust their height to encompass the content. Presently though the initial height of the controls is one line regardless of content (but they should adjust on first edit).
      * You can use Save All (⌥⌘S) to save affected files after Replace All.
      * The key equivalents / actions available in the Find dialog is now easier to find via the action pop-up, which also improves accessibility.
      * Using Find Next (⌘G) or Previous (⇧⌘G) with search results will move selection up/down.
      * Using Find All with ‘in’ set to ‘Selection’ will find and select all matches in the current document, though it might be more desirable to show the results in the find dialog (like Find All does for a document or folder).
	  * Replace and Find has been implemented and is bound to ⌥⌘G.

## 2013-03-04 ([a9389](https://github.com/textmate/textmate/compare/r9387...r9389))

* Add “scroll past end” option to the View menu. Enabling this means that the last line of the document is no longer anchored to the bottom but can e.g. be centered in the view port. *[Steven Clukey]*
* Improve voice over / accessibility support: The editor (with status bar) and file browser are now two distinct groups and all image buttons have descriptive accessibility attributes. *[Boris Dušek]*
* Improve heuristic for swipe gestures in HTML views.

## 2013-02-25 ([a9387](https://github.com/textmate/textmate/compare/r9385...r9387))

* After using the open dialog, TextMate would not switch back to internal GPU.

## 2013-02-22 ([a9385](https://github.com/textmate/textmate/compare/r9383...r9385))

* When toggling the file browser, the window will adjust its width. Likewise the width of new windows depend on wether or not the file browser is initially visible.
* Updated key equivalents for the _File_ menu. Also added 3 new items: _New File_, _New Folder_, and _Close All Tabs_. The first two actions were previously only available through the file browsers action menu.
* For HTML views we now only treat two-finger left/right scroll as a swipe gesture if no scroll can be performed (i.e. the content is at its leftmost or rightmost position).
* Deleting symbolic links in the file browser would trash the item that the link pointed to, instead of the link itself.
* Commands which use the `requiredCommands` key (in the `tmCommand` property list) and specify a variable will now always have that variable set. Previously it was only set if the required executable was not found via `PATH`.

## 2013-02-18 ([a9383](https://github.com/textmate/textmate/compare/r9381...r9383))

* Themes can now specify they want to use the [sRGB color profile](http://en.wikipedia.org/wiki/SRGB). This is done by adding the following to the theme:

		colorSpaceName = sRGB;

	This color profile is a better choice for interoperability so new themes should use it (and we will likely convert the old themes).

* The `menu` dialog command would return wrong items for menus with separators or headings.
* Add line, type, and display name flags to `rmate`. *[Toby Butzon]*

## 2013-02-16 ([a9381](https://github.com/textmate/textmate/compare/r9379...r9381))

* Fix: Calling `mate -w` would hang forever.
* Fix: _Edit → Select → None_ would crash if the file browser wasn’t visible.

## 2013-02-15 ([a9379](https://github.com/textmate/textmate/compare/r9377...r9379))

  * The ‘New Document’ file browser action will now create a document on disk in addition to a new tab. The type of this document (and thereby the file extension) is taken from _Preferences → Files → New document type_ but you can set it per-folder via `.tm_properties`, e.g.:

        [ attr.untitled ]
        fileType = 'source.objc++'

  * The _Edit → Select → None_ action bound to ⇧⌘A can now be used to deselect all in the file browser. It does not require focus to be in the file browser.
  * When focus is not in the text view then the _Go → Back/Forward_ menu items get ⌘[ and ⌘] as key equivalents. Additionally 10.8 users should now (also) be able to use the two-finger swipe gesture for back/forward in file browser and command output (HTML).
  * `file` links in HTML output that link to directories will now open the potentially contained `index.html` (this is mainly for documentation commands).
  * Introduce `TM_PROPERTIES_PATH`. This variable contains a colon-separated list of `.tm_properties` files that have been read (to create the current “environment”). This is mainly meant as a debug aid.

## 2013-02-11 ([a9377](https://github.com/textmate/textmate/compare/r9375...r9377))

  * Starting with this build, the summary for each update is going to be shorter so if you want all the details visit GitHub by clicking the link in the heading above.

  * When you change the spelling language via _Edit → Spelling_ then we record your preference as:

    1. The new default for documents without a more specific setting.
    2. The current document (based on its path).
    3. All documents with same path prefix as the current document (**new!**).

    We no longer store the preference for documents with same type (e.g. `text.html`) as that wasn’t useful.

  * Add bottom tool bar to file browser. The current _“New Document”_ action is likely to change.

  * Relaunching as part of an software update will restore your open documents even if you disabled that option in preferences.

## 2013-02-08 (a9375)

  * When moving focus to file browser via _Navigate → Move Focus to File Browser_ (⌥⌘⇥) and there is no selection, we now select the first item (mainly to give an indication of successfully having moved focus, as there is no focus ring for this view).
  * On 10.8 TextMate will now show a notification when posting a crash report to `macromates.com`. This way the notification center provides a list of recent crashes that can be clicked to view the online version.
  * When closing other tabs (either right-clicking a tab and selecting that action or option-clicking a tab’s close button) documents which are modified but untitled are left open (rather than asking you about what to do).
  * When TextMate run commands it creates a “clean” environment, only inheriting a select few variables from its parent process. You can now alter the whitelist via the `environmentWhitelist` defaults key. This is a colon-separated list of variables to inherit. If an item in the list contains an asterisk, then it is treated as a glob.

    Example:

        defaults write com.macromates.TextMate.preview environmentWhitelist '$default:MANPATH:*EDITOR'

    Here ‘$default’ will expand to TextMate’s default whitelist.

    Normally TextMate will setup `HOME`, `PATH`, `TMPDIR`, `LOGNAME`, and `USER`. If you whitelist any of these, then the variable (if set) will instead be inherited from the parent process.

  * TextMate now respect setting `disableAutoIndent` and `disableOutputAutoIndent` in snippet and command bundle items.
  * When inserting identical paired characters (like straight quotes), we now check the entire line to see if it’s unbalanced, rather than only what’s to the left of the caret.
  * If you check “keep bundles updated” in _Preferences → Software Update_ then we no longer update the installed bundles. The index is however still kept up-to-date and it’s not recommended to disable bundle updates.
  * Fix issue where entering a file suffix in the file chooser (⌘T) would get a too high rank.
  * Fix searching for case-sensitive regular expressions. Previously the search would always be case insensitive.

## 2013-02-06 (a9371)

* Performing file operations in the file browser now instantly reload rather than wait for fs-events. This not only give a better experience (instant feedback) but also makes the file browser update properly when using file systems that doesn’t support fs-events.
* It’s (again) possible to make renames that only change case (on case-insensitive file systems).
* The text view scroll bars now auto-hide when the content fits the viewport (for users who have opted to have them always visible).
* Stability improvements.

## 2013-02-05 (a9367)

* When re-activating TextMate, the file browser will reload modified folders (based on stat’ing them and looking at the modification date). This is to support file systems which do not support fs-events.
* Fix high CPU load when opening documents from outside the window’s project folder.
* Fix missing items when collapsing and then expanding in file browser.
* Use proper cursor image for resizing the command output view when it’s placed to the right of text (arrow left/right instead of up/down).

## 2013-02-05 (a9363)

* Fix crash introduced in last build. For the records, if you update to an unstable nightly build you can switch back to “Normal Releases” in _Preferences → Software Update_. When you then do _Check Now_ it’ll allow you to downgrade. Also, if a build gets pulled again, as was the case with r9361, staying on nightly builds also offer the downgrade option when you _Check Now_ — I may make it automatically downgrade in the future, when builds are pulled, seeing how many users prefer to use the nightly builds.

## 2013-02-04 (a9361)

* Files missing on disk but tracked by your version control system (with status as deleted) will now show in the file browser (with appropriate badge).
* Improve performance relating to disk access. If you had SCM badges disabled I encourage you to enable them and speak up if there are still performance issues. Additionally TextMate should also perform better on (high-latency) network file systems.
* The default save folder now use parent of selected file in the file browser (if any).
* Use the custom (grid) icon for `.less` files. *[Bob Rockefeller]*
* Fix wrong color used in red file labels. *[michael starke]*
* Fix wrong behavior when selecting to end of line (problem introduced in last build).

## 2013-01-30 (a9359)

* A (square) column selection is now broken into multiple carets (discontinuous selections) when selecting to begin/end of typing pairs (⌃⇧↑/⌃⇧↓) or to end of line/paragraph. Previously only left/right movement or “unit selection” would show this behavior.
* The “Go to Symbol…” panel now open with the current symbol selected and will reflect movement in the document or switching to another document (if you keep the panel open). *[Steven Clukey]*
* Fix crash when pasting in file browser (introduced in last build).
* Tab bar labels now have a shadow. *[Adam Strzelecki]*
* Fix occasional “too light” title bar gradient. *[Adam Strzelecki]*
* Fix issue where click-dragging scrollbars would cause entire window to be dragged.

## 2013-01-29 (a9353)

### Settings

* You can now set a default file type for documents with unknown extension using _Preferences → Files → Unknown document type_. *[Steven Clukey]*
* You can now set *Preferences → Projects → Tab bar above document* if you dislike that the file browser is placed below the tab bar. *[Adam Strzelecki]*
* Tab bar collapsing is now disabled by default.

### File browser

* Added _Navigate → Move Focus_ (⌥⌘⇥). This moves focus between file browser and document.
* With focus in file browser _View → Show Invisibles_ (⌥⌘I) toggles the include/exclude patterns (and thus show everything when enabled).
* By holding option when opening a file, it will now open in TextMate, even if TextMate is not the default application for this type. *[Steven Clukey]*
* Deleting items in file browser will select the succeeding item (rather than leave you with no selection).
* Going to SCM status will now have the _Uncommitted Changes_ and _Untracked Items_ groups expanded by default.
* When toggling the QuickLook preview panel (using space when focus is in file browser) will now do a zoom in/out transition.
* When using _Go → Current Document_ (⌃⌘R) we drop other (non-visible) expanded folders.
* No longer plays sounds on file renames.
* Fix “Go to Favorites” (it went to a wrong location).

### File chooser (⌘T)

* Improve extension filtering when dealing with multiple extensions. We now only require that the extension entered (e.g. `.js`) be a subset of the file’s (e.g. `foo.js.erb`). This subset must however match the entire sub-extension. E.g. `.m` as filter string will still have `.md` files excluded.
* Enable focus ring when 'Full Keyboard Access' is enabled. *[Michael Sheets]*
* Add inactive state coloring for dividers. *[Michael Sheets]*
* Tweak how we position the file chooser: Now it’s relative to the text view rather than the entire window.
* Fix display string for untitled documents when filter string contained a slash.
* Fix lack of clearing filter string.

### Text editing

* Incremental search (⌃S) now remember last string used.
* Dropping files on the text view, which caused a command execution, would not update the buffer (until text view was activated).
* Fix crash related to using undo while a snippet was active. Presently the snippet is dropped if you use undo. Long-term it should be improved.
* Improve autoscroll when using mouse to select text.

### Other

* Sections in `.tm_properties` now also match folders (e.g. file browser looks up settings with just a folder rather than file, as “key”).
* Folders you open are once again added to the _File → Open Recent_ submenu.
* The HTML output window would have a blank title, if the command didn’t output one.
* The code signature on `TextMate.app` for r9351 wasn’t timestamped. This may have been the reason some users had Finder report that the program was broken. I have re-enabled timestamps.

## 2013-01-23 (a9351)

* Actions performed in the file browser now support undo. The context menu of the file browser contains undo/redo menu items, alternatively the Edit menu’s Undo and Redo actions work on the file browser, when it has focus.
* You can now delete items on network drives but will get a warning dialog informing you that the items will be permanently deleted.
* When you open files without extension nor identifying content (like a shebang) then they now open as plain text (instead of asking you to pick a type).
* When you select units, e.g. via the _Edit → Select_ submenu, TextMate does a more intelligent job of making the selection visible, and if only a subset can be shown, it no longer scrolls to the bottom of the selection.
* Add scope bar to file chooser (⌘T) and do various visual improvements. The _Uncommitted Documents_ source is currently unavailable.
* Improve speed of rendering styled text. *[Joachim Mårtensson]*
* If your clipboard data contains `<NUL>` characters then these can now be pasted into TextMate.
* Default theme has been changed to Twilight, default location for (HTML) command output is set to new window, and the position of the file browser now defaults to the right side. As for the latter, it takes some getting used to, but give it a chance, as it means your text doesn’t “move” depending on wether or not the file browser is visible.

## 2013-01-21 (a9349)

 *	Rework file chooser (⌘T):
	It now show document icons (with potential SCM status) and close buttons for open documents. Filtering has also seen some minor improvements, mainly related to “full path” filtering (which you activate by putting a slash in the filter string). Performance should be better when dealing with large directories (try e.g. ⌘↑ to move to the parent of your current folder), this mainly relates to not stalling the application (the scanning itself could likely be a little faster).
	Presently though a few things are missing:
	* You can no longer use `@` to descend into a file and see its symbols (will be added back).
	* The close buttons are not yet hooked up.
	* There is no UI for switching “source” but you can use ⌘1 and ⌘2 to switch between all files and open files, where the latter now actually works.
	* The look is provisional.

 *	Opening multiple documents at once will now select the last one so that ⌘W can be used to go through them. Try e.g. holding down shift when using arrow keys in the file chooser.

 *	When you open documents and select to “close other” then it leaves modified documents open rather than ask about saving them. Try e.g. holding down option (⌥) when you hit ↩ in the file chooser.

 *	When you open the file chooser, we look at the find clipboard to see if it contains something of the form `«file»:«line»` and if so, uses that as the default filter string. This “matching” now also works if the file contains slashes. The idea is that when you have “error output” e.g. in your terminal, you can select it, hit ⌘E and switch to TextMate where you hit ⌘T + ↩ to go directly to the file + line. If you find yourself doing it a lot, you may like the automator workflow described in [issue #665](https://github.com/textmate/textmate/issues/665).

 *	Restyle navigation bar to be more in line with Lion. *[Michael Sheets]*

 *	We now download software updates to the system’s cache folder (instead of temporary folder). This should hopefully fix the problem some ware having with the “temp cleaner” waking up before the update got installed. If you still get this warning, let me know.

 *	The encoding pop-up in _Preferences → Files_ would reset itself to UTF-8 after a relaunch (which really should have been viewed as a feature :) ).

 *	Fix crash introduced in r9345.

## 2013-01-17 (a9347)

* If you have “open documents from last session” enabled in preferences then TextMate will no longer ask you to save untitled documents when you quit, as these will be restored next time you launch it, and as they are untitled, no other program can edit them (causing a conflict, as would be the case if the document had a location on disk).
* If the ‘inode’ of an open file changed (e.g. because of `git stash`/`rebase`) and you opened the file again (e.g. via `mate`) then TextMate would open a new tab, even though the file was already open.
* Add “Show in Finder” context menu when no items are selected. *[Caleb Land]*
* Make OakTabBarView and OakPasteboardSelector accessible. *[Boris Dušek]*
* The tab bar will again close excess tabs to keeping it from overflowing.
* The _Go → Reload_ menu item was disabled unless file browser was active.
* Other minor fixes and improvements.

## 2013-01-13 (a9345)

* File browser has a new navigation bar. This is work in progress. You can find most actions of the old bar in the Go menu (where you can also see the key equivalents). Presently missing is “Show Hidden Items”, a toolbar below the file browser will soon appear.

* Improve QuickLook support: When the preview panel is showing (activated by pressing space with focus in the file browser or using a file’s context menu to select Show Preview) then you can use arrow up/down to move between the files in the file browser.

* Improve full screen support: If you quit with windows in full screen mode, these are restored in that mode. Opening windows while a window is in full screen mode no longer re-uses the (huge) dimensions of the full screen window, likewise windows opened directly in full screen mode will have a more useful size when leaving full screen mode.

* Fixed file chooser’s abbreviation matcher (⌘T): If you had multiple “corrections” stored for an abbreviation, all but the first of these would actually go to the bottom of the list, the opposite of what was desired.

* Using _Go → Current Document_ (⌃⌘R) will no longer have the file browser switch away from your project folder but instead expand folders to make the current document visible. *[Jacob Bandes-Storch]*

* Using _Go → Enclosing Folder_ will select the item we came from (like Finder).

* Previously the file browser would remember the selection state of all visited folders. This is now cleared during relaunch (except the folder currently showing) and also when using various actions that request a selection, e.g. the two previous items mentioned.

* Using `exit_discard` in a command with HTML output will now close the output view (like 1.x did).

* Find in Folder will now (also) use `excludeDirectories` and `excludeFiles` settings (it previously would only use those with an `InFolderSearch`-prefix).

* Running commands with no document windows open (or in the responder chain) would miss a few essential variables from the environment (such as `TM_SUPPORT_PATH`).

* Tweaked the SCM status gathering code so there should hopefully be less overhead. People who have previously disabled SCM badges because of poor performance may wish to give them another try.

* Various fixes to improve stability.

## 2013-01-10 (a9343)

* Save All with more than one modified document would crash (r9341).
* Merge All windows would double the current window’s tabs (r9341).

## 2013-01-10 (a9341)

* When you open a folder or some document(s), a default project folder will be based on the path(s) opened. This means that as long as you always open the root of your project (via `mate`, `open -a TextMate`, dragging the folder to TextMate, using the favorites (⇧⌘O), open dialog, `txmt:` URL scheme, or what have you), your project no longer requires a `.tm_properties` file setting `projectDirectory` for Find in Folder (⇧⌘F), Go to File (⌘T), and similar to work as desired. Additionally the default properties now set a window title that includes the project folder’s name (and SCM branch when available), so most projects can now also drop setting `windowTitle`. Note that windows restored from a session created prior to r9339 will not have a default project folder, so you should close and re-open the project folder, after this, the project folder will be preserved in the session save data.
* The file browser now defaults to the project folder rather than the folder set in Preferences (the latter is still used if there is no document, e.g. after ⌘N or ⌃⌘N).
* Since opening a single document now assigns a project folder to the window, opening more documents from the same folder (or descendent folders) will have the documents open as tabs in the existing window. I _think_ this takes care of the common request of having all documents opened appear in new tabs of the frontmost window.
* The ‘Merge All Windows’ action now ignores minimized windows. This means that if you wish to only merge a few windows then you can minimize all but those you wish to merge and then perform the action.
* Closing last tab via file browser or similar no longer close the entire window. Likewise with canceling a file open, though there’s still some work to do in this department (actually backtracking to previous state, which could mean closing the window if the window was opened only to show this document).
* Automatic pruning of the tab bar is temporarily suspended.
* Render glyphs in Unicode’s “private use area”.
* The `open` command used with the `txmt:` URL scheme now supports an `uuid` parameter. This is for command runners that work with untitled documents (the current document’s UUID is available as the `TM_DOCUMENT_UUID` environment variable).
* Fix some unsupported edge-cases with percent-escaping the `url` parameter used with the `txmt` URL scheme. Also allow tilde to be used in the URL (mainly to be 100% compatible with TextMate 1.x).
* Clicking a ‘find all’ result would be handled twice, meaning it would open two document windows, if the project window had been closed.
* Fix potential crash when activating the OS dictionary on an empty document.
* The file chooser window (⌘T) was leaking in r9337 and wouldn’t terminate its scanner thread if the window was closed w/o making a selection (although it would terminate when scanning had finished). It would also throw an exception about a key/value observer not having been removed.

## 2013-01-03 (a9337)

* Restore 10.7 compatibility.

## 2013-01-03 (a9335)

* Assign ⇧⌘P to the _Go → Project Folder_ menu item.
* Support HTML output placed on right side (again). This can be set in Preferences → Project.
* Opening a `*.tmPlugIn` file will now install it (hold down ⌥ to open as folder). It’s no longer required that the bundle identifier of the plug-in starts with `com.macromates` but instead the plug-in’s `Info.plist` **must** have `TMPlugInAPIVersion` set to `2`. Despite this change, there is still no formal support for plug-ins and the internal API hasn’t stabilized, if you wish to extend TextMate (beyond what can be done via bundles) then the current way is to dig into the source.
* Support “Select Output” as caret placement for commands.
* When activating an already open _Find_ or _Go to Line_ window, it will move the window to the current space (instead of switching to the space containing the window).
* When restoring session (after relaunch), file browser width and HTML output size is now restored individually for each window (previously there were just a global value).
* Remove the _Edit → Mode_ submenu as the menu item wasn’t hooked up. Currently you can use the tab size pop-up in the status bar to select between tabs and soft tabs.
* Improve stability.

## 2012-12-24 (a9331)

* When syntax-highlighting a file, do batch redraw instead of line-by-line (should improve perceived performance). *[Joachim Mårtensson]*
* The “Edit Command…” button in error dialogs would often cause a crash.
* Fix another potential crash, memory leak, and restore 10.7 compatibility.

## 2012-12-20 (a9327)

* Commands which fail due to unsatisfied requirements will no longer show “Edit Command” but instead will optionally show a “More Info…” button when the command includes a URL for more info.
* When manually checking for updates and your version is newer than what’s found on the server (e.g. you’re running a nightly build but checking for regular builds), you’ll be offered to downgrade (generally though the nightly builds are safe to run as they contain more fixes than the regular builds :) ).
* TextMate no longer ignores line/selection from programs opening files via the ODBEditor protocol (64 bit issue).
* Fix help book indexing (so the link in Preferences → Terminal now work).
* The “Select Bundle Item” window didn’t filter based on current scope.
* Fix potential crash when closing document window.
* When a snippet pop-up menu was showing, and TextMate was not the active application, the window would be hidden.

## 2012-12-14 (a9325)

* Add support for VoiceOver/accessibility. *[Boris Dušek]*
* Add support for “Zoom follows the keyboard focus”. *[Boris Dušek]*
* Always set the `TM_SOFT_TABS` variable. *[Michael Sheets]*
* Limit file browser refresh. *[Joachim Mårtensson]*

## 2012-11-14 (a9323)

* Fix crash bug introduced in last build.

## 2012-11-12 (a9321)

* Add `filepanel` dialog command. For details use ⌃R (execute current line) on a line containing: `"$DIALOG" help filepanel`. *[Hans-Jörg Bibiko]*

* The contributions page in the About window now list the commits from contributors. *[Brad Choate]*

* When the file browser is showing a lot of items, delays can happen when opening, closing, saving, or changing a file (when it goes from unmodified to modified) so as a temporary workaround you can now disable the file browser status by adding the following to `.tm_properties`:

		fileBrowserDocumentStatus = false

	Long-term the goal is of course to improve the slow file browser refreshing. *[Josh Goebel]*

* Command input and the `TM_SELECTED_TEXT` variable now work correctly with column selections.

* If no theme is selected, the gutter now get a default set of colors. *[Robert Hencke]*

* The command properties drawer in the bundle editor has been made less wide by wrapping a few lines. *[Adam Strzelecki]*

## 2012-10-01 (a9319)

* Using “Transpose” (⌃T) with a discontinuous selection will now swap the selected strings. If pressed repeatedly, and more than two strings are selected, it will cycle through all possible permutations.

* The scope now contains `dyn.selection` and/or `dyn.caret.*` based on the following rules:

	- If there is one or more selections: `dyn.selection`.
	- If there is a single zero-width selection: `dyn.caret.mixed.columnar`.
	- If there are multiple carets and/or selections: `dyn.caret.mixed`.
	- When there is only a single caret or a single continuous selection the left scope may contain: `dyn.caret.begin.line` or `dyn.caret.begin.document`.
	- Likewise the right scope may contain: `dyn.caret.end.line` or `dyn.caret.end.document`.

* When expanding tab triggers, the left scope is the scope to the left of the start of the potential tab trigger and the right scope is likewise that to the right of the potential tab trigger.

* `rmate`: Overwriting an existing file now preserve the existing file’s group and owner.

## 2012-09-29 (a9317)

* New semantic class: `callback.document.will-save`. This can be used to have a command called prior to saving a document, the command could e.g. strip trailing whitespace or ensure the document has a `LF` character on last line. Two minor issues is that selection is currently lost after running a “did save” command (when it replaces entire document) and caret is scrolled into the visible area.
* The about window has been combined with credits, release notes, and a new option allows you to see changes for installed bundles, although presently not much is showing, as you’ll need updated bundles before version information is available (so a lot of info should show in a few days when the various bundles have auto-updated). *[Rasmus Abrahamsen]*
* If the file browser was showing a lot of items, editor speed would be affected. There still is an issue opening/closing files or when a document goes from modified to non-modified or vice versa.
* Fix issues with lack of resizing the gutter.

## 2012-09-24 (a9315)

* Files which use CRLF no longer cause problems for Find in Folder. *[Rasmus Abrahamsen]*
* One can now use ⌃S and ⌃⇧S for Find Next/Previous when incremental search is active. Also improved the look of the incremental search control. *[Mads Hartmann Jensen]*
* It’s now possible to use “replace selected” after Find All for a single document (previously the selection was not respected). *[Rasmus Abrahamsen]*
* Added a `TM_QUERY` command that can be used to query (project specific) settings for bundle commands. For example if a bundle command wish to use the current document’s font settings it can be obtained via these two (shell) commands:

		"$TM_QUERY" --setting fontName
		"$TM_QUERY" --setting fontSize

	If no `--setting` is given then all settings are returned. *[Ole Gammelgaard Poulsen]*

## 2012-09-21 (a9313)

* Add indent aware begin/end of line action methods.

	The methods going to “begin of indented line” will go to the first non-whitespace character on the line, unless the caret is already there or to the left of this character, in which case it will go to the actual beginning of the line.

	The “end of indented line” methods work similarly.

	If you want (⇧)⌘⇠/⌘⇢ and ⌘⌫/⌘⌦ to use this behavior, you can add the following to your key bindings file:

		"@\UF702"  = "moveToBeginningOfIndentedLine:";
		"$@\UF702" = "moveToBeginningOfIndentedLineAndModifySelection:";
		"@\UF703"  = "moveToEndOfIndentedLine:";
		"$@\UF703" = "moveToEndOfIndentedLineAndModifySelection:";
		"@\U007F"  = "deleteToBeginningOfIndentedLine:";
		"@\UF728"  = "deleteToEndOfIndentedLine:";

* If you create a set of local key bindings then they no longer eclipse the default set. For the records, key bindings are consulted in the following order:

		~/Library/Application Support/TextMate/KeyBindings.dict
		/path/to/TextMate.app/Contents/Resources/KeyBindings.dict
		~/Library/KeyBindings/DefaultKeyBinding.dict
		/Library/KeyBindings/DefaultKeyBinding.dict
		/System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict

	If you edit any of the above files you will need to relaunch TextMate (⌃⌘Q) before the changes take effect.

* The gear icon in the Find in Folder status bar (above the results view) now have four flavors of copying the results to the clipboard. These work on the selected items falling back to all matches, if there is no selection (here selection means actual list view selection, not “checked” via the check boxes).
* Launching commands from TextMate now only inherit a few whitelisted variables from the process that launched TextMate. This should fix inconsistencies between running TextMate from the shell versus Finder, and in the former case, avoid problems if you set `RUBYOPT` or similar.
* When moving the selection and padding is inserted, we no longer extend the selection to include this padding.
* Moving selection left/right with an east asian character on left/right side would insert a space rather than move the selection.
* Creating new files via New Tab no longer have the file show up in the Open Recent menu.
* Opening files via `mate` no longer have the item show up in the Open Recent menu. Though you can call `mate --recent` to force the file to appear (or make an alias). The rationale is that `mate` is often used to edit temporary files like commit messages and that the shell history serves as an alternative to “Open Recent”.
* If more than 256 KB of text was selected, running commands would fail, as the `TM_SELECTED_TEXT` variable exceeded `ARG_MAX` which would cause `execve` to fail. If more than 256 KB of text is selected we now set the `TM_SELECTED_TEXT` variable to a placeholder.
* Add some checks that should prevent a crash when system ask about text metrics (with a bogus text index). Unsure exactly under which circumstance the crash happens, but likely related to the system-wide dictionary service or multi-lingual input.
* Update `rmate`: Saving would fail if editing `file` and `file~` already existed.
* If ⌘-clicking a single caret two times, TextMate would crash, as ⌘-clicking an existing caret normally removes that caret, except when there is only a single one, in that case, it would (wrongly) add a caret leaving two carets at the same position, and make the second click remove both of these two carets, leaving the editor with no carets.
* Fix crash when clicking exactly on the split view dividers.
* Fix potential crash when quitting TextMate (in `network::launch_tbz`).

## 2012-09-18 (a9311)

* Untitled documents now base their scope attributes on the file browser’s location. This mean things like ⌘B or ⌘Y will call the proper build / SCM action(s) when used in untitled documents (e.g. the initial document showing after opening a project folder via ⇧⌘O or `mate .`).
* Previous build changed the semantics of the include/exclude patterns for the file browser: Now a file **must** be matched by the include pattern in order to be shown. The default patterns have been updated, but if you have edited them, or have your own include pattern in a `.tm_properties` file, then you should add the asterisk to your set of included files, e.g. `{*,.tm_properties,.htaccess,.gitignore}`.
* I also forgot to mention that previous build is WIP with respect to the main window, presently you can size splits larger than the window, which trigger a resize, and the HTML output view can only go at the bottom. If you wish to “downgrade” an easy way is to hold down shift (⇧) when clicking “Check Now” (with the type set to “Normal Releases”).
* The start/stop fold marker patterns are still used for a line even if that line is also matched by the fold indented block ignore pattern.
* Fix crash which would often happen when closing document while a tool tip was showing.
* Fix wrong document (sometimes) showing after session restore and in theory if quickly cycling through tabs of documents that hasn’t been loaded yet.
* Previous build introduced a bug that affected bundle items with “empty” strings as values for their property list keys.
* Column selection cursor (crosshair) should be less likely to stick (e.g. after ⌥F2).
* Fix `x-insert` dialog command to work even when the text view doesn’t have focus. *[Hans-Jörg Bibiko]*
* Fix missing curly quotes in a few dialog texts. *[Ryan Maxwell]*

## 2012-09-16 (a9309)

* Minimum dimensions are enforced when resizing the file browser or HTML output. The explicit resize buttons have been removed but dragging the dividers should now be easier, as the hot zone has been increased.

* Setting a file type for a dot file wouldn’t stick.

* Sections (in `.tm_properties` files) which use a scope selector now apply according to their rank (when matched against current scope) so e.g. a setting for `source.ruby` in `Global.tmProperties` will win over a setting for just `source` in a more local `.tm_properties` file.

* Added two new variables:

	1. `TM_APP_IDENTIFIER` — bundles which need to read settings from TextMate should use this instead of hardcoding `com.macromates.TextMate.preview`. Normally though bundles should **not** need to read TextMate settings except those they write themselves, for example the Preferences dialogs for various bundles are bound to TextMate’s user defaults.
	2. `TM_CURRENT_THEME_PATH` — this is the path of the current theme. Incase the theme is in delta format, it points to the base theme, long-term TextMate may store a non-delta version in a temporary folder and let it point to that, so that bundles can always assume the theme is in non-delta format.

* Various improvements to glob matching:

	1. `**` can now be followed by something other than `/`, e.g.: `src/**.{cc,mm,h}`.
	2. `*` and `**` now match leading dot when they are not followed by `/` or at the beginning of the glob, e.g. `main*` will match `main.cc`.
	3. Negating a glob (by prefixing with `!`) now also rejects dot files, e.g. `!cache/**` will exclude `cache/.DS_Store` even though `cache/**` (non-negated) would not have matched that path.

* Find in Folder (⇧⌘F) and File Chooser (⌘T) now exclude files matched by the “Non-text files” glob setup in _Preferences → Projects_.

* The various include/exclude globs you can set in `.tm_properties` are now aggregated. So setting `exclude` and `excludeDirectories` will exclude directories matched by either of the two globs (rather than the “most specific”).

* Include SCM info in window title by default. This is done by having the following in `Default.tmProperties`:

		windowTitleSCM = '${TM_SCM_BRANCH:+ ($TM_SCM_NAME: $TM_SCM_BRANCH)}'
		windowTitle    = '$TM_DISPLAYNAME$windowTitleSCM'

	Incase you want to set a custom window title but keep the SCM info you can use the `windowTitleSCM` variable, e.g.:

		windowTitle     = '$TM_DISPLAYNAME — ${CWD/^.*\///}$windowTitleSCM'

	Exploiting the way sections with scope selectors are now evaluated last, it is possible to set a global window title based on a potential `projectDirectory` variable. E.g. put the following in `~/Library/Application Support/TextMate/Global.tmProperties`:

		[ 'attr, ' ]
		windowTitleProject = '${projectDirectory:+ — ${projectDirectory/^.*\///}}'
		windowTitle        = '$TM_DISPLAYNAME$windowTitleProject$windowTitleSCM'

	Normally we can’t use `projectDirectory` in a default/global setting, because that variable is set later, and we have no way to specify “late binding”. However, by setting it in a scope selector section we evaluate the variable after a potential `projectDirectory` has been set in a more local `.tm_properties` file. The scope selector used here is `attr, ` which means all `attr` scopes or (the comma operator) the empty scope selector (which matches everything).

	The above is just a “hack” until better solutions have been implemented.

* Removed Chinese translation. Since the translated interface files contain more than just strings, outdated translations affect program behavior. We’ll soon move fully to constraint-based layout which should allow translations to be purely string-based (on 10.8).

* r9307 accidentally included a “Select Next” macro bound to ⌃W for when there was a selection. This interfered with using ⌃W with multiple carets (as presently multiple carets trigger the `dyn.selection.discontinuous` scope even if they are all zero-width).

## TextMate Hackathon

If there is some feature you would like to add to TextMate but need a little guidance on how to go about it, we are holding a TextMate Hackathon Saturday the **22nd of September 2012** starting at 2 pm, the address is:

	SHAPE
	Gothersgade 8B, 3rd floor
	1123 København K
	Denmark

We can fit 20-25 people so please let us know in advance if you wish to attend, either using the [facebook event][] or by emailing [Allan Odgaard](mailto:hackathon@textmate.org?subject=TextMate%20Hackathon) (me).

We also welcome people who just want to drop by and say hello, share a pizza, or have some other project they want to hack on, in the company of fellow Mac/TextMate devotees.

[facebook event]: http://www.facebook.com/events/399041873494929
[SHAPE]: http://shapehq.com/

## 2012-09-10 (a9307)

* If an open file is renamed on disk, TextMate will wait up to one second for a new file to appear before updating the document’s path. This is to work with programs that renames the existing file and saves a new in its place, instead of saving a new file and using [exchangedata(2)](x-man-page:///exchangedata/2).
* When a file was externally modified, using undo followed by redo would freeze TextMate.
* The undo/redo menu items are now disabled when the action is unavailable.

## 2012-09-10 (a9306)

* Spelling dot was drawn upside down.
* Gutter and folded text images were drawn using wrong dimensions on retina Macs.

## 2012-09-10 (a9305)

* Fix crash introduced in r9303.

## 2012-09-09 (a9304)

* Escape PAC URL strings when they are invalid. Using “Choose File” in network settings could lead to a file URL with spaces which would cause a crash when TextMate needed to do network requests (for software update and crash report submissions).
* Default host for rmate is now ‘auto’ (when using ssh).

## 2012-09-09 (a9303)

* When toggling _Check Spelling as You Type_ (⌥⌘;) or changing spelling language, we now recheck/refresh the document.
* Remove overwrite and freehanded toggles from the UI (_Edit_ → _Mode_ and status bar). If you wish to edit “freehanded” you can ⌥-click to get a caret that moves unrestrained.
* An image is now used as placeholder for folded text. It defaults to the color of the text’s color but you can add a theme style scoped to `deco.folding` to change it.
* A few updates to gutter theming, the colors now need to go in their own dictionary in the theme with a key of `gutterSettings`, e.g.:

		gutterSettings = {
			divider             = '#1A3853';
			foreground          = '#111111';
			background          = '#888888';
			selectionForeground = '#FFFFFF';
			selectionBackground = '#3A68A3';
		};

	In addition to the above keys you can now also set:

	- `icons`
	- `iconsHover`
	- `iconsPressed`
	- `selectionIcons`
	- `selectionIconsHover`
	- `selectionIconsPressed`
	- `selectionBorder`

	If unset, the icon colors default to the colors used for text and selection border defaults to the divider color.

## 2012-09-07 (a9302)

### Proxy Items

Proxy items are bundle items with a regular scope selector, key equivalent and/or tab trigger. Their body is a “query” which will find bundle items with a semantic class matching this (simple) query string.

Until now we have only used this for the SCM items. None of the actions (in Git, Mercurial, Subversion, and other SCM bundles) actually have a key binding, but they all have a semantic class which start with `action.scm`.

There is an SCM bundle with a single proxy item bound to ⌘Y which does a query for `action.scm` so if the user press ⌘Y, it will map to the SCM actions valid in the current scope (e.g. `attr.scm.git`).

A few improvements has been done with respect to how proxy items appear in the interface:

* Proxy items in the menu now show the items matched by the query. E.g. if you look in _Bundles_ → _SCM_ you will see the SCM actions available. Previously you saw the proxy item. If no items are matched by the query, the proxy item itself is shown but as disabled. The idea is that we can have _Run_, _Documentation for Word_, _Validate Syntax_, and similar common actions appear in e.g. the Source bundle or (in the future) a toolbar/palette, and the user can go via these actions to run the action relevant for the current context.
* In the bundle item chooser (⌃⌘T) duplicates no longer appear (items matched would appear as a duplicate for each proxy item matching that item).
* Items without a key equivalent but matched by a proxy item will now show the key equivalent of the proxy item. E.g. looking in the Git menu you will now see that (most of) the actions can be triggered with ⌘Y.
* The query string in proxy items now support the `||` operator for “fallback”, e.g. we can make a proxy item with a query like: `action.build || format.strong` which will find a build action for the current context (scope) and if none is found, it will instead search for a `format.strong` (bold) action.

I plan to expand the query string to also support unions, so that we can e.g. bind ⌘D to a query on `diff + scm.diff` (to show Diff actions including those for the current version control system) and additionally put a scope assertion on the class query, for example my ideal ⌘B binding would likely be a query for:

	action.build[-dyn.selection] || format.strong || action.build || action.run

So the first term (`action.build`) only match items if there is no selection, if there is a selection, we would instead search for a `format.strong` item, and if there isn’t any **then** we would look for a build action (this time even when there is a selection) and finally fallback on a run item.

If you have any comments on the above, please share on the [mailing list](http://lists.macromates.com/listinfo/textmate).

### Other Changes

* Add two new action methods:

		findNextAndModifySelection:
		findPreviousAndModifySelection:

	These find the next/previous occurrence of what’s on the find clipboard and selects that, but preserves the existing selection. One could e.g. add this to `Keybindings.dict`:

		"@d" = ( "copySelectionToFindPboard:", "findNextAndModifySelection:" );

	This binding will likely be default in an upcoming build, but bound to ⌃W (Select Word) and scoped to `dyn.selection` (so only when a word is already selected).

* During save, the potential dialogs asking to unlock files or change encoding would not accept the user’s choice (bug recently introduced).
* Fix out of range assertion in some cases when switching files (gutter was querying the new document using the selection used in the old one).
* When extending a selection we now only highlight the new ranges selected rather than all of them (e.g. Find All (⌥⌘F) when there already is one instance selected or the new `find{Next,Previous}AndModifySelection:`).
* Jumping files with ⌘G (after a Find in Folder) would lead to an anchored selection, a few other cases would also cause the selection to be anchored. This is no longer the case (the subtle difference here is that unanchored selections will always extend for the initial shift (⇧) + movement action).
* Add revision numbers to release notes. *[Elia Schito]*

## 2012-09-05 (a9301)

* Fix crash for when one of the `Bundles` folders (under `Library/Application Support`) was a symbolic link.
* Fix crash related to trying to use a grammar that no-longer exists on disk, i.e. before the bundle index had a chance to catch up with disk changes.

## 2012-09-04 (a9300)

* The underline drawn for misspelled words now use high-DPI artwork on retina macs.
* Bundle item names using “«unit» / Selection” in their title now display either “«unit»” or “Selection” depending on whether or not there is a selection.
* The pop-up menu used for multiple choices (in snippets) now filter items based on the prefix typed (rather than just changing selection). It also no longer pin the selected item at the first visible row.
* The various filter lists (⌘T, ⇧⌘T, and ⌃⌘T) are now positioned relative to the current document window.

## 2012-09-03 (a9298)

* Wrap Column → Other… now show a sheet where you can enter wrap column.
* We now search the menus “right to left” when searching for a key equivalent. The problem is that on some key maps the keys used to Toggle Foldings at Level (⌥⌘1-n) clash with modifier + bracket used in the Text menu for shift left/right.
* When editing property lists in the bundle editor, the dictionary keys are now ordered more naturally.
* Drag-selecting beyond visible document no longer leaves tears in the selection.
* When viewing a saved search (spotlight query) in the file browser, going to parent now correctly goes to the folder containing the saved search.
* Fix slow performance with subversion repositories (introduced in last build, only present when all files were “clean”). Also fix issue where badges wouldn’t always update instantly (also introduced recently).
* When showing SCM Status for subversion repositories in the file browser, we no longer remove parent folders for uncommitted items nor child items (reported by `svn status`) for untracked folders.
* It is now possible to “open” an Xcode project file inside TextMate. This “descends” into the project descriebed by the project file. *[Zach Drayer]*

## 2012-09-02 (a9296)

* You can now disable SCM badges by adding this to `.tm_properties`:

		scmStatus = false

	This can be set either globally (`~/.tm_properties`) or for specific projects. If you see file browser performance issues then you should try to disable SCM badges.

* Improve performance of obtaining subversion status by using `xsltproc` to process the XML (instead of `python`) and by skipping status parsing for clean files. *[Jeremy Whitlock]*

* Fix issue with bundle item key equivalents sometimes eclipsing regular menu items even when the bundle item in question should not be enabled (due to scope selector).

## 2012-08-31 (a9294)

* Doing authenticated saves would fail (out of memory) if you had an older (32 bit) version of the “auth server” installed. TextMate now ensures the installed version is up-to-date.
* Running `sudo mate` would fail to establish connection with TextMate (bug introduced when socket name was changed to include user ID).
* The tab size dialog (Other…) now allow you to set a tab size of 1. *[Dennis Vennink]*
* Improved alignment of the gutter images and made them adjust to font size. *[Dennis Vennink]*
* Calling TextMate via QuickCursor would show wrong display name or in worst case cause a crash.
* Improved subversion status parser so unexpected output doesn’t cause a crash.
* Running commands from TextMate wouldn’t always set the working directory to that of the current document (introduced in r9292 / 2012-08-28).
* Gutter line numbers wouldn’t always update (introduced in last nightly).

## 2012-08-30 (a9293)

* Fix random crash mainly when bringing focus back to TextMate or opening a new window.
* If TextMate fails to read a link (`readlink`) it will show an alert with some diagnostics that you should submit to us (more info in the dialog). This is an attempt to track down another random crash that has been frequent (but has been in decline in recent versions, though not because it is fixed).
* The Go to File window (⌘T) would lack the last path separator for the path shown in the status bar.
* Key equivalent field now use the normal system font (instead of Menlo and previously Monaco).
* Fix missing svn status for long committer names. *[Jeremy Whitlock]*
* WIP: Updated gutter images, presently not aligned/sized optimally. *[Dennis Vennink]*

## 2012-08-28 (a9292)

* TextMate is now built with [`libc++`](http://libcxx.llvm.org/index.html) and as a 64 bit application using the new Objective-C run-time. Ideally no changes affecting the user, but a lot has changed under the hood. *[Jacob Bandes-Storch]*
* New `TM_SCM_NAME` variable giving the name of the SCM system used for the current file (git, svn, or hg) which can be used when setting `windowTitle`. *[Adam Strzelecki]*
* Untitled documents now get their `TM_SCM_BRANCH` and `TM_SCM_NAME` variables based on the current project. *[Adam Strzelecki]*
* Update window title when application is activated for the case where the current branch (SCM) is part of the title. *[Adam Strzelecki]*
* Update look of release notes and make it a normal window (instead of floating). *[Dustin Wilson]*
* Fix crash when using Find All in the Find dialog for an untitled file (introduced in last release).

## 2012-08-27 (a9291)

* While recording a macro, several actions would cause TM to misbehave/freeze (uncaught exception).
* Using projects outside user’s home folder with symbolic links into the user’s home folder could cause a crash from the file chooser (⌘T) if doing full path search.
* Hopefully fix crash (for some people) related to file browser actions like New Folder.
* If `mate` was installed in `~/bin` then TextMate wouldn’t properly update it, but prompt for admin password on each launch (until manually updated).
* The bundle disambiguation menu now use Cocoa API with the small system font *[Jacob Bandes-Storch]*
* ⌘-clicking file browser icons (to show in Finder) now only acts on actual files *[Jacob Bandes-Storch]*
* Add New File and Open… actions to the dock menu *[Jacob Bandes-Storch]*

## 2012-08-26 (a9290)

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

## 2012-08-22 (a9289)

* You can now ⌘-click icons in the file browser for “Show in Finder” *[Jacob Bandes-Storch]*
* Bundle editor now remember column widths and item titles are truncated with ellipsis *[Gerd Knops]*
* File browser now remember its width *[Gerd Knops]*
* You can now also set `gutterSelectionBackground` in a theme (or as a global style) to affect gutter rendering *[David Howden]*
* Suppress borders and disallow horizontal scroll for Find in Folder results *[Jacob Bandes-Storch]*
* Add missing retina images *[Paul Wilde]*
* Fix issue with gutter’s border being drawn behind incremental search (and disappearing).
* Yesterday’s build was missing “double-click bundles and bundle items to install” functionality.

## 2012-08-21 (a9287)

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

## 2012-08-19 (a9286)

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

## 2012-08-15 (a9283)

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

## 2012-08-14 (a9281)

* Pressing ⌥F2 with focus in the file browser now show the context menu.
* The `TM_DROPPED_FILE` variable had a path relative to project directory instead of current file. This would make some drop commands insert wrong path.
* Dropping binary files onto the text view for which there is no drop command will now insert the path for the file dropped. Hint: You can hold down control (⌃) when dropping text files to get the “insert path” behavior.
* The `--host=auto` option to `rmate` didn’t work. *[Timothy Andrew]*
* Added simplified Chinese localization for interface files. *[Bo Xiao]*
* Bundle items with both a tab trigger and key equivalent now show both. *[Adam Strzelecki]*
* The bundle menu in the status bar can now be opened via ⌥F1 and will default to select the bundle for the current language. *[Adam Strzelecki]*
* Further fixes to handling of txmt: URLs without a file argument.

## 2012-08-13 (a9278)

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

## 2012-08-11 (a9270)

* Implement ⌃Y for yanking last deletion. *[Nathaniel Tagg]*
* Added fullscreen behavior to document window and View → Enter/Exit Full Screen menu item. *[Jesse B. Hannah]*
* When closing last document (except empty untitled ones) for a window with a visible file browser, we now keep the window around *[Mads Hartmann Jensen]*
* If TextMate was launched via `git commit` then the Git bundle would have issues (since it would inherit a wrong `GIT_DIR` environment variables)
* Preliminary support for installing bundles and bundle items (including themes) via double-click (from Finder). You can hold down option (⌥) if you wish to open them as folders/property lists.

## 2012-08-10 (a9269)

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


