title: TextMate Help
meta: AppleTitle="TextMate 2 Help", AppleIcon="TextMate Help/images/tm_small.png"

# TextMate Help

Some non-visible stuff.

## Mouse Gestures

 * ⌘-click to add a new caret.
 * ⌥-click close buttons to close other tabs.
 * Hold ⌃ when dropping file to text view to insert its path.

Holding ⌥ to “close other” also works in the file browser or when using ⌘T (and ⌘T now does multi-select).

One exception to “⌥ closes other” is when single-clicking the icon of a bundle (e.g. `.app`) in the file browser. In this case the single-click works as “Show Package Contents”.

## Completion (⎋)

The current word suffix is taken into consideration when hitting escape.

For example in this case:

	enum mark_t { kErrorMarkType, kWarningMarkType };
	mark_t m = ‸MarkType;

The resulting line will become:

	mark_t m = kWarning‸MarkType;

## Grammar

 * Scope names are format strings and can reference captures ($0, $1, $n).
 * `\G` matches end of parent rule’s `begin` — or end of last stacked `while`.
 * `begin`/`while` rule construct.
 * Includes can reference grammar#repos.

## Bundles

 * Shell variables are format strings and can e.g. reference `TM_SUPPORT_PATH`.
 * Completion commands have all variables set from current context.

## Discontinuous Selection

Activated by typing/deleting or using a leftward or rightward movement while a column selection is active. Alternatively use “Find All”

## Find History

Just like clipboard history: ⌃⌥⌘F.

## Other

Possible to enter e.g. `main.{cc,h}` in a Save As dialog for brace expansion (saves as first expansion, background tabs are created for further expansions).

⌘T can filter on full path by including `/`, extension by starting with `.`, can go to a line by suffixing with a line specification (see elsewhere for syntax)

Using ⌘T with find clipboard containing `«file»:«line»` will use that as default text.

## Syntax / API

* [Bundle Dependencies][]
* [Format String Syntax][]
* [Glob String Syntax][]
* [JavaScript Object][]
* [Scope Selector Syntax][]
* [Selection String Syntax][]
* [Folder Specific Properties][]
* [Non-Content Scopes][]
* [Events / Filters][]
* [mate & rmate](mate_and_rmate.html)

[Bundle Dependencies]:        bundle_dependencies.html
[Format String Syntax]:       format_string_syntax.html
[Glob String Syntax]:         glob_string_syntax.html
[JavaScript Object]:          javascript_object.html
[Scope Selector Syntax]:      scope_selector_syntax.html
[Selection String Syntax]:    selection_string_syntax.html
[Folder Specific Properties]: properties.html
[Non-Content Scopes]:         non-content_scopes.html
[Events / Filters]:           events.html
