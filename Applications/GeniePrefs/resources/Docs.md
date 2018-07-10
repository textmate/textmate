# Documentation

## Introduction

Genie is a tool to simplify common tasks such as launching applications, performing web searches, setting timers, controlling iTunes, doing calculations, currency conversions, checking time zones, and much more.

You may already be using a tool for some of this, but Genie has a major advantage in that it is designed to give the user full control over what actions are offered and how they are presented.

All the default actions are in fact created using the same customization mechanisms which are exposed to the user. This means that you can make any change you want to the default actions, but also that when you add custom actions, these are first class citizens with all the same capabilities as “built-in” actions, and because all the default actions have been created using Genie’s customization capabilities, a lot of thought has gone into making it simple yet powerful.

As an example of this, you would normally think of an action as something you select, and a result gets produced, but Genie also allows actions to show their status in a WebView, so an action that schedules a timer can show a countdown, or an action can be created which just brings up an interactive calendar widget.

Furthermore, because nothing is hardcoded, you can also control sub-actions associated with items, for example a document normally has Open With, Show in Finder, and Move to Trash as sub-actions, but you may want to extend this list, e.g. for screenshots (images) you may want an action to share it online, or for movies, an action to play the movie and then move it to trash, etc.

## Data Sources

At its core, Genie presents you with a list of items, each of which can have an associated direct action, such as opening a document, launching an application, opening a URL, or running a script.

You can either create these items manually in Genie’s catalog editor, or you can use a data source to provide items to Genie, for example if you wish to have the ability to quit applications from Genie then you would create a data source that queries the system for running applications and then use a template to associate a proper “Quit «application»” title with the results and a script action to tell the application to quit.

In this section we will go over the three main data sources and explain how they can be used.

## Spotlight

You may already be familiar with Spotlight, as it is Apple’s search system which can already be used to query your computer for a lot of different information.

Genie’s data source for Spotlight allows you to set a custom query, this uses the [query language defined by Apple][SpotlightQueryFormat] to make the query.

Most queries would involve `kMDItemContentTypeTree` to limit the type of results to a certain type.

If you do not know what type to use, but have a file on disk of the desired type, you can use the `mdls` shell command to get information about the file, for example:

    mdls "How HHVM Uses Modern C++ for Fun and Profit.mp4"
    ⋮
    kMDItemContentType     = "public.mpeg-4"
    kMDItemContentTypeTree = (
        "public.mpeg-4",
        "public.movie",
        "public.audiovisual-content",
        "public.data",
        "public.content"
        "public.item",
    )

Here we can see that the exact type is `public.mpeg-4`, but if we do a query using `kMDItemContentType == "public.mpeg-4"` then we would be limited to movies in the MPEG4 format, so assuming that we want all movies then we should instead do a query for `kMDItemContentTypeTree == "public.movie"`.

After entering the above query you can test it by selecting Data Source → Run (⌘R) from the menu. This will open a sheet with the results found by Spotlight.

You can set the initial sort order for the results and you can also limit the query to specific folders, for example for our movie query we may limit it to `~/Movies`.

When Genie gets results from Spotlight it will convert them to items where it defaults to using the value of `kMDItemDisplayName` as the title, `kMDItemPath` as subtitle, and if the item is executed, it will open the value of `kMDItemPath` using the default handler for this file type.

This might be appropriate for certain queries, but if we are building a dedicated query for movies, we may want to make some slight changes which can be done by providing a template.

The type of the template will reflect how it should act when the item is executed, e.g. open a file or run a script.

This template can reference values from the query results as variables, for example by looking at the results when running the query in Genie’s catalog editor, we can see that movie files have a `kMDItemDurationSeconds` that we may want to display.

In the simplest case we can just make the subtitle contain `${kMDItemDurationSeconds}` but Genie supports a few value formatters and one of them is converting a number to a duration so a nicer result is achieved by using `${kMDItemDurationSeconds:/duration}`.

More about this in the section about format strings.

Some of the default data sources use Spotlight for showing you contacts, recently received email messages, selecting active developer directory based on available Xcode versions, etc.

[SpotlightQueryFormat]: https://developer.apple.com/library/archive/documentation/Carbon/Conceptual/SpotlightQuery/Concepts/QueryFormat.html

## SQLite

Using the SQLite data source is a simple way to import data from tools that store their data in this format.

For example the [shiori bookmark manager](https://github.com/RadhiFadlillah/shiori/) will (by default) store bookmarks in `~/.shiori.db` so you can use that as the database location and then make a query like this:

    SELECT id AS uid, title, url FROM bookmark ORDER BY id DESC

This will bring all the bookmarks into Genie.

The query can select database columns as the name expected by Genie for the corresponding field (for a list of fields and their meaning see the script data source section), for example in the above example our result items will have `uid`, `title`, and `url`, but like with Spotlight, we can also provide a template and then we can reference selected column names as variables in this template.

A caveat here is that both `title` and `subtitle` are interpreted as format strings, therefor if your SQLite database may contain strings that have special characters (like `$` and `\`) then they should not be selected as `title` or `subtitle`. The `match` field is not a format string, so we can select as `match` and then use `${match}` for our template’s title.

In addition to the query you can provide bindings for the query. This is mainly for nested queries, for example imagine we have a database with artists, albums, and tracks. To show all artists we would use this query:

    SELECT id AS uid, artist_name AS title FROM artists ORDER BY artist_name

Under each artist, we want the artist’s albums, so we add a template to our artist query which has another SQLite data source as child, and which uses a query like this:

    SELECT id AS uid, album_name AS title FROM albums WHERE artist_id = ? ORDER BY album_date

Then under bindings we add:

    ${parent.uid}

What this means is that when we execute the albums query, we use the `uid` of our parent item in the `WHERE` clause, and that `uid` corresponds to `artists.id`.

Under Preferences → General you can enable Clipboard History which is stored in a SQLite database, making it trivial to have Genie interact with this history.

I also recommend storing your bash history in a SQLite database, allowing Genie to be used to search your bash history and easily execute past commands.

## Script

Script is by far the most versatile data source, as it can do whatever it needs and then write result items to stdout as a JSON data structure.

The structure should have the following format:

    {
        "items": [
            {
                "uid":   "FDAD2014-2BBE-4326-AC25-3C717EB15B2C",
                "title": "Some item",
                "url":   "https://example.org"
                "icon":  { "image": "path/to/image.png" },
            },
            ...
        ],
        "expires_in": "10m"
    }

The following keys in the item dictionary decides how the item is displayed:

- `title`: The title of the item.
- `subtitle`: The subtitle of the item.
- `icon`: A dictionary which describes how to create the icon for this item.

You should include one of the following keys to provide the item with an action (executed when pressing return or double-clicking the item):

- `file`: A file to open if the item is executed.
- `url`: A URL to open if the item is executed.
- `exec`: This is an array where first argument is the command to run and the following elements are passed as arguments to this command.

If you want the item to have sub-items then you can include a `children` key where the value is an array of sub-items. The user can get to these items either by pressing tab or ⌘↓ when the parent item is selected.

### Caching and Cache Invalidation

By default Genie will run script data sources once when the user brings up the main window, although it will show results from last execution until it gets the new results.

If the script data source references `${query}` in its arguments then it will also run each time the query string changes, which is how the currency converter and calculator data sources work.

You may however have a data source that takes a long time to produce its results, and where results are unlikely to change, for example the Emoji Database data source uses an online reference to obtain the list of emojis, but as this is rarely changing, it does not make sense to fetch this data each time the user brings up the main window, so for this data source we include the `expires_in` key and sets it to `1w`.

This means that Genie should not call the data source again until at least one week has passed.

You may also use this to force the data source to run even before the user has closed the window, for example the iTunes play/pause data source sets `expires_in` to `1s` which means that it reloads every second to update the play status of iTunes.

This is effectively polling and should generally be avoided, but as the iTunes play/pause data source is only active when you descend into iTunes, it has limited overhead.

Another useful key is `depends_on` which can be set to a file that the results depend on. For example the iTunes artists and albums data source depends on `iTunes Music Library.xml` so by declaring that, Genie will cache the results until iTunes has updated the XML file.

Like with `expires_in`, should the dependency change while the main window is showing, Genie will re-run the script data source. The currency conversion makes use of this feature by immediately returning results to Genie using whatever version of `rates.json` is in the cache folder. But it sets `depends_on` to `/path/to/rates.json` and if these are too old, it spawns a background process to update them, which will cause Genie to update the results automatically, once the background process completes updating the rates.

Lastly there is the ability to explicitly invalidate cached data source items using the `invalidate` key in an item. This should be set to the identifier of the data source that should have its cached results invalidated, and the invalidation will happen, if the item is executed.

For example the Google Drive data source fetches items from your Google Drive and sets `expires_in` to `10m`, but it also offers actions like New Document, which would cause a new item to appear on your Google Drive. Therefor the New Document item sets `invalidate` to `https://www.googleapis.com/drive/v3/files` (the identifier of the Google Drive data source), so when creating new Google Drive items, the cached Google Drive results will immediately be made invalid.

## Predicate Groups

In the catalog editor you can create Predicate Groups which are groups of items that will appear as sub-items for any item that matches the predicate.

The predicate is using [Apple’s predicate format string syntax][PredicateFormat] which is close to Spotlight’s query language, but there are some subtle differences, for example where in Spotlight we use `kMDItemContentTypeTree == "public.movie"` to match all movie files, if we want our predicate to evaluate true for all movie files, we would either use `"public.movie" IN kMDItemContentTypeTree` or (recommended) `kMDItemContentType UTI-CONFORMS-TO "public.movie"`.

A predicate can test against any of the item’s properties, for example the iTunes artists/albums data source includes a `iTunesTrackId` property for all tracks, which allows us to add sub-items to all tracks returned by this data source by creating a predicate group for `iTunesTrackId != NULL` .

[PredicateFormat]: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Predicates/Articles/pSyntax.html

## Actions

Each item can have an action associated with it, in the simplest case this can be a file or URL to open, but to allow flexibility, an action can also be a user defined script.

The script is created in the catalog editor. If there is no shebang on the first line (like `#!/usr/bin/ruby`) then the script will be executed using `/bin/sh -c «script» «arguments»`.

It is possible to provide arguments to the script. These are format strings and makes it possible to pass values to the script, such as the current query string or properties from the item being executed, the latter being useful when a template is used for data source results, e.g. the Quit Application is a spotlight query for running applications and the template is a script action that takes `${kMDItemCFBundleIdentifier}` as argument, as the bundle identifier of the application to quit.

In addition to arguments passed to the script, a script can also read selected items as JSON from standard input, which allows to have a single action run for multiple selected items.

When multiple items are selected and the user press return Genie will group the items based on their action and arguments.

All items with same action (i.e. script source code) and arguments (after expanding the format strings) will only cause the first item’s script to be executed, but providing all of the items as JSON through its standard input.

In practice this means that if your action should act differently when multiple items are selected, it should read selected items from standard input, but if it is OK to execute the items one-by-one, it can rely on argument passing.

If a script terminates with a non-zero return code, an alert is shown to the user, as this is considered an abnormal state.

If a script writes to standard output then a notification is shown to the user.

### HTML Output

Items can show HTML output in Genie’s list of items.

This is shown below the item, but only when the item is selected.

To indicate that your item has HTML output you need to use the “Advanced…” button in the catalog editor and add `output = "html";` to the property list shown.

After this, your item’s script will be executed with the `GENIE_HTML_OUTPUT` environment variable set to `1`.

Genie will execute your script once the user selects your item. If you need the HTML output to be dynamic or interactive, you should use JavaScript. Be aware that Genie will not dynamically resize the view port for your HTML output, so you should make sure that your view port has the proper size on first load.

To see potential errors from your JavaScript you can run the following in a terminal:

    log stream --level info --predicate 'subsystem == "com.macromates.JavaScript"'

The JavaScript running has access to the following API:

    genie.log(str)                            -- log string to console
    genie.exec(argv, callback)                -- execute argv[0] with argv[1...] as arguments
                                                 calback is passed (returnCode, stdout, stderr)
    genie.query                               -- read/write of current query string
    window.addEventListener("querychange", …) -- posted when query string changes

## References

### Variables

The following variables can be used in an item’s title, subtitle, URL, file, script arguments, SQLite bindings, icon’s text value, and a few additional fields only available by using the “Advanced…” button (see schema).

* `query`: The query is anything the user types after the initial filter string, e.g. `st 4` will treat `4` as the query. For an item set to match `*` the entire filter string is treated as the query.
* `last_query`: This is the last query used with this item. This is useful when you want to have a value default to the last query used, which would be expressed as `${query:-${last_query}}`.
* `clipboard`: The contents of the regular clipboard.
* `find`: The contents of the find clipboard.
* `parent`: This can be used to reference fields of the parent item. For example we may make a generic “Move to Trash” sub-item that would use `${parent.file}` as the file which should be trashed.
* _«any other field»_: Any field in the item is available as a variable in those fields marked as format strings. For example a data source’s template will often reference variables generated by the data source.

When using variables in the URL field, the values are automatically URL-encoded.

### Format Strings

A format string is a string where variables are expanded and possibly formatted, the following syntax is available:

    ${«var»:?«if»:«else»}                -- insert «if» when «var» is set, otherwise «else»
    ${«var»:+«if»}                       -- insert «if» when «var» is set
    ${«var»:-«else»}                     -- insert «else» when «var» is unset
    ${«var»/«regexp»/«format»/«options»} -- do a regexp replacement on «var» and insert the result
                                            Options are: g = global (repeat), i = case insensitive
                                            In «format» captures are available as variables $0-$n
    
    ${«var»:/«filter_1»[/«filter_2»[/«filter_n»]]} -- filter «var» using one or more of these:
    
        upcase        Convert «var» to UPPERCASE
        downcase      Convert «var» to lowercase
        titlecase     Convert «var» to Titlecase
        capitalize    Alias for titlecase
        asciify       Transliterate «var» using ASCII characters
        urlencode     Escape characters with special meaning in URLs
        shellescape   Escape characters with special meaning in a shell
        number        Treat «var» as a number and show with thousand separators
        duration      Treat «var» as number of seconds and show as a duration, e.g. “2 minutes”
        relative      Treat «var» as a date (YYYY-MM-DD) and show as e.g. “2 days ago”
        dirname       Treat «var» as a path and show its dirname
        basename      Treat «var» as a path and show its basename

### Schema

While it is recommended that items be created and updated in the catalog editor, not all fields are exposed through this editor, and it may not be clear if a field is a format string or not.

Furthermore, a script data source can generate arbitrary items that must follow the same schema as edited by the catalog editor, therefor the full schema is shown below:

    {
        // Abstract types
        FormatString     = "NSString|NSNumber";     // String allow variables like: ${query}, ${last_query}, ${clipboard}, ${find}
        TimePeriod       = "NSString|.doubleValue"; // Example: 2w, 3d, 5h, 8m, 13s, 2.1 (default unit is seconds)
        Boolean          = ".boolValue";            // Must respond to: boolValue
        Float            = ".doubleValue";          // Must respond to: doubleValue
    
        // Structure of ~/Library/Application Support/Genie/Items.plist
        Root = {
            items     = ( "Item" );
            variables = (
                { name = "NSString"; value = "FormatString"; disabled = "Boolean"; }
            );
        };
    
        // Required structure of data source result
        DataSourceResult = {
            items     = ( "Item" );
            expires_in = "TimePeriod";
            depends_on = "NSString";                // File that the data depends on, e.g. ‘~/Music/iTunes/iTunes Music Library.xml’
        };
    
        // Structure of a single item
        Item = {
            uid           = "NSString";
    
            kind          = "NSString";            // Values: (web|script|file|sqlite|spotlight|recentDocuments|exec)
            predicate     = "NSString";            // Make children of this item be children of items matching predicate
            isTemplate    = "Boolean";             // This is a template to be used by parent data source
            isPlaceholder = "Boolean";             // This is a placeholder to be used by parent data source
    
            disabled      = "Boolean";
            readOnly      = "Boolean";             // TODO
    
            title         = "FormatString";
            subtitle      = "FormatString";
            match         = "NSString";            // Match the filter string against this field instead of title
            uiTitle       = "FormatString";        // Title used for notifications and other dialogs
    
            icon = {
                image       = "NSString";          // Path to icon file
                fileType    = "NSString";          // UTI, file type extension, or HFS file type
                application = "NSString";          // Application bundle identifier, e.g. ‘com.macromates.TextMate’
                text        = "FormatString";      // Arbitrary text, useful for emojis
                named       = "NSString";          // System image as defined in NSImage.h
                file        = "NSString";          // Use the icon of an existing file
                url         = "NSString";          // Obtain favicon from URL
                alpha       = "Float";             // Set the icon’s alpha value
            };
    
            disableLRUOrdering   = "Boolean";      // Recently used items will not be shown at the top
            disableLearning      = "Boolean";      // Recently used items with the current filter string will not be shown at the top
            disableFuzzyMatching = "Boolean";      // Filter string must be a contiguous substring of the item to be matched
            disableRankOrdering  = "Boolean";      // Items deemed a “better” match of the filter string will not be shown at the top
    
            exec                = ( "NSString" );      // Defaults to: exec = ( "/bin/sh", "-c", «script», «scriptArguments»... );
            script              = "NSString";          // When there is a shebang: exec = ( "/path/to/script", «scriptArguments»... );
            scriptArguments     = ( "FormatString" );  // These are appended to exec and are also set for multi-select
            standardInputString = "FormatString";      // Defaults to an array of selected items as JSON
            output              = "NSString";          // Values: (notification|html). Defaults to "notification"
            live                = "Boolean";           // TODO Idea is to control how often scripts generating HMTL output should run
            variables           = (
                { name = "NSString"; value = "FormatString"; disabled = "Boolean"; }
            );
    
            url        = "FormatString";
            file       = "FormatString";
            value      = "FormatString";           // Fallback for ⇥ and ⌘C
    
            invalidate = "FormatString";           // Set to uid of data source which cache should be invalidated
    
            option     = "Item";                   // Alternative item to use when option (⌥) is down
    
            children   = ( "Item" );               // Child items accessible with ⇥ or ⌘↓
    
            // ====================
            // = Data Source Keys =
            // ====================
    
            bundleIdentifier = "FormatString";     // Used by: recentDocuments
    
            sqlDatabase      = "FormatString";     // Used by: sqlite3
            sqlQuery         = "NSString";         // Used by: sqlite3
            sqlBindings      = ( "FormatString" ); // Used by: sqlite3
    
            mdQuery          = "NSString";         // Used by: spotlight
            mdScope          = (                   // Used by: spotlight
                { path = "NSString"; disabled = "Boolean"; }
            );
    
            mdApplicationIsRunning = "Boolean";      // Limit Spotlight results to applications that are running
            mdApplicationCanOpen   = "FormatString"; // Limit Spotlight results to applications which can open this file/URL
    
            sortBy           = "NSString";           // Which key to use for sorting
            descending       = "Boolean";
    
            // exec, script, scriptArguments, and output (= json|list) for ‘kind = exec’ (re-used for ‘kind = commnad’)
            timeOut          = "NSNumber";           // Kill script/script data source after n seconds
        };
    }
