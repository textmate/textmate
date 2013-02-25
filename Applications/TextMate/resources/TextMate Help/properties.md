title: File and Folder Targeted Settings

# Properties

For many settings TextMate will look for a `.tm_properties` file in the current folder and in any parent folders (up to the user’s home folder).

These are simple `setting = value` listings where the value is a format string in which other variables can be referenced.

If the setting name is uppercase it will be available as an environment variable (for commands and snippets).

For example to setup the basic stuff one could have `~/.tm_properties` with these values:

	# Settings
	theme            = "71D40D9D-AE48-11D9-920A-000D93589AF6"
	fontName         = "Menlo"
	fontSize         = 13
	fileBrowserGlob  = "{*,.tm_properties,.htaccess}"

	# Variables
	PATH             = "$PATH:$HOME/bin"
	TM_GIT           = "/opt/local/bin/git"

It is possible to target only files matching a given glob, for example:

	[ *.txt ]
	softWrap         = true

	[ .git/COMMIT_EDITMSG ]
	softWrap         = true
	spellChecking    = true
	spellingLanguage = 'en'

	[ "/usr/include/{**/,}*" ]
	tabSize          = 8

A magic value of `attr.untitled` is used for untitled files. This allows setting the file type for these, e.g.:

	[ attr.untitled ]
	fileType         = 'source.c++'

The normal TM variables are available when expanding the format strings (values). In addition a `CWD` variable is available which represents the folder from which the `.tm_properties` file is read. This is necessary to set the project directory, for example I have `~/Source/Avian/.tm_properties` with these settings:

	projectDirectory = "$CWD"
	windowTitle      = "$TM_DISPLAYNAME — Avian"
	fileChooserGlob  = "{{src,Shared/include}/**/*.{cc,mm,h},target{,s},Makefile{,.*},.tm_properties}"

The first setting effectively sets `TM_PROJECT_DIRECTORY` to `~/Source/Avian`. In `~/Source/Avian/Applications` I have a (variable) setting like this:

	TM_MAKE_TARGET = '${TM_DIRECTORY/^.*\/Applications\/([^\/]+)(\/.*)?$/$1\/run/}'

What this does is set the make target based on the current directory. So if I am editing `~/Source/Avian/Applications/mate/src/main.cc` ⌘B will make `mate/run` whereas if I am in `~/Source/Avian/Applications/Avian/src/main.cc` it will make `Avian/run`.

## Grammar

The grammar used to parse the `.tm_properties` files are as below. Whitespace (in the form of spaces or tabs) is allowed between elements.

	file:          ( «line» )*
	line:          ( «comment» | ( «section» | «assignment» )? ( «comment» )? ) ( '\n' | EOF )
	section:       '[' «name» ( ";" «name» )* ']'
	name:          ( /[^\] \t\n]/ | /\\[\] \t\n\\]/ )+
	assignment:    «key» '=' «value»
	key:           ( /[^= \t\n]/ | /\\[= \t\n\\]/ )+
	value:         ( «single_string» | «double_string» | «bare_string» )
	single_string: "'" ( /[^']/ | /\\['\\]/ )* "'"
	double_string: '"' ( /[^"]/ | /\\["\\]/ )* '"'
	bare_string:   ( /[^ \t\n]/ | /\\[ \t\n\\]/ )+
	comment:       '#' ( /[^\n]/ )*
