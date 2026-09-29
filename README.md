# TextMate

## Download

You can [download TextMate from here](https://macromates.com/download).

## Feedback

You can use [the TextMate mailing list](https://lists.macromates.com/listinfo/textmate) or [#textmate][] IRC channel on [freenode.net][] for questions, comments, and bug reports.

You can also [contact MacroMates](https://macromates.com/support).

Before you submit a bug report please read the [writing bug reports](https://github.com/textmate/textmate/wiki/writing-bug-reports) instructions.

## Screenshot

![textmate](https://raw.github.com/textmate/textmate/gh-pages/images/screenshot.png)

# Building

TextMate builds with **Xcode**. The Xcode project is self-contained — it compiles the checked-in generated sources directly and links the dependencies below.

## Setup

Build the static third-party dependencies into the checkout:

```sh
bin/build_universal_dependencies
```

This downloads Boost, Cap’n Proto, and google-sparsehash, builds those plus
Onigmo for `arm64` and `x86_64`, and writes universal libraries plus headers to
`vendor/prebuilt`. The Xcode project links against that repo-local prefix, so
Homebrew libraries are not used for normal app builds.

Set `MACOSX_DEPLOYMENT_TARGET`, `BOOST_VERSION`, `CAPNP_VERSION`, or
`SPARSEHASH_VERSION` in the environment to override the script defaults.

`ragel` and `multimarkdown` are only needed if you regenerate the checked-in generated sources (see below); they are not required for a normal build.

Make sure you have a full checkout including submodules:

```sh
git clone --recursive https://github.com/textmate/textmate.git
cd textmate
```

### Build the Onigmo dependency

The regular-expression library (Onigmo) is vendored and built with its own autotools setup, producing the static `libonig.a` that the app links against. Build it once after checkout:

```sh
cd vendor/Onigmo/vendor
./configure && make
cd -
```

## Building the app

Open `Applications/TextMate/TextMate.xcodeproj` in Xcode and build/run the `TextMate` scheme (⌘R), or from the command line:

```sh
xcodebuild -project Applications/TextMate/TextMate.xcodeproj -scheme TextMate -configuration Debug build
```

## Regenerating generated sources (optional)

Some sources are generated and committed to the repository, so a normal build does not run any code generators. Regenerate them only when you edit the corresponding schema/source:

 * **Cap’n Proto** — `*.capnp` → `*.capnp.c++` / `*.capnp.h` (requires `capnp`)
 * **Ragel** — `Frameworks/plist/src/ascii.rl` → `ascii.cc` (requires `ragel`)

# Python Diagnostics

This fork ships a *Python Diagnostics* bundle (in `Bundles/`, copied into the app at build time). Whenever a Python file is saved, it runs [Ruff][] and [Pyright][] and shows the results directly in the editor: a gutter icon, a tinted line, a squiggly underline under the offending range, and an Xcode-style message banner at the right edge. Clicking the banner’s icon opens a popover with all issues on that line and offers Ruff’s auto-fixes. Option-click the icon to also show Pyright actions that add `# type: ignore` to the affected line or disable the reported rule in the nearest `pyrightconfig.json`.

## Getting started

1. Install the checkers (either one alone also works):

   ```sh
   brew install ruff pyright
   ```

2. Save a Python file. That’s it — diagnostics appear a moment later (Ruff instantly, Pyright after a few seconds). If something is missing, a notification tells you what to install.

To normalize the current document—including indentation inside multiline lists—run *Bundles → Python Diagnostics → Format Python with Ruff*. The command formats the editor buffer, adopts the document’s current tabs/spaces and tab-width settings, and uses the nearest Ruff configuration for all other options.

If neither checker is installed, or the `mate` command-line tool cannot be found, the bundle posts a macOS notification (at most once per hour) explaining what to do. Detailed logging for troubleshooting: `tail -f /tmp/tm-python-diagnostics.log`.

## Disabling

Add to any `.tm_properties` file (global `~/.tm_properties`, per project, or per directory — TextMate’s usual scoping applies):

```
TM_PYTHON_DIAGNOSTICS = disabled
```

You can also disable the whole bundle under *Preferences → Bundles → Python Diagnostics*, or scope the variable to specific paths:

```
[ vendor/** ]
TM_PYTHON_DIAGNOSTICS = disabled
```

## Configuration

Both tools use their native configuration files:

 * **Ruff** looks for `ruff.toml` / `.ruff.toml` / `pyproject.toml` next to (or above) the checked file; without one it falls back to the user-level `~/.config/ruff/ruff.toml`.
 * **Pyright** — the bundle searches upward from the file for a `pyrightconfig.json`; without one it uses `~/Library/Application Support/TextMate/pyrightconfig.json` (e.g. for `extraPaths` to custom module stubs). Override the fallback with the `TM_PYRIGHT_PROJECT` variable.

Pyright targets Python 3.13 by default. To select another language version, set `TM_PYRIGHT_PYTHON_VERSION` under *Preferences → Variables* or in `.tm_properties`:

```
TM_PYRIGHT_PYTHON_VERSION = 3.14
```

Extra command-line flags can be passed via the `TM_RUFF_ARGS` and `TM_PYRIGHT_ARGS` variables; `TM_RUFF` / `TM_PYRIGHT` override the tool binaries themselves. An explicit `--pythonversion` in `TM_PYRIGHT_ARGS` takes precedence over `TM_PYRIGHT_PYTHON_VERSION`.

[Ruff]:    https://docs.astral.sh/ruff/
[Pyright]: https://microsoft.github.io/pyright/

# Legal

The source for TextMate is released under the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

TextMate is a trademark of Allan Odgaard.

[boost]:         http://www.boost.org/
[multimarkdown]: http://fletcherpenney.net/multimarkdown/
[ragel]:         http://www.complang.org/ragel/
[capnp]:         https://github.com/capnproto/capnproto.git
[Homebrew]:      http://brew.sh/
[sparsehash]:    https://code.google.com/p/sparsehash/
[#textmate]:     irc://irc.freenode.net/#textmate
[freenode.net]:  http://freenode.net/
