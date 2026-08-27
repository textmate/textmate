# TextMate 3

TextMate is a powerful, customizable text editor for macOS. TextMate 3 keeps the established editor and bundle ecosystem while incrementally modernizing the app with SwiftUI, current macOS APIs, stronger security boundaries, and a maintained build and test toolchain.

TextMate is free and open-source software. No registration or license key is required.

## Status

This branch builds TextMate 3 with a minimum deployment target of macOS 14. It is active modernization work rather than a notarized production release. The [official download page](https://macromates.com/download) continues to provide the latest published stable build.

See [Applications/TextMate/about/Changes.md](Applications/TextMate/about/Changes.md) for release notes and [Applications/TextMate/SwiftUI/MIGRATION.md](Applications/TextMate/SwiftUI/MIGRATION.md) for the incremental SwiftUI and WebView migration plan.

## Features

* Native macOS editor with projects, multiple carets, snippets, macros, and configurable themes.
* Extensible bundle system for languages, commands, grammars, templates, and workflows.
* Native SwiftUI command output with selectable streaming text.
* Native TextMate 3 About window and modern Settings toolbar.
* Hardened remote editing, privileged-helper, update, dialog, and commit-window trust boundaries.
* GPLv3-or-later source with no product registration requirement.

## Requirements

Building requires:

* macOS 14 or later.
* A current full Xcode installation with command-line tools selected.
* [Boost](https://www.boost.org/).
* [Cap’n Proto](https://capnproto.org/).
* [Google SparseHash](https://github.com/sparsehash/sparsehash).
* [MultiMarkdown](https://fletcher.github.io/MultiMarkdown-6/).
* [Ninja](https://ninja-build.org/).
* [Ragel](https://www.colm.net/open-source/ragel/).

Install the command-line dependencies with Homebrew:

```sh
brew install boost capnp google-sparsehash multimarkdown ninja ragel
```

MacPorts users can install the equivalent packages:

```sh
sudo port install boost capnproto multimarkdown ninja ragel sparsehash
```

## Quick start

Clone the complete repository, including submodules:

```sh
git clone --recursive https://github.com/textmate/textmate.git
cd textmate
```

If the repository was cloned without `--recursive`, initialize it before configuring:

```sh
git submodule update --init --recursive
```

On Apple silicon Homebrew systems, create `local.rave` with the Homebrew include and library locations before running the configure script:

```text
add FLAGS    "-I/opt/homebrew/include"
add LN_FLAGS "-L/opt/homebrew/lib"
```

Intel Homebrew and MacPorts users should adjust those two paths for their package prefix. `local.rave` is intentionally local configuration and is not committed.

Generate the Ninja graph and build TextMate:

```sh
./configure
ninja TextMate
```

The generated application is placed below `~/build/textmate/` in the selected configuration directory. The default configure flow uses the release configuration and ad-hoc code signing.

To build, sign, and launch the application:

```sh
ninja TextMate/run
```

Do not replace a production TextMate installation with an unnotarized development build unless you understand the signing and runtime differences.

## Build configurations

Rave generates `build.ninja` and supports release and debug configurations:

```sh
bin/rave -crelease -tTextMate
bin/rave -cdebug -tTextMate
```

Debug builds enable Address Sanitizer. Release builds enable ThinLTO and dead-code stripping. Both target macOS 14 and use ad-hoc signing unless a local signing identity is configured.

Useful targets:

```sh
ninja TextMate              # Build and sign TextMate
ninja TextMate/run          # Build, sign, and relaunch TextMate
ninja bundles/test          # Run bundle framework tests
ninja HTMLOutput/test       # Run HTML/native-output tests
ninja authorization/test    # Run privileged IPC framing tests
```

Run the SwiftUI package tests separately:

```sh
swift test --package-path Applications/TextMate/SwiftUI
```

List all generated targets with `ninja -t targets all`. Clean generated output with `ninja -t clean`; the out-of-tree build directory can also be removed when a fully clean rebuild is needed.

## Building from TextMate

Install the [Ninja bundle](https://github.com/textmate/ninja.tmbundle) from **Settings → Bundles**, then press ⌘B. Ensure the PATH configured under **Settings → Variables** or in `~/.tm_properties` contains Ninja and the other build tools. Typical Homebrew additions are `/opt/homebrew/bin` on Apple silicon and `/usr/local/bin` on Intel Macs.

The repository’s `.tm_properties` selects `TextMate/run` by default and chooses a more focused target when the current file belongs to a test or another application target.

## Bundles and plug-ins

TextMate loads bundles from these locations:

* Built-in: `TextMate.app/Contents/SharedSupport/Bundles/`
* User-installed or modified: `~/Library/Application Support/TextMate/Bundles/`
* Managed pristine copies: `~/Library/Application Support/TextMate/Pristine Copy/Bundles/`
* Legacy TextMate 2 alpha/beta: `~/Library/Application Support/Avian/Bundles/`

The built-in Avian bundle uses a self-contained system-Ruby support library. Its current encrypted format is authenticated; historical OpenSSL `Salted__` files remain readable for migration.

## Architecture

TextMate combines an AppKit/Objective-C++ application shell with C++ editor and framework code. Rave generates the Ninja build graph. New native surfaces live in the `TextMateUI` Swift package and are hosted through narrow C interfaces so the editor can be modernized incrementally without destabilizing core editing.

Legacy HTML rendering remains as an explicit compatibility path, but it no longer receives the removed privileged JavaScript-to-native bridge. New UI work should use native SwiftUI/AppKit interfaces or tightly constrained WKWebView content.

## Contributing and support

Read [CONTRIBUTING.md](CONTRIBUTING.md) before submitting a patch. Keep commits focused, include tests where practical, and describe security or compatibility trade-offs in the commit message.

* Report reproducible defects through [GitHub Issues](https://github.com/textmate/textmate/issues).
* Review the project’s [bug-reporting guidance](https://github.com/textmate/textmate/wiki/writing-bug-reports).
* General TextMate support is available from [MacroMates](https://macromates.com/support).

Please do not include secrets, private documents, or live credentials in public bug reports.

## License

TextMate source is distributed under the GNU General Public License, version 3 or (at your option) any later version. See [LICENSE](LICENSE) and [COPYING](COPYING).

TextMate is a trademark of Allan Odgaard.
