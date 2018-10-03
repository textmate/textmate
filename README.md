# TextMate

## Download

You can [download TextMate from here](http://macromates.com/download).

## Feedback

You can use [the TextMate mailing list](http://lists.macromates.com/listinfo/textmate) or [#textmate][] IRC channel on [freenode.net][] for questions, comments, and bug reports.

You can also [contact MacroMates](http://macromates.com/contact).

Before you submit a bug report please read the [writing bug reports](http://kb.textmate.org/writing_bug_reports) instructions.

## Screenshot

![textmate](https://raw.github.com/textmate/textmate/gh-pages/images/screenshot.png)

# Building

## Bootstrap

To bootstrap the build you need to run `./configure` (in the root of the source tree). You can set a few (environment) variables read by this script that change the generated build file:

* `builddir` — location of built files. Defaults to `~/build/TextMate`.
* `identity` — for Apple’s `codesign`. Defaults to ad-hoc signing, which does not use an identity at all.
* `boostdir` — location of boost includes. By default it will search various locations including MacPorts and Homebrew.
* `sparsedir` — location of sparsehash includes. By default it will search various locations including MacPorts and Homebrew.
* `CC` and `CXX` — C and C++ compiler.

In the simplest case (assuming [Homebrew][] is installed) you would run:

	brew install ragel boost multimarkdown hg ninja capnp google-sparsehash libressl
	git clone --recursive https://github.com/textmate/textmate.git
	cd textmate
	./configure && ninja

If you're using [MacPorts][] then instead run this line to install dependencies:

	sudo port install ninja ragel boost multimarkdown mercurial sparsehash libressl

Unless you’re using [Homebrew][] then [Cap’n Proto][capnp] must be manually installed. Feel free to submit a PR to update `configure`.

If `port` fails with a build error then likely you need to agree (system-wide) to Apple’s Xcode license:

	sudo xcodebuild -license

## Prerequisites

Building TextMate has the following dependencies:

 * [ninja][]         — build system similar to `make`
 * [ragel][]         — state machine compiler
 * [boost][]         — portable C++ source libraries
 * [sparsehash][]    — A cache friendly hash_map
 * [multimarkdown][] — marked-up plain text compiler
 * [mercurial][]     — distributed SCM system
 * [Cap’n Proto][capnp] — serialization library
 * [LibreSSL][libressl] - OpenBSD fork of OpenSSL

In practice `hg` ([mercurial][]) is only required for the SCM library’s tests so you can skip this dependency if you don’t mind a failing test.

If you want to avoid the libressl linker warnings about being built for different deployment target then run `brew edit libressl` and make the following change:

	-    system "./configure", *args
	+    system "env", "LDFLAGS=-mmacosx-version-min=10.8", "CFLAGS=-mmacosx-version-min=10.8", "./configure", *args

Afterward you must rebuild using: `brew reinstall --build-from-source libressl`

## Building from within TextMate

You should install the [Ninja][NinjaBundle] bundle which can be installed via _Preferences_ → _Bundles_.

After this you can press ⌘B to build from within TextMate. In case you haven't already you also need to set up the `PATH` variable either in _Preferences_ → _Variables_ or `~/.tm_properties` so it can find `ninja` and related tools; an example could be `$PATH:/opt/local/bin`.

The default target is `TextMate/run`. This will relaunch TextMate but when called from within TextMate, a dialog will appear before the current instance is killed. As there is full session restore, it is safe to relaunch even with unsaved changes.

If the current file is a test file then the target to build is changed to build the library to which the test belongs (this is done by setting `TM_NINJA_TARGET` in the `.tm_properties` file found in the root of the source tree).
Similarly, if the current file belongs to an application target (other than `TextMate.app`) then `TM_NINJA_TARGET` is set to build and run this application.

## Build Targets

The build system classifies a target either as a library or an application. The latter can either be a bundled or non-bundled application. E.g. `mate` is non-bundled (just a `mate` executable) where `TextMate.app` is a bundled application.

For each output there are a few symbolic targets you can build. While the examples below refer to a specific library or application, they exist for all targets of same type.

For the `io` library:

	ninja io                 # Build the io library and run tests.
	ninja io/clean           # Remove the build folder for the io library.
	ninja io/headers         # Copy exported headers to $builddir/include.

For the `mate` (non-bundled) application:

	ninja mate               # Build the mate executable.
	ninja mate/run           # Build and run the mate executable.
	ninja mate/clean         # Remove the build folder for the mate executable.

For the `TextMate.app` application:

	ninja TextMate           # Build and sign TextMate.app.
	ninja TextMate/run       # Build, sign, and run TextMate.app.
	ninja TextMate/clean     # Remove the build folder for TextMate.app.
	ninja TextMate/dsym      # Create a tarball with extracted dSYM files.
	ninja TextMate/tbz       # Create a tarball of TextMate.app. Also produce the dsym tarball.
	ninja TextMate/deploy    # Push a nightly build. Fails without proper credentials :)

Note that `ninja TextMate/clean` only cleans the TextMate build folder (`$builddir/Applications/TextMate`), but all libraries and applications it depends on are not cleaned.

To clean everything run:

	ninja -t clean

# Legal

The source for TextMate is released under the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

TextMate is a trademark of Allan Odgaard.

[boost]:         http://www.boost.org/
[ninja]:         https://ninja-build.org/
[multimarkdown]: http://fletcherpenney.net/multimarkdown/
[ragel]:         http://www.complang.org/ragel/
[mercurial]:     https://www.mercurial-scm.org/
[capnp]:         https://github.com/capnproto/capnproto.git
[libressl]:      http://www.libressl.org
[MacPorts]:      http://www.macports.org/
[Homebrew]:      http://brew.sh/
[NinjaBundle]:   https://github.com/textmate/ninja.tmbundle
[sparsehash]:    https://code.google.com/p/sparsehash/
[#textmate]:     irc://irc.freenode.net/#textmate
[freenode.net]:  http://freenode.net/
