# TextMate

This repository contains the source code for TextMate 2, a text editor for OS X 10.7+.

![textmate](https://raw.github.com/textmate/textmate/gh-pages/images/screenshot.png)

# Building

To bootstrap the build you need to run `./configure` (in the root of the source tree). You can set a few (environment) variables read by this script that change the generated build file:

* `builddir` — location of built files. Defaults to `~/build/TextMate`.
* `identity` — for Apple’s `codesign`. Defaults to ad-hoc signing, which does not use an identity at all.

In the simplest case you would run:

	git clone https://github.com/textmate/textmate.git
	cd textmate
	git submodule update --init
	./configure && ninja

Please note that if you downloaded the source code (rather than cloned via git) you likely miss the submodules and the build will therefore fail.

## Prerequisites

To build the source the following must first be installed on your system:

 * [ninja][]         — build system similar to `make`
 * [ragel][]         — state machine compiler
 * [boost][]         — portable C++ source libraries
 * [multimarkdown][] — marked-up plain text compiler
 * [mercurial][]     — distributed SCM system

To install using [MacPorts][] run:

	sudo port install ninja ragel boost multimarkdown mercurial

If `port` fails with a build error then likely you need to agree (system-wide) to Apple’s Xcode license:

	sudo xcodebuild -license

You can also install the above using [homebrew][]:

	brew install ragel boost multimarkdown hg ninja

In practice `hg` ([mercurial][]) is only required for the SCM library’s tests so you can skip this dependency if you don’t mind a failing test.

### OS X 10.7 (Lion)

If you are on OS X 10.7 you need `pgrep` and `pkill` (used by the “relaunch” build targets). To install using [MacPorts][]:

	sudo port install proctools

Or using [homebrew][]:

	brew install proctools

### Clang 3.2 / 4.0

You also need a recent version of clang. This should be included with Xcode 4.4+ (available for both Lion and Mountain Lion). If you don’t have it, you can build [clang 3.2][] from [MacPorts][]:

	sudo port install clang-3.2 clang_select
	sudo port select clang mp-clang-3.2

Or using [homebrew][]:
 
	brew install --HEAD llvm --with-clang

## Building from within TextMate

You should install the [Ninja][NinjaBundle] and [CxxTest][] bundles. Both can be installed via _Preferences_ → _Bundles_.

After this you can press ⌘B to build from within TextMate. In case you haven't already you also need to set up the `PATH` variable either in _Preferences_ → _Variables_ or `~/.tm_properties` so it can find `ninja` and related tools; an example could be `$PATH:/opt/local/bin`.

The default target is `TextMate/run`. This will relaunch TextMate but when called from within TextMate, a dialog will appear before the current instance is killed. As there is full session restore, it is safe to relaunch even with unsaved changes.

If the current file is a test file then the target to build is changed to build the library to which the test belongs (this is done by setting `TM_NINJA_TARGET` in the `.tm_properties` file found in the root of the source tree).

Similarly, if the current file belongs to an application target (other than `TextMate.app`) then `TM_NINJA_TARGET` is set to build and run this application.

## Build Targets

The build system classifies a target either as a library or an application. The latter can either be a bundled or non-bundled application. E.g. `mate` is non-bundled (just a `mate` executable) where `TextMate.app` is a bundled application.

For each output there are a few symbolic targets you can build. While the examples below refer to a specific library or application, they exist for all targets of same type.

For the `io` library:

	ninja io                 # Build the io library and run tests.
	ninja io/coerce          # Build the io library and skip tests.
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

Note that `ninja TextMate/clean` only cleans the TextMate build folder (`$builddir/Applications/TextMate`) but all libraries and applications it depends on, are not cleaned.

To clean everything run:

	ninja -t clean

# Contributing

You can send pull requests via GitHub. Patches should:

1. Follow the style of the existing code.
2. One commit should do exactly one thing.
3. Commit messages should start with a summary line below 80 characters followed by a blank line, and then the reasoning/analysis for why the change was made (if appropriate).
4. Commits that fix a bug in a previous commit (which has already been merged) should start with `fixup!` and then the summary line of the commit it fixes. If you are writing your commit message in TextMate then type `fix⇥` to get the prefix and a menu allowing you to pick the summary line from one of the last 15 commits.
5. Rebase your branch against the upstream’s master. We don’t want to pull redundant merge commits.
6. **Be clear about what license applies to your patch:** The files within this repository are under the [GPL 3][] (or later) but (as the original creator) we are still allowed to create non-free derivatives. However, if patches are given to us under GPL then those cannot make it into any non-free derivatives we may later wish to create. So to make it easier for us (and avoid any legal issues) we prefer if patches are released as public domain.

There is both the [textmate-dev][] mailing list and [#textmate][] IRC channel at [freenode.net][] where this project can be discussed.

## GitHub Workflow

Developing patches should follow this workflow:

### Initial Setup

1.	Fork on GitHub (click Fork button)
2.	Clone to computer: `git clone git@github.com:«github account»/textmate.git`
3.	cd into your repo: `cd textmate`
4.	Set up remote upstream: `git remote add -f upstream git://github.com/textmate/textmate.git`

### Adding a Feature

1.	Create a branch for the new feature: `git checkout -b my_new_feature`
2.	Work on your feature, add and commit as usual

Creating a branch is not strictly necessary, but it makes it easy to delete your branch when the feature has been merged into upstream, diff your branch with the version that actually ended in upstream, and to submit pull requests for multiple features (branches).

### Pushing to GitHub

8.	Push branch to GitHub: `git push origin my_new_feature`
9.	Issue pull request: Click Pull Request button on GitHub

### Useful Commands

If a lot of changes has happened upstream you can replay your local changes on top of these, this is done with `rebase`, e.g.:

	git fetch upstream
	git rebase upstream/master

This will fetch changes and re-apply your commits on top of these.

This is generally better than merge, as it will give a clear picture of which commits are local to your branch. It will also “prune” any of your local commits if the same changes have been applied upstream.

You can use `-i` with `rebase` for an “interactive” rebase. This allows you to drop, re-arrange, merge, and reword commits, e.g.:

	git rebase -i upstream/master

## Changing a xib File

When you change a `xib` file then please look at the diff before you push. If the diff seems to have a lot of changes unrelated to what actually did change, please revert back to `HEAD` and open the pristine `xib` in Xcode and save that (without changing anything).

Commit this saved `xib` with a commit message of `Save xib file with Xcode «version»`. Here version is the version of Xcode you are using, but be sure you don’t downgrade the format. To check the version that `resources/English.lproj/MainMenu.xib` was last saved with, you can run (add appropriate grep if desired):

	git log --oneline resources/English.lproj/MainMenu.xib

You can safely assume that all `xib` files without such message are saved with Xcode 4.4 or earlier (i.e. you won’t downgrade them).

After this, re-apply your change and commit. If the change is non-trivial it is a good idea to write how you made the change in the commit body. E.g. a commit message could be:

	Only enable install button when we can install
	
	The install button’s “enabled” property
	has been bound to the “canInstall”
	property of File’s Owner.

# Legal

The source for TextMate is released under the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

TextMate is a trademark of Allan Odgaard.

[boost]:         http://www.boost.org/
[ninja]:         http://martine.github.com/ninja/
[multimarkdown]: http://fletcherpenney.net/multimarkdown/
[ragel]:         http://www.complang.org/ragel/
[mercurial]:     http://mercurial.selenic.com/
[clang 3.2]:     http://clang.llvm.org/
[MacPorts]:      http://www.macports.org/
[homebrew]:      http://mxcl.github.com/homebrew/
[NinjaBundle]:   https://github.com/textmate/ninja.tmbundle
[CxxTest]:       https://github.com/textmate/cxxtest.tmbundle
[GPL 3]:         http://www.gnu.org/copyleft/gpl.html
[textmate-dev]:  http://lists.macromates.com/listinfo/textmate-dev
[#textmate]:     irc://irc.freenode.net/#textmate
[freenode.net]:  http://freenode.net/
