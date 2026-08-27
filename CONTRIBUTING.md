# Contributing to TextMate

TextMate welcomes focused pull requests through GitHub. Read the build instructions in [README.md](README.md), initialize all submodules, and verify the affected targets before submitting a change.

## Development baseline

TextMate 3 development currently targets macOS 14 or later and requires a current full Xcode installation. The application combines AppKit, Objective-C++, C++, and incremental SwiftUI modules; Rave generates the Ninja build graph.

Build and run the primary checks with:

```sh
./configure
ninja TextMate
ninja bundles/test HTMLOutput/test authorization/test
swift test --package-path Applications/TextMate/SwiftUI
```

Run additional generated tests for every framework you change. Use `ninja -t targets all` to discover available targets.

## Patch expectations

1. Follow the style of the code being changed.
2. Keep each commit focused on one coherent change.
3. Start commit messages with an imperative summary shorter than 70 characters. Add a blank line and explain the reason, risk, and compatibility impact when useful.
4. Include regression tests where practical, especially for parsers, IPC, file handling, security boundaries, and data structures.
5. Preserve user data and existing bundle behavior unless the change explicitly documents a migration.
6. Do not weaken trust boundaries to preserve compatibility. Rendered HTML and bundle content must not receive unrestricted native execution privileges.
7. Keep submodule updates isolated and identify the exact upstream revision and reason.
8. Avoid unrelated generated-file, formatting, or Xcode project churn.

For a defect in a commit already published upstream, use `git commit --fixup=<commit>` when appropriate. Amend or autosquash defects in unpublished local commits before opening the pull request.

## GitHub workflow

Fork the repository, then clone your fork with submodules and add the official repository as `upstream`:

```sh
git clone --recursive git@github.com:YOUR-ACCOUNT/textmate.git
cd textmate
git remote add upstream https://github.com/textmate/textmate.git
git fetch upstream
```

Create a descriptive branch, make and verify the change, then rebase it on the current upstream branch before pushing:

```sh
git switch -c feature/short-description
git fetch upstream
git rebase upstream/master
git push --set-upstream origin feature/short-description
```

Open a pull request that states:

* The problem and user-visible result.
* The implementation approach and important trade-offs.
* Tests and manual verification performed.
* Security, compatibility, deployment-target, or migration implications.
* Any submodule revisions included.

Never include credentials, signing identities, private documents, local build products, or machine-specific configuration in a commit.

## SwiftUI and AppKit changes

TextMate is being modernized incrementally. Keep the editor core in place and use narrow interfaces when hosting SwiftUI from AppKit. New native surfaces belong in `Applications/TextMate/SwiftUI/Sources/TextMateUI/` and need focused tests under `Applications/TextMate/SwiftUI/Tests/`.

Do not restore the removed privileged JavaScript bridge. If HTML must remain for compatibility, use tightly constrained WKWebView content and explicit native actions. See [Applications/TextMate/SwiftUI/MIGRATION.md](Applications/TextMate/SwiftUI/MIGRATION.md).

## XIB changes

Review every XIB diff before committing. Xcode may rewrite unrelated document metadata or constraints. Separate a necessary format upgrade from the functional change when possible, and state the Xcode version used. Do not downgrade a document’s existing format.

## Bundle changes

Treat bundle commands as code that processes untrusted filenames and document content:

* Pass arguments as argv values instead of constructing shell source.
* Escape HTML and encode URLs before generating previews.
* Avoid placing passwords, tokens, or private content in arguments, logs, or environment values.
* Write output atomically and reject partial results on failure.
* Preserve compatibility only when it does not bypass authentication or integrity checks.

## Contribution licensing

The application does not require product registration or a license key. That is separate from source-code licensing: repository files remain under GPLv3-or-later as described in [COPYING](COPYING) and [LICENSE](LICENSE).

The historical project policy prefers patches dedicated to the public domain so the original copyright holder retains flexibility for other distributions. If you cannot make that dedication, clearly state the license under which you are submitting the patch in the pull request. Do not contribute code you do not have the right to license.

## Support

Use [GitHub Issues](https://github.com/textmate/textmate/issues) for reproducible defects and the [MacroMates support page](https://macromates.com/support) for general product support. Please follow the project’s [bug-reporting guidance](https://github.com/textmate/textmate/wiki/writing-bug-reports).
