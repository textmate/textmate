# Project Changes

Choose **View → Show Project Changes** in a project window to review the whole Git repository. Select a file in the left-hand list to see its diff. **Previous Change** and **Next Change** move between changed passages, continuing into the adjacent file at either end. **Open File**, or double-clicking a file, opens it in the original editor window at the current passage.

The baseline is the last Git commit. The current side includes staged and unstaged changes, new files, deleted files, and unsaved named documents in the owning editor window. Unsaved documents are labeled and compared using an immutable buffer snapshot. Untitled documents have no repository path and are not included. New repositories without a commit compare their files against an empty baseline.

The view refreshes every five seconds while it is visible and TextMate is active. **Refresh** updates it immediately. Repository queries and diff computation run in the background. This first version is a review view: it does not stage, save, apply, or revert changes. It shows all changes relative to the commit, regardless of whether they were made by you or an agent.

A rename currently appears as a deletion and a new file. Binary files, submodules, files over 4 MB, and text that is not UTF-8 show an explanation instead of a text diff. An unsaved buffer uses TextMate's decoded UTF-8 text; CRLF newlines are normalized for that comparison. If a file changes during a refresh, refresh again to capture its current state.

## Developer preview

Build the optional standalone preview with:

```sh
ninja -C build-debug ProjectChangesPreview
```

Open `build-debug/Frameworks/DocumentWindow/ProjectChangesPreview.app` to review this checkout with the production Changes window, without restoring or saving an editor session. The preview reads saved files; the integrated view also receives unsaved buffers from its owning editor window. Running the preview executable with a directory argument reviews a different repository.

The regression tests use temporary repositories to cover combined staged/unstaged edits, multiple diff passages, new/deleted files, unusual Unicode filenames, symbolic links, binary files, repositories without a first commit, unsaved-buffer snapshots, and opening a source location from the review window.
