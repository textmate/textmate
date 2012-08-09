;;; vc.el --- drive a version-control system from within Emacs

;; Copyright (C) 1992-1998, 2000-2012  Free Software Foundation, Inc.

;; Author:     FSF (see below for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>
;; Keywords: vc tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Credits:

;; VC was initially designed and implemented by Eric S. Raymond
;; <esr@thyrsus.com> in 1992.  Over the years, many other people have
;; contributed substantial amounts of work to VC.  These include:
;;
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Paul Eggert <eggert@twinsun.com>
;;   Sebastian Kremer <sk@thp.uni-koeln.de>
;;   Martin Lorentzson <martinl@gnu.org>
;;   Dave Love <fx@gnu.org>
;;   Stefan Monnier <monnier@cs.yale.edu>
;;   Thien-Thi Nguyen <ttn@gnu.org>
;;   Dan Nicolaescu <dann@ics.uci.edu>
;;   J.D. Smith <jdsmith@alum.mit.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;
;; In July 2007 ESR returned and redesigned the mode to cope better
;; with modern version-control systems that do commits by fileset
;; rather than per individual file.
;;
;; If you maintain a client of the mode or customize it in your .emacs,
;; note that some backend functions which formerly took single file arguments
;; now take a list of files.  These include: register, checkin, print-log,
;; rollback, and diff.

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; Supported version-control systems presently include CVS, RCS, GNU
;; Arch, Subversion, Bzr, Git, Mercurial, Monotone and SCCS
;; (or its free replacement, CSSC).
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function `log-edit-comment-to-change-log' could prove a useful checkin hook,
;; although you might prefer to use C-c C-a (i.e. `log-edit-insert-changelog')
;; from the commit buffer instead or to set `log-edit-setup-invert'.
;;
;; When using SCCS, RCS, CVS: be careful not to do repo surgery, or
;; operations like registrations and deletions and renames, outside VC
;; while VC is running. The support for these systems was designed
;; when disks were much slower, and the code maintains a lot of
;; internal state in order to reduce expensive operations to a
;; minimum. Thus, if you mess with the repo while VC's back is turned,
;; VC may get seriously confused.
;;
;; When using Subversion or a later system, anything you do outside VC
;; *through the VCS tools* should safely interlock with VC
;; operations. Under these VC does little state caching, because local
;; operations are assumed to be fast.  The dividing line is
;;
;; ADDING SUPPORT FOR OTHER BACKENDS
;;
;; VC can use arbitrary version control systems as a backend.  To add
;; support for a new backend named SYS, write a library vc-sys.el that
;; contains functions of the form `vc-sys-...' (note that SYS is in lower
;; case for the function and library names).  VC will use that library if
;; you put the symbol SYS somewhere into the list of
;; `vc-handled-backends'.  Then, for example, if `vc-sys-registered'
;; returns non-nil for a file, all SYS-specific versions of VC commands
;; will be available for that file.
;;
;; VC keeps some per-file information in the form of properties (see
;; vc-file-set/getprop in vc-hooks.el).  The backend-specific functions
;; do not generally need to be aware of these properties.  For example,
;; `vc-sys-working-revision' should compute the working revision and
;; return it; it should not look it up in the property, and it needn't
;; store it there either.  However, if a backend-specific function does
;; store a value in a property, that value takes precedence over any
;; value that the generic code might want to set (check for uses of
;; the macro `with-vc-properties' in vc.el).
;;
;; In the list of functions below, each identifier needs to be prepended
;; with `vc-sys-'.  Some of the functions are mandatory (marked with a
;; `*'), others are optional (`-').

;; BACKEND PROPERTIES
;;
;; * revision-granularity
;;
;;   Takes no arguments.  Returns either 'file or 'repository.  Backends
;;   that return 'file have per-file revision numbering; backends
;;   that return 'repository have per-repository revision numbering,
;;   so a revision level implicitly identifies a changeset

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)
;;
;;   Return non-nil if FILE is registered in this backend.  Both this
;;   function as well as `state' should be careful to fail gracefully
;;   in the event that the backend executable is absent.  It is
;;   preferable that this function's body is autoloaded, that way only
;;   calling vc-registered does not cause the backend to be loaded
;;   (all the vc-FOO-registered functions are called to try to find
;;   the controlling backend for FILE.
;;
;; * state (file)
;;
;;   Return the current version control state of FILE.  For a list of
;;   possible values, see `vc-state'.  This function should do a full and
;;   reliable state computation; it is usually called immediately after
;;   C-x v v.  If you want to use a faster heuristic when visiting a
;;   file, put that into `state-heuristic' below.  Note that under most
;;   VCSes this won't be called at all, dir-status is used instead.
;;
;; - state-heuristic (file)
;;
;;   If provided, this function is used to estimate the version control
;;   state of FILE at visiting time.  It should be considerably faster
;;   than the implementation of `state'.  For a list of possible values,
;;   see the doc string of `vc-state'.
;;
;; - dir-status (dir update-function)
;;
;;   Produce RESULT: a list of lists of the form (FILE VC-STATE EXTRA)
;;   for the files in DIR.
;;   EXTRA can be used for backend specific information about FILE.
;;   If a command needs to be run to compute this list, it should be
;;   run asynchronously using (current-buffer) as the buffer for the
;;   command.  When RESULT is computed, it should be passed back by
;;   doing: (funcall UPDATE-FUNCTION RESULT nil).
;;   If the backend uses a process filter, hence it produces partial results,
;;   they can be passed back by doing:
;;      (funcall UPDATE-FUNCTION RESULT t)
;;   and then do a (funcall UPDATE-FUNCTION RESULT nil)
;;   when all the results have been computed.
;;   To provide more backend specific functionality for `vc-dir'
;;   the following functions might be needed: `dir-extra-headers',
;;   `dir-printer', `extra-dir-menu' and `dir-status-files'.
;;
;; - dir-status-files (dir files default-state update-function)
;;
;;   This function is identical to dir-status except that it should
;;   only report status for the specified FILES. Also it needs to
;;   report on all requested files, including up-to-date or ignored
;;   files. If not provided, the default is to consider that the files
;;   are in DEFAULT-STATE.
;;
;; - dir-extra-headers (dir)
;;
;;   Return a string that will be added to the *vc-dir* buffer header.
;;
;; - dir-printer (fileinfo)
;;
;;   Pretty print the `vc-dir-fileinfo' FILEINFO.
;;   If a backend needs to show more information than the default FILE
;;   and STATE in the vc-dir listing, it can store that extra
;;   information in `vc-dir-fileinfo->extra'.  This function can be
;;   used to display that extra information in the *vc-dir* buffer.
;;
;; - status-fileinfo-extra (file)
;;
;;   Compute `vc-dir-fileinfo->extra' for FILE.
;;
;; * working-revision (file)
;;
;;   Return the working revision of FILE.  This is the revision fetched
;;   by the last checkout or update, not necessarily the same thing as the
;;   head or tip revision.  Should return "0" for a file added but not yet
;;   committed.
;;
;; - latest-on-branch-p (file)
;;
;;   Return non-nil if the working revision of FILE is the latest revision
;;   on its branch (many VCSes call this the 'tip' or 'head' revision).
;;   The default implementation always returns t, which means that
;;   working with non-current revisions is not supported by default.
;;
;; * checkout-model (files)
;;
;;   Indicate whether FILES need to be "checked out" before they can be
;;   edited.  See `vc-checkout-model' for a list of possible values.
;;
;; - workfile-unchanged-p (file)
;;
;;   Return non-nil if FILE is unchanged from the working revision.
;;   This function should do a brief comparison of FILE's contents
;;   with those of the repository copy of the working revision.  If
;;   the backend does not have such a brief-comparison feature, the
;;   default implementation of this function can be used, which
;;   delegates to a full vc-BACKEND-diff.  (Note that vc-BACKEND-diff
;;   must not run asynchronously in this case, see variable
;;   `vc-disable-async-diff'.)
;;
;; - mode-line-string (file)
;;
;;   If provided, this function should return the VC-specific mode
;;   line string for FILE.  The returned string should have a
;;   `help-echo' property which is the text to be displayed as a
;;   tooltip when the mouse hovers over the VC entry on the mode-line.
;;   The default implementation deals well with all states that
;;   `vc-state' can return.
;;
;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo (backend)
;;
;;   Create an empty repository in the current directory and initialize
;;   it so VC mode can add files to it.  For file-oriented systems, this
;;   need do no more than create a subdirectory with the right name.
;;
;; * register (files &optional rev comment)
;;
;;   Register FILES in this backend.  Optionally, an initial revision REV
;;   and an initial description of the file, COMMENT, may be specified,
;;   but it is not guaranteed that the backend will do anything with this.
;;   The implementation should pass the value of vc-register-switches
;;   to the backend command.  (Note: in older versions of VC, this
;;   command took a single file argument and not a list.)
;;
;; - init-revision (file)
;;
;;   The initial revision to use when registering FILE if one is not
;;   specified by the user.  If not provided, the variable
;;   vc-default-init-revision is used instead.
;;
;; - responsible-p (file)
;;
;;   Return non-nil if this backend considers itself "responsible" for
;;   FILE, which can also be a directory.  This function is used to find
;;   out what backend to use for registration of new files and for things
;;   like change log generation.  The default implementation always
;;   returns nil.
;;
;; - could-register (file)
;;
;;   Return non-nil if FILE could be registered under this backend.  The
;;   default implementation always returns t.
;;
;; - receive-file (file rev)
;;
;;   Let this backend "receive" a file that is already registered under
;;   another backend.  The default implementation simply calls `register'
;;   for FILE, but it can be overridden to do something more specific,
;;   e.g. keep revision numbers consistent or choose editing modes for
;;   FILE that resemble those of the other backend.
;;
;; - unregister (file)
;;
;;   Unregister FILE from this backend.  This is only needed if this
;;   backend may be used as a "more local" backend for temporary editing.
;;
;; * checkin (files rev comment)
;;
;;   Commit changes in FILES to this backend.  REV is a historical artifact
;;   and should be ignored.  COMMENT is used as a check-in comment.
;;   The implementation should pass the value of vc-checkin-switches to
;;   the backend command.
;;
;; * find-revision (file rev buffer)
;;
;;   Fetch revision REV of file FILE and put it into BUFFER.
;;   If REV is the empty string, fetch the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
;;
;; * checkout (file &optional editable rev)
;;
;;   Check out revision REV of FILE into the working area.  If EDITABLE
;;   is non-nil, FILE should be writable by the user and if locking is
;;   used for FILE, a lock should also be set.  If REV is non-nil, that
;;   is the revision to check out (default is the working revision).
;;   If REV is t, that means to check out the head of the current branch;
;;   if it is the empty string, check out the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
;;
;; * revert (file &optional contents-done)
;;
;;   Revert FILE back to the working revision.  If optional
;;   arg CONTENTS-DONE is non-nil, then the contents of FILE have
;;   already been reverted from a version backup, and this function
;;   only needs to update the status of FILE within the backend.
;;   If FILE is in the `added' state it should be returned to the
;;   `unregistered' state.
;;
;; - rollback (files)
;;
;;   Remove the tip revision of each of FILES from the repository.  If
;;   this function is not provided, trying to cancel a revision is
;;   caught as an error.  (Most backends don't provide it.)  (Also
;;   note that older versions of this backend command were called
;;   'cancel-version' and took a single file arg, not a list of
;;   files.)
;;
;; - merge (file rev1 rev2)
;;
;;   Merge the changes between REV1 and REV2 into the current working file
;;   (for non-distributed VCS).
;;
;; - merge-branch ()
;;
;;   Merge another branch into the current one, prompting for a
;;   location to merge from.
;;
;; - merge-news (file)
;;
;;   Merge recent changes from the current branch into FILE.
;;   (for non-distributed VCS).
;;
;; - pull (prompt)
;;
;;   Pull "upstream" changes into the current branch (for distributed
;;   VCS).  If PROMPT is non-nil, or if necessary, prompt for a
;;   location to pull from.
;;
;; - steal-lock (file &optional revision)
;;
;;   Steal any lock on the working revision of FILE, or on REVISION if
;;   that is provided.  This function is only needed if locking is
;;   used for files under this backend, and if files can indeed be
;;   locked by other users.
;;
;; - modify-change-comment (files rev comment)
;;
;;   Modify the change comments associated with the files at the
;;   given revision.  This is optional, many backends do not support it.
;;
;; - mark-resolved (files)
;;
;;   Mark conflicts as resolved.  Some VC systems need to run a
;;   command to mark conflicts as resolved.

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;;
;;   Insert the revision log for FILES into BUFFER.
;;   If SHORTLOG is true insert a short version of the log.
;;   If LIMIT is true insert only insert LIMIT log entries.  If the
;;   backend does not support limiting the number of entries to show
;;   it should return `limit-unsupported'.
;;   If START-REVISION is given, then show the log starting from the
;;   revision.  At this point START-REVISION is only required to work
;;   in conjunction with LIMIT = 1.
;;
;; * log-outgoing (backend remote-location)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   sent when performing a push operation to REMOTE-LOCATION.
;;
;; * log-incoming (backend remote-location)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   received when performing a pull operation from REMOTE-LOCATION.
;;
;; - log-view-mode ()
;;
;;   Mode to use for the output of print-log.  This defaults to
;;   `log-view-mode' and is expected to be changed (if at all) to a derived
;;   mode of `log-view-mode'.
;;
;; - show-log-entry (revision)
;;
;;   If provided, search the log entry for REVISION in the current buffer,
;;   and make sure it is displayed in the buffer's window.  The default
;;   implementation of this function works for RCS-style logs.
;;
;; - comment-history (file)
;;
;;   Return a string containing all log entries that were made for FILE.
;;   This is used for transferring a file from one backend to another,
;;   retaining comment information.
;;
;; - update-changelog (files)
;;
;;   Using recent log entries, create ChangeLog entries for FILES, or for
;;   all files at or below the default-directory if FILES is nil.  The
;;   default implementation runs rcs2log, which handles RCS- and
;;   CVS-style logs.
;;
;; * diff (files &optional rev1 rev2 buffer)
;;
;;   Insert the diff for FILE into BUFFER, or the *vc-diff* buffer if
;;   BUFFER is nil.  If REV1 and REV2 are non-nil, report differences
;;   from REV1 to REV2.  If REV1 is nil, use the working revision (as
;;   found in the repository) as the older revision; if REV2 is nil,
;;   use the current working-copy contents as the newer revision.  This
;;   function should pass the value of (vc-switches BACKEND 'diff) to
;;   the backend command.  It should return a status of either 0 (no
;;   differences found), or 1 (either non-empty diff or the diff is
;;   run asynchronously).
;;
;; - revision-completion-table (files)
;;
;;   Return a completion table for existing revisions of FILES.
;;   The default is to not use any completion table.
;;
;; - annotate-command (file buf &optional rev)
;;
;;   If this function is provided, it should produce an annotated display
;;   of FILE in BUF, relative to revision REV.  Annotation means each line
;;   of FILE displayed is prefixed with version information associated with
;;   its addition (deleted lines leave no history) and that the text of the
;;   file is fontified according to age.
;;
;; - annotate-time ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Return the time of the next line of annotation at or after point,
;;   as a floating point fractional number of days.  The helper
;;   function `vc-annotate-convert-time' may be useful for converting
;;   multi-part times as returned by `current-time' and `encode-time'
;;   to this format.  Return nil if no more lines of annotation appear
;;   in the buffer.  You can safely assume that point is placed at the
;;   beginning of each line, starting at `point-min'.  The buffer that
;;   point is placed in is the Annotate output, as defined by the
;;   relevant backend.  This function also affects how much of the line
;;   is fontified; where it leaves point is where fontification begins.
;;
;; - annotate-current-time ()
;;
;;   Only required if `annotate-command' is defined for the backend,
;;   AND you'd like the current time considered to be anything besides
;;   (vc-annotate-convert-time (current-time)) -- i.e. the current
;;   time with hours, minutes, and seconds included.  Probably safe to
;;   ignore.  Return the current-time, in units of fractional days.
;;
;; - annotate-extract-revision-at-line ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Invoked from a buffer in vc-annotate-mode, return the revision
;;   corresponding to the current line, or nil if there is no revision
;;   corresponding to the current line.
;;   If the backend supports annotating through copies and renames,
;;   and displays a file name and a revision, then return a cons
;;   (REVISION . FILENAME).

;; TAG SYSTEM
;;
;; - create-tag (dir name branchp)
;;
;;   Attach the tag NAME to the state of the working copy.  This
;;   should make sure that files are up-to-date before proceeding with
;;   the action.  DIR can also be a file and if BRANCHP is specified,
;;   NAME should be created as a branch and DIR should be checked out
;;   under this new branch.  The default implementation does not
;;   support branches but does a sanity check, a tree traversal and
;;   assigns the tag to each file.
;;
;; - retrieve-tag (dir name update)
;;
;;   Retrieve the version tagged by NAME of all registered files at or below DIR.
;;   If UPDATE is non-nil, then update buffers of any files in the
;;   tag that are currently visited.  The default implementation
;;   does a sanity check whether there aren't any uncommitted changes at
;;   or below DIR, and then performs a tree walk, using the `checkout'
;;   function to retrieve the corresponding revisions.

;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;;
;;   Return non-nil if unmodified repository revisions of FILE should be
;;   backed up locally.  If this is done, VC can perform `diff' and
;;   `revert' operations itself, without calling the backend system.  The
;;   default implementation always returns nil.
;;
;; - root (file)
;;   Return the root of the VC controlled hierarchy for file.
;;
;; - repository-hostname (dirname)
;;
;;   Return the hostname that the backend will have to contact
;;   in order to operate on a file in DIRNAME.  If the return value
;;   is nil, it means that the repository is local.
;;   This function is used in `vc-stay-local-p' which backends can use
;;   for their convenience.
;;
;; - previous-revision (file rev)
;;
;;   Return the revision number that precedes REV for FILE, or nil if no such
;;   revision exists.
;;
;; - next-revision (file rev)
;;
;;   Return the revision number that follows REV for FILE, or nil if no such
;;   revision exists.
;;
;; - log-edit-mode ()
;;
;;   Turn on the mode used for editing the check in log.  This
;;   defaults to `log-edit-mode'.  If changed, it should use a mode
;;   derived from`log-edit-mode'.
;;
;; - check-headers ()
;;
;;   Return non-nil if the current buffer contains any version headers.
;;
;; - clear-headers ()
;;
;;   In the current buffer, reset all version headers to their unexpanded
;;   form.  This function should be provided if the state-querying code
;;   for this backend uses the version headers to determine the state of
;;   a file.  This function will then be called whenever VC changes the
;;   version control state in such a way that the headers would give
;;   wrong information.
;;
;; - delete-file (file)
;;
;;   Delete FILE and mark it as deleted in the repository.  If this
;;   function is not provided, the command `vc-delete-file' will
;;   signal an error.
;;
;; - rename-file (old new)
;;
;;   Rename file OLD to NEW, both in the working area and in the
;;   repository.  If this function is not provided, the renaming
;;   will be done by (vc-delete-file old) and (vc-register new).
;;
;; - find-file-hook ()
;;
;;   Operation called in current buffer when opening a file.  This can
;;   be used by the backend to setup some local variables it might need.
;;
;; - extra-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the Version Control menu.  The goal is to allow backends
;;   to specify extra menu items that appear in the VC menu.  This way
;;   you can provide menu entries for functionality that is specific
;;   to your backend and which does not map to any of the VC generic
;;   concepts.
;;
;; - extra-dir-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the VC Status menu.  The goal is to allow backends to
;;   specify extra menu items that appear in the VC Status menu.  This
;;   makes it possible to provide menu entries for functionality that
;;   is specific to a backend and which does not map to any of the VC
;;   generic concepts.
;;
;; - conflicted-files (dir)
;;
;;   Return the list of files where conflict resolution is needed in
;;   the project that contains DIR.
;;   FIXME: what should it do with non-text conflicts?

;;; Todo:

;; - Get rid of the "master file" terminology.

;; - Add key-binding for vc-delete-file.

;;;; New Primitives:
;;
;; - deal with push/pull operations.
;;
;; - add a mechanism for editing the underlying VCS's list of files
;;   to be ignored, when that's possible.
;;
;;;; Primitives that need changing:
;;
;; - vc-update/vc-merge should deal with VC systems that don't
;;   update/merge on a file basis, but on a whole repository basis.
;;   vc-update and vc-merge assume the arguments are always files,
;;   they don't deal with directories.  Make sure the *vc-dir* buffer
;;   is updated after these operations.
;;   At least bzr, git and hg should benefit from this.
;;
;;;; Improved branch and tag handling:
;;
;; - add a generic mechanism for remembering the current branch names,
;;   display the branch name in the mode-line. Replace
;;   vc-cvs-sticky-tag with that.
;;
;;;; Internal cleanups:
;;
;; - backends that care about vc-stay-local should try to take it into
;;   account for vc-dir.  Is this likely to be useful???  YES!
;;
;; - vc-expand-dirs should take a backend parameter and only look for
;;   files managed by that backend.
;;
;; - Another important thing: merge all the status-like backend operations.
;;   We should remove dir-status, state, and dir-status-files, and
;;   replace them with just `status' which takes a fileset and a continuation
;;   (like dir-status) and returns a buffer in which the process(es) are run
;;   (or nil if it worked synchronously).  Hopefully we can define the old
;;   4 operations in term of this one.
;;
;;;; Other
;;
;; - when a file is in `conflict' state, turn on smerge-mode.
;;
;; - figure out what to do with conflicts that are not caused by the
;;   file contents, but by metadata or other causes.  Example: File A
;;   gets renamed to B in one branch and to C in another and you merge
;;   the two branches.  Or you locally add file FOO and then pull a
;;   change that also adds a new file FOO, ...
;;
;; - make it easier to write logs.  Maybe C-x 4 a should add to the log
;;   buffer, if one is present, instead of adding to the ChangeLog.
;;
;; - When vc-next-action calls vc-checkin it could pre-fill the
;;   *vc-log* buffer with some obvious items: the list of files that
;;   were added, the list of files that were removed.  If the diff is
;;   available, maybe it could even call something like
;;   `diff-add-change-log-entries-other-window' to create a detailed
;;   skeleton for the log...
;;
;; - most vc-dir backends need more work.  They might need to
;;   provide custom headers, use the `extra' field and deal with all
;;   possible VC states.
;;
;; - add a function that calls vc-dir to `find-directory-functions'.
;;
;; - vc-diff, vc-annotate, etc. need to deal better with unregistered
;;   files. Now that unregistered and ignored files are shown in
;;   vc-dir, it is possible that these commands are called
;;   for unregistered/ignored files.
;;
;; - vc-next-action needs work in order to work with multiple
;;   backends: `vc-state' returns the state for the default backend,
;;   not for the backend in the current *vc-dir* buffer.
;;
;; - vc-dir-kill-dir-status-process should not be specific to dir-status,
;;   it should work for other async commands done through vc-do-command
;;   as well,
;;
;; - vc-dir toolbar needs more icons.
;;
;; - The backends should avoid using `vc-file-setprop' and `vc-file-getprop'.
;;
;;; Code:

(require 'vc-hooks)
(require 'vc-dispatcher)
(require 'ediff)

(eval-when-compile
  (require 'cl)
  (require 'dired))

(unless (assoc 'vc-parent-buffer minor-mode-alist)
  (setq minor-mode-alist
	(cons '(vc-parent-buffer vc-parent-buffer-name)
	      minor-mode-alist)))

;; General customization

(defgroup vc nil
  "Version-control system in Emacs."
  :group 'tools)

(defcustom vc-initial-comment nil
  "If non-nil, prompt for initial comment when a file is registered."
  :type 'boolean
  :group 'vc)

(make-obsolete-variable 'vc-initial-comment "it has no effect." "23.2")

(defcustom vc-default-init-revision "1.1"
  "A string used as the default revision number when a new file is registered.
This can be overridden by giving a prefix argument to \\[vc-register].  This
can also be overridden by a particular VC backend."
  :type 'string
  :group 'vc
  :version "20.3")

(defcustom vc-checkin-switches nil
  "A string or list of strings specifying extra switches for checkin.
These are passed to the checkin program by \\[vc-checkin]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-checkout-switches nil
  "A string or list of strings specifying extra switches for checkout.
These are passed to the checkout program by \\[vc-checkout]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-register-switches nil
  "A string or list of strings; extra switches for registering a file.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc)

(defcustom vc-diff-switches nil
  "A string or list of strings specifying switches for diff under VC.
When running diff under a given BACKEND, VC uses the first
non-nil value of `vc-BACKEND-diff-switches', `vc-diff-switches',
and `diff-switches', in that order.  Since nil means to check the
next variable in the sequence, either of the first two may use
the value t to mean no switches at all.  `vc-diff-switches'
should contain switches that are specific to version control, but
not specific to any particular backend."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :group 'vc
  :version "21.1")

(defcustom vc-diff-knows-L nil
  "Indicates whether diff understands the -L option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use -L and sets this variable to remember whether it worked."
  :type '(choice (const :tag "Work out" nil) (const yes) (const no))
  :group 'vc)

(defcustom vc-log-show-limit 2000
  "Limit the number of items shown by the VC log commands.
Zero means unlimited.
Not all VC backends are able to support this feature."
  :type 'integer
  :group 'vc)

(defcustom vc-allow-async-revert nil
  "Specifies whether the diff during \\[vc-revert] may be asynchronous.
Enabling this option means that you can confirm a revert operation even
if the local changes in the file have not been found and displayed yet."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'vc
  :version "22.1")

;;;###autoload
(defcustom vc-checkout-hook nil
  "Normal hook (list of functions) run after checking out a file.
See `run-hooks'."
  :type 'hook
  :group 'vc
  :version "21.1")

;;;###autoload
(defcustom vc-checkin-hook nil
  "Normal hook (list of functions) run after commit or file checkin.
See also `log-edit-done-hook'."
  :type 'hook
  :options '(log-edit-comment-to-change-log)
  :group 'vc)

;;;###autoload
(defcustom vc-before-checkin-hook nil
  "Normal hook (list of functions) run before a commit or a file checkin.
See `run-hooks'."
  :type 'hook
  :group 'vc)

(defcustom vc-revert-show-diff t
  "If non-nil, `vc-revert' shows a `vc-diff' buffer before querying."
  :type 'boolean
  :group 'vc
  :version "24.1")

;; Header-insertion hair

(defcustom vc-static-header-alist
  '(("\\.c\\'" .
     "\n#ifndef lint\nstatic char vcid[] = \"\%s\";\n#endif /* lint */\n"))
  "Associate static header string templates with file types.
A \%s in the template is replaced with the first string associated with
the file's version control type in `vc-BACKEND-header'."
  :type '(repeat (cons :format "%v"
		       (regexp :tag "File Type")
		       (string :tag "Header String")))
  :group 'vc)

(defcustom vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "Special comment delimiters for generating VC headers.
Add an entry in this list if you need to override the normal `comment-start'
and `comment-end' variables.  This will only be necessary if the mode language
is sensitive to blank lines."
  :type '(repeat (list :format "%v"
		       (symbol :tag "Mode")
		       (string :tag "Comment Start")
		       (string :tag "Comment End")))
  :group 'vc)

(defcustom vc-checkout-carefully (= (user-uid) 0)
  "Non-nil means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the repository version says."
  :type 'boolean
  :group 'vc)
(make-obsolete-variable 'vc-checkout-carefully
                        "the corresponding checks are always done now."
                        "21.1")


;; Variables users don't need to see

(defvar vc-disable-async-diff nil
  "VC sets this to t locally to disable some async diff operations.
Backends that offer asynchronous diffs should respect this variable
in their implementation of vc-BACKEND-diff.")

;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties."
  (interactive)
  (fillarray vc-file-prop-obarray 0))

(defmacro with-vc-properties (files form settings)
  "Execute FORM, then maybe set per-file properties for FILES.
If any of FILES is actually a directory, then do the same for all
buffers for files in that directory.
SETTINGS is an association list of property/value pairs.  After
executing FORM, set those properties from SETTINGS that have not yet
been updated to their corresponding values."
  (declare (debug t))
  `(let ((vc-touched-properties (list t))
	 (flist nil))
     (dolist (file ,files)
       (if (file-directory-p file)
	   (dolist (buffer (buffer-list))
	     (let ((fname (buffer-file-name buffer)))
	       (when (and fname (vc-string-prefix-p file fname))
		 (push fname flist))))
	 (push file flist)))
     ,form
     (dolist (file flist)
       (dolist (setting ,settings)
         (let ((property (car setting)))
           (unless (memq property vc-touched-properties)
             (put (intern file vc-file-prop-obarray)
                  property (cdr setting))))))))

;;; Code for deducing what fileset and backend to assume

(defun vc-backend-for-registration (file)
  "Return a backend that can be used for registering FILE.

If no backend declares itself responsible for FILE, then FILE
must not be in a version controlled directory, so try to create a
repository, prompting for the directory and the VC backend to
use."
  (catch 'found
    ;; First try: find a responsible backend, it must be a backend
    ;; under which FILE is not yet registered.
    (dolist (backend vc-handled-backends)
      (and (not (vc-call-backend backend 'registered file))
	   (vc-call-backend backend 'responsible-p file)
	   (throw 'found backend)))
    ;; no responsible backend
    (let* ((possible-backends
	    (let (pos)
	      (dolist (crt vc-handled-backends)
		(when (vc-find-backend-function crt 'create-repo)
		  (push crt pos)))
	      pos))
	   (bk
	    (intern
	     ;; Read the VC backend from the user, only
	     ;; complete with the backends that have the
	     ;; 'create-repo method.
	     (completing-read
	      (format "%s is not in a version controlled directory.\nUse VC backend: " file)
	      (mapcar 'symbol-name possible-backends) nil t)))
	   (repo-dir
	    (let ((def-dir (file-name-directory file)))
	      ;; read the directory where to create the
	      ;; repository, make sure it's a parent of
	      ;; file.
	      (read-file-name
	       (format "create %s repository in: " bk)
	       default-directory def-dir t nil
	       (lambda (arg)
		 (message "arg %s" arg)
		 (and (file-directory-p arg)
		      (vc-string-prefix-p (expand-file-name arg) def-dir)))))))
	   (let ((default-directory repo-dir))
	(vc-call-backend bk 'create-repo))
      (throw 'found bk))))

(defun vc-responsible-backend (file)
  "Return the name of a backend system that is responsible for FILE.

If FILE is already registered, return the
backend of FILE.  If FILE is not registered, then the
first backend in `vc-handled-backends' that declares itself
responsible for FILE is returned."
  (or (and (not (file-directory-p file)) (vc-backend file))
      (catch 'found
	;; First try: find a responsible backend.  If this is for registration,
	;; it must be a backend under which FILE is not yet registered.
	(dolist (backend vc-handled-backends)
	  (and (vc-call-backend backend 'responsible-p file)
	       (throw 'found backend))))
      (error "No VC backend is responsible for %s" file)))

(defun vc-expand-dirs (file-or-dir-list)
  "Expands directories in a file list specification.
Within directories, only files already under version control are noticed."
  (let ((flattened '()))
    (dolist (node file-or-dir-list)
      (when (file-directory-p node)
	(vc-file-tree-walk
	 node (lambda (f) (when (vc-backend f) (push f flattened)))))
      (unless (file-directory-p node) (push node flattened)))
    (nreverse flattened)))

(defvar vc-dir-backend)
(defvar log-view-vc-backend)
(defvar diff-vc-backend)

(defun vc-deduce-backend ()
  (cond ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
	((derived-mode-p 'log-view-mode) log-view-vc-backend)
	((derived-mode-p 'diff-mode)     diff-vc-backend)
        ;; Maybe we could even use comint-mode rather than shell-mode?
	((derived-mode-p 'dired-mode 'shell-mode 'compilation-mode)
	 (vc-responsible-backend default-directory))
	(vc-mode (vc-backend buffer-file-name))))

(declare-function vc-dir-current-file "vc-dir" ())
(declare-function vc-dir-deduce-fileset "vc-dir" (&optional state-model-only-files))

(defun vc-deduce-fileset (&optional observer allow-unregistered
				    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.
Return (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL).

If we're in VC-dir mode, FILESET is the list of marked files.
Otherwise, if in a buffer visiting a version-controlled file,
FILESET is a single-file fileset containing that file.
Otherwise, if ALLOW-UNREGISTERED is non-nil and the visited file
is unregistered, FILESET is a single-file fileset containing it.
Otherwise, throw an error.

STATE-MODEL-ONLY-FILES if non-nil, means that the caller needs
the FILESET-ONLY-FILES STATE and MODEL info.  Otherwise, that
part may be skipped.
BEWARE: this function may change the
current buffer."
  ;; FIXME: OBSERVER is unused.  The name is not intuitive and is not
  ;; documented.  It's set to t when called from diff and print-log.
  (let (backend)
    (cond
     ((derived-mode-p 'vc-dir-mode)
      (vc-dir-deduce-fileset state-model-only-files))
     ((derived-mode-p 'dired-mode)
      (if observer
	  (vc-dired-deduce-fileset)
	(error "State changing VC operations not supported in `dired-mode'")))
     ((setq backend (vc-backend buffer-file-name))
      (if state-model-only-files
	(list backend (list buffer-file-name)
	      (list buffer-file-name)
	      (vc-state buffer-file-name)
	      (vc-checkout-model backend buffer-file-name))
	(list backend (list buffer-file-name))))
     ((and (buffer-live-p vc-parent-buffer)
           ;; FIXME: Why this test?  --Stef
           (or (buffer-file-name vc-parent-buffer)
				(with-current-buffer vc-parent-buffer
				  (derived-mode-p 'vc-dir-mode))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
	(set-buffer vc-parent-buffer)
	(vc-deduce-fileset observer allow-unregistered state-model-only-files)))
     ((not buffer-file-name)
       (error "Buffer %s is not associated with a file" (buffer-name)))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
	  (list (vc-backend-for-registration (buffer-file-name))
		(list buffer-file-name)
		(list buffer-file-name)
		(when state-model-only-files 'unregistered)
		nil)
	(list (vc-backend-for-registration (buffer-file-name))
	      (list buffer-file-name))))
     (t (error "No fileset is available here")))))

(defun vc-dired-deduce-fileset ()
  (let ((backend (vc-responsible-backend default-directory)))
    (unless backend (error "Directory not under VC"))
    (list backend
          (dired-map-over-marks (dired-get-filename nil t) nil))))

(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (cond
   ((derived-mode-p 'vc-dir-mode)
    (set-buffer (find-file-noselect (vc-dir-current-file))))
   (t
    (while (and vc-parent-buffer
                (buffer-live-p vc-parent-buffer)
		;; Avoid infinite looping when vc-parent-buffer and
		;; current buffer are the same buffer.
 		(not (eq vc-parent-buffer (current-buffer))))
      (set-buffer vc-parent-buffer))
    (if (not buffer-file-name)
	(error "Buffer %s is not associated with a file" (buffer-name))
      (unless (vc-backend buffer-file-name)
	(error "File %s is not under version control" buffer-file-name))))))

;;; Support for the C-x v v command.
;; This is where all the single-file-oriented code from before the fileset
;; rewrite lives.

(defsubst vc-editable-p (file)
  "Return non-nil if FILE can be edited."
  (let ((backend (vc-backend file)))
    (and backend
         (or (eq (vc-checkout-model backend (list file)) 'implicit)
             (memq (vc-state file) '(edited needs-merge conflict))))))

(defun vc-compatible-state (p q)
  "Controls which states can be in the same commit."
  (or
   (eq p q)
   (and (member p '(edited added removed)) (member q '(edited added removed)))))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical version control operation on the current fileset.
This requires that all files in the current VC fileset be in the
same state.  If not, signal an error.

For merging-based version control systems:
  If every file in the VC fileset is not registered for version
   control, register the fileset (but don't commit).
  If every work file in the VC fileset is added or changed, pop
   up a *vc-log* buffer to commit the fileset.
  For a centralized version control system, if any work file in
   the VC fileset is out of date, offer to update the fileset.

For old-style locking-based version control systems, like RCS:
  If every file is not registered, register the file(s).
  If every file is registered and unlocked, check out (lock)
   the file(s) for editing.
  If every file is locked by you and has changes, pop up a
   *vc-log* buffer to check in the changes.  If the variable
   `vc-keep-workfiles' is non-nil (the default), leave a
   read-only copy of each changed file after checking in.
  If every file is locked by you and unchanged, unlock them.
  If every file is locked by someone else, offer to steal the lock."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (backend (car vc-fileset))
	 (files (nth 1 vc-fileset))
         (fileset-only-files (nth 2 vc-fileset))
         ;; FIXME: We used to call `vc-recompute-state' here.
         (state (nth 3 vc-fileset))
         ;; The backend should check that the checkout-model is consistent
         ;; among all the `files'.
	 (model (nth 4 vc-fileset)))

    ;; Do the right thing
    (cond
     ((eq state 'missing)
      (error "Fileset files are missing, so cannot be operated on"))
     ((eq state 'ignored)
      (error "Fileset files are ignored by the version-control system"))
     ((or (null state) (eq state 'unregistered))
      (vc-register nil vc-fileset))
     ;; Files are up-to-date, or need a merge and user specified a revision
     ((or (eq state 'up-to-date) (and verbose (eq state 'needs-update)))
      (cond
       (verbose
	;; go to a different revision
	(let* ((revision
                (read-string "Branch, revision, or backend to move to: "))
               (revision-downcase (downcase revision)))
	  (if (member
	       revision-downcase
	       (mapcar (lambda (arg) (downcase (symbol-name arg)))
                       vc-handled-backends))
	      (let ((vsym (intern-soft revision-downcase)))
		(dolist (file files) (vc-transfer-file file vsym)))
	    (dolist (file files)
              (vc-checkout file (eq model 'implicit) revision)))))
       ((not (eq model 'implicit))
	;; check the files out
	(dolist (file files) (vc-checkout file t)))
       (t
        ;; do nothing
        (message "Fileset is up-to-date"))))
     ;; Files have local changes
     ((vc-compatible-state state 'edited)
      (let ((ready-for-commit files))
	;; If files are edited but read-only, give user a chance to correct.
	(dolist (file files)
	  ;; If committing a mix of removed and edited files, the
	  ;; fileset has state = 'edited.  Rather than checking the
	  ;; state of each individual file in the fileset, it seems
	  ;; simplest to just check if the file exists.	 Bug#9781.
	  (when (and (file-exists-p file) (not (file-writable-p file)))
	    ;; Make the file+buffer read-write.
	    (unless (y-or-n-p (format "%s is edited but read-only; make it writable and continue? " file))
	      (error "Aborted"))
            ;; Maybe we somehow lost permissions on the directory.
            (condition-case nil
                (set-file-modes file (logior (file-modes file) 128))
              (error (error "Unable to make file writable")))
	    (let ((visited (get-file-buffer file)))
	      (when visited
		(with-current-buffer visited
		  (toggle-read-only -1))))))
	;; Allow user to revert files with no changes
	(save-excursion
          (dolist (file files)
            (let ((visited (get-file-buffer file)))
              ;; For files with locking, if the file does not contain
              ;; any changes, just let go of the lock, i.e. revert.
              (when (and (not (eq model 'implicit))
			 (vc-workfile-unchanged-p file)
			 ;; If buffer is modified, that means the user just
			 ;; said no to saving it; in that case, don't revert,
			 ;; because the user might intend to save after
			 ;; finishing the log entry and committing.
			 (not (and visited (buffer-modified-p))))
		(vc-revert-file file)
		(setq ready-for-commit (delete file ready-for-commit))))))
	;; Remaining files need to be committed
	(if (not ready-for-commit)
	    (message "No files remain to be committed")
	  (if (not verbose)
	      (vc-checkin ready-for-commit backend)
	    (let* ((revision (read-string "New revision or backend: "))
                   (revision-downcase (downcase revision)))
	      (if (member
		   revision-downcase
		   (mapcar (lambda (arg) (downcase (symbol-name arg)))
			   vc-handled-backends))
		  (let ((vsym (intern revision-downcase)))
		    (dolist (file files) (vc-transfer-file file vsym)))
		(vc-checkin ready-for-commit backend revision)))))))
     ;; locked by somebody else (locking VCSes only)
     ((stringp state)
      ;; In the old days, we computed the revision once and used it on
      ;; the single file.  Then, for the 2007-2008 fileset rewrite, we
      ;; computed the revision once (incorrectly, using a free var) and
      ;; used it on all files.  To fix the free var bug, we can either
      ;; use `(car files)' or do what we do here: distribute the
      ;; revision computation among `files'.  Although this may be
      ;; tedious for those backends where a "revision" is a trans-file
      ;; concept, it is nonetheless correct for both those and (more
      ;; importantly) for those where "revision" is a per-file concept.
      ;; If the intersection of the former group and "locking VCSes" is
      ;; non-empty [I vaguely doubt it --ttn], we can reinstate the
      ;; pre-computation approach of yore.
      (dolist (file files)
        (vc-steal-lock
         file (if verbose
                  (read-string (format "%s revision to steal: " file))
                (vc-working-revision file))
         state)))
     ;; conflict
     ((eq state 'conflict)
      ;; FIXME: Is it really the UI we want to provide?
      ;; In my experience, the conflicted files should be marked as resolved
      ;; one-by-one when saving the file after resolving the conflicts.
      ;; I.e. stating explicitly that the conflicts are resolved is done
      ;; very rarely.
      (vc-mark-resolved backend files))
     ;; needs-update
     ((eq state 'needs-update)
      (dolist (file files)
	(if (yes-or-no-p (format
			  "%s is not up-to-date.  Get latest revision? "
			  (file-name-nondirectory file)))
	    (vc-checkout file (eq model 'implicit) t)
	  (when (and (not (eq model 'implicit))
		     (yes-or-no-p "Lock this revision? "))
	    (vc-checkout file t)))))
     ;; needs-merge
     ((eq state 'needs-merge)
      (dolist (file files)
	(when (yes-or-no-p (format
			  "%s is not up-to-date.  Merge in changes now? "
			  (file-name-nondirectory file)))
	  (vc-maybe-resolve-conflicts
           file (vc-call-backend backend 'merge-news file)))))

     ;; unlocked-changes
     ((eq state 'unlocked-changes)
      (dolist (file files)
	(when (not (equal buffer-file-name file))
	  (find-file-other-window file))
	(if (save-window-excursion
	      (vc-diff-internal nil
				(cons (car vc-fileset) (cons (cadr vc-fileset) (list file)))
				(vc-working-revision file) nil)
	      (goto-char (point-min))
	      (let ((inhibit-read-only t))
		(insert
		 (format "Changes to %s since last lock:\n\n" file)))
	      (not (beep))
	      (yes-or-no-p (concat "File has unlocked changes.  "
				   "Claim lock retaining changes? ")))
	    (progn (vc-call-backend backend 'steal-lock file)
		   (clear-visited-file-modtime)
		   ;; Must clear any headers here because they wouldn't
		   ;; show that the file is locked now.
		   (vc-clear-headers file)
		   (write-file buffer-file-name)
		   (vc-mode-line file backend))
	  (if (not (yes-or-no-p
		    "Revert to checked-in revision, instead? "))
	      (error "Checkout aborted")
	    (vc-revert-buffer-internal t t)
	    (vc-checkout file t)))))
     ;; Unknown fileset state
     (t
      (error "Fileset is in an unknown state %s" state)))))

(defun vc-create-repo (backend)
  "Create an empty repository in the current directory."
  (interactive
   (list
    (intern
     (upcase
      (completing-read
       "Create repository for: "
       (mapcar (lambda (b) (list (downcase (symbol-name b)))) vc-handled-backends)
       nil t)))))
  (vc-call-backend backend 'create-repo))

(declare-function vc-dir-move-to-goal-column "vc-dir" ())

;;;###autoload
(defun vc-register (&optional set-revision vc-fileset comment)
  "Register into a version control system.
If VC-FILESET is given, register the files in that fileset.
Otherwise register the current file.
With prefix argument SET-REVISION, allow user to specify initial revision
level.  If COMMENT is present, use that as an initial comment.

The version control system to use is found by cycling through the list
`vc-handled-backends'.  The first backend in that list which declares
itself responsible for the file (usually because other files in that
directory are already registered under that backend) will be used to
register the file.  If no backend declares itself responsible, the
first backend that could register the file is used."
  (interactive "P")
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
	 (files (nth 1 fileset-arg)))
    ;; We used to operate on `only-files', but VC wants to provide the
    ;; possibility to register directories rather than files only, since
    ;; many VCS allow that as well.
    (dolist (fname files)
      (let ((bname (get-file-buffer fname)))
	(unless fname (setq fname buffer-file-name))
	(when (vc-backend fname)
	  (if (vc-registered fname)
	      (error "This file is already registered")
	    (unless (y-or-n-p "Previous master file has vanished.  Make a new one? ")
	      (error "Aborted"))))
	;; Watch out for new buffers of size 0: the corresponding file
	;; does not exist yet, even though buffer-modified-p is nil.
	(when bname
	  (with-current-buffer bname
	    (when (and (not (buffer-modified-p))
		       (zerop (buffer-size))
		       (not (file-exists-p buffer-file-name)))
	      (set-buffer-modified-p t))
	    (vc-buffer-sync)))))
    (message "Registering %s... " files)
    (mapc 'vc-file-clearprops files)
    (vc-call-backend backend 'register files
		     (if set-revision
			 (read-string (format "Initial revision level for %s: " files))
		       (vc-call-backend backend 'init-revision))
		     comment)
    (mapc
     (lambda (file)
       (vc-file-setprop file 'vc-backend backend)
       ;; FIXME: This is wrong: it should set `backup-inhibited' in all
       ;; the buffers visiting files affected by this `vc-register', not
       ;; in the current-buffer.
       ;; (unless vc-make-backup-files
       ;;   (make-local-variable 'backup-inhibited)
       ;;   (setq backup-inhibited t))

       (vc-resynch-buffer file vc-keep-workfiles t))
     files)
    (when (derived-mode-p 'vc-dir-mode)
      (vc-dir-move-to-goal-column))
    (message "Registering %s... done" files)))

(defun vc-register-with (backend)
  "Register the current file with a specified back end."
  (interactive "SBackend: ")
  (when (not (member backend vc-handled-backends))
    (error "Unknown back end"))
  (let ((vc-handled-backends (list backend)))
    (call-interactively 'vc-register)))

(defun vc-checkout (file &optional writable rev)
  "Retrieve a copy of the revision REV of FILE.
If WRITABLE is non-nil, make sure the retrieved file is writable.
REV defaults to the latest revision.

After check-out, runs the normal hook `vc-checkout-hook'."
  (and writable
       (not rev)
       (vc-call make-version-backups-p file)
       (vc-up-to-date-p file)
       (vc-make-version-backup file))
  (let ((backend (vc-backend file)))
    (with-vc-properties (list file)
      (condition-case err
          (vc-call-backend backend 'checkout file writable rev)
        (file-error
         ;; Maybe the backend is not installed ;-(
         (when writable
           (let ((buf (get-file-buffer file)))
             (when buf (with-current-buffer buf (toggle-read-only -1)))))
         (signal (car err) (cdr err))))
      `((vc-state . ,(if (or (eq (vc-checkout-model backend (list file)) 'implicit)
                             (not writable))
                         (if (vc-call-backend backend 'latest-on-branch-p file)
                             'up-to-date
                           'needs-update)
                       'edited))
        (vc-checkout-time . ,(nth 5 (file-attributes file))))))
  (vc-resynch-buffer file t t)
  (run-hooks 'vc-checkout-hook))

(defun vc-mark-resolved (backend files)
  (prog1 (with-vc-properties
	  files
	  (vc-call-backend backend 'mark-resolved files)
	  ;; FIXME: Is this TRTD?  Might not be.
	  `((vc-state . edited)))
    (message
     (substitute-command-keys
      "Conflicts have been resolved in %s.  \
Type \\[vc-next-action] to check in changes.")
     (if (> (length files) 1)
	 (format "%d files" (length files))
       "this file"))))

(defun vc-steal-lock (file rev owner)
  "Steal the lock on FILE."
  (let (file-description)
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (when (not (yes-or-no-p (format "Steal the lock on %s from %s? "
				    file-description owner)))
      (error "Steal canceled"))
    (message "Stealing lock on %s..." file)
    (with-vc-properties
     (list file)
     (vc-call steal-lock file rev)
     `((vc-state . edited)))
    (vc-resynch-buffer file t t)
    (message "Stealing lock on %s...done" file)
    ;; Write mail after actually stealing, because if the stealing
    ;; goes wrong, we don't want to send any mail.
    (compose-mail owner (format "Stolen lock on %s" file-description))
    (setq default-directory (expand-file-name "~/"))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

(defun vc-checkin (files backend &optional rev comment initial-contents)
  "Check in FILES.
The optional argument REV may be a string specifying the new revision
level (strongly deprecated).  COMMENT is a comment
string; if omitted, a buffer is popped up to accept a comment.  If
INITIAL-CONTENTS is non-nil, then COMMENT is used as the initial contents
of the log entry buffer.

If `vc-keep-workfiles' is nil, FILE is deleted afterwards, provided
that the version control system supports this mode of operation.

Runs the normal hooks `vc-before-checkin-hook' and `vc-checkin-hook'."
  (when vc-before-checkin-hook
    (run-hooks 'vc-before-checkin-hook))
  (lexical-let
   ((backend backend))
   (vc-start-logentry
    files comment initial-contents
    "Enter a change comment."
    "*vc-log*"
    (lambda ()
      (vc-call-backend backend 'log-edit-mode))
    (lexical-let ((rev rev))
      (lambda (files comment)
        (message "Checking in %s..." (vc-delistify files))
        ;; "This log message intentionally left almost blank".
        ;; RCS 5.7 gripes about white-space-only comments too.
        (or (and comment (string-match "[^\t\n ]" comment))
            (setq comment "*** empty log message ***"))
        (with-vc-properties
            files
          ;; We used to change buffers to get local value of
          ;; vc-checkin-switches, but 'the' local buffer is
          ;; not a well-defined concept for filesets.
          (progn
            (vc-call-backend backend 'checkin files rev comment)
            (mapc 'vc-delete-automatic-version-backups files))
          `((vc-state . up-to-date)
            (vc-checkout-time . ,(nth 5 (file-attributes file)))
            (vc-working-revision . nil)))
        (message "Checking in %s...done" (vc-delistify files))))
    'vc-checkin-hook)))

;;; Additional entry points for examining version histories

;; (defun vc-default-diff-tree (backend dir rev1 rev2)
;;   "List differences for all registered files at and below DIR.
;; The meaning of REV1 and REV2 is the same as for `vc-revision-diff'."
;;   ;; This implementation does an explicit tree walk, and calls
;;   ;; vc-BACKEND-diff directly for each file.  An optimization
;;   ;; would be to use `vc-diff-internal', so that diffs can be local,
;;   ;; and to call it only for files that are actually changed.
;;   ;; However, this is expensive for some backends, and so it is left
;;   ;; to backend-specific implementations.
;;   (setq default-directory dir)
;;   (vc-file-tree-walk
;;    default-directory
;;    (lambda (f)
;;      (vc-exec-after
;;       `(let ((coding-system-for-read (vc-coding-system-for-diff ',f)))
;;          (message "Looking at %s" ',f)
;;          (vc-call-backend ',(vc-backend f)
;;                           'diff (list ',f) ',rev1 ',rev2))))))

(defvar vc-coding-system-inherit-eol t
  "When non-nil, inherit the EOL format for reading Diff output from the file.

Used in `vc-coding-system-for-diff' to determine the EOL format to use
for reading Diff output for a file.  If non-nil, the EOL format is
inherited from the file itself.
Set this variable to nil if your Diff tool might use a different
EOL.  Then Emacs will auto-detect the EOL format in Diff output, which
gives better results.") ;; Cf. bug#4451.

(defun vc-coding-system-for-diff (file)
  "Return the coding system for reading diff output for FILE."
  (or coding-system-for-read
      ;; if we already have this file open,
      ;; use the buffer's coding system
      (let ((buf (find-buffer-visiting file)))
        (when buf (with-current-buffer buf
		    (if vc-coding-system-inherit-eol
			buffer-file-coding-system
		      ;; Don't inherit the EOL part of the coding-system,
		      ;; because some Diff tools may choose to use
		      ;; a different one.  bug#4451.
		      (coding-system-base buffer-file-coding-system)))))
      ;; otherwise, try to find one based on the file name
      (car (find-operation-coding-system 'insert-file-contents file))
      ;; and a final fallback
      'undecided))

(defun vc-switches (backend op)
  "Return a list of vc-BACKEND switches for operation OP.
BACKEND is a symbol such as `CVS', which will be downcased.
OP is a symbol such as `diff'.

In decreasing order of preference, return the value of:
vc-BACKEND-OP-switches (e.g. `vc-cvs-diff-switches');
vc-OP-switches (e.g. `vc-diff-switches'); or, in the case of
diff only, `diff-switches'.

If the chosen value is not a string or a list, return nil.
This is so that you may set, e.g. `vc-svn-diff-switches' to t in order
to override the value of `vc-diff-switches' and `diff-switches'."
  (let ((switches
	 (or (when backend
	       (let ((sym (vc-make-backend-sym
			   backend (intern (concat (symbol-name op)
						   "-switches")))))
		   (when (boundp sym) (symbol-value sym))))
	     (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
	       (when (boundp sym) (symbol-value sym)))
	     (cond
	      ((eq op 'diff) diff-switches)))))
    (if (stringp switches) (list switches)
      ;; If not a list, return nil.
      ;; This is so we can set vc-diff-switches to t to override
      ;; any switches in diff-switches.
      (when (listp switches) switches))))

;; Old def for compatibility with Emacs-21.[123].
(defmacro vc-diff-switches-list (backend) `(vc-switches ',backend 'diff))
(make-obsolete 'vc-diff-switches-list 'vc-switches "22.1")

(defun vc-diff-finish (buffer messages)
  ;; The empty sync output case has already been handled, so the only
  ;; possibility of an empty output is for an async process.
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer t))
          (emptyp (zerop (buffer-size buffer))))
      (with-current-buffer buffer
        (and messages emptyp
             (let ((inhibit-read-only t))
               (insert (cdr messages) ".\n")
               (message "%s" (cdr messages))))
        (goto-char (point-min))
        (when window
          (shrink-window-if-larger-than-buffer window)))
      (when (and messages (not emptyp))
        (message "%sdone" (car messages))))))

(defvar vc-diff-added-files nil
  "If non-nil, diff added files by comparing them to /dev/null.")

(defun vc-diff-internal (async vc-fileset rev1 rev2 &optional verbose buffer)
  "Report diffs between two revisions of a fileset.
Output goes to the buffer BUFFER, which defaults to *vc-diff*.
BUFFER, if non-nil, should be a buffer or a buffer name.
Return t if the buffer had changes, nil otherwise."
  (unless buffer
    (setq buffer "*vc-diff*"))
  (let* ((files (cadr vc-fileset))
	 (messages (cons (format "Finding changes in %s..."
                                 (vc-delistify files))
                         (format "No changes between %s and %s"
                                 (or rev1 "working revision")
                                 (or rev2 "workfile"))))
	 ;; Set coding system based on the first file.  It's a kluge,
	 ;; but the only way to set it for each file included would
	 ;; be to call the back end separately for each file.
	 (coding-system-for-read
	  (if files (vc-coding-system-for-diff (car files)) 'undecided)))
    (vc-setup-buffer buffer)
    (message "%s" (car messages))
    ;; Many backends don't handle well the case of a file that has been
    ;; added but not yet committed to the repo (notably CVS and Subversion).
    ;; Do that work here so the backends don't have to futz with it.  --ESR
    ;;
    ;; Actually most backends (including CVS) have options to control the
    ;; behavior since which one is better depends on the user and on the
    ;; situation).  Worse yet: this code does not handle the case where
    ;; `file' is a directory which contains added files.
    ;; I made it conditional on vc-diff-added-files but it should probably
    ;; just be removed (or copied/moved to specific backends).  --Stef.
    (when vc-diff-added-files
      (let ((filtered '())
	    process-file-side-effects)
        (dolist (file files)
          (if (or (file-directory-p file)
                  (not (string= (vc-working-revision file) "0")))
              (push file filtered)
            ;; This file is added but not yet committed;
            ;; there is no repository version to diff against.
            (if (or rev1 rev2)
                (error "No revisions of %s exist" file)
              ;; We regard this as "changed".
              ;; Diff it against /dev/null.
              (apply 'vc-do-command buffer
                     1 "diff" file
                     (append (vc-switches nil 'diff) '("/dev/null"))))))
        (setq files (nreverse filtered))))
    (let ((vc-disable-async-diff (not async)))
      (vc-call-backend (car vc-fileset) 'diff files rev1 rev2 buffer))
    (set-buffer buffer)
    (if (and (zerop (buffer-size))
             (not (get-buffer-process (current-buffer))))
        ;; Treat this case specially so as not to pop the buffer.
        (progn
          (message "%s" (cdr messages))
          nil)
      (diff-mode)
      (set (make-local-variable 'diff-vc-backend) (car vc-fileset))
      (set (make-local-variable 'revert-buffer-function)
	   `(lambda (ignore-auto noconfirm)
	      (vc-diff-internal ,async ',vc-fileset ,rev1 ,rev2 ,verbose)))
      ;; Make the *vc-diff* buffer read only, the diff-mode key
      ;; bindings are nicer for read only buffers. pcl-cvs does the
      ;; same thing.
      (setq buffer-read-only t)
      ;; Display the buffer, but at the end because it can change point.
      (pop-to-buffer (current-buffer))
      ;; The diff process may finish early, so call `vc-diff-finish'
      ;; after `pop-to-buffer'; the former assumes the diff buffer is
      ;; shown in some window.
      (vc-exec-after `(vc-diff-finish ,(current-buffer)
				      ',(when verbose messages)))
      ;; In the async case, we return t even if there are no differences
      ;; because we don't know that yet.
      t)))

(defun vc-read-revision (prompt &optional files backend default initial-input)
  (cond
   ((null files)
    (let ((vc-fileset (vc-deduce-fileset t))) ;FIXME: why t?  --Stef
      (setq files (cadr vc-fileset))
      (setq backend (car vc-fileset))))
   ((null backend) (setq backend (vc-backend (car files)))))
  (let ((completion-table
         (vc-call-backend backend 'revision-completion-table files)))
    (if completion-table
        (completing-read prompt completion-table
                         nil nil initial-input nil default)
      (read-string prompt initial-input nil default))))

(defun vc-diff-build-argument-list-internal ()
  "Build argument list for calling internal diff functions."
  (let* ((vc-fileset (vc-deduce-fileset t)) ;FIXME: why t?  --Stef
         (files (cadr vc-fileset))
         (backend (car vc-fileset))
         (first (car files))
         (rev1-default nil)
         (rev2-default nil))
    (cond
     ;; someday we may be able to do revision completion on non-singleton
     ;; filesets, but not yet.
     ((/= (length files) 1)
      nil)
     ;; if it's a directory, don't supply any revision default
     ((file-directory-p first)
      nil)
     ;; if the file is not up-to-date, use working revision as older revision
     ((not (vc-up-to-date-p first))
      (setq rev1-default (vc-working-revision first)))
     ;; if the file is not locked, use last and previous revisions as defaults
     (t
      (setq rev1-default (vc-call-backend backend 'previous-revision first
                                          (vc-working-revision first)))
      (when (string= rev1-default "") (setq rev1-default nil))
      (setq rev2-default (vc-working-revision first))))
    ;; construct argument list
    (let* ((rev1-prompt (if rev1-default
                            (concat "Older revision (default "
                                    rev1-default "): ")
                          "Older revision: "))
           (rev2-prompt (concat "Newer revision (default "
                                (or rev2-default "current source") "): "))
           (rev1 (vc-read-revision rev1-prompt files backend rev1-default))
           (rev2 (vc-read-revision rev2-prompt files backend rev2-default)))
      (when (string= rev1 "") (setq rev1 nil))
      (when (string= rev2 "") (setq rev2 nil))
      (list files rev1 rev2))))

;;;###autoload
(defun vc-version-diff (files rev1 rev2)
  "Report diffs between revisions of the fileset in the repository history."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  ;; Yes, it's painful to call (vc-deduce-fileset) again.  Alas, the
  ;; placement rules for (interactive) don't actually leave us a choice.
  (vc-diff-internal t (vc-deduce-fileset t) rev1 rev2
		    (called-interactively-p 'interactive)))

;;;###autoload
(defun vc-diff (historic &optional not-urgent)
  "Display diffs between file revisions.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-diff)
    (when buffer-file-name (vc-buffer-sync not-urgent))
    (vc-diff-internal t (vc-deduce-fileset t) nil nil
		      (called-interactively-p 'interactive))))

(declare-function ediff-vc-internal (rev1 rev2 &optional startup-hooks))

;;;###autoload
(defun vc-version-ediff (files rev1 rev2)
  "Show differences between revisions of the fileset in the
repository history using ediff."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))

  (message "%s" (format "Finding changes in %s..." (vc-delistify files)))

  ;; Functions ediff-(vc|rcs)-internal use "" instead of nil.
  (when (null rev1) (setq rev1 ""))
  (when (null rev2) (setq rev2 ""))

  (cond
   ;; FIXME We only support running ediff on one file for now.
   ;; We could spin off an ediff session per file in the file set.
   ((= (length files) 1)
    (ediff-load-version-control)
    (find-file (car files))             ;FIXME: find-file from Elisp is bad.
    (ediff-vc-internal rev1 rev2 nil))
   (t
    (error "More than one file is not supported"))))

;;;###autoload
(defun vc-ediff (historic &optional not-urgent)
  "Display diffs between file revisions using ediff.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-ediff)
    (when buffer-file-name (vc-buffer-sync not-urgent))
    (vc-version-ediff (cadr (vc-deduce-fileset t)) nil nil)))

;;;###autoload
(defun vc-root-diff (historic &optional not-urgent)
  "Display diffs between VC-controlled whole tree revisions.
Normally, this compares the tree corresponding to the current
fileset with the working revision.
With a prefix argument HISTORIC, prompt for two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      ;; FIXME: this does not work right, `vc-version-diff' ends up
      ;; calling `vc-deduce-fileset' to find the files to diff, and
      ;; that's not what we want here, we want the diff for the VC root dir.
      (call-interactively 'vc-version-diff)
    (when buffer-file-name (vc-buffer-sync not-urgent))
    (let ((backend (vc-deduce-backend))
	  rootdir working-revision)
      (unless backend
	(error "Buffer is not version controlled"))
      (setq rootdir (vc-call-backend backend 'root default-directory))
      (setq working-revision (vc-working-revision rootdir))
      ;; VC diff for the root directory produces output that is
      ;; relative to it.  Bind default-directory to the root directory
      ;; here, this way the *vc-diff* buffer is setup correctly, so
      ;; relative file names work.
      (let ((default-directory rootdir))
	(vc-diff-internal
	 t (list backend (list rootdir) working-revision) nil nil
	 (called-interactively-p 'interactive))))))

;;;###autoload
(defun vc-revision-other-window (rev)
  "Visit revision REV of the current file in another window.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  (interactive
   (save-current-buffer
     (vc-ensure-vc-buffer)
     (list
      (vc-read-revision "Revision to visit (default is working revision): "
                        (list buffer-file-name)))))
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (revision (if (string-equal rev "")
		      (vc-working-revision file)
		    rev)))
    (switch-to-buffer-other-window (vc-find-revision file revision))))

(defun vc-find-revision (file revision &optional backend)
  "Read REVISION of FILE into a buffer and return the buffer.
Use BACKEND as the VC backend if specified."
  (let ((automatic-backup (vc-version-backup-file-name file revision))
	(filebuf (or (get-file-buffer file) (current-buffer)))
        (filename (vc-version-backup-file-name file revision 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup filename nil)
	(message "Checking out %s..." filename)
	(with-current-buffer filebuf
	  (let ((failed t))
	    (unwind-protect
		(let ((coding-system-for-read 'no-conversion)
		      (coding-system-for-write 'no-conversion))
		  (with-temp-file filename
		    (let ((outbuf (current-buffer)))
		      ;; Change buffer to get local value of
		      ;; vc-checkout-switches.
		      (with-current-buffer filebuf
			(if backend
			    (vc-call-backend backend 'find-revision file revision outbuf)
			  (vc-call find-revision file revision outbuf)))))
		  (setq failed nil))
	      (when (and failed (file-exists-p filename))
		(delete-file filename))))
	  (vc-mode-line file))
	(message "Checking out %s...done" filename)))
    (let ((result-buf (find-file-noselect filename)))
      (with-current-buffer result-buf
	;; Set the parent buffer so that things like
	;; C-x v g, C-x v l, ... etc work.
	(set (make-local-variable 'vc-parent-buffer) filebuf))
      result-buf)))

;; Header-insertion code

;;;###autoload
(defun vc-insert-headers ()
  "Insert headers into a file for use with a version control system.
Headers desired are inserted at point, and are pulled from
the variable `vc-BACKEND-header'."
  (interactive)
  (vc-ensure-vc-buffer)
  (save-excursion
    (save-restriction
      (widen)
      (when (or (not (vc-check-headers))
		(y-or-n-p "Version headers already exist.  Insert another set? "))
	(let* ((delims (cdr (assq major-mode vc-comment-alist)))
	       (comment-start-vc (or (car delims) comment-start "#"))
	       (comment-end-vc (or (car (cdr delims)) comment-end ""))
	       (hdsym (vc-make-backend-sym (vc-backend buffer-file-name)
					   'header))
	       (hdstrings (and (boundp hdsym) (symbol-value hdsym))))
	  (dolist (s hdstrings)
	    (insert comment-start-vc "\t" s "\t"
		    comment-end-vc "\n"))
	  (when vc-static-header-alist
	    (dolist (f vc-static-header-alist)
	      (when (string-match (car f) buffer-file-name)
		(insert (format (cdr f) (car hdstrings)))))))))))

(defun vc-clear-headers (&optional file)
  "Clear all version headers in the current buffer (or FILE).
The headers are reset to their non-expanded form."
  (let* ((filename (or file buffer-file-name))
	 (visited (find-buffer-visiting filename))
	 (backend (vc-backend filename)))
    (when (vc-find-backend-function backend 'clear-headers)
	(if visited
	    (let ((context (vc-buffer-context)))
	      ;; save-excursion may be able to relocate point and mark
	      ;; properly.  If it fails, vc-restore-buffer-context
	      ;; will give it a second try.
	      (save-excursion
		(vc-call-backend backend 'clear-headers))
	      (vc-restore-buffer-context context))
	  (set-buffer (find-file-noselect filename))
	  (vc-call-backend backend 'clear-headers)
	  (kill-buffer filename)))))

(defun vc-modify-change-comment (files rev oldcomment)
  "Edit the comment associated with the given files and revision."
  ;; Less of a kluge than it looks like; log-view mode only passes
  ;; this function a singleton list.  Arguments left in this form in
  ;; case the more general operation ever becomes meaningful.
  (let ((backend (vc-responsible-backend (car files))))
    (vc-start-logentry
     files oldcomment t
     "Enter a replacement change comment."
     "*vc-log*"
     (lambda () (vc-call-backend backend 'log-edit-mode))
     (lexical-let ((rev rev)
                   (backend backend))
       (lambda (files comment)
         (vc-call-backend backend
                          'modify-change-comment files rev comment))))))

;;;###autoload
(defun vc-merge ()
  "Perform a version control merge operation.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"merge\"
operation to incorporate changes from another branch onto the
current branch, prompting for an argument list.

On a non-distributed version control system, this merges changes
between two revisions into the current fileset.  This asks for
two revisions to merge from in the minibuffer.  If the first
revision is a branch number, then merge all changes from that
branch.  If the first revision is empty, merge the most recent
changes from the current branch."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a branch-merge operation is defined, use it.
     ((vc-find-backend-function backend 'merge-branch)
      (vc-call-backend backend 'merge-branch))
     ;; Otherwise, do a per-file merge.
     ((vc-find-backend-function backend 'merge)
      (vc-buffer-sync)
      (dolist (file files)
	(let* ((state (vc-state file))
	       first-revision second-revision status)
	  (cond
	   ((stringp state)	;; Locking VCses only
	    (error "File %s is locked by %s" file state))
	   ((not (vc-editable-p file))
	    (vc-checkout file t)))
	  (setq first-revision
		(vc-read-revision
		 (concat "Merge " file
			 " from branch or revision "
			 "(default news on current branch): ")
		 (list file)
		 backend))
	  (cond
	   ((string= first-revision "")
	    (setq status (vc-call-backend backend 'merge-news file)))
	   (t
	    (if (not (vc-branch-p first-revision))
		(setq second-revision
		      (vc-read-revision
		       "Second revision: "
		       (list file) backend nil
		       ;; FIXME: This is CVS/RCS/SCCS specific.
		       (concat (vc-branch-part first-revision) ".")))
	      ;; We want to merge an entire branch.  Set revisions
	      ;; accordingly, so that vc-BACKEND-merge understands us.
	      (setq second-revision first-revision)
	      ;; first-revision must be the starting point of the branch
	      (setq first-revision (vc-branch-part first-revision)))
	    (setq status (vc-call-backend backend 'merge file
					  first-revision second-revision))))
	  (vc-maybe-resolve-conflicts file status "WORKFILE" "MERGE SOURCE"))))
     (t
      (error "Sorry, merging is not implemented for %s" backend)))))


(defun vc-maybe-resolve-conflicts (file status &optional name-A name-B)
  (vc-resynch-buffer file t (not (buffer-modified-p)))
  (if (zerop status) (message "Merge successful")
    (smerge-mode 1)
    (message "File contains conflicts.")))

;;;###autoload
(defalias 'vc-resolve-conflicts 'smerge-ediff)

;; TODO: This is OK but maybe we could integrate it better.
;; E.g. it could be run semi-automatically (via a prompt?) when saving a file
;; that was conflicted (i.e. upon mark-resolved).
;; FIXME: should we add an "other-window" version?  Or maybe we should
;; hook it inside find-file so it automatically works for
;; find-file-other-window as well.  E.g. find-file could use a new
;; `default-next-file' variable for its default file (M-n), and
;; we could then set it upon mark-resolve, so C-x C-s C-x C-f M-n would
;; automatically offer the next conflicted file.
(defun vc-find-conflicted-file ()
  "Visit the next conflicted file in the current project."
  (interactive)
  (let* ((backend (or (if buffer-file-name (vc-backend buffer-file-name))
                      (vc-responsible-backend default-directory)
                      (error "No VC backend")))
         (files (vc-call-backend backend
                                 'conflicted-files default-directory)))
    ;; Don't try and visit the current file.
    (if (equal (car files) buffer-file-name) (pop files))
    (if (null files)
        (message "No more conflicted files")
      (find-file (pop files))
      (message "%s more conflicted files after this one"
               (if files (length files) "No")))))

;; Named-configuration entry points

(defun vc-tag-precondition (dir)
  "Scan the tree below DIR, looking for files not up-to-date.
If any file is not up-to-date, return the name of the first such file.
\(This means, neither tag creation nor retrieval is allowed.\)
If one or more of the files are currently visited, return `visited'.
Otherwise, return nil."
  (let ((status nil))
    (catch 'vc-locked-example
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (if (not (vc-up-to-date-p f)) (throw 'vc-locked-example f)
	   (when (get-file-buffer f) (setq status 'visited)))))
      status)))

;;;###autoload
(defun vc-create-tag (dir name branchp)
  "Descending recursively from DIR, make a tag called NAME.
For each registered file, the working revision becomes part of
the named configuration.  If the prefix argument BRANCHP is
given, the tag is made as a new branch and the files are
checked out in that new branch."
  (interactive
   (let ((granularity
	  (vc-call-backend (vc-responsible-backend default-directory)
			   'revision-granularity)))
     (list
      (if (eq granularity 'repository)
	  ;; For VC's that do not work at file level, it's pointless
	  ;; to ask for a directory, branches are created at repository level.
	  default-directory
	(read-directory-name "Directory: " default-directory default-directory t))
      (read-string (if current-prefix-arg "New branch name: " "New tag name: "))
      current-prefix-arg)))
  (message "Making %s... " (if branchp "branch" "tag"))
  (when (file-directory-p dir) (setq dir (file-name-as-directory dir)))
  (vc-call-backend (vc-responsible-backend dir)
		   'create-tag dir name branchp)
  (vc-resynch-buffer dir t t t)
  (message "Making %s... done" (if branchp "branch" "tag")))

;;;###autoload
(defun vc-retrieve-tag (dir name)
  "Descending recursively from DIR, retrieve the tag called NAME.
If NAME is empty, it refers to the latest revisions.
If locking is used for the files in DIR, then there must not be any
locked files at or below DIR (but if NAME is empty, locked files are
allowed and simply skipped)."
  (interactive
   (let ((granularity
	  (vc-call-backend (vc-responsible-backend default-directory)
			   'revision-granularity)))
     (list
      (if (eq granularity 'repository)
	  ;; For VC's that do not work at file level, it's pointless
	  ;; to ask for a directory, branches are created at repository level.
	  default-directory
	(read-directory-name "Directory: " default-directory default-directory t))
      (read-string "Tag name to retrieve (default latest revisions): "))))
  (let ((update (yes-or-no-p "Update any affected buffers? "))
	(msg (if (or (not name) (string= name ""))
		 (format "Updating %s... " (abbreviate-file-name dir))
	       (format "Retrieving tag into %s... "
		       (abbreviate-file-name dir)))))
    (message "%s" msg)
    (vc-call-backend (vc-responsible-backend dir)
		     'retrieve-tag dir name update)
    (vc-resynch-buffer dir t t t)
    (message "%s" (concat msg "done"))))


;; Miscellaneous other entry points

;; FIXME: this should be a defcustom
;; FIXME: maybe add another choice:
;; `root-directory' (or somesuch), which would mean show a short log
;; for the root directory.
(defvar vc-log-short-style '(directory)
  "Whether or not to show a short log.
If it contains `directory' then if the fileset contains a directory show a short log.
If it contains `file' then show short logs for files.
Not all VC backends support short logs!")

(defvar log-view-vc-fileset)

(defun vc-print-log-setup-buttons (working-revision is-start-revision limit pl-return)
  (when (and limit (not (eq 'limit-unsupported pl-return))
	     (not is-start-revision))
    (goto-char (point-max))
    (lexical-let ((working-revision working-revision)
		  (limit limit))
      (insert "\n")
      (insert-text-button "Show 2X entries"
			  'action (lambda (&rest ignore)
				    (vc-print-log-internal
				     log-view-vc-backend log-view-vc-fileset
				     working-revision nil (* 2 limit)))
			  'help-echo "Show the log again, and double the number of log entries shown")
      (insert "    ")
      (insert-text-button "Show unlimited entries"
			  'action (lambda (&rest ignore)
				    (vc-print-log-internal
				     log-view-vc-backend log-view-vc-fileset
				     working-revision nil nil))
			  'help-echo "Show the log again, including all entries"))))

(defun vc-print-log-internal (backend files working-revision
                                      &optional is-start-revision limit)
  ;; Don't switch to the output buffer before running the command,
  ;; so that any buffer-local settings in the vc-controlled
  ;; buffer can be accessed by the command.
  (let ((dir-present nil)
	(vc-short-log nil)
	(buffer-name "*vc-change-log*")
	type
	pl-return)
    (dolist (file files)
      (when (file-directory-p file)
	(setq dir-present t)))
    (setq vc-short-log
	  (not (null (if dir-present
			 (memq 'directory vc-log-short-style)
		       (memq 'file vc-log-short-style)))))
    (setq type (if vc-short-log 'short 'long))
    (lexical-let
	((working-revision working-revision)
	 (backend backend)
	 (limit limit)
	 (shortlog vc-short-log)
	 (files files)
	 (is-start-revision is-start-revision))
      (vc-log-internal-common
       backend buffer-name files type
       (lambda (bk buf type-arg files-arg)
	 (vc-call-backend bk 'print-log files-arg buf
			  shortlog (when is-start-revision working-revision) limit))
       (lambda (bk files-arg ret)
	 (vc-print-log-setup-buttons working-revision
				     is-start-revision limit ret))
       (lambda (bk)
	 (vc-call-backend bk 'show-log-entry working-revision))
       (lambda (ignore-auto noconfirm)
	 (vc-print-log-internal backend files working-revision is-start-revision limit))))))

(defvar vc-log-view-type nil
  "Set this to differentiate the different types of logs.")
(put 'vc-log-view-type 'permanent-local t)

(defun vc-log-internal-common (backend
			       buffer-name
			       files
			       type
			       backend-func
			       setup-buttons-func
			       goto-location-func
			       rev-buff-func)
  (let (retval)
    (with-current-buffer (get-buffer-create buffer-name)
      (set (make-local-variable 'vc-log-view-type) type))
    (setq retval (funcall backend-func backend buffer-name type files))
    (pop-to-buffer buffer-name)
    (let ((inhibit-read-only t))
      ;; log-view-mode used to be called with inhibit-read-only bound
      ;; to t, so let's keep doing it, just in case.
      (vc-call-backend backend 'log-view-mode)
      (set (make-local-variable 'log-view-vc-backend) backend)
      (set (make-local-variable 'log-view-vc-fileset) files)
      (set (make-local-variable 'revert-buffer-function)
	   rev-buff-func))
    (vc-exec-after
     `(let ((inhibit-read-only t))
	(funcall ',setup-buttons-func ',backend ',files ',retval)
	(shrink-window-if-larger-than-buffer)
	(funcall ',goto-location-func ',backend)
	(setq vc-sentinel-movepoint (point))
	(set-buffer-modified-p nil)))))

(defun vc-incoming-outgoing-internal (backend remote-location buffer-name type)
  (vc-log-internal-common
   backend buffer-name nil type
   (lexical-let
       ((remote-location remote-location))
     (lambda (bk buf type-arg files)
       (vc-call-backend bk type-arg buf remote-location)))
   (lambda (bk files-arg ret))
   (lambda (bk)
     (goto-char (point-min)))
   (lexical-let
    ((backend backend)
     (remote-location remote-location)
     (buffer-name buffer-name)
     (type type))
    (lambda (ignore-auto noconfirm)
      (vc-incoming-outgoing-internal backend remote-location buffer-name type)))))

;;;###autoload
(defun vc-print-log (&optional working-revision limit)
  "List the change log of the current fileset in a window.
If WORKING-REVISION is non-nil, leave point at that revision.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.

When called interactively with a prefix argument, prompt for
WORKING-REVISION and LIMIT."
  (interactive
   (cond
    (current-prefix-arg
     (let ((rev (read-from-minibuffer "Log from revision (default: last revision): " nil
				      nil nil nil))
	   (lim (string-to-number
		 (read-from-minibuffer
		  "Limit display (unlimited: 0): "
		  (format "%s" vc-log-show-limit)
		  nil nil nil))))
       (when (string= rev "") (setq rev nil))
       (when (<= lim 0) (setq lim nil))
       (list rev lim)))
    (t
     (list nil (when (> vc-log-show-limit 0) vc-log-show-limit)))))
  (let* ((vc-fileset (vc-deduce-fileset t)) ;FIXME: Why t? --Stef
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset))
	 (working-revision (or working-revision (vc-working-revision (car files)))))
    (vc-print-log-internal backend files working-revision nil limit)))

;;;###autoload
(defun vc-print-root-log (&optional limit)
  "List the change log for the current VC controlled tree in a window.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.
When called interactively with a prefix argument, prompt for LIMIT."
  (interactive
   (cond
    (current-prefix-arg
     (let ((lim (string-to-number
		 (read-from-minibuffer
		  "Limit display (unlimited: 0): "
		  (format "%s" vc-log-show-limit)
		  nil nil nil))))
       (when (<= lim 0) (setq lim nil))
       (list lim)))
    (t
     (list (when (> vc-log-show-limit 0) vc-log-show-limit)))))
  (let ((backend (vc-deduce-backend))
	rootdir working-revision)
    (unless backend
      (error "Buffer is not version controlled"))
    (setq rootdir (vc-call-backend backend 'root default-directory))
    (setq working-revision (vc-working-revision rootdir))
    (vc-print-log-internal backend (list rootdir) working-revision nil limit)))

;;;###autoload
(defun vc-log-incoming (&optional remote-location)
  "Show a log of changes that will be received with a pull operation from REMOTE-LOCATION.
When called interactively with a prefix argument, prompt for REMOTE-LOCATION.."
  (interactive
   (when current-prefix-arg
     (list (read-string "Remote location (empty for default): "))))
  (let ((backend (vc-deduce-backend))
	rootdir working-revision)
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend remote-location "*vc-incoming*" 'log-incoming)))

;;;###autoload
(defun vc-log-outgoing (&optional remote-location)
  "Show a log of changes that will be sent with a push operation to REMOTE-LOCATION.
When called interactively with a prefix argument, prompt for REMOTE-LOCATION."
  (interactive
   (when current-prefix-arg
     (list (read-string "Remote location (empty for default): "))))
  (let ((backend (vc-deduce-backend))
	rootdir working-revision)
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend remote-location "*vc-outgoing*" 'log-outgoing)))

;;;###autoload
(defun vc-revert ()
  "Revert working copies of the selected fileset to their repository contents.
This asks for confirmation if the buffer contents are not identical
to the working revision (except for keyword expansion)."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset))
	 (files (cadr vc-fileset))
	 (queried nil)
	 diff-buffer)
    ;; If any of the files is visited by the current buffer, make sure
    ;; buffer is saved.  If the user says `no', abort since we cannot
    ;; show the changes and ask for confirmation to discard them.
    (when (or (not files) (memq (buffer-file-name) files))
      (vc-buffer-sync nil))
    (dolist (file files)
      (let ((buf (get-file-buffer file)))
	(when (and buf (buffer-modified-p buf))
	  (error "Please kill or save all modified buffers before reverting")))
      (when (vc-up-to-date-p file)
	(if (yes-or-no-p (format "%s seems up-to-date.  Revert anyway? " file))
	    (setq queried t)
	  (error "Revert canceled"))))
    (unwind-protect
	(when (if vc-revert-show-diff
		  (progn
		    (setq diff-buffer (generate-new-buffer-name "*vc-diff*"))
		    (vc-diff-internal vc-allow-async-revert vc-fileset
				      nil nil nil diff-buffer))
		;; Avoid querying the user again.
		(null queried))
	  (unless (yes-or-no-p
		   (format "Discard changes in %s? "
			   (let ((str (vc-delistify files))
				 (nfiles (length files)))
			     (if (< (length str) 50)
				 str
			       (format "%d file%s" nfiles
				       (if (= nfiles 1) "" "s"))))))
	    (error "Revert canceled")))
      (when diff-buffer
	(quit-windows-on diff-buffer t)))
    (dolist (file files)
      (message "Reverting %s..." (vc-delistify files))
      (vc-revert-file file)
      (message "Reverting %s...done" (vc-delistify files)))))

;;;###autoload
(defun vc-rollback ()
  "Roll back (remove) the most recent changeset committed to the repository.
This may be either a file-level or a repository-level operation,
depending on the underlying version-control system."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset))
	 (granularity (vc-call-backend backend 'revision-granularity)))
    (unless (vc-find-backend-function backend 'rollback)
      (error "Rollback is not supported in %s" backend))
    (when (and (not (eq granularity 'repository)) (/= (length files) 1))
      (error "Rollback requires a singleton fileset or repository versioning"))
    ;; FIXME: latest-on-branch-p should take the fileset.
    (when (not (vc-call-backend backend 'latest-on-branch-p (car files)))
      (error "Rollback is only possible at the tip revision"))
    ;; If any of the files is visited by the current buffer, make
    ;; sure buffer is saved.  If the user says `no', abort since
    ;; we cannot show the changes and ask for confirmation to
    ;; discard them.
    (when (or (not files) (memq (buffer-file-name) files))
      (vc-buffer-sync nil))
    (dolist (file files)
      (when (buffer-modified-p (get-file-buffer file))
	(error "Please kill or save all modified buffers before rollback"))
      (when (not (vc-up-to-date-p file))
	(error "Please revert all modified workfiles before rollback")))
    ;; Accumulate changes associated with the fileset
    (vc-setup-buffer "*vc-diff*")
    (not-modified)
    (message "Finding changes...")
    (let* ((tip (vc-working-revision (car files)))
           ;; FIXME: `previous-revision' should take the fileset.
	   (previous (vc-call-backend backend 'previous-revision
                                      (car files) tip)))
      (vc-diff-internal nil vc-fileset previous tip))
    ;; Display changes
    (unless (yes-or-no-p "Discard these revisions? ")
      (error "Rollback canceled"))
    (quit-windows-on "*vc-diff*" t)
    ;; Do the actual reversions
    (message "Rolling back %s..." (vc-delistify files))
    (with-vc-properties
     files
     (vc-call-backend backend 'rollback files)
     `((vc-state . ,'up-to-date)
       (vc-checkout-time . , (nth 5 (file-attributes file)))
       (vc-working-revision . nil)))
    (dolist (f files) (vc-resynch-buffer f t t))
    (message "Rolling back %s...done" (vc-delistify files))))

;;;###autoload
(define-obsolete-function-alias 'vc-revert-buffer 'vc-revert "23.1")

;;;###autoload
(defun vc-pull (&optional arg)
  "Update the current fileset or branch.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"pull\"
operation to update the current branch, prompting for an argument
list if required.  Optional prefix ARG forces a prompt.

On a non-distributed version control system, update the current
fileset to the tip revisions.  For each unchanged and unlocked
file, this simply replaces the work file with the latest revision
on its branch.  If the file contains changes, any changes in the
tip revision are merged into the working file."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a pull operation is defined, use it.
     ((vc-find-backend-function backend 'pull)
      (vc-call-backend backend 'pull arg))
     ;; If VCS has `merge-news' functionality (CVS and SVN), use it.
     ((vc-find-backend-function backend 'merge-news)
      (save-some-buffers ; save buffers visiting files
       nil (lambda ()
	     (and (buffer-modified-p)
		  (let ((file (buffer-file-name)))
		    (and file (member file files))))))
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file nil t)
	  (vc-maybe-resolve-conflicts
	   file (vc-call-backend backend 'merge-news file)))))
     ;; For a locking VCS, check out each file.
     ((eq (vc-checkout-model backend files) 'locking)
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file nil t))))
     (t
      (error "VC update is unsupported for `%s'" backend)))))

;;;###autoload
(defalias 'vc-update 'vc-pull)

(defun vc-version-backup-file (file &optional rev)
  "Return name of backup file for revision REV of FILE.
If version backups should be used for FILE, and there exists
such a backup for REV or the working revision of file, return
its name; otherwise return nil."
  (when (vc-call make-version-backups-p file)
    (let ((backup-file (vc-version-backup-file-name file rev)))
      (if (file-exists-p backup-file)
          backup-file
        ;; there is no automatic backup, but maybe the user made one manually
        (setq backup-file (vc-version-backup-file-name file rev 'manual))
        (when (file-exists-p backup-file)
	  backup-file)))))

(defun vc-revert-file (file)
  "Revert FILE back to the repository working revision it was based on."
  (with-vc-properties
   (list file)
   (let ((backup-file (vc-version-backup-file file)))
     (when backup-file
       (copy-file backup-file file 'ok-if-already-exists)
       (vc-delete-automatic-version-backups file))
     (vc-call revert file backup-file))
   `((vc-state . up-to-date)
     (vc-checkout-time . ,(nth 5 (file-attributes file)))))
  (vc-resynch-buffer file t t))

;;;###autoload
(defun vc-switch-backend (file backend)
  "Make BACKEND the current version control system for FILE.
FILE must already be registered in BACKEND.  The change is not
permanent, only for the current session.  This function only changes
VC's perspective on FILE, it does not register or unregister it.
By default, this command cycles through the registered backends.
To get a prompt, use a prefix argument."
  (interactive
   (list
    (or buffer-file-name
        (error "There is no version-controlled file in this buffer"))
    (let ((crt-bk (vc-backend buffer-file-name))
	  (backends nil))
      (unless crt-bk
        (error "File %s is not under version control" buffer-file-name))
      ;; Find the registered backends.
      (dolist (crt vc-handled-backends)
	(when (and (vc-call-backend crt 'registered buffer-file-name)
		   (not (eq crt-bk crt)))
	  (push crt backends)))
      ;; Find the next backend.
      (let ((def (car backends))
	    (others backends))
	(cond
	 ((null others) (error "No other backend to switch to"))
	 (current-prefix-arg
	  (intern
	   (upcase
	    (completing-read
	     (format "Switch to backend [%s]: " def)
	     (mapcar (lambda (b) (list (downcase (symbol-name b)))) backends)
	     nil t nil nil (downcase (symbol-name def))))))
	 (t def))))))
  (unless (eq backend (vc-backend file))
    (vc-file-clearprops file)
    (vc-file-setprop file 'vc-backend backend)
    ;; Force recomputation of the state
    (unless (vc-call-backend backend 'registered file)
      (vc-file-clearprops file)
      (error "%s is not registered in %s" file backend))
    (vc-mode-line file)))

;;;###autoload
(defun vc-transfer-file (file new-backend)
  "Transfer FILE to another version control system NEW-BACKEND.
If NEW-BACKEND has a higher precedence than FILE's current backend
\(i.e.  it comes earlier in `vc-handled-backends'), then register FILE in
NEW-BACKEND, using the revision number from the current backend as the
base level.  If NEW-BACKEND has a lower precedence than the current
backend, then commit all changes that were made under the current
backend to NEW-BACKEND, and unregister FILE from the current backend.
\(If FILE is not yet registered under NEW-BACKEND, register it.)"
  (let* ((old-backend (vc-backend file))
	 (edited (memq (vc-state file) '(edited needs-merge)))
	 (registered (vc-call-backend new-backend 'registered file))
	 (move
	  (and registered    ; Never move if not registered in new-backend yet.
	       ;; move if new-backend comes later in vc-handled-backends
	       (or (memq new-backend (memq old-backend vc-handled-backends))
		   (y-or-n-p "Final transfer? "))))
	 (comment nil))
    (when (eq old-backend new-backend)
      (error "%s is the current backend of %s" new-backend file))
    (if registered
	(set-file-modes file (logior (file-modes file) 128))
      ;; `registered' might have switched under us.
      (vc-switch-backend file old-backend)
      (let* ((rev (vc-working-revision file))
	     (modified-file (and edited (make-temp-file file)))
	     (unmodified-file (and modified-file (vc-version-backup-file file))))
	;; Go back to the base unmodified file.
	(unwind-protect
	    (progn
	      (when modified-file
		(copy-file file modified-file 'ok-if-already-exists)
		;; If we have a local copy of the unmodified file, handle that
		;; here and not in vc-revert-file because we don't want to
		;; delete that copy -- it is still useful for OLD-BACKEND.
		(if unmodified-file
		    (copy-file unmodified-file file
			       'ok-if-already-exists 'keep-date)
		  (when (y-or-n-p "Get base revision from repository? ")
		    (vc-revert-file file))))
	      (vc-call-backend new-backend 'receive-file file rev))
	  (when modified-file
	    (vc-switch-backend file new-backend)
	    (unless (eq (vc-checkout-model new-backend (list file)) 'implicit)
	      (vc-checkout file t nil))
	    (rename-file modified-file file 'ok-if-already-exists)
	    (vc-file-setprop file 'vc-checkout-time nil)))))
    (when move
      (vc-switch-backend file old-backend)
      (setq comment (vc-call-backend old-backend 'comment-history file))
      (vc-call-backend old-backend 'unregister file))
    (vc-switch-backend file new-backend)
    (when (or move edited)
      (vc-file-setprop file 'vc-state 'edited)
      (vc-mode-line file new-backend)
      (vc-checkin file new-backend nil comment (stringp comment)))))

(defun vc-rename-master (oldmaster newfile templates)
  "Rename OLDMASTER to be the master file for NEWFILE based on TEMPLATES."
  (let* ((dir (file-name-directory (expand-file-name oldmaster)))
	 (newdir (or (file-name-directory newfile) ""))
	 (newbase (file-name-nondirectory newfile))
	 (masters
	  ;; List of potential master files for `newfile'
	  (mapcar
	   (lambda (s) (vc-possible-master s newdir newbase))
	   templates)))
    (when (or (file-symlink-p oldmaster)
	      (file-symlink-p (file-name-directory oldmaster)))
      (error "This is unsafe in the presence of symbolic links"))
    (rename-file
     oldmaster
     (catch 'found
       ;; If possible, keep the master file in the same directory.
       (dolist (f masters)
	 (when (and f (string= (file-name-directory (expand-file-name f)) dir))
	   (throw 'found f)))
       ;; If not, just use the first possible place.
       (dolist (f masters)
	 (and f (or (not (setq dir (file-name-directory f)))
		    (file-directory-p dir))
	      (throw 'found f)))
       (error "New file lacks a version control directory")))))

;;;###autoload
(defun vc-delete-file (file)
  "Delete file and mark it as such in the version control system."
  (interactive "fVC delete file: ")
  (setq file (expand-file-name file))
  (let ((buf (get-file-buffer file))
        (backend (vc-backend file)))
    (unless backend
      (error "File %s is not under version control"
             (file-name-nondirectory file)))
    (unless (vc-find-backend-function backend 'delete-file)
      (error "Deleting files under %s is not supported in VC" backend))
    (when (and buf (buffer-modified-p buf))
      (error "Please save or undo your changes before deleting %s" file))
    (let ((state (vc-state file)))
      (when (eq state 'edited)
        (error "Please commit or undo your changes before deleting %s" file))
      (when (eq state 'conflict)
        (error "Please resolve the conflicts before deleting %s" file)))
    (unless (y-or-n-p (format "Really want to delete %s? "
			      (file-name-nondirectory file)))
      (error "Abort!"))
    (unless (or (file-directory-p file) (null make-backup-files)
                (not (file-exists-p file)))
      (with-current-buffer (or buf (find-file-noselect file))
	(let ((backup-inhibited nil))
	  (backup-buffer))))
    ;; Bind `default-directory' so that the command that the backend
    ;; runs to remove the file is invoked in the correct context.
    (let ((default-directory (file-name-directory file)))
      (vc-call-backend backend 'delete-file file))
    ;; If the backend hasn't deleted the file itself, let's do it for him.
    (when (file-exists-p file) (delete-file file))
    ;; Forget what VC knew about the file.
    (vc-file-clearprops file)
    ;; Make sure the buffer is deleted and the *vc-dir* buffers are
    ;; updated after this.
    (vc-resynch-buffer file nil t)))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW in both work area and repository."
  (interactive "fVC rename file: \nFRename to: ")
  ;; in CL I would have said (setq new (merge-pathnames new old))
  (let ((old-base (file-name-nondirectory old)))
    (when (and (not (string= "" old-base))
               (string= "" (file-name-nondirectory new)))
      (setq new (concat new old-base))))
  (let ((oldbuf (get-file-buffer old)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (error "Please save files before moving them"))
    (when (get-file-buffer new)
      (error "Already editing new file name"))
    (when (file-exists-p new)
      (error "New file already exists"))
    (let ((state (vc-state old)))
      (unless (memq state '(up-to-date edited))
	(error "Please %s files before moving them"
	       (if (stringp state) "check in" "update"))))
    (vc-call rename-file old new)
    (vc-file-clearprops old)
    ;; Move the actual file (unless the backend did it already)
    (when (file-exists-p old) (rename-file old new))
    ;; ?? Renaming a file might change its contents due to keyword expansion.
    ;; We should really check out a new copy if the old copy was precisely equal
    ;; to some checked-in revision.  However, testing for this is tricky....
    (when oldbuf
      (with-current-buffer oldbuf
	(let ((buffer-read-only buffer-read-only))
	  (set-visited-file-name new))
	(vc-mode-line new (vc-backend new))
	(set-buffer-modified-p nil)))))

;;;###autoload
(defun vc-update-change-log (&rest args)
  "Find change log file and add entries from recent version control logs.
Normally, find log entries for all registered files in the default
directory.

With prefix arg of \\[universal-argument], only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any ARGS are assumed to be filenames for which
log entries should be gathered."
  (interactive
   (cond ((consp current-prefix-arg)	;C-u
	  (list buffer-file-name))
	 (current-prefix-arg		;Numeric argument.
	  (let ((files nil)
		(buffers (buffer-list))
		file)
	    (while buffers
	      (setq file (buffer-file-name (car buffers)))
	      (and file (vc-backend file)
		   (setq files (cons file files)))
	      (setq buffers (cdr buffers)))
	    files))
	 (t
          ;; Don't supply any filenames to backend; this means
          ;; it should find all relevant files relative to
          ;; the default-directory.
	  nil)))
  (vc-call-backend (vc-responsible-backend default-directory)
                   'update-changelog args))

;; functions that operate on RCS revision numbers.  This code should
;; also be moved into the backends.  It stays for now, however, since
;; it is used in code below.
(defun vc-branch-p (rev)
  "Return t if REV is a branch revision."
  (not (eq nil (string-match "\\`[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*\\'" rev))))

;;;###autoload
(defun vc-branch-part (rev)
  "Return the branch part of a revision number REV."
  (let ((index (string-match "\\.[0-9]+\\'" rev)))
    (when index
      (substring rev 0 index))))

(defun vc-default-responsible-p (backend file)
  "Indicate whether BACKEND is responsible for FILE.
The default is to return nil always."
  nil)

(defun vc-default-could-register (backend file)
  "Return non-nil if BACKEND could be used to register FILE.
The default implementation returns t for all files."
  t)

(defun vc-default-latest-on-branch-p (backend file)
  "Return non-nil if FILE is the latest on its branch.
This default implementation always returns non-nil, which means that
editing non-current revisions is not supported by default."
  t)

(defun vc-default-init-revision (backend) vc-default-init-revision)

(defun vc-default-find-revision (backend file rev buffer)
  "Provide the new `find-revision' op based on the old `checkout' op.
This is only for compatibility with old backends.  They should be updated
to provide the `find-revision' operation instead."
  (let ((tmpfile (make-temp-file (expand-file-name file))))
    (unwind-protect
	(progn
	  (vc-call-backend backend 'checkout file nil rev tmpfile)
	  (with-current-buffer buffer
	    (insert-file-contents-literally tmpfile)))
      (delete-file tmpfile))))

(defun vc-default-rename-file (backend old new)
  (condition-case nil
      (add-name-to-file old new)
    (error (rename-file old new)))
  (vc-delete-file old)
  (with-current-buffer (find-file-noselect new)
    (vc-register)))

(defalias 'vc-default-check-headers 'ignore)

(declare-function log-edit-mode "log-edit" ())

(defun vc-default-log-edit-mode (backend) (log-edit-mode))

(defun vc-default-log-view-mode (backend) (log-view-mode))

(defun vc-default-show-log-entry (backend rev)
  (with-no-warnings
   (log-view-goto-rev rev)))

(defun vc-default-comment-history (backend file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (when (vc-find-backend-function backend 'print-log)
    (with-current-buffer "*vc*"
      (vc-call-backend backend 'print-log (list file))
      (buffer-string))))

(defun vc-default-receive-file (backend file rev)
  "Let BACKEND receive FILE from another version control system."
  (vc-call-backend backend 'register (list file) rev ""))

(defun vc-default-retrieve-tag (backend dir name update)
  (if (string= name "")
      (progn
        (vc-file-tree-walk
         dir
         (lambda (f) (and
		 (vc-up-to-date-p f)
		 (vc-error-occurred
		  (vc-call-backend backend 'checkout f nil "")
		  (when update (vc-resynch-buffer f t t)))))))
    (let ((result (vc-tag-precondition dir)))
      (if (stringp result)
          (error "File %s is locked" result)
        (setq update (and (eq result 'visited) update))
        (vc-file-tree-walk
         dir
         (lambda (f) (vc-error-occurred
		 (vc-call-backend backend 'checkout f nil name)
		 (when update (vc-resynch-buffer f t t)))))))))

(defun vc-default-revert (backend file contents-done)
  (unless contents-done
    (let ((rev (vc-working-revision file))
          (file-buffer (or (get-file-buffer file) (current-buffer))))
      (message "Checking out %s..." file)
      (let ((failed t)
            (backup-name (car (find-backup-file-name file))))
        (when backup-name
          (copy-file file backup-name 'ok-if-already-exists 'keep-date)
          (unless (file-writable-p file)
            (set-file-modes file (logior (file-modes file) 128))))
        (unwind-protect
            (let ((coding-system-for-read 'no-conversion)
                  (coding-system-for-write 'no-conversion))
              (with-temp-file file
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to get local value of vc-checkout-switches.
                  (with-current-buffer file-buffer
                    (let ((default-directory (file-name-directory file)))
                      (vc-call-backend backend 'find-revision
                                       file rev outbuf)))))
              (setq failed nil))
          (when backup-name
            (if failed
                (rename-file backup-name file 'ok-if-already-exists)
              (and (not vc-make-backup-files) (delete-file backup-name))))))
      (message "Checking out %s...done" file))))

(defalias 'vc-default-revision-completion-table 'ignore)
(defalias 'vc-default-mark-resolved 'ignore)

(defun vc-default-dir-status-files (backend dir files default-state update-function)
  (funcall update-function
           (mapcar (lambda (file) (list file default-state)) files)))

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (vc-call-backend (vc-backend buffer-file-name) 'check-headers))



;; These things should probably be generally available

(defun vc-string-prefix-p (prefix string)
  (let ((lpref (length prefix)))
    (and (>= (length string) lpref)
	 (eq t (compare-strings prefix nil nil string nil lpref)))))

(defun vc-file-tree-walk (dirname func &rest args)
  "Walk recursively through DIRNAME.
Invoke FUNC f ARGS on each VC-managed file f underneath it."
  (vc-file-tree-walk-internal (expand-file-name dirname) func args)
  (message "Traversing directory %s...done" dirname))

(defun vc-file-tree-walk-internal (file func args)
  (if (not (file-directory-p file))
      (when (vc-backend file) (apply func file args))
    (message "Traversing directory %s..." (abbreviate-file-name file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (lambda (f) (or
               (string-equal f ".")
               (string-equal f "..")
               (member f vc-directory-exclusion-list)
               (let ((dirf (expand-file-name f dir)))
                 (or
                  (file-symlink-p dirf) ;; Avoid possible loops.
                  (vc-file-tree-walk-internal dirf func args)))))
       (directory-files dir)))))

(provide 'vc)

;;; vc.el ends here
