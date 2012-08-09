;;; files.el --- file input and output commands for Emacs

;; Copyright (C) 1985-1987, 1992-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Package: emacs

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

;;; Commentary:

;; Defines most of Emacs'ss file- and directory-handling functions,
;; including basic file visiting, backup generation, link handling,
;; ITS-id version control, load- and write-hook handling, and the like.

;;; Code:

(eval-when-compile (require 'cl))

(defvar font-lock-keywords)

(defgroup backup nil
  "Backups of edited data files."
  :group 'files)

(defgroup find-file nil
  "Finding files."
  :group 'files)


(defcustom delete-auto-save-files t
  "Non-nil means delete auto-save file when a buffer is saved or killed.

Note that the auto-save file will not be deleted if the buffer is killed
when it has unsaved changes."
  :type 'boolean
  :group 'auto-save)

(defcustom directory-abbrev-alist
  nil
  "Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
FROM with TO when it appears in a directory name.  This replacement is
done when setting up the default directory of a newly visited file.

FROM is matched against directory names anchored at the first
character, so it should start with a \"\\\\`\", or, if directory
names cannot have embedded newlines, with a \"^\".

FROM and TO should be equivalent names, which refer to the
same directory.  Do not use `~' in the TO strings;
they should be ordinary absolute directory names.

Use this feature when you have directories which you normally refer to
via absolute symbolic links.  Make TO the name of the link, and FROM
the name it is linked to."
  :type '(repeat (cons :format "%v"
		       :value ("\\`" . "")
		       (regexp :tag "From")
		       (string :tag "To")))
  :group 'abbrev
  :group 'find-file)

(defcustom make-backup-files t
  "Non-nil means make a backup of a file the first time it is saved.
This can be done by renaming the file or by copying.

Renaming means that Emacs renames the existing file so that it is a
backup file, then writes the buffer into a new file.  Any other names
that the old file had will now refer to the backup file.  The new file
is owned by you and its group is defaulted.

Copying means that Emacs copies the existing file into the backup
file, then writes the buffer on top of the existing file.  Any other
names that the old file had will now refer to the new (edited) file.
The file's owner and group are unchanged.

The choice of renaming or copying is controlled by the variables
`backup-by-copying', `backup-by-copying-when-linked',
`backup-by-copying-when-mismatch' and
`backup-by-copying-when-privileged-mismatch'.  See also `backup-inhibited'."
  :type 'boolean
  :group 'backup)

;; Do this so that local variables based on the file name
;; are not overridden by the major mode.
(defvar backup-inhibited nil
  "Non-nil means don't make a backup, regardless of the other parameters.
This variable is intended for use by making it local to a buffer.
But it is local only if you make it local.")
(put 'backup-inhibited 'permanent-local t)

(defcustom backup-by-copying nil
 "Non-nil means always use copying to create backup files.
See documentation of variable `make-backup-files'."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying-when-linked nil
 "Non-nil means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if `backup-by-copying' is nil."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying-when-mismatch t
  "Non-nil means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if `backup-by-copying' is nil."
  :version "24.1"
  :type 'boolean
  :group 'backup)
(put 'backup-by-copying-when-mismatch 'permanent-local t)

(defcustom backup-by-copying-when-privileged-mismatch 200
  "Non-nil means create backups by copying to preserve a privileged owner.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner of the file or if the owner
has a user id greater than the value of this variable.  This is useful
when low-numbered uid's are used for special system users (such as root)
that must maintain ownership of certain files.
This variable is relevant only if `backup-by-copying' and
`backup-by-copying-when-mismatch' are nil."
  :type '(choice (const nil) integer)
  :group 'backup)

(defvar backup-enable-predicate 'normal-backup-enable-predicate
  "Predicate that looks at a file name and decides whether to make backups.
Called with an absolute file name as argument, it returns t to enable backup.")

(defcustom buffer-offer-save nil
  "Non-nil in a buffer means always offer to save buffer on exit.
Do so even if the buffer is not visiting a file.
Automatically local in all buffers."
  :type 'boolean
  :group 'backup)
(make-variable-buffer-local 'buffer-offer-save)
(put 'buffer-offer-save 'permanent-local t)

(defcustom find-file-existing-other-name t
  "Non-nil means find a file under alternative names, in existing buffers.
This means if any existing buffer is visiting the file you want
under another name, you get the existing buffer instead of a new buffer."
  :type 'boolean
  :group 'find-file)

(defcustom find-file-visit-truename nil
  "Non-nil means visit a file under its truename.
The truename of a file is found by chasing all links
both at the file level and at the levels of the containing directories."
  :type 'boolean
  :group 'find-file)
(put 'find-file-visit-truename 'safe-local-variable 'booleanp)

(defcustom revert-without-query nil
  "Specify which files should be reverted without query.
The value is a list of regular expressions.
If the file name matches one of these regular expressions,
then `revert-buffer' reverts the file without querying
if the file has changed on disk and you have not edited the buffer."
  :type '(repeat regexp)
  :group 'find-file)

(defvar buffer-file-number nil
  "The device number and file number of the file visited in the current buffer.
The value is a list of the form (FILENUM DEVNUM).
This pair of numbers uniquely identifies the file.
If the buffer is visiting a new file, the value is nil.")
(make-variable-buffer-local 'buffer-file-number)
(put 'buffer-file-number 'permanent-local t)

(defvar buffer-file-numbers-unique (not (memq system-type '(windows-nt)))
  "Non-nil means that `buffer-file-number' uniquely identifies files.")

(defvar buffer-file-read-only nil
  "Non-nil if visited file was read-only when visited.")
(make-variable-buffer-local 'buffer-file-read-only)

(defcustom small-temporary-file-directory
  (if (eq system-type 'ms-dos) (getenv "TMPDIR"))
  "The directory for writing small temporary files.
If non-nil, this directory is used instead of `temporary-file-directory'
by programs that create small temporary files.  This is for systems that
have fast storage with limited space, such as a RAM disk."
  :group 'files
  :initialize 'custom-initialize-delay
  :type '(choice (const nil) directory))

;; The system null device. (Should reference NULL_DEVICE from C.)
(defvar null-device (purecopy "/dev/null") "The system null device.")

(declare-function msdos-long-file-names "msdos.c")
(declare-function w32-long-file-name "w32proc.c")
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function dired-unmark "dired" (arg))
(declare-function dired-do-flagged-delete "dired" (&optional nomessage))
(declare-function dos-8+3-filename "dos-fns" (filename))
(declare-function view-mode-disable "view" ())
(declare-function dosified-file-name "dos-fns" (file-name))

(defvar file-name-invalid-regexp
  (cond ((and (eq system-type 'ms-dos) (not (msdos-long-file-names)))
	 (purecopy
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[+, ;=|<>\"?*]\\|\\[\\|\\]\\|"  ; invalid characters
		 "[\000-\037]\\|"		  ; control characters
		 "\\(/\\.\\.?[^/]\\)\\|"	  ; leading dots
		 "\\(/[^/.]+\\.[^/.]*\\.\\)")))	  ; more than a single dot
	((memq system-type '(ms-dos windows-nt cygwin))
	 (purecopy
	 (concat "^\\([^A-Z[-`a-z]\\|..+\\)?:\\|" ; colon except after drive
		 "[|<>\"?*\000-\037]")))		  ; invalid characters
	(t (purecopy "[\000]")))
  "Regexp recognizing file names which aren't allowed by the filesystem.")

(defcustom file-precious-flag nil
  "Non-nil means protect against I/O errors while saving files.
Some modes set this non-nil in particular buffers.

This feature works by writing the new contents into a temporary file
and then renaming the temporary file to replace the original.
In this way, any I/O error in writing leaves the original untouched,
and there is never any instant where the file is nonexistent.

Note that this feature forces backups to be made by copying.
Yet, at the same time, saving a precious file
breaks any hard links between it and other files.

This feature is advisory: for example, if the directory in which the
file is being saved is not writable, Emacs may ignore a non-nil value
of `file-precious-flag' and write directly into the file.

See also: `break-hardlink-on-save'."
  :type 'boolean
  :group 'backup)

(defcustom break-hardlink-on-save nil
  "Non-nil means when saving a file that exists under several names
\(i.e., has multiple hardlinks), break the hardlink associated with
`buffer-file-name' and write to a new file, so that the other
instances of the file are not affected by the save.

If `buffer-file-name' refers to a symlink, do not break the symlink.

Unlike `file-precious-flag', `break-hardlink-on-save' is not advisory.
For example, if the directory in which a file is being saved is not
itself writable, then error instead of saving in some
hardlink-nonbreaking way.

See also `backup-by-copying' and `backup-by-copying-when-linked'."
  :type 'boolean
  :group 'files
  :version "23.1")

(defcustom version-control nil
  "Control use of version numbers for backup files.
When t, make numeric backup versions unconditionally.
When nil, make them for files that have some already.
The value `never' means do not make them."
  :type '(choice (const :tag "Never" never)
		 (const :tag "If existing" nil)
		 (other :tag "Always" t))
  :group 'backup
  :group 'vc)
(put 'version-control 'safe-local-variable
     (lambda (x) (or (booleanp x) (equal x 'never))))

(defcustom dired-kept-versions 2
  "When cleaning directory, number of versions to keep."
  :type 'integer
  :group 'backup
  :group 'dired)

(defcustom delete-old-versions nil
  "If t, delete excess backup versions silently.
If nil, ask confirmation.  Any other value prevents any trimming."
  :type '(choice (const :tag "Delete" t)
		 (const :tag "Ask" nil)
		 (other :tag "Leave" other))
  :group 'backup)

(defcustom kept-old-versions 2
  "Number of oldest versions to keep when a new numbered backup is made."
  :type 'integer
  :group 'backup)
(put 'kept-old-versions 'safe-local-variable 'integerp)

(defcustom kept-new-versions 2
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0"
  :type 'integer
  :group 'backup)
(put 'kept-new-versions 'safe-local-variable 'integerp)

(defcustom require-final-newline nil
  "Whether to add a newline automatically at the end of the file.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.
A value of nil means don't add newlines.

Certain major modes set this locally to the value obtained
from `mode-require-final-newline'."
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics)

(defcustom mode-require-final-newline t
  "Whether to add a newline at end of file, in certain major modes.
Those modes set `require-final-newline' to this value when you enable them.
They do so because they are often used for files that are supposed
to end in newlines, and the question is how to arrange that.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.

A value of nil means do not add newlines.  That is a risky choice in this
variable since this value is used for modes for files that ought to have
final newlines.  So if you set this to nil, you must explicitly check and
add a final newline, whenever you save a file that really needs one."
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics
  :version "22.1")

(defcustom auto-save-default t
  "Non-nil says by default do auto-saving of every file-visiting buffer."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-file-name-transforms
  `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
     ;; Don't put "\\2" inside expand-file-name, since it will be
     ;; transformed to "/2" on DOS/Windows.
     ,(concat temporary-file-directory "\\2") t))
  "Transforms to apply to buffer file name before making auto-save file name.
Each transform is a list (REGEXP REPLACEMENT UNIQUIFY):
REGEXP is a regular expression to match against the file name.
If it matches, `replace-match' is used to replace the
matching part with REPLACEMENT.
If the optional element UNIQUIFY is non-nil, the auto-save file name is
constructed by taking the directory part of the replaced file-name,
concatenated with the buffer file name with all directory separators
changed to `!' to prevent clashes.  This will not work
correctly if your filesystem truncates the resulting name.

All the transforms in the list are tried, in the order they are listed.
When one transform applies, its result is final;
no further transforms are tried.

The default value is set up to put the auto-save file into the
temporary directory (see the variable `temporary-file-directory') for
editing a remote file.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'auto-save
  :type '(repeat (list (string :tag "Regexp") (string :tag "Replacement")
					   (boolean :tag "Uniquify")))
  :initialize 'custom-initialize-delay
  :version "21.1")

(defcustom save-abbrevs t
  "Non-nil means save word abbrevs too when files are saved.
If `silently', don't ask the user before saving."
  :type '(choice (const t) (const nil) (const silently))
  :group 'abbrev)

(defcustom find-file-run-dired t
  "Non-nil means allow `find-file' to visit directories.
To visit the directory, `find-file' runs `find-directory-functions'."
  :type 'boolean
  :group 'find-file)

(defcustom find-directory-functions '(cvs-dired-noselect dired-noselect)
  "List of functions to try in sequence to visit a directory.
Each function is called with the directory name as the sole argument
and should return either a buffer or nil."
  :type '(hook :options (cvs-dired-noselect dired-noselect))
  :group 'find-file)

;; FIXME: also add a hook for `(thing-at-point 'filename)'
(defcustom file-name-at-point-functions '(ffap-guess-file-name-at-point)
  "List of functions to try in sequence to get a file name at point.
Each function should return either nil or a file name found at the
location of point in the current buffer."
  :type '(hook :options (ffap-guess-file-name-at-point))
  :group 'find-file)

;;;It is not useful to make this a local variable.
;;;(put 'find-file-not-found-hooks 'permanent-local t)
(defvar find-file-not-found-functions nil
  "List of functions to be called for `find-file' on nonexistent file.
These functions are called as soon as the error is detected.
Variable `buffer-file-name' is already set up.
The functions are called in the order given until one of them returns non-nil.")
(define-obsolete-variable-alias 'find-file-not-found-hooks
    'find-file-not-found-functions "22.1")

;;;It is not useful to make this a local variable.
;;;(put 'find-file-hooks 'permanent-local t)
(define-obsolete-variable-alias 'find-file-hooks 'find-file-hook "22.1")
(defcustom find-file-hook nil
  "List of functions to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
functions are called."
  :group 'find-file
  :type 'hook
  :options '(auto-insert)
  :version "22.1")

(defvar write-file-functions nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the visited file.
So any buffer-local binding of this variable is discarded if you change
the visited file name with \\[set-visited-file-name], but not when you
change the major mode.

This hook is not run if any of the functions in
`write-contents-functions' returns non-nil.  Both hooks pertain
to how to save a buffer to file, for instance, choosing a suitable
coding system and setting mode bits.  (See Info
node `(elisp)Saving Buffers'.)  To perform various checks or
updates before the buffer is saved, use `before-save-hook'.")
(put 'write-file-functions 'permanent-local t)
(define-obsolete-variable-alias 'write-file-hooks 'write-file-functions "22.1")

(defvar local-write-file-hooks nil)
(make-variable-buffer-local 'local-write-file-hooks)
(put 'local-write-file-hooks 'permanent-local t)
(make-obsolete-variable 'local-write-file-hooks 'write-file-functions "22.1")

(defvar write-contents-functions nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called and neither are the functions in
`write-file-functions'.

This variable is meant to be used for hooks that pertain to the
buffer's contents, not to the particular visited file; thus,
`set-visited-file-name' does not clear this variable; but changing the
major mode does clear it.

For hooks that _do_ pertain to the particular visited file, use
`write-file-functions'.  Both this variable and
`write-file-functions' relate to how a buffer is saved to file.
To perform various checks or updates before the buffer is saved,
use `before-save-hook'.")
(make-variable-buffer-local 'write-contents-functions)
(define-obsolete-variable-alias 'write-contents-hooks
    'write-contents-functions "22.1")

(defcustom enable-local-variables t
  "Control use of local variables in files you visit.
The value can be t, nil, :safe, :all, or something else.

A value of t means file local variables specifications are obeyed
if all the specified variable values are safe; if any values are
not safe, Emacs queries you, once, whether to set them all.
\(When you say yes to certain values, they are remembered as safe.)

:safe means set the safe variables, and ignore the rest.
:all means set all variables, whether safe or not.
 (Don't set it permanently to :all.)
A value of nil means always ignore the file local variables.

Any other value means always query you once whether to set them all.
\(When you say yes to certain values, they are remembered as safe, but
this has no effect when `enable-local-variables' is \"something else\".)

This variable also controls use of major modes specified in
a -*- line.

The command \\[normal-mode], when used interactively,
always obeys file local variable specifications and the -*- line,
and ignores this variable."
  :risky t
  :type '(choice (const :tag "Query Unsafe" t)
		 (const :tag "Safe Only" :safe)
		 (const :tag "Do all" :all)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

;; This is an odd variable IMO.
;; You might wonder why it is needed, when we could just do:
;; (set (make-local-variable 'enable-local-variables) nil)
;; These two are not precisely the same.
;; Setting this variable does not cause -*- mode settings to be
;; ignored, whereas setting enable-local-variables does.
;; Only three places in Emacs use this variable: tar and arc modes,
;; and rmail.  The first two don't need it.  They already use
;; inhibit-local-variables-regexps, which is probably enough, and
;; could also just set enable-local-variables locally to nil.
;; Them setting it has the side-effect that dir-locals cannot apply to
;; eg tar files (?).  FIXME Is this appropriate?
;; AFAICS, rmail is the only thing that needs this, and the only
;; reason it uses it is for BABYL files (which are obsolete).
;; These contain "-*- rmail -*-" in the first line, which rmail wants
;; to respect, so that find-file on a BABYL file will switch to
;; rmail-mode automatically (this is nice, but hardly essential,
;; since most people are used to explicitly running a command to
;; access their mail; M-x gnus etc).  Rmail files may happen to
;; contain Local Variables sections in messages, which Rmail wants to
;; ignore.  So AFAICS the only reason this variable exists is for a
;; minor convenience feature for handling of an obsolete Rmail file format.
(defvar local-enable-local-variables t
  "Like `enable-local-variables' but meant for buffer-local bindings.
The meaningful values are nil and non-nil.  The default is non-nil.
If a major mode sets this to nil, buffer-locally, then any local
variables list in a file visited in that mode will be ignored.

This variable does not affect the use of major modes specified
in a -*- line.")

(defcustom enable-local-eval 'maybe
  "Control processing of the \"variable\" `eval' in a file's local variables.
The value can be t, nil or something else.
A value of t means obey `eval' variables.
A value of nil means ignore them; anything else means query."
  :risky t
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'find-file)

;; Avoid losing in versions where CLASH_DETECTION is disabled.
(or (fboundp 'lock-buffer)
    (defalias 'lock-buffer 'ignore))
(or (fboundp 'unlock-buffer)
    (defalias 'unlock-buffer 'ignore))
(or (fboundp 'file-locked-p)
    (defalias 'file-locked-p 'ignore))

(defcustom view-read-only nil
  "Non-nil means buffers visiting files read-only do so in view mode.
In fact, this means that all read-only buffers normally have
View mode enabled, including buffers that are read-only because
you visit a file you cannot alter, and buffers you make read-only
using \\[toggle-read-only]."
  :type 'boolean
  :group 'view)

(defvar file-name-history nil
  "History list of file names entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(put 'ange-ftp-completion-hook-function 'safe-magic t)
(defun ange-ftp-completion-hook-function (op &rest args)
  "Provides support for ange-ftp host name completion.
Runs the usual ange-ftp hook, but only for completion operations."
  ;; Having this here avoids the need to load ange-ftp when it's not
  ;; really in use.
  (if (memq op '(file-name-completion file-name-all-completions))
      (apply 'ange-ftp-hook-function op args)
    (let ((inhibit-file-name-handlers
	   (cons 'ange-ftp-completion-hook-function
		 (and (eq inhibit-file-name-operation op)
		      inhibit-file-name-handlers)))
	  (inhibit-file-name-operation op))
      (apply op args))))

(declare-function dos-convert-standard-filename "dos-fns.el" (filename))
(declare-function w32-convert-standard-filename "w32-fns.el" (filename))

(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the OS.
This means to guarantee valid names and perhaps to canonicalize
certain patterns.

FILENAME should be an absolute file name since the conversion rules
sometimes vary depending on the position in the file name.  E.g. c:/foo
is a valid DOS file name, but c:/bar/c:/foo is not.

This function's standard definition is trivial; it just returns
the argument.  However, on Windows and DOS, replace invalid
characters.  On DOS, make sure to obey the 8.3 limitations.
In the native Windows build, turn Cygwin names into native names,
and also turn slashes into backslashes if the shell requires it (see
`w32-shell-dos-semantics').

See Info node `(elisp)Standard File Names' for more details."
  (cond
   ((eq system-type 'cygwin)
    (let ((name (copy-sequence filename))
	  (start 0))
      ;; Replace invalid filename characters with !
      (while (string-match "[?*:<>|\"\000-\037]" name start)
	(aset name (match-beginning 0) ?!)
	(setq start (match-end 0)))
      name))
   ((eq system-type 'windows-nt)
    (w32-convert-standard-filename filename))
   ((eq system-type 'ms-dos)
    (dos-convert-standard-filename filename))
   (t filename)))

(defun read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-DIRNAME if user exits with the same
non-empty string that was inserted by this function.
 (If DEFAULT-DIRNAME is omitted, DIR combined with INITIAL is used,
  or just DIR if INITIAL is nil.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg MUSTMATCH non-nil means require existing directory's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
DIR should be an absolute directory name.  It defaults to
the value of `default-directory'."
  (unless dir
    (setq dir default-directory))
  (read-file-name prompt dir (or default-dirname
				 (if initial (expand-file-name initial dir)
				   dir))
		  mustmatch initial
		  'file-directory-p))


(defun pwd ()
  "Show the current default directory."
  (interactive nil)
  (message "Directory %s" default-directory))

(defvar cd-path nil
  "Value of the CDPATH environment variable, as a list.
Not actually set up until the first time you use it.")

(defun parse-colon-path (search-path)
  "Explode a search path into a list of directory names.
Directories are separated by occurrences of `path-separator'
\(which is colon in GNU and GNU-like systems)."
  ;; We could use split-string here.
  (and search-path
       (let (cd-list (cd-start 0) cd-colon)
	 (setq search-path (concat search-path path-separator))
	 (while (setq cd-colon (string-match path-separator search-path cd-start))
	   (setq cd-list
		 (nconc cd-list
			(list (if (= cd-start cd-colon)
				   nil
				(substitute-in-file-name
				 (file-name-as-directory
				  (substring search-path cd-start cd-colon)))))))
	   (setq cd-start (+ cd-colon 1)))
	 cd-list)))

(defun cd-absolute (dir)
  "Change current directory to given absolute file name DIR."
  ;; Put the name into directory syntax now,
  ;; because otherwise expand-file-name may give some bad results.
  (setq dir (file-name-as-directory dir))
  ;; We used to additionally call abbreviate-file-name here, for an
  ;; unknown reason.  Problem is that most buffers are setup
  ;; without going through cd-absolute and don't call
  ;; abbreviate-file-name on their default-directory, so the few that
  ;; do end up using a superficially different directory.
  (setq dir (expand-file-name dir))
  (if (not (file-directory-p dir))
      (if (file-exists-p dir)
	  (error "%s is not a directory" dir)
	(error "%s: no such directory" dir))
    (unless (file-executable-p dir)
      (error "Cannot cd to %s:  Permission denied" dir))
    (setq default-directory dir)
    (setq list-buffers-directory dir)))

(defun cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of
that list of directories (separated by occurrences of
`path-separator') when resolving a relative directory name.
The path separator is colon in GNU and GNU-like systems."
  (interactive
   (list
    ;; FIXME: There's a subtle bug in the completion below.  Seems linked
    ;; to a fundamental difficulty of implementing `predicate' correctly.
    ;; The manifestation is that TAB may list non-directories in the case where
    ;; those files also correspond to valid directories (if your cd-path is (A/
    ;; B/) and you have A/a a file and B/a a directory, then both `a' and `a/'
    ;; will be listed as valid completions).
    ;; This is because `a' (listed because of A/a) is indeed a valid choice
    ;; (which will lead to the use of B/a).
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table
                (apply-partially #'locate-file-completion-table
                                 cd-path nil))
          (setq minibuffer-completion-predicate
                (lambda (dir)
                  (locate-file dir cd-path nil
                               (lambda (f) (and (file-directory-p f) 'dir-ok))))))
      (unless cd-path
        (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                          (list "./"))))
      (read-directory-name "Change default directory: "
                           default-directory default-directory
                           t))))
  (unless cd-path
    (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                      (list "./"))))
  (cd-absolute
   (or (locate-file dir cd-path nil
                    (lambda (f) (and (file-directory-p f) 'dir-ok)))
       (error "No such directory found via CDPATH environment variable"))))

(defun load-file (file)
  "Load the Lisp file named FILE."
  ;; This is a case where .elc makes a lot of sense.
  (interactive (list (let ((completion-ignored-extensions
			    (remove ".elc" completion-ignored-extensions)))
		       (read-file-name "Load file: "))))
  (load (expand-file-name file) nil nil t))

(defun locate-file (filename path &optional suffixes predicate)
  "Search for FILENAME through PATH.
If found, return the absolute file name of FILENAME, with its suffixes;
otherwise return nil.
PATH should be a list of directories to look in, like the lists in
`exec-path' or `load-path'.
If SUFFIXES is non-nil, it should be a list of suffixes to append to
file name when searching.  If SUFFIXES is nil, it is equivalent to '(\"\").
Use '(\"/\") to disable PATH search, but still try the suffixes in SUFFIXES.
If non-nil, PREDICATE is used instead of `file-readable-p'.

This function will normally skip directories, so if you want it to find
directories, make sure the PREDICATE function returns `dir-ok' for them.

PREDICATE can also be an integer to pass to the `access' system call,
in which case file-name handlers are ignored.  This usage is deprecated.
For compatibility, PREDICATE can also be one of the symbols
`executable', `readable', `writable', or `exists', or a list of
one or more of those symbols."
  (if (and predicate (symbolp predicate) (not (functionp predicate)))
      (setq predicate (list predicate)))
  (when (and (consp predicate) (not (functionp predicate)))
    (setq predicate
	  (logior (if (memq 'executable predicate) 1 0)
		  (if (memq 'writable predicate) 2 0)
		  (if (memq 'readable predicate) 4 0))))
  (locate-file-internal filename path suffixes predicate))

(defun locate-file-completion-table (dirs suffixes string pred action)
  "Do completion for file names passed to `locate-file'."
  (cond
   ((file-name-absolute-p string)
    ;; FIXME: maybe we should use completion-file-name-table instead,
    ;; tho at least for `load', the arg is passed through
    ;; substitute-in-file-name for historical reasons.
    (read-file-name-internal string pred action))
   ((eq (car-safe action) 'boundaries)
    (let ((suffix (cdr action)))
      (list* 'boundaries
             (length (file-name-directory string))
             (let ((x (file-name-directory suffix)))
               (if x (1- (length x)) (length suffix))))))
   (t
    (let ((names '())
          ;; If we have files like "foo.el" and "foo.elc", we could load one of
          ;; them with "foo.el", "foo.elc", or "foo", where just "foo" is the
          ;; preferred way.  So if we list all 3, that gives a lot of redundant
          ;; entries for the poor soul looking just for "foo".  OTOH, sometimes
          ;; the user does want to pay attention to the extension.  We try to
          ;; diffuse this tension by stripping the suffix, except when the
          ;; result is a single element (i.e. usually we only list "foo" unless
          ;; it's the only remaining element in the list, in which case we do
          ;; list "foo", "foo.elc" and "foo.el").
          (fullnames '())
	  (suffix (concat (regexp-opt suffixes t) "\\'"))
	  (string-dir (file-name-directory string))
          (string-file (file-name-nondirectory string)))
      (dolist (dir dirs)
        (unless dir
          (setq dir default-directory))
        (if string-dir (setq dir (expand-file-name string-dir dir)))
        (when (file-directory-p dir)
          (dolist (file (file-name-all-completions
                         string-file dir))
            (if (not (string-match suffix file))
                (push file names)
              (push file fullnames)
              (push (substring file 0 (match-beginning 0)) names)))))
      ;; Switching from names to names+fullnames creates a non-monotonicity
      ;; which can cause problems with things like partial-completion.
      ;; To minimize the problem, filter out completion-regexp-list, so that
      ;; M-x load-library RET t/x.e TAB finds some files.  Also remove elements
      ;; from `names' which only matched `string' when they still had
      ;; their suffix.
      (setq names (all-completions string names))
      ;; Remove duplicates of the first element, so that we can easily check
      ;; if `names' really only contains a single element.
      (when (cdr names) (setcdr names (delete (car names) (cdr names))))
      (unless (cdr names)
        ;; There's no more than one matching non-suffixed element, so expand
        ;; the list by adding the suffixed elements as well.
        (setq names (nconc names fullnames)))
      (completion-table-with-context
       string-dir names string-file pred action)))))

(defun locate-file-completion (string path-and-suffixes action)
  "Do completion for file names passed to `locate-file'.
PATH-AND-SUFFIXES is a pair of lists, (DIRECTORIES . SUFFIXES)."
  (locate-file-completion-table (car path-and-suffixes)
                                (cdr path-and-suffixes)
                                string nil action))
(make-obsolete 'locate-file-completion 'locate-file-completion-table "23.1")

(defvar locate-dominating-stop-dir-regexp
  (purecopy "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
  "Regexp of directory names which stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file' which will stop its search
when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which filenames are interpreted as hostnames,
or mount points potentially requiring authentication as a different user.")

;; (defun locate-dominating-files (file regexp)
;;   "Look up the directory hierarchy from FILE for a file matching REGEXP.
;; Stop at the first parent where a matching file is found and return the list
;; of files that that match in this directory."
;;   (catch 'found
;;     ;; `user' is not initialized yet because `file' may not exist, so we may
;;     ;; have to walk up part of the hierarchy before we find the "initial UID".
;;     (let ((user nil)
;;           ;; Abbreviate, so as to stop when we cross ~/.
;;           (dir (abbreviate-file-name (file-name-as-directory file)))
;;           files)
;;       (while (and dir
;;                   ;; As a heuristic, we stop looking up the hierarchy of
;;                   ;; directories as soon as we find a directory belonging to
;;                   ;; another user.  This should save us from looking in
;;                   ;; things like /net and /afs.  This assumes that all the
;;                   ;; files inside a project belong to the same user.
;;                   (let ((prev-user user))
;;                     (setq user (nth 2 (file-attributes dir)))
;;                     (or (null prev-user) (equal user prev-user))))
;;         (if (setq files (condition-case nil
;; 			    (directory-files dir 'full regexp 'nosort)
;; 			  (error nil)))
;;             (throw 'found files)
;;           (if (equal dir
;;                      (setq dir (file-name-directory
;;                                 (directory-file-name dir))))
;;               (setq dir nil))))
;;       nil)))

(defun locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.

This function only tests if FILE exists.  If you care about whether
it is readable, regular, etc., you should test the result."
  ;; We used to use the above locate-dominating-files code, but the
  ;; directory-files call is very costly, so we're much better off doing
  ;; multiple calls using the code in here.
  ;;
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `name' in /home or in /.
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        ;; `user' is not initialized outside the loop because
        ;; `file' may not exist, so we may have to walk up part of the
        ;; hierarchy before we find the "initial UID".  Note: currently unused
        ;; (user nil)
        try)
    (while (not (or root
                    (null file)
                    ;; FIXME: Disabled this heuristic because it is sometimes
                    ;; inappropriate.
                    ;; As a heuristic, we stop looking up the hierarchy of
                    ;; directories as soon as we find a directory belonging
                    ;; to another user.  This should save us from looking in
                    ;; things like /net and /afs.  This assumes that all the
                    ;; files inside a project belong to the same user.
                    ;; (let ((prev-user user))
                    ;;   (setq user (nth 2 (file-attributes file)))
                    ;;   (and prev-user (not (equal user prev-user))))
                    (string-match locate-dominating-stop-dir-regexp file)))
      ;; FIXME? maybe this function should (optionally?)
      ;; use file-readable-p instead.  In many cases, an unreadable
      ;; FILE is no better than a non-existent one.
      ;; See eg dir-locals-find-file.
      (setq try (file-exists-p (expand-file-name name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    root))


(defun executable-find (command)
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  ;; Use 1 rather than file-executable-p to better match the behavior of
  ;; call-process.
  (locate-file command exec-path exec-suffixes 1))

(defun load-library (library)
  "Load the Emacs Lisp library named LIBRARY.
This is an interface to the function `load'.  LIBRARY is searched
for in `load-path', both with and without `load-suffixes' (as
well as `load-file-rep-suffixes').

See Info node `(emacs)Lisp Libraries' for more details.
See `load-file' for a different interface to `load'."
  (interactive
   (list (completing-read "Load library: "
			  (apply-partially 'locate-file-completion-table
                                           load-path
                                           (get-load-suffixes)))))
  (load library))

(defun file-remote-p (file &optional identification connected)
  "Test whether FILE specifies a location on a remote system.
A file is considered remote if accessing it is likely to
be slower or less reliable than accessing local files.

`file-remote-p' never opens a new remote connection.  It can
only reuse a connection that is already open.

Return nil or a string identifying the remote connection
\(ideally a prefix of FILE).  Return nil if FILE is a relative
file name.

When IDENTIFICATION is nil, the returned string is a complete
remote identifier: with components method, user, and host.  The
components are those present in FILE, with defaults filled in for
any that are missing.

IDENTIFICATION can specify which part of the identification to
return.  IDENTIFICATION can be the symbol `method', `user',
`host', or `localname'.  Any other value is handled like nil and
means to return the complete identification.  The string returned
for IDENTIFICATION `localname' can differ depending on whether
there is an existing connection.

If CONNECTED is non-nil, return an identification only if FILE is
located on a remote system and a connection is established to
that remote system.

Tip: You can use this expansion of remote identifier components
     to derive a new remote file name from an existing one.  For
     example, if FILE is \"/sudo::/path/to/file\" then

       \(concat \(file-remote-p FILE) \"/bin/sh\")

     returns a remote file name for file \"/bin/sh\" that has the
     same remote identifier as FILE but expanded; a name such as
     \"/sudo:root@myhost:/bin/sh\"."
  (let ((handler (find-file-name-handler file 'file-remote-p)))
    (if handler
	(funcall handler 'file-remote-p file identification connected)
      nil)))

(defcustom remote-file-name-inhibit-cache 10
  "Whether to use the remote file-name cache for read access.
When `nil', never expire cached values (caution)
When `t', never use the cache (safe, but may be slow)
A number means use cached values for that amount of seconds since caching.

The attributes of remote files are cached for better performance.
If they are changed outside of Emacs's control, the cached values
become invalid, and must be reread.  If you are sure that nothing
other than Emacs changes the files, you can set this variable to `nil'.

If a remote file is checked regularly, it might be a good idea to
let-bind this variable to a value less than the interval between
consecutive checks.  For example:

  (defun display-time-file-nonempty-p (file)
    (let ((remote-file-name-inhibit-cache (- display-time-interval 5)))
      (and (file-exists-p file)
           (< 0 (nth 7 (file-attributes (file-chase-links file)))))))"
  :group 'files
  :version "24.1"
  :type `(choice
	  (const   :tag "Do not inhibit file name cache" nil)
	  (const   :tag "Do not use file name cache" t)
	  (integer :tag "Do not use file name cache"
		   :format "Do not use file name cache older then %v seconds"
		   :value 10)))

(defun file-local-copy (file)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  ;; This formerly had an optional BUFFER argument that wasn't used by
  ;; anything.
  (let ((handler (find-file-name-handler file 'file-local-copy)))
    (if handler
	(funcall handler 'file-local-copy file)
      nil)))

(defun file-truename (filename &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are only used in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
	 (setq filename (expand-file-name filename))
	 (if (string= filename "")
	     (setq filename "/")))
	((and (string= (substring filename 0 1) "~")
	      (string-match "~[^/]*/?" filename))
	 (let ((first-part
		(substring filename 0 (match-end 0)))
	       (rest (substring filename (match-end 0))))
	   (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list 100)))
  (let (done
	;; For speed, remove the ange-ftp completion handler from the list.
	;; We know it's not needed here.
	;; For even more speed, do this only on the outermost call.
	(file-name-handler-alist
	 (if prev-dirs file-name-handler-alist
	   (let ((tem (copy-sequence file-name-handler-alist)))
	     (delq (rassq 'ange-ftp-completion-hook-function tem) tem)))))
    (or prev-dirs (setq prev-dirs (list nil)))

    ;; andrewi@harlequin.co.uk - none of the following code (except for
    ;; invoking the file-name handler) currently applies on Windows
    ;; (ie. there are no native symlinks), but there is an issue with
    ;; case differences being ignored by the OS, and short "8.3 DOS"
    ;; name aliases existing for all files.  (The short names are not
    ;; reported by directory-files, but can be used to refer to files.)
    ;; It seems appropriate for file-truename to resolve these issues in
    ;; the most natural way, which on Windows is to call the function
    ;; `w32-long-file-name' - this returns the exact name of a file as
    ;; it is stored on disk (expanding short name aliases with the full
    ;; name in the process).
    (if (eq system-type 'windows-nt)
      (let ((handler (find-file-name-handler filename 'file-truename)))
	;; For file name that has a special handler, call handler.
	;; This is so that ange-ftp can save time by doing a no-op.
	(if handler
	    (setq filename (funcall handler 'file-truename filename))
	  ;; If filename contains a wildcard, newname will be the old name.
	  (unless (string-match "[[*?]" filename)
	    ;; If filename exists, use the long name.  If it doesn't exist,
            ;; drill down until we find a directory that exists, and use
            ;; the long name of that, with the extra non-existent path
            ;; components concatenated.
            (let ((longname (w32-long-file-name filename))
                  missing rest)
              (if longname
                  (setq filename longname)
                ;; Include the preceding directory separator in the missing
                ;; part so subsequent recursion on the rest works.
                (setq missing (concat "/" (file-name-nondirectory filename)))
		(let ((length (length missing)))
		  (setq rest
			(if (> length (length filename))
			    ""
			  (substring filename 0 (- length)))))
                (setq filename (concat (file-truename rest) missing))))))
	(setq done t)))

    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
	  (error "Apparent cycle of symbolic links for %s" filename))
      (let ((handler (find-file-name-handler filename 'file-truename)))
	;; For file name that has a special handler, call handler.
	;; This is so that ange-ftp can save time by doing a no-op.
	(if handler
	    (setq filename (funcall handler 'file-truename filename)
		  done t)
	  (let ((dir (or (file-name-directory filename) default-directory))
		target dirfile)
	    ;; Get the truename of the directory.
	    (setq dirfile (directory-file-name dir))
	    ;; If these are equal, we have the (or a) root directory.
	    (or (string= dir dirfile)
		;; If this is the same dir we last got the truename for,
		;; save time--don't recalculate.
		(if (assoc dir (car prev-dirs))
		    (setq dir (cdr (assoc dir (car prev-dirs))))
		  (let ((old dir)
			(new (file-name-as-directory (file-truename dirfile counter prev-dirs))))
		    (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
		    (setq dir new))))
	    (if (equal ".." (file-name-nondirectory filename))
		(setq filename
		      (directory-file-name (file-name-directory (directory-file-name dir)))
		      done t)
	      (if (equal "." (file-name-nondirectory filename))
		  (setq filename (directory-file-name dir)
			done t)
		;; Put it back on the file name.
		(setq filename (concat dir (file-name-nondirectory filename)))
		;; Is the file name the name of a link?
		(setq target (file-symlink-p filename))
		(if target
		    ;; Yes => chase that link, then start all over
		    ;; since the link may point to a directory name that uses links.
		    ;; We can't safely use expand-file-name here
		    ;; since target might look like foo/../bar where foo
		    ;; is itself a link.  Instead, we handle . and .. above.
		    (setq filename
			  (if (file-name-absolute-p target)
			      target
			    (concat dir target))
			  done nil)
		  ;; No, we are done!
		  (setq done t))))))))
    filename))

(defun file-chase-links (filename &optional limit)
  "Chase links in FILENAME until a name that is not a link.
Unlike `file-truename', this does not check whether a parent
directory name is a symbolic link.
If the optional argument LIMIT is a number,
it means chase no more than that many links and then stop."
  (let (tem (newname filename)
	    (count 0))
    (while (and (or (null limit) (< count limit))
		(setq tem (file-symlink-p newname)))
      (save-match-data
	(if (and (null limit) (= count 100))
	    (error "Apparent cycle of symbolic links for %s" filename))
	;; In the context of a link, `//' doesn't mean what Emacs thinks.
	(while (string-match "//+" tem)
	  (setq tem (replace-match "/" nil nil tem)))
	;; Handle `..' by hand, since it needs to work in the
	;; target of any directory symlink.
	;; This code is not quite complete; it does not handle
	;; embedded .. in some cases such as ./../foo and foo/bar/../../../lose.
	(while (string-match "\\`\\.\\./" tem)
	  (setq tem (substring tem 3))
	  (setq newname (expand-file-name newname))
	  ;; Chase links in the default dir of the symlink.
	  (setq newname
		(file-chase-links
		 (directory-file-name (file-name-directory newname))))
	  ;; Now find the parent of that dir.
	  (setq newname (file-name-directory newname)))
	(setq newname (expand-file-name tem (file-name-directory newname)))
	(setq count (1+ count))))
    newname))

;; A handy function to display file sizes in human-readable form.
;; See http://en.wikipedia.org/wiki/Kibibyte for the reference.
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
		   1024.0
		 1000.0))
	(post-fixes
	 ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
	 (list "" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr post-fixes))
      (setq file-size (/ file-size power)
	    post-fixes (cdr post-fixes)))
    (format (if (> (mod file-size 1.0) 0.05)
		"%.1f%s%s"
	      "%.0f%s%s")
	    file-size
	    (if (and (eq flavor 'iec) (string= (car post-fixes) "k"))
		"K"
	      (car post-fixes))
	    (if (eq flavor 'iec) "iB" ""))))

(defun make-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
  (let ((umask (default-file-modes))
	file)
    (unwind-protect
	(progn
	  ;; Create temp files with strict access rights.  It's easy to
	  ;; loosen them later, whereas it's impossible to close the
	  ;; time-window of loose permissions otherwise.
	  (set-default-file-modes ?\700)
	  (while (condition-case ()
		     (progn
		       (setq file
			     (make-temp-name
                              (if (zerop (length prefix))
                                  (file-name-as-directory
                                   temporary-file-directory)
                                (expand-file-name prefix
                                                  temporary-file-directory))))
		       (if suffix
			   (setq file (concat file suffix)))
		       (if dir-flag
			   (make-directory file)
			 (write-region "" nil file nil 'silent nil 'excl))
		       nil)
		   (file-already-exists t))
	    ;; the file was somehow created by someone else between
	    ;; `make-temp-name' and `write-region', let's try again.
	    nil)
	  file)
      ;; Reset the umask.
      (set-default-file-modes umask))))

(defun recode-file-name (file coding new-coding &optional ok-if-already-exists)
  "Change the encoding of FILE's name from CODING to NEW-CODING.
The value is a new name of FILE.
Signals a `file-already-exists' error if a file of the new name
already exists unless optional fourth argument OK-IF-ALREADY-EXISTS
is non-nil.  A number as fourth arg means request confirmation if
the new name already exists.  This is what happens in interactive
use with M-x."
  (interactive
   (let ((default-coding (or file-name-coding-system
			     default-file-name-coding-system))
	 (filename (read-file-name "Recode filename: " nil nil t))
	 from-coding to-coding)
     (if (and default-coding
	      ;; We provide the default coding only when it seems that
	      ;; the filename is correctly decoded by the default
	      ;; coding.
	      (let ((charsets (find-charset-string filename)))
		(and (not (memq 'eight-bit-control charsets))
		     (not (memq 'eight-bit-graphic charsets)))))
	 (setq from-coding (read-coding-system
			    (format "Recode filename %s from (default %s): "
				    filename default-coding)
			    default-coding))
       (setq from-coding (read-coding-system
			  (format "Recode filename %s from: " filename))))

     ;; We provide the default coding only when a user is going to
     ;; change the encoding not from the default coding.
     (if (eq from-coding default-coding)
	 (setq to-coding (read-coding-system
			  (format "Recode filename %s from %s to: "
				  filename from-coding)))
       (setq to-coding (read-coding-system
			(format "Recode filename %s from %s to (default %s): "
				filename from-coding default-coding)
			default-coding)))
     (list filename from-coding to-coding)))

  (let* ((default-coding (or file-name-coding-system
			     default-file-name-coding-system))
	 ;; FILE should have been decoded by DEFAULT-CODING.
	 (encoded (encode-coding-string file default-coding))
	 (newname (decode-coding-string encoded coding))
	 (new-encoded (encode-coding-string newname new-coding))
	 ;; Suppress further encoding.
	 (file-name-coding-system nil)
	 (default-file-name-coding-system nil)
	 (locale-coding-system nil))
    (rename-file encoded new-encoded ok-if-already-exists)
    newname))

(defcustom confirm-nonexistent-file-or-buffer 'after-completion
  "Whether confirmation is requested before visiting a new file or buffer.
If nil, confirmation is not requested.
If the value is `after-completion', confirmation is only
 requested if the user called `minibuffer-complete' right before
 `minibuffer-complete-and-exit'.
Any other non-nil value means to request confirmation.

This affects commands like `switch-to-buffer' and `find-file'."
  :group 'find-file
  :version "23.1"
  :type '(choice (const :tag "After completion" after-completion)
		 (const :tag "Never" nil)
		 (other :tag "Always" t)))

(defun confirm-nonexistent-file-or-buffer ()
  "Whether to request confirmation before visiting a new file or buffer.
The variable `confirm-nonexistent-file-or-buffer' determines the
return value, which may be passed as the REQUIRE-MATCH arg to
`read-buffer' or `find-file-read-args'."
  (cond ((eq confirm-nonexistent-file-or-buffer 'after-completion)
	 'confirm-after-completion)
	(confirm-nonexistent-file-or-buffer
	 'confirm)
	(t nil)))

(defmacro minibuffer-with-setup-hook (fun &rest body)
  "Temporarily add FUN to `minibuffer-setup-hook' while executing BODY.
BODY should use the minibuffer at most once.
Recursive uses of the minibuffer are unaffected (FUN is not
called additional times).

This macro actually adds an auxiliary function that calls FUN,
rather than FUN itself, to `minibuffer-setup-hook'."
  (declare (indent 1) (debug t))
  (let ((hook (make-symbol "setup-hook")))
    `(let (,hook)
       (setq ,hook
	     (lambda ()
	       ;; Clear out this hook so it does not interfere
	       ;; with any recursive minibuffer usage.
	       (remove-hook 'minibuffer-setup-hook ,hook)
	       (funcall ,fun)))
       (unwind-protect
	   (progn
	     (add-hook 'minibuffer-setup-hook ,hook)
	     ,@body)
	 (remove-hook 'minibuffer-setup-hook ,hook)))))

(defun find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt nil default-directory mustmatch)
	t))

(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)Filename Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))

(defun find-file-other-window (filename &optional wildcards)
  "Edit file FILENAME, in another window.

Like \\[find-file] (which see), but creates a new window or reuses
an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (cons (switch-to-buffer-other-window (car value))
		(mapcar 'switch-to-buffer (cdr value))))
      (switch-to-buffer-other-window value))))

(defun find-file-other-frame (filename &optional wildcards)
  "Edit file FILENAME, in another frame.

Like \\[find-file] (which see), but creates a new frame or reuses
an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "Find file in other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (cons (switch-to-buffer-other-frame (car value))
		(mapcar 'switch-to-buffer (cdr value))))
      (switch-to-buffer-other-frame value))))

(defun find-file-existing (filename)
   "Edit the existing file FILENAME.
Like \\[find-file], but only allow a file that exists, and do not allow
file names with wildcards."
   (interactive (nbutlast (find-file-read-args "Find existing file: " t)))
   (if (and (not (called-interactively-p 'interactive))
	    (not (file-exists-p filename)))
       (error "%s does not exist" filename)
     (find-file filename)
     (current-buffer)))

(defun find-file-read-only (filename &optional wildcards)
  "Edit file FILENAME but don't allow changes.
Like \\[find-file], but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only: "
                        (confirm-nonexistent-file-or-buffer)))
  (unless (or (and wildcards find-file-wildcards
		   (not (string-match "\\`/:" filename))
		   (string-match "[[*?]" filename))
	      (file-exists-p filename))
    (error "%s does not exist" filename))
  (let ((value (find-file filename wildcards)))
    (mapc (lambda (b) (with-current-buffer b (toggle-read-only 1)))
	  (if (listp value) value (list value)))
    value))

(defun find-file-read-only-other-window (filename &optional wildcards)
  "Edit file FILENAME in another window but don't allow changes.
Like \\[find-file-other-window], but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (unless (or (and wildcards find-file-wildcards
		   (not (string-match "\\`/:" filename))
		   (string-match "[[*?]" filename))
	      (file-exists-p filename))
    (error "%s does not exist" filename))
  (let ((value (find-file-other-window filename wildcards)))
    (mapc (lambda (b) (with-current-buffer b (toggle-read-only 1)))
	  (if (listp value) value (list value)))
    value))

(defun find-file-read-only-other-frame (filename &optional wildcards)
  "Edit file FILENAME in another frame but don't allow changes.
Like \\[find-file-other-frame], but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive
   (find-file-read-args "Find file read-only other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (unless (or (and wildcards find-file-wildcards
		   (not (string-match "\\`/:" filename))
		   (string-match "[[*?]" filename))
	      (file-exists-p filename))
    (error "%s does not exist" filename))
  (let ((value (find-file-other-frame filename wildcards)))
    (mapc (lambda (b) (with-current-buffer b (toggle-read-only 1)))
	  (if (listp value) value (list value)))
    value))

(defun find-alternate-file-other-window (filename &optional wildcards)
  "Find file FILENAME as a replacement for the file in the next window.
This command does not select that window.

See \\[find-file] for the possible forms of the FILENAME argument.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and replace the file with multiple files."
  (interactive
   (save-selected-window
     (other-window 1)
     (let ((file buffer-file-name)
	   (file-name nil)
	   (file-dir nil))
       (and file
	    (setq file-name (file-name-nondirectory file)
		  file-dir (file-name-directory file)))
       (list (read-file-name
	      "Find alternate file: " file-dir nil
              (confirm-nonexistent-file-or-buffer) file-name)
	     t))))
  (if (one-window-p)
      (find-file-other-window filename wildcards)
    (save-selected-window
      (other-window 1)
      (find-alternate-file filename wildcards))))

(defvar kill-buffer-hook)  ; from buffer.c

(defun find-alternate-file (filename &optional wildcards)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want.

See \\[find-file] for the possible forms of the FILENAME argument.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and replace the file with multiple files.

If the current buffer is an indirect buffer, or the base buffer
for one or more indirect buffers, the other buffer(s) are not
killed."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name
	    "Find alternate file: " file-dir nil
            (confirm-nonexistent-file-or-buffer) file-name)
	   t)))
  (unless (run-hook-with-args-until-failure 'kill-buffer-query-functions)
    (error "Aborted"))
  (when (and (buffer-modified-p) buffer-file-name)
    (if (yes-or-no-p (format "Buffer %s is modified; save it first? "
			     (buffer-name)))
        (save-buffer)
      (unless (yes-or-no-p "Kill and replace the buffer without saving it? ")
        (error "Aborted"))))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(onum buffer-file-number)
	(odir dired-directory)
	(otrue buffer-file-truename)
	(oname (buffer-name)))
    ;; Run `kill-buffer-hook' here.  It needs to happen before
    ;; variables like `buffer-file-name' etc are set to nil below,
    ;; because some of the hooks that could be invoked
    ;; (e.g., `save-place-to-alist') depend on those variables.
    ;;
    ;; Note that `kill-buffer-hook' is not what queries whether to
    ;; save a modified buffer visiting a file.  Rather, `kill-buffer'
    ;; asks that itself.  Thus, there's no need to temporarily do
    ;; `(set-buffer-modified-p nil)' before running this hook.
    (run-hooks 'kill-buffer-hook)
    ;; Okay, now we can end-of-life the old buffer.
    (if (get-buffer " **lose**")
	(kill-buffer " **lose**"))
    (rename-buffer " **lose**")
    (unwind-protect
	(progn
	  (unlock-buffer)
	  ;; This prevents us from finding the same buffer
	  ;; if we specified the same file again.
	  (setq buffer-file-name nil)
	  (setq buffer-file-number nil)
	  (setq buffer-file-truename nil)
	  ;; Likewise for dired buffers.
	  (setq dired-directory nil)
	  (find-file filename wildcards))
      (when (eq obuf (current-buffer))
	;; This executes if find-file gets an error
	;; and does not really find anything.
	;; We put things back as they were.
	;; If find-file actually finds something, we kill obuf below.
	(setq buffer-file-name ofile)
	(setq buffer-file-number onum)
	(setq buffer-file-truename otrue)
	(setq dired-directory odir)
	(lock-buffer)
	(rename-buffer oname)))
    (unless (eq (current-buffer) obuf)
      (with-current-buffer obuf
	;; We already ran these; don't run them again.
	(let (kill-buffer-query-functions kill-buffer-hook)
	  (kill-buffer obuf))))))

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name.
Spaces at the start of FILENAME (sans directory) are removed."
  (let ((lastname (file-name-nondirectory filename)))
    (if (string= lastname "")
	(setq lastname filename))
    (save-match-data
      (string-match "^ *\\(.*\\)" lastname)
      (generate-new-buffer (match-string 1 lastname)))))

(defun generate-new-buffer (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'."
  (get-buffer-create (generate-new-buffer-name name)))

(defcustom automount-dir-prefix (purecopy "^/tmp_mnt/")
  "Regexp to match the automounter prefix in a directory name."
  :group 'files
  :type 'regexp)

(defvar abbreviated-home-dir nil
  "The user's homedir abbreviated according to `directory-abbrev-alist'.")

(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory (unless the
home directory is a root directory) and removes automounter prefixes
\(see the variable `automount-dir-prefix')."
  ;; Get rid of the prefixes added by the automounter.
  (save-match-data
    (if (and automount-dir-prefix
	     (string-match automount-dir-prefix filename)
	     (file-exists-p (file-name-directory
			     (substring filename (1- (match-end 0))))))
	(setq filename (substring filename (1- (match-end 0)))))
    ;; Avoid treating /home/foo as /home/Foo during `~' substitution.
    ;; To fix this right, we need a `file-name-case-sensitive-p'
    ;; function, but we don't have that yet, so just guess.
    (let ((case-fold-search
	   (memq system-type '(ms-dos windows-nt darwin cygwin))))
      ;; If any elt of directory-abbrev-alist matches this name,
      ;; abbreviate accordingly.
      (dolist (dir-abbrev directory-abbrev-alist)
	(if (string-match (car dir-abbrev) filename)
	    (setq filename
		  (concat (cdr dir-abbrev)
			  (substring filename (match-end 0))))))
      ;; Compute and save the abbreviated homedir name.
      ;; We defer computing this until the first time it's needed, to
      ;; give time for directory-abbrev-alist to be set properly.
      ;; We include a slash at the end, to avoid spurious matches
      ;; such as `/usr/foobar' when the home dir is `/usr/foo'.
      (or abbreviated-home-dir
	  (setq abbreviated-home-dir
		(let ((abbreviated-home-dir "$foo"))
		  (concat "\\`" (abbreviate-file-name (expand-file-name "~"))
			  "\\(/\\|\\'\\)"))))

      ;; If FILENAME starts with the abbreviated homedir,
      ;; make it start with `~' instead.
      (if (and (string-match abbreviated-home-dir filename)
	       ;; If the home dir is just /, don't change it.
	       (not (and (= (match-end 0) 1)
			 (= (aref filename 0) ?/)))
	       ;; MS-DOS root directories can come with a drive letter;
	       ;; Novell Netware allows drive letters beyond `Z:'.
	       (not (and (memq system-type '(ms-dos windows-nt cygwin))
			 (save-match-data
			   (string-match "^[a-zA-`]:/$" filename)))))
	  (setq filename
		(concat "~"
			(match-string 1 filename)
			(substring filename (match-end 0)))))
      filename)))

(defun find-buffer-visiting (filename &optional predicate)
  "Return the buffer visiting file FILENAME (a string).
This is like `get-file-buffer', except that it checks for any buffer
visiting the same file, possibly under a different name.
If PREDICATE is non-nil, only buffers satisfying it are eligible,
and others are ignored.
If there is no such live buffer, return nil."
  (let ((predicate (or predicate #'identity))
        (truename (abbreviate-file-name (file-truename filename))))
    (or (let ((buf (get-file-buffer filename)))
          (when (and buf (funcall predicate buf)) buf))
        (let ((list (buffer-list)) found)
          (while (and (not found) list)
            (with-current-buffer (car list)
              (if (and buffer-file-name
                       (string= buffer-file-truename truename)
                       (funcall predicate (current-buffer)))
                  (setq found (car list))))
            (setq list (cdr list)))
          found)
        (let* ((attributes (file-attributes truename))
               (number (nthcdr 10 attributes))
               (list (buffer-list)) found)
          (and buffer-file-numbers-unique
               (car-safe number)       ;Make sure the inode is not just nil.
               (while (and (not found) list)
                 (with-current-buffer (car list)
                   (if (and buffer-file-name
                            (equal buffer-file-number number)
                            ;; Verify this buffer's file number
                            ;; still belongs to its file.
                            (file-exists-p buffer-file-name)
                            (equal (file-attributes buffer-file-truename)
                                   attributes)
                            (funcall predicate (current-buffer)))
                       (setq found (car list))))
                 (setq list (cdr list))))
          found))))

(defcustom find-file-wildcards t
  "Non-nil means file-visiting commands should handle wildcards.
For example, if you specify `*.c', that would visit all the files
whose names match the pattern."
  :group 'files
  :version "20.4"
  :type 'boolean)

(defcustom find-file-suppress-same-file-warnings nil
  "Non-nil means suppress warning messages for symlinked files.
When nil, Emacs prints a warning when visiting a file that is already
visited, but with a different name.  Setting this option to t
suppresses this warning."
  :group 'files
  :version "21.1"
  :type 'boolean)

(defcustom large-file-warning-threshold 10000000
  "Maximum size of file above which a confirmation is requested.
When nil, never request confirmation."
  :group 'files
  :group 'find-file
  :version "22.1"
  :type '(choice integer (const :tag "Never request confirmation" nil)))

(defun abort-if-file-too-large (size op-type filename)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message to user)."
  (when (and large-file-warning-threshold size
	     (> size large-file-warning-threshold)
	     (not (y-or-n-p (format "File %s is large (%dMB), really %s? "
				    (file-name-nondirectory filename)
				    (/ size 1048576) op-type))))
    (error "Aborted")))

(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (or (and find-file-run-dired
	       (run-hook-with-args-until-success
		'find-directory-functions
		(if find-file-visit-truename
		    (abbreviate-file-name (file-truename filename))
		  filename)))
	  (error "%s is a directory" filename))
    (if (and wildcards
	     find-file-wildcards
	     (not (string-match "\\`/:" filename))
	     (string-match "[[*?]" filename))
	(let ((files (condition-case nil
			 (file-expand-wildcards filename t)
		       (error (list filename))))
	      (find-file-wildcards nil))
	  (if (null files)
	      (find-file-noselect filename)
	    (mapcar #'find-file-noselect files)))
      (let* ((buf (get-file-buffer filename))
	     (truename (abbreviate-file-name (file-truename filename)))
	     (attributes (file-attributes truename))
	     (number (nthcdr 10 attributes))
	     ;; Find any buffer for a file which has same truename.
	     (other (and (not buf) (find-buffer-visiting filename))))
	;; Let user know if there is a buffer with the same truename.
	(if other
	    (progn
	      (or nowarn
		  find-file-suppress-same-file-warnings
		  (string-equal filename (buffer-file-name other))
		  (message "%s and %s are the same file"
			   filename (buffer-file-name other)))
	      ;; Optionally also find that buffer.
	      (if (or find-file-existing-other-name find-file-visit-truename)
		  (setq buf other))))
	;; Check to see if the file looks uncommonly large.
	(when (not (or buf nowarn))
	  (abort-if-file-too-large (nth 7 attributes) "open" filename))
	(if buf
	    ;; We are using an existing buffer.
	    (let (nonexistent)
	      (or nowarn
		  (verify-visited-file-modtime buf)
		  (cond ((not (file-exists-p filename))
			 (setq nonexistent t)
			 (message "File %s no longer exists!" filename))
			;; Certain files should be reverted automatically
			;; if they have changed on disk and not in the buffer.
			((and (not (buffer-modified-p buf))
			      (let ((tail revert-without-query)
				    (found nil))
				(while tail
				  (if (string-match (car tail) filename)
				      (setq found t))
				  (setq tail (cdr tail)))
				found))
			 (with-current-buffer buf
			   (message "Reverting file %s..." filename)
			   (revert-buffer t t)
			   (message "Reverting file %s...done" filename)))
			((yes-or-no-p
			  (if (string= (file-name-nondirectory filename)
				       (buffer-name buf))
			      (format
			       (if (buffer-modified-p buf)
				   "File %s changed on disk.  Discard your edits? "
				 "File %s changed on disk.  Reread from disk? ")
			       (file-name-nondirectory filename))
			    (format
			     (if (buffer-modified-p buf)
				 "File %s changed on disk.  Discard your edits in %s? "
			       "File %s changed on disk.  Reread from disk into %s? ")
			     (file-name-nondirectory filename)
			     (buffer-name buf))))
			 (with-current-buffer buf
			   (revert-buffer t t)))))
	      (with-current-buffer buf

		;; Check if a formerly read-only file has become
		;; writable and vice versa, but if the buffer agrees
		;; with the new state of the file, that is ok too.
		(let ((read-only (not (file-writable-p buffer-file-name))))
		  (unless (or nonexistent
			      (eq read-only buffer-file-read-only)
			      (eq read-only buffer-read-only))
		    (when (or nowarn
			      (let ((question
				     (format "File %s is %s on disk.  Change buffer mode? "
					     buffer-file-name
					     (if read-only "read-only" "writable"))))
				(y-or-n-p question)))
		      (setq buffer-read-only read-only)))
		  (setq buffer-file-read-only read-only))

		(when (and (not (eq (not (null rawfile))
				    (not (null find-file-literally))))
			   (not nonexistent)
			   ;; It is confusing to ask whether to visit
			   ;; non-literally if they have the file in
			   ;; hexl-mode or image-mode.
			   (not (memq major-mode '(hexl-mode image-mode))))
		  (if (buffer-modified-p)
		      (if (y-or-n-p
			   (format
			    (if rawfile
				"The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it literally instead? "
				"The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it normally instead? ")
			    (file-name-nondirectory filename)))
			  (progn
			    (save-buffer)
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number))
			(if (y-or-n-p
			     (format
			      (if rawfile
				  "\
Do you want to discard your changes, and visit the file literally now? "
				"\
Do you want to discard your changes, and visit the file normally now? ")))
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number)
			  (error (if rawfile "File already visited non-literally"
				   "File already visited literally"))))
		    (if (y-or-n-p
			 (format
			  (if rawfile
			      "The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can only visit a file in one way at a time.

Do you want to revisit the file literally now? "
			    "The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can only visit a file in one way at a time.

Do you want to revisit the file normally now? ")
			  (file-name-nondirectory filename)))
			(find-file-noselect-1 buf filename nowarn
					      rawfile truename number)
		      (error (if rawfile "File already visited non-literally"
			       "File already visited literally"))))))
	      ;; Return the buffer we are using.
	      buf)
	  ;; Create a new buffer.
	  (setq buf (create-file-buffer filename))
	  ;; find-file-noselect-1 may use a different buffer.
	  (find-file-noselect-1 buf filename nowarn
				rawfile truename number))))))

(defun find-file-noselect-1 (buf filename nowarn rawfile truename number)
  (let (error)
    (with-current-buffer buf
      (kill-local-variable 'find-file-literally)
      ;; Needed in case we are re-visiting the file with a different
      ;; text representation.
      (kill-local-variable 'buffer-file-coding-system)
      (kill-local-variable 'cursor-type)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (and (default-value 'enable-multibyte-characters)
	   (not rawfile)
	   (set-buffer-multibyte t))
      (if rawfile
	  (condition-case ()
	      (let ((inhibit-read-only t))
		(insert-file-contents-literally filename t))
	    (file-error
	     (when (and (file-exists-p filename)
			(not (file-readable-p filename)))
	       (kill-buffer buf)
	       (signal 'file-error (list "File is not readable"
					 filename)))
	     ;; Unconditionally set error
	     (setq error t)))
	(condition-case ()
	    (let ((inhibit-read-only t))
	      (insert-file-contents filename t))
	  (file-error
	   (when (and (file-exists-p filename)
		      (not (file-readable-p filename)))
	     (kill-buffer buf)
	     (signal 'file-error (list "File is not readable"
				       filename)))
	   ;; Run find-file-not-found-functions until one returns non-nil.
	   (or (run-hook-with-args-until-success 'find-file-not-found-functions)
	       ;; If they fail too, set error.
	       (setq error t)))))
      ;; Record the file's truename, and maybe use that as visited name.
      (if (equal filename buffer-file-name)
	  (setq buffer-file-truename truename)
	(setq buffer-file-truename
	      (abbreviate-file-name (file-truename buffer-file-name))))
      (setq buffer-file-number number)
      (if find-file-visit-truename
	  (setq buffer-file-name (expand-file-name buffer-file-truename)))
      ;; Set buffer's default directory to that of the file.
      (setq default-directory (file-name-directory buffer-file-name))
      ;; Turn off backup files for certain file names.  Since
      ;; this is a permanent local, the major mode won't eliminate it.
      (and backup-enable-predicate
	   (not (funcall backup-enable-predicate buffer-file-name))
	   (progn
	     (make-local-variable 'backup-inhibited)
	     (setq backup-inhibited t)))
      (if rawfile
	  (progn
	    (set-buffer-multibyte nil)
	    (setq buffer-file-coding-system 'no-conversion)
	    (set-buffer-major-mode buf)
	    (make-local-variable 'find-file-literally)
	    (setq find-file-literally t))
	(after-find-file error (not nowarn)))
      (current-buffer))))

(defun insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, `find-file-hook', automatic uncompression, etc.

This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'no-conversion)
	(coding-system-for-write 'no-conversion)
	(find-buffer-file-type-function
         (if (fboundp 'find-buffer-file-type)
             (symbol-function 'find-buffer-file-type)
           nil))
        (inhibit-file-name-handlers
         (append '(jka-compr-handler image-file-handler epa-file-handler)
                 inhibit-file-name-handlers))
        (inhibit-file-name-operation 'insert-file-contents))
    (unwind-protect
         (progn
           (fset 'find-buffer-file-type (lambda (_filename) t))
           (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))

(defun insert-file-1 (filename insert-func)
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "file is a directory"
                                filename)))
  ;; Check whether the file is uncommonly large
  (abort-if-file-too-large (nth 7 (file-attributes filename)) "insert" filename)
  (let* ((buffer (find-buffer-visiting (abbreviate-file-name (file-truename filename))
                                       #'buffer-modified-p))
         (tem (funcall insert-func filename)))
    (push-mark (+ (point) (car (cdr tem))))
    (when buffer
      (message "File %s already visited and modified in buffer %s"
               filename (buffer-name buffer)))))

(defun insert-file-literally (filename)
  "Insert contents of file FILENAME into buffer after point with no conversion.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents-literally' instead.
\(Its calling sequence is different; see its documentation)."
  (interactive "*fInsert file literally: ")
  (insert-file-1 filename #'insert-file-contents-literally))

(defvar find-file-literally nil
  "Non-nil if this buffer was made by `find-file-literally' or equivalent.
This has the `permanent-local' property, which takes effect if you
make the variable buffer-local.")
(put 'find-file-literally 'permanent-local t)

(defun find-file-literally (filename)
  "Visit file FILENAME with no conversion of any kind.
Format conversion and character code conversion are both disabled,
and multibyte characters are disabled in the resulting buffer.
The major mode used is Fundamental mode regardless of the file name,
and local variable specifications in the file are ignored.
Automatic uncompression and adding a newline at the end of the
file due to `require-final-newline' is also disabled.

You cannot absolutely rely on this function to result in
visiting the file literally.  If Emacs already has a buffer
which is visiting the file, you get the existing buffer,
regardless of whether it was created literally or not.

In a Lisp program, if you want to be sure of accessing a file's
contents literally, you should create a temporary buffer and then read
the file contents into it using `insert-file-contents-literally'."
  (interactive
   (list (read-file-name
  	  "Find file literally: " nil default-directory
  	  (confirm-nonexistent-file-or-buffer))))
  (switch-to-buffer (find-file-noselect filename nil t)))

(defun after-find-file (&optional error warn noauto
				  _after-find-file-from-revert-buffer
				  nomodes)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables.
Optional args ERROR, WARN, and NOAUTO: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
NOAUTO means don't mess with auto-save mode.
Fourth arg AFTER-FIND-FILE-FROM-REVERT-BUFFER is ignored
\(see `revert-buffer-in-progress-p' for similar functionality).
Fifth arg NOMODES non-nil means don't alter the file's modes.
Finishes by calling the functions in `find-file-hook'
unless NOMODES is non-nil."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
	   (msg
	    (cond
	     ((not warn) nil)
	     ((and error (file-attributes buffer-file-name))
	      (setq buffer-read-only t)
	      (if (and (file-symlink-p buffer-file-name)
		       (not (file-exists-p
			     (file-chase-links buffer-file-name))))
		  "Symbolic link that points to nonexistent file"
		"File exists, but cannot be read"))
	     ((not buffer-read-only)
	      (if (and warn
		       ;; No need to warn if buffer is auto-saved
		       ;; under the name of the visited file.
		       (not (and buffer-file-name
				 auto-save-visited-file-name))
		       (file-newer-than-file-p (or buffer-auto-save-file-name
						   (make-auto-save-file-name))
					       buffer-file-name))
		  (format "%s has auto save data; consider M-x recover-this-file"
			  (file-name-nondirectory buffer-file-name))
		(setq not-serious t)
		(if error "(New file)" nil)))
	     ((not error)
	      (setq not-serious t)
	      "Note: file is write protected")
	     ((file-attributes (directory-file-name default-directory))
	      "File not found and directory write-protected")
	     ((file-exists-p (file-name-directory buffer-file-name))
	      (setq buffer-read-only nil))
	     (t
	      (setq buffer-read-only nil)
	      "Use M-x make-directory RET RET to create the directory and its parents"))))
      (when msg
	(message "%s" msg)
	(or not-serious (sit-for 1 t))))
    (when (and auto-save-default (not noauto))
      (auto-save-mode 1)))
  ;; Make people do a little extra work (C-x C-q)
  ;; before altering a backup file.
  (when (backup-file-name-p buffer-file-name)
    (setq buffer-read-only t))
  ;; When a file is marked read-only,
  ;; make the buffer read-only even if root is looking at it.
  (when (and (file-modes (buffer-file-name))
	     (zerop (logand (file-modes (buffer-file-name)) #o222)))
    (setq buffer-read-only t))
  (unless nomodes
    (when (and view-read-only view-mode)
      (view-mode-disable))
    (normal-mode t)
    ;; If requested, add a newline at the end of the file.
    (and (memq require-final-newline '(visit visit-save))
	 (> (point-max) (point-min))
	 (/= (char-after (1- (point-max))) ?\n)
	 (not (and (eq selective-display t)
		   (= (char-after (1- (point-max))) ?\r)))
	 (save-excursion
	   (goto-char (point-max))
	   (insert "\n")))
    (when (and buffer-read-only
	       view-read-only
	       (not (eq (get major-mode 'mode-class) 'special)))
      (view-mode-enter))
    (run-hooks 'find-file-hook)))

(defmacro report-errors (format &rest body)
  "Eval BODY and turn any error into a FORMAT message.
FORMAT can have a %s escape which will be replaced with the actual error.
If `debug-on-error' is set, errors are not caught, so that you can
debug them.
Avoid using a large BODY since it is duplicated."
  (declare (debug t) (indent 1))
  `(if debug-on-error
       (progn . ,body)
     (condition-case err
	 (progn . ,body)
       (error (message ,format (prin1-to-string err))))))

(defun normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
we may set up the file-specified mode and local variables,
depending on the value of `enable-local-variables'.
In addition, if `local-enable-local-variables' is nil, we do
not set local variables (though we do notice a mode specified with -*-.)

`enable-local-variables' is ignored if you run `normal-mode' interactively,
or from Lisp without specifying the optional argument FIND-FILE;
in that case, this function acts as if `enable-local-variables' were t."
  (interactive)
  (funcall (or (default-value 'major-mode) 'fundamental-mode))
  (let ((enable-local-variables (or (not find-file) enable-local-variables)))
    ;; FIXME this is less efficient than it could be, since both
    ;; s-a-m and h-l-v may parse the same regions, looking for "mode:".
    (report-errors "File mode specification error: %s"
      (set-auto-mode))
    (report-errors "File local-variables error: %s"
      (hack-local-variables)))
  ;; Turn font lock off and on, to make sure it takes account of
  ;; whatever file local variables are relevant to it.
  (when (and font-lock-mode
             ;; Font-lock-mode (now in font-core.el) can be ON when
             ;; font-lock.el still hasn't been loaded.
             (boundp 'font-lock-keywords)
             (eq (car font-lock-keywords) t))
    (setq font-lock-keywords (cadr font-lock-keywords))
    (font-lock-mode 1))

  (if (fboundp 'ucs-set-table-for-input) ; don't lose when building
      (ucs-set-table-for-input)))

(defcustom auto-mode-case-fold t
  "Non-nil means to try second pass through `auto-mode-alist'.
This means that if the first case-sensitive search through the alist fails
to find a matching major mode, a second case-insensitive search is made.
On systems with case-insensitive file names, this variable is ignored,
since only a single case-insensitive search through the alist is made."
  :group 'files
  :version "22.1"
  :type 'boolean)

(defvar auto-mode-alist
  ;; Note: The entries for the modes defined in cc-mode.el (c-mode,
  ;; c++-mode, java-mode and more) are added through autoload
  ;; directives in that file.  That way is discouraged since it
  ;; spreads out the definition of the initial value.
  (mapcar
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   `(;; do this first, so that .html.pl is Polish html, not Perl
     ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . html-mode)
     ("\\.svgz?\\'" . image-mode)
     ("\\.svgz?\\'" . xml-mode)
     ("\\.x[bp]m\\'" . image-mode)
     ("\\.x[bp]m\\'" . c-mode)
     ("\\.p[bpgn]m\\'" . image-mode)
     ("\\.tiff?\\'" . image-mode)
     ("\\.gif\\'" . image-mode)
     ("\\.png\\'" . image-mode)
     ("\\.jpe?g\\'" . image-mode)
     ("\\.te?xt\\'" . text-mode)
     ("\\.[tT]e[xX]\\'" . tex-mode)
     ("\\.ins\\'" . tex-mode)		;Installation files for TeX packages.
     ("\\.ltx\\'" . latex-mode)
     ("\\.dtx\\'" . doctex-mode)
     ("\\.org\\'" . org-mode)
     ("\\.el\\'" . emacs-lisp-mode)
     ("Project\\.ede\\'" . emacs-lisp-mode)
     ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
     ("\\.l\\'" . lisp-mode)
     ("\\.li?sp\\'" . lisp-mode)
     ("\\.[fF]\\'" . fortran-mode)
     ("\\.for\\'" . fortran-mode)
     ("\\.p\\'" . pascal-mode)
     ("\\.pas\\'" . pascal-mode)
     ("\\.\\(dpr\\|DPR\\)\\'" . delphi-mode)
     ("\\.ad[abs]\\'" . ada-mode)
     ("\\.ad[bs].dg\\'" . ada-mode)
     ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode)
     ("Imakefile\\'" . makefile-imake-mode)
     ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode) ; Put this before .mk
     ("\\.makepp\\'" . makefile-makepp-mode)
     ,@(if (memq system-type '(berkeley-unix darwin))
	   '(("\\.mk\\'" . makefile-bsdmake-mode)
	     ("GNUmakefile\\'" . makefile-gmake-mode)
	     ("[Mm]akefile\\'" . makefile-bsdmake-mode))
	 '(("\\.mk\\'" . makefile-gmake-mode)	; Might be any make, give Gnu the host advantage
	   ("[Mm]akefile\\'" . makefile-gmake-mode)))
     ("\\.am\\'" . makefile-automake-mode)
     ;; Less common extensions come here
     ;; so more common ones above are found faster.
     ("\\.texinfo\\'" . texinfo-mode)
     ("\\.te?xi\\'" . texinfo-mode)
     ("\\.[sS]\\'" . asm-mode)
     ("\\.asm\\'" . asm-mode)
     ("\\.css\\'" . css-mode)
     ("\\.mixal\\'" . mixal-mode)
     ("\\.gcov\\'" . compilation-mode)
     ;; Besides .gdbinit, gdb documents other names to be usable for init
     ;; files, cross-debuggers can use something like
     ;; .PROCESSORNAME-gdbinit so that the host and target gdbinit files
     ;; don't interfere with each other.
     ("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode)
     ("[cC]hange\\.?[lL]og?\\'" . change-log-mode)
     ("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode)
     ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
     ("\\.scm\\.[0-9]*\\'" . scheme-mode)
     ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
     ("\\.bash\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\(bash_logout\\|shrc\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
     ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
     ("\\.m?spec\\'" . sh-mode)
     ("\\.m[mes]\\'" . nroff-mode)
     ("\\.man\\'" . nroff-mode)
     ("\\.sty\\'" . latex-mode)
     ("\\.cl[so]\\'" . latex-mode)		;LaTeX 2e class option
     ("\\.bbl\\'" . latex-mode)
     ("\\.bib\\'" . bibtex-mode)
     ("\\.bst\\'" . bibtex-style-mode)
     ("\\.sql\\'" . sql-mode)
     ("\\.m[4c]\\'" . m4-mode)
     ("\\.mf\\'" . metafont-mode)
     ("\\.mp\\'" . metapost-mode)
     ("\\.vhdl?\\'" . vhdl-mode)
     ("\\.article\\'" . text-mode)
     ("\\.letter\\'" . text-mode)
     ("\\.i?tcl\\'" . tcl-mode)
     ("\\.exp\\'" . tcl-mode)
     ("\\.itk\\'" . tcl-mode)
     ("\\.icn\\'" . icon-mode)
     ("\\.sim\\'" . simula-mode)
     ("\\.mss\\'" . scribe-mode)
     ;; The Fortran standard does not say anything about file extensions.
     ;; .f90 was widely used for F90, now we seem to be trapped into
     ;; using a different extension for each language revision.
     ;; Anyway, the following extensions are supported by gfortran.
     ("\\.f9[05]\\'" . f90-mode)
     ("\\.f0[38]\\'" . f90-mode)
     ("\\.indent\\.pro\\'" . fundamental-mode) ; to avoid idlwave-mode
     ("\\.\\(pro\\|PRO\\)\\'" . idlwave-mode)
     ("\\.srt\\'" . srecode-template-mode)
     ("\\.prolog\\'" . prolog-mode)
     ("\\.tar\\'" . tar-mode)
     ;; The list of archive file extensions should be in sync with
     ;; `auto-coding-alist' with `no-conversion' coding system.
     ("\\.\\(\
arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|7z\\|\
ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|7Z\\)\\'" . archive-mode)
     ("\\.\\(sx[dmicw]\\|od[fgpst]\\|oxt\\)\\'" . archive-mode) ;OpenOffice.org
     ("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode) ; Debian/Opkg packages.
     ;; Mailer puts message to be edited in
     ;; /tmp/Re.... or Message
     ("\\`/tmp/Re" . text-mode)
     ("/Message[0-9]*\\'" . text-mode)
     ;; some news reader is reported to use this
     ("\\`/tmp/fol/" . text-mode)
     ("\\.oak\\'" . scheme-mode)
     ("\\.sgml?\\'" . sgml-mode)
     ("\\.x[ms]l\\'" . xml-mode)
     ("\\.dbk\\'" . xml-mode)
     ("\\.dtd\\'" . sgml-mode)
     ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
     ("\\.js\\'" . js-mode)		; javascript-mode would be better
     ("\\.json\\'" . js-mode)
     ("\\.[ds]?vh?\\'" . verilog-mode)
     ;; .emacs or .gnus or .viper following a directory delimiter in
     ;; Unix, MSDOG or VMS syntax.
     ("[]>:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode)
     ("\\`\\..*emacs\\'" . emacs-lisp-mode)
     ;; _emacs following a directory delimiter
     ;; in MsDos syntax
     ("[:/]_emacs\\'" . emacs-lisp-mode)
     ("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
     ("\\.ml\\'" . lisp-mode)
     ;; Linux-2.6.9 uses some different suffix for linker scripts:
     ;; "ld", "lds", "lds.S", "lds.in", "ld.script", and "ld.script.balo".
     ;; eCos uses "ld" and "ldi".  Netbsd uses "ldscript.*".
     ("\\.ld[si]?\\'" . ld-script-mode)
     ("ld\\.?script\\'" . ld-script-mode)
     ;; .xs is also used for ld scripts, but seems to be more commonly
     ;; associated with Perl .xs files (C with Perl bindings).  (Bug#7071)
     ("\\.xs\\'" . c-mode)
     ;; Explained in binutils ld/genscripts.sh.  Eg:
     ;; A .x script file is the default script.
     ;; A .xr script is for linking without relocation (-r flag).  Etc.
     ("\\.x[abdsru]?[cnw]?\\'" . ld-script-mode)
     ("\\.zone\\'" . dns-mode)
     ("\\.soa\\'" . dns-mode)
     ;; Common Lisp ASDF package system.
     ("\\.asd\\'" . lisp-mode)
     ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
     ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
     ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
     ("\\.\\(dif\\|pat\\)\\'" . diff-mode) ; for MSDOG
     ("\\.[eE]?[pP][sS]\\'" . ps-mode)
     ("\\.\\(?:PDF\\|DVI\\|OD[FGPST]\\|DOCX?\\|XLSX?\\|PPTX?\\|pdf\\|dvi\\|od[fgpst]\\|docx?\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe)
     ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
     ("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode)
     ("BROWSE\\'" . ebrowse-tree-mode)
     ("\\.ebrowse\\'" . ebrowse-tree-mode)
     ("#\\*mail\\*" . mail-mode)
     ("\\.g\\'" . antlr-mode)
     ("\\.mod\\'" . m2-mode)
     ("\\.ses\\'" . ses-mode)
     ("\\.docbook\\'" . sgml-mode)
     ("\\.com\\'" . dcl-mode)
     ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)
     ;; Windows candidates may be opened case sensitively on Unix
     ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode)
     ("\\.\\(?:desktop\\|la\\)\\'" . conf-unix-mode)
     ("\\.ppd\\'" . conf-ppd-mode)
     ("java.+\\.conf\\'" . conf-javaprop-mode)
     ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
     ("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode)
     ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
     ;; ChangeLog.old etc.  Other change-log-mode entries are above;
     ;; this has lower priority to avoid matching changelog.sgml etc.
     ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode)
     ;; either user's dot-files or under /etc or some such
     ("/\\.?\\(?:gnokiirc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
     ;; alas not all ~/.*rc files are like this
     ("/\\.\\(?:enigma\\|gltron\\|gtk\\|hxplayer\\|net\\|neverball\\|qt/.+\\|realplayer\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)
     ("/\\.\\(?:gdbtkinit\\|grip\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode)
     ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
     ("/X11.+app-defaults/" . conf-xdefaults-mode)
     ("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
     ;; this contains everything twice, with space and with colon :-(
     ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)
     ;; Get rid of any trailing .n.m and try again.
     ;; This is for files saved by cvs-merge that look like .#<file>.<rev>
     ;; or .#<file>.<rev>-<rev> or VC's <file>.~<rev>~.
     ;; Using mode nil rather than `ignore' would let the search continue
     ;; through this list (with the shortened name) rather than start over.
     ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t)
     ("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t)
     ;; This should come after "in" stripping (e.g. config.h.in).
     ;; *.cf, *.cfg, *.conf, *.config[.local|.de_DE.UTF8|...], */config
     ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe)
     ;; The following should come after the ChangeLog pattern
     ;; for the sake of ChangeLog.1, etc.
     ;; and after the .scm.[0-9] and CVS' <file>.<rev> patterns too.
     ("\\.[1-9]\\'" . nroff-mode)))
  "Alist of filename patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (REGEXP FUNCTION NON-NIL).
\(NON-NIL stands for anything that is not nil; the value does not matter.)
Visiting a file whose name matches REGEXP specifies FUNCTION as the
mode function to use.  FUNCTION will be called, unless it is nil.

If the element has the form (REGEXP FUNCTION NON-NIL), then after
calling FUNCTION (if it's not nil), we delete the suffix that matched
REGEXP and search the list again for another match.

The extensions whose FUNCTION is `archive-mode' should also
appear in `auto-coding-alist' with `no-conversion' coding system.

See also `interpreter-mode-alist', which detects executable script modes
based on the interpreters they specify to run,
and `magic-mode-alist', which determines modes based on file contents.")
(put 'auto-mode-alist 'risky-local-variable t)

(defun conf-mode-maybe ()
  "Select Conf mode or XML mode according to start of file."
  (if (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (looking-at "<\\?xml \\|<!-- \\|<!DOCTYPE ")))
      (xml-mode)
    (conf-mode)))

(defvar interpreter-mode-alist
  ;; Note: The entries for the modes defined in cc-mode.el (awk-mode
  ;; and pike-mode) are added through autoload directives in that
  ;; file.  That way is discouraged since it spreads out the
  ;; definition of the initial value.
  (mapcar
   (lambda (l)
     (cons (purecopy (car l)) (cdr l)))
   '(("perl" . perl-mode)
     ("perl5" . perl-mode)
     ("miniperl" . perl-mode)
     ("wish" . tcl-mode)
     ("wishx" . tcl-mode)
     ("tcl" . tcl-mode)
     ("tclsh" . tcl-mode)
     ("scm" . scheme-mode)
     ("ash" . sh-mode)
     ("bash" . sh-mode)
     ("bash2" . sh-mode)
     ("csh" . sh-mode)
     ("dtksh" . sh-mode)
     ("es" . sh-mode)
     ("itcsh" . sh-mode)
     ("jsh" . sh-mode)
     ("ksh" . sh-mode)
     ("oash" . sh-mode)
     ("pdksh" . sh-mode)
     ("rbash" . sh-mode)
     ("rc" . sh-mode)
     ("rpm" . sh-mode)
     ("sh" . sh-mode)
     ("sh5" . sh-mode)
     ("tcsh" . sh-mode)
     ("wksh" . sh-mode)
     ("wsh" . sh-mode)
     ("zsh" . sh-mode)
     ("tail" . text-mode)
     ("more" . text-mode)
     ("less" . text-mode)
     ("pg" . text-mode)
     ("make" . makefile-gmake-mode)		; Debian uses this
     ("guile" . scheme-mode)
     ("clisp" . lisp-mode)
     ("emacs" . emacs-lisp-mode)))
  "Alist mapping interpreter names to major modes.
This is used for files whose first lines match `auto-mode-interpreter-regexp'.
Each element looks like (INTERPRETER . MODE).
If INTERPRETER matches the name of the interpreter specified in the first line
of a script, mode MODE is enabled.

See also `auto-mode-alist'.")

(define-obsolete-variable-alias 'inhibit-first-line-modes-regexps
  'inhibit-file-local-variables-regexps "24.1")

;; TODO really this should be a list of modes (eg tar-mode), not regexps,
;; because we are duplicating info from auto-mode-alist.
;; TODO many elements of this list are also in auto-coding-alist.
(defvar inhibit-local-variables-regexps
  (mapcar 'purecopy '("\\.tar\\'" "\\.t[bg]z\\'"
		      "\\.arc\\'" "\\.zip\\'" "\\.lzh\\'" "\\.lha\\'"
		      "\\.zoo\\'" "\\.[jew]ar\\'" "\\.xpi\\'" "\\.rar\\'"
		      "\\.7z\\'"
		      "\\.sx[dmicw]\\'" "\\.odt\\'"
		      "\\.tiff?\\'" "\\.gif\\'" "\\.png\\'" "\\.jpe?g\\'"))
  "List of regexps matching file names in which to ignore local variables.
This includes `-*-' lines as well as trailing \"Local Variables\" sections.
Files matching this list are typically binary file formats.
They may happen to contain sequences that look like local variable
specifications, but are not really, or they may be containers for
member files with their own local variable sections, which are
not appropriate for the containing file.
See also `inhibit-local-variables-suffixes'.")

(define-obsolete-variable-alias 'inhibit-first-line-modes-suffixes
  'inhibit-local-variables-suffixes "24.1")

(defvar inhibit-local-variables-suffixes nil
  "List of regexps matching suffixes to remove from file names.
When checking `inhibit-local-variables-regexps', we first discard
from the end of the file name anything that matches one of these regexps.")

;; TODO explicitly add case-fold-search t?
(defun inhibit-local-variables-p ()
  "Return non-nil if file local variables should be ignored.
This checks the file (or buffer) name against `inhibit-local-variables-regexps'
and `inhibit-local-variables-suffixes'."
  (let ((temp inhibit-local-variables-regexps)
	(name (if buffer-file-name
		  (file-name-sans-versions buffer-file-name)
		(buffer-name))))
    (while (let ((sufs inhibit-local-variables-suffixes))
	     (while (and sufs (not (string-match (car sufs) name)))
	       (setq sufs (cdr sufs)))
	     sufs)
      (setq name (substring name 0 (match-beginning 0))))
    (while (and temp
		(not (string-match (car temp) name)))
      (setq temp (cdr temp)))
    temp))

(defvar auto-mode-interpreter-regexp
  (purecopy "#![ \t]?\\([^ \t\n]*\
/bin/env[ \t]\\)?\\([^ \t\n]+\\)")
  "Regexp matching interpreters, for file mode determination.
This regular expression is matched against the first line of a file
to determine the file's mode in `set-auto-mode'.  If it matches, the file
is assumed to be interpreted by the interpreter matched by the second group
of the regular expression.  The mode is then determined as the mode
associated with that interpreter in `interpreter-mode-alist'.")

(defvar magic-mode-alist nil
  "Alist of buffer beginnings vs. corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (MATCH-FUNCTION . FUNCTION).
After visiting a file, if REGEXP matches the text at the beginning of the
buffer, or calling MATCH-FUNCTION returns non-nil, `normal-mode' will
call FUNCTION rather than allowing `auto-mode-alist' to decide the buffer's
major mode.

If FUNCTION is nil, then it is not called.  (That is a way of saying
\"allow `auto-mode-alist' to decide for these files.\")")
(put 'magic-mode-alist 'risky-local-variable t)

(defvar magic-fallback-mode-alist
  (purecopy
  `((image-type-auto-detected-p . image-mode)
    ("\\(PK00\\)?[P]K\003\004" . archive-mode) ; zip
    ;; The < comes before the groups (but the first) to reduce backtracking.
    ;; TODO: UTF-16 <?xml may be preceded by a BOM 0xff 0xfe or 0xfe 0xff.
    ;; We use [ \t\r\n] instead of `\\s ' to make regex overflow less likely.
    (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
	     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
	(concat "\\(?:<\\?xml[ \t\r\n]+[^>]*>\\)?[ \t\r\n]*<"
		comment-re "*"
		"\\(?:!DOCTYPE[ \t\r\n]+[^>]*>[ \t\r\n]*<[ \t\r\n]*" comment-re "*\\)?"
		"[Hh][Tt][Mm][Ll]"))
     . html-mode)
    ("<!DOCTYPE[ \t\r\n]+[Hh][Tt][Mm][Ll]" . html-mode)
    ;; These two must come after html, because they are more general:
    ("<\\?xml " . xml-mode)
    (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
	     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
	(concat "[ \t\r\n]*<" comment-re "*!DOCTYPE "))
     . sgml-mode)
    ("%!PS" . ps-mode)
    ("# xmcd " . conf-unix-mode)))
  "Like `magic-mode-alist' but has lower priority than `auto-mode-alist'.
Each element looks like (REGEXP . FUNCTION) or (MATCH-FUNCTION . FUNCTION).
After visiting a file, if REGEXP matches the text at the beginning of the
buffer, or calling MATCH-FUNCTION returns non-nil, `normal-mode' will
call FUNCTION, provided that `magic-mode-alist' and `auto-mode-alist'
have not specified a mode for this file.

If FUNCTION is nil, then it is not called.")
(put 'magic-fallback-mode-alist 'risky-local-variable t)

(defvar magic-mode-regexp-match-limit 4000
  "Upper limit on `magic-mode-alist' regexp matches.
Also applies to `magic-fallback-mode-alist'.")

(defun set-auto-mode (&optional keep-mode-if-same)
  "Select major mode appropriate for current buffer.

To find the right major mode, this function checks for a -*- mode tag
checks for a `mode:' entry in the Local Variables section of the file,
checks if it uses an interpreter listed in `interpreter-mode-alist',
matches the buffer beginning against `magic-mode-alist',
compares the filename against the entries in `auto-mode-alist',
then matches the buffer beginning against `magic-fallback-mode-alist'.

If `enable-local-variables' is nil, or if the file name matches
`inhibit-local-variables-regexps', this function does not check
for any mode: tag anywhere in the file.  If `local-enable-local-variables'
is nil, then the only mode: tag that can be relevant is a -*- one.

If the optional argument KEEP-MODE-IF-SAME is non-nil, then we
set the major mode only if that would change it.  In other words
we don't actually set it to the same mode the buffer already has."
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let ((try-locals (not (inhibit-local-variables-p)))
	end done mode modes)
    ;; Once we drop the deprecated feature where mode: is also allowed to
    ;; specify minor-modes (ie, there can be more than one "mode:"), we can
    ;; remove this section and just let (hack-local-variables t) handle it.
    ;; Find a -*- mode tag.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      ;; Note by design local-enable-local-variables does not matter here.
      (and enable-local-variables
	   try-locals
	   (setq end (set-auto-mode-1))
	   (if (save-excursion (search-forward ":" end t))
	       ;; Find all specifications for the `mode:' variable
	       ;; and execute them left to right.
	       (while (let ((case-fold-search t))
			(or (and (looking-at "mode:")
				 (goto-char (match-end 0)))
			    (re-search-forward "[ \t;]mode:" end t)))
		 (skip-chars-forward " \t")
		 (let ((beg (point)))
		   (if (search-forward ";" end t)
		       (forward-char -1)
		     (goto-char end))
		   (skip-chars-backward " \t")
		   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
			 modes)))
	     ;; Simple -*-MODE-*- case.
	     (push (intern (concat (downcase (buffer-substring (point) end))
				   "-mode"))
		   modes))))
    ;; If we found modes to use, invoke them now, outside the save-excursion.
    (if modes
	(catch 'nop
	  (dolist (mode (nreverse modes))
	    (if (not (functionp mode))
		(message "Ignoring unknown mode `%s'" mode)
	      (setq done t)
	      (or (set-auto-mode-0 mode keep-mode-if-same)
		  ;; continuing would call minor modes again, toggling them off
		  (throw 'nop nil))))))
    ;; hack-local-variables checks local-enable-local-variables etc, but
    ;; we might as well be explicit here for the sake of clarity.
    (and (not done)
	 enable-local-variables
	 local-enable-local-variables
	 try-locals
	 (setq mode (hack-local-variables t))
	 (not (memq mode modes))	; already tried and failed
	 (if (not (functionp mode))
	     (message "Ignoring unknown mode `%s'" mode)
	   (setq done t)
	   (set-auto-mode-0 mode keep-mode-if-same)))
    ;; If we didn't, look for an interpreter specified in the first line.
    ;; As a special case, allow for things like "#!/bin/env perl", which
    ;; finds the interpreter anywhere in $PATH.
    (unless done
      (setq mode (save-excursion
		   (goto-char (point-min))
		   (if (looking-at auto-mode-interpreter-regexp)
		       (match-string 2)
		     ""))
	    ;; Map interpreter name to a mode, signaling we're done at the
	    ;; same time.
	    done (assoc (file-name-nondirectory mode)
			interpreter-mode-alist))
      ;; If we found an interpreter mode to use, invoke it now.
      (if done
	  (set-auto-mode-0 (cdr done) keep-mode-if-same)))
    ;; Next try matching the buffer beginning against magic-mode-alist.
    (unless done
      (if (setq done (save-excursion
		       (goto-char (point-min))
		       (save-restriction
			 (narrow-to-region (point-min)
					   (min (point-max)
						(+ (point-min) magic-mode-regexp-match-limit)))
			 (assoc-default nil magic-mode-alist
					(lambda (re _dummy)
					  (if (functionp re)
					      (funcall re)
					    (looking-at re)))))))
	  (set-auto-mode-0 done keep-mode-if-same)))
    ;; Next compare the filename against the entries in auto-mode-alist.
    (unless done
      (if buffer-file-name
	  (let ((name buffer-file-name)
		(remote-id (file-remote-p buffer-file-name)))
	    ;; Remove backup-suffixes from file name.
	    (setq name (file-name-sans-versions name))
	    ;; Remove remote file name identification.
	    (when (and (stringp remote-id)
		       (string-match (regexp-quote remote-id) name))
	      (setq name (substring name (match-end 0))))
	    (while name
	      ;; Find first matching alist entry.
	      (setq mode
		    (if (memq system-type '(windows-nt cygwin))
			;; System is case-insensitive.
			(let ((case-fold-search t))
			  (assoc-default name auto-mode-alist
					 'string-match))
		      ;; System is case-sensitive.
		      (or
		       ;; First match case-sensitively.
		       (let ((case-fold-search nil))
			 (assoc-default name auto-mode-alist
					'string-match))
		       ;; Fallback to case-insensitive match.
		       (and auto-mode-case-fold
			    (let ((case-fold-search t))
			      (assoc-default name auto-mode-alist
					     'string-match))))))
	      (if (and mode
		       (consp mode)
		       (cadr mode))
		  (setq mode (car mode)
			name (substring name 0 (match-beginning 0)))
		(setq name))
	      (when mode
		(set-auto-mode-0 mode keep-mode-if-same)
		(setq done t))))))
    ;; Next try matching the buffer beginning against magic-fallback-mode-alist.
    (unless done
      (if (setq done (save-excursion
		       (goto-char (point-min))
		       (save-restriction
			 (narrow-to-region (point-min)
					   (min (point-max)
						(+ (point-min) magic-mode-regexp-match-limit)))
			 (assoc-default nil magic-fallback-mode-alist
					(lambda (re _dummy)
					  (if (functionp re)
					      (funcall re)
					    (looking-at re)))))))
	  (set-auto-mode-0 done keep-mode-if-same)))))

;; When `keep-mode-if-same' is set, we are working on behalf of
;; set-visited-file-name.  In that case, if the major mode specified is the
;; same one we already have, don't actually reset it.  We don't want to lose
;; minor modes such as Font Lock.
(defun set-auto-mode-0 (mode &optional keep-mode-if-same)
  "Apply MODE and return it.
If optional arg KEEP-MODE-IF-SAME is non-nil, MODE is chased of
any aliases and compared to current major mode.  If they are the
same, do nothing and return nil."
  (unless (and keep-mode-if-same
	       (eq (indirect-function mode)
		   (indirect-function major-mode)))
    (when mode
      (funcall mode)
      mode)))

(defun set-auto-mode-1 ()
  "Find the -*- spec in the buffer.
Call with point at the place to start searching from.
If one is found, set point to the beginning and return the position
of the end.  Otherwise, return nil; may change point.
The variable `inhibit-local-variables-regexps' can cause a -*- spec to
be ignored; but `enable-local-variables' and `local-enable-local-variables'
have no effect."
  (let (beg end)
    (and
     ;; Don't look for -*- if this file name matches any
     ;; of the regexps in inhibit-local-variables-regexps.
     (not (inhibit-local-variables-p))
     (search-forward "-*-" (line-end-position
                            ;; If the file begins with "#!"  (exec
                            ;; interpreter magic), look for mode frobs
                            ;; in the first two lines.  You cannot
                            ;; necessarily put them in the first line
                            ;; of such a file without screwing up the
                            ;; interpreter invocation.  The same holds
                            ;; for '\" in man pages (preprocessor
                            ;; magic for the `man' program).
                            (and (looking-at "^\\(#!\\|'\\\\\"\\)") 2)) t)
     (progn
       (skip-chars-forward " \t")
       (setq beg (point))
       (search-forward "-*-" (line-end-position) t))
     (progn
       (forward-char -3)
       (skip-chars-backward " \t")
       (setq end (point))
       (goto-char beg)
       end))))

;;; Handling file local variables

(defvar ignored-local-variables
  '(ignored-local-variables safe-local-variable-values
    file-local-variables-alist dir-local-variables-alist)
  "Variables to be ignored in a file's local variable spec.")
(put 'ignored-local-variables 'risky-local-variable t)

(defvar hack-local-variables-hook nil
  "Normal hook run after processing a file's local variables specs.
Major modes can use this to examine user-specified local variables
in order to initialize other data structure based on them.")

(defcustom safe-local-variable-values nil
  "List variable-value pairs that are considered safe.
Each element is a cons cell (VAR . VAL), where VAR is a variable
symbol and VAL is a value that is considered safe."
  :risky t
  :group 'find-file
  :type 'alist)

(defcustom safe-local-eval-forms
  ;; This should be here at least as long as Emacs supports write-file-hooks.
  '((add-hook 'write-file-hooks 'time-stamp)
    (add-hook 'write-file-functions 'time-stamp)
    (add-hook 'before-save-hook 'time-stamp))
  "Expressions that are considered safe in an `eval:' local variable.
Add expressions to this list if you want Emacs to evaluate them, when
they appear in an `eval' local variable specification, without first
asking you for confirmation."
  :risky t
  :group 'find-file
  :version "24.1"			; added write-file-hooks
  :type '(repeat sexp))

;; Risky local variables:
(mapc (lambda (var) (put var 'risky-local-variable t))
      '(after-load-alist
	buffer-auto-save-file-name
	buffer-file-name
	buffer-file-truename
	buffer-undo-list
	debugger
	default-text-properties
	eval
	exec-directory
	exec-path
	file-name-handler-alist
	frame-title-format
	global-mode-string
	header-line-format
	icon-title-format
	inhibit-quit
	load-path
	max-lisp-eval-depth
	max-specpdl-size
	minor-mode-map-alist
	minor-mode-overriding-map-alist
	mode-line-format
	mode-name
	overriding-local-map
	overriding-terminal-local-map
	process-environment
	standard-input
	standard-output
	unread-command-events))

;; Safe local variables:
;;
;; For variables defined by major modes, the safety declarations can go into
;; the major mode's file, since that will be loaded before file variables are
;; processed.
;;
;; For variables defined by minor modes, put the safety declarations in the
;; file defining the minor mode after the defcustom/defvar using an autoload
;; cookie, e.g.:
;;
;;   ;;;###autoload(put 'variable 'safe-local-variable 'stringp)
;;
;; Otherwise, when Emacs visits a file specifying that local variable, the
;; minor mode file may not be loaded yet.
;;
;; For variables defined in the C source code the declaration should go here:

(dolist (pair
	 '((buffer-read-only        . booleanp)	;; C source code
	   (default-directory       . stringp)	;; C source code
	   (fill-column             . integerp)	;; C source code
	   (indent-tabs-mode        . booleanp)	;; C source code
	   (left-margin             . integerp)	;; C source code
	   (no-update-autoloads     . booleanp)
	   (lexical-binding	 . booleanp)	  ;; C source code
	   (tab-width               . integerp)	  ;; C source code
	   (truncate-lines          . booleanp)	  ;; C source code
	   (word-wrap               . booleanp)	  ;; C source code
	   (bidi-display-reordering . booleanp))) ;; C source code
  (put (car pair) 'safe-local-variable (cdr pair)))

(put 'bidi-paragraph-direction 'safe-local-variable
     (lambda (v) (memq v '(nil right-to-left left-to-right))))

(put 'c-set-style 'safe-local-eval-function t)

(defvar file-local-variables-alist nil
  "Alist of file-local variable settings in the current buffer.
Each element in this list has the form (VAR . VALUE), where VAR
is a file-local variable (a symbol) and VALUE is the value
specified.  The actual value in the buffer may differ from VALUE,
if it is changed by the major or minor modes, or by the user.")
(make-variable-buffer-local 'file-local-variables-alist)
(put 'file-local-variables-alist 'permanent-local t)

(defvar dir-local-variables-alist nil
  "Alist of directory-local variable settings in the current buffer.
Each element in this list has the form (VAR . VALUE), where VAR
is a directory-local variable (a symbol) and VALUE is the value
specified in .dir-locals.el.  The actual value in the buffer
may differ from VALUE, if it is changed by the major or minor modes,
or by the user.")
(make-variable-buffer-local 'dir-local-variables-alist)

(defvar before-hack-local-variables-hook nil
  "Normal hook run before setting file-local variables.
It is called after checking for unsafe/risky variables and
setting `file-local-variables-alist', and before applying the
variables stored in `file-local-variables-alist'.  A hook
function is allowed to change the contents of this alist.

This hook is called only if there is at least one file-local
variable to set.")

(defun hack-local-variables-confirm (all-vars unsafe-vars risky-vars dir-name)
  "Get confirmation before setting up local variable values.
ALL-VARS is the list of all variables to be set up.
UNSAFE-VARS is the list of those that aren't marked as safe or risky.
RISKY-VARS is the list of those that are marked as risky.
If these settings come from directory-local variables, then
DIR-NAME is the name of the associated directory.  Otherwise it is nil."
  (if noninteractive
      nil
    (save-window-excursion
      (let* ((name (or dir-name
		       (if buffer-file-name
			   (file-name-nondirectory buffer-file-name)
			 (concat "buffer " (buffer-name)))))
	     (offer-save (and (eq enable-local-variables t)
			      unsafe-vars))
	     (exit-chars
	      (if offer-save '(?! ?y ?n ?\s ?\C-g) '(?y ?n ?\s ?\C-g)))
	     (buf (pop-to-buffer "*Local Variables*"))
	     prompt char)
	(set (make-local-variable 'cursor-type) nil)
	(erase-buffer)
	(cond
	 (unsafe-vars
	  (insert "The local variables list in " name
		  "\ncontains values that may not be safe (*)"
		  (if risky-vars
		      ", and variables that are risky (**)."
		    ".")))
	 (risky-vars
	  (insert "The local variables list in " name
		  "\ncontains variables that are risky (**)."))
	 (t
	  (insert "A local variables list is specified in " name ".")))
	(insert "\n\nDo you want to apply it?  You can type
y  -- to apply the local variables list.
n  -- to ignore the local variables list.")
	(if offer-save
	    (insert "
!  -- to apply the local variables list, and permanently mark these
      values (*) as safe (in the future, they will be set automatically.)\n\n")
	  (insert "\n\n"))
	(dolist (elt all-vars)
	  (cond ((member elt unsafe-vars)
		 (insert "  * "))
		((member elt risky-vars)
		 (insert " ** "))
		(t
		 (insert "    ")))
	  (princ (car elt) buf)
	  (insert " : ")
	  ;; Make strings with embedded whitespace easier to read.
	  (let ((print-escape-newlines t))
	    (prin1 (cdr elt) buf))
	  (insert "\n"))
	(setq prompt
	      (format "Please type %s%s: "
		      (if offer-save "y, n, or !" "y or n")
		      (if (< (line-number-at-pos) (window-body-height))
			  ""
			(push ?\C-v exit-chars)
			", or C-v to scroll")))
	(goto-char (point-min))
	(while (null char)
	  (setq char (read-char-choice prompt exit-chars t))
	  (when (eq char ?\C-v)
	    (condition-case nil
		(scroll-up)
	      (error (goto-char (point-min))))
	    (setq char nil)))
	(kill-buffer buf)
	(when (and offer-save (= char ?!) unsafe-vars)
	  (customize-push-and-save 'safe-local-variable-values unsafe-vars))
	(memq char '(?! ?\s ?y))))))

(defun hack-local-variables-prop-line (&optional mode-only)
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.  Ignores any specification for
`mode:' and `coding:' (which should have already been handled
by `set-auto-mode' and `set-auto-coding', respectively).
Return nil if the -*- line is malformed.

If MODE-ONLY is non-nil, just returns the symbol specifying the
mode, if there is one, otherwise nil."
  (catch 'malformed-line
    (save-excursion
      (goto-char (point-min))
      (let ((end (set-auto-mode-1))
	    result)
	(cond ((not end)
	       nil)
	      ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	       ;; Simple form: "-*- MODENAME -*-".
	       (if mode-only
		   (intern (concat (match-string 1) "-mode"))))
	      (t
	       ;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	       ;; (last ";" is optional).
	       ;; If MODE-ONLY, just check for `mode'.
	       ;; Otherwise, parse the -*- line into the RESULT alist.
	       (while (and (or (not mode-only)
			       (not result))
			   (< (point) end))
		 (unless (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		   (message "Malformed mode-line")
		   (throw 'malformed-line nil))
		 (goto-char (match-end 0))
		 ;; There used to be a downcase here,
		 ;; but the manual didn't say so,
		 ;; and people want to set var names that aren't all lc.
		 (let* ((key (intern (match-string 1)))
			(val (save-restriction
			       (narrow-to-region (point) end)
			       (let ((read-circle nil))
				 (read (current-buffer)))))
			;; It is traditional to ignore
			;; case when checking for `mode' in set-auto-mode,
			;; so we must do that here as well.
			;; That is inconsistent, but we're stuck with it.
			;; The same can be said for `coding' in set-auto-coding.
			(keyname (downcase (symbol-name key))))
		   (if mode-only
		       (and (equal keyname "mode")
			    (setq result
				  (intern (concat (downcase (symbol-name val))
						  "-mode"))))
		     (or (equal keyname "coding")
			 (condition-case nil
			     (push (cons (cond ((eq key 'eval) 'eval)
					       ;; Downcase "Mode:".
					       ((equal keyname "mode") 'mode)
					       (t (indirect-variable key)))
					 val) result)
			   (error nil))))
		   (skip-chars-forward " \t;")))
	       result))))))

(defun hack-local-variables-filter (variables dir-name)
  "Filter local variable settings, querying the user if necessary.
VARIABLES is the alist of variable-value settings.  This alist is
 filtered based on the values of `ignored-local-variables',
 `enable-local-eval', `enable-local-variables', and (if necessary)
 user interaction.  The results are added to
 `file-local-variables-alist', without applying them.
If these settings come from directory-local variables, then
DIR-NAME is the name of the associated directory.  Otherwise it is nil."
  ;; Find those variables that we may want to save to
  ;; `safe-local-variable-values'.
  (let (all-vars risky-vars unsafe-vars)
    (dolist (elt variables)
      (let ((var (car elt))
	    (val (cdr elt)))
	(cond ((memq var ignored-local-variables)
	       ;; Ignore any variable in `ignored-local-variables'.
	       nil)
	      ;; Obey `enable-local-eval'.
	      ((eq var 'eval)
	       (when enable-local-eval
		 (push elt all-vars)
		 (or (eq enable-local-eval t)
		     (hack-one-local-variable-eval-safep (eval (quote val)))
		     (safe-local-variable-p var val)
		     (push elt unsafe-vars))))
	      ;; Ignore duplicates (except `mode') in the present list.
	      ((and (assq var all-vars) (not (eq var 'mode))) nil)
	      ;; Accept known-safe variables.
	      ((or (memq var '(mode unibyte coding))
		   (safe-local-variable-p var val))
	       (push elt all-vars))
	      ;; The variable is either risky or unsafe:
	      ((not (eq enable-local-variables :safe))
	       (push elt all-vars)
	       (if (risky-local-variable-p var val)
		   (push elt risky-vars)
		 (push elt unsafe-vars))))))
    (and all-vars
	 ;; Query, unless all vars are safe or user wants no querying.
	 (or (and (eq enable-local-variables t)
		  (null unsafe-vars)
		  (null risky-vars))
	     (memq enable-local-variables '(:all :safe))
	     (hack-local-variables-confirm all-vars unsafe-vars
					   risky-vars dir-name))
	 (dolist (elt all-vars)
	   (unless (memq (car elt) '(eval mode))
	     (unless dir-name
	       (setq dir-local-variables-alist
		     (assq-delete-all (car elt) dir-local-variables-alist)))
	     (setq file-local-variables-alist
		   (assq-delete-all (car elt) file-local-variables-alist)))
	   (push elt file-local-variables-alist)))))

(defun hack-local-variables (&optional mode-only)
  "Parse and put into effect this buffer's local variables spec.
Uses `hack-local-variables-apply' to apply the variables.

If MODE-ONLY is non-nil, all we do is check whether a \"mode:\"
is specified, and return the corresponding mode symbol, or nil.
In this case, we try to ignore minor-modes, and only return a
major-mode.

If `enable-local-variables' or `local-enable-local-variables' is nil,
this function does nothing.  If `inhibit-local-variables-regexps'
applies to the file in question, the file is not scanned for
local variables, but directory-local variables may still be applied."
  ;; We don't let inhibit-local-variables-p influence the value of
  ;; enable-local-variables, because then it would affect dir-local
  ;; variables.  We don't want to search eg tar files for file local
  ;; variable sections, but there is no reason dir-locals cannot apply
  ;; to them.  The real meaning of inhibit-local-variables-p is "do
  ;; not scan this file for local variables".
  (let ((enable-local-variables
	 (and local-enable-local-variables enable-local-variables))
	result)
    (unless mode-only
      (setq file-local-variables-alist nil)
      (report-errors "Directory-local variables error: %s"
	;; Note this is a no-op if enable-local-variables is nil.
	(hack-dir-local-variables)))
    ;; This entire function is basically a no-op if enable-local-variables
    ;; is nil.  All it does is set file-local-variables-alist to nil.
    (when enable-local-variables
      ;; This part used to ignore enable-local-variables when mode-only
      ;; was non-nil.  That was inappropriate, eg consider the
      ;; (artificial) example of:
      ;; (setq local-enable-local-variables nil)
      ;; Open a file foo.txt that contains "mode: sh".
      ;; It correctly opens in text-mode.
      ;; M-x set-visited-file name foo.c, and it incorrectly stays in text-mode.
      (unless (or (inhibit-local-variables-p)
		  ;; If MODE-ONLY is non-nil, and the prop line specifies a
		  ;; mode, then we're done, and have no need to scan further.
		  (and (setq result (hack-local-variables-prop-line mode-only))
		       mode-only))
	;; Look for "Local variables:" line in last page.
	(save-excursion
	  (goto-char (point-max))
	  (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
			   'move)
	  (when (let ((case-fold-search t))
		  (search-forward "Local Variables:" nil t))
	    (skip-chars-forward " \t")
	    ;; suffix is what comes after "local variables:" in its line.
	    ;; prefix is what comes before "local variables:" in its line.
	    (let ((suffix
		   (concat
		    (regexp-quote (buffer-substring (point)
						    (line-end-position)))
		    "$"))
		  (prefix
		   (concat "^" (regexp-quote
				(buffer-substring (line-beginning-position)
						  (match-beginning 0)))))
		  beg)

	      (forward-line 1)
	      (let ((startpos (point))
		    endpos
		    (thisbuf (current-buffer)))
		(save-excursion
		  (unless (let ((case-fold-search t))
			    (re-search-forward
			     (concat prefix "[ \t]*End:[ \t]*" suffix)
			     nil t))
		    ;; This used to be an error, but really all it means is
		    ;; that this may simply not be a local-variables section,
		    ;; so just ignore it.
		    (message "Local variables list is not properly terminated"))
		  (beginning-of-line)
		  (setq endpos (point)))

		(with-temp-buffer
		  (insert-buffer-substring thisbuf startpos endpos)
		  (goto-char (point-min))
		  (subst-char-in-region (point) (point-max) ?\^m ?\n)
		  (while (not (eobp))
		    ;; Discard the prefix.
		    (if (looking-at prefix)
			(delete-region (point) (match-end 0))
		      (error "Local variables entry is missing the prefix"))
		    (end-of-line)
		    ;; Discard the suffix.
		    (if (looking-back suffix)
			(delete-region (match-beginning 0) (point))
		      (error "Local variables entry is missing the suffix"))
		    (forward-line 1))
		  (goto-char (point-min))

		  (while (and (not (eobp))
			      (or (not mode-only)
				  (not result)))
		    ;; Find the variable name; strip whitespace.
		    (skip-chars-forward " \t")
		    (setq beg (point))
		    (skip-chars-forward "^:\n")
		    (if (eolp) (error "Missing colon in local variables entry"))
		    (skip-chars-backward " \t")
		    (let* ((str (buffer-substring beg (point)))
			   (var (let ((read-circle nil))
				  (read str)))
			   val val2)
		      (and (equal (downcase (symbol-name var)) "mode")
			   (setq var 'mode))
		      ;; Read the variable value.
		      (skip-chars-forward "^:")
		      (forward-char 1)
		      (let ((read-circle nil))
			(setq val (read (current-buffer))))
		      (if mode-only
			  (and (eq var 'mode)
			       ;; Specifying minor-modes via mode: is
			       ;; deprecated, but try to reject them anyway.
			       (not (string-match
				     "-minor\\'"
				     (setq val2 (downcase (symbol-name val)))))
			       (setq result (intern (concat val2 "-mode"))))
			(unless (eq var 'coding)
			  (condition-case nil
			      (push (cons (if (eq var 'eval)
					      'eval
					    (indirect-variable var))
					  val) result)
			    (error nil)))))
		    (forward-line 1))))))))
      ;; Now we've read all the local variables.
      ;; If MODE-ONLY is non-nil, return whether the mode was specified.
      (if mode-only result
	;; Otherwise, set the variables.
	(hack-local-variables-filter result nil)
	(hack-local-variables-apply)))))

(defun hack-local-variables-apply ()
  "Apply the elements of `file-local-variables-alist'.
If there are any elements, runs `before-hack-local-variables-hook',
then calls `hack-one-local-variable' to apply the alist elements one by one.
Finishes by running `hack-local-variables-hook', regardless of whether
the alist is empty or not.

Note that this function ignores a `mode' entry if it specifies the same
major mode as the buffer already has."
  (when file-local-variables-alist
    ;; Any 'evals must run in the Right sequence.
    (setq file-local-variables-alist
	  (nreverse file-local-variables-alist))
    (run-hooks 'before-hack-local-variables-hook)
    (dolist (elt file-local-variables-alist)
      (hack-one-local-variable (car elt) (cdr elt))))
  (run-hooks 'hack-local-variables-hook))

(defun safe-local-variable-p (sym val)
  "Non-nil if SYM is safe as a file-local variable with value VAL.
It is safe if any of these conditions are met:

 * There is a matching entry (SYM . VAL) in the
   `safe-local-variable-values' user option.

 * The `safe-local-variable' property of SYM is a function that
   evaluates to a non-nil value with VAL as an argument."
  (or (member (cons sym val) safe-local-variable-values)
      (let ((safep (get sym 'safe-local-variable)))
        (and (functionp safep)
             ;; If the function signals an error, that means it
             ;; can't assure us that the value is safe.
             (with-demoted-errors (funcall safep val))))))

(defun risky-local-variable-p (sym &optional _ignored)
  "Non-nil if SYM could be dangerous as a file-local variable.
It is dangerous if either of these conditions are met:

 * Its `risky-local-variable' property is non-nil.

 * Its name ends with \"hook(s)\", \"function(s)\", \"form(s)\", \"map\",
   \"program\", \"command(s)\", \"predicate(s)\", \"frame-alist\",
   \"mode-alist\", \"font-lock-(syntactic-)keyword*\",
   \"map-alist\", or \"bindat-spec\"."
  ;; If this is an alias, check the base name.
  (condition-case nil
      (setq sym (indirect-variable sym))
    (error nil))
  (or (get sym 'risky-local-variable)
      (string-match "-hooks?$\\|-functions?$\\|-forms?$\\|-program$\\|\
-commands?$\\|-predicates?$\\|font-lock-keywords$\\|font-lock-keywords\
-[0-9]+$\\|font-lock-syntactic-keywords$\\|-frame-alist$\\|-mode-alist$\\|\
-map$\\|-map-alist$\\|-bindat-spec$" (symbol-name sym))))

(defun hack-one-local-variable-quotep (exp)
  (and (consp exp) (eq (car exp) 'quote) (consp (cdr exp))))

(defun hack-one-local-variable-constantp (exp)
  (or (and (not (symbolp exp)) (not (consp exp)))
      (memq exp '(t nil))
      (keywordp exp)
      (hack-one-local-variable-quotep exp)))

(defun hack-one-local-variable-eval-safep (exp)
  "Return t if it is safe to eval EXP when it is found in a file."
  (or (not (consp exp))
      ;; Detect certain `put' expressions.
      (and (eq (car exp) 'put)
	   (hack-one-local-variable-quotep (nth 1 exp))
	   (hack-one-local-variable-quotep (nth 2 exp))
	   (let ((prop (nth 1 (nth 2 exp)))
		 (val (nth 3 exp)))
	     (cond ((memq prop '(lisp-indent-hook
				 lisp-indent-function
				 scheme-indent-function))
		    ;; Only allow safe values (not functions).
		    (or (numberp val)
			(and (hack-one-local-variable-quotep val)
			     (eq (nth 1 val) 'defun))))
		   ((eq prop 'edebug-form-spec)
		    ;; Only allow indirect form specs.
		    ;; During bootstrapping, edebug-basic-spec might not be
		    ;; defined yet.
                    (and (fboundp 'edebug-basic-spec)
			 (hack-one-local-variable-quotep val)
                         (edebug-basic-spec (nth 1 val)))))))
      ;; Allow expressions that the user requested.
      (member exp safe-local-eval-forms)
      ;; Certain functions can be allowed with safe arguments
      ;; or can specify verification functions to try.
      (and (symbolp (car exp))
	   ;; Allow (minor)-modes calls with no arguments.
	   ;; This obsoletes the use of "mode:" for such things.  (Bug#8613)
	   (or (and (member (cdr exp) '(nil (1) (0) (-1)))
		    (string-match "-mode\\'" (symbol-name (car exp))))
	       (let ((prop (get (car exp) 'safe-local-eval-function)))
		 (cond ((eq prop t)
			(let ((ok t))
			  (dolist (arg (cdr exp))
			    (unless (hack-one-local-variable-constantp arg)
			      (setq ok nil)))
			  ok))
		       ((functionp prop)
			(funcall prop exp))
		       ((listp prop)
			(let ((ok nil))
			  (dolist (function prop)
			    (if (funcall function exp)
				(setq ok t)))
			  ok))))))))

(defun hack-one-local-variable (var val)
  "Set local variable VAR with value VAL.
If VAR is `mode', call `VAL-mode' as a function unless it's
already the major mode."
  (cond ((eq var 'mode)
	 (let ((mode (intern (concat (downcase (symbol-name val))
				     "-mode"))))
	   (unless (eq (indirect-function mode)
		       (indirect-function major-mode))
	     (if (memq mode minor-mode-list)
		 ;; A minor mode must be passed an argument.
		 ;; Otherwise, if the user enables the minor mode in a
		 ;; major mode hook, this would toggle it off.
		 (funcall mode 1)
	       (funcall mode)))))
	((eq var 'eval)
	 (save-excursion (eval val)))
	(t
         ;; Make sure the string has no text properties.
         ;; Some text properties can get evaluated in various ways,
         ;; so it is risky to put them on with a local variable list.
         (if (stringp val)
             (set-text-properties 0 (length val) nil val))
         (set (make-local-variable var) val))))

;;; Handling directory-local variables, aka project settings.

(defvar dir-locals-class-alist '()
  "Alist mapping directory-local variable classes (symbols) to variable lists.")

(defvar dir-locals-directory-cache '()
  "List of cached directory roots for directory-local variable classes.
Each element in this list has the form (DIR CLASS MTIME).
DIR is the name of the directory.
CLASS is the name of a variable class (a symbol).
MTIME is the recorded modification time of the directory-local
variables file associated with this entry.  This time is a list
of two integers (the same format as `file-attributes'), and is
used to test whether the cache entry is still valid.
Alternatively, MTIME can be nil, which means the entry is always
considered valid.")

(defsubst dir-locals-get-class-variables (class)
  "Return the variable list for CLASS."
  (cdr (assq class dir-locals-class-alist)))

(defun dir-locals-collect-mode-variables (mode-variables variables)
  "Collect directory-local variables from MODE-VARIABLES.
VARIABLES is the initial list of variables.
Returns the new list."
  (dolist (pair mode-variables variables)
    (let* ((variable (car pair))
	   (value (cdr pair))
	   (slot (assq variable variables)))
      ;; If variables are specified more than once, only use the last.  (Why?)
      ;; The pseudo-variables mode and eval are different (bug#3430).
      (if (and slot (not (memq variable '(mode eval))))
	  (setcdr slot value)
	;; Need a new cons in case we setcdr later.
	(push (cons variable value) variables)))))

(defun dir-locals-collect-variables (class-variables root variables)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list."
  (let* ((file-name (buffer-file-name))
	 (sub-file-name (if file-name
                            ;; FIXME: Why not use file-relative-name?
			    (substring file-name (length root)))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (cdr entry) root variables))))
             ((or (not key)
                  (derived-mode-p key))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (delq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root default-directory))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message ".dir-locals error: %s" (error-message-string err))
       nil))))

(defun dir-locals-set-directory-class (directory class &optional mtime)
  "Declare that the DIRECTORY root is an instance of CLASS.
DIRECTORY is the name of a directory, a string.
CLASS is the name of a project class, a symbol.
MTIME is either the modification time of the directory-local
variables file that defined this class, or nil.

When a file beneath DIRECTORY is visited, the mode-specific
variables from CLASS are applied to the buffer.  The variables
for a class are defined using `dir-locals-set-class-variables'."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (unless (assq class dir-locals-class-alist)
    (error "No such class `%s'" (symbol-name class)))
  (push (list directory class mtime) dir-locals-directory-cache))

(defun dir-locals-set-class-variables (class variables)
  "Map the type CLASS to a list of variable settings.
CLASS is the project class, a symbol.  VARIABLES is a list
that declares directory-local variables for the class.
An element in VARIABLES is either of the form:
    (MAJOR-MODE . ALIST)
or
    (DIRECTORY . LIST)

In the first form, MAJOR-MODE is a symbol, and ALIST is an alist
whose elements are of the form (VARIABLE . VALUE).

In the second form, DIRECTORY is a directory name (a string), and
LIST is a list of the form accepted by the function.

When a file is visited, the file's class is found.  A directory
may be assigned a class using `dir-locals-set-directory-class'.
Then variables are set in the file's buffer according to the
VARIABLES list of the class.  The list is processed in order.

* If the element is of the form (MAJOR-MODE . ALIST), and the
  buffer's major mode is derived from MAJOR-MODE (as determined
  by `derived-mode-p'), then all the variables in ALIST are
  applied.  A MAJOR-MODE of nil may be used to match any buffer.
  `make-local-variable' is called for each variable before it is
  set.

* If the element is of the form (DIRECTORY . LIST), and DIRECTORY
  is an initial substring of the file's directory, then LIST is
  applied by recursively following these rules."
  (let ((elt (assq class dir-locals-class-alist)))
    (if elt
	(setcdr elt variables)
      (push (cons class variables) dir-locals-class-alist))))

(defconst dir-locals-file ".dir-locals.el"
  "File that contains directory-local variables.
It has to be constant to enforce uniform values
across different environments and users.")

(defun dir-locals-find-file (file)
  "Find the directory-local variables for FILE.
This searches upward in the directory tree from FILE.
It stops at the first directory that has been registered in
`dir-locals-directory-cache' or contains a `dir-locals-file'.
If it finds an entry in the cache, it checks that it is valid.
A cache entry with no modification time element (normally, one that
has been assigned directly using `dir-locals-set-directory-class', not
set from a file) is always valid.
A cache entry based on a `dir-locals-file' is valid if the modification
time stored in the cache matches the current file modification time.
If not, the cache entry is cleared so that the file will be re-read.

This function returns either nil (no directory local variables found),
or the matching entry from `dir-locals-directory-cache' (a list),
or the full path to the `dir-locals-file' (a string) in the case
of no valid cache entry."
  (setq file (expand-file-name file))
  (let* ((dir-locals-file-name
	  (if (eq system-type 'ms-dos)
	      (dosified-file-name dir-locals-file)
	    dir-locals-file))
	 (locals-file (locate-dominating-file file dir-locals-file-name))
	 (dir-elt nil))
    ;; `locate-dominating-file' may have abbreviated the name.
    (and locals-file
	 (setq locals-file (expand-file-name dir-locals-file-name locals-file)))
	 ;; Let dir-locals-read-from-file inform us via demoted-errors
	 ;; about unreadable files, etc.
	 ;; Maybe we'd want to keep searching though - that is
	 ;; a locate-dominating-file issue.
;;;	 (or (not (file-readable-p locals-file))
;;;	     (not (file-regular-p locals-file)))
;;;	 (setq locals-file nil))
    ;; Find the best cached value in `dir-locals-directory-cache'.
    (dolist (elt dir-locals-directory-cache)
      (when (and (eq t (compare-strings file nil (length (car elt))
					(car elt) nil nil
					(memq system-type
					      '(windows-nt cygwin ms-dos))))
		 (> (length (car elt)) (length (car dir-elt))))
	(setq dir-elt elt)))
    (if (and dir-elt
	     (or (null locals-file)
		 (<= (length (file-name-directory locals-file))
		     (length (car dir-elt)))))
	;; Found a potential cache entry.  Check validity.
	;; A cache entry with no MTIME is assumed to always be valid
	;; (ie, set directly, not from a dir-locals file).
	;; Note, we don't bother to check that there is a matching class
	;; element in dir-locals-class-alist, since that's done by
	;; dir-locals-set-directory-class.
	(if (or (null (nth 2 dir-elt))
		(let ((cached-file (expand-file-name dir-locals-file-name
						     (car dir-elt))))
		  (and (file-readable-p cached-file)
		       (equal (nth 2 dir-elt)
			      (nth 5 (file-attributes cached-file))))))
	    ;; This cache entry is OK.
	    dir-elt
	  ;; This cache entry is invalid; clear it.
	  (setq dir-locals-directory-cache
		(delq dir-elt dir-locals-directory-cache))
	  ;; Return the first existing dir-locals file.  Might be the same
	  ;; as dir-elt's, might not (eg latter might have been deleted).
	  locals-file)
      ;; No cache entry.
      locals-file)))

(defun dir-locals-read-from-file (file)
  "Load a variables FILE and register a new class and instance.
FILE is the name of the file holding the variables to apply.
The new class name is the same as the directory in which FILE
is found.  Returns the new class name."
  (with-temp-buffer
    ;; This is with-demoted-errors, but we want to mention dir-locals
    ;; in any error message.
    (let (err)
      (condition-case err
	  (progn
	    (insert-file-contents file)
	    (let* ((dir-name (file-name-directory file))
		   (class-name (intern dir-name))
		   (variables (let ((read-circle nil))
				(read (current-buffer)))))
	      (dir-locals-set-class-variables class-name variables)
	      (dir-locals-set-directory-class dir-name class-name
					      (nth 5 (file-attributes file)))
	      class-name))
	(error (message "Error reading dir-locals: %S" err) nil)))))

(defun hack-dir-local-variables ()
  "Read per-directory local variables for the current buffer.
Store the directory-local variables in `dir-local-variables-alist'
and `file-local-variables-alist', without applying them."
  (when (and enable-local-variables
	     (not (file-remote-p (or (buffer-file-name) default-directory))))
    ;; Find the variables file.
    (let ((variables-file (dir-locals-find-file (or (buffer-file-name) default-directory)))
	  (class nil)
	  (dir-name nil))
      (cond
       ((stringp variables-file)
	(setq dir-name (file-name-directory variables-file)
	      class (dir-locals-read-from-file variables-file)))
       ((consp variables-file)
	(setq dir-name (nth 0 variables-file))
	(setq class (nth 1 variables-file))))
      (when class
	(let ((variables
	       (dir-locals-collect-variables
		(dir-locals-get-class-variables class) dir-name nil)))
	  (when variables
	    (dolist (elt variables)
	      (unless (memq (car elt) '(eval mode))
		(setq dir-local-variables-alist
		      (assq-delete-all (car elt) dir-local-variables-alist)))
	      (push elt dir-local-variables-alist))
	    (hack-local-variables-filter variables dir-name)))))))

(defun hack-dir-local-variables-non-file-buffer ()
  "Apply directory-local variables to a non-file buffer.
For non-file buffers, such as Dired buffers, directory-local
variables are looked for in `default-directory' and its parent
directories."
  (hack-dir-local-variables)
  (hack-local-variables-apply))


(defcustom change-major-mode-with-file-name t
  "Non-nil means \\[write-file] should set the major mode from the file name.
However, the mode will not be changed if
\(1) a local variables list or the `-*-' line specifies a major mode, or
\(2) the current major mode is a \"special\" mode,
\     not suitable for ordinary files, or
\(3) the new file name does not particularly specify any mode."
  :type 'boolean
  :group 'editing-basics)

(defun set-visited-file-name (filename &optional no-query along-with-file)
  "Change name of file visited in current buffer to FILENAME.
This also renames the buffer to correspond to the new file.
The next time the buffer is saved it will go in the newly specified file.
FILENAME nil or an empty string means mark buffer as not visiting any file.
Remember to delete the initial contents of the minibuffer
if you wish to pass an empty string as the argument.

The optional second argument NO-QUERY, if non-nil, inhibits asking for
confirmation in the case where another buffer is already visiting FILENAME.

The optional third argument ALONG-WITH-FILE, if non-nil, means that
the old visited file has been renamed to the new name FILENAME."
  (interactive "FSet visited file name: ")
  (if (buffer-base-buffer)
      (error "An indirect buffer cannot visit a file"))
  (let (truename old-try-locals)
    (if filename
	(setq filename
	      (if (string-equal filename "")
		  nil
		(expand-file-name filename))))
    (if filename
	(progn
	  (setq truename (file-truename filename))
	  (if find-file-visit-truename
	      (setq filename truename))))
    (if filename
	(let ((new-name (file-name-nondirectory filename)))
	  (if (string= new-name "")
	      (error "Empty file name"))))
    (let ((buffer (and filename (find-buffer-visiting filename))))
      (and buffer (not (eq buffer (current-buffer)))
	   (not no-query)
	   (not (y-or-n-p (format "A buffer is visiting %s; proceed? "
				  filename)))
	   (error "Aborted")))
    (or (equal filename buffer-file-name)
	(progn
	  (and filename (lock-buffer filename))
	  (unlock-buffer)))
    (setq old-try-locals (not (inhibit-local-variables-p))
	  buffer-file-name filename)
    (if filename			; make buffer name reflect filename.
	(let ((new-name (file-name-nondirectory buffer-file-name)))
	  (setq default-directory (file-name-directory buffer-file-name))
	  ;; If new-name == old-name, renaming would add a spurious <2>
	  ;; and it's considered as a feature in rename-buffer.
	  (or (string= new-name (buffer-name))
	      (rename-buffer new-name t))))
    (setq buffer-backed-up nil)
    (or along-with-file
	(clear-visited-file-modtime))
    ;; Abbreviate the file names of the buffer.
    (if truename
	(progn
	  (setq buffer-file-truename (abbreviate-file-name truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name truename))))
    (setq buffer-file-number
	  (if filename
	      (nthcdr 10 (file-attributes buffer-file-name))
	    nil))
    ;; write-file-functions is normally used for things like ftp-find-file
    ;; that visit things that are not local files as if they were files.
    ;; Changing to visit an ordinary local file instead should flush the hook.
    (kill-local-variable 'write-file-functions)
    (kill-local-variable 'local-write-file-hooks)
    (kill-local-variable 'revert-buffer-function)
    (kill-local-variable 'backup-inhibited)
    ;; If buffer was read-only because of version control,
    ;; that reason is gone now, so make it writable.
    (if vc-mode
	(setq buffer-read-only nil))
    (kill-local-variable 'vc-mode)
    ;; Turn off backup files for certain file names.
    ;; Since this is a permanent local, the major mode won't eliminate it.
    (and buffer-file-name
	 backup-enable-predicate
	 (not (funcall backup-enable-predicate buffer-file-name))
	 (progn
	   (make-local-variable 'backup-inhibited)
	   (setq backup-inhibited t)))
    (let ((oauto buffer-auto-save-file-name))
      ;; If auto-save was not already on, turn it on if appropriate.
      (if (not buffer-auto-save-file-name)
	  (and buffer-file-name auto-save-default
	       (auto-save-mode t))
	;; If auto save is on, start using a new name.
	;; We deliberately don't rename or delete the old auto save
	;; for the old visited file name.  This is because perhaps
	;; the user wants to save the new state and then compare with the
	;; previous state from the auto save file.
	(setq buffer-auto-save-file-name
	      (make-auto-save-file-name)))
      ;; Rename the old auto save file if any.
      (and oauto buffer-auto-save-file-name
	   (file-exists-p oauto)
	   (rename-file oauto buffer-auto-save-file-name t)))
    (and buffer-file-name
	 (not along-with-file)
	 (set-buffer-modified-p t))
    ;; Update the major mode, if the file name determines it.
    (condition-case nil
	;; Don't change the mode if it is special.
	(or (not change-major-mode-with-file-name)
	    (get major-mode 'mode-class)
	    ;; Don't change the mode if the local variable list specifies it.
	    ;; The file name can influence whether the local variables apply.
	    (and old-try-locals
		 ;; h-l-v also checks it, but might as well be explicit.
		 (not (inhibit-local-variables-p))
		 (hack-local-variables t))
	    ;; TODO consider making normal-mode handle this case.
	    (let ((old major-mode))
	      (set-auto-mode t)
	      (or (eq old major-mode)
		  (hack-local-variables))))
    (error nil))))

(defun write-file (filename &optional confirm)
  "Write current buffer into file FILENAME.
This makes the buffer visit that file, and marks it as not modified.

If you specify just a directory name as FILENAME, that means to use
the default file name but in that directory.  You can also yank
the default file name into the minibuffer to edit it, using \\<minibuffer-local-map>\\[next-history-element].

If the buffer is not already visiting a file, the default file name
for the output file is the buffer name.

If optional second arg CONFIRM is non-nil, this function
asks for confirmation before overwriting an existing file.
Interactively, confirmation is required unless you supply a prefix argument."
;;  (interactive "FWrite file: ")
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file: "
			     nil nil nil nil)
	   (read-file-name "Write file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))
  (or (null filename) (string-equal filename "")
      (progn
	;; If arg is just a directory,
	;; use the default file name, but in that directory.
	(if (file-directory-p filename)
	    (setq filename (concat (file-name-as-directory filename)
				   (file-name-nondirectory
				    (or buffer-file-name (buffer-name))))))
	(and confirm
	     (file-exists-p filename)
	     (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
		 (error "Canceled")))
	(set-visited-file-name filename (not confirm))))
  (set-buffer-modified-p t)
  ;; Make buffer writable if file is writable.
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil))
  (save-buffer)
  ;; It's likely that the VC status at the new location is different from
  ;; the one at the old location.
  (vc-find-file-hook))

(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.

A backup may be done by renaming or by copying; see documentation of
variable `make-backup-files'.  If it's done by renaming, then the file is
no longer accessible under its old name.

The value is non-nil after a backup was made by renaming.
It has the form (MODES SELINUXCONTEXT BACKUPNAME).
MODES is the result of `file-modes' on the original
file; this means that the caller, after saving the buffer, should change
the modes of the new file to agree with the old modes.
SELINUXCONTEXT is the result of `file-selinux-context' on the original
file; this means that the caller, after saving the buffer, should change
the SELinux context of the new file to agree with the old context.
BACKUPNAME is the backup file name, which is the old file renamed."
  (if (and make-backup-files (not backup-inhibited)
	   (not buffer-backed-up)
	   (file-exists-p buffer-file-name)
	   (memq (aref (elt (file-attributes buffer-file-name) 8) 0)
		 '(?- ?l)))
      (let ((real-file-name buffer-file-name)
	    backup-info backupname targets setmodes)
	;; If specified name is a symbolic link, chase it to the target.
	;; Thus we make the backups in the directory where the real file is.
	(setq real-file-name (file-chase-links real-file-name))
	(setq backup-info (find-backup-file-name real-file-name)
	      backupname (car backup-info)
	      targets (cdr backup-info))
	;; (if (file-directory-p buffer-file-name)
	;;     (error "Cannot save buffer in directory %s" buffer-file-name))
	(if backup-info
	    (condition-case ()
		(let ((delete-old-versions
		       ;; If have old versions to maybe delete,
		       ;; ask the user to confirm now, before doing anything.
		       ;; But don't actually delete til later.
		       (and targets
			    (or (eq delete-old-versions t) (eq delete-old-versions nil))
			    (or delete-old-versions
				(y-or-n-p (format "Delete excess backup versions of %s? "
						  real-file-name)))))
		      (modes (file-modes buffer-file-name))
		      (context (file-selinux-context buffer-file-name)))
		  ;; Actually write the back up file.
		  (condition-case ()
		      (if (or file-precious-flag
    ;			      (file-symlink-p buffer-file-name)
			      backup-by-copying
			      ;; Don't rename a suid or sgid file.
			      (and modes (< 0 (logand modes #o6000)))
			      (not (file-writable-p (file-name-directory real-file-name)))
			      (and backup-by-copying-when-linked
				   (> (file-nlinks real-file-name) 1))
			      (and (or backup-by-copying-when-mismatch
				       (integerp backup-by-copying-when-privileged-mismatch))
				   (let ((attr (file-attributes real-file-name)))
				     (and (or backup-by-copying-when-mismatch
					      (and (integerp (nth 2 attr))
						   (integerp backup-by-copying-when-privileged-mismatch)
						   (<= (nth 2 attr) backup-by-copying-when-privileged-mismatch)))
					  (or (nth 9 attr)
					      (not (file-ownership-preserved-p real-file-name)))))))
			  (backup-buffer-copy real-file-name backupname modes context)
			;; rename-file should delete old backup.
			(rename-file real-file-name backupname t)
			(setq setmodes (list modes context backupname)))
		    (file-error
		     ;; If trouble writing the backup, write it in
		     ;; .emacs.d/%backup%.
		     (setq backupname (locate-user-emacs-file "%backup%~"))
		     (message "Cannot write backup file; backing up in %s"
			      backupname)
		     (sleep-for 1)
		     (backup-buffer-copy real-file-name backupname modes context)))
		  (setq buffer-backed-up t)
		  ;; Now delete the old versions, if desired.
		  (if delete-old-versions
		      (while targets
			(condition-case ()
			    (delete-file (car targets))
			  (file-error nil))
			(setq targets (cdr targets))))
		  setmodes)
	    (file-error nil))))))

(defun backup-buffer-copy (from-name to-name modes context)
  (let ((umask (default-file-modes)))
    (unwind-protect
	(progn
	  ;; Create temp files with strict access rights.  It's easy to
	  ;; loosen them later, whereas it's impossible to close the
	  ;; time-window of loose permissions otherwise.
	  (set-default-file-modes ?\700)
	  (when (condition-case nil
		    ;; Try to overwrite old backup first.
		    (copy-file from-name to-name t t t)
		  (error t))
	    (while (condition-case nil
		       (progn
			 (when (file-exists-p to-name)
			   (delete-file to-name))
			 (copy-file from-name to-name nil t t)
			 nil)
		     (file-already-exists t))
	      ;; The file was somehow created by someone else between
	      ;; `delete-file' and `copy-file', so let's try again.
	      ;; rms says "I think there is also a possible race
	      ;; condition for making backup files" (emacs-devel 20070821).
	      nil)))
      ;; Reset the umask.
      (set-default-file-modes umask)))
  (and modes
       (set-file-modes to-name (logand modes #o1777)))
  (and context
       (set-file-selinux-context to-name context)))

(defvar file-name-version-regexp
  "\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)"
  ;; The last ~[[:digit]]+ matches relative versions in git,
  ;; e.g. `foo.js.~HEAD~1~'.
  "Regular expression matching the backup/version part of a file name.
Used by `file-name-sans-versions'.")

(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return file NAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers.
See also `file-name-version-regexp'."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (unless keep-backup-version
                   (string-match (concat file-name-version-regexp "\\'")
                                 name))))))

(defun file-ownership-preserved-p (file)
  "Return t if deleting FILE and rewriting it would preserve the owner."
  (let ((handler (find-file-name-handler file 'file-ownership-preserved-p)))
    (if handler
	(funcall handler 'file-ownership-preserved-p file)
      (let ((attributes (file-attributes file 'integer)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (= (nth 2 attributes) (user-uid))
	    ;; Files created on Windows by Administrator (RID=500)
	    ;; have the Administrators group (RID=544) recorded as
	    ;; their owner.  Rewriting them will still preserve the
	    ;; owner.
	    (and (eq system-type 'windows-nt)
		 (= (user-uid) 500) (= (nth 2 attributes) 544)))))))

(defun file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.',
except that a leading `.', if any, doesn't count."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
	  (if (setq directory (file-name-directory filename))
	      ;; Don't use expand-file-name here; if DIRECTORY is relative,
	      ;; we don't want to expand it.
	      (concat directory (substring file 0 (match-beginning 0)))
	    (substring file 0 (match-beginning 0)))
	filename))))

(defun file-name-extension (filename &optional period)
  "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that follows the last `.',
excluding version numbers and backup suffixes,
except that a leading `.', if any, doesn't count.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

If PERIOD is non-nil, then the returned value includes the period
that delimits the extension, and if FILENAME has no extension,
the value is \"\"."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (and (string-match "\\.[^.]*\\'" file)
	       (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) (if period 0 1)))
        (if period
            "")))))

(defcustom make-backup-file-name-function nil
  "A function to use instead of the default `make-backup-file-name'.
A value of nil gives the default `make-backup-file-name' behavior.

This could be buffer-local to do something special for specific
files.  If you define it, you may need to change `backup-file-name-p'
and `file-name-sans-versions' too.

See also `backup-directory-alist'."
  :group 'backup
  :type '(choice (const :tag "Default" nil)
		 (function :tag "Your function")))

(defcustom backup-directory-alist nil
  "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY).  Backups of files with
names matching REGEXP will be made in DIRECTORY.  DIRECTORY may be
relative or absolute.  If it is absolute, so that all matching files
are backed up into the same directory, the file names in this
directory will be the full name of the file backed up with all
directory separators changed to `!' to prevent clashes.  This will not
work correctly if your filesystem truncates the resulting name.

For the common case of all backups going into one directory, the alist
should contain a single element pairing \".\" with the appropriate
directory name.

If this variable is nil, or it fails to match a filename, the backup
is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'backup
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Backup directory name"))))

(defun normal-backup-enable-predicate (name)
  "Default `backup-enable-predicate' function.
Checks for files in `temporary-file-directory',
`small-temporary-file-directory', and /tmp."
  (not (or (let ((comp (compare-strings temporary-file-directory 0 nil
					name 0 nil)))
	     ;; Directory is under temporary-file-directory.
	     (and (not (eq comp t))
		  (< comp (- (length temporary-file-directory)))))
	   (let ((comp (compare-strings "/tmp" 0 nil
					name 0 nil)))
	     ;; Directory is under /tmp.
	     (and (not (eq comp t))
		  (< comp (- (length "/tmp")))))
	   (if small-temporary-file-directory
	       (let ((comp (compare-strings small-temporary-file-directory
					    0 nil
					    name 0 nil)))
		 ;; Directory is under small-temporary-file-directory.
		 (and (not (eq comp t))
		      (< comp (- (length small-temporary-file-directory)))))))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
Normally this will just be the file's name with `~' appended.
Customization hooks are provided as follows.

If the variable `make-backup-file-name-function' is non-nil, its value
should be a function which will be called with FILE as its argument;
the resulting name is used.

Otherwise a match for FILE is sought in `backup-directory-alist'; see
the documentation of that variable.  If the directory for the backup
doesn't exist, it is created."
  (if make-backup-file-name-function
      (funcall make-backup-file-name-function file)
    (if (and (eq system-type 'ms-dos)
	     (not (msdos-long-file-names)))
	(let ((fn (file-name-nondirectory file)))
	  (concat (file-name-directory file)
		  (or (and (string-match "\\`[^.]+\\'" fn)
			   (concat (match-string 0 fn) ".~"))
		      (and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
			   (concat (match-string 0 fn) "~")))))
      (concat (make-backup-file-name-1 file) "~"))))

(defun make-backup-file-name-1 (file)
  "Subroutine of `make-backup-file-name' and `find-backup-file-name'."
  (let ((alist backup-directory-alist)
	elt backup-directory abs-backup-directory)
    (while alist
      (setq elt (pop alist))
      (if (string-match (car elt) file)
	  (setq backup-directory (cdr elt)
		alist nil)))
    ;; If backup-directory is relative, it should be relative to the
    ;; file's directory.  By expanding explicitly here, we avoid
    ;; depending on default-directory.
    (if backup-directory
	(setq abs-backup-directory
	      (expand-file-name backup-directory
				(file-name-directory file))))
    (if (and abs-backup-directory (not (file-exists-p abs-backup-directory)))
	(condition-case nil
	    (make-directory abs-backup-directory 'parents)
	  (file-error (setq backup-directory nil
			    abs-backup-directory nil))))
    (if (null backup-directory)
	file
      (if (file-name-absolute-p backup-directory)
	  (progn
	    (when (memq system-type '(windows-nt ms-dos cygwin))
	      ;; Normalize DOSish file names: downcase the drive
	      ;; letter, if any, and replace the leading "x:" with
	      ;; "/drive_x".
	      (or (file-name-absolute-p file)
		  (setq file (expand-file-name file))) ; make defaults explicit
	      ;; Replace any invalid file-name characters (for the
	      ;; case of backing up remote files).
	      (setq file (expand-file-name (convert-standard-filename file)))
	      (if (eq (aref file 1) ?:)
		  (setq file (concat "/"
				     "drive_"
				     (char-to-string (downcase (aref file 0)))
				     (if (eq (aref file 2) ?/)
					 ""
				       "/")
				     (substring file 2)))))
	    ;; Make the name unique by substituting directory
	    ;; separators.  It may not really be worth bothering about
	    ;; doubling `!'s in the original name...
	    (expand-file-name
	     (subst-char-in-string
	      ?/ ?!
	      (replace-regexp-in-string "!" "!!" file))
	     backup-directory))
	(expand-file-name (file-name-nondirectory file)
			  (file-name-as-directory abs-backup-directory))))))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
    (string-match "~\\'" file))

(defvar backup-extract-version-start)

;; This is used in various files.
;; The usage of backup-extract-version-start is not very clean,
;; but I can't see a good alternative, so as of now I am leaving it alone.
(defun backup-extract-version (fn)
  "Given the name of a numeric backup file, FN, return the backup number.
Uses the free variable `backup-extract-version-start', whose value should be
the index in the name where the version number begins."
  (if (and (string-match "[0-9]+~/?$" fn backup-extract-version-start)
	   (= (match-beginning 0) backup-extract-version-start))
      (string-to-number (substring fn backup-extract-version-start -1))
      0))

(defun find-backup-file-name (fn)
  "Find a file name for a backup file FN, and suggestions for deletions.
Value is a list whose car is the name for the backup file
and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup.
Uses `backup-directory-alist' in the same way as does
`make-backup-file-name'."
  (let ((handler (find-file-name-handler fn 'find-backup-file-name)))
    ;; Run a handler for this function so that ange-ftp can refuse to do it.
    (if handler
	(funcall handler 'find-backup-file-name fn)
      (if (or (eq version-control 'never)
	      ;; We don't support numbered backups on plain MS-DOS
	      ;; when long file names are unavailable.
	      (and (eq system-type 'ms-dos)
		   (not (msdos-long-file-names))))
	  (list (make-backup-file-name fn))
	(let* ((basic-name (make-backup-file-name-1 fn))
	       (base-versions (concat (file-name-nondirectory basic-name)
				      ".~"))
	       (backup-extract-version-start (length base-versions))
	       (high-water-mark 0)
	       (number-to-delete 0)
	       possibilities deserve-versions-p versions)
	  (condition-case ()
	      (setq possibilities (file-name-all-completions
				   base-versions
				   (file-name-directory basic-name))
		    versions (sort (mapcar #'backup-extract-version
					   possibilities)
				   #'<)
		    high-water-mark (apply 'max 0 versions)
		    deserve-versions-p (or version-control
					   (> high-water-mark 0))
		    number-to-delete (- (length versions)
					kept-old-versions
					kept-new-versions
					-1))
	    (file-error (setq possibilities nil)))
	  (if (not deserve-versions-p)
	      (list (make-backup-file-name fn))
	    (cons (format "%s.~%d~" basic-name (1+ high-water-mark))
		  (if (and (> number-to-delete 0)
			   ;; Delete nothing if there is overflow
			   ;; in the number of versions to keep.
			   (>= (+ kept-new-versions kept-old-versions -1) 0))
		      (mapcar (lambda (n)
				(format "%s.~%d~" basic-name n))
			      (let ((v (nthcdr kept-old-versions versions)))
				(rplacd (nthcdr (1- number-to-delete) v) ())
				v))))))))))

(defun file-nlinks (filename)
  "Return number of names file FILENAME has."
  (car (cdr (file-attributes filename))))

;; (defun file-relative-name (filename &optional directory)
;;   "Convert FILENAME to be relative to DIRECTORY (default: `default-directory').
;; This function returns a relative file name which is equivalent to FILENAME
;; when used with that default directory as the default.
;; If this is impossible (which can happen on MSDOS and Windows
;; when the file name and directory use different drive names)
;; then it returns FILENAME."
;;   (save-match-data
;;     (let ((fname (expand-file-name filename)))
;;       (setq directory (file-name-as-directory
;; 		       (expand-file-name (or directory default-directory))))
;;       ;; On Microsoft OSes, if FILENAME and DIRECTORY have different
;;       ;; drive names, they can't be relative, so return the absolute name.
;;       (if (and (or (eq system-type 'ms-dos)
;; 		   (eq system-type 'cygwin)
;; 		   (eq system-type 'windows-nt))
;; 	       (not (string-equal (substring fname  0 2)
;; 				  (substring directory 0 2))))
;; 	  filename
;; 	(let ((ancestor ".")
;; 	      (fname-dir (file-name-as-directory fname)))
;; 	  (while (and (not (string-match (concat "^" (regexp-quote directory)) fname-dir))
;; 		      (not (string-match (concat "^" (regexp-quote directory)) fname)))
;; 	    (setq directory (file-name-directory (substring directory 0 -1))
;; 		  ancestor (if (equal ancestor ".")
;; 			       ".."
;; 			     (concat "../" ancestor))))
;; 	  ;; Now ancestor is empty, or .., or ../.., etc.
;; 	  (if (string-match (concat "^" (regexp-quote directory)) fname)
;; 	      ;; We matched within FNAME's directory part.
;; 	      ;; Add the rest of FNAME onto ANCESTOR.
;; 	      (let ((rest (substring fname (match-end 0))))
;; 		(if (and (equal ancestor ".")
;; 			 (not (equal rest "")))
;; 		    ;; But don't bother with ANCESTOR if it would give us `./'.
;; 		    rest
;; 		  (concat (file-name-as-directory ancestor) rest)))
;; 	    ;; We matched FNAME's directory equivalent.
;; 	    ancestor))))))

(defun file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: `default-directory').
This function returns a relative file name which is equivalent to FILENAME
when used with that default directory as the default.
If FILENAME and DIRECTORY lie on different machines or on different drives
on a DOS/Windows machine, it returns FILENAME in expanded form."
  (save-match-data
    (setq directory
	  (file-name-as-directory (expand-file-name (or directory
							default-directory))))
    (setq filename (expand-file-name filename))
    (let ((fremote (file-remote-p filename))
          (dremote (file-remote-p directory)))
      (if ;; Conditions for separate trees
	  (or
	   ;; Test for different filesystems on DOS/Windows
	   (and
	    ;; Should `cygwin' really be included here?  --stef
	    (memq system-type '(ms-dos cygwin windows-nt))
	    (or
	     ;; Test for different drive letters
	     (not (eq t (compare-strings filename 0 2 directory 0 2)))
	     ;; Test for UNCs on different servers
	     (not (eq t (compare-strings
			 (progn
			   (if (string-match "\\`//\\([^:/]+\\)/" filename)
			       (match-string 1 filename)
			     ;; Windows file names cannot have ? in
			     ;; them, so use that to detect when
			     ;; neither FILENAME nor DIRECTORY is a
			     ;; UNC.
			     "?"))
			 0 nil
			 (progn
			   (if (string-match "\\`//\\([^:/]+\\)/" directory)
			       (match-string 1 directory)
			     "?"))
			 0 nil t)))))
	   ;; Test for different remote file system identification
	   (not (equal fremote dremote)))
	  filename
        (let ((ancestor ".")
	      (filename-dir (file-name-as-directory filename)))
          (while (not
		  (or
		   (eq t (compare-strings filename-dir nil (length directory)
					  directory nil nil case-fold-search))
		   (eq t (compare-strings filename nil (length directory)
					  directory nil nil case-fold-search))))
            (setq directory (file-name-directory (substring directory 0 -1))
		  ancestor (if (equal ancestor ".")
			       ".."
			     (concat "../" ancestor))))
          ;; Now ancestor is empty, or .., or ../.., etc.
          (if (eq t (compare-strings filename nil (length directory)
				     directory nil nil case-fold-search))
	      ;; We matched within FILENAME's directory part.
	      ;; Add the rest of FILENAME onto ANCESTOR.
	      (let ((rest (substring filename (length directory))))
		(if (and (equal ancestor ".") (not (equal rest "")))
		    ;; But don't bother with ANCESTOR if it would give us `./'.
		    rest
		  (concat (file-name-as-directory ancestor) rest)))
            ;; We matched FILENAME's directory equivalent.
            ancestor))))))

(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.
Variations are described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
Prefixed with one \\[universal-argument], marks this version
 to become a backup when the next save is done.
Prefixed with two \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
Prefixed with three \\[universal-argument]'s, marks this version
 to become a backup when the next save is done,
 and unconditionally makes the previous version into a backup file.

With a numeric argument of 0, never make the previous version
into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 non-nil.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Emacs how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
`dired-kept-versions' controls dired's clean-directory (.) command.
If `delete-old-versions' is nil, system will query user
 before trimming versions.  Otherwise it does it silently.

If `vc-make-backup-files' is nil, which is the default,
 no backup files are made for files managed by version control.
 (This is because the version control system itself records previous versions.)

See the subroutine `basic-save-buffer' for more information."
  (interactive "p")
  (let ((modp (buffer-modified-p))
	(make-backup-files (or (and make-backup-files (not (eq args 0)))
			       (memq args '(16 64)))))
    (and modp (memq args '(16 64)) (setq buffer-backed-up nil))
    ;; We used to display the message below only for files > 50KB, but
    ;; then Rmail-mbox never displays it due to buffer swapping.  If
    ;; the test is ever re-introduced, be sure to handle saving of
    ;; Rmail files.
    (if (and modp (buffer-file-name))
	(message "Saving file %s..." (buffer-file-name)))
    (basic-save-buffer)
    (and modp (memq args '(4 64)) (setq buffer-backed-up nil))))

(defun delete-auto-save-file-if-necessary (&optional force)
  "Delete auto-save file for current buffer if `delete-auto-save-files' is t.
Normally delete only if the file was written by this Emacs since
the last real save, but optional arg FORCE non-nil means delete anyway."
  (and buffer-auto-save-file-name delete-auto-save-files
       (not (string= buffer-file-name buffer-auto-save-file-name))
       (or force (recent-auto-save-p))
       (progn
	 (condition-case ()
	     (delete-file buffer-auto-save-file-name)
	   (file-error nil))
	 (set-buffer-auto-saved))))

(defvar auto-save-hook nil
  "Normal hook run just before auto-saving.")

(defcustom before-save-hook nil
  "Normal hook that is run before a buffer is saved to its file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'files)

(defcustom after-save-hook nil
  "Normal hook that is run after a buffer is saved to its file."
  :options '(executable-make-buffer-file-executable-if-script-p)
  :type 'hook
  :group 'files)

(defvar save-buffer-coding-system nil
  "If non-nil, use this coding system for saving the buffer.
More precisely, use this coding system in place of the
value of `buffer-file-coding-system', when saving the buffer.
Calling `write-region' for any purpose other than saving the buffer
will still use `buffer-file-coding-system'; this variable has no effect
in such cases.")

(make-variable-buffer-local 'save-buffer-coding-system)
(put 'save-buffer-coding-system 'permanent-local t)

(defun basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified.
The hooks `write-contents-functions' and `write-file-functions' get a chance
to do the job of saving; if they do not, then the buffer is saved in
the visited file in the usual way.
Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive)
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (or (buffer-modified-p)
	    ;; handle the case when no modification has been made but
	    ;; the file disappeared since visited
	    (and buffer-file-name
		 (not (file-exists-p buffer-file-name))))
	(let ((recent-save (recent-auto-save-p))
	      setmodes)
	  ;; If buffer has no file name, ask user for one.
	  (or buffer-file-name
	      (let ((filename
		     (expand-file-name
		      (read-file-name "File to save in: ") nil)))
		(if (file-exists-p filename)
		    (if (file-directory-p filename)
			;; Signal an error if the user specified the name of an
			;; existing directory.
			(error "%s is a directory" filename)
		      (unless (y-or-n-p (format "File `%s' exists; overwrite? "
						filename))
			(error "Canceled")))
		  ;; Signal an error if the specified name refers to a
		  ;; non-existing directory.
		  (let ((dir (file-name-directory filename)))
		    (unless (file-directory-p dir)
		      (if (file-exists-p dir)
			  (error "%s is not a directory" dir)
			(error "%s: no such directory" dir)))))
		(set-visited-file-name filename)))
	  (or (verify-visited-file-modtime (current-buffer))
	      (not (file-exists-p buffer-file-name))
	      (yes-or-no-p
	       (format
		"%s has changed since visited or saved.  Save anyway? "
		(file-name-nondirectory buffer-file-name)))
	      (error "Save not confirmed"))
	  (save-restriction
	    (widen)
	    (save-excursion
	      (and (> (point-max) (point-min))
		   (not find-file-literally)
		   (/= (char-after (1- (point-max))) ?\n)
		   (not (and (eq selective-display t)
			     (= (char-after (1- (point-max))) ?\r)))
		   (or (eq require-final-newline t)
		       (eq require-final-newline 'visit-save)
		       (and require-final-newline
			    (y-or-n-p
			     (format "Buffer %s does not end in newline.  Add one? "
				     (buffer-name)))))
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n))))
	    ;; Support VC version backups.
	    (vc-before-save)
	    (run-hooks 'before-save-hook)
	    (or (run-hook-with-args-until-success 'write-contents-functions)
		(run-hook-with-args-until-success 'local-write-file-hooks)
		(run-hook-with-args-until-success 'write-file-functions)
		;; If a hook returned t, file is already "written".
		;; Otherwise, write it the usual way now.
		(setq setmodes (basic-save-buffer-1)))
	    ;; Now we have saved the current buffer.  Let's make sure
	    ;; that buffer-file-coding-system is fixed to what
	    ;; actually used for saving by binding it locally.
	    (if save-buffer-coding-system
		(setq save-buffer-coding-system last-coding-system-used)
	      (setq buffer-file-coding-system last-coding-system-used))
	    (setq buffer-file-number
		  (nthcdr 10 (file-attributes buffer-file-name)))
	    (if setmodes
		(condition-case ()
		    (progn
		      (set-file-modes buffer-file-name (car setmodes))
		      (set-file-selinux-context buffer-file-name (nth 1 setmodes)))
		  (error nil))))
	  ;; If the auto-save file was recent before this command,
	  ;; delete it now.
	  (delete-auto-save-file-if-necessary recent-save)
	  ;; Support VC `implicit' locking.
	  (vc-after-save)
	  (run-hooks 'after-save-hook))
      (message "(No changes need to be saved)"))))

;; This does the "real job" of writing a buffer into its visited file
;; and making a backup file.  This is what is normally done
;; but inhibited if one of write-file-functions returns non-nil.
;; It returns a value (MODES SELINUXCONTEXT BACKUPNAME), like backup-buffer.
(defun basic-save-buffer-1 ()
  (prog1
      (if save-buffer-coding-system
	  (let ((coding-system-for-write save-buffer-coding-system))
	    (basic-save-buffer-2))
	(basic-save-buffer-2))
    (if buffer-file-coding-system-explicit
	(setcar buffer-file-coding-system-explicit last-coding-system-used)
      (setq buffer-file-coding-system-explicit
	    (cons last-coding-system-used nil)))))

;; This returns a value (MODES SELINUXCONTEXT BACKUPNAME), like backup-buffer.
(defun basic-save-buffer-2 ()
  (let (tempsetmodes setmodes)
    (if (not (file-writable-p buffer-file-name))
	(let ((dir (file-name-directory buffer-file-name)))
	  (if (not (file-directory-p dir))
	      (if (file-exists-p dir)
		  (error "%s is not a directory" dir)
		(error "%s: no such directory" dir))
	    (if (not (file-exists-p buffer-file-name))
		(error "Directory %s write-protected" dir)
	      (if (yes-or-no-p
		   (format
		    "File %s is write-protected; try to save anyway? "
		    (file-name-nondirectory
		     buffer-file-name)))
		  (setq tempsetmodes t)
		(error "Attempt to save to a file which you aren't allowed to write"))))))
    (or buffer-backed-up
	(setq setmodes (backup-buffer)))
    (let* ((dir (file-name-directory buffer-file-name))
           (dir-writable (file-writable-p dir)))
      (if (or (and file-precious-flag dir-writable)
              (and break-hardlink-on-save
                   (file-exists-p buffer-file-name)
                   (> (file-nlinks buffer-file-name) 1)
                   (or dir-writable
                       (error (concat (format
                                       "Directory %s write-protected; " dir)
                                      "cannot break hardlink when saving")))))
	  ;; Write temp name, then rename it.
	  ;; This requires write access to the containing dir,
	  ;; which is why we don't try it if we don't have that access.
	  (let ((realname buffer-file-name)
		tempname succeed
		(umask (default-file-modes))
		(old-modtime (visited-file-modtime)))
	    ;; Create temp files with strict access rights.  It's easy to
	    ;; loosen them later, whereas it's impossible to close the
	    ;; time-window of loose permissions otherwise.
	    (unwind-protect
		(progn
		  (clear-visited-file-modtime)
		  (set-default-file-modes ?\700)
		  ;; Try various temporary names.
		  ;; This code follows the example of make-temp-file,
		  ;; but it calls write-region in the appropriate way
		  ;; for saving the buffer.
		  (while (condition-case ()
			     (progn
			       (setq tempname
				     (make-temp-name
				      (expand-file-name "tmp" dir)))
                               ;; Pass in nil&nil rather than point-min&max
                               ;; cause we're saving the whole buffer.
                               ;; write-region-annotate-functions may use it.
			       (write-region nil nil
					     tempname nil  realname
					     buffer-file-truename 'excl)
			       nil)
			   (file-already-exists t))
		    ;; The file was somehow created by someone else between
		    ;; `make-temp-name' and `write-region', let's try again.
		    nil)
		  (setq succeed t))
	      ;; Reset the umask.
	      (set-default-file-modes umask)
	      ;; If we failed, restore the buffer's modtime.
	      (unless succeed
		(set-visited-file-modtime old-modtime)))
	    ;; Since we have created an entirely new file,
	    ;; make sure it gets the right permission bits set.
	    (setq setmodes (or setmodes
 			       (list (or (file-modes buffer-file-name)
					 (logand ?\666 umask))
				     (file-selinux-context buffer-file-name)
				     buffer-file-name)))
	    ;; We succeeded in writing the temp file,
	    ;; so rename it.
	    (rename-file tempname buffer-file-name t))
	;; If file not writable, see if we can make it writable
	;; temporarily while we write it.
	;; But no need to do so if we have just backed it up
	;; (setmodes is set) because that says we're superseding.
	(cond ((and tempsetmodes (not setmodes))
	       ;; Change the mode back, after writing.
	       (setq setmodes (list (file-modes buffer-file-name)
				    (file-selinux-context buffer-file-name)
				    buffer-file-name))
	       (set-file-modes buffer-file-name (logior (car setmodes) 128))
	       (set-file-selinux-context buffer-file-name (nth 1 setmodes)))))
	(let (success)
	  (unwind-protect
	      (progn
                ;; Pass in nil&nil rather than point-min&max to indicate
                ;; we're saving the buffer rather than just a region.
                ;; write-region-annotate-functions may make us of it.
		(write-region nil nil
			      buffer-file-name nil t buffer-file-truename)
		(setq success t))
	    ;; If we get an error writing the new file, and we made
	    ;; the backup by renaming, undo the backing-up.
	    (and setmodes (not success)
		 (progn
		   (rename-file (nth 2 setmodes) buffer-file-name t)
		   (setq buffer-backed-up nil))))))
    setmodes))

(declare-function diff-no-select "diff"
		  (old new &optional switches no-async buf))

(defvar save-some-buffers-action-alist
  `((?\C-r
     ,(lambda (buf)
        (if (not enable-recursive-minibuffers)
            (progn (display-buffer buf)
                   (setq other-window-scroll-buffer buf))
          (view-buffer buf (lambda (_) (exit-recursive-edit)))
          (recursive-edit))
        ;; Return nil to ask about BUF again.
        nil)
     ,(purecopy "view this buffer"))
    (?d ,(lambda (buf)
           (if (null (buffer-file-name buf))
               (message "Not applicable: no file")
             (require 'diff)            ;for diff-no-select.
             (let ((diffbuf (diff-no-select (buffer-file-name buf) buf
                                            nil 'noasync)))
               (if (not enable-recursive-minibuffers)
                   (progn (display-buffer diffbuf)
                          (setq other-window-scroll-buffer diffbuf))
                 (view-buffer diffbuf (lambda (_) (exit-recursive-edit)))
                 (recursive-edit))))
           ;; Return nil to ask about BUF again.
           nil)
	,(purecopy "view changes in this buffer")))
  "ACTION-ALIST argument used in call to `map-y-or-n-p'.")
(put 'save-some-buffers-action-alist 'risky-local-variable t)

(defvar buffer-save-without-query nil
  "Non-nil means `save-some-buffers' should save this buffer without asking.")
(make-variable-buffer-local 'buffer-save-without-query)

(defun save-some-buffers (&optional arg pred)
  "Save some modified file-visiting buffers.  Asks user about each one.
You can answer `y' to save, `n' not to save, `C-r' to look at the
buffer in question with `view-buffer' before deciding or `d' to
view the differences using `diff-buffer-with-file'.

This command first saves any buffers where `buffer-save-without-query' is
non-nil, without asking.

Optional argument (the prefix) non-nil means save all with no questions.
Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a zero-argument function, it indicates for each buffer whether
to consider it or not when called with that buffer current.

See `save-some-buffers-action-alist' if you want to
change the additional actions you can take on files."
  (interactive "P")
  (save-window-excursion
    (let* (queried autosaved-buffers
	   files-done abbrevs-done)
      (dolist (buffer (buffer-list))
	;; First save any buffers that we're supposed to save unconditionally.
	;; That way the following code won't ask about them.
	(with-current-buffer buffer
	  (when (and buffer-save-without-query (buffer-modified-p))
	    (push (buffer-name) autosaved-buffers)
	    (save-buffer))))
      ;; Ask about those buffers that merit it,
      ;; and record the number thus saved.
      (setq files-done
	    (map-y-or-n-p
             (lambda (buffer)
	       ;; Note that killing some buffers may kill others via
	       ;; hooks (e.g. Rmail and its viewing buffer).
	       (and (buffer-live-p buffer)
		    (buffer-modified-p buffer)
                    (not (buffer-base-buffer buffer))
                    (or
                     (buffer-file-name buffer)
                     (and pred
                          (progn
                            (set-buffer buffer)
                            (and buffer-offer-save (> (buffer-size) 0)))))
                    (or (not (functionp pred))
                        (with-current-buffer buffer (funcall pred)))
                    (if arg
                        t
                      (setq queried t)
                      (if (buffer-file-name buffer)
                          (format "Save file %s? "
                                  (buffer-file-name buffer))
                        (format "Save buffer %s? "
                                (buffer-name buffer))))))
             (lambda (buffer)
               (with-current-buffer buffer
                 (save-buffer)))
             (buffer-list)
	     '("buffer" "buffers" "save")
	     save-some-buffers-action-alist))
      ;; Maybe to save abbrevs, and record whether
      ;; we either saved them or asked to.
      (and save-abbrevs abbrevs-changed
	   (progn
	     (if (or arg
		     (eq save-abbrevs 'silently)
		     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
		 (write-abbrev-file nil))
	     ;; Don't keep bothering user if he says no.
	     (setq abbrevs-changed nil)
	     (setq abbrevs-done t)))
      (or queried (> files-done 0) abbrevs-done
	  (cond
	   ((null autosaved-buffers)
	    (message "(No files need saving)"))
	   ((= (length autosaved-buffers) 1)
	    (message "(Saved %s)" (car autosaved-buffers)))
	   (t
	    (message "(Saved %d files: %s)"
		     (length autosaved-buffers)
		     (mapconcat 'identity autosaved-buffers ", "))))))))

(defun not-modified (&optional arg)
  "Mark current buffer as unmodified, not needing to be saved.
With prefix ARG, mark buffer as modified, so \\[save-buffer] will save.

It is not a good idea to use this function in Lisp programs, because it
prints a message in the minibuffer.  Instead, use `set-buffer-modified-p'."
  (interactive "P")
  (message (if arg "Modification-flag set"
	       "Modification-flag cleared"))
  (set-buffer-modified-p arg))

(defun toggle-read-only (&optional arg)
  "Change whether this buffer is read-only.
With prefix argument ARG, make the buffer read-only if ARG is
positive, otherwise make it writable.  If buffer is read-only
and `view-read-only' is non-nil, enter view mode.

This function is usually the wrong thing to use in a Lisp program.
It can have side-effects beyond changing the read-only status of a buffer
\(e.g., enabling view mode), and does not affect read-only regions that
are caused by text properties.  To make a buffer read-only in Lisp code,
set `buffer-read-only'.  To ignore read-only status (whether due to text
properties or buffer state) and make changes, temporarily bind
`inhibit-read-only'."
  (interactive "P")
  (if (and arg
           (if (> (prefix-numeric-value arg) 0) buffer-read-only
             (not buffer-read-only)))  ; If buffer-read-only is set correctly,
      nil			       ; do nothing.
    ;; Toggle.
    (cond
     ((and buffer-read-only view-mode)
      (View-exit-and-edit)
      (make-local-variable 'view-read-only)
      (setq view-read-only t))		; Must leave view mode.
     ((and (not buffer-read-only) view-read-only
	   ;; If view-mode is already active, `view-mode-enter' is a nop.
	   (not view-mode)
           (not (eq (get major-mode 'mode-class) 'special)))
      (view-mode-enter))
     (t (setq buffer-read-only (not buffer-read-only))
        (force-mode-line-update)))))

(defun insert-file (filename)
  "Insert contents of file FILENAME into buffer after point.
Set mark after the inserted text.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents' instead.
\(Its calling sequence is different; see its documentation)."
  (interactive "*fInsert file: ")
  (insert-file-1 filename #'insert-file-contents))

(defun append-to-file (start end filename)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

This does character code conversion and applies annotations
like `write-region' does."
  (interactive "r\nFAppend to file: ")
  (write-region start end filename t))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist."
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
		    (make-backup-file-name (expand-file-name filename))))
	 (file (file-name-nondirectory filename))
	 (dir  (file-name-directory    filename))
	 (comp (file-name-all-completions file dir))
         (newest nil)
         tem)
    (while comp
      (setq tem (pop comp))
      (cond ((and (backup-file-name-p tem)
                  (string= (file-name-sans-versions tem) file))
             (setq tem (concat dir tem))
             (if (or (null newest)
                     (file-newer-than-file-p tem newest))
                 (setq newest tem)))))
    newest))

(defun rename-uniquely ()
  "Rename current buffer to a similar name not already taken.
This function is useful for creating multiple shell process buffers
or multiple mail buffers, etc.

Note that some commands, in particular those based on `compilation-mode'
\(`compile', `grep', etc.) will reuse the current buffer if it has the
appropriate mode even if it has been renamed.  So as well as renaming
the buffer, you also need to switch buffers before running another
instance of such commands."
  (interactive)
  (save-match-data
    (let ((base-name (buffer-name)))
      (and (string-match "<[0-9]+>\\'" base-name)
	   (not (and buffer-file-name
		     (string= base-name
			      (file-name-nondirectory buffer-file-name))))
	   ;; If the existing buffer name has a <NNN>,
	   ;; which isn't part of the file name (if any),
	   ;; then get rid of that.
	   (setq base-name (substring base-name 0 (match-beginning 0))))
      (rename-buffer (generate-new-buffer-name base-name))
      (force-mode-line-update))))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and optionally any nonexistent parent dirs.
If DIR already exists as a directory, signal an error, unless
PARENTS is non-nil.

Interactively, the default choice of directory to create is the
current buffer's default directory.  That is useful when you have
visited a file in a nonexistent directory.

Noninteractively, the second (optional) argument PARENTS, if
non-nil, says whether to create parent directories that don't
exist.  Interactively, this happens by default.

If creating the directory or directories fail, an error will be
raised."
  (interactive
   (list (read-file-name "Make directory: " default-directory default-directory
			 nil nil)
	 t))
  ;; If default-directory is a remote directory,
  ;; make sure we find its make-directory handler.
  (setq dir (expand-file-name dir))
  (let ((handler (find-file-name-handler dir 'make-directory)))
    (if handler
	(funcall handler 'make-directory dir parents)
      (if (not parents)
	  (make-directory-internal dir)
	(let ((dir (directory-file-name (expand-file-name dir)))
	      create-list)
	  (while (and (not (file-exists-p dir))
		      ;; If directory is its own parent, then we can't
		      ;; keep looping forever
		      (not (equal dir
				  (directory-file-name
				   (file-name-directory dir)))))
	    (setq create-list (cons dir create-list)
		  dir (directory-file-name (file-name-directory dir))))
	  (while create-list
	    (make-directory-internal (car create-list))
	    (setq create-list (cdr create-list))))))))

(defconst directory-files-no-dot-files-regexp
  "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regexp matching any file name except \".\" and \"..\".")

(defun delete-directory (directory &optional recursive trash)
  "Delete the directory named DIRECTORY.  Does not follow symlinks.
If RECURSIVE is non-nil, all files in DIRECTORY are deleted as well.
TRASH non-nil means to trash the directory instead, provided
`delete-by-moving-to-trash' is non-nil.

When called interactively, TRASH is t if no prefix argument is
given.  With a prefix argument, TRASH is nil."
  (interactive
   (let* ((trashing (and delete-by-moving-to-trash
			 (null current-prefix-arg)))
	  (dir (expand-file-name
		(read-directory-name
		 (if trashing
		     "Move directory to trash: "
		   "Delete directory: ")
		 default-directory default-directory nil nil))))
     (list dir
	   (if (directory-files	dir nil directory-files-no-dot-files-regexp)
	       (y-or-n-p
		(format "Directory `%s' is not empty, really %s? "
			dir (if trashing "trash" "delete")))
	     nil)
	   (null current-prefix-arg))))
  ;; If default-directory is a remote directory, make sure we find its
  ;; delete-directory handler.
  (setq directory (directory-file-name (expand-file-name directory)))
  (let ((handler (find-file-name-handler directory 'delete-directory)))
    (cond
     (handler
      (funcall handler 'delete-directory directory recursive))
     ((and delete-by-moving-to-trash trash)
      ;; Only move non-empty dir to trash if recursive deletion was
      ;; requested.  This mimics the non-`delete-by-moving-to-trash'
      ;; case, where the operation fails in delete-directory-internal.
      ;; As `move-file-to-trash' trashes directories (empty or
      ;; otherwise) as a unit, we do not need to recurse here.
      (if (and (not recursive)
	       ;; Check if directory is empty apart from "." and "..".
	       (directory-files
		directory 'full directory-files-no-dot-files-regexp))
	  (error "Directory is not empty, not moving to trash")
	(move-file-to-trash directory)))
     ;; Otherwise, call ourselves recursively if needed.
     (t
      (if (and recursive (not (file-symlink-p directory)))
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file recursive nil)
		    (delete-file file nil)))
		;; We do not want to delete "." and "..".
		(directory-files
		 directory 'full directory-files-no-dot-files-regexp)))
      (delete-directory-internal directory)))))

(defun file-equal-p (file1 file2)
  "Return non-nil if files FILE1 and FILE2 name the same file.
If FILE1 or FILE2 does not exist, the return value is unspecified."
  (let ((handler (or (find-file-name-handler file1 'file-equal-p)
                     (find-file-name-handler file2 'file-equal-p))))
    (if handler
        (funcall handler 'file-equal-p file1 file2)
      (let (f1-attr f2-attr)
        (and (setq f1-attr (file-attributes (file-truename file1)))
	     (setq f2-attr (file-attributes (file-truename file2)))
	     (equal f1-attr f2-attr))))))

(defun file-in-directory-p (file dir)
  "Return non-nil if FILE is in DIR or a subdirectory of DIR.
A directory is considered to be \"in\" itself.
Return nil if DIR is not an existing directory."
  (let ((handler (or (find-file-name-handler file 'file-in-directory-p)
                     (find-file-name-handler dir  'file-in-directory-p))))
    (if handler
        (funcall handler 'file-in-directory-p file dir)
      (when (file-directory-p dir) ; DIR must exist.
	(setq file (file-truename file)
	      dir  (file-truename dir))
	(let ((ls1 (split-string file "/" t))
	      (ls2 (split-string dir  "/" t))
	      (root (if (string-match "\\`/" file) "/" ""))
	      (mismatch nil))
	  (while (and ls1 ls2 (not mismatch))
	    (if (string-equal (car ls1) (car ls2))
		(setq root (concat root (car ls1) "/"))
	      (setq mismatch t))
	    (setq ls1 (cdr ls1)
		  ls2 (cdr ls2)))
	  (unless mismatch
	    (file-equal-p root dir)))))))

(defun copy-directory (directory newname &optional keep-time parents copy-contents)
  "Copy DIRECTORY to NEWNAME.  Both args must be strings.
This function always sets the file modes of the output files to match
the corresponding input file.

The third arg KEEP-TIME non-nil means give the output files the same
last-modified time as the old ones.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

Noninteractively, the last argument PARENTS says whether to
create parent directories if they don't exist.  Interactively,
this happens by default.

If NEWNAME names an existing directory, copy DIRECTORY as a
subdirectory there.  However, if called from Lisp with a non-nil
optional argument COPY-CONTENTS, copy the contents of DIRECTORY
directly into NEWNAME instead."
  (interactive
   (let ((dir (read-directory-name
	       "Copy directory: " default-directory default-directory t nil)))
     (list dir
	   (read-directory-name
	    (format "Copy directory %s to: " dir)
	    default-directory default-directory nil nil)
	   current-prefix-arg t nil)))
  (when (file-in-directory-p newname directory)
    (error "Cannot copy `%s' into its subdirectory `%s'"
           directory newname))
  ;; If default-directory is a remote directory, make sure we find its
  ;; copy-directory handler.
  (let ((handler (or (find-file-name-handler directory 'copy-directory)
		     (find-file-name-handler newname 'copy-directory))))
    (if handler
	(funcall handler 'copy-directory directory
                 newname keep-time parents copy-contents)

      ;; Compute target name.
      (setq directory (directory-file-name (expand-file-name directory))
	    newname   (directory-file-name (expand-file-name newname)))

      (cond ((not (file-directory-p newname))
	     ;; If NEWNAME is not an existing directory, create it;
	     ;; that is where we will copy the files of DIRECTORY.
	     (make-directory newname parents))
	    ;; If NEWNAME is an existing directory and COPY-CONTENTS
	    ;; is nil, copy into NEWNAME/[DIRECTORY-BASENAME].
	    ((not copy-contents)
	     (setq newname (expand-file-name
			    (file-name-nondirectory
			     (directory-file-name directory))
			    newname))
	     (and (file-exists-p newname)
		  (not (file-directory-p newname))
		  (error "Cannot overwrite non-directory %s with a directory"
			 newname))
	     (make-directory newname t)))

      ;; Copy recursively.
      (dolist (file
	       ;; We do not want to copy "." and "..".
	       (directory-files directory 'full
				directory-files-no-dot-files-regexp))
	(let ((target (expand-file-name (file-name-nondirectory file) newname))
	      (filetype (car (file-attributes file))))
	  (cond
	   ((eq filetype t)       ; Directory but not a symlink.
	    (copy-directory file newname keep-time parents))
	   ((stringp filetype)    ; Symbolic link
	    (make-symbolic-link filetype target t))
	   ((copy-file file target t keep-time)))))

      ;; Set directory attributes.
      (let ((modes (file-modes directory))
	    (times (and keep-time (nth 5 (file-attributes directory)))))
	(if modes (set-file-modes newname modes))
	(if times (set-file-times newname times))))))

(put 'revert-buffer-function 'permanent-local t)
(defvar revert-buffer-function nil
  "Function to use to revert this buffer, or nil to do the default.
The function receives two arguments IGNORE-AUTO and NOCONFIRM,
which are the arguments that `revert-buffer' received.")

(put 'revert-buffer-insert-file-contents-function 'permanent-local t)
(defvar revert-buffer-insert-file-contents-function nil
  "Function to use to insert contents when reverting this buffer.
Gets two args, first the nominal file name to use,
and second, t if reading the auto-save file.

The function you specify is responsible for updating (or preserving) point.")

(defvar buffer-stale-function nil
  "Function to check whether a non-file buffer needs reverting.
This should be a function with one optional argument NOCONFIRM.
Auto Revert Mode passes t for NOCONFIRM.  The function should return
non-nil if the buffer should be reverted.  A return value of
`fast' means that the need for reverting was not checked, but
that reverting the buffer is fast.  The buffer is current when
this function is called.

The idea behind the NOCONFIRM argument is that it should be
non-nil if the buffer is going to be reverted without asking the
user.  In such situations, one has to be careful with potentially
time consuming operations.

For more information on how this variable is used by Auto Revert mode,
see Info node `(emacs)Supporting additional buffers'.")

(defvar before-revert-hook nil
  "Normal hook for `revert-buffer' to run before reverting.
If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar after-revert-hook nil
  "Normal hook for `revert-buffer' to run after reverting.
Note that the hook value that it runs is the value that was in effect
before reverting; that makes a difference if you have buffer-local
hook functions.

If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar revert-buffer-in-progress-p nil
  "Non-nil if a `revert-buffer' operation is in progress, nil otherwise.
This is true even if a `revert-buffer-function' is being used.")

(defvar revert-buffer-internal-hook)

(defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Replace current buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
With a prefix argument, offer to revert from latest auto-save file, if
that is more recent than the visited file.

This command also implements an interface for special buffers
that contain text which doesn't come from a file, but reflects
some other data instead (e.g. Dired buffers, `buffer-list'
buffers).  This is done via the variable `revert-buffer-function'.
In these cases, it should reconstruct the buffer contents from the
appropriate data.

When called from Lisp, the first argument is IGNORE-AUTO; only offer
to revert from the auto-save file when this is nil.  Note that the
sense of this argument is the reverse of the prefix argument, for the
sake of backward compatibility.  IGNORE-AUTO is optional, defaulting
to nil.

Optional second argument NOCONFIRM means don't ask for confirmation
at all.  (The variable `revert-without-query' offers another way to
revert buffers without querying for confirmation.)

Optional third argument PRESERVE-MODES non-nil means don't alter
the files modes.  Normally we reinitialize them using `normal-mode'.

This function binds `revert-buffer-in-progress-p' non-nil while it operates.

If the value of `revert-buffer-function' is non-nil, it is called to
do all the work for this command.  Otherwise, the hooks
`before-revert-hook' and `after-revert-hook' are run at the beginning
and the end, and if `revert-buffer-insert-file-contents-function' is
non-nil, it is called instead of rereading visited file contents."

  ;; I admit it's odd to reverse the sense of the prefix argument, but
  ;; there is a lot of code out there which assumes that the first
  ;; argument should be t to avoid consulting the auto-save file, and
  ;; there's no straightforward way to encourage authors to notice a
  ;; reversal of the argument sense.  So I'm just changing the user
  ;; interface, but leaving the programmatic interface the same.
  (interactive (list (not current-prefix-arg)))
  (if revert-buffer-function
      (let ((revert-buffer-in-progress-p t))
        (funcall revert-buffer-function ignore-auto noconfirm))
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
			     (current-buffer))
      (let* ((revert-buffer-in-progress-p t)
             (auto-save-p (and (not ignore-auto)
			       (recent-auto-save-p)
			       buffer-auto-save-file-name
			       (file-readable-p buffer-auto-save-file-name)
			       (y-or-n-p
     "Buffer has been auto-saved recently.  Revert from auto-save file? ")))
	     (file-name (if auto-save-p
			    buffer-auto-save-file-name
			  buffer-file-name)))
	(cond ((null file-name)
	       (error "Buffer does not seem to be associated with any file"))
	      ((or noconfirm
		   (and (not (buffer-modified-p))
			(catch 'found
			  (dolist (regexp revert-without-query)
			    (when (string-match regexp file-name)
			      (throw 'found t)))))
		   (yes-or-no-p (format "Revert buffer from file %s? "
					file-name)))
	       (run-hooks 'before-revert-hook)
	       ;; If file was backed up but has changed since,
	       ;; we should make another backup.
	       (and (not auto-save-p)
		    (not (verify-visited-file-modtime (current-buffer)))
		    (setq buffer-backed-up nil))
	       ;; Effectively copy the after-revert-hook status,
	       ;; since after-find-file will clobber it.
	       (let ((global-hook (default-value 'after-revert-hook))
		     (local-hook (when (local-variable-p 'after-revert-hook)
				   after-revert-hook))
		     (inhibit-read-only t))
		 (cond
		  (revert-buffer-insert-file-contents-function
		   (unless (eq buffer-undo-list t)
		     ;; Get rid of all undo records for this buffer.
		     (setq buffer-undo-list nil))
		   ;; Don't make undo records for the reversion.
		   (let ((buffer-undo-list t))
		     (funcall revert-buffer-insert-file-contents-function
			      file-name auto-save-p)))
		  ((not (file-exists-p file-name))
		   (error (if buffer-file-number
			      "File %s no longer exists!"
			    "Cannot revert nonexistent file %s")
			  file-name))
		  ((not (file-readable-p file-name))
		   (error (if buffer-file-number
			      "File %s no longer readable!"
			    "Cannot revert unreadable file %s")
			  file-name))
		  (t
		   ;; Bind buffer-file-name to nil
		   ;; so that we don't try to lock the file.
		   (let ((buffer-file-name nil))
		     (or auto-save-p
			 (unlock-buffer)))
		   (widen)
		   (let ((coding-system-for-read
			  ;; Auto-saved file should be read by Emacs's
			  ;; internal coding.
			  (if auto-save-p 'auto-save-coding
			    (or coding-system-for-read
				(and
				 buffer-file-coding-system-explicit
				 (car buffer-file-coding-system-explicit))))))
		     (if (and (not enable-multibyte-characters)
			      coding-system-for-read
			      (not (memq (coding-system-base
					  coding-system-for-read)
					 '(no-conversion raw-text))))
			 ;; As a coding system suitable for multibyte
			 ;; buffer is specified, make the current
			 ;; buffer multibyte.
			 (set-buffer-multibyte t))

		     ;; This force after-insert-file-set-coding
		     ;; (called from insert-file-contents) to set
		     ;; buffer-file-coding-system to a proper value.
		     (kill-local-variable 'buffer-file-coding-system)

		     ;; Note that this preserves point in an intelligent way.
		     (if preserve-modes
			 (let ((buffer-file-format buffer-file-format))
			   (insert-file-contents file-name (not auto-save-p)
						 nil nil t))
		       (insert-file-contents file-name (not auto-save-p)
					     nil nil t)))))
		 ;; Recompute the truename in case changes in symlinks
		 ;; have changed the truename.
		 (setq buffer-file-truename
		       (abbreviate-file-name (file-truename buffer-file-name)))
		 (after-find-file nil nil t nil preserve-modes)
		 ;; Run after-revert-hook as it was before we reverted.
		 (setq-default revert-buffer-internal-hook global-hook)
		 (if local-hook
		     (set (make-local-variable 'revert-buffer-internal-hook)
			  local-hook)
		   (kill-local-variable 'revert-buffer-internal-hook))
		 (run-hooks 'revert-buffer-internal-hook))
	       t))))))

(defun recover-this-file ()
  "Recover the visited file--get contents from its last auto-save file."
  (interactive)
  (recover-file buffer-file-name))

(defun recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  ;; Actually putting the file name in the minibuffer should be used
  ;; only rarely.
  ;; Not just because users often use the default.
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (if (auto-save-file-name-p (file-name-nondirectory file))
      (error "%s is an auto-save file" (abbreviate-file-name file)))
  (let ((file-name (let ((buffer-file-name file))
		     (make-auto-save-file-name))))
    (cond ((if (file-exists-p file)
	       (not (file-newer-than-file-p file-name file))
	     (not (file-exists-p file-name)))
	   (error "Auto-save file %s not current"
		  (abbreviate-file-name file-name)))
	  ((save-window-excursion
	     (with-output-to-temp-buffer "*Directory*"
	       (buffer-disable-undo standard-output)
	       (save-excursion
		 (let ((switches dired-listing-switches))
		   (if (file-symlink-p file)
		       (setq switches (concat switches " -L")))
		   (set-buffer standard-output)
		   ;; Use insert-directory-safely, not insert-directory,
		   ;; because these files might not exist.  In particular,
		   ;; FILE might not exist if the auto-save file was for
		   ;; a buffer that didn't visit a file, such as "*mail*".
		   ;; The code in v20.x called `ls' directly, so we need
		   ;; to emulate what `ls' did in that case.
		   (insert-directory-safely file switches)
		   (insert-directory-safely file-name switches))))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (switch-to-buffer (find-file-noselect file t))
	   (let ((inhibit-read-only t)
		 ;; Keep the current buffer-file-coding-system.
		 (coding-system buffer-file-coding-system)
		 ;; Auto-saved file should be read with special coding.
		 (coding-system-for-read 'auto-save-coding))
	     (erase-buffer)
	     (insert-file-contents file-name nil)
	     (set-buffer-file-coding-system coding-system))
	   (after-find-file nil nil t))
	  (t (error "Recover-file cancelled")))))

(defun recover-session ()
  "Recover auto save files from a previous Emacs session.
This command first displays a Dired buffer showing you the
previous sessions that you could recover from.
To choose one, move point to the proper line and then type C-c C-c.
Then you'll be asked about a number of files to recover."
  (interactive)
  (if (null auto-save-list-file-prefix)
      (error "You set `auto-save-list-file-prefix' to disable making session files"))
  (let ((dir (file-name-directory auto-save-list-file-prefix)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (directory-files dir nil
			     (concat "\\`" (regexp-quote
					    (file-name-nondirectory
					     auto-save-list-file-prefix)))
			     t)
      (error "No previous sessions to recover")))
  (let ((ls-lisp-support-shell-wildcards t))
    (dired (concat auto-save-list-file-prefix "*")
	   (concat dired-listing-switches " -t")))
  (save-excursion
    (goto-char (point-min))
    (or (looking-at " Move to the session you want to recover,")
	(let ((inhibit-read-only t))
	  ;; Each line starts with a space
	  ;; so that Font Lock mode won't highlight the first character.
	  (insert " Move to the session you want to recover,\n"
		  " then type C-c C-c to select it.\n\n"
		  " You can also delete some of these files;\n"
		  " type d on a line to mark that file for deletion.\n\n"))))
  (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
  (define-key (current-local-map) "\C-c\C-c" 'recover-session-finish))

(defun recover-session-finish ()
  "Choose one saved session to recover auto-save files from.
This command is used in the special Dired buffer created by
\\[recover-session]."
  (interactive)
  ;; Get the name of the session file to recover from.
  (let ((file (dired-get-filename))
	files
	(buffer (get-buffer-create " *recover*")))
    (dired-unmark 1)
    (dired-do-flagged-delete t)
    (unwind-protect
	(with-current-buffer buffer
	  ;; Read in the auto-save-list file.
	  (erase-buffer)
	  (insert-file-contents file)
	  ;; Loop thru the text of that file
	  ;; and get out the names of the files to recover.
	  (while (not (eobp))
	    (let (thisfile autofile)
	      (if (eolp)
		  ;; This is a pair of lines for a non-file-visiting buffer.
		  ;; Get the auto-save file name and manufacture
		  ;; a "visited file name" from that.
		  (progn
		    (forward-line 1)
		    ;; If there is no auto-save file name, the
		    ;; auto-save-list file is probably corrupted.
		    (unless (eolp)
		      (setq autofile
			    (buffer-substring-no-properties
			     (point)
			     (line-end-position)))
		      (setq thisfile
			    (expand-file-name
			     (substring
			      (file-name-nondirectory autofile)
			      1 -1)
			     (file-name-directory autofile))))
		    (forward-line 1))
		;; This pair of lines is a file-visiting
		;; buffer.  Use the visited file name.
		(progn
		  (setq thisfile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)
		  (setq autofile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)))
	      ;; Ignore a file if its auto-save file does not exist now.
	      (if (and autofile (file-exists-p autofile))
		  (setq files (cons thisfile files)))))
	  (setq files (nreverse files))
	  ;; The file contains a pair of line for each auto-saved buffer.
	  ;; The first line of the pair contains the visited file name
	  ;; or is empty if the buffer was not visiting a file.
	  ;; The second line is the auto-save file name.
	  (if files
	      (map-y-or-n-p  "Recover %s? "
			     (lambda (file)
			       (condition-case nil
				   (save-excursion (recover-file file))
				 (error
				  "Failed to recover `%s'" file)))
			     files
			     '("file" "files" "recover"))
	    (message "No files can be recovered from this session now")))
      (kill-buffer buffer))))

(defun kill-buffer-ask (buffer)
  "Kill BUFFER if confirmed."
  (when (yes-or-no-p (format "Buffer %s %s.  Kill? "
			     (buffer-name buffer)
			     (if (buffer-modified-p buffer)
				 "HAS BEEN EDITED" "is unmodified")))
    (kill-buffer buffer)))

(defun kill-some-buffers (&optional list)
  "Kill some buffers.  Asks the user whether to kill each one of them.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and name				; Can be nil for an indirect buffer
					; if we killed the base buffer.
	   (not (string-equal name ""))
	   (/= (aref name 0) ?\s)
	   (kill-buffer-ask buffer)))
    (setq list (cdr list))))

(defun kill-matching-buffers (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP.
The optional second argument indicates whether to kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer-ask buffer)))))


(defun rename-auto-save-file ()
  "Adjust current buffer's auto save file name for current conditions.
Also rename any existing auto save file, if it was made in this session."
  (let ((osave buffer-auto-save-file-name))
    (setq buffer-auto-save-file-name
	  (make-auto-save-file-name))
    (if (and osave buffer-auto-save-file-name
	     (not (string= buffer-auto-save-file-name buffer-file-name))
	     (not (string= buffer-auto-save-file-name osave))
	     (file-exists-p osave)
	     (recent-auto-save-p))
	(rename-file osave buffer-auto-save-file-name t))))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      (let ((handler (find-file-name-handler buffer-file-name
					     'make-auto-save-file-name)))
	(if handler
	    (funcall handler 'make-auto-save-file-name)
	  (let ((list auto-save-file-name-transforms)
		(filename buffer-file-name)
		result uniq)
	    ;; Apply user-specified translations
	    ;; to the file name.
	    (while (and list (not result))
	      (if (string-match (car (car list)) filename)
		  (setq result (replace-match (cadr (car list)) t nil
					      filename)
			uniq (car (cddr (car list)))))
	      (setq list (cdr list)))
	    (if result
		(if uniq
		    (setq filename (concat
				    (file-name-directory result)
				    (subst-char-in-string
				     ?/ ?!
				     (replace-regexp-in-string "!" "!!"
							       filename))))
		  (setq filename result)))
	    (setq result
		  (if (and (eq system-type 'ms-dos)
			   (not (msdos-long-file-names)))
		      ;; We truncate the file name to DOS 8+3 limits
		      ;; before doing anything else, because the regexp
		      ;; passed to string-match below cannot handle
		      ;; extensions longer than 3 characters, multiple
		      ;; dots, and other atrocities.
		      (let ((fn (dos-8+3-filename
				 (file-name-nondirectory buffer-file-name))))
			(string-match
			 "\\`\\([^.]+\\)\\(\\.\\(..?\\)?.?\\|\\)\\'"
			 fn)
			(concat (file-name-directory buffer-file-name)
				"#" (match-string 1 fn)
				"." (match-string 3 fn) "#"))
		    (concat (file-name-directory filename)
			    "#"
			    (file-name-nondirectory filename)
			    "#")))
	    ;; Make sure auto-save file names don't contain characters
	    ;; invalid for the underlying filesystem.
	    (if (and (memq system-type '(ms-dos windows-nt cygwin))
		     ;; Don't modify remote (ange-ftp) filenames
		     (not (string-match "^/\\w+@[-A-Za-z0-9._]+:" result)))
		(convert-standard-filename result)
	      result))))

    ;; Deal with buffers that don't have any associated files.  (Mail
    ;; mode tends to create a good number of these.)

    (let ((buffer-name (buffer-name))
	  (limit 0)
	  file-name)
      ;; Restrict the characters used in the file name to those which
      ;; are known to be safe on all filesystems, url-encoding the
      ;; rest.
      ;; We do this on all platforms, because even if we are not
      ;; running on DOS/Windows, the current directory may be on a
      ;; mounted VFAT filesystem, such as a USB memory stick.
      (while (string-match "[^A-Za-z0-9-_.~#+]" buffer-name limit)
	(let* ((character (aref buffer-name (match-beginning 0)))
	       (replacement
                ;; For multibyte characters, this will produce more than
                ;; 2 hex digits, so is not true URL encoding.
                (format "%%%02X" character)))
	  (setq buffer-name (replace-match replacement t t buffer-name))
	  (setq limit (1+ (match-end 0)))))
      ;; Generate the file name.
      (setq file-name
	    (make-temp-file
	     (let ((fname
		    (expand-file-name
		     (format "#%s#" buffer-name)
		     ;; Try a few alternative directories, to get one we can
		     ;; write it.
		     (cond
		      ((file-writable-p default-directory) default-directory)
		      ((file-writable-p "/var/tmp/") "/var/tmp/")
		      ("~/")))))
	       (if (and (memq system-type '(ms-dos windows-nt cygwin))
			;; Don't modify remote (ange-ftp) filenames
			(not (string-match "^/\\w+@[-A-Za-z0-9._]+:" fname)))
		   ;; The call to convert-standard-filename is in case
		   ;; buffer-name includes characters not allowed by the
		   ;; DOS/Windows filesystems.  make-temp-file writes to the
		   ;; file it creates, so we must fix the file name _before_
		   ;; make-temp-file is called.
		   (convert-standard-filename fname)
		 fname))
	     nil "#"))
      ;; make-temp-file creates the file,
      ;; but we don't want it to exist until we do an auto-save.
      (condition-case ()
	  (delete-file file-name)
	(file-error nil))
      file-name)))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.  You can redefine this for customization."
  (string-match "^#.*#$" filename))

(defun wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a filename only if the filename
matches that wildcard according to shell rules.  Only wildcards known
by `sh' are supported."
  (let* ((i (string-match "[[.*+\\^$?]" wildcard))
	 ;; Copy the initial run of non-special characters.
	 (result (substring wildcard 0 i))
	 (len (length wildcard)))
    ;; If no special characters, we're almost done.
    (if i
	(while (< i len)
	  (let ((ch (aref wildcard i))
		j)
	    (setq
	     result
	     (concat result
		     (cond
		      ((and (eq ch ?\[)
			    (< (1+ i) len)
			    (eq (aref wildcard (1+ i)) ?\]))
		       "\\[")
		      ((eq ch ?\[)	; [...] maps to regexp char class
		       (progn
			 (setq i (1+ i))
			 (concat
			  (cond
			   ((eq (aref wildcard i) ?!) ; [!...] -> [^...]
			    (progn
			      (setq i (1+ i))
			      (if (eq (aref wildcard i) ?\])
				  (progn
				    (setq i (1+ i))
				    "[^]")
				"[^")))
			   ((eq (aref wildcard i) ?^)
			    ;; Found "[^".  Insert a `\0' character
			    ;; (which cannot happen in a filename)
			    ;; into the character class, so that `^'
			    ;; is not the first character after `[',
			    ;; and thus non-special in a regexp.
			    (progn
			      (setq i (1+ i))
			      "[\000^"))
			   ((eq (aref wildcard i) ?\])
			    ;; I don't think `]' can appear in a
			    ;; character class in a wildcard, but
			    ;; let's be general here.
			    (progn
			      (setq i (1+ i))
			      "[]"))
			   (t "["))
			  (prog1	; copy everything upto next `]'.
			      (substring wildcard
					 i
					 (setq j (string-match
						  "]" wildcard i)))
			    (setq i (if j (1- j) (1- len)))))))
		      ((eq ch ?.)  "\\.")
		      ((eq ch ?*)  "[^\000]*")
		      ((eq ch ?+)  "\\+")
		      ((eq ch ?^)  "\\^")
		      ((eq ch ?$)  "\\$")
		      ((eq ch ?\\) "\\\\") ; probably cannot happen...
		      ((eq ch ??)  "[^\000]")
		      (t (char-to-string ch)))))
	    (setq i (1+ i)))))
    ;; Shell wildcards should match the entire filename,
    ;; not its part.  Make the regexp say so.
    (concat "\\`" result "\\'")))

(defcustom list-directory-brief-switches
  (purecopy "-CF")
  "Switches for `list-directory' to pass to `ls' for brief listing."
  :type 'string
  :group 'dired)

(defcustom list-directory-verbose-switches
    (purecopy "-l")
  "Switches for `list-directory' to pass to `ls' for verbose listing."
  :type 'string
  :group 'dired)

(defun file-expand-wildcards (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names which match the pattern.

If PATTERN is written as an absolute file name,
the values are absolute also.

If PATTERN is written as a relative file name, it is interpreted
relative to the current default directory, `default-directory'.
The file names returned are normally also relative to the current
default directory.  However, if FULL is non-nil, they are absolute."
  (save-match-data
    (let* ((nondir (file-name-nondirectory pattern))
	   (dirpart (file-name-directory pattern))
	   ;; A list of all dirs that DIRPART specifies.
	   ;; This can be more than one dir
	   ;; if DIRPART contains wildcards.
	   (dirs (if (and dirpart
			  (string-match "[[*?]"
					(or (file-remote-p dirpart 'localname)
					    dirpart)))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (while dirs
	(when (or (null (car dirs))	; Possible if DIRPART is not wild.
		  (and (file-directory-p (directory-file-name (car dirs)))
		       (file-readable-p (car dirs))))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
				   (unless (string-match "\\`\\.\\.?\\'"
							 (file-name-nondirectory name))
				     name))
			       (directory-files (or (car dirs) ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and (car dirs) (not full))
		       (mapcar (function (lambda (name) (concat (car dirs) name)))
			       this-dir-contents)
		     this-dir-contents)
		   contents))))
	(setq dirs (cdr dirs)))
      contents)))

;; Let Tramp know that `file-expand-wildcards' does not need an advice.
(provide 'files '(remote-wildcards))

(defun list-directory (dirname &optional verbose)
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables `list-directory-brief-switches'
and `list-directory-verbose-switches'."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-directory-name (if pfx "List directory (verbose): "
					 "List directory (brief): ")
				       nil default-directory nil)
		       pfx)))
  (let ((switches (if verbose list-directory-verbose-switches
		    list-directory-brief-switches))
	buffer)
    (or dirname (setq dirname default-directory))
    (setq dirname (expand-file-name dirname))
    (with-output-to-temp-buffer "*Directory*"
      (setq buffer standard-output)
      (buffer-disable-undo standard-output)
      (princ "Directory ")
      (princ dirname)
      (terpri)
      (with-current-buffer "*Directory*"
	(let ((wildcard (not (file-directory-p dirname))))
	  (insert-directory dirname switches wildcard (not wildcard)))))
    ;; Finishing with-output-to-temp-buffer seems to clobber default-directory.
    (with-current-buffer buffer
      (setq default-directory
	    (if (file-directory-p dirname)
		(file-name-as-directory dirname)
	      (file-name-directory dirname))))))

(defun shell-quote-wildcard-pattern (pattern)
  "Quote characters special to the shell in PATTERN, leave wildcards alone.

PATTERN is assumed to represent a file-name wildcard suitable for the
underlying filesystem.  For Unix and GNU/Linux, each character from the
set [ \\t\\n;<>&|()'\"#$] is quoted with a backslash; for DOS/Windows, all
the parts of the pattern which don't include wildcard characters are
quoted with double quotes.

This function leaves alone existing quote characters (\\ on Unix and \"
on Windows), so PATTERN can use them to quote wildcard characters that
need to be passed verbatim to shell commands."
  (save-match-data
    (cond
     ((memq system-type '(ms-dos windows-nt cygwin))
      ;; DOS/Windows don't allow `"' in file names.  So if the
      ;; argument has quotes, we can safely assume it is already
      ;; quoted by the caller.
      (if (or (string-match "[\"]" pattern)
	      ;; We quote [&()#$'] in case their shell is a port of a
	      ;; Unixy shell.  We quote [,=+] because stock DOS and
	      ;; Windows shells require that in some cases, such as
	      ;; passing arguments to batch files that use positional
	      ;; arguments like %1.
	      (not (string-match "[ \t;&()#$',=+]" pattern)))
	  pattern
	(let ((result "\"")
	      (beg 0)
	      end)
	  (while (string-match "[*?]+" pattern beg)
	    (setq end (match-beginning 0)
		  result (concat result (substring pattern beg end)
				 "\""
				 (substring pattern end (match-end 0))
				 "\"")
		  beg (match-end 0)))
	  (concat result (substring pattern beg) "\""))))
     (t
      (let ((beg 0))
	(while (string-match "[ \t\n;<>&|()'\"#$]" pattern beg)
	  (setq pattern
		(concat (substring pattern 0 (match-beginning 0))
			"\\"
			(substring pattern (match-beginning 0)))
		beg (1+ (match-end 0)))))
      pattern))))


(defvar insert-directory-program (purecopy "ls")
  "Absolute or relative name of the `ls' program used by `insert-directory'.")

(defcustom directory-free-space-program (purecopy "df")
  "Program to get the amount of free space on a file system.
We assume the output has the format of `df'.
The value of this variable must be just a command name or file name;
if you want to specify options, use `directory-free-space-args'.

A value of nil disables this feature.

If the function `file-system-info' is defined, it is always used in
preference to the program given by this variable."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'dired)

(defcustom directory-free-space-args
  (purecopy (if (eq system-type 'darwin) "-k" "-Pk"))
  "Options to use when running `directory-free-space-program'."
  :type 'string
  :group 'dired)

(defun get-free-disk-space (dir)
  "Return the amount of free space on directory DIR's file system.
The return value is a string describing the amount of free
space (normally, the number of free 1KB blocks).

This function calls `file-system-info' if it is available, or
invokes the program specified by `directory-free-space-program'
and `directory-free-space-args'.  If the system call or program
is unsuccessful, or if DIR is a remote directory, this function
returns nil."
  (unless (file-remote-p dir)
    ;; Try to find the number of free blocks.  Non-Posix systems don't
    ;; always have df, but might have an equivalent system call.
    (if (fboundp 'file-system-info)
	(let ((fsinfo (file-system-info dir)))
	  (if fsinfo
	      (format "%.0f" (/ (nth 2 fsinfo) 1024))))
      (setq dir (expand-file-name dir))
      (save-match-data
	(with-temp-buffer
	  (when (and directory-free-space-program
		     ;; Avoid failure if the default directory does
		     ;; not exist (Bug#2631, Bug#3911).
		     (let ((default-directory "/"))
		       (eq (call-process directory-free-space-program
					 nil t nil
					 directory-free-space-args
					 dir)
			   0)))
	    ;; Assume that the "available" column is before the
	    ;; "capacity" column.  Find the "%" and scan backward.
	    (goto-char (point-min))
	    (forward-line 1)
	    (when (re-search-forward
		   "[[:space:]]+[^[:space:]]+%[^%]*$"
		   (line-end-position) t)
	      (goto-char (match-beginning 0))
	      (let ((endpt (point)))
		(skip-chars-backward "^[:space:]")
		(buffer-substring-no-properties (point) endpt)))))))))

;; The following expression replaces `dired-move-to-filename-regexp'.
(defvar directory-listing-before-filename-regexp
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
	 (l-or-quote "\\([A-Za-z']\\|[^\0-\177]\\)")
	 ;; In some locales, month abbreviations are as short as 2 letters,
	 ;; and they can be followed by ".".
	 ;; In Breton, a month name  can include a quote character.
	 (month (concat l-or-quote l-or-quote "+\\.?"))
	 (s " ")
	 (yyyy "[0-9][0-9][0-9][0-9]")
	 (dd "[ 0-3][0-9]")
	 (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
	 (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
	 (zone "[-+][0-2][0-9][0-5][0-9]")
	 (iso-mm-dd "[01][0-9]-[0-3][0-9]")
	 (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
	 (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
		      "\\|" yyyy "-" iso-mm-dd "\\)"))
	 (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
			  s "+"
			  "\\(" HH:MM "\\|" yyyy "\\)"))
	 (western-comma (concat month s "+" dd "," s "+" yyyy))
	 ;; Japanese MS-Windows ls-lisp has one-digit months, and
	 ;; omits the Kanji characters after month and day-of-month.
	 ;; On Mac OS X 10.3, the date format in East Asian locales is
	 ;; day-of-month digits followed by month digits.
	 (mm "[ 0-1]?[0-9]")
	 (east-asian
	  (concat "\\(" mm l "?" s dd l "?" s "+"
		  "\\|" dd s mm s "+" "\\)"
		  "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
	 ;; The "[0-9]" below requires the previous column to end in a digit.
	 ;; This avoids recognizing `1 may 1997' as a date in the line:
	 ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README

	 ;; The "[BkKMGTPEZY]?" below supports "ls -alh" output.

	 ;; For non-iso date formats, we add the ".*" in order to find
	 ;; the last possible match.  This avoids recognizing
	 ;; `jservice 10 1024' as a date in the line:
	 ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host

         ;; vc dired listings provide the state or blanks between file
         ;; permissions and date.  The state is always surrounded by
         ;; parentheses:
         ;; -rw-r--r-- (modified) 2005-10-22 21:25 files.el
         ;; This is not supported yet.
    (purecopy (concat "\\([0-9][BkKMGTPEZY]? " iso
		      "\\|.*[0-9][BkKMGTPEZY]? "
		      "\\(" western "\\|" western-comma "\\|" east-asian "\\)"
		      "\\) +")))
  "Regular expression to match up to the file name in a directory listing.
The default value is designed to recognize dates and times
regardless of the language.")

(defvar insert-directory-ls-version 'unknown)

;; insert-directory
;; - must insert _exactly_one_line_ describing FILE if WILDCARD and
;;   FULL-DIRECTORY-P is nil.
;;   The single line of output must display FILE's name as it was
;;   given, namely, an absolute path name.
;; - must insert exactly one line for each file if WILDCARD or
;;   FULL-DIRECTORY-P is t, plus one optional "total" line
;;   before the file lines, plus optional text after the file lines.
;;   Lines are delimited by "\n", so filenames containing "\n" are not
;;   allowed.
;;   File lines should display the basename.
;; - must be consistent with
;;   - functions dired-move-to-filename, (these two define what a file line is)
;;   		 dired-move-to-end-of-filename,
;;		 dired-between-files, (shortcut for (not (dired-move-to-filename)))
;;   		 dired-insert-headerline
;;   		 dired-after-subdir-garbage (defines what a "total" line is)
;;   - variable dired-subdir-regexp
;; - may be passed "--dired" as the first argument in SWITCHES.
;;   Filename handlers might have to remove this switch if their
;;   "ls" command does not support it.
(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings
representing individual options.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'.

When SWITCHES contains the long `--dired' option, this function
treats it specially, for the sake of dired.  However, the
normally equivalent short `-D' option is just passed on to
`insert-directory-program', as any other option."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
	(let (result (beg (point)))

	  ;; Read the actual directory using `insert-directory-program'.
	  ;; RESULT gets the status code.
	  (let* (;; We at first read by no-conversion, then after
		 ;; putting text property `dired-filename, decode one
		 ;; bunch by one to preserve that property.
		 (coding-system-for-read 'no-conversion)
		 ;; This is to control encoding the arguments in call-process.
		 (coding-system-for-write
		  (and enable-multibyte-characters
		       (or file-name-coding-system
			   default-file-name-coding-system))))
	    (setq result
		  (if wildcard
		      ;; Run ls in the directory part of the file pattern
		      ;; using the last component as argument.
		      (let ((default-directory
			      (if (file-name-absolute-p file)
				  (file-name-directory file)
				(file-name-directory (expand-file-name file))))
			    (pattern (file-name-nondirectory file)))
			;; NB since switches is passed to the shell, be
			;; careful of malicious values, eg "-l;reboot".
			;; See eg dired-safe-switches-p.
			(call-process
			 shell-file-name nil t nil
			 "-c"
			 (concat (if (memq system-type '(ms-dos windows-nt))
				     ""
				   "\\") ; Disregard Unix shell aliases!
				 insert-directory-program
				 " -d "
				 (if (stringp switches)
				     switches
				   (mapconcat 'identity switches " "))
				 " -- "
				 ;; Quote some characters that have
				 ;; special meanings in shells; but
				 ;; don't quote the wildcards--we want
				 ;; them to be special.  We also
				 ;; currently don't quote the quoting
				 ;; characters in case people want to
				 ;; use them explicitly to quote
				 ;; wildcard characters.
				 (shell-quote-wildcard-pattern pattern))))
		    ;; SunOS 4.1.3, SVr4 and others need the "." to list the
		    ;; directory if FILE is a symbolic link.
 		    (unless full-directory-p
 		      (setq switches
 			    (if (stringp switches)
 				(concat switches " -d")
 			      (add-to-list 'switches "-d" 'append))))
		    (apply 'call-process
			   insert-directory-program nil t nil
			   (append
			    (if (listp switches) switches
			      (unless (equal switches "")
				;; Split the switches at any spaces so we can
				;; pass separate options as separate args.
				(split-string switches)))
			    ;; Avoid lossage if FILE starts with `-'.
			    '("--")
			    (progn
			      (if (string-match "\\`~" file)
				  (setq file (expand-file-name file)))
			      (list
			       (if full-directory-p
				   (concat (file-name-as-directory file) ".")
				 file))))))))

	  ;; If we got "//DIRED//" in the output, it means we got a real
	  ;; directory listing, even if `ls' returned nonzero.
	  ;; So ignore any errors.
	  (when (if (stringp switches)
		    (string-match "--dired\\>" switches)
		  (member "--dired" switches))
	    (save-excursion
	      (forward-line -2)
	      (when (looking-at "//SUBDIRED//")
		(forward-line -1))
	      (if (looking-at "//DIRED//")
		  (setq result 0))))

	  (when (and (not (eq 0 result))
		     (eq insert-directory-ls-version 'unknown))
	    ;; The first time ls returns an error,
	    ;; find the version numbers of ls,
	    ;; and set insert-directory-ls-version
	    ;; to > if it is more than 5.2.1, < if it is less, nil if it
	    ;; is equal or if the info cannot be obtained.
	    ;; (That can mean it isn't GNU ls.)
	    (let ((version-out
		   (with-temp-buffer
		     (call-process "ls" nil t nil "--version")
		     (buffer-string))))
	      (if (string-match "ls (.*utils) \\([0-9.]*\\)$" version-out)
		  (let* ((version (match-string 1 version-out))
			 (split (split-string version "[.]"))
			 (numbers (mapcar 'string-to-number split))
			 (min '(5 2 1))
			 comparison)
		    (while (and (not comparison) (or numbers min))
		      (cond ((null min)
			     (setq comparison '>))
			    ((null numbers)
			     (setq comparison '<))
			    ((> (car numbers) (car min))
			     (setq comparison '>))
			    ((< (car numbers) (car min))
			     (setq comparison '<))
			    (t
			     (setq numbers (cdr numbers)
				   min (cdr min)))))
		    (setq insert-directory-ls-version (or comparison '=)))
		(setq insert-directory-ls-version nil))))

	  ;; For GNU ls versions 5.2.2 and up, ignore minor errors.
	  (when (and (eq 1 result) (eq insert-directory-ls-version '>))
	    (setq result 0))

	  ;; If `insert-directory-program' failed, signal an error.
	  (unless (eq 0 result)
	    ;; Delete the error message it may have output.
	    (delete-region beg (point))
	    ;; On non-Posix systems, we cannot open a directory, so
	    ;; don't even try, because that will always result in
	    ;; the ubiquitous "Access denied".  Instead, show the
	    ;; command line so the user can try to guess what went wrong.
	    (if (and (file-directory-p file)
		     (memq system-type '(ms-dos windows-nt)))
		(error
		 "Reading directory: \"%s %s -- %s\" exited with status %s"
		 insert-directory-program
		 (if (listp switches) (concat switches) switches)
		 file result)
	      ;; Unix.  Access the file to get a suitable error.
	      (access-file file "Reading directory")
	      (error "Listing directory failed but `access-file' worked")))

	  (when (if (stringp switches)
		    (string-match "--dired\\>" switches)
		  (member "--dired" switches))
	    ;; The following overshoots by one line for an empty
	    ;; directory listed with "--dired", but without "-a"
	    ;; switch, where the ls output contains a
	    ;; "//DIRED-OPTIONS//" line, but no "//DIRED//" line.
	    ;; We take care of that case later.
	    (forward-line -2)
            (when (looking-at "//SUBDIRED//")
              (delete-region (point) (progn (forward-line 1) (point)))
              (forward-line -1))
	    (if (looking-at "//DIRED//")
		(let ((end (line-end-position))
		      (linebeg (point))
		      error-lines)
		  ;; Find all the lines that are error messages,
		  ;; and record the bounds of each one.
		  (goto-char beg)
		  (while (< (point) linebeg)
		    (or (eql (following-char) ?\s)
			(push (list (point) (line-end-position)) error-lines))
		    (forward-line 1))
		  (setq error-lines (nreverse error-lines))
		  ;; Now read the numeric positions of file names.
		  (goto-char linebeg)
		  (forward-word 1)
		  (forward-char 3)
		  (while (< (point) end)
		    (let ((start (insert-directory-adj-pos
				  (+ beg (read (current-buffer)))
				  error-lines))
			  (end (insert-directory-adj-pos
				(+ beg (read (current-buffer)))
				error-lines)))
		      (if (memq (char-after end) '(?\n ?\s))
			  ;; End is followed by \n or by " -> ".
			  (put-text-property start end 'dired-filename t)
			;; It seems that we can't trust ls's output as to
			;; byte positions of filenames.
			(put-text-property beg (point) 'dired-filename nil)
			(end-of-line))))
		  (goto-char end)
		  (beginning-of-line)
		  (delete-region (point) (progn (forward-line 1) (point))))
	      ;; Take care of the case where the ls output contains a
	      ;; "//DIRED-OPTIONS//"-line, but no "//DIRED//"-line
	      ;; and we went one line too far back (see above).
	      (forward-line 1))
	    (if (looking-at "//DIRED-OPTIONS//")
		(delete-region (point) (progn (forward-line 1) (point)))))

	  ;; Now decode what read if necessary.
	  (let ((coding (or coding-system-for-read
			    file-name-coding-system
			    default-file-name-coding-system
			    'undecided))
		coding-no-eol
		val pos)
	    (when (and enable-multibyte-characters
		       (not (memq (coding-system-base coding)
				  '(raw-text no-conversion))))
	      ;; If no coding system is specified or detection is
	      ;; requested, detect the coding.
	      (if (eq (coding-system-base coding) 'undecided)
		  (setq coding (detect-coding-region beg (point) t)))
	      (if (not (eq (coding-system-base coding) 'undecided))
		  (save-restriction
		    (setq coding-no-eol
			  (coding-system-change-eol-conversion coding 'unix))
		    (narrow-to-region beg (point))
		    (goto-char (point-min))
		    (while (not (eobp))
		      (setq pos (point)
			    val (get-text-property (point) 'dired-filename))
		      (goto-char (next-single-property-change
				  (point) 'dired-filename nil (point-max)))
		      ;; Force no eol conversion on a file name, so
		      ;; that CR is preserved.
		      (decode-coding-region pos (point)
					    (if val coding-no-eol coding))
		      (if val
			  (put-text-property pos (point)
					     'dired-filename t)))))))

	  (if full-directory-p
	      ;; Try to insert the amount of free space.
	      (save-excursion
		(goto-char beg)
		;; First find the line to put it on.
		(when (re-search-forward "^ *\\(total\\)" nil t)
		  (let ((available (get-free-disk-space ".")))
		    (when available
		      ;; Replace "total" with "used", to avoid confusion.
		      (replace-match "total used in directory" nil nil nil 1)
		      (end-of-line)
		      (insert " available " available))))))))))

(defun insert-directory-adj-pos (pos error-lines)
  "Convert `ls --dired' file name position value POS to a buffer position.
File name position values returned in ls --dired output
count only stdout; they don't count the error messages sent to stderr.
So this function converts to them to real buffer positions.
ERROR-LINES is a list of buffer positions of error message lines,
of the form (START END)."
  (while (and error-lines (< (caar error-lines) pos))
    (setq pos (+ pos (- (nth 1 (car error-lines)) (nth 0 (car error-lines)))))
    (pop error-lines))
  pos)

(defun insert-directory-safely (file switches
				     &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.

Like `insert-directory', but if FILE does not exist, it inserts a
message to that effect instead of signaling an error."
  (if (file-exists-p file)
      (insert-directory file switches wildcard full-directory-p)
    ;; Simulate the message printed by `ls'.
    (insert (format "%s: No such file or directory\n" file))))

(defvar kill-emacs-query-functions nil
  "Functions to call with no arguments to query about killing Emacs.
If any of these functions returns nil, killing Emacs is canceled.
`save-buffers-kill-emacs' calls these functions, but `kill-emacs',
the low level primitive, does not.  See also `kill-emacs-hook'.")

(defcustom confirm-kill-emacs nil
  "How to ask for confirmation when leaving Emacs.
If nil, the default, don't ask at all.  If the value is non-nil, it should
be a predicate function such as `yes-or-no-p'."
  :type '(choice (const :tag "Ask with yes-or-no-p" yes-or-no-p)
		 (const :tag "Ask with y-or-n-p" y-or-n-p)
		 (const :tag "Don't confirm" nil))
  :group 'convenience
  :version "21.1")

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers without asking.
If there are active processes where `process-query-on-exit-flag'
returns non-nil, asks whether processes should be killed.
Runs the members of `kill-emacs-query-functions' in turn and stops
if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (memq t (mapcar (function
				  (lambda (buf) (and (buffer-file-name buf)
						     (buffer-modified-p buf))))
				(buffer-list))))
	   (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
	   ;; process-list is not defined on MSDOS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop open listen))
		    (process-query-on-exit-flag (car processes))
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or (not active)
		 (progn (list-processes t)
			(yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
	   (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

(defun save-buffers-kill-terminal (&optional arg)
  "Offer to save each buffer, then kill the current connection.
If the current frame has no client, kill Emacs itself.

With prefix ARG, silently save all file-visiting buffers, then kill.

If emacsclient was started with a list of filenames to edit, then
only these files will be asked to be saved."
  (interactive "P")
  (if (frame-parameter (selected-frame) 'client)
      (server-save-buffers-kill-terminal arg)
    (save-buffers-kill-emacs arg)))

;; We use /: as a prefix to "quote" a file name
;; so that magic file name handlers will not apply to it.

(setq file-name-handler-alist
      (cons (cons (purecopy "\\`/:") 'file-name-non-special)
	    file-name-handler-alist))

;; We depend on being the last handler on the list,
;; so that anything else which does need handling
;; has been handled already.
;; So it is safe for us to inhibit *all* magic file name handlers.

(defun file-name-non-special (operation &rest arguments)
  (let ((file-name-handler-alist nil)
	(default-directory
	  (if (eq operation 'insert-directory)
	      (directory-file-name
	       (expand-file-name
		(unhandled-file-name-directory default-directory)))
	    default-directory))
	;; Get a list of the indices of the args which are file names.
	(file-arg-indices
	 (cdr (or (assq operation
			;; The first six are special because they
			;; return a file name.  We want to include the /:
			;; in the return value.
			;; So just avoid stripping it in the first place.
			'((expand-file-name . nil)
			  (file-name-directory . nil)
			  (file-name-as-directory . nil)
			  (directory-file-name . nil)
			  (file-name-sans-versions . nil)
			  (find-backup-file-name . nil)
			  ;; `identity' means just return the first arg
			  ;; not stripped of its quoting.
			  (substitute-in-file-name identity)
			  ;; `add' means add "/:" to the result.
			  (file-truename add 0)
			  (insert-file-contents insert-file-contents 0)
			  ;; `unquote-then-quote' means set buffer-file-name
			  ;; temporarily to unquoted filename.
			  (verify-visited-file-modtime unquote-then-quote)
			  ;; List the arguments which are filenames.
			  (file-name-completion 1)
			  (file-name-all-completions 1)
			  (write-region 2 5)
			  (rename-file 0 1)
			  (copy-file 0 1)
			  (make-symbolic-link 0 1)
			  (add-name-to-file 0 1)))
		  ;; For all other operations, treat the first argument only
		  ;; as the file name.
		  '(nil 0))))
	method
	;; Copy ARGUMENTS so we can replace elements in it.
	(arguments (copy-sequence arguments)))
    (if (symbolp (car file-arg-indices))
	(setq method (pop file-arg-indices)))
    ;; Strip off the /: from the file names that have it.
    (save-match-data
      (while (consp file-arg-indices)
	(let ((pair (nthcdr (car file-arg-indices) arguments)))
	  (and (car pair)
	       (string-match "\\`/:" (car pair))
	       (setcar pair
		       (if (= (length (car pair)) 2)
			   "/"
			 (substring (car pair) 2)))))
	(setq file-arg-indices (cdr file-arg-indices))))
    (case method
      (identity (car arguments))
      (add (concat "/:" (apply operation arguments)))
      (insert-file-contents
       (let ((visit (nth 1 arguments)))
         (prog1
	       (apply operation arguments)
           (when (and visit buffer-file-name)
             (setq buffer-file-name (concat "/:" buffer-file-name))))))
      (unquote-then-quote
       (let ((buffer-file-name (substring buffer-file-name 2)))
         (apply operation arguments)))
	  (t
	   (apply operation arguments)))))

;; Symbolic modes and read-file-modes.

(defun file-modes-char-to-who (char)
  "Convert CHAR to a numeric bit-mask for extracting mode bits.
CHAR is in [ugoa] and represents the category of users (Owner, Group,
Others, or All) for whom to produce the mask.
The bit-mask that is returned extracts from mode bits the access rights
for the specified category of users."
  (cond ((= char ?u) #o4700)
	((= char ?g) #o2070)
	((= char ?o) #o1007)
	((= char ?a) #o7777)
	(t (error "%c: bad `who' character" char))))

(defun file-modes-char-to-right (char &optional from)
  "Convert CHAR to a numeric value of mode bits.
CHAR is in [rwxXstugo] and represents symbolic access permissions.
If CHAR is in [Xugo], the value is taken from FROM (or 0 if omitted)."
  (or from (setq from 0))
  (cond ((= char ?r) #o0444)
	((= char ?w) #o0222)
	((= char ?x) #o0111)
	((= char ?s) #o1000)
	((= char ?t) #o6000)
	;; Rights relative to the previous file modes.
	((= char ?X) (if (= (logand from #o111) 0) 0 #o0111))
	((= char ?u) (let ((uright (logand #o4700 from)))
		       (+ uright (/ uright #o10) (/ uright #o100))))
	((= char ?g) (let ((gright (logand #o2070 from)))
		       (+ gright (/ gright #o10) (* gright #o10))))
	((= char ?o) (let ((oright (logand #o1007 from)))
		       (+ oright (* oright #o10) (* oright #o100))))
	(t (error "%c: bad right character" char))))

(defun file-modes-rights-to-number (rights who-mask &optional from)
  "Convert a symbolic mode string specification to an equivalent number.
RIGHTS is the symbolic mode spec, it should match \"([+=-][rwxXstugo]*)+\".
WHO-MASK is the bit-mask specifying the category of users to which to
apply the access permissions.  See `file-modes-char-to-who'.
FROM (or 0 if nil) gives the mode bits on which to base permissions if
RIGHTS request to add, remove, or set permissions based on existing ones,
as in \"og+rX-w\"."
  (let* ((num-rights (or from 0))
	 (list-rights (string-to-list rights))
	 (op (pop list-rights)))
    (while (memq op '(?+ ?- ?=))
      (let ((num-right 0)
	    char-right)
	(while (memq (setq char-right (pop list-rights))
		     '(?r ?w ?x ?X ?s ?t ?u ?g ?o))
	  (setq num-right
		(logior num-right
			(file-modes-char-to-right char-right num-rights))))
	(setq num-right (logand who-mask num-right)
	      num-rights
	      (cond ((= op ?+) (logior num-rights num-right))
		    ((= op ?-) (logand num-rights (lognot num-right)))
		    (t (logior (logand num-rights (lognot who-mask)) num-right)))
	      op char-right)))
    num-rights))

(defun file-modes-symbolic-to-number (modes &optional from)
  "Convert symbolic file modes to numeric file modes.
MODES is the string to convert, it should match
\"[ugoa]*([+-=][rwxXstugo]*)+,...\".
See Info node `(coreutils)File permissions' for more information on this
notation.
FROM (or 0 if nil) gives the mode bits on which to base permissions if
MODES request to add, remove, or set permissions based on existing ones,
as in \"og+rX-w\"."
  (save-match-data
    (let ((case-fold-search nil)
	  (num-modes (or from 0)))
      (while (/= (string-to-char modes) 0)
	(if (string-match "^\\([ugoa]*\\)\\([+=-][rwxXstugo]*\\)+\\(,\\|\\)" modes)
	    (let ((num-who (apply 'logior 0
				  (mapcar 'file-modes-char-to-who
					  (match-string 1 modes)))))
	      (when (= num-who 0)
		(setq num-who (default-file-modes)))
	      (setq num-modes
		    (file-modes-rights-to-number (substring modes (match-end 1))
						 num-who num-modes)
		    modes (substring modes (match-end 3))))
	  (error "Parse error in modes near `%s'" (substring modes 0))))
      num-modes)))

(defun read-file-modes (&optional prompt orig-file)
  "Read file modes in octal or symbolic notation and return its numeric value.
PROMPT is used as the prompt, default to `File modes (octal or symbolic): '.
ORIG-FILE is the name of a file on whose mode bits to base returned
permissions if what user types requests to add, remove, or set permissions
based on existing mode bits, as in \"og+rX-w\"."
  (let* ((modes (or (if orig-file (file-modes orig-file) 0)
		    (error "File not found")))
	 (modestr (and (stringp orig-file)
		       (nth 8 (file-attributes orig-file))))
	 (default
	   (and (stringp modestr)
		(string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
		(replace-regexp-in-string
		 "-" ""
		 (format "u=%s,g=%s,o=%s"
			 (match-string 1 modestr)
			 (match-string 2 modestr)
			 (match-string 3 modestr)))))
	 (value (read-string (or prompt "File modes (octal or symbolic): ")
			     nil nil default)))
    (save-match-data
      (if (string-match "^[0-7]+" value)
	  (string-to-number value 8)
	(file-modes-symbolic-to-number value modes)))))


;; Trashcan handling.
(defcustom trash-directory nil
  "Directory for `move-file-to-trash' to move files and directories to.
This directory is only used when the function `system-move-file-to-trash'
is not defined.
Relative paths are interpreted relative to `default-directory'.
If the value is nil, Emacs uses a freedesktop.org-style trashcan."
  :type  '(choice (const nil) directory)
  :group 'auto-save
  :version "23.2")

(defvar trash--hexify-table)

(declare-function system-move-file-to-trash "w32fns.c" (filename))

(defun move-file-to-trash (filename)
  "Move the file (or directory) named FILENAME to the trash.
When `delete-by-moving-to-trash' is non-nil, this function is
called by `delete-file' and `delete-directory' instead of
deleting files outright.

If the function `system-move-file-to-trash' is defined, call it
 with FILENAME as an argument.
Otherwise, if `trash-directory' is non-nil, move FILENAME to that
 directory.
Otherwise, trash FILENAME using the freedesktop.org conventions,
 like the GNOME, KDE and XFCE desktop environments.  Emacs only
 moves files to \"home trash\", ignoring per-volume trashcans."
  (interactive "fMove file to trash: ")
  (cond (trash-directory
	 ;; If `trash-directory' is non-nil, move the file there.
	 (let* ((trash-dir   (expand-file-name trash-directory))
		(fn          (directory-file-name (expand-file-name filename)))
		(new-fn      (expand-file-name (file-name-nondirectory fn)
					       trash-dir)))
	   ;; We can't trash a parent directory of trash-directory.
	   (if (string-prefix-p fn trash-dir)
	       (error "Trash directory `%s' is a subdirectory of `%s'"
		      trash-dir filename))
	   (unless (file-directory-p trash-dir)
	     (make-directory trash-dir t))
	   ;; Ensure that the trashed file-name is unique.
	   (if (file-exists-p new-fn)
	       (let ((version-control t)
		     (backup-directory-alist nil))
		 (setq new-fn (car (find-backup-file-name new-fn)))))
	   (let (delete-by-moving-to-trash)
	     (rename-file fn new-fn))))
	;; If `system-move-file-to-trash' is defined, use it.
	((fboundp 'system-move-file-to-trash)
	 (system-move-file-to-trash filename))
	;; Otherwise, use the freedesktop.org method, as specified at
	;; http://freedesktop.org/wiki/Specifications/trash-spec
	(t
	 (let* ((xdg-data-dir
		 (directory-file-name
		  (expand-file-name "Trash"
				    (or (getenv "XDG_DATA_HOME")
					"~/.local/share"))))
		(trash-files-dir (expand-file-name "files" xdg-data-dir))
		(trash-info-dir (expand-file-name "info" xdg-data-dir))
		(fn (directory-file-name (expand-file-name filename))))

	   ;; Check if we have permissions to delete.
	   (unless (file-writable-p (directory-file-name
				     (file-name-directory fn)))
	     (error "Cannot move %s to trash: Permission denied" filename))
	   ;; The trashed file cannot be the trash dir or its parent.
	   (if (string-prefix-p fn trash-files-dir)
	       (error "The trash directory %s is a subdirectory of %s"
		      trash-files-dir filename))
	   (if (string-prefix-p fn trash-info-dir)
	       (error "The trash directory %s is a subdirectory of %s"
		      trash-info-dir filename))

	   ;; Ensure that the trash directory exists; otherwise, create it.
	   (let ((saved-default-file-modes (default-file-modes)))
	     (unwind-protect
		 (progn
		   (set-default-file-modes #o700)
		   (unless (file-exists-p trash-files-dir)
		     (make-directory trash-files-dir t))
		   (unless (file-exists-p trash-info-dir)
		     (make-directory trash-info-dir t)))
	       (set-default-file-modes saved-default-file-modes)))

	   ;; Try to move to trash with .trashinfo undo information
	   (save-excursion
	     (with-temp-buffer
	       (set-buffer-file-coding-system 'utf-8-unix)
	       (insert "[Trash Info]\nPath=")
	       ;; Perform url-encoding on FN.  For compatibility with
	       ;; other programs (e.g. XFCE Thunar), allow literal "/"
	       ;; for path separators.
	       (unless (boundp 'trash--hexify-table)
		 (setq trash--hexify-table (make-vector 256 nil))
		 (let ((unreserved-chars
			(list ?/ ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
			      ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A
			      ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O
			      ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2
			      ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?_ ?. ?! ?~ ?* ?'
			      ?\( ?\))))
		   (dotimes (byte 256)
		     (aset trash--hexify-table byte
			   (if (memq byte unreserved-chars)
			       (char-to-string byte)
			     (format "%%%02x" byte))))))
	       (mapc (lambda (byte)
		       (insert (aref trash--hexify-table byte)))
		     (if (multibyte-string-p fn)
			 (encode-coding-string fn 'utf-8)
		       fn))
	       (insert "\nDeletionDate="
		       (format-time-string "%Y-%m-%dT%T")
		       "\n")

	       ;; Attempt to make .trashinfo file, trying up to 5
	       ;; times.  The .trashinfo file is opened with O_EXCL,
	       ;; as per trash-spec 0.7, even if that can be a problem
	       ;; on old NFS versions...
	       (let* ((tries 5)
		      (base-fn (expand-file-name
				(file-name-nondirectory fn)
				trash-files-dir))
		      (new-fn base-fn)
		      success info-fn)
		 (while (> tries 0)
		   (setq info-fn (expand-file-name
				  (concat (file-name-nondirectory new-fn)
					  ".trashinfo")
				  trash-info-dir))
		   (unless (condition-case nil
			       (progn
				 (write-region nil nil info-fn nil
					       'quiet info-fn 'excl)
				 (setq tries 0 success t))
			     (file-already-exists nil))
		     (setq tries (1- tries))
		     ;; Uniquify new-fn.  (Some file managers do not
		     ;; like Emacs-style backup file names---e.g. bug
		     ;; 170956 in Konqueror bug tracker.)
		     (setq new-fn (make-temp-name (concat base-fn "_")))))
		 (unless success
		   (error "Cannot move %s to trash: Lock failed" filename))

		 ;; Finally, try to move the file to the trashcan.
		 (let ((delete-by-moving-to-trash nil))
		   (rename-file fn new-fn)))))))))


(define-key ctl-x-map "\C-f" 'find-file)
(define-key ctl-x-map "\C-r" 'find-file-read-only)
(define-key ctl-x-map "\C-v" 'find-alternate-file)
(define-key ctl-x-map "\C-s" 'save-buffer)
(define-key ctl-x-map "s" 'save-some-buffers)
(define-key ctl-x-map "\C-w" 'write-file)
(define-key ctl-x-map "i" 'insert-file)
(define-key esc-map "~" 'not-modified)
(define-key ctl-x-map "\C-d" 'list-directory)
(define-key ctl-x-map "\C-c" 'save-buffers-kill-terminal)
(define-key ctl-x-map "\C-q" 'toggle-read-only)

(define-key ctl-x-4-map "f" 'find-file-other-window)
(define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
(define-key ctl-x-4-map "\C-f" 'find-file-other-window)
(define-key ctl-x-4-map "b" 'switch-to-buffer-other-window)
(define-key ctl-x-4-map "\C-o" 'display-buffer)

(define-key ctl-x-5-map "b" 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map "f" 'find-file-other-frame)
(define-key ctl-x-5-map "\C-f" 'find-file-other-frame)
(define-key ctl-x-5-map "r" 'find-file-read-only-other-frame)
(define-key ctl-x-5-map "\C-o" 'display-buffer-other-frame)

;;; files.el ends here
