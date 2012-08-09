;;; pcvs-defs.el --- variable definitions for PCL-CVS

;; Copyright (C) 1991-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs
;; Package: pcvs

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


;;; Code:

(eval-when-compile (require 'cl))
(require 'pcvs-util)

;;;; -------------------------------------------------------
;;;;	    START OF THINGS TO CHECK WHEN INSTALLING

(defvar cvs-program "cvs"
  "*Name or full path of the cvs executable.")

(defvar cvs-version
  ;; With the divergence of the CVSNT codebase and version numbers, this is
  ;; not really good any more.
  (ignore-errors
    (with-temp-buffer
      (call-process cvs-program nil t nil "-v")
      (goto-char (point-min))
      (when (re-search-forward "(CVS\\(NT\\)?) \\([0-9]+\\)\\.\\([0-9]+\\)"
                               nil t)
	(cons (string-to-number (match-string 1))
	      (string-to-number (match-string 2))))))
  "*Version of `cvs' installed on your system.
It must be in the (MAJOR . MINOR) format.")

;; FIXME: this is only used by cvs-mode-diff-backup
(defvar cvs-diff-program (or (and (boundp 'diff-command) diff-command) "diff")
  "*Name or full path of the best diff program you've got.
NOTE:  there are some nasty bugs in the context diff variants of some vendor
versions, such as the one in SunOS-4.")

;;;;	     END OF THINGS TO CHECK WHEN INSTALLING
;;;; --------------------------------------------------------

;;;;
;;;;	User configuration variables:
;;;;
;;;; NOTE: these should be set in your ~/.emacs (or site-lisp/default.el) file.
;;;;

(defgroup pcl-cvs nil
  "Special support for the CVS versioning system."
  :version "21.1"
  :group 'tools
  :prefix "cvs-")

;;
;;  cvsrc options
;;

(defcustom cvs-cvsrc-file (convert-standard-filename "~/.cvsrc")
  "Path to your cvsrc file."
  :group 'pcl-cvs
  :type '(file))

(defvar cvs-shared-start 4
  "Index of the first shared flag.
If set to 4, for instance, a numeric argument smaller than 4 will
select a non-shared flag, while a numeric argument greater than 3
will select a shared-flag.")

(defvar cvs-shared-flags (make-list cvs-shared-start nil)
  "List of flags whose settings is shared among several commands.")

(defvar cvs-cvsroot nil
  "*Specifies where the (current) cvs master repository is.
Overrides the environment variable $CVSROOT by sending \" -d dir\" to
all CVS commands. This switch is useful if you have multiple CVS
repositories. It can be set interactively with \\[cvs-change-cvsroot.]
There is no need to set this if $CVSROOT is set to a correct value.")

(defcustom cvs-auto-remove-handled nil
  "If up-to-date files should be acknowledged automatically.
If T, they will be removed from the *cvs* buffer after every command.
If DELAYED, they will be removed from the *cvs* buffer before every command.
If STATUS, they will only be removed after a `cvs-mode-status' command.
Else, they will never be automatically removed from the *cvs* buffer."
  :group 'pcl-cvs
  :type '(choice (const nil) (const status) (const delayed) (const t)))

(defcustom cvs-auto-remove-directories 'handled
  "If ALL, directory entries will never be shown.
If HANDLED, only non-handled directories will be shown.
If EMPTY, only non-empty directories will be shown."
  :group 'pcl-cvs
  :type '(choice (const :tag "No" nil) (const all) (const handled) (const empty)))

(defcustom cvs-auto-revert t
  "Non-nil if changed files should automatically be reverted."
  :group 'pcl-cvs
  :type '(boolean))

(defcustom cvs-sort-ignore-file t
  "Non-nil if `cvs-mode-ignore' should sort the .cvsignore automatically."
  :group 'pcl-cvs
  :type '(boolean))

(defcustom cvs-force-dir-tag t
  "If non-nil, tagging can only be applied to directories.
Tagging should generally be applied a directory at a time, but sometimes it is
useful to be able to tag a single file.  The normal way to do that is to use
`cvs-mode-force-command' so as to temporarily override the restrictions,"
  :group 'pcl-cvs
  :type '(boolean))

(defcustom cvs-default-ignore-marks nil
  "Non-nil if cvs mode commands should ignore any marked files.
Normally they run on the files that are marked (with `cvs-mode-mark'),
or the file under the cursor if no files are marked.  If this variable
is set to a non-nil value they will by default run on the file on the
current line.  See also `cvs-invert-ignore-marks'"
  :group 'pcl-cvs
  :type '(boolean))

(defvar cvs-diff-ignore-marks t)
(make-obsolete-variable 'cvs-diff-ignore-marks
                        'cvs-invert-ignore-marks
			"21.1")

(defcustom cvs-invert-ignore-marks
  (let ((l ()))
    (unless (equal cvs-diff-ignore-marks cvs-default-ignore-marks)
      (push "diff" l))
    (when (and cvs-force-dir-tag (not cvs-default-ignore-marks))
      (push "tag" l))
    l)
  "List of cvs commands that invert the default ignore-mark behavior.
Commands in this set will use the opposite default from the one set
in `cvs-default-ignore-marks'."
  :group 'pcl-cvs
  :type '(set (const "diff")
	      (const "tag")
	      (const "ignore")))

(defcustom cvs-confirm-removals t
  "Ask for confirmation before removing files.
Non-nil means that PCL-CVS will ask confirmation before removing files
except for files whose content can readily be recovered from the repository.
A value of `list' means that the list of files to be deleted will be
displayed when asking for confirmation."
  :group 'pcl-cvs
  :type '(choice (const list)
		 (const t)
		 (const nil)))

(defcustom cvs-add-default-message nil
  "Default message to use when adding files.
If set to nil, `cvs-mode-add' will always prompt for a message."
  :group 'pcl-cvs
  :type '(choice (const :tag "Prompt" nil)
		 (string)))

(defvar cvs-diff-buffer-name "*cvs-diff*")
(make-obsolete-variable 'cvs-diff-buffer-name
                        'cvs-buffer-name-alist
			"21.1")

(defcustom cvs-find-file-and-jump nil
  "Jump to the modified area when finding a file.
If non-nil, `cvs-mode-find-file' will place the cursor at the beginning of
the modified area.  If the file is not locally modified, this will obviously
have no effect."
  :group 'pcl-cvs
  :type '(boolean))

(defcustom cvs-buffer-name-alist
  '(("diff" cvs-diff-buffer-name diff-mode)
    ("status" "*cvs-info*" cvs-status-mode)
    ("tree" "*cvs-info*" cvs-status-mode)
    ("message" "*cvs-commit*" nil log-edit)
    ("log" "*cvs-info*" log-view-mode))
  "Buffer name and mode to be used for each command.
This is a list of elements of the form

	(CMD BUFNAME MODE &optional POSTPROC)

CMD is the name of the command.
BUFNAME is an expression that should evaluate to a string used as
  a buffer name.  It can use the variable CMD if it wants to.
MODE is the command to use to setup the buffer.
POSTPROC is a function that should be executed when the command terminates

The CMD used for `cvs-mode-commit' is \"message\".  For that special
  case, POSTPROC is called just after MODE with special arguments."
  :group 'pcl-cvs
  :type '(repeat
	  (list (choice (const "diff")
			(const "status")
			(const "tree")
			(const "message")
			(const "log")
			(string))
		(choice (const "*vc-diff*")
			(const "*cvs-info*")
			(const "*cvs-commit*")
			(const (expand-file-name "*cvs-commit*"))
			(const (format "*cvs-%s*" cmd))
			(const (expand-file-name (format "*cvs-%s*" cmd)))
			(sexp :value "my-cvs-info-buffer")
			(const nil))
		(choice (function-item diff-mode)
			(function-item cvs-edit-mode)
			(function-item cvs-status-mode)
			function
			(const nil))
		(set :inline t
		     (choice (function-item cvs-status-cvstrees)
			     (function-item cvs-status-trees)
			     function)))))

(defvar cvs-buffer-name '(expand-file-name "*cvs*" dir) ;; "*cvs*"
  "Name of the cvs buffer.
This expression will be evaluated in an environment where DIR is set to
the directory name of the cvs buffer.")

(defvar cvs-temp-buffer-name
  ;; Was '(expand-file-name " *cvs-tmp*" dir), but that causes them to
  ;; become non-hidden if uniquification is done `forward'.
  " *cvs-tmp*"
  "*Name of the cvs temporary buffer.
Output from cvs is placed here for asynchronous commands.")

(defcustom cvs-idiff-imerge-handlers
  (if (fboundp 'ediff)
      '(cvs-ediff-diff . cvs-ediff-merge)
    '(cvs-emerge-diff . cvs-emerge-merge))
  "Pair of functions to be used for resp.  diff'ing and merg'ing interactively."
  :group 'pcl-cvs
  :type '(choice (const :tag "Ediff" (cvs-ediff-diff . cvs-ediff-merge))
		 (const :tag "Emerge" (cvs-emerge-diff . cvs-emerge-merge))))

(defvar cvs-mode-hook nil
  "Run after `cvs-mode' was setup.")


;;;;
;;;; Internal variables, used in the process buffer.
;;;;

(defvar cvs-postprocess nil
  "(Buffer local) what to do once the process exits.")

;;;;
;;;; Internal variables for the *cvs* buffer.
;;;;

(defcustom cvs-reuse-cvs-buffer 'subdir
  "When to reuse an existing cvs buffer.
Alternatives are:
 CURRENT: just reuse the current buffer if it is a cvs buffer
 SAMEDIR: reuse any cvs buffer displaying the same directory
 SUBDIR:  or reuse any cvs buffer displaying any sub- or super- directory
 ALWAYS:  reuse any cvs buffer."
  :group 'pcl-cvs
  :type '(choice (const always) (const subdir) (const samedir) (const current)))

(defvar cvs-temp-buffer nil
  "(Buffer local) The temporary buffer associated with this *cvs* buffer.")

(defvar cvs-lock-file nil
  "Full path to a lock file that CVS is waiting for (or was waiting for).
This variable is buffer local and only used in the *cvs* buffer.")

(defvar cvs-lock-file-regexp "^#cvs\\.\\([trw]fl\\.[-.a-z0-9]+\\|lock\\)\\'"
  "Regexp matching the possible names of locks in the CVS repository.")

(defconst cvs-cursor-column 22
  "Column to position cursor in in `cvs-mode'.")

;;;;
;;;; Global internal variables
;;;;

(defconst cvs-vendor-branch "1.1.1"
  "The default branch used by CVS for vendor code.")

(easy-mmode-defmap cvs-mode-diff-map
  '(("E" "imerge" .	cvs-mode-imerge)
    ("=" .		cvs-mode-diff)
    ("e" "idiff" .	cvs-mode-idiff)
    ("2" "other" .	cvs-mode-idiff-other)
    ("d" "diff" .	cvs-mode-diff)
    ("b" "backup" .	cvs-mode-diff-backup)
    ("h" "head" .	cvs-mode-diff-head)
    ("r" "repository" .	cvs-mode-diff-repository)
    ("y" "yesterday" .	cvs-mode-diff-yesterday)
    ("v" "vendor" .	cvs-mode-diff-vendor))
  "Keymap for diff-related operations in `cvs-mode'."
  :name "Diff")
;; This is necessary to allow correct handling of \\[cvs-mode-diff-map]
;; in substitute-command-keys.
(fset 'cvs-mode-diff-map cvs-mode-diff-map)

(easy-mmode-defmap cvs-mode-map
  ;;(define-prefix-command 'cvs-mode-map-diff-prefix)
  ;;(define-prefix-command 'cvs-mode-map-control-c-prefix)
  '(;; various
    ;; (undo .	cvs-mode-undo)
    ("?" .	cvs-help)
    ("h" .	cvs-help)
    ("q" .	cvs-bury-buffer)
    ("z" .	kill-this-buffer)
    ("F" .	cvs-mode-set-flags)
    ;; ("\M-f" .	cvs-mode-force-command)
    ("!" .	cvs-mode-force-command)
    ("\C-c\C-c" . cvs-mode-kill-process)
    ;; marking
    ("m" .	cvs-mode-mark)
    ("M" .	cvs-mode-mark-all-files)
    ("S" .	cvs-mode-mark-on-state)
    ("u" .	cvs-mode-unmark)
    ("\C-?".	cvs-mode-unmark-up)
    ("%" .	cvs-mode-mark-matching-files)
    ("T" .	cvs-mode-toggle-marks)
    ("\M-\C-?" .	cvs-mode-unmark-all-files)
    ;; navigation keys
    (" " .	cvs-mode-next-line)
    ("n" .	cvs-mode-next-line)
    ("p" .	cvs-mode-previous-line)
    ("\t" .	cvs-mode-next-line)
    ([backtab] . cvs-mode-previous-line)
    ;; M- keys are usually those that operate on modules
    ;;("\M-C".	cvs-mode-rcs2log) ; i.e. "Create a ChangeLog"
    ;;("\M-t".	cvs-rtag)
    ;;("\M-l".	cvs-rlog)
    ("\M-c".	cvs-checkout)
    ("\M-e".	cvs-examine)
    ("g" .	cvs-mode-revert-buffer)
    ("\M-u".	cvs-update)
    ("\M-s".	cvs-status)
    ;; diff commands
    ("=" .	cvs-mode-diff)
    ("d" .	cvs-mode-diff-map)
    ;; keys that operate on individual files
    ("\C-k" .	cvs-mode-acknowledge)
    ("A" .	cvs-mode-add-change-log-entry-other-window)
    ;;("B" .	cvs-mode-byte-compile-files)
    ("C" .	cvs-mode-commit-setup)
    ("O" .	cvs-mode-update)
    ("U" .	cvs-mode-undo)
    ("I" .	cvs-mode-insert)
    ("a" .	cvs-mode-add)
    ("b" .	cvs-set-branch-prefix)
    ("B" .	cvs-set-secondary-branch-prefix)
    ("c" .	cvs-mode-commit)
    ("e" .	cvs-mode-examine)
    ("f" .	cvs-mode-find-file)
    ("\C-m" .	cvs-mode-find-file)
    ("i" .	cvs-mode-ignore)
    ("l" .	cvs-mode-log)
    ("o" .	cvs-mode-find-file-other-window)
    ("r" .	cvs-mode-remove)
    ("s" .	cvs-mode-status)
    ("t" .	cvs-mode-tag)
    ("v" .	cvs-mode-view-file)
    ("x" .	cvs-mode-remove-handled)
    ;; cvstree bindings
    ("+" .	cvs-mode-tree)
    ;; mouse bindings
    ([mouse-2] . cvs-mode-find-file)
    ([follow-link] . (lambda (pos)
		       (if (eq (get-char-property pos 'face) 'cvs-filename) t)))
    ([(down-mouse-3)] . cvs-menu)
    ;; dired-like bindings
    ("\C-o" .   cvs-mode-display-file)
    ;; Emacs-21 toolbar
    ;;([tool-bar item1] . (menu-item "Examine" cvs-examine :image (image :file "/usr/share/icons/xpaint.xpm" :type xpm)))
    ;;([tool-bar item2] . (menu-item "Update" cvs-update :image (image :file "/usr/share/icons/mail1.xpm" :type xpm)))
    )
  "Keymap for `cvs-mode'."
  :dense t
  :suppress t)

(fset 'cvs-mode-map cvs-mode-map)

(easy-menu-define cvs-menu cvs-mode-map "Menu used in `cvs-mode'."
  '("CVS"
    ["Open file"		cvs-mode-find-file	t]
    ["Open in other window"	cvs-mode-find-file-other-window	t]
    ["Display in other window"  cvs-mode-display-file   t]
    ["Interactive merge"	cvs-mode-imerge		t]
    ("View diff"
     ["Interactive diff"	cvs-mode-idiff		t]
     ["Current diff"		cvs-mode-diff		t]
     ["Diff with head"		cvs-mode-diff-head	t]
     ["Diff with vendor"	cvs-mode-diff-vendor	t]
     ["Diff against yesterday"	cvs-mode-diff-yesterday	t]
     ["Diff with backup"	cvs-mode-diff-backup	t])
    ["View log"			cvs-mode-log		t]
    ["View status"		cvs-mode-status		t]
    ["View tag tree"		cvs-mode-tree		t]
    "----"
    ["Insert"			cvs-mode-insert]
    ["Update"			cvs-mode-update		(cvs-enabledp 'update)]
    ["Re-examine"		cvs-mode-examine	t]
    ["Commit"			cvs-mode-commit-setup	(cvs-enabledp 'commit)]
    ["Tag"			cvs-mode-tag		(cvs-enabledp (when cvs-force-dir-tag 'tag))]
    ["Undo changes"		cvs-mode-undo		(cvs-enabledp 'undo)]
    ["Add"			cvs-mode-add		(cvs-enabledp 'add)]
    ["Remove"			cvs-mode-remove		(cvs-enabledp 'remove)]
    ["Ignore"			cvs-mode-ignore		(cvs-enabledp 'ignore)]
    ["Add ChangeLog"		cvs-mode-add-change-log-entry-other-window t]
    "----"
    ["Mark"                     cvs-mode-mark t]
    ["Mark all"			cvs-mode-mark-all-files	t]
    ["Mark by regexp..."        cvs-mode-mark-matching-files t]
    ["Mark by state..."         cvs-mode-mark-on-state t]
    ["Unmark"                   cvs-mode-unmark	t]
    ["Unmark all"		cvs-mode-unmark-all-files t]
    ["Hide handled"		cvs-mode-remove-handled	t]
    "----"
    ["PCL-CVS Manual"		(lambda () (interactive)
				  (info "(pcl-cvs)Top")) t]
    "----"
    ["Quit"			cvs-mode-quit		t]))

;;;;
;;;; CVS-Minor mode
;;;;

(defcustom cvs-minor-mode-prefix "\C-xc"
  "Prefix key for the `cvs-mode' bindings in `cvs-minor-mode'."
  :group 'pcl-cvs)

(easy-mmode-defmap cvs-minor-mode-map
  `((,cvs-minor-mode-prefix . cvs-mode-map)
    ("e" . (menu-item nil cvs-mode-edit-log
	    :filter (lambda (x) (if (derived-mode-p 'log-view-mode) x)))))
  "Keymap for `cvs-minor-mode', used in buffers related to PCL-CVS.")

(defvar cvs-buffer nil
  "(Buffer local) The *cvs* buffer associated with this buffer.")
(put 'cvs-buffer 'permanent-local t)
;;(make-variable-buffer-local 'cvs-buffer)

(defvar cvs-minor-wrap-function nil
  "Function to call when switching to the *cvs* buffer.
Takes two arguments:
- a *cvs* buffer.
- a zero-arg function which is guaranteed not to switch buffer.
It is expected to call the function.")
;;(make-variable-buffer-local 'cvs-minor-wrap-function)

(defvar cvs-minor-current-files)
;;"Current files in a `cvs-minor-mode' buffer."
;; This should stay `void' because we want to be able to tell the difference
;; between an empty list and no list at all.

(defconst cvs-pcl-cvs-dirchange-re "^pcl-cvs: descending directory \\(.*\\)$")

;;;;
;;;; autoload the global menu
;;;;

;;;###autoload
(defvar cvs-global-menu
  (let ((m (make-sparse-keymap "PCL-CVS")))
    (define-key m [status]
      `(menu-item ,(purecopy "Directory Status") cvs-status
		  :help ,(purecopy "A more verbose status of a workarea")))
    (define-key m [checkout]
      `(menu-item ,(purecopy "Checkout Module") cvs-checkout
		  :help ,(purecopy "Check out a module from the repository")))
    (define-key m [update]
      `(menu-item ,(purecopy "Update Directory") cvs-update
		  :help ,(purecopy "Fetch updates from the repository")))
    (define-key m [examine]
      `(menu-item ,(purecopy "Examine Directory") cvs-examine
		  :help ,(purecopy "Examine the current state of a workarea")))
    (fset 'cvs-global-menu m))
  "Global menu used by PCL-CVS.")


;; cvs-1.10 and above can take file arguments in other directories
;; while others need to be executed once per directory
(defvar cvs-execute-single-dir
  (if (or (null cvs-version)
          (or (>= (cdr cvs-version) 10) (> (car cvs-version) 1)))
      ;; Supposedly some recent versions of CVS output some directory info
      ;; as they recurse down the tree, but it's not good enough in the case
      ;; where we run "cvs status foo bar/foo".
      '("status")
    t)
  "Whether cvs commands should be executed a directory at a time.
If a list, specifies for which commands the single-dir mode should be used.
If T, single-dir mode should be used for all operations.

CVS versions before 1.10 did not allow passing them arguments in different
directories, so pcl-cvs checks what version you're using to determine
whether to use the new feature or not.
Sadly, even with a new cvs executable, if you connect to an older cvs server
\(typically a cvs-1.9 on the server), the old restriction applies.  In such
a case the sanity check made by pcl-cvs fails and you will have to manually
set this variable to t (until the cvs server is upgraded).
When the above problem occurs, pcl-cvs should (hopefully) catch cvs' error
message and replace it with a message telling you to change this variable.")

;;
(provide 'pcvs-defs)

;;; pcvs-defs.el ends here
