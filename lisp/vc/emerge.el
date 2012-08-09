;;; emerge.el --- merge diffs under Emacs control

;;; The author has placed this file in the public domain.

;; This file is part of GNU Emacs.

;; Author: Dale R. Worley <worley@world.std.com>
;; Keywords: unix, vc, tools

;; This software was created by Dale R. Worley and is
;; distributed free of charge.  It is placed in the public domain and
;; permission is granted to anyone to use, duplicate, modify and redistribute
;; it provided that this notice is attached.

;; Dale R. Worley provides absolutely NO WARRANTY OF ANY KIND
;; with respect to this software.  The entire risk as to the quality and
;; performance of this software is with the user.  IN NO EVENT WILL DALE
;; R. WORLEY BE LIABLE TO ANYONE FOR ANY DAMAGES ARISING OUT THE
;; USE OF THIS SOFTWARE, INCLUDING, WITHOUT LIMITATION, DAMAGES RESULTING FROM
;; LOST DATA OR LOST PROFITS, OR FOR ANY SPECIAL, INCIDENTAL OR CONSEQUENTIAL
;; DAMAGES.

;;; Commentary:

;;; Code:

;; There aren't really global variables, just dynamic bindings
(defvar A-begin)
(defvar A-end)
(defvar B-begin)
(defvar B-end)
(defvar diff-vector)
(defvar merge-begin)
(defvar merge-end)
(defvar valid-diff)

;;; Macros

(defmacro emerge-defvar-local (var value doc)
  "Defines SYMBOL as an advertised variable.
Performs a defvar, then executes `make-variable-buffer-local' on
the variable.  Also sets the `permanent-local' property, so that
`kill-all-local-variables' (called by major-mode setting commands)
won't destroy Emerge control variables."
  `(progn
    (defvar ,var ,value ,doc)
    (make-variable-buffer-local ',var)
    (put ',var 'permanent-local t)))

;; Add entries to minor-mode-alist so that emerge modes show correctly
(defvar emerge-minor-modes-list
  '((emerge-mode " Emerge")
    (emerge-fast-mode " F")
    (emerge-edit-mode " E")
    (emerge-auto-advance " A")
    (emerge-skip-prefers " S")))
(if (not (assq 'emerge-mode minor-mode-alist))
    (setq minor-mode-alist (append emerge-minor-modes-list
				   minor-mode-alist)))

;; We need to define this function so describe-mode can describe Emerge mode.
(defun emerge-mode ()
  "Emerge mode is used by the Emerge file-merging package.
It is entered only through one of the functions:
	`emerge-files'
	`emerge-files-with-ancestor'
	`emerge-buffers'
	`emerge-buffers-with-ancestor'
	`emerge-files-command'
	`emerge-files-with-ancestor-command'
	`emerge-files-remote'
	`emerge-files-with-ancestor-remote'

Commands:
\\{emerge-basic-keymap}
Commands must be prefixed by \\<emerge-fast-keymap>\\[emerge-basic-keymap] in `edit' mode,
but can be invoked directly in `fast' mode.")

(define-obsolete-variable-alias 'emerge-version 'emacs-version "23.2")

(defun emerge-version ()
  "Return string describing the version of Emerge.
When called interactively, displays the version."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "Emerge version %s" emacs-version)
    emacs-version))

(make-obsolete 'emerge-version 'emacs-version "23.2")

;;; Emerge configuration variables

(defgroup emerge nil
  "Merge diffs under Emacs control."
  :group 'tools)

;; Commands that produce difference files
;; All that can be configured is the name of the programs to execute
;; (emerge-diff-program and emerge-diff3-program) and the options
;; to be provided (emerge-diff-options).  The order in which the file names
;; are given is fixed.
;; The file names are always expanded (see expand-file-name) before being
;; passed to diff, thus they need not be invoked under a shell that
;; understands `~'.
;; The code which processes the diff/diff3 output depends on all the
;; finicky details of their output, including the somewhat strange
;; way they number lines of a file.
(defcustom emerge-diff-program "diff"
  "Name of the program which compares two files."
  :type 'string
  :group 'emerge)
(defcustom emerge-diff3-program "diff3"
  "Name of the program which compares three files.
Its arguments are the ancestor file and the two variant files."
  :type 'string
  :group 'emerge)
(defcustom emerge-diff-options ""
  "Options to pass to `emerge-diff-program' and `emerge-diff3-program'."
  :type 'string
  :group 'emerge)
(defcustom emerge-match-diff-line
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
    (concat "^" x "\\([acd]\\)" x "$"))
  "Pattern to match lines produced by diff that describe differences.
This is as opposed to lines from the source files."
  :type 'regexp
  :group 'emerge)
(defcustom emerge-diff-ok-lines-regexp
  "^\\([0-9,]+[acd][0-9,]+$\\|[<>] \\|---\\)"
  "Regexp that matches normal output lines from `emerge-diff-program'.
Lines that do not match are assumed to be error messages."
  :type 'regexp
  :group 'emerge)
(defcustom emerge-diff3-ok-lines-regexp
  "^\\([1-3]:\\|====\\|  \\)"
  "Regexp that matches normal output lines from `emerge-diff3-program'.
Lines that do not match are assumed to be error messages."
  :type 'regexp
  :group 'emerge)

(defcustom emerge-rcs-ci-program "ci"
  "Name of the program that checks in RCS revisions."
  :type 'string
  :group 'emerge)
(defcustom emerge-rcs-co-program "co"
  "Name of the program that checks out RCS revisions."
  :type 'string
  :group 'emerge)

(defcustom emerge-process-local-variables nil
  "Non-nil if Emerge should process local-variables lists in merge buffers.
\(You can explicitly request processing the local-variables
by executing `(hack-local-variables)'.)"
  :type 'boolean
  :group 'emerge)
(defcustom emerge-execute-line-deletions nil
  "If non-nil: `emerge-execute-line' makes no output if an input was deleted.
It concludes that an input version has been deleted when an ancestor entry
is present, only one A or B entry is present, and an output entry is present.
If nil: In such circumstances, the A or B file that is present will be
copied to the designated output file."
  :type 'boolean
  :group 'emerge)

(defcustom emerge-before-flag "vvvvvvvvvvvvvvvvvvvv\n"
  "Flag placed above the highlighted block of code.  Must end with newline.
Must be set before Emerge is loaded, or  emerge-new-flags  must be run
after setting."
  :type 'string
  :group 'emerge)
(defcustom emerge-after-flag "^^^^^^^^^^^^^^^^^^^^\n"
  "Flag placed below the highlighted block of code.  Must end with newline.
Must be set before Emerge is loaded, or  emerge-new-flags  must be run
after setting."
  :type 'string
  :group 'emerge)

;; Hook variables

(defcustom emerge-startup-hook nil
  "Hook to run in the merge buffer after the merge has been set up."
  :type 'hook
  :group 'emerge)
(defcustom emerge-select-hook nil
  "Hook to run after a difference has been selected.
The variable `n' holds the (internal) number of the difference."
  :type 'hook
  :group 'emerge)
(defcustom emerge-unselect-hook nil
  "Hook to run after a difference has been unselected.
The variable `n' holds the (internal) number of the difference."
  :type 'hook
  :group 'emerge)

;; Variables to control the default directories of the arguments to
;; Emerge commands.

(defcustom emerge-default-last-directories nil
  "If nil, default dir for filenames in emerge is `default-directory'.
If non-nil, filenames complete in the directory of the last argument of the
same type to an `emerge-files...' command."
  :type 'boolean
  :group 'emerge)

(defvar emerge-last-dir-A nil
  "Last directory for the first file of an `emerge-files...' command.")
(defvar emerge-last-dir-B nil
  "Last directory for the second file of an `emerge-files...' command.")
(defvar emerge-last-dir-ancestor nil
  "Last directory for the ancestor file of an `emerge-files...' command.")
(defvar emerge-last-dir-output nil
  "Last directory for the output file of an `emerge-files...' command.")
(defvar emerge-last-revision-A nil
  "Last RCS revision used for first file of an `emerge-revisions...' command.")
(defvar emerge-last-revision-B nil
  "Last RCS revision used for second file of an `emerge-revisions...' command.")
(defvar emerge-last-revision-ancestor nil
  "Last RCS revision used for ancestor file of an `emerge-revisions...' command.")

(defvar emerge-before-flag-length)
(defvar emerge-before-flag-lines)
(defvar emerge-before-flag-match)
(defvar emerge-after-flag-length)
(defvar emerge-after-flag-lines)
(defvar emerge-after-flag-match)
(defvar emerge-diff-buffer)
(defvar emerge-diff-error-buffer)
(defvar emerge-prefix-argument)
(defvar emerge-file-out)
(defvar emerge-exit-func)
(defvar emerge-globalized-difference-list)
(defvar emerge-globalized-number-of-differences)

;; The flags used to mark differences in the buffers.

;; These function definitions need to be up here, because they are used
;; during loading.
(defun emerge-new-flags ()
  "Function to be called after `emerge-{before,after}-flag'.
This is called after these functions are changed to compute values that
depend on the flags."
  (setq emerge-before-flag-length (length emerge-before-flag))
  (setq emerge-before-flag-lines
	(emerge-count-matches-string emerge-before-flag "\n"))
  (setq emerge-before-flag-match (regexp-quote emerge-before-flag))
  (setq emerge-after-flag-length (length emerge-after-flag))
  (setq emerge-after-flag-lines
	(emerge-count-matches-string emerge-after-flag "\n"))
  (setq emerge-after-flag-match (regexp-quote emerge-after-flag)))

(defun emerge-count-matches-string (string regexp)
  "Return the number of matches in STRING for REGEXP."
  (let ((i 0)
	(count 0))
    (while (string-match regexp string i)
      (setq count (1+ count))
      (setq i (match-end 0)))
    count))

;; Calculate dependent variables
(emerge-new-flags)

(defcustom emerge-min-visible-lines 3
  "Number of lines that we want to show above and below the flags when we are
displaying a difference."
  :type 'integer
  :group 'emerge)

(defcustom emerge-temp-file-prefix
  (expand-file-name "emerge" temporary-file-directory)
  "Prefix to put on Emerge temporary file names.
Do not start with `~/' or `~USERNAME/'."
  :type 'string
  :group 'emerge)

(defcustom emerge-temp-file-mode 384	; u=rw only
  "Mode for Emerge temporary files."
  :type 'integer
  :group 'emerge)

(defcustom emerge-combine-versions-template
  "#ifdef NEW\n%b#else /* not NEW */\n%a#endif /* not NEW */\n"
  "Template for `emerge-combine-versions' to combine the two versions.
The template is inserted as a string, with the following interpolations:
	%a	the A version of the difference
	%b	the B version of the difference
	%%	the character `%'
Don't forget to end the template with a newline.
Note that this variable can be made local to a particular merge buffer by
giving a prefix argument to `emerge-set-combine-versions-template'."
  :type 'string
  :group 'emerge)

;; Build keymaps

(defvar emerge-basic-keymap nil
  "Keymap of Emerge commands.
Directly available in `fast' mode;
must be prefixed by \\<emerge-fast-keymap>\\[emerge-basic-keymap] in `edit' mode.")

(defvar emerge-fast-keymap nil
  "Local keymap used in Emerge `fast' mode.
Makes Emerge commands directly available.")

(defvar emerge-options-menu
  (make-sparse-keymap "Options"))

(defvar emerge-merge-menu
  (make-sparse-keymap "Merge"))

(defvar emerge-move-menu
  (make-sparse-keymap "Move"))

(defcustom emerge-command-prefix "\C-c\C-c"
  "Command prefix for Emerge commands in `edit' mode.
Must be set before Emerge is loaded."
  :type 'string
  :group 'emerge)

;; This function sets up the fixed keymaps.  It is executed when the first
;; Emerge is done to allow the user maximum time to set up the global keymap.
(defun emerge-setup-fixed-keymaps ()
  ;; Set up the basic keymap
  (setq emerge-basic-keymap (make-keymap))
  (suppress-keymap emerge-basic-keymap)	; this sets 0..9 to digit-argument and
					; - to negative-argument
  (define-key emerge-basic-keymap "p" 'emerge-previous-difference)
  (define-key emerge-basic-keymap "n" 'emerge-next-difference)
  (define-key emerge-basic-keymap "a" 'emerge-select-A)
  (define-key emerge-basic-keymap "b" 'emerge-select-B)
  (define-key emerge-basic-keymap "j" 'emerge-jump-to-difference)
  (define-key emerge-basic-keymap "." 'emerge-find-difference)
  (define-key emerge-basic-keymap "q" 'emerge-quit)
  (define-key emerge-basic-keymap "\C-]" 'emerge-abort)
  (define-key emerge-basic-keymap "f" 'emerge-fast-mode)
  (define-key emerge-basic-keymap "e" 'emerge-edit-mode)
  (define-key emerge-basic-keymap "s" nil)
  (define-key emerge-basic-keymap "sa" 'emerge-auto-advance)
  (define-key emerge-basic-keymap "ss" 'emerge-skip-prefers)
  (define-key emerge-basic-keymap "l" 'emerge-recenter)
  (define-key emerge-basic-keymap "d" nil)
  (define-key emerge-basic-keymap "da" 'emerge-default-A)
  (define-key emerge-basic-keymap "db" 'emerge-default-B)
  (define-key emerge-basic-keymap "c" nil)
  (define-key emerge-basic-keymap "ca" 'emerge-copy-as-kill-A)
  (define-key emerge-basic-keymap "cb" 'emerge-copy-as-kill-B)
  (define-key emerge-basic-keymap "i" nil)
  (define-key emerge-basic-keymap "ia" 'emerge-insert-A)
  (define-key emerge-basic-keymap "ib" 'emerge-insert-B)
  (define-key emerge-basic-keymap "m" 'emerge-mark-difference)
  (define-key emerge-basic-keymap "v" 'emerge-scroll-up)
  (define-key emerge-basic-keymap "^" 'emerge-scroll-down)
  (define-key emerge-basic-keymap "<" 'emerge-scroll-left)
  (define-key emerge-basic-keymap ">" 'emerge-scroll-right)
  (define-key emerge-basic-keymap "|" 'emerge-scroll-reset)
  (define-key emerge-basic-keymap "x" nil)
  (define-key emerge-basic-keymap "x1" 'emerge-one-line-window)
  (define-key emerge-basic-keymap "xc" 'emerge-combine-versions)
  (define-key emerge-basic-keymap "xC" 'emerge-combine-versions-register)
  (define-key emerge-basic-keymap "xf" 'emerge-file-names)
  (define-key emerge-basic-keymap "xj" 'emerge-join-differences)
  (define-key emerge-basic-keymap "xl" 'emerge-line-numbers)
  (define-key emerge-basic-keymap "xm" 'emerge-set-merge-mode)
  (define-key emerge-basic-keymap "xs" 'emerge-split-difference)
  (define-key emerge-basic-keymap "xt" 'emerge-trim-difference)
  (define-key emerge-basic-keymap "xx" 'emerge-set-combine-versions-template)
  ;; Allow emerge-basic-keymap to be referenced indirectly
  (fset 'emerge-basic-keymap emerge-basic-keymap)
  ;; Set up the fast mode keymap
  (setq emerge-fast-keymap (copy-keymap emerge-basic-keymap))
  ;; Allow prefixed commands to work in fast mode
  (define-key emerge-fast-keymap emerge-command-prefix 'emerge-basic-keymap)
  ;; Allow emerge-fast-keymap to be referenced indirectly
  (fset 'emerge-fast-keymap emerge-fast-keymap)
  ;; Suppress write-file and save-buffer
  (define-key emerge-fast-keymap [remap write-file] 'emerge-query-write-file)
  (define-key emerge-fast-keymap [remap save-buffer] 'emerge-query-save-buffer)

  (define-key emerge-basic-keymap [menu-bar] (make-sparse-keymap))

  (define-key emerge-fast-keymap [menu-bar emerge-options]
    (cons "Merge-Options" emerge-options-menu))
  (define-key emerge-fast-keymap [menu-bar merge]
    (cons "Merge" emerge-merge-menu))
  (define-key emerge-fast-keymap [menu-bar move]
    (cons "Move" emerge-move-menu))

  (define-key emerge-move-menu [emerge-scroll-reset]
    '("Scroll Reset" . emerge-scroll-reset))
  (define-key emerge-move-menu [emerge-scroll-right]
    '("Scroll Right" . emerge-scroll-right))
  (define-key emerge-move-menu [emerge-scroll-left]
    '("Scroll Left" . emerge-scroll-left))
  (define-key emerge-move-menu [emerge-scroll-down]
    '("Scroll Down" . emerge-scroll-down))
  (define-key emerge-move-menu [emerge-scroll-up]
    '("Scroll Up" . emerge-scroll-up))
  (define-key emerge-move-menu [emerge-recenter]
    '("Recenter" . emerge-recenter))
  (define-key emerge-move-menu [emerge-mark-difference]
    '("Mark Difference" . emerge-mark-difference))
  (define-key emerge-move-menu [emerge-jump-to-difference]
    '("Jump To Difference" . emerge-jump-to-difference))
  (define-key emerge-move-menu [emerge-find-difference]
    '("Find Difference" . emerge-find-difference))
  (define-key emerge-move-menu [emerge-previous-difference]
    '("Previous Difference" . emerge-previous-difference))
  (define-key emerge-move-menu [emerge-next-difference]
    '("Next Difference" . emerge-next-difference))


  (define-key emerge-options-menu [emerge-one-line-window]
    '("One Line Window" . emerge-one-line-window))
  (define-key emerge-options-menu [emerge-set-merge-mode]
    '("Set Merge Mode..." . emerge-set-merge-mode))
  (define-key emerge-options-menu [emerge-set-combine-template]
    '("Set Combine Template..." . emerge-set-combine-template))
  (define-key emerge-options-menu [emerge-default-B]
    '("Default B" . emerge-default-B))
  (define-key emerge-options-menu [emerge-default-A]
    '("Default A" . emerge-default-A))
  (define-key emerge-options-menu [emerge-skip-prefers]
    '(menu-item "Skip Prefers" emerge-skip-prefers
		:button (:toggle . emerge-skip-prefers)))
  (define-key emerge-options-menu [emerge-auto-advance]
    '(menu-item "Auto Advance" emerge-auto-advance
		:button (:toggle . emerge-auto-advance)))
  (define-key emerge-options-menu [emerge-edit-mode]
    '(menu-item "Edit Mode" emerge-edit-mode :enable (not emerge-edit-mode)))
  (define-key emerge-options-menu [emerge-fast-mode]
    '(menu-item "Fast Mode" emerge-fast-mode :enable (not emerge-fast-mode)))

  (define-key emerge-merge-menu [emerge-abort] '("Abort" . emerge-abort))
  (define-key emerge-merge-menu [emerge-quit] '("Quit" . emerge-quit))
  (define-key emerge-merge-menu [emerge-split-difference]
    '("Split Difference" . emerge-split-difference))
  (define-key emerge-merge-menu [emerge-join-differences]
    '("Join Differences" . emerge-join-differences))
  (define-key emerge-merge-menu [emerge-trim-difference]
    '("Trim Difference" . emerge-trim-difference))
  (define-key emerge-merge-menu [emerge-combine-versions]
    '("Combine Versions" . emerge-combine-versions))
  (define-key emerge-merge-menu [emerge-copy-as-kill-B]
    '("Copy B as Kill" . emerge-copy-as-kill-B))
  (define-key emerge-merge-menu [emerge-copy-as-kill-A]
    '("Copy A as Kill" . emerge-copy-as-kill-A))
  (define-key emerge-merge-menu [emerge-insert-B]
    '("Insert B" . emerge-insert-B))
  (define-key emerge-merge-menu [emerge-insert-A]
    '("Insert A" . emerge-insert-A))
  (define-key emerge-merge-menu [emerge-select-B]
    '("Select B" . emerge-select-B))
  (define-key emerge-merge-menu [emerge-select-A]
    '("Select A" . emerge-select-A)))


;; Variables which control each merge.  They are local to the merge buffer.

;; Mode variables
(emerge-defvar-local emerge-mode nil
  "Indicator for emerge-mode.")
(emerge-defvar-local emerge-fast-mode nil
  "Indicator for emerge-mode fast submode.")
(emerge-defvar-local emerge-edit-mode nil
  "Indicator for emerge-mode edit submode.")
(emerge-defvar-local emerge-A-buffer nil
  "The buffer in which the A variant is stored.")
(emerge-defvar-local emerge-B-buffer nil
  "The buffer in which the B variant is stored.")
(emerge-defvar-local emerge-merge-buffer nil
  "The buffer in which the merged file is manipulated.")
(emerge-defvar-local emerge-ancestor-buffer nil
  "The buffer in which the ancestor variant is stored,
or nil if there is none.")

(defconst emerge-saved-variables
  '((buffer-modified-p set-buffer-modified-p)
    buffer-read-only
    buffer-auto-save-file-name)
  "Variables and properties of a buffer which are saved, modified and restored
during a merge.")
(defconst emerge-merging-values '(nil t nil)
  "Values to be assigned to emerge-saved-variables during a merge.")

(emerge-defvar-local emerge-A-buffer-values nil
  "Remembers emerge-saved-variables for emerge-A-buffer.")
(emerge-defvar-local emerge-B-buffer-values nil
  "Remembers emerge-saved-variables for emerge-B-buffer.")

(emerge-defvar-local emerge-difference-list nil
  "Vector of differences between the variants, and markers in the buffers to
show where they are.  Each difference is represented by a vector of seven
elements.  The first two are markers to the beginning and end of the difference
section in the A buffer, the second two are markers for the B buffer, the third
two are markers for the merge buffer, and the last element is the \"state\" of
that difference in the merge buffer.
  A section of a buffer is described by two markers, one to the beginning of
the first line of the section, and one to the beginning of the first line
after the section.  (If the section is empty, both markers point to the same
point.)  If the section is part of the selected difference, then the markers
are moved into the flags, so the user can edit the section without disturbing
the markers.
  The \"states\" are:
	A		the merge buffer currently contains the A variant
	B		the merge buffer currently contains the B variant
	default-A	the merge buffer contains the A variant by default,
			but this difference hasn't been selected yet, so
			change-default commands can alter it
	default-B	the merge buffer contains the B variant by default,
			but this difference hasn't been selected yet, so
			change-default commands can alter it
	prefer-A	in a three-file merge, the A variant is the preferred
			choice
	prefer-B	in a three-file merge, the B variant is the preferred
			choice")
(emerge-defvar-local emerge-current-difference -1
  "The difference that is currently selected.")
(emerge-defvar-local emerge-number-of-differences nil
  "Number of differences found.")
(emerge-defvar-local emerge-edit-keymap nil
  "The local keymap for the merge buffer, with the emerge commands defined in
it.  Used to save the local keymap during fast mode, when the local keymap is
replaced by emerge-fast-keymap.")
(emerge-defvar-local emerge-old-keymap nil
  "The original local keymap for the merge buffer.")
(emerge-defvar-local emerge-auto-advance nil
  "*If non-nil, emerge-select-A and emerge-select-B automatically advance to
the next difference.")
(emerge-defvar-local emerge-skip-prefers nil
  "*If non-nil, differences for which there is a preference are automatically
skipped.")
(emerge-defvar-local emerge-quit-hook nil
  "Hooks to run in the merge buffer after the merge has been finished.
`emerge-prefix-argument' will hold the prefix argument of the `emerge-quit'
command.
This is *not* a user option, since Emerge uses it for its own processing.")
(emerge-defvar-local emerge-output-description nil
  "Describes output destination of emerge, for `emerge-file-names'.")

;;; Setup functions for two-file mode.

(defun emerge-files-internal (file-A file-B &optional startup-hooks quit-hooks
                              output-file)
  (if (not (file-readable-p file-A))
      (error "File `%s' does not exist or is not readable" file-A))
  (if (not (file-readable-p file-B))
      (error "File `%s' does not exist or is not readable" file-B))
  (let ((buffer-A (find-file-noselect file-A))
	(buffer-B (find-file-noselect file-B)))
    ;; Record the directories of the files
    (setq emerge-last-dir-A (file-name-directory file-A))
    (setq emerge-last-dir-B (file-name-directory file-B))
    (if output-file
	(setq emerge-last-dir-output (file-name-directory output-file)))
    ;; Make sure the entire files are seen, and they reflect what is on disk
    (with-current-buffer
     buffer-A
     (widen)
     (let ((temp (file-local-copy file-A)))
       (if temp
	   (setq file-A temp
		 startup-hooks
		 (cons `(lambda () (delete-file ,file-A))
		       startup-hooks))
           ;; Verify that the file matches the buffer
           (emerge-verify-file-buffer))))
    (with-current-buffer
     buffer-B
     (widen)
     (let ((temp (file-local-copy file-B)))
       (if temp
	   (setq file-B temp
		 startup-hooks
		 (cons `(lambda () (delete-file ,file-B))
		       startup-hooks))
           ;; Verify that the file matches the buffer
           (emerge-verify-file-buffer))))
    (emerge-setup buffer-A file-A buffer-B file-B startup-hooks quit-hooks
		  output-file)))

;; Start up Emerge on two files
(defun emerge-setup (buffer-A file-A buffer-B file-B startup-hooks quit-hooks
			      output-file)
  (setq file-A (expand-file-name file-A))
  (setq file-B (expand-file-name file-B))
  (setq output-file (and output-file (expand-file-name output-file)))
  (let* ((merge-buffer-name (emerge-unique-buffer-name "*merge" "*"))
	 ;; create the merge buffer from buffer A, so it inherits buffer A's
	 ;; default directory, etc.
	 (merge-buffer (with-current-buffer
			buffer-A
			(get-buffer-create merge-buffer-name))))
    (with-current-buffer
     merge-buffer
     (emerge-copy-modes buffer-A)
     (setq buffer-read-only nil)
     (auto-save-mode 1)
     (setq emerge-mode t)
     (setq emerge-A-buffer buffer-A)
     (setq emerge-B-buffer buffer-B)
     (setq emerge-ancestor-buffer nil)
     (setq emerge-merge-buffer merge-buffer)
     (setq emerge-output-description
	   (if output-file
	       (concat "Output to file: " output-file)
	     (concat "Output to buffer: " (buffer-name merge-buffer))))
     (save-excursion (insert-buffer-substring emerge-A-buffer))
     (emerge-set-keys)
     (setq emerge-difference-list (emerge-make-diff-list file-A file-B))
     (setq emerge-number-of-differences (length emerge-difference-list))
     (setq emerge-current-difference -1)
     (setq emerge-quit-hook quit-hooks)
     (emerge-remember-buffer-characteristics)
     (emerge-handle-local-variables))
    (emerge-setup-windows buffer-A buffer-B merge-buffer t)
    (with-current-buffer merge-buffer
			   (run-hooks 'startup-hooks 'emerge-startup-hook)
			   (setq buffer-read-only t))))

;; Generate the Emerge difference list between two files
(defun emerge-make-diff-list (file-A file-B)
  (setq emerge-diff-buffer (get-buffer-create "*emerge-diff*"))
  (with-current-buffer
   emerge-diff-buffer
   (erase-buffer)
   (shell-command
    (format "%s %s %s %s"
	    emerge-diff-program emerge-diff-options
	    (emerge-protect-metachars file-A)
	    (emerge-protect-metachars file-B))
    t))
  (emerge-prepare-error-list emerge-diff-ok-lines-regexp)
  (emerge-convert-diffs-to-markers
   emerge-A-buffer emerge-B-buffer emerge-merge-buffer
   (emerge-extract-diffs emerge-diff-buffer)))

(defun emerge-extract-diffs (diff-buffer)
  (let (list)
    (with-current-buffer
     diff-buffer
     (goto-char (point-min))
     (while (re-search-forward emerge-match-diff-line nil t)
       (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
                                                           (match-end 1))))
	      (a-end  (let ((b (match-beginning 3))
			    (e (match-end 3)))
			(if b
			    (string-to-number (buffer-substring b e))
			  a-begin)))
	      (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	      (b-begin (string-to-number (buffer-substring (match-beginning 5)
                                                           (match-end 5))))
	      (b-end (let ((b (match-beginning 7))
			   (e (match-end 7)))
		       (if b
			   (string-to-number (buffer-substring b e))
			 b-begin))))
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	 (if (string-equal diff-type "a")
	     (progn
	       (setq b-end (1+ b-end))
	       (setq a-begin (1+ a-begin))
	       (setq a-end a-begin))
	   (if (string-equal diff-type "d")
	       (progn
		 (setq a-end (1+ a-end))
		 (setq b-begin (1+ b-begin))
		 (setq b-end b-begin))
	     ;; (string-equal diff-type "c")
	     (progn
	       (setq a-end (1+ a-end))
	       (setq b-end (1+ b-end)))))
	 (setq list (cons (vector a-begin a-end
				  b-begin b-end
				  'default-A)
			  list)))))
    (nreverse list)))

;; Set up buffer of diff/diff3 error messages.
(defun emerge-prepare-error-list (ok-regexp)
  (setq emerge-diff-error-buffer (get-buffer-create "*emerge-diff-errors*"))
  (with-current-buffer
   emerge-diff-error-buffer
   (erase-buffer)
   (save-excursion (insert-buffer-substring emerge-diff-buffer))
   (delete-matching-lines ok-regexp)))

;;; Top-level and setup functions for three-file mode.

(defun emerge-files-with-ancestor-internal (file-A file-B file-ancestor
					    &optional startup-hooks quit-hooks
					    output-file)
  (if (not (file-readable-p file-A))
      (error "File `%s' does not exist or is not readable" file-A))
  (if (not (file-readable-p file-B))
      (error "File `%s' does not exist or is not readable" file-B))
  (if (not (file-readable-p file-ancestor))
      (error "File `%s' does not exist or is not readable" file-ancestor))
  (let ((buffer-A (find-file-noselect file-A))
	(buffer-B (find-file-noselect file-B))
	(buffer-ancestor (find-file-noselect file-ancestor)))
    ;; Record the directories of the files
    (setq emerge-last-dir-A (file-name-directory file-A))
    (setq emerge-last-dir-B (file-name-directory file-B))
    (setq emerge-last-dir-ancestor (file-name-directory file-ancestor))
    (if output-file
	(setq emerge-last-dir-output (file-name-directory output-file)))
    ;; Make sure the entire files are seen, and they reflect what is on disk
    (with-current-buffer
     buffer-A
     (widen)
     (let ((temp (file-local-copy file-A)))
       (if temp
	   (setq file-A temp
		 startup-hooks
		 (cons `(lambda () (delete-file ,file-A))
		       startup-hooks))
           ;; Verify that the file matches the buffer
           (emerge-verify-file-buffer))))
    (with-current-buffer
     buffer-B
     (widen)
     (let ((temp (file-local-copy file-B)))
       (if temp
	   (setq file-B temp
		 startup-hooks
		 (cons `(lambda () (delete-file ,file-B))
		       startup-hooks))
           ;; Verify that the file matches the buffer
           (emerge-verify-file-buffer))))
    (with-current-buffer
     buffer-ancestor
     (widen)
     (let ((temp (file-local-copy file-ancestor)))
       (if temp
	   (setq file-ancestor temp
		 startup-hooks
		 (cons `(lambda () (delete-file ,file-ancestor))
		       startup-hooks))
           ;; Verify that the file matches the buffer
           (emerge-verify-file-buffer))))
    (emerge-setup-with-ancestor buffer-A file-A buffer-B file-B
				buffer-ancestor file-ancestor
				startup-hooks quit-hooks output-file)))

;; Start up Emerge on two files with an ancestor
(defun emerge-setup-with-ancestor (buffer-A file-A buffer-B file-B
					    buffer-ancestor file-ancestor
					    &optional startup-hooks quit-hooks
					    output-file)
  (setq file-A (expand-file-name file-A))
  (setq file-B (expand-file-name file-B))
  (setq file-ancestor (expand-file-name file-ancestor))
  (setq output-file (and output-file (expand-file-name output-file)))
  (let* ((merge-buffer-name (emerge-unique-buffer-name "*merge" "*"))
	 ;; create the merge buffer from buffer A, so it inherits buffer A's
	 ;; default directory, etc.
	 (merge-buffer (with-current-buffer
			buffer-A
			(get-buffer-create merge-buffer-name))))
    (with-current-buffer
     merge-buffer
     (emerge-copy-modes buffer-A)
     (setq buffer-read-only nil)
     (auto-save-mode 1)
     (setq emerge-mode t)
     (setq emerge-A-buffer buffer-A)
     (setq emerge-B-buffer buffer-B)
     (setq emerge-ancestor-buffer buffer-ancestor)
     (setq emerge-merge-buffer merge-buffer)
     (setq emerge-output-description
	   (if output-file
	       (concat "Output to file: " output-file)
	     (concat "Output to buffer: " (buffer-name merge-buffer))))
     (save-excursion (insert-buffer-substring emerge-A-buffer))
     (emerge-set-keys)
     (setq emerge-difference-list
	   (emerge-make-diff3-list file-A file-B file-ancestor))
     (setq emerge-number-of-differences (length emerge-difference-list))
     (setq emerge-current-difference -1)
     (setq emerge-quit-hook quit-hooks)
     (emerge-remember-buffer-characteristics)
     (emerge-select-prefer-Bs)
     (emerge-handle-local-variables))
    (emerge-setup-windows buffer-A buffer-B merge-buffer t)
    (with-current-buffer merge-buffer
			   (run-hooks 'startup-hooks 'emerge-startup-hook)
			   (setq buffer-read-only t))))

;; Generate the Emerge difference list between two files with an ancestor
(defun emerge-make-diff3-list (file-A file-B file-ancestor)
  (setq emerge-diff-buffer (get-buffer-create "*emerge-diff*"))
  (with-current-buffer
   emerge-diff-buffer
   (erase-buffer)
   (shell-command
    (format "%s %s %s %s %s"
	    emerge-diff3-program emerge-diff-options
	    (emerge-protect-metachars file-A)
	    (emerge-protect-metachars file-ancestor)
	    (emerge-protect-metachars file-B))
    t))
  (emerge-prepare-error-list emerge-diff3-ok-lines-regexp)
  (emerge-convert-diffs-to-markers
   emerge-A-buffer emerge-B-buffer emerge-merge-buffer
   (emerge-extract-diffs3 emerge-diff-buffer)))

(defun emerge-extract-diffs3 (diff-buffer)
  (let (list)
    (with-current-buffer
     diff-buffer
     (while (re-search-forward "^====\\(.?\\)$" nil t)
       ;; leave point after matched line
       (beginning-of-line 2)
       (let ((agreement (buffer-substring (match-beginning 1) (match-end 1))))
	 ;; if the A and B files are the same, ignore the difference
	 (if (not (string-equal agreement "2"))
	     (setq list
		   (cons
		    (let (group-1 group-3 pos)
		      (setq pos (point))
		      (setq group-1 (emerge-get-diff3-group "1"))
		      (goto-char pos)
		      (setq group-3 (emerge-get-diff3-group "3"))
		      (vector (car group-1) (car (cdr group-1))
			      (car group-3) (car (cdr group-3))
			      (cond ((string-equal agreement "1") 'prefer-A)
				    ((string-equal agreement "3") 'prefer-B)
				    (t 'default-A))))
		    list))))))
    (nreverse list)))

(defun emerge-get-diff3-group (file)
  ;; This save-excursion allows emerge-get-diff3-group to be called for the
  ;; various groups of lines (1, 2, 3) in any order, and for the lines to
  ;; appear in any order.  The reason this is necessary is that Gnu diff3
  ;; can produce the groups in the order 1, 2, 3 or 1, 3, 2.
  (save-excursion
    (re-search-forward
     (concat "^" file ":\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?\\([ac]\\)$"))
    (beginning-of-line 2)
    ;; treatment depends on whether it is an "a" group or a "c" group
    (if (string-equal (buffer-substring (match-beginning 4) (match-end 4)) "c")
	;; it is a "c" group
	(if (match-beginning 2)
	    ;; it has two numbers
	    (list (string-to-number
		   (buffer-substring (match-beginning 1) (match-end 1)))
		  (1+ (string-to-number
		       (buffer-substring (match-beginning 3) (match-end 3)))))
	  ;; it has one number
	  (let ((x (string-to-number
		    (buffer-substring (match-beginning 1) (match-end 1)))))
	    (list x (1+ x))))
      ;; it is an "a" group
      (let ((x (1+ (string-to-number
		    (buffer-substring (match-beginning 1) (match-end 1))))))
	(list x x)))))

;;; Functions to start Emerge on files

;;;###autoload
(defun emerge-files (arg file-A file-B file-out &optional startup-hooks
		     quit-hooks)
  "Run Emerge on two files."
  (interactive
   (let (f)
     (list current-prefix-arg
	   (setq f (emerge-read-file-name "File A to merge" emerge-last-dir-A
					  nil nil t))
	   (emerge-read-file-name "File B to merge" emerge-last-dir-B nil f t)
	   (and current-prefix-arg
		(emerge-read-file-name "Output file" emerge-last-dir-output
				       f f nil)))))
  (if file-out
      (add-hook 'quit-hooks `(lambda () (emerge-files-exit ,file-out))))
  (emerge-files-internal
   file-A file-B startup-hooks
   quit-hooks
   file-out))

;;;###autoload
(defun emerge-files-with-ancestor (arg file-A file-B file-ancestor file-out
				   &optional startup-hooks quit-hooks)
  "Run Emerge on two files, giving another file as the ancestor."
  (interactive
   (let (f)
     (list current-prefix-arg
	   (setq f (emerge-read-file-name "File A to merge" emerge-last-dir-A
					  nil nil t))
	   (emerge-read-file-name "File B to merge" emerge-last-dir-B nil f t)
	   (emerge-read-file-name "Ancestor file" emerge-last-dir-ancestor
				  nil f t)
	   (and current-prefix-arg
		(emerge-read-file-name "Output file" emerge-last-dir-output
				       f f nil)))))
  (if file-out
      (add-hook 'quit-hooks `(lambda () (emerge-files-exit ,file-out))))
  (emerge-files-with-ancestor-internal
   file-A file-B file-ancestor startup-hooks
   quit-hooks
   file-out))

;; Write the merge buffer out in place of the file the A buffer is visiting.
(defun emerge-files-exit (file-out)
  ;; if merge was successful was given, save to disk
  (if (not emerge-prefix-argument)
      (emerge-write-and-delete file-out)))

;;; Functions to start Emerge on buffers

;;;###autoload
(defun emerge-buffers (buffer-A buffer-B &optional startup-hooks quit-hooks)
  "Run Emerge on two buffers."
  (interactive "bBuffer A to merge: \nbBuffer B to merge: ")
  (let ((emerge-file-A (emerge-make-temp-file "A"))
	(emerge-file-B (emerge-make-temp-file "B")))
    (with-current-buffer
     buffer-A
     (write-region (point-min) (point-max) emerge-file-A nil 'no-message))
    (with-current-buffer
     buffer-B
     (write-region (point-min) (point-max) emerge-file-B nil 'no-message))
    (emerge-setup (get-buffer buffer-A) emerge-file-A
		  (get-buffer buffer-B) emerge-file-B
		  (cons `(lambda ()
                          (delete-file ,emerge-file-A)
                          (delete-file ,emerge-file-B))
			startup-hooks)
		  quit-hooks
		  nil)))

;;;###autoload
(defun emerge-buffers-with-ancestor (buffer-A buffer-B buffer-ancestor
                                     &optional startup-hooks
                                     quit-hooks)
  "Run Emerge on two buffers, giving another buffer as the ancestor."
  (interactive
   "bBuffer A to merge: \nbBuffer B to merge: \nbAncestor buffer: ")
  (let ((emerge-file-A (emerge-make-temp-file "A"))
	(emerge-file-B (emerge-make-temp-file "B"))
	(emerge-file-ancestor (emerge-make-temp-file "anc")))
    (with-current-buffer
     buffer-A
     (write-region (point-min) (point-max) emerge-file-A nil 'no-message))
    (with-current-buffer
     buffer-B
     (write-region (point-min) (point-max) emerge-file-B nil 'no-message))
    (with-current-buffer
     buffer-ancestor
     (write-region (point-min) (point-max) emerge-file-ancestor nil
		   'no-message))
    (emerge-setup-with-ancestor (get-buffer buffer-A) emerge-file-A
				(get-buffer buffer-B) emerge-file-B
				(get-buffer buffer-ancestor)
				emerge-file-ancestor
				(cons `(lambda ()
                                        (delete-file ,emerge-file-A)
                                        (delete-file ,emerge-file-B)
                                        (delete-file
                                         ,emerge-file-ancestor))
				      startup-hooks)
				quit-hooks
				nil)))

;;; Functions to start Emerge from the command line

;;;###autoload
(defun emerge-files-command ()
  (let ((file-a (nth 0 command-line-args-left))
	(file-b (nth 1 command-line-args-left))
	(file-out (nth 2 command-line-args-left)))
    (setq command-line-args-left (nthcdr 3 command-line-args-left))
    (emerge-files-internal
     file-a file-b nil
     (list `(lambda () (emerge-command-exit ,file-out))))))

;;;###autoload
(defun emerge-files-with-ancestor-command ()
  (let (file-a file-b file-anc file-out)
    ;; check for a -a flag, for filemerge compatibility
    (if (string= (car command-line-args-left) "-a")
	;; arguments are "-a ancestor file-a file-b file-out"
	(progn
	  (setq file-a (nth 2 command-line-args-left))
	  (setq file-b (nth 3 command-line-args-left))
	  (setq file-anc (nth 1 command-line-args-left))
	  (setq file-out (nth 4 command-line-args-left))
	  (setq command-line-args-left (nthcdr 5 command-line-args-left)))
        ;; arguments are "file-a file-b ancestor file-out"
        (setq file-a (nth 0 command-line-args-left))
        (setq file-b (nth 1 command-line-args-left))
        (setq file-anc (nth 2 command-line-args-left))
        (setq file-out (nth 3 command-line-args-left))
        (setq command-line-args-left (nthcdr 4 command-line-args-left)))
    (emerge-files-with-ancestor-internal
     file-a file-b file-anc nil
     (list `(lambda () (emerge-command-exit ,file-out))))))

(defun emerge-command-exit (file-out)
  (emerge-write-and-delete file-out)
  (kill-emacs (if emerge-prefix-argument 1 0)))

;;; Functions to start Emerge via remote request

;;;###autoload
(defun emerge-files-remote (file-a file-b file-out)
  (setq emerge-file-out file-out)
  (emerge-files-internal
   file-a file-b nil
   (list `(lambda () (emerge-remote-exit ,file-out ',emerge-exit-func)))
   file-out)
  (throw 'client-wait nil))

;;;###autoload
(defun emerge-files-with-ancestor-remote (file-a file-b file-anc file-out)
  (setq emerge-file-out file-out)
  (emerge-files-with-ancestor-internal
   file-a file-b file-anc nil
   (list `(lambda () (emerge-remote-exit ,file-out ',emerge-exit-func)))
   file-out)
  (throw 'client-wait nil))

(defun emerge-remote-exit (file-out emerge-exit-func)
  (emerge-write-and-delete file-out)
  (kill-buffer emerge-merge-buffer)
  (funcall emerge-exit-func (if emerge-prefix-argument 1 0)))

;;; Functions to start Emerge on RCS versions

;;;###autoload
(defun emerge-revisions (arg file revision-A revision-B
			 &optional startup-hooks quit-hooks)
  "Emerge two RCS revisions of a file."
  (interactive
   (list current-prefix-arg
	 (read-file-name "File to merge: " nil nil 'confirm)
	 (read-string "Revision A to merge: " emerge-last-revision-A)
	 (read-string "Revision B to merge: " emerge-last-revision-B)))
  (setq emerge-last-revision-A revision-A
	emerge-last-revision-B revision-B)
  (emerge-revisions-internal
   file revision-A revision-B startup-hooks
   (if arg
       (cons `(lambda ()
               (shell-command
                ,(format "%s %s" emerge-rcs-ci-program file)))
	     quit-hooks)
       quit-hooks)))

;;;###autoload
(defun emerge-revisions-with-ancestor (arg file revision-A
                                       revision-B ancestor
                                       &optional
                                       startup-hooks quit-hooks)
  "Emerge two RCS revisions of a file, with another revision as ancestor."
  (interactive
   (list current-prefix-arg
	 (read-file-name "File to merge: " nil nil 'confirm)
	 (read-string "Revision A to merge: " emerge-last-revision-A)
	 (read-string "Revision B to merge: " emerge-last-revision-B)
	 (read-string "Ancestor: " emerge-last-revision-ancestor)))
  (setq emerge-last-revision-A revision-A
	emerge-last-revision-B revision-B
	emerge-last-revision-ancestor ancestor)
  (emerge-revision-with-ancestor-internal
   file revision-A revision-B ancestor startup-hooks
   (if arg
       (let ((cmd ))
	 (cons `(lambda ()
                 (shell-command
                  ,(format "%s %s" emerge-rcs-ci-program file)))
	       quit-hooks))
       quit-hooks)))

(defun emerge-revisions-internal (file revision-A revision-B &optional
                                  startup-hooks quit-hooks output-file)
  (let ((buffer-A (get-buffer-create (format "%s,%s" file revision-A)))
	(buffer-B (get-buffer-create (format "%s,%s" file revision-B)))
	(emerge-file-A (emerge-make-temp-file "A"))
	(emerge-file-B (emerge-make-temp-file "B")))
    ;; Get the revisions into buffers
    (with-current-buffer
     buffer-A
     (erase-buffer)
     (shell-command
      (format "%s -q -p%s %s" emerge-rcs-co-program revision-A file)
      t)
     (write-region (point-min) (point-max) emerge-file-A nil 'no-message)
     (set-buffer-modified-p nil))
    (with-current-buffer
     buffer-B
     (erase-buffer)
     (shell-command
      (format "%s -q -p%s %s" emerge-rcs-co-program revision-B file)
      t)
     (write-region (point-min) (point-max) emerge-file-B nil 'no-message)
     (set-buffer-modified-p nil))
    ;; Do the merge
    (emerge-setup buffer-A emerge-file-A
		  buffer-B emerge-file-B
		  (cons `(lambda ()
                          (delete-file ,emerge-file-A)
                          (delete-file ,emerge-file-B))
			startup-hooks)
		  (cons `(lambda () (emerge-files-exit ,file))
			quit-hooks)
		  nil)))

(defun emerge-revision-with-ancestor-internal (file revision-A revision-B
                                               ancestor
                                               &optional startup-hooks
                                               quit-hooks output-file)
  (let ((buffer-A (get-buffer-create (format "%s,%s" file revision-A)))
	(buffer-B (get-buffer-create (format "%s,%s" file revision-B)))
	(buffer-ancestor (get-buffer-create (format "%s,%s" file ancestor)))
	(emerge-file-A (emerge-make-temp-file "A"))
	(emerge-file-B (emerge-make-temp-file "B"))
	(emerge-ancestor (emerge-make-temp-file "ancestor")))
    ;; Get the revisions into buffers
    (with-current-buffer
     buffer-A
     (erase-buffer)
     (shell-command
      (format "%s -q -p%s %s" emerge-rcs-co-program
	      revision-A file)
      t)
     (write-region (point-min) (point-max) emerge-file-A nil 'no-message)
     (set-buffer-modified-p nil))
    (with-current-buffer
     buffer-B
     (erase-buffer)
     (shell-command
      (format "%s -q -p%s %s" emerge-rcs-co-program revision-B file)
      t)
     (write-region (point-min) (point-max) emerge-file-B nil 'no-message)
     (set-buffer-modified-p nil))
    (with-current-buffer
     buffer-ancestor
     (erase-buffer)
     (shell-command
      (format "%s -q -p%s %s" emerge-rcs-co-program ancestor file)
      t)
     (write-region (point-min) (point-max) emerge-ancestor nil 'no-message)
     (set-buffer-modified-p nil))
    ;; Do the merge
    (emerge-setup-with-ancestor
     buffer-A emerge-file-A buffer-B emerge-file-B
     buffer-ancestor emerge-ancestor
     (cons `(lambda ()
             (delete-file ,emerge-file-A)
             (delete-file ,emerge-file-B)
             (delete-file ,emerge-ancestor))
	   startup-hooks)
     (cons `(lambda () (emerge-files-exit ,file))
	   quit-hooks)
     output-file)))

;;; Function to start Emerge based on a line in a file

(defun emerge-execute-line ()
  "Run Emerge using files named in current text line.
Looks in that line for whitespace-separated entries of these forms:
	a=file1
	b=file2
	ancestor=file3
	output=file4
to specify the files to use in Emerge.

In addition, if only one of `a=file' or `b=file' is present, and `output=file'
is present:
If `emerge-execute-line-deletions' is non-nil and `ancestor=file' is present,
it is assumed that the file in question has been deleted, and it is
not copied to the output file.
Otherwise, the A or B file present is copied to the output file."
  (interactive)
  (let (file-A file-B file-ancestor file-out
	       (case-fold-search t))
    ;; Stop if at end of buffer (even though we might be in a line, if
    ;; the line does not end with newline)
    (if (eobp)
	(error "At end of buffer"))
    ;; Go to the beginning of the line
    (beginning-of-line)
    ;; Skip any initial whitespace
    (if (looking-at "[ \t]*")
	(goto-char (match-end 0)))
    ;; Process the entire line
    (while (not (eolp))
      ;; Get the next entry
      (if (looking-at "\\([a-z]+\\)=\\([^ \t\n]+\\)[ \t]*")
	  ;; Break apart the tab (before =) and the filename (after =)
	  (let ((tag (downcase
		      (buffer-substring (match-beginning 1) (match-end 1))))
		(file (buffer-substring (match-beginning 2) (match-end 2))))
	    ;; Move point after the entry
	    (goto-char (match-end 0))
	    ;; Store the filename in the right variable
	    (cond
              ((string-equal tag "a")
               (if file-A
                   (error "This line has two `A' entries"))
               (setq file-A file))
              ((string-equal tag "b")
               (if file-B
                   (error "This line has two `B' entries"))
               (setq file-B file))
              ((or (string-equal tag "anc") (string-equal tag "ancestor"))
               (if file-ancestor
                   (error "This line has two `ancestor' entries"))
               (setq file-ancestor file))
              ((or (string-equal tag "out") (string-equal tag "output"))
               (if file-out
                   (error "This line has two `output' entries"))
               (setq file-out file))
              (t
               (error "Unrecognized entry"))))
          ;; If the match on the entry pattern failed
          (error "Unparsable entry")))
    ;; Make sure that file-A and file-B are present
    (if (not (or (and file-A file-B) file-out))
	(error "Must have both `A' and `B' entries"))
    (if (not (or file-A file-B))
	(error "Must have `A' or `B' entry"))
    ;; Go to the beginning of the next line, so next execution will use
    ;; next line in buffer.
    (beginning-of-line 2)
    ;; Execute the correct command
    (cond
      ;; Merge of two files with ancestor
      ((and file-A file-B file-ancestor)
       (message "Merging %s and %s..." file-A file-B)
       (emerge-files-with-ancestor (not (not file-out)) file-A file-B
                                   file-ancestor file-out
                                   nil
                                   ;; When done, return to this buffer.
                                   (list
                                    `(lambda ()
                                      (switch-to-buffer ,(current-buffer))
                                      (message "Merge done.")))))
      ;; Merge of two files without ancestor
      ((and file-A file-B)
       (message "Merging %s and %s..." file-A file-B)
       (emerge-files (not (not file-out)) file-A file-B file-out
                     nil
                     ;; When done, return to this buffer.
                     (list
                      `(lambda ()
                        (switch-to-buffer ,(current-buffer))
                        (message "Merge done.")))))
      ;; There is an output file (or there would have been an error above),
      ;; but only one input file.
      ;; The file appears to have been deleted in one version; do nothing.
      ((and file-ancestor emerge-execute-line-deletions)
       (message "No action."))
      ;; The file should be copied from the version that contains it
      (t (let ((input-file (or file-A file-B)))
           (message "Copying...")
           (copy-file input-file file-out)
           (message "%s copied to %s." input-file file-out))))))

;;; Sample function for creating information for emerge-execute-line

(defcustom emerge-merge-directories-filename-regexp "[^.]"
  "Regexp describing files to be processed by `emerge-merge-directories'."
  :type 'regexp
  :group 'emerge)

;;;###autoload
(defun emerge-merge-directories (a-dir b-dir ancestor-dir output-dir)
  (interactive
   (list
    (read-directory-name "A directory: " nil nil 'confirm)
    (read-directory-name "B directory: " nil nil 'confirm)
    (read-directory-name "Ancestor directory (null for none): " nil nil 'confirm)
    (read-directory-name "Output directory (null for none): " nil nil 'confirm)))
  ;; Check that we're not on a line
  (if (not (and (bolp) (eolp)))
      (error "There is text on this line"))
  ;; Turn null strings into nil to indicate directories not used.
  (if (and ancestor-dir (string-equal ancestor-dir ""))
      (setq ancestor-dir nil))
  (if (and output-dir (string-equal output-dir ""))
      (setq output-dir nil))
  ;; Canonicalize the directory names
  (setq a-dir (expand-file-name a-dir))
  (if (not (string-equal (substring a-dir -1) "/"))
      (setq a-dir (concat a-dir "/")))
  (setq b-dir (expand-file-name b-dir))
  (if (not (string-equal (substring b-dir -1) "/"))
      (setq b-dir (concat b-dir "/")))
  (if ancestor-dir
      (progn
	(setq ancestor-dir (expand-file-name ancestor-dir))
	(if (not (string-equal (substring ancestor-dir -1) "/"))
	    (setq ancestor-dir (concat ancestor-dir "/")))))
  (if output-dir
      (progn
	(setq output-dir (expand-file-name output-dir))
	(if (not (string-equal (substring output-dir -1) "/"))
	    (setq output-dir (concat output-dir "/")))))
  ;; Set the mark to where we start
  (push-mark)
  ;; Find out what files are in the directories.
  (let* ((a-dir-files
	  (directory-files a-dir nil emerge-merge-directories-filename-regexp))
	 (b-dir-files
	  (directory-files b-dir nil emerge-merge-directories-filename-regexp))
	 (ancestor-dir-files
	  (and ancestor-dir
	       (directory-files ancestor-dir nil
				emerge-merge-directories-filename-regexp)))
	 (all-files (sort (nconc (copy-sequence a-dir-files)
				 (copy-sequence b-dir-files)
				 (copy-sequence ancestor-dir-files))
			  (function string-lessp))))
    ;; Remove duplicates from all-files.
    (let ((p all-files))
      (while p
	(if (and (cdr p) (string-equal (car p) (car (cdr p))))
	    (setcdr p (cdr (cdr p)))
	  (setq p (cdr p)))))
    ;; Generate the control lines for the various files.
    (while all-files
      (let ((f (car all-files)))
	(setq all-files (cdr all-files))
	(if (and a-dir-files (string-equal (car a-dir-files) f))
	    (progn
	      (insert "A=" a-dir f "\t")
	      (setq a-dir-files (cdr a-dir-files))))
	(if (and b-dir-files (string-equal (car b-dir-files) f))
	    (progn
	      (insert "B=" b-dir f "\t")
	      (setq b-dir-files (cdr b-dir-files))))
	(if (and ancestor-dir-files (string-equal (car ancestor-dir-files) f))
	    (progn
	      (insert "ancestor=" ancestor-dir f "\t")
	      (setq ancestor-dir-files (cdr ancestor-dir-files))))
	(if output-dir
	    (insert "output=" output-dir f "\t"))
	(backward-delete-char 1)
	(insert "\n")))))

;;; Common setup routines

;; Set up the window configuration.  If POS is given, set the points to
;; the beginnings of the buffers.
(defun emerge-setup-windows (buffer-A buffer-B merge-buffer &optional pos)
  ;; Make sure we are not in the minibuffer window when we try to delete
  ;; all other windows.
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))
  (delete-other-windows)
  (switch-to-buffer merge-buffer)
  (emerge-refresh-mode-line)
  (split-window-below)
  (split-window-right)
  (switch-to-buffer buffer-A)
  (if pos
      (goto-char (point-min)))
  (other-window 1)
  (switch-to-buffer buffer-B)
  (if pos
      (goto-char (point-min)))
  (other-window 1)
  (if pos
      (goto-char (point-min)))
  ;; If diff/diff3 reports errors, display them rather than the merge buffer.
  (if (/= 0 (with-current-buffer emerge-diff-error-buffer (buffer-size)))
      (progn
	(ding)
	(message "Errors found in diff/diff3 output.  Merge buffer is %s."
		 (buffer-name emerge-merge-buffer))
	(switch-to-buffer emerge-diff-error-buffer))))

;; Set up the keymap in the merge buffer
(defun emerge-set-keys ()
  ;; Set up fixed keymaps if necessary
  (if (not emerge-basic-keymap)
      (emerge-setup-fixed-keymaps))
  ;; Save the old local map
  (setq emerge-old-keymap (current-local-map))
  ;; Construct the edit keymap
  (setq emerge-edit-keymap (if emerge-old-keymap
			       (copy-keymap emerge-old-keymap)
			     (make-sparse-keymap)))
  ;; Install the Emerge commands
  (emerge-force-define-key emerge-edit-keymap emerge-command-prefix
			   'emerge-basic-keymap)
  (define-key emerge-edit-keymap [menu-bar] (make-sparse-keymap))

  ;; Create the additional menu bar items.
  (define-key emerge-edit-keymap [menu-bar emerge-options]
    (cons "Merge-Options" emerge-options-menu))
  (define-key emerge-edit-keymap [menu-bar merge]
    (cons "Merge" emerge-merge-menu))
  (define-key emerge-edit-keymap [menu-bar move]
    (cons "Move" emerge-move-menu))

  ;; Suppress write-file and save-buffer
  (substitute-key-definition 'write-file
			     'emerge-query-write-file
			     emerge-edit-keymap)
  (substitute-key-definition 'save-buffer
			     'emerge-query-save-buffer
			     emerge-edit-keymap)
  (define-key emerge-edit-keymap [remap write-file] 'emerge-query-write-file)
  (define-key emerge-edit-keymap [remap save-buffer] 'emerge-query-save-buffer)
  (use-local-map emerge-fast-keymap)
  (setq emerge-edit-mode nil)
  (setq emerge-fast-mode t))

(defun emerge-remember-buffer-characteristics ()
  "Record certain properties of the buffers being merged.
Must be called in the merge buffer.  Remembers read-only, modified,
auto-save, and saves them in buffer local variables.  Sets the buffers
read-only and turns off `auto-save-mode'.
These characteristics are restored by `emerge-restore-buffer-characteristics'."
  ;; force auto-save, because we will turn off auto-saving in buffers for the
  ;; duration
  (do-auto-save)
  ;; remember and alter buffer characteristics
  (setq emerge-A-buffer-values
	(with-current-buffer
	 emerge-A-buffer
	 (prog1
	     (emerge-save-variables emerge-saved-variables)
	   (emerge-restore-variables emerge-saved-variables
				     emerge-merging-values))))
  (setq emerge-B-buffer-values
	(with-current-buffer
	 emerge-B-buffer
	 (prog1
	     (emerge-save-variables emerge-saved-variables)
	   (emerge-restore-variables emerge-saved-variables
				     emerge-merging-values)))))

(defun emerge-restore-buffer-characteristics ()
  "Restore characteristics saved by `emerge-remember-buffer-characteristics'."
  (let ((A-values emerge-A-buffer-values)
	(B-values emerge-B-buffer-values))
    (with-current-buffer emerge-A-buffer
			   (emerge-restore-variables emerge-saved-variables
						     A-values))
    (with-current-buffer emerge-B-buffer
			   (emerge-restore-variables emerge-saved-variables
						     B-values))))

;; Move to line DESIRED-LINE assuming we are at line CURRENT-LINE.
;; Return DESIRED-LINE.
(defun emerge-goto-line (desired-line current-line)
  (forward-line (- desired-line current-line))
  desired-line)

(defun emerge-convert-diffs-to-markers (A-buffer
					B-buffer
					merge-buffer
					lineno-list)
  (let* (marker-list
	 (A-point-min (with-current-buffer A-buffer (point-min)))
	 (offset (1- A-point-min))
	 (B-point-min (with-current-buffer B-buffer (point-min)))
	 ;; Record current line number in each buffer
	 ;; so we don't have to count from the beginning.
	 (a-line 1)
	 (b-line 1))
    (with-current-buffer A-buffer (goto-char (point-min)))
    (with-current-buffer B-buffer (goto-char (point-min)))
    (while lineno-list
      (let* ((list-element (car lineno-list))
	     a-begin-marker
	     a-end-marker
	     b-begin-marker
	     b-end-marker
	     merge-begin-marker
	     merge-end-marker
	     (a-begin (aref list-element 0))
	     (a-end (aref list-element 1))
	     (b-begin (aref list-element 2))
	     (b-end (aref list-element 3))
	     (state (aref list-element 4)))
	;; place markers at the appropriate places in the buffers
	(with-current-buffer
	 A-buffer
	 (setq a-line (emerge-goto-line a-begin a-line))
	 (setq a-begin-marker (point-marker))
	 (setq a-line (emerge-goto-line a-end a-line))
	 (setq a-end-marker (point-marker)))
	(with-current-buffer
	 B-buffer
	 (setq b-line (emerge-goto-line b-begin b-line))
	 (setq b-begin-marker (point-marker))
	 (setq b-line (emerge-goto-line b-end b-line))
	 (setq b-end-marker (point-marker)))
	(setq merge-begin-marker (set-marker
				  (make-marker)
				  (- (marker-position a-begin-marker)
				     offset)
				  merge-buffer))
	(setq merge-end-marker (set-marker
				(make-marker)
				(- (marker-position a-end-marker)
				   offset)
				merge-buffer))
	;; record all the markers for this difference
	(setq marker-list (cons (vector a-begin-marker a-end-marker
					b-begin-marker b-end-marker
					merge-begin-marker merge-end-marker
					state)
				marker-list)))
      (setq lineno-list (cdr lineno-list)))
    ;; convert the list of difference information into a vector for
    ;; fast access
    (setq emerge-difference-list (apply 'vector (nreverse marker-list)))))

;; If we have an ancestor, select all B variants that we prefer
(defun emerge-select-prefer-Bs ()
  (let ((n 0))
    (while (< n emerge-number-of-differences)
      (if (eq (aref (aref emerge-difference-list n) 6) 'prefer-B)
	  (progn
	    (emerge-unselect-and-select-difference n t)
	    (emerge-select-B)
	    (aset (aref emerge-difference-list n) 6 'prefer-B)))
      (setq n (1+ n))))
  (emerge-unselect-and-select-difference -1))

;; Process the local-variables list at the end of the merged file, if
;; requested.
(defun emerge-handle-local-variables ()
  (if emerge-process-local-variables
      (condition-case err
	  (hack-local-variables)
	(error (message "Local-variables error in merge buffer: %s"
			(prin1-to-string err))))))

;;; Common exit routines

(defun emerge-write-and-delete (file-out)
  ;; clear screen format
  (delete-other-windows)
  ;; delete A, B, and ancestor buffers, if they haven't been changed
  (if (not (buffer-modified-p emerge-A-buffer))
      (kill-buffer emerge-A-buffer))
  (if (not (buffer-modified-p emerge-B-buffer))
      (kill-buffer emerge-B-buffer))
  (if (and emerge-ancestor-buffer
	   (not (buffer-modified-p emerge-ancestor-buffer)))
      (kill-buffer emerge-ancestor-buffer))
  ;; Write merge buffer to file
  (and file-out
       (write-file file-out)))

;;; Commands

(defun emerge-recenter (&optional arg)
  "Bring the highlighted region of all three merge buffers into view.
This brings the buffers into view if they are in windows.
With an argument, reestablish the default three-window display."
  (interactive "P")
  ;; If there is an argument, rebuild the window structure
  (if arg
      (emerge-setup-windows emerge-A-buffer emerge-B-buffer
			    emerge-merge-buffer))
  ;; Redisplay whatever buffers are showing, if there is a selected difference
  (if (and (>= emerge-current-difference 0)
	   (< emerge-current-difference emerge-number-of-differences))
      (let* ((merge-buffer emerge-merge-buffer)
	     (buffer-A emerge-A-buffer)
	     (buffer-B emerge-B-buffer)
	     (window-A (get-buffer-window buffer-A 'visible))
	     (window-B (get-buffer-window buffer-B 'visible))
	     (merge-window (get-buffer-window merge-buffer))
	     (diff-vector
	      (aref emerge-difference-list emerge-current-difference)))
	(if window-A (progn
		       (select-window window-A)
		       (emerge-position-region
			(- (aref diff-vector 0)
			   (1- emerge-before-flag-length))
			(+ (aref diff-vector 1)
			   (1- emerge-after-flag-length))
			(1+ (aref diff-vector 0)))))
	(if window-B (progn
		       (select-window window-B)
		       (emerge-position-region
			(- (aref diff-vector 2)
			   (1- emerge-before-flag-length))
			(+ (aref diff-vector 3)
			   (1- emerge-after-flag-length))
			(1+ (aref diff-vector 2)))))
	(if merge-window (progn
			   (select-window merge-window)
			   (emerge-position-region
			    (- (aref diff-vector 4)
			       (1- emerge-before-flag-length))
			    (+ (aref diff-vector 5)
			       (1- emerge-after-flag-length))
			    (1+ (aref diff-vector 4))))))))

;;; Window scrolling operations
;; These operations are designed to scroll all three windows the same amount,
;; so as to keep the text in them aligned.

;; Perform some operation on all three windows (if they are showing).
;; Catches all errors on the operation in the A and B windows, but not
;; in the merge window.  Usually, errors come from scrolling off the
;; beginning or end of the buffer, and this gives a nice error message:
;; End of buffer is reported in the merge buffer, but if the scroll was
;; possible in the A or B windows, it is performed there before the error
;; is reported.
(defun emerge-operate-on-windows (operation arg)
  (let* ((merge-buffer emerge-merge-buffer)
	 (buffer-A emerge-A-buffer)
	 (buffer-B emerge-B-buffer)
	 (window-A (get-buffer-window buffer-A 'visible))
	 (window-B (get-buffer-window buffer-B 'visible))
	 (merge-window (get-buffer-window merge-buffer)))
    (if window-A (progn
		   (select-window window-A)
		   (condition-case nil
		       (funcall operation arg)
		     (error))))
    (if window-B (progn
		   (select-window window-B)
		   (condition-case nil
		       (funcall operation arg)
		     (error))))
    (if merge-window (progn
		       (select-window merge-window)
		       (funcall operation arg)))))

(defun emerge-scroll-up (&optional arg)
  "Scroll up all three merge buffers, if they are in windows.
With argument N, scroll N lines; otherwise scroll by nearly
the height of the merge window.
`C-u -' alone as argument scrolls half the height of the merge window."
  (interactive "P")
  (emerge-operate-on-windows
   'scroll-up
   ;; calculate argument to scroll-up
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let ((merge-window (get-buffer-window emerge-merge-buffer)))
       (if (null merge-window)
	   ;; no window, use nil
	   nil
	 (let ((default-amount
		 (- (window-height merge-window) 1 next-screen-context-lines)))
	   ;; the window was found
	   (if arg
	       ;; C-u as argument means half of default amount
	       (/ default-amount 2)
	     ;; no argument means default amount
	     default-amount)))))))

(defun emerge-scroll-down (&optional arg)
  "Scroll down all three merge buffers, if they are in windows.
With argument N, scroll N lines; otherwise scroll by nearly
the height of the merge window.
`C-u -' alone as argument scrolls half the height of the merge window."
  (interactive "P")
  (emerge-operate-on-windows
   'scroll-down
   ;; calculate argument to scroll-down
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let ((merge-window (get-buffer-window emerge-merge-buffer)))
       (if (null merge-window)
	   ;; no window, use nil
	   nil
	 (let ((default-amount
		 (- (window-height merge-window) 1 next-screen-context-lines)))
	   ;; the window was found
	   (if arg
	       ;; C-u as argument means half of default amount
	       (/ default-amount 2)
	     ;; no argument means default amount
	     default-amount)))))))

(defun emerge-scroll-left (&optional arg)
  "Scroll left all three merge buffers, if they are in windows.
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A and B windows.  `C-u -' alone as argument scrolls half the
width of the A and B windows."
  (interactive "P")
  (emerge-operate-on-windows
   'scroll-left
   ;; calculate argument to scroll-left
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (let ((merge-window (get-buffer-window emerge-merge-buffer)))
       (if (null merge-window)
	   ;; no window, use nil
	   nil
	 (let ((default-amount
		 (- (/ (window-width merge-window) 2) 3)))
	   ;; the window was found
	   (if arg
	       ;; C-u as argument means half of default amount
	       (/ default-amount 2)
	     ;; no argument means default amount
	     default-amount)))))))

(defun emerge-scroll-right (&optional arg)
  "Scroll right all three merge buffers, if they are in windows.
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A and B windows.  `C-u -' alone as argument scrolls half the
width of the A and B windows."
  (interactive "P")
  (emerge-operate-on-windows
   'scroll-right
   ;; calculate argument to scroll-right
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (let ((merge-window (get-buffer-window emerge-merge-buffer)))
       (if (null merge-window)
	   ;; no window, use nil
	   nil
	 (let ((default-amount
		 (- (/ (window-width merge-window) 2) 3)))
	   ;; the window was found
	   (if arg
	       ;; C-u as argument means half of default amount
	       (/ default-amount 2)
	     ;; no argument means default amount
	     default-amount)))))))

(defun emerge-scroll-reset ()
  "Reset horizontal scrolling in Emerge.
This resets the horizontal scrolling of all three merge buffers
to the left margin, if they are in windows."
  (interactive)
  (emerge-operate-on-windows
   (lambda (x) (set-window-hscroll (selected-window) 0))
   nil))

;; Attempt to show the region nicely.
;; If there are min-lines lines above and below the region, then don't do
;; anything.
;; If not, recenter the region to make it so.
;; If that isn't possible, remove context lines evenly from top and bottom
;; so the entire region shows.
;; If that isn't possible, show the top of the region.
;; BEG must be at the beginning of a line.
(defun emerge-position-region (beg end pos)
  ;; First test whether the entire region is visible with
  ;; emerge-min-visible-lines above and below it
  (if (not (and (<= (progn
		      (move-to-window-line emerge-min-visible-lines)
		      (point))
		    beg)
		(<= end (progn
			  (move-to-window-line
			   (- (1+ emerge-min-visible-lines)))
			  (point)))))
      ;; We failed that test, see if it fits at all
      ;; Meanwhile positioning it correctly in case it doesn't fit
      (progn
	(set-window-start (selected-window) beg)
	(if (pos-visible-in-window-p end)
	    ;; Determine the number of lines that the region occupies
	    (let ((lines 0))
	      (while (> end (progn
			      (move-to-window-line lines)
			      (point)))
		(setq lines (1+ lines)))
	      ;; And position the beginning on the right line
	      (goto-char beg)
	      (recenter (/ (1+ (- (1- (window-height (selected-window)))
				  lines))
			   2))))))
  (goto-char pos))

(defun emerge-next-difference ()
  "Advance to the next difference."
  (interactive)
  (if (< emerge-current-difference emerge-number-of-differences)
      (let ((n (1+ emerge-current-difference)))
	(while (and emerge-skip-prefers
		    (< n emerge-number-of-differences)
		    (memq (aref (aref emerge-difference-list n) 6)
			  '(prefer-A prefer-B)))
	  (setq n (1+ n)))
	(let ((buffer-read-only nil))
	  (emerge-unselect-and-select-difference n)))
    (error "At end")))

(defun emerge-previous-difference ()
  "Go to the previous difference."
  (interactive)
  (if (> emerge-current-difference -1)
      (let ((n (1- emerge-current-difference)))
	(while (and emerge-skip-prefers
		    (> n -1)
		    (memq (aref (aref emerge-difference-list n) 6)
			  '(prefer-A prefer-B)))
	  (setq n (1- n)))
	(let ((buffer-read-only nil))
	  (emerge-unselect-and-select-difference n)))
    (error "At beginning")))

(defun emerge-jump-to-difference (difference-number)
  "Go to the N-th difference."
  (interactive "p")
  (let ((buffer-read-only nil))
    (setq difference-number (1- difference-number))
    (if (and (>= difference-number -1)
	     (< difference-number (1+ emerge-number-of-differences)))
	(emerge-unselect-and-select-difference difference-number)
      (error "Bad difference number"))))

(defun emerge-abort ()
  "Abort the Emerge session."
  (interactive)
  (emerge-quit t))

(defun emerge-quit (arg)
  "Finish the Emerge session and exit Emerge.
Prefix argument means to abort rather than successfully finish.
The difference depends on how the merge was started,
but usually means to not write over one of the original files, or to signal
to some process which invoked Emerge a failure code.

Unselects the selected difference, if any, restores the read-only and modified
flags of the merged file buffers, restores the local keymap of the merge
buffer, and sets off various emerge flags.  Using Emerge commands in this
buffer after this will cause serious problems."
  (interactive "P")
  (if (prog1
	  (y-or-n-p
	   (if (not arg)
	       "Do you really want to successfully finish this merge? "
	     "Do you really want to abort this merge? "))
	(message ""))
      (emerge-really-quit arg)))

;; Perform the quit operations.
(defun emerge-really-quit (arg)
  (setq buffer-read-only nil)
  (emerge-unselect-and-select-difference -1)
  (emerge-restore-buffer-characteristics)
  ;; null out the difference markers so they don't slow down future editing
  ;; operations
  (mapc (lambda (d)
	  (set-marker (aref d 0) nil)
	  (set-marker (aref d 1) nil)
	  (set-marker (aref d 2) nil)
	  (set-marker (aref d 3) nil)
	  (set-marker (aref d 4) nil)
	  (set-marker (aref d 5) nil))
	  emerge-difference-list)
  ;; allow them to be garbage collected
  (setq emerge-difference-list nil)
  ;; restore the local map
  (use-local-map emerge-old-keymap)
  ;; turn off all the emerge modes
  (setq emerge-mode nil)
  (setq emerge-fast-mode nil)
  (setq emerge-edit-mode nil)
  (setq emerge-auto-advance nil)
  (setq emerge-skip-prefers nil)
  ;; restore mode line
  (kill-local-variable 'mode-line-buffer-identification)
  (let ((emerge-prefix-argument arg))
    (run-hooks 'emerge-quit-hook)))

(defun emerge-select-A (&optional force)
  "Select the A variant of this difference.
Refuses to function if this difference has been edited, i.e., if it
is neither the A nor the B variant.
A prefix argument forces the variant to be selected
even if the difference has been edited."
  (interactive "P")
  (let ((operate
	 (lambda ()
	   (emerge-select-A-edit merge-begin merge-end A-begin A-end)
	   (if emerge-auto-advance
	       (emerge-next-difference))))
	(operate-no-change
	 (lambda () (if emerge-auto-advance
			(emerge-next-difference)))))
    (emerge-select-version force operate-no-change operate operate)))

;; Actually select the A variant
(defun emerge-select-A-edit (merge-begin merge-end A-begin A-end)
  (with-current-buffer
   emerge-merge-buffer
   (delete-region merge-begin merge-end)
   (goto-char merge-begin)
   (insert-buffer-substring emerge-A-buffer A-begin A-end)
   (goto-char merge-begin)
   (aset diff-vector 6 'A)
   (emerge-refresh-mode-line)))

(defun emerge-select-B (&optional force)
  "Select the B variant of this difference.
Refuses to function if this difference has been edited, i.e., if it
is neither the A nor the B variant.
A prefix argument forces the variant to be selected
even if the difference has been edited."
  (interactive "P")
  (let ((operate
	 (lambda ()
	   (emerge-select-B-edit merge-begin merge-end B-begin B-end)
	   (if emerge-auto-advance
	       (emerge-next-difference))))
	(operate-no-change
	 (lambda () (if emerge-auto-advance
			(emerge-next-difference)))))
    (emerge-select-version force operate operate-no-change operate)))

;; Actually select the B variant
(defun emerge-select-B-edit (merge-begin merge-end B-begin B-end)
  (with-current-buffer
   emerge-merge-buffer
   (delete-region merge-begin merge-end)
   (goto-char merge-begin)
   (insert-buffer-substring emerge-B-buffer B-begin B-end)
   (goto-char merge-begin)
   (aset diff-vector 6 'B)
   (emerge-refresh-mode-line)))

(defun emerge-default-A ()
  "Make the A variant the default from here down.
This selects the A variant for all differences from here down in the buffer
which are still defaulted, i.e., which the user has not selected and for
which there is no preference."
  (interactive)
  (let ((buffer-read-only nil))
    (let ((selected-difference emerge-current-difference)
	  (n (max emerge-current-difference 0)))
      (while (< n emerge-number-of-differences)
	(let ((diff-vector (aref emerge-difference-list n)))
	  (if (eq (aref diff-vector 6) 'default-B)
	      (progn
		(emerge-unselect-and-select-difference n t)
		(emerge-select-A)
		(aset diff-vector 6 'default-A))))
	(setq n (1+ n))
	(if (zerop (% n 10))
	    (message "Setting default to A...%d" n)))
      (emerge-unselect-and-select-difference selected-difference)))
  (message "Default choice is now A"))

(defun emerge-default-B ()
  "Make the B variant the default from here down.
This selects the B variant for all differences from here down in the buffer
which are still defaulted, i.e., which the user has not selected and for
which there is no preference."
  (interactive)
  (let ((buffer-read-only nil))
    (let ((selected-difference emerge-current-difference)
	  (n (max emerge-current-difference 0)))
      (while (< n emerge-number-of-differences)
	(let ((diff-vector (aref emerge-difference-list n)))
	  (if (eq (aref diff-vector 6) 'default-A)
	      (progn
		(emerge-unselect-and-select-difference n t)
		(emerge-select-B)
		(aset diff-vector 6 'default-B))))
	(setq n (1+ n))
	(if (zerop (% n 10))
	    (message "Setting default to B...%d" n)))
      (emerge-unselect-and-select-difference selected-difference)))
  (message "Default choice is now B"))

(defun emerge-fast-mode ()
  "Set fast mode, for Emerge.
In this mode ordinary Emacs commands are disabled, and Emerge commands
need not be prefixed with \\<emerge-fast-keymap>\\[emerge-basic-keymap]."
  (interactive)
  (setq buffer-read-only t)
  (use-local-map emerge-fast-keymap)
  (setq emerge-mode t)
  (setq emerge-fast-mode t)
  (setq emerge-edit-mode nil)
  (message "Fast mode set")
  (force-mode-line-update))

(defun emerge-edit-mode ()
  "Set edit mode, for Emerge.
In this mode ordinary Emacs commands are available, and Emerge commands
must be prefixed with \\<emerge-fast-keymap>\\[emerge-basic-keymap]."
  (interactive)
  (setq buffer-read-only nil)
  (use-local-map emerge-edit-keymap)
  (setq emerge-mode t)
  (setq emerge-fast-mode nil)
  (setq emerge-edit-mode t)
  (message "Edit mode set")
  (force-mode-line-update))

(defun emerge-auto-advance (arg)
  "Toggle Auto-Advance mode, for Emerge.
This mode causes `emerge-select-A' and `emerge-select-B' to automatically
advance to the next difference.
With a positive argument, turn on Auto-Advance mode.
With a negative argument, turn off Auto-Advance mode."
  (interactive "P")
  (setq emerge-auto-advance (if (null arg)
				(not emerge-auto-advance)
			      (> (prefix-numeric-value arg) 0)))
  (message (if emerge-auto-advance
	       "Auto-advance set"
	     "Auto-advance cleared"))
  (force-mode-line-update))

(defun emerge-skip-prefers (arg)
  "Toggle Skip-Prefers mode, for Emerge.
This mode causes `emerge-next-difference' and `emerge-previous-difference'
to automatically skip over differences for which there is a preference.
With a positive argument, turn on Skip-Prefers mode.
With a negative argument, turn off Skip-Prefers mode."
  (interactive "P")
  (setq emerge-skip-prefers (if (null arg)
				(not emerge-skip-prefers)
			      (> (prefix-numeric-value arg) 0)))
  (message (if emerge-skip-prefers
	       "Skip-prefers set"
	     "Skip-prefers cleared"))
  (force-mode-line-update))

(defun emerge-copy-as-kill-A ()
  "Put the A variant of this difference in the kill ring."
  (interactive)
  (emerge-validate-difference)
  (let* ((diff-vector
	  (aref emerge-difference-list emerge-current-difference))
	 (A-begin (1+ (aref diff-vector 0)))
	 (A-end (1- (aref diff-vector 1)))
	 ;; so further kills don't append
	 this-command)
    (with-current-buffer emerge-A-buffer
      (copy-region-as-kill A-begin A-end))))

(defun emerge-copy-as-kill-B ()
  "Put the B variant of this difference in the kill ring."
  (interactive)
  (emerge-validate-difference)
  (let* ((diff-vector
	  (aref emerge-difference-list emerge-current-difference))
	 (B-begin (1+ (aref diff-vector 2)))
	 (B-end (1- (aref diff-vector 3)))
	 ;; so further kills don't append
	 this-command)
    (with-current-buffer emerge-B-buffer
      (copy-region-as-kill B-begin B-end))))

(defun emerge-insert-A (arg)
  "Insert the A variant of this difference at the point.
Leaves point after text, mark before.
With prefix argument, puts point before, mark after."
  (interactive "P")
  (emerge-validate-difference)
  (let* ((diff-vector
	  (aref emerge-difference-list emerge-current-difference))
	 (A-begin (1+ (aref diff-vector 0)))
	 (A-end (1- (aref diff-vector 1)))
	 (opoint (point))
	 (buffer-read-only nil))
    (insert-buffer-substring emerge-A-buffer A-begin A-end)
    (if (not arg)
	(set-mark opoint)
      (set-mark (point))
      (goto-char opoint))))

(defun emerge-insert-B (arg)
  "Insert the B variant of this difference at the point.
Leaves point after text, mark before.
With prefix argument, puts point before, mark after."
  (interactive "P")
  (emerge-validate-difference)
  (let* ((diff-vector
	  (aref emerge-difference-list emerge-current-difference))
	 (B-begin (1+ (aref diff-vector 2)))
	 (B-end (1- (aref diff-vector 3)))
	 (opoint (point))
	 (buffer-read-only nil))
    (insert-buffer-substring emerge-B-buffer B-begin B-end)
    (if (not arg)
	(set-mark opoint)
      (set-mark (point))
      (goto-char opoint))))

(defun emerge-mark-difference (arg)
  "Leaves the point before this difference and the mark after it.
With prefix argument, puts mark before, point after."
  (interactive "P")
  (emerge-validate-difference)
  (let* ((diff-vector
	  (aref emerge-difference-list emerge-current-difference))
	 (merge-begin (1+ (aref diff-vector 4)))
	 (merge-end (1- (aref diff-vector 5))))
    (if (not arg)
	(progn
	  (goto-char merge-begin)
	  (set-mark merge-end))
      (goto-char merge-end)
      (set-mark merge-begin))))

(defun emerge-file-names ()
  "Show the names of the buffers or files being operated on by Emerge.
Use C-u l to reset the windows afterward."
  (interactive)
  (delete-other-windows)
  (let ((temp-buffer-show-function
	 (lambda (buf)
	   (split-window-below)
	   (switch-to-buffer buf)
	   (other-window 1))))
    (with-output-to-temp-buffer "*Help*"
      (with-current-buffer emerge-A-buffer
			     (if buffer-file-name
				 (progn
				   (princ "File A is: ")
				   (princ buffer-file-name))
			       (progn
				 (princ "Buffer A is: ")
				 (princ (buffer-name))))
			     (princ "\n"))
      (with-current-buffer emerge-B-buffer
			     (if buffer-file-name
				 (progn
				   (princ "File B is: ")
				   (princ buffer-file-name))
			       (progn
				 (princ "Buffer B is: ")
				 (princ (buffer-name))))
			     (princ "\n"))
      (if emerge-ancestor-buffer
	    (with-current-buffer emerge-ancestor-buffer
				   (if buffer-file-name
				       (progn
					 (princ "Ancestor file is: ")
					 (princ buffer-file-name))
				     (progn
				       (princ "Ancestor buffer is: ")
				       (princ (buffer-name))))
				   (princ "\n")))
      (princ emerge-output-description)
      (with-current-buffer standard-output
	(help-mode)))))

(defun emerge-join-differences (arg)
  "Join the selected difference with the following one.
With a prefix argument, join with the preceding one."
  (interactive "P")
  (let ((n emerge-current-difference))
    ;; adjust n to be first difference to join
    (if arg
	(setq n (1- n)))
    ;; n and n+1 are the differences to join
    ;; check that they are both differences
    (if (or (< n 0) (>= n (1- emerge-number-of-differences)))
	(error "Incorrect differences to join"))
    ;; remove the flags
    (emerge-unselect-difference emerge-current-difference)
    ;; decrement total number of differences
    (setq emerge-number-of-differences (1- emerge-number-of-differences))
    ;; build new differences vector
    (let ((i 0)
	  (new-differences (make-vector emerge-number-of-differences nil)))
      (while (< i emerge-number-of-differences)
	(aset new-differences i
	      (cond
	       ((< i n) (aref emerge-difference-list i))
	       ((> i n) (aref emerge-difference-list (1+ i)))
	       (t (let ((prev (aref emerge-difference-list i))
			(next (aref emerge-difference-list (1+ i))))
		    (vector (aref prev 0)
			    (aref next 1)
			    (aref prev 2)
			    (aref next 3)
			    (aref prev 4)
			    (aref next 5)
			    (let ((ps (aref prev 6))
				  (ns (aref next 6)))
			      (cond
			       ((eq ps ns)
				ps)
			       ((and (or (eq ps 'B) (eq ps 'prefer-B))
				     (or (eq ns 'B) (eq ns 'prefer-B)))
				'B)
			       (t 'A))))))))
	(setq i (1+ i)))
      (setq emerge-difference-list new-differences))
    ;; set the current difference correctly
    (setq emerge-current-difference n)
    ;; fix the mode line
    (emerge-refresh-mode-line)
    ;; reinsert the flags
    (emerge-select-difference emerge-current-difference)
    (emerge-recenter)))

(defun emerge-split-difference ()
  "Split the current difference where the points are in the three windows."
  (interactive)
  (let ((n emerge-current-difference))
    ;; check that this is a valid difference
    (emerge-validate-difference)
    ;; get the point values and old difference
    (let ((A-point (with-current-buffer emerge-A-buffer
					  (point-marker)))
	  (B-point (with-current-buffer emerge-B-buffer
					  (point-marker)))
	  (merge-point (point-marker))
	  (old-diff (aref emerge-difference-list n)))
      ;; check location of the points, give error if they aren't in the
      ;; differences
      (if (or (< A-point (aref old-diff 0))
	      (> A-point (aref old-diff 1)))
	  (error "Point outside of difference in A buffer"))
      (if (or (< B-point (aref old-diff 2))
	      (> B-point (aref old-diff 3)))
	  (error "Point outside of difference in B buffer"))
      (if (or (< merge-point (aref old-diff 4))
	      (> merge-point (aref old-diff 5)))
	  (error "Point outside of difference in merge buffer"))
      ;; remove the flags
      (emerge-unselect-difference emerge-current-difference)
      ;; increment total number of differences
      (setq emerge-number-of-differences (1+ emerge-number-of-differences))
      ;; build new differences vector
      (let ((i 0)
	    (new-differences (make-vector emerge-number-of-differences nil)))
	(while (< i emerge-number-of-differences)
	  (aset new-differences i
		(cond
		 ((< i n)
		  (aref emerge-difference-list i))
		 ((> i (1+ n))
		  (aref emerge-difference-list (1- i)))
		 ((= i n)
		  (vector (aref old-diff 0)
			  A-point
			  (aref old-diff 2)
			  B-point
			  (aref old-diff 4)
			  merge-point
			  (aref old-diff 6)))
		 (t
		  (vector (copy-marker A-point)
			  (aref old-diff 1)
			  (copy-marker B-point)
			  (aref old-diff 3)
			  (copy-marker merge-point)
			  (aref old-diff 5)
			  (aref old-diff 6)))))
	  (setq i (1+ i)))
	(setq emerge-difference-list new-differences))
      ;; set the current difference correctly
      (setq emerge-current-difference n)
      ;; fix the mode line
      (emerge-refresh-mode-line)
      ;; reinsert the flags
      (emerge-select-difference emerge-current-difference)
      (emerge-recenter))))

(defun emerge-trim-difference ()
  "Trim lines off top and bottom of difference that are the same.
If lines are the same in both the A and the B versions, strip them off.
\(This can happen when the A and B versions have common lines that the
ancestor version does not share.)"
  (interactive)
  ;; make sure we are in a real difference
  (emerge-validate-difference)
  ;; remove the flags
  (emerge-unselect-difference emerge-current-difference)
  (let* ((diff (aref emerge-difference-list emerge-current-difference))
	 (top-a (marker-position (aref diff 0)))
	 (bottom-a (marker-position (aref diff 1)))
	 (top-b (marker-position (aref diff 2)))
	 (bottom-b (marker-position (aref diff 3)))
	 (top-m (marker-position (aref diff 4)))
	 (bottom-m (marker-position (aref diff 5)))
	 size success sa sb sm)
    ;; move down the tops of the difference regions as much as possible
    ;; Try advancing comparing 1000 chars at a time.
    ;; When that fails, go 500 chars at a time, and so on.
    (setq size 1000)
    (while (> size 0)
      (setq success t)
      (while success
	(setq size (min size (- bottom-a top-a) (- bottom-b top-b)
			(- bottom-m top-m)))
	(setq sa (with-current-buffer emerge-A-buffer
					(buffer-substring top-a
							  (+ size top-a))))
	(setq sb (with-current-buffer emerge-B-buffer
					(buffer-substring top-b
							  (+ size top-b))))
	(setq sm (buffer-substring top-m (+ size top-m)))
	(setq success (and (> size 0) (equal sa sb) (equal sb sm)))
	(if success
	    (setq top-a (+ top-a size)
		  top-b (+ top-b size)
		  top-m (+ top-m size))))
      (setq size (/ size 2)))
    ;; move up the bottoms of the difference regions as much as possible
    ;; Try advancing comparing 1000 chars at a time.
    ;; When that fails, go 500 chars at a time, and so on.
    (setq size 1000)
    (while (> size 0)
      (setq success t)
      (while success
	(setq size (min size (- bottom-a top-a) (- bottom-b top-b)
			(- bottom-m top-m)))
	(setq sa (with-current-buffer emerge-A-buffer
					(buffer-substring (- bottom-a size)
							  bottom-a)))
	(setq sb (with-current-buffer emerge-B-buffer
					(buffer-substring (- bottom-b size)
							  bottom-b)))
	(setq sm (buffer-substring (- bottom-m size) bottom-m))
	(setq success (and (> size 0) (equal sa sb) (equal sb sm)))
	(if success
	    (setq bottom-a (- bottom-a size)
		  bottom-b (- bottom-b size)
		  bottom-m (- bottom-m size))))
      (setq size (/ size 2)))
    ;; {top,bottom}-{a,b,m} are now set at the new beginnings and ends
    ;; of the difference regions.  Move them to the beginning of lines, as
    ;; appropriate.
    (with-current-buffer emerge-A-buffer
			   (goto-char top-a)
			   (beginning-of-line)
			   (aset diff 0 (point-marker))
			   (goto-char bottom-a)
			   (beginning-of-line 2)
			   (aset diff 1 (point-marker)))
    (with-current-buffer emerge-B-buffer
			   (goto-char top-b)
			   (beginning-of-line)
			   (aset diff 2 (point-marker))
			   (goto-char bottom-b)
			   (beginning-of-line 2)
			   (aset diff 3 (point-marker)))
    (goto-char top-m)
    (beginning-of-line)
    (aset diff 4 (point-marker))
    (goto-char bottom-m)
    (beginning-of-line 2)
    (aset diff 5 (point-marker))
    ;; put the flags back in, recenter the display
    (emerge-select-difference emerge-current-difference)
    (emerge-recenter)))

;; FIXME the manual advertised this as working in the A or B buffers,
;; but it does not, because all the buffer locals are nil there.
;; It would work to call it from the merge buffer and specify that one
;; wants to use the value of point in the A or B buffer.
;; But with the prefix argument already in use, there is no easy way
;; to have it ask for a buffer.
(defun emerge-find-difference (arg)
  "Find the difference containing the current position of the point.
If there is no containing difference and the prefix argument is positive,
it finds the nearest following difference.  A negative prefix argument finds
the nearest previous difference."
  (interactive "P")
  (cond ((eq (current-buffer) emerge-A-buffer)
	 (emerge-find-difference-A arg))
	((eq (current-buffer) emerge-B-buffer)
	 (emerge-find-difference-B arg))
	(t (emerge-find-difference-merge arg))))

(defun emerge-find-difference-merge (arg)
  "Find the difference containing point, in the merge buffer.
If there is no containing difference and the prefix argument is positive,
it finds the nearest following difference.  A negative prefix argument finds
the nearest previous difference."
  (interactive "P")
  ;; search for the point in the merge buffer, using the markers
  ;; for the beginning and end of the differences in the merge buffer
  (emerge-find-difference1 arg (point) 4 5))

(defun emerge-find-difference-A (arg)
  "Find the difference containing point, in the A buffer.
This command must be executed in the merge buffer.
If there is no containing difference and the prefix argument is positive,
it finds the nearest following difference.  A negative prefix argument finds
the nearest previous difference."
  (interactive "P")
  ;; search for the point in the A buffer, using the markers
  ;; for the beginning and end of the differences in the A buffer
  (emerge-find-difference1 arg
			   (with-current-buffer emerge-A-buffer (point))
			   0 1))

(defun emerge-find-difference-B (arg)
  "Find the difference containing point, in the B buffer.
This command must be executed in the merge buffer.
If there is no containing difference and the prefix argument is positive,
it finds the nearest following difference.  A negative prefix argument finds
the nearest previous difference."
  (interactive "P")
  ;; search for the point in the B buffer, using the markers
  ;; for the beginning and end of the differences in the B buffer
  (emerge-find-difference1 arg
			   (with-current-buffer emerge-B-buffer (point))
			   2 3))

(defun emerge-find-difference1 (arg location begin end)
  (let* ((index
	  ;; find first difference containing or after the current position
	  (catch 'search
	    (let ((n 0))
	      (while (< n emerge-number-of-differences)
		(let ((diff-vector (aref emerge-difference-list n)))
		  (if (<= location (marker-position (aref diff-vector end)))
		      (throw 'search n)))
		(setq n (1+ n))))
	    emerge-number-of-differences))
	 (contains
	  ;; whether the found difference contains the current position
	  (and (< index emerge-number-of-differences)
	       (<= (marker-position (aref (aref emerge-difference-list index)
					  begin))
		   location)))
	 (arg-value
	  ;; numeric value of prefix argument
	  (prefix-numeric-value arg)))
    (emerge-unselect-and-select-difference
     (cond
      ;; if the point is in a difference, select it
      (contains index)
      ;; if the arg is nil and the point is not in a difference, error
      ((null arg) (error "No difference contains point"))
      ;; if the arg is positive, select the following difference
      ((> arg-value 0)
       (if (< index emerge-number-of-differences)
	   index
	 (error "No difference contains or follows point")))
      ;; if the arg is negative, select the preceding difference
      (t
       (if (> index 0)
	   (1- index)
	 (error "No difference contains or precedes point")))))))

(defun emerge-line-numbers ()
  "Display the current line numbers.
This function displays the line numbers of the points in the A, B, and
merge buffers."
  (interactive)
  (let* ((valid-diff
	 (and (>= emerge-current-difference 0)
	      (< emerge-current-difference emerge-number-of-differences)))
	 (emerge-line-diff (and valid-diff
				(aref emerge-difference-list
				      emerge-current-difference)))
	 (merge-line (emerge-line-number-in-buf 4 5))
	 (A-line (with-current-buffer emerge-A-buffer
					(emerge-line-number-in-buf 0 1)))
	 (B-line (with-current-buffer emerge-B-buffer
					(emerge-line-number-in-buf 2 3))))
    (message "At lines: merge = %d, A = %d, B = %d"
	     merge-line A-line B-line)))

(defvar emerge-line-diff)

(defun emerge-line-number-in-buf (begin-marker end-marker)
  ;; FIXME point-min rather than 1? widen?
  (let ((temp (1+ (count-lines 1 (line-beginning-position)))))
    (if valid-diff
	(progn
	  (if (> (point) (aref emerge-line-diff begin-marker))
	      (setq temp (- temp emerge-before-flag-lines)))
	  (if (> (point) (aref emerge-line-diff end-marker))
	      (setq temp (- temp emerge-after-flag-lines)))))
    temp))

(defun emerge-set-combine-template (string &optional localize)
  "Set `emerge-combine-versions-template' to STRING.
This value controls how `emerge-combine-versions' combines the two versions.
With prefix argument, `emerge-combine-versions-template' is made local to this
merge buffer.  Localization is permanent for any particular merge buffer."
  (interactive "s\nP")
  (if localize
      (make-local-variable 'emerge-combine-versions-template))
  (setq emerge-combine-versions-template string)
  (message
   (if (assq 'emerge-combine-versions-template (buffer-local-variables))
       "emerge-set-combine-versions-template set locally"
     "emerge-set-combine-versions-template set")))

(defun emerge-set-combine-versions-template (start end &optional localize)
  "Copy region into `emerge-combine-versions-template'.
This controls how `emerge-combine-versions' will combine the two versions.
With prefix argument, `emerge-combine-versions-template' is made local to this
merge buffer.  Localization is permanent for any particular merge buffer."
  (interactive "r\nP")
  (if localize
      (make-local-variable 'emerge-combine-versions-template))
  (setq emerge-combine-versions-template (buffer-substring start end))
  (message
   (if (assq 'emerge-combine-versions-template (buffer-local-variables))
       "emerge-set-combine-versions-template set locally."
     "emerge-set-combine-versions-template set.")))

(defun emerge-combine-versions (&optional force)
  "Combine versions using the template in `emerge-combine-versions-template'.
Refuses to function if this difference has been edited, i.e., if it is
neither the A nor the B variant.
An argument forces the variant to be selected even if the difference has
been edited."
  (interactive "P")
  (emerge-combine-versions-internal emerge-combine-versions-template force))

(defun emerge-combine-versions-register (char &optional force)
  "Combine the two versions using the template in register REG.
See documentation of the variable `emerge-combine-versions-template'
for how the template is interpreted.
Refuses to function if this difference has been edited, i.e., if it is
neither the A nor the B variant.
An argument forces the variant to be selected even if the difference has
been edited."
  (interactive "cRegister containing template: \nP")
  (let ((template (get-register char)))
    (if (not (stringp template))
	(error "Register does not contain text"))
    (emerge-combine-versions-internal template force)))

(defun emerge-combine-versions-internal (emerge-combine-template force)
  (let ((operate
	 (lambda ()
	   (emerge-combine-versions-edit merge-begin merge-end
					 A-begin A-end B-begin B-end)
	   (if emerge-auto-advance
	       (emerge-next-difference)))))
    (emerge-select-version force operate operate operate)))

(defvar emerge-combine-template)

(defun emerge-combine-versions-edit (merge-begin merge-end
				     A-begin A-end B-begin B-end)
  (with-current-buffer
   emerge-merge-buffer
   (delete-region merge-begin merge-end)
   (goto-char merge-begin)
   (let ((i 0))
     (while (< i (length emerge-combine-template))
       (let ((c (aref emerge-combine-template i)))
	 (if (= c ?%)
	     (progn
	       (setq i (1+ i))
	       (setq c
		     (condition-case nil
			 (aref emerge-combine-template i)
		       (error ?%)))
	       (cond ((= c ?a)
		      (insert-buffer-substring emerge-A-buffer A-begin A-end))
		     ((= c ?b)
		      (insert-buffer-substring emerge-B-buffer B-begin B-end))
		     ((= c ?%)
		      (insert ?%))
		     (t
		      (insert c))))
	   (insert c)))
       (setq i (1+ i))))
   (goto-char merge-begin)
   (aset diff-vector 6 'combined)
   (emerge-refresh-mode-line)))

(defun emerge-set-merge-mode (mode)
  "Set the major mode in a merge buffer.
Overrides any change that the mode might make to the mode line or local
keymap.  Leaves merge in fast mode."
  (interactive
   (list (intern (completing-read "New major mode for merge buffer: "
				  obarray 'commandp t nil))))
  (funcall mode)
  (emerge-refresh-mode-line)
  (if emerge-fast-mode
      (emerge-fast-mode)
    (emerge-edit-mode)))

(defun emerge-one-line-window ()
  (interactive)
  (let ((window-min-height 1))
    (shrink-window (- (window-height) 2))))

;;; Support routines

;; Select a difference by placing the visual flags around the appropriate
;; group of lines in the A, B, and merge buffers
(defun emerge-select-difference (n)
  (let ((emerge-globalized-difference-list emerge-difference-list)
	(emerge-globalized-number-of-differences emerge-number-of-differences))
    (emerge-place-flags-in-buffer emerge-A-buffer n 0 1)
    (emerge-place-flags-in-buffer emerge-B-buffer n 2 3)
    (emerge-place-flags-in-buffer nil n 4 5))
  (run-hooks 'emerge-select-hook))

(defun emerge-place-flags-in-buffer (buffer difference before-index
					    after-index)
  (if buffer
      (with-current-buffer
       buffer
       (emerge-place-flags-in-buffer1 difference before-index after-index))
    (emerge-place-flags-in-buffer1 difference before-index after-index)))

(defun emerge-place-flags-in-buffer1 (difference before-index after-index)
  (let ((buffer-read-only nil))
    ;; insert the flag before the difference
    (let ((before (aref (aref emerge-globalized-difference-list difference)
			before-index))
	  here)
      (goto-char before)
      ;; insert the flag itself
      (insert-before-markers emerge-before-flag)
      (setq here (point))
      ;; Put the marker(s) referring to this position 1 character before the
      ;; end of the flag, so it won't be damaged by the user.
      ;; This gets a bit tricky, as there could be a number of markers
      ;; that have to be moved.
      (set-marker before (1- before))
      (let ((n (1- difference)) after-marker before-marker diff-list)
	(while (and
		(>= n 0)
		(progn
		  (setq diff-list (aref emerge-globalized-difference-list n)
			after-marker (aref diff-list after-index))
		  (= after-marker here)))
	  (set-marker after-marker (1- after-marker))
	  (setq before-marker (aref diff-list before-index))
	  (if (= before-marker here)
	      (setq before-marker (1- before-marker)))
	  (setq n (1- n)))))
    ;; insert the flag after the difference
    (let* ((after (aref (aref emerge-globalized-difference-list difference)
			after-index))
	   (here (marker-position after)))
      (goto-char here)
      ;; insert the flag itself
      (insert emerge-after-flag)
      ;; Put the marker(s) referring to this position 1 character after the
      ;; beginning of the flag, so it won't be damaged by the user.
      ;; This gets a bit tricky, as there could be a number of markers
      ;; that have to be moved.
      (set-marker after (1+ after))
      (let ((n (1+ difference)) before-marker after-marker diff-list)
	(while (and
		(< n emerge-globalized-number-of-differences)
		(progn
		  (setq diff-list (aref emerge-globalized-difference-list n)
			before-marker (aref diff-list before-index))
		  (= before-marker here)))
	  (set-marker before-marker (1+ before-marker))
	  (setq after-marker (aref diff-list after-index))
	  (if (= after-marker here)
	      (setq after-marker (1+ after-marker)))
	  (setq n (1+ n)))))))

;; Unselect a difference by removing the visual flags in the buffers.
(defun emerge-unselect-difference (n)
  (let ((diff-vector (aref emerge-difference-list n)))
    (emerge-remove-flags-in-buffer emerge-A-buffer
				   (aref diff-vector 0) (aref diff-vector 1))
    (emerge-remove-flags-in-buffer emerge-B-buffer
				   (aref diff-vector 2) (aref diff-vector 3))
    (emerge-remove-flags-in-buffer emerge-merge-buffer
				   (aref diff-vector 4) (aref diff-vector 5)))
  (run-hooks 'emerge-unselect-hook))

(defun emerge-remove-flags-in-buffer (buffer before after)
  (with-current-buffer
   buffer
   (let ((buffer-read-only nil))
     ;; remove the flags, if they're there
     (goto-char (- before (1- emerge-before-flag-length)))
     (if (looking-at emerge-before-flag-match)
	 (delete-char emerge-before-flag-length)
       ;; the flag isn't there
       (ding)
       (message "Trouble removing flag"))
     (goto-char (1- after))
     (if (looking-at emerge-after-flag-match)
	 (delete-char emerge-after-flag-length)
       ;; the flag isn't there
       (ding)
       (message "Trouble removing flag")))))

;; Select a difference, removing any flags that exist now.
(defun emerge-unselect-and-select-difference (n &optional suppress-display)
  (if (and (>= emerge-current-difference 0)
	   (< emerge-current-difference emerge-number-of-differences))
      (emerge-unselect-difference emerge-current-difference))
  (if (and (>= n 0) (< n emerge-number-of-differences))
      (progn
	(emerge-select-difference n)
	(let* ((diff-vector (aref emerge-difference-list n))
	       (selection-type (aref diff-vector 6)))
	  (if (eq selection-type 'default-A)
	      (aset diff-vector 6 'A)
	    (if (eq selection-type 'default-B)
		(aset diff-vector 6 'B))))))
  (setq emerge-current-difference n)
  (if (not suppress-display)
      (progn
	(emerge-recenter)
	(emerge-refresh-mode-line))))

;; Perform tests to see whether user should be allowed to select a version
;; of this difference:
;;   a valid difference has been selected; and
;;   the difference text in the merge buffer is:
;;     the A version (execute a-version), or
;;     the B version (execute b-version), or
;;     empty (execute neither-version), or
;;     argument FORCE is true (execute neither-version)
;; Otherwise, signal an error.
(defun emerge-select-version (force a-version b-version neither-version)
  (emerge-validate-difference)
  (let ((buffer-read-only nil))
    (let* ((diff-vector
	    (aref emerge-difference-list emerge-current-difference))
	   (A-begin (1+ (aref diff-vector 0)))
	   (A-end (1- (aref diff-vector 1)))
	   (B-begin (1+ (aref diff-vector 2)))
	   (B-end (1- (aref diff-vector 3)))
	   (merge-begin (1+ (aref diff-vector 4)))
	   (merge-end (1- (aref diff-vector 5))))
      (if (emerge-compare-buffers emerge-A-buffer A-begin A-end
				  emerge-merge-buffer merge-begin
				  merge-end)
	  (funcall a-version)
	(if (emerge-compare-buffers emerge-B-buffer B-begin B-end
				    emerge-merge-buffer merge-begin
				    merge-end)
	    (funcall b-version)
	  (if (or force (= merge-begin merge-end))
	      (funcall neither-version)
	    (error "This difference region has been edited")))))))

;; Read a file name, handling all of the various defaulting rules.

(defun emerge-read-file-name (prompt alternative-default-dir default-file
			      A-file must-match)
  ;; `prompt' should not have trailing ": ", so that it can be modified
  ;; according to context.
  ;; If alternative-default-dir is non-nil, it should be used as the default
  ;; directory instead if default-directory, if emerge-default-last-directories
  ;; is set.
  ;; If default-file is set, it should be used as the default value.
  ;; If A-file is set, and its directory is different from
  ;; alternative-default-dir, and if emerge-default-last-directories is set,
  ;; the default file should be the last part of A-file in the default
  ;; directory.  (Overriding default-file.)
  (cond
   ;; If this is not the A-file argument (shown by non-nil A-file), and
   ;; if emerge-default-last-directories is set, and
   ;; the default directory exists but is not the same as the directory of the
   ;; A-file,
   ;; then make the default file have the same name as the A-file, but in
   ;; the default directory.
   ((and emerge-default-last-directories
	 A-file
	 alternative-default-dir
	 (not (string-equal alternative-default-dir
			    (file-name-directory A-file))))
    (read-file-name (format "%s (default %s): "
			    prompt (file-name-nondirectory A-file))
		    alternative-default-dir
		    (concat alternative-default-dir
			    (file-name-nondirectory A-file))
		    (and must-match 'confirm)))
   ;; If there is a default file, use it.
   (default-file
     (read-file-name (format "%s (default %s): " prompt default-file)
		     ;; If emerge-default-last-directories is set, use the
		     ;; directory from the same argument of the last call of
		     ;; Emerge as the default for this argument.
		     (and emerge-default-last-directories
			  alternative-default-dir)
		     default-file (and must-match 'confirm)))
   (t
    (read-file-name (concat prompt ": ")
		    ;; If emerge-default-last-directories is set, use the
		    ;; directory from the same argument of the last call of
		    ;; Emerge as the default for this argument.
		    (and emerge-default-last-directories
			 alternative-default-dir)
		    nil (and must-match 'confirm)))))

;; Revise the mode line to display which difference we have selected

(defun emerge-refresh-mode-line ()
  (setq mode-line-buffer-identification
	(list (format "Emerge: %%b   diff %d of %d%s"
		      (1+ emerge-current-difference)
		      emerge-number-of-differences
		      (if (and (>= emerge-current-difference 0)
			       (< emerge-current-difference
				  emerge-number-of-differences))
			  (cdr (assq (aref (aref emerge-difference-list
						 emerge-current-difference)
					   6)
				     '((A . " - A")
				       (B . " - B")
				       (prefer-A . " - A*")
				       (prefer-B . " - B*")
				       (combined . " - comb"))))
			""))))
  (force-mode-line-update))

;; compare two regions in two buffers for containing the same text
(defun emerge-compare-buffers (buffer-x x-begin x-end buffer-y y-begin y-end)
  ;; first check that the two regions are the same length
  (if (not (and (= (- x-end x-begin) (- y-end y-begin))))
      nil
    (catch 'exit
      (while (< x-begin x-end)
	;; bite off and compare no more than 1000 characters at a time
	(let* ((compare-length (min (- x-end x-begin) 1000))
	       (x-string (with-current-buffer
			  buffer-x
			  (buffer-substring x-begin
					    (+ x-begin compare-length))))
	       (y-string (with-current-buffer
			  buffer-y
			  (buffer-substring y-begin
					    (+ y-begin compare-length)))))
	  (if (not (string-equal x-string y-string))
	      (throw 'exit nil)
	    (setq x-begin (+ x-begin compare-length))
	    (setq y-begin (+ y-begin compare-length)))))
      t)))

;; Construct a unique buffer name.
;; The first one tried is prefixsuffix, then prefix<2>suffix,
;; prefix<3>suffix, etc.
(defun emerge-unique-buffer-name (prefix suffix)
  (if (null (get-buffer (concat prefix suffix)))
      (concat prefix suffix)
    (let ((n 2))
      (while (get-buffer (format "%s<%d>%s" prefix n suffix))
	(setq n (1+ n)))
      (format "%s<%d>%s" prefix n suffix))))

;; Verify that we have a difference selected.
(defun emerge-validate-difference ()
  (if (not (and (>= emerge-current-difference 0)
		(< emerge-current-difference emerge-number-of-differences)))
      (error "No difference selected")))

;;; Functions for saving and restoring a batch of variables

;; These functions save (get the values of) and restore (set the values of)
;; a list of variables.  The argument is a list of symbols (the names of
;; the variables).  A list element can also be a list of two functions,
;; the first of which (when called with no arguments) gets the value, and
;; the second (when called with a value as an argument) sets the value.
;; A "function" is anything that funcall can handle as an argument.

(defun emerge-save-variables (vars)
  (mapcar (lambda (v) (if (symbolp v)
			  (symbol-value v)
			(funcall (car v))))
	  vars))

(defun emerge-restore-variables (vars values)
  (while vars
    (let ((var (car vars))
	  (value (car values)))
      (if (symbolp var)
	  (set var value)
	(funcall (car (cdr var)) value)))
    (setq vars (cdr vars))
    (setq values (cdr values))))

;; Make a temporary file that only we have access to.
;; PREFIX is appended to emerge-temp-file-prefix to make the filename prefix.
(defun emerge-make-temp-file (prefix)
  (let (f (old-modes (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes emerge-temp-file-mode)
	  (setq f (make-temp-file (concat emerge-temp-file-prefix prefix))))
      (set-default-file-modes old-modes))
    f))

;;; Functions that query the user before he can write out the current buffer.

(defun emerge-query-write-file ()
  "Ask the user whether to write out an incomplete merge.
If answer is yes, call `write-file' to do so.  See `emerge-query-and-call'
for details of the querying process."
  (interactive)
  (emerge-query-and-call 'write-file))

(defun emerge-query-save-buffer ()
  "Ask the user whether to save an incomplete merge.
If answer is yes, call `save-buffer' to do so.  See `emerge-query-and-call'
for details of the querying process."
  (interactive)
  (emerge-query-and-call 'save-buffer))

(defun emerge-query-and-call (command)
  "Ask the user whether to save or write out the incomplete merge.
If answer is yes, call COMMAND interactively.  During the call, the flags
around the current difference are removed."
  (if (yes-or-no-p "Do you really write to write out this unfinished merge? ")
      ;; He really wants to do it -- unselect the difference for the duration
      (progn
	(if (and (>= emerge-current-difference 0)
		 (< emerge-current-difference emerge-number-of-differences))
	    (emerge-unselect-difference emerge-current-difference))
	;; call-interactively takes the value of current-prefix-arg as the
	;; prefix argument value to be passed to the command.  Thus, we have
	;; to do nothing special to make sure the prefix argument is
	;; transmitted to the command.
	(call-interactively command)
	(if (and (>= emerge-current-difference 0)
		 (< emerge-current-difference emerge-number-of-differences))
	    (progn
	      (emerge-select-difference emerge-current-difference)
	      (emerge-recenter))))
    ;; He's being smart and not doing it
    (message "Not written")))

;; Make sure the current buffer (for a file) has the same contents as the
;; file on disk, and attempt to remedy the situation if not.
;; Signal an error if we can't make them the same, or the user doesn't want
;; to do what is necessary to make them the same.
(defun emerge-verify-file-buffer ()
  ;; First check if the file has been modified since the buffer visited it.
  (if (verify-visited-file-modtime (current-buffer))
      (if (buffer-modified-p)
	  ;; If buffer is not obsolete and is modified, offer to save
	  (if (yes-or-no-p (format "Save file %s? " buffer-file-name))
	      (save-buffer)
	    (error "Buffer out of sync for file %s" buffer-file-name))
	;; If buffer is not obsolete and is not modified, do nothing
	nil)
    (if (buffer-modified-p)
	;; If buffer is obsolete and is modified, give error
	(error "Buffer out of sync for file %s" buffer-file-name)
      ;; If buffer is obsolete and is not modified, offer to revert
      (if (yes-or-no-p (format "Revert file %s? " buffer-file-name))
	      (revert-buffer t t)
	(error "Buffer out of sync for file %s" buffer-file-name)))))

;; Utilities that might have value outside of Emerge.

;; Set up the mode in the current buffer to duplicate the mode in another
;; buffer.
(defun emerge-copy-modes (buffer)
  ;; Set the major mode
  (funcall (with-current-buffer buffer major-mode)))

;; Define a key, even if a prefix of it is defined
(defun emerge-force-define-key (keymap key definition)
  "Like `define-key', but forcibly creates prefix characters as needed.
If some prefix of KEY has a non-prefix definition, it is redefined."
  ;; Find out if a prefix of key is defined
  (let ((v (lookup-key keymap key)))
    ;; If so, undefine it
    (if (integerp v)
	(define-key keymap (substring key 0 v) nil)))
  ;; Now define the key
  (define-key keymap key definition))

;;;;; Improvements to describe-mode, so that it describes minor modes as well
;;;;; as the major mode
;;(defun describe-mode (&optional minor)
;;  "Display documentation of current major mode.
;;If optional arg MINOR is non-nil (or prefix argument is given if interactive),
;;display documentation of active minor modes as well.
;;For this to work correctly for a minor mode, the mode's indicator variable
;;\(listed in `minor-mode-alist') must also be a function whose documentation
;;describes the minor mode."
;;  (interactive)
;;  (with-output-to-temp-buffer "*Help*"
;;    (princ mode-name)
;;    (princ " Mode:\n")
;;    (princ (documentation major-mode))
;;    (let ((minor-modes minor-mode-alist)
;;	  (locals (buffer-local-variables)))
;;      (while minor-modes
;;	(let* ((minor-mode (car (car minor-modes)))
;;	       (indicator (car (cdr (car minor-modes))))
;;	       (local-binding (assq minor-mode locals)))
;;	  ;; Document a minor mode if it is listed in minor-mode-alist,
;;	  ;; bound locally in this buffer, non-nil, and has a function
;;	  ;; definition.
;;	  (if (and local-binding
;;		   (cdr local-binding)
;;		   (fboundp minor-mode))
;;	      (progn
;;		(princ (format "\n\n\n%s minor mode (indicator%s):\n"
;;			       minor-mode indicator))
;;		(princ (documentation minor-mode)))))
;;	(setq minor-modes (cdr minor-modes))))
;;    (with-current-buffer standard-output
;;      (help-mode))
;;    (help-print-return-message)))

;; This goes with the redefinition of describe-mode.
;;;; Adjust things so that keyboard macro definitions are documented correctly.
;;(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

;; substitute-key-definition should work now.
;;;; Function to shadow a definition in a keymap with definitions in another.
;;(defun emerge-shadow-key-definition (olddef newdef keymap shadowmap)
;;  "Shadow OLDDEF with NEWDEF for any keys in KEYMAP with entries in SHADOWMAP.
;;In other words, SHADOWMAP will now shadow all definitions of OLDDEF in KEYMAP
;;with NEWDEF.  Does not affect keys that are already defined in SHADOWMAP,
;;including those whose definition is OLDDEF."
;;  ;; loop through all keymaps accessible from keymap
;;  (let ((maps (accessible-keymaps keymap)))
;;    (while maps
;;      (let ((prefix (car (car maps)))
;;	    (map (cdr (car maps))))
;;	;; examine a keymap
;;	(if (arrayp map)
;;	    ;; array keymap
;;	    (let ((len (length map))
;;		  (i 0))
;;	      (while (< i len)
;;		(if (eq (aref map i) olddef)
;;		    ;; set the shadowing definition
;;		    (let ((key (concat prefix (char-to-string i))))
;;		      (emerge-define-key-if-possible shadowmap key newdef)))
;;		(setq i (1+ i))))
;;	  ;; sparse keymap
;;	  (while map
;;	    (if (eq (cdr-safe (car-safe map)) olddef)
;;		;; set the shadowing definition
;;		(let ((key
;;		       (concat prefix (char-to-string (car (car map))))))
;;		      (emerge-define-key-if-possible shadowmap key newdef)))
;;	    (setq map (cdr map)))))
;;      (setq maps (cdr maps)))))

;; Define a key if it (or a prefix) is not already defined in the map.
(defun emerge-define-key-if-possible (keymap key definition)
  ;; look up the present definition of the key
  (let ((present (lookup-key keymap key)))
    (if (integerp present)
	;; if it is "too long", look up the valid prefix
	(if (not (lookup-key keymap (substring key 0 present)))
	    ;; if the prefix isn't defined, define it
	    (define-key keymap key definition))
      ;; if there is no present definition, define it
      (if (not present)
	  (define-key keymap key definition)))))

;; Ordinary substitute-key-definition should do this now.
;;(defun emerge-recursively-substitute-key-definition (olddef newdef keymap)
;;  "Like `substitute-key-definition', but act recursively on subkeymaps.
;;Make sure that subordinate keymaps aren't shared with other keymaps!
;;\(`copy-keymap' will suffice.)"
;;  ;; Loop through all keymaps accessible from keymap
;;  (let ((maps (accessible-keymaps keymap)))
;;    (while maps
;;      ;; Substitute in this keymap
;;      (substitute-key-definition olddef newdef (cdr (car maps)))
;;      (setq maps (cdr maps)))))

;; Show the name of the file in the buffer.
(defun emerge-show-file-name ()
  "Displays the name of the file loaded into the current buffer.
If the name won't fit on one line, the minibuffer is expanded to hold it,
and the command waits for a keystroke from the user.  If the keystroke is
SPC, it is ignored; if it is anything else, it is processed as a command."
  (interactive)
  (let ((name (buffer-file-name)))
    (or name
	(setq name "Buffer has no file name."))
    (save-window-excursion
      (select-window (minibuffer-window))
      (unwind-protect
	  (progn
	    (erase-buffer)
	    (insert name)
	    (while (and (not (pos-visible-in-window-p))
			(not (window-full-height-p)))
	      (enlarge-window 1))
	    (let* ((echo-keystrokes 0)
		   (c (read-event)))
	      (if (not (eq c 32))
		  (setq unread-command-events (list c)))))
	(erase-buffer)))))

;; Improved auto-save file names.
;; This function fixes many problems with the standard auto-save file names:
;; Auto-save files for non-file buffers get put in the default directory
;; for the buffer, whether that makes sense or not.
;; Auto-save files for file buffers get put in the directory of the file,
;; regardless of whether we can write into it or not.
;; Auto-save files for non-file buffers don't use the process id, so if a
;; user runs more than on Emacs, they can make auto-save files that overwrite
;; each other.
;; To use this function, do:
;;	(fset 'make-auto-save-file-name
;;	      (symbol-function 'emerge-make-auto-save-file-name))
(defun emerge-make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name';
that is checked before calling this function.
You can redefine this for customization.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      ;; if buffer has a file, try the format <file directory>/#<file name>#
      (let ((f (concat (file-name-directory buffer-file-name)
		       "#"
		       (file-name-nondirectory buffer-file-name)
		       "#")))
	(if (file-writable-p f)
	    ;; the file is writable, so use it
	    f
	  ;; the file isn't writable, so use the format
	  ;; ~/#&<file name>&<hash of directory>#
	  (concat (getenv "HOME")
		  "/#&"
		  (file-name-nondirectory buffer-file-name)
		  "&"
		  (emerge-hash-string-into-string
		   (file-name-directory buffer-file-name))
		  "#")))
    ;; if buffer has no file, use the format ~/#%<buffer name>%<process id>#
    (expand-file-name (concat (getenv "HOME")
			      "/#%"
			      ;; quote / into \! and \ into \\
			      (emerge-unslashify-name (buffer-name))
			      "%"
			      (make-temp-name "")
			      "#"))))

;; Hash a string into five characters more-or-less suitable for use in a file
;; name.  (Allowed characters are ! through ~, except /.)
(defun emerge-hash-string-into-string (s)
  (let ((bins (vector 0 0 0 0 0))
	(i 0))
    (while (< i (length s))
      (aset bins (% i 5) (% (+ (* (aref bins (% i 5)) 35)
			       (aref s i))
			    65536))
      (setq i (1+ i)))
    (mapconcat (lambda (b)
		 (setq b (+ (% b 93) ?!))
		 (if (>= b ?/)
		     (setq b (1+ b)))
		 (char-to-string b))
	       bins "")))

;; Quote any /s in a string by replacing them with \!.
;; Also, replace any \s by \\, to make it one-to-one.
(defun emerge-unslashify-name (s)
  (let ((limit 0))
    (while (string-match "[/\\]" s limit)
      (setq s (concat (substring s 0 (match-beginning 0))
		      (if (string= (substring s (match-beginning 0)
					      (match-end 0))
				   "/")
			  "\\!"
			"\\\\")
		      (substring s (match-end 0))))
      (setq limit (1+ (match-end 0)))))
  s)

;; Metacharacters that have to be protected from the shell when executing
;; a diff/diff3 command.
(defcustom emerge-metachars
  (if (memq system-type '(ms-dos windows-nt))
      "[ \t\"<>|?*^&=]"
    "[ \t\n!\"#$&'()*;<=>?[\\^`{|~]")
  "Characters that must be quoted when used in a shell command line.
More precisely, a [...] regexp to match any one such character."
  :type 'regexp
  :group 'emerge)

;; Quote metacharacters (using \) when executing a diff/diff3 command.
(defun emerge-protect-metachars (s)
  (if (memq system-type '(ms-dos windows-nt))
      (shell-quote-argument s)
    (let ((limit 0))
      (while (string-match emerge-metachars s limit)
	(setq s (concat (substring s 0 (match-beginning 0))
			"\\"
			(substring s (match-beginning 0))))
	(setq limit (1+ (match-end 0)))))
    s))

(provide 'emerge)

;;; emerge.el ends here
