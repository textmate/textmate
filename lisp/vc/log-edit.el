;;; log-edit.el --- Major mode for editing CVS commit messages -*- lexical-binding: t -*-

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs cvs commit log vc

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

;; Todo:

;; - Move in VC's code
;; - Add compatibility for VC's hook variables

;;; Code:

(eval-when-compile (require 'cl))
(require 'add-log)			; for all the ChangeLog goodies
(require 'pcvs-util)
(require 'ring)

;;;;
;;;; Global Variables
;;;;

(defgroup log-edit nil
  "Major mode for editing RCS and CVS commit messages."
  :group 'pcl-cvs
  :group 'vc				; It's used by VC.
  :version "21.1"
  :prefix "log-edit-")

;; compiler pacifiers
(defvar cvs-buffer)


;; The main keymap

(easy-mmode-defmap log-edit-mode-map
  `(("\C-c\C-c" . log-edit-done)
    ("\C-c\C-a" . log-edit-insert-changelog)
    ("\C-c\C-d" . log-edit-show-diff)
    ("\C-c\C-f" . log-edit-show-files)
    ("\M-n"	. log-edit-next-comment)
    ("\M-p"	. log-edit-previous-comment)
    ("\M-r"	. log-edit-comment-search-backward)
    ("\M-s"	. log-edit-comment-search-forward)
    ("\C-c?"	. log-edit-mode-help))
  "Keymap for the `log-edit-mode' (to edit version control log messages)."
  :group 'log-edit)

;; Compatibility with old names.  Should we bother ?
(defvar vc-log-mode-map log-edit-mode-map)
(defvar vc-log-entry-mode vc-log-mode-map)

(easy-menu-define log-edit-menu log-edit-mode-map
  "Menu used for `log-edit-mode'."
  '("Log-Edit"
    ["Done" log-edit-done
     :help "Exit log-edit and proceed with the actual action."]
    "--"
    ["Insert ChangeLog" log-edit-insert-changelog
     :help "Insert a log message by looking at the ChangeLog"]
    ["Add to ChangeLog" log-edit-add-to-changelog
     :help "Insert this log message into the appropriate ChangeLog file"]
    "--"
    ["Show diff" log-edit-show-diff
     :help "Show the diff for the files to be committed."]
    ["List files" log-edit-show-files
     :help "Show the list of relevant files."]
    "--"
    ["Previous comment"		log-edit-previous-comment
     :help "Cycle backwards through comment history"]
    ["Next comment"		log-edit-next-comment
     :help "Cycle forwards through comment history."]
    ["Search comment forward"	log-edit-comment-search-forward
     :help "Search forwards through comment history for a substring match of str"]
    ["Search comment backward"	log-edit-comment-search-backward
     :help "Search backwards through comment history for substring match of str"]))

(defcustom log-edit-confirm 'changed
  "If non-nil, `log-edit-done' will request confirmation.
If 'changed, only request confirmation if the list of files has
  changed since the beginning of the log-edit session."
  :group 'log-edit
  :type '(choice (const changed) (const t) (const nil)))

(defcustom log-edit-keep-buffer nil
  "If non-nil, don't hide the buffer after `log-edit-done'."
  :group 'log-edit
  :type 'boolean)

(defvar cvs-commit-buffer-require-final-newline t)
(make-obsolete-variable 'cvs-commit-buffer-require-final-newline
                        'log-edit-require-final-newline
			"21.1")

(defcustom log-edit-require-final-newline
  cvs-commit-buffer-require-final-newline
  "Enforce a newline at the end of commit log messages.
Enforce it silently if t, query if non-nil and don't do anything if nil."
  :group 'log-edit
  :type '(choice (const ask) (const t) (const nil)))

(defcustom log-edit-setup-invert nil
  "Non-nil means `log-edit' should invert the meaning of its SETUP arg.
If SETUP is 'force, this variable has no effect."
  :group 'log-edit
  :type 'boolean)

(defcustom log-edit-hook '(log-edit-insert-cvs-template
                           log-edit-show-files
			   log-edit-insert-changelog)
  "Hook run at the end of `log-edit'."
  :group 'log-edit
  :type '(hook :options (log-edit-insert-changelog
                         log-edit-insert-cvs-rcstemplate
                         log-edit-insert-cvs-template
			 log-edit-insert-filenames)))

(defcustom log-edit-mode-hook (if (boundp 'vc-log-mode-hook) vc-log-mode-hook)
  "Hook run when entering `log-edit-mode'."
  :group 'log-edit
  :type 'hook)

(defcustom log-edit-done-hook nil
  "Hook run before doing the actual commit.
This hook can be used to cleanup the message, enforce various
conventions, or to allow recording the message in some other database,
such as a bug-tracking system.  The list of files about to be committed
can be obtained from `log-edit-files'."
  :group 'log-edit
  :type '(hook :options (log-edit-set-common-indentation
			 log-edit-add-to-changelog)))

(defcustom log-edit-strip-single-file-name nil
  "If non-nil, remove file name from single-file log entries."
  :type 'boolean
  :safe 'booleanp
  :group 'log-edit
  :version "24.1")

(defvar cvs-changelog-full-paragraphs t)
(make-obsolete-variable 'cvs-changelog-full-paragraphs
                        'log-edit-changelog-full-paragraphs
			"21.1")

(defvar log-edit-changelog-full-paragraphs cvs-changelog-full-paragraphs
  "*If non-nil, include full ChangeLog paragraphs in the log.
This may be set in the ``local variables'' section of a ChangeLog, to
indicate the policy for that ChangeLog.

A ChangeLog paragraph is a bunch of log text containing no blank lines;
a paragraph usually describes a set of changes with a single purpose,
but perhaps spanning several functions in several files.  Changes in
different paragraphs are unrelated.

You could argue that the log entry for a file should contain the
full ChangeLog paragraph mentioning the change to the file, even though
it may mention other files, because that gives you the full context you
need to understand the change.  This is the behavior you get when this
variable is set to t.

On the other hand, you could argue that the log entry for a change
should contain only the text for the changes which occurred in that
file, because the log is per-file.  This is the behavior you get
when this variable is set to nil.")

;;;; Internal global or buffer-local vars

(defconst log-edit-files-buf "*log-edit-files*")
(defvar log-edit-initial-files nil)
(defvar log-edit-callback nil)
(defvar log-edit-diff-function nil)
(defvar log-edit-listfun nil)

(defvar log-edit-parent-buffer nil)

;;; Originally taken from VC-Log mode

(defconst log-edit-maximum-comment-ring-size 32
  "Maximum number of saved comments in the comment ring.")
(defvar log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
(defvar log-edit-comment-ring-index nil)
(defvar log-edit-last-comment-match "")

(defun log-edit-new-comment-index (stride len)
  "Return the comment index STRIDE elements from the current one.
LEN is the length of `log-edit-comment-ring'."
  (mod (cond
	(log-edit-comment-ring-index (+ log-edit-comment-ring-index stride))
	;; Initialize the index on the first use of this command
	;; so that the first M-p gets index 0, and the first M-n gets
	;; index -1.
	((> stride 0) (1- stride))
	(t stride))
       len))

(defun log-edit-previous-comment (arg)
  "Cycle backwards through comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (let ((len (ring-length log-edit-comment-ring)))
    (if (<= len 0)
	(progn (message "Empty comment ring") (ding))
      ;; Don't use `erase-buffer' because we don't want to `widen'.
      (delete-region (point-min) (point-max))
      (setq log-edit-comment-ring-index (log-edit-new-comment-index arg len))
      (message "Comment %d" (1+ log-edit-comment-ring-index))
      (insert (ring-ref log-edit-comment-ring log-edit-comment-ring-index)))))

(defun log-edit-next-comment (arg)
  "Cycle forwards through comment history.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (log-edit-previous-comment (- arg)))

(defun log-edit-comment-search-backward (str &optional stride)
  "Search backwards through comment history for substring match of STR.
If the optional argument STRIDE is present, that is a step-width to use
when going through the comment ring."
  ;; Why substring rather than regexp ?   -sm
  (interactive
   (list (read-string "Comment substring: " nil nil log-edit-last-comment-match)))
  (unless stride (setq stride 1))
  (if (string= str "")
      (setq str log-edit-last-comment-match)
    (setq log-edit-last-comment-match str))
  (let* ((str (regexp-quote str))
	 (len (ring-length log-edit-comment-ring))
	 (n (log-edit-new-comment-index stride len)))
    (while (progn (when (or (>= n len) (< n 0)) (error "Not found"))
		  (not (string-match str (ring-ref log-edit-comment-ring n))))
      (setq n (+ n stride)))
    (setq log-edit-comment-ring-index n)
    (log-edit-previous-comment 0)))

(defun log-edit-comment-search-forward (str)
  "Search forwards through comment history for a substring match of STR."
  (interactive
   (list (read-string "Comment substring: " nil nil log-edit-last-comment-match)))
  (log-edit-comment-search-backward str -1))

(defun log-edit-comment-to-change-log (&optional whoami file-name)
  "Enter last VC comment into the change log for the current file.
WHOAMI (interactive prefix) non-nil means prompt for user name
and site.  FILE-NAME is the name of the change log; if nil, use
`change-log-default-name'.

This may be useful as a `log-edit-checkin-hook' to update change logs
automatically."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  (let (;; Extract the comment first so we get any error before doing anything.
	(comment (ring-ref log-edit-comment-ring 0))
	;; Don't let add-change-log-entry insert a defun name.
	(add-log-current-defun-function 'ignore)
	end)
    ;; Call add-log to do half the work.
    (add-change-log-entry whoami file-name t t)
    ;; Insert the VC comment, leaving point before it.
    (setq end (save-excursion (insert comment) (point-marker)))
    (if (looking-at "\\s *\\s(")
	;; It starts with an open-paren, as in "(foo): Frobbed."
	;; So remove the ": " add-log inserted.
	(delete-char -2))
    ;; Canonicalize the white space between the file name and comment.
    (just-one-space)
    ;; Indent rest of the text the same way add-log indented the first line.
    (let ((indentation (current-indentation)))
      (save-excursion
	(while (< (point) end)
	  (forward-line 1)
	  (indent-to indentation))
	(setq end (point))))
    ;; Fill the inserted text, preserving open-parens at bol.
    (let ((paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
      (beginning-of-line)
      (fill-region (point) end))
    ;; Canonicalize the white space at the end of the entry so it is
    ;; separated from the next entry by a single blank line.
    (skip-syntax-forward " " end)
    (delete-char (- (skip-syntax-backward " ")))
    (or (eobp) (looking-at "\n\n")
	(insert "\n"))))

;; Compatibility with old names.
(define-obsolete-variable-alias 'vc-comment-ring 'log-edit-comment-ring "22.1")
(define-obsolete-variable-alias 'vc-comment-ring-index 'log-edit-comment-ring-index "22.1")
(define-obsolete-function-alias 'vc-previous-comment 'log-edit-previous-comment "22.1")
(define-obsolete-function-alias 'vc-next-comment 'log-edit-next-comment "22.1")
(define-obsolete-function-alias 'vc-comment-search-reverse 'log-edit-comment-search-backward "22.1")
(define-obsolete-function-alias 'vc-comment-search-forward 'log-edit-comment-search-forward "22.1")
(define-obsolete-function-alias 'vc-comment-to-change-log 'log-edit-comment-to-change-log "22.1")

;;;
;;; Actual code
;;;

(defface log-edit-summary '((t :inherit font-lock-function-name-face))
  "Face for the summary in `log-edit-mode' buffers.")

(defface log-edit-header '((t :inherit font-lock-keyword-face))
  "Face for the headers in `log-edit-mode' buffers.")

(defface log-edit-unknown-header '((t :inherit font-lock-comment-face))
  "Face for unknown headers in `log-edit-mode' buffers.")

(defvar log-edit-headers-alist '(("Summary" . log-edit-summary)
                                 ("Fixes") ("Author"))
  "AList of known headers and the face to use to highlight them.")

(defconst log-edit-header-contents-regexp
  "[ \t]*\\(.*\\(\n[ \t].*\\)*\\)\n?")

(defun log-edit-match-to-eoh (_limit)
  ;; FIXME: copied from message-match-to-eoh.
  (let ((start (point)))
    (rfc822-goto-eoh)
    ;; Typical situation: some temporary change causes the header to be
    ;; incorrect, so EOH comes earlier than intended: the last lines of the
    ;; intended headers are now not considered part of the header any more,
    ;; so they don't have the multiline property set.  When the change is
    ;; completed and the header has its correct shape again, the lack of the
    ;; multiline property means we won't rehighlight the last lines of
    ;; the header.
    (if (< (point) start)
        nil                             ;No header within start..limit.
      ;; Here we disregard LIMIT so that we may extend the area again.
      (set-match-data (list start (point)))
      (point))))

(defvar log-edit-font-lock-keywords
  ;; Copied/inspired by message-font-lock-keywords.
  `((log-edit-match-to-eoh
     (,(concat "^\\(\\([a-z]+\\):\\)" log-edit-header-contents-regexp)
      (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (1 (if (assoc (match-string 2) log-edit-headers-alist)
             'log-edit-header
           'log-edit-unknown-header)
         nil lax)
      ;; From `log-edit-header-contents-regexp':
      (3 (or (cdr (assoc (match-string 2) log-edit-headers-alist))
             'log-edit-header)
         nil lax)))))

;;;###autoload
(defun log-edit (callback &optional setup params buffer mode &rest _ignore)
  "Setup a buffer to enter a log message.
\\<log-edit-mode-map>The buffer will be put in mode MODE or `log-edit-mode'
if MODE is nil.
If SETUP is non-nil, the buffer is then erased and `log-edit-hook' is run.
Mark and point will be set around the entire contents of the buffer so
that it is easy to kill the contents of the buffer with \\[kill-region].
Once you're done editing the message, pressing \\[log-edit-done] will call
`log-edit-done' which will end up calling CALLBACK to do the actual commit.

PARAMS if non-nil is an alist.  Possible keys and associated values:
 `log-edit-listfun' -- function taking no arguments that returns the list of
 files that are concerned by the current operation (using relative names);
 `log-edit-diff-function' -- function taking no arguments that
 displays a diff of the files concerned by the current operation.

If BUFFER is non-nil `log-edit' will jump to that buffer, use it to edit the
log message and go back to the current buffer when done.  Otherwise, it
uses the current buffer."
  (let ((parent (current-buffer)))
    (if buffer (pop-to-buffer buffer))
    (when (and log-edit-setup-invert (not (eq setup 'force)))
      (setq setup (not setup)))
    (when setup
      (erase-buffer)
      (insert "Summary: \nAuthor: ")
      (save-excursion (insert "\n\n")))
    (if mode
	(funcall mode)
      (log-edit-mode))
    (set (make-local-variable 'log-edit-callback) callback)
    (if (listp params)
	(dolist (crt params)
	  (set (make-local-variable (car crt)) (cdr crt)))
      ;; For backward compatibility with log-edit up to version 22.2
      ;; accept non-list PARAMS to mean `log-edit-list'.
      (set (make-local-variable 'log-edit-listfun) params))

    (if buffer (set (make-local-variable 'log-edit-parent-buffer) parent))
    (set (make-local-variable 'log-edit-initial-files) (log-edit-files))
    (when setup (run-hooks 'log-edit-hook))
    (goto-char (point-min)) (push-mark (point-max))
    (message "%s" (substitute-command-keys
	      "Press \\[log-edit-done] when you are done editing."))))

(define-derived-mode log-edit-mode text-mode "Log-Edit"
  "Major mode for editing version-control log messages.
When done editing the log entry, just type \\[log-edit-done] which
will trigger the actual commit of the file(s).
Several other handy support commands are provided of course and
the package from which this is used might also provide additional
commands (under C-x v for VC, for example).

\\{log-edit-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(log-edit-font-lock-keywords t t))
  (make-local-variable 'log-edit-comment-ring-index)
  (hack-dir-local-variables-non-file-buffer))

(defun log-edit-hide-buf (&optional buf where)
  (when (setq buf (get-buffer (or buf log-edit-files-buf)))
    (let ((win (get-buffer-window buf where)))
      (if win (ignore-errors (delete-window win))))
    (bury-buffer buf)))

(defun log-edit-done ()
  "Finish editing the log message and commit the files.
If you want to abort the commit, simply delete the buffer."
  (interactive)
  ;; Clean up empty headers.
  (goto-char (point-min))
  (while (looking-at (concat "^[a-z]*:" log-edit-header-contents-regexp))
    (let ((beg (match-beginning 0)))
      (goto-char (match-end 0))
      (if (string-match "\\`[ \n\t]*\\'" (match-string 1))
          (delete-region beg (point)))))
  ;; Get rid of leading empty lines.
  (goto-char (point-min))
  (when (looking-at "\\([ \t]*\n\\)+")
    (delete-region (match-beginning 0) (match-end 0)))
  ;; Get rid of trailing empty lines
  (goto-char (point-max))
  (skip-syntax-backward " ")
  (when (equal (char-after) ?\n) (forward-char 1))
  (delete-region (point) (point-max))
  ;; Check for final newline
  (if (and (> (point-max) (point-min))
	   (/= (char-before (point-max)) ?\n)
	   (or (eq log-edit-require-final-newline t)
	       (and log-edit-require-final-newline
		    (y-or-n-p
		     (format "Buffer %s does not end in newline.  Add one? "
			     (buffer-name))))))
      (save-excursion
	(goto-char (point-max))
	(insert ?\n)))
  (let ((comment (buffer-string)))
    (when (or (ring-empty-p log-edit-comment-ring)
	      (not (equal comment (ring-ref log-edit-comment-ring 0))))
      (ring-insert log-edit-comment-ring comment)))
  (let ((win (get-buffer-window log-edit-files-buf)))
    (if (and log-edit-confirm
	     (not (and (eq log-edit-confirm 'changed)
		       (equal (log-edit-files) log-edit-initial-files)))
	     (progn
	       (log-edit-show-files)
	       (not (y-or-n-p "Really commit? "))))
	(progn (when (not win) (log-edit-hide-buf))
	       (message "Oh, well!  Later maybe?"))
      (run-hooks 'log-edit-done-hook)
      (log-edit-hide-buf)
      (unless (or log-edit-keep-buffer (not log-edit-parent-buffer))
	(cvs-bury-buffer (current-buffer) log-edit-parent-buffer))
      (call-interactively log-edit-callback))))

(defun log-edit-files ()
  "Return the list of files that are about to be committed."
  (ignore-errors (funcall log-edit-listfun)))

(defun log-edit-mode-help ()
  "Provide help for the `log-edit-mode-map'."
  (interactive)
  (if (eq last-command 'log-edit-mode-help)
      (describe-function major-mode)
    (message "%s"
     (substitute-command-keys
      "Type `\\[log-edit-done]' to finish commit.  Try `\\[describe-function] log-edit-done' for more help."))))

(defcustom log-edit-common-indent 0
  "Minimum indentation to use in `log-edit-set-common-indentation'."
  :group 'log-edit
  :type 'integer)

(defun log-edit-set-common-indentation ()
  "(Un)Indent the current buffer rigidly to `log-edit-common-indent'."
  (save-excursion
    (let ((common (point-max)))
      (rfc822-goto-eoh)
      (while (< (point) (point-max))
        (if (not (looking-at "^[ \t]*$"))
            (setq common (min common (current-indentation))))
        (forward-line 1))
      (rfc822-goto-eoh)
      (indent-rigidly (point) (point-max)
		      (- log-edit-common-indent common)))))

(defun log-edit-show-diff ()
  "Show the diff for the files to be committed."
  (interactive)
  (if (functionp log-edit-diff-function)
      (funcall log-edit-diff-function)
    (error "Diff functionality has not been setup")))

(defun log-edit-show-files ()
  "Show the list of files to be committed."
  (interactive)
  (let* ((files (log-edit-files))
	 (buf (get-buffer-create log-edit-files-buf)))
    (with-current-buffer buf
      (log-edit-hide-buf buf 'all)
      (setq buffer-read-only nil)
      (erase-buffer)
      (cvs-insert-strings files)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (save-selected-window
	(cvs-pop-to-buffer-same-frame buf)
	(shrink-window-if-larger-than-buffer)
	(selected-window)))))

(defun log-edit-empty-buffer-p ()
  "Return non-nil if the buffer is \"empty\"."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: \\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

(defun log-edit-insert-cvs-template ()
  "Insert the template specified by the CVS administrator, if any.
This simply uses the local CVS/Template file."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    ;; Should the template take precedence over an empty Summary:,
    ;; ie should we first erase the buffer?
    (when (file-readable-p "CVS/Template")
      (goto-char (point-max))
      (insert-file-contents "CVS/Template"))))

(defun log-edit-insert-cvs-rcstemplate ()
  "Insert the rcstemplate from the CVS repository.
This contacts the repository to get the rcstemplate file and
can thus take some time."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    (when (file-readable-p "CVS/Root")
      (goto-char (point-max))
      ;; Ignore the stderr stuff, even if it's an error.
      (call-process "cvs" nil '(t nil) nil
                    "checkout" "-p" "CVSROOT/rcstemplate"))))

(defun log-edit-insert-filenames ()
  "Insert the list of files that are to be committed."
  (interactive)
  (insert "Affected files:  \n"
          (mapconcat 'identity (log-edit-files) "  \n")))

(defun log-edit-add-to-changelog ()
  "Insert this log message into the appropriate ChangeLog file."
  (interactive)
  ;; Yuck!
  (unless (string= (buffer-string) (ring-ref log-edit-comment-ring 0))
    (ring-insert log-edit-comment-ring (buffer-string)))
  (dolist (f (log-edit-files))
    (let ((buffer-file-name (expand-file-name f)))
      (save-excursion
	(log-edit-comment-to-change-log)))))

(defvar log-edit-changelog-use-first nil)

(defvar log-edit-rewrite-fixes nil
  "Rule to rewrite bug numbers into Fixes: headers.
The value should be of the form (REGEXP . REPLACEMENT)
where REGEXP should match the expression referring to a bug number
in the text, and REPLACEMENT is an expression to pass to `replace-match'
to build the Fixes: header.")
(put 'log-edit-rewrite-fixes 'safe-local-variable
     (lambda (v) (and (stringp (car-safe v)) (stringp (cdr v)))))

(defun log-edit-add-field (field value)
  (rfc822-goto-eoh)
  (if (save-excursion (re-search-backward (concat "^" field ":\\([ \t]*\\)$")
                                          nil t))
      (replace-match (concat " " value) t t nil 1)
    (insert field ": " value "\n" (if (looking-at "\n") "" "\n"))))

(defun log-edit-insert-changelog (&optional use-first)
  "Insert a log message by looking at the ChangeLog.
The idea is to write your ChangeLog entries first, and then use this
command to commit your changes.

To select default log text, we:
- find the ChangeLog entries for the files to be checked in,
- verify that the top entry in the ChangeLog is on the current date
  and by the current user; if not, we don't provide any default text,
- search the ChangeLog entry for paragraphs containing the names of
  the files we're checking in, and finally
- use those paragraphs as the log text.

If the optional prefix arg USE-FIRST is given (via \\[universal-argument]),
or if the command is repeated a second time in a row, use the first log entry
regardless of user name or time."
  (interactive "P")
  (let ((eoh (save-excursion (rfc822-goto-eoh) (point))))
    (when (<= (point) eoh)
      (goto-char eoh)
      (if (looking-at "\n") (forward-char 1))))
  (let ((author
         (let ((log-edit-changelog-use-first
                (or use-first (eq last-command 'log-edit-insert-changelog))))
           (log-edit-insert-changelog-entries (log-edit-files)))))
    (log-edit-set-common-indentation)
    ;; Add an Author: field if appropriate.
    (when author (log-edit-add-field "Author" author))
    ;; Add a Fixes: field if applicable.
    (when (consp log-edit-rewrite-fixes)
      (rfc822-goto-eoh)
      (when (re-search-forward (car log-edit-rewrite-fixes) nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (fixes (match-substitute-replacement
                      (cdr log-edit-rewrite-fixes))))
          (delete-region start end)
          (log-edit-add-field "Fixes" fixes))))
    (and log-edit-strip-single-file-name
         (progn (rfc822-goto-eoh)
                (if (looking-at "\n") (forward-char 1))
                (looking-at "\\*\\s-+"))
         (let ((start (point)))
           (forward-line 1)
           (when (not (re-search-forward "^\\*\\s-+" nil t))
             (goto-char start)
             (skip-chars-forward "^():")
             (skip-chars-forward ": ")
             (delete-region start (point)))))
    (goto-char (point-min))))

;;;;
;;;; functions for getting commit message from ChangeLog a file...
;;;; Courtesy Jim Blandy
;;;;

(defun log-edit-narrow-changelog ()
  "Narrow to the top page of the current buffer, a ChangeLog file.
Actually, the narrowed region doesn't include the date line.
A \"page\" in a ChangeLog file is the area between two dates."
  (or (eq major-mode 'change-log-mode)
      (error "log-edit-narrow-changelog: current buffer isn't a ChangeLog"))

  (goto-char (point-min))

  ;; Skip date line and subsequent blank lines.
  (forward-line 1)
  (if (looking-at "[ \t\n]*\n")
      (goto-char (match-end 0)))

  (let ((start (point)))
    (forward-page 1)
    (narrow-to-region start (point))
    (goto-char (point-min))))

(defun log-edit-changelog-paragraph ()
  "Return the bounds of the ChangeLog paragraph containing point.
If we are between paragraphs, return the previous paragraph."
  (beginning-of-line)
  (if (looking-at "^[ \t]*$")
      (skip-chars-backward " \t\n" (point-min)))
  (list (progn
          (if (re-search-backward "^[ \t]*\n" nil 'or-to-limit)
              (goto-char (match-end 0)))
          (point))
        (if (re-search-forward "^[ \t\n]*$" nil t)
            (match-beginning 0)
          (point-max))))

(defun log-edit-changelog-subparagraph ()
  "Return the bounds of the ChangeLog subparagraph containing point.
A subparagraph is a block of non-blank lines beginning with an asterisk.
If we are between sub-paragraphs, return the previous subparagraph."
    (end-of-line)
    (if (search-backward "*" nil t)
        (list (progn (beginning-of-line) (point))
              (progn
                (forward-line 1)
                (if (re-search-forward "^[ \t]*[\n*]" nil t)
                    (match-beginning 0)
                  (point-max))))
    (list (point) (point))))

(defun log-edit-changelog-entry ()
  "Return the bounds of the ChangeLog entry containing point.
The variable `log-edit-changelog-full-paragraphs' decides whether an
\"entry\" is a paragraph or a subparagraph; see its documentation string
for more details."
  (save-excursion
    (if log-edit-changelog-full-paragraphs
        (log-edit-changelog-paragraph)
      (log-edit-changelog-subparagraph))))

(defvar user-full-name)
(defvar user-mail-address)

(defvar log-edit-author)                ;Dynamically scoped.

(defun log-edit-changelog-ours-p ()
  "See if ChangeLog entry at point is for the current user, today.
Return non-nil if it is."
  ;; Code adapted from add-change-log-entry.
  (let ((name (or (and (boundp 'add-log-full-name) add-log-full-name)
		  (and (fboundp 'user-full-name) (user-full-name))
		  (and (boundp 'user-full-name) user-full-name)))
        (mail (or (and (boundp 'add-log-mailing-address) add-log-mailing-address)
		  ;;(and (fboundp 'user-mail-address) (user-mail-address))
		  (and (boundp 'user-mail-address) user-mail-address)))
	(time (or (and (boundp 'add-log-time-format)
		       (functionp add-log-time-format)
		       (funcall add-log-time-format))
		  (format-time-string "%Y-%m-%d"))))
    (if (null log-edit-changelog-use-first)
        (looking-at (regexp-quote (format "%s  %s  <%s>" time name mail)))
      ;; Check the author, to potentially add it as a "Author: " header.
      (when (looking-at "[^ \t]")
        (when (and (boundp 'log-edit-author)
                   (not (looking-at (format ".+  .+  <%s>"
                                            (regexp-quote mail))))
                   (looking-at ".+  \\(.+  <.+>\\)"))
          (let ((author (replace-regexp-in-string "  " " "
                                                  (match-string 1))))
            (unless (and log-edit-author
                         (string-match (regexp-quote author) log-edit-author))
              (setq log-edit-author
                    (if log-edit-author
                        (concat log-edit-author ", " author)
                      author)))))
        t))))

(defun log-edit-changelog-entries (file)
  "Return the ChangeLog entries for FILE, and the ChangeLog they came from.
The return value looks like this:
  (LOGBUFFER (ENTRYSTART ENTRYEND) ...)
where LOGBUFFER is the name of the ChangeLog buffer, and each
\(ENTRYSTART . ENTRYEND\) pair is a buffer region."
  (let ((changelog-file-name
         (let ((default-directory
                 (file-name-directory (expand-file-name file)))
               (visiting-buffer (find-buffer-visiting file)))
           ;; If there is a buffer visiting FILE, and it has a local
           ;; value for `change-log-default-name', use that.
           (if (and visiting-buffer
                    (local-variable-p 'change-log-default-name
                                      visiting-buffer))
               (with-current-buffer visiting-buffer
                 change-log-default-name)
             ;; `find-change-log' uses `change-log-default-name' if set
             ;; and sets it before exiting, so we need to work around
             ;; that memoizing which is undesired here
             (setq change-log-default-name nil)
             (find-change-log)))))
    (with-current-buffer (find-file-noselect changelog-file-name)
      (unless (eq major-mode 'change-log-mode) (change-log-mode))
      (goto-char (point-min))
      (if (looking-at "\\s-*\n") (goto-char (match-end 0)))
      (if (not (log-edit-changelog-ours-p))
	  (list (current-buffer))
	(save-restriction
	  (log-edit-narrow-changelog)
	  (goto-char (point-min))

	  ;; Search for the name of FILE relative to the ChangeLog.  If that
	  ;; doesn't occur anywhere, they're not using full relative
	  ;; filenames in the ChangeLog, so just look for FILE; we'll accept
	  ;; some false positives.
	  (let ((pattern (file-relative-name
			  file (file-name-directory changelog-file-name))))
	    (if (or (string= pattern "")
		    (not (save-excursion
			   (search-forward pattern nil t))))
		(setq pattern (file-name-nondirectory file)))

            (setq pattern (concat "\\(^\\|[^[:alnum:]]\\)"
                                  (regexp-quote pattern)
                                  "\\($\\|[^[:alnum:]]\\)"))

	    (let (texts
                  (pos (point)))
	      (while (and (not (eobp)) (re-search-forward pattern nil t))
		(let ((entry (log-edit-changelog-entry)))
                  (if (< (elt entry 1) (max (1+ pos) (point)))
                      ;; This is not relevant, actually.
                      nil
                    (push entry texts))
                  ;; Make sure we make progress.
                  (setq pos (max (1+ pos) (elt entry 1)))
		  (goto-char pos)))

	      (cons (current-buffer) texts))))))))

(defun log-edit-changelog-insert-entries (buffer beg end &rest files)
  "Insert the text from BUFFER between BEG and END.
Rename relative filenames in the ChangeLog entry as FILES."
  (let ((opoint (point))
	(log-name (buffer-file-name buffer))
	(case-fold-search nil)
	bound)
    (insert-buffer-substring buffer beg end)
    (setq bound (point-marker))
    (when log-name
      (dolist (f files)
	(save-excursion
	  (goto-char opoint)
	  (when (re-search-forward
		 (concat "\\(^\\|[ \t]\\)\\("
			 (file-relative-name f (file-name-directory log-name))
			 "\\)[, :\n]")
		 bound t)
	    (replace-match f t t nil 2)))))
    ;; Eliminate tabs at the beginning of the line.
    (save-excursion
      (goto-char opoint)
      (while (re-search-forward "^\\(\t+\\)" bound t)
	(replace-match "")))))

(defun log-edit-insert-changelog-entries (files)
  "Given a list of files FILES, insert the ChangeLog entries for them."
  (let ((log-entries nil)
        (log-edit-author nil))
    ;; Note that any ChangeLog entry can apply to more than one file.
    ;; Here we construct a log-entries list with elements of the form
    ;;   ((LOGBUFFER ENTRYSTART ENTRYEND) FILE1 FILE2...)
    (dolist (file files)
      (let* ((entries (log-edit-changelog-entries file))
	     (buf (car entries))
	     key entry)
	(dolist (region (cdr entries))
	  (setq key (cons buf region))
	  (if (setq entry (assoc key log-entries))
	      (setcdr entry (append (cdr entry) (list file)))
	    (push (list key file) log-entries)))))
    ;; Now map over log-entries, and extract the strings.
    (dolist (log-entry (nreverse log-entries))
      (apply 'log-edit-changelog-insert-entries
	     (append (car log-entry) (cdr log-entry)))
      (insert "\n"))
    log-edit-author))

(defun log-edit-extract-headers (headers comment)
  "Extract headers from COMMENT to form command line arguments.
HEADERS should be an alist with elements of the form (HEADER . CMDARG)
associating header names to the corresponding cmdline option name and the
result is then a list of the form (MSG CMDARG1 HDRTEXT1 CMDARG2 HDRTEXT2...).
where MSG is the remaining text from STRING.
If \"Summary\" is not in HEADERS, then the \"Summary\" header is extracted
anyway and put back as the first line of MSG."
  (with-temp-buffer
    (insert comment)
    (rfc822-goto-eoh)
    (narrow-to-region (point-min) (point))
    (let ((case-fold-search t)
          (summary ())
          (res ()))
      (dolist (header (if (assoc "Summary" headers) headers
                        (cons '("Summary" . t) headers)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (car header)
                                          ":" log-edit-header-contents-regexp)
                                  nil t)
          (if (eq t (cdr header))
              (setq summary (match-string 1))
            (push (match-string 1) res)
            (push (or (cdr header) (car header)) res))
          (replace-match "" t t)))
      ;; Remove header separator if the header is empty.
      (widen)
      (goto-char (point-min))
      (when (looking-at "\\([ \t]*\n\\)+")
        (delete-region (match-beginning 0) (match-end 0)))
      (if summary (insert summary "\n"))
      (cons (buffer-string) res))))

(provide 'log-edit)

;;; log-edit.el ends here
