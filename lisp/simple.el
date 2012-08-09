;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985-1987, 1993-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
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

;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.

;;; Code:

(eval-when-compile (require 'cl))       ;For define-minor-mode.

(declare-function widget-convert "wid-edit" (type &rest args))
(declare-function shell-mode "shell" ())

;;; From compile.el
(defvar compilation-current-error)
(defvar compilation-context-lines)

(defcustom idle-update-delay 0.5
  "Idle time delay before updating various things on the screen.
Various Emacs features that update auxiliary information when point moves
wait this many seconds after Emacs becomes idle before doing an update."
  :type 'number
  :group 'display
  :version "22.1")

(defgroup killing nil
  "Killing and yanking commands."
  :group 'editing)

(defgroup paren-matching nil
  "Highlight (un)matching of parens and expressions."
  :group 'matching)

;;; next-error support framework

(defgroup next-error nil
  "`next-error' support framework."
  :group 'compilation
  :version "22.1")

(defface next-error
  '((t (:inherit region)))
  "Face used to highlight next error locus."
  :group 'next-error
  :version "22.1")

(defcustom next-error-highlight 0.5
  "Highlighting of locations in selected source buffers.
If a number, highlight the locus in `next-error' face for the given time
in seconds, or until the next command is executed.
If t, highlight the locus until the next command is executed, or until
some other locus replaces it.
If nil, don't highlight the locus in the source buffer.
If `fringe-arrow', indicate the locus by the fringe arrow
indefinitely until some other locus replaces it."
  :type '(choice (number :tag "Highlight for specified time")
                 (const :tag "Semipermanent highlighting" t)
                 (const :tag "No highlighting" nil)
                 (const :tag "Fringe arrow" fringe-arrow))
  :group 'next-error
  :version "22.1")

(defcustom next-error-highlight-no-select 0.5
  "Highlighting of locations in `next-error-no-select'.
If number, highlight the locus in `next-error' face for given time in seconds.
If t, highlight the locus indefinitely until some other locus replaces it.
If nil, don't highlight the locus in the source buffer.
If `fringe-arrow', indicate the locus by the fringe arrow
indefinitely until some other locus replaces it."
  :type '(choice (number :tag "Highlight for specified time")
                 (const :tag "Semipermanent highlighting" t)
                 (const :tag "No highlighting" nil)
                 (const :tag "Fringe arrow" fringe-arrow))
  :group 'next-error
  :version "22.1")

(defcustom next-error-recenter nil
  "Display the line in the visited source file recentered as specified.
If non-nil, the value is passed directly to `recenter'."
  :type '(choice (integer :tag "Line to recenter to")
                 (const :tag "Center of window" (4))
                 (const :tag "No recentering" nil))
  :group 'next-error
  :version "23.1")

(defcustom next-error-hook nil
  "List of hook functions run by `next-error' after visiting source file."
  :type 'hook
  :group 'next-error)

(defvar next-error-highlight-timer nil)

(defvar next-error-overlay-arrow-position nil)
(put 'next-error-overlay-arrow-position 'overlay-arrow-string (purecopy "=>"))
(add-to-list 'overlay-arrow-variable-list 'next-error-overlay-arrow-position)

(defvar next-error-last-buffer nil
  "The most recent `next-error' buffer.
A buffer becomes most recent when its compilation, grep, or
similar mode is started, or when it is used with \\[next-error]
or \\[compile-goto-error].")

(defvar next-error-function nil
  "Function to use to find the next error in the current buffer.
The function is called with 2 parameters:
ARG is an integer specifying by how many errors to move.
RESET is a boolean which, if non-nil, says to go back to the beginning
of the errors before moving.
Major modes providing compile-like functionality should set this variable
to indicate to `next-error' that this is a candidate buffer and how
to navigate in it.")
(make-variable-buffer-local 'next-error-function)

(defvar next-error-move-function nil
  "Function to use to move to an error locus.
It takes two arguments, a buffer position in the error buffer
and a buffer position in the error locus buffer.
The buffer for the error locus should already be current.
nil means use goto-char using the second argument position.")
(make-variable-buffer-local 'next-error-move-function)

(defsubst next-error-buffer-p (buffer
			       &optional avoid-current
			       extra-test-inclusive
			       extra-test-exclusive)
  "Test if BUFFER is a `next-error' capable buffer.

If AVOID-CURRENT is non-nil, treat the current buffer
as an absolute last resort only.

The function EXTRA-TEST-INCLUSIVE, if non-nil, is called in each buffer
that normally would not qualify.  If it returns t, the buffer
in question is treated as usable.

The function EXTRA-TEST-EXCLUSIVE, if non-nil, is called in each buffer
that would normally be considered usable.  If it returns nil,
that buffer is rejected."
  (and (buffer-name buffer)		;First make sure it's live.
       (not (and avoid-current (eq buffer (current-buffer))))
       (with-current-buffer buffer
	 (if next-error-function   ; This is the normal test.
	     ;; Optionally reject some buffers.
	     (if extra-test-exclusive
		 (funcall extra-test-exclusive)
	       t)
	   ;; Optionally accept some other buffers.
	   (and extra-test-inclusive
		(funcall extra-test-inclusive))))))

(defun next-error-find-buffer (&optional avoid-current
					 extra-test-inclusive
					 extra-test-exclusive)
  "Return a `next-error' capable buffer.

If AVOID-CURRENT is non-nil, treat the current buffer
as an absolute last resort only.

The function EXTRA-TEST-INCLUSIVE, if non-nil, is called in each buffer
that normally would not qualify.  If it returns t, the buffer
in question is treated as usable.

The function EXTRA-TEST-EXCLUSIVE, if non-nil, is called in each buffer
that would normally be considered usable.  If it returns nil,
that buffer is rejected."
  (or
   ;; 1. If one window on the selected frame displays such buffer, return it.
   (let ((window-buffers
          (delete-dups
           (delq nil (mapcar (lambda (w)
                               (if (next-error-buffer-p
				    (window-buffer w)
                                    avoid-current
                                    extra-test-inclusive extra-test-exclusive)
                                   (window-buffer w)))
                             (window-list))))))
     (if (eq (length window-buffers) 1)
         (car window-buffers)))
   ;; 2. If next-error-last-buffer is an acceptable buffer, use that.
   (if (and next-error-last-buffer
            (next-error-buffer-p next-error-last-buffer avoid-current
                                 extra-test-inclusive extra-test-exclusive))
       next-error-last-buffer)
   ;; 3. If the current buffer is acceptable, choose it.
   (if (next-error-buffer-p (current-buffer) avoid-current
			    extra-test-inclusive extra-test-exclusive)
       (current-buffer))
   ;; 4. Look for any acceptable buffer.
   (let ((buffers (buffer-list)))
     (while (and buffers
                 (not (next-error-buffer-p
		       (car buffers) avoid-current
		       extra-test-inclusive extra-test-exclusive)))
       (setq buffers (cdr buffers)))
     (car buffers))
   ;; 5. Use the current buffer as a last resort if it qualifies,
   ;; even despite AVOID-CURRENT.
   (and avoid-current
	(next-error-buffer-p (current-buffer) nil
			     extra-test-inclusive extra-test-exclusive)
	(progn
	  (message "This is the only buffer with error message locations")
	  (current-buffer)))
   ;; 6. Give up.
   (error "No buffers contain error message locations")))

(defun next-error (&optional arg reset)
  "Visit next `next-error' message and corresponding source code.

If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages.
Just \\[universal-argument] as a prefix means reparse the error message buffer
and start at the first error.

The RESET argument specifies that we should restart from the beginning.

\\[next-error] normally uses the most recently started
compilation, grep, or occur buffer.  It can also operate on any
buffer with output from the \\[compile], \\[grep] commands, or,
more generally, on any buffer in Compilation mode or with
Compilation Minor mode enabled, or any buffer in which
`next-error-function' is bound to an appropriate function.
To specify use of a particular buffer for error messages, type
\\[next-error] in that buffer when it is the only one displayed
in the current frame.

Once \\[next-error] has chosen the buffer for error messages, it
runs `next-error-hook' with `run-hooks', and stays with that buffer
until you use it in some other buffer which uses Compilation mode
or Compilation Minor mode.

To control which errors are matched, customize the variable
`compilation-error-regexp-alist'."
  (interactive "P")
  (if (consp arg) (setq reset t arg nil))
  (when (setq next-error-last-buffer (next-error-find-buffer))
    ;; we know here that next-error-function is a valid symbol we can funcall
    (with-current-buffer next-error-last-buffer
      (funcall next-error-function (prefix-numeric-value arg) reset)
      (when next-error-recenter
        (recenter next-error-recenter))
      (run-hooks 'next-error-hook))))

(defun next-error-internal ()
  "Visit the source code corresponding to the `next-error' message at point."
  (setq next-error-last-buffer (current-buffer))
  ;; we know here that next-error-function is a valid symbol we can funcall
  (with-current-buffer next-error-last-buffer
    (funcall next-error-function 0 nil)
    (when next-error-recenter
      (recenter next-error-recenter))
    (run-hooks 'next-error-hook)))

(defalias 'goto-next-locus 'next-error)
(defalias 'next-match 'next-error)

(defun previous-error (&optional n)
  "Visit previous `next-error' message and corresponding source code.

Prefix arg N says how many error messages to move backwards (or
forwards, if negative).

This operates on the output from the \\[compile] and \\[grep] commands."
  (interactive "p")
  (next-error (- (or n 1))))

(defun first-error (&optional n)
  "Restart at the first error.
Visit corresponding source code.
With prefix arg N, visit the source code of the Nth error.
This operates on the output from the \\[compile] command, for instance."
  (interactive "p")
  (next-error n t))

(defun next-error-no-select (&optional n)
  "Move point to the next error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Finds and highlights the source line like \\[next-error], but does not
select the source buffer."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n))
  (pop-to-buffer next-error-last-buffer))

(defun previous-error-no-select (&optional n)
  "Move point to the previous error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move backwards (or
forwards, if negative).
Finds and highlights the source line like \\[previous-error], but does not
select the source buffer."
  (interactive "p")
  (next-error-no-select (- (or n 1))))

;; Internal variable for `next-error-follow-mode-post-command-hook'.
(defvar next-error-follow-last-line nil)

(define-minor-mode next-error-follow-minor-mode
  "Minor mode for compilation, occur and diff modes.
With a prefix argument ARG, enable mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable mode if ARG is
omitted or nil.
When turned on, cursor motion in the compilation, grep, occur or diff
buffer causes automatic display of the corresponding source code location."
  :group 'next-error :init-value nil :lighter " Fol"
  (if (not next-error-follow-minor-mode)
      (remove-hook 'post-command-hook 'next-error-follow-mode-post-command-hook t)
    (add-hook 'post-command-hook 'next-error-follow-mode-post-command-hook nil t)
    (make-local-variable 'next-error-follow-last-line)))

;; Used as a `post-command-hook' by `next-error-follow-mode'
;; for the *Compilation* *grep* and *Occur* buffers.
(defun next-error-follow-mode-post-command-hook ()
  (unless (equal next-error-follow-last-line (line-number-at-pos))
    (setq next-error-follow-last-line (line-number-at-pos))
    (condition-case nil
	(let ((compilation-context-lines nil))
	  (setq compilation-current-error (point))
	  (next-error-no-select 0))
      (error t))))


;;;

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables)
  (unless delay-mode-hooks
    (run-hooks 'after-change-major-mode-hook)))

;; Special major modes to view specially formatted data rather than files.

(defvar special-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'quit-window)
    (define-key map " " 'scroll-up-command)
    (define-key map "\C-?" 'scroll-down-command)
    (define-key map "?" 'describe-mode)
    (define-key map "h" 'describe-mode)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map "g" 'revert-buffer)
    (define-key map "z" 'kill-this-buffer)
    map))

(put 'special-mode 'mode-class 'special)
(define-derived-mode special-mode nil "Special"
  "Parent major mode from which special major modes should inherit."
  (setq buffer-read-only t))

;; Major mode meant to be the parent of programming modes.

(defvar prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-\M-q] 'prog-indent-sexp)
    map)
  "Keymap used for programming modes.")

(defun prog-indent-sexp ()
  "Indent the expression after point."
  (interactive)
  (let ((start (point))
        (end (save-excursion (forward-sexp 1) (point))))
    (indent-region start end nil)))

(define-derived-mode prog-mode fundamental-mode "Prog"
  "Major mode for editing programming language source code."
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Any programming language is always written left to right.
  (setq bidi-paragraph-direction 'left-to-right))

;; Making and deleting lines.

(defvar hard-newline (propertize "\n" 'hard t 'rear-nonsticky '(hard))
  "Propertized string representing a hard newline character.")

(defun newline (&optional arg)
  "Insert a newline, and move to left margin of the new line if it's blank.
If `use-hard-newlines' is non-nil, the newline is marked with the
text-property `hard'.
With ARG, insert that many newlines.
Call `auto-fill-function' if the current column number is greater
than the value of `fill-column' and ARG is nil."
  (interactive "*P")
  (barf-if-buffer-read-only)
  ;; Call self-insert so that auto-fill, abbrev expansion etc. happens.
  ;; Set last-command-event to tell self-insert what to insert.
  (let* ((was-page-start (and (bolp) (looking-at page-delimiter)))
         (beforepos (point))
         (last-command-event ?\n)
         ;; Don't auto-fill if we have a numeric argument.
         (auto-fill-function (if arg nil auto-fill-function))
         (postproc
          ;; Do the rest in post-self-insert-hook, because we want to do it
          ;; *before* other functions on that hook.
          (lambda ()
            ;; Mark the newline(s) `hard'.
            (if use-hard-newlines
                (set-hard-newline-properties
                 (- (point) (prefix-numeric-value arg)) (point)))
            ;; If the newline leaves the previous line blank, and we
            ;; have a left margin, delete that from the blank line.
            (save-excursion
              (goto-char beforepos)
              (beginning-of-line)
              (and (looking-at "[ \t]$")
                   (> (current-left-margin) 0)
                   (delete-region (point)
                                  (line-end-position))))
            ;; Indent the line after the newline, except in one case:
            ;; when we added the newline at the beginning of a line which
            ;; starts a page.
            (or was-page-start
                (move-to-left-margin nil t)))))
    (unwind-protect
        (progn
          (add-hook 'post-self-insert-hook postproc)
          (self-insert-command (prefix-numeric-value arg)))
      ;; We first used let-binding to protect the hook, but that was naive
      ;; since add-hook affects the symbol-default value of the variable,
      ;; whereas the let-binding might only protect the buffer-local value.
      (remove-hook 'post-self-insert-hook postproc)))
  nil)

(defun set-hard-newline-properties (from to)
  (let ((sticky (get-text-property from 'rear-nonsticky)))
    (put-text-property from to 'hard 't)
    ;; If rear-nonsticky is not "t", add 'hard to rear-nonsticky list
    (if (and (listp sticky) (not (memq 'hard sticky)))
	(put-text-property from (point) 'rear-nonsticky
			   (cons 'hard sticky)))))

(defun open-line (n)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (newline n)
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)))

(defun split-line (&optional arg)
  "Split current line, moving portion beyond point vertically down.
If the current line starts with `fill-prefix', insert it on the new
line as well.  With prefix ARG, don't insert `fill-prefix' on new line.

When called from Lisp code, ARG may be a prefix string to copy."
  (interactive "*P")
  (skip-chars-forward " \t")
  (let* ((col (current-column))
	 (pos (point))
	 ;; What prefix should we check for (nil means don't).
	 (prefix (cond ((stringp arg) arg)
		       (arg nil)
		       (t fill-prefix)))
	 ;; Does this line start with it?
	 (have-prfx (and prefix
			 (save-excursion
			   (beginning-of-line)
			   (looking-at (regexp-quote prefix))))))
    (newline 1)
    (if have-prfx (insert-and-inherit prefix))
    (indent-to col 0)
    (goto-char pos)))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	;; If the second line started with the fill prefix,
	;; delete the prefix.
	(if (and fill-prefix
		 (<= (+ (point) (length fill-prefix)) (point-max))
		 (string= fill-prefix
			  (buffer-substring (point)
					    (+ (point) (length fill-prefix)))))
	    (delete-region (point) (+ (point) (length fill-prefix))))
	(fixup-whitespace))))

(defalias 'join-line #'delete-indentation) ; easier to find

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete any immediately following blank lines."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    ;; Delete preceding blank lines, and this one too if it's the only one.
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    ;; Delete following blank lines, unless the current line is blank
    ;; and there are no following blank lines.
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))
    ;; Handle the special case where point is followed by newline and eob.
    ;; Delete the line, leaving point at eob.
    (if (looking-at "^[ \t]*\n\\'")
	(delete-region (point) (point-max)))))

(defun delete-trailing-whitespace (&optional start end)
  "Delete all the trailing whitespace across the current buffer.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
A formfeed is not considered whitespace by this function.
If END is nil, also delete all trailing lines at the end of the buffer.
If the region is active, only delete whitespace within the region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (copy-marker (or end (point-max))))
            (start (or start (point-min))))
        (goto-char start)
        (while (re-search-forward "\\s-$" end-marker t)
          (skip-syntax-backward "-" (line-beginning-position))
          ;; Don't delete formfeeds, even if they are considered whitespace.
          (if (looking-at-p ".*\f")
              (goto-char (match-end 0)))
          (delete-region (point) (match-end 0)))
        ;; Delete trailing empty lines.
        (goto-char end-marker)
        (when (and (not end)
                   ;; Really the end of buffer.
                   (save-restriction (widen) (eobp))
                   (<= (skip-chars-backward "\n") -2))
          (delete-region (1+ (point)) end-marker))
        (set-marker end-marker nil))))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (delete-horizontal-space t)
  (newline)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means calling the current value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (let ((pos (point)))
    ;; Be careful to insert the newline before indenting the line.
    ;; Otherwise, the indentation might be wrong.
    (newline)
    (save-excursion
      (goto-char pos)
      ;; We are at EOL before the call to indent-according-to-mode, and
      ;; after it we usually are as well, but not always.  We tried to
      ;; address it with `save-excursion' but that uses a normal marker
      ;; whereas we need `move after insertion', so we do the save/restore
      ;; by hand.
      (setq pos (copy-marker pos t))
      (indent-according-to-mode)
      (goto-char pos)
      ;; Remove the trailing white-space after indentation because
      ;; indentation may introduce the whitespace.
      (delete-horizontal-space t))
    (indent-according-to-mode)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.
With argument, insert ARG copies of the character.

If the first character you type after this command is an octal digit,
you should type a sequence of octal digits which specify a character code.
Any nondigit terminates the sequence.  If the terminator is a RET,
it is discarded; any other terminator is used itself as input.
The variable `read-quoted-char-radix' specifies the radix for this feature;
set it to 10 or 16 to use decimal or hex instead of octal.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is intended to be
useful for editing binary files."
  (interactive "*p")
  (let* ((char
	  ;; Avoid "obsolete" warnings for translation-table-for-input.
	  (with-no-warnings
	    (let (translation-table-for-input input-method-function)
	      (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		(read-char))))))
    ;; This used to assume character codes 0240 - 0377 stand for
    ;; characters in some single-byte character set, and converted them
    ;; to Emacs characters.  But in 23.1 this feature is deprecated
    ;; in favor of inserting the corresponding Unicode characters.
    ;; (if (and enable-multibyte-characters
    ;;          (>= char ?\240)
    ;;          (<= char ?\377))
    ;;     (setq char (unibyte-char-to-multibyte char)))
    (if (> arg 0)
	(if (eq overwrite-mode 'overwrite-mode-binary)
	    (delete-char arg)))
    (while (> arg 0)
      (insert-and-inherit char)
      (setq arg (1- arg)))))

(defun forward-to-indentation (&optional arg)
  "Move forward ARG lines and position at first nonblank character."
  (interactive "^p")
  (forward-line (or arg 1))
  (skip-chars-forward " \t"))

(defun backward-to-indentation (&optional arg)
  "Move backward ARG lines and position at first nonblank character."
  (interactive "^p")
  (forward-line (- (or arg 1)))
  (skip-chars-forward " \t"))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive "^")
  (beginning-of-line 1)
  (skip-syntax-forward " " (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'")))
	nil
      (insert ?\s))))

(defun delete-horizontal-space (&optional backward-only)
  "Delete all spaces and tabs around point.
If BACKWARD-ONLY is non-nil, only delete them before point."
  (interactive "*P")
  (let ((orig-pos (point)))
    (delete-region
     (if backward-only
	 orig-pos
       (progn
	 (skip-chars-forward " \t")
	 (constrain-to-field nil orig-pos t)))
     (progn
       (skip-chars-backward " \t")
       (constrain-to-field nil orig-pos)))))

(defun just-one-space (&optional n)
  "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well."
  (interactive "*p")
  (unless n (setq n 1))
  (let ((orig-pos (point))
        (skip-characters (if (< n 0) " \t\n\r" " \t"))
        (n (abs n)))
    (skip-chars-backward skip-characters)
    (constrain-to-field nil orig-pos)
    (dotimes (i n)
      (if (= (following-char) ?\s)
	  (forward-char 1)
	(insert ?\s)))
    (delete-region
     (point)
     (progn
       (skip-chars-forward skip-characters)
       (constrain-to-field nil orig-pos t)))))

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer.
With numeric arg N, put point N/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.

If Transient Mark mode is disabled, leave mark at previous
position, unless a \\[universal-argument] prefix is supplied.

Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster."
  (interactive "^P")
  (or (consp arg)
      (region-active-p)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))
		 (point-min))))
  (if (and arg (not (consp arg))) (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer.
With numeric arg N, put point N/10 of the way from the end.
If the buffer is narrowed, this command uses the end of the
accessible part of the buffer.

If Transient Mark mode is disabled, leave mark at previous
position, unless a \\[universal-argument] prefix is supplied.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster."
  (interactive "^P")
  (or (consp arg) (region-active-p) (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
		   (- (point-max)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (* size (prefix-numeric-value arg)) 10)))
		 (point-max))))
  ;; If we went to a place in the middle of the buffer,
  ;; adjust it to the beginning of a line.
  (cond ((and arg (not (consp arg))) (forward-line 1))
	((> (point) (window-end nil t))
	 ;; If the end of the buffer is not already on the screen,
	 ;; then scroll specially to put it near, but not at, the bottom.
	 (overlay-recenter (point))
	 (recenter -3))))

(defcustom delete-active-region t
  "Whether single-char deletion commands delete an active region.
This has an effect only if Transient Mark mode is enabled, and
affects `delete-forward-char' and `delete-backward-char', though
not `delete-char'.

If the value is the symbol `kill', the active region is killed
instead of deleted."
  :type '(choice (const :tag "Delete active region" t)
                 (const :tag "Kill active region" kill)
                 (const :tag "Do ordinary deletion" nil))
  :group 'killing
  :version "24.1")

(defun delete-backward-char (n &optional killflag)
  "Delete the previous N characters (following if N is negative).
If Transient Mark mode is enabled, the mark is active, and N is 1,
delete the text in the region and deactivate the mark instead.
To disable this, set `delete-active-region' to nil.

Optional second arg KILLFLAG, if non-nil, means to kill (save in
kill ring) instead of delete.  Interactively, N is the prefix
arg, and KILLFLAG is set if N is explicitly specified.

In Overwrite mode, single character backward deletion may replace
tabs with spaces so as to back over columns, unless point is at
the end of the line."
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
	      delete-active-region
	      (= n 1))
	 ;; If a region is active, kill or delete it.
	 (if (eq delete-active-region 'kill)
	     (kill-region (region-beginning) (region-end))
	   (delete-region (region-beginning) (region-end))))
	;; In Overwrite mode, maybe untabify while deleting
	((null (or (null overwrite-mode)
		   (<= n 0)
		   (memq (char-before) '(?\t ?\n))
		   (eobp)
		   (eq (char-after) ?\n)))
	 (let ((ocol (current-column)))
           (delete-char (- n) killflag)
	   (save-excursion
	     (insert-char ?\s (- ocol (current-column)) nil))))
	;; Otherwise, do simple deletion.
	(t (delete-char (- n) killflag))))

(defun delete-forward-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
If Transient Mark mode is enabled, the mark is active, and N is 1,
delete the text in the region and deactivate the mark instead.
To disable this, set `delete-active-region' to nil.

Optional second arg KILLFLAG non-nil means to kill (save in kill
ring) instead of delete.  Interactively, N is the prefix arg, and
KILLFLAG is set if N was explicitly specified."
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
	      delete-active-region
	      (= n 1))
	 ;; If a region is active, kill or delete it.
	 (if (eq delete-active-region 'kill)
	     (kill-region (region-beginning) (region-end))
	   (delete-region (region-beginning) (region-end))))
	;; Otherwise, do simple deletion.
	(t (delete-char n killflag))))

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
If narrowing is in effect, only uses the accessible part of the buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))


;; Counting lines, one way or another.

(defun goto-line (line &optional buffer)
  "Go to LINE, counting from line 1 at beginning of buffer.
If called interactively, a numeric prefix argument specifies
LINE; without a numeric prefix argument, read LINE from the
minibuffer.

If optional argument BUFFER is non-nil, switch to that buffer and
move to line LINE there.  If called interactively with \\[universal-argument]
as argument, BUFFER is the most recently selected other buffer.

Prior to moving point, this function sets the mark (without
activating it), unless Transient Mark mode is enabled and the
mark is already active.

This function is usually the wrong thing to use in a Lisp program.
What you probably want instead is something like:
  (goto-char (point-min))
  (forward-line (1- N))
If at all possible, an even better solution is to use char counts
rather than line counts."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     ;; Look for a default, a number in the buffer at point.
     (let* ((default
	      (save-excursion
		(skip-chars-backward "0-9")
		(if (looking-at "[0-9]")
		    (string-to-number
		     (buffer-substring-no-properties
		      (point)
		      (progn (skip-chars-forward "0-9")
			     (point)))))))
	    ;; Decide if we're switching buffers.
	    (buffer
	     (if (consp current-prefix-arg)
		 (other-buffer (current-buffer) t)))
	    (buffer-prompt
	     (if buffer
		 (concat " in " (buffer-name buffer))
	       "")))
       ;; Read the argument, offering that number (if any) as default.
       (list (read-number (format (if default "Goto line%s (%s): "
                                    "Goto line%s: ")
                                  buffer-prompt
                                  default)
                          default)
	     buffer))))
  ;; Switch to the desired buffer, one way or another.
  (if buffer
      (let ((window (get-buffer-window buffer)))
	(if window (select-window window)
	  (switch-to-buffer-other-window buffer))))
  ;; Leave mark at previous position
  (or (region-active-p) (push-mark))
  ;; Move to the specified line number in that buffer.
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

(defun count-words-region (start end)
  "Count the number of words in the region.
If called interactively, print a message reporting the number of
lines, words, and chars in the region.
If called from Lisp, return the number of words between positions
START and END."
  (interactive "r")
  (if (called-interactively-p 'any)
      (count-words--message "Region" start end)
    (count-words start end)))

(defun count-words (start end)
  "Count words between START and END.
If called interactively, START and END are normally the start and
end of the buffer; but if the region is active, START and END are
the start and end of the region.  Print a message reporting the
number of lines, words, and chars.

If called from Lisp, return the number of words between START and
END, without printing any message."
  (interactive (list nil nil))
  (cond ((not (called-interactively-p 'any))
	 (let ((words 0))
	   (save-excursion
	     (save-restriction
	       (narrow-to-region start end)
	       (goto-char (point-min))
	       (while (forward-word 1)
		 (setq words (1+ words)))))
	   words))
	((use-region-p)
	 (call-interactively 'count-words-region))
	(t
	 (count-words--message "Buffer" (point-min) (point-max)))))

(defun count-words--message (str start end)
  (let ((lines (count-lines start end))
	(words (count-words start end))
	(chars (- end start)))
    (message "%s has %d line%s, %d word%s, and %d character%s."
	     str
	     lines (if (= lines 1) "" "s")
	     words (if (= words 1) "" "s")
	     chars (if (= chars 1) "" "s"))))

(define-obsolete-function-alias 'count-lines-region 'count-words-region "24.1")

(defun what-line ()
  "Print the current buffer line number and narrowed line number of point."
  (interactive)
  (let ((start (point-min))
	(n (line-number-at-pos)))
    (if (= start 1)
	(message "Line %d" n)
      (save-excursion
	(save-restriction
	  (widen)
	  (message "line %d (narrowed line %d)"
		   (+ n (line-number-at-pos start) -1) n))))))

(defun count-lines (start end)
  "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (eq selective-display t)
	  (save-match-data
	    (let ((done 0))
                     (while (re-search-forward "[\n\C-m]" nil t 40)
                       (setq done (+ 40 done)))
                     (while (re-search-forward "[\n\C-m]" nil t 1)
                       (setq done (+ 1 done)))
                     (goto-char (point-max))
                     (if (and (/= start end)
		       (not (bolp)))
		  (1+ done)
		done)))
	(- (buffer-size) (forward-line (buffer-size)))))))

(defun line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
Counting starts at (point-min), so the value refers
to the contents of the accessible portion of the buffer."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun what-cursor-position (&optional detail)
  "Print info on cursor position (on screen and within buffer).
Also describe the character after point, and give its character code
in octal, decimal and hex.

For a non-ASCII multibyte character, also give its encoding in the
buffer's selected coding system if the coding system encodes the
character safely.  If the character is encoded into one byte, that
code is shown in hex.  If the character is encoded into more than one
byte, just \"...\" is shown.

In addition, with prefix argument, show details about that character
in *Help* buffer.  See also the command `describe-char'."
  (interactive "P")
  (let* ((char (following-char))
	 (bidi-fixer
	  (cond ((memq char '(?\x202a ?\x202b ?\x202d ?\x202e))
		 ;; If the character is one of LRE, LRO, RLE, RLO, it
		 ;; will start a directional embedding, which could
		 ;; completely disrupt the rest of the line (e.g., RLO
		 ;; will display the rest of the line right-to-left).
		 ;; So we put an invisible PDF character after these
		 ;; characters, to end the embedding, which eliminates
		 ;; any effects on the rest of the line.
		 (propertize (string ?\x202c) 'invisible t))
		;; Strong right-to-left characters cause reordering of
		;; the following numerical characters which show the
		;; codepoint, so append LRM to countermand that.
		((memq (get-char-code-property char 'bidi-class) '(R AL))
		 (propertize (string ?\x200e) 'invisible t))
		(t
		 "")))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column)))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d (%d%%) <%d-%d> column=%d%s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d (EOB) column=%d%s"
		   pos total col hscroll))
      (let ((coding buffer-file-coding-system)
	    encoded encoding-msg display-prop under-display)
	(if (or (not coding)
		(eq (coding-system-type coding) t))
	    (setq coding (default-value 'buffer-file-coding-system)))
	(if (eq (char-charset char) 'eight-bit)
	    (setq encoding-msg
		  (format "(%d, #o%o, #x%x, raw-byte)" char char char))
	  ;; Check if the character is displayed with some `display'
	  ;; text property.  In that case, set under-display to the
	  ;; buffer substring covered by that property.
	  (setq display-prop (get-char-property pos 'display))
	  (if display-prop
	      (let ((to (or (next-single-char-property-change pos 'display)
			    (point-max))))
		(if (< to (+ pos 4))
		    (setq under-display "")
		  (setq under-display "..."
			to (+ pos 4)))
		(setq under-display
		      (concat (buffer-substring-no-properties pos to)
			      under-display)))
	    (setq encoded (and (>= char 128) (encode-coding-char char coding))))
	  (setq encoding-msg
		(if display-prop
		    (if (not (stringp display-prop))
			(format "(%d, #o%o, #x%x, part of display \"%s\")"
				char char char under-display)
		      (format "(%d, #o%o, #x%x, part of display \"%s\"->\"%s\")"
			      char char char under-display display-prop))
		  (if encoded
		      (format "(%d, #o%o, #x%x, file %s)"
			      char char char
			      (if (> (length encoded) 1)
				  "..."
				(encoded-string-description encoded coding)))
		    (format "(%d, #o%o, #x%x)" char char char)))))
	(if detail
	    ;; We show the detailed information about CHAR.
	    (describe-char (point)))
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "Char: %s%s %s point=%d of %d (%d%%) <%d-%d> column=%d%s"
		     (if (< char 256)
			 (single-key-description char)
		       (buffer-substring-no-properties (point) (1+ (point))))
		     bidi-fixer
		     encoding-msg pos total percent beg end col hscroll)
	  (message "Char: %s%s %s point=%d of %d (%d%%) column=%d%s"
		   (if enable-multibyte-characters
		       (if (< char 128)
			   (single-key-description char)
			 (buffer-substring-no-properties (point) (1+ (point))))
		     (single-key-description char))
		   bidi-fixer encoding-msg pos total percent col hscroll))))))

;; Initialize read-expression-map.  It is defined at C level.
(let ((m (make-sparse-keymap)))
  (define-key m "\M-\t" 'lisp-complete-symbol)
  ;; Might as well bind TAB to completion, since inserting a TAB char is much
  ;; too rarely useful.
  (define-key m "\t" 'lisp-complete-symbol)
  (set-keymap-parent m minibuffer-local-map)
  (setq read-expression-map m))

(defvar minibuffer-completing-symbol nil
  "Non-nil means completing a Lisp symbol in the minibuffer.")
(make-obsolete-variable 'minibuffer-completing-symbol nil "24.1" 'get)

(defvar minibuffer-default nil
  "The current default value or list of default values in the minibuffer.
The functions `read-from-minibuffer' and `completing-read' bind
this variable locally.")

(defcustom eval-expression-print-level 4
  "Value for `print-level' while printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom eval-expression-print-length 12
  "Value for `print-length' while printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom eval-expression-debug-on-error t
  "If non-nil set `debug-on-error' to t in `eval-expression'.
If nil, don't change the value of `debug-on-error'."
  :group 'lisp
  :type 'boolean
  :version "21.1")

(defun eval-expression-print-format (value)
  "Format VALUE as a result of evaluated expression.
Return a formatted string which is displayed in the echo area
in addition to the value printed by prin1 in functions which
display the result of expression evaluation."
  (if (and (integerp value)
           (or (not (memq this-command '(eval-last-sexp eval-print-last-sexp)))
               (eq this-command last-command)
               (if (boundp 'edebug-active) edebug-active)))
      (let ((char-string
             (if (or (if (boundp 'edebug-active) edebug-active)
		     (memq this-command '(eval-last-sexp eval-print-last-sexp)))
                 (prin1-char value))))
        (if char-string
            (format " (#o%o, #x%x, %s)" value value char-string)
          (format " (#o%o, #x%x)" value value)))))

;; We define this, rather than making `eval' interactive,
;; for the sake of completion of names like eval-region, eval-buffer.
(defun eval-expression (eval-expression-arg
			&optional eval-expression-insert-value)
  "Evaluate EVAL-EXPRESSION-ARG and print value in the echo area.
When called interactively, read an Emacs Lisp expression and
evaluate it.
Value is also consed on to front of the variable `values'.
Optional argument EVAL-EXPRESSION-INSERT-VALUE non-nil (interactively,
with prefix argument) means insert the result into the current buffer
instead of printing it in the echo area.  Truncates long output
according to the value of the variables `eval-expression-print-length'
and `eval-expression-print-level'.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger."
  (interactive
   (list (let ((minibuffer-completing-symbol t))
	   (read-from-minibuffer "Eval: "
				 nil read-expression-map t
				 'read-expression-history))
	 current-prefix-arg))

  (if (null eval-expression-debug-on-error)
      (push (eval eval-expression-arg lexical-binding) values)
    (let ((old-value (make-symbol "t")) new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evalled code changes it.
      (let ((debug-on-error old-value))
	(push (eval eval-expression-arg lexical-binding) values)
	(setq new-value debug-on-error))
      ;; If evalled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
	(setq debug-on-error new-value))))

  (let ((print-length eval-expression-print-length)
	(print-level eval-expression-print-level))
    (if eval-expression-insert-value
	(with-no-warnings
	 (let ((standard-output (current-buffer)))
	   (prin1 (car values))))
      (prog1
          (prin1 (car values) t)
        (let ((str (eval-expression-print-format (car values))))
          (if str (princ str t)))))))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command
	 (let ((print-level nil)
	       (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
	   (unwind-protect
	       (read-from-minibuffer prompt
				     (prin1-to-string command)
				     read-expression-map t
				     'command-history)
	     ;; If command was added to command-history as a string,
	     ;; get rid of that.  We want only evaluable expressions there.
	     (if (stringp (car command-history))
		 (setq command-history (cdr command-history)))))))

    ;; If command to be redone does not match front of history,
    ;; add it to the history.
    (or (equal command (car command-history))
	(setq command-history (cons command command-history)))
    (eval command)))

(defun repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous
command it is added to the front of the command history.
You can use the minibuffer history commands \
\\<minibuffer-local-map>\\[next-history-element] and \\[previous-history-element]
to get different commands to edit and resubmit."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))
	newcmd)
    (if elt
	(progn
	  (setq newcmd
		(let ((print-level nil)
		      (minibuffer-history-position arg)
		      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
		  (unwind-protect
		      (read-from-minibuffer
		       "Redo: " (prin1-to-string elt) read-expression-map t
		       (cons 'command-history arg))

		    ;; If command was added to command-history as a
		    ;; string, get rid of that.  We want only
		    ;; evaluable expressions there.
		    (if (stringp (car command-history))
			(setq command-history (cdr command-history))))))

	  ;; If command to be redone does not match front of history,
	  ;; add it to the history.
	  (or (equal newcmd (car command-history))
	      (setq command-history (cons newcmd command-history)))
	  (eval newcmd))
      (if command-history
	  (error "Argument %d is beyond length of command history" arg)
	(error "There are no previous complex commands to repeat")))))

(defun read-extended-command ()
  "Read command name to invoke in `execute-extended-command'."
  (minibuffer-with-setup-hook
      (lambda ()
	(set (make-local-variable 'minibuffer-default-add-function)
	     (lambda ()
	       ;; Get a command name at point in the original buffer
	       ;; to propose it after M-n.
	       (with-current-buffer (window-buffer (minibuffer-selected-window))
		 (and (commandp (function-called-at-point))
		      (format "%S" (function-called-at-point)))))))
    ;; Read a string, completing from and restricting to the set of
    ;; all defined commands.  Don't provide any initial input.
    ;; Save the command read on the extended-command history list.
    (completing-read
     (concat (cond
	      ((eq current-prefix-arg '-) "- ")
	      ((and (consp current-prefix-arg)
		    (eq (car current-prefix-arg) 4)) "C-u ")
	      ((and (consp current-prefix-arg)
		    (integerp (car current-prefix-arg)))
	       (format "%d " (car current-prefix-arg)))
	      ((integerp current-prefix-arg)
	       (format "%d " current-prefix-arg)))
	     ;; This isn't strictly correct if `execute-extended-command'
	     ;; is bound to anything else (e.g. [menu]).
	     ;; It could use (key-description (this-single-command-keys)),
	     ;; but actually a prompt other than "M-x" would be confusing,
	     ;; because "M-x" is a well-known prompt to read a command
	     ;; and it serves as a shorthand for "Extended command: ".
	     "M-x ")
     obarray 'commandp t nil 'extended-command-history)))


(defvar minibuffer-history nil
  "Default minibuffer history list.
This is used for all minibuffer input
except when an alternate history list is specified.

Maximum length of the history list is determined by the value
of `history-length', which see.")
(defvar minibuffer-history-sexp-flag nil
  "Control whether history list elements are expressions or strings.
If the value of this variable equals current minibuffer depth,
they are expressions; otherwise they are strings.
\(That convention is designed to do the right thing for
recursive uses of the minibuffer.)")
(setq minibuffer-history-variable 'minibuffer-history)
(setq minibuffer-history-position nil)  ;; Defvar is in C code.
(defvar minibuffer-history-search-history nil)

(defvar minibuffer-text-before-history nil
  "Text that was in this minibuffer before any history commands.
This is nil if there have not yet been any history commands
in this use of the minibuffer.")

(add-hook 'minibuffer-setup-hook 'minibuffer-history-initialize)

(defun minibuffer-history-initialize ()
  (setq minibuffer-text-before-history nil))

(defun minibuffer-avoid-prompt (_new _old)
  "A point-motion hook for the minibuffer, that moves point out of the prompt."
  (constrain-to-field nil (point-max)))

(defcustom minibuffer-history-case-insensitive-variables nil
  "Minibuffer history variables for which matching should ignore case.
If a history variable is a member of this list, then the
\\[previous-matching-history-element] and \\[next-matching-history-element]\
 commands ignore case when searching it, regardless of `case-fold-search'."
  :type '(repeat variable)
  :group 'minibuffer)

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match.
Normally, history elements are matched case-insensitively if
`case-fold-search' is non-nil, but an uppercase letter in REGEXP
makes the search case-sensitive.
See also `minibuffer-history-case-insensitive-variables'."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Previous element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history
					(car minibuffer-history-search-history))))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (if minibuffer-history-search-history
		   (car minibuffer-history-search-history)
		 (error "No previous history search regexp"))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (unless (zerop n)
    (if (and (zerop minibuffer-history-position)
	     (null minibuffer-text-before-history))
	(setq minibuffer-text-before-history
	      (minibuffer-contents-no-properties)))
    (let ((history (symbol-value minibuffer-history-variable))
	  (case-fold-search
	   (if (isearch-no-upper-case-p regexp t) ; assume isearch.el is dumped
	       ;; On some systems, ignore case for file names.
	       (if (memq minibuffer-history-variable
			 minibuffer-history-case-insensitive-variables)
		   t
		 ;; Respect the user's setting for case-fold-search:
		 case-fold-search)
	     nil))
	  prevpos
	  match-string
	  match-offset
	  (pos minibuffer-history-position))
      (while (/= n 0)
	(setq prevpos pos)
	(setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
	(when (= pos prevpos)
	  (error (if (= pos 1)
		     "No later matching history item"
		   "No earlier matching history item")))
	(setq match-string
	      (if (eq minibuffer-history-sexp-flag (minibuffer-depth))
		  (let ((print-level nil))
		    (prin1-to-string (nth (1- pos) history)))
		(nth (1- pos) history)))
	(setq match-offset
	      (if (< n 0)
		  (and (string-match regexp match-string)
		       (match-end 0))
		(and (string-match (concat ".*\\(" regexp "\\)") match-string)
		     (match-beginning 1))))
	(when match-offset
	  (setq n (+ n (if (< n 0) 1 -1)))))
      (setq minibuffer-history-position pos)
      (goto-char (point-max))
      (delete-minibuffer-contents)
      (insert match-string)
      (goto-char (+ (minibuffer-prompt-end) match-offset))))
  (if (memq (car (car command-history)) '(previous-matching-history-element
					  next-matching-history-element))
      (setq command-history (cdr command-history))))

(defun next-matching-history-element (regexp n)
  "Find the next history element that matches REGEXP.
\(The next history element refers to a more recent action.)
With prefix argument N, search for Nth next match.
If N is negative, find the previous or Nth previous match.
Normally, history elements are matched case-insensitively if
`case-fold-search' is non-nil, but an uppercase letter in REGEXP
makes the search case-sensitive."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Next element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history
 					(car minibuffer-history-search-history))))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (if minibuffer-history-search-history
		   (car minibuffer-history-search-history)
		 (error "No previous history search regexp"))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (previous-matching-history-element regexp (- n)))

(defvar minibuffer-temporary-goal-position nil)

(defvar minibuffer-default-add-function 'minibuffer-default-add-completions
  "Function run by `goto-history-element' before consuming default values.
This is useful to dynamically add more elements to the list of default values
when `goto-history-element' reaches the end of this list.
Before calling this function `goto-history-element' sets the variable
`minibuffer-default-add-done' to t, so it will call this function only
once.  In special cases, when this function needs to be called more
than once, it can set `minibuffer-default-add-done' to nil explicitly,
overriding the setting of this variable to t in `goto-history-element'.")

(defvar minibuffer-default-add-done nil
  "When nil, add more elements to the end of the list of default values.
The value nil causes `goto-history-element' to add more elements to
the list of defaults when it reaches the end of this list.  It does
this by calling a function defined by `minibuffer-default-add-function'.")

(make-variable-buffer-local 'minibuffer-default-add-done)

(defun minibuffer-default-add-completions ()
  "Return a list of all completions without the default value.
This function is used to add all elements of the completion table to
the end of the list of defaults just after the default value."
  (let ((def minibuffer-default)
	(all (all-completions ""
			      minibuffer-completion-table
			      minibuffer-completion-predicate)))
    (if (listp def)
	(append def all)
      (cons def (delete def all)))))

(defun goto-history-element (nabs)
  "Puts element of the minibuffer history in the minibuffer.
The argument NABS specifies the absolute history position."
  (interactive "p")
  (when (and (not minibuffer-default-add-done)
	     (functionp minibuffer-default-add-function)
	     (< nabs (- (if (listp minibuffer-default)
			    (length minibuffer-default)
			  1))))
    (setq minibuffer-default-add-done t
	  minibuffer-default (funcall minibuffer-default-add-function)))
  (let ((minimum (if minibuffer-default
		     (- (if (listp minibuffer-default)
			    (length minibuffer-default)
			  1))
		   0))
	elt minibuffer-returned-to-present)
    (if (and (zerop minibuffer-history-position)
	     (null minibuffer-text-before-history))
	(setq minibuffer-text-before-history
	      (minibuffer-contents-no-properties)))
    (if (< nabs minimum)
	(if minibuffer-default
	    (error "End of defaults; no next item")
	  (error "End of history; no default available")))
    (if (> nabs (length (symbol-value minibuffer-history-variable)))
	(error "Beginning of history; no preceding item"))
    (unless (memq last-command '(next-history-element
				 previous-history-element))
      (let ((prompt-end (minibuffer-prompt-end)))
	(set (make-local-variable 'minibuffer-temporary-goal-position)
	     (cond ((<= (point) prompt-end) prompt-end)
		   ((eobp) nil)
		   (t (point))))))
    (goto-char (point-max))
    (delete-minibuffer-contents)
    (setq minibuffer-history-position nabs)
    (cond ((< nabs 0)
	   (setq elt (if (listp minibuffer-default)
			 (nth (1- (abs nabs)) minibuffer-default)
		       minibuffer-default)))
	  ((= nabs 0)
	   (setq elt (or minibuffer-text-before-history ""))
	   (setq minibuffer-returned-to-present t)
	   (setq minibuffer-text-before-history nil))
	  (t (setq elt (nth (1- minibuffer-history-position)
			    (symbol-value minibuffer-history-variable)))))
    (insert
     (if (and (eq minibuffer-history-sexp-flag (minibuffer-depth))
	      (not minibuffer-returned-to-present))
	 (let ((print-level nil))
	   (prin1-to-string elt))
       elt))
    (goto-char (or minibuffer-temporary-goal-position (point-max)))))

(defun next-history-element (n)
  "Puts next element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth following element."
  (interactive "p")
  (or (zerop n)
      (goto-history-element (- minibuffer-history-position n))))

(defun previous-history-element (n)
  "Puts previous element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth previous element."
  (interactive "p")
  (or (zerop n)
      (goto-history-element (+ minibuffer-history-position n))))

(defun next-complete-history-element (n)
  "Get next history element which completes the minibuffer before the point.
The contents of the minibuffer after the point are deleted, and replaced
by the new completion."
  (interactive "p")
  (let ((point-at-start (point)))
    (next-matching-history-element
     (concat
      "^" (regexp-quote (buffer-substring (minibuffer-prompt-end) (point))))
     n)
    ;; next-matching-history-element always puts us at (point-min).
    ;; Move to the position we were at before changing the buffer contents.
    ;; This is still sensible, because the text before point has not changed.
    (goto-char point-at-start)))

(defun previous-complete-history-element (n)
  "\
Get previous history element which completes the minibuffer before the point.
The contents of the minibuffer after the point are deleted, and replaced
by the new completion."
  (interactive "p")
  (next-complete-history-element (- n)))

;; For compatibility with the old subr of the same name.
(defun minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt.
Return 0 if current buffer is not a minibuffer."
  ;; Return the width of everything before the field at the end of
  ;; the buffer; this should be 0 for normal buffers.
  (1- (minibuffer-prompt-end)))

;; isearch minibuffer history
(add-hook 'minibuffer-setup-hook 'minibuffer-history-isearch-setup)

(defvar minibuffer-history-isearch-message-overlay)
(make-variable-buffer-local 'minibuffer-history-isearch-message-overlay)

(defun minibuffer-history-isearch-setup ()
  "Set up a minibuffer for using isearch to search the minibuffer history.
Intended to be added to `minibuffer-setup-hook'."
  (set (make-local-variable 'isearch-search-fun-function)
       'minibuffer-history-isearch-search)
  (set (make-local-variable 'isearch-message-function)
       'minibuffer-history-isearch-message)
  (set (make-local-variable 'isearch-wrap-function)
       'minibuffer-history-isearch-wrap)
  (set (make-local-variable 'isearch-push-state-function)
       'minibuffer-history-isearch-push-state)
  (add-hook 'isearch-mode-end-hook 'minibuffer-history-isearch-end nil t))

(defun minibuffer-history-isearch-end ()
  "Clean up the minibuffer after terminating isearch in the minibuffer."
  (if minibuffer-history-isearch-message-overlay
      (delete-overlay minibuffer-history-isearch-message-overlay)))

(defun minibuffer-history-isearch-search ()
  "Return the proper search function, for isearch in minibuffer history."
  (cond
   (isearch-word
    (if isearch-forward 'word-search-forward 'word-search-backward))
   (t
    (lambda (string bound noerror)
      (let ((search-fun
	     ;; Use standard functions to search within minibuffer text
             (cond
              (isearch-regexp
               (if isearch-forward 're-search-forward 're-search-backward))
              (t
               (if isearch-forward 'search-forward 'search-backward))))
	    found)
	;; Avoid lazy-highlighting matches in the minibuffer prompt when
	;; searching forward.  Lazy-highlight calls this lambda with the
	;; bound arg, so skip the minibuffer prompt.
	(if (and bound isearch-forward (< (point) (minibuffer-prompt-end)))
	    (goto-char (minibuffer-prompt-end)))
        (or
	 ;; 1. First try searching in the initial minibuffer text
	 (funcall search-fun string
		  (if isearch-forward bound (minibuffer-prompt-end))
		  noerror)
	 ;; 2. If the above search fails, start putting next/prev history
	 ;; elements in the minibuffer successively, and search the string
	 ;; in them.  Do this only when bound is nil (i.e. not while
	 ;; lazy-highlighting search strings in the current minibuffer text).
	 (unless bound
	   (condition-case nil
	       (progn
		 (while (not found)
		   (cond (isearch-forward
			  (next-history-element 1)
			  (goto-char (minibuffer-prompt-end)))
			 (t
			  (previous-history-element 1)
			  (goto-char (point-max))))
		   (setq isearch-barrier (point) isearch-opoint (point))
		   ;; After putting the next/prev history element, search
		   ;; the string in them again, until next-history-element
		   ;; or previous-history-element raises an error at the
		   ;; beginning/end of history.
		   (setq found (funcall search-fun string
					(unless isearch-forward
					  ;; For backward search, don't search
					  ;; in the minibuffer prompt
					  (minibuffer-prompt-end))
					noerror)))
		 ;; Return point of the new search result
		 (point))
	     ;; Return nil when next(prev)-history-element fails
	     (error nil)))))))))

(defun minibuffer-history-isearch-message (&optional c-q-hack ellipsis)
  "Display the minibuffer history search prompt.
If there are no search errors, this function displays an overlay with
the isearch prompt which replaces the original minibuffer prompt.
Otherwise, it displays the standard isearch message returned from
`isearch-message'."
  (if (not (and (minibufferp) isearch-success (not isearch-error)))
      ;; Use standard function `isearch-message' when not in the minibuffer,
      ;; or search fails, or has an error (like incomplete regexp).
      ;; This function overwrites minibuffer text with isearch message,
      ;; so it's possible to see what is wrong in the search string.
      (isearch-message c-q-hack ellipsis)
    ;; Otherwise, put the overlay with the standard isearch prompt over
    ;; the initial minibuffer prompt.
    (if (overlayp minibuffer-history-isearch-message-overlay)
	(move-overlay minibuffer-history-isearch-message-overlay
		      (point-min) (minibuffer-prompt-end))
      (setq minibuffer-history-isearch-message-overlay
	    (make-overlay (point-min) (minibuffer-prompt-end)))
      (overlay-put minibuffer-history-isearch-message-overlay 'evaporate t))
    (overlay-put minibuffer-history-isearch-message-overlay
		 'display (isearch-message-prefix c-q-hack ellipsis))
    ;; And clear any previous isearch message.
    (message "")))

(defun minibuffer-history-isearch-wrap ()
  "Wrap the minibuffer history search when search fails.
Move point to the first history element for a forward search,
or to the last history element for a backward search."
  (unless isearch-word
    ;; When `minibuffer-history-isearch-search' fails on reaching the
    ;; beginning/end of the history, wrap the search to the first/last
    ;; minibuffer history element.
    (if isearch-forward
	(goto-history-element (length (symbol-value minibuffer-history-variable)))
      (goto-history-element 0))
    (setq isearch-success t))
  (goto-char (if isearch-forward (minibuffer-prompt-end) (point-max))))

(defun minibuffer-history-isearch-push-state ()
  "Save a function restoring the state of minibuffer history search.
Save `minibuffer-history-position' to the additional state parameter
in the search status stack."
  `(lambda (cmd)
     (minibuffer-history-isearch-pop-state cmd ,minibuffer-history-position)))

(defun minibuffer-history-isearch-pop-state (_cmd hist-pos)
  "Restore the minibuffer history search state.
Go to the history element by the absolute history position HIST-POS."
  (goto-history-element hist-pos))


;Put this on C-x u, so we can force that rather than C-_ into startup msg
(define-obsolete-function-alias 'advertised-undo 'undo "23.2")

(defconst undo-equiv-table (make-hash-table :test 'eq :weakness t)
  "Table mapping redo records to the corresponding undo one.
A redo record for undo-in-region maps to t.
A redo record for ordinary undo maps to the following (earlier) undo.")

(defvar undo-in-region nil
  "Non-nil if `pending-undo-list' is not just a tail of `buffer-undo-list'.")

(defvar undo-no-redo nil
  "If t, `undo' doesn't go through redo entries.")

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.
If t, we undid all the way to the end of it.")

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes within
the current region.  Similarly, when not in Transient Mark mode, just \\[universal-argument]
as an argument limits undo to changes within the current region."
  (interactive "*P")
  ;; Make last-command indicate for the next command that this was an undo.
  ;; That way, another undo will undo more.
  ;; If we get to the end of the undo history and get an error,
  ;; another undo command will find the undo history empty
  ;; and will get another error.  To begin undoing the undos,
  ;; you must type some other command.
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p))
	message)
    ;; If we get an error in undo-start,
    ;; the next command should not be a "consecutive undo".
    ;; So set `this-command' to something other than `undo'.
    (setq this-command 'undo-start)

    (unless (and (eq last-command 'undo)
		 (or (eq pending-undo-list t)
		     ;; If something (a timer or filter?) changed the buffer
		     ;; since the previous command, don't continue the undo seq.
		     (let ((list buffer-undo-list))
		       (while (eq (car list) nil)
			 (setq list (cdr list)))
		       ;; If the last undo record made was made by undo
		       ;; it shows nothing else happened in between.
		       (gethash list undo-equiv-table))))
      (setq undo-in-region
	    (or (region-active-p) (and arg (not (numberp arg)))))
      (if undo-in-region
	  (undo-start (region-beginning) (region-end))
	(undo-start))
      ;; get rid of initial undo boundary
      (undo-more 1))
    ;; If we got this far, the next command should be a consecutive undo.
    (setq this-command 'undo)
    ;; Check to see whether we're hitting a redo record, and if
    ;; so, ask the user whether she wants to skip the redo/undo pair.
    (let ((equiv (gethash pending-undo-list undo-equiv-table)))
      (or (eq (selected-window) (minibuffer-window))
	  (setq message (if undo-in-region
			    (if equiv "Redo in region!" "Undo in region!")
			  (if equiv "Redo!" "Undo!"))))
      (when (and (consp equiv) undo-no-redo)
	;; The equiv entry might point to another redo record if we have done
	;; undo-redo-undo-redo-... so skip to the very last equiv.
	(while (let ((next (gethash equiv undo-equiv-table)))
		 (if next (setq equiv next))))
	(setq pending-undo-list equiv)))
    (undo-more
     (if (numberp arg)
	 (prefix-numeric-value arg)
       1))
    ;; Record the fact that the just-generated undo records come from an
    ;; undo operation--that is, they are redo records.
    ;; In the ordinary case (not within a region), map the redo
    ;; record to the following undos.
    ;; I don't know how to do that in the undo-in-region case.
    (let ((list buffer-undo-list))
      ;; Strip any leading undo boundaries there might be, like we do
      ;; above when checking.
      (while (eq (car list) nil)
	(setq list (cdr list)))
      (puthash list (if undo-in-region t pending-undo-list)
	       undo-equiv-table))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
	  (prev nil))
      (while (car tail)
	(when (integerp (car tail))
	  (let ((pos (car tail)))
	    (if prev
		(setcdr prev (cdr tail))
	      (setq buffer-undo-list (cdr tail)))
	    (setq tail (cdr tail))
	    (while (car tail)
	      (if (eq pos (car tail))
		  (if prev
		      (setcdr prev (cdr tail))
		    (setq buffer-undo-list (cdr tail)))
		(setq prev tail))
	      (setq tail (cdr tail)))
	    (setq tail nil)))
	(setq prev tail tail (cdr tail))))
    ;; Record what the current undo list says,
    ;; so the next command can tell if the buffer was modified in between.
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))
    ;; Display a message announcing success.
    (if message
	(message "%s" message))))

(defun buffer-disable-undo (&optional buffer)
  "Make BUFFER stop keeping undo information.
No argument or nil as argument means do this for the current buffer."
  (interactive)
  (with-current-buffer (if buffer (get-buffer buffer) (current-buffer))
    (setq buffer-undo-list t)))

(defun undo-only (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.
Contrary to `undo', this will not redo a previous undo."
  (interactive "*p")
  (let ((undo-no-redo t)) (undo arg)))

(defvar undo-in-progress nil
  "Non-nil while performing an undo.
Some change-hooks test this variable to do something different.")

(defun undo-more (n)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or (listp pending-undo-list)
      (error (concat "No further undo information"
		     (and undo-in-region " for region"))))
  (let ((undo-in-progress t))
    ;; Note: The following, while pulling elements off
    ;; `pending-undo-list' will call primitive change functions which
    ;; will push more elements onto `buffer-undo-list'.
    (setq pending-undo-list (primitive-undo n pending-undo-list))
    (if (null pending-undo-list)
	(setq pending-undo-list t))))

;; Deep copy of a list
(defun undo-copy-list (list)
  "Make a copy of undo list LIST."
  (mapcar 'undo-copy-list-1 list))

(defun undo-copy-list-1 (elt)
  (if (consp elt)
      (cons (car elt) (undo-copy-list-1 (cdr elt)))
    elt))

(defun undo-start (&optional beg end)
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change.
If BEG and END are specified, then only undo elements
that apply to text between BEG and END are used; other undo elements
are ignored.  If BEG and END are nil, all undo elements are used."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list
	(if (and beg end (not (= beg end)))
	    (undo-make-selective-list (min beg end) (max beg end))
	  buffer-undo-list)))

(defvar undo-adjusted-markers)

(defun undo-make-selective-list (start end)
  "Return a list of undo elements for the region START to END.
The elements come from `buffer-undo-list', but we keep only
the elements inside this region, and discard those outside this region.
If we find an element that crosses an edge of this region,
we stop and ignore all further elements."
  (let ((undo-list-copy (undo-copy-list buffer-undo-list))
	(undo-list (list nil))
	undo-adjusted-markers
	some-rejected
	undo-elt temp-undo-list delta)
    (while undo-list-copy
      (setq undo-elt (car undo-list-copy))
      (let ((keep-this
	     (cond ((and (consp undo-elt) (eq (car undo-elt) t))
		    ;; This is a "was unmodified" element.
		    ;; Keep it if we have kept everything thus far.
		    (not some-rejected))
		   (t
		    (undo-elt-in-region undo-elt start end)))))
	(if keep-this
	    (progn
	      (setq end (+ end (cdr (undo-delta undo-elt))))
	      ;; Don't put two nils together in the list
	      (if (not (and (eq (car undo-list) nil)
			    (eq undo-elt nil)))
		  (setq undo-list (cons undo-elt undo-list))))
	  (if (undo-elt-crosses-region undo-elt start end)
	      (setq undo-list-copy nil)
	    (setq some-rejected t)
	    (setq temp-undo-list (cdr undo-list-copy))
	    (setq delta (undo-delta undo-elt))

	    (when (/= (cdr delta) 0)
	      (let ((position (car delta))
		    (offset (cdr delta)))

		;; Loop down the earlier events adjusting their buffer
		;; positions to reflect the fact that a change to the buffer
		;; isn't being undone. We only need to process those element
		;; types which undo-elt-in-region will return as being in
		;; the region since only those types can ever get into the
		;; output

		(while temp-undo-list
		  (setq undo-elt (car temp-undo-list))
		  (cond ((integerp undo-elt)
			 (if (>= undo-elt position)
			     (setcar temp-undo-list (- undo-elt offset))))
			((atom undo-elt) nil)
			((stringp (car undo-elt))
			 ;; (TEXT . POSITION)
			 (let ((text-pos (abs (cdr undo-elt)))
			       (point-at-end (< (cdr undo-elt) 0 )))
			   (if (>= text-pos position)
			       (setcdr undo-elt (* (if point-at-end -1 1)
						   (- text-pos offset))))))
			((integerp (car undo-elt))
			 ;; (BEGIN . END)
			 (when (>= (car undo-elt) position)
			   (setcar undo-elt (- (car undo-elt) offset))
			   (setcdr undo-elt (- (cdr undo-elt) offset))))
			((null (car undo-elt))
			 ;; (nil PROPERTY VALUE BEG . END)
			 (let ((tail (nthcdr 3 undo-elt)))
			   (when (>= (car tail) position)
			     (setcar tail (- (car tail) offset))
			     (setcdr tail (- (cdr tail) offset))))))
		  (setq temp-undo-list (cdr temp-undo-list))))))))
      (setq undo-list-copy (cdr undo-list-copy)))
    (nreverse undo-list)))

(defun undo-elt-in-region (undo-elt start end)
  "Determine whether UNDO-ELT falls inside the region START ... END.
If it crosses the edge, we return nil."
  (cond ((integerp undo-elt)
	 (and (>= undo-elt start)
	      (<= undo-elt end)))
	((eq undo-elt nil)
	 t)
	((atom undo-elt)
	 nil)
	((stringp (car undo-elt))
	 ;; (TEXT . POSITION)
	 (and (>= (abs (cdr undo-elt)) start)
	      (< (abs (cdr undo-elt)) end)))
	((and (consp undo-elt) (markerp (car undo-elt)))
	 ;; This is a marker-adjustment element (MARKER . ADJUSTMENT).
	 ;; See if MARKER is inside the region.
	 (let ((alist-elt (assq (car undo-elt) undo-adjusted-markers)))
	   (unless alist-elt
	     (setq alist-elt (cons (car undo-elt)
				   (marker-position (car undo-elt))))
	     (setq undo-adjusted-markers
		   (cons alist-elt undo-adjusted-markers)))
	   (and (cdr alist-elt)
		(>= (cdr alist-elt) start)
		(<= (cdr alist-elt) end))))
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (and (>= (car tail) start)
		(<= (cdr tail) end))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (and (>= (car undo-elt) start)
	      (<= (cdr undo-elt) end)))))

(defun undo-elt-crosses-region (undo-elt start end)
  "Test whether UNDO-ELT crosses one edge of that region START ... END.
This assumes we have already decided that UNDO-ELT
is not *inside* the region START...END."
  (cond ((atom undo-elt) nil)
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (and (< (car tail) end)
		(> (cdr tail) start))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (and (< (car undo-elt) end)
	      (> (cdr undo-elt) start)))))

;; Return the first affected buffer position and the delta for an undo element
;; delta is defined as the change in subsequent buffer positions if we *did*
;; the undo.
(defun undo-delta (undo-elt)
  (if (consp undo-elt)
      (cond ((stringp (car undo-elt))
	     ;; (TEXT . POSITION)
	     (cons (abs (cdr undo-elt)) (length (car undo-elt))))
	    ((integerp (car undo-elt))
	     ;; (BEGIN . END)
	     (cons (car undo-elt) (- (car undo-elt) (cdr undo-elt))))
	    (t
	     '(0 . 0)))
    '(0 . 0)))

(defcustom undo-ask-before-discard nil
  "If non-nil ask about discarding undo info for the current command.
Normally, Emacs discards the undo info for the current command if
it exceeds `undo-outer-limit'.  But if you set this option
non-nil, it asks in the echo area whether to discard the info.
If you answer no, there is a slight risk that Emacs might crash, so
only do it if you really want to undo the command.

This option is mainly intended for debugging.  You have to be
careful if you use it for other purposes.  Garbage collection is
inhibited while the question is asked, meaning that Emacs might
leak memory.  So you should make sure that you do not wait
excessively long before answering the question."
  :type 'boolean
  :group 'undo
  :version "22.1")

(defvar undo-extra-outer-limit nil
  "If non-nil, an extra level of size that's ok in an undo item.
We don't ask the user about truncating the undo list until the
current item gets bigger than this amount.

This variable only matters if `undo-ask-before-discard' is non-nil.")
(make-variable-buffer-local 'undo-extra-outer-limit)

;; When the first undo batch in an undo list is longer than
;; undo-outer-limit, this function gets called to warn the user that
;; the undo info for the current command was discarded.  Garbage
;; collection is inhibited around the call, so it had better not do a
;; lot of consing.
(setq undo-outer-limit-function 'undo-outer-limit-truncate)
(defun undo-outer-limit-truncate (size)
  (if undo-ask-before-discard
      (when (or (null undo-extra-outer-limit)
		(> size undo-extra-outer-limit))
	;; Don't ask the question again unless it gets even bigger.
	;; This applies, in particular, if the user quits from the question.
	;; Such a quit quits out of GC, but something else will call GC
	;; again momentarily.  It will call this function again,
	;; but we don't want to ask the question again.
	(setq undo-extra-outer-limit (+ size 50000))
	(if (let (use-dialog-box track-mouse executing-kbd-macro )
	      (yes-or-no-p (format "Buffer `%s' undo info is %d bytes long; discard it? "
				   (buffer-name) size)))
	    (progn (setq buffer-undo-list nil)
		   (setq undo-extra-outer-limit nil)
		   t)
	  nil))
    (display-warning '(undo discard-info)
		     (concat
		      (format "Buffer `%s' undo info was %d bytes long.\n"
			      (buffer-name) size)
		      "The undo info was discarded because it exceeded \
`undo-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer.  In that case, to prevent similar problems in the
future, set `undo-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(undo discard-info) to the user option `warning-suppress-types',
which is defined in the `warnings' library.\n")
		     :warning)
    (setq buffer-undo-list nil)
    t))

(defvar shell-command-history nil
  "History list for some commands that read shell commands.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar shell-command-switch (purecopy "-c")
  "Switch used to have the shell execute its command line argument.")

(defvar shell-command-default-error-buffer nil
  "*Buffer name for `shell-command' and `shell-command-on-region' error output.
This buffer is used when `shell-command' or `shell-command-on-region'
is run interactively.  A value of nil means that output to stderr and
stdout will be intermixed in the output stream.")

(declare-function mailcap-file-default-commands "mailcap" (files))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defun minibuffer-default-add-shell-commands ()
  "Return a list of all commands associated with the current file.
This function is used to add all related commands retrieved by `mailcap'
to the end of the list of defaults just after the default value."
  (interactive)
  (let* ((filename (if (listp minibuffer-default)
		       (car minibuffer-default)
		     minibuffer-default))
	 (commands (and filename (require 'mailcap nil t)
			(mailcap-file-default-commands (list filename)))))
    (setq commands (mapcar (lambda (command)
			     (concat command " " filename))
			   commands))
    (if (listp minibuffer-default)
	(append minibuffer-default commands)
      (cons minibuffer-default commands))))

(declare-function shell-completion-vars "shell" ())

(defvar minibuffer-local-shell-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap used for completing shell commands in minibuffer.")

(defun read-shell-command (prompt &optional initial-contents hist &rest args)
  "Read a shell command from the minibuffer.
The arguments are the same as the ones of `read-from-minibuffer',
except READ and KEYMAP are missing and HIST defaults
to `shell-command-history'."
  (require 'shell)
  (minibuffer-with-setup-hook
      (lambda ()
        (shell-completion-vars)
	(set (make-local-variable 'minibuffer-default-add-function)
	     'minibuffer-default-add-shell-commands))
    (apply 'read-from-minibuffer prompt initial-contents
	   minibuffer-local-shell-command-map
	   nil
	   (or hist 'shell-command-history)
	   args)))

(defun async-shell-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND asynchronously in background.

Like `shell-command' but if COMMAND doesn't end in ampersand, adds `&'
surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'.

In Elisp, you will often be better served by calling `start-process'
directly, since it offers more control and does not impose the use of a
shell (with its need to quote arguments)."
  (interactive
   (list
    (read-shell-command "Async shell command: " nil nil
			(and buffer-file-name
			     (file-relative-name buffer-file-name)))
    current-prefix-arg
    shell-command-default-error-buffer))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (shell-command command output-buffer error-buffer))

(defun shell-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.
With prefix argument, insert the COMMAND's output at point.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

Otherwise, COMMAND is executed synchronously.  The output appears in
the buffer `*Shell Command Output*'.  If the output is short enough to
display in the echo area (which is determined by the variables
`resize-mini-windows' and `max-mini-window-height'), it is shown
there, but it is nonetheless available in buffer `*Shell Command
Output*' even though that buffer is not automatically displayed.

To specify a coding system for converting non-ASCII characters
in the shell command output, use \\[universal-coding-system-argument] \
before this command.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the buffer is first erased, and the output is
inserted after point (leaving mark after it).

If the command terminates without error, but generates output,
and you did not specify \"insert it in the current buffer\",
the output can be displayed in the echo area or in its buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise,the buffer containing the output is displayed.

If there is output and an error, and you did not specify \"insert it
in the current buffer\", a message about the error goes at the end
of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional third argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER.

In Elisp, you will often be better served by calling `call-process' or
`start-process' directly, since it offers more control and does not impose
the use of a shell (with its need to quote arguments)."

  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  ;; Look for a handler in case default-directory is a remote file name.
  (let ((handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler
	(funcall handler 'shell-command command output-buffer error-buffer)
      (if (and output-buffer
	       (not (or (bufferp output-buffer)  (stringp output-buffer))))
	  ;; Output goes in current buffer.
	  (let ((error-file
		 (if error-buffer
		     (make-temp-file
		      (expand-file-name "scor"
					(or small-temporary-file-directory
					    temporary-file-directory)))
		   nil)))
	    (barf-if-buffer-read-only)
	    (push-mark nil t)
	    ;; We do not use -f for csh; we will not support broken use of
	    ;; .cshrcs.  Even the BSD csh manual says to use
	    ;; "if ($?prompt) exit" before things which are not useful
	    ;; non-interactively.  Besides, if someone wants their other
	    ;; aliases for shell commands then they can still have them.
	    (call-process shell-file-name nil
			  (if error-file
			      (list t error-file)
			    t)
			  nil shell-command-switch command)
	    (when (and error-file (file-exists-p error-file))
	      (if (< 0 (nth 7 (file-attributes error-file)))
		  (with-current-buffer (get-buffer-create error-buffer)
		    (let ((pos-from-end (- (point-max) (point))))
		      (or (bobp)
			  (insert "\f\n"))
		      ;; Do no formatting while reading error file,
		      ;; because that can run a shell command, and we
		      ;; don't want that to cause an infinite recursion.
		      (format-insert-file error-file nil)
		      ;; Put point after the inserted errors.
		      (goto-char (- (point-max) pos-from-end)))
		    (display-buffer (current-buffer))))
	      (delete-file error-file))
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (goto-char (prog1 (mark t)
			 (set-marker (mark-marker) (point)
				     (current-buffer)))))
	;; Output goes in a separate buffer.
	;; Preserve the match data in case called from a program.
	(save-match-data
	  (if (string-match "[ \t]*&[ \t]*\\'" command)
	      ;; Command ending with ampersand means asynchronous.
	      (let ((buffer (get-buffer-create
			     (or output-buffer "*Async Shell Command*")))
		    (directory default-directory)
		    proc)
		;; Remove the ampersand.
		(setq command (substring command 0 (match-beginning 0)))
		;; If will kill a process, query first.
		(setq proc (get-buffer-process buffer))
		(if proc
		    (if (yes-or-no-p "A command is running.  Kill it? ")
			(kill-process proc)
		      (error "Shell command in progress")))
		(with-current-buffer buffer
		  (setq buffer-read-only nil)
		  ;; Setting buffer-read-only to nil doesn't suffice
		  ;; if some text has a non-nil read-only property,
		  ;; which comint sometimes adds for prompts.
		  (let ((inhibit-read-only t))
		    (erase-buffer))
		  (display-buffer buffer)
		  (setq default-directory directory)
		  (setq proc (start-process "Shell" buffer shell-file-name
					    shell-command-switch command))
		  (setq mode-line-process '(":%s"))
		  (require 'shell) (shell-mode)
		  (set-process-sentinel proc 'shell-command-sentinel)
		  ;; Use the comint filter for proper handling of carriage motion
		  ;; (see `comint-inhibit-carriage-motion'),.
		  (set-process-filter proc 'comint-output-filter)
		  ))
	    ;; Otherwise, command is executed synchronously.
	    (shell-command-on-region (point) (point) command
				     output-buffer nil error-buffer)))))))

(defun display-message-or-buffer (message
				  &optional buffer-name not-this-window frame)
  "Display MESSAGE in the echo area if possible, otherwise in a pop-up buffer.
MESSAGE may be either a string or a buffer.

A buffer is displayed using `display-buffer' if MESSAGE is too long for
the maximum height of the echo area, as defined by `max-mini-window-height'
if `resize-mini-windows' is non-nil.

Returns either the string shown in the echo area, or when a pop-up
buffer is used, the window used to display it.

If MESSAGE is a string, then the optional argument BUFFER-NAME is the
name of the buffer used to display it in the case where a pop-up buffer
is used, defaulting to `*Message*'.  In the case where MESSAGE is a
string and it is displayed in the echo area, it is not specified whether
the contents are inserted into the buffer anyway.

Optional arguments NOT-THIS-WINDOW and FRAME are as for `display-buffer',
and only used if a buffer is displayed."
  (cond ((and (stringp message) (not (string-match "\n" message)))
	 ;; Trivial case where we can use the echo area
	 (message "%s" message))
	((and (stringp message)
	      (= (string-match "\n" message) (1- (length message))))
	 ;; Trivial case where we can just remove single trailing newline
	 (message "%s" (substring message 0 (1- (length message)))))
	(t
	 ;; General case
	 (with-current-buffer
	     (if (bufferp message)
		 message
	       (get-buffer-create (or buffer-name "*Message*")))

	   (unless (bufferp message)
	     (erase-buffer)
	     (insert message))

	   (let ((lines
		  (if (= (buffer-size) 0)
		      0
		    (count-screen-lines nil nil nil (minibuffer-window)))))
	     (cond ((= lines 0))
		   ((and (or (<= lines 1)
			     (<= lines
				 (if resize-mini-windows
				     (cond ((floatp max-mini-window-height)
					    (* (frame-height)
					       max-mini-window-height))
					   ((integerp max-mini-window-height)
					    max-mini-window-height)
					   (t
					    1))
				   1)))
			 ;; Don't use the echo area if the output buffer is
			 ;; already displayed in the selected frame.
			 (not (get-buffer-window (current-buffer))))
		    ;; Echo area
		    (goto-char (point-max))
		    (when (bolp)
		      (backward-char 1))
		    (message "%s" (buffer-substring (point-min) (point))))
		   (t
		    ;; Buffer
		    (goto-char (point-min))
		    (display-buffer (current-buffer)
				    not-this-window frame))))))))


;; We have a sentinel to prevent insertion of a termination message
;; in the buffer itself.
(defun shell-command-sentinel (process signal)
  (if (memq (process-status process) '(exit signal))
      (message "%s: %s."
	       (car (cdr (cdr (process-command process))))
	       (substring signal 0 -1))))

(defun shell-command-on-region (start end command
				      &optional output-buffer replace
				      error-buffer display-error-buffer)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded using coding-system specified by `process-coding-system-alist',
falling back to `default-process-coding-system' if no match for COMMAND
is found in `process-coding-system-alist'.

The noninteractive arguments are START, END, COMMAND,
OUTPUT-BUFFER, REPLACE, ERROR-BUFFER, and DISPLAY-ERROR-BUFFER.
Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise
it is displayed in the buffer `*Shell Command Output*'.  The output
is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it).

If REPLACE, the optional fifth argument, is non-nil, that means insert
the output in place of text from START to END, putting point and mark
around it.

If optional sixth argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
If DISPLAY-ERROR-BUFFER is non-nil, display the error buffer if there
were any errors.  (This is always t, interactively.)
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
  (interactive (let (string)
		 (unless (mark)
		   (error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t)))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    (if (or replace
	    (and output-buffer
		 (not (or (bufferp output-buffer) (stringp output-buffer)))))
	;; Replace specified region with output from command.
	(let ((swap (and replace (< start end))))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (goto-char start)
	  (and replace (push-mark (point) 'nomsg))
	  (setq exit-status
		(call-process-region start end shell-file-name t
				     (if error-file
					 (list t error-file)
				       t)
				     nil shell-command-switch command))
	  ;; It is rude to delete a buffer which the command is not using.
	  ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
	  ;; 	 (kill-buffer shell-buffer)))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
		     (or output-buffer "*Shell Command Output*"))))
	(unwind-protect
	    (if (eq buffer (current-buffer))
		;; If the input is the same buffer as the output,
		;; delete everything but the specified region,
		;; then replace that region with the output.
		(progn (setq buffer-read-only nil)
		       (delete-region (max start end) (point-max))
		       (delete-region (point-min) (min start end))
		       (setq exit-status
			     (call-process-region (point-min) (point-max)
						  shell-file-name t
						  (if error-file
						      (list t error-file)
						    t)
						  nil shell-command-switch
						  command)))
	      ;; Clear the output buffer, then run the command with
	      ;; output there.
	      (let ((directory default-directory))
		(with-current-buffer buffer
		  (setq buffer-read-only nil)
		  (if (not output-buffer)
		      (setq default-directory directory))
		  (erase-buffer)))
	      (setq exit-status
		    (call-process-region start end shell-file-name nil
					 (if error-file
					     (list buffer error-file)
					   buffer)
					 nil shell-command-switch command)))
	  ;; Report the output.
	  (with-current-buffer buffer
	    (setq mode-line-process
		  (cond ((null exit-status)
			 " - Error")
			((stringp exit-status)
			 (format " - Signal [%s]" exit-status))
			((not (equal 0 exit-status))
			 (format " - Exit [%d]" exit-status)))))
	  (if (with-current-buffer buffer (> (point-max) (point-min)))
	      ;; There's some output, display it
	      (display-message-or-buffer buffer)
	    ;; No output; error?
	    (let ((output
		   (if (and error-file
			    (< 0 (nth 7 (file-attributes error-file))))
		       (format "some error output%s"
			       (if shell-command-default-error-buffer
				   (format " to the \"%s\" buffer"
					   shell-command-default-error-buffer)
				 ""))
		     "no output")))
	      (cond ((null exit-status)
		     (message "(Shell command failed with error)"))
		    ((equal 0 exit-status)
		     (message "(Shell command succeeded with %s)"
			      output))
		    ((stringp exit-status)
		     (message "(Shell command killed by signal %s)"
			      exit-status))
		    (t
		     (message "(Shell command failed with code %d and %s)"
			      exit-status output))))
	    ;; Don't kill: there might be useful info in the undo-log.
	    ;; (kill-buffer buffer)
	    ))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (and display-error-buffer
		 (display-buffer (current-buffer)))))
      (delete-file error-file))
    exit-status))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer
      standard-output
      (process-file shell-file-name nil t nil shell-command-switch command))))

(defun process-file (program &optional infile buffer display &rest args)
  "Process files synchronously in a separate process.
Similar to `call-process', but may invoke a file handler based on
`default-directory'.  The current working directory of the
subprocess is `default-directory'.

File names in INFILE and BUFFER are handled normally, but file
names in ARGS should be relative to `default-directory', as they
are passed to the process verbatim.  \(This is a difference to
`call-process' which does not support file handlers for INFILE
and BUFFER.\)

Some file handlers might not support all variants, for example
they might behave as if DISPLAY was nil, regardless of the actual
value passed."
  (let ((fh (find-file-name-handler default-directory 'process-file))
        lc stderr-file)
    (unwind-protect
        (if fh (apply fh 'process-file program infile buffer display args)
          (when infile (setq lc (file-local-copy infile)))
          (setq stderr-file (when (and (consp buffer) (stringp (cadr buffer)))
                              (make-temp-file "emacs")))
          (prog1
              (apply 'call-process program
                     (or lc infile)
                     (if stderr-file (list (car buffer) stderr-file) buffer)
                     display args)
            (when stderr-file (copy-file stderr-file (cadr buffer)))))
      (when stderr-file (delete-file stderr-file))
      (when lc (delete-file lc)))))

(defvar process-file-side-effects t
  "Whether a call of `process-file' changes remote files.

By default, this variable is always set to `t', meaning that a
call of `process-file' could potentially change any file on a
remote host.  When set to `nil', a file handler could optimize
its behavior with respect to remote file attribute caching.

You should only ever change this variable with a let-binding;
never with `setq'.")

(defun start-file-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.

Similar to `start-process', but may invoke a file handler based on
`default-directory'.  See Info node `(elisp)Magic File Names'.

This handler ought to run PROGRAM, perhaps on the local host,
perhaps on a remote host that corresponds to `default-directory'.
In the latter case, the local part of `default-directory' becomes
the working directory of the process.

PROGRAM and PROGRAM-ARGS might be file names.  They are not
objects of file handler invocation.  File handlers might not
support pty association, if PROGRAM is nil."
  (let ((fh (find-file-name-handler default-directory 'start-file-process)))
    (if fh (apply fh 'start-file-process name buffer program program-args)
      (apply 'start-process name buffer program program-args))))

;;;; Process menu

(defvar tabulated-list-format)
(defvar tabulated-list-entries)
(defvar tabulated-list-sort-key)
(declare-function tabulated-list-init-header  "tabulated-list" ())
(declare-function tabulated-list-print "tabulated-list"
                  (&optional remember-pos))

(defvar process-menu-query-only nil)

(define-derived-mode process-menu-mode tabulated-list-mode "Process Menu"
  "Major mode for listing the processes called by Emacs."
  (setq tabulated-list-format [("Process" 15 t)
			       ("Status"   7 t)
			       ("Buffer"  15 t)
			       ("TTY"     12 t)
			       ("Command"  0 t)])
  (make-local-variable 'process-menu-query-only)
  (setq tabulated-list-sort-key (cons "Process" nil))
  (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t)
  (tabulated-list-init-header))

(defun list-processes--refresh ()
  "Recompute the list of processes for the Process List buffer.
Also, delete any process that is exited or signaled."
  (setq tabulated-list-entries nil)
  (dolist (p (process-list))
    (cond ((memq (process-status p) '(exit signal closed))
	   (delete-process p))
	  ((or (not process-menu-query-only)
	       (process-query-on-exit-flag p))
	   (let* ((buf (process-buffer p))
		  (type (process-type p))
		  (name (process-name p))
		  (status (symbol-name (process-status p)))
		  (buf-label (if (buffer-live-p buf)
				 `(,(buffer-name buf)
				   face link
				   help-echo ,(concat "Visit buffer `"
						      (buffer-name buf) "'")
				   follow-link t
				   process-buffer ,buf
				   action process-menu-visit-buffer)
			       "--"))
		  (tty (or (process-tty-name p) "--"))
		  (cmd
		   (if (memq type '(network serial))
		       (let ((contact (process-contact p t)))
			 (if (eq type 'network)
			     (format "(%s %s)"
				     (if (plist-get contact :type)
					 "datagram"
				       "network")
				     (if (plist-get contact :server)
					 (format "server on %s"
						 (plist-get contact :server))
				       (format "connection to %s"
					       (plist-get contact :host))))
			   (format "(serial port %s%s)"
				   (or (plist-get contact :port) "?")
				   (let ((speed (plist-get contact :speed)))
				     (if speed
					 (format " at %s b/s" speed)
				       "")))))
		     (mapconcat 'identity (process-command p) " "))))
	     (push (list p (vector name status buf-label tty cmd))
		   tabulated-list-entries))))))

(defun process-menu-visit-buffer (button)
  (display-buffer (button-get button 'process-buffer)))

(defun list-processes (&optional query-only buffer)
  "Display a list of all processes.
If optional argument QUERY-ONLY is non-nil, only processes with
the query-on-exit flag set are listed.
Any process listed as exited or signaled is actually eliminated
after the listing is made.
Optional argument BUFFER specifies a buffer to use, instead of
\"*Process List\".
The return value is always nil."
  (interactive)
  (or (fboundp 'process-list)
      (error "Asynchronous subprocesses are not supported on this system"))
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Process List*")))
  (with-current-buffer buffer
    (process-menu-mode)
    (setq process-menu-query-only query-only)
    (list-processes--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(defvar universal-argument-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'universal-argument-other-key)
    (define-key map (vector meta-prefix-char t) 'universal-argument-other-key)
    (define-key map [switch-frame] nil)
    (define-key map [?\C-u] 'universal-argument-more)
    (define-key map [?-] 'universal-argument-minus)
    (define-key map [?0] 'digit-argument)
    (define-key map [?1] 'digit-argument)
    (define-key map [?2] 'digit-argument)
    (define-key map [?3] 'digit-argument)
    (define-key map [?4] 'digit-argument)
    (define-key map [?5] 'digit-argument)
    (define-key map [?6] 'digit-argument)
    (define-key map [?7] 'digit-argument)
    (define-key map [?8] 'digit-argument)
    (define-key map [?9] 'digit-argument)
    (define-key map [kp-0] 'digit-argument)
    (define-key map [kp-1] 'digit-argument)
    (define-key map [kp-2] 'digit-argument)
    (define-key map [kp-3] 'digit-argument)
    (define-key map [kp-4] 'digit-argument)
    (define-key map [kp-5] 'digit-argument)
    (define-key map [kp-6] 'digit-argument)
    (define-key map [kp-7] 'digit-argument)
    (define-key map [kp-8] 'digit-argument)
    (define-key map [kp-9] 'digit-argument)
    (define-key map [kp-subtract] 'universal-argument-minus)
    map)
  "Keymap used while processing \\[universal-argument].")

(defvar universal-argument-num-events nil
  "Number of argument-specifying events read by `universal-argument'.
`universal-argument-other-key' uses this to discard those events
from (this-command-keys), and reread only the final command.")

(defvar saved-overriding-map t
  "The saved value of `overriding-terminal-local-map'.
That variable gets restored to this value on exiting \"universal
argument mode\".")

(defun save&set-overriding-map (map)
  "Set `overriding-terminal-local-map' to MAP."
  (when (eq saved-overriding-map t)
    (setq saved-overriding-map overriding-terminal-local-map)
    (setq overriding-terminal-local-map map)))

(defun restore-overriding-map ()
  "Restore `overriding-terminal-local-map' to its saved value."
  (setq overriding-terminal-local-map saved-overriding-map)
  (setq saved-overriding-map t))

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time.
For some commands, just \\[universal-argument] by itself serves as a flag
which is different in effect from any particular numeric argument.
These commands include \\[set-mark-command] and \\[start-kbd-macro]."
  (interactive)
  (setq prefix-arg (list 4))
  (setq universal-argument-num-events (length (this-command-keys)))
  (save&set-overriding-map universal-argument-map))

;; A subsequent C-u means to multiply the factor by 4 if we've typed
;; nothing but C-u's; otherwise it means to terminate the prefix arg.
(defun universal-argument-more (arg)
  (interactive "P")
  (if (consp arg)
      (setq prefix-arg (list (* 4 (car arg))))
    (if (eq arg '-)
	(setq prefix-arg (list -4))
      (setq prefix-arg arg)
      (restore-overriding-map)))
  (setq universal-argument-num-events (length (this-command-keys))))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (cond ((integerp arg)
	 (setq prefix-arg (- arg)))
	((eq arg '-)
	 (setq prefix-arg nil))
	(t
	 (setq prefix-arg '-)))
  (setq universal-argument-num-events (length (this-command-keys)))
  (save&set-overriding-map universal-argument-map))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
		   last-command-event
		 (get last-command-event 'ascii-character)))
	 (digit (- (logand char ?\177) ?0)))
    (cond ((integerp arg)
	   (setq prefix-arg (+ (* arg 10)
			       (if (< arg 0) (- digit) digit))))
	  ((eq arg '-)
	   ;; Treat -0 as just -, so that -01 will work.
	   (setq prefix-arg (if (zerop digit) '- (- digit))))
	  (t
	   (setq prefix-arg digit))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (save&set-overriding-map universal-argument-map))

;; For backward compatibility, minus with no modifiers is an ordinary
;; command if digits have already been entered.
(defun universal-argument-minus (arg)
  (interactive "P")
  (if (integerp arg)
      (universal-argument-other-key arg)
    (negative-argument arg)))

;; Anything else terminates the argument and is left in the queue to be
;; executed as a command.
(defun universal-argument-other-key (arg)
  (interactive "P")
  (setq prefix-arg arg)
  (let* ((key (this-command-keys))
	 (keylist (listify-key-sequence key)))
    (setq unread-command-events
	  (append (nthcdr universal-argument-num-events keylist)
		  unread-command-events)))
  (reset-this-command-lengths)
  (restore-overriding-map))


(defvar filter-buffer-substring-functions nil
  "This variable is a wrapper hook around `filter-buffer-substring'.
Each member of the hook should be a function accepting four arguments:
\(FUN BEG END DELETE), where FUN is itself a function of three arguments
\(BEG END DELETE).  The arguments BEG, END, and DELETE are the same
as those of `filter-buffer-substring' in each case.

The first hook function to be called receives a FUN equivalent
to the default operation of `filter-buffer-substring',
i.e. one that returns the buffer-substring between BEG and
END (processed by any `buffer-substring-filters').  Normally,
the hook function will call FUN and then do its own processing
of the result.  The next hook function receives a FUN equivalent
to the previous hook function, calls it, and does its own
processing, and so on.  The overall result is that of all hook
functions acting in sequence.

Any hook may choose not to call FUN though, in which case it
effectively replaces the default behavior with whatever it chooses.
Of course, a later hook function may do the same thing.")

(defvar buffer-substring-filters nil
  "List of filter functions for `filter-buffer-substring'.
Each function must accept a single argument, a string, and return
a string.  The buffer substring is passed to the first function
in the list, and the return value of each function is passed to
the next.  The final result (if `buffer-substring-filters' is
nil, this is the unfiltered buffer-substring) is passed to the
first function on `filter-buffer-substring-functions'.

As a special convention, point is set to the start of the buffer text
being operated on (i.e., the first argument of `filter-buffer-substring')
before these functions are called.")
(make-obsolete-variable 'buffer-substring-filters
                        'filter-buffer-substring-functions "24.1")

(defun filter-buffer-substring (beg end &optional delete)
  "Return the buffer substring between BEG and END, after filtering.
The wrapper hook `filter-buffer-substring-functions' performs
the actual filtering.  The obsolete variable `buffer-substring-filters'
is also consulted.  If both of these are nil, no filtering is done.

If DELETE is non-nil, the text between BEG and END is deleted
from the buffer.

This function should be used instead of `buffer-substring',
`buffer-substring-no-properties', or `delete-and-extract-region'
when you want to allow filtering to take place.  For example,
major or minor modes can use `filter-buffer-substring-functions' to
extract characters that are special to a buffer, and should not
be copied into other buffers."
  (with-wrapper-hook filter-buffer-substring-functions (beg end delete)
    (cond
     ((or delete buffer-substring-filters)
      (save-excursion
        (goto-char beg)
        (let ((string (if delete (delete-and-extract-region beg end)
                        (buffer-substring beg end))))
          (dolist (filter buffer-substring-filters)
            (setq string (funcall filter string)))
          string)))
     (t
      (buffer-substring beg end)))))


;;;; Window system cut and paste hooks.

(defvar interprogram-cut-function nil
  "Function to call to make a killed region available to other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls whenever text
is put in the kill ring, to make the new kill available to other
programs.

The function takes one argument, TEXT, which is a string containing
the text which should be made available.")

(defvar interprogram-paste-function nil
  "Function to call to get text cut from other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls to obtain
text that other programs have provided for pasting.

The function should be called with no arguments.  If the function
returns nil, then no other program has provided such text, and the top
of the Emacs kill ring should be used.  If the function returns a
string, then the caller of the function \(usually `current-kill')
should put this string in the kill ring as the latest kill.

This function may also return a list of strings if the window
system supports multiple selections.  The first string will be
used as the pasted text, but the other will be placed in the
kill ring for easy access via `yank-pop'.

Note that the function should return a string only if a program other
than Emacs has provided a string for pasting; if Emacs provided the
most recent string, the function should return nil.  If it is
difficult to tell whether Emacs or some other program provided the
current string, it is probably good enough to return nil if the string
is equal (according to `string=') to the last text Emacs provided.")



;;;; The kill ring data structure.

(defvar kill-ring nil
  "List of killed text sequences.
Since the kill ring is supposed to interact nicely with cut-and-paste
facilities offered by window systems, use of this variable should
interact nicely with `interprogram-cut-function' and
`interprogram-paste-function'.  The functions `kill-new',
`kill-append', and `current-kill' are supposed to implement this
interaction; you may want to use them instead of manipulating the kill
ring directly.")

(defcustom kill-ring-max 60
  "Maximum length of kill ring before oldest elements are thrown away."
  :type 'integer
  :group 'killing)

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defcustom save-interprogram-paste-before-kill nil
  "Save clipboard strings into kill ring before replacing them.
When one selects something in another program to paste it into Emacs,
but kills something in Emacs before actually pasting it,
this selection is gone unless this variable is non-nil,
in which case the other program's selection is saved in the `kill-ring'
before the Emacs kill and one can still paste it using \\[yank] \\[yank-pop]."
  :type 'boolean
  :group 'killing
  :version "23.2")

(defcustom kill-do-not-save-duplicates nil
  "Do not add a new string to `kill-ring' if it duplicates the last one.
The comparison is done using `equal-including-properties'."
  :type 'boolean
  :group 'killing
  :version "23.2")

(defun kill-new (string &optional replace yank-handler)
  "Make STRING the latest kill in the kill ring.
Set `kill-ring-yank-pointer' to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list.

When `save-interprogram-paste-before-kill' and `interprogram-paste-function'
are non-nil, saves the interprogram paste string(s) into `kill-ring' before
STRING.

When the yank handler has a non-nil PARAM element, the original STRING
argument is not used by `insert-for-yank'.  However, since Lisp code
may access and use elements from the kill ring directly, the STRING
argument should still be a \"useful\" string for such uses."
  (if (> (length string) 0)
      (if yank-handler
	  (put-text-property 0 (length string)
			     'yank-handler yank-handler string))
    (if yank-handler
	(signal 'args-out-of-range
		(list string "yank-handler specified for empty string"))))
  (unless (and kill-do-not-save-duplicates
	       ;; Due to text properties such as 'yank-handler that
	       ;; can alter the contents to yank, comparison using
	       ;; `equal' is unsafe.
	       (equal-including-properties string (car kill-ring)))
    (if (fboundp 'menu-bar-update-yank-menu)
	(menu-bar-update-yank-menu string (and replace (car kill-ring)))))
  (when save-interprogram-paste-before-kill
    (let ((interprogram-paste (and interprogram-paste-function
                                   (funcall interprogram-paste-function))))
      (when interprogram-paste
        (dolist (s (if (listp interprogram-paste)
		       (nreverse interprogram-paste)
		     (list interprogram-paste)))
	  (unless (and kill-do-not-save-duplicates
		       (equal-including-properties s (car kill-ring)))
	    (push s kill-ring))))))
  (unless (and kill-do-not-save-duplicates
	       (equal-including-properties string (car kill-ring)))
    (if (and replace kill-ring)
	(setcar kill-ring string)
      (push string kill-ring)
      (if (> (length kill-ring) kill-ring-max)
	  (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string)))
(set-advertised-calling-convention
 'kill-new '(string &optional replace) "23.3")

(defun kill-append (string before-p &optional yank-handler)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If `interprogram-cut-function' is set, pass the resulting kill to it."
  (let* ((cur (car kill-ring)))
    (kill-new (if before-p (concat string cur) (concat cur string))
	      (or (= (length cur) 0)
		  (equal yank-handler (get-text-property 0 'yank-handler cur)))
	      yank-handler)))
(set-advertised-calling-convention 'kill-append '(string before-p) "23.3")

(defcustom yank-pop-change-selection nil
  "If non-nil, rotating the kill ring changes the window system selection."
  :type 'boolean
  :group 'killing
  :version "23.1")

(defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If N is zero and `interprogram-paste-function' is set to a
function that returns a string or a list of strings, and if that
function doesn't return nil, then that string (or list) is added
to the front of the kill ring and the string (or first string in
the list) is returned as the latest kill.

If N is not zero, and if `yank-pop-change-selection' is
non-nil, use `interprogram-cut-function' to transfer the
kill at the new yank point into the window system selection.

If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."

  (let ((interprogram-paste (and (= n 0)
				 interprogram-paste-function
				 (funcall interprogram-paste-function))))
    (if interprogram-paste
	(progn
	  ;; Disable the interprogram cut function when we add the new
	  ;; text to the kill ring, so Emacs doesn't try to own the
	  ;; selection, with identical text.
	  (let ((interprogram-cut-function nil))
	    (if (listp interprogram-paste)
	      (mapc 'kill-new (nreverse interprogram-paste))
	      (kill-new interprogram-paste)))
	  (car kill-ring))
      (or kill-ring (error "Kill ring is empty"))
      (let ((ARGth-kill-element
	     (nthcdr (mod (- n (length kill-ring-yank-pointer))
			  (length kill-ring))
		     kill-ring)))
	(unless do-not-move
	  (setq kill-ring-yank-pointer ARGth-kill-element)
	  (when (and yank-pop-change-selection
		     (> n 0)
		     interprogram-cut-function)
	    (funcall interprogram-cut-function (car ARGth-kill-element))))
	(car ARGth-kill-element)))))



;;;; Commands for manipulating the kill ring.

(defcustom kill-read-only-ok nil
  "Non-nil means don't signal an error for killing read-only text."
  :type 'boolean
  :group 'killing)

(put 'text-read-only 'error-conditions
     '(text-read-only buffer-read-only error))
(put 'text-read-only 'error-message (purecopy "Text is read-only"))

(defun kill-region (beg end &optional yank-handler)
  "Kill (\"cut\") text between point and mark.
This deletes the text from the buffer and saves it in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to save the region without killing it, use \\[kill-ring-save].)

If you want to append the killed region to the last killed text,
use \\[append-next-kill] before \\[kill-region].

If the buffer is read-only, Emacs will beep and refrain from deleting
the text, but put the text in the kill ring anyway.  This means that
you can use the killing commands to copy text from a read-only buffer.

Lisp programs should use this function for killing text.
 (To delete text, use `delete-region'.)
Supply two arguments, character positions indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  ;; Pass point first, then mark, because the order matters
  ;; when calling kill-append.
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (condition-case nil
      (let ((string (filter-buffer-substring beg end t)))
	(when string			;STRING is nil if BEG = END
	  ;; Add that string to the kill ring, one way or another.
	  (if (eq last-command 'kill-region)
	      (kill-append string (< end beg) yank-handler)
	    (kill-new string nil yank-handler)))
	(when (or string (eq last-command 'kill-region))
	  (setq this-command 'kill-region))
	nil)
    ((buffer-read-only text-read-only)
     ;; The code above failed because the buffer, or some of the characters
     ;; in the region, are read-only.
     ;; We should beep, in case the user just isn't aware of this.
     ;; However, there's no harm in putting
     ;; the region's text in the kill ring, anyway.
     (copy-region-as-kill beg end)
     ;; Set this-command now, so it will be set even if we get an error.
     (setq this-command 'kill-region)
     ;; This should barf, if appropriate, and give us the correct error.
     (if kill-read-only-ok
	 (progn (message "Read only text copied to kill ring") nil)
       ;; Signal an error if the buffer is read-only.
       (barf-if-buffer-read-only)
       ;; If the buffer isn't read-only, the text is.
       (signal 'text-read-only (list (current-buffer)))))))
(set-advertised-calling-convention 'kill-region '(beg end) "23.3")

;; copy-region-as-kill no longer sets this-command, because it's confusing
;; to get two copies of the text when the user accidentally types M-w and
;; then corrects it with the intended C-w.
(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

This command's old key binding has been given to `kill-ring-save'."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (filter-buffer-substring beg end) (< end beg))
    (kill-new (filter-buffer-substring beg end)))
  (setq deactivate-mark t)
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied."
  (interactive "r")
  (copy-region-as-kill beg end)
  ;; This use of called-interactively-p is correct
  ;; because the code it controls just gives the user visual feedback.
  (if (called-interactively-p 'interactive)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
            ;; Swap point-and-mark quickly so as to show the region that
            ;; was selected.  Don't do it if the region is highlighted.
	    (unless (and (region-active-p)
			 (face-background 'region))
	      ;; Swap point and mark.
	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
	      (sit-for blink-matching-delay)
	      ;; Swap back.
	      (set-marker (mark-marker) other-end (current-buffer))
	      (goto-char opoint)
	      ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag mark-active
		   (deactivate-mark)))
	  (let* ((killed-text (current-kill 0))
		 (message-len (min (length killed-text) 40)))
	    (if (= (point) beg)
		;; Don't say "killed"; that is misleading.
		(message "Saved text until \"%s\""
			(substring killed-text (- message-len)))
	      (message "Saved text from \"%s\""
		      (substring killed-text 0 message-len))))))))

(defun append-next-kill (&optional interactive)
  "Cause following command, if it kills, to append to previous kill.
The argument is used for internal purposes; do not supply one."
  (interactive "p")
  ;; We don't use (interactive-p), since that breaks kbd macros.
  (if interactive
      (progn
	(setq this-command 'kill-region)
	(message "If the next command is a kill, it will append"))
    (setq last-command 'kill-region)))

;; Yanking.

;; This is actually used in subr.el but defcustom does not work there.
(defcustom yank-excluded-properties
  '(read-only invisible intangible field mouse-face help-echo local-map keymap
    yank-handler follow-link fontified)
  "Text properties to discard when yanking.
The value should be a list of text properties to discard or t,
which means to discard all text properties."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'killing
  :version "22.1")

(defvar yank-window-start nil)
(defvar yank-undo-function nil
  "If non-nil, function used by `yank-pop' to delete last stretch of yanked text.
Function is called with two parameters, START and END corresponding to
the value of the mark and point; it is guaranteed that START <= END.
Normally set from the UNDO element of a yank-handler; see `insert-for-yank'.")

(defun yank-pop (&optional arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one.

When this command inserts killed text into the buffer, it honors
`yank-excluded-properties' and `yank-handler' as described in the
doc string for `insert-for-yank-1', which see."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (unless arg (setq arg 1))
  (let ((inhibit-read-only t)
	(before (< (point) (mark t))))
    (if before
	(funcall (or yank-undo-function 'delete-region) (point) (mark t))
      (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
    (setq yank-undo-function nil)
    (set-marker (mark-marker) (point) (current-buffer))
    (insert-for-yank (current-kill arg))
    ;; Set the window start back where it was in the yank command,
    ;; if possible.
    (set-window-start (selected-window) yank-window-start t)
    (if before
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))))
  nil)

(defun yank (&optional arg)
  "Reinsert (\"paste\") the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just \\[universal-argument] as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.

When this command inserts killed text into the buffer, it honors
`yank-excluded-properties' and `yank-handler' as described in the
doc string for `insert-for-yank-1', which see.

See also the command `yank-pop' (\\[yank-pop])."
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))
  (insert-for-yank (current-kill (cond
				  ((listp arg) 0)
				  ((eq arg '-) -2)
				  (t (1- arg)))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (if (eq this-command t)
      (setq this-command 'yank))
  nil)

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring.
With ARG, rotate that many kills forward (or backward, if negative)."
  (interactive "p")
  (current-kill arg))

;; Some kill commands.

;; Internal subroutine of delete-char
(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (+ (point) arg)))

;; Internal subroutine of backward-delete-char
(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (- (point) arg)))

(defcustom backward-delete-char-untabify-method 'untabify
  "The method for untabifying when deleting backward.
Can be `untabify' -- turn a tab to many spaces, then delete one space;
       `hungry' -- delete all whitespace, both tabs and spaces;
       `all' -- delete all whitespace, including tabs, spaces and newlines;
       nil -- just delete one character."
  :type '(choice (const untabify) (const hungry) (const all) (const nil))
  :version "20.3"
  :group 'killing)

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
The exact behavior depends on `backward-delete-char-untabify-method'.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if a prefix arg was specified."
  (interactive "*p\nP")
  (when (eq backward-delete-char-untabify-method 'untabify)
    (let ((count arg))
      (save-excursion
	(while (and (> count 0) (not (bobp)))
	  (if (= (preceding-char) ?\t)
	      (let ((col (current-column)))
		(forward-char -1)
		(setq col (- col (current-column)))
		(insert-char ?\s col)
		(delete-char 1)))
	  (forward-char -1)
	  (setq count (1- count))))))
  (let* ((skip (cond ((eq backward-delete-char-untabify-method 'hungry) " \t")
                     ((eq backward-delete-char-untabify-method 'all)
                      " \t\n\r")))
         (n (if skip
                (let* ((oldpt (point))
                       (wh (- oldpt (save-excursion
                                      (skip-chars-backward skip)
                                      (constrain-to-field nil oldpt)))))
                  (+ arg (if (zerop wh) 0 (1- wh))))
              arg)))
    ;; Avoid warning about delete-backward-char
    (with-no-warnings (delete-backward-char n killp))))

(defun zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
;			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			 (point))))

;; kill-line and its subroutines.

(defcustom kill-whole-line nil
  "If non-nil, `kill-line' with no arg at beg of line kills the whole line."
  :type 'boolean
  :group 'killing)

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument ARG, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[move-beginning-of-line] \\[kill-line] \\[kill-line].

If `show-trailing-whitespace' is non-nil, this command will just
kill the rest of the current line, even if there are only
nonblanks there.

If `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[move-beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visible-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (forward-visible-line 1)
		       (goto-char end))))
		 (point))))

(defun kill-whole-line (&optional arg)
  "Kill current line.
With prefix ARG, kill that many lines starting from the current line.
If ARG is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If ARG is zero, kill current line but exclude the trailing newline."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'kill-region)
    (kill-new "")
    (setq last-command 'kill-region))
  (cond ((zerop arg)
	 ;; We need to kill in two steps, because the previous command
	 ;; could have been a kill command, in which case the text
	 ;; before point needs to be prepended to the current kill
	 ;; ring entry and the text after point appended.  Also, we
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   (kill-region (point) (progn (forward-visible-line 0) (point))))
	 (kill-region (point) (progn (end-of-visible-line) (point))))
	((< arg 0)
	 (save-excursion
	   (kill-region (point) (progn (end-of-visible-line) (point))))
	 (kill-region (point)
		      (progn (forward-visible-line (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (kill-region (point) (progn (forward-visible-line 0) (point))))
	 (kill-region (point)
		      (progn (forward-visible-line arg) (point))))))

(defun forward-visible-line (arg)
  "Move forward by ARG lines, ignoring currently invisible newlines only.
If ARG is negative, move backward -ARG lines.
If ARG is zero, move to the beginning of the current line."
  (condition-case nil
      (if (> arg 0)
	  (progn
	    (while (> arg 0)
	      (or (zerop (forward-line 1))
		  (signal 'end-of-buffer nil))
	      ;; If the newline we just skipped is invisible,
	      ;; don't count it.
	      (let ((prop
		     (get-char-property (1- (point)) 'invisible)))
		(if (if (eq buffer-invisibility-spec t)
			prop
		      (or (memq prop buffer-invisibility-spec)
			  (assq prop buffer-invisibility-spec)))
		    (setq arg (1+ arg))))
	      (setq arg (1- arg)))
	    ;; If invisible text follows, and it is a number of complete lines,
	    ;; skip it.
	    (let ((opoint (point)))
	      (while (and (not (eobp))
			  (let ((prop
				 (get-char-property (point) 'invisible)))
			    (if (eq buffer-invisibility-spec t)
				prop
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))))
		(goto-char
		 (if (get-text-property (point) 'invisible)
		     (or (next-single-property-change (point) 'invisible)
			 (point-max))
		   (next-overlay-change (point)))))
	      (unless (bolp)
		(goto-char opoint))))
	(let ((first t))
	  (while (or first (<= arg 0))
	    (if first
		(beginning-of-line)
	      (or (zerop (forward-line -1))
		  (signal 'beginning-of-buffer nil)))
	    ;; If the newline we just moved to is invisible,
	    ;; don't count it.
	    (unless (bobp)
	      (let ((prop
		     (get-char-property (1- (point)) 'invisible)))
		(unless (if (eq buffer-invisibility-spec t)
			    prop
			  (or (memq prop buffer-invisibility-spec)
			      (assq prop buffer-invisibility-spec)))
		  (setq arg (1+ arg)))))
	    (setq first nil))
	  ;; If invisible text follows, and it is a number of complete lines,
	  ;; skip it.
	  (let ((opoint (point)))
	    (while (and (not (bobp))
			(let ((prop
			       (get-char-property (1- (point)) 'invisible)))
			  (if (eq buffer-invisibility-spec t)
			      prop
			    (or (memq prop buffer-invisibility-spec)
				(assq prop buffer-invisibility-spec)))))
	      (goto-char
	       (if (get-text-property (1- (point)) 'invisible)
		   (or (previous-single-property-change (point) 'invisible)
		       (point-min))
		 (previous-overlay-change (point)))))
	    (unless (bolp)
	      (goto-char opoint)))))
    ((beginning-of-buffer end-of-buffer)
     nil)))

(defun end-of-visible-line ()
  "Move to end of current visible line."
  (end-of-line)
  ;; If the following character is currently invisible,
  ;; skip all characters with that same `invisible' property value,
  ;; then find the next newline.
  (while (and (not (eobp))
	      (save-excursion
		(skip-chars-forward "^\n")
		(let ((prop
		       (get-char-property (point) 'invisible)))
		  (if (eq buffer-invisibility-spec t)
		      prop
		    (or (memq prop buffer-invisibility-spec)
			(assq prop buffer-invisibility-spec))))))
    (skip-chars-forward "^\n")
    (if (get-text-property (point) 'invisible)
	(goto-char (or (next-single-property-change (point) 'invisible)
		       (point-max)))
      (goto-char (next-overlay-change (point))))
    (end-of-line)))

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name.

This function is meant for the user to run interactively.
Don't call it from programs: use `insert-buffer-substring' instead!"
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (read-buffer "Insert buffer: "
		   (if (eq (selected-window) (next-window (selected-window)))
		       (other-buffer (current-buffer))
		     (window-buffer (next-window (selected-window))))
		   t))))
  (push-mark
   (save-excursion
     (insert-buffer-substring (get-buffer buffer))
     (point)))
  nil)

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
	 (region-beginning) (region-end)))
  (let* ((oldbuf (current-buffer))
         (append-to (get-buffer-create buffer))
         (windows (get-buffer-window-list append-to t t))
         point)
    (save-excursion
      (with-current-buffer append-to
        (setq point (point))
        (barf-if-buffer-read-only)
        (insert-buffer-substring oldbuf start end)
        (dolist (window windows)
          (when (= (window-point window) point)
            (set-window-point window (point))))))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BPrepend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(put 'mark-inactive 'error-conditions '(mark-inactive error))
(put 'mark-inactive 'error-message (purecopy "The mark is not active now"))

(defvar activate-mark-hook nil
  "Hook run when the mark becomes active.
It is also run at the end of a command, if the mark is active and
it is possible that the region may have changed.")

(defvar deactivate-mark-hook nil
  "Hook run when the mark becomes inactive.")

(defun mark (&optional force)
  "Return this buffer's mark value as integer, or nil if never set.

In Transient Mark mode, this function signals an error if
the mark is not active.  However, if `mark-even-if-inactive' is non-nil,
or the argument FORCE is non-nil, it disregards whether the mark
is active, and returns an integer or nil in the usual way.

If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (if (or force (not transient-mark-mode) mark-active mark-even-if-inactive)
      (marker-position (mark-marker))
    (signal 'mark-inactive nil)))

(defsubst deactivate-mark (&optional force)
  "Deactivate the mark.
If Transient Mark mode is disabled, this function normally does
nothing; but if FORCE is non-nil, it deactivates the mark anyway.

Deactivating the mark sets `mark-active' to nil, updates the
primary selection according to `select-active-regions', and runs
`deactivate-mark-hook'.

If Transient Mark mode was temporarily enabled, reset the value
of the variable `transient-mark-mode'; if this causes Transient
Mark mode to be disabled, don't change `mark-active' to nil or
run `deactivate-mark-hook'."
  (when (or transient-mark-mode force)
    (when (and (if (eq select-active-regions 'only)
		   (eq (car-safe transient-mark-mode) 'only)
		 select-active-regions)
	       (region-active-p)
	       (display-selections-p))
      ;; The var `saved-region-selection', if non-nil, is the text in
      ;; the region prior to the last command modifying the buffer.
      ;; Set the selection to that, or to the current region.
      (cond (saved-region-selection
	     (x-set-selection 'PRIMARY saved-region-selection)
	     (setq saved-region-selection nil))
	    ((/= (region-beginning) (region-end))
	     (x-set-selection 'PRIMARY
			      (buffer-substring-no-properties
			       (region-beginning)
			       (region-end))))))
    (if (and (null force)
	     (or (eq transient-mark-mode 'lambda)
		 (and (eq (car-safe transient-mark-mode) 'only)
		      (null (cdr transient-mark-mode)))))
	;; When deactivating a temporary region, don't change
	;; `mark-active' or run `deactivate-mark-hook'.
	(setq transient-mark-mode nil)
      (if (eq (car-safe transient-mark-mode) 'only)
	  (setq transient-mark-mode (cdr transient-mark-mode)))
      (setq mark-active nil)
      (run-hooks 'deactivate-mark-hook))))

(defun activate-mark ()
  "Activate the mark."
  (when (mark t)
    (setq mark-active t)
    (unless transient-mark-mode
      (setq transient-mark-mode 'lambda))))

(defun set-mark (pos)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use `push-mark', not `set-mark'.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (if pos
      (progn
	(setq mark-active t)
	(run-hooks 'activate-mark-hook)
	(set-marker (mark-marker) pos (current-buffer)))
    ;; Normally we never clear mark-active except in Transient Mark mode.
    ;; But when we actually clear out the mark value too, we must
    ;; clear mark-active in any mode.
    (deactivate-mark t)
    (set-marker (mark-marker) nil)))

(defcustom use-empty-active-region nil
  "Whether \"region-aware\" commands should act on empty regions.
If nil, region-aware commands treat empty regions as inactive.
If non-nil, region-aware commands treat the region as active as
long as the mark is active, even if the region is empty.

Region-aware commands are those that act on the region if it is
active and Transient Mark mode is enabled, and on the text near
point otherwise."
  :type 'boolean
  :version "23.1"
  :group 'editing-basics)

(defun use-region-p ()
  "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
  (and (region-active-p)
       (or use-empty-active-region (> (region-end) (region-beginning)))))

(defun region-active-p ()
  "Return t if Transient Mark mode is enabled and the mark is active.

Some commands act specially on the region when Transient Mark
mode is enabled.  Usually, such commands should use
`use-region-p' instead of this function, because `use-region-p'
also checks the value of `use-empty-active-region'."
  (and transient-mark-mode mark-active))

(defvar mark-ring nil
  "The list of former marks of the current buffer, most recent first.")
(make-variable-buffer-local 'mark-ring)
(put 'mark-ring 'permanent-local t)

(defcustom mark-ring-max 16
  "Maximum size of mark ring.  Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defvar global-mark-ring nil
  "The list of saved global marks, most recent first.")

(defcustom global-mark-ring-max 16
  "Maximum size of global mark ring.  \
Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defun pop-to-mark-command ()
  "Jump to mark, and pop a new position for mark off the ring.
\(Does not affect global mark ring\)."
  (interactive)
  (if (null (mark t))
      (error "No mark set in this buffer")
    (if (= (point) (mark t))
	(message "Mark popped"))
    (goto-char (mark t))
    (pop-mark)))

(defun push-mark-command (arg &optional nomsg)
  "Set mark at where point is.
If no prefix ARG and mark is already set there, just activate it.
Display `Mark set' unless the optional second arg NOMSG is non-nil."
  (interactive "P")
  (let ((mark (marker-position (mark-marker))))
    (if (or arg (null mark) (/= mark (point)))
	(push-mark nil nomsg t)
      (setq mark-active t)
      (run-hooks 'activate-mark-hook)
      (unless nomsg
	(message "Mark activated")))))

(defcustom set-mark-command-repeat-pop nil
  "Non-nil means repeating \\[set-mark-command] after popping mark pops it again.
That means that C-u \\[set-mark-command] \\[set-mark-command]
will pop the mark twice, and
C-u \\[set-mark-command] \\[set-mark-command] \\[set-mark-command]
will pop the mark three times.

A value of nil means \\[set-mark-command]'s behavior does not change
after C-u \\[set-mark-command]."
  :type 'boolean
  :group 'editing-basics)

(defcustom set-mark-default-inactive nil
  "If non-nil, setting the mark does not activate it.
This causes \\[set-mark-command] and \\[exchange-point-and-mark] to
behave the same whether or not `transient-mark-mode' is enabled."
  :type 'boolean
  :group 'editing-basics
  :version "23.1")

(defun set-mark-command (arg)
  "Set the mark where point is, or jump to the mark.
Setting the mark also alters the region, which is the text
between point and mark; this is the closest equivalent in
Emacs to what some editors call the \"selection\".

With no prefix argument, set the mark at point, and push the
old mark position on local mark ring.  Also push the old mark on
global mark ring, if the previous mark was set in another buffer.

When Transient Mark Mode is off, immediately repeating this
command activates `transient-mark-mode' temporarily.

With prefix argument \(e.g., \\[universal-argument] \\[set-mark-command]\), \
jump to the mark, and set the mark from
position popped off the local mark ring \(this does not affect the global
mark ring\).  Use \\[pop-global-mark] to jump to a mark popped off the global
mark ring \(see `pop-global-mark'\).

If `set-mark-command-repeat-pop' is non-nil, repeating
the \\[set-mark-command] command with no prefix argument pops the next position
off the local (or global) mark ring and jumps there.

With \\[universal-argument] \\[universal-argument] as prefix
argument, unconditionally set mark where point is, even if
`set-mark-command-repeat-pop' is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (cond ((eq transient-mark-mode 'lambda)
	 (setq transient-mark-mode nil))
	((eq (car-safe transient-mark-mode) 'only)
	 (deactivate-mark)))
  (cond
   ((and (consp arg) (> (prefix-numeric-value arg) 4))
    (push-mark-command nil))
   ((not (eq this-command 'set-mark-command))
    (if arg
	(pop-to-mark-command)
      (push-mark-command t)))
   ((and set-mark-command-repeat-pop
	 (eq last-command 'pop-to-mark-command))
    (setq this-command 'pop-to-mark-command)
    (pop-to-mark-command))
   ((and set-mark-command-repeat-pop
	 (eq last-command 'pop-global-mark)
	 (not arg))
    (setq this-command 'pop-global-mark)
    (pop-global-mark))
   (arg
    (setq this-command 'pop-to-mark-command)
    (pop-to-mark-command))
   ((eq last-command 'set-mark-command)
    (if (region-active-p)
        (progn
          (deactivate-mark)
          (message "Mark deactivated"))
      (activate-mark)
      (message "Mark activated")))
   (t
    (push-mark-command nil)
    (if set-mark-default-inactive (deactivate-mark)))))

(defun push-mark (&optional location nomsg activate)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
  (unless (null (mark t))
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (when (> (length mark-ring) mark-ring-max)
      (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (if (and global-mark-ring
	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      ;; The last global mark pushed was in this same buffer.
      ;; Don't push another one.
      nil
    (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
    (when (> (length global-mark-ring) global-mark-ring-max)
      (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
      (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (if (or activate (not transient-mark-mode))
      (set-mark (mark t)))
  nil)

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (when mark-ring
    (setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
    (set-marker (mark-marker) (+ 0 (car mark-ring)) (current-buffer))
    (move-marker (car mark-ring) nil)
    (if (null (mark t)) (ding))
    (setq mark-ring (cdr mark-ring)))
  (deactivate-mark))

(define-obsolete-function-alias
  'exchange-dot-and-mark 'exchange-point-and-mark "23.3")
(defun exchange-point-and-mark (&optional arg)
  "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active,
and it reactivates the mark.

If Transient Mark mode is on, a prefix ARG deactivates the mark
if it is active, and otherwise avoids reactivating it.  If
Transient Mark mode is off, a prefix ARG enables Transient Mark
mode temporarily."
  (interactive "P")
  (let ((omark (mark t))
	(temp-highlight (eq (car-safe transient-mark-mode) 'only)))
    (if (null omark)
        (error "No mark set in this buffer"))
    (deactivate-mark)
    (set-mark (point))
    (goto-char omark)
    (if set-mark-default-inactive (deactivate-mark))
    (cond (temp-highlight
	   (setq transient-mark-mode (cons 'only transient-mark-mode)))
	  ((or (and arg (region-active-p)) ; (xor arg (not (region-active-p)))
	       (not (or arg (region-active-p))))
	   (deactivate-mark))
	  (t (activate-mark)))
    nil))

(defcustom shift-select-mode t
  "When non-nil, shifted motion keys activate the mark momentarily.

While the mark is activated in this way, any shift-translated point
motion key extends the region, and if Transient Mark mode was off, it
is temporarily turned on.  Furthermore, the mark will be deactivated
by any subsequent point motion key that was not shift-translated, or
by any action that normally deactivates the mark in Transient Mark mode.

See `this-command-keys-shift-translated' for the meaning of
shift-translation."
  :type 'boolean
  :group 'editing-basics)

(defun handle-shift-selection ()
  "Activate/deactivate mark depending on invocation thru shift translation.
This function is called by `call-interactively' when a command
with a `^' character in its `interactive' spec is invoked, before
running the command itself.

If `shift-select-mode' is enabled and the command was invoked
through shift translation, set the mark and activate the region
temporarily, unless it was already set in this way.  See
`this-command-keys-shift-translated' for the meaning of shift
translation.

Otherwise, if the region has been activated temporarily,
deactivate it, and restore the variable `transient-mark-mode' to
its earlier value."
  (cond ((and shift-select-mode this-command-keys-shift-translated)
         (unless (and mark-active
		      (eq (car-safe transient-mark-mode) 'only))
	   (setq transient-mark-mode
                 (cons 'only
                       (unless (eq transient-mark-mode 'lambda)
                         transient-mark-mode)))
           (push-mark nil nil t)))
        ((eq (car-safe transient-mark-mode) 'only)
         (setq transient-mark-mode (cdr transient-mark-mode))
         (deactivate-mark))))

(define-minor-mode transient-mark-mode
  "Toggle Transient Mark mode.
With a prefix argument ARG, enable Transient Mark mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Transient Mark mode if ARG is omitted or nil.

Transient Mark mode is a global minor mode.  When enabled, the
region is highlighted whenever the mark is active.  The mark is
\"deactivated\" by changing the buffer, and after certain other
operations that set the mark but whose main purpose is something
else--for example, incremental search, \\[beginning-of-buffer], and \\[end-of-buffer].

You can also deactivate the mark by typing \\[keyboard-quit] or
\\[keyboard-escape-quit].

Many commands change their behavior when Transient Mark mode is in effect
and the mark is active, by acting on the region instead of their usual
default part of the buffer's text.  Examples of such commands include
\\[comment-dwim], \\[flush-lines], \\[keep-lines], \
\\[query-replace], \\[query-replace-regexp], \\[ispell], and \\[undo].
Invoke \\[apropos-documentation] and type \"transient\" or
\"mark.*active\" at the prompt, to see the documentation of
commands which are sensitive to the Transient Mark mode."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable transient-mark-mode)

(defvar widen-automatically t
  "Non-nil means it is ok for commands to call `widen' when they want to.
Some commands will do this in order to go to positions outside
the current accessible part of the buffer.

If `widen-automatically' is nil, these commands will do something else
as a fallback, and won't change the buffer bounds.")

(defvar non-essential nil
  "Whether the currently executing code is performing an essential task.
This variable should be non-nil only when running code which should not
disturb the user.  E.g. it can be used to prevent Tramp from prompting the
user for a password when we are simply scanning a set of files in the
background or displaying possible completions before the user even asked
for it.")

(defun pop-global-mark ()
  "Pop off global mark ring and jump to the top location."
  (interactive)
  ;; Pop entries which refer to non-existent buffers.
  (while (and global-mark-ring (not (marker-buffer (car global-mark-ring))))
    (setq global-mark-ring (cdr global-mark-ring)))
  (or global-mark-ring
      (error "No global mark set"))
  (let* ((marker (car global-mark-ring))
	 (buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (setq global-mark-ring (nconc (cdr global-mark-ring)
				  (list (car global-mark-ring))))
    (set-buffer buffer)
    (or (and (>= position (point-min))
	     (<= position (point-max)))
	(if widen-automatically
	    (widen)
	  (error "Global mark position is outside accessible part of buffer")))
    (goto-char position)
    (switch-to-buffer buffer)))

(defcustom next-line-add-newlines nil
  "If non-nil, `next-line' inserts newline to avoid `end of buffer' error."
  :type 'boolean
  :version "21.1"
  :group 'editing-basics)

(defun next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is enabled.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer.

If the variable `line-move-visual' is non-nil, this command moves
by display lines.  Otherwise, it moves by buffer lines, without
taking variable-width characters or continued lines into account.

The command \\[set-goal-column] can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.  Note that setting `goal-column'
overrides `line-move-visual' and causes this command to move by buffer
lines rather than by display lines.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (and next-line-add-newlines (= arg 1))
      (if (save-excursion (end-of-line) (eobp))
	  ;; When adding a newline, don't expand an abbrev.
	  (let ((abbrev-mode nil))
	    (end-of-line)
	    (insert (if use-hard-newlines hard-newline "\n")))
	(line-move arg nil nil try-vscroll))
    (if (called-interactively-p 'interactive)
	(condition-case err
	    (line-move arg nil nil try-vscroll)
	  ((beginning-of-buffer end-of-buffer)
	   (signal (car err) (cdr err))))
      (line-move arg nil nil try-vscroll)))
  nil)

(defun previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is enabled.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

If the variable `line-move-visual' is non-nil, this command moves
by display lines.  Otherwise, it moves by buffer lines, without
taking variable-width characters or continued lines into account.

The command \\[set-goal-column] can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.  Note that setting `goal-column'
overrides `line-move-visual' and causes this command to move by buffer
lines rather than by display lines.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (called-interactively-p 'interactive)
      (condition-case err
	  (line-move (- arg) nil nil try-vscroll)
	((beginning-of-buffer end-of-buffer)
	 (signal (car err) (cdr err))))
    (line-move (- arg) nil nil try-vscroll))
  nil)

(defcustom track-eol nil
  "Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line.
This has no effect when `line-move-visual' is non-nil."
  :type 'boolean
  :group 'editing-basics)

(defcustom goal-column nil
  "Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil.
A non-nil setting overrides `line-move-visual', which see."
  :type '(choice integer
		 (const :tag "None" nil))
  :group 'editing-basics)
(make-variable-buffer-local 'goal-column)

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was at the start of the current run
of vertical motion commands.

When moving by visual lines via `line-move-visual', it is a cons
cell (COL . HSCROLL), where COL is the x-position, in pixels,
divided by the default column width, and HSCROLL is the number of
columns by which window is scrolled from left margin.

When the `track-eol' feature is doing its job, the value is
`most-positive-fixnum'.")

(defcustom line-move-ignore-invisible t
  "Non-nil means \\[next-line] and \\[previous-line] ignore invisible lines.
Outline mode sets this."
  :type 'boolean
  :group 'editing-basics)

(defcustom line-move-visual t
  "When non-nil, `line-move' moves point by visual lines.
This movement is based on where the cursor is displayed on the
screen, instead of relying on buffer contents alone.  It takes
into account variable-width characters and line continuation.
If nil, `line-move' moves point by logical lines.
A non-nil setting of `goal-column' overrides the value of this variable
and forces movement by logical lines.
A window that is  horizontally scrolled also forces movement by logical
lines."
  :type 'boolean
  :group 'editing-basics
  :version "23.1")

;; Returns non-nil if partial move was done.
(defun line-move-partial (arg noerror to-end)
  (if (< arg 0)
      ;; Move backward (up).
      ;; If already vscrolled, reduce vscroll
      (let ((vs (window-vscroll nil t)))
	(when (> vs (frame-char-height))
	  (set-window-vscroll nil (- vs (frame-char-height)) t)))

    ;; Move forward (down).
    (let* ((lh (window-line-height -1))
	   (vpos (nth 1 lh))
	   (ypos (nth 2 lh))
	   (rbot (nth 3 lh))
	   py vs)
      (when (or (null lh)
		(>= rbot (frame-char-height))
		(<= ypos (- (frame-char-height))))
	(unless lh
	  (let ((wend (pos-visible-in-window-p t nil t)))
	    (setq rbot (nth 3 wend)
		  vpos (nth 5 wend))))
	(cond
	 ;; If last line of window is fully visible, move forward.
	 ((or (null rbot) (= rbot 0))
	  nil)
	 ;; If cursor is not in the bottom scroll margin, move forward.
	 ((and (> vpos 0)
	       (< (setq py
			(or (nth 1 (window-line-height))
			    (let ((ppos (posn-at-point)))
			      (cdr (or (posn-actual-col-row ppos)
				       (posn-col-row ppos))))))
		  (min (- (window-text-height) scroll-margin 1) (1- vpos))))
	  nil)
	 ;; When already vscrolled, we vscroll some more if we can,
	 ;; or clear vscroll and move forward at end of tall image.
	 ((> (setq vs (window-vscroll nil t)) 0)
	  (when (> rbot 0)
	    (set-window-vscroll nil (+ vs (min rbot (frame-char-height))) t)))
	 ;; If cursor just entered the bottom scroll margin, move forward,
	 ;; but also vscroll one line so redisplay won't recenter.
	 ((and (> vpos 0)
	       (= py (min (- (window-text-height) scroll-margin 1)
			  (1- vpos))))
	  (set-window-vscroll nil (frame-char-height) t)
	  (line-move-1 arg noerror to-end)
	  t)
	 ;; If there are lines above the last line, scroll-up one line.
	 ((> vpos 0)
	  (scroll-up 1)
	  t)
	 ;; Finally, start vscroll.
	 (t
	  (set-window-vscroll nil (frame-char-height) t)))))))


;; This is like line-move-1 except that it also performs
;; vertical scrolling of tall images if appropriate.
;; That is not really a clean thing to do, since it mixes
;; scrolling with cursor motion.  But so far we don't have
;; a cleaner solution to the problem of making C-n do something
;; useful given a tall image.
(defun line-move (arg &optional noerror to-end try-vscroll)
  (unless (and auto-window-vscroll try-vscroll
	       ;; Only vscroll for single line moves
	       (= (abs arg) 1)
	       ;; But don't vscroll in a keyboard macro.
	       (not defining-kbd-macro)
	       (not executing-kbd-macro)
	       (line-move-partial arg noerror to-end))
    (set-window-vscroll nil 0 t)
    (if (and line-move-visual
	     ;; Display-based column are incompatible with goal-column.
	     (not goal-column)
	     ;; When the text in the window is scrolled to the left,
	     ;; display-based motion doesn't make sense (because each
	     ;; logical line occupies exactly one screen line).
	     (not (> (window-hscroll) 0)))
	(line-move-visual arg noerror)
      (line-move-1 arg noerror to-end))))

;; Display-based alternative to line-move-1.
;; Arg says how many lines to move.  The value is t if we can move the
;; specified number of lines.
(defun line-move-visual (arg &optional noerror)
  (let ((opoint (point))
	(hscroll (window-hscroll))
	target-hscroll)
    ;; Check if the previous command was a line-motion command, or if
    ;; we were called from some other command.
    (if (and (consp temporary-goal-column)
	     (memq last-command `(next-line previous-line ,this-command)))
	;; If so, there's no need to reset `temporary-goal-column',
	;; but we may need to hscroll.
	(if (or (/= (cdr temporary-goal-column) hscroll)
		(>  (cdr temporary-goal-column) 0))
	    (setq target-hscroll (cdr temporary-goal-column)))
      ;; Otherwise, we should reset `temporary-goal-column'.
      (let ((posn (posn-at-point)))
	(cond
	 ;; Handle the `overflow-newline-into-fringe' case:
	 ((eq (nth 1 posn) 'right-fringe)
	  (setq temporary-goal-column (cons (- (window-width) 1) hscroll)))
	 ((car (posn-x-y posn))
	  (setq temporary-goal-column
		(cons (/ (float (car (posn-x-y posn)))
			 (frame-char-width)) hscroll))))))
    (if target-hscroll
	(set-window-hscroll (selected-window) target-hscroll))
    (or (and (= (vertical-motion
		 (cons (or goal-column
			   (if (consp temporary-goal-column)
			       (car temporary-goal-column)
			     temporary-goal-column))
		       arg))
		arg)
	     (or (>= arg 0)
		 (/= (point) opoint)
		 ;; If the goal column lies on a display string,
		 ;; `vertical-motion' advances the cursor to the end
		 ;; of the string.  For arg < 0, this can cause the
		 ;; cursor to get stuck.  (Bug#3020).
		 (= (vertical-motion arg) arg)))
	(unless noerror
	  (signal (if (< arg 0) 'beginning-of-buffer 'end-of-buffer)
		  nil)))))

;; This is the guts of next-line and previous-line.
;; Arg says how many lines to move.
;; The value is t if we can move the specified number of lines.
(defun line-move-1 (arg &optional noerror _to-end)
  ;; Don't run any point-motion hooks, and disregard intangibility,
  ;; for intermediate positions.
  (let ((inhibit-point-motion-hooks t)
	(opoint (point))
	(orig-arg arg))
    (if (consp temporary-goal-column)
	(setq temporary-goal-column (+ (car temporary-goal-column)
				       (cdr temporary-goal-column))))
    (unwind-protect
	(progn
	  (if (not (memq last-command '(next-line previous-line)))
	      (setq temporary-goal-column
		    (if (and track-eol (eolp)
			     ;; Don't count beg of empty line as end of line
			     ;; unless we just did explicit end-of-line.
			     (or (not (bolp)) (eq last-command 'move-end-of-line)))
			most-positive-fixnum
		      (current-column))))

	  (if (not (or (integerp selective-display)
                       line-move-ignore-invisible))
	      ;; Use just newline characters.
	      ;; Set ARG to 0 if we move as many lines as requested.
	      (or (if (> arg 0)
		      (progn (if (> arg 1) (forward-line (1- arg)))
			     ;; This way of moving forward ARG lines
			     ;; verifies that we have a newline after the last one.
			     ;; It doesn't get confused by intangible text.
			     (end-of-line)
			     (if (zerop (forward-line 1))
				 (setq arg 0)))
		    (and (zerop (forward-line arg))
			 (bolp)
			 (setq arg 0)))
		  (unless noerror
		    (signal (if (< arg 0)
				'beginning-of-buffer
			      'end-of-buffer)
			    nil)))
	    ;; Move by arg lines, but ignore invisible ones.
	    (let (done)
	      (while (and (> arg 0) (not done))
		;; If the following character is currently invisible,
		;; skip all characters with that same `invisible' property value.
		(while (and (not (eobp)) (invisible-p (point)))
		  (goto-char (next-char-property-change (point))))
		;; Move a line.
		;; We don't use `end-of-line', since we want to escape
		;; from field boundaries occurring exactly at point.
		(goto-char (constrain-to-field
			    (let ((inhibit-field-text-motion t))
			      (line-end-position))
			    (point) t t
			    'inhibit-line-move-field-capture))
		;; If there's no invisibility here, move over the newline.
		(cond
		 ((eobp)
		  (if (not noerror)
		      (signal 'end-of-buffer nil)
		    (setq done t)))
		 ((and (> arg 1)  ;; Use vertical-motion for last move
		       (not (integerp selective-display))
		       (not (invisible-p (point))))
		  ;; We avoid vertical-motion when possible
		  ;; because that has to fontify.
		  (forward-line 1))
		 ;; Otherwise move a more sophisticated way.
		 ((zerop (vertical-motion 1))
		  (if (not noerror)
		      (signal 'end-of-buffer nil)
		    (setq done t))))
		(unless done
		  (setq arg (1- arg))))
	      ;; The logic of this is the same as the loop above,
	      ;; it just goes in the other direction.
	      (while (and (< arg 0) (not done))
		;; For completely consistency with the forward-motion
		;; case, we should call beginning-of-line here.
		;; However, if point is inside a field and on a
		;; continued line, the call to (vertical-motion -1)
		;; below won't move us back far enough; then we return
		;; to the same column in line-move-finish, and point
		;; gets stuck -- cyd
		(forward-line 0)
		(cond
		 ((bobp)
		  (if (not noerror)
		      (signal 'beginning-of-buffer nil)
		    (setq done t)))
		 ((and (< arg -1) ;; Use vertical-motion for last move
		       (not (integerp selective-display))
		       (not (invisible-p (1- (point)))))
		  (forward-line -1))
		 ((zerop (vertical-motion -1))
		  (if (not noerror)
		      (signal 'beginning-of-buffer nil)
		    (setq done t))))
		(unless done
		  (setq arg (1+ arg))
		  (while (and ;; Don't move over previous invis lines
			  ;; if our target is the middle of this line.
			  (or (zerop (or goal-column temporary-goal-column))
			      (< arg 0))
			  (not (bobp)) (invisible-p (1- (point))))
		    (goto-char (previous-char-property-change (point))))))))
	  ;; This is the value the function returns.
	  (= arg 0))

      (cond ((> arg 0)
	     ;; If we did not move down as far as desired, at least go
	     ;; to end of line.  Be sure to call point-entered and
	     ;; point-left-hooks.
	     (let* ((npoint (prog1 (line-end-position)
			      (goto-char opoint)))
		    (inhibit-point-motion-hooks nil))
	       (goto-char npoint)))
	    ((< arg 0)
	     ;; If we did not move up as far as desired,
	     ;; at least go to beginning of line.
	     (let* ((npoint (prog1 (line-beginning-position)
			      (goto-char opoint)))
		    (inhibit-point-motion-hooks nil))
	       (goto-char npoint)))
	    (t
	     (line-move-finish (or goal-column temporary-goal-column)
			       opoint (> orig-arg 0)))))))

(defun line-move-finish (column opoint forward)
  (let ((repeat t))
    (while repeat
      ;; Set REPEAT to t to repeat the whole thing.
      (setq repeat nil)

      (let (new
	    (old (point))
	    (line-beg (line-beginning-position))
	    (line-end
	     ;; Compute the end of the line
	     ;; ignoring effectively invisible newlines.
	     (save-excursion
	       ;; Like end-of-line but ignores fields.
	       (skip-chars-forward "^\n")
	       (while (and (not (eobp)) (invisible-p (point)))
		 (goto-char (next-char-property-change (point)))
		 (skip-chars-forward "^\n"))
	       (point))))

	;; Move to the desired column.
	(line-move-to-column (truncate column))

	;; Corner case: suppose we start out in a field boundary in
	;; the middle of a continued line.  When we get to
	;; line-move-finish, point is at the start of a new *screen*
	;; line but the same text line; then line-move-to-column would
	;; move us backwards. Test using C-n with point on the "x" in
	;;   (insert "a" (propertize "x" 'field t) (make-string 89 ?y))
	(and forward
	     (< (point) old)
	     (goto-char old))

	(setq new (point))

	;; Process intangibility within a line.
	;; With inhibit-point-motion-hooks bound to nil, a call to
	;; goto-char moves point past intangible text.

	;; However, inhibit-point-motion-hooks controls both the
	;; intangibility and the point-entered/point-left hooks.  The
	;; following hack avoids calling the point-* hooks
	;; unnecessarily.  Note that we move *forward* past intangible
	;; text when the initial and final points are the same.
	(goto-char new)
	(let ((inhibit-point-motion-hooks nil))
	  (goto-char new)

	  ;; If intangibility moves us to a different (later) place
	  ;; in the same line, use that as the destination.
	  (if (<= (point) line-end)
	      (setq new (point))
	    ;; If that position is "too late",
	    ;; try the previous allowable position.
	    ;; See if it is ok.
	    (backward-char)
	    (if (if forward
		    ;; If going forward, don't accept the previous
		    ;; allowable position if it is before the target line.
		    (< line-beg (point))
		  ;; If going backward, don't accept the previous
		  ;; allowable position if it is still after the target line.
		  (<= (point) line-end))
		(setq new (point))
	      ;; As a last resort, use the end of the line.
	      (setq new line-end))))

	;; Now move to the updated destination, processing fields
	;; as well as intangibility.
	(goto-char opoint)
	(let ((inhibit-point-motion-hooks nil))
	  (goto-char
	   ;; Ignore field boundaries if the initial and final
	   ;; positions have the same `field' property, even if the
	   ;; fields are non-contiguous.  This seems to be "nicer"
	   ;; behavior in many situations.
	   (if (eq (get-char-property new 'field)
	   	   (get-char-property opoint 'field))
	       new
	     (constrain-to-field new opoint t t
				 'inhibit-line-move-field-capture))))

	;; If all this moved us to a different line,
	;; retry everything within that new line.
	(when (or (< (point) line-beg) (> (point) line-end))
	  ;; Repeat the intangibility and field processing.
	  (setq repeat t))))))

(defun line-move-to-column (col)
  "Try to find column COL, considering invisibility.
This function works only in certain cases,
because what we really need is for `move-to-column'
and `current-column' to be able to ignore invisible text."
  (if (zerop col)
      (beginning-of-line)
    (move-to-column col))

  (when (and line-move-ignore-invisible
	     (not (bolp)) (invisible-p (1- (point))))
    (let ((normal-location (point))
	  (normal-column (current-column)))
      ;; If the following character is currently invisible,
      ;; skip all characters with that same `invisible' property value.
      (while (and (not (eobp))
		  (invisible-p (point)))
	(goto-char (next-char-property-change (point))))
      ;; Have we advanced to a larger column position?
      (if (> (current-column) normal-column)
	  ;; We have made some progress towards the desired column.
	  ;; See if we can make any further progress.
	  (line-move-to-column (+ (current-column) (- col normal-column)))
	;; Otherwise, go to the place we originally found
	;; and move back over invisible text.
	;; that will get us to the same place on the screen
	;; but with a more reasonable buffer position.
	(goto-char normal-location)
	(let ((line-beg (line-beginning-position)))
	  (while (and (not (bolp)) (invisible-p (1- (point))))
	    (goto-char (previous-char-property-change (point) line-beg))))))))

(defun move-end-of-line (arg)
  "Move point to end of current line as displayed.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

To ignore the effects of the `intangible' text or overlay
property, bind `inhibit-point-motion-hooks' to t.
If there is an image in the current line, this function
disregards newlines that are part of the text on which the image
rests."
  (interactive "^p")
  (or arg (setq arg 1))
  (let (done)
    (while (not done)
      (let ((newpos
	     (save-excursion
	       (let ((goal-column 0)
		     (line-move-visual nil))
		 (and (line-move arg t)
		      ;; With bidi reordering, we may not be at bol,
		      ;; so make sure we are.
		      (skip-chars-backward "^\n")
		      (not (bobp))
		      (progn
			(while (and (not (bobp)) (invisible-p (1- (point))))
			  (goto-char (previous-single-char-property-change
                                      (point) 'invisible)))
			(backward-char 1)))
		 (point)))))
	(goto-char newpos)
	(if (and (> (point) newpos)
		 (eq (preceding-char) ?\n))
	    (backward-char 1)
	  (if (and (> (point) newpos) (not (eobp))
		   (not (eq (following-char) ?\n)))
	      ;; If we skipped something intangible and now we're not
	      ;; really at eol, keep going.
	      (setq arg 1)
	    (setq done t)))))))

(defun move-beginning-of-line (arg)
  "Move point to beginning of current line as displayed.
\(If there's an image in the line, this disregards newlines
which are part of the text that the image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (interactive "^p")
  (or arg (setq arg 1))

  (let ((orig (point))
	first-vis first-vis-field-value)

    ;; Move by lines, if ARG is not 1 (the default).
    (if (/= arg 1)
	(let ((line-move-visual nil))
	  (line-move (1- arg) t)))

    ;; Move to beginning-of-line, ignoring fields and invisible text.
    (skip-chars-backward "^\n")
    (while (and (not (bobp)) (invisible-p (1- (point))))
      (goto-char (previous-char-property-change (point)))
      (skip-chars-backward "^\n"))

    ;; Now find first visible char in the line
    (while (and (not (eobp)) (invisible-p (point)))
      (goto-char (next-char-property-change (point))))
    (setq first-vis (point))

    ;; See if fields would stop us from reaching FIRST-VIS.
    (setq first-vis-field-value
	  (constrain-to-field first-vis orig (/= arg 1) t nil))

    (goto-char (if (/= first-vis-field-value first-vis)
		   ;; If yes, obey them.
		   first-vis-field-value
		 ;; Otherwise, move to START with attention to fields.
		 ;; (It is possible that fields never matter in this case.)
		 (constrain-to-field (point) orig
				     (/= arg 1) t nil)))))


;; Many people have said they rarely use this feature, and often type
;; it by accident.  Maybe it shouldn't even be on a key.
(put 'set-goal-column 'disabled t)

(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument ARG, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion.
The goal column is stored in the variable `goal-column'."
  (interactive "P")
  (if arg
      (progn
        (setq goal-column nil)
        (message "No goal column"))
    (setq goal-column (current-column))
    ;; The older method below can be erroneous if `set-goal-column' is bound
    ;; to a sequence containing %
    ;;(message (substitute-command-keys
    ;;"Goal column %d (use \\[set-goal-column] with an arg to unset it)")
    ;;goal-column)
    (message "%s"
	     (concat
	      (format "Goal column %d " goal-column)
	      (substitute-command-keys
	       "(use \\[set-goal-column] with an arg to unset it)")))

    )
  nil)

;;; Editing based on visual lines, as opposed to logical lines.

(defun end-of-visual-line (&optional n)
  "Move point to end of current visual line.
With argument N not nil or 1, move forward N - 1 visual lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (interactive "^p")
  (or n (setq n 1))
  (if (/= n 1)
      (let ((line-move-visual t))
	(line-move (1- n) t)))
  ;; Unlike `move-beginning-of-line', `move-end-of-line' doesn't
  ;; constrain to field boundaries, so we don't either.
  (vertical-motion (cons (window-width) 0)))

(defun beginning-of-visual-line (&optional n)
  "Move point to beginning of current visual line.
With argument N not nil or 1, move forward N - 1 visual lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (if (/= n 1)
	(let ((line-move-visual t))
	  (line-move (1- n) t)))
    (vertical-motion 0)
    ;; Constrain to field boundaries, like `move-beginning-of-line'.
    (goto-char (constrain-to-field (point) opoint (/= n 1)))))

(defun kill-visual-line (&optional arg)
  "Kill the rest of the visual line.
With prefix argument ARG, kill that many visual lines from point.
If ARG is negative, kill visual lines backward.
If ARG is zero, kill the text before point on the current visual
line.

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  ;; Like in `kill-line', it's better to move point to the other end
  ;; of the kill before killing.
  (let ((opoint (point))
	(kill-whole-line (and kill-whole-line (bolp))))
    (if arg
	(vertical-motion (prefix-numeric-value arg))
      (end-of-visual-line 1)
      (if (= (point) opoint)
	  (vertical-motion 1)
	;; Skip any trailing whitespace at the end of the visual line.
	;; We used to do this only if `show-trailing-whitespace' is
	;; nil, but that's wrong; the correct thing would be to check
	;; whether the trailing whitespace is highlighted.  But, it's
	;; OK to just do this unconditionally.
	(skip-chars-forward " \t")))
    (kill-region opoint (if (and kill-whole-line (looking-at "\n"))
			    (1+ (point))
			  (point)))))

(defun next-logical-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines.
This is identical to `next-line', except that it always moves
by logical lines instead of visual lines, ignoring the value of
the variable `line-move-visual'."
  (interactive "^p\np")
  (let ((line-move-visual nil))
    (with-no-warnings
      (next-line arg try-vscroll))))

(defun previous-logical-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines.
This is identical to `previous-line', except that it always moves
by logical lines instead of visual lines, ignoring the value of
the variable `line-move-visual'."
  (interactive "^p\np")
  (let ((line-move-visual nil))
    (with-no-warnings
      (previous-line arg try-vscroll))))

(defgroup visual-line nil
  "Editing based on visual lines."
  :group 'convenience
  :version "23.1")

(defvar visual-line-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kill-line] 'kill-visual-line)
    (define-key map [remap move-beginning-of-line] 'beginning-of-visual-line)
    (define-key map [remap move-end-of-line]  'end-of-visual-line)
    ;; These keybindings interfere with xterm function keys.  Are
    ;; there any other suitable bindings?
    ;; (define-key map "\M-[" 'previous-logical-line)
    ;; (define-key map "\M-]" 'next-logical-line)
    map))

(defcustom visual-line-fringe-indicators '(nil nil)
  "How fringe indicators are shown for wrapped lines in `visual-line-mode'.
The value should be a list of the form (LEFT RIGHT), where LEFT
and RIGHT are symbols representing the bitmaps to display, to
indicate wrapped lines, in the left and right fringes respectively.
See also `fringe-indicator-alist'.
The default is not to display fringe indicators for wrapped lines.
This variable does not affect fringe indicators displayed for
other purposes."
  :type '(list (choice (const :tag "Hide left indicator" nil)
		       (const :tag "Left curly arrow" left-curly-arrow)
		       (symbol :tag "Other bitmap"))
	       (choice (const :tag "Hide right indicator" nil)
		       (const :tag "Right curly arrow" right-curly-arrow)
		       (symbol :tag "Other bitmap")))
  :set (lambda (symbol value)
	 (dolist (buf (buffer-list))
	   (with-current-buffer buf
	     (when (and (boundp 'visual-line-mode)
			(symbol-value 'visual-line-mode))
	       (setq fringe-indicator-alist
		     (cons (cons 'continuation value)
			   (assq-delete-all
			    'continuation
			    (copy-tree fringe-indicator-alist)))))))
	 (set-default symbol value)))

(defvar visual-line--saved-state nil)

(define-minor-mode visual-line-mode
  "Toggle visual line based editing (Visual Line mode).
With a prefix argument ARG, enable Visual Line mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Visual Line mode is enabled, `word-wrap' is turned on in
this buffer, and simple editing commands are redefined to act on
visual lines, not logical lines.  See Info node `Visual Line
Mode' for details."
  :keymap visual-line-mode-map
  :group 'visual-line
  :lighter " Wrap"
  (if visual-line-mode
      (progn
	(set (make-local-variable 'visual-line--saved-state) nil)
	;; Save the local values of some variables, to be restored if
	;; visual-line-mode is turned off.
	(dolist (var '(line-move-visual truncate-lines
		       truncate-partial-width-windows
		       word-wrap fringe-indicator-alist))
	  (if (local-variable-p var)
	      (push (cons var (symbol-value var))
		    visual-line--saved-state)))
	(set (make-local-variable 'line-move-visual) t)
	(set (make-local-variable 'truncate-partial-width-windows) nil)
	(setq truncate-lines nil
	      word-wrap t
	      fringe-indicator-alist
	      (cons (cons 'continuation visual-line-fringe-indicators)
		    fringe-indicator-alist)))
    (kill-local-variable 'line-move-visual)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'truncate-partial-width-windows)
    (kill-local-variable 'fringe-indicator-alist)
    (dolist (saved visual-line--saved-state)
      (set (make-local-variable (car saved)) (cdr saved)))
    (kill-local-variable 'visual-line--saved-state)))

(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(define-globalized-minor-mode global-visual-line-mode
  visual-line-mode turn-on-visual-line-mode
  :lighter " vl")


(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(defun transpose-words (arg)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  ;; FIXME: `foo a!nd bar' should transpose into `bar and foo'.
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr
   (lambda (arg)
     ;; Here we should try to simulate the behavior of
     ;; (cons (progn (forward-sexp x) (point))
     ;;       (progn (forward-sexp (- x)) (point)))
     ;; Except that we don't want to rely on the second forward-sexp
     ;; putting us back to where we want to be, since forward-sexp-function
     ;; might do funny things like infix-precedence.
     (if (if (> arg 0)
	     (looking-at "\\sw\\|\\s_")
	   (and (not (bobp))
		(save-excursion (forward-char -1) (looking-at "\\sw\\|\\s_"))))
	 ;; Jumping over a symbol.  We might be inside it, mind you.
	 (progn (funcall (if (> arg 0)
			     'skip-syntax-backward 'skip-syntax-forward)
			 "w_")
		(cons (save-excursion (forward-sexp arg) (point)) (point)))
       ;; Otherwise, we're between sexps.  Take a step back before jumping
       ;; to make sure we'll obey the same precedence no matter which direction
       ;; we're going.
       (funcall (if (> arg 0) 'skip-syntax-backward 'skip-syntax-forward) " .")
       (cons (save-excursion (forward-sexp arg) (point))
	     (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			       (not (zerop (funcall (if (> arg 0)
							'skip-syntax-forward
						      'skip-syntax-backward)
						    ".")))))
		    (point)))))
   arg 'special))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (> arg 0)
			 (progn
			   ;; Move forward over ARG lines,
			   ;; but create newlines if necessary.
			   (setq arg (forward-line arg))
			   (if (/= (preceding-char) ?\n)
			       (setq arg (1+ arg)))
			   (if (> arg 0)
			       (newline arg)))
		       (forward-line arg))))
		  arg))

;; FIXME seems to leave point BEFORE the current object when ARG = 0,
;; which seems inconsistent with the ARG /= 0 case.
;; FIXME document SPECIAL.
(defun transpose-subr (mover arg &optional special)
  "Subroutine to do the work of transposing objects.
Works for lines, sentences, paragraphs, etc.  MOVER is a function that
moves forward by units of the given object (e.g. forward-sentence,
forward-paragraph).  If ARG is zero, exchanges the current object
with the one containing mark.  If ARG is an integer, moves the
current object past ARG following (if ARG is positive) or
preceding (if ARG is negative) objects, leaving point after the
current object."
  (let ((aux (if special mover
	       (lambda (x)
		 (cons (progn (funcall mover x) (point))
		       (progn (funcall mover (- x)) (point))))))
	pos1 pos2)
    (cond
     ((= arg 0)
      (save-excursion
	(setq pos1 (funcall aux 1))
	(goto-char (or (mark) (error "No mark set in this buffer")))
	(setq pos2 (funcall aux 1))
	(transpose-subr-1 pos1 pos2))
      (exchange-point-and-mark))
     ((> arg 0)
      (setq pos1 (funcall aux -1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)
      (goto-char (car pos2)))
     (t
      (setq pos1 (funcall aux -1))
      (goto-char (car pos1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)))))

(defun transpose-subr-1 (pos1 pos2)
  (when (> (car pos1) (cdr pos1)) (setq pos1 (cons (cdr pos1) (car pos1))))
  (when (> (car pos2) (cdr pos2)) (setq pos2 (cons (cdr pos2) (car pos2))))
  (when (> (car pos1) (car pos2))
    (let ((swap pos1))
      (setq pos1 pos2 pos2 swap)))
  (if (> (cdr pos1) (car pos2)) (error "Don't have two things to transpose"))
  (atomic-change-group
   (let (word2)
     ;; FIXME: We first delete the two pieces of text, so markers that
     ;; used to point to after the text end up pointing to before it :-(
     (setq word2 (delete-and-extract-region (car pos2) (cdr pos2)))
     (goto-char (car pos2))
     (insert (delete-and-extract-region (car pos1) (cdr pos1)))
     (goto-char (car pos1))
     (insert word2))))

(defun backward-word (&optional arg)
  "Move backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "^p")
  (forward-word (- (or arg 1))))

(defun mark-word (&optional arg allow-extend)
  "Set mark ARG words away from point.
The place mark goes is the same place \\[forward-word] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG words after the ones already marked."
  (interactive "P\np")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (region-active-p)))
	 (setq arg (if arg (prefix-numeric-value arg)
		     (if (< (mark) (point)) -1 1)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-word arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (forward-word (prefix-numeric-value arg))
	    (point))
	  nil t))))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-word (- arg)))

(defun current-word (&optional strict really-word)
  "Return the symbol or word that point is on (or a nearby one) as a string.
The return value includes no text properties.
If optional arg STRICT is non-nil, return nil unless point is within
or adjacent to a symbol or word.  In all cases the value can be nil
if there is no word nearby.
The function, belying its name, normally finds a symbol.
If optional arg REALLY-WORD is non-nil, it finds just a word."
  (save-excursion
    (let* ((oldpoint (point)) (start (point)) (end (point))
	   (syntaxes (if really-word "w" "w_"))
	   (not-syntaxes (concat "^" syntaxes)))
      (skip-syntax-backward syntaxes) (setq start (point))
      (goto-char oldpoint)
      (skip-syntax-forward syntaxes) (setq end (point))
      (when (and (eq start oldpoint) (eq end oldpoint)
		 ;; Point is neither within nor adjacent to a word.
		 (not strict))
	;; Look for preceding word in same line.
	(skip-syntax-backward not-syntaxes (line-beginning-position))
	(if (bolp)
	    ;; No preceding word in same line.
	    ;; Look for following word in same line.
	    (progn
	      (skip-syntax-forward not-syntaxes (line-end-position))
	      (setq start (point))
	      (skip-syntax-forward syntaxes)
	      (setq end (point)))
	  (setq end (point))
	  (skip-syntax-backward syntaxes)
	  (setq start (point))))
      ;; If we found something nonempty, return it as a string.
      (unless (= start end)
	(buffer-substring-no-properties start end)))))

(defcustom fill-prefix nil
  "String for filling to insert at front of new line, or nil for none."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill)
(make-variable-buffer-local 'fill-prefix)
(put 'fill-prefix 'safe-local-variable 'string-or-null-p)

(defcustom auto-fill-inhibit-regexp nil
  "Regexp to match lines which should not be auto-filled."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'fill)

(defun do-auto-fill ()
  "The default value for `normal-auto-fill-function'.
This is the default auto-fill function, some major modes use a different one.
Returns t if it really did any work."
  (let (fc justify give-up
	   (fill-prefix fill-prefix))
    (if (or (not (setq justify (current-justification)))
	    (null (setq fc (current-fill-column)))
	    (and (eq justify 'left)
		 (<= (current-column) fc))
	    (and auto-fill-inhibit-regexp
		 (save-excursion (beginning-of-line)
				 (looking-at auto-fill-inhibit-regexp))))
	nil ;; Auto-filling not required
      (if (memq justify '(full center right))
	  (save-excursion (unjustify-current-line)))

      ;; Choose a fill-prefix automatically.
      (when (and adaptive-fill-mode
		 (or (null fill-prefix) (string= fill-prefix "")))
	(let ((prefix
	       (fill-context-prefix
		(save-excursion (fill-forward-paragraph -1) (point))
		(save-excursion (fill-forward-paragraph 1) (point)))))
	  (and prefix (not (equal prefix ""))
	       ;; Use auto-indentation rather than a guessed empty prefix.
	       (not (and fill-indent-according-to-mode
			 (string-match "\\`[ \t]*\\'" prefix)))
	       (setq fill-prefix prefix))))

      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (after-prefix
	       (fill-point
		(save-excursion
		  (beginning-of-line)
		  (setq after-prefix (point))
		  (and fill-prefix
		       (looking-at (regexp-quote fill-prefix))
		       (setq after-prefix (match-end 0)))
		  (move-to-column (1+ fc))
		  (fill-move-to-break-point after-prefix)
		  (point))))

	  ;; See whether the place we found is any good.
	  (if (save-excursion
		(goto-char fill-point)
		(or (bolp)
		    ;; There is no use breaking at end of line.
		    (save-excursion (skip-chars-forward " ") (eolp))
		    ;; It is futile to split at the end of the prefix
		    ;; since we would just insert the prefix again.
		    (and after-prefix (<= (point) after-prefix))
		    ;; Don't split right after a comment starter
		    ;; since we would just make another comment starter.
		    (and comment-start-skip
			 (let ((limit (point)))
			   (beginning-of-line)
			   (and (re-search-forward comment-start-skip
						   limit t)
				(eq (point) limit))))))
	      ;; No good place to break => stop trying.
	      (setq give-up t)
	    ;; Ok, we have a useful place to break the line.  Do it.
	    (let ((prev-column (current-column)))
	      ;; If point is at the fill-point, do not `save-excursion'.
	      ;; Otherwise, if a comment prefix or fill-prefix is inserted,
	      ;; point will end up before it rather than after it.
	      (if (save-excursion
		    (skip-chars-backward " \t")
		    (= (point) fill-point))
		  (default-indent-new-line t)
		(save-excursion
		  (goto-char fill-point)
		  (default-indent-new-line t)))
	      ;; Now do justification, if required
	      (if (not (eq justify 'left))
		  (save-excursion
		    (end-of-line 0)
		    (justify-current-line justify nil t)))
	      ;; If making the new line didn't reduce the hpos of
	      ;; the end of the line, then give up now;
	      ;; trying again will not help.
	      (if (>= (current-column) prev-column)
		  (setq give-up t))))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))

(defvar comment-line-break-function 'comment-indent-new-line
  "*Mode-specific function which line breaks and continues a comment.
This function is called during auto-filling when a comment syntax
is defined.
The function should take a single optional argument, which is a flag
indicating whether it should use soft newlines.")

(defun default-indent-new-line (&optional soft)
  "Break line at point and indent.
If a comment syntax is defined, call `comment-indent-new-line'.

The inserted newline is marked hard if variable `use-hard-newlines' is true,
unless optional argument SOFT is non-nil."
  (interactive)
  (if comment-start
      (funcall comment-line-break-function soft)
    ;; Insert the newline before removing empty space so that markers
    ;; get preserved better.
    (if soft (insert-and-inherit ?\n) (newline 1))
    (save-excursion (forward-char -1) (delete-horizontal-space))
    (delete-horizontal-space)

    (if (and fill-prefix (not adaptive-fill-mode))
	;; Blindly trust a non-adaptive fill-prefix.
	(progn
	  (indent-to-left-margin)
	  (insert-before-markers-and-inherit fill-prefix))

      (cond
       ;; If there's an adaptive prefix, use it unless we're inside
       ;; a comment and the prefix is not a comment starter.
       (fill-prefix
	(indent-to-left-margin)
	(insert-and-inherit fill-prefix))
       ;; If we're not inside a comment, just try to indent.
       (t (indent-according-to-mode))))))

(defvar normal-auto-fill-function 'do-auto-fill
  "The function to use for `auto-fill-function' if Auto Fill mode is turned on.
Some major modes set this.")

(put 'auto-fill-function :minor-mode-function 'auto-fill-mode)
;; `functions' and `hooks' are usually unsafe to set, but setting
;; auto-fill-function to nil in a file-local setting is safe and
;; can be useful to prevent auto-filling.
(put 'auto-fill-function 'safe-local-variable 'null)

(define-minor-mode auto-fill-mode
  "Toggle automatic line breaking (Auto Fill mode).
With a prefix argument ARG, enable Auto Fill mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Auto Fill mode is enabled, inserting a space at a column
beyond `current-fill-column' automatically breaks the line at a
previous space.

When `auto-fill-mode' is on, the `auto-fill-function' variable is
non-`nil'.

The value of `normal-auto-fill-function' specifies the function to use
for `auto-fill-function' when turning Auto Fill mode on."
  :variable (eq auto-fill-function normal-auto-fill-function))

;; This holds a document string used to document auto-fill-mode.
(defun auto-fill-function ()
  "Automatically break line at a previous space, in insertion of text."
  nil)

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun turn-off-auto-fill ()
  "Unconditionally turn off Auto Fill mode."
  (auto-fill-mode -1))

(custom-add-option 'text-mode-hook 'turn-on-auto-fill)

(defun set-fill-column (arg)
  "Set `fill-column' to specified argument.
Use \\[universal-argument] followed by a number to specify a column.
Just \\[universal-argument] as argument means to use the current column."
  (interactive
   (list (or current-prefix-arg
             ;; We used to use current-column silently, but C-x f is too easily
             ;; typed as a typo for C-x C-f, so we turned it into an error and
             ;; now an interactive prompt.
             (read-number "Set fill-column to: " (current-column)))))
  (if (consp arg)
      (setq arg (current-column)))
  (if (not (integerp arg))
      ;; Disallow missing argument; it's probably a typo for C-x C-f.
      (error "set-fill-column requires an explicit argument")
    (message "Fill column set to %d (was %d)" arg fill-column)
    (setq fill-column arg)))

(defun set-selective-display (arg)
  "Set `selective-display' to ARG; clear it if no arg.
When the value of `selective-display' is a number > 0,
lines whose indentation is >= that value are not displayed.
The variable `selective-display' has a separate value for each buffer."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (let ((current-vpos
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (goto-char (window-start))
	   (vertical-motion (window-height)))))
    (setq selective-display
	  (and arg (prefix-numeric-value arg)))
    (recenter current-vpos))
  (set-window-start (selected-window) (window-start (selected-window)))
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

(defvaralias 'indicate-unused-lines 'indicate-empty-lines)

(defun toggle-truncate-lines (&optional arg)
  "Toggle truncating of long lines for the current buffer.
When truncating is off, long lines are folded.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise fold them.  Note that in side-by-side windows, this
command has no effect if `truncate-partial-width-windows' is
non-nil."
  (interactive "P")
  (setq truncate-lines
	(if (null arg)
	    (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
		      (if (eq buffer (window-buffer window))
			  (set-window-hscroll window 0)))
		    nil t)))
  (message "Truncate long lines %s"
	   (if truncate-lines "enabled" "disabled")))

(defun toggle-word-wrap (&optional arg)
  "Toggle whether to use word-wrapping for continuation lines.
With prefix argument ARG, wrap continuation lines at word boundaries
if ARG is positive, otherwise wrap them at the right screen edge.
This command toggles the value of `word-wrap'.  It has no effect
if long lines are truncated."
  (interactive "P")
  (setq word-wrap
	(if (null arg)
	    (not word-wrap)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (message "Word wrapping %s"
	   (if word-wrap "enabled" "disabled")))

(defvar overwrite-mode-textual (purecopy " Ovwrt")
  "The string displayed in the mode line when in overwrite mode.")
(defvar overwrite-mode-binary (purecopy " Bin Ovwrt")
  "The string displayed in the mode line when in binary overwrite mode.")

(define-minor-mode overwrite-mode
  "Toggle Overwrite mode.
With a prefix argument ARG, enable Overwrite mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Overwrite mode is enabled, printing characters typed in
replace existing text on a one-for-one basis, rather than pushing
it to the right.  At the end of a line, such characters extend
the line.  Before a tab, such characters insert until the tab is
filled in.  \\[quoted-insert] still inserts characters in
overwrite mode; this is supposed to make it easier to insert
characters when necessary."
  :variable (eq overwrite-mode 'overwrite-mode-textual))

(define-minor-mode binary-overwrite-mode
  "Toggle Binary Overwrite mode.
With a prefix argument ARG, enable Binary Overwrite mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Binary Overwrite mode is enabled, printing characters typed
in replace existing text.  Newlines are not treated specially, so
typing at the end of a line joins the line to the next, with the
typed character between them.  Typing before a tab character
simply replaces the tab with the character typed.
\\[quoted-insert] replaces the text at the cursor, just as
ordinary typing characters do.

Note that Binary Overwrite mode is not its own minor mode; it is
a specialization of overwrite mode, entered by setting the
`overwrite-mode' variable to `overwrite-mode-binary'."
  :variable (eq overwrite-mode 'overwrite-mode-binary))

(define-minor-mode line-number-mode
  "Toggle line number display in the mode line (Line Number mode).
With a prefix argument ARG, enable Line Number mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Line numbers do not appear for very large buffers and buffers
with very long lines; see variables `line-number-display-limit'
and `line-number-display-limit-width'."
  :init-value t :global t :group 'mode-line)

(define-minor-mode column-number-mode
  "Toggle column number display in the mode line (Column Number mode).
With a prefix argument ARG, enable Column Number mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil."
  :global t :group 'mode-line)

(define-minor-mode size-indication-mode
  "Toggle buffer size display in the mode line (Size Indication mode).
With a prefix argument ARG, enable Size Indication mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil."
  :global t :group 'mode-line)

(define-minor-mode auto-save-mode
  "Toggle auto-saving in the current buffer (Auto Save mode).
With a prefix argument ARG, enable Auto Save mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil."
  :variable ((and buffer-auto-save-file-name
                  ;; If auto-save is off because buffer has shrunk,
                  ;; then toggling should turn it on.
                  (>= buffer-saved-size 0))
             . (lambda (val)
                 (setq buffer-auto-save-file-name
                       (cond
                        ((null val) nil)
                        ((and buffer-file-name auto-save-visited-file-name
                              (not buffer-read-only))
                         buffer-file-name)
                        (t (make-auto-save-file-name))))))
  ;; If -1 was stored here, to temporarily turn off saving,
  ;; turn it back on.
  (and (< buffer-saved-size 0)
       (setq buffer-saved-size 0)))

(defgroup paren-blinking nil
  "Blinking matching of parens and expressions."
  :prefix "blink-matching-"
  :group 'paren-matching)

(defcustom blink-matching-paren t
  "Non-nil means show matching open-paren when close-paren is inserted."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-on-screen t
  "Non-nil means show matching open-paren when it is on screen.
If nil, don't show it (but the open-paren can still be shown
when it is off screen).

This variable has no effect if `blink-matching-paren' is nil.
\(In that case, the open-paren is never shown.)
It is also ignored if `show-paren-mode' is enabled."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-distance (* 100 1024)
  "If non-nil, maximum distance to search backwards for matching open-paren.
If nil, search stops at the beginning of the accessible portion of the buffer."
  :version "23.2"                       ; 25->100k
  :type '(choice (const nil) integer)
  :group 'paren-blinking)

(defcustom blink-matching-delay 1
  "Time in seconds to delay after showing a matching paren."
  :type 'number
  :group 'paren-blinking)

(defcustom blink-matching-paren-dont-ignore-comments nil
  "If nil, `blink-matching-paren' ignores comments.
More precisely, when looking for the matching parenthesis,
it skips the contents of comments that end before point."
  :type 'boolean
  :group 'paren-blinking)

(defun blink-matching-check-mismatch (start end)
  "Return whether or not START...END are matching parens.
END is the current point and START is the blink position.
START might be nil if no matching starter was found.
Returns non-nil if we find there is a mismatch."
  (let* ((end-syntax (syntax-after (1- end)))
         (matching-paren (and (consp end-syntax)
                              (eq (syntax-class end-syntax) 5)
                              (cdr end-syntax))))
    ;; For self-matched chars like " and $, we can't know when they're
    ;; mismatched or unmatched, so we can only do it for parens.
    (when matching-paren
      (not (and start
                (or
                 (eq (char-after start) matching-paren)
                 ;; The cdr might hold a new paren-class info rather than
                 ;; a matching-char info, in which case the two CDRs
                 ;; should match.
                 (eq matching-paren (cdr-safe (syntax-after start)))))))))

(defvar blink-matching-check-function #'blink-matching-check-mismatch
  "Function to check parentheses mismatches.
The function takes two arguments (START and END) where START is the
position just before the opening token and END is the position right after.
START can be nil, if it was not found.
The function should return non-nil if the two tokens do not match.")

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive)
  (when (and (not (bobp))
	     blink-matching-paren)
    (let* ((oldpos (point))
	   (message-log-max nil) ; Don't log messages about paren matching.
	   (blinkpos
            (save-excursion
              (save-restriction
                (if blink-matching-paren-distance
                    (narrow-to-region
                     (max (minibuffer-prompt-end) ;(point-min) unless minibuf.
                          (- (point) blink-matching-paren-distance))
                     oldpos))
                (let ((parse-sexp-ignore-comments
                       (and parse-sexp-ignore-comments
                            (not blink-matching-paren-dont-ignore-comments))))
                  (condition-case ()
                      (progn
                        (forward-sexp -1)
                        ;; backward-sexp skips backward over prefix chars,
                        ;; so move back to the matching paren.
                        (while (and (< (point) (1- oldpos))
                                    (let ((code (syntax-after (point))))
                                      (or (eq (syntax-class code) 6)
                                          (eq (logand 1048576 (car code))
                                              1048576))))
                          (forward-char 1))
                        (point))
                    (error nil))))))
           (mismatch (funcall blink-matching-check-function blinkpos oldpos)))
      (cond
       (mismatch
        (if blinkpos
            (if (minibufferp)
                (minibuffer-message "Mismatched parentheses")
              (message "Mismatched parentheses"))
          (if (minibufferp)
              (minibuffer-message "No matching parenthesis found")
            (message "No matching parenthesis found"))))
       ((not blinkpos) nil)
       ((pos-visible-in-window-p blinkpos)
        ;; Matching open within window, temporarily move to blinkpos but only
        ;; if `blink-matching-paren-on-screen' is non-nil.
        (and blink-matching-paren-on-screen
             (not show-paren-mode)
             (save-excursion
               (goto-char blinkpos)
               (sit-for blink-matching-delay))))
       (t
        (save-excursion
          (goto-char blinkpos)
          (let ((open-paren-line-string
                 ;; Show what precedes the open in its line, if anything.
                 (cond
                  ((save-excursion (skip-chars-backward " \t") (not (bolp)))
                   (buffer-substring (line-beginning-position)
                                     (1+ blinkpos)))
                  ;; Show what follows the open in its line, if anything.
                  ((save-excursion
                     (forward-char 1)
                     (skip-chars-forward " \t")
                     (not (eolp)))
                   (buffer-substring blinkpos
                                     (line-end-position)))
                  ;; Otherwise show the previous nonblank line,
                  ;; if there is one.
                  ((save-excursion (skip-chars-backward "\n \t") (not (bobp)))
                   (concat
                    (buffer-substring (progn
                                        (skip-chars-backward "\n \t")
                                        (line-beginning-position))
                                      (progn (end-of-line)
                                             (skip-chars-backward " \t")
                                             (point)))
                    ;; Replace the newline and other whitespace with `...'.
                    "..."
                    (buffer-substring blinkpos (1+ blinkpos))))
                  ;; There is nothing to show except the char itself.
                  (t (buffer-substring blinkpos (1+ blinkpos))))))
            (message "Matches %s"
                     (substring-no-properties open-paren-line-string)))))))))

(defvar blink-paren-function 'blink-matching-open
  "Function called, if non-nil, whenever a close parenthesis is inserted.
More precisely, a char with closeparen syntax is self-inserted.")

(defun blink-paren-post-self-insert-function ()
  (when (and (eq (char-before) last-command-event) ; Sanity check.
             (memq (char-syntax last-command-event) '(?\) ?\$))
             blink-paren-function
             (not executing-kbd-macro)
             (not noninteractive)
	     ;; Verify an even number of quoting characters precede the close.
	     (= 1 (logand 1 (- (point)
			       (save-excursion
				 (forward-char -1)
				 (skip-syntax-backward "/\\")
				 (point))))))
    (funcall blink-paren-function)))

(add-hook 'post-self-insert-hook #'blink-paren-post-self-insert-function
          ;; Most likely, this hook is nil, so this arg doesn't matter,
          ;; but I use it as a reminder that this function usually
          ;; likes to be run after others since it does `sit-for'.
          'append)

;; This executes C-g typed while Emacs is waiting for a command.
;; Quitting out of a program does not go through here;
;; that happens in the QUIT macro at the C code level.
(defun keyboard-quit ()
  "Signal a `quit' condition.
During execution of Lisp code, this character causes a quit directly.
At top-level, as an editor command, this simply beeps."
  (interactive)
  ;; Avoid adding the region to the window selection.
  (setq saved-region-selection nil)
  (let (select-active-regions)
    (deactivate-mark))
  (if (fboundp 'kmacro-keyboard-quit)
      (kmacro-keyboard-quit))
  (setq defining-kbd-macro nil)
  (let ((debug-on-quit nil))
    (signal 'quit nil)))

(defvar buffer-quit-function nil
  "Function to call to \"quit\" the current buffer, or nil if none.
\\[keyboard-escape-quit] calls this function when its more local actions
\(such as canceling a prefix argument, minibuffer or region) do not apply.")

(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	((not (one-window-p t))
	 (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

(defun play-sound-file (file &optional volume device)
  "Play sound stored in FILE.
VOLUME and DEVICE correspond to the keywords of the sound
specification for `play-sound'."
  (interactive "fPlay sound file: ")
  (let ((sound (list :file file)))
    (if volume
	(plist-put sound :volume volume))
    (if device
	(plist-put sound :device device))
    (push 'sound sound)
    (play-sound sound)))


(defcustom read-mail-command 'rmail
  "Your preference for a mail reading package.
This is used by some keybindings which support reading mail.
See also `mail-user-agent' concerning sending mail."
  :type '(radio (function-item :tag "Rmail" :format "%t\n" rmail)
                (function-item :tag "Gnus" :format "%t\n" gnus)
                (function-item :tag "Emacs interface to MH"
                               :format "%t\n" mh-rmail)
                (function :tag "Other"))
  :version "21.1"
  :group 'mail)

(defcustom mail-user-agent 'message-user-agent
  "Your preference for a mail composition package.
Various Emacs Lisp packages (e.g. Reporter) require you to compose an
outgoing email message.  This variable lets you specify which
mail-sending package you prefer.

Valid values include:

  `message-user-agent'  -- use the Message package.
                           See Info node `(message)'.
  `sendmail-user-agent' -- use the Mail package.
                           See Info node `(emacs)Sending Mail'.
  `mh-e-user-agent'     -- use the Emacs interface to the MH mail system.
                           See Info node `(mh-e)'.
  `gnus-user-agent'     -- like `message-user-agent', but with Gnus
                           paraphernalia if Gnus is running, particularly
                           the Gcc: header for archiving.

Additional valid symbols may be available; check with the author of
your package for details.  The function should return non-nil if it
succeeds.

See also `read-mail-command' concerning reading mail."
  :type '(radio (function-item :tag "Message package"
			       :format "%t\n"
			       message-user-agent)
		(function-item :tag "Mail package"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Emacs interface to MH"
			       :format "%t\n"
			       mh-e-user-agent)
		(function-item :tag "Message with full Gnus features"
			       :format "%t\n"
			       gnus-user-agent)
		(function :tag "Other"))
  :version "23.2"                       ; sendmail->message
  :group 'mail)

(defcustom compose-mail-user-agent-warnings t
  "If non-nil, `compose-mail' warns about changes in `mail-user-agent'.
If the value of `mail-user-agent' is the default, and the user
appears to have customizations applying to the old default,
`compose-mail' issues a warning."
  :type 'boolean
  :version "23.2"
  :group 'mail)

(defun rfc822-goto-eoh ()
  "If the buffer starts with a mail header, move point to the header's end.
Otherwise, moves to `point-min'.
The end of the header is the start of the next line, if there is one,
else the end of the last line.  This function obeys RFC822."
  (goto-char (point-min))
  (when (re-search-forward
	 "^\\([:\n]\\|[^: \t\n]+[ \t\n]\\)" nil 'move)
    (goto-char (match-beginning 0))))

;; Used by Rmail (e.g., rmail-forward).
(defvar mail-encode-mml nil
  "If non-nil, mail-user-agent's `sendfunc' command should mml-encode
the outgoing message before sending it.")

(defun compose-mail (&optional to subject other-headers continue
		     switch-function yank-action send-actions
		     return-action)
  "Start composing a mail message to send.
This uses the user's chosen mail composition package
as selected with the variable `mail-user-agent'.
The optional arguments TO and SUBJECT specify recipients
and the initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, if non-nil, says to continue editing a message already
being composed.  Interactively, CONTINUE is the prefix argument.

SWITCH-FUNCTION, if non-nil, is a function to use to
switch to and display the buffer used for mail composition.

YANK-ACTION, if non-nil, is an action to perform, if and when necessary,
to insert the raw text of the message being replied to.
It has the form (FUNCTION . ARGS).  The user agent will apply
FUNCTION to ARGS, to insert the raw text of the original message.
\(The user agent will also run `mail-citation-hook', *after* the
original text has been inserted in this way.)

SEND-ACTIONS is a list of actions to call when the message is sent.
Each action has the form (FUNCTION . ARGS).

RETURN-ACTION, if non-nil, is an action for returning to the
caller.  It has the form (FUNCTION . ARGS).  The function is
called after the mail has been sent or put aside, and the mail
buffer buried."
  (interactive
   (list nil nil nil current-prefix-arg))

  ;; In Emacs 23.2, the default value of `mail-user-agent' changed
  ;; from sendmail-user-agent to message-user-agent.  Some users may
  ;; encounter incompatibilities.  This hack tries to detect problems
  ;; and warn about them.
  (and compose-mail-user-agent-warnings
       (eq mail-user-agent 'message-user-agent)
       (let (warn-vars)
	 (dolist (var '(mail-mode-hook mail-send-hook mail-setup-hook
			mail-yank-hooks mail-archive-file-name
			mail-default-reply-to mail-mailing-lists
			mail-self-blind))
	   (and (boundp var)
		(symbol-value var)
		(push var warn-vars)))
	 (when warn-vars
	   (display-warning 'mail
			    (format "\
The default mail mode is now Message mode.
You have the following Mail mode variable%s customized:
\n  %s\n\nTo use Mail mode, set `mail-user-agent' to sendmail-user-agent.
To disable this warning, set `compose-mail-user-agent-warnings' to nil."
				    (if (> (length warn-vars) 1) "s" "")
				    (mapconcat 'symbol-name
					       warn-vars " "))))))

  (let ((function (get mail-user-agent 'composefunc)))
    (funcall function to subject other-headers continue switch-function
	     yank-action send-actions return-action)))

(defun compose-mail-other-window (&optional to subject other-headers continue
					    yank-action send-actions
					    return-action)
  "Like \\[compose-mail], but edit the outgoing message in another window."
  (interactive (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-window yank-action send-actions
		return-action))

(defun compose-mail-other-frame (&optional to subject other-headers continue
					    yank-action send-actions
					    return-action)
  "Like \\[compose-mail], but edit the outgoing message in another frame."
  (interactive (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-frame yank-action send-actions
		return-action))


(defvar set-variable-value-history nil
  "History of values entered with `set-variable'.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defun set-variable (variable value &optional make-local)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
VARIABLE should be a user option variable name, a Lisp variable
meant to be customized by users.  You should enter VALUE in Lisp syntax,
so if you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read VALUE.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive
   (let* ((default-var (variable-at-point))
          (var (if (user-variable-p default-var)
		   (read-variable (format "Set variable (default %s): " default-var)
				  default-var)
		 (read-variable "Set variable: ")))
	  (minibuffer-help-form '(describe-variable var))
	  (prop (get var 'variable-interactive))
          (obsolete (car (get var 'byte-obsolete-variable)))
	  (prompt (format "Set %s %s to value: " var
			  (cond ((local-variable-p var)
				 "(buffer-local)")
				((or current-prefix-arg
				     (local-variable-if-set-p var))
				 "buffer-locally")
				(t "globally"))))
	  (val (progn
                 (when obsolete
                   (message (concat "`%S' is obsolete; "
                                    (if (symbolp obsolete) "use `%S' instead" "%s"))
                            var obsolete)
                   (sit-for 3))
                 (if prop
                     ;; Use VAR's `variable-interactive' property
                     ;; as an interactive spec for prompting.
                     (call-interactively `(lambda (arg)
                                            (interactive ,prop)
                                            arg))
                   (read
                    (read-string prompt nil
                                 'set-variable-value-history
				 (format "%S" (symbol-value var))))))))
     (list var val current-prefix-arg)))

  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (let ((type (get variable 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match value)
	(error "Value `%S' does not match type %S of %S"
	       value (car type) variable))))

  (if make-local
      (make-local-variable variable))

  (set variable value)

  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

;; Define the major mode for lists of completions.

(defvar completion-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'mouse-choose-completion)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [down-mouse-2] nil)
    (define-key map "\C-m" 'choose-completion)
    (define-key map "\e\e\e" 'delete-completion-window)
    (define-key map [left] 'previous-completion)
    (define-key map [right] 'next-completion)
    (define-key map "q" 'quit-window)
    (define-key map "z" 'kill-this-buffer)
    map)
  "Local map for completion list buffers.")

;; Completion mode is suitable only for specially formatted data.
(put 'completion-list-mode 'mode-class 'special)

(defvar completion-reference-buffer nil
  "Record the buffer that was current when the completion list was requested.
This is a local variable in the completion list buffer.
Initial value is nil to avoid some compiler warnings.")

(defvar completion-no-auto-exit nil
  "Non-nil means `choose-completion-string' should never exit the minibuffer.
This also applies to other functions such as `choose-completion'.")

(defvar completion-base-position nil
  "Position of the base of the text corresponding to the shown completions.
This variable is used in the *Completions* buffers.
Its value is a list of the form (START END) where START is the place
where the completion should be inserted and END (if non-nil) is the end
of the text to replace.  If END is nil, point is used instead.")

(defvar completion-list-insert-choice-function #'completion--replace
  "Function to use to insert the text chosen in *Completions*.
Called with three arguments (BEG END TEXT), it should replace the text
between BEG and END with TEXT.  Expected to be set buffer-locally
in the *Completions* buffer.")

(defvar completion-base-size nil
  "Number of chars before point not involved in completion.
This is a local variable in the completion list buffer.
It refers to the chars in the minibuffer if completing in the
minibuffer, or in `completion-reference-buffer' otherwise.
Only characters in the field at point are included.

If nil, Emacs determines which part of the tail end of the
buffer's text is involved in completion by comparing the text
directly.")
(make-obsolete-variable 'completion-base-size 'completion-base-position "23.2")

(defun delete-completion-window ()
  "Delete the completion list window.
Go to the window from which completion was requested."
  (interactive)
  (let ((buf completion-reference-buffer))
    (if (one-window-p t)
	(if (window-dedicated-p (selected-window))
	    (delete-frame (selected-frame)))
      (delete-window (selected-window))
      (if (get-buffer-window buf)
	  (select-window (get-buffer-window buf))))))

(defun previous-completion (n)
  "Move to the previous item in the completion list."
  (interactive "p")
  (next-completion (- n)))

(defun next-completion (n)
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (let ((beg (point-min)) (end (point-max)))
    (while (and (> n 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'mouse-face)
	(goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'mouse-face)
	(goto-char (next-single-property-change (point) 'mouse-face nil end)))
      (setq n (1- n)))
    (while (and (< n 0) (not (bobp)))
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
	;; If in a completion, move to the start of it.
	(when (and prop (eq prop (get-text-property (point) 'mouse-face)))
	  (goto-char (previous-single-property-change
		      (point) 'mouse-face nil beg)))
	;; Move to end of the previous completion.
	(unless (or (bobp) (get-text-property (1- (point)) 'mouse-face))
	  (goto-char (previous-single-property-change
		      (point) 'mouse-face nil beg)))
	;; Move to the start of that one.
	(goto-char (previous-single-property-change
		    (point) 'mouse-face nil beg))
	(setq n (1+ n))))))

(defun choose-completion (&optional event)
  "Choose the completion at point."
  (interactive (list last-nonmenu-event))
  ;; In case this is run via the mouse, give temporary modes such as
  ;; isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (let ((buffer completion-reference-buffer)
          (base-size completion-base-size)
          (base-position completion-base-position)
          (insert-function completion-list-insert-choice-function)
          (choice
           (save-excursion
             (goto-char (posn-point (event-start event)))
             (let (beg end)
               (cond
                ((and (not (eobp)) (get-text-property (point) 'mouse-face))
                 (setq end (point) beg (1+ (point))))
                ((and (not (bobp))
                      (get-text-property (1- (point)) 'mouse-face))
                 (setq end (1- (point)) beg (point)))
                (t (error "No completion here")))
               (setq beg (previous-single-property-change beg 'mouse-face))
               (setq end (or (next-single-property-change end 'mouse-face)
                             (point-max)))
               (buffer-substring-no-properties beg end))))
          (owindow (selected-window)))

      (unless (buffer-live-p buffer)
        (error "Destination buffer is dead"))
      (select-window (posn-window (event-start event)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window
       (or (get-buffer-window buffer 0)
           owindow))

      (with-current-buffer buffer
        (choose-completion-string
         choice buffer
         (or base-position
             (when base-size
               ;; Someone's using old completion code that doesn't know
               ;; about base-position yet.
               (list (+ base-size (field-beginning))))
             ;; If all else fails, just guess.
             (list (choose-completion-guess-base-position choice)))
         insert-function)))))

;; Delete the longest partial match for STRING
;; that can be found before POINT.
(defun choose-completion-guess-base-position (string)
  (save-excursion
    (let ((opoint (point))
          len)
      ;; Try moving back by the length of the string.
      (goto-char (max (- (point) (length string))
                      (minibuffer-prompt-end)))
      ;; See how far back we were actually able to move.  That is the
      ;; upper bound on how much we can match and delete.
      (setq len (- opoint (point)))
      (if completion-ignore-case
          (setq string (downcase string)))
      (while (and (> len 0)
                  (let ((tail (buffer-substring (point) opoint)))
                    (if completion-ignore-case
                        (setq tail (downcase tail)))
                    (not (string= tail (substring string 0 len)))))
        (setq len (1- len))
        (forward-char 1))
      (point))))

(defun choose-completion-delete-max-match (string)
  (delete-region (choose-completion-guess-base-position string) (point)))
(make-obsolete 'choose-completion-delete-max-match
               'choose-completion-guess-base-position "23.2")

(defvar choose-completion-string-functions nil
  "Functions that may override the normal insertion of a completion choice.
These functions are called in order with four arguments:
CHOICE - the string to insert in the buffer,
BUFFER - the buffer in which the choice should be inserted,
MINI-P - non-nil if BUFFER is a minibuffer, and
BASE-SIZE - the number of characters in BUFFER before
the string being completed.

If a function in the list returns non-nil, that function is supposed
to have inserted the CHOICE in the BUFFER, and possibly exited
the minibuffer; no further functions will be called.

If all functions in the list return nil, that means to use
the default method of inserting the completion in BUFFER.")

(defun choose-completion-string (choice &optional
                                        buffer base-position insert-function)
  "Switch to BUFFER and insert the completion choice CHOICE.
BASE-POSITION, says where to insert the completion."

  ;; If BUFFER is the minibuffer, exit the minibuffer
  ;; unless it is reading a file name and CHOICE is a directory,
  ;; or completion-no-auto-exit is non-nil.

  ;; Some older code may call us passing `base-size' instead of
  ;; `base-position'.  It's difficult to make any use of `base-size',
  ;; so we just ignore it.
  (unless (consp base-position)
    (message "Obsolete `base-size' passed to choose-completion-string")
    (setq base-position nil))

  (let* ((buffer (or buffer completion-reference-buffer))
	 (mini-p (minibufferp buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and mini-p
             (not (and (active-minibuffer-window)
                       (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Set buffer so buffer-local choose-completion-string-functions works.
      (set-buffer buffer)
      (unless (run-hook-with-args-until-success
	       'choose-completion-string-functions
               ;; The fourth arg used to be `mini-p' but was useless
               ;; (since minibufferp can be used on the `buffer' arg)
               ;; and indeed unused.  The last used to be `base-size', so we
               ;; keep it to try and avoid breaking old code.
	       choice buffer base-position nil)
        ;; This remove-text-properties should be unnecessary since `choice'
        ;; comes from buffer-substring-no-properties.
        ;;(remove-text-properties 0 (length choice) '(mouse-face nil) choice)
	;; Insert the completion into the buffer where it was requested.
        (funcall (or insert-function completion-list-insert-choice-function)
                 (or (car base-position) (point))
                 (or (cadr base-position) (point))
                 choice)
        ;; Update point in the window that BUFFER is showing in.
	(let ((window (get-buffer-window buffer t)))
	  (set-window-point window (point)))
	;; If completing for the minibuffer, exit it with this choice.
	(and (not completion-no-auto-exit)
             (minibufferp buffer)
	     minibuffer-completion-table
	     ;; If this is reading a file name, and the file name chosen
	     ;; is a directory, don't exit the minibuffer.
             (let* ((result (buffer-substring (field-beginning) (point)))
                    (bounds
                     (completion-boundaries result minibuffer-completion-table
                                            minibuffer-completion-predicate
                                            "")))
               (if (eq (car bounds) (length result))
                   ;; The completion chosen leads to a new set of completions
                   ;; (e.g. it's a directory): don't exit the minibuffer yet.
                   (let ((mini (active-minibuffer-window)))
                     (select-window mini)
                     (when minibuffer-auto-raise
                       (raise-frame (window-frame mini))))
                 (exit-minibuffer))))))))

(define-derived-mode completion-list-mode nil "Completion List"
  "Major mode for buffers showing lists of possible completions.
Type \\<completion-list-mode-map>\\[choose-completion] in the completion list\
 to select the completion near point.
Use \\<completion-list-mode-map>\\[mouse-choose-completion] to select one\
 with the mouse.

\\{completion-list-mode-map}"
  (set (make-local-variable 'completion-base-size) nil))

(defun completion-list-mode-finish ()
  "Finish setup of the completions buffer.
Called from `temp-buffer-show-hook'."
  (when (eq major-mode 'completion-list-mode)
    (toggle-read-only 1)))

(add-hook 'temp-buffer-show-hook 'completion-list-mode-finish)


;; Variables and faces used in `completion-setup-function'.

(defcustom completion-show-help t
  "Non-nil means show help message in *Completions* buffer."
  :type 'boolean
  :version "22.1"
  :group 'completion)

;; This function goes in completion-setup-hook, so that it is called
;; after the text of the completion list buffer is written.
(defun completion-setup-function ()
  (let* ((mainbuf (current-buffer))
         (base-dir
          ;; When reading a file name in the minibuffer,
          ;; try and find the right default-directory to set in the
          ;; completion list buffer.
          ;; FIXME: Why do we do that, actually?  --Stef
          (if minibuffer-completing-file-name
              (file-name-as-directory
               (expand-file-name
                (substring (minibuffer-completion-contents)
                           0 (or completion-base-size 0)))))))
    (with-current-buffer standard-output
      (let ((base-size completion-base-size) ;Read before killing localvars.
            (base-position completion-base-position)
            (insert-fun completion-list-insert-choice-function))
        (completion-list-mode)
        (set (make-local-variable 'completion-base-size) base-size)
        (set (make-local-variable 'completion-base-position) base-position)
        (set (make-local-variable 'completion-list-insert-choice-function)
	     insert-fun))
      (set (make-local-variable 'completion-reference-buffer) mainbuf)
      (if base-dir (setq default-directory base-dir))
      ;; Maybe insert help string.
      (when completion-show-help
	(goto-char (point-min))
	(if (display-mouse-p)
	    (insert (substitute-command-keys
		     "Click \\[mouse-choose-completion] on a completion to select it.\n")))
	(insert (substitute-command-keys
		 "In this buffer, type \\[choose-completion] to \
select the completion near point.\n\n"))))))

(add-hook 'completion-setup-hook 'completion-setup-function)

(define-key minibuffer-local-completion-map [prior] 'switch-to-completions)
(define-key minibuffer-local-completion-map "\M-v"  'switch-to-completions)

(defun switch-to-completions ()
  "Select the completion list window."
  (interactive)
  (let ((window (or (get-buffer-window "*Completions*" 0)
		    ;; Make sure we have a completions window.
                    (progn (minibuffer-completion-help)
                           (get-buffer-window "*Completions*" 0)))))
    (when window
      (select-window window)
      ;; In the new buffer, go to the first completion.
      ;; FIXME: Perhaps this should be done in `minibuffer-completion-help'.
      (when (bobp)
	(next-completion 1)))))

;;; Support keyboard commands to turn on various modifiers.

;; These functions -- which are not commands -- each add one modifier
;; to the following event.

(defun event-apply-alt-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Alt modifier to the following event.
For example, type \\[event-apply-alt-modifier] & to enter Alt-&."
  (vector (event-apply-modifier (read-event) 'alt 22 "A-")))
(defun event-apply-super-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Super modifier to the following event.
For example, type \\[event-apply-super-modifier] & to enter Super-&."
  (vector (event-apply-modifier (read-event) 'super 23 "s-")))
(defun event-apply-hyper-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Hyper modifier to the following event.
For example, type \\[event-apply-hyper-modifier] & to enter Hyper-&."
  (vector (event-apply-modifier (read-event) 'hyper 24 "H-")))
(defun event-apply-shift-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Shift modifier to the following event.
For example, type \\[event-apply-shift-modifier] & to enter Shift-&."
  (vector (event-apply-modifier (read-event) 'shift 25 "S-")))
(defun event-apply-control-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Ctrl modifier to the following event.
For example, type \\[event-apply-control-modifier] & to enter Ctrl-&."
  (vector (event-apply-modifier (read-event) 'control 26 "C-")))
(defun event-apply-meta-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Meta modifier to the following event.
For example, type \\[event-apply-meta-modifier] & to enter Meta-&."
  (vector (event-apply-modifier (read-event) 'meta 27 "M-")))

(defun event-apply-modifier (event symbol lshiftby prefix)
  "Apply a modifier flag to event EVENT.
SYMBOL is the name of this modifier, as a symbol.
LSHIFTBY is the numeric value of this modifier, in keyboard events.
PREFIX is the string that represents this modifier in an event type symbol."
  (if (numberp event)
      (cond ((eq symbol 'control)
	     (if (and (<= (downcase event) ?z)
		      (>= (downcase event) ?a))
		 (- (downcase event) ?a -1)
	       (if (and (<= (downcase event) ?Z)
			(>= (downcase event) ?A))
		   (- (downcase event) ?A -1)
		 (logior (lsh 1 lshiftby) event))))
	    ((eq symbol 'shift)
	     (if (and (<= (downcase event) ?z)
		      (>= (downcase event) ?a))
		 (upcase event)
	       (logior (lsh 1 lshiftby) event)))
	    (t
	     (logior (lsh 1 lshiftby) event)))
    (if (memq symbol (event-modifiers event))
	event
      (let ((event-type (if (symbolp event) event (car event))))
	(setq event-type (intern (concat prefix (symbol-name event-type))))
	(if (symbolp event)
	    event-type
	  (cons event-type (cdr event)))))))

(define-key function-key-map [?\C-x ?@ ?h] 'event-apply-hyper-modifier)
(define-key function-key-map [?\C-x ?@ ?s] 'event-apply-super-modifier)
(define-key function-key-map [?\C-x ?@ ?m] 'event-apply-meta-modifier)
(define-key function-key-map [?\C-x ?@ ?a] 'event-apply-alt-modifier)
(define-key function-key-map [?\C-x ?@ ?S] 'event-apply-shift-modifier)
(define-key function-key-map [?\C-x ?@ ?c] 'event-apply-control-modifier)

;;;; Keypad support.

;; Make the keypad keys act like ordinary typing keys.  If people add
;; bindings for the function key symbols, then those bindings will
;; override these, so this shouldn't interfere with any existing
;; bindings.

;; Also tell read-char how to handle these keys.
(mapc
 (lambda (keypad-normal)
   (let ((keypad (nth 0 keypad-normal))
	 (normal (nth 1 keypad-normal)))
     (put keypad 'ascii-character normal)
     (define-key function-key-map (vector keypad) (vector normal))))
 '((kp-0 ?0) (kp-1 ?1) (kp-2 ?2) (kp-3 ?3) (kp-4 ?4)
   (kp-5 ?5) (kp-6 ?6) (kp-7 ?7) (kp-8 ?8) (kp-9 ?9)
   (kp-space ?\s)
   (kp-tab ?\t)
   (kp-enter ?\r)
   (kp-multiply ?*)
   (kp-add ?+)
   (kp-separator ?,)
   (kp-subtract ?-)
   (kp-decimal ?.)
   (kp-divide ?/)
   (kp-equal ?=)
   ;; Do the same for various keys that are represented as symbols under
   ;; GUIs but naturally correspond to characters.
   (backspace 127)
   (delete 127)
   (tab ?\t)
   (linefeed ?\n)
   (clear ?\C-l)
   (return ?\C-m)
   (escape ?\e)
   ))

;;;;
;;;; forking a twin copy of a buffer.
;;;;

(defvar clone-buffer-hook nil
  "Normal hook to run in the new buffer at the end of `clone-buffer'.")

(defvar clone-indirect-buffer-hook nil
  "Normal hook to run in the new buffer at the end of `clone-indirect-buffer'.")

(defun clone-process (process &optional newname)
  "Create a twin copy of PROCESS.
If NEWNAME is nil, it defaults to PROCESS' name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.
If PROCESS is associated with a buffer, the new process will be associated
  with the current buffer instead.
Returns nil if PROCESS has already terminated."
  (setq newname (or newname (process-name process)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (when (memq (process-status process) '(run stop open))
    (let* ((process-connection-type (process-tty-name process))
	   (new-process
	    (if (memq (process-status process) '(open))
		(let ((args (process-contact process t)))
		  (setq args (plist-put args :name newname))
		  (setq args (plist-put args :buffer
					(if (process-buffer process)
					    (current-buffer))))
		  (apply 'make-network-process args))
	      (apply 'start-process newname
		     (if (process-buffer process) (current-buffer))
		     (process-command process)))))
      (set-process-query-on-exit-flag
       new-process (process-query-on-exit-flag process))
      (set-process-inherit-coding-system-flag
       new-process (process-inherit-coding-system-flag process))
      (set-process-filter new-process (process-filter process))
      (set-process-sentinel new-process (process-sentinel process))
      (set-process-plist new-process (copy-sequence (process-plist process)))
      new-process)))

;; things to maybe add (currently partly covered by `funcall mode'):
;; - syntax-table
;; - overlays
(defun clone-buffer (&optional newname display-flag)
  "Create and return a twin copy of the current buffer.
Unlike an indirect buffer, the new buffer can be edited
independently of the old one (if it is not read-only).
NEWNAME is the name of the new buffer.  It may be modified by
adding or incrementing <N> at the end as necessary to create a
unique buffer name.  If nil, it defaults to the name of the
current buffer, with the proper suffix.  If DISPLAY-FLAG is
non-nil, the new buffer is shown with `pop-to-buffer'.  Trying to
clone a file-visiting buffer, or a buffer whose major mode symbol
has a non-nil `no-clone' property, results in an error.

Interactively, DISPLAY-FLAG is t and NEWNAME is the name of the
current buffer with appropriate suffix.  However, if a prefix
argument is given, then the command prompts for NEWNAME in the
minibuffer.

This runs the normal hook `clone-buffer-hook' in the new buffer
after it has been set up properly in other respects."
  (interactive
   (progn
     (if buffer-file-name
	 (error "Cannot clone a file-visiting buffer"))
     (if (get major-mode 'no-clone)
	 (error "Cannot clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
	       (read-buffer "Name of new cloned buffer: " (current-buffer)))
	   t)))
  (if buffer-file-name
      (error "Cannot clone a file-visiting buffer"))
  (if (get major-mode 'no-clone)
      (error "Cannot clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let ((buf (current-buffer))
	(ptmin (point-min))
	(ptmax (point-max))
	(pt (point))
	(mk (if mark-active (mark t)))
	(modified (buffer-modified-p))
	(mode major-mode)
	(lvars (buffer-local-variables))
	(process (get-buffer-process (current-buffer)))
	(new (generate-new-buffer (or newname (buffer-name)))))
    (save-restriction
      (widen)
      (with-current-buffer new
	(insert-buffer-substring buf)))
    (with-current-buffer new
      (narrow-to-region ptmin ptmax)
      (goto-char pt)
      (if mk (set-mark mk))
      (set-buffer-modified-p modified)

      ;; Clone the old buffer's process, if any.
      (when process (clone-process process))

      ;; Now set up the major mode.
      (funcall mode)

      ;; Set up other local variables.
      (mapc (lambda (v)
	      (condition-case ()	;in case var is read-only
		  (if (symbolp v)
		      (makunbound v)
		    (set (make-local-variable (car v)) (cdr v)))
		(error nil)))
	    lvars)

      ;; Run any hooks (typically set up by the major mode
      ;; for cloning to work properly).
      (run-hooks 'clone-buffer-hook))
    (if display-flag
        ;; Presumably the current buffer is shown in the selected frame, so
        ;; we want to display the clone elsewhere.
        (let ((same-window-regexps nil)
              (same-window-buffer-names))
          (pop-to-buffer new)))
    new))


(defun clone-indirect-buffer (newname display-flag &optional norecord)
  "Create an indirect buffer that is a twin copy of the current buffer.

Give the indirect buffer name NEWNAME.  Interactively, read NEWNAME
from the minibuffer when invoked with a prefix arg.  If NEWNAME is nil
or if not called with a prefix arg, NEWNAME defaults to the current
buffer's name.  The name is modified by adding a `<N>' suffix to it
or by incrementing the N in an existing suffix.  Trying to clone a
buffer whose major mode symbol has a non-nil `no-clone-indirect'
property results in an error.

DISPLAY-FLAG non-nil means show the new buffer with `pop-to-buffer'.
This is always done when called interactively.

Optional third arg NORECORD non-nil means do not put this buffer at the
front of the list of recently selected ones."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
	 (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
	       (read-buffer "Name of indirect buffer: " (current-buffer)))
	   t)))
  (if (get major-mode 'no-clone-indirect)
      (error "Cannot indirectly clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let* ((name (generate-new-buffer-name newname))
	 (buffer (make-indirect-buffer (current-buffer) name t)))
    (with-current-buffer buffer
      (run-hooks 'clone-indirect-buffer-hook))
    (when display-flag
      (pop-to-buffer buffer norecord))
    buffer))


(defun clone-indirect-buffer-other-window (newname display-flag &optional norecord)
  "Like `clone-indirect-buffer' but display in another window."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
	 (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
	       (read-buffer "Name of indirect buffer: " (current-buffer)))
	   t)))
  (let ((pop-up-windows t))
    (clone-indirect-buffer newname display-flag norecord)))


;;; Handling of Backspace and Delete keys.

(defcustom normal-erase-is-backspace 'maybe
  "Set the default behavior of the Delete and Backspace keys.

If set to t, Delete key deletes forward and Backspace key deletes
backward.

If set to nil, both Delete and Backspace keys delete backward.

If set to 'maybe (which is the default), Emacs automatically
selects a behavior.  On window systems, the behavior depends on
the keyboard used.  If the keyboard has both a Backspace key and
a Delete key, and both are mapped to their usual meanings, the
option's default value is set to t, so that Backspace can be used
to delete backward, and Delete can be used to delete forward.

If not running under a window system, customizing this option
accomplishes a similar effect by mapping C-h, which is usually
generated by the Backspace key, to DEL, and by mapping DEL to C-d
via `keyboard-translate'.  The former functionality of C-h is
available on the F1 key.  You should probably not use this
setting if you don't have both Backspace, Delete and F1 keys.

Setting this variable with setq doesn't take effect.  Programmatically,
call `normal-erase-is-backspace-mode' (which see) instead."
  :type '(choice (const :tag "Off" nil)
		 (const :tag "Maybe" maybe)
		 (other :tag "On" t))
  :group 'editing-basics
  :version "21.1"
  :set (lambda (symbol value)
	 ;; The fboundp is because of a problem with :set when
	 ;; dumping Emacs.  It doesn't really matter.
	 (if (fboundp 'normal-erase-is-backspace-mode)
	     (normal-erase-is-backspace-mode (or value 0))
	   (set-default symbol value))))

(defun normal-erase-is-backspace-setup-frame (&optional frame)
  "Set up `normal-erase-is-backspace-mode' on FRAME, if necessary."
  (unless frame (setq frame (selected-frame)))
  (with-selected-frame frame
    (unless (terminal-parameter nil 'normal-erase-is-backspace)
      (normal-erase-is-backspace-mode
       (if (if (eq normal-erase-is-backspace 'maybe)
               (and (not noninteractive)
                    (or (memq system-type '(ms-dos windows-nt))
			(memq window-system '(ns))
                        (and (memq window-system '(x))
                             (fboundp 'x-backspace-delete-keys-p)
                             (x-backspace-delete-keys-p))
                        ;; If the terminal Emacs is running on has erase char
                        ;; set to ^H, use the Backspace key for deleting
                        ;; backward, and the Delete key for deleting forward.
                        (and (null window-system)
                             (eq tty-erase-char ?\^H))))
             normal-erase-is-backspace)
           1 0)))))

(define-minor-mode normal-erase-is-backspace-mode
  "Toggle the Erase and Delete mode of the Backspace and Delete keys.
With a prefix argument ARG, enable this feature if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

On window systems, when this mode is on, Delete is mapped to C-d
and Backspace is mapped to DEL; when this mode is off, both
Delete and Backspace are mapped to DEL.  (The remapping goes via
`local-function-key-map', so binding Delete or Backspace in the
global or local keymap will override that.)

In addition, on window systems, the bindings of C-Delete, M-Delete,
C-M-Delete, C-Backspace, M-Backspace, and C-M-Backspace are changed in
the global keymap in accordance with the functionality of Delete and
Backspace.  For example, if Delete is remapped to C-d, which deletes
forward, C-Delete is bound to `kill-word', but if Delete is remapped
to DEL, which deletes backward, C-Delete is bound to
`backward-kill-word'.

If not running on a window system, a similar effect is accomplished by
remapping C-h (normally produced by the Backspace key) and DEL via
`keyboard-translate': if this mode is on, C-h is mapped to DEL and DEL
to C-d; if it's off, the keys are not remapped.

When not running on a window system, and this mode is turned on, the
former functionality of C-h is available on the F1 key.  You should
probably not turn on this mode on a text-only terminal if you don't
have both Backspace, Delete and F1 keys.

See also `normal-erase-is-backspace'."
  :variable (eq (terminal-parameter
                 nil 'normal-erase-is-backspace) 1)
  (let ((enabled (eq 1 (terminal-parameter
                        nil 'normal-erase-is-backspace))))

    (cond ((or (memq window-system '(x w32 ns pc))
	       (memq system-type '(ms-dos windows-nt)))
	   (let ((bindings
		  `(([M-delete] [M-backspace])
		    ([C-M-delete] [C-M-backspace])
		    ([?\e C-delete] [?\e C-backspace]))))

	     (if enabled
		 (progn
		   (define-key local-function-key-map [delete] [deletechar])
		   (define-key local-function-key-map [kp-delete] [?\C-d])
		   (define-key local-function-key-map [backspace] [?\C-?])
                   (dolist (b bindings)
                     ;; Not sure if input-decode-map is really right, but
                     ;; keyboard-translate-table (used below) only works
                     ;; for integer events, and key-translation-table is
                     ;; global (like the global-map, used earlier).
                     (define-key input-decode-map (car b) nil)
                     (define-key input-decode-map (cadr b) nil)))
	       (define-key local-function-key-map [delete] [?\C-?])
	       (define-key local-function-key-map [kp-delete] [?\C-?])
	       (define-key local-function-key-map [backspace] [?\C-?])
               (dolist (b bindings)
                 (define-key input-decode-map (car b) (cadr b))
                 (define-key input-decode-map (cadr b) (car b))))))
	  (t
	   (if enabled
	       (progn
		 (keyboard-translate ?\C-h ?\C-?)
		 (keyboard-translate ?\C-? ?\C-d))
	     (keyboard-translate ?\C-h ?\C-h)
	     (keyboard-translate ?\C-? ?\C-?))))

    (if (called-interactively-p 'interactive)
	(message "Delete key deletes %s"
		 (if (eq 1 (terminal-parameter nil 'normal-erase-is-backspace))
		     "forward" "backward")))))

(defvar vis-mode-saved-buffer-invisibility-spec nil
  "Saved value of `buffer-invisibility-spec' when Visible mode is on.")

(define-minor-mode visible-mode
  "Toggle making all invisible text temporarily visible (Visible mode).
With a prefix argument ARG, enable Visible mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This mode works by saving the value of `buffer-invisibility-spec'
and setting it to nil."
  :lighter " Vis"
  :group 'editing-basics
  (when (local-variable-p 'vis-mode-saved-buffer-invisibility-spec)
    (setq buffer-invisibility-spec vis-mode-saved-buffer-invisibility-spec)
    (kill-local-variable 'vis-mode-saved-buffer-invisibility-spec))
  (when visible-mode
    (set (make-local-variable 'vis-mode-saved-buffer-invisibility-spec)
	 buffer-invisibility-spec)
    (setq buffer-invisibility-spec nil)))

;; Minibuffer prompt stuff.

;;(defun minibuffer-prompt-modification (start end)
;;  (error "You cannot modify the prompt"))
;;
;;
;;(defun minibuffer-prompt-insertion (start end)
;;  (let ((inhibit-modification-hooks t))
;;    (delete-region start end)
;;    ;; Discard undo information for the text insertion itself
;;    ;; and for the text deletion.above.
;;    (when (consp buffer-undo-list)
;;      (setq buffer-undo-list (cddr buffer-undo-list)))
;;    (message "You cannot modify the prompt")))
;;
;;
;;(setq minibuffer-prompt-properties
;;  (list 'modification-hooks '(minibuffer-prompt-modification)
;;	'insert-in-front-hooks '(minibuffer-prompt-insertion)))


;;;; Problematic external packages.

;; rms says this should be done by specifying symbols that define
;; versions together with bad values.  This is therefore not as
;; flexible as it could be.  See the thread:
;; http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg00300.html
(defconst bad-packages-alist
  ;; Not sure exactly which semantic versions have problems.
  ;; Definitely 2.0pre3, probably all 2.0pre's before this.
  '((semantic semantic-version "\\`2\\.0pre[1-3]\\'"
              "The version of `semantic' loaded does not work in Emacs 22.
It can cause constant high CPU load.
Upgrade to at least Semantic 2.0pre4 (distributed with CEDET 1.0pre4).")
    ;; CUA-mode does not work with GNU Emacs version 22.1 and newer.
    ;; Except for version 1.2, all of the 1.x and 2.x version of cua-mode
    ;; provided the `CUA-mode' feature.  Since this is no longer true,
    ;; we can warn the user if the `CUA-mode' feature is ever provided.
    (CUA-mode t nil
"CUA-mode is now part of the standard GNU Emacs distribution,
so you can now enable CUA via the Options menu or by customizing `cua-mode'.

You have loaded an older version of CUA-mode which does not work
correctly with this version of Emacs.  You should remove the old
version and use the one distributed with Emacs."))
  "Alist of packages known to cause problems in this version of Emacs.
Each element has the form (PACKAGE SYMBOL REGEXP STRING).
PACKAGE is either a regular expression to match file names, or a
symbol (a feature name); see the documentation of
`after-load-alist', to which this variable adds functions.
SYMBOL is either the name of a string variable, or `t'.  Upon
loading PACKAGE, if SYMBOL is t or matches REGEXP, display a
warning using STRING as the message.")

(defun bad-package-check (package)
  "Run a check using the element from `bad-packages-alist' matching PACKAGE."
  (condition-case nil
      (let* ((list (assoc package bad-packages-alist))
             (symbol (nth 1 list)))
        (and list
             (boundp symbol)
             (or (eq symbol t)
                 (and (stringp (setq symbol (eval symbol)))
                      (string-match-p (nth 2 list) symbol)))
             (display-warning package (nth 3 list) :warning)))
    (error nil)))

(mapc (lambda (elem)
        (eval-after-load (car elem) `(bad-package-check ',(car elem))))
      bad-packages-alist)


(provide 'simple)

;;; simple.el ends here
