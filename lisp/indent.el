;;; indent.el --- indentation commands for Emacs

;; Copyright (C) 1985, 1995, 2001-2012  Free Software Foundation, Inc.

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

;; Commands for making and changing indentation in text.  These are
;; described in the Emacs manual.

;;; Code:

(defgroup indent nil
  "Indentation commands."
  :group 'editing)

(defcustom standard-indent 4
  "Default number of columns for margin-changing functions to indent."
  :group 'indent
  :type 'integer)

(defvar indent-line-function 'indent-relative
  "Function to indent the current line.
This function will be called with no arguments.
If it is called somewhere where auto-indentation cannot be done
\(e.g. inside a string), the function should simply return `noindent'.
Setting this function is all you need to make TAB indent appropriately.
Don't rebind TAB unless you really need to.")

(defcustom tab-always-indent t
  "Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.
If nil, hitting TAB indents the current line if point is at the left margin
or in the line's indentation, otherwise it inserts a \"real\" TAB character.
If `complete', TAB first tries to indent the current line, and if the line
was already indented, then try to complete the thing at point.

Some programming language modes have their own variable to control this,
e.g., `c-tab-always-indent', and do not respect this variable."
  :group 'indent
  :type '(choice
	  (const :tag "Always indent" t)
	  (const :tag "Indent if inside indentation, else TAB" nil)
	  (const :tag "Indent, or if already indented complete" complete)))


(defun indent-according-to-mode ()
  "Indent line in proper way for current major mode.
Normally, this is done by calling the function specified by the
variable `indent-line-function'.  However, if the value of that
variable is `indent-relative' or `indent-relative-maybe', handle
it specially (since those functions are used for tabbing); in
that case, indent by aligning to the previous non-blank line."
  (interactive)
  (syntax-propertize (line-end-position))
  (if (memq indent-line-function
	    '(indent-relative indent-relative-maybe))
      ;; These functions are used for tabbing, but can't be used for
      ;; indenting.  Replace with something ad-hoc.
      (let ((column (save-excursion
		      (beginning-of-line)
		      (skip-chars-backward "\n \t")
		      (beginning-of-line)
		      (current-indentation))))
	(if (<= (current-column) (current-indentation))
	    (indent-line-to column)
	  (save-excursion (indent-line-to column))))
    ;; The normal case.
    (funcall indent-line-function)))

(defun indent-for-tab-command (&optional arg)
  "Indent the current line or region, or insert a tab, as appropriate.
This function either inserts a tab, or indents the current line,
or performs symbol completion, depending on `tab-always-indent'.
The function called to actually indent the line or insert a tab
is given by the variable `indent-line-function'.

If a prefix argument is given, after this function indents the
current line or inserts a tab, it also rigidly indents the entire
balanced expression which starts at the beginning of the current
line, to reflect the current line's indentation.

In most major modes, if point was in the current line's
indentation, it is moved to the first non-whitespace character
after indenting; otherwise it stays at the same position relative
to the text.

If `transient-mark-mode' is turned on and the region is active,
this function instead calls `indent-region'.  In this case, any
prefix argument is ignored."
  (interactive "P")
  (cond
   ;; The region is active, indent it.
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((or ;; indent-to-left-margin is only meant for indenting,
	;; so we force it to always insert a tab here.
	(eq indent-line-function 'indent-to-left-margin)
	(and (not tab-always-indent)
	     (or (> (current-column) (current-indentation))
		 (eq this-command last-command))))
    (insert-tab arg))
   (t
    (let ((old-tick (buffer-chars-modified-tick))
          (old-point (point))
	  (old-indent (current-indentation)))

      ;; Indent the line.
      (funcall indent-line-function)

      (cond
       ;; If the text was already indented right, try completion.
       ((and (eq tab-always-indent 'complete)
             (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick)))
        (completion-at-point))

       ;; If a prefix argument was given, rigidly indent the following
       ;; sexp to match the change in the current line's indentation.
       (arg
        (let ((end-marker
               (save-excursion
                 (forward-line 0) (forward-sexp) (point-marker)))
              (indentation-change (- (current-indentation) old-indent)))
          (save-excursion
            (forward-line 1)
            (when (and (not (zerop indentation-change))
                       (< (point) end-marker))
              (indent-rigidly (point) end-marker indentation-change))))))))))

(defun insert-tab (&optional arg)
  (let ((count (prefix-numeric-value arg)))
    (if (and abbrev-mode
	     (eq (char-syntax (preceding-char)) ?w))
	(expand-abbrev))
    (if indent-tabs-mode
	(insert-char ?\t count)
      (indent-to (* tab-width (+ count (/ (current-column) tab-width)))))))

(defun indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large negative ARG."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (let ((indent (current-indentation))
	    eol-flag)
	(save-excursion
	  (skip-chars-forward " \t")
	  (setq eol-flag (eolp)))
	(or eol-flag
	    (indent-to (max 0 (+ indent arg)) 0))
	(delete-region (point) (progn (skip-chars-forward " \t") (point))))
      (forward-line 1))
    (move-marker end nil)))

(defun indent-line-to (column)
  "Indent current line to COLUMN.
This function removes or adds spaces and tabs at beginning of line
only if necessary.  It leaves point at end of indentation."
  (back-to-indentation)
  (let ((cur-col (current-column)))
    (cond ((< cur-col column)
	   (if (>= (- column (* (/ cur-col tab-width) tab-width)) tab-width)
	       (delete-region (point)
			      (progn (skip-chars-backward " ") (point))))
	   (indent-to column))
	  ((> cur-col column) ; too far right (after tab?)
	   (delete-region (progn (move-to-column column t) (point))
			  (progn (back-to-indentation) (point)))))))

(defun current-left-margin ()
  "Return the left margin to use for this line.
This is the value of the buffer-local variable `left-margin' plus the value
of the `left-margin' text-property at the start of the line."
  (save-excursion
    (back-to-indentation)
    (max 0
	 (+ left-margin (or (get-text-property
			     (if (and (eobp) (not (bobp)))
				 (1- (point)) (point))
			     'left-margin) 0)))))

(defun move-to-left-margin (&optional n force)
  "Move to the left margin of the current line.
With optional argument, move forward N-1 lines first.
The column moved to is the one given by the `current-left-margin' function.
If the line's indentation appears to be wrong, and this command is called
interactively or with optional argument FORCE, it will be fixed."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (beginning-of-line n)
  (skip-chars-forward " \t")
  (if (minibufferp (current-buffer))
      (if (save-excursion (beginning-of-line) (bobp))
	  (goto-char (minibuffer-prompt-end))
	(beginning-of-line))
    (let ((lm (current-left-margin))
	  (cc (current-column)))
      (cond ((> cc lm)
	     (if (> (move-to-column lm force) lm)
		 ;; If lm is in a tab and we are not forcing, move before tab
		 (backward-char 1)))
	    ((and force (< cc lm))
	     (indent-to-left-margin))))))

;; This used to be the default indent-line-function,
;; used in Fundamental Mode, Text Mode, etc.
(defun indent-to-left-margin ()
  "Indent current line to the column given by `current-left-margin'."
  (save-excursion (indent-line-to (current-left-margin)))
  ;; If we are within the indentation, move past it.
  (when (save-excursion
	  (skip-chars-backward " \t")
	  (bolp))
    (skip-chars-forward " \t")))

(defun delete-to-left-margin (&optional from to)
  "Remove left margin indentation from a region.
This deletes to the column given by `current-left-margin'.
In no case will it delete non-whitespace.
Args FROM and TO are optional; default is the whole buffer."
  (save-excursion
    (goto-char (or to (point-max)))
    (setq to (point-marker))
    (goto-char (or from (point-min)))
    (or (bolp) (forward-line 1))
    (while (< (point) to)
      (delete-region (point) (progn (move-to-left-margin nil t) (point)))
      (forward-line 1))
    (move-marker to nil)))

(defun set-left-margin (from to width)
  "Set the left margin of the region to WIDTH.
If `auto-fill-mode' is active, re-fill the region to fit the new margin.

Interactively, WIDTH is the prefix argument, if specified.
Without prefix argument, the command prompts for WIDTH."
  (interactive "r\nNSet left margin to column: ")
  (save-excursion
    ;; If inside indentation, start from BOL.
    (goto-char from)
    (skip-chars-backward " \t")
    (if (bolp) (setq from (point)))
    ;; Place end after whitespace
    (goto-char to)
    (skip-chars-forward " \t")
    (setq to (point-marker)))
  ;; Delete margin indentation first, but keep paragraph indentation.
  (delete-to-left-margin from to)
  (put-text-property from to 'left-margin width)
  (indent-rigidly from to width)
  (if auto-fill-function (save-excursion (fill-region from to nil t t)))
  (move-marker to nil))

(defun set-right-margin (from to width)
  "Set the right margin of the region to WIDTH.
If `auto-fill-mode' is active, re-fill the region to fit the new margin.

Interactively, WIDTH is the prefix argument, if specified.
Without prefix argument, the command prompts for WIDTH."
  (interactive "r\nNSet right margin to width: ")
  (save-excursion
    (goto-char from)
    (skip-chars-backward " \t")
    (if (bolp) (setq from (point))))
  (put-text-property from to 'right-margin width)
  (if auto-fill-function (save-excursion (fill-region from to nil t t))))

(defun alter-text-property (from to prop func &optional object)
  "Programmatically change value of a text-property.
For each region between FROM and TO that has a single value for PROPERTY,
apply FUNCTION to that value and sets the property to the function's result.
Optional fifth argument OBJECT specifies the string or buffer to operate on."
  (let ((begin from)
	end val)
    (while (setq val (get-text-property begin prop object)
		 end (text-property-not-all begin to prop val object))
      (put-text-property begin end prop (funcall func val) object)
      (setq begin end))
    (if (< begin to)
	(put-text-property begin to prop (funcall func val) object))))

(defun increase-left-margin (from to inc)
  "Increase or decrease the left-margin of the region.
With no prefix argument, this adds `standard-indent' of indentation.
A prefix arg (optional third arg INC noninteractively) specifies the amount
to change the margin by, in characters.
If `auto-fill-mode' is active, re-fill the region to fit the new margin."
  (interactive "*r\nP")
  (setq inc (if inc (prefix-numeric-value inc) standard-indent))
  (save-excursion
    (goto-char from)
    (skip-chars-backward " \t")
    (if (bolp) (setq from (point)))
    (goto-char to)
    (setq to (point-marker)))
  (alter-text-property from to 'left-margin
		       (lambda (v) (max (- left-margin) (+ inc (or v 0)))))
  (indent-rigidly from to inc)
  (if auto-fill-function (save-excursion (fill-region from to nil t t)))
  (move-marker to nil))

(defun decrease-left-margin (from to inc)
  "Make the left margin of the region smaller.
With no prefix argument, decrease the indentation by `standard-indent'.
A prefix arg (optional third arg INC noninteractively) specifies the amount
to change the margin by, in characters.
If `auto-fill-mode' is active, re-fill the region to fit the new margin."
  (interactive "*r\nP")
  (setq inc (if inc (prefix-numeric-value inc) standard-indent))
  (increase-left-margin from to (- inc)))

(defun increase-right-margin (from to inc)
  "Increase the right-margin of the region.
With no prefix argument, increase the right margin by `standard-indent'.
A prefix arg (optional third arg INC noninteractively) specifies the amount
to change the margin by, in characters.  A negative argument decreases
the right margin width.
If `auto-fill-mode' is active, re-fill the region to fit the new margin."
  (interactive "r\nP")
  (setq inc (if inc (prefix-numeric-value inc) standard-indent))
  (save-excursion
    (alter-text-property from to 'right-margin
			 (lambda (v) (+ inc (or v 0))))
    (if auto-fill-function
	(fill-region from to nil t t))))

(defun decrease-right-margin (from to inc)
  "Make the right margin of the region smaller.
With no prefix argument, decrease the right margin by `standard-indent'.
A prefix arg (optional third arg INC noninteractively) specifies the amount
of width to remove, in characters.  A negative argument increases
the right margin width.
If `auto-fill-mode' is active, re-fills region to fit in new margin."
  (interactive "*r\nP")
  (setq inc (if inc (prefix-numeric-value inc) standard-indent))
  (increase-right-margin from to (- inc)))

(defun beginning-of-line-text (&optional n)
  "Move to the beginning of the text on this line.
With optional argument, move forward N-1 lines first.
From the beginning of the line, moves past the left-margin indentation, the
fill-prefix, and any indentation used for centering or right-justifying the
line, but does not move past any whitespace that was explicitly inserted
\(such as a tab used to indent the first line of a paragraph)."
  (interactive "p")
  (beginning-of-line n)
  (skip-chars-forward " \t")
  ;; Skip over fill-prefix.
  (if (and fill-prefix
	   (not (string-equal fill-prefix "")))
      (if (equal fill-prefix
		 (buffer-substring
		  (point) (min (point-max) (+ (length fill-prefix) (point)))))
	  (forward-char (length fill-prefix)))
    (if (and adaptive-fill-mode adaptive-fill-regexp
	     (looking-at adaptive-fill-regexp))
	(goto-char (match-end 0))))
  ;; Skip centering or flushright indentation
  (if (memq (current-justification) '(center right))
      (skip-chars-forward " \t")))

(defvar indent-region-function nil
  "Short cut function to indent region using `indent-according-to-mode'.
A value of nil means really run `indent-according-to-mode' on each line.")

(defun indent-region (start end &optional column)
  "Indent each nonblank line in the region.
A numeric prefix argument specifies a column: indent each line to that column.

With no prefix argument, the command chooses one of these methods and
indents all the lines with it:

  1) If `fill-prefix' is non-nil, insert `fill-prefix' at the
     beginning of each line in the region that does not already begin
     with it.
  2) If `indent-region-function' is non-nil, call that function
     to indent the region.
  3) Indent each line via `indent-according-to-mode'.

Called from a program, START and END specify the region to indent.
If the third argument COLUMN is an integer, it specifies the
column to indent to; if it is nil, use one of the three methods above."
  (interactive "r\nP")
  (cond
   (column
    (setq column (prefix-numeric-value column))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
	(delete-region (point) (progn (skip-chars-forward " \t") (point)))
	(or (eolp)
	    (indent-to column 0))
	(forward-line 1))
      (move-marker end nil)))
   (fill-prefix
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (let ((regexp (regexp-quote fill-prefix)))
	(while (< (point) end)
	  (or (looking-at regexp)
	      (and (bolp) (eolp))
	      (insert fill-prefix))
	  (forward-line 1)))))
   (indent-region-function
    (funcall indent-region-function start end))
   (t
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (while (< (point) end)
	(or (and (bolp) (eolp))
	    (indent-according-to-mode))
	(forward-line 1))
      (move-marker end nil))))
  ;; In most cases, reindenting modifies the buffer, but it may also
  ;; leave it unmodified, in which case we have to deactivate the mark
  ;; by hand.
  (deactivate-mark))

(defun indent-relative-maybe ()
  "Indent a new line like previous nonblank line.
If the previous nonblank line has no indent points beyond the
column point starts at, this command does nothing.

See also `indent-relative'."
  (interactive)
  (indent-relative t))

(defun indent-relative (&optional unindented-ok)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
The following line shows the indentation points in this line.
    ^         ^    ^     ^   ^           ^      ^  ^    ^
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead, unless
this command is invoked with a numeric argument, in which case it
does nothing.

See also `indent-relative-maybe'."
  (interactive "P")
  (if (and abbrev-mode
	   (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
	  (let ((end (save-excursion (forward-line 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (or (looking-at "[ \t]")
		unindented-ok
		(skip-chars-forward "^ \t" end))
	    (skip-chars-forward " \t" end)
	    (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (move-marker opoint nil))
      (tab-to-tab-stop))))

(defcustom tab-stop-list
  '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
  "List of tab stop positions used by `tab-to-tab-stop'.
This should be a list of integers, ordered from smallest to largest."
  :group 'indent
  :type '(repeat integer))
(put 'tab-stop-list 'safe-local-variable 'listp)

(defvar edit-tab-stops-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'edit-tab-stops-note-changes)
    (define-key map "\C-c\C-c" 'edit-tab-stops-note-changes)
    map)
  "Keymap used in `edit-tab-stops'.")

(defvar edit-tab-stops-buffer nil
  "Buffer whose tab stops are being edited.
This matters if the variable `tab-stop-list' is local in that buffer.")

(defun edit-tab-stops ()
  "Edit the tab stops used by `tab-to-tab-stop'.
Creates a buffer *Tab Stops* containing text describing the tab stops.
A colon indicates a column where there is a tab stop.
You can add or remove colons and then do \\<edit-tab-stops-map>\\[edit-tab-stops-note-changes] to make changes take effect."
  (interactive)
  (setq edit-tab-stops-buffer (current-buffer))
  (switch-to-buffer (get-buffer-create "*Tab Stops*"))
  (use-local-map edit-tab-stops-map)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (overwrite-mode 1)
  (setq truncate-lines t)
  (erase-buffer)
  (let ((tabs tab-stop-list))
    (while tabs
      (indent-to (car tabs) 0)
      (insert ?:)
      (setq tabs (cdr tabs))))
  (let ((count 0))
    (insert ?\n)
    (while (< count 8)
      (insert (+ count ?0))
    (insert "         ")
      (setq count (1+ count)))
    (insert ?\n)
    (while (> count 0)
      (insert "0123456789")
      (setq count (1- count))))
  (insert "\nTo install changes, type C-c C-c")
  (goto-char (point-min)))

(defun edit-tab-stops-note-changes ()
  "Put edited tab stops into effect."
  (interactive)
    (let (tabs)
      (save-excursion
	(goto-char 1)
	(end-of-line)
	(while (search-backward ":" nil t)
	  (setq tabs (cons (current-column) tabs))))
      (bury-buffer (prog1 (current-buffer)
			  (switch-to-buffer edit-tab-stops-buffer)))
      (setq tab-stop-list tabs))
  (message "Tab stops installed"))

(defun tab-to-tab-stop ()
  "Insert spaces or tabs to next defined tab-stop column.
The variable `tab-stop-list' is a list of columns at which there are tab stops.
Use \\[edit-tab-stops] to edit them interactively."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
        (progn
          (delete-horizontal-space t)
	  (indent-to (car tabs)))
      (insert ?\s))))

(defun move-to-tab-stop ()
  "Move point to next defined tab-stop column.
The variable `tab-stop-list' is a list of columns at which there are tab stops.
Use \\[edit-tab-stops] to edit them interactively."
  (interactive)
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(let ((before (point)))
	  (move-to-column (car tabs) t)
	  (save-excursion
	    (goto-char before)
	    ;; If we just added a tab, or moved over one,
	    ;; delete any superfluous spaces before the old point.
	    (if (and (eq (preceding-char) ?\s)
		     (eq (following-char) ?\t))
		(let ((tabend (* (/ (current-column) tab-width) tab-width)))
		  (while (and (> (current-column) tabend)
			      (eq (preceding-char) ?\s))
		    (forward-char -1))
		  (delete-region (point) before))))))))

(define-key global-map "\t" 'indent-for-tab-command)
(define-key esc-map "\C-\\" 'indent-region)
(define-key ctl-x-map "\t" 'indent-rigidly)
(define-key esc-map "i" 'tab-to-tab-stop)

;;; indent.el ends here
