;;; replace.el --- replace commands for Emacs

;; Copyright (C) 1985-1987, 1992, 1994, 1996-1997, 2000-2012
;;   Free Software Foundation, Inc.

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

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(defcustom case-replace t
  "Non-nil means `query-replace' should preserve case in replacements."
  :type 'boolean
  :group 'matching)

(defvar query-replace-history nil
  "Default history list for query-replace commands.
See `query-replace-from-history-variable' and
`query-replace-to-history-variable'.")

(defvar query-replace-defaults nil
  "Default values of FROM-STRING and TO-STRING for `query-replace'.
This is a cons cell (FROM-STRING . TO-STRING), or nil if there is
no default value.")

(defvar query-replace-interactive nil
  "Non-nil means `query-replace' uses the last search string.
That becomes the \"string to replace\".")

(defcustom query-replace-from-history-variable 'query-replace-history
  "History list to use for the FROM argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for the strings
or patterns to be replaced."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-to-history-variable 'query-replace-history
  "History list to use for the TO argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-skip-read-only nil
  "Non-nil means `query-replace' and friends ignore read-only matches."
  :type 'boolean
  :group 'matching
  :version "22.1")

(defcustom query-replace-show-replacement t
  "Non-nil means to show what actual replacement text will be."
  :type 'boolean
  :group 'matching
  :version "23.1")

(defcustom query-replace-highlight t
  "Non-nil means to highlight matches during query replacement."
  :type 'boolean
  :group 'matching)

(defcustom query-replace-lazy-highlight t
  "Controls the lazy-highlighting during query replacements.
When non-nil, all text in the buffer matching the current match
is highlighted lazily using isearch lazy highlighting (see
`lazy-highlight-initial-delay' and `lazy-highlight-interval')."
  :type 'boolean
  :group 'lazy-highlight
  :group 'matching
  :version "22.1")

(defface query-replace
  '((t (:inherit isearch)))
  "Face for highlighting query replacement matches."
  :group 'matching
  :version "22.1")

(defvar replace-count 0
  "Number of replacements done so far.
See `replace-regexp' and `query-replace-regexp-eval'.")

(defun query-replace-descr (string)
  (mapconcat 'isearch-text-char-description string ""))

(defun query-replace-read-from (prompt regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let* ((history-add-new-input nil)
	   (from
	    ;; The save-excursion here is in case the user marks and copies
	    ;; a region in order to specify the minibuffer input.
	    ;; That should not clobber the region for the query-replace itself.
	    (save-excursion
	      (read-from-minibuffer
	       (if query-replace-defaults
		   (format "%s (default %s -> %s): " prompt
			   (query-replace-descr (car query-replace-defaults))
			   (query-replace-descr (cdr query-replace-defaults)))
		 (format "%s: " prompt))
	       nil nil nil
	       query-replace-from-history-variable
	       nil t))))
      (if (and (zerop (length from)) query-replace-defaults)
	  (cons (car query-replace-defaults)
		(query-replace-compile-replacement
		 (cdr query-replace-defaults) regexp-flag))
	(add-to-history query-replace-from-history-variable from nil t)
	;; Warn if user types \n or \t, but don't reject the input.
	(and regexp-flag
	     (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
	     (let ((match (match-string 3 from)))
	       (cond
		((string= match "\\n")
		 (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
		((string= match "\\t")
		 (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
	       (sit-for 2)))
	from))))

(defun query-replace-compile-replacement (to regexp-flag)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (if (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
      (let (pos list char)
	(while
	    (progn
	      (setq pos (match-end 0))
	      (push (substring to 0 (- pos 2)) list)
	      (setq char (aref to (1- pos))
		    to (substring to pos))
	      (cond ((eq char ?\#)
		     (push '(number-to-string replace-count) list))
		    ((eq char ?\,)
		     (setq pos (read-from-string to))
		     (push `(replace-quote ,(car pos)) list)
		     (let ((end
			    ;; Swallow a space after a symbol
			    ;; if there is a space.
			    (if (and (or (symbolp (car pos))
					 ;; Swallow a space after 'foo
					 ;; but not after (quote foo).
					 (and (eq (car-safe (car pos)) 'quote)
					      (not (= ?\( (aref to 0)))))
				     (eq (string-match " " to (cdr pos))
					 (cdr pos)))
				(1+ (cdr pos))
			      (cdr pos))))
		       (setq to (substring to end)))))
	      (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
	(setq to (nreverse (delete "" (cons to list))))
	(replace-match-string-symbols to)
	(cons 'replace-eval-replacement
	      (if (cdr to)
		  (cons 'concat to)
		(car to))))
    to))


(defun query-replace-read-to (from prompt regexp-flag)
  "Query and return the `to' argument of a query-replace operation."
  (query-replace-compile-replacement
   (save-excursion
     (let* ((history-add-new-input nil)
	    (to (read-from-minibuffer
		 (format "%s %s with: " prompt (query-replace-descr from))
		 nil nil nil
		 query-replace-to-history-variable from t)))
       (add-to-history query-replace-to-history-variable to nil t)
       (setq query-replace-defaults (cons from to))
       to))
   regexp-flag))

(defun query-replace-read-args (prompt regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let* ((from (query-replace-read-from prompt regexp-flag))
	 (to (if (consp from) (prog1 (cdr from) (setq from (car from)))
	       (query-replace-read-to from prompt regexp-flag))))
    (list from to current-prefix-arg)))

(defun query-replace (from-string to-string &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

Matching is independent of case if `case-fold-search' is non-nil and
FROM-STRING has no uppercase letters.  Replacement transfers the case
pattern of the old text to the new text, if `case-replace' and
`case-fold-search' are non-nil and FROM-STRING has no uppercase
letters.  \(Transferring the case pattern means that if the old text
matched is all caps, or capitalized, then its replacement is upcased
or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

To customize possible responses, change the \"bindings\" in `query-replace-map'."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg " word" "")
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace from-string to-string t nil delimited nil nil start end))

(define-key esc-map "%" 'query-replace)

(defun query-replace-regexp (regexp to-string &optional delimited start end)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Matching is independent of case if `case-fold-search' is non-nil and
REGEXP has no uppercase letters.  Replacement transfers the case
pattern of the old text to the new text, if `case-replace' and
`case-fold-search' are non-nil and REGEXP has no uppercase letters.
\(Transferring the case pattern means that if the old text matched is
all caps, or capitalized, then its replacement is upcased or
capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
whatever what matched the Nth `\\(...\\)' in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text can contain `\\,'
followed by a Lisp expression.  Each
replacement evaluates that expression to compute the replacement
string.  Inside of that expression, `\\&' is a string denoting the
whole match as a string, `\\N' for a partial match, `\\#&' and `\\#N'
for the whole or a partial match converted to a number with
`string-to-number', and `\\#' itself for the number of replacements
done so far (starting with zero).

If the replacement expression is a symbol, write a space after it
to terminate it.  One space there, if any, will be discarded.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg " word" "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace regexp to-string t t delimited nil nil start end))

(define-key esc-map [?\C-%] 'query-replace-regexp)

(defun query-replace-regexp-eval (regexp to-expr &optional delimited start end)
  "Replace some things after point matching REGEXP with the result of TO-EXPR.

Interactive use of this function is deprecated in favor of the
`\\,' feature of `query-replace-regexp'.  For non-interactive use, a loop
using `search-forward-regexp' and `replace-match' is preferred.

As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

TO-EXPR is a Lisp expression evaluated to compute each replacement.  It may
reference `replace-count' to get the number of replacements already made.
If the result of TO-EXPR is not a string, it is converted to one using
`prin1-to-string' with the NOESCAPE argument (which see).

For convenience, when entering TO-EXPR interactively, you can use `\\&' or
`\\0' to stand for whatever matched the whole of REGEXP, and `\\N' (where
N is a digit) to stand for whatever matched the Nth `\\(...\\)' in REGEXP.
Use `\\#&' or `\\#N' if you want a number instead of a string.
In interactive use, `\\#' in itself stands for `replace-count'.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches that are surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (progn
   (barf-if-buffer-read-only)
   (let* ((from
	   ;; Let-bind the history var to disable the "foo -> bar" default.
	   ;; Maybe we shouldn't disable this default, but for now I'll
	   ;; leave it off.  --Stef
	   (let ((query-replace-to-history-variable nil))
	     (query-replace-read-from "Query replace regexp" t)))
	  (to (list (read-from-minibuffer
		     (format "Query replace regexp %s with eval: "
			     (query-replace-descr from))
		     nil nil t query-replace-to-history-variable from t))))
     ;; We make TO a list because replace-match-string-symbols requires one,
     ;; and the user might enter a single token.
     (replace-match-string-symbols to)
     (list from (car to) current-prefix-arg
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end))))))
  (perform-replace regexp (cons 'replace-eval-replacement to-expr)
		   t 'literal delimited nil nil start end))

(make-obsolete 'query-replace-regexp-eval
  "for interactive use, use the special `\\,' feature of
`query-replace-regexp' instead.  Non-interactively, a loop
using `search-forward-regexp' and `replace-match' is preferred." "22.1")

(defun map-query-replace-regexp (regexp to-strings &optional n start end)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings, separated
by spaces.  This command works like `query-replace-regexp' except that
each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Non-interactively, TO-STRINGS may be a list of replacement strings.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

A prefix argument N says to use each replacement string N times
before rotating to the next.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (let* ((from (if query-replace-interactive
		    (car regexp-search-ring)
		  (read-from-minibuffer "Map query replace (regexp): "
					nil nil nil
					query-replace-from-history-variable
					nil t)))
	  (to (read-from-minibuffer
	       (format "Query replace %s with (space-separated strings): "
		       (query-replace-descr from))
	       nil nil nil
	       query-replace-to-history-variable from t)))
     (list from to
	   (and current-prefix-arg
		(prefix-numeric-value current-prefix-arg))
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-match " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-match " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-match " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
    (perform-replace regexp replacements t t nil n nil start end)))

(defun replace-string (from-string to-string &optional delimited start end)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward FROM-STRING nil t)
    (replace-match TO-STRING nil t))
which will run faster and will not set the mark or print anything.
\(You may need a more complex loop if FROM-STRING can match the null string
and TO-STRING is also null.)"
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   (if current-prefix-arg " word" "")
		   " string"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace from-string to-string nil nil delimited nil nil start end))

(defun replace-regexp (regexp to-string &optional delimited start end)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
whatever what matched the Nth `\\(...\\)' in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text may contain `\\,'
followed by a Lisp expression used as part of the replacement
text.  Inside of that expression, `\\&' is a string denoting the
whole match, `\\N' a partial match, `\\#&' and `\\#N' the respective
numeric values from `string-to-number', and `\\#' itself for
`replace-count', the number of replacements occurred so far.

If your Lisp expression is an identifier and the next letter in
the replacement string would be interpreted as part of it, you
can wrap it with an expression like `\\,(or \\#)'.  Incidentally,
for this particular case you may also enter `\\#' in the
replacement text directly.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match TO-STRING nil nil))
which will run faster and will not set the mark or print anything."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   (if current-prefix-arg " word" "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace regexp to-string nil t delimited nil nil start end))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar occur-collect-regexp-history '("\\1")
  "History of regexp for occur's collect operation")

(defun read-regexp (prompt &optional default-value)
  "Read regexp as a string using the regexp history and some useful defaults.
Prompt for a regular expression with PROMPT (without a colon and
space) in the minibuffer.  The optional argument DEFAULT-VALUE
provides the value to display in the minibuffer prompt that is
returned if the user just types RET.
Values available via M-n are the string at point, the last isearch
regexp, the last isearch string, and the last replacement regexp."
  (let* ((defaults
	   (list (regexp-quote
		  (or (funcall (or find-tag-default-function
				   (get major-mode 'find-tag-default-function)
				   'find-tag-default))
		      ""))
		 (car regexp-search-ring)
		 (regexp-quote (or (car search-ring) ""))
		 (car (symbol-value
		       query-replace-from-history-variable))))
	 (defaults (delete-dups (delq nil (delete "" defaults))))
	 ;; Don't add automatically the car of defaults for empty input
	 (history-add-new-input nil)
	 (input
	  (read-from-minibuffer
	   (if default-value
	       (format "%s (default %s): " prompt
		       (query-replace-descr default-value))
	     (format "%s: " prompt))
	   nil nil nil 'regexp-history defaults t)))
    (if (equal input "")
	(or default-value input)
      (prog1 input
	(add-to-history 'regexp-history input)))))


(defalias 'delete-non-matching-lines 'keep-lines)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'count-matches 'how-many)


(defun keep-lines-read-args (prompt)
  "Read arguments for `keep-lines' and friends.
Prompt for a regexp with PROMPT.
Value is a list, (REGEXP)."
  (list (read-regexp prompt) nil nil t))

(defun keep-lines (regexp &optional rstart rend interactive)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all lines starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all lines whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (A newline ending a line counts as part of that line.)

Interactively, in Transient Mark mode when the mark is active, operate
on all lines whose accessible part is entirely contained in the region.
Otherwise, the command applies to all lines starting after point.
When calling this function from Lisp, you can pretend that it was
called interactively by passing a non-nil INTERACTIVE argument.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Keep lines containing match for regexp")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend
	      (progn
		(save-excursion
		  (goto-char (max rstart rend))
		  (unless (or (bolp) (eobp))
		    (forward-line 0))
		  (point-marker)))))
    (if (and interactive transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (progn
		     (goto-char (region-end))
		     (unless (or (bolp) (eobp))
		       (forward-line 0))
		     (point-marker)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point))
	  (case-fold-search
	   (if (and case-fold-search search-upper-case)
	       (isearch-no-upper-case-p regexp t)
	     case-fold-search)))
      (while (< (point) rend)
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp rend 'move))
	    (delete-region start rend)
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (forward-line 0)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))

	(setq start (save-excursion (forward-line 1) (point)))
	;; If the match was empty, avoid matching again at same place.
	(and (< (point) rend)
	     (= (match-beginning 0) (match-end 0))
	     (forward-char 1)))))
  (set-marker rend nil)
  nil)


(defun flush-lines (regexp &optional rstart rend interactive)
 "Delete lines containing matches for REGEXP.
When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The line point is in is deleted if and only if it contains a
match for regexp starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
Lines partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil INTERACTIVE argument.

If a match is split across lines, all the lines it lies in are deleted.
They are deleted _before_ looking for the next match.  Hence, a match
starting on the same line at which another match ended is ignored."

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Flush lines containing match for regexp")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend (copy-marker (max rstart rend))))
    (if (and interactive transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((case-fold-search
	 (if (and case-fold-search search-upper-case)
	     (isearch-no-upper-case-p regexp t)
	   case-fold-search)))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
	(delete-region (save-excursion (goto-char (match-beginning 0))
				       (forward-line 0)
				       (point))
		       (progn (forward-line 1) (point))))))
  (set-marker rend nil)
  nil)


(defun how-many (regexp &optional rstart rend interactive)
  "Print and return number of matches for REGEXP following point.
When called from Lisp and INTERACTIVE is omitted or nil, just return
the number, do not print it; if INTERACTIVE is t, the function behaves
in all respects as if it had been called interactively.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (keep-lines-read-args "How many matches for regexp"))
  (save-excursion
    (if rstart
	(progn
	  (goto-char (min rstart rend))
	  (setq rend (max rstart rend)))
      (if (and interactive transient-mark-mode mark-active)
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
	  opoint
	  (case-fold-search
	   (if (and case-fold-search search-upper-case)
	       (isearch-no-upper-case-p regexp t)
	     case-fold-search)))
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward regexp rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (when interactive (message "%d occurrence%s"
				 count
				 (if (= count 1) "" "s")))
      count)))


(defvar occur-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [next-error-follow-minor-mode]
      `(menu-item ,(purecopy "Auto Occurrence Display")
		  next-error-follow-minor-mode
		  :help ,(purecopy
			  "Display another occurrence when moving the cursor")
		  :button (:toggle . (and (boundp 'next-error-follow-minor-mode)
					  next-error-follow-minor-mode))))
    (define-key map [separator-1] menu-bar-separator)
    (define-key map [kill-this-buffer]
      `(menu-item ,(purecopy "Kill Occur Buffer") kill-this-buffer
		  :help ,(purecopy "Kill the current *Occur* buffer")))
    (define-key map [quit-window]
      `(menu-item ,(purecopy "Quit Occur Window") quit-window
		  :help ,(purecopy "Quit the current *Occur* buffer.  Bury it, and maybe delete the selected frame")))
    (define-key map [revert-buffer]
      `(menu-item ,(purecopy "Revert Occur Buffer") revert-buffer
		  :help ,(purecopy "Replace the text in the *Occur* buffer with the results of rerunning occur")))
    (define-key map [clone-buffer]
      `(menu-item ,(purecopy "Clone Occur Buffer") clone-buffer
		  :help ,(purecopy "Create and return a twin copy of the current *Occur* buffer")))
    (define-key map [occur-rename-buffer]
      `(menu-item ,(purecopy "Rename Occur Buffer") occur-rename-buffer
		  :help ,(purecopy "Rename the current *Occur* buffer to *Occur: original-buffer-name*.")))
    (define-key map [occur-edit-buffer]
      `(menu-item ,(purecopy "Edit Occur Buffer") occur-edit-mode
		  :help ,(purecopy "Edit the *Occur* buffer and apply changes to the original buffers.")))
    (define-key map [separator-2] menu-bar-separator)
    (define-key map [occur-mode-goto-occurrence-other-window]
      `(menu-item ,(purecopy "Go To Occurrence Other Window") occur-mode-goto-occurrence-other-window
		  :help ,(purecopy "Go to the occurrence the current line describes, in another window")))
    (define-key map [occur-mode-goto-occurrence]
      `(menu-item ,(purecopy "Go To Occurrence") occur-mode-goto-occurrence
		  :help ,(purecopy "Go to the occurrence the current line describes")))
    (define-key map [occur-mode-display-occurrence]
      `(menu-item ,(purecopy "Display Occurrence") occur-mode-display-occurrence
		  :help ,(purecopy "Display in another window the occurrence the current line describes")))
    (define-key map [occur-next]
      `(menu-item ,(purecopy "Move to Next Match") occur-next
		  :help ,(purecopy "Move to the Nth (default 1) next match in an Occur mode buffer")))
    (define-key map [occur-prev]
      `(menu-item ,(purecopy "Move to Previous Match") occur-prev
		  :help ,(purecopy "Move to the Nth (default 1) previous match in an Occur mode buffer")))
    map)
  "Menu keymap for `occur-mode'.")

(defvar occur-mode-map
  (let ((map (make-sparse-keymap)))
    ;; We use this alternative name, so we can use \\[occur-mode-mouse-goto].
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-mode-goto-occurrence)
    (define-key map "e" 'occur-edit-mode)
    (define-key map "\C-m" 'occur-mode-goto-occurrence)
    (define-key map "o" 'occur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\M-n" 'occur-next)
    (define-key map "\M-p" 'occur-prev)
    (define-key map "r" 'occur-rename-buffer)
    (define-key map "c" 'clone-buffer)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map [menu-bar occur] (cons (purecopy "Occur") occur-menu-map))
    map)
  "Keymap for `occur-mode'.")

(defvar occur-revert-arguments nil
  "Arguments to pass to `occur-1' to revert an Occur mode buffer.
See `occur-revert-function'.")
(make-variable-buffer-local 'occur-revert-arguments)
(put 'occur-revert-arguments 'permanent-local t)

(defcustom occur-mode-hook '(turn-on-font-lock)
  "Hook run when entering Occur mode."
  :type 'hook
  :group 'matching)

(defcustom occur-hook nil
  "Hook run by Occur when there are any matches."
  :type 'hook
  :group 'matching)

(defcustom occur-mode-find-occurrence-hook nil
  "Hook run by Occur after locating an occurrence.
This will be called with the cursor position at the occurrence.  An application
for this is to reveal context in an outline-mode when the occurrence is hidden."
  :type 'hook
  :group 'matching)

(put 'occur-mode 'mode-class 'special)
(define-derived-mode occur-mode special-mode "Occur"
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (set (make-local-variable 'revert-buffer-function) 'occur-revert-function)
  (setq next-error-function 'occur-next-error))


;;; Occur Edit mode

(defvar occur-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-cease-edit)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map [menu-bar occur] (cons (purecopy "Occur") occur-menu-map))
    map)
  "Keymap for `occur-edit-mode'.")

(define-derived-mode occur-edit-mode occur-mode "Occur-Edit"
  "Major mode for editing *Occur* buffers.
In this mode, changes to the *Occur* buffer are also applied to
the originating buffer.

To return to ordinary Occur mode, use \\[occur-cease-edit]."
  (setq buffer-read-only nil)
  (add-hook 'after-change-functions 'occur-after-change-function nil t)
  (message (substitute-command-keys
	    "Editing: Type \\[occur-cease-edit] to return to Occur mode.")))

(defun occur-cease-edit ()
  "Switch from Occur Edit mode to Occur mode."
  (interactive)
  (when (derived-mode-p 'occur-edit-mode)
    (occur-mode)
    (message "Switching to Occur mode.")))

(defun occur-after-change-function (beg end length)
  (save-excursion
    (goto-char beg)
    (let* ((line-beg (line-beginning-position))
	   (m (get-text-property line-beg 'occur-target))
	   (buf (marker-buffer m))
	   col)
      (when (and (get-text-property line-beg 'occur-prefix)
		 (not (get-text-property end 'occur-prefix)))
	(when (= length 0)
	  ;; Apply occur-target property to inserted (e.g. yanked) text.
	  (put-text-property beg end 'occur-target m)
	  ;; Did we insert a newline?  Occur Edit mode can't create new
	  ;; Occur entries; just discard everything after the newline.
	  (save-excursion
	    (and (search-forward "\n" end t)
		 (delete-region (1- (point)) end))))
	(let* ((line (- (line-number-at-pos)
			(line-number-at-pos (window-start))))
	       (readonly (with-current-buffer buf buffer-read-only))
	       (win (or (get-buffer-window buf)
			(display-buffer buf t)))
	       (line-end (line-end-position))
	       (text (save-excursion
		       (goto-char (next-single-property-change
				   line-beg 'occur-prefix nil
				   line-end))
		       (setq col (- (point) line-beg))
		       (buffer-substring-no-properties (point) line-end))))
	  (with-selected-window win
	    (goto-char m)
	    (recenter line)
	    (if readonly
		(message "Buffer `%s' is read only." buf)
	      (delete-region (line-beginning-position) (line-end-position))
	      (insert text))
	    (move-to-column col)))))))


(defun occur-revert-function (_ignore1 _ignore2)
  "Handle `revert-buffer' for Occur mode buffers."
  (apply 'occur-1 (append occur-revert-arguments (list (buffer-name)))))

(defun occur-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'occur-target)))
    (unless pos
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this occurrence was killed"))
    pos))

(defalias 'occur-mode-mouse-goto 'occur-mode-goto-occurrence)
(defun occur-mode-goto-occurrence (&optional event)
  "Go to the occurrence on the current line."
  (interactive (list last-nonmenu-event))
  (let ((pos
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (occur-mode-find-occurrence)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (occur-mode-find-occurrence))))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence))
	window)
    (setq window (display-buffer (marker-buffer pos) t))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (run-hooks 'occur-mode-find-occurrence-hook))))

(defun occur-find-match (n search message)
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (setq r (funcall search (point) 'occur-match))
      (and r
           (get-text-property r 'occur-match)
           (setq r (funcall search r 'occur-match)))
      (if r
          (goto-char r)
        (error message))
      (setq n (1- n)))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'next-single-property-change "No more matches"))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'previous-single-property-change "No earlier matches"))

(defun occur-next-error (&optional argp reset)
  "Move to the Nth (default 1) next match in an Occur mode buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  ;; we need to run occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'occur-mode))))

    (goto-char (cond (reset (point-min))
		     ((< argp 0) (line-beginning-position))
		     ((> argp 0) (line-end-position))
		     ((point))))
    (occur-find-match
     (abs argp)
     (if (> 0 argp)
	 #'previous-single-property-change
       #'next-single-property-change)
     "No more matches")
    ;; In case the *Occur* buffer is visible in a nonselected window.
    (let ((win (get-buffer-window (current-buffer) t)))
      (if win (set-window-point win (point))))
    (occur-mode-goto-occurrence)))

(defface match
  '((((class color) (min-colors 88) (background light))
     :background "yellow1")
    (((class color) (min-colors 88) (background dark))
     :background "RoyalBlue3")
    (((class color) (min-colors 8) (background light))
     :background "yellow" :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face used to highlight matches permanently."
  :group 'matching
  :version "22.1")

(defcustom list-matching-lines-default-context-lines 0
  "Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'match
  "Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type 'face
  :group 'matching)

(defcustom occur-excluded-properties
  '(read-only invisible intangible field mouse-face help-echo local-map keymap
    yank-handler follow-link)
  "Text properties to discard when copying lines to the *Occur* buffer.
The value should be a list of text properties to discard or t,
which means to discard all text properties."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'matching
  :version "22.1")

(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              (car regexp-history))))
    (list regexp
	  (if perform-collect
	      ;; Perform collect operation
	      (if (zerop (regexp-opt-depth regexp))
		  ;; No subexpression so collect the entire match.
		  "\\&"
		;; Get the regexp for collection pattern.
		(let ((default (car occur-collect-regexp-history)))
		  (read-string
		   (format "Regexp to collect (default %s): " default)
		   nil 'occur-collect-regexp-history default)))
	    ;; Otherwise normal occur takes numerical prefix argument.
	    (when current-prefix-arg
	      (prefix-numeric-value current-prefix-arg))))))

(defun occur-rename-buffer (&optional unique-p interactive-p)
  "Rename the current *Occur* buffer to *Occur: original-buffer-name*.
Here `original-buffer-name' is the buffer name where Occur was originally run.
When given the prefix argument, or called non-interactively, the renaming
will not clobber the existing buffer(s) of that name, but use
`generate-new-buffer-name' instead.  You can add this to `occur-hook'
if you always want a separate *Occur* buffer for each buffer where you
invoke `occur'."
  (interactive "P\np")
  (with-current-buffer
      (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
    (rename-buffer (concat "*Occur: "
                           (mapconcat #'buffer-name
                                      (car (cddr occur-revert-arguments)) "/")
                           "*")
                   (or unique-p (not interactive-p)))))

(defun occur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.
If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

When NLINES is a string or when the function is called
interactively with prefix argument without a number (`C-u' alone
as prefix) the matching strings are collected into the `*Occur*'
buffer by using NLINES as a replacement regexp.  NLINES may
contain \\& and \\N which convention follows `replace-match'.
For example, providing \"defun\\s +\\(\\S +\\)\" for REGEXP and
\"\\1\" for NLINES collects all the function names in a lisp
program.  When there is no parenthesized subexpressions in REGEXP
the entire match is collected.  In any case the searched buffers
are not modified."
  (interactive (occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

(defvar ido-ignore-item-temp-list)

(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'.  When you invoke this command interactively, you must specify
the buffer names that you want, one by one.
See also `multi-occur-in-matching-buffers'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
	   (ido-ignore-item-temp-list bufs))
      (while (not (string-equal
		   (setq buf (read-buffer
			      (if (eq read-buffer-function 'ido-read-buffer)
				  "Next buffer to search (C-j to end): "
				"Next buffer to search (RET to end): ")
			      nil t))
		   ""))
	(add-to-list 'bufs buf)
	(setq ido-ignore-item-temp-list bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun multi-occur-in-matching-buffers (bufregexp regexp &optional allbufs)
  "Show all lines matching REGEXP in buffers specified by BUFREGEXP.
Normally BUFREGEXP matches against each buffer's visited file name,
but if you specify a prefix argument, it matches against the buffer name.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-from-minibuffer
	     (if current-prefix-arg
		 "List lines in buffers whose names match regexp: "
	       "List lines in buffers whose filenames match regexp: ")
	     nil
	     nil
	     nil
	     'regexp-history)))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nil
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (if allbufs
				       (string-match bufregexp
						     (buffer-name buf))
				     (and (buffer-file-name buf)
					  (string-match bufregexp
							(buffer-file-name buf))))
			       buf))
			   (buffer-list))))))

(defun occur-1 (regexp nlines bufs &optional buf-name)
  (unless (and regexp (not (equal regexp "")))
    (error "Occur doesn't work with the empty regexp"))
  (unless buf-name
    (setq buf-name "*Occur*"))
  (let (occur-buf
	(active-bufs (delq nil (mapcar #'(lambda (buf)
					   (when (buffer-live-p buf) buf))
				       bufs))))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name (mapcar 'buffer-name active-bufs))
      (with-current-buffer (get-buffer buf-name)
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-current-buffer occur-buf
      (if (stringp nlines)
	  (fundamental-mode) ;; This is for collect operation.
	(occur-mode))
      (let ((inhibit-read-only t)
	    ;; Don't generate undo entries for creation of the initial contents.
	    (buffer-undo-list t))
	(erase-buffer)
	(let ((count
	       (if (stringp nlines)
                   ;; Treat nlines as a regexp to collect.
		   (let ((bufs active-bufs)
			 (count 0))
		     (while bufs
		       (with-current-buffer (car bufs)
			 (save-excursion
			   (goto-char (point-min))
			   (while (re-search-forward regexp nil t)
                             ;; Insert the replacement regexp.
			     (let ((str (match-substitute-replacement nlines)))
			       (if str
				   (with-current-buffer occur-buf
				     (insert str)
				     (setq count (1+ count))
				     (or (zerop (current-column))
					 (insert "\n"))))))))
                       (setq bufs (cdr bufs)))
                     count)
		 ;; Perform normal occur.
		 (occur-engine
		  regexp active-bufs occur-buf
		  (or nlines list-matching-lines-default-context-lines)
		  (if (and case-fold-search search-upper-case)
		      (isearch-no-upper-case-p regexp t)
		    case-fold-search)
		  list-matching-lines-buffer-name-face
		  nil list-matching-lines-face
		  (not (eq occur-excluded-properties t))))))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d buffer%s%s; %s match%s%s"
		     bufcount (if (= bufcount 1) "" "s")
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (if (= count 1) "" "es")
		     ;; Don't display regexp if with remaining text
		     ;; it is longer than window-width.
		     (if (> (+ (length regexp) 42) (window-width))
			 "" (format " for `%s'" (query-replace-descr regexp)))))
	  (setq occur-revert-arguments (list regexp nlines bufs))
          (if (= count 0)
              (kill-buffer occur-buf)
            (display-buffer occur-buf)
            (setq next-error-last-buffer occur-buf)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (run-hooks 'occur-hook)))))))

(defun occur-engine (regexp buffers out-buf nlines case-fold
			    title-face prefix-face match-face keep-props)
  (with-current-buffer out-buf
    (let ((globalcount 0)
	  (coding nil)
	  (case-fold-search case-fold))
      ;; Map over all the buffers
      (dolist (buf buffers)
	(when (buffer-live-p buf)
	  (let ((matches 0)	;; count of matched lines
		(lines 1)	;; line count
		(prev-after-lines nil)	;; context lines of prev match
		(prev-lines nil)        ;; line number of prev match endpt
		(matchbeg 0)
		(origpt nil)
		(begpt nil)
		(endpt nil)
		(marker nil)
		(curstring "")
		(ret nil)
		(inhibit-field-text-motion t)
		(headerpt (with-current-buffer out-buf (point))))
	    (with-current-buffer buf
	      (or coding
		  ;; Set CODING only if the current buffer locally
		  ;; binds buffer-file-coding-system.
		  (not (local-variable-p 'buffer-file-coding-system))
		  (setq coding buffer-file-coding-system))
	      (save-excursion
		(goto-char (point-min)) ;; begin searching in the buffer
		(while (not (eobp))
		  (setq origpt (point))
		  (when (setq endpt (re-search-forward regexp nil t))
		    (setq matches (1+ matches)) ;; increment match count
		    (setq matchbeg (match-beginning 0))
		    ;; Get beginning of first match line and end of the last.
		    (save-excursion
		      (goto-char matchbeg)
		      (setq begpt (line-beginning-position))
		      (goto-char endpt)
		      (setq endpt (line-end-position)))
		    ;; Sum line numbers up to the first match line.
		    (setq lines (+ lines (count-lines origpt begpt)))
		    (setq marker (make-marker))
		    (set-marker marker matchbeg)
		    (setq curstring (occur-engine-line begpt endpt keep-props))
		    ;; Highlight the matches
		    (let ((len (length curstring))
			  (start 0))
		      (while (and (< start len)
				  (string-match regexp curstring start))
			(add-text-properties
			 (match-beginning 0) (match-end 0)
			 (append
			  `(occur-match t)
			  (when match-face
			    ;; Use `face' rather than `font-lock-face' here
			    ;; so as to override faces copied from the buffer.
			    `(face ,match-face)))
			 curstring)
			(setq start (match-end 0))))
		    ;; Generate the string to insert for this match
		    (let* ((match-prefix
			    ;; Using 7 digits aligns tabs properly.
			    (apply #'propertize (format "%7d:" lines)
				   (append
				    (when prefix-face
				      `(font-lock-face prefix-face))
				    `(occur-prefix t mouse-face (highlight)
				      ;; Allow insertion of text at
				      ;; the end of the prefix (for
				      ;; Occur Edit mode).
				      front-sticky t rear-nonsticky t
				      occur-target ,marker follow-link t
				      help-echo "mouse-2: go to this occurrence"))))
			   (match-str
			    ;; We don't put `mouse-face' on the newline,
			    ;; because that loses.  And don't put it
			    ;; on context lines to reduce flicker.
			    (propertize curstring 'mouse-face (list 'highlight)
					'occur-target marker
					'follow-link t
					'help-echo
					"mouse-2: go to this occurrence"))
			   (out-line
			    (concat
			     match-prefix
			     ;; Add non-numeric prefix to all non-first lines
			     ;; of multi-line matches.
			     (replace-regexp-in-string
			      "\n"
			      "\n       :"
			      match-str)
			     ;; Add marker at eol, but no mouse props.
			     (propertize "\n" 'occur-target marker)))
			   (data
			    (if (= nlines 0)
				;; The simple display style
				out-line
			      ;; The complex multi-line display style.
			      (setq ret (occur-context-lines
					 out-line nlines keep-props begpt endpt
					 lines prev-lines prev-after-lines))
			      ;; Set first elem of the returned list to `data',
			      ;; and the second elem to `prev-after-lines'.
			      (setq prev-after-lines (nth 1 ret))
			      (nth 0 ret))))
		      ;; Actually insert the match display data
		      (with-current-buffer out-buf
			(insert data)))
		    (goto-char endpt))
		  (if endpt
		      (progn
			;; Sum line numbers between first and last match lines.
			(setq lines (+ lines (count-lines begpt endpt)
				       ;; Add 1 for empty last match line since
				       ;; count-lines returns 1 line less.
				       (if (and (bolp) (eolp)) 1 0)))
			;; On to the next match...
			(forward-line 1))
		    (goto-char (point-max)))
		  (setq prev-lines (1- lines)))
		;; Flush remaining context after-lines.
		(when prev-after-lines
		  (with-current-buffer out-buf
		    (insert (apply #'concat (occur-engine-add-prefix
					     prev-after-lines)))))))
	    (when (not (zerop matches)) ;; is the count zero?
	      (setq globalcount (+ globalcount matches))
	      (with-current-buffer out-buf
		(goto-char headerpt)
		(let ((beg (point))
		      end)
		  (insert (propertize
			   (format "%d match%s%s in buffer: %s\n"
				   matches (if (= matches 1) "" "es")
				   ;; Don't display regexp for multi-buffer.
				   (if (> (length buffers) 1)
				       "" (format " for \"%s\""
						  (query-replace-descr regexp)))
				   (buffer-name buf))
			   'read-only t))
		  (setq end (point))
		  (add-text-properties beg end
				       (append
					(when title-face
					  `(font-lock-face ,title-face))
					`(occur-title ,buf))))
		(goto-char (point-min)))))))
      ;; Display total match count and regexp for multi-buffer.
      (when (and (not (zerop globalcount)) (> (length buffers) 1))
	(goto-char (point-min))
	(let ((beg (point))
	      end)
	  (insert (format "%d match%s total for \"%s\":\n"
			  globalcount (if (= globalcount 1) "" "es")
			  (query-replace-descr regexp)))
	  (setq end (point))
	  (add-text-properties beg end (when title-face
					 `(font-lock-face ,title-face))))
	(goto-char (point-min)))
      (if coding
	  ;; CODING is buffer-file-coding-system of the first buffer
	  ;; that locally binds it.  Let's use it also for the output
	  ;; buffer.
	  (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      globalcount)))

(defun occur-engine-line (beg end &optional keep-props)
  (if (and keep-props (if (boundp 'jit-lock-mode) jit-lock-mode)
	   (text-property-not-all beg end 'fontified t))
      (if (fboundp 'jit-lock-fontify-now)
	  (jit-lock-fontify-now beg end)))
  (if (and keep-props (not (eq occur-excluded-properties t)))
      (let ((str (buffer-substring beg end)))
	(remove-list-of-text-properties
	 0 (length str) occur-excluded-properties str)
	str)
    (buffer-substring-no-properties beg end)))

(defun occur-engine-add-prefix (lines)
  (mapcar
   #'(lambda (line)
       (concat "       :" line "\n"))
   lines))

(defun occur-accumulate-lines (count &optional keep-props pt)
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((forwardp (> count 0))
	  result beg end moved)
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(and (bobp) (not moved)))))
	(setq count (+ count (if forwardp -1 1)))
	(setq beg (line-beginning-position)
	      end (line-end-position))
	(push (occur-engine-line beg end keep-props) result)
	(setq moved (= 0 (forward-line (if forwardp 1 -1)))))
      (nreverse result))))

;; Generate context display for occur.
;; OUT-LINE is the line where the match is.
;; NLINES and KEEP-PROPS are args to occur-engine.
;; LINES is line count of the current match,
;; PREV-LINES is line count of the previous match,
;; PREV-AFTER-LINES is a list of after-context lines of the previous match.
;; Generate a list of lines, add prefixes to all but OUT-LINE,
;; then concatenate them all together.
(defun occur-context-lines (out-line nlines keep-props begpt endpt
				     lines prev-lines prev-after-lines)
  ;; Find after- and before-context lines of the current match.
  (let ((before-lines
	 (nreverse (cdr (occur-accumulate-lines
			 (- (1+ (abs nlines))) keep-props begpt))))
	(after-lines
	 (cdr (occur-accumulate-lines
	       (1+ nlines) keep-props endpt)))
	separator)

    ;; Combine after-lines of the previous match
    ;; with before-lines of the current match.

    (when prev-after-lines
      ;; Don't overlap prev after-lines with current before-lines.
      (if (>= (+ prev-lines (length prev-after-lines))
	      (- lines      (length before-lines)))
	  (setq prev-after-lines
		(butlast prev-after-lines
			 (- (length prev-after-lines)
			    (- lines prev-lines (length before-lines) 1))))
	;; Separate non-overlapping context lines with a dashed line.
	(setq separator "-------\n")))

    (when prev-lines
      ;; Don't overlap current before-lines with previous match line.
      (if (<= (- lines (length before-lines))
	      prev-lines)
	  (setq before-lines
		(nthcdr (- (length before-lines)
			   (- lines prev-lines 1))
			before-lines))
	;; Separate non-overlapping before-context lines.
	(unless (> nlines 0)
	  (setq separator "-------\n"))))

    (list
     ;; Return a list where the first element is the output line.
     (apply #'concat
	    (append
	     (and prev-after-lines
		  (occur-engine-add-prefix prev-after-lines))
	     (and separator (list separator))
	     (occur-engine-add-prefix before-lines)
	     (list out-line)))
     ;; And the second element is the list of context after-lines.
     (if (> nlines 0) after-lines))))


;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match,
E to edit the replacement string"
  "Help message while in `query-replace'.")

(defvar query-replace-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'act)
    (define-key map "\d" 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "e" 'edit-replacement)
    (define-key map "E" 'edit-replacement)
    (define-key map "," 'act-and-show)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    (define-key map "." 'act-and-exit)
    (define-key map "\C-r" 'edit)
    (define-key map "\C-w" 'delete-and-edit)
    (define-key map "\C-l" 'recenter)
    (define-key map "!" 'automatic)
    (define-key map "^" 'backup)
    (define-key map "\C-h" 'help)
    (define-key map [f1] 'help)
    (define-key map [help] 'help)
    (define-key map "?" 'help)
    (define-key map "\C-g" 'quit)
    (define-key map "\C-]" 'quit)
    (define-key map "\e" 'exit-prefix)
    (define-key map [escape] 'exit-prefix)
    map)
  "Keymap that defines the responses to questions in `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`exit', `act-and-exit', `edit', `edit-replacement', `delete-and-edit',
`recenter', `automatic', `backup', `exit-prefix', `quit', and `help'.")

(defvar multi-query-replace-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map query-replace-map)
    (define-key map "Y" 'automatic-all)
    (define-key map "N" 'exit-current)
    map)
  "Keymap that defines additional bindings for multi-buffer replacements.
It extends its parent map `query-replace-map' with new bindings to
operate on a set of buffers/files.  The difference with its parent map
is the additional answers `automatic-all' to replace all remaining
matches in all remaining buffers with no more questions, and
`exit-current' to skip remaining matches in the current buffer
and to continue with the next buffer in the sequence.")

(defun replace-match-string-symbols (n)
  "Process a list (and any sub-lists), expanding certain symbols.
Symbol  Expands To
N     (match-string N)           (where N is a string of digits)
#N    (string-to-number (match-string N))
&     (match-string 0)
#&    (string-to-number (match-string 0))
#     replace-count

Note that these symbols must be preceded by a backslash in order to
type them using Lisp syntax."
  (while (consp n)
    (cond
     ((consp (car n))
      (replace-match-string-symbols (car n))) ;Process sub-list
     ((symbolp (car n))
      (let ((name (symbol-name (car n))))
        (cond
         ((string-match "^[0-9]+$" name)
          (setcar n (list 'match-string (string-to-number name))))
         ((string-match "^#[0-9]+$" name)
          (setcar n (list 'string-to-number
                          (list 'match-string
                                (string-to-number (substring name 1))))))
         ((string= "&" name)
          (setcar n '(match-string 0)))
         ((string= "#&" name)
          (setcar n '(string-to-number (match-string 0))))
	 ((string= "#" name)
	  (setcar n 'replace-count))))))
    (setq n (cdr n))))

(defun replace-eval-replacement (expression count)
  (let* ((replace-count count)
         (replacement (eval expression)))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-quote (replacement)
  "Quote a replacement string.
This just doubles all backslashes in REPLACEMENT and
returns the resulting string.  If REPLACEMENT is not
a string, it is first passed through `prin1-to-string'
with the `noescape' argument set.

`match-data' is preserved across the call."
  (save-match-data
    (replace-regexp-in-string "\\\\" "\\\\"
			      (if (stringp replacement)
				  replacement
				(prin1-to-string replacement t))
			      t t)))

(defun replace-loop-through-replacements (data count)
  ;; DATA is a vector containing the following values:
  ;;   0 next-rotate-count
  ;;   1 repeat-count
  ;;   2 next-replacement
  ;;   3 replacements
  (if (= (aref data 0) count)
      (progn
        (aset data 0 (+ count (aref data 1)))
        (let ((next (cdr (aref data 2))))
          (aset data 2 (if (consp next) next (aref data 3))))))
  (car (aref data 2)))

(defun replace-match-data (integers reuse &optional new)
  "Like `match-data', but markers in REUSE get invalidated.
If NEW is non-nil, it is set and returned instead of fresh data,
but coerced to the correct value of INTEGERS."
  (or (and new
	   (progn
	     (set-match-data new)
	     (and (eq new reuse)
		  (eq (null integers) (markerp (car reuse)))
		  new)))
      (match-data integers reuse t)))

(defun replace-match-maybe-edit (newtext fixedcase literal noedit match-data)
  "Make a replacement with `replace-match', editing `\\?'.
NEWTEXT, FIXEDCASE, LITERAL are just passed on.  If NOEDIT is true, no
check for `\\?' is made to save time.  MATCH-DATA is used for the
replacement.  In case editing is done, it is changed to use markers.

The return value is non-nil if there has been no `\\?' or NOEDIT was
passed in.  If LITERAL is set, no checking is done, anyway."
  (unless (or literal noedit)
    (setq noedit t)
    (while (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\?\\)"
			 newtext)
      (setq newtext
	    (read-string "Edit replacement string: "
                         (prog1
                             (cons
                              (replace-match "" t t newtext 3)
                              (1+ (match-beginning 3)))
                           (setq match-data
                                 (replace-match-data
                                  nil match-data match-data))))
	    noedit nil)))
  (set-match-data match-data)
  (replace-match newtext fixedcase literal)
  noedit)

(defvar replace-search-function 'search-forward
  "Function to use when searching for strings to replace.
It is used by `query-replace' and `replace-string', and is called
with three arguments, as if it were `search-forward'.")

(defvar replace-re-search-function 're-search-forward
  "Function to use when searching for regexps to replace.
It is used by `query-replace-regexp', `replace-regexp',
`query-replace-regexp-eval', and `map-query-replace-regexp'.
It is called with three arguments, as if it were
`re-search-forward'.")

(defun perform-replace (from-string replacements
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map start end)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:

  (while (re-search-forward \"foo[ \\t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))

which will run faster and probably do exactly what you want.  Please
see the documentation of `replace-match' to find out how to simulate
`case-replace'.

This function returns nil if and only if there were no matches to
make, or the user didn't cancel the call."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((case-fold-search
	  (if (and case-fold-search search-upper-case)
	      (isearch-no-upper-case-p from-string regexp-flag)
	    case-fold-search))
         (nocasify (not (and case-replace case-fold-search)))
         (literal (or (not regexp-flag) (eq regexp-flag 'literal)))
         (search-function
	  (if regexp-flag
	      replace-re-search-function
	    replace-search-function))
         (search-string from-string)
         (real-match-data nil)       ; The match data for the current match.
         (next-replacement nil)
         ;; This is non-nil if we know there is nothing for the user
         ;; to edit in the replacement.
         (noedit nil)
         (keep-going t)
         (stack nil)
         (replace-count 0)
         (nonempty-match nil)
	 (multi-buffer nil)
	 (recenter-last-op nil)	; Start cycling order with initial position.

         ;; If non-nil, it is marker saying where in the buffer to stop.
         (limit nil)

         ;; Data for the next match.  If a cons, it has the same format as
         ;; (match-data); otherwise it is t if a match is possible at point.
         (match-again t)

         (message
          (if query-flag
              (apply 'propertize
                     (substitute-command-keys
                      "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) ")
                     minibuffer-prompt-properties))))

    ;; If region is active, in Transient Mark mode, operate on region.
    (when start
      (setq limit (copy-marker (max start end)))
      (goto-char (min start end))
      (deactivate-mark))

    ;; If last typed key in previous call of multi-buffer perform-replace
    ;; was `automatic-all', don't ask more questions in next files
    (when (eq (lookup-key map (vector last-input-event)) 'automatic-all)
      (setq query-flag nil multi-buffer t))

    ;; REPLACEMENTS is either a string, a list of strings, or a cons cell
    ;; containing a function and its first argument.  The function is
    ;; called to generate each replacement like this:
    ;;   (funcall (car replacements) (cdr replacements) replace-count)
    ;; It must return a string.
    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      (setq replacements (cons 'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (or (eobp) (and limit (>= (point) limit))))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (cond ((consp match-again)
				 (goto-char (nth 1 match-again))
				 (replace-match-data
				  t real-match-data match-again))
				;; MATCH-AGAIN non-nil means accept an
				;; adjacent match.
				(match-again
				 (and
				  (funcall search-function search-string
					   limit t)
				  ;; For speed, use only integers and
				  ;; reuse the list used last time.
				  (replace-match-data t real-match-data)))
				((and (< (1+ (point)) (point-max))
				      (or (null limit)
					  (< (1+ (point)) limit)))
				 ;; If not accepting adjacent matches,
				 ;; move one char to the right before
				 ;; searching again.  Undo the motion
				 ;; if the search fails.
				 (let ((opoint (point)))
				   (forward-char 1)
				   (if (funcall
					search-function search-string
					limit t)
				       (replace-match-data
					t real-match-data)
				     (goto-char opoint)
				     nil))))))

	  ;; Record whether the match is nonempty, to avoid an infinite loop
	  ;; repeatedly matching the same empty string.
	  (setq nonempty-match
		(/= (nth 0 real-match-data) (nth 1 real-match-data)))

	  ;; If the match is empty, record that the next one can't be
	  ;; adjacent.

	  ;; Otherwise, if matching a regular expression, do the next
	  ;; match now, since the replacement for this match may
	  ;; affect whether the next match is adjacent to this one.
	  ;; If that match is empty, don't use it.
	  (setq match-again
		(and nonempty-match
		     (or (not regexp-flag)
			 (and (looking-at search-string)
			      (let ((match (match-data)))
				(and (/= (nth 0 match) (nth 1 match))
				     match))))))

	  ;; Optionally ignore matches that have a read-only property.
	  (unless (and query-replace-skip-read-only
		       (text-property-not-all
			(nth 0 real-match-data) (nth 1 real-match-data)
			'read-only nil))

	    ;; Calculate the replacement string, if necessary.
	    (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)))
	    (if (not query-flag)
		(progn
		  (unless (or literal noedit)
		    (replace-highlight
		     (nth 0 real-match-data) (nth 1 real-match-data)
		     start end search-string
		     (or delimited-flag regexp-flag) case-fold-search))
		  (setq noedit
			(replace-match-maybe-edit
			 next-replacement nocasify literal
			 noedit real-match-data)
			replace-count (1+ replace-count)))
	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this
		;; occurrence.  Any command that sets `done' should
		;; leave behind proper match data for the stack.
		;; Commands not setting `done' need to adjust
		;; `real-match-data'.
		(while (not done)
		  (set-match-data real-match-data)
		  (replace-highlight
		   (match-beginning 0) (match-end 0)
		   start end search-string
		   (or delimited-flag regexp-flag) case-fold-search)
		  ;; Bind message-log-max so we don't fill up the message log
		  ;; with a bunch of identical messages.
		  (let ((message-log-max nil)
			(replacement-presentation
			 (if query-replace-show-replacement
			     (save-match-data
			       (set-match-data real-match-data)
			       (match-substitute-replacement next-replacement
							     nocasify literal))
			   next-replacement)))
		    (message message
                             (query-replace-descr from-string)
                             (query-replace-descr replacement-presentation)))
		  (setq key (read-event))
		  ;; Necessary in case something happens during read-event
		  ;; that clobbers the match data.
		  (set-match-data real-match-data)
		  (setq key (vector key))
		  (setq def (lookup-key map key))
		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (with-output-to-temp-buffer "*Help*"
			   (princ
			    (concat "Query replacing "
				    (if delimited-flag "word " "")
				    (if regexp-flag "regexp " "")
				    from-string " with "
				    next-replacement ".\n\n"
				    (substitute-command-keys
				     query-replace-help)))
			   (with-current-buffer standard-output
			     (help-mode))))
			((eq def 'exit)
			 (setq keep-going nil)
			 (setq done t))
			((eq def 'exit-current)
			 (setq multi-buffer t keep-going nil done t))
			((eq def 'backup)
			 (if stack
			     (let ((elt (pop stack)))
			       (goto-char (nth 0 elt))
			       (setq replaced (nth 1 elt)
				     real-match-data
				     (replace-match-data
				      t real-match-data
				      (nth 2 elt))))
			   (message "No previous match")
			   (ding 'no-terminate)
			   (sit-for 1)))
			((eq def 'act)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq done t replaced t))
			((eq def 'act-and-exit)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq keep-going nil)
			 (setq done t replaced t))
			((eq def 'act-and-show)
			 (if (not replaced)
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)
				   real-match-data (replace-match-data
						    t real-match-data)
				   replaced t)))
			((or (eq def 'automatic) (eq def 'automatic-all))
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq done t query-flag nil replaced t)
			 (if (eq def 'automatic-all) (setq multi-buffer t)))
			((eq def 'skip)
			 (setq done t))
			((eq def 'recenter)
			 ;; `this-command' has the value `query-replace',
			 ;; so we need to bind it to `recenter-top-bottom'
			 ;; to allow it to detect a sequence of `C-l'.
			 (let ((this-command 'recenter-top-bottom)
			       (last-command 'recenter-top-bottom))
			   (recenter-top-bottom)))
			((eq def 'edit)
			 (let ((opos (point-marker)))
			   (setq real-match-data (replace-match-data
						  nil real-match-data
						  real-match-data))
			   (goto-char (match-beginning 0))
			   (save-excursion
			     (save-window-excursion
			       (recursive-edit)))
			   (goto-char opos)
			   (set-marker opos nil))
			 ;; Before we make the replacement,
			 ;; decide whether the search string
			 ;; can match again just after this match.
			 (if (and regexp-flag nonempty-match)
			     (setq match-again (and (looking-at search-string)
						    (match-data)))))
			;; Edit replacement.
			((eq def 'edit-replacement)
			 (setq real-match-data (replace-match-data
						nil real-match-data
						real-match-data)
			       next-replacement
			       (read-string "Edit replacement string: "
                                            next-replacement)
			       noedit nil)
			 (if replaced
			     (set-match-data real-match-data)
			   (setq noedit
				 (replace-match-maybe-edit
				  next-replacement nocasify literal noedit
				  real-match-data)
				 replaced t))
			 (setq done t))

			((eq def 'delete-and-edit)
			 (replace-match "" t t)
			 (setq real-match-data (replace-match-data
						nil real-match-data))
			 (replace-dehighlight)
			 (save-excursion (recursive-edit))
			 (setq replaced t))
			;; Note: we do not need to treat `exit-prefix'
			;; specially here, since we reread
			;; any unrecognized character.
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t)))
		  (when query-replace-lazy-highlight
		    ;; Force lazy rehighlighting only after replacements.
		    (if (not (memq def '(skip backup)))
			(setq isearch-lazy-highlight-last-string nil)))
		  (unless (eq def 'recenter)
		    ;; Reset recenter cycling order to initial position.
		    (setq recenter-last-op nil)))
		;; Record previous position for ^ when we move on.
		;; Change markers to numbers in the match data
		;; since lots of markers slow down editing.
		(push (list (point) replaced
;;;  If the replacement has already happened, all we need is the
;;;  current match start and end.  We could get this with a trivial
;;;  match like
;;;  (save-excursion (goto-char (match-beginning 0))
;;;		     (search-forward (match-string 0))
;;;                  (match-data t))
;;;  if we really wanted to avoid manually constructing match data.
;;;  Adding current-buffer is necessary so that match-data calls can
;;;  return markers which are appropriate for editing.
			    (if replaced
				(list
				 (match-beginning 0)
				 (match-end 0)
				 (current-buffer))
			      (match-data t)))
		      stack)))))

      (replace-dehighlight))
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (or (and keep-going stack) multi-buffer)))

(defvar isearch-error)
(defvar isearch-forward)
(defvar isearch-case-fold-search)
(defvar isearch-string)

(defvar replace-overlay nil)

(defun replace-highlight (match-beg match-end range-beg range-end
			  string regexp case-fold)
  (if query-replace-highlight
      (if replace-overlay
	  (move-overlay replace-overlay match-beg match-end (current-buffer))
	(setq replace-overlay (make-overlay match-beg match-end))
	(overlay-put replace-overlay 'priority 1001) ;higher than lazy overlays
	(overlay-put replace-overlay 'face 'query-replace)))
  (if query-replace-lazy-highlight
      (let ((isearch-string string)
	    (isearch-regexp regexp)
	    ;; Set isearch-word to nil because word-replace is regexp-based,
	    ;; so `isearch-search-fun' should not use `word-search-forward'.
	    (isearch-word nil)
	    (search-whitespace-regexp nil)
	    (isearch-case-fold-search case-fold)
	    (isearch-forward t)
	    (isearch-error nil))
	(isearch-lazy-highlight-new-loop range-beg range-end))))

(defun replace-dehighlight ()
  (when replace-overlay
    (delete-overlay replace-overlay))
  (when query-replace-lazy-highlight
    (lazy-highlight-cleanup lazy-highlight-cleanup)
    (setq isearch-lazy-highlight-last-string nil)))

;;; replace.el ends here
