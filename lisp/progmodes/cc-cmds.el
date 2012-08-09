;;; cc-cmds.el --- user level commands for CC Mode

;; Copyright (C) 1985, 1987, 1992-2012  Free Software Foundation, Inc.

;; Authors:    2003- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs
;;             1987 Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Keywords:   c languages
;; Package:    cc-mode

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

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-engine)

;; Silence the compiler.
(cc-bytecomp-defun delete-forward-p)	; XEmacs
(cc-bytecomp-defvar filladapt-mode)	; c-fill-paragraph contains a kludge
					; which looks at this.

;; Indentation / Display syntax functions
(defvar c-fix-backslashes t)

(defun c-indent-line (&optional syntax quiet ignore-point-pos)
  "Indent the current line according to the syntactic context,
if `c-syntactic-indentation' is non-nil.  Optional SYNTAX is the
syntactic information for the current line.  Be silent about syntactic
errors if the optional argument QUIET is non-nil, even if
`c-report-syntactic-errors' is non-nil.  Normally the position of
point is used to decide where the old indentation is on a lines that
is otherwise empty \(ignoring any line continuation backslash), but
that's not done if IGNORE-POINT-POS is non-nil.  Returns the amount of
indentation change \(in columns)."

  (let ((line-cont-backslash (save-excursion
			       (end-of-line)
			       (eq (char-before) ?\\)))
	(c-fix-backslashes c-fix-backslashes)
	bs-col
	shift-amt)
    (when (and (not ignore-point-pos)
	       (save-excursion
		 (beginning-of-line)
		 (looking-at (if line-cont-backslash
				 ;; Don't use "\\s " - ^L doesn't count as WS
				 ;; here
				 "\\([ \t]*\\)\\\\$"
			       "\\([ \t]*\\)$")))
	       (<= (point) (match-end 1)))
      ;; Delete all whitespace after point if there's only whitespace
      ;; on the line, so that any code that does back-to-indentation
      ;; or similar gets the current column in this case.  If this
      ;; removes a line continuation backslash it'll be restored
      ;; at the end.
      (unless c-auto-align-backslashes
	;; Should try to keep the backslash alignment
	;; in this case.
	(save-excursion
	  (goto-char (match-end 0))
	  (setq bs-col (1- (current-column)))))
      (delete-region (point) (match-end 0))
      (setq c-fix-backslashes t))
    (if c-syntactic-indentation
	(setq c-parsing-error
	      (or (let ((c-parsing-error nil)
			(c-syntactic-context
			 (or syntax
			     (and (boundp 'c-syntactic-context)
				  c-syntactic-context))))
		    (c-save-buffer-state (indent)
		      (unless c-syntactic-context
			(setq c-syntactic-context (c-guess-basic-syntax)))
		      (setq indent (c-get-syntactic-indentation
				    c-syntactic-context))
		      (and (not (c-echo-parsing-error quiet))
			   c-echo-syntactic-information-p
			   (message "syntax: %s, indent: %d"
				    c-syntactic-context indent))
		      (setq shift-amt (- indent (current-indentation))))
		    (c-shift-line-indentation shift-amt)
		    (run-hooks 'c-special-indent-hook)
		    c-parsing-error)
		  c-parsing-error))
      (let ((indent 0))
	(save-excursion
	  (while (and (= (forward-line -1) 0)
		      (if (looking-at "\\s *\\\\?$")
			  t
			(setq indent (current-indentation))
			nil))))
	(setq shift-amt (- indent (current-indentation)))
	(c-shift-line-indentation shift-amt)))
    (when (and c-fix-backslashes line-cont-backslash)
      (if bs-col
	  (save-excursion
	    (indent-to bs-col)
	    (insert ?\\))
	(when c-auto-align-backslashes
	  ;; Realign the line continuation backslash.
	  (c-backslash-region (point) (point) nil t))))
    shift-amt))

(defun c-newline-and-indent (&optional newline-arg)
  "Insert a newline and indent the new line.
This function fixes line continuation backslashes if inside a macro,
and takes care to set the indentation before calling
`indent-according-to-mode', so that lineup functions like
`c-lineup-dont-change' works better."

  ;; TODO: Backslashes before eol in comments and literals aren't
  ;; kept intact.
  (let ((c-macro-start (c-query-macro-start))
	;; Avoid calling c-backslash-region from c-indent-line if it's
	;; called during the newline call, which can happen due to
	;; c-electric-continued-statement, for example.  We also don't
	;; want any backslash alignment from indent-according-to-mode.
	(c-fix-backslashes nil)
	has-backslash insert-backslash
	start col)
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (while (and (looking-at "[ \t]*\\\\?$")
		  (= (forward-line -1) 0)))
      (setq col (current-indentation)))
    (when c-macro-start
      (if (and (eolp) (eq (char-before) ?\\))
	  (setq insert-backslash t
		has-backslash t)
	(setq has-backslash (eq (char-before (c-point 'eol)) ?\\))))
    (newline newline-arg)
    (indent-to col)
    (when c-macro-start
      (if insert-backslash
	  (progn
	    ;; The backslash stayed on the previous line.  Insert one
	    ;; before calling c-backslash-region, so that
	    ;; bs-col-after-end in it works better.  Fixup the
	    ;; backslashes on the newly inserted line.
	    (insert ?\\)
	    (backward-char)
	    (c-backslash-region (point) (point) nil t))
	;; The backslash moved to the new line, if there was any.  Let
	;; c-backslash-region fix a backslash on the previous line,
	;; and the one that might be on the new line.
	;; c-auto-align-backslashes is intentionally ignored here;
	;; maybe the moved backslash should be left alone if it's set,
	;; but we fix both lines on the grounds that the old backslash
	;; has been moved anyway and is now in a different context.
	(c-backslash-region start (if has-backslash (point) start) nil t)))
    (when c-syntactic-indentation
      ;; Reindent syntactically.  The indentation done above is not
      ;; wasted, since c-indent-line might look at the current
      ;; indentation.
      (let ((c-syntactic-context (c-save-buffer-state nil
				   (c-guess-basic-syntax))))
	;; We temporarily insert another line break, so that the
	;; lineup functions will see the line as empty.  That makes
	;; e.g. c-lineup-cpp-define more intuitive since it then
	;; proceeds to the preceding line in this case.
	(insert ?\n)
	(delete-horizontal-space)
	(setq start (- (point-max) (point)))
	(unwind-protect
	    (progn
	      (backward-char)
	      (indent-according-to-mode))
	  (goto-char (- (point-max) start))
	  (delete-char -1)))
      (when has-backslash
	;; Must align the backslash again after reindentation.  The
	;; c-backslash-region call above can't be optimized to ignore
	;; this line, since it then won't align correctly with the
	;; lines below if the first line in the macro is broken.
	(c-backslash-region (point) (point) nil t)))))

(defun c-show-syntactic-information (arg)
  "Show syntactic information for current line.
With universal argument, inserts the analysis as a comment on that line."
  (interactive "P")
  (let* ((c-parsing-error nil)
	 (syntax (if (boundp 'c-syntactic-context)
		     ;; Use `c-syntactic-context' in the same way as
		     ;; `c-indent-line', to be consistent.
		     c-syntactic-context
		   (c-save-buffer-state nil
		     (c-guess-basic-syntax)))))
    (if (not (consp arg))
	(let (elem pos ols)
	  (message "Syntactic analysis: %s" syntax)
	  (unwind-protect
	      (progn
		(while syntax
		  (setq elem (pop syntax))
		  (when (setq pos (c-langelem-pos elem))
		    (push (c-put-overlay pos (1+ pos)
					 'face 'highlight)
			  ols))
		  (when (setq pos (c-langelem-2nd-pos elem))
		    (push (c-put-overlay pos (1+ pos)
					 'face 'secondary-selection)
			  ols)))
		(sit-for 10))
	    (while ols
	      (c-delete-overlay (pop ols)))))
      (indent-for-comment)
      (insert-and-inherit (format "%s" syntax))
      ))
  (c-keep-region-active))

(defun c-syntactic-information-on-region (from to)
  "Insert a comment with the syntactic analysis on every line in the region."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (not (eobp))
	(c-show-syntactic-information '(0))
	(forward-line)))))


;; Minor mode functions.
(defun c-update-modeline ()
  (let ((fmt (format "/%s%s%s%s"
		     (if c-electric-flag "l" "")
		     (if (and c-electric-flag c-auto-newline)
			 "a" "")
		     (if c-hungry-delete-key "h" "")
		     (if (and
			  ;; subword might not be loaded.
			  (boundp 'subword-mode)
			  (symbol-value 'subword-mode))
			 "w"
		       "")))
        ;; FIXME: Derived modes might want to use something else
        ;; than a string for `mode-name'.
	(bare-mode-name (if (string-match "\\(^[^/]*\\)/" mode-name)
			    (match-string 1 mode-name)
			  mode-name)))
;;     (setq c-submode-indicators
;; 	  (if (> (length fmt) 1)
;; 	      fmt))
    (setq mode-name
	  (if (> (length fmt) 1)
	      (concat bare-mode-name fmt)
	bare-mode-name))
    (force-mode-line-update)))

(defun c-toggle-syntactic-indentation (&optional arg)
  "Toggle syntactic indentation.
Optional numeric ARG, if supplied, turns on syntactic indentation when
positive, turns it off when negative, and just toggles it when zero or
left out.

When syntactic indentation is turned on (the default), the indentation
functions and the electric keys indent according to the syntactic
context keys, when applicable.

When it's turned off, the electric keys don't reindent, the indentation
functions indents every new line to the same level as the previous
nonempty line, and \\[c-indent-command] adjusts the indentation in steps
specified by `c-basic-offset'.  The indentation style has no effect in
this mode, nor any of the indentation associated variables,
e.g. `c-special-indent-hook'.

This command sets the variable `c-syntactic-indentation'."
  (interactive "P")
  (setq c-syntactic-indentation
	(c-calculate-state arg c-syntactic-indentation))
  (c-keep-region-active))

(defun c-toggle-auto-newline (&optional arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied, turns on auto-newline when
positive, turns it off when negative, and just toggles it when zero or
left out.

Turning on auto-newline automatically enables electric indentation.

When the auto-newline feature is enabled (indicated by \"/la\" on the
modeline after the mode name) newlines are automatically inserted
after special characters such as brace, comma, semi-colon, and colon."
  (interactive "P")
  (setq c-auto-newline
	(c-calculate-state arg (and c-auto-newline c-electric-flag)))
  (if c-auto-newline (setq c-electric-flag t))
  (c-update-modeline)
  (c-keep-region-active))

(defalias 'c-toggle-auto-state 'c-toggle-auto-newline)
(make-obsolete 'c-toggle-auto-state 'c-toggle-auto-newline "22.1")

(defun c-toggle-hungry-state (&optional arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied, turns on hungry-delete when
positive, turns it off when negative, and just toggles it when zero or
left out.

When the hungry-delete-key feature is enabled (indicated by \"/h\" on
the modeline after the mode name) the delete key gobbles all preceding
whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (&optional arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied, turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero or left out.

See `c-toggle-auto-newline' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (setq c-electric-flag (c-calculate-state arg c-electric-flag))
  (c-update-modeline)
  (c-keep-region-active))


;; Electric keys

(defun c-electric-backspace (arg)
  "Delete the preceding character or whitespace.
If `c-hungry-delete-key' is non-nil (indicated by \"/h\" on the mode
line) then all preceding whitespace is consumed.  If however a prefix
argument is supplied, or `c-hungry-delete-key' is nil, or point is
inside a literal then the function in the variable
`c-backspace-function' is called."
  (interactive "*P")
  (if (c-save-buffer-state ()
	(or (not c-hungry-delete-key)
	    arg
	    (c-in-literal)))
      (funcall c-backspace-function (prefix-numeric-value arg))
    (c-hungry-delete-backwards)))

(defun c-hungry-delete-backwards ()
  "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also \\[c-hungry-delete-forward]."
  (interactive)
  (let ((here (point)))
    (c-skip-ws-backward)
    (if (/= (point) here)
	(delete-region (point) here)
      (funcall c-backspace-function 1))))

(defalias 'c-hungry-backspace 'c-hungry-delete-backwards)

(defun c-electric-delete-forward (arg)
  "Delete the following character or whitespace.
If `c-hungry-delete-key' is non-nil (indicated by \"/h\" on the mode
line) then all following whitespace is consumed.  If however a prefix
argument is supplied, or `c-hungry-delete-key' is nil, or point is
inside a literal then the function in the variable `c-delete-function'
is called."
  (interactive "*P")
  (if (c-save-buffer-state ()
	(or (not c-hungry-delete-key)
	    arg
	    (c-in-literal)))
      (funcall c-delete-function (prefix-numeric-value arg))
    (c-hungry-delete-forward)))

(defun c-hungry-delete-forward ()
  "Delete the following character or all following whitespace
up to the next non-whitespace character.
See also \\[c-hungry-delete-backwards]."
  (interactive)
  (let ((here (point)))
    (c-skip-ws-forward)
    (if (/= (point) here)
	(delete-region (point) here)
      (funcall c-delete-function 1))))

;; This function is only used in XEmacs.
(defun c-electric-delete (arg)
  "Deletes preceding or following character or whitespace.
This function either deletes forward as `c-electric-delete-forward' or
backward as `c-electric-backspace', depending on the configuration: If
the function `delete-forward-p' is defined and returns non-nil, it
deletes forward.  Otherwise it deletes backward.

Note: This is the way in XEmacs to choose the correct action for the
\[delete] key, whichever key that means.  Other flavors don't use this
function to control that."
  (interactive "*P")
  (if (and (fboundp 'delete-forward-p)
	   (delete-forward-p))
      (c-electric-delete-forward arg)
    (c-electric-backspace arg)))

;; This function is only used in XEmacs.
(defun c-hungry-delete ()
  "Delete a non-whitespace char, or all whitespace up to the next non-whitespace char.
The direction of deletion depends on the configuration: If the
function `delete-forward-p' is defined and returns non-nil, it deletes
forward using `c-hungry-delete-forward'.  Otherwise it deletes
backward using `c-hungry-backspace'.

Note: This is the way in XEmacs to choose the correct action for the
\[delete] key, whichever key that means.  Other flavors don't use this
function to control that."
  (interactive)
  (if (and (fboundp 'delete-forward-p)
	   (delete-forward-p))
      (c-hungry-delete-forward)
    (c-hungry-delete-backwards)))

(defun c-electric-pound (arg)
  "Insert a \"#\".
If `c-electric-flag' is set, handle it specially according to the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if point is
inside a literal or a macro, nothing special happens."
  (interactive "*P")
  (if (c-save-buffer-state ()
	(or arg
	    (not c-electric-flag)
	    (not (memq 'alignleft c-electric-pound-behavior))
	    (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (save-excursion
	      (and (= (forward-line -1) 0)
		   (progn (end-of-line)
			  (eq (char-before) ?\\))))
	    (c-in-literal)))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert last-command-event)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-point-syntax ()
  ;; Return the syntactic context of the construct at point.  (This is NOT
  ;; nec. the same as the s.c. of the line point is on).  N.B. This won't work
  ;; between the `#' of a cpp thing and what follows (see c-opt-cpp-prefix).
  (c-save-buffer-state (;; shut this up too
	(c-echo-syntactic-information-p nil)
	syntax)
    (c-tentative-buffer-changes
      ;; insert a newline to isolate the construct at point for syntactic
      ;; analysis.
      (insert-char ?\n 1)
      ;; In AWK (etc.) or in a macro, make sure this CR hasn't changed
      ;; the syntax.  (There might already be an escaped NL there.)
      (when (or (c-at-vsemi-p (1- (point)))
		(let ((pt (point)))
		  (save-excursion
		    (backward-char)
		    (and (c-beginning-of-macro)
			 (progn (c-end-of-macro)
				(< (point) pt))))))
	(backward-char)
	(insert-char ?\\ 1)
	(forward-char))
      (let ((c-syntactic-indentation-in-macros t)
	    (c-auto-newline-analysis t))
	;; Turn on syntactic macro analysis to help with auto
	;; newlines only.
	(setq syntax (c-guess-basic-syntax))
	nil))
    syntax))

(defun c-brace-newlines (syntax)
  ;; A brace stands at point.  SYNTAX is the syntactic context of this brace
  ;; (not necessarily the same as the S.C. of the line it is on).  Return
  ;; NEWLINES, the list containing some combination of the symbols `before'
  ;; and `after' saying where newlines should be inserted.
  (c-save-buffer-state
      ((syms
	;; This is the list of brace syntactic symbols that can hang.
	;; If any new ones are added to c-offsets-alist, they should be
	;; added here as well.
	;;
	;; The order of this list is important; if SYNTAX has several
	;; elements, the element that "wins" is the earliest in SYMS.
	'(arglist-cont-nonempty		; e.g. an array literal.
		     class-open class-close defun-open defun-close
		     inline-open inline-close
		     brace-list-open brace-list-close
		     brace-list-intro brace-entry-open
		     block-open block-close
		     substatement-open statement-case-open
		     extern-lang-open extern-lang-close
		     namespace-open namespace-close
		     module-open module-close
		     composition-open composition-close
		     inexpr-class-open inexpr-class-close
		     ;; `statement-cont' is here for the case with a brace
		     ;; list opener inside a statement.  C.f. CASE B.2 in
		     ;; `c-guess-continued-construct'.
		     statement-cont))
       ;; shut this up too
       (c-echo-syntactic-information-p nil)
       symb-newlines)		     ; e.g. (substatement-open . (after))

    (setq symb-newlines
	  ;; Do not try to insert newlines around a special
	  ;; (Pike-style) brace list.
	  (if (and c-special-brace-lists
		   (save-excursion
		     (c-safe (if (= (char-before) ?{)
				 (forward-char -1)
			       (c-forward-sexp -1))
			     (c-looking-at-special-brace-list))))
	      nil
	    ;; Seek the matching entry in c-hanging-braces-alist.
	    (or (c-lookup-lists
		 syms
		 ;; Substitute inexpr-class and class-open or
		 ;; class-close with inexpr-class-open or
		 ;; inexpr-class-close.
		 (if (assq 'inexpr-class syntax)
		     (cond ((assq 'class-open syntax)
			    '((inexpr-class-open)))
			   ((assq 'class-close syntax)
			    '((inexpr-class-close)))
			   (t syntax))
		   syntax)
		 c-hanging-braces-alist)
		'(ignore before after)))) ; Default, when not in c-h-b-l.

    ;; If syntax is a function symbol, then call it using the
    ;; defined semantics.
    (if (and (not (consp (cdr symb-newlines)))
	     (functionp (cdr symb-newlines)))
	(let ((c-syntactic-context syntax))
	  (funcall (cdr symb-newlines)
		   (car symb-newlines)
		   (point)))
      (cdr symb-newlines))))

(defun c-try-one-liner ()
  ;; Point is just after a newly inserted }.  If the non-whitespace
  ;; content of the braces is a single line of code, compact the whole
  ;; construct to a single line, if this line isn't too long.  The Right
  ;; Thing is done with comments.
  ;;
  ;; Point will be left after the }, regardless of whether the clean-up is
  ;; done.  Return NON-NIL if the clean-up happened, NIL if it didn't.

  (let ((here (point))
	(pos (- (point-max) (point)))
	mbeg1 mend1 mbeg4 mend4
	eol-col cmnt-pos cmnt-col cmnt-gap)

    (when
	(save-excursion
	  (save-restriction
	    ;; Avoid backtracking over a very large block.  The one we
	    ;; deal with here can never be more than three lines.
	    (narrow-to-region (save-excursion
				(forward-line -2)
				(point))
			      (point))
	    (and (c-safe (c-backward-sexp))
		 (progn
		   (forward-char)
		   (narrow-to-region (point) (1- here)) ; innards of {.}
		   (looking-at
		    (cc-eval-when-compile
		      (concat
		       "\\("		; (match-beginning 1)
		       "[ \t]*\\([\r\n][ \t]*\\)?" ; WS with opt. NL
		       "\\)"		; (match-end 1)
		       "[^ \t\r\n]+\\([ \t]+[^ \t\r\n]+\\)*" ; non-WS
		       "\\("		; (match-beginning 4)
		       "[ \t]*\\([\r\n][ \t]*\\)?" ; WS with opt. NL
		       "\\)\\'")))))))	; (match-end 4) at EOB.

      (if (c-tentative-buffer-changes
	    (setq mbeg1 (match-beginning 1) mend1 (match-end 1)
		  mbeg4 (match-beginning 4) mend4 (match-end 4))
	    (backward-char)		; back over the `}'
	    (save-excursion
	      (setq cmnt-pos (and (c-backward-single-comment)
				  (- (point) (- mend1 mbeg1)))))
	    (delete-region mbeg4 mend4)
	    (delete-region mbeg1 mend1)
	    (setq eol-col (save-excursion (end-of-line) (current-column)))

	    ;; Necessary to put the closing brace before any line
	    ;; oriented comment to keep it syntactically significant.
	    ;; This isn't necessary for block comments, but the result
	    ;; looks nicer anyway.
	    (when cmnt-pos
	      (delete-char 1)		; the `}' has blundered into a comment
	      (goto-char cmnt-pos)
	      (setq cmnt-col (1+ (current-column)))
	      (setq cmnt-pos (1+ cmnt-pos)) ; we're inserting a `}'
	      (c-skip-ws-backward)
	      (insert-char ?\} 1)	; reinsert the `}' before the comment.
	      (setq cmnt-gap (- cmnt-col (current-column)))
	      (when (zerop cmnt-gap)
		(insert-char ?\  1)	; Put a space before a bare comment.
		(setq cmnt-gap 1)))

	    (or (null c-max-one-liner-length)
		(zerop c-max-one-liner-length)
		(<= eol-col c-max-one-liner-length)
		;; Can we trim space before comment to make the line fit?
		(and cmnt-gap
		     (< (- eol-col cmnt-gap) c-max-one-liner-length)
		     (progn (goto-char cmnt-pos)
			    (backward-delete-char-untabify
			     (- eol-col c-max-one-liner-length))
			    t))))
	  (goto-char (- (point-max) pos))))))

(defun c-electric-brace (arg)
  "Insert a brace.

If `c-electric-flag' is non-nil, the brace is not inside a literal and a
numeric ARG hasn't been supplied, the command performs several electric
actions:

\(a) If the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) newlines are inserted before and after the brace as
directed by the settings in `c-hanging-braces-alist'.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, various newline cleanups based on the
settings of `c-cleanup-list' are done."

  (interactive "*P")
  (let (safepos literal
	;; We want to inhibit blinking the paren since this would be
	;; most disruptive.  We'll blink it ourselves later on.
	(old-blink-paren blink-paren-function)
	blink-paren-function case-fold-search)

    (c-save-buffer-state ()
      (setq safepos (c-safe-position (point) (c-parse-state))
	    literal (c-in-literal safepos)))

    ;; Insert the brace.  Note that expand-abbrev might reindent
    ;; the line here if there's a preceding "else" or something.
    (self-insert-command (prefix-numeric-value arg))

    (when (and c-electric-flag (not literal) (not arg))
      (if (not (looking-at "[ \t]*\\\\?$"))
	  (if c-syntactic-indentation
	      (indent-according-to-mode))

	(let ( ;; shut this up too
	      (c-echo-syntactic-information-p nil)
	      newlines
	      ln-syntax br-syntax syntax) ; Syntactic context of the original line,
			; of the brace itself, of the line the brace ends up on.
	  (c-save-buffer-state ((c-syntactic-indentation-in-macros t)
				(c-auto-newline-analysis t))
	    (setq ln-syntax (c-guess-basic-syntax)))
	  (if c-syntactic-indentation
	      (c-indent-line ln-syntax))

	  (when c-auto-newline
	    (backward-char)
	    (setq br-syntax (c-point-syntax)
		  newlines (c-brace-newlines br-syntax))

	    ;; Insert the BEFORE newline, if wanted, and reindent the newline.
	    (if (and (memq 'before newlines)
		     (> (current-column) (current-indentation)))
		(if c-syntactic-indentation
		    ;; Only a plain newline for now - it's indented
		    ;; after the cleanups when the line has its final
		    ;; appearance.
		    (newline)
		  (c-newline-and-indent)))
	    (forward-char)

	    ;; `syntax' is the syntactic context of the line which ends up
	    ;; with the brace on it.
	    (setq syntax (if (memq 'before newlines) br-syntax ln-syntax))

	    ;; Do all appropriate clean ups
	    (let ((here (point))
		  (pos (- (point-max) (point)))
		  mbeg mend
		  )

	      ;; `}': clean up empty defun braces
	      (when (c-save-buffer-state ()
		      (and (memq 'empty-defun-braces c-cleanup-list)
			   (eq last-command-event ?\})
			   (c-intersect-lists '(defun-close class-close inline-close)
					      syntax)
			   (progn
			     (forward-char -1)
			     (c-skip-ws-backward)
			     (eq (char-before) ?\{))
			   ;; make sure matching open brace isn't in a comment
			   (not (c-in-literal))))
		(delete-region (point) (1- here))
		(setq here (- (point-max) pos)))
	      (goto-char here)

	      ;; `}': compact to a one-liner defun?
	      (save-match-data
		(when
		    (and (eq last-command-event ?\})
			 (memq 'one-liner-defun c-cleanup-list)
			 (c-intersect-lists '(defun-close) syntax)
			 (c-try-one-liner))
		  (setq here (- (point-max) pos))))

	      ;; `{': clean up brace-else-brace and brace-elseif-brace
	      (when (eq last-command-event ?\{)
		(cond
		 ((and (memq 'brace-else-brace c-cleanup-list)
		       (re-search-backward
			(concat "}"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"else"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"{"
				"\\=")
			nil t))
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert-and-inherit "} else {"))
		 ((and (memq 'brace-elseif-brace c-cleanup-list)
		       (progn
			 (goto-char (1- here))
			 (setq mend (point))
			 (c-skip-ws-backward)
			 (setq mbeg (point))
			 (eq (char-before) ?\)))
		       (zerop (c-save-buffer-state nil (c-backward-token-2 1 t)))
		       (eq (char-after) ?\()
		      ; (progn
			; (setq tmp (point))
			 (re-search-backward
			  (concat "}"
				  "\\([ \t\n]\\|\\\\\n\\)*"
				  "else"
				  "\\([ \t\n]\\|\\\\\n\\)+"
				  "if"
				  "\\([ \t\n]\\|\\\\\n\\)*"
				  "\\=")
			  nil t);)
		       ;(eq (match-end 0) tmp);
			 )
		  (delete-region mbeg mend)
		  (goto-char mbeg)
		  (insert ?\ ))))

	      (goto-char (- (point-max) pos))

	      ;; Indent the line after the cleanups since it might
	      ;; very well indent differently due to them, e.g. if
	      ;; c-indent-one-line-block is used together with the
	      ;; one-liner-defun cleanup.
	      (when c-syntactic-indentation
		(c-indent-line)))

	    ;; does a newline go after the brace?
	    (if (memq 'after newlines)
		(c-newline-and-indent))
	    ))))

    ;; blink the paren
    (and (eq last-command-event ?\})
	 (not executing-kbd-macro)
	 old-blink-paren
	 (save-excursion
	   (c-save-buffer-state nil
	     (c-backward-syntactic-ws safepos))
	   (funcall old-blink-paren)))))

(defun c-electric-slash (arg)
  "Insert a slash character.

If the slash is inserted immediately after the comment prefix in a c-style
comment, the comment might get closed by removing whitespace and possibly
inserting a \"*\".  See the variable `c-cleanup-list'.

Indent the line as a comment, if:

  1. The slash is second of a \"//\" line oriented comment introducing
     token and we are on a comment-only-line, or

  2. The slash is part of a \"*/\" token that closes a block oriented
     comment.

If a numeric ARG is supplied, point is inside a literal, or
`c-syntactic-indentation' is nil or `c-electric-flag' is nil, indentation
is inhibited."
  (interactive "*P")
  (let ((literal (c-save-buffer-state () (c-in-literal)))
	indentp
	;; shut this up
	(c-echo-syntactic-information-p nil))

    ;; comment-close-slash cleanup?  This DOESN'T need `c-electric-flag' or
    ;; `c-syntactic-indentation' set.
    (when (and (not arg)
	       (eq literal 'c)
	       (memq 'comment-close-slash c-cleanup-list)
	       (eq last-command-event ?/)
	       (looking-at (concat "[ \t]*\\("
				   (regexp-quote comment-end) "\\)?$"))
	; (eq c-block-comment-ender "*/") ; C-style comments ALWAYS end in */
	       (save-excursion
		 (save-restriction
		   (narrow-to-region (point-min) (point))
		   (back-to-indentation)
		   (looking-at (concat c-current-comment-prefix "[ \t]*$")))))
      (delete-region (progn (forward-line 0) (point))
		     (progn (end-of-line) (point)))
      (insert-char ?* 1)) ; the / comes later. ; Do I need a t (retain sticky properties) here?

    (setq indentp (and (not arg)
		       c-syntactic-indentation
		       c-electric-flag
		       (eq last-command-event ?/)
		       (eq (char-before) (if literal ?* ?/))))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(indent-according-to-mode))))

(defun c-electric-star (arg)
  "Insert a star character.
If `c-electric-flag' and `c-syntactic-indentation' are both non-nil, and
the star is the second character of a C style comment starter on a
comment-only-line, indent the line as a comment.  If a numeric ARG is
supplied, point is inside a literal, or `c-syntactic-indentation' is nil,
this indentation is inhibited."

  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; if we are in a literal, or if arg is given do not reindent the
  ;; current line, unless this star introduces a comment-only line.
  (if (c-save-buffer-state ()
	(and c-syntactic-indentation
	     c-electric-flag
	     (not arg)
	     (eq (c-in-literal) 'c)
	     (eq (char-before) ?*)
	     (save-excursion
	       (forward-char -1)
	       (skip-chars-backward "*")
	       (if (eq (char-before) ?/)
		   (forward-char -1))
	       (skip-chars-backward " \t")
	       (bolp))))
      (let (c-echo-syntactic-information-p) ; shut this up
	(indent-according-to-mode))
    ))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.

If `c-electric-flag' is non-nil, point isn't inside a literal and a
numeric ARG hasn't been supplied, the command performs several electric
actions:

\(a) When the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) a newline might be inserted.  See the variable
`c-hanging-semi&comma-criteria' for how newline insertion is determined.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, a comma following a brace list or a
semicolon following a defun might be cleaned up, depending on the
settings of `c-cleanup-list'."
  (interactive "*P")
  (let* (lim literal c-syntactic-context
	 (here (point))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))

    (c-save-buffer-state ()
      (setq lim (c-most-enclosing-brace (c-parse-state))
	    literal (c-in-literal lim)))

    (self-insert-command (prefix-numeric-value arg))

    (if (and c-electric-flag (not literal) (not arg))
	;; do all cleanups and newline insertions if c-auto-newline is on.
	(if (or (not c-auto-newline)
		(not (looking-at "[ \t]*\\\\?$")))
	    (if c-syntactic-indentation
		(c-indent-line))
	  ;; clean ups: list-close-comma or defun-close-semi
	  (let ((pos (- (point-max) (point))))
	    (if (c-save-buffer-state ()
		  (and (or (and
			    (eq last-command-event ?,)
			    (memq 'list-close-comma c-cleanup-list))
			   (and
			    (eq last-command-event ?\;)
			    (memq 'defun-close-semi c-cleanup-list)))
		       (progn
			 (forward-char -1)
			 (c-skip-ws-backward)
			 (eq (char-before) ?}))
		       ;; make sure matching open brace isn't in a comment
		       (not (c-in-literal lim))))
		(delete-region (point) here))
	    (goto-char (- (point-max) pos)))
	  ;; reindent line
	  (when c-syntactic-indentation
	    (setq c-syntactic-context (c-guess-basic-syntax))
	    (c-indent-line c-syntactic-context))
	  ;; check to see if a newline should be added
	  (let ((criteria c-hanging-semi&comma-criteria)
		answer add-newline-p)
	    (while criteria
	      (setq answer (funcall (car criteria)))
	      ;; only nil value means continue checking
	      (if (not answer)
		  (setq criteria (cdr criteria))
		(setq criteria nil)
		;; only 'stop specifically says do not add a newline
		(setq add-newline-p (not (eq answer 'stop)))
		))
	    (if add-newline-p
		(c-newline-and-indent))
	    )))))

(defun c-electric-colon (arg)
  "Insert a colon.

If `c-electric-flag' is non-nil, the colon is not inside a literal and a
numeric ARG hasn't been supplied, the command performs several electric
actions:

\(a) If the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) newlines are inserted before and after the colon based on
the settings in `c-hanging-colons-alist'.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, whitespace between two colons will be
\"cleaned up\" leaving a scope operator, if this action is set in
`c-cleanup-list'."

  (interactive "*P")
  (let* ((bod (c-point 'bod))
	 (literal (c-save-buffer-state () (c-in-literal bod)))
	 newlines is-scope-op
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    ;; Any electric action?
    (if (and c-electric-flag (not literal) (not arg))
	;; Unless we're at EOL, only re-indentation happens.
	(if (not (looking-at "[ \t]*\\\\?$"))
	    (if c-syntactic-indentation
		(indent-according-to-mode))

	  ;; scope-operator clean-up?
	  (let ((pos (- (point-max) (point)))
		(here (point)))
	    (if (c-save-buffer-state ()	; Why do we need this? [ACM, 2003-03-12]
		  (and c-auto-newline
		       (memq 'scope-operator c-cleanup-list)
		       (eq (char-before) ?:)
		       (progn
			 (forward-char -1)
			 (c-skip-ws-backward)
			 (eq (char-before) ?:))
		       (not (c-in-literal))
		       (not (eq (char-after (- (point) 2)) ?:))))
		(progn
		  (delete-region (point) (1- here))
		  (setq is-scope-op t)))
	    (goto-char (- (point-max) pos)))

	  ;; indent the current line if it's done syntactically.
	  (if c-syntactic-indentation
	      ;; Cannot use the same syntax analysis as we find below,
	      ;; since that's made with c-syntactic-indentation-in-macros
	      ;; always set to t.
	      (indent-according-to-mode))

	  ;; Calculate where, if anywhere, we want newlines.
	  (c-save-buffer-state
	      ((c-syntactic-indentation-in-macros t)
	       (c-auto-newline-analysis t)
	       ;; Turn on syntactic macro analysis to help with auto newlines
	       ;; only.
	       (syntax (c-guess-basic-syntax))
	       (elem syntax))
	    ;; Translate substatement-label to label for this operation.
	    (while elem
	      (if (eq (car (car elem)) 'substatement-label)
		  (setcar (car elem) 'label))
	      (setq elem (cdr elem)))
	    ;; some language elements can only be determined by checking
	    ;; the following line.  Let's first look for ones that can be
	    ;; found when looking on the line with the colon
	    (setq newlines
		  (and c-auto-newline
		       (or (c-lookup-lists '(case-label label access-label)
					   syntax c-hanging-colons-alist)
			   (c-lookup-lists '(member-init-intro inher-intro)
					   (progn
					     (insert ?\n)
					     (unwind-protect
						 (c-guess-basic-syntax)
					       (delete-char -1)))
					   c-hanging-colons-alist)))))
	  ;; does a newline go before the colon?  Watch out for already
	  ;; non-hung colons.  However, we don't unhang them because that
	  ;; would be a cleanup (and anti-social).
	  (if (and (memq 'before newlines)
		   (not is-scope-op)
		   (save-excursion
		     (skip-chars-backward ": \t")
		     (not (bolp))))
	      (let ((pos (- (point-max) (point))))
		(forward-char -1)
		(c-newline-and-indent)
		(goto-char (- (point-max) pos))))
	  ;; does a newline go after the colon?
	  (if (and (memq 'after (cdr-safe newlines))
		   (not is-scope-op))
	      (c-newline-and-indent))
	  ))))

(defun c-electric-lt-gt (arg)
  "Insert a \"<\" or \">\" character.
If the current language uses angle bracket parens (e.g. template
arguments in C++), try to find out if the inserted character is a
paren and give it paren syntax if appropriate.

If `c-electric-flag' and `c-syntactic-indentation' are both non-nil, the
line will be reindented if the inserted character is a paren or if it
finishes a C++ style stream operator in C++ mode.  Exceptions are when a
numeric argument is supplied, or the point is inside a literal."

  (interactive "*P")
  (let ((c-echo-syntactic-information-p nil)
	final-pos close-paren-inserted found-delim case-fold-search)

    (self-insert-command (prefix-numeric-value arg))
    (setq final-pos (point))

;;;; 2010-01-31: There used to be code here to put a syntax-table text
;;;; property on the new < or > and its mate (if any) when they are template
;;;; parens.  This is now done in an after-change function.

    ;; Indent the line if appropriate.
    (when (and c-electric-flag c-syntactic-indentation c-recognize-<>-arglists)
      (setq found-delim
	    (if (eq last-command-event ?<)
		;; If a <, basically see if it's got "template" before it .....
		(or (and (progn
			   (backward-char)
			   (= (point)
			      (progn (c-beginning-of-current-token) (point))))
			 (progn
			   (c-backward-token-2)
			   (looking-at c-opt-<>-sexp-key)))
		    ;; ..... or is a C++ << operator.
		    (and (c-major-mode-is 'c++-mode)
			 (progn
			   (goto-char (1- final-pos))
			   (c-beginning-of-current-token)
			   (looking-at "<<"))
			 (>= (match-end 0) final-pos)))

	      ;; It's a >.  Either a C++ >> operator. ......
	      (or (and (c-major-mode-is 'c++-mode)
		       (progn
			 (goto-char (1- final-pos))
			 (c-beginning-of-current-token)
			 (looking-at ">>"))
		       (>= (match-end 0) final-pos))
		  ;; ...., or search back for a < which isn't already marked as an
		  ;; opening template delimiter.
		  (save-restriction
		    (widen)
		    ;; Narrow to avoid `c-forward-<>-arglist' below searching past
		    ;; our position.
		    (narrow-to-region (point-min) final-pos)
		    (goto-char final-pos)
		    (while
			(and
			 (progn
			   (c-syntactic-skip-backward "^<;}" nil t)
			   (eq (char-before) ?<))
			 (progn
			   (backward-char)
			   (looking-at "\\s\("))))
		    (and (eq (char-after) ?<)
			 (not (looking-at "\\s\("))
			 (progn (c-backward-syntactic-ws)
				(c-simple-skip-symbol-backward))
			 (or (looking-at c-opt-<>-sexp-key)
			     (not (looking-at c-keywords-regexp)))))))))

    (goto-char final-pos)
    (when found-delim
      (indent-according-to-mode)
      (when (and (eq (char-before) ?>)
		 (not executing-kbd-macro)
		 blink-paren-function)
	    ;; Note: Most paren blink functions, such as the standard
	    ;; `blink-matching-open', currently doesn't handle paren chars
	    ;; marked with text properties very well.  Maybe we should avoid
	    ;; this call for the time being?
	    (funcall blink-paren-function)))))

(defun c-electric-paren (arg)
  "Insert a parenthesis.

If `c-syntactic-indentation' and `c-electric-flag' are both non-nil, the
line is reindented unless a numeric ARG is supplied, or the parenthesis
is inserted inside a literal.

Whitespace between a function name and the parenthesis may get added or
removed; see the variable `c-cleanup-list'.

Also, if `c-electric-flag' and `c-auto-newline' are both non-nil, some
newline cleanups are done if appropriate; see the variable `c-cleanup-list'."
  (interactive "*P")
  (let ((literal (c-save-buffer-state () (c-in-literal)))
	;; shut this up
	(c-echo-syntactic-information-p nil)
	case-fold-search)
    (self-insert-command (prefix-numeric-value arg))

    (if (and (not arg) (not literal))
	(let* (	;; We want to inhibit blinking the paren since this will
	       ;; be most disruptive.  We'll blink it ourselves
	       ;; afterwards.
	       (old-blink-paren blink-paren-function)
	       blink-paren-function)
	  (if (and c-syntactic-indentation c-electric-flag)
	      (indent-according-to-mode))

	  ;; If we're at EOL, check for new-line clean-ups.
	  (when (and c-electric-flag c-auto-newline
		     (looking-at "[ \t]*\\\\?$"))

	    ;; clean up brace-elseif-brace
	    (when
		(and (memq 'brace-elseif-brace c-cleanup-list)
		     (eq last-command-event ?\()
		     (re-search-backward
		      (concat "}"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "else"
			      "\\([ \t\n]\\|\\\\\n\\)+"
			      "if"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "("
			      "\\=")
		      nil t)
		     (not  (c-save-buffer-state () (c-in-literal))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert-and-inherit "} else if ("))

	    ;; clean up brace-catch-brace
	    (when
		(and (memq 'brace-catch-brace c-cleanup-list)
		     (eq last-command-event ?\()
		     (re-search-backward
		      (concat "}"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "catch"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "("
			      "\\=")
		      nil t)
		     (not  (c-save-buffer-state () (c-in-literal))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert-and-inherit "} catch (")))

	  ;; Check for clean-ups at function calls.  These two DON'T need
	  ;; `c-electric-flag' or `c-syntactic-indentation' set.
	  ;; Point is currently just after the inserted paren.
	  (let (beg (end (1- (point))))
	    (cond

	     ;; space-before-funcall clean-up?
	     ((and (memq 'space-before-funcall c-cleanup-list)
		   (eq last-command-event ?\()
		   (save-excursion
		     (backward-char)
		     (skip-chars-backward " \t")
		     (setq beg (point))
		     (and (c-save-buffer-state () (c-on-identifier))
                          ;; Don't add a space into #define FOO()....
                          (not (and (c-beginning-of-macro)
                                    (c-forward-over-cpp-define-id)
                                    (eq (point) beg))))))
	      (save-excursion
		(delete-region beg end)
		(goto-char beg)
		(insert ?\ )))

	     ;; compact-empty-funcall clean-up?
		  ((c-save-buffer-state ()
		     (and (memq 'compact-empty-funcall c-cleanup-list)
			  (eq last-command-event ?\))
			  (save-excursion
			    (c-safe (backward-char 2))
			    (when (looking-at "()")
			      (setq end (point))
			      (skip-chars-backward " \t")
			      (setq beg (point))
			      (c-on-identifier)))))
		   (delete-region beg end))))
	  (and (eq last-input-event ?\))
	       (not executing-kbd-macro)
	       old-blink-paren
	       (funcall old-blink-paren))))))

(defun c-electric-continued-statement ()
  "Reindent the current line if appropriate.

This function is used to reindent the line after a keyword which
continues an earlier statement is typed, e.g. an \"else\" or the
\"while\" in a do-while block.

The line is reindented if there is nothing but whitespace before the
keyword on the line, the keyword is not inserted inside a literal, and
`c-electric-flag' and `c-syntactic-indentation' are both non-nil."
  (let (;; shut this up
	(c-echo-syntactic-information-p nil))
    (when (c-save-buffer-state ()
	    (and c-electric-flag
		 c-syntactic-indentation
		 (not (eq last-command-event ?_))
		 (= (save-excursion
		      (skip-syntax-backward "w")
		      (point))
		    (c-point 'boi))
		 (not (c-in-literal (c-point 'bod)))))
      ;; Have to temporarily insert a space so that
      ;; c-guess-basic-syntax recognizes the keyword.  Follow the
      ;; space with a nonspace to avoid messing up any whitespace
      ;; sensitive meddling that might be done, e.g. by
      ;; `c-backslash-region'.
      (insert-and-inherit " x")
      (unwind-protect
	  (indent-according-to-mode)
	(delete-char -2)))))



(declare-function subword-forward "subword" (&optional arg))
(declare-function subword-backward "subword" (&optional arg))

;; "nomenclature" functions + c-scope-operator.
(defun c-forward-into-nomenclature (&optional arg)
  "Compatibility alias for `c-forward-subword'."
  (interactive "p")
  (require 'subword)
  (subword-forward arg))
(make-obsolete 'c-forward-into-nomenclature 'subword-forward "23.2")

(defun c-backward-into-nomenclature (&optional arg)
  "Compatibility alias for `c-backward-subword'."
  (interactive "p")
  (require 'subword)
  (subword-backward arg))
(make-obsolete 'c-backward-into-nomenclature 'subword-backward "23.2")

(defun c-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive "*")
  (insert-and-inherit "::"))


;; Movement (etc.) by defuns.
(defun c-in-function-trailer-p (&optional lim)
  ;; Return non-nil if point is between the closing brace and the semicolon of
  ;; a brace construct which needs a semicolon, e.g. within the "variables"
  ;; portion of a declaration like "struct foo {...} bar ;".
  ;;
  ;; Return the position of the main declaration.  Otherwise, return nil.
  ;; Point is assumed to be at the top level and outside of any macro or
  ;; literal.
  ;;
  ;; If LIM is non-nil, it is the bound on a the backward search for the
  ;; beginning of the declaration.
  ;;
  ;; This function might do hidden buffer changes.
  (and c-opt-block-decls-with-vars-key
       (save-excursion
	 (c-syntactic-skip-backward "^;}" lim)
	 (let ((eo-block (point))
	       bod)
	   (and (eq (char-before) ?\})
		(eq (car (c-beginning-of-decl-1 lim)) 'previous)
		(setq bod (point))
		;; Look for struct or union or ...  If we find one, it might
		;; be the return type of a function, or the like.  Exclude
		;; this case.
		(c-syntactic-re-search-forward
		 (concat "[;=\(\[{]\\|\\("
			 c-opt-block-decls-with-vars-key
			 "\\)")
		 eo-block t t t)
		(match-beginning 1)	; Is there a "struct" etc., somewhere?
		(not (eq (char-before) ?_))
		(c-syntactic-re-search-forward "[;=\(\[{]" eo-block t t t)
		(eq (char-before) ?\{)
		bod)))))

(defun c-where-wrt-brace-construct ()
  ;; Determine where we are with respect to functions (or other brace
  ;; constructs, included in the term "function" in the rest of this comment).
  ;; Point is assumed to be outside any macro or literal.
  ;; This is used by c-\(beginning\|end\)-of-defun.
  ;;
  ;; Return one of these symbols:
  ;; at-header       : we're at the start of a function's header.
  ;; in-header       : we're inside a function's header, this extending right
  ;;                   up to the brace.  This bit includes any k&r declarations.
  ;; in-block        : we're inside a function's brace block.
  ;; in-trailer      : we're in the area between the "}" and ";" of something
  ;;                  like "struct foo {...} bar, baz;".
  ;; at-function-end : we're just after the closing brace (or semicolon) that
  ;;                   terminates the function.
  ;; outwith-function: we're not at or in any function.  Being inside a
  ;;                   non-brace construct also counts as 'outwith-function'.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (let* (kluge-start
	   decl-result brace-decl-p
	   (start (point))
	   (paren-state (c-parse-state))
	   (least-enclosing (c-least-enclosing-brace paren-state)))

      (cond
       ((and least-enclosing
	     (eq (char-after least-enclosing) ?\{))
	'in-block)
       ((c-in-function-trailer-p)
	'in-trailer)
       ((and (not least-enclosing)
	     (consp paren-state)
	     (consp (car paren-state))
	     (eq start (cdar paren-state)))
	'at-function-end)
       (t
	;; Find the start of the current declaration.  NOTE: If we're in the
	;; variables after a "struct/eval" type block, we don't get to the
	;; real declaration here - we detect and correct for this later.

	;;If we're in the parameters' parens, move back out of them.
	(if least-enclosing (goto-char least-enclosing))
	;; Kluge so that c-beginning-of-decl-1 won't go back if we're already
	;; at a declaration.
	(if (or (and (eolp) (not (eobp))) ; EOL is matched by "\\s>"
		(not (looking-at
"\\([;#]\\|\\'\\|\\s(\\|\\s)\\|\\s\"\\|\\s\\\\|\\s$\\|\\s<\\|\\s>\\|\\s!\\)")))
	    (forward-char))
	(setq kluge-start (point))
	(setq decl-result
	      (car (c-beginning-of-decl-1
		    ;; NOTE: If we're in a K&R region, this might be the start
		    ;; of a parameter declaration, not the actual function.
		    (and least-enclosing ; LIMIT for c-b-of-decl-1
			 (c-safe-position least-enclosing paren-state)))))

	;; Has the declaration we've gone back to got braces?
	(setq brace-decl-p
	      (save-excursion
		    (and (c-syntactic-re-search-forward "[;{]" nil t t)
			 (or (eq (char-before) ?\{)
			     (and c-recognize-knr-p
				  ;; Might have stopped on the
				  ;; ';' in a K&R argdecl.  In
				  ;; that case the declaration
				  ;; should contain a block.
				  (c-in-knr-argdecl))))))

	(cond
	 ((= (point) kluge-start)	; might be BOB or unbalanced parens.
	  'outwith-function)
	 ((eq decl-result 'same)
	  (if brace-decl-p
	      (if (eq (point) start)
		  'at-header
		'in-header)
	    'outwith-function))
	 ((eq decl-result 'previous)
	  (if (and (not brace-decl-p)
		   (c-in-function-trailer-p))
	      'at-function-end
	    'outwith-function))
	 (t (error
	     "c-where-wrt-brace-construct: c-beginning-of-decl-1 returned %s"
	     decl-result))))))))

(defun c-backward-to-nth-BOF-{ (n where)
  ;; Skip to the opening brace of the Nth function before point.  If
  ;; point is inside a function, this counts as the first.  Point must be
  ;; outside any comment/string or macro.
  ;;
  ;; N must be strictly positive.
  ;; WHERE describes the position of point, one of the symbols `at-header',
  ;; `in-header', `in-block', `in-trailer', `at-function-end',
  ;; `outwith-function' as returned by c-where-wrt-brace-construct.
  ;;
  ;; If we run out of functions, leave point at BOB.  Return zero on success,
  ;; otherwise the number of {s still to go.
  ;;
  ;; This function may do hidden buffer changes
  (cond
   ;; What we do to go back the first defun depends on where we start.
   ((bobp))
   ((eq where 'in-block)
    (goto-char (c-least-enclosing-brace (c-parse-state)))
    (setq n (1- n)))
   ((eq where 'in-header)
    (c-syntactic-re-search-forward "{")
    (backward-char)
    (setq n (1- n)))
   ((memq where '(at-header outwith-function at-function-end in-trailer))
    (c-syntactic-skip-backward "^}")
    (when (eq (char-before) ?\})
      (backward-sexp)
      (setq n (1- n))))
   (t (error "Unknown `where' %s in c-backward-to-nth-EOF-{" where)))

   ;; Each time round the loop, go back to a "{" at the outermost level.
  (while (and (> n 0) (not (bobp)))
    (c-parse-state)		       ; This call speeds up the following one
					; by a factor of ~6.  Hmmm.  2006/4/5.
    (c-syntactic-skip-backward "^}")
    (when (eq (char-before) ?\})
      (backward-sexp)
      (setq n (1- n))))
   n)

(defun c-narrow-to-most-enclosing-decl-block (&optional inclusive)
  ;; If we are inside a decl-block (in the sense of c-looking-at-decl-block),
  ;; i.e. something like namespace{} or extern{}, narrow to the insides of
  ;; that block (NOT including the enclosing braces) if INCLUSIVE is nil,
  ;; otherwise include the braces.  If the closing brace is missing,
  ;; (point-max) is used instead.
  (let ((paren-state (c-parse-state))
	encl-decl)
    (setq encl-decl (and paren-state (c-most-enclosing-decl-block paren-state)))
    (if encl-decl
	(save-excursion
	  (narrow-to-region
	   (if inclusive
	       (progn (goto-char encl-decl)
		      (c-beginning-of-decl-1)
		      (point))
	     (1+ encl-decl))
	   (progn
	     (goto-char encl-decl)
	     (or (c-safe (forward-list)
			 (if inclusive
			     (point)
			   (1- (point))))
		 (point-max))))))))

(defun c-widen-to-enclosing-decl-scope (paren-state orig-point-min orig-point-max)
  ;; Narrow the buffer to the innermost declaration scope (e.g. a class, a
  ;; namespace or the "whole buffer") recorded in PAREN-STATE, the bounding
  ;; braces NOT being included in the resulting region.  On no account may the
  ;; final region exceed that bounded by ORIG-POINT-MIN, ORIG-POINT-MAX.
  ;; PAREN-STATE is a list of buffer positions in the style of
  ;; (c-parse-state), one of which will be that of the desired opening brace,
  ;; if there is one.
  ;;
  ;; Return the position of the enclosing opening brace, or nil
  (let (encl-decl)	    ; putative position of decl-scope's opening brace.
    (save-restriction
      (narrow-to-region orig-point-min orig-point-max)
      (setq encl-decl (and paren-state
			   (c-most-enclosing-decl-block paren-state))))
    (if encl-decl
	(progn
	  (widen)
	  (narrow-to-region (1+ encl-decl)
			    (save-excursion
			      (goto-char encl-decl)
			      (or (c-safe (forward-list)
					  (1- (point)))
				  orig-point-max)))
	  encl-decl)
      (narrow-to-region orig-point-min orig-point-max)
      nil)))

(eval-and-compile
  (defmacro c-while-widening-to-decl-block (condition)
    ;; Repeatedly evaluate CONDITION until it returns nil.  After each
    ;; evaluation, if `c-defun-tactic' is set appropriately, widen to innards
    ;; of the next enclosing declaration block (e.g. namespace, class), or the
    ;; buffer's original restriction.
    ;;
    ;; This is a very special purpose macro, which assumes the existence of
    ;; several variables.  It is for use only in c-beginning-of-defun and
    ;; c-end-of-defun.
    `(while
	 (and ,condition
	      (eq c-defun-tactic 'go-outward)
	      lim)
       (setq paren-state (c-whack-state-after lim paren-state))
       (setq lim (c-widen-to-enclosing-decl-scope
		  paren-state orig-point-min orig-point-max))
       (setq where 'in-block))))

(defun c-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Every top level declaration that contains a brace paren block is
considered to be a defun.

With a positive argument, move backward that many defuns.  A negative
argument -N means move forward to the Nth following beginning.  Return
t unless search stops due to beginning or end of buffer.

Unlike the built-in `beginning-of-defun' this tries to be smarter
about finding the char with open-parenthesis syntax that starts the
defun."

  (interactive "p")
  (or arg (setq arg 1))

  (or (not (eq this-command 'c-beginning-of-defun))
      (eq last-command 'c-beginning-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))

  (c-save-buffer-state
      (beginning-of-defun-function end-of-defun-function
       (start (point))
       (paren-state (copy-tree (c-parse-state))) ; This must not share list
					; structure with other users of c-state-cache.
       (orig-point-min (point-min)) (orig-point-max (point-max))
       lim			    ; Position of { which has been widened to.
       where pos case-fold-search)

    (save-restriction
      (if (eq c-defun-tactic 'go-outward)
	  (setq lim (c-widen-to-enclosing-decl-scope ; e.g. class, namespace.
		     paren-state orig-point-min orig-point-max)))

      ;; Move back out of any macro/comment/string we happen to be in.
      (c-beginning-of-macro)
      (setq pos (c-literal-limits))
      (if pos (goto-char (car pos)))

      (setq where (c-where-wrt-brace-construct))

      (if (< arg 0)
	  ;; Move forward to the closing brace of a function.
	  (progn
	    (if (memq where '(at-function-end outwith-function))
		(setq arg (1+ arg)))
	    (if (< arg 0)
		(c-while-widening-to-decl-block
		 (< (setq arg (- (c-forward-to-nth-EOF-} (- arg) where))) 0)))
	    ;; Move forward to the next opening brace....
	    (when (and (= arg 0)
		       (progn
			 (c-while-widening-to-decl-block
			  (not (c-syntactic-re-search-forward "{" nil 'eob)))
			 (eq (char-before) ?{)))
	      (backward-char)
	      ;; ... and backward to the function header.
	      (c-beginning-of-decl-1)
	      t))

	;; Move backward to the opening brace of a function, making successively
	;; larger portions of the buffer visible as necessary.
	(when (> arg 0)
	  (c-while-widening-to-decl-block
	   (> (setq arg (c-backward-to-nth-BOF-{ arg where)) 0)))

	(when (eq arg 0)
	  ;; Go backward to this function's header.
	  (c-beginning-of-decl-1)

	  (setq pos (point))
	  ;; We're now there, modulo comments and whitespace.
	  ;; Try to be line oriented; position point at the closest
	  ;; preceding boi that isn't inside a comment, but if we hit
	  ;; the previous declaration then we use the current point
	  ;; instead.
	  (while (and (/= (point) (c-point 'boi))
		      (c-backward-single-comment)))
	  (if (/= (point) (c-point 'boi))
	      (goto-char pos)))

	(c-keep-region-active)
	(= arg 0)))))

(defun c-forward-to-nth-EOF-} (n where)
  ;; Skip to the closing brace of the Nth function after point.  If
  ;; point is inside a function, this counts as the first.  Point must be
  ;; outside any comment/string or macro.
  ;;
  ;; N must be strictly positive.
  ;; WHERE describes the position of point, one of the symbols `at-header',
  ;; `in-header', `in-block', `in-trailer', `at-function-end',
  ;; `outwith-function' as returned by c-where-wrt-brace-construct.
  ;;
  ;; If we run out of functions, leave point at EOB.  Return zero on success,
  ;; otherwise the number of }s still to go.
  ;;
  ;; This function may do hidden buffer changes.

  (cond
  ;; What we do to go forward over the first defun depends on where we
  ;; start.  We go to the closing brace of that defun, even when we go
  ;; backwards to it (in a "struct foo {...} bar ;").
   ((eobp))
   ((eq where 'in-block)
    (goto-char (c-least-enclosing-brace (c-parse-state)))
    (forward-sexp)
    (setq n (1- n)))
   ((eq where 'in-trailer)
    (c-syntactic-skip-backward "^}")
    (setq n (1- n)))
   ((memq where '(at-function-end outwith-function at-header in-header))
    (when (c-syntactic-re-search-forward "{" nil 'eob)
      (backward-char)
      (forward-sexp)
      (setq n (1- n))))
   (t (error "c-forward-to-nth-EOF-}: `where' is %s" where)))

  ;; Each time round the loop, go forward to a "}" at the outermost level.
  (while (and (> n 0) (not (eobp)))
					;(c-parse-state)	; This call speeds up the following one by a factor
					; of ~6.  Hmmm.  2006/4/5.
    (when (c-syntactic-re-search-forward "{" nil 'eob)
      (backward-char)
      (forward-sexp))
    (setq n (1- n)))
  n)

(defun c-end-of-defun (&optional arg)
  "Move forward to the end of a top level declaration.
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end.  Returns t unless search stops due to
beginning or end of buffer.

An end of a defun occurs right after the close-parenthesis that matches
the open-parenthesis that starts a defun; see `beginning-of-defun'."
  (interactive "p")
  (or arg (setq arg 1))

  (or (not (eq this-command 'c-end-of-defun))
      (eq last-command 'c-end-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))

  (c-save-buffer-state
      (beginning-of-defun-function end-of-defun-function
       (start (point))
       (paren-state (copy-tree (c-parse-state))) ; This must not share list
				  ; structure with other users of c-state-cache.
       (orig-point-min (point-min)) (orig-point-max (point-max))
       lim
       where pos case-fold-search)

    (save-restriction
      (if (eq c-defun-tactic 'go-outward)
	  (setq lim (c-widen-to-enclosing-decl-scope ; e.g. class, namespace
		     paren-state orig-point-min orig-point-max)))

      ;; Move back out of any macro/comment/string we happen to be in.
      (c-beginning-of-macro)
      (setq pos (c-literal-limits))
      (if pos (goto-char (car pos)))

      (setq where (c-where-wrt-brace-construct))

      (if (< arg 0)
	  ;; Move backwards to the } of a function
	  (progn
	    (if (memq where '(at-header outwith-function))
		(setq arg (1+ arg)))
	    (if (< arg 0)
		(c-while-widening-to-decl-block
		 (< (setq arg (- (c-backward-to-nth-BOF-{ (- arg) where))) 0)))
	    (if (= arg 0)
		(c-while-widening-to-decl-block
		 (progn (c-syntactic-skip-backward "^}")
			(not (eq (char-before) ?}))))))

	;; Move forward to the } of a function
	(if (> arg 0)
	    (c-while-widening-to-decl-block
	     (> (setq arg (c-forward-to-nth-EOF-} arg where)) 0))))

      ;; Do we need to move forward from the brace to the semicolon?
      (when (eq arg 0)
	(if (c-in-function-trailer-p) ; after "}" of struct/enum, etc.
	    (c-syntactic-re-search-forward ";"))

	(setq pos (point))
	;; We're there now, modulo comments and whitespace.
	;; Try to be line oriented; position point after the next
	;; newline that isn't inside a comment, but if we hit the
	;; next declaration then we use the current point instead.
	(while (and (not (bolp))
		    (not (looking-at "\\s *$"))
		    (c-forward-single-comment)))
	(cond ((bolp))
	      ((looking-at "\\s *$")
	       (forward-line 1))
	      (t
	       (goto-char pos))))

      (c-keep-region-active)
      (= arg 0))))

(defun c-defun-name ()
  "Return the name of the current defun, or NIL if there isn't one.
\"Defun\" here means a function, or other top level construct
with a brace block."
  (interactive)
  (c-save-buffer-state
      (beginning-of-defun-function end-of-defun-function
       where pos name-end case-fold-search)
 
    (save-restriction
      (widen)
      (save-excursion
	;; Move back out of any macro/comment/string we happen to be in.
	(c-beginning-of-macro)
	(setq pos (c-literal-limits))
	(if pos (goto-char (car pos)))

	(setq where (c-where-wrt-brace-construct))

	;; Move to the beginning of the current defun, if any, if we're not
	;; already there.
	(if (eq where 'outwith-function)
	    nil
	  (unless (eq where 'at-header)
	    (c-backward-to-nth-BOF-{ 1 where)
	    (c-beginning-of-decl-1))

	  ;; Pick out the defun name, according to the type of defun.
	  (cond
	   ;; struct, union, enum, or similar:
	   ((and (looking-at c-type-prefix-key)
		 (progn (c-forward-token-2 2) ; over "struct foo "
			(or (eq (char-after) ?\{)
			    (looking-at c-symbol-key)))) ; "struct foo bar ..."
	    (save-match-data (c-forward-token-2))
	    (when (eq (char-after) ?\{)
	      (c-backward-token-2)
	      (looking-at c-symbol-key))
	    (match-string-no-properties 0))

	   ((looking-at "DEFUN\\_>")
	    ;; DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory, ...) ==> Ffile_name_directory
	    ;; DEFUN(POSIX::STREAM-LOCK, stream lockp &key BLOCK SHARED START LENGTH) ==> POSIX::STREAM-LOCK
	    (down-list 1)
	    (c-forward-syntactic-ws)
	    (when (eq (char-after) ?\")
	      (forward-sexp 1)
	      (c-forward-token-2))	; over the comma and following WS.
	    (buffer-substring-no-properties
	     (point)
	     (progn
	       (c-forward-token-2)
	       (when (looking-at ":")  ; CLISP: DEFUN(PACKAGE:LISP-SYMBOL,...)
		 (skip-chars-forward "^,"))
	       (c-backward-syntactic-ws)
	       (point))))

	   ((looking-at "DEF[a-zA-Z0-9_]* *( *\\([^, ]*\\) *,")
	    ;; DEFCHECKER(sysconf_arg,prefix=_SC,default=, ...) ==> sysconf_arg
	    ;; DEFFLAGSET(syslog_opt_flags,LOG_PID ...) ==> syslog_opt_flags
	    (match-string-no-properties 1))

	   (t
	    ;; Normal function or initializer.
	    (when (c-syntactic-re-search-forward "[{(]" nil t)
	      (backward-char)
	      (c-backward-syntactic-ws)
	      (when (eq (char-before) ?\=) ; struct foo bar = {0, 0} ;
		(c-backward-token-2)
		(c-backward-syntactic-ws))
	      (setq name-end (point))
	      (c-backward-token-2)
	      (buffer-substring-no-properties (point) name-end)))))))))

(defun c-declaration-limits (near)
  ;; Return a cons of the beginning and end positions of the current
  ;; top level declaration or macro.  If point is not inside any then
  ;; nil is returned, unless NEAR is non-nil in which case the closest
  ;; following one is chosen instead (if there is any).  The end
  ;; position is at the next line, providing there is one before the
  ;; declaration.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (save-restriction
      (when (eq c-defun-tactic 'go-outward)
	(c-narrow-to-most-enclosing-decl-block t)  ; e.g. class, namespace
	(or (save-restriction
	      (c-narrow-to-most-enclosing-decl-block nil)

    ;; Note: Some code duplication in `c-beginning-of-defun' and
    ;; `c-end-of-defun'.
    (catch 'exit
      (let ((start (point))
	    (paren-state (c-parse-state))
	    lim pos end-pos)
	(unless (c-safe
		  (goto-char (c-least-enclosing-brace paren-state))
			    ;; If we moved to the outermost enclosing paren
			    ;; then we can use c-safe-position to set the
			    ;; limit. Can't do that otherwise since the
			    ;; earlier paren pair on paren-state might very
			    ;; well be part of the declaration we should go
			    ;; to.
		  (setq lim (c-safe-position (point) paren-state))
		  t)
	  ;; At top level.  Make sure we aren't inside a literal.
	  (setq pos (c-literal-limits
		     (c-safe-position (point) paren-state)))
	  (if pos (goto-char (car pos))))

	(when (c-beginning-of-macro)
	  (throw 'exit
		 (cons (point)
		       (save-excursion
			 (c-end-of-macro)
			 (forward-line 1)
			 (point)))))

	(setq pos (point))
	(when (or (eq (car (c-beginning-of-decl-1 lim)) 'previous)
		  (= pos (point)))
	  ;; We moved back over the previous defun.  Skip to the next
	  ;; one.  Not using c-forward-syntactic-ws here since we
	  ;; should not skip a macro.  We can also be directly after
	  ;; the block in a `c-opt-block-decls-with-vars-key'
	  ;; declaration, but then we won't move significantly far
	  ;; here.
	  (goto-char pos)
	  (c-forward-comments)

	  (when (and near (c-beginning-of-macro))
	    (throw 'exit
		   (cons (point)
			 (save-excursion
			   (c-end-of-macro)
			   (forward-line 1)
			   (point))))))

	(if (eobp) (throw 'exit nil))

	;; Check if `c-beginning-of-decl-1' put us after the block in a
	;; declaration that doesn't end there.  We're searching back and
	;; forth over the block here, which can be expensive.
	(setq pos (point))
	(if (and c-opt-block-decls-with-vars-key
		 (progn
		   (c-backward-syntactic-ws)
		   (eq (char-before) ?}))
		 (eq (car (c-beginning-of-decl-1))
		     'previous)
		 (save-excursion
		   (c-end-of-decl-1)
		   (and (> (point) pos)
			(setq end-pos (point)))))
	    nil
	  (goto-char pos))

	(if (and (not near) (> (point) start))
	    nil

	  ;; Try to be line oriented; position the limits at the
	  ;; closest preceding boi, and after the next newline, that
	  ;; isn't inside a comment, but if we hit a neighboring
	  ;; declaration then we instead use the exact declaration
	  ;; limit in that direction.
	  (cons (progn
		  (setq pos (point))
		  (while (and (/= (point) (c-point 'boi))
			      (c-backward-single-comment)))
		  (if (/= (point) (c-point 'boi))
		      pos
		    (point)))
		(progn
		  (if end-pos
		      (goto-char end-pos)
		    (c-end-of-decl-1))
		  (setq pos (point))
		  (while (and (not (bolp))
			      (not (looking-at "\\s *$"))
			      (c-forward-single-comment)))
		  (cond ((bolp)
			 (point))
			((looking-at "\\s *$")
			 (forward-line 1)
			 (point))
			(t
				   pos))))))))
	    (and (not near)
		 (goto-char (point-min))
		 (c-forward-decl-or-cast-1 -1 nil nil)
		 (eq (char-after) ?\{)
		 (cons (point-min) (point-max))))))))

(defun c-mark-function ()
  "Put mark at end of the current top-level declaration or macro, point at beginning.
If point is not inside any then the closest following one is
chosen.  Each successive call of this command extends the marked
region by one function.

A mark is left where the command started, unless the region is already active
\(in Transient Mark mode).

As opposed to \\[c-beginning-of-defun] and \\[c-end-of-defun], this
function does not require the declaration to contain a brace block."
  (interactive)

  (let (decl-limits case-fold-search)
    (c-save-buffer-state nil
      ;; We try to be line oriented, unless there are several
      ;; declarations on the same line.
      (if (looking-at c-syntactic-eol)
	  (c-backward-token-2 1 nil (c-point 'bol)))
      (setq decl-limits (c-declaration-limits t)))

    (if (not decl-limits)
	(error "Cannot find any declaration")
      (let* ((extend-region-p
	      (and (eq this-command 'c-mark-function)
		   (eq last-command 'c-mark-function)))
	     (push-mark-p (and (eq this-command 'c-mark-function)
			       (not extend-region-p)
			       (not (and transient-mark-mode mark-active)))))
	(if push-mark-p (push-mark (point)))
	(if extend-region-p
	    (progn
	      (exchange-point-and-mark)
	      (setq decl-limits (c-declaration-limits t))
	      (when (not decl-limits)
		(exchange-point-and-mark)
		(error "Cannot find any declaration"))
	      (goto-char (cdr decl-limits))
	      (exchange-point-and-mark))
	  (goto-char (car decl-limits))
	  (push-mark (cdr decl-limits) nil t))))))

(defun c-cpp-define-name ()
  "Return the name of the current CPP macro, or NIL if we're not in one."
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (and c-opt-cpp-macro-define-start
	   (c-beginning-of-macro)
	   (looking-at c-opt-cpp-macro-define-start)
	   (match-string-no-properties 1)))))


;; Movement by statements.
(defun c-in-comment-line-prefix-p ()
  ;; Point is within a comment.  Is it also within a comment-prefix?
  ;; Space at BOL which precedes a comment-prefix counts as part of it.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((here (point)))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (looking-at c-current-comment-prefix)
	   (/= (match-beginning 0) (match-end 0))
	   (< here (match-end 0))))))

(defun c-narrow-to-comment-innards (range)
  ;; Narrow to the "inside" of the comment (block) defined by range, as
  ;; follows:
  ;;
  ;; A c-style block comment has its opening "/*" and its closing "*/" (if
  ;; present) removed.  A c++-style line comment retains its opening "//" but
  ;; has any final NL removed.  If POINT is currently outwith these innards,
  ;; move it to the appropriate boundary.
  ;;
  ;; This narrowing simplifies the sentence movement functions, since it
  ;; eliminates awkward things at the boundaries of the comment (block).
  ;;
  ;; This function might do hidden buffer changes.
  (let* ((lit-type (c-literal-type range))
	 (beg (if (eq lit-type 'c) (+ (car range) 2) (car range)))
	 (end (if (eq lit-type 'c)
		  (if (and (eq (char-before (cdr range)) ?/)
			   (eq (char-before (1- (cdr range))) ?*))
		      (- (cdr range) 2)
		    (point-max))
		(if (eq (cdr range) (point-max))
		    (point-max)
		  (- (cdr range) 1)))))
    (if (> (point) end)
	(goto-char end))		; This would be done automatically by ...
    (if (< (point) beg)
	(goto-char beg))	;  ... narrow-to-region but is not documented.
    (narrow-to-region beg end)))

(defun c-beginning-of-sentence-in-comment (range)
  ;; Move backwards to the "beginning of a sentence" within the comment
  ;; defined by RANGE, a cons of its starting and ending positions.  If we
  ;; find a BOS, return NIL.  Otherwise, move point to just before the start
  ;; of the comment and return T.
  ;;
  ;; The BOS is either text which follows a regexp match of sentence-end,
  ;; or text which is a beginning of "paragraph".
  ;; Comment-prefixes are treated like WS when calculating BOSes or BOPs.
  ;;
  ;; This code was adapted from GNU Emacs's forward-sentence in paragraphs.el.
  ;; It is not a general function, but is intended only for calling from
  ;; c-move-over-sentence.  Not all preconditions have been explicitly stated.
  ;;
  ;; This function might do hidden buffer changes.
  (save-match-data
    (let ((start-point (point)))
      (save-restriction
	(c-narrow-to-comment-innards range) ; This may move point back.
	(let* ((here (point))
	       last
	       (here-filler	   ; matches WS and comment-prefixes at point.
		(concat "\\=\\(^[ \t]*\\(" c-current-comment-prefix "\\)"
			"\\|[ \t\n\r\f]\\)*"))
	       (prefix-at-bol-here ; matches WS and prefix at BOL, just before point
		(concat "^[ \t]*\\(" c-current-comment-prefix "\\)[ \t\n\r\f]*\\="))
	       ;; First, find the previous paragraph start, if any.
	       (par-beg	; point where non-WS/non-prefix text of paragraph starts.
		(save-excursion
		  (forward-paragraph -1) ; uses cc-mode values of
					; paragraph-\(start\|separate\)
		  (if (> (re-search-forward here-filler nil t) here)
		      (goto-char here))
		  (when (>= (point) here)
		    (forward-paragraph -2)
		    (if (> (re-search-forward here-filler nil t) here)
			(goto-char here)))
		  (point))))

	  ;; Now seek successively earlier sentence ends between PAR-BEG and
	  ;; HERE, until the "start of sentence" following it is earlier than
	  ;; HERE, or we hit PAR-BEG.  Beware of comment prefixes!
	  (while (and (re-search-backward (c-sentence-end) par-beg 'limit)
		      (setq last (point))
		      (goto-char (match-end 0))	; tentative beginning of sentence
		      (or (>= (point) here)
			  (and (not (bolp)) ; Found a non-blank comment-prefix?
			       (save-excursion
				 (if (re-search-backward prefix-at-bol-here nil t)
				     (/= (match-beginning 1) (match-end 1)))))
			  (progn	; Skip the crud to find a real b-o-s.
			    (if (c-in-comment-line-prefix-p)
				(beginning-of-line))
			    (re-search-forward here-filler) ; always succeeds.
			    (>= (point) here))))
	    (goto-char last))
	  (re-search-forward here-filler)))

      (if (< (point) start-point)
	  nil
	(goto-char (car range))
	t))))

(defun c-end-of-sentence-in-comment (range)
  ;; Move forward to the "end of a sentence" within the comment defined by
  ;; RANGE, a cons of its starting and ending positions (enclosing the opening
  ;; comment delimiter and the terminating */ or newline).  If we find an EOS,
  ;; return NIL.  Otherwise, move point to just after the end of the comment
  ;; and return T.
  ;;
  ;; The EOS is just after the non-WS part of the next match of the regexp
  ;; sentence-end.  Typically, this is just after one of [.!?].  If there is
  ;; no sentence-end match following point, any WS before the end of the
  ;; comment will count as EOS, providing we're not already in it.
  ;;
  ;; This code was adapted from GNU Emacs's forward-sentence in paragraphs.el.
  ;; It is not a general function, but is intended only for calling from
  ;; c-move-over-sentence.
  ;;
  ;; This function might do hidden buffer changes.
  (save-match-data
    (let ((start-point (point))
	  ;; (lit-type (c-literal-type range))  ; Commented out, 2005/11/23, ACM
	  )
      (save-restriction
	(c-narrow-to-comment-innards range) ; This might move point forwards.
	(let* ((here (point))
	       (par-end	; EOL position of last text in current/next paragraph.
		(save-excursion
		  ;; The cc-mode values of paragraph-\(start\|separate\), set
		  ;; in c-setup-paragraph-variables, are used in the
		  ;; following.
		  (forward-paragraph 1)
		  (if (eq (preceding-char) ?\n) (forward-char -1))
		  (when (<= (point) here) ; can happen, e.g., when HERE is at EOL.
		    (goto-char here)
		    (forward-paragraph 2)
		    (if (eq (preceding-char) ?\n) (forward-char -1)))
		  (point)))

	       last
	       (prefix-at-bol-here
		(concat "^[ \t]*\\(" c-current-comment-prefix "\\)\\=")))
	  ;; Go forward one "comment-prefix which looks like sentence-end"
	  ;; each time round the following:
	  (while (and (re-search-forward (c-sentence-end) par-end 'limit)
		      (progn
			(setq last (point))
			(skip-chars-backward " \t\n")
			(or (and (not (bolp))
				 (re-search-backward prefix-at-bol-here nil t)
				 (/= (match-beginning 1) (match-end 1)))
			    (<= (point) here))))
	    (goto-char last))

	  ;; Take special action if we're up against the end of a comment (of
	  ;; either sort): Leave point just after the last non-ws text.
	  (if (eq (point) (point-max))
	      (while (or (/= (skip-chars-backward " \t\n") 0)
			 (and (re-search-backward prefix-at-bol-here nil t)
			      (/= (match-beginning 1) (match-end 1))))))))

      (if (> (point) start-point)
	      nil
	    (goto-char (cdr range))
	    t))))

(defun c-beginning-of-sentence-in-string (range)
  ;; Move backwards to the "beginning of a sentence" within the string defined
  ;; by RANGE, a cons of its starting and ending positions (enclosing the
  ;; string quotes).  If we find a BOS, return NIL.  Otherwise, move point to
  ;; just before the start of the string and return T.
  ;;
  ;; The BOS is either the text which follows a regexp match of sentence-end
  ;; or text which is a beginning of "paragraph".  For the purposes of
  ;; determining paragraph boundaries, escaped newlines are treated as
  ;; ordinary newlines.
  ;;
  ;; This code was adapted from GNU Emacs's forward-sentence in paragraphs.el.
  ;; It is not a general function, but is intended only for calling from
  ;; c-move-over-sentence.
  ;;
  ;; This function might do hidden buffer changes.
  (save-match-data
    (let* ((here (point)) last
	   (end (1- (cdr range)))
	   (here-filler		   ; matches WS and escaped newlines at point.
	    "\\=\\([ \t\n\r\f]\\|\\\\[\n\r]\\)*")
	   ;; Enhance paragraph-start and paragraph-separate also to recognize
	   ;; blank lines terminated by escaped EOLs.  IT MAY WELL BE that
	   ;; these values should be customizable user options, or something.
	   (paragraph-start c-string-par-start)
	   (paragraph-separate c-string-par-separate)

	   (par-beg	       ; beginning of current (or previous) paragraph.
	    (save-excursion
	      (save-restriction
		(narrow-to-region (1+ (car range)) end)
		(forward-paragraph -1)	; uses above values of
					; paragraph-\(start\|separate\)
		(if (> (re-search-forward here-filler nil t) here)
		    (goto-char here))
		(when (>= (point) here)
		  (forward-paragraph -2)
		  (if (> (re-search-forward here-filler nil t) here)
		      (goto-char here)))
		(point)))))
      ;; Now see if we can find a sentence end after PAR-BEG.
      (while (and (re-search-backward c-sentence-end-with-esc-eol par-beg 'limit)
		  (setq last (point))
		  (goto-char (match-end 0))
		  (or (> (point) end)
		      (progn
			(re-search-forward
			 here-filler end t) ; always succeeds.  Use end rather
					; than here, in case point starts
					; beyond the closing quote.
			(>= (point) here))))
	(goto-char last))
      (re-search-forward here-filler here t)
      (if (< (point) here)
	  nil
	(goto-char (car range))
	t))))

(defun c-end-of-sentence-in-string (range)
  ;; Move forward to the "end of a sentence" within the string defined by
  ;; RANGE, a cons of its starting and ending positions.  If we find an EOS,
  ;; return NIL.  Otherwise, move point to just after the end of the string
  ;; and return T.
  ;;
  ;; The EOS is just after the non-WS part of the next match of the regexp
  ;; sentence-end.  Typically, this is just after one of [.!?].  If there is
  ;; no sentence-end match following point, any WS before the end of the
  ;; string will count as EOS, providing we're not already in it.
  ;;
  ;; This code was adapted from GNU Emacs's forward-sentence in paragraphs.el.
  ;; It is not a general function, but is intended only for calling from
  ;; c-move-over-sentence.
  ;;
  ;; This function might do hidden buffer changes.
  (save-match-data
    (let* ((here (point))
	   last
	   ;; Enhance paragraph-start and paragraph-separate to recognize
	   ;; blank lines terminated by escaped EOLs.
	   (paragraph-start c-string-par-start)
	   (paragraph-separate c-string-par-separate)

	   (par-end	; EOL position of last text in current/next paragraph.
	    (save-excursion
	      (save-restriction
		(narrow-to-region (car range) (1- (cdr range)))
		;; The above values of paragraph-\(start\|separate\) are used
		;; in the following.
		(forward-paragraph 1)
		(setq last (point))
		;; (re-search-backward filler-here nil t) would find an empty
		;; string.  Therefore we simulate it by the following:
		(while (or (/= (skip-chars-backward " \t\n\r\f") 0)
			   (re-search-backward "\\\\\\($\\)\\=" nil t)))
		(unless (> (point) here)
		  (goto-char last)
		  (forward-paragraph 1)
		  (while (or (/= (skip-chars-backward " \t\n\r\f") 0)
			     (re-search-backward "\\\\\\($\\)\\=" nil t))))
		(point)))))
      ;; Try to go forward a sentence.
      (when (re-search-forward c-sentence-end-with-esc-eol par-end 'limit)
	(setq last (point))
	(while (or (/= (skip-chars-backward " \t\n") 0)
		   (re-search-backward "\\\\\\($\\)\\=" nil t))))
      ;; Did we move a sentence, or did we hit the end of the string?
      (if (> (point) here)
	  nil
	(goto-char (cdr range))
	t))))

(defun c-ascertain-preceding-literal ()
  ;; Point is not in a literal (i.e. comment or string (include AWK regexp)).
  ;; If a literal is the next thing (aside from whitespace) to be found before
  ;; point, return a cons of its start.end positions (enclosing the
  ;; delimiters).  Otherwise return NIL.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (c-collect-line-comments
     (let ((here (point))
	   pos)
       (if (c-backward-single-comment)
	   (cons (point) (progn (c-forward-single-comment) (point)))
	 (save-restriction
	   ;; to prevent `looking-at' seeing a " at point.
	   (narrow-to-region (point-min) here)
	   (when
	       (or
		;; An EOL can act as an "open string" terminator in AWK.
		(looking-at c-ws*-string-limit-regexp)
		(and (not (bobp))
		     (progn (backward-char)
			    (looking-at c-string-limit-regexp))))
	     (goto-char (match-end 0))	; just after the string terminator.
	     (setq pos (point))
	     (c-safe (c-backward-sexp 1) ; move back over the string.
		     (cons (point) pos)))))))))

(defun c-ascertain-following-literal ()
  ;; Point is not in a literal (i.e. comment or string (include AWK regexp)).
  ;; If a literal is the next thing (aside from whitespace) following point,
  ;; return a cons of its start.end positions (enclosing the delimiters).
  ;; Otherwise return NIL.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (c-collect-line-comments
     (let (pos)
       (c-skip-ws-forward)
       (if (looking-at c-string-limit-regexp) ; string-delimiter.
	   (cons (point) (or (c-safe (progn (c-forward-sexp 1) (point)))
			     (point-max)))
	 (setq pos (point))
	 (if (c-forward-single-comment)
	     (cons pos (point))))))))

(defun c-after-statement-terminator-p () ; Should we pass in LIM here?
  ;; Does point immediately follow a statement "terminator"?  A virtual
  ;; semicolon is regarded here as such.  So is an opening brace ;-)
  ;;
  ;; This function might do hidden buffer changes.
  (or (save-excursion
	(backward-char)
	(and (looking-at "[;{}]")
	     (not (and c-special-brace-lists ; Pike special brace lists.
		       (eq (char-after) ?{)
		       (c-looking-at-special-brace-list)))))
      (c-at-vsemi-p)
      ;; The following (for macros) is not strict about exactly where we are
      ;; wrt white space at the end of the macro.  Doesn't seem to matter too
      ;; much.  ACM 2004/3/29.
      (let (eom)
	(save-excursion
	  (if (c-beginning-of-macro)
	      (setq eom (progn (c-end-of-macro)
			       (point)))))
	(when eom
	  (save-excursion
	    (c-forward-comments)
	    (>= (point) eom))))))

(defun c-back-over-illiterals (macro-start)
  ;; Move backwards over code which isn't a literal (i.e. comment or string),
  ;; stopping before reaching BOB or a literal or the boundary of a
  ;; preprocessor statement or the "beginning of a statement".  MACRO-START is
  ;; the position of the '#' beginning the current preprocessor directive, or
  ;; NIL if we're not in such.
  ;;
  ;; Return a cons (A.B), where
  ;;   A is NIL if we moved back to a BOS (and know it), T otherwise (we
  ;;     didn't move, or we hit a literal, or we're not sure about BOS).
  ;;   B is MACRO-BOUNDARY if we are about to cross the boundary out of or
  ;;     into a macro, otherwise LITERAL if we've hit a literal, otherwise NIL
  ;;
  ;;   The total collection of returned values is as follows:
  ;;     (nil . nil): Found a BOS whilst remaining inside the illiterals.
  ;;     (t . literal): No BOS found: only a comment/string.  We _might_ be at
  ;;                    a BOS - the caller must check this.
  ;;     (nil . macro-boundary): only happens with non-nil macro-start.  We've
  ;;                             moved and reached the opening # of the macro.
  ;;     (t . macro-boundary): Every other circumstance in which we're at a
  ;;                           macro-boundary.  We might be at a BOS.
  ;;
  ;; Point is left either at the beginning-of-statement, or at the last non-ws
  ;; code before encountering the literal/BOB or macro-boundary.
  ;;
  ;; Note that this function moves within either preprocessor commands
  ;; (macros) or normal code, but will not cross a boundary between the two,
  ;; or between two distinct preprocessor commands.
  ;;
  ;; Stop before `{' and after `;', `{', `}' and `};' when not followed by `}'
  ;; or `)', but on the other side of the syntactic ws.  Move by sexps and
  ;; move into parens.  Also stop before `#' when it's at boi on a line.
  ;;
  ;; This function might do hidden buffer changes.
  (save-match-data
    (let ((here (point))
	  last) ; marks the position of non-ws code, what'll be BOS if, say, a
					; semicolon precedes it.
      (catch 'done
	(while t ;; We go back one "token" each iteration of the loop.
	  (setq last (point))
	  (cond
	  ;; Stop at the token after a comment.
	   ((c-backward-single-comment) ; Also functions as backwards-ws.
	    (goto-char last)
	    (throw 'done '(t . literal)))

	  ;; If we've gone back over a LF, we might have moved into or out of
	  ;; a preprocessor line.
	   ((and (save-excursion
		   (beginning-of-line)
		   (re-search-forward "\\(^\\|[^\\]\\)[\n\r]" last t))
		 (if macro-start
		     (< (point) macro-start)
		   (c-beginning-of-macro)))
	    (goto-char last)
	    ;; Return a car of NIL ONLY if we've hit the opening # of a macro.
	    (throw 'done (cons (or (eq (point) here)
				   (not macro-start))
			       'macro-boundary)))

	   ;; Have we found a virtual semicolon?  If so, stop, unless the next
	   ;; statement is where we started from.
	   ((and (c-at-vsemi-p)
		 (< last here)
		 (not (memq (char-after last) '(?\) ?})))) ; we've moved back from ) or }
	    (goto-char last)
	    (throw 'done '(nil . nil)))

	   ;; Hit the beginning of the buffer/region?
	   ((bobp)
	    (if (/= here last)
		(goto-char last))
	    (throw 'done '(nil . nil)))

	   ;; Move back a character.
	   ((progn (backward-char) nil))

	   ;; Stop at "{" (unless it's a PIKE special brace list.)
	   ((eq (char-after) ?\{)
	    (if (and c-special-brace-lists
		     (c-looking-at-special-brace-list))
		(skip-syntax-backward "w_") ; Speedup only.
	      (if (/= here last)
		  (goto-char last))
	      (throw 'done '(nil . nil))))

	   ;; Have we reached the start of a macro?  This always counts as
	   ;; BOS.  (N.B. I don't think (eq (point) here) can ever be true
	   ;; here.  FIXME!!! ACM 2004/3/29)
	   ((and macro-start (eq (point) macro-start))
 	    (throw 'done (cons (eq (point) here) 'macro-boundary)))

	   ;; Stop at token just after "}" or ";".
	   ((looking-at "[;}]")
	    ;; If we've gone back over ;, {, or }, we're done.
	    (if (or (= here last)
		    (memq (char-after last) '(?\) ?})))	; we've moved back from ) or }
		(if (and (eq (char-before) ?}) ; If };, treat them as a unit.
			 (eq (char-after) ?\;))
		    (backward-char))
	      (goto-char last)	 ; To the statement starting after the ; or }.
	      (throw 'done '(nil . nil))))

	   ;; Stop at the token after a string.
	   ((looking-at c-string-limit-regexp) ; Just gone back over a string terminator?
	    (goto-char last)
	    (throw 'done '(t . literal)))

	   ;; Nothing special: go back word characters.
	   (t (skip-syntax-backward "w_")) ; Speedup only.
	   ))))))

(defun c-forward-over-illiterals (macro-end allow-early-stop)
  ;; Move forwards over code, stopping before reaching EOB or a literal
  ;; (i.e. a comment/string) or the boundary of a preprocessor statement or
  ;; the "end of a statement".  MACRO-END is the position of the EOL/EOB which
  ;; terminates the current preprocessor directive, or NIL if we're not in
  ;; such.
  ;;
  ;; ALLOW-EARLY-STOP is non-nil if it is permissible to return without moving
  ;; forward at all, should we encounter a `{'.  This is an ugly kludge, but
  ;; seems unavoidable.  Depending on the context this function is called
  ;; from, we _sometimes_ need to stop there.  Currently (2004/4/3),
  ;; ALLOW-EARLY-STOP is applied only to open braces, not to virtual
  ;; semicolons, or anything else.
  ;;
  ;; Return a cons (A.B), where
  ;;   A is NIL if we moved forward to an EOS, or stay at one (when
  ;;     ALLOW-EARLY-STOP is set), T otherwise (we hit a literal).
  ;;   B is 'MACRO-BOUNDARY if we are about to cross the boundary out of or
  ;;     into a macro, otherwise 'LITERAL if we've hit a literal, otherwise NIL
  ;;
  ;; Point is left either after the end-of-statement, or at the last non-ws
  ;; code before encountering the literal, or the # of the preprocessor
  ;; statement, or at EOB [or just after last non-WS stuff??].
  ;;
  ;; As a clarification of "after the end-of-statement", if a comment or
  ;; whitespace follows a completed AWK statement, that statement is treated
  ;; as ending just after the last non-ws character before the comment.
  ;;
  ;; Note that this function moves within either preprocessor commands
  ;; (macros) or normal code, but not both within the same invocation.
  ;;
  ;; Stop before `{', `}', and `#' when it's at boi on a line, but on the
  ;; other side of the syntactic ws, and after `;', `}' and `};'.  Only
  ;; stop before `{' if at top level or inside braces, though.  Move by
  ;; sexps and move into parens.  Also stop at eol of lines with `#' at
  ;; the boi.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((here (point))
	last)
    (catch 'done
      (while t ;; We go one "token" forward each time round this loop.
	(setq last (point))

	;; If we've moved forward to a virtual semicolon, we're done.
	(if (and (> last here) ; Should we check ALLOW-EARLY-STOP, here? 2004/4/3
		 (c-at-vsemi-p))
	    (throw 'done '(nil . nil)))

	(c-skip-ws-forward)
	(cond
	 ;; Gone past the end of a macro?
	 ((and macro-end (> (point) macro-end))
	  (goto-char last)
	  (throw 'done (cons (eq (point) here) 'macro-boundary)))

	 ;; About to hit a comment?
	 ((save-excursion (c-forward-single-comment))
	  (goto-char last)
	  (throw 'done '(t . literal)))

	 ;; End of buffer?
	 ((eobp)
	  (if (/= here last)
	      (goto-char last))
	  (throw 'done '(nil . nil)))

	 ;; If we encounter a '{', stop just after the previous token.
	 ((and (eq (char-after) ?{)
	       (not (and c-special-brace-lists
			 (c-looking-at-special-brace-list)))
	       (or allow-early-stop (/= here last))
	       (save-excursion	; Is this a check that we're NOT at top level?
;;;; NO!  This seems to check that (i) EITHER we're at the top level; OR (ii) The next enclosing
;;;; level of bracketing is a '{'.  HMM.  Doesn't seem to make sense.
;;;; 2003/8/8 This might have something to do with the GCC extension "Statement Expressions", e.g.
;;;; while ({stmt1 ; stmt2 ; exp ;}).  This form excludes such Statement Expressions.
		 (or (not (c-safe (up-list -1) t))
		     (= (char-after) ?{))))
	  (goto-char last)
	  (throw 'done '(nil . nil)))

	 ;; End of a PIKE special brace list?  If so, step over it and continue.
	 ((and c-special-brace-lists
	       (eq (char-after) ?})
	       (save-excursion
		 (and (c-safe (up-list -1) t)
		      (c-looking-at-special-brace-list))))
	  (forward-char)
	  (skip-syntax-forward "w_"))	; Speedup only.

	 ;; Have we got a '}' after having moved?  If so, stop after the
	 ;; previous token.
	 ((and (eq (char-after) ?})
	       (/= here last))
	  (goto-char last)
	  (throw 'done '(nil . nil)))

	 ;; Stop if we encounter a preprocessor line.  Continue if we
	 ;; hit a naked #
	 ((and c-opt-cpp-prefix
	       (not macro-end)
	       (eq (char-after) ?#)
	       (= (point) (c-point 'boi)))
	  (if (= (point) here)          ; Not a macro, therefore naked #.
	      (forward-char)
	    (throw 'done '(t . macro-boundary))))

	 ;; Stop after a ';', '}', or "};"
	 ((looking-at ";\\|};?")
	  (goto-char (match-end 0))
	  (throw 'done '(nil . nil)))

	 ;; Found a string (this subsumes AWK regexps)?
	 ((looking-at c-string-limit-regexp)
	  (goto-char last)
	  (throw 'done '(t . literal)))

	 (t
	  (forward-char)	  ; Can't fail - we checked (eobp) earlier on.
	  (skip-syntax-forward "w_")	; Speedup only.
	  (when (and macro-end (> (point) macro-end))
	    (goto-char last)
	    (throw 'done (cons (eq (point) here) 'macro-boundary))))
	 )))))

(defun c-one-line-string-p (range)
  ;; Is the literal defined by RANGE a string contained in a single line?
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (goto-char (car range))
    (and (looking-at c-string-limit-regexp)
	 (progn (skip-chars-forward "^\n" (cdr range))
		(eq (point) (cdr range))))))

(defun c-beginning-of-statement (&optional count lim sentence-flag)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the closest
preceding one, moving into nested blocks if necessary (use
\\[backward-sexp] to skip over a block).  If within or next to a
comment or multiline string, move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search for the syntactic context, and a flag saying whether to do
sentence motion in or near comments and multiline strings.

Note that for use in programs, `c-beginning-of-statement-1' is
usually better.  It has much better defined semantics than this one,
which is intended for interactive use, and might therefore change to
be more \"DWIM:ey\"."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (if (< count 0)
      (c-end-of-statement (- count) lim sentence-flag)
    (c-save-buffer-state
	((count (or count 1))
	 last ; start point for going back ONE chunk.  Updated each chunk movement.
	 (macro-fence
	  (save-excursion (and (not (bobp)) (c-beginning-of-macro) (point))))
	 res				; result from sub-function call
	 not-bos			; "not beginning-of-statement"
	 (range (c-collect-line-comments (c-literal-limits lim)))) ; (start.end) of current literal or NIL

      ;; Go back one statement at each iteration of the following loop.
      (while (and (/= count 0)
		  (or (not lim) (> (point) lim)))
	;; Go back one "chunk" each time round the following loop, stopping
	;; when we reach a statement boundary, etc.
	(setq last (point))
	(while
	    (cond ; Each arm of this cond returns NIL on reaching a desired
		  ; statement boundary, non-NIL otherwise.
	     ((bobp)
	      (setq count 0)
	      nil)

	     (range		   ; point is within or approaching a literal.
	      (cond
	       ;; Single line string or sentence-flag is null => skip the
	       ;; entire literal.
	       ((or (null sentence-flag)
		    (c-one-line-string-p range))
		(goto-char (car range))
		(setq range (c-ascertain-preceding-literal))
		;; N.B. The following is essentially testing for an AWK regexp
		;; at BOS:
		;; Was the previous non-ws thing an end of statement?
		(save-excursion
		  (if macro-fence
		      (c-backward-comments)
		    (c-backward-syntactic-ws))
		  (not (or (bobp) (c-after-statement-terminator-p)))))

	       ;; Comment inside a statement or a multi-line string.
	       (t (when (setq res ; returns non-nil when we go out of the literal
			      (if (eq (c-literal-type range) 'string)
				  (c-beginning-of-sentence-in-string range)
				(c-beginning-of-sentence-in-comment range)))
		    (setq range (c-ascertain-preceding-literal)))
		  res)))

	     ;; Non-literal code.
	     (t (setq res (c-back-over-illiterals macro-fence))
		(setq not-bos	       ; "not reached beginning-of-statement".
		      (or (= (point) last)
			  (memq (char-after) '(?\) ?\}))
			  (and
			   (car res)
			   ;; We're at a tentative BOS.  The next form goes
			   ;; back over WS looking for an end of previous
			   ;; statement.
			   (not (save-excursion
				  (if macro-fence
				      (c-backward-comments)
				    (c-backward-syntactic-ws))
				  (or (bobp) (c-after-statement-terminator-p)))))))
		;; Are we about to move backwards into or out of a
		;; preprocessor command?  If so, locate its beginning.
		(when (eq (cdr res) 'macro-boundary)
		  (save-excursion
		    (beginning-of-line)
		    (setq macro-fence
			  (and (not (bobp))
			       (progn (c-skip-ws-backward) (c-beginning-of-macro))
			       (point)))))
		;; Are we about to move backwards into a literal?
		(when (memq (cdr res) '(macro-boundary literal))
		  (setq range (c-ascertain-preceding-literal)))
		not-bos))
	  (setq last (point)))

	(if (/= count 0) (setq count (1- count))))
      (c-keep-region-active))))

(defun c-end-of-statement (&optional count lim sentence-flag)
  "Go to the end of the innermost C statement.
With prefix arg, go forward N - 1 statements.  Move forward to the end
of the next statement if already at end, and move into nested blocks
\(use \\[forward-sexp] to skip over a block).  If within or next to a
comment or multiline string, move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search for the syntactic context, and a flag saying whether to do
sentence motion in or near comments and multiline strings."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (setq count (or count 1))
  (if (< count 0) (c-beginning-of-statement (- count) lim sentence-flag)

    (c-save-buffer-state
	(here ; start point for going forward ONE statement.  Updated each statement.
	 (macro-fence
	  (save-excursion
	    (and (not (eobp)) (c-beginning-of-macro)
		 (progn (c-end-of-macro) (point)))))
	 res
	 (range (c-collect-line-comments (c-literal-limits lim)))) ; (start.end) of current literal or NIL

      ;; Go back/forward one statement at each iteration of the following loop.
      (while (and (/= count 0)
		  (or (not lim) (< (point) lim)))
	(setq here (point))		; ONLY HERE is HERE updated

	;; Go forward one "chunk" each time round the following loop, stopping
	;; when we reach a statement boundary, etc.
	(while
	    (cond    ; Each arm of this cond returns NIL on reaching a desired
		     ; statement boundary, non-NIL otherwise.
	     ((eobp)
	      (setq count 0)
	      nil)

	     (range			; point is within a literal.
	      (cond
	       ;; sentence-flag is null => skip the entire literal.
	       ;; or a Single line string.
	       ((or (null sentence-flag)
		    (c-one-line-string-p range))
		(goto-char (cdr range))
		(setq range (c-ascertain-following-literal))
		;; Is there a virtual semicolon here (e.g. for AWK)?
		(not (c-at-vsemi-p)))

	       ;; Comment or multi-line string.
	       (t (when (setq res ; gets non-nil when we go out of the literal
			      (if (eq (c-literal-type range) 'string)
				  (c-end-of-sentence-in-string range)
				(c-end-of-sentence-in-comment range)))
		    (setq range (c-ascertain-following-literal)))
		  ;; If we've just come forward out of a literal, check for
		  ;; vsemi.  (N.B. AWK can't have a vsemi after a comment, but
		  ;; some other language may do in the future)
		  (and res
		       (not (c-at-vsemi-p))))))

	     ;; Non-literal code.
	     (t (setq res (c-forward-over-illiterals macro-fence
						     (> (point) here)))
		;; Are we about to move forward into or out of a
		;; preprocessor command?
		(when (eq (cdr res) 'macro-boundary)
		  (setq macro-fence
			(save-excursion
			  (if macro-fence
			      (progn
				(end-of-line)
				(and (not (eobp))
				     (progn (c-skip-ws-forward)
					    (c-beginning-of-macro))
				     (progn (c-end-of-macro)
					    (point))))
			    (and (not (eobp))
				 (c-beginning-of-macro)
				 (progn (c-end-of-macro) (point)))))))
		;; Are we about to move forward into a literal?
		(when (memq (cdr res) '(macro-boundary literal))
		  (setq range (c-ascertain-following-literal)))
		(car res))))

	(if (/= count 0) (setq count (1- count))))
      (c-keep-region-active))))


;; set up electric character functions to work with pending-del,
;; (a.k.a. delsel) mode.  All symbols get the t value except
;; the functions which delete, which gets 'supersede.
(mapc
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for delsel (Emacs)
    (put sym 'pending-delete t)))	; for pending-del (XEmacs)
 '(c-electric-pound
   c-electric-brace
   c-electric-slash
   c-electric-star
   c-electric-semi&comma
   c-electric-lt-gt
   c-electric-colon
   c-electric-paren))
(put 'c-electric-delete    'delete-selection 'supersede) ; delsel
(put 'c-electric-delete    'pending-delete   'supersede) ; pending-del
(put 'c-electric-backspace 'delete-selection 'supersede) ; delsel
(put 'c-electric-backspace 'pending-delete   'supersede) ; pending-del
(put 'c-electric-delete-forward 'delete-selection 'supersede) ; delsel
(put 'c-electric-delete-forward 'pending-delete   'supersede) ; pending-del


;; Inserting/indenting comments
(defun c-calc-comment-indent (entry)
  ;; This function might do hidden buffer changes.
  (if (symbolp entry)
      (setq entry (or (assq entry c-indent-comment-alist)
		      (assq 'other c-indent-comment-alist)
		      '(default . (column . nil)))))
  (let ((action (car (cdr entry)))
	(value (cdr (cdr entry)))
	(col (current-column)))
    (cond ((eq action 'space)
	   (+ col value))
	  ((eq action 'column)
	   (unless value (setq value comment-column))
	   (if (bolp)
	       ;; Do not pad with one space if we're at bol.
	       value
	     (max (1+ col) value)))
	  ((eq action 'align)
	   (or (save-excursion
		 (beginning-of-line)
		 (unless (bobp)
		   (backward-char)
		   (let ((lim (c-literal-limits (c-point 'bol) t)))
		     (when (consp lim)
		       (goto-char (car lim))
		       (when (looking-at "/[/*]") ; FIXME!!!  Adapt for AWK! (ACM, 2005/11/18)
			 ;; Found comment to align with.
			 (if (bolp)
			     ;; Do not pad with one space if we're at bol.
			     0
			   (max (1+ col) (current-column))))))))
	       ;; Recurse to handle value as a new spec.
	       (c-calc-comment-indent (cdr entry)))))))

(defun c-comment-indent ()
  "Used by `indent-for-comment' to create and indent comments.
See `c-indent-comment-alist' for a description."
  (save-excursion
    (end-of-line)
    (c-save-buffer-state
	  ((eot (let ((lim (c-literal-limits (c-point 'bol) t)))
		  (or (when (consp lim)
			(goto-char (car lim))
			(when (looking-at "/[/*]")
			  (skip-chars-backward " \t")
			  (point)))
		      (progn
			(skip-chars-backward " \t")
			(point)))))
	   (line-type
	    (cond ((looking-at "^/[/*]")
		   'anchored-comment)
		  ((progn (beginning-of-line)
			  (eq (point) eot))
		   'empty-line)
		  ((progn (back-to-indentation)
			  (and (eq (char-after) ?})
			       (eq (point) (1- eot))))
		   'end-block)
		  ((and (looking-at "#[ \t]*\\(endif\\|else\\)")
			(eq (match-end 0) eot))
		   'cpp-end-block)
		  (t
		   'other)))
	   case-fold-search)
      (if (and (memq line-type '(anchored-comment empty-line))
	       c-indent-comments-syntactically-p)
	  (let ((c-syntactic-context (c-guess-basic-syntax)))
	    ;; BOGOSITY ALERT: if we're looking at the eol, its
	    ;; because indent-for-comment hasn't put the comment-start
	    ;; in the buffer yet.  this will screw up the syntactic
	    ;; analysis so we kludge in the necessary info.  Another
	    ;; kludge is that if we're at the bol, then we really want
	    ;; to ignore any anchoring as specified by
	    ;; c-comment-only-line-offset since it doesn't apply here.
	    (if (eolp)
		(c-add-syntax 'comment-intro))
	    (let ((c-comment-only-line-offset
		   (if (consp c-comment-only-line-offset)
		       c-comment-only-line-offset
		     (cons c-comment-only-line-offset
			   c-comment-only-line-offset))))
	      (c-get-syntactic-indentation c-syntactic-context)))
	(goto-char eot)
	(c-calc-comment-indent line-type)))))


;; used by outline-minor-mode
(defun c-outline-level ()
  (let (buffer-invisibility-spec);; This so that `current-column' DTRT
				 ;; in otherwise-hidden text.
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))


;; Movement by CPP conditionals.
(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.

\"#elif\" is treated like \"#else\" followed by \"#if\", so the
function stops at them when going backward, but not when going
forward."
  (interactive "p")
  (let ((new-point (c-scan-conditionals (- count) -1)))
    (push-mark)
    (goto-char new-point))
  (c-keep-region-active))

(defun c-up-conditional-with-else (count)
  "Move back to the containing preprocessor conditional, including \"#else\".
Just like `c-up-conditional', except it also stops at \"#else\"
directives."
  (interactive "p")
  (let ((new-point (c-scan-conditionals (- count) -1 t)))
    (push-mark)
    (goto-char new-point))
  (c-keep-region-active))

(defun c-down-conditional (count)
  "Move forward into the next preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward into the previous preprocessor conditional.

\"#elif\" is treated like \"#else\" followed by \"#if\", so the
function stops at them when going forward, but not when going
backward."
  (interactive "p")
  (let ((new-point (c-scan-conditionals count 1)))
    (push-mark)
    (goto-char new-point))
  (c-keep-region-active))

(defun c-down-conditional-with-else (count)
  "Move forward into the next preprocessor conditional, including \"#else\".
Just like `c-down-conditional', except it also stops at \"#else\"
directives."
  (interactive "p")
  (let ((new-point (c-scan-conditionals count 1 t)))
    (push-mark)
    (goto-char new-point))
  (c-keep-region-active))

(defun c-backward-conditional (count &optional target-depth with-else)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional.

The optional arguments TARGET-DEPTH and WITH-ELSE are historical,
and have the same meanings as in `c-scan-conditionals'.  If you
are calling c-forward-conditional from a program, you might want
to call `c-scan-conditionals' directly instead."
  (interactive "p")
  (let ((new-point (c-scan-conditionals (- count) target-depth with-else)))
    (push-mark)
    (goto-char new-point))
  (c-keep-region-active))

(defun c-forward-conditional (count &optional target-depth with-else)
  "Move forward across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward across a preprocessor conditional.

If there aren't enough conditionals after \(or before) point, an
error is signaled.

\"#elif\" is treated like \"#else\" followed by \"#if\", except that
the nesting level isn't changed when tracking subconditionals.

The optional arguments TARGET-DEPTH and WITH-ELSE are historical,
and have the same meanings as in `c-scan-conditionals'.  If you
are calling c-forward-conditional from a program, you might want
to call `c-scan-conditionals' directly instead."
  (interactive "p")
  (let ((new-point (c-scan-conditionals count target-depth with-else)))
    (push-mark)
    (goto-char new-point)))

(defun c-scan-conditionals (count &optional target-depth with-else)
  "Scan forward across COUNT preprocessor conditionals.
With a negative argument, scan backward across preprocessor
conditionals.  Return the end position.  Point is not moved.

If there aren't enough preprocessor conditionals, throw an error.

\"#elif\" is treated like \"#else\" followed by \"#if\", except that
the nesting level isn't changed when tracking subconditionals.

The optional argument TARGET-DEPTH specifies the wanted nesting depth
after each scan.  E.g. if TARGET-DEPTH is -1, the end position will be
outside the enclosing conditional.  A non-integer non-nil TARGET-DEPTH
counts as -1.

If the optional argument WITH-ELSE is non-nil, \"#else\" directives
are treated as conditional clause limits.  Normally they are ignored."
  (let* ((forward (> count 0))
	 (increment (if forward -1 1))
	 (search-function (if forward 're-search-forward 're-search-backward))
	 new case-fold-search)
    (unless (integerp target-depth)
      (setq target-depth (if target-depth -1 0)))
    (save-excursion
      (while (/= count 0)
	(let ((depth 0)
	      ;; subdepth is the depth in "uninteresting" subtrees,
	      ;; i.e. those that takes us farther from the target
	      ;; depth instead of closer.
	      (subdepth 0)
	      found)
	  (save-excursion
	    ;; Find the "next" significant line in the proper direction.
	    (while (and (not found)
			;; Rather than searching for a # sign that
			;; comes at the beginning of a line aside from
			;; whitespace, search first for a string
			;; starting with # sign.  Then verify what
			;; precedes it.  This is faster on account of
			;; the fastmap feature of the regexp matcher.
			(funcall search-function
				 "#[ \t]*\\(if\\|elif\\|endif\\|else\\)"
				 nil t))
	      (beginning-of-line)
	      ;; Now verify it is really a preproc line.
	      (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\|else\\)")
		  (let (dchange (directive (match-string 1)))
		    (cond ((string= directive "if")
			   (setq dchange (- increment)))
			  ((string= directive "endif")
			   (setq dchange increment))
			  ((= subdepth 0)
			   ;; When we're not in an "uninteresting"
			   ;; subtree, we might want to act on "elif"
			   ;; and "else" too.
			   (if (cond (with-else
				      ;; Always move toward the target depth.
				      (setq dchange
					    (if (> target-depth 0) 1 -1)))
				     ((string= directive "elif")
				      (setq dchange (- increment))))
			       ;; Ignore the change if it'd take us
			       ;; into an "uninteresting" subtree.
			       (if (eq (> dchange 0) (<= target-depth 0))
				   (setq dchange nil)))))
		    (when dchange
		      (when (or (/= subdepth 0)
				(eq (> dchange 0) (<= target-depth 0)))
			(setq subdepth (+ subdepth dchange)))
		      (setq depth (+ depth dchange))
		      ;; If we are trying to move across, and we find an
		      ;; end before we find a beginning, get an error.
		      (if (and (< depth target-depth) (< dchange 0))
			  (error (if forward
				     "No following conditional at this level"
				   "No previous conditional at this level"))))
		    ;; When searching forward, start from next line so
		    ;; that we don't find the same line again.
		    (if forward (forward-line 1))
		    ;; We found something if we've arrived at the
		    ;; target depth.
		    (if (and dchange (= depth target-depth))
			(setq found (point))))
		;; else
		(if forward (forward-line 1)))))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (c-keep-region-active)
    new))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional arg)
  "Indent current line as C code, and/or insert some whitespace.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert some whitespace[*].  If
other than nil or t, then some whitespace[*] is inserted only within
literals (comments and strings), but the line is always reindented.

If `c-syntactic-indentation' is t, indentation is done according to
the syntactic context.  A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after
point so that this line becomes properly indented.  The relative
indentation among the lines of the expression is preserved.

If `c-syntactic-indentation' is nil, the line is just indented one
step according to `c-basic-offset'.  In this mode, a numeric argument
indents a number of such steps, positive or negative, and an empty
prefix argument is equivalent to -1.

  [*] The amount and kind of whitespace inserted is controlled by the
  variable `c-insert-tab-function', which is called to do the actual
  insertion of whitespace.  Normally the function in this variable
  just inserts a tab character, or the equivalent number of spaces,
  depending on the variable `indent-tabs-mode'."

  (interactive "P")
  (let ((indent-function
	 (if c-syntactic-indentation
	     (symbol-function 'indent-according-to-mode)
	   (lambda ()
	     (let ((c-macro-start c-macro-start)
		   (steps (if (equal arg '(4))
			      -1
			    (prefix-numeric-value arg))))
	       (c-shift-line-indentation (* steps c-basic-offset))
	       (when (and c-auto-align-backslashes
			  (save-excursion
			    (end-of-line)
			    (eq (char-before) ?\\))
			  (c-query-and-set-macro-start))
		 ;; Realign the line continuation backslash if inside a macro.
		 (c-backslash-region (point) (point) nil t)))
	     ))))
    (if (and c-syntactic-indentation arg)
	;; If c-syntactic-indentation and got arg, always indent this
	;; line as C and shift remaining lines of expression the same
	;; amount.
	(let ((shift-amt (save-excursion
			   (back-to-indentation)
			   (current-column)))
	      beg end)
	  (c-indent-line)
	  (setq shift-amt (- (save-excursion
			       (back-to-indentation)
			       (current-column))
			     shift-amt))
	  (save-excursion
	    (if (eq c-tab-always-indent t)
		(beginning-of-line))	; FIXME!!! What is this here for?  ACM 2005/10/31
	    (setq beg (point))
	    (c-forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end shift-amt "#")))
      ;; Else use c-tab-always-indent to determine behavior.
      (cond
       ;; CASE 1: indent when at column zero or in line's indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (funcall c-insert-tab-function)
	  (funcall indent-function)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(funcall indent-function))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-save-buffer-state () (c-in-literal))
	    (funcall c-insert-tab-function))
	(funcall indent-function)
	)))))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in the balanced expression following point syntactically.
If optional SHUTUP-P is non-nil, no errors are signaled if no
balanced expression is found."
  (interactive "*P")
  (let ((here (point-marker))
	end)
    (set-marker-insertion-type here t)
    (unwind-protect
	(let ((start (save-restriction
		       ;; Find the closest following open paren that
		       ;; ends on another line.
		       (narrow-to-region (point-min) (c-point 'eol))
		       (let (beg (end (point)))
			 (while (and (setq beg (c-down-list-forward end))
				     (setq end (c-up-list-forward beg))))
			 (and beg
			      (eq (char-syntax (char-before beg)) ?\()
			      (1- beg))))))
	  ;; sanity check
	  (if (not start)
	     (unless shutup-p
	       (error "Cannot find start of balanced expression to indent"))
	    (goto-char start)
	    (setq end (c-safe (scan-sexps (point) 1)))
	    (if (not end)
		(unless shutup-p
		  (error "Cannot find end of balanced expression to indent"))
	      (forward-line)
	      (if (< (point) end)
		  (c-indent-region (point) end)))))
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-defun ()
  "Indent the current top-level declaration or macro syntactically.
In the macro case this also has the effect of realigning any line
continuation backslashes, unless `c-auto-align-backslashes' is nil."
  (interactive "*")
  (let ((here (point-marker)) decl-limits case-fold-search)
    (unwind-protect
	(progn
	  (c-save-buffer-state nil
	    ;; We try to be line oriented, unless there are several
	    ;; declarations on the same line.
	    (if (looking-at c-syntactic-eol)
		(c-backward-token-2 1 nil (c-point 'bol))
	      (c-forward-token-2 0 nil (c-point 'eol)))
	    (setq decl-limits (c-declaration-limits nil)))
	  (if decl-limits
	      (c-indent-region (car decl-limits)
			       (cdr decl-limits))))
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-region (start end &optional quiet)
  "Indent syntactically every line whose first char is between START
and END inclusive.  If the optional argument QUIET is non-nil then no
syntactic errors are reported, even if `c-report-syntactic-errors' is
non-nil."
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t\n\r\f\v")
    (setq end (point))
    (goto-char start)
    ;; Advance to first nonblank line.
    (beginning-of-line)
    (skip-chars-forward " \t\n\r\f\v")
    (setq start (point))
    (beginning-of-line)
    (setq c-parsing-error
	  (or (let ((endmark (copy-marker end))
		    (c-parsing-error nil)
		    ;; shut up any echo msgs on indiv lines
		    (c-echo-syntactic-information-p nil)
		    (ml-macro-start	; Start pos of multi-line macro.
		     (and (c-save-buffer-state ()
			    (save-excursion (c-beginning-of-macro)))
			  (eq (char-before (c-point 'eol)) ?\\)
			  start))
		    (c-fix-backslashes nil)
		    syntax)
		(unwind-protect
		    (progn
		      (c-progress-init start end 'c-indent-region)

		      (while (and (bolp) ;; One line each time round the loop.
				  (not (eobp))
				  (< (point) endmark))
			;; update progress
			(c-progress-update)
			;; skip empty lines
			(unless (or (looking-at "\\s *$")
				    (and ml-macro-start (looking-at "\\s *\\\\$")))
			  ;; Get syntax and indent.
			  (c-save-buffer-state nil
			    (setq syntax (c-guess-basic-syntax)))
			  (c-indent-line syntax t t))

			(if ml-macro-start
			    ;; End of current multi-line macro?
			    (when (and c-auto-align-backslashes
				       (not (eq (char-before (c-point 'eol)) ?\\)))
			      ;; Fixup macro backslashes.
			      (c-backslash-region ml-macro-start (c-point 'bonl) nil)
			      (setq ml-macro-start nil))
			  ;; New multi-line macro?
			  (if (and (assq 'cpp-macro syntax)
				   (eq (char-before (c-point 'eol)) ?\\))
			    (setq ml-macro-start (point))))

			(forward-line))

		      (if (and ml-macro-start c-auto-align-backslashes)
			  (c-backslash-region ml-macro-start (c-point 'bopl) nil t)))
		  (set-marker endmark nil)
		  (c-progress-fini 'c-indent-region))
		(c-echo-parsing-error quiet))
	      c-parsing-error))))

(defun c-fn-region-is-active-p ()
  ;; Function version of the macro for use in places that aren't
  ;; compiled, e.g. in the menus.
  (c-region-is-active-p))

(defun c-indent-line-or-region (&optional arg region)
  "Indent active region, current line, or block starting on this line.
In Transient Mark mode, when the region is active, reindent the region.
Otherwise, with a prefix argument, rigidly reindent the expression
starting on the current line.
Otherwise reindent just the current line."
  (interactive
   (list current-prefix-arg (use-region-p)))
  (if region
      (c-indent-region (region-beginning) (region-end))
    (c-indent-command arg)))

;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  (cond
   ;; Be silent
   ((not c-progress-interval))
   ;; Start the progress update messages.  If this Emacs doesn't have
   ;; a built-in timer, just be dumb about it.
   ((not (fboundp 'current-time))
    (message "Indenting region... (this may take a while)"))
   ;; If progress has already been initialized, do nothing. otherwise
   ;; initialize the counter with a vector of:
   ;;     [start end lastsec context]
   (c-progress-info)
   (t (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "Indenting region..."))
   ))

(defun c-progress-update ()
  (if (not (and c-progress-info c-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref c-progress-info 0))
	  (end (aref c-progress-info 1))
	  (lastsecs (aref c-progress-info 2)))
      ;; should we update?  currently, update happens every 2 seconds,
      ;; what's the right value?
      (if (< c-progress-interval (- now lastsecs))
	  (progn
	    (message "Indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset c-progress-info 2 now)))
      )))

(defun c-progress-fini (context)
  (if (not c-progress-interval)
      nil
    (if (or (eq context (aref c-progress-info 3))
	    (eq context t))
	(progn
	  (set-marker (aref c-progress-info 1) nil)
	  (setq c-progress-info nil)
	  (message "Indenting region... done")))))



;;; This page handles insertion and removal of backslashes for C macros.

(defun c-backslash-region (from to delete-flag &optional line-mode)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.  The backslash alignment is
done according to the settings in `c-backslash-column',
`c-backslash-max-column' and `c-auto-align-backslashes'.

This function does not modify blank lines at the start of the region.
If the region ends at the start of a line and the macro doesn't
continue below it, the backslash (if any) at the end of the previous
line is deleted.

You can put the region around an entire macro definition and use this
command to conveniently insert and align the necessary backslashes."
  (interactive "*r\nP")
  (let ((endmark (make-marker))
	;; Keep the backslash trimming functions from changing the
	;; whitespace around point, since in this case it's only the
	;; position of point that tells the indentation of the line.
	(point-pos (if (save-excursion
			 (skip-chars-backward " \t")
			 (and (bolp) (looking-at "[ \t]*\\\\?$")))
		       (point-marker)
		     (point-min)))
	column longest-line-col bs-col-after-end)
    (save-excursion
      (goto-char to)
      (if (and (not line-mode) (bobp))
	  ;; Nothing to do if to is at bob, since we should back up
	  ;; and there's no line to back up to.
	  nil
	(when (and (not line-mode) (bolp))
	  ;; Do not back up the to line if line-mode is set, to make
	  ;; e.g. c-newline-and-indent consistent regardless whether
	  ;; the (newline) call leaves point at bol or not.
	  (backward-char)
	  (setq to (point)))
	(if delete-flag
	    (progn
	      (set-marker endmark (point))
	      (goto-char from)
	      (c-delete-backslashes-forward endmark point-pos))
	  ;; Set bs-col-after-end to the column of any backslash
	  ;; following the region, or nil if there is none.
	  (setq bs-col-after-end
		(and (progn (end-of-line)
			    (eq (char-before) ?\\))
		     (= (forward-line 1) 0)
		     (progn (end-of-line)
			    (eq (char-before) ?\\))
		     (1- (current-column))))
	  (when line-mode
	    ;; Back up the to line if line-mode is set, since the line
	    ;; after the newly inserted line break should not be
	    ;; touched in c-newline-and-indent.
	    (setq to (max from (or (c-safe (c-point 'eopl)) from)))
	    (unless bs-col-after-end
	      ;; Set bs-col-after-end to non-nil in any case, since we
	      ;; do not want to delete the backslash at the last line.
	      (setq bs-col-after-end t)))
	  (if (and line-mode
		   (not c-auto-align-backslashes))
	      (goto-char from)
	    ;; Compute the smallest column number past the ends of all
	    ;; the lines.
	    (setq longest-line-col 0)
	    (goto-char to)
	    (if bs-col-after-end
		;; Include one more line in the max column
		;; calculation, since the to line will be backslashed
		;; too.
		(forward-line 1))
	    (end-of-line)
	    (while (and (>= (point) from)
			(progn
			  (if (eq (char-before) ?\\)
			      (forward-char -1))
			  (skip-chars-backward " \t")
			  (setq longest-line-col (max longest-line-col
						      (1+ (current-column))))
			  (beginning-of-line)
			  (not (bobp))))
	      (backward-char))
	    ;; Try to align with surrounding backslashes.
	    (goto-char from)
	    (beginning-of-line)
	    (if (and (not (bobp))
		     (progn (backward-char)
			    (eq (char-before) ?\\)))
		(progn
		  (setq column (1- (current-column)))
		  (if (numberp bs-col-after-end)
		      ;; Both a preceding and a following backslash.
		      ;; Choose the greatest of them.
		      (setq column (max column bs-col-after-end)))
		  (goto-char from))
	      ;; No preceding backslash.  Try to align with one
	      ;; following the region.  Disregard the backslash at the
	      ;; to line since it's likely to be bogus (e.g. when
	      ;; called from c-newline-and-indent).
	      (if (numberp bs-col-after-end)
		  (setq column bs-col-after-end))
	      ;; Don't modify blank lines at start of region.
	      (goto-char from)
	      (while (and (< (point) to) (bolp) (eolp))
		(forward-line 1)))
	    (if (and column (< column longest-line-col))
		;; Don't try to align with surrounding backslashes if
		;; any line is too long.
		(setq column nil))
	    (unless column
	      ;; Impose minimum limit and tab width alignment only if
	      ;; we can't align with surrounding backslashes.
	      (if (> (% longest-line-col tab-width) 0)
		  (setq longest-line-col
			(* (/ (+ longest-line-col tab-width -1)
			      tab-width)
			   tab-width)))
	      (setq column (max c-backslash-column
				longest-line-col)))
	    ;; Always impose maximum limit.
	    (setq column (min column c-backslash-max-column)))
	  (if bs-col-after-end
	      ;; Add backslashes on all lines if the macro continues
	      ;; after the to line.
	      (progn
		(set-marker endmark to)
		(c-append-backslashes-forward endmark column point-pos))
	    ;; Add backslashes on all lines except the last, and
	    ;; remove any on the last line.
	    (if (save-excursion
		  (goto-char to)
		  (beginning-of-line)
		  (if (not (bobp))
		      (set-marker endmark (1- (point)))))
		(progn
		  (c-append-backslashes-forward endmark column point-pos)
		  ;; The function above leaves point on the line
		  ;; following endmark.
		  (set-marker endmark (point)))
	      (set-marker endmark to))
	    (c-delete-backslashes-forward endmark point-pos)))))
    (set-marker endmark nil)
    (if (markerp point-pos)
	(set-marker point-pos nil))))

(defun c-append-backslashes-forward (to-mark column point-pos)
  (let ((state (parse-partial-sexp (c-point 'bol) (point))))
    (if column
	(while
	    (and
	     (<= (point) to-mark)

	     (let ((start (point)) (inserted nil) end col)
	       (end-of-line)
	       (unless (eq (char-before) ?\\)
		 (insert ?\\)
		 (setq inserted t))
	       (setq state (parse-partial-sexp
			    start (point) nil nil state))
	       (backward-char)
	       (setq col (current-column))

	       ;; Avoid unnecessary changes of the buffer.
	       (cond ((and (not inserted) (nth 3 state))
		      ;; Don't realign backslashes in string literals
		      ;; since that would change them.
		      )

		     ((< col column)
		      (delete-region
		       (point)
		       (progn
			 (skip-chars-backward
			  " \t" (if (>= (point) point-pos) point-pos))
			 (point)))
		      (indent-to column))

		     ((and (= col column)
			   (memq (char-before) '(?\  ?\t))))

		     ((progn
			(setq end (point))
			(or (/= (skip-chars-backward
				 " \t" (if (>= (point) point-pos) point-pos))
				-1)
			    (/= (char-after) ?\ )))
		      (delete-region (point) end)
		      (indent-to column 1)))

	       (zerop (forward-line 1)))
	     (bolp)))			; forward-line has funny behavior at eob.

      ;; Make sure there are backslashes with at least one space in
      ;; front of them.
      (while
	  (and
	   (<= (point) to-mark)

	   (let ((start (point)))
	     (end-of-line)
	     (setq state (parse-partial-sexp
			  start (point) nil nil state))

	     (if (eq (char-before) ?\\)
		 (unless (nth 3 state)
		   (backward-char)
		   (unless (and (memq (char-before) '(?\  ?\t))
				(/= (point) point-pos))
		     (insert ?\ )))

	       (if (and (memq (char-before) '(?\  ?\t))
			(/= (point) point-pos))
		   (insert ?\\)
		 (insert ?\  ?\\)))

	     (zerop (forward-line 1)))
	   (bolp))))))			; forward-line has funny behavior at eob.

(defun c-delete-backslashes-forward (to-mark point-pos)
  (while
      (and (<= (point) to-mark)
	   (progn
	     (end-of-line)
	     (if (eq (char-before) ?\\)
		 (delete-region
		  (point)
		  (progn (backward-char)
			 (skip-chars-backward " \t" (if (>= (point) point-pos)
							point-pos))
			 (point))))
	     (zerop (forward-line 1)))
	   (bolp))))			; forward-line has funny behavior at eob.



;;; Line breaking and paragraph filling.

(defvar c-auto-fill-prefix t)
(defvar c-lit-limits nil)
(defvar c-lit-type nil)

;; The filling code is based on a simple theory; leave the intricacies
;; of the text handling to the currently active mode for that
;; (e.g. adaptive-fill-mode or filladapt-mode) and do as little as
;; possible to make them work correctly wrt the comment and string
;; separators, one-line paragraphs etc.  Unfortunately, when it comes
;; to it, there's quite a lot of special cases to handle which makes
;; the code anything but simple.  The intention is that it will work
;; with any well-written text filling package that preserves a fill
;; prefix.
;;
;; We temporarily mask comment starters and enders as necessary for
;; the filling code to do its job on a seemingly normal text block.
;; We do _not_ mask the fill prefix, so it's up to the filling code to
;; preserve it correctly (especially important when filling C++ style
;; line comments).  By default, we set up and use adaptive-fill-mode,
;; which is standard in all supported Emacs flavors.

(defun c-guess-fill-prefix (lit-limits lit-type)
  ;; Determine the appropriate comment fill prefix for a block or line
  ;; comment.  Return a cons of the prefix string and the column where
  ;; it ends.  If fill-prefix is set, it'll override.  Note that this
  ;; function also uses the value of point in some heuristics.
  ;;
  ;; This function might do hidden buffer changes.

  (let* ((here (point))
	 (prefix-regexp (concat "[ \t]*\\("
				c-current-comment-prefix
				"\\)[ \t]*"))
	 (comment-start-regexp (if (eq lit-type 'c++)
				   prefix-regexp
				 comment-start-skip))
	 prefix-line comment-prefix res comment-text-end)

    (cond
     (fill-prefix
      (setq res (cons fill-prefix
		      ;; Ugly way of getting the column after the fill
		      ;; prefix; it'd be nice with a current-column
		      ;; that works on strings..
		      (let ((start (point)))
			(unwind-protect
			    (progn
			      (insert-and-inherit "\n" fill-prefix)
			      (current-column))
			  (delete-region start (point)))))))

     ((eq lit-type 'c++)
      (save-excursion
	;; Set fallback for comment-prefix if none is found.
	(setq comment-prefix "// "
	      comment-text-end (cdr lit-limits))

	(beginning-of-line)
	(if (> (point) (car lit-limits))
	    ;; The current line is not the comment starter, so the
	    ;; comment has more than one line, and it can therefore be
	    ;; used to find the comment fill prefix.
	    (setq prefix-line (point))

	  (goto-char (car lit-limits))
	  (if (and (= (forward-line 1) 0)
		   (< (point) (cdr lit-limits)))
	      ;; The line after the comment starter is inside the
	      ;; comment, so we can use it.
	      (setq prefix-line (point))

	    ;; The comment is only one line.  Take the comment prefix
	    ;; from it and keep the indentation.
	    (goto-char (car lit-limits))
	    (if (looking-at prefix-regexp)
		(goto-char (match-end 0))
	      (forward-char 2)
	      (skip-chars-forward " \t"))

	    (let (str col)
	      (if (eq (c-point 'boi) (car lit-limits))
		  ;; There is only whitespace before the comment
		  ;; starter; take the prefix straight from this line.
		  (setq str (buffer-substring-no-properties
			     (c-point 'bol) (point))
			col (current-column))

		;; There is code before the comment starter, so we
		;; have to temporarily insert and indent a new line to
		;; get the right space/tab mix in the indentation.
		(let ((prefix-len (- (point) (car lit-limits)))
		      tmp)
		  (unwind-protect
		      (progn
			(goto-char (car lit-limits))
			(indent-to (prog1 (current-column)
				     (insert ?\n)))
			(setq tmp (point))
			(forward-char prefix-len)
			(setq str (buffer-substring-no-properties
				   (c-point 'bol) (point))
			      col (current-column)))
		    (delete-region (car lit-limits) tmp))))

	      (setq res
		    (if (or (string-match "\\s \\'" str) (not (eolp)))
			(cons str col)
		      ;; The prefix ends the line with no whitespace
		      ;; after it.  Default to a single space.
		      (cons (concat str " ") (1+ col))))
	      )))))

     (t
      (setq comment-text-end
	    (save-excursion
	      (goto-char (- (cdr lit-limits) 2))
	      (if (looking-at "\\*/") (point) (cdr lit-limits))))

      (save-excursion
	(beginning-of-line)
	(if (and (> (point) (car lit-limits))
		 (not (and (looking-at "[ \t]*\\*/")
			   (eq (cdr lit-limits) (match-end 0)))))
	    ;; The current line is not the comment starter and
	    ;; contains more than just the ender, so it's good enough
	    ;; to be used for the comment fill prefix.
	    (setq prefix-line (point))
	  (goto-char (car lit-limits))

	  (cond ((or (/= (forward-line 1) 0)
		     (>= (point) (cdr lit-limits))
		     (and (looking-at "[ \t]*\\*/")
			  (eq (cdr lit-limits) (match-end 0)))
		     (and (looking-at prefix-regexp)
			  (<= (1- (cdr lit-limits)) (match-end 0))))
		 ;; The comment is either one line or the next line contains
		 ;; just the comment ender.  In this case we have no
		 ;; information about a suitable comment prefix, so we resort
		 ;; to c-block-comment-prefix.
		 (setq comment-prefix (or c-block-comment-prefix "")))

		((< here (point))
		 ;; The point was on the comment opener line, so we might want
		 ;; to treat this as a not yet closed comment.

		 (if (and (match-beginning 1)
			  (/= (match-beginning 1) (match-end 1)))
		     ;; Above `prefix-regexp' matched a nonempty prefix on the
		     ;; second line, so let's use it.  Normally it should do
		     ;; to set `prefix-line' and let the code below pick up
		     ;; the whole prefix, but if there's no text after the
		     ;; match then it will probably fall back to no prefix at
		     ;; all if the comment isn't closed yet, so in that case
		     ;; it's better to force use of the prefix matched now.
		     (if (= (match-end 0) (c-point 'eol))
			 (setq comment-prefix (match-string 1))
		       (setq prefix-line (point)))

		   ;; There's no nonempty prefix on the line after the
		   ;; comment opener.  If the line is empty, or if the
		   ;; text on it has less or equal indentation than the
		   ;; comment starter we assume it's an unclosed
		   ;; comment starter, i.e. that
		   ;; `c-block-comment-prefix' should be used.
		   ;; Otherwise we assume it's a closed comment where
		   ;; the prefix really is the empty string.
		   ;; E.g. this is an unclosed comment:
		   ;;
		   ;;     /*
		   ;;     foo
		   ;;
		   ;; But this is not:
		   ;;
		   ;;     /*
		   ;;       foo
		   ;;     */
		   ;;
		   ;; (Looking for the presence of the comment closer
		   ;; rarely works since it's probably the closer of
		   ;; some comment further down when the comment
		   ;; really is unclosed.)
		   (if (<= (save-excursion (back-to-indentation)
					   (current-column))
			   (save-excursion (goto-char (car lit-limits))
					   (current-column)))
		       (setq comment-prefix (or c-block-comment-prefix ""))
		     (setq prefix-line (point)))))

		(t
		 ;; Otherwise the line after the comment starter is good
		 ;; enough to find the prefix in.
		 (setq prefix-line (point))))

	  (when comment-prefix
	    ;; Haven't got the comment prefix on any real line that we
	    ;; can take it from, so we have to temporarily insert
	    ;; `comment-prefix' on a line and indent it to find the
	    ;; correct column and the correct mix of tabs and spaces.
	    (setq res
		  (let (tmp-pre tmp-post)
		    (unwind-protect
			(progn

			  (goto-char (car lit-limits))
			  (if (looking-at comment-start-regexp)
			      (goto-char (min (match-end 0)
					      comment-text-end))
			    (forward-char 2)
			    (skip-chars-forward " \t"))

			  (when (eq (char-syntax (char-before)) ?\ )
			    ;; If there's ws on the current line, we'll use it
			    ;; instead of what's ending comment-prefix.
			    (setq comment-prefix
				  (concat (substring comment-prefix
						     0 (string-match
							"\\s *\\'"
							comment-prefix))
					  (buffer-substring-no-properties
					   (save-excursion
					     (skip-chars-backward " \t")
					     (point))
					   (point)))))

			  (setq tmp-pre (point-marker))

			  ;; We insert an extra non-whitespace character
			  ;; before the line break and after comment-prefix in
			  ;; case it's "" or ends with whitespace.
			  (insert-and-inherit "x\n" comment-prefix "x")
			  (setq tmp-post (point-marker))

			  (indent-according-to-mode)

			  (goto-char (1- tmp-post))
			  (cons (buffer-substring-no-properties
				 (c-point 'bol) (point))
				(current-column)))

		      (when tmp-post
			(delete-region tmp-pre tmp-post)
			(set-marker tmp-pre nil)
			(set-marker tmp-post nil))))))))))

    (or res				; Found a good prefix above.

	(save-excursion
	  ;; prefix-line is the bol of a line on which we should try
	  ;; to find the prefix.
	  (let* (fb-string fb-endpos	; Contains any fallback prefix found.
		 (test-line
		  (lambda ()
		    (when (and (looking-at prefix-regexp)
			       (<= (match-end 0) comment-text-end))
		      (unless (eq (match-end 0) (c-point 'eol))
			;; The match is fine if there's text after it.
			(throw 'found (cons (buffer-substring-no-properties
					     (match-beginning 0) (match-end 0))
					    (progn (goto-char (match-end 0))
						   (current-column)))))
		      (unless fb-string
			;; This match is better than nothing, so let's
			;; remember it in case nothing better is found
			;; on another line.
			(setq fb-string (buffer-substring-no-properties
					 (match-beginning 0) (match-end 0))
			      fb-endpos (match-end 0)))
		      t))))

	    (or (catch 'found
		  ;; Search for a line which has text after the prefix
		  ;; so that we get the proper amount of whitespace
		  ;; after it.  We start with the current line, then
		  ;; search backwards, then forwards.

		  (goto-char prefix-line)
		  (when (and (funcall test-line)
			     (or (/= (match-end 1) (match-end 0))
				 ;; The whitespace is sucked up by the
				 ;; first [ \t]* glob if the prefix is empty.
				 (and (= (match-beginning 1) (match-end 1))
				      (/= (match-beginning 0) (match-end 0)))))
		    ;; If the current line doesn't have text but do
		    ;; have whitespace after the prefix, we'll use it.
		    (throw 'found (cons fb-string
					(progn (goto-char fb-endpos)
					       (current-column)))))

		  (if (eq lit-type 'c++)
		      ;; For line comments we can search up to and
		      ;; including the first line.
		      (while (and (zerop (forward-line -1))
				  (>= (point) (car lit-limits)))
			(funcall test-line))
		    ;; For block comments we must stop before the
		    ;; block starter.
		    (while (and (zerop (forward-line -1))
				(> (point) (car lit-limits)))
		      (funcall test-line)))

		  (goto-char prefix-line)
		  (while (and (zerop (forward-line 1))
			      (< (point) (cdr lit-limits)))
		    (funcall test-line))

		  (goto-char prefix-line)
		  nil)

		(when fb-string
		  ;; A good line wasn't found, but at least we have a
		  ;; fallback that matches the comment prefix regexp.
		  (cond ((or (string-match "\\s \\'" fb-string)
			     (progn
			       (goto-char fb-endpos)
			       (not (eolp))))
			 ;; There are ws or text after the prefix, so
			 ;; let's use it.
			 (cons fb-string (current-column)))

			((progn
			   ;; Check if there's any whitespace padding
			   ;; on the comment start line that we can
			   ;; use after the prefix.
			   (goto-char (car lit-limits))
			   (if (looking-at comment-start-regexp)
			       (goto-char (match-end 0))
			     (forward-char 2)
			     (skip-chars-forward " \t"))
			   (or (not (eolp))
			       (eq (char-syntax (char-before)) ?\ )))

			 (setq fb-string (buffer-substring-no-properties
					  (save-excursion
					    (skip-chars-backward " \t")
					    (point))
					  (point)))
			 (goto-char fb-endpos)
			 (skip-chars-backward " \t")

			 (let ((tmp (point)))
			   ;; Got to mess in the buffer once again to
			   ;; ensure the column gets correct.  :P
			   (unwind-protect
			       (progn
				 (insert-and-inherit fb-string)
				 (cons (buffer-substring-no-properties
					(c-point 'bol)
					(point))
				       (current-column)))
			     (delete-region tmp (point)))))

			(t
			 ;; Last resort: Just add a single space after
			 ;; the prefix.
			 (cons (concat fb-string " ")
			       (progn (goto-char fb-endpos)
				      (1+ (current-column)))))))

		;; The line doesn't match the comment prefix regexp.
		(if comment-prefix
		    ;; We have a fallback for line comments that we must use.
		    (cons (concat (buffer-substring-no-properties
				   prefix-line (c-point 'boi))
				  comment-prefix)
			  (progn (back-to-indentation)
				 (+ (current-column) (length comment-prefix))))

		  ;; Assume we are dealing with a "free text" block
		  ;; comment where the lines doesn't have any comment
		  ;; prefix at all and we should just fill it as
		  ;; normal text.
		  '("" . 0))))))
    ))

(defun c-mask-paragraph (fill-paragraph apply-outside-literal fun &rest args)
  ;; Calls FUN with ARGS ar arguments while the current paragraph is
  ;; masked to allow adaptive filling to work correctly.  That
  ;; includes narrowing the buffer and, if point is inside a comment,
  ;; masking the comment starter and ender appropriately.
  ;;
  ;; FILL-PARAGRAPH is non-nil if called for whole paragraph filling.
  ;; The position of point is then less significant when doing masking
  ;; and narrowing.
  ;;
  ;; If APPLY-OUTSIDE-LITERAL is nil then the function will be called
  ;; only if the point turns out to be inside a comment or a string.
  ;;
  ;; Note that this function does not do any hidden buffer changes.

  (let (fill
	;; beg and end limit the region to narrow.  end is a marker.
	beg end
	;; tmp-pre and tmp-post mark strings that are temporarily
	;; inserted at the start and end of the region.  tmp-pre is a
	;; cons of the positions of the prepended string.  tmp-post is
	;; a marker pointing to the single character of the appended
	;; string.
	tmp-pre tmp-post
	;; If hang-ender-stuck isn't nil, the comment ender is
	;; hanging.  In that case it's set to the number of spaces
	;; that should be between the text and the ender.
	hang-ender-stuck
	;; auto-fill-spaces is the exact sequence of whitespace between a
	;; comment's last word and the comment ender, temporarily replaced
	;; with 'x's before calling FUN when FILL-PARAGRAPH is nil.
	auto-fill-spaces
	(here (point))
	(c-lit-limits c-lit-limits)
	(c-lit-type c-lit-type))

    ;; Restore point on undo.  It's necessary since we do a lot of
    ;; hidden inserts and deletes below that should be as transparent
    ;; as possible.
      (if (and buffer-undo-list (not (eq buffer-undo-list t)))
	(setq buffer-undo-list (cons (point) buffer-undo-list)))

    ;; Determine the limits and type of the containing literal (if any):
    ;; C-LIT-LIMITS, C-LIT-TYPE;  and the limits of the current paragraph:
    ;; BEG and END.
    (c-save-buffer-state ()
      (save-restriction
	;; Widen to catch comment limits correctly.
	(widen)
	(unless c-lit-limits
	  (setq c-lit-limits (c-literal-limits nil fill-paragraph)))
	(setq c-lit-limits (c-collect-line-comments c-lit-limits))
	(unless c-lit-type
	  (setq c-lit-type (c-literal-type c-lit-limits))))

      (save-excursion
	(unless (c-safe (backward-char)
			(forward-paragraph)
			(>= (point) here))
	  (goto-char here)
	  (forward-paragraph))
	(setq end (point-marker)))
      (save-excursion
	(unless (c-safe (forward-char)
			(backward-paragraph)
			(<= (point) here))
	  (goto-char here)
	  (backward-paragraph))
	(setq beg (point))))

    (unwind-protect
	(progn
	  ;; For each of the possible types of text (string, C comment ...)
	  ;; determine BEG and END, the region we will narrow to.  If we're in
	  ;; a literal, constrain BEG and END to the limits of this literal.
	  ;;
	  ;; For some of these text types, particularly a block comment, we
	  ;; may need to massage whitespace near literal delimiters, so that
	  ;; these don't get filled inappropriately.
	  (cond

	   ((eq c-lit-type 'c++)	; Line comment.
	    (save-excursion
	      ;; Limit to the comment or paragraph end, whichever
	      ;; comes first.
	      (set-marker end (min end (cdr c-lit-limits)))

	      (when (<= beg (car c-lit-limits))
		;; The region includes the comment starter, so we must
		;; check it.
		(goto-char (car c-lit-limits))
		(back-to-indentation)
		(if (eq (point) (car c-lit-limits))
		    ;; Include the first line in the region.
		    (setq beg (c-point 'bol))
		  ;; The first line contains code before the
		  ;; comment.  We must fake a line that doesn't.
		  (setq tmp-pre t))))

	    (setq apply-outside-literal t))

	   ((eq c-lit-type 'c)		; Block comment.
	    (when
		(or (> end (cdr c-lit-limits))
		    (and (= end (cdr c-lit-limits))
			 (eq (char-before end) ?/)
			 (eq (char-before (1- end)) ?*)
			 ;; disallow "/*/"
			 (> (- (cdr c-lit-limits) (car c-lit-limits)) 3)))
	      ;; There is a comment ender, and the region includes it.  If
	      ;; it's on its own line, it stays on its own line.  If it's got
	      ;; company on the line, it keeps (at least one word of) it.
	      ;; "=====*/" counts as a comment ender here, but "===== */"
	      ;; doesn't and "foo*/" doesn't.
	      (unless
		  (save-excursion
		    (goto-char (cdr c-lit-limits))
		    (beginning-of-line)
		    ;; The following conjunct was added to avoid an
		    ;; "Invalid search bound (wrong side of point)"
		    ;; error in the subsequent re-search.  Maybe
		    ;; another fix would be needed (2007-12-08).
;		    (or (<= (- (cdr c-lit-limits) 2) (point))
; 2010-10-17  Construct removed.
;		    (or (< (- (cdr c-lit-limits) 2) (point))
		    (and
		     (search-forward-regexp
		      (concat "\\=[ \t]*\\(" c-current-comment-prefix "\\)")
		      (- (cdr c-lit-limits) 2) t)
		     (not (search-forward-regexp
			   "\\(\\s \\|\\sw\\)"
			   (- (cdr c-lit-limits) 2) 'limit))
		     ;; The comment ender IS on its own line.  Exclude this
		     ;; line from the filling.
		     (set-marker end (c-point 'bol))));)

		;; The comment ender is hanging.  Replace all space between it
		;; and the last word either by one or two 'x's (when
		;; FILL-PARAGRAPH is non-nil), or a row of x's the same width
		;; as the whitespace (when auto filling), and include it in
		;; the region.  We'll change them back to whitespace
		;; afterwards.  The effect of this is to glue the comment
		;; ender to the last word in the comment during filling.
		(let* ((ender-start (save-excursion
				      (goto-char (cdr c-lit-limits))
				      (skip-syntax-backward "^w ")
				      (point)))
		       (ender-column (save-excursion
				       (goto-char ender-start)
				       (current-column)))
		       (point-rel (- ender-start here))
		       (sentence-ends-comment
			(save-excursion
			  (goto-char ender-start)
			  (and (search-backward-regexp
				(c-sentence-end) (c-point 'bol) t)
			       (goto-char (match-end 0))
			  (looking-at "[ \t]*")
			  (= (match-end 0) ender-start))))
		       spaces)

		  (save-excursion
		    ;; Insert a CR after the "*/", adjust END
		    (goto-char (cdr c-lit-limits))
		    (setq tmp-post (point-marker))
		    (insert ?\n)
		    (set-marker end (point))

		    (forward-line -1)	; last line of the comment
		    (if (and (looking-at (concat "[ \t]*\\(\\("
						 c-current-comment-prefix
						 "\\)[ \t]*\\)"))
			     (eq ender-start (match-end 0)))
			;; The comment ender is prefixed by nothing but a
			;; comment line prefix.  IS THIS POSSIBLE?  (ACM,
			;; 2006/4/28).  Remove it along with surrounding ws.
			(setq spaces (- (match-end 1) (match-end 2)))
		      (goto-char ender-start))
		    (skip-chars-backward " \t\r\n") ; Surely this can be
					; " \t"? "*/" is NOT alone on the line (ACM, 2005/8/18)

		    ;; What's being tested here?  2006/4/20.  FIXME!!!
		    (if (/= (point) ender-start)
			(progn
			  (if (<= here (point))
			      ;; Don't adjust point below if it's
			      ;; before the string we replace.
			      (setq point-rel -1))
			  ;; Keep one or two spaces between the
			  ;; text and the ender, depending on how
			  ;; many there are now.
			  (unless spaces
			    (setq spaces (- ender-column (current-column))))
			  (setq auto-fill-spaces (c-delete-and-extract-region
						  (point) ender-start))
			  ;; paragraph filling condenses multiple spaces to
			  ;; single or double spaces.  auto-fill doesn't.
			  (if fill-paragraph
			      (setq spaces
				    (max
				     (min spaces
					  (if (and sentence-ends-comment
						   sentence-end-double-space)
					      2 1))
				     1)))
			  ;; Insert the filler first to keep marks right.
			  (insert-char ?x spaces t)
			  (setq hang-ender-stuck spaces)
			  (setq point-rel
				(and (>= point-rel 0)
				     (- (point) (min point-rel spaces)))))
		      (setq point-rel nil)))

		  (if point-rel
		      ;; Point was in the middle of the string we
		      ;; replaced above, so put it back in the same
		      ;; relative position, counting from the end.
		      (goto-char point-rel)))
		))

	    (when (<= beg (car c-lit-limits))
	      ;; The region includes the comment starter.
	      (save-excursion
		(goto-char (car c-lit-limits))
		(if (looking-at (concat "\\(" comment-start-skip "\\)$"))
		    ;; Begin with the next line.
		    (setq beg (c-point 'bonl))
		  ;; Fake the fill prefix in the first line.
		  (setq tmp-pre t))))

	    (setq apply-outside-literal t))

	   ((eq c-lit-type 'string)	; String.
	    (save-excursion
	      (when (>= end (cdr c-lit-limits))
		(goto-char (1- (cdr c-lit-limits)))
		(setq tmp-post (point-marker))
		(insert ?\n)
		(set-marker end (point)))
	      (when (<= beg (car c-lit-limits))
		(goto-char (1+ (car c-lit-limits)))
		(setq beg (if (looking-at "\\\\$")
			      ;; Leave the start line if it's
			      ;; nothing but an escaped newline.
			      (1+ (match-end 0))
			    (point)))))
	    (setq apply-outside-literal t))

	   ((eq c-lit-type 'pound)	; Macro
	    ;; Narrow to the macro limits if they are nearer than the
	    ;; paragraph limits.  Don't know if this is necessary but
	    ;; do it for completeness sake (doing auto filling at all
	    ;; inside macros is bogus to begin with since the line
	    ;; continuation backslashes aren't handled).
	    (save-excursion
	      (c-save-buffer-state ()
		(c-beginning-of-macro)
		(beginning-of-line)
		(if (> (point) beg)
		    (setq beg (point)))
		(c-end-of-macro)
		(forward-line)
		(if (< (point) end)
		    (set-marker end (point))))))

	   (t				; Other code.
	    ;; Try to avoid comments and macros in the paragraph to
	    ;; avoid that the adaptive fill mode gets the prefix from
	    ;; them.
	    (c-save-buffer-state nil
	      (save-excursion
		(goto-char beg)
		(c-forward-syntactic-ws end)
		(beginning-of-line)
		(setq beg (point))
		(goto-char end)
		(c-backward-syntactic-ws beg)
		(forward-line)
		(set-marker end (point))))))

	  (when tmp-pre
	    ;; Temporarily insert the fill prefix after the comment
	    ;; starter so that the first line looks like any other
	    ;; comment line in the narrowed region.
	    (setq fill (c-save-buffer-state nil
			 (c-guess-fill-prefix c-lit-limits c-lit-type)))
	    (unless (string-match (concat "\\`[ \t]*\\("
					  c-current-comment-prefix
					  "\\)[ \t]*\\'")
				  (car fill))
	      ;; Oops, the prefix doesn't match the comment prefix
	      ;; regexp.  This could produce very confusing
	      ;; results with adaptive fill packages together with
	      ;; the insert prefix magic below, since the prefix
	      ;; often doesn't appear at all.  So let's warn about
	      ;; it.
	      (message "\
Warning: Regexp from `c-comment-prefix-regexp' doesn't match the comment prefix %S"
		       (car fill)))
	    ;; Find the right spot on the line, break it, insert
	    ;; the fill prefix and make sure we're back in the
	    ;; same column by temporarily prefixing the first word
	    ;; with a number of 'x'.
	    (save-excursion
	      (goto-char (car c-lit-limits))
	      (if (looking-at (if (eq c-lit-type 'c++)
				  c-current-comment-prefix
				comment-start-skip))
		  (goto-char (match-end 0))
		(forward-char 2)
		(skip-chars-forward " \t"))
	      (while (and (< (current-column) (cdr fill))
			  (not (eolp)))
		(forward-char 1))
	      (let ((col (current-column)))
		(setq beg (1+ (point))
		      tmp-pre (list (point)))
		(unwind-protect
		    (progn
		      (insert-and-inherit "\n" (car fill))
		      (insert-char ?x (- col (current-column)) t))
		  (setcdr tmp-pre (point))))))

	  (when apply-outside-literal
	    ;; `apply-outside-literal' is always set to t here if
	    ;; we're inside a literal.

	    (let ((fill-prefix
		   (or fill-prefix
		       ;; Kludge: If the function that adapts the fill prefix
		       ;; doesn't produce the required comment starter for
		       ;; line comments, then force it by setting fill-prefix.
		       (when (and (eq c-lit-type 'c++)
				  ;; Kludge the kludge: filladapt-mode doesn't
				  ;; have this problem, but it currently
				  ;; doesn't override fill-context-prefix
				  ;; (version 2.12).
				  (not (and (boundp 'filladapt-mode)
					    filladapt-mode))
				  (not (string-match
					"\\`[ \t]*//"
					(or (fill-context-prefix beg end)
					    ""))))
			 (c-save-buffer-state nil
			   (car (or fill (c-guess-fill-prefix
					  c-lit-limits c-lit-type)))))))

		  ;; Save the relative position of point if it's outside the
		  ;; region we're going to narrow.  Want to restore it in that
		  ;; case, but otherwise it should be moved according to the
		  ;; called function.
		  (point-rel (cond ((< (point) beg) (- (point) beg))
				   ((> (point) end) (- (point) end)))))

	      ;; Preparations finally done!  Now we can call the
	      ;; actual function.
	      (prog1
		  (save-restriction
		    (narrow-to-region beg end)
		    (apply fun args))
		(if point-rel
		    ;; Restore point if it was outside the region.
		    (if (< point-rel 0)
			(goto-char (+ beg point-rel))
		      (goto-char (+ end point-rel))))))))

      (when (consp tmp-pre)
	(delete-region (car tmp-pre) (cdr tmp-pre)))

      (when tmp-post
	(save-excursion
	  (goto-char tmp-post)
	  (delete-char 1))
	(when hang-ender-stuck
	  ;; Preserve point even if it's in the middle of the string
	  ;; we replace; save-excursion doesn't work in that case.
	  (setq here (point))
	  (goto-char tmp-post)
	  (skip-syntax-backward "^w ")
	  (forward-char (- hang-ender-stuck))
	  (if (or fill-paragraph (not auto-fill-spaces))
	      (insert-char ?\  hang-ender-stuck t)
	    (insert auto-fill-spaces))
	  (delete-char hang-ender-stuck)
	  (goto-char here))
	(set-marker tmp-post nil))

      (set-marker end nil))))

(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment, fill the
comment or the paragraph of it that point is in, preserving the
comment indentation or line-starting decorations (see the
`c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
details).

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

char description[] = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let ((fill-paragraph-function
	 ;; Avoid infinite recursion.
	 (if (not (eq fill-paragraph-function 'c-fill-paragraph))
	     fill-paragraph-function)))
    (c-mask-paragraph t nil 'fill-paragraph arg))
  ;; Always return t.  This has the effect that if filling isn't done
  ;; above, it isn't done at all, and it's therefore effectively
  ;; disabled in normal code.
  t)

(defun c-do-auto-fill ()
  ;; Do automatic filling if not inside a context where it should be
  ;; ignored.
  (let ((c-auto-fill-prefix
	 ;; The decision whether the line should be broken is actually
	 ;; done in c-indent-new-comment-line, which do-auto-fill
	 ;; calls to break lines.  We just set this special variable
	 ;; so that we'll know when we're called from there.  It's
	 ;; also used to detect whether fill-prefix is user set or
	 ;; generated automatically by do-auto-fill.
	 fill-prefix))
    (c-mask-paragraph nil t 'do-auto-fill)))

(defun c-indent-new-comment-line (&optional soft allow-auto-fill)
  "Break line at point and indent, continuing comment or macro if within one.
If inside a comment and `comment-multi-line' is non-nil, the
indentation and line prefix are preserved (see the
`c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
details).  If inside a single line comment and `comment-multi-line' is
nil, a new comment of the same type is started on the next line and
indented as appropriate for comments.  If inside a macro, a line
continuation backslash is inserted and aligned as appropriate, and the
new line is indented according to `c-syntactic-indentation'.

If a fill prefix is specified, it overrides all the above."
  ;; allow-auto-fill is used from c-context-line-break to allow auto
  ;; filling to break the line more than once.  Since this function is
  ;; used from auto-fill itself, that's normally disabled to avoid
  ;; unnecessary recursion.
  (interactive)
  (let ((fill-prefix fill-prefix)
	(do-line-break
	 (lambda ()
	   (delete-horizontal-space)
	   (if soft
	       (insert-and-inherit ?\n)
	     (newline (if allow-auto-fill nil 1)))))
	;; Already know the literal type and limits when called from
	;; c-context-line-break.
	(c-lit-limits c-lit-limits)
	(c-lit-type c-lit-type)
	(c-macro-start c-macro-start))

    (c-save-buffer-state ()
      (when (not (eq c-auto-fill-prefix t))
	;; Called from do-auto-fill.
	(unless c-lit-limits
	  (setq c-lit-limits (c-literal-limits nil nil t)))
	(unless c-lit-type
	  (setq c-lit-type (c-literal-type c-lit-limits)))
	(if (memq (cond ((c-query-and-set-macro-start) 'cpp)
			((null c-lit-type) 'code)
			(t c-lit-type))
		  c-ignore-auto-fill)
	    (setq fill-prefix t)	; Used as flag in the cond.
	  (if (and (null c-auto-fill-prefix)
		   (eq c-lit-type 'c)
		   (<= (c-point 'bol) (car c-lit-limits)))
	      ;; The adaptive fill function has generated a prefix, but
	      ;; we're on the first line in a block comment so it'll be
	      ;; wrong.  Ignore it to guess a better one below.
	      (setq fill-prefix nil)
	    (when (and (eq c-lit-type 'c++)
		       (not (string-match (concat "\\`[ \t]*"
						  c-line-comment-starter)
					  (or fill-prefix ""))))
	      ;; Kludge: If the function that adapted the fill prefix
	      ;; doesn't produce the required comment starter for line
	      ;; comments, then we ignore it.
	      (setq fill-prefix nil)))
	  )))

    (cond ((eq fill-prefix t)
	   ;; A call from do-auto-fill which should be ignored.
	   )
	  (fill-prefix
	   ;; A fill-prefix overrides anything.
	   (funcall do-line-break)
	   (insert-and-inherit fill-prefix))
	  ((c-save-buffer-state ()
	     (unless c-lit-limits
	       (setq c-lit-limits (c-literal-limits)))
	     (unless c-lit-type
	       (setq c-lit-type (c-literal-type c-lit-limits)))
	     (memq c-lit-type '(c c++)))
	   ;; Some sort of comment.
	   (if (or comment-multi-line
		   (save-excursion
		     (goto-char (car c-lit-limits))
		     (end-of-line)
		     (< (point) (cdr c-lit-limits))))
	       ;; Inside a comment that should be continued.
	       (let ((fill (c-save-buffer-state nil
			     (c-guess-fill-prefix
			      (setq c-lit-limits
				    (c-collect-line-comments c-lit-limits))
			      c-lit-type)))
		     (pos (point))
		     (start-col (current-column))
		     (comment-text-end
		      (or (and (eq c-lit-type 'c)
			       (save-excursion
				 (goto-char (- (cdr c-lit-limits) 2))
				 (if (looking-at "\\*/") (point))))
			  (cdr c-lit-limits))))
		 ;; Skip forward past the fill prefix in case
		 ;; we're standing in it.
		 ;;
		 ;; FIXME: This doesn't work well in cases like
		 ;;
		 ;; /* Bla bla bla bla bla
		 ;;         bla bla
		 ;;
		 ;; If point is on the 'B' then the line will be
		 ;; broken after "Bla b".
		 ;;
		 ;; If we have an empty comment, /*   */, the next
		 ;; lot of code pushes point to the */.  We fix
		 ;; this by never allowing point to end up to the
		 ;; right of where it started.
		 (while (and (< (current-column) (cdr fill))
			     (not (eolp)))
		   (forward-char 1))
		 (if (and (> (point) comment-text-end)
			  (> (c-point 'bol) (car c-lit-limits)))
		     (progn
		       ;; The skip takes us out of the (block)
		       ;; comment; insert the fill prefix at bol
		       ;; instead and keep the position.
		       (setq pos (copy-marker pos t))
		       (beginning-of-line)
		       (insert-and-inherit (car fill))
		       (if soft (insert-and-inherit ?\n) (newline 1))
		       (goto-char pos)
		       (set-marker pos nil))
		   ;; Don't break in the middle of a comment starter
		   ;; or ender.
		   (cond ((> (point) comment-text-end)
			  (goto-char comment-text-end))
			 ((< (point) (+ (car c-lit-limits) 2))
			  (goto-char (+ (car c-lit-limits) 2))))
		   (funcall do-line-break)
		   (insert-and-inherit (car fill))
		   (if (> (current-column) start-col)
		       (move-to-column start-col)))) ; can this hit the
					             ; middle of a TAB?
	     ;; Inside a comment that should be broken.
	     (let ((comment-start comment-start)
		   (comment-end comment-end)
		   col)
	       (if (eq c-lit-type 'c)
		   (unless (string-match "[ \t]*/\\*" comment-start)
		     (setq comment-start "/* " comment-end " */"))
		 (unless (string-match "[ \t]*//" comment-start)
		   (setq comment-start "// " comment-end "")))
	       (setq col (save-excursion
			   (back-to-indentation)
			   (current-column)))
	       (funcall do-line-break)
	       (when (and comment-end (not (equal comment-end "")))
		 (forward-char -1)
		 (insert-and-inherit comment-end)
		 (forward-char 1))
	       ;; c-comment-indent may look at the current
	       ;; indentation, so let's start out with the same
	       ;; indentation as the previous one.
	       (indent-to col)
	       (insert-and-inherit comment-start)
	       (indent-for-comment))))
	  ((c-query-and-set-macro-start)
	   ;; In a macro.
	   (unless (looking-at "[ \t]*\\\\$")
	     ;; Do not clobber the alignment of the line continuation
	     ;; slash; c-backslash-region might look at it.
	     (delete-horizontal-space))
	   ;; Got an asymmetry here: In normal code this command
	   ;; doesn't indent the next line syntactically, and otoh a
	   ;; normal syntactically indenting newline doesn't continue
	   ;; the macro.
	   (c-newline-and-indent (if allow-auto-fill nil 1)))
	  (t
	   ;; Somewhere else in the code.
	   (let ((col (save-excursion
			(beginning-of-line)
			(while (and (looking-at "[ \t]*\\\\?$")
				    (= (forward-line -1) 0)))
			(current-indentation))))
	     (funcall do-line-break)
	     (indent-to col))))))

(defalias 'c-comment-line-break-function 'c-indent-new-comment-line)
(make-obsolete 'c-comment-line-break-function 'c-indent-new-comment-line "21.1")

;; advice for indent-new-comment-line for older Emacsen
(unless (boundp 'comment-line-break-function)
  (defvar c-inside-line-break-advice nil)
  (defadvice indent-new-comment-line (around c-line-break-advice
					     activate preactivate)
    "Call `c-indent-new-comment-line' if in CC Mode."
    (if (or c-inside-line-break-advice
	    (not c-buffer-is-cc-mode))
	ad-do-it
      (let ((c-inside-line-break-advice t))
	(c-indent-new-comment-line (ad-get-arg 0))))))

(defun c-context-line-break ()
  "Do a line break suitable to the context.

When point is outside a comment or macro, insert a newline and indent
according to the syntactic context, unless `c-syntactic-indentation'
is nil, in which case the new line is indented as the previous
non-empty line instead.

When point is inside the content of a preprocessor directive, a line
continuation backslash is inserted before the line break and aligned
appropriately.  The end of the cpp directive doesn't count as inside
it.

When point is inside a comment, continue it with the appropriate
comment prefix (see the `c-comment-prefix-regexp' and
`c-block-comment-prefix' variables for details).  The end of a
C++-style line comment doesn't count as inside it.

When point is inside a string, only insert a backslash when it is also
inside a preprocessor directive."

  (interactive "*")
  (let* (c-lit-limits c-lit-type
	 (c-macro-start c-macro-start)
	 case-fold-search)

    (c-save-buffer-state ()
      (setq c-lit-limits (c-literal-limits nil nil t)
	    c-lit-type (c-literal-type c-lit-limits))
      (when (eq c-lit-type 'c++)
	(setq c-lit-limits (c-collect-line-comments c-lit-limits)))
      (c-query-and-set-macro-start))

    (cond
     ((or (eq c-lit-type 'c)
	  (and (eq c-lit-type 'c++) ; C++ comment, but not at the very end of it.
	       (< (save-excursion
		    (skip-chars-forward " \t")
		    (point))
		  (1- (cdr c-lit-limits))))
	  (and (numberp c-macro-start)	; Macro, but not at the very end of
					; it, not in a string, and not in the
					; cpp keyword.
	       (not (eq c-lit-type 'string))
	       (or (not (looking-at "\\s *$"))
		   (eq (char-before) ?\\))
	       (<= (save-excursion
		     (goto-char c-macro-start)
		     (if (looking-at c-opt-cpp-start)
			 (goto-char (match-end 0)))
		     (point))
		   (point))))
      (let ((comment-multi-line t)
	    (fill-prefix nil))
	(c-indent-new-comment-line nil t)))

     ((eq c-lit-type 'string)
      (if (and (numberp c-macro-start)
	       (not (eq (char-before) ?\\)))
	  (insert ?\\))
      (newline))

     (t (delete-horizontal-space)
	(newline)
      ;; c-indent-line may look at the current indentation, so let's
      ;; start out with the same indentation as the previous line.
	(let ((col (save-excursion
		     (backward-char)
		     (forward-line 0)
		     (while (and (looking-at "[ \t]*\\\\?$")
				 (= (forward-line -1) 0)))
		     (current-indentation))))
	  (indent-to col))
     (indent-according-to-mode)))))

(defun c-context-open-line ()
  "Insert a line break suitable to the context and leave point before it.
This is the `c-context-line-break' equivalent to `open-line', which is
normally bound to C-o.  See `c-context-line-break' for the details."
  (interactive "*")
  (let ((here (point)))
    (unwind-protect
	(progn
	  ;; Temporarily insert a non-whitespace char to keep any
	  ;; preceding whitespace intact.
	  (insert ?x)
	  (c-context-line-break))
      (goto-char here)
      (delete-char 1))))


(cc-provide 'cc-cmds)

;;; cc-cmds.el ends here
