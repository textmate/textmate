;;; cc-defs.el --- compile time definitions for CC Mode

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

;; This file contains macros, defsubsts, and various other things that
;; must be loaded early both during compilation and at runtime.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(eval-when-compile (require 'cl)) ; was (cc-external-require 'cl).  ACM 2005/11/29.
(cc-external-require 'regexp-opt)

;; Silence the compiler.
(cc-bytecomp-defvar c-enable-xemacs-performance-kludge-p) ; In cc-vars.el
(cc-bytecomp-defun buffer-syntactic-context-depth) ; XEmacs
(cc-bytecomp-defun region-active-p)	; XEmacs
(cc-bytecomp-defvar zmacs-region-stays)	; XEmacs
(cc-bytecomp-defvar zmacs-regions)	; XEmacs
(cc-bytecomp-defvar mark-active)	; Emacs
(cc-bytecomp-defvar deactivate-mark)	; Emacs
(cc-bytecomp-defvar inhibit-point-motion-hooks) ; Emacs
(cc-bytecomp-defvar parse-sexp-lookup-properties) ; Emacs
(cc-bytecomp-defvar text-property-default-nonsticky) ; Emacs 21
(cc-bytecomp-defvar lookup-syntax-properties) ; XEmacs
(cc-bytecomp-defun string-to-syntax)	; Emacs 21


;; cc-fix.el contains compatibility macros that should be used if
;; needed.
(eval-and-compile
  (if (or (/= (regexp-opt-depth "\\(\\(\\)\\)") 2)
	  (not (fboundp 'push)))
      (cc-load "cc-fix")))

; (eval-after-load "font-lock"  ; 2006-07-09.  font-lock is now preloaded
;   '
(if (and (featurep 'xemacs)	; There is now (2005/12) code in GNU Emacs CVS
				; to make the call to f-l-c-k throw an error.
	 (not (featurep 'cc-fix)) ; only load the file once.
	 (let (font-lock-keywords)
	   (font-lock-compile-keywords '("\\<\\>"))
	   font-lock-keywords))     ; did the previous call foul this up?
    (load "cc-fix")) ;)

;; The above takes care of the delayed loading, but this is necessary
;; to ensure correct byte compilation.
(eval-when-compile
  (if (and (featurep 'xemacs)
	   (not (featurep 'cc-fix))
	   (progn
	     (require 'font-lock)
	     (let (font-lock-keywords)
	       (font-lock-compile-keywords '("\\<\\>"))
	       font-lock-keywords)))
      (cc-load "cc-fix")))


;;; Variables also used at compile time.

(defconst c-version "5.32.3"
  "CC Mode version number.")

(defconst c-version-sym (intern c-version))
;; A little more compact and faster in comparisons.

(defvar c-buffer-is-cc-mode nil
  "Non-nil for all buffers with a major mode derived from CC Mode.
Otherwise, this variable is nil.  I.e. this variable is non-nil for
`c-mode', `c++-mode', `objc-mode', `java-mode', `idl-mode',
`pike-mode', `awk-mode', and any other non-CC Mode mode that calls
`c-initialize-cc-mode'.  The value is the mode symbol itself
\(i.e. `c-mode' etc) of the original CC Mode mode, or just t if it's
not known.")
(make-variable-buffer-local 'c-buffer-is-cc-mode)

;; Have to make `c-buffer-is-cc-mode' permanently local so that it
;; survives the initialization of the derived mode.
(put 'c-buffer-is-cc-mode 'permanent-local t)


;; The following is used below during compilation.
(eval-and-compile
  (defvar c-inside-eval-when-compile nil)

  (defmacro cc-eval-when-compile (&rest body)
    "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant.

This variant works around bugs in `eval-when-compile' in various
\(X)Emacs versions.  See cc-defs.el for details."

    (if c-inside-eval-when-compile
	;; XEmacs 21.4.6 has a bug in `eval-when-compile' in that it
	;; evaluates its body at macro expansion time if it's nested
	;; inside another `eval-when-compile'.  So we use a dynamically
	;; bound variable to avoid nesting them.
	`(progn ,@body)

      `(eval-when-compile
	 ;; In all (X)Emacsen so far, `eval-when-compile' byte compiles
	 ;; its contents before evaluating it.  That can cause forms to
	 ;; be compiled in situations they aren't intended to be
	 ;; compiled.
	 ;;
	 ;; Example: It's not possible to defsubst a primitive, e.g. the
	 ;; following will produce an error (in any emacs flavor), since
	 ;; `nthcdr' is a primitive function that's handled specially by
	 ;; the byte compiler and thus can't be redefined:
	 ;;
	 ;;     (defsubst nthcdr (val) val)
	 ;;
	 ;; `defsubst', like `defmacro', needs to be evaluated at
	 ;; compile time, so this will produce an error during byte
	 ;; compilation.
	 ;;
	 ;; CC Mode occasionally needs to do things like this for
	 ;; cross-emacs compatibility.  It therefore uses the following
	 ;; to conditionally do a `defsubst':
	 ;;
	 ;;     (eval-when-compile
	 ;;       (if (not (fboundp 'foo))
	 ;;           (defsubst foo ...)))
	 ;;
	 ;; But `eval-when-compile' byte compiles its contents and
	 ;; _then_ evaluates it (in all current emacs versions, up to
	 ;; and including Emacs 20.6 and XEmacs 21.1 as of this
	 ;; writing).  So this will still produce an error, since the
	 ;; byte compiler will get to the defsubst anyway.  That's
	 ;; arguably a bug because the point with `eval-when-compile' is
	 ;; that it should evaluate rather than compile its contents.
	 ;;
	 ;; We get around it by expanding the body to a quoted
	 ;; constant that we eval.  That otoh introduce a problem in
	 ;; that a returned lambda expression doesn't get byte
	 ;; compiled (even if `function' is used).
	 (eval '(let ((c-inside-eval-when-compile t)) ,@body)))))

  (put 'cc-eval-when-compile 'lisp-indent-hook 0))


;;; Macros.

(defmacro c-point (position &optional point)
  "Return the value of certain commonly referenced POSITIONs relative to POINT.
The current point is used if POINT isn't specified.  POSITION can be
one of the following symbols:

`bol'   -- beginning of line
`eol'   -- end of line
`bod'   -- beginning of defun
`eod'   -- end of defun
`boi'   -- beginning of indentation
`ionl'  -- indentation of next line
`iopl'  -- indentation of previous line
`bonl'  -- beginning of next line
`eonl'  -- end of next line
`bopl'  -- beginning of previous line
`eopl'  -- end of previous line
`bosws' -- beginning of syntactic whitespace
`eosws' -- end of syntactic whitespace

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify the point or the mark."

  (if (eq (car-safe position) 'quote)
      (let ((position (eval position)))
	(cond

	 ((eq position 'bol)
	  (if (and (cc-bytecomp-fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (point))))

	 ((eq position 'eol)
	  (if (and (cc-bytecomp-fboundp 'line-end-position) (not point))
	      `(line-end-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (end-of-line)
	       (point))))

	 ((eq position 'boi)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (back-to-indentation)
	     (point)))

	 ((eq position 'bod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-beginning-of-defun-1)
	     (point)))

	 ((eq position 'eod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-end-of-defun-1)
	     (point)))

	 ((eq position 'bopl)
	  (if (and (cc-bytecomp-fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line -1)
	       (point))))

	 ((eq position 'bonl)
	  (if (and (cc-bytecomp-fboundp 'line-beginning-position) (not point))
	      `(line-beginning-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (point))))

	 ((eq position 'eopl)
	  (if (and (cc-bytecomp-fboundp 'line-end-position) (not point))
	      `(line-end-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (or (bobp) (backward-char))
	       (point))))

	 ((eq position 'eonl)
	  (if (and (cc-bytecomp-fboundp 'line-end-position) (not point))
	      `(line-end-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (end-of-line)
	       (point))))

	 ((eq position 'iopl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line -1)
	     (back-to-indentation)
	     (point)))

	 ((eq position 'ionl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line 1)
	     (back-to-indentation)
	     (point)))

	 ((eq position 'bosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-backward-syntactic-ws)
	     (point)))

	 ((eq position 'eosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-forward-syntactic-ws)
	     (point)))

	 (t (error "Unknown buffer position requested: %s" position))))

    ;; The bulk of this should perhaps be in a function to avoid large
    ;; expansions, but this case is not used anywhere in CC Mode (and
    ;; probably not anywhere else either) so we only have it to be on
    ;; the safe side.
    (message "Warning: c-point long expansion")
    `(save-excursion
       ,@(if point `((goto-char ,point)))
       (let ((position ,position))
	 (cond
	  ((eq position 'bol)	(beginning-of-line))
	  ((eq position 'eol)	(end-of-line))
	  ((eq position 'boi)	(back-to-indentation))
	  ((eq position 'bod)	(c-beginning-of-defun-1))
	  ((eq position 'eod)	(c-end-of-defun-1))
	  ((eq position 'bopl)	(forward-line -1))
	  ((eq position 'bonl)	(forward-line 1))
	  ((eq position 'eopl)	(progn
				  (beginning-of-line)
				  (or (bobp) (backward-char))))
	  ((eq position 'eonl)	(progn
				  (forward-line 1)
				  (end-of-line)))
	  ((eq position 'iopl)	(progn
				  (forward-line -1)
				  (back-to-indentation)))
	  ((eq position 'ionl)	(progn
				  (forward-line 1)
				(back-to-indentation)))
	  ((eq position 'bosws)	(c-backward-syntactic-ws))
	  ((eq position 'eosws)	(c-forward-syntactic-ws))
	  (t (error "Unknown buffer position requested: %s" position))))
       (point))))

(defmacro c-region-is-active-p ()
  ;; Return t when the region is active.  The determination of region
  ;; activeness is different in both Emacs and XEmacs.
  (if (cc-bytecomp-boundp 'mark-active)
      ;; Emacs.
      'mark-active
    ;; XEmacs.
    '(region-active-p)))

(defmacro c-set-region-active (activate)
  ;; Activate the region if ACTIVE is non-nil, deactivate it
  ;; otherwise.  Covers the differences between Emacs and XEmacs.
  (if (cc-bytecomp-fboundp 'zmacs-activate-region)
      ;; XEmacs.
      `(if ,activate
	   (zmacs-activate-region)
	 (zmacs-deactivate-region))
    ;; Emacs.
    `(setq mark-active ,activate)))

(defmacro c-delete-and-extract-region (start end)
  "Delete the text between START and END and return it."
  (if (cc-bytecomp-fboundp 'delete-and-extract-region)
      ;; Emacs 21.1 and later
      `(delete-and-extract-region ,start ,end)
    ;; XEmacs and Emacs 20.x
    `(prog1
       (buffer-substring ,start ,end)
       (delete-region ,start ,end))))

(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  `(condition-case nil
       (progn ,@body)
     (error nil)))
(put 'c-safe 'lisp-indent-function 0)

(defmacro c-int-to-char (integer)
  ;; In Emacs, a character is an integer.  In XEmacs, a character is a
  ;; type distinct from an integer.  Sometimes we need to convert integers to
  ;; characters.  `c-int-to-char' makes this conversion, if necessary.
  (if (fboundp 'int-to-char)
      `(int-to-char ,integer)
    integer))

(defmacro c-sentence-end ()
  ;; Get the regular expression `sentence-end'.
  (if (cc-bytecomp-fboundp 'sentence-end)
      ;; Emacs 22:
      `(sentence-end)
    ;; Emacs <22 + XEmacs
    `sentence-end))

(defmacro c-default-value-sentence-end ()
  ;; Get the default value of the variable sentence end.
  (if (cc-bytecomp-fboundp 'sentence-end)
      ;; Emacs 22:
      `(let (sentence-end) (sentence-end))
    ;; Emacs <22 + XEmacs
    `(default-value 'sentence-end)))

;; The following is essentially `save-buffer-state' from lazy-lock.el.
;; It ought to be a standard macro.
(defmacro c-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST (in `let*' style) and eval BODY,
then restore the buffer state under the assumption that no significant
modification has been made in BODY.  A change is considered
significant if it affects the buffer text in any way that isn't
completely restored again.  Changes in text properties like `face' or
`syntax-table' are considered insignificant.  This macro allows text
properties to be changed, even in a read-only buffer.

This macro should be placed around all calculations which set
\"insignificant\" text properties in a buffer, even when the buffer is
known to be writable.  That way, these text properties remain set
even if the user undoes the command which set them.

This macro should ALWAYS be placed around \"temporary\" internal buffer
changes \(like adding a newline to calculate a text-property then
deleting it again\), so that the user never sees them on his
`buffer-undo-list'.  See also `c-tentative-buffer-changes'.

However, any user-visible changes to the buffer \(like auto-newlines\)
must not be within a `c-save-buffer-state', since the user then
wouldn't be able to undo them.

The return value is the value of the last form in BODY."
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
	  (inhibit-read-only t) (inhibit-point-motion-hooks t)
	  before-change-functions after-change-functions
	  deactivate-mark
	  buffer-file-name buffer-file-truename ; Prevent primitives checking
						; for file modification
	  ,@varlist)
     (unwind-protect
	 (progn ,@body)
       (and (not modified)
	    (buffer-modified-p)
	    (set-buffer-modified-p nil)))))
(put 'c-save-buffer-state 'lisp-indent-function 1)

(defmacro c-tentative-buffer-changes (&rest body)
  "Eval BODY and optionally restore the buffer contents to the state it
was in before BODY.  Any changes are kept if the last form in BODY
returns non-nil.  Otherwise it's undone using the undo facility, and
various other buffer state that might be affected by the changes is
restored.  That includes the current buffer, point, mark, mark
activation \(similar to `save-excursion'), and the modified state.
The state is also restored if BODY exits nonlocally.

If BODY makes a change that unconditionally is undone then wrap this
macro inside `c-save-buffer-state'.  That way the change can be done
even when the buffer is read-only, and without interference from
various buffer change hooks."
  `(let (-tnt-chng-keep
	 -tnt-chng-state)
     (unwind-protect
	 ;; Insert an undo boundary for use with `undo-more'.  We
	 ;; don't use `undo-boundary' since it doesn't insert one
	 ;; unconditionally.
	 (setq buffer-undo-list (cons nil buffer-undo-list)
	       -tnt-chng-state (c-tnt-chng-record-state)
	       -tnt-chng-keep (progn ,@body))
       (c-tnt-chng-cleanup -tnt-chng-keep -tnt-chng-state))))
(put 'c-tentative-buffer-changes 'lisp-indent-function 0)

(defun c-tnt-chng-record-state ()
  ;; Used internally in `c-tentative-buffer-changes'.
  (vector buffer-undo-list		; 0
	  (current-buffer)		; 1
	  ;; No need to use markers for the point and mark; if the
	  ;; undo got out of synch we're hosed anyway.
	  (point)			; 2
	  (mark t)			; 3
	  (c-region-is-active-p)	; 4
	  (buffer-modified-p)))		; 5

(defun c-tnt-chng-cleanup (keep saved-state)
  ;; Used internally in `c-tentative-buffer-changes'.

  (let ((saved-undo-list (elt saved-state 0)))
    (if (eq buffer-undo-list saved-undo-list)
	;; No change was done after all.
	(setq buffer-undo-list (cdr saved-undo-list))

      (if keep
	  ;; Find and remove the undo boundary.
	  (let ((p buffer-undo-list))
	    (while (not (eq (cdr p) saved-undo-list))
	      (setq p (cdr p)))
	    (setcdr p (cdr saved-undo-list)))

	;; `primitive-undo' will remove the boundary.
	(setq saved-undo-list (cdr saved-undo-list))
	(let ((undo-in-progress t))
	  (while (not (eq (setq buffer-undo-list
				(primitive-undo 1 buffer-undo-list))
			  saved-undo-list))))

	(when (buffer-live-p (elt saved-state 1))
	  (set-buffer (elt saved-state 1))
	  (goto-char (elt saved-state 2))
	  (set-mark (elt saved-state 3))
	  (c-set-region-active (elt saved-state 4))
	  (and (not (elt saved-state 5))
	       (buffer-modified-p)
	       (set-buffer-modified-p nil)))))))

(defmacro c-forward-syntactic-ws (&optional limit)
  "Forward skip over syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.

LIMIT sets an upper limit of the forward movement, if specified.  If
LIMIT or the end of the buffer is reached inside a comment or
preprocessor directive, the point will be left there.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (if limit
      `(save-restriction
	 (narrow-to-region (point-min) (or ,limit (point-max)))
	 (c-forward-sws))
    '(c-forward-sws)))

(defmacro c-backward-syntactic-ws (&optional limit)
  "Backward skip over syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.

LIMIT sets a lower limit of the backward movement, if specified.  If
LIMIT is reached inside a line comment or preprocessor directive then
the point is moved into it past the whitespace at the end.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (if limit
      `(save-restriction
	 (narrow-to-region (or ,limit (point-min)) (point-max))
	 (c-backward-sws))
    '(c-backward-sws)))

(defmacro c-forward-sexp (&optional count)
  "Move forward across COUNT balanced expressions.
A negative COUNT means move backward.  Signal an error if the move
fails for any reason.

This is like `forward-sexp' except that it isn't interactive and does
not do any user friendly adjustments of the point and that it isn't
susceptible to user configurations such as disabling of signals in
certain situations."
  (or count (setq count 1))
  `(goto-char (scan-sexps (point) ,count)))

(defmacro c-backward-sexp (&optional count)
  "See `c-forward-sexp' and reverse directions."
  (or count (setq count 1))
  `(c-forward-sexp ,(if (numberp count) (- count) `(- ,count))))

(defmacro c-safe-scan-lists (from count depth &optional limit)
  "Like `scan-lists' but returns nil instead of signaling errors
for unbalanced parens.

A limit for the search may be given.  FROM is assumed to be on the
right side of it."
  (let ((res (if (featurep 'xemacs)
		 `(scan-lists ,from ,count ,depth nil t)
	       `(c-safe (scan-lists ,from ,count ,depth)))))
    (if limit
	`(save-restriction
	   ,(if (numberp count)
		(if (< count 0)
		    `(narrow-to-region ,limit (point-max))
		  `(narrow-to-region (point-min) ,limit))
	      `(if (< ,count 0)
		   (narrow-to-region ,limit (point-max))
		 (narrow-to-region (point-min) ,limit)))
	   ,res)
      res)))


;; Wrappers for common scan-lists cases, mainly because it's almost
;; impossible to get a feel for how that function works.

(defmacro c-go-list-forward ()
  "Move backward across one balanced group of parentheses.

Return POINT when we succeed, NIL when we fail.  In the latter case, leave
point unmoved."
  `(c-safe (let ((endpos (scan-lists (point) 1 0)))
	     (goto-char endpos)
	     endpos)))

(defmacro c-go-list-backward ()
  "Move backward across one balanced group of parentheses.

Return POINT when we succeed, NIL when we fail.  In the latter case, leave
point unmoved."
  `(c-safe (let ((endpos (scan-lists (point) -1 0)))
	     (goto-char endpos)
	     endpos)))

(defmacro c-up-list-forward (&optional pos limit)
  "Return the first position after the list sexp containing POS,
or nil if no such position exists.  The point is used if POS is left out.

A limit for the search may be given.  The start position is assumed to
be before it."
  `(c-safe-scan-lists ,(or pos `(point)) 1 1 ,limit))

(defmacro c-up-list-backward (&optional pos limit)
  "Return the position of the start of the list sexp containing POS,
or nil if no such position exists.  The point is used if POS is left out.

A limit for the search may be given.  The start position is assumed to
be after it."
  `(c-safe-scan-lists ,(or pos `(point)) -1 1 ,limit))

(defmacro c-down-list-forward (&optional pos limit)
  "Return the first position inside the first list sexp after POS,
or nil if no such position exists.  The point is used if POS is left out.

A limit for the search may be given.  The start position is assumed to
be before it."
  `(c-safe-scan-lists ,(or pos `(point)) 1 -1 ,limit))

(defmacro c-down-list-backward (&optional pos limit)
  "Return the last position inside the last list sexp before POS,
or nil if no such position exists.  The point is used if POS is left out.

A limit for the search may be given.  The start position is assumed to
be after it."
  `(c-safe-scan-lists ,(or pos `(point)) -1 -1 ,limit))

(defmacro c-go-up-list-forward (&optional pos limit)
  "Move the point to the first position after the list sexp containing POS,
or containing the point if POS is left out.  Return t if such a
position exists, otherwise nil is returned and the point isn't moved.

A limit for the search may be given.  The start position is assumed to
be before it."
  (let ((res `(c-safe (goto-char (scan-lists ,(or pos `(point)) 1 1)) t)))
    (if limit
	`(save-restriction
	   (narrow-to-region (point-min) ,limit)
	   ,res)
      res)))

(defmacro c-go-up-list-backward (&optional pos limit)
  "Move the point to the position of the start of the list sexp containing POS,
or containing the point if POS is left out.  Return t if such a
position exists, otherwise nil is returned and the point isn't moved.

A limit for the search may be given.  The start position is assumed to
be after it."
  (let ((res `(c-safe (goto-char (scan-lists ,(or pos `(point)) -1 1)) t)))
    (if limit
	`(save-restriction
	   (narrow-to-region ,limit (point-max))
	   ,res)
      res)))

(defmacro c-go-down-list-forward (&optional pos limit)
  "Move the point to the first position inside the first list sexp after POS,
or before the point if POS is left out.  Return t if such a position
exists, otherwise nil is returned and the point isn't moved.

A limit for the search may be given.  The start position is assumed to
be before it."
  (let ((res `(c-safe (goto-char (scan-lists ,(or pos `(point)) 1 -1)) t)))
    (if limit
	`(save-restriction
	   (narrow-to-region (point-min) ,limit)
	   ,res)
      res)))

(defmacro c-go-down-list-backward (&optional pos limit)
  "Move the point to the last position inside the last list sexp before POS,
or before the point if POS is left out.  Return t if such a position
exists, otherwise nil is returned and the point isn't moved.

A limit for the search may be given.  The start position is assumed to
be after it."
  (let ((res `(c-safe (goto-char (scan-lists ,(or pos `(point)) -1 -1)) t)))
    (if limit
	`(save-restriction
	   (narrow-to-region ,limit (point-max))
	   ,res)
      res)))


(defmacro c-beginning-of-defun-1 ()
  ;; Wrapper around beginning-of-defun.
  ;;
  ;; NOTE: This function should contain the only explicit use of
  ;; beginning-of-defun in CC Mode.  Eventually something better than
  ;; b-o-d will be available and this should be the only place the
  ;; code needs to change.  Everything else should use
  ;; (c-beginning-of-defun-1)
  ;;
  ;; This is really a bit too large to be a macro but that isn't a
  ;; problem as long as it only is used in one place in
  ;; `c-parse-state'.

  `(progn
     (if (and ,(cc-bytecomp-fboundp 'buffer-syntactic-context-depth)
	      c-enable-xemacs-performance-kludge-p)
	 ,(when (cc-bytecomp-fboundp 'buffer-syntactic-context-depth)
	    ;; XEmacs only.  This can improve the performance of
	    ;; c-parse-state to between 3 and 60 times faster when
	    ;; braces are hung.  It can also degrade performance by
	    ;; about as much when braces are not hung.
	    '(let (beginning-of-defun-function end-of-defun-function
					       pos)
	       (while (not pos)
		 (save-restriction
		   (widen)
		   (setq pos (c-safe-scan-lists
			      (point) -1 (buffer-syntactic-context-depth))))
		 (cond
		  ((bobp) (setq pos (point-min)))
		  ((not pos)
		   (let ((distance (skip-chars-backward "^{")))
		     ;; unbalanced parenthesis, while invalid C code,
		     ;; shouldn't cause an infloop!  See unbal.c
		     (when (zerop distance)
		       ;; Punt!
		       (beginning-of-defun)
		       (setq pos (point)))))
		  ((= pos 0))
		  ((not (eq (char-after pos) ?{))
		   (goto-char pos)
		   (setq pos nil))
		  ))
	       (goto-char pos)))
       ;; Emacs, which doesn't have buffer-syntactic-context-depth
       (let (beginning-of-defun-function end-of-defun-function)
	 (beginning-of-defun)))
     ;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at the
     ;; open brace.
     (and defun-prompt-regexp
	  (looking-at defun-prompt-regexp)
	  (goto-char (match-end 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V i r t u a l   S e m i c o l o n s
;;
;; In most CC Mode languages, statements are terminated explicitly by
;; semicolons or closing braces.  In some of the CC modes (currently AWK Mode
;; and certain user-specified #define macros in C, C++, etc. (November 2008)),
;; statements are (or can be) terminated by EOLs.  Such a statement is said to
;; be terminated by a "virtual semicolon" (VS).  A statement terminated by an
;; actual semicolon or brace is never considered to have a VS.
;;
;; The indentation engine (or whatever) tests for a VS at a specific position
;; by invoking the macro `c-at-vsemi-p', which in its turn calls the mode
;; specific function (if any) which is the value of the language variable
;; `c-at-vsemi-p-fn'.  This function should only use "low-level" features of
;; CC Mode, i.e. features which won't trigger infinite recursion.  ;-) The
;; actual details of what constitutes a VS in a language are thus encapsulated
;; in code specific to that language (e.g. cc-awk.el).  `c-at-vsemi-p' returns
;; non-nil if point (or the optional parameter POS) is at a VS, nil otherwise.
;;
;; The language specific function might well do extensive analysis of the
;; source text, and may use a caching scheme to speed up repeated calls.
;;
;; The "virtual semicolon" lies just after the last non-ws token on the line.
;; Like POINT, it is considered to lie between two characters.  For example,
;; at the place shown in the following AWK source line:
;;
;;          kbyte = 1024             # 1000 if you're not picky
;;                      ^
;;                      |
;;              Virtual Semicolon
;;
;; In addition to `c-at-vsemi-p-fn', a mode may need to supply a function for
;; `c-vsemi-status-unknown-p-fn'.  The macro `c-vsemi-status-unknown-p' is a
;; rather recondite kludge.  It exists because the function
;; `c-beginning-of-statement-1' sometimes tests for VSs as an optimization,
;; but `c-at-vsemi-p' might well need to call `c-beginning-of-statement-1' in
;; its calculations, thus potentially leading to infinite recursion.
;;
;; The macro `c-vsemi-status-unknown-p' resolves this problem; it may return
;; non-nil at any time; returning nil is a guarantee that an immediate
;; invocation of `c-at-vsemi-p' at point will NOT call
;; `c-beginning-of-statement-1'.  `c-vsemi-status-unknown-p' may not itself
;; call `c-beginning-of-statement-1'.
;;
;; The macro `c-vsemi-status-unknown-p' will typically check the caching
;; scheme used by the `c-at-vsemi-p-fn', hence the name - the status is
;; "unknown" if there is no cache entry current for the line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro c-at-vsemi-p (&optional pos)
  ;; Is there a virtual semicolon (not a real one or a }) at POS (defaults to
  ;; point)?  Always returns nil for languages which don't have Virtual
  ;; semicolons.
  ;; This macro might do hidden buffer changes.
  `(if c-at-vsemi-p-fn
       (funcall c-at-vsemi-p-fn ,@(if pos `(,pos)))))

(defmacro c-vsemi-status-unknown-p ()
  ;; Return NIL only if it can be guaranteed that an immediate
  ;; (c-at-vsemi-p) will NOT call c-beginning-of-statement-1.  Otherwise,
  ;; return non-nil.  (See comments above).  The function invoked by this
  ;; macro MUST NOT UNDER ANY CIRCUMSTANCES itself call
  ;; c-beginning-of-statement-1.
  ;; Languages which don't have EOL terminated statements always return NIL
  ;; (they _know_ there's no vsemi ;-).
  `(if c-vsemi-status-unknown-p-fn (funcall c-vsemi-status-unknown-p-fn)))


(defmacro c-benign-error (format &rest args)
  ;; Formats an error message for the echo area and dings, i.e. like
  ;; `error' but doesn't abort.
  `(progn
     (message ,format ,@args)
     (ding)))

(defmacro c-with-syntax-table (table &rest code)
  ;; Temporarily switches to the specified syntax table in a failsafe
  ;; way to execute code.
  `(let ((c-with-syntax-table-orig-table (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table ,table)
	   ,@code)
       (set-syntax-table c-with-syntax-table-orig-table))))
(put 'c-with-syntax-table 'lisp-indent-function 1)

(defmacro c-skip-ws-forward (&optional limit)
  "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace and line
continuations."
  (if limit
      `(let ((limit (or ,limit (point-max))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-forward " \t\n\r\f\v" limit)
		  (when (and (eq (char-after) ?\\)
			     (< (point) limit))
		    (forward-char)
		    (or (eolp)
			(progn (backward-char) nil))))))
    '(while (progn
	      (skip-chars-forward " \t\n\r\f\v")
	      (when (eq (char-after) ?\\)
		(forward-char)
		(or (eolp)
		    (progn (backward-char) nil)))))))

(defmacro c-skip-ws-backward (&optional limit)
  "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace and line
continuations."
  (if limit
      `(let ((limit (or ,limit (point-min))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-backward " \t\n\r\f\v" limit)
		  (and (eolp)
		       (eq (char-before) ?\\)
		       (> (point) limit)))
	   (backward-char)))
    '(while (progn
	      (skip-chars-backward " \t\n\r\f\v")
	      (and (eolp)
		   (eq (char-before) ?\\)))
       (backward-char))))

(eval-and-compile
  (defvar c-langs-are-parametric nil))

(defmacro c-major-mode-is (mode)
  "Return non-nil if the current CC Mode major mode is MODE.
MODE is either a mode symbol or a list of mode symbols."

  (if c-langs-are-parametric
      ;; Inside a `c-lang-defconst'.
      `(c-lang-major-mode-is ,mode)

    (if (eq (car-safe mode) 'quote)
	(let ((mode (eval mode)))
	  (if (listp mode)
	      `(memq c-buffer-is-cc-mode ',mode)
	    `(eq c-buffer-is-cc-mode ',mode)))

      `(let ((mode ,mode))
	 (if (listp mode)
	     (memq c-buffer-is-cc-mode mode)
	   (eq c-buffer-is-cc-mode mode))))))


;; Macros/functions to handle so-called "char properties", which are
;; properties set on a single character and that never spread to any
;; other characters.

(eval-and-compile
  ;; Constant used at compile time to decide whether or not to use
  ;; XEmacs extents.  Check all the extent functions we'll use since
  ;; some packages might add compatibility aliases for some of them in
  ;; Emacs.
  (defconst c-use-extents (and (cc-bytecomp-fboundp 'extent-at)
			       (cc-bytecomp-fboundp 'set-extent-property)
			       (cc-bytecomp-fboundp 'set-extent-properties)
			       (cc-bytecomp-fboundp 'make-extent)
			       (cc-bytecomp-fboundp 'extent-property)
			       (cc-bytecomp-fboundp 'delete-extent)
			       (cc-bytecomp-fboundp 'map-extents))))

;; `c-put-char-property' is complex enough in XEmacs and Emacs < 21 to
;; make it a function.
(defalias 'c-put-char-property-fun
  (cc-eval-when-compile
    (cond (c-use-extents
	   ;; XEmacs.
	   (byte-compile
	    (lambda (pos property value)
	      (let ((ext (extent-at pos nil property)))
		(if ext
		    (set-extent-property ext property value)
		  (set-extent-properties (make-extent pos (1+ pos))
					 (cons property
					       (cons value
						     '(start-open t
						       end-open t)))))))))

	  ((not (cc-bytecomp-boundp 'text-property-default-nonsticky))
	   ;; In Emacs < 21 we have to mess with the `rear-nonsticky' property.
	   (byte-compile
	    (lambda (pos property value)
	      (put-text-property pos (1+ pos) property value)
	      (let ((prop (get-text-property pos 'rear-nonsticky)))
		(or (memq property prop)
		    (put-text-property pos (1+ pos)
				       'rear-nonsticky
				       (cons property prop)))))))
	  ;; This won't be used for anything.
	  (t 'ignore))))
(cc-bytecomp-defun c-put-char-property-fun) ; Make it known below.

(defmacro c-put-char-property (pos property value)
  ;; Put the given property with the given value on the character at
  ;; POS and make it front and rear nonsticky, or start and end open
  ;; in XEmacs vocabulary.  If the character already has the given
  ;; property then the value is replaced, and the behavior is
  ;; undefined if that property has been put by some other function.
  ;; PROPERTY is assumed to be constant.
  ;;
  ;; If there's a `text-property-default-nonsticky' variable (Emacs
  ;; 21) then it's assumed that the property is present on it.
  ;;
  ;; This macro does a hidden buffer change.
  (setq property (eval property))
  (if (or c-use-extents
	  (not (cc-bytecomp-boundp 'text-property-default-nonsticky)))
      ;; XEmacs and Emacs < 21.
      `(c-put-char-property-fun ,pos ',property ,value)
    ;; In Emacs 21 we got the `rear-nonsticky' property covered
    ;; by `text-property-default-nonsticky'.
    `(let ((-pos- ,pos))
       (put-text-property -pos- (1+ -pos-) ',property ,value))))

(defmacro c-get-char-property (pos property)
  ;; Get the value of the given property on the character at POS if
  ;; it's been put there by `c-put-char-property'.  PROPERTY is
  ;; assumed to be constant.
  (setq property (eval property))
  (if c-use-extents
      ;; XEmacs.
      `(let ((ext (extent-at ,pos nil ',property)))
	 (if ext (extent-property ext ',property)))
    ;; Emacs.
    `(get-text-property ,pos ',property)))

;; `c-clear-char-property' is complex enough in Emacs < 21 to make it
;; a function, since we have to mess with the `rear-nonsticky' property.
(defalias 'c-clear-char-property-fun
  (cc-eval-when-compile
    (unless (or c-use-extents
		(cc-bytecomp-boundp 'text-property-default-nonsticky))
      (byte-compile
       (lambda (pos property)
	 (when (get-text-property pos property)
	   (remove-text-properties pos (1+ pos) (list property nil))
	   (put-text-property pos (1+ pos)
			      'rear-nonsticky
			      (delq property (get-text-property
					      pos 'rear-nonsticky)))))))))
(cc-bytecomp-defun c-clear-char-property-fun) ; Make it known below.

(defmacro c-clear-char-property (pos property)
  ;; Remove the given property on the character at POS if it's been put
  ;; there by `c-put-char-property'.  PROPERTY is assumed to be
  ;; constant.
  ;;
  ;; This macro does a hidden buffer change.
  (setq property (eval property))
  (cond (c-use-extents
	 ;; XEmacs.
	 `(let ((ext (extent-at ,pos nil ',property)))
	    (if ext (delete-extent ext))))
	((cc-bytecomp-boundp 'text-property-default-nonsticky)
	 ;; In Emacs 21 we got the `rear-nonsticky' property covered
	 ;; by `text-property-default-nonsticky'.
	 `(let ((pos ,pos))
	    (remove-text-properties pos (1+ pos)
				    '(,property nil))))
	(t
	 ;; Emacs < 21.
	 `(c-clear-char-property-fun ,pos ',property))))

(defmacro c-clear-char-properties (from to property)
  ;; Remove all the occurrences of the given property in the given
  ;; region that has been put with `c-put-char-property'.  PROPERTY is
  ;; assumed to be constant.
  ;;
  ;; Note that this function does not clean up the property from the
  ;; lists of the `rear-nonsticky' properties in the region, if such
  ;; are used.  Thus it should not be used for common properties like
  ;; `syntax-table'.
  ;;
  ;; This macro does hidden buffer changes.
  (setq property (eval property))
  (if c-use-extents
      ;; XEmacs.
      `(map-extents (lambda (ext ignored)
		      (delete-extent ext))
		    nil ,from ,to nil nil ',property)
    ;; Emacs.
    `(remove-text-properties ,from ,to '(,property nil))))

(defmacro c-search-forward-char-property (property value &optional limit)
  "Search forward for a text-property PROPERTY having value VALUE.
LIMIT bounds the search.  The comparison is done with `equal'.

Leave point just after the character, and set the match data on
this character, and return point.  If VALUE isn't found, Return
nil; point is then left undefined."
  `(let ((place (point)))
     (while
	 (and
	  (< place ,(or limit '(point-max)))
	  (not (equal (get-text-property place ,property) ,value)))
       (setq place (next-single-property-change
		    place ,property nil ,(or limit '(point-max)))))
     (when (< place ,(or limit '(point-max)))
       (goto-char place)
       (search-forward-regexp ".")	; to set the match-data.
       (point))))

(defmacro c-search-backward-char-property (property value &optional limit)
  "Search backward for a text-property PROPERTY having value VALUE.
LIMIT bounds the search.  The comparison is done with `equal'.

Leave point just before the character, set the match data on this
character, and return point.  If VALUE isn't found, Return nil;
point is then left undefined."
  `(let ((place (point)))
     (while
	 (and
	  (> place ,(or limit '(point-min)))
	  (not (equal (get-text-property (1- place) ,property) ,value)))
       (setq place (previous-single-property-change
		    place ,property nil ,(or limit '(point-min)))))
     (when (> place ,(or limit '(point-max)))
       (goto-char place)
       (search-backward-regexp ".")	; to set the match-data.
       (point))))

(defun c-clear-char-property-with-value-function (from to property value)
  "Remove all text-properties PROPERTY from the region (FROM, TO)
which have the value VALUE, as tested by `equal'.  These
properties are assumed to be over individual characters, having
been put there by c-put-char-property.  POINT remains unchanged."
  (let ((place from) end-place)
    (while			  ; loop round occurrences of (PROPERTY VALUE)
	(progn
	  (while	   ; loop round changes in PROPERTY till we find VALUE
	      (and
	       (< place to)
	       (not (equal (get-text-property place property) value)))
	    (setq place (next-single-property-change place property nil to)))
	  (< place to))
      (setq end-place (next-single-property-change place property nil to))
      (remove-text-properties place end-place (cons property nil))
      ;; Do we have to do anything with stickiness here?
      (setq place end-place))))

(defmacro c-clear-char-property-with-value (from to property value)
  "Remove all text-properties PROPERTY from the region [FROM, TO)
which have the value VALUE, as tested by `equal'.  These
properties are assumed to be over individual characters, having
been put there by c-put-char-property.  POINT remains unchanged."
  (if c-use-extents
    ;; XEmacs
      `(let ((-property- ,property))
	 (map-extents (lambda (ext val)
			(if (equal (extent-property ext -property-) val)
			    (delete-extent ext)))
		      nil ,from ,to ,value nil -property-))
  ;; Gnu Emacs
    `(c-clear-char-property-with-value-function ,from ,to ,property ,value)))

;; Macros to put overlays (Emacs) or extents (XEmacs) on buffer text.
;; For our purposes, these are characterized by being possible to
;; remove again without affecting the other text properties in the
;; buffer that got overridden when they were put.

(defmacro c-put-overlay (from to property value)
  ;; Put an overlay/extent covering the given range in the current
  ;; buffer.  It's currently undefined whether it's front/end sticky
  ;; or not.  The overlay/extent object is returned.
  (if (cc-bytecomp-fboundp 'make-overlay)
      ;; Emacs.
      `(let ((ol (make-overlay ,from ,to)))
	 (overlay-put ol ,property ,value)
	 ol)
    ;; XEmacs.
    `(let ((ext (make-extent ,from ,to)))
       (set-extent-property ext ,property ,value)
       ext)))

(defmacro c-delete-overlay (overlay)
  ;; Deletes an overlay/extent object previously retrieved using
  ;; `c-put-overlay'.
  (if (cc-bytecomp-fboundp 'make-overlay)
      ;; Emacs.
      `(delete-overlay ,overlay)
    ;; XEmacs.
    `(delete-extent ,overlay)))


;; Make edebug understand the macros.
;(eval-after-load "edebug" ; 2006-07-09: def-edebug-spec is now in subr.el.
;  '(progn
(def-edebug-spec cc-eval-when-compile t)
(def-edebug-spec c-point t)
(def-edebug-spec c-set-region-active t)
(def-edebug-spec c-safe t)
(def-edebug-spec c-save-buffer-state let*)
(def-edebug-spec c-tentative-buffer-changes t)
(def-edebug-spec c-forward-syntactic-ws t)
(def-edebug-spec c-backward-syntactic-ws t)
(def-edebug-spec c-forward-sexp t)
(def-edebug-spec c-backward-sexp t)
(def-edebug-spec c-up-list-forward t)
(def-edebug-spec c-up-list-backward t)
(def-edebug-spec c-down-list-forward t)
(def-edebug-spec c-down-list-backward t)
(def-edebug-spec c-add-syntax t)
(def-edebug-spec c-add-class-syntax t)
(def-edebug-spec c-benign-error t)
(def-edebug-spec c-with-syntax-table t)
(def-edebug-spec c-skip-ws-forward t)
(def-edebug-spec c-skip-ws-backward t)
(def-edebug-spec c-major-mode-is t)
(def-edebug-spec c-put-char-property t)
(def-edebug-spec c-get-char-property t)
(def-edebug-spec c-clear-char-property t)
(def-edebug-spec c-clear-char-properties t)
(def-edebug-spec c-put-overlay t)
(def-edebug-spec c-delete-overlay t) ;))


;;; Functions.

;; Note: All these after the macros, to be on safe side in avoiding
;; bugs where macros are defined too late.  These bugs often only show
;; when the files are compiled in a certain order within the same
;; session.

(defsubst c-end-of-defun-1 ()
  ;; Replacement for end-of-defun that use c-beginning-of-defun-1.
  (let ((start (point)))
    ;; Skip forward into the next defun block. Don't bother to avoid
    ;; comments, literals etc, since beginning-of-defun doesn't do that
    ;; anyway.
    (skip-chars-forward "^}")
    (c-beginning-of-defun-1)
    (if (eq (char-after) ?{)
	(c-forward-sexp))
    (if (< (point) start)
	(goto-char (point-max)))))

(defconst c-<-as-paren-syntax '(4 . ?>))
(put 'c-<-as-paren-syntax 'syntax-table c-<-as-paren-syntax)

(defsubst c-mark-<-as-paren (pos)
  ;; Mark the "<" character at POS as a template opener using the
  ;; `syntax-table' property via the `category' property.
  ;;
  ;; This function does a hidden buffer change.  Note that we use
  ;; indirection through the `category' text property.  This allows us to
  ;; toggle the property in all template brackets simultaneously and
  ;; cheaply.  We use this, for instance, in `c-parse-state'.
  (c-put-char-property pos 'category 'c-<-as-paren-syntax))

(defconst c->-as-paren-syntax '(5 . ?<))
(put 'c->-as-paren-syntax 'syntax-table c->-as-paren-syntax)

(defsubst c-mark->-as-paren (pos)
  ;; Mark the ">" character at POS as an sexp list closer using the
  ;; syntax-table property.
  ;;
  ;; This function does a hidden buffer change.  Note that we use
  ;; indirection through the `category' text property.  This allows us to
  ;; toggle the property in all template brackets simultaneously and
  ;; cheaply.  We use this, for instance, in `c-parse-state'.
  (c-put-char-property pos 'category 'c->-as-paren-syntax))

(defsubst c-unmark-<->-as-paren (pos)
  ;; Unmark the "<" or "<" character at POS as an sexp list opener using
  ;; the syntax-table property indirectly through the `category' text
  ;; property.
  ;;
  ;; This function does a hidden buffer change.  Note that we use
  ;; indirection through the `category' text property.  This allows us to
  ;; toggle the property in all template brackets simultaneously and
  ;; cheaply.  We use this, for instance, in `c-parse-state'.
  (c-clear-char-property pos 'category))

(defsubst c-suppress-<->-as-parens ()
  ;; Suppress the syntactic effect of all marked < and > as parens.  Note
  ;; that this effect is NOT buffer local.  You should probably not use
  ;; this directly, but only through the macro
  ;; `c-with-<->-as-parens-suppressed'
  (put 'c-<-as-paren-syntax 'syntax-table nil)
  (put 'c->-as-paren-syntax 'syntax-table nil))

(defsubst c-restore-<->-as-parens ()
  ;; Restore the syntactic effect of all marked <s and >s as parens.  This
  ;; has no effect on unmarked <s and >s
  (put 'c-<-as-paren-syntax 'syntax-table c-<-as-paren-syntax)
  (put 'c->-as-paren-syntax 'syntax-table c->-as-paren-syntax))

(defmacro c-with-<->-as-parens-suppressed (&rest forms)
  ;; Like progn, except that the paren property is suppressed on all
  ;; template brackets whilst they are running.  This macro does a hidden
  ;; buffer change.
  `(unwind-protect
       (progn
	 (c-suppress-<->-as-parens)
	 ,@forms)
     (c-restore-<->-as-parens)))

;;;;;;;;;;;;;;;

(defconst c-cpp-delimiter '(14)) ; generic comment syntax
;; This is the value of the `category' text property placed on every #
;; which introduces a CPP construct and every EOL (or EOB, or character
;; preceding //, etc.) which terminates it.  We can instantly "comment
;; out" all CPP constructs by giving `c-cpp-delimiter' a syntax-table
;; property '(14) (generic comment delimiter).
(defmacro c-set-cpp-delimiters (beg end)
  ;; This macro does a hidden buffer change.
  `(progn
     (c-put-char-property ,beg 'category 'c-cpp-delimiter)
     (if (< ,end (point-max))
	 (c-put-char-property ,end 'category 'c-cpp-delimiter))))
(defmacro c-clear-cpp-delimiters (beg end)
  ;; This macro does a hidden buffer change.
  `(progn
     (c-clear-char-property ,beg 'category)
     (if (< ,end (point-max))
	 (c-clear-char-property ,end 'category))))

(defsubst c-comment-out-cpps ()
  ;; Render all preprocessor constructs syntactically commented out.
  (put 'c-cpp-delimiter 'syntax-table c-cpp-delimiter))
(defsubst c-uncomment-out-cpps ()
  ;; Restore the syntactic visibility of preprocessor constructs.
  (put 'c-cpp-delimiter 'syntax-table nil))

(defmacro c-with-cpps-commented-out (&rest forms)
  ;; Execute FORMS... whilst the syntactic effect of all characters in
  ;; all CPP regions is suppressed.  In particular, this is to suppress
  ;; the syntactic significance of parens/braces/brackets to functions
  ;; such as `scan-lists' and `parse-partial-sexp'.
  `(unwind-protect
       (c-save-buffer-state ()
	   (c-comment-out-cpps)
	   ,@forms)
     (c-save-buffer-state ()
       (c-uncomment-out-cpps))))

(defmacro c-with-all-but-one-cpps-commented-out (beg end &rest forms)
  ;; Execute FORMS... whilst the syntactic effect of all characters in
  ;; every CPP region APART FROM THE ONE BETWEEN BEG and END is
  ;; suppressed.
  `(unwind-protect
       (c-save-buffer-state ()
	 (c-clear-cpp-delimiters ,beg ,end)
	 ,`(c-with-cpps-commented-out ,@forms))
     (c-save-buffer-state ()
       (c-set-cpp-delimiters ,beg ,end))))

(defsubst c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defsubst c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  (assq (car (c-intersect-lists list alist1)) alist2))

(defsubst c-langelem-sym (langelem)
  "Return the syntactic symbol in LANGELEM.

LANGELEM is either a cons cell on the \"old\" form given as the first
argument to lineup functions or a syntactic element on the \"new\"
form as used in `c-syntactic-element'."
  (car langelem))

(defsubst c-langelem-pos (langelem)
  "Return the anchor position in LANGELEM, or nil if there is none.

LANGELEM is either a cons cell on the \"old\" form given as the first
argument to lineup functions or a syntactic element on the \"new\"
form as used in `c-syntactic-element'."
  (if (consp (cdr langelem))
      (car-safe (cdr langelem))
    (cdr langelem)))

(defun c-langelem-col (langelem &optional preserve-point)
  "Return the column of the anchor position in LANGELEM.
Also move the point to that position unless PRESERVE-POINT is non-nil.

LANGELEM is either a cons cell on the \"old\" form given as the first
argument to lineup functions or a syntactic element on the \"new\"
form as used in `c-syntactic-element'."
  (let ((pos (c-langelem-pos langelem))
	(here (point)))
    (if pos
	(progn
	  (goto-char pos)
	  (prog1 (current-column)
	    (if preserve-point
		(goto-char here))))
      0)))

(defsubst c-langelem-2nd-pos (langelem)
  "Return the secondary position in LANGELEM, or nil if there is none.

LANGELEM is typically a syntactic element on the \"new\" form as used
in `c-syntactic-element'.  It may also be a cons cell as passed in the
first argument to lineup functions, but then the returned value always
will be nil."
  (car-safe (cdr-safe (cdr-safe langelem))))

(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in XEmacs.
  ;; This is not needed for Emacs.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(put 'c-mode    'c-mode-prefix "c-")
(put 'c++-mode  'c-mode-prefix "c++-")
(put 'objc-mode 'c-mode-prefix "objc-")
(put 'java-mode 'c-mode-prefix "java-")
(put 'idl-mode  'c-mode-prefix "idl-")
(put 'pike-mode 'c-mode-prefix "pike-")
(put 'awk-mode  'c-mode-prefix "awk-")

(defsubst c-mode-symbol (suffix)
  "Prefix the current mode prefix (e.g. \"c-\") to SUFFIX and return
the corresponding symbol."
  (or c-buffer-is-cc-mode
      (error "Not inside a CC Mode based mode"))
  (let ((mode-prefix (get c-buffer-is-cc-mode 'c-mode-prefix)))
    (or mode-prefix
	(error "%S has no mode prefix known to `c-mode-symbol'"
	       c-buffer-is-cc-mode))
    (intern (concat mode-prefix suffix))))

(defsubst c-mode-var (suffix)
  "Prefix the current mode prefix (e.g. \"c-\") to SUFFIX and return
the value of the variable with that name."
  (symbol-value (c-mode-symbol suffix)))

(defsubst c-got-face-at (pos faces)
  "Return non-nil if position POS in the current buffer has any of the
faces in the list FACES."
  (let ((pos-faces (get-text-property pos 'face)))
    (if (consp pos-faces)
	(progn
	  (while (and pos-faces
		      (not (memq (car pos-faces) faces)))
	    (setq pos-faces (cdr pos-faces)))
	  pos-faces)
      (memq pos-faces faces))))

(defsubst c-face-name-p (facename)
  ;; Return t if FACENAME is the name of a face.  This method is
  ;; necessary since facep in XEmacs only returns t for the actual
  ;; face objects (while it's only their names that are used just
  ;; about anywhere else) without providing a predicate that tests
  ;; face names.
  (memq facename (face-list)))

(defun c-concat-separated (list separator)
  "Like `concat' on LIST, but separate each element with SEPARATOR.
Notably, null elements in LIST are ignored."
  (mapconcat 'identity (delete nil (append list nil)) separator))

(defun c-make-keywords-re (adorn list &optional mode)
  "Make a regexp that matches all the strings the list.
Duplicates and nil elements in the list are removed.  The resulting
regexp may contain zero or more submatch expressions.

If ADORN is t there will be at least one submatch and the first
surrounds the matched alternative, and the regexp will also not match
a prefix of any identifier.  Adorned regexps cannot be appended.  The
language variable `c-nonsymbol-key' is used to make the adornment.

A value 'appendable for ADORN is like above, but all alternatives in
the list that end with a word constituent char will have \\> appended
instead, so that the regexp remains appendable.  Note that this
variant doesn't always guarantee that an identifier prefix isn't
matched since the symbol constituent '_' is normally considered a
nonword token by \\>.

The optional MODE specifies the language to get `c-nonsymbol-key' from
when it's needed.  The default is the current language taken from
`c-buffer-is-cc-mode'."

  (let (unique)
    (dolist (elt list)
      (unless (member elt unique)
	(push elt unique)))
    (setq list (delete nil unique)))
  (if list
      (let (re)

	(if (eq adorn 'appendable)
	    ;; This is kludgy but it works: Search for a string that
	    ;; doesn't occur in any word in LIST.  Append it to all
	    ;; the alternatives where we want to add \>.  Run through
	    ;; `regexp-opt' and then replace it with \>.
	    (let ((unique "") pos)
	      (while (let (found)
		       (setq unique (concat unique "@")
			     pos list)
		       (while (and pos
				   (if (string-match unique (car pos))
				       (progn (setq found t)
					      nil)
				     t))
			 (setq pos (cdr pos)))
		       found))
	      (setq pos list)
	      (while pos
		(if (string-match "\\w\\'" (car pos))
		    (setcar pos (concat (car pos) unique)))
		(setq pos (cdr pos)))
	      (setq re (regexp-opt list))
	      (setq pos 0)
	      (while (string-match unique re pos)
		(setq pos (+ (match-beginning 0) 2)
		      re (replace-match "\\>" t t re))))

	  (setq re (regexp-opt list)))

	;; Emacs 20 and XEmacs (all versions so far) has a buggy
	;; regexp-opt that doesn't always cope with strings containing
	;; newlines.  This kludge doesn't handle shy parens correctly
	;; so we can't advice regexp-opt directly with it.
	(let (fail-list)
	  (while list
	    (and (string-match "\n" (car list)) ; To speed it up a little.
		 (not (string-match (concat "\\`\\(" re "\\)\\'")
				    (car list)))
		 (setq fail-list (cons (car list) fail-list)))
	    (setq list (cdr list)))
	  (when fail-list
	    (setq re (concat re
			     "\\|"
			     (mapconcat
			      (if (eq adorn 'appendable)
				  (lambda (str)
				    (if (string-match "\\w\\'" str)
					(concat (regexp-quote str)
						"\\>")
				      (regexp-quote str)))
				'regexp-quote)
			      (sort fail-list
				    (lambda (a b)
				      (> (length a) (length b))))
			      "\\|")))))

	;; Add our own grouping parenthesis around re instead of
	;; passing adorn to `regexp-opt', since in XEmacs it makes the
	;; top level grouping "shy".
	(cond ((eq adorn 'appendable)
	       (concat "\\(" re "\\)"))
	      (adorn
	       (concat "\\(" re "\\)"
		       "\\("
		       (c-get-lang-constant 'c-nonsymbol-key nil mode)
		       "\\|$\\)"))
	      (t
	       re)))

    ;; Produce a regexp that matches nothing.
    (if adorn
	"\\(\\<\\>\\)"
      "\\<\\>")))

(put 'c-make-keywords-re 'lisp-indent-function 1)

(defun c-make-bare-char-alt (chars &optional inverted)
  "Make a character alternative string from the list of characters CHARS.
The returned string is of the type that can be used with
`skip-chars-forward' and `skip-chars-backward'.  If INVERTED is
non-nil, a caret is prepended to invert the set."
  ;; This function ought to be in the elisp core somewhere.
  (let ((str (if inverted "^" "")) char char2)
    (setq chars (sort (append chars nil) `<))
    (while chars
      (setq char (pop chars))
      (if (memq char '(?\\ ?^ ?-))
	  ;; Quoting necessary (this method only works in the skip
	  ;; functions).
	  (setq str (format "%s\\%c" str char))
	(setq str (format "%s%c" str char)))
      ;; Check for range.
      (setq char2 char)
      (while (and chars (>= (1+ char2) (car chars)))
	(setq char2 (pop chars)))
      (unless (= char char2)
	(if (< (1+ char) char2)
	    (setq str (format "%s-%c" str char2))
	  (push char2 chars))))
    str))

;; Leftovers from (X)Emacs 19 compatibility.
(defalias 'c-regexp-opt 'regexp-opt)
(defalias 'c-regexp-opt-depth 'regexp-opt-depth)


;; Figure out what features this Emacs has

(cc-bytecomp-defvar open-paren-in-column-0-is-defun-start)

(defconst c-emacs-features
  (let (list)

    (if (boundp 'infodock-version)
	;; I've no idea what this actually is, but it's legacy. /mast
	(setq list (cons 'infodock list)))

    ;; XEmacs uses 8-bit modify-syntax-entry flags.
    ;; Emacs uses a 1-bit flag.  We will have to set up our
    ;; syntax tables differently to handle this.
    (let ((table (copy-syntax-table))
	  entry)
      (modify-syntax-entry ?a ". 12345678" table)
      (cond
       ;; Emacs
       ((arrayp table)
	(setq entry (aref table ?a))
	;; In Emacs, table entries are cons cells
	(if (consp entry) (setq entry (car entry))))
       ;; XEmacs
       ((fboundp 'get-char-table)
	(setq entry (get-char-table ?a table)))
       ;; incompatible
       (t (error "CC Mode is incompatible with this version of Emacs")))
      (setq list (cons (if (= (logand (lsh entry -16) 255) 255)
			   '8-bit
			 '1-bit)
		       list)))

    ;; Check whether beginning/end-of-defun call
    ;; beginning/end-of-defun-function nicely, passing through the
    ;; argument and respecting the return code.
    (let* (mark-ring
	   (bod-param 'foo) (eod-param 'foo)
	   (beginning-of-defun-function
	    (lambda (&optional arg)
	      (or (eq bod-param 'foo) (setq bod-param 'bar))
	      (and (eq bod-param 'foo)
		   (setq bod-param arg)
		   (eq arg 3))))
	   (end-of-defun-function
	    (lambda (&optional arg)
	      (and (eq eod-param 'foo)
		   (setq eod-param arg)
		   (eq arg 3)))))
      (if (save-excursion (and (beginning-of-defun 3) (eq bod-param 3)
			       (not (beginning-of-defun))
			       (end-of-defun 3) (eq eod-param 3)
			       (not (end-of-defun))))
	  (setq list (cons 'argumentative-bod-function list))))

    (let ((buf (generate-new-buffer " test"))
	  parse-sexp-lookup-properties
	  parse-sexp-ignore-comments
	  lookup-syntax-properties)
      (with-current-buffer buf
	(set-syntax-table (make-syntax-table))

	;; For some reason we have to set some of these after the
	;; buffer has been made current.  (Specifically,
	;; `parse-sexp-ignore-comments' in Emacs 21.)
	(setq parse-sexp-lookup-properties t
	      parse-sexp-ignore-comments t
	      lookup-syntax-properties t)

	;; Find out if the `syntax-table' text property works.
	(modify-syntax-entry ?< ".")
	(modify-syntax-entry ?> ".")
	(insert "<()>")
	(c-mark-<-as-paren (point-min))
	(c-mark->-as-paren (+ 3 (point-min)))
	(goto-char (point-min))
	(c-forward-sexp)
	(if (= (point) (+ 4 (point-min)))
	    (setq list (cons 'syntax-properties list))
	  (error (concat
		  "CC Mode is incompatible with this version of Emacs - "
		  "support for the `syntax-table' text property "
		  "is required.")))

	;; Find out if generic comment delimiters work.
	(c-safe
	  (modify-syntax-entry ?x "!")
	  (if (string-match "\\s!" "x")
	      (setq list (cons 'gen-comment-delim list))))

	;; Find out if generic string delimiters work.
	(c-safe
	  (modify-syntax-entry ?x "|")
	  (if (string-match "\\s|" "x")
	      (setq list (cons 'gen-string-delim list))))

	;; See if POSIX char classes work.
	(when (and (string-match "[[:alpha:]]" "a")
		   ;; All versions of Emacs 21 so far haven't fixed
		   ;; char classes in `skip-chars-forward' and
		   ;; `skip-chars-backward'.
		   (progn
		     (delete-region (point-min) (point-max))
		     (insert "foo123")
		     (skip-chars-backward "[:alnum:]")
		     (bobp))
		   (= (skip-chars-forward "[:alpha:]") 3))
	  (setq list (cons 'posix-char-classes list)))

	;; See if `open-paren-in-column-0-is-defun-start' exists and
	;; isn't buggy (Emacs >= 21.4).
	(when (boundp 'open-paren-in-column-0-is-defun-start)
	  (let ((open-paren-in-column-0-is-defun-start nil)
		(parse-sexp-ignore-comments t))
	    (delete-region (point-min) (point-max))
	    (set-syntax-table (make-syntax-table))
	    (modify-syntax-entry ?\' "\"")
	    (cond
	     ;; XEmacs.  Afaik this is currently an Emacs-only
	     ;; feature, but it's good to be prepared.
	     ((memq '8-bit list)
	      (modify-syntax-entry ?/ ". 1456")
	      (modify-syntax-entry ?* ". 23"))
	     ;; Emacs
	     ((memq '1-bit list)
	      (modify-syntax-entry ?/ ". 124b")
	      (modify-syntax-entry ?* ". 23")))
	    (modify-syntax-entry ?\n "> b")
	    (insert "/* '\n   () */")
	    (backward-sexp)
	    (if (bobp)
		(setq list (cons 'col-0-paren list)))))

	(set-buffer-modified-p nil))
      (kill-buffer buf))

    ;; See if `parse-partial-sexp' returns the eighth element.
    (if (c-safe (>= (length (save-excursion (parse-partial-sexp (point) (point))))
		    10))
	(setq list (cons 'pps-extended-state list))
      (error (concat
	      "CC Mode is incompatible with this version of Emacs - "
	      "`parse-partial-sexp' has to return at least 10 elements.")))

    ;;(message "c-emacs-features: %S" list)
    list)
  "A list of certain features in the (X)Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by CC Mode.  The following values
might be present:

'8-bit              8 bit syntax entry flags (XEmacs style).
'1-bit              1 bit syntax entry flags (Emacs style).
'argumentative-bod-function         beginning-of-defun passes ARG through
                    to a non-null beginning-of-defun-function.  It is assumed
		    the end-of-defun does the same thing.
'syntax-properties  It works to override the syntax for specific characters
		    in the buffer with the 'syntax-table property.  It's
		    always set - CC Mode no longer works in emacsen without
		    this feature.
'gen-comment-delim  Generic comment delimiters work
		    (i.e. the syntax class `!').
'gen-string-delim   Generic string delimiters work
		    (i.e. the syntax class `|').
'pps-extended-state `parse-partial-sexp' returns a list with at least 10
		    elements, i.e. it contains the position of the start of
		    the last comment or string.  It's always set - CC Mode
                    no longer works in emacsen without this feature.
'posix-char-classes The regexp engine understands POSIX character classes.
'col-0-paren        It's possible to turn off the ad-hoc rule that a paren
		    in column zero is the start of a defun.
'infodock           This is Infodock (based on XEmacs).

'8-bit and '1-bit are mutually exclusive.")


;;; Some helper constants.

;; If the regexp engine supports POSIX char classes then we can use
;; them to handle extended charsets correctly.
(if (memq 'posix-char-classes c-emacs-features)
    (progn
      (defconst c-alpha "[:alpha:]")
      (defconst c-alnum "[:alnum:]")
      (defconst c-digit "[:digit:]")
      (defconst c-upper "[:upper:]")
      (defconst c-lower "[:lower:]"))
  (defconst c-alpha "a-zA-Z")
  (defconst c-alnum "a-zA-Z0-9")
  (defconst c-digit "0-9")
  (defconst c-upper "A-Z")
  (defconst c-lower "a-z"))


;;; System for handling language dependent constants.

;; This is used to set various language dependent data in a flexible
;; way: Language constants can be built from the values of other
;; language constants, also those for other languages.  They can also
;; process the values of other language constants uniformly across all
;; the languages.  E.g. one language constant can list all the type
;; keywords in each language, and another can build a regexp for each
;; language from those lists without code duplication.
;;
;; Language constants are defined with `c-lang-defconst', and their
;; value forms (referred to as source definitions) are evaluated only
;; on demand when requested for a particular language with
;; `c-lang-const'.  It's therefore possible to refer to the values of
;; constants defined later in the file, or in another file, just as
;; long as all the relevant `c-lang-defconst' have been loaded when
;; `c-lang-const' is actually evaluated from somewhere else.
;;
;; `c-lang-const' forms are also evaluated at compile time and
;; replaced with the values they produce.  Thus there's no overhead
;; for this system when compiled code is used - only the values
;; actually used in the code are present, and the file(s) containing
;; the `c-lang-defconst' forms don't need to be loaded at all then.
;; There are however safeguards to make sure that they can be loaded
;; to get the source definitions for the values if there's a mismatch
;; in compiled versions, or if `c-lang-const' is used uncompiled.
;;
;; Note that the source definitions in a `c-lang-defconst' form are
;; compiled into the .elc file where it stands; there's no need to
;; load the source file to get it.
;;
;; See cc-langs.el for more details about how this system is deployed
;; in CC Mode, and how the associated language variable system
;; (`c-lang-defvar') works.  That file also contains a lot of
;; examples.

(defun c-add-language (mode base-mode)
  "Declare a new language in the language dependent variable system.
This is intended to be used by modes that inherit CC Mode to add new
languages.  It should be used at the top level before any calls to
`c-lang-defconst'.  MODE is the mode name symbol for the new language,
and BASE-MODE is the mode name symbol for the language in CC Mode that
is to be the template for the new mode.

The exact effect of BASE-MODE is to make all language constants that
haven't got a setting in the new language fall back to their values in
BASE-MODE.  It does not have any effect outside the language constant
system."
  (unless (string-match "\\`\\(.*-\\)mode\\'" (symbol-name mode))
    (error "The mode name symbol `%s' must end with \"-mode\"" mode))
  (put mode 'c-mode-prefix (match-string 1 (symbol-name mode)))
  (unless (get base-mode 'c-mode-prefix)
    (error "Unknown base mode `%s'" base-mode))
  (put mode 'c-fallback-mode base-mode))

(defvar c-lang-constants (make-vector 151 0))
;; This obarray is a cache to keep track of the language constants
;; defined by `c-lang-defconst' and the evaluated values returned by
;; `c-lang-const'.  It's mostly used at compile time but it's not
;; stored in compiled files.
;;
;; The obarray contains all the language constants as symbols.  The
;; value cells hold the evaluated values as alists where each car is
;; the mode name symbol and the corresponding cdr is the evaluated
;; value in that mode.  The property lists hold the source definitions
;; and other miscellaneous data.  The obarray might also contain
;; various other symbols, but those don't have any variable bindings.

(defvar c-lang-const-expansion nil)

(defsubst c-get-current-file ()
  ;; Return the base name of the current file.
  (let ((file (cond
	       (load-in-progress
		;; Being loaded.
		load-file-name)
	       ((and (boundp 'byte-compile-dest-file)
		     (stringp byte-compile-dest-file))
		;; Being compiled.
		byte-compile-dest-file)
	       (t
		;; Being evaluated interactively.
		(buffer-file-name)))))
    (and file
	 (file-name-sans-extension
	  (file-name-nondirectory file)))))

(defmacro c-lang-defconst-eval-immediately (form)
  "Can be used inside a VAL in `c-lang-defconst' to evaluate FORM
immediately, i.e. at the same time as the `c-lang-defconst' form
itself is evaluated."
  ;; Evaluate at macro expansion time, i.e. in the
  ;; `cl-macroexpand-all' inside `c-lang-defconst'.
  (eval form))

;; Only used at compile time - suppress "might not be defined at runtime".
(declare-function cl-macroexpand-all "cl-extra" (form &optional env))

(defmacro c-lang-defconst (name &rest args)
  "Set the language specific values of the language constant NAME.
The second argument can optionally be a docstring.  The rest of the
arguments are one or more repetitions of LANG VAL where LANG specifies
the language(s) that VAL applies to.  LANG is the name of the
language, i.e. the mode name without the \"-mode\" suffix, or a list
of such language names, or `t' for all languages.  VAL is a form to
evaluate to get the value.

If LANG isn't `t' or one of the core languages in CC Mode, it must
have been declared with `c-add-language'.

Neither NAME, LANG nor VAL are evaluated directly - they should not be
quoted.  `c-lang-defconst-eval-immediately' can however be used inside
VAL to evaluate parts of it directly.

When VAL is evaluated for some language, that language is temporarily
made current so that `c-lang-const' without an explicit language can
be used inside VAL to refer to the value of a language constant in the
same language.  That is particularly useful if LANG is `t'.

VAL is not evaluated right away but rather when the value is requested
with `c-lang-const'.  Thus it's possible to use `c-lang-const' inside
VAL to refer to language constants that haven't been defined yet.
However, if the definition of a language constant is in another file
then that file must be loaded \(at compile time) before it's safe to
reference the constant.

The assignments in ARGS are processed in sequence like `setq', so
\(c-lang-const NAME) may be used inside a VAL to refer to the last
assigned value to this language constant, or a value that it has
gotten in another earlier loaded file.

To work well with repeated loads and interactive reevaluation, only
one `c-lang-defconst' for each NAME is permitted per file.  If there
already is one it will be completely replaced; the value in the
earlier definition will not affect `c-lang-const' on the same
constant.  A file is identified by its base name."

  (let* ((sym (intern (symbol-name name) c-lang-constants))
	 ;; Make `c-lang-const' expand to a straightforward call to
	 ;; `c-get-lang-constant' in `cl-macroexpand-all' below.
	 ;;
	 ;; (The default behavior, i.e. to expand to a call inside
	 ;; `eval-when-compile' should be equivalent, since that macro
	 ;; should only expand to its content if it's used inside a
	 ;; form that's already evaluated at compile time.  It's
	 ;; however necessary to use our cover macro
	 ;; `cc-eval-when-compile' due to bugs in `eval-when-compile',
	 ;; and it expands to a bulkier form that in this case only is
	 ;; unnecessary garbage that we don't want to store in the
	 ;; language constant source definitions.)
	 (c-lang-const-expansion 'call)
	 (c-langs-are-parametric t)
	 bindings
	 pre-files)

    (or (symbolp name)
	(error "Not a symbol: %s" name))

    (when (stringp (car-safe args))
      ;; The docstring is hardly used anywhere since there's no normal
      ;; symbol to attach it to.  It's primarily for getting the right
      ;; format in the source.
      (put sym 'variable-documentation (car args))
      (setq args (cdr args)))

    (or args
	(error "No assignments in `c-lang-defconst' for %s" name))

    ;; Rework ARGS to an association list to make it easier to handle.
    ;; It's reversed at the same time to make it easier to implement
    ;; the demand-driven (i.e. reversed) evaluation in `c-lang-const'.
    (while args
      (let ((assigned-mode
	     (cond ((eq (car args) t) t)
		   ((symbolp (car args))
		    (list (intern (concat (symbol-name (car args))
					  "-mode"))))
		   ((listp (car args))
		    (mapcar (lambda (lang)
			      (or (symbolp lang)
				  (error "Not a list of symbols: %s"
					 (car args)))
			      (intern (concat (symbol-name lang)
					      "-mode")))
			    (car args)))
		   (t (error "Not a symbol or a list of symbols: %s"
			     (car args)))))
	    val)

	(or (cdr args)
	    (error "No value for %s" (car args)))
	(setq args (cdr args)
	      val (car args))

	;; Emacs has a weird bug where it seems to fail to read
	;; backquote lists from byte compiled files correctly (,@
	;; forms, to be specific), so make sure the bindings in the
	;; expansion below don't contain any backquote stuff.
	;; (XEmacs handles it correctly and doesn't need this for that
	;; reason, but we also use this expansion handle
	;; `c-lang-defconst-eval-immediately' and to register
	;; dependencies on the `c-lang-const's in VAL.)
	(setq val (cl-macroexpand-all val))

	(setq bindings (cons (cons assigned-mode val) bindings)
	      args (cdr args))))

    ;; Compile in the other files that have provided source
    ;; definitions for this symbol, to make sure the order in the
    ;; `source' property is correct even when files are loaded out of
    ;; order.
    (setq pre-files (nreverse
		     ;; Reverse to get the right load order.
		     (mapcar 'car (get sym 'source))))

    `(eval-and-compile
       (c-define-lang-constant ',name ',bindings
			       ,@(and pre-files `(',pre-files))))))

(put 'c-lang-defconst 'lisp-indent-function 1)
;(eval-after-load "edebug" ; 2006-07-09: def-edebug-spec is now in subr.el.
;  '
(def-edebug-spec c-lang-defconst
  (&define name [&optional stringp] [&rest sexp def-form]))

(defun c-define-lang-constant (name bindings &optional pre-files)
  ;; Used by `c-lang-defconst'.

  (let* ((sym (intern (symbol-name name) c-lang-constants))
	 (source (get sym 'source))
	 (file (intern
		(or (c-get-current-file)
		    (error "`c-lang-defconst' must be used in a file"))))
	 (elem (assq file source)))

    ;;(when (cdr-safe elem)
    ;;  (message "Language constant %s redefined in %S" name file))

    ;; Note that the order in the source alist is relevant.  Like how
    ;; `c-lang-defconst' reverses the bindings, this reverses the
    ;; order between files so that the last to evaluate comes first.
    (unless elem
      (while pre-files
	(unless (assq (car pre-files) source)
	  (setq source (cons (list (car pre-files)) source)))
	(setq pre-files (cdr pre-files)))
      (put sym 'source (cons (setq elem (list file)) source)))

    (setcdr elem bindings)

    ;; Bind the symbol as a variable, or clear any earlier evaluated
    ;; value it has.
    (set sym nil)

    ;; Clear the evaluated values that depend on this source.
    (let ((agenda (get sym 'dependents))
	  (visited (make-vector 101 0))
	  ptr)
      (while agenda
	(setq sym (car agenda)
	      agenda (cdr agenda))
	(intern (symbol-name sym) visited)
	(set sym nil)
	(setq ptr (get sym 'dependents))
	(while ptr
	  (setq sym (car ptr)
		ptr (cdr ptr))
	  (unless (intern-soft (symbol-name sym) visited)
	    (setq agenda (cons sym agenda))))))

    name))

(defmacro c-lang-const (name &optional lang)
  "Get the mode specific value of the language constant NAME in language LANG.
LANG is the name of the language, i.e. the mode name without the
\"-mode\" suffix.  If used inside `c-lang-defconst' or
`c-lang-defvar', LANG may be left out to refer to the current
language.  NAME and LANG are not evaluated so they should not be
quoted."

  (or (symbolp name)
      (error "Not a symbol: %s" name))
  (or (symbolp lang)
      (error "Not a symbol: %s" lang))

  (let ((sym (intern (symbol-name name) c-lang-constants))
	mode source-files args)

    (when lang
      (setq mode (intern (concat (symbol-name lang) "-mode")))
      (unless (get mode 'c-mode-prefix)
	(error
	 "Unknown language %S since it got no `c-mode-prefix' property"
	 (symbol-name lang))))

    (if (eq c-lang-const-expansion 'immediate)
	;; No need to find out the source file(s) when we evaluate
	;; immediately since all the info is already there in the
	;; `source' property.
	`',(c-get-lang-constant name nil mode)

      (let ((file (c-get-current-file)))
	(if file (setq file (intern file)))
	;; Get the source file(s) that must be loaded to get the value
	;; of the constant.  If the symbol isn't defined yet we assume
	;; that its definition will come later in this file, and thus
	;; are no file dependencies needed.
	(setq source-files (nreverse
			    ;; Reverse to get the right load order.
			    (apply 'nconc
				   (mapcar (lambda (elem)
					     (if (eq file (car elem))
						 nil ; Exclude our own file.
					       (list (car elem))))
					   (get sym 'source))))))

      ;; Make some effort to do a compact call to
      ;; `c-get-lang-constant' since it will be compiled in.
      (setq args (and mode `(',mode)))
      (if (or source-files args)
	  (setq args (cons (and source-files `',source-files)
			   args)))

      (if (or (eq c-lang-const-expansion 'call)
	      (and (not c-lang-const-expansion)
		   (not mode))
	      load-in-progress
	      (not (boundp 'byte-compile-dest-file))
	      (not (stringp byte-compile-dest-file)))
	  ;; Either a straight call is requested in the context, or
	  ;; we're in an "uncontrolled" context and got no language,
	  ;; or we're not being byte compiled so the compile time
	  ;; stuff below is unnecessary.
	  `(c-get-lang-constant ',name ,@args)

	;; Being compiled.  If the loading and compiling version is
	;; the same we use a value that is evaluated at compile time,
	;; otherwise it's evaluated at runtime.
	`(if (eq c-version-sym ',c-version-sym)
	     (cc-eval-when-compile
	       (c-get-lang-constant ',name ,@args))
	   (c-get-lang-constant ',name ,@args))))))

(defvar c-lang-constants-under-evaluation nil)

(defun c-get-lang-constant (name &optional source-files mode)
  ;; Used by `c-lang-const'.

  (or mode
      (setq mode c-buffer-is-cc-mode)
      (error "No current language"))

  (let* ((sym (intern (symbol-name name) c-lang-constants))
	 (source (get sym 'source))
	 elem
	 (eval-in-sym (and c-lang-constants-under-evaluation
			   (caar c-lang-constants-under-evaluation))))

    ;; Record the dependencies between this symbol and the one we're
    ;; being evaluated in.
    (when eval-in-sym
      (or (memq eval-in-sym (get sym 'dependents))
	  (put sym 'dependents (cons eval-in-sym (get sym 'dependents)))))

    ;; Make sure the source files have entries on the `source'
    ;; property so that loading will take place when necessary.
    (while source-files
      (unless (assq (car source-files) source)
	(put sym 'source
	     (setq source (cons (list (car source-files)) source)))
	;; Might pull in more definitions which affect the value.  The
	;; clearing of dependent values etc is done when the
	;; definition is encountered during the load; this is just to
	;; jump past the check for a cached value below.
	(set sym nil))
      (setq source-files (cdr source-files)))

    (if (and (boundp sym)
	     (setq elem (assq mode (symbol-value sym))))
	(cdr elem)

      ;; Check if an evaluation of this symbol is already underway.
      ;; In that case we just continue with the "assignment" before
      ;; the one currently being evaluated, thereby creating the
      ;; illusion if a `setq'-like sequence of assignments.
      (let* ((c-buffer-is-cc-mode mode)
	     (source-pos
	      (or (assq sym c-lang-constants-under-evaluation)
		  (cons sym (vector source nil))))
	     ;; Append `c-lang-constants-under-evaluation' even if an
	     ;; earlier entry is found.  It's only necessary to get
	     ;; the recording of dependencies above correct.
	     (c-lang-constants-under-evaluation
	      (cons source-pos c-lang-constants-under-evaluation))
	     (fallback (get mode 'c-fallback-mode))
	     value
	     ;; Make sure the recursion limits aren't very low
	     ;; since the `c-lang-const' dependencies can go deep.
	     (max-specpdl-size (max max-specpdl-size 3000))
	     (max-lisp-eval-depth (max max-lisp-eval-depth 1000)))

	(if (if fallback
		(let ((backup-source-pos (copy-sequence (cdr source-pos))))
		  (and
		   ;; First try the original mode but don't accept an
		   ;; entry matching all languages since the fallback
		   ;; mode might have an explicit entry before that.
		   (eq (setq value (c-find-assignment-for-mode
				    (cdr source-pos) mode nil name))
		       c-lang-constants)
		   ;; Try again with the fallback mode from the
		   ;; original position.  Note that
		   ;; `c-buffer-is-cc-mode' still is the real mode if
		   ;; language parameterization takes place.
		   (eq (setq value (c-find-assignment-for-mode
				    (setcdr source-pos backup-source-pos)
				    fallback t name))
		       c-lang-constants)))
	      ;; A simple lookup with no fallback mode.
	      (eq (setq value (c-find-assignment-for-mode
			       (cdr source-pos) mode t name))
		  c-lang-constants))
	    (error
	     "`%s' got no (prior) value in %s (might be a cyclic reference)"
	     name mode))

	(condition-case err
	    (setq value (eval value))
	  (error
	   ;; Print a message to aid in locating the error.  We don't
	   ;; print the error itself since that will be done later by
	   ;; some caller higher up.
	   (message "Eval error in the `c-lang-defconst' for `%s' in %s:"
		    sym mode)
	   (makunbound sym)
	   (signal (car err) (cdr err))))

	(set sym (cons (cons mode value) (symbol-value sym)))
	value))))

(defun c-find-assignment-for-mode (source-pos mode match-any-lang name)
  ;; Find the first assignment entry that applies to MODE at or after
  ;; SOURCE-POS.  If MATCH-ANY-LANG is non-nil, entries with `t' as
  ;; the language list are considered to match, otherwise they don't.
  ;; On return SOURCE-POS is updated to point to the next assignment
  ;; after the returned one.  If no assignment is found,
  ;; `c-lang-constants' is returned as a magic value.
  ;;
  ;; SOURCE-POS is a vector that points out a specific assignment in
  ;; the double alist that's used in the `source' property.  The first
  ;; element is the position in the top alist which is indexed with
  ;; the source files, and the second element is the position in the
  ;; nested bindings alist.
  ;;
  ;; NAME is only used for error messages.

  (catch 'found
    (let ((file-entry (elt source-pos 0))
	  (assignment-entry (elt source-pos 1))
	  assignment)

      (while (if assignment-entry
		 t
	       ;; Handled the last assignment from one file, begin on the
	       ;; next.  Due to the check in `c-lang-defconst', we know
	       ;; there's at least one.
	       (when file-entry

		 (unless (aset source-pos 1
			       (setq assignment-entry (cdar file-entry)))
		   ;; The file containing the source definitions has not
		   ;; been loaded.
		   (let ((file (symbol-name (caar file-entry)))
			 (c-lang-constants-under-evaluation nil))
		     ;;(message (concat "Loading %s to get the source "
		     ;;			"value for language constant %s")
		     ;;		file name)
		     (load file))

		   (unless (setq assignment-entry (cdar file-entry))
		     ;; The load didn't fill in the source for the
		     ;; constant as expected.  The situation is
		     ;; probably that a derived mode was written for
		     ;; and compiled with another version of CC Mode,
		     ;; and the requested constant isn't in the
		     ;; currently loaded one.  Put in a dummy
		     ;; assignment that matches no language.
		     (setcdr (car file-entry)
			     (setq assignment-entry (list (list nil))))))

		 (aset source-pos 0 (setq file-entry (cdr file-entry)))
		 t))

	(setq assignment (car assignment-entry))
	(aset source-pos 1
	      (setq assignment-entry (cdr assignment-entry)))

	(when (if (listp (car assignment))
		  (memq mode (car assignment))
		match-any-lang)
	  (throw 'found (cdr assignment))))

      c-lang-constants)))

(defun c-lang-major-mode-is (mode)
  ;; `c-major-mode-is' expands to a call to this function inside
  ;; `c-lang-defconst'.  Here we also match the mode(s) against any
  ;; fallback modes for the one in `c-buffer-is-cc-mode', so that
  ;; e.g. (c-major-mode-is 'c++-mode) is true in a derived language
  ;; that has c++-mode as base mode.
  (unless (listp mode)
    (setq mode (list mode)))
  (let (match (buf-mode c-buffer-is-cc-mode))
    (while (if (memq buf-mode mode)
	       (progn
		 (setq match t)
		 nil)
	     (setq buf-mode (get buf-mode 'c-fallback-mode))))
    match))


(cc-provide 'cc-defs)

;;; cc-defs.el ends here
