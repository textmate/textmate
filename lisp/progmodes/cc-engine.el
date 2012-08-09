;;; cc-engine.el --- core syntax guessing engine for CC mode

;; Copyright (C) 1985, 1987, 1992-2012  Free Software Foundation, Inc.

;; Authors:    2001- Alan Mackenzie
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

;; The functions which have docstring documentation can be considered
;; part of an API which other packages can use in CC Mode buffers.
;; Otoh, undocumented functions and functions with the documentation
;; in comments are considered purely internal and can change semantics
;; or even disappear in the future.
;;
;; (This policy applies to CC Mode as a whole, not just this file.  It
;; probably also applies to many other Emacs packages, but here it's
;; clearly spelled out.)

;; Hidden buffer changes
;;
;; Various functions in CC Mode use text properties for caching and
;; syntactic markup purposes, and those of them that might modify such
;; properties but still don't modify the buffer in a visible way are
;; said to do "hidden buffer changes".  They should be used within
;; `c-save-buffer-state' or a similar function that saves and restores
;; buffer modifiedness, disables buffer change hooks, etc.
;;
;; Interactive functions are assumed to not do hidden buffer changes,
;; except in the specific parts of them that do real changes.
;;
;; Lineup functions are assumed to do hidden buffer changes.  They
;; must not do real changes, though.
;;
;; All other functions that do hidden buffer changes have that noted
;; in their doc string or comment.
;;
;; The intention with this system is to avoid wrapping every leaf
;; function that do hidden buffer changes inside
;; `c-save-buffer-state'.  It should be used as near the top of the
;; interactive functions as possible.
;;
;; Functions called during font locking are allowed to do hidden
;; buffer changes since the font-lock package run them in a context
;; similar to `c-save-buffer-state' (in fact, that function is heavily
;; inspired by `save-buffer-state' in the font-lock package).

;; Use of text properties
;;
;; CC Mode uses several text properties internally to mark up various
;; positions, e.g. to improve speed and to eliminate glitches in
;; interactive refontification.
;;
;; Note: This doc is for internal use only.  Other packages should not
;; assume that these text properties are used as described here.
;;
;; 'category
;;   Used for "indirection".  With its help, some other property can
;;   be cheaply and easily switched on or off everywhere it occurs.
;;
;; 'syntax-table
;;   Used to modify the syntax of some characters.  It is used to
;;   mark the "<" and ">" of angle bracket parens with paren syntax, and
;;   to "hide" obtrusive characters in preprocessor lines.
;;
;;   This property is used on single characters and is therefore
;;   always treated as front and rear nonsticky (or start and end open
;;   in XEmacs vocabulary).  It's therefore installed on
;;   `text-property-default-nonsticky' if that variable exists (Emacs
;;   >= 21).
;;
;; 'c-is-sws and 'c-in-sws
;;   Used by `c-forward-syntactic-ws' and `c-backward-syntactic-ws' to
;;   speed them up.  See the comment blurb before `c-put-is-sws'
;;   below for further details.
;;
;; 'c-type
;;   This property is used on single characters to mark positions with
;;   special syntactic relevance of various sorts.  Its primary use is
;;   to avoid glitches when multiline constructs are refontified
;;   interactively (on font lock decoration level 3).  It's cleared in
;;   a region before it's fontified and is then put on relevant chars
;;   in that region as they are encountered during the fontification.
;;   The value specifies the kind of position:
;;
;;     'c-decl-arg-start
;;  	 Put on the last char of the token preceding each declaration
;;  	 inside a declaration style arglist (typically in a function
;;  	 prototype).
;;
;;     'c-decl-end
;;  	 Put on the last char of the token preceding a declaration.
;;  	 This is used in cases where declaration boundaries can't be
;;  	 recognized simply by looking for a token like ";" or "}".
;;  	 `c-type-decl-end-used' must be set if this is used (see also
;;  	 `c-find-decl-spots').
;;
;;     'c-<>-arg-sep
;;  	 Put on the commas that separate arguments in angle bracket
;;  	 arglists like C++ template arglists.
;;
;;     'c-decl-id-start and 'c-decl-type-start
;;  	 Put on the last char of the token preceding each declarator
;;  	 in the declarator list of a declaration.  They are also used
;;  	 between the identifiers cases like enum declarations.
;;  	 'c-decl-type-start is used when the declarators are types,
;;  	 'c-decl-id-start otherwise.
;;
;; 'c-awk-NL-prop
;;   Used in AWK mode to mark the various kinds of newlines.  See
;;   cc-awk.el.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require-when-compile 'cc-langs)
(cc-require 'cc-vars)

;; Silence the compiler.
(cc-bytecomp-defun buffer-syntactic-context) ; XEmacs


;; Make declarations for all the `c-lang-defvar' variables in cc-langs.

(defmacro c-declare-lang-variables ()
  `(progn
     ,@(apply 'nconc
	      (mapcar (lambda (init)
			`(,(if (elt init 2)
			       `(defvar ,(car init) nil ,(elt init 2))
			     `(defvar ,(car init) nil))
			  (make-variable-buffer-local ',(car init))))
		      (cdr c-lang-variable-inits)))))
(c-declare-lang-variables)


;;; Internal state variables.

;; Internal state of hungry delete key feature
(defvar c-hungry-delete-key nil)
(make-variable-buffer-local 'c-hungry-delete-key)

;; The electric flag (toggled by `c-toggle-electric-state').
;; If t, electric actions (like automatic reindentation, and (if
;; c-auto-newline is also set) auto newlining) will happen when an electric
;; key like `{' is pressed (or an electric keyword like `else').
(defvar c-electric-flag t)
(make-variable-buffer-local 'c-electric-flag)

;; Internal state of auto newline feature.
(defvar c-auto-newline nil)
(make-variable-buffer-local 'c-auto-newline)

;; Included in the mode line to indicate the active submodes.
;; (defvar c-submode-indicators nil)
;; (make-variable-buffer-local 'c-submode-indicators)

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))


;; Basic handling of preprocessor directives.

;; This is a dynamically bound cache used together with
;; `c-query-macro-start' and `c-query-and-set-macro-start'.  It only
;; works as long as point doesn't cross a macro boundary.
(defvar c-macro-start 'unknown)

(defsubst c-query-and-set-macro-start ()
  (if (symbolp c-macro-start)
      (setq c-macro-start (save-excursion
			    (c-save-buffer-state ()
			      (and (c-beginning-of-macro)
				   (point)))))
    c-macro-start))

(defsubst c-query-macro-start ()
  (if (symbolp c-macro-start)
      (save-excursion
	(c-save-buffer-state ()
	  (and (c-beginning-of-macro)
	       (point))))
    c-macro-start))

;; One element macro cache to cope with continual movement within very large
;; CPP macros.
(defvar c-macro-cache nil)
(make-variable-buffer-local 'c-macro-cache)
;; Nil or cons of the bounds of the most recent CPP form probed by
;; `c-beginning-of-macro', `c-end-of-macro' or `c-syntactic-end-of-macro'.
;; The cdr will be nil if we know only the start of the CPP form.
(defvar c-macro-cache-start-pos nil)
(make-variable-buffer-local 'c-macro-cache-start-pos)
;; The starting position from where we determined `c-macro-cache'.
(defvar c-macro-cache-syntactic nil)
(make-variable-buffer-local 'c-macro-cache-syntactic)
;; non-nil iff `c-macro-cache' has both elements set AND the cdr is at a
;; syntactic end of macro, not merely an apparent one.

(defun c-invalidate-macro-cache (beg end)
  ;; Called from a before-change function.  If the change region is before or
  ;; in the macro characterized by `c-macro-cache' etc., nullify it
  ;; appropriately.  BEG and END are the standard before-change-functions
  ;; parameters.  END isn't used.
  (cond
   ((null c-macro-cache))
   ((< beg (car c-macro-cache))
    (setq c-macro-cache nil
	  c-macro-cache-start-pos nil
	  c-macro-cache-syntactic nil))
   ((and (cdr c-macro-cache)
	 (< beg (cdr c-macro-cache)))
    (setcdr c-macro-cache nil)
    (setq c-macro-cache-start-pos beg
	  c-macro-cache-syntactic nil))))

(defun c-beginning-of-macro (&optional lim)
  "Go to the beginning of a preprocessor directive.
Leave point at the beginning of the directive and return t if in one,
otherwise return nil and leave point unchanged.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (let ((here (point)))
    (when c-opt-cpp-prefix
      (if (and (car c-macro-cache)
	       (>= (point) (car c-macro-cache))
	       (or (and (cdr c-macro-cache)
			(<= (point) (cdr c-macro-cache)))
		   (<= (point) c-macro-cache-start-pos)))
	  (unless (< (car c-macro-cache) (or lim (point-min)))
	    (progn (goto-char (max (or lim (point-min)) (car c-macro-cache)))
		   (setq c-macro-cache-start-pos
			 (max c-macro-cache-start-pos here))
		   t))
	(setq c-macro-cache nil
	      c-macro-cache-start-pos nil
	      c-macro-cache-syntactic nil)

	(save-restriction
	  (if lim (narrow-to-region lim (point-max)))
	  (beginning-of-line)
	  (while (eq (char-before (1- (point))) ?\\)
	    (forward-line -1))
	  (back-to-indentation)
	  (if (and (<= (point) here)
		   (looking-at c-opt-cpp-start))
	      (progn
		(setq c-macro-cache (cons (point) nil)
		      c-macro-cache-start-pos here)
		t)
	    (goto-char here)
	    nil))))))

(defun c-end-of-macro ()
  "Go to the end of a preprocessor directive.
More accurately, move the point to the end of the closest following
line that doesn't end with a line continuation backslash - no check is
done that the point is inside a cpp directive to begin with.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
   (if (and (cdr c-macro-cache)
	    (<= (point) (cdr c-macro-cache))
	    (>= (point) (car c-macro-cache)))
       (goto-char (cdr c-macro-cache))
     (unless (and (car c-macro-cache)
		  (<= (point) c-macro-cache-start-pos)
		  (>= (point) (car c-macro-cache)))
       (setq c-macro-cache nil
	     c-macro-cache-start-pos nil
	     c-macro-cache-syntactic nil))
     (while (progn
	      (end-of-line)
	      (when (and (eq (char-before) ?\\)
			 (not (eobp)))
		(forward-char)
		t)))
     (when (car c-macro-cache)
       (setcdr c-macro-cache (point)))))

(defun c-syntactic-end-of-macro ()
  ;; Go to the end of a CPP directive, or a "safe" pos just before.
  ;;
  ;; This is normally the end of the next non-escaped line.  A "safe"
  ;; position is one not within a string or comment.  (The EOL on a line
  ;; comment is NOT "safe").
  ;;
  ;; This function must only be called from the beginning of a CPP construct.
  ;;
  ;; Note that this function might do hidden buffer changes.  See the comment
  ;; at the start of cc-engine.el for more info.
  (let* ((here (point))
	 (there (progn (c-end-of-macro) (point)))
	 s)
    (unless c-macro-cache-syntactic
      (setq s (parse-partial-sexp here there))
      (while (and (or (nth 3 s)	 ; in a string
		      (nth 4 s)) ; in a comment (maybe at end of line comment)
		  (> there here))	; No infinite loops, please.
	(setq there (1- (nth 8 s)))
	(setq s (parse-partial-sexp here there)))
      (setq c-macro-cache-syntactic (car c-macro-cache)))
    (point)))

(defun c-forward-over-cpp-define-id ()
  ;; Assuming point is at the "#" that introduces a preprocessor
  ;; directive, it's moved forward to the end of the identifier which is
  ;; "#define"d (or whatever c-opt-cpp-macro-define specifies).  Non-nil
  ;; is returned in this case, in all other cases nil is returned and
  ;; point isn't moved.
  ;;
  ;; This function might do hidden buffer changes.
  (when (and c-opt-cpp-macro-define-id
	     (looking-at c-opt-cpp-macro-define-id))
    (goto-char (match-end 0))))

(defun c-forward-to-cpp-define-body ()
  ;; Assuming point is at the "#" that introduces a preprocessor
  ;; directive, it's moved forward to the start of the definition body
  ;; if it's a "#define" (or whatever c-opt-cpp-macro-define
  ;; specifies).  Non-nil is returned in this case, in all other cases
  ;; nil is returned and point isn't moved.
  ;;
  ;; This function might do hidden buffer changes.
  (when (and c-opt-cpp-macro-define-start
	     (looking-at c-opt-cpp-macro-define-start)
	     (not (= (match-end 0) (c-point 'eol))))
    (goto-char (match-end 0))))


;;; Basic utility functions.

(defun c-syntactic-content (from to paren-level)
  ;; Return the given region as a string where all syntactic
  ;; whitespace is removed or, where necessary, replaced with a single
  ;; space.  If PAREN-LEVEL is given then all parens in the region are
  ;; collapsed to "()", "[]" etc.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (let* ((parts (list nil)) (tail parts) pos in-paren)

	(while (re-search-forward c-syntactic-ws-start to t)
	  (goto-char (setq pos (match-beginning 0)))
	  (c-forward-syntactic-ws)
	  (if (= (point) pos)
	      (forward-char)

	    (when paren-level
	      (save-excursion
		(setq in-paren (= (car (parse-partial-sexp from pos 1)) 1)
		      pos (point))))

	    (if (and (> pos from)
		     (< (point) to)
		     (looking-at "\\w\\|\\s_")
		     (save-excursion
		       (goto-char (1- pos))
		       (looking-at "\\w\\|\\s_")))
		(progn
		  (setcdr tail (list (buffer-substring-no-properties from pos)
				     " "))
		  (setq tail (cddr tail)))
	      (setcdr tail (list (buffer-substring-no-properties from pos)))
	      (setq tail (cdr tail)))

	    (when in-paren
	      (when (= (car (parse-partial-sexp pos to -1)) -1)
		(setcdr tail (list (buffer-substring-no-properties
				    (1- (point)) (point))))
		(setq tail (cdr tail))))

	    (setq from (point))))

	(setcdr tail (list (buffer-substring-no-properties from to)))
	(apply 'concat (cdr parts))))))

(defun c-shift-line-indentation (shift-amt)
  ;; Shift the indentation of the current line with the specified
  ;; amount (positive inwards).  The buffer is modified only if
  ;; SHIFT-AMT isn't equal to zero.
  (let ((pos (- (point-max) (point)))
	(c-macro-start c-macro-start)
	tmp-char-inserted)
    (if (zerop shift-amt)
	nil
      ;; If we're on an empty line inside a macro, we take the point
      ;; to be at the current indentation and shift it to the
      ;; appropriate column. This way we don't treat the extra
      ;; whitespace out to the line continuation as indentation.
      (when (and (c-query-and-set-macro-start)
		 (looking-at "[ \t]*\\\\$")
		 (save-excursion
		   (skip-chars-backward " \t")
		   (bolp)))
	(insert ?x)
	(backward-char)
	(setq tmp-char-inserted t))
      (unwind-protect
	  (let ((col (current-indentation)))
	    (delete-region (c-point 'bol) (c-point 'boi))
	    (beginning-of-line)
	    (indent-to (+ col shift-amt)))
	(when tmp-char-inserted
	  (delete-char 1))))
    ;; If initial point was within line's indentation and we're not on
    ;; a line with a line continuation in a macro, position after the
    ;; indentation.  Else stay at same point in text.
    (if (and (< (point) (c-point 'boi))
	     (not tmp-char-inserted))
	(back-to-indentation)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defsubst c-keyword-sym (keyword)
  ;; Return non-nil if the string KEYWORD is a known keyword.  More
  ;; precisely, the value is the symbol for the keyword in
  ;; `c-keywords-obarray'.
  (intern-soft keyword c-keywords-obarray))

(defsubst c-keyword-member (keyword-sym lang-constant)
  ;; Return non-nil if the symbol KEYWORD-SYM, as returned by
  ;; `c-keyword-sym', is a member of LANG-CONSTANT, which is the name
  ;; of a language constant that ends with "-kwds".  If KEYWORD-SYM is
  ;; nil then the result is nil.
  (get keyword-sym lang-constant))

;; String syntax chars, suitable for skip-syntax-(forward|backward).
(defconst c-string-syntax (if (memq 'gen-string-delim c-emacs-features)
                              "\"|"
                            "\""))

;; Regexp matching string limit syntax.
(defconst c-string-limit-regexp (if (memq 'gen-string-delim c-emacs-features)
                                    "\\s\"\\|\\s|"
                                  "\\s\""))

;; Regexp matching WS followed by string limit syntax.
(defconst c-ws*-string-limit-regexp
  (concat "[ \t]*\\(" c-string-limit-regexp "\\)"))

;; Holds formatted error strings for the few cases where parse errors
;; are reported.
(defvar c-parsing-error nil)
(make-variable-buffer-local 'c-parsing-error)

(defun c-echo-parsing-error (&optional quiet)
  (when (and c-report-syntactic-errors c-parsing-error (not quiet))
    (c-benign-error "%s" c-parsing-error))
  c-parsing-error)

;; Faces given to comments and string literals.  This is used in some
;; situations to speed up recognition; it isn't mandatory that font
;; locking is in use.  This variable is extended with the face in
;; `c-doc-face-name' when fontification is activated in cc-fonts.el.
(defvar c-literal-faces
  (append '(font-lock-comment-face font-lock-string-face)
	  (when (facep 'font-lock-comment-delimiter-face)
	    ;; New in Emacs 22.
	    '(font-lock-comment-delimiter-face))))

(defsubst c-put-c-type-property (pos value)
  ;; Put a c-type property with the given value at POS.
  (c-put-char-property pos 'c-type value))

(defun c-clear-c-type-property (from to value)
  ;; Remove all occurrences of the c-type property that has the given
  ;; value in the region between FROM and TO.  VALUE is assumed to not
  ;; be nil.
  ;;
  ;; Note: This assumes that c-type is put on single chars only; it's
  ;; very inefficient if matching properties cover large regions.
  (save-excursion
    (goto-char from)
    (while (progn
	     (when (eq (get-text-property (point) 'c-type) value)
	       (c-clear-char-property (point) 'c-type))
	     (goto-char (next-single-property-change (point) 'c-type nil to))
	     (< (point) to)))))


;; Some debug tools to visualize various special positions.  This
;; debug code isn't as portable as the rest of CC Mode.

(cc-bytecomp-defun overlays-in)
(cc-bytecomp-defun overlay-get)
(cc-bytecomp-defun overlay-start)
(cc-bytecomp-defun overlay-end)
(cc-bytecomp-defun delete-overlay)
(cc-bytecomp-defun overlay-put)
(cc-bytecomp-defun make-overlay)

(defun c-debug-add-face (beg end face)
  (c-save-buffer-state ((overlays (overlays-in beg end)) overlay)
    (while overlays
      (setq overlay (car overlays)
	    overlays (cdr overlays))
      (when (eq (overlay-get overlay 'face) face)
	(setq beg (min beg (overlay-start overlay))
	      end (max end (overlay-end overlay)))
	(delete-overlay overlay)))
    (overlay-put (make-overlay beg end) 'face face)))

(defun c-debug-remove-face (beg end face)
  (c-save-buffer-state ((overlays (overlays-in beg end)) overlay
			(ol-beg beg) (ol-end end))
    (while overlays
      (setq overlay (car overlays)
	    overlays (cdr overlays))
      (when (eq (overlay-get overlay 'face) face)
	(setq ol-beg (min ol-beg (overlay-start overlay))
	      ol-end (max ol-end (overlay-end overlay)))
	(delete-overlay overlay)))
    (when (< ol-beg beg)
      (overlay-put (make-overlay ol-beg beg) 'face face))
    (when (> ol-end end)
      (overlay-put (make-overlay end ol-end) 'face face))))


;; `c-beginning-of-statement-1' and accompanying stuff.

;; KLUDGE ALERT: c-maybe-labelp is used to pass information between
;; c-crosses-statement-barrier-p and c-beginning-of-statement-1.  A
;; better way should be implemented, but this will at least shut up
;; the byte compiler.
(defvar c-maybe-labelp)

;; New awk-compatible version of c-beginning-of-statement-1, ACM 2002/6/22

;; Macros used internally in c-beginning-of-statement-1 for the
;; automaton actions.
(defmacro c-bos-push-state ()
  '(setq stack (cons (cons state saved-pos)
		     stack)))
(defmacro c-bos-pop-state (&optional do-if-done)
  `(if (setq state (car (car stack))
	     saved-pos (cdr (car stack))
	     stack (cdr stack))
       t
     ,do-if-done
     (throw 'loop nil)))
(defmacro c-bos-pop-state-and-retry ()
  '(throw 'loop (setq state (car (car stack))
		      saved-pos (cdr (car stack))
		      ;; Throw nil if stack is empty, else throw non-nil.
		      stack (cdr stack))))
(defmacro c-bos-save-pos ()
  '(setq saved-pos (vector pos tok ptok pptok)))
(defmacro c-bos-restore-pos ()
  '(unless (eq (elt saved-pos 0) start)
     (setq pos (elt saved-pos 0)
	   tok (elt saved-pos 1)
	   ptok (elt saved-pos 2)
	   pptok (elt saved-pos 3))
     (goto-char pos)
     (setq sym nil)))
(defmacro c-bos-save-error-info (missing got)
  `(setq saved-pos (vector pos ,missing ,got)))
(defmacro c-bos-report-error ()
  '(unless noerror
     (setq c-parsing-error
	   (format "No matching `%s' found for `%s' on line %d"
		   (elt saved-pos 1)
		   (elt saved-pos 2)
		   (1+ (count-lines (point-min)
				    (c-point 'bol (elt saved-pos 0))))))))

(defun c-beginning-of-statement-1 (&optional lim ignore-labels
					     noerror comma-delim)
  "Move to the start of the current statement or declaration, or to
the previous one if already at the beginning of one.  Only
statements/declarations on the same level are considered, i.e. don't
move into or out of sexps (not even normal expression parentheses).

If point is already at the earliest statement within braces or parens,
this function doesn't move back into any whitespace preceding it; it
returns 'same in this case.

Stop at statement continuation tokens like \"else\", \"catch\",
\"finally\" and the \"while\" in \"do ... while\" if the start point
is within the continuation.  If starting at such a token, move to the
corresponding statement start.  If at the beginning of a statement,
move to the closest containing statement if there is any.  This might
also stop at a continuation clause.

Labels are treated as part of the following statements if
IGNORE-LABELS is non-nil.  (FIXME: Doesn't work if we stop at a known
statement start keyword.)  Otherwise, each label is treated as a
separate statement.

Macros are ignored \(i.e. skipped over) unless point is within one, in
which case the content of the macro is treated as normal code.  Aside
from any normal statement starts found in it, stop at the first token
of the content in the macro, i.e. the expression of an \"#if\" or the
start of the definition in a \"#define\".  Also stop at start of
macros before leaving them.

Return:
'label          if stopped at a label or \"case...:\" or \"default:\";
'same           if stopped at the beginning of the current statement;
'up             if stepped to a containing statement;
'previous       if stepped to a preceding statement;
'beginning      if stepped from a statement continuation clause to
                its start clause; or
'macro          if stepped to a macro start.
Note that 'same and not 'label is returned if stopped at the same
label without crossing the colon character.

LIM may be given to limit the search.  If the search hits the limit,
point will be left at the closest following token, or at the start
position if that is less ('same is returned in this case).

NOERROR turns off error logging to `c-parsing-error'.

Normally only ';' and virtual semicolons are considered to delimit
statements, but if COMMA-DELIM is non-nil then ',' is treated
as a delimiter too.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  ;; The bulk of this function is a pushdown automaton that looks at statement
  ;; boundaries and the tokens (such as "while") in c-opt-block-stmt-key.  Its
  ;; purpose is to keep track of nested statements, ensuring that such
  ;; statements are skipped over in their entirety (somewhat akin to what C-M-p
  ;; does with nested braces/brackets/parentheses).
  ;;
  ;; Note: The position of a boundary is the following token.
  ;;
  ;; Beginning with the current token (the one following point), move back one
  ;; sexp at a time (where a sexp is, more or less, either a token or the
  ;; entire contents of a brace/bracket/paren pair).  Each time a statement
  ;; boundary is crossed or a "while"-like token is found, update the state of
  ;; the PDA.  Stop at the beginning of a statement when the stack (holding
  ;; nested statement info) is empty and the position has been moved.
  ;;
  ;; The following variables constitute the PDA:
  ;;
  ;; sym:    This is either the "while"-like token (e.g. 'for) we've just
  ;;         scanned back over, 'boundary if we've just gone back over a
  ;;         statement boundary, or nil otherwise.
  ;; state:  takes one of the values (nil else else-boundary while
  ;;         while-boundary catch catch-boundary).
  ;;         nil means "no "while"-like token yet scanned".
  ;;         'else, for example, means "just gone back over an else".
  ;;         'else-boundary means "just gone back over a statement boundary
  ;;         immediately after having gone back over an else".
  ;; saved-pos: A vector of either saved positions (tok ptok pptok, etc.) or
  ;;         of error reporting information.
  ;; stack:  The stack onto which the PDA pushes its state.  Each entry
  ;;         consists of a saved value of state and saved-pos.  An entry is
  ;;         pushed when we move back over a "continuation" token (e.g. else)
  ;;         and popped when we encounter the corresponding opening token
  ;;         (e.g. if).
  ;;
  ;;
  ;; The following diagram briefly outlines the PDA.
  ;;
  ;; Common state:
  ;;   "else": Push state, goto state `else'.
  ;;   "while": Push state, goto state `while'.
  ;;   "catch" or "finally": Push state, goto state `catch'.
  ;;   boundary: Pop state.
  ;;   other: Do nothing special.
  ;;
  ;; State `else':
  ;;   boundary: Goto state `else-boundary'.
  ;;   other: Error, pop state, retry token.
  ;;
  ;; State `else-boundary':
  ;;   "if": Pop state.
  ;;   boundary: Error, pop state.
  ;;   other: See common state.
  ;;
  ;; State `while':
  ;;   boundary: Save position, goto state `while-boundary'.
  ;;   other: Pop state, retry token.
  ;;
  ;; State `while-boundary':
  ;;   "do": Pop state.
  ;;   boundary: Restore position if it's not at start, pop state. [*see below]
  ;;   other: See common state.
  ;;
  ;; State `catch':
  ;;   boundary: Goto state `catch-boundary'.
  ;;   other: Error, pop state, retry token.
  ;;
  ;; State `catch-boundary':
  ;;   "try": Pop state.
  ;;   "catch": Goto state `catch'.
  ;;   boundary: Error, pop state.
  ;;   other: See common state.
  ;;
  ;; [*] In the `while-boundary' state, we had pushed a 'while state, and were
  ;; searching for a "do" which would have opened a do-while.  If we didn't
  ;; find it, we discard the analysis done since the "while", go back to this
  ;; token in the buffer and restart the scanning there, this time WITHOUT
  ;; pushing the 'while state onto the stack.
  ;;
  ;; In addition to the above there is some special handling of labels
  ;; and macros.

  (let ((case-fold-search nil)
	(start (point))
	macro-start
	(delims (if comma-delim '(?\; ?,) '(?\;)))
	(c-stmt-delim-chars (if comma-delim
				c-stmt-delim-chars-with-comma
			      c-stmt-delim-chars))
	c-in-literal-cache c-maybe-labelp after-case:-pos saved
	;; Current position.
	pos
	;; Position of last stmt boundary character (e.g. ;).
	boundary-pos
	;; The position of the last sexp or bound that follows the
	;; first found colon, i.e. the start of the nonlabel part of
	;; the statement.  It's `start' if a colon is found just after
	;; the start.
	after-labels-pos
	;; Like `after-labels-pos', but the first such position inside
	;; a label, i.e. the start of the last label before the start
	;; of the nonlabel part of the statement.
	last-label-pos
	;; The last position where a label is possible provided the
	;; statement started there.  It's nil as long as no invalid
	;; label content has been found (according to
	;; `c-nonlabel-token-key').  It's `start' if no valid label
	;; content was found in the label.  Note that we might still
	;; regard it a label if it starts with `c-label-kwds'.
	label-good-pos
	;; Putative positions of the components of a bitfield declaration,
	;; e.g. "int foo : NUM_FOO_BITS ;"
	bitfield-type-pos bitfield-id-pos bitfield-size-pos
	;; Symbol just scanned back over (e.g. 'while or 'boundary).
	;; See above.
	sym
	;; Current state in the automaton.  See above.
	state
	;; Current saved positions.  See above.
	saved-pos
	;; Stack of conses (state . saved-pos).
	stack
	;; Regexp which matches "for", "if", etc.
	(cond-key (or c-opt-block-stmt-key
		      "\\<\\>"))	; Matches nothing.
	;; Return value.
	(ret 'same)
	;; Positions of the last three sexps or bounds we've stopped at.
	tok ptok pptok)

    (save-restriction
      (if lim (narrow-to-region lim (point-max)))

      (if (save-excursion
	    (and (c-beginning-of-macro)
		 (/= (point) start)))
	  (setq macro-start (point)))

      ;; Try to skip back over unary operator characters, to register
      ;; that we've moved.
      (while (progn
	       (setq pos (point))
	       (c-backward-syntactic-ws)
	       ;; Protect post-++/-- operators just before a virtual semicolon.
	       (and (not (c-at-vsemi-p))
		    (/= (skip-chars-backward "-+!*&~@`#") 0))))

      ;; Skip back over any semicolon here.  If it was a bare semicolon, we're
      ;; done.  Later on we ignore the boundaries for statements that don't
      ;; contain any sexp.  The only thing that is affected is that the error
      ;; checking is a little less strict, and we really don't bother.
      (if (and (memq (char-before) delims)
	       (progn (forward-char -1)
		      (setq saved (point))
		      (c-backward-syntactic-ws)
		      (or (memq (char-before) delims)
			  (memq (char-before) '(?: nil))
			  (eq (char-syntax (char-before)) ?\()
			  (c-at-vsemi-p))))
	  (setq ret 'previous
		pos saved)

	;; Begin at start and not pos to detect macros if we stand
	;; directly after the #.
	(goto-char start)
	(if (looking-at "\\<\\|\\W")
	    ;; Record this as the first token if not starting inside it.
	    (setq tok start))


	;; The following while loop goes back one sexp (balanced parens,
	;; etc. with contents, or symbol or suchlike) each iteration.  This
	;; movement is accomplished with a call to c-backward-sexp approx 170
	;; lines below.
	;;
	;; The loop is exited only by throwing nil to the (catch 'loop ...):
	;; 1. On reaching the start of a macro;
	;; 2. On having passed a stmt boundary with the PDA stack empty;
	;; 3. On reaching the start of an Objective C method def;
	;; 4. From macro `c-bos-pop-state'; when the stack is empty;
	;; 5. From macro `c-bos-pop-state-and-retry' when the stack is empty.
	(while
	    (catch 'loop ;; Throw nil to break, non-nil to continue.
	      (cond
	       ;; Are we in a macro, just after the opening #?
	       ((save-excursion
		  (and macro-start	; Always NIL for AWK.
		       (progn (skip-chars-backward " \t")
			      (eq (char-before) ?#))
		       (progn (setq saved (1- (point)))
			      (beginning-of-line)
			      (not (eq (char-before (1- (point))) ?\\)))
		       (looking-at c-opt-cpp-start)
		       (progn (skip-chars-forward " \t")
			      (eq (point) saved))))
		(goto-char saved)
		(if (and (c-forward-to-cpp-define-body)
			 (progn (c-forward-syntactic-ws start)
				(< (point) start)))
		    ;; Stop at the first token in the content of the macro.
		    (setq pos (point)
			  ignore-labels t) ; Avoid the label check on exit.
		  (setq pos saved
			ret 'macro
			ignore-labels t))
		(throw 'loop nil))	; 1. Start of macro.

	       ;; Do a round through the automaton if we've just passed a
	       ;; statement boundary or passed a "while"-like token.
	       ((or sym
		    (and (looking-at cond-key)
			 (setq sym (intern (match-string 1)))))

		(when (and (< pos start) (null stack))
		  (throw 'loop nil))	; 2. Statement boundary.

		;; The PDA state handling.
                ;;
                ;; Refer to the description of the PDA in the opening
                ;; comments.  In the following OR form, the first leaf
                ;; attempts to handles one of the specific actions detailed
                ;; (e.g., finding token "if" whilst in state `else-boundary').
                ;; We drop through to the second leaf (which handles common
                ;; state) if no specific handler is found in the first cond.
                ;; If a parsing error is detected (e.g. an "else" with no
                ;; preceding "if"), we throw to the enclosing catch.
                ;;
                ;; Note that the (eq state 'else) means
		;; "we've just passed an else", NOT "we're looking for an
		;; else".
		(or (cond
		     ((eq state 'else)
		      (if (eq sym 'boundary)
			  (setq state 'else-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'else-boundary)
		      (cond ((eq sym 'if)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary)
			     (c-bos-report-error)
			     (c-bos-pop-state))))

		     ((eq state 'while)
		      (if (and (eq sym 'boundary)
			       ;; Since this can cause backtracking we do a
			       ;; little more careful analysis to avoid it:
			       ;; If there's a label in front of the while
			       ;; it can't be part of a do-while.
			       (not after-labels-pos))
			  (progn (c-bos-save-pos)
				 (setq state 'while-boundary))
			(c-bos-pop-state-and-retry))) ; Can't be a do-while

		     ((eq state 'while-boundary)
		      (cond ((eq sym 'do)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary) ; isn't a do-while
			     (c-bos-restore-pos) ; the position of the while
			     (c-bos-pop-state)))) ; no longer searching for do.

		     ((eq state 'catch)
		      (if (eq sym 'boundary)
			  (setq state 'catch-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'catch-boundary)
		      (cond
		       ((eq sym 'try)
			(c-bos-pop-state (setq ret 'beginning)))
		       ((eq sym 'catch)
			(setq state 'catch))
		       ((eq sym 'boundary)
			(c-bos-report-error)
			(c-bos-pop-state)))))

		    ;; This is state common.  We get here when the previous
		    ;; cond statement found no particular state handler.
		    (cond ((eq sym 'boundary)
			   ;; If we have a boundary at the start
			   ;; position we push a frame to go to the
			   ;; previous statement.
			   (if (>= pos start)
			       (c-bos-push-state)
			     (c-bos-pop-state)))
			  ((eq sym 'else)
			   (c-bos-push-state)
			   (c-bos-save-error-info 'if 'else)
			   (setq state 'else))
			  ((eq sym 'while)
			   ;; Is this a real while, or a do-while?
			   ;; The next `when' triggers unless we are SURE that
			   ;; the `while' is not the tail end of a `do-while'.
			   (when (or (not pptok)
				     (memq (char-after pptok) delims)
				     ;; The following kludge is to prevent
				     ;; infinite recursion when called from
				     ;; c-awk-after-if-for-while-condition-p,
				     ;; or the like.
				     (and (eq (point) start)
					  (c-vsemi-status-unknown-p))
				     (c-at-vsemi-p pptok))
			     ;; Since this can cause backtracking we do a
			     ;; little more careful analysis to avoid it: If
			     ;; the while isn't followed by a (possibly
			     ;; virtual) semicolon it can't be a do-while.
			     (c-bos-push-state)
			     (setq state 'while)))
			  ((memq sym '(catch finally))
			   (c-bos-push-state)
			   (c-bos-save-error-info 'try sym)
			   (setq state 'catch))))

		(when c-maybe-labelp
		  ;; We're either past a statement boundary or at the
		  ;; start of a statement, so throw away any label data
		  ;; for the previous one.
		  (setq after-labels-pos nil
			last-label-pos nil
			c-maybe-labelp nil))))

	      ;; Step to the previous sexp, but not if we crossed a
	      ;; boundary, since that doesn't consume an sexp.
	      (if (eq sym 'boundary)
		  (setq ret 'previous)

                ;; HERE IS THE SINGLE PLACE INSIDE THE PDA LOOP WHERE WE MOVE
		;; BACKWARDS THROUGH THE SOURCE.

		(c-backward-syntactic-ws)
		(let ((before-sws-pos (point))
		      ;; The end position of the area to search for statement
		      ;; barriers in this round.
		      (maybe-after-boundary-pos pos))

		  ;; Go back over exactly one logical sexp, taking proper
		  ;; account of macros and escaped EOLs.
		  (while
		      (progn
			(unless (c-safe (c-backward-sexp) t)
			  ;; Give up if we hit an unbalanced block.  Since the
			  ;; stack won't be empty the code below will report a
			  ;; suitable error.
			  (throw 'loop nil))
			(cond
			 ;; Have we moved into a macro?
			 ((and (not macro-start)
			       (c-beginning-of-macro))
			  ;; Have we crossed a statement boundary?  If not,
			  ;; keep going back until we find one or a "real" sexp.
			  (and
			   (save-excursion
			     (c-end-of-macro)
			     (not (c-crosses-statement-barrier-p
				   (point) maybe-after-boundary-pos)))
			   (setq maybe-after-boundary-pos (point))))
			 ;; Have we just gone back over an escaped NL?  This
			 ;; doesn't count as a sexp.
			 ((looking-at "\\\\$")))))

		  ;; Have we crossed a statement boundary?
		  (setq boundary-pos
			(cond
			 ;; Are we at a macro beginning?
			 ((and (not macro-start)
			       c-opt-cpp-prefix
			       (looking-at c-opt-cpp-prefix))
			  (save-excursion
			    (c-end-of-macro)
			    (c-crosses-statement-barrier-p
			     (point) maybe-after-boundary-pos)))
			 ;; Just gone back over a brace block?
			 ((and
			   (eq (char-after) ?{)
			   (not (c-looking-at-inexpr-block lim nil t)))
			  (save-excursion
			    (c-forward-sexp) (point)))
			 ;; Just gone back over some paren block?
			 ((looking-at "\\s\(")
			  (save-excursion
			    (goto-char (1+ (c-down-list-backward
					    before-sws-pos)))
			    (c-crosses-statement-barrier-p
			     (point) maybe-after-boundary-pos)))
			 ;; Just gone back over an ordinary symbol of some sort?
			 (t (c-crosses-statement-barrier-p
			     (point) maybe-after-boundary-pos))))

		  (when boundary-pos
		    (setq pptok ptok
			  ptok tok
			  tok boundary-pos
			  sym 'boundary)
		    ;; Like a C "continue".  Analyze the next sexp.
		    (throw 'loop t))))

	      ;; ObjC method def?
	      (when (and c-opt-method-key
			 (setq saved (c-in-method-def-p)))
		(setq pos saved
		      ignore-labels t)	; Avoid the label check on exit.
		(throw 'loop nil))	; 3. ObjC method def.

	      ;; Might we have a bitfield declaration, "<type> <id> : <size>"?
	      (if c-has-bitfields
		  (cond
		   ;; The : <size> and <id> fields?
		   ((and (numberp c-maybe-labelp)
			 (not bitfield-size-pos)
			 (save-excursion
			   (goto-char (or tok start))
			   (not (looking-at c-keywords-regexp)))
			 (not (looking-at c-keywords-regexp))
			 (not (c-punctuation-in (point) c-maybe-labelp)))
		    (setq bitfield-size-pos (or tok start)
			  bitfield-id-pos (point)))
		   ;; The <type> field?
		   ((and bitfield-id-pos
			 (not bitfield-type-pos))
		    (if (and (looking-at c-symbol-key) ; Can only be an integer type.  :-)
			     (not (looking-at c-not-primitive-type-keywords-regexp))
			     (not (c-punctuation-in (point) tok)))
			(setq bitfield-type-pos (point))
		      (setq bitfield-size-pos nil
			    bitfield-id-pos nil)))))

	      ;; Handle labels.
	      (unless (eq ignore-labels t)
		(when (numberp c-maybe-labelp)
		  ;; `c-crosses-statement-barrier-p' has found a colon, so we
		  ;; might be in a label now.  Have we got a real label
		  ;; (including a case label) or something like C++'s "public:"?
		  ;; A case label might use an expression rather than a token.
		  (setq after-case:-pos (or tok start))
		  (if (or (looking-at c-nonlabel-token-key) ; e.g. "while" or "'a'"
			  ;; Catch C++'s inheritance construct "class foo : bar".
			  (save-excursion
			    (and
			     (c-safe (c-backward-sexp) t)
			     (looking-at c-nonlabel-token-2-key))))
		      (setq c-maybe-labelp nil)
		    (if after-labels-pos ; Have we already encountered a label?
			(if (not last-label-pos)
			    (setq last-label-pos (or tok start)))
		      (setq after-labels-pos (or tok start)))
		    (setq c-maybe-labelp t
			  label-good-pos nil))) ; bogus "label"

		(when (and (not label-good-pos)	; i.e. no invalid "label"'s yet
						; been found.
			   (looking-at c-nonlabel-token-key)) ; e.g. "while :"
		  ;; We're in a potential label and it's the first
		  ;; time we've found something that isn't allowed in
		  ;; one.
		  (setq label-good-pos (or tok start))))

	      ;; We've moved back by a sexp, so update the token positions.
	      (setq sym nil
		    pptok ptok
		    ptok tok
		    tok (point)
		    pos tok) ; always non-nil
	      )		     ; end of (catch loop ....)
	  )		     ; end of sexp-at-a-time (while ....)

	;; If the stack isn't empty there might be errors to report.
	(while stack
	  (if (and (vectorp saved-pos) (eq (length saved-pos) 3))
	      (c-bos-report-error))
	  (setq saved-pos (cdr (car stack))
		stack (cdr stack)))

	(when (and (eq ret 'same)
		   (not (memq sym '(boundary ignore nil))))
	  ;; Need to investigate closer whether we've crossed
	  ;; between a substatement and its containing statement.
	  (if (setq saved (if (looking-at c-block-stmt-1-key)
			      ptok
			    pptok))
	      (cond ((> start saved) (setq pos saved))
		    ((= start saved) (setq ret 'up)))))

	(when (and (not ignore-labels)
		   (eq c-maybe-labelp t)
		   (not (eq ret 'beginning))
		   after-labels-pos
		   (not bitfield-type-pos) ; Bitfields take precedence over labels.
		   (or (not label-good-pos)
		       (<= label-good-pos pos)
		       (progn
			 (goto-char (if (and last-label-pos
					     (< last-label-pos start))
					last-label-pos
				      pos))
			 (looking-at c-label-kwds-regexp))))
	  ;; We're in a label.  Maybe we should step to the statement
	  ;; after it.
	  (if (< after-labels-pos start)
	      (setq pos after-labels-pos)
	    (setq ret 'label)
	    (if (and last-label-pos (< last-label-pos start))
		;; Might have jumped over several labels.  Go to the last one.
		(setq pos last-label-pos)))))

      ;; Have we got "case <expression>:"?
      (goto-char pos)
      (when (and after-case:-pos
		 (not (eq ret 'beginning))
		 (looking-at c-case-kwds-regexp))
	(if (< after-case:-pos start)
	    (setq pos after-case:-pos))
	(if (eq ret 'same)
	    (setq ret 'label)))

      ;; Skip over the unary operators that can start the statement.
      (while (progn
	       (c-backward-syntactic-ws)
	       ;; protect AWK post-inc/decrement operators, etc.
	       (and (not (c-at-vsemi-p (point)))
		    (/= (skip-chars-backward "-+!*&~@`#") 0)))
	(setq pos (point)))
      (goto-char pos)
      ret)))

(defun c-punctuation-in (from to)
  "Return non-nil if there is a non-comment non-macro punctuation character
between FROM and TO.  FROM must not be in a string or comment.  The returned
value is the position of the first such character."
  (save-excursion
    (goto-char from)
    (let ((pos (point)))
      (while (progn (skip-chars-forward c-symbol-chars to)
		    (c-forward-syntactic-ws to)
		    (> (point) pos))
	(setq pos (point))))
    (and (< (point) to) (point))))

(defun c-crosses-statement-barrier-p (from to)
  "Return non-nil if buffer positions FROM to TO cross one or more
statement or declaration boundaries.  The returned value is actually
the position of the earliest boundary char.  FROM must not be within
a string or comment.

The variable `c-maybe-labelp' is set to the position of the first `:' that
might start a label (i.e. not part of `::' and not preceded by `?').  If a
single `?' is found, then `c-maybe-labelp' is cleared.

For AWK, a statement which is terminated by an EOL (not a \; or a }) is
regarded as having a \"virtual semicolon\" immediately after the last token on
the line.  If this virtual semicolon is _at_ from, the function recognizes it.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (let* ((skip-chars
	  ;; If the current language has CPP macros, insert # into skip-chars.
	  (if c-opt-cpp-symbol
	      (concat (substring c-stmt-delim-chars 0 1) ; "^"
		      c-opt-cpp-symbol			 ; usually "#"
		      (substring c-stmt-delim-chars 1))	 ; e.g. ";{}?:"
	    c-stmt-delim-chars))
	 (non-skip-list
	  (append (substring skip-chars 1) nil)) ; e.g. (?# ?\; ?{ ?} ?? ?:)
	 lit-range vsemi-pos)
    (save-restriction
      (widen)
      (save-excursion
	(catch 'done
	  (goto-char from)
	  (while (progn (skip-chars-forward
			 skip-chars
			 (min to (c-point 'bonl)))
			(< (point) to))
	    (cond
	     ;; Virtual semicolon?
	     ((and (bolp)
		   (save-excursion
		     (progn
		       (if (setq lit-range (c-literal-limits from)) ; Have we landed in a string/comment?
			   (goto-char (car lit-range)))
		       (c-backward-syntactic-ws) ; ? put a limit here, maybe?
		       (setq vsemi-pos (point))
		       (c-at-vsemi-p))))
	      (throw 'done vsemi-pos))
	     ;; In a string/comment?
	     ((setq lit-range (c-literal-limits from))
	      (goto-char (cdr lit-range)))
	     ((eq (char-after) ?:)
	      (forward-char)
	      (if (and (eq (char-after) ?:)
		       (< (point) to))
		  ;; Ignore scope operators.
		  (forward-char)
		(setq c-maybe-labelp (1- (point)))))
	     ((eq (char-after) ??)
	      ;; A question mark.  Can't be a label, so stop
	      ;; looking for more : and ?.
	      (setq c-maybe-labelp nil
		    skip-chars (substring c-stmt-delim-chars 0 -2)))
	     ;; At a CPP construct?
	     ((and c-opt-cpp-symbol (looking-at c-opt-cpp-symbol)
		   (save-excursion
		     (forward-line 0)
		     (looking-at c-opt-cpp-prefix)))
	      (c-end-of-macro))
	     ((memq (char-after) non-skip-list)
	      (throw 'done (point)))))
	  ;; In trailing space after an as yet undetected virtual semicolon?
	  (c-backward-syntactic-ws from)
	  (if (and (< (point) to)
		   (c-at-vsemi-p))
	      (point)
	    nil))))))

(defun c-at-statement-start-p ()
  "Return non-nil if the point is at the first token in a statement
or somewhere in the syntactic whitespace before it.

A \"statement\" here is not restricted to those inside code blocks.
Any kind of declaration-like construct that occur outside function
bodies is also considered a \"statement\".

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (save-excursion
    (let ((end (point))
	  c-maybe-labelp)
      (c-syntactic-skip-backward (substring c-stmt-delim-chars 1) nil t)
      (or (bobp)
	  (eq (char-before) ?})
	  (and (eq (char-before) ?{)
	       (not (and c-special-brace-lists
			 (progn (backward-char)
				(c-looking-at-special-brace-list)))))
	  (c-crosses-statement-barrier-p (point) end)))))

(defun c-at-expression-start-p ()
  "Return non-nil if the point is at the first token in an expression or
statement, or somewhere in the syntactic whitespace before it.

An \"expression\" here is a bit different from the normal language
grammar sense: It's any sequence of expression tokens except commas,
unless they are enclosed inside parentheses of some kind.  Also, an
expression never continues past an enclosing parenthesis, but it might
contain parenthesis pairs of any sort except braces.

Since expressions never cross statement boundaries, this function also
recognizes statement beginnings, just like `c-at-statement-start-p'.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (save-excursion
    (let ((end (point))
	  (c-stmt-delim-chars c-stmt-delim-chars-with-comma)
	  c-maybe-labelp)
      (c-syntactic-skip-backward (substring c-stmt-delim-chars 1) nil t)
      (or (bobp)
	  (memq (char-before) '(?{ ?}))
	  (save-excursion (backward-char)
			  (looking-at "\\s("))
	  (c-crosses-statement-barrier-p (point) end)))))


;; A set of functions that covers various idiosyncrasies in
;; implementations of `forward-comment'.

;; Note: Some emacsen considers incorrectly that any line comment
;; ending with a backslash continues to the next line.  I can't think
;; of any way to work around that in a reliable way without changing
;; the buffer, though.  Suggestions welcome. ;) (No, temporarily
;; changing the syntax for backslash doesn't work since we must treat
;; escapes in string literals correctly.)

(defun c-forward-single-comment ()
  "Move forward past whitespace and the closest following comment, if any.
Return t if a comment was found, nil otherwise.  In either case, the
point is moved past the following whitespace.  Line continuations,
i.e. a backslashes followed by line breaks, are treated as whitespace.
The line breaks that end line comments are considered to be the
comment enders, so the point will be put on the beginning of the next
line if it moved past a line comment.

This function does not do any hidden buffer changes."

  (let ((start (point)))
    (when (looking-at "\\([ \t\n\r\f\v]\\|\\\\[\n\r]\\)+")
      (goto-char (match-end 0)))

    (when (forward-comment 1)
      (if (eobp)
	  ;; Some emacsen (e.g. XEmacs 21) return t when moving
	  ;; forwards at eob.
	  nil

	;; Emacs includes the ending newline in a b-style (c++)
	;; comment, but XEmacs doesn't.  We depend on the Emacs
	;; behavior (which also is symmetric).
	(if (and (eolp) (elt (parse-partial-sexp start (point)) 7))
	    (condition-case nil (forward-char 1)))

	t))))

(defsubst c-forward-comments ()
  "Move forward past all following whitespace and comments.
Line continuations, i.e. a backslashes followed by line breaks, are
treated as whitespace.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (while (or
	  ;; If forward-comment in at least XEmacs 21 is given a large
	  ;; positive value, it'll loop all the way through if it hits
	  ;; eob.
	  (and (forward-comment 5)
	       ;; Some emacsen (e.g. XEmacs 21) return t when moving
	       ;; forwards at eob.
	       (not (eobp)))

	  (when (looking-at "\\\\[\n\r]")
	    (forward-char 2)
	    t))))

(defun c-backward-single-comment ()
  "Move backward past whitespace and the closest preceding comment, if any.
Return t if a comment was found, nil otherwise.  In either case, the
point is moved past the preceding whitespace.  Line continuations,
i.e. a backslashes followed by line breaks, are treated as whitespace.
The line breaks that end line comments are considered to be the
comment enders, so the point cannot be at the end of the same line to
move over a line comment.

This function does not do any hidden buffer changes."

  (let ((start (point)))
    ;; When we got newline terminated comments, forward-comment in all
    ;; supported emacsen so far will stop at eol of each line not
    ;; ending with a comment when moving backwards.  This corrects for
    ;; that, and at the same time handles line continuations.
    (while (progn
	     (skip-chars-backward " \t\n\r\f\v")
	     (and (looking-at "[\n\r]")
		  (eq (char-before) ?\\)))
      (backward-char))

    (if (bobp)
	;; Some emacsen (e.g. Emacs 19.34) return t when moving
	;; backwards at bob.
	nil

      ;; Leave point after the closest following newline if we've
      ;; backed up over any above, since forward-comment won't move
      ;; backward over a line comment if point is at the end of the
      ;; same line.
      (re-search-forward "\\=\\s *[\n\r]" start t)

      (if (if (let (open-paren-in-column-0-is-defun-start) (forward-comment -1))
	      (if (eolp)
		  ;; If forward-comment above succeeded and we're at eol
		  ;; then the newline we moved over above didn't end a
		  ;; line comment, so we give it another go.
		  (let (open-paren-in-column-0-is-defun-start)
		    (forward-comment -1))
		t))

	  ;; Emacs <= 20 and XEmacs move back over the closer of a
	  ;; block comment that lacks an opener.
	  (if (looking-at "\\*/")
	      (progn (forward-char 2) nil)
	    t)))))

(defsubst c-backward-comments ()
  "Move backward past all preceding whitespace and comments.
Line continuations, i.e. a backslashes followed by line breaks, are
treated as whitespace.  The line breaks that end line comments are
considered to be the comment enders, so the point cannot be at the end
of the same line to move over a line comment.  Unlike
c-backward-syntactic-ws, this function doesn't move back over
preprocessor directives.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (let ((start (point)))
    (while (and
	    ;; `forward-comment' in some emacsen (e.g. XEmacs 21.4)
	    ;; return t when moving backwards at bob.
	    (not (bobp))

	    (if (let (open-paren-in-column-0-is-defun-start)
		  (forward-comment -1))
		(if (looking-at "\\*/")
		    ;; Emacs <= 20 and XEmacs move back over the
		    ;; closer of a block comment that lacks an opener.
		    (progn (forward-char 2) nil)
		  t)

	      ;; XEmacs treats line continuations as whitespace but
	      ;; only in the backward direction, which seems a bit
	      ;; odd.  Anyway, this is necessary for Emacs.
	      (when (and (looking-at "[\n\r]")
			 (eq (char-before) ?\\)
			 (< (point) start))
		(backward-char)
		t))))))


;; Tools for skipping over syntactic whitespace.

;; The following functions use text properties to cache searches over
;; large regions of syntactic whitespace.  It works as follows:
;;
;; o  If a syntactic whitespace region contains anything but simple
;;    whitespace (i.e. space, tab and line breaks), the text property
;;    `c-in-sws' is put over it.  At places where we have stopped
;;    within that region there's also a `c-is-sws' text property.
;;    That since there typically are nested whitespace inside that
;;    must be handled separately, e.g. whitespace inside a comment or
;;    cpp directive.  Thus, from one point with `c-is-sws' it's safe
;;    to jump to another point with that property within the same
;;    `c-in-sws' region.  It can be likened to a ladder where
;;    `c-in-sws' marks the bars and `c-is-sws' the rungs.
;;
;; o  The `c-is-sws' property is put on the simple whitespace chars at
;;    a "rung position" and also maybe on the first following char.
;;    As many characters as can be conveniently found in this range
;;    are marked, but no assumption can be made that the whole range
;;    is marked (it could be clobbered by later changes, for
;;    instance).
;;
;;    Note that some part of the beginning of a sequence of simple
;;    whitespace might be part of the end of a preceding line comment
;;    or cpp directive and must not be considered part of the "rung".
;;    Such whitespace is some amount of horizontal whitespace followed
;;    by a newline.  In the case of cpp directives it could also be
;;    two newlines with horizontal whitespace between them.
;;
;;    The reason to include the first following char is to cope with
;;    "rung positions" that doesn't have any ordinary whitespace.  If
;;    `c-is-sws' is put on a token character it does not have
;;    `c-in-sws' set simultaneously.  That's the only case when that
;;    can occur, and the reason for not extending the `c-in-sws'
;;    region to cover it is that the `c-in-sws' region could then be
;;    accidentally merged with a following one if the token is only
;;    one character long.
;;
;; o  On buffer changes the `c-in-sws' and `c-is-sws' properties are
;;    removed in the changed region.  If the change was inside
;;    syntactic whitespace that means that the "ladder" is broken, but
;;    a later call to `c-forward-sws' or `c-backward-sws' will use the
;;    parts on either side and use an ordinary search only to "repair"
;;    the gap.
;;
;;    Special care needs to be taken if a region is removed: If there
;;    are `c-in-sws' on both sides of it which do not connect inside
;;    the region then they can't be joined.  If e.g. a marked macro is
;;    broken, syntactic whitespace inside the new text might be
;;    marked.  If those marks would become connected with the old
;;    `c-in-sws' range around the macro then we could get a ladder
;;    with one end outside the macro and the other at some whitespace
;;    within it.
;;
;; The main motivation for this system is to increase the speed in
;; skipping over the large whitespace regions that can occur at the
;; top level in e.g. header files that contain a lot of comments and
;; cpp directives.  For small comments inside code it's probably
;; slower than using `forward-comment' straightforwardly, but speed is
;; not a significant factor there anyway.

; (defface c-debug-is-sws-face
;   '((t (:background "GreenYellow")))
;   "Debug face to mark the `c-is-sws' property.")
; (defface c-debug-in-sws-face
;   '((t (:underline t)))
;   "Debug face to mark the `c-in-sws' property.")

; (defun c-debug-put-sws-faces ()
;   ;; Put the sws debug faces on all the `c-is-sws' and `c-in-sws'
;   ;; properties in the buffer.
;   (interactive)
;   (save-excursion
;     (c-save-buffer-state (in-face)
;       (goto-char (point-min))
;       (setq in-face (if (get-text-property (point) 'c-is-sws)
; 			(point)))
;       (while (progn
; 	       (goto-char (next-single-property-change
; 			   (point) 'c-is-sws nil (point-max)))
; 	       (if in-face
; 		   (progn
; 		     (c-debug-add-face in-face (point) 'c-debug-is-sws-face)
; 		     (setq in-face nil))
; 		 (setq in-face (point)))
; 	       (not (eobp))))
;       (goto-char (point-min))
;       (setq in-face (if (get-text-property (point) 'c-in-sws)
; 			(point)))
;       (while (progn
; 	       (goto-char (next-single-property-change
; 			   (point) 'c-in-sws nil (point-max)))
; 	       (if in-face
; 		   (progn
; 		     (c-debug-add-face in-face (point) 'c-debug-in-sws-face)
; 		     (setq in-face nil))
; 		 (setq in-face (point)))
; 	       (not (eobp)))))))

(defmacro c-debug-sws-msg (&rest args)
  ;;`(message ,@args)
  )

(defmacro c-put-is-sws (beg end)
  ;; This macro does a hidden buffer change.
  `(let ((beg ,beg) (end ,end))
     (put-text-property beg end 'c-is-sws t)
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-add-face beg end 'c-debug-is-sws-face)))))

(defmacro c-put-in-sws (beg end)
  ;; This macro does a hidden buffer change.
  `(let ((beg ,beg) (end ,end))
     (put-text-property beg end 'c-in-sws t)
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-add-face beg end 'c-debug-in-sws-face)))))

(defmacro c-remove-is-sws (beg end)
  ;; This macro does a hidden buffer change.
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-is-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-is-sws-face)))))

(defmacro c-remove-in-sws (beg end)
  ;; This macro does a hidden buffer change.
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-in-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-in-sws-face)))))

(defmacro c-remove-is-and-in-sws (beg end)
  ;; This macro does a hidden buffer change.
  `(let ((beg ,beg) (end ,end))
     (remove-text-properties beg end '(c-is-sws nil c-in-sws nil))
     ,@(when (facep 'c-debug-is-sws-face)
	 `((c-debug-remove-face beg end 'c-debug-is-sws-face)
	   (c-debug-remove-face beg end 'c-debug-in-sws-face)))))

(defsubst c-invalidate-sws-region-after (beg end)
  ;; Called from `after-change-functions'.  Note that if
  ;; `c-forward-sws' or `c-backward-sws' are used outside
  ;; `c-save-buffer-state' or similar then this will remove the cache
  ;; properties right after they're added.
  ;;
  ;; This function does hidden buffer changes.

  (save-excursion
    ;; Adjust the end to remove the properties in any following simple
    ;; ws up to and including the next line break, if there is any
    ;; after the changed region. This is necessary e.g. when a rung
    ;; marked empty line is converted to a line comment by inserting
    ;; "//" before the line break. In that case the line break would
    ;; keep the rung mark which could make a later `c-backward-sws'
    ;; move into the line comment instead of over it.
    (goto-char end)
    (skip-chars-forward " \t\f\v")
    (when (and (eolp) (not (eobp)))
      (setq end (1+ (point)))))

  (when (and (= beg end)
	     (get-text-property beg 'c-in-sws)
	     (> beg (point-min))
	     (get-text-property (1- beg) 'c-in-sws))
    ;; Ensure that an `c-in-sws' range gets broken.  Note that it isn't
    ;; safe to keep a range that was continuous before the change.  E.g:
    ;;
    ;;    #define foo
    ;;         \
    ;;    bar
    ;;
    ;; There can be a "ladder" between "#" and "b".  Now, if the newline
    ;; after "foo" is removed then "bar" will become part of the cpp
    ;; directive instead of a syntactically relevant token.  In that
    ;; case there's no longer syntactic ws from "#" to "b".
    (setq beg (1- beg)))

  (c-debug-sws-msg "c-invalidate-sws-region-after [%s..%s]" beg end)
  (c-remove-is-and-in-sws beg end))

(defun c-forward-sws ()
  ;; Used by `c-forward-syntactic-ws' to implement the unbounded search.
  ;;
  ;; This function might do hidden buffer changes.

  (let (;; `rung-pos' is set to a position as early as possible in the
	;; unmarked part of the simple ws region.
	(rung-pos (point)) next-rung-pos rung-end-pos last-put-in-sws-pos
	rung-is-marked next-rung-is-marked simple-ws-end
	;; `safe-start' is set when it's safe to cache the start position.
	;; It's not set if we've initially skipped over comments and line
	;; continuations since we might have gone out through the end of a
	;; macro then.  This provision makes `c-forward-sws' not populate the
	;; cache in the majority of cases, but otoh is `c-backward-sws' by far
	;; more common.
	safe-start)

    ;; Skip simple ws and do a quick check on the following character to see
    ;; if it's anything that can't start syntactic ws, so we can bail out
    ;; early in the majority of cases when there just are a few ws chars.
    (skip-chars-forward " \t\n\r\f\v")
    (when (looking-at c-syntactic-ws-start)

      (setq rung-end-pos (min (1+ (point)) (point-max)))
      (if (setq rung-is-marked (text-property-any rung-pos rung-end-pos
						  'c-is-sws t))
	  ;; Find the last rung position to avoid setting properties in all
	  ;; the cases when the marked rung is complete.
	  ;; (`next-single-property-change' is certain to move at least one
	  ;; step forward.)
	  (setq rung-pos (1- (next-single-property-change
			      rung-is-marked 'c-is-sws nil rung-end-pos)))
	;; Got no marked rung here.  Since the simple ws might have started
	;; inside a line comment or cpp directive we must set `rung-pos' as
	;; high as possible.
	(setq rung-pos (point)))

      (while
	  (progn
	    (while
		(when (and rung-is-marked
			   (get-text-property (point) 'c-in-sws))

		  ;; The following search is the main reason that `c-in-sws'
		  ;; and `c-is-sws' aren't combined to one property.
		  (goto-char (next-single-property-change
			      (point) 'c-in-sws nil (point-max)))
		  (unless (get-text-property (point) 'c-is-sws)
		    ;; If the `c-in-sws' region extended past the last
		    ;; `c-is-sws' char we have to go back a bit.
		    (or (get-text-property (1- (point)) 'c-is-sws)
			(goto-char (previous-single-property-change
				    (point) 'c-is-sws)))
		    (backward-char))

		  (c-debug-sws-msg
		   "c-forward-sws cached move %s -> %s (max %s)"
		   rung-pos (point) (point-max))

		  (setq rung-pos (point))
		  (and (> (skip-chars-forward " \t\n\r\f\v") 0)
		       (not (eobp))))

	      ;; We'll loop here if there is simple ws after the last rung.
	      ;; That means that there's been some change in it and it's
	      ;; possible that we've stepped into another ladder, so extend
	      ;; the previous one to join with it if there is one, and try to
	      ;; use the cache again.
	      (c-debug-sws-msg
	       "c-forward-sws extending rung with [%s..%s] (max %s)"
	       (1+ rung-pos) (1+ (point)) (point-max))
	      (unless (get-text-property (point) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (point) (1+ (point))))
	      (c-put-is-sws (1+ rung-pos)
			    (1+ (point)))
	      (c-put-in-sws rung-pos
			    (setq rung-pos (point)
				  last-put-in-sws-pos rung-pos)))

	    (setq simple-ws-end (point))
	    (c-forward-comments)

	    (cond
	     ((/= (point) simple-ws-end)
	      ;; Skipped over comments.  Don't cache at eob in case the buffer
	      ;; is narrowed.
	      (not (eobp)))

	     ((save-excursion
		(and c-opt-cpp-prefix
		     (looking-at c-opt-cpp-start)
		     (progn (skip-chars-backward " \t")
			    (bolp))
		     (or (bobp)
			 (progn (backward-char)
				(not (eq (char-before) ?\\))))))
	      ;; Skip a preprocessor directive.
	      (end-of-line)
	      (while (and (eq (char-before) ?\\)
			  (= (forward-line 1) 0))
		(end-of-line))
	      (forward-line 1)
	      (setq safe-start t)
	      ;; Don't cache at eob in case the buffer is narrowed.
	      (not (eobp)))))

	;; We've searched over a piece of non-white syntactic ws.  See if this
	;; can be cached.
	(setq next-rung-pos (point))
	(skip-chars-forward " \t\n\r\f\v")
	(setq rung-end-pos (min (1+ (point)) (point-max)))

	(if (or
	     ;; Cache if we haven't skipped comments only, and if we started
	     ;; either from a marked rung or from a completely uncached
	     ;; position.
	     (and safe-start
		  (or rung-is-marked
		      (not (get-text-property simple-ws-end 'c-in-sws))))

	     ;; See if there's a marked rung in the encountered simple ws.  If
	     ;; so then we can cache, unless `safe-start' is nil.  Even then
	     ;; we need to do this to check if the cache can be used for the
	     ;; next step.
	     (and (setq next-rung-is-marked
			(text-property-any next-rung-pos rung-end-pos
					   'c-is-sws t))
		  safe-start))

	    (progn
	      (c-debug-sws-msg
	       "c-forward-sws caching [%s..%s] - [%s..%s] (max %s)"
	       rung-pos (1+ simple-ws-end) next-rung-pos rung-end-pos
	       (point-max))

	      ;; Remove the properties for any nested ws that might be cached.
	      ;; Only necessary for `c-is-sws' since `c-in-sws' will be set
	      ;; anyway.
	      (c-remove-is-sws (1+ simple-ws-end) next-rung-pos)
	      (unless (and rung-is-marked (= rung-pos simple-ws-end))
		(c-put-is-sws rung-pos
			      (1+ simple-ws-end))
		(setq rung-is-marked t))
	      (c-put-in-sws rung-pos
			    (setq rung-pos (point)
				  last-put-in-sws-pos rung-pos))
	      (unless (get-text-property (1- rung-end-pos) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (1- rung-end-pos) rung-end-pos))
	      (c-put-is-sws next-rung-pos
			    rung-end-pos))

	  (c-debug-sws-msg
	   "c-forward-sws not caching [%s..%s] - [%s..%s] (max %s)"
	   rung-pos (1+ simple-ws-end) next-rung-pos rung-end-pos
	   (point-max))

	  ;; Set `rung-pos' for the next rung.  It's the same thing here as
	  ;; initially, except that the rung position is set as early as
	  ;; possible since we can't be in the ending ws of a line comment or
	  ;; cpp directive now.
	  (if (setq rung-is-marked next-rung-is-marked)
	      (setq rung-pos (1- (next-single-property-change
				  rung-is-marked 'c-is-sws nil rung-end-pos)))
	    (setq rung-pos next-rung-pos))
	  (setq safe-start t)))

      ;; Make sure that the newly marked `c-in-sws' region doesn't connect to
      ;; another one after the point (which might occur when editing inside a
      ;; comment or macro).
      (when (eq last-put-in-sws-pos (point))
	(cond ((< last-put-in-sws-pos (point-max))
	       (c-debug-sws-msg
		"c-forward-sws clearing at %s for cache separation"
		last-put-in-sws-pos)
	       (c-remove-in-sws last-put-in-sws-pos
				(1+ last-put-in-sws-pos)))
	      (t
	       ;; If at eob we have to clear the last character before the end
	       ;; instead since the buffer might be narrowed and there might
	       ;; be a `c-in-sws' after (point-max).  In this case it's
	       ;; necessary to clear both properties.
	       (c-debug-sws-msg
		"c-forward-sws clearing thoroughly at %s for cache separation"
		(1- last-put-in-sws-pos))
	       (c-remove-is-and-in-sws (1- last-put-in-sws-pos)
				       last-put-in-sws-pos))))
      )))

(defun c-backward-sws ()
  ;; Used by `c-backward-syntactic-ws' to implement the unbounded search.
  ;;
  ;; This function might do hidden buffer changes.

  (let (;; `rung-pos' is set to a position as late as possible in the unmarked
	;; part of the simple ws region.
	(rung-pos (point)) next-rung-pos last-put-in-sws-pos
	rung-is-marked simple-ws-beg cmt-skip-pos)

    ;; Skip simple horizontal ws and do a quick check on the preceding
    ;; character to see if it's anything that can't end syntactic ws, so we can
    ;; bail out early in the majority of cases when there just are a few ws
    ;; chars.  Newlines are complicated in the backward direction, so we can't
    ;; skip over them.
    (skip-chars-backward " \t\f")
    (when (and (not (bobp))
	       (save-excursion
		 (backward-char)
		 (looking-at c-syntactic-ws-end)))

      ;; Try to find a rung position in the simple ws preceding point, so that
      ;; we can get a cache hit even if the last bit of the simple ws has
      ;; changed recently.
      (setq simple-ws-beg (point))
      (skip-chars-backward " \t\n\r\f\v")
      (if (setq rung-is-marked (text-property-any
				(point) (min (1+ rung-pos) (point-max))
				'c-is-sws t))
	  ;; `rung-pos' will be the earliest marked position, which means that
	  ;; there might be later unmarked parts in the simple ws region.
	  ;; It's not worth the effort to fix that; the last part of the
	  ;; simple ws is also typically edited often, so it could be wasted.
	  (goto-char (setq rung-pos rung-is-marked))
	(goto-char simple-ws-beg))

      (while
	  (progn
	    (while
		(when (and rung-is-marked
			   (not (bobp))
			   (get-text-property (1- (point)) 'c-in-sws))

		  ;; The following search is the main reason that `c-in-sws'
		  ;; and `c-is-sws' aren't combined to one property.
		  (goto-char (previous-single-property-change
			      (point) 'c-in-sws nil (point-min)))
		  (unless (get-text-property (point) 'c-is-sws)
		    ;; If the `c-in-sws' region extended past the first
		    ;; `c-is-sws' char we have to go forward a bit.
		    (goto-char (next-single-property-change
				(point) 'c-is-sws)))

		  (c-debug-sws-msg
		   "c-backward-sws cached move %s <- %s (min %s)"
		   (point) rung-pos (point-min))

		  (setq rung-pos (point))
		  (if (and (< (min (skip-chars-backward " \t\f\v")
				   (progn
				     (setq simple-ws-beg (point))
				     (skip-chars-backward " \t\n\r\f\v")))
			      0)
			   (setq rung-is-marked
				 (text-property-any (point) rung-pos
						    'c-is-sws t)))
		      t
		    (goto-char simple-ws-beg)
		    nil))

	      ;; We'll loop here if there is simple ws before the first rung.
	      ;; That means that there's been some change in it and it's
	      ;; possible that we've stepped into another ladder, so extend
	      ;; the previous one to join with it if there is one, and try to
	      ;; use the cache again.
	      (c-debug-sws-msg
	       "c-backward-sws extending rung with [%s..%s] (min %s)"
	       rung-is-marked rung-pos (point-min))
	      (unless (get-text-property (1- rung-pos) 'c-is-sws)
		;; Remove any `c-in-sws' property from the last char of
		;; the rung before we mark it with `c-is-sws', so that we
		;; won't connect with the remains of a broken "ladder".
		(c-remove-in-sws (1- rung-pos) rung-pos))
	      (c-put-is-sws rung-is-marked
			    rung-pos)
	      (c-put-in-sws rung-is-marked
			    (1- rung-pos))
	      (setq rung-pos rung-is-marked
		    last-put-in-sws-pos rung-pos))

	    (c-backward-comments)
	    (setq cmt-skip-pos (point))

	    (cond
	     ((and c-opt-cpp-prefix
		   (/= cmt-skip-pos simple-ws-beg)
		   (c-beginning-of-macro))
	      ;; Inside a cpp directive.  See if it should be skipped over.
	      (let ((cpp-beg (point)))

		;; Move back over all line continuations in the region skipped
		;; over by `c-backward-comments'.  If we go past it then we
		;; started inside the cpp directive.
		(goto-char simple-ws-beg)
		(beginning-of-line)
		(while (and (> (point) cmt-skip-pos)
			    (progn (backward-char)
				   (eq (char-before) ?\\)))
		  (beginning-of-line))

		(if (< (point) cmt-skip-pos)
		    ;; Don't move past the cpp directive if we began inside
		    ;; it.  Note that the position at the end of the last line
		    ;; of the macro is also considered to be within it.
		    (progn (goto-char cmt-skip-pos)
			   nil)

		  ;; It's worthwhile to spend a little bit of effort on finding
		  ;; the end of the macro, to get a good `simple-ws-beg'
		  ;; position for the cache.  Note that `c-backward-comments'
		  ;; could have stepped over some comments before going into
		  ;; the macro, and then `simple-ws-beg' must be kept on the
		  ;; same side of those comments.
		  (goto-char simple-ws-beg)
		  (skip-chars-backward " \t\n\r\f\v")
		  (if (eq (char-before) ?\\)
		      (forward-char))
		  (forward-line 1)
		  (if (< (point) simple-ws-beg)
		      ;; Might happen if comments after the macro were skipped
		      ;; over.
		      (setq simple-ws-beg (point)))

		  (goto-char cpp-beg)
		  t)))

	     ((/= (save-excursion
		    (skip-chars-forward " \t\n\r\f\v" simple-ws-beg)
		    (setq next-rung-pos (point)))
		  simple-ws-beg)
	      ;; Skipped over comments.  Must put point at the end of
	      ;; the simple ws at point since we might be after a line
	      ;; comment or cpp directive that's been partially
	      ;; narrowed out, and we can't risk marking the simple ws
	      ;; at the end of it.
	      (goto-char next-rung-pos)
	      t)))

	;; We've searched over a piece of non-white syntactic ws.  See if this
	;; can be cached.
	(setq next-rung-pos (point))
	(skip-chars-backward " \t\f\v")

	(if (or
	     ;; Cache if we started either from a marked rung or from a
	     ;; completely uncached position.
	     rung-is-marked
	     (not (get-text-property (1- simple-ws-beg) 'c-in-sws))

	     ;; Cache if there's a marked rung in the encountered simple ws.
	     (save-excursion
	       (skip-chars-backward " \t\n\r\f\v")
	       (text-property-any (point) (min (1+ next-rung-pos) (point-max))
				  'c-is-sws t)))

	    (progn
	      (c-debug-sws-msg
	       "c-backward-sws caching [%s..%s] - [%s..%s] (min %s)"
	       (point) (1+ next-rung-pos)
	       simple-ws-beg (min (1+ rung-pos) (point-max))
	       (point-min))

	      ;; Remove the properties for any nested ws that might be cached.
	      ;; Only necessary for `c-is-sws' since `c-in-sws' will be set
	      ;; anyway.
	      (c-remove-is-sws (1+ next-rung-pos) simple-ws-beg)
	      (unless (and rung-is-marked (= simple-ws-beg rung-pos))
		(let ((rung-end-pos (min (1+ rung-pos) (point-max))))
		  (unless (get-text-property (1- rung-end-pos) 'c-is-sws)
		    ;; Remove any `c-in-sws' property from the last char of
		    ;; the rung before we mark it with `c-is-sws', so that we
		    ;; won't connect with the remains of a broken "ladder".
		    (c-remove-in-sws (1- rung-end-pos) rung-end-pos))
		  (c-put-is-sws simple-ws-beg
				rung-end-pos)
		  (setq rung-is-marked t)))
	      (c-put-in-sws (setq simple-ws-beg (point)
				  last-put-in-sws-pos simple-ws-beg)
			    rung-pos)
	      (c-put-is-sws (setq rung-pos simple-ws-beg)
			    (1+ next-rung-pos)))

	  (c-debug-sws-msg
	   "c-backward-sws not caching [%s..%s] - [%s..%s] (min %s)"
	   (point) (1+ next-rung-pos)
	   simple-ws-beg (min (1+ rung-pos) (point-max))
	   (point-min))
	  (setq rung-pos next-rung-pos
		simple-ws-beg (point))
	  ))

      ;; Make sure that the newly marked `c-in-sws' region doesn't connect to
      ;; another one before the point (which might occur when editing inside a
      ;; comment or macro).
      (when (eq last-put-in-sws-pos (point))
	(cond ((< (point-min) last-put-in-sws-pos)
	       (c-debug-sws-msg
		"c-backward-sws clearing at %s for cache separation"
		(1- last-put-in-sws-pos))
	       (c-remove-in-sws (1- last-put-in-sws-pos)
				last-put-in-sws-pos))
	      ((> (point-min) 1)
	       ;; If at bob and the buffer is narrowed, we have to clear the
	       ;; character we're standing on instead since there might be a
	       ;; `c-in-sws' before (point-min).  In this case it's necessary
	       ;; to clear both properties.
	       (c-debug-sws-msg
		"c-backward-sws clearing thoroughly at %s for cache separation"
		last-put-in-sws-pos)
	       (c-remove-is-and-in-sws last-put-in-sws-pos
				       (1+ last-put-in-sws-pos)))))
      )))


;; Other whitespace tools
(defun c-partial-ws-p (beg end)
  ;; Is the region (beg end) WS, and is there WS (or BOB/EOB) next to the
  ;; region?  This is a "heuristic" function.  .....
  ;;
  ;; The motivation for the second bit is to check whether removing this
  ;; region would coalesce two symbols.
  ;;
  ;; FIXME!!!  This function doesn't check virtual semicolons in any way.  Be
  ;; careful about using this function for, e.g. AWK.  (2007/3/7)
  (save-excursion
    (let ((end+1 (min (1+ end) (point-max))))
      (or (progn (goto-char (max (point-min) (1- beg)))
		 (c-skip-ws-forward end)
		 (eq (point) end))
	  (progn (goto-char beg)
		 (c-skip-ws-forward end+1)
		 (eq (point) end+1))))))

;; A system for finding noteworthy parens before the point.

(defconst c-state-cache-too-far 5000)
;; A maximum comfortable scanning distance, e.g. between
;; `c-state-cache-good-pos' and "HERE" (where we call c-parse-state).  When
;; this distance is exceeded, we take "emergency measures", e.g. by clearing
;; the cache and starting again from point-min or a beginning of defun.  This
;; value can be tuned for efficiency or set to a lower value for testing.

(defvar c-state-cache nil)
(make-variable-buffer-local 'c-state-cache)
;; The state cache used by `c-parse-state' to cut down the amount of
;; searching.  It's the result from some earlier `c-parse-state' call.  See
;; `c-parse-state''s doc string for details of its structure.
;;
;; The use of the cached info is more effective if the next
;; `c-parse-state' call is on a line close by the one the cached state
;; was made at; the cache can actually slow down a little if the
;; cached state was made very far back in the buffer.  The cache is
;; most effective if `c-parse-state' is used on each line while moving
;; forward.

(defvar c-state-cache-good-pos 1)
(make-variable-buffer-local 'c-state-cache-good-pos)
;; This is a position where `c-state-cache' is known to be correct, or
;; nil (see below).  It's a position inside one of the recorded unclosed
;; parens or the top level, but not further nested inside any literal or
;; subparen that is closed before the last recorded position.
;;
;; The exact position is chosen to try to be close to yet earlier than
;; the position where `c-state-cache' will be called next.  Right now
;; the heuristic is to set it to the position after the last found
;; closing paren (of any type) before the line on which
;; `c-parse-state' was called.  That is chosen primarily to work well
;; with refontification of the current line.
;;
;; 2009-07-28: When `c-state-point-min' and the last position where
;; `c-parse-state' or for which `c-invalidate-state-cache' was called, are
;; both in the same literal, there is no such "good position", and
;; c-state-cache-good-pos is then nil.  This is the ONLY circumstance in which
;; it can be nil.  In this case, `c-state-point-min-literal' will be non-nil.
;;
;; 2009-06-12: In a brace desert, c-state-cache-good-pos may also be in
;; the middle of the desert, as long as it is not within a brace pair
;; recorded in `c-state-cache' or a paren/bracket pair.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We maintain a simple cache of positions which aren't in a literal, so as to
;; speed up testing for non-literality.
(defconst c-state-nonlit-pos-interval 3000)
;; The approximate interval between entries in `c-state-nonlit-pos-cache'.

(defvar c-state-nonlit-pos-cache nil)
(make-variable-buffer-local 'c-state-nonlit-pos-cache)
;; A list of buffer positions which are known not to be in a literal or a cpp
;; construct.  This is ordered with higher positions at the front of the list.
;; Only those which are less than `c-state-nonlit-pos-cache-limit' are valid.

(defvar c-state-nonlit-pos-cache-limit 1)
(make-variable-buffer-local 'c-state-nonlit-pos-cache-limit)
;; An upper limit on valid entries in `c-state-nonlit-pos-cache'.  This is
;; reduced by buffer changes, and increased by invocations of
;; `c-state-literal-at'.

(defvar c-state-semi-nonlit-pos-cache nil)
(make-variable-buffer-local 'c-state-semi-nonlit-pos-cache)
;; A list of buffer positions which are known not to be in a literal.  This is
;; ordered with higher positions at the front of the list.  Only those which
;; are less than `c-state-semi-nonlit-pos-cache-limit' are valid.

(defvar c-state-semi-nonlit-pos-cache-limit 1)
(make-variable-buffer-local 'c-state-semi-nonlit-pos-cache-limit)
;; An upper limit on valid entries in `c-state-semi-nonlit-pos-cache'.  This is
;; reduced by buffer changes, and increased by invocations of
;; `c-state-literal-at'.  FIXME!!!

(defsubst c-state-pp-to-literal (from to)
  ;; Do a parse-partial-sexp from FROM to TO, returning either
  ;;     (STATE TYPE (BEG . END))     if TO is in a literal; or
  ;;     (STATE)                      otherwise,
  ;; where STATE is the parsing state at TO, TYPE is the type of the literal
  ;; (one of 'c, 'c++, 'string) and (BEG . END) is the boundaries of the literal.
  ;;
  ;; Only elements 3 (in a string), 4 (in a comment), 5 (following a quote),
  ;; 7 (comment type) and 8 (start of comment/string) (and possibly 9) of
  ;; STATE are valid.
  (save-excursion
    (let ((s (parse-partial-sexp from to))
	  ty)
      (when (or (nth 3 s) (nth 4 s))	; in a string or comment
	(setq ty (cond
		  ((nth 3 s) 'string)
		  ((eq (nth 7 s) t) 'c++)
		  (t 'c)))
	(parse-partial-sexp (point) (point-max)
			    nil			 ; TARGETDEPTH
			    nil			 ; STOPBEFORE
			    s			 ; OLDSTATE
			    'syntax-table))	 ; stop at end of literal
      (if ty
	  `(,s ,ty (,(nth 8 s) . ,(point)))
	`(,s)))))

(defun c-state-safe-place (here)
  ;; Return a buffer position before HERE which is "safe", i.e. outside any
  ;; string, comment, or macro.
  ;;
  ;; NOTE: This function manipulates `c-state-nonlit-pos-cache'.  This cache
  ;; MAY NOT contain any positions within macros, since macros are frequently
  ;; turned into comments by use of the `c-cpp-delimiter' category properties.
  ;; We cannot rely on this mechanism whilst determining a cache pos since
  ;; this function is also called from outwith `c-parse-state'.
  (save-restriction
    (widen)
    (save-excursion
      (let ((c c-state-nonlit-pos-cache)
	    pos npos high-pos lit macro-beg macro-end)
	;; Trim the cache to take account of buffer changes.
	(while (and c (> (car c) c-state-nonlit-pos-cache-limit))
	  (setq c (cdr c)))
	(setq c-state-nonlit-pos-cache c)

	(while (and c (> (car c) here))
	  (setq high-pos (car c))
	  (setq c (cdr c)))
	(setq pos (or (car c) (point-min)))

	(unless high-pos
	  (while
	      ;; Add an element to `c-state-nonlit-pos-cache' each iteration.
	      (and
	       (<= (setq npos (+ pos c-state-nonlit-pos-interval)) here)

	       ;; Test for being in a literal.  If so, go to after it.
	       (progn
		 (setq lit (car (cddr (c-state-pp-to-literal pos npos))))
		 (or (null lit)
		     (prog1 (<= (cdr lit) here)
		       (setq npos (cdr lit)))))

	       ;; Test for being in a macro.  If so, go to after it.
	       (progn
		 (goto-char npos)
		 (setq macro-beg
		       (and (c-beginning-of-macro) (/= (point) npos) (point)))
		 (when macro-beg
		   (c-syntactic-end-of-macro)
		   (or (eobp) (forward-char))
		   (setq macro-end (point)))
		 (or (null macro-beg)
		     (prog1 (<= macro-end here)
		       (setq npos macro-end)))))

	    (setq pos npos)
	    (setq c-state-nonlit-pos-cache (cons pos c-state-nonlit-pos-cache)))
	  ;; Add one extra element above HERE so as to to avoid the previous
	  ;; expensive calculation when the next call is close to the current
	  ;; one.  This is especially useful when inside a large macro.
	  (setq c-state-nonlit-pos-cache (cons npos c-state-nonlit-pos-cache)))

	(if (> pos c-state-nonlit-pos-cache-limit)
	    (setq c-state-nonlit-pos-cache-limit pos))
	pos))))

(defun c-state-semi-safe-place (here)
  ;; Return a buffer position before HERE which is "safe", i.e. outside any
  ;; string or comment.  It may be in a macro.
  (save-restriction
    (widen)
    (save-excursion
      (let ((c c-state-semi-nonlit-pos-cache)
	    pos npos high-pos lit macro-beg macro-end)
	;; Trim the cache to take account of buffer changes.
	(while (and c (> (car c) c-state-semi-nonlit-pos-cache-limit))
	  (setq c (cdr c)))
	(setq c-state-semi-nonlit-pos-cache c)
	
	(while (and c (> (car c) here))
	  (setq high-pos (car c))
	  (setq c (cdr c)))
	(setq pos (or (car c) (point-min)))
	
	(unless high-pos
	  (while
	      ;; Add an element to `c-state-semi-nonlit-pos-cache' each iteration.
	      (and
	       (<= (setq npos (+ pos c-state-nonlit-pos-interval)) here)
	       
	       ;; Test for being in a literal.  If so, go to after it.
	       (progn
		 (setq lit (car (cddr (c-state-pp-to-literal pos npos))))
		 (or (null lit)
		     (prog1 (<= (cdr lit) here)
		       (setq npos (cdr lit))))))
	       
	    (setq pos npos)
	    (setq c-state-semi-nonlit-pos-cache
		  (cons pos c-state-semi-nonlit-pos-cache))))

	(if (> pos c-state-semi-nonlit-pos-cache-limit)
	    (setq c-state-semi-nonlit-pos-cache-limit pos))
	pos))))

(defun c-state-literal-at (here)
  ;; If position HERE is inside a literal, return (START . END), the
  ;; boundaries of the literal (which may be outside the accessible bit of the
  ;; buffer).  Otherwise, return nil.
  ;;
  ;; This function is almost the same as `c-literal-limits'.  Previously, it
  ;; differed in that it was a lower level function, and that it rigorously
  ;; followed the syntax from BOB.  `c-literal-limits' is now (2011-12)
  ;; virtually identical to this function.
  (save-restriction
    (widen)
    (save-excursion
      (let ((pos (c-state-safe-place here)))
	    (car (cddr (c-state-pp-to-literal pos here)))))))

(defsubst c-state-lit-beg (pos)
  ;; Return the start of the literal containing POS, or POS itself.
  (or (car (c-state-literal-at pos))
      pos))

(defsubst c-state-cache-non-literal-place (pos state)
  ;; Return a position outside of a string/comment/macro at or before POS.
  ;; STATE is the parse-partial-sexp state at POS.
  (let ((res (if (or (nth 3 state)	; in a string?
		     (nth 4 state))	; in a comment?
		 (nth 8 state)
	       pos)))
    (save-excursion
      (goto-char res)
      (if (c-beginning-of-macro)
	  (point)
	res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff to do with point-min, and coping with any literal there.
(defvar c-state-point-min 1)
(make-variable-buffer-local 'c-state-point-min)
;; This is (point-min) when `c-state-cache' was last calculated.  A change of
;; narrowing is likely to affect the parens that are visible before the point.

(defvar c-state-point-min-lit-type nil)
(make-variable-buffer-local 'c-state-point-min-lit-type)
(defvar c-state-point-min-lit-start nil)
(make-variable-buffer-local 'c-state-point-min-lit-start)
;; These two variables define the literal, if any, containing point-min.
;; Their values are, respectively, 'string, c, or c++, and the start of the
;; literal.  If there's no literal there, they're both nil.

(defvar c-state-min-scan-pos 1)
(make-variable-buffer-local 'c-state-min-scan-pos)
;; This is the earliest buffer-pos from which scanning can be done.  It is
;; either the end of the literal containing point-min, or point-min itself.
;; It becomes nil if the buffer is changed earlier than this point.
(defun c-state-get-min-scan-pos ()
  ;; Return the lowest valid scanning pos.  This will be the end of the
  ;; literal enclosing point-min, or point-min itself.
  (or c-state-min-scan-pos
      (save-restriction
	(save-excursion
	  (widen)
	  (goto-char c-state-point-min-lit-start)
	  (if (eq c-state-point-min-lit-type 'string)
	      (forward-sexp)
	    (forward-comment 1))
	  (setq c-state-min-scan-pos (point))))))

(defun c-state-mark-point-min-literal ()
  ;; Determine the properties of any literal containing POINT-MIN, setting the
  ;; variables `c-state-point-min-lit-type', `c-state-point-min-lit-start',
  ;; and `c-state-min-scan-pos' accordingly.  The return value is meaningless.
  (let ((p-min (point-min))
	lit)
    (save-restriction
      (widen)
      (setq lit (c-state-literal-at p-min))
      (if lit
	  (setq c-state-point-min-lit-type
		(save-excursion
		  (goto-char (car lit))
		  (cond
		   ((looking-at c-block-comment-start-regexp) 'c)
		   ((looking-at c-line-comment-starter) 'c++)
		   (t 'string)))
		c-state-point-min-lit-start (car lit)
		c-state-min-scan-pos (cdr lit))
	(setq c-state-point-min-lit-type nil
	      c-state-point-min-lit-start nil
	      c-state-min-scan-pos p-min)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A variable which signals a brace dessert - helpful for reducing the number
;; of fruitless backward scans.
(defvar c-state-brace-pair-desert nil)
(make-variable-buffer-local 'c-state-brace-pair-desert)
;; Used only in `c-append-lower-brace-pair-to-state-cache'.  It is set when
;; that defun has searched backwards for a brace pair and not found one.  Its
;; value is either nil or a cons (PA . FROM), where PA is the position of the
;; enclosing opening paren/brace/bracket which bounds the backwards search (or
;; nil when at top level) and FROM is where the backward search started.  It
;; is reset to nil in `c-invalidate-state-cache'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lowish level functions/macros which work directly on `c-state-cache', or a
;; list of like structure.
(defmacro c-state-cache-top-lparen (&optional cache)
  ;; Return the address of the top left brace/bracket/paren recorded in CACHE
  ;; (default `c-state-cache') (or nil).
  (let ((cash (or cache 'c-state-cache)))
    `(if (consp (car ,cash))
	 (caar ,cash)
       (car ,cash))))

(defmacro c-state-cache-top-paren (&optional cache)
  ;; Return the address of the latest brace/bracket/paren (whether left or
  ;; right) recorded in CACHE (default `c-state-cache') or nil.
  (let ((cash (or cache 'c-state-cache)))
    `(if (consp (car ,cash))
	 (cdar ,cash)
       (car ,cash))))

(defmacro c-state-cache-after-top-paren (&optional cache)
  ;; Return the position just after the latest brace/bracket/paren (whether
  ;; left or right) recorded in CACHE (default `c-state-cache') or nil.
  (let ((cash (or cache 'c-state-cache)))
    `(if (consp (car ,cash))
	 (cdar ,cash)
       (and (car ,cash)
	    (1+ (car ,cash))))))

(defun c-get-cache-scan-pos (here)
  ;; From the state-cache, determine the buffer position from which we might
  ;; scan forward to HERE to update this cache.  This position will be just
  ;; after a paren/brace/bracket recorded in the cache, if possible, otherwise
  ;; return the earliest position in the accessible region which isn't within
  ;; a literal.  If the visible portion of the buffer is entirely within a
  ;; literal, return NIL.
  (let ((c c-state-cache) elt)
    ;(while (>= (or (c-state-cache-top-lparen c) 1) here)
    (while (and c
		(>= (c-state-cache-top-lparen c) here))
      (setq c (cdr c)))

    (setq elt (car c))
    (cond
     ((consp elt)
      (if (> (cdr elt) here)
	  (1+ (car elt))
	(cdr elt)))
     (elt (1+ elt))
     ((<= (c-state-get-min-scan-pos) here)
      (c-state-get-min-scan-pos))
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables which keep track of preprocessor constructs.
(defvar c-state-old-cpp-beg nil)
(make-variable-buffer-local 'c-state-old-cpp-beg)
(defvar c-state-old-cpp-end nil)
(make-variable-buffer-local 'c-state-old-cpp-end)
;; These are the limits of the macro containing point at the previous call of
;; `c-parse-state', or nil.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defuns which analyze the buffer, yet don't change `c-state-cache'.
(defun c-get-fallback-scan-pos (here)
  ;; Return a start position for building `c-state-cache' from
  ;; scratch.  This will be at the top level, 2 defuns back.
  (save-excursion
    ;; Go back 2 bods, but ignore any bogus positions returned by
    ;; beginning-of-defun (i.e. open paren in column zero).
    (goto-char here)
    (let ((cnt 2))
      (while (not (or (bobp) (zerop cnt)))
	(c-beginning-of-defun-1)	; Pure elisp BOD.
	(if (eq (char-after) ?\{)
	    (setq cnt (1- cnt)))))
    (point)))

(defun c-state-balance-parens-backwards (here- here+ top)
  ;; Return the position of the opening paren/brace/bracket before HERE- which
  ;; matches the outermost close p/b/b between HERE+ and TOP.  Except when
  ;; there's a macro, HERE- and HERE+ are the same.  Like this:
  ;;
  ;;	  ............................................
  ;;	  |				             |
  ;;	  (    [ ( .........#macro.. )      ( )  ]  )
  ;;	  ^		    ^	  ^			    ^
  ;;	  |		    |	  |			    |
  ;;   return		  HERE- HERE+			   TOP
  ;;
  ;; If there aren't enough opening paren/brace/brackets, return the position
  ;; of the outermost one found, or HERE- if there are none.  If there are no
  ;; closing p/b/bs between HERE+ and TOP, return HERE-.  HERE-/+ and TOP
  ;; must not be inside literals.  Only the accessible portion of the buffer
  ;; will be scanned.

  ;; PART 1: scan from `here+' up to `top', accumulating ")"s which enclose
  ;; `here'.  Go round the next loop each time we pass over such a ")".	 These
  ;; probably match "("s before `here-'.
  (let (pos pa ren+1 lonely-rens)
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) top) ; This can move point, sometimes.
	(setq pos here+)
	(c-safe
	  (while
	      (setq ren+1 (scan-lists pos 1 1)) ; might signal
	    (setq lonely-rens (cons ren+1 lonely-rens)
		  pos ren+1)))))

      ;; PART 2: Scan back before `here-' searching for the "("s
      ;; matching/mismatching the ")"s found above. We only need to direct the
      ;; caller to scan when we've encountered unmatched right parens.
    (setq pos here-)
    (when lonely-rens
      (c-safe
	(while
	    (and lonely-rens		; actual values aren't used.
		 (setq pa (scan-lists pos -1 1)))
	  (setq pos pa)
	  (setq lonely-rens (cdr lonely-rens)))))
    pos))

(defun c-parse-state-get-strategy (here good-pos)
  ;; Determine the scanning strategy for adjusting `c-parse-state', attempting
  ;; to minimize the amount of scanning.  HERE is the pertinent position in
  ;; the buffer, GOOD-POS is a position where `c-state-cache' (possibly with
  ;; its head trimmed) is known to be good, or nil if there is no such
  ;; position.
  ;;
  ;; The return value is a list, one of the following:
  ;;
  ;; o - ('forward CACHE-POS START-POINT) - scan forward from START-POINT,
  ;;                                        which is not less than CACHE-POS.
  ;; o - ('backward CACHE-POS nil) - scan backwards (from HERE).
  ;; o - ('BOD nil START-POINT) - scan forwards from START-POINT, which is at the
  ;;   top level.
  ;; o - ('IN-LIT nil nil) - point is inside the literal containing point-min.
  ;; , where CACHE-POS is the highest position recorded in `c-state-cache' at
  ;; or below HERE.
  (let ((cache-pos (c-get-cache-scan-pos here))	; highest position below HERE in cache (or 1)
	BOD-pos		    ; position of 2nd BOD before HERE.
	strategy	    ; 'forward, 'backward, 'BOD, or 'IN-LIT.
	start-point
	how-far)			; putative scanning distance.
    (setq good-pos (or good-pos (c-state-get-min-scan-pos)))
    (cond
     ((< here (c-state-get-min-scan-pos))
      (setq strategy 'IN-LIT
	    start-point nil
	    cache-pos nil
	    how-far 0))
     ((<= good-pos here)
      (setq strategy 'forward
	    start-point (max good-pos cache-pos)
	    how-far (- here start-point)))
     ((< (- good-pos here) (- here cache-pos)) ; FIXME!!! ; apply some sort of weighting.
      (setq strategy 'backward
	    how-far (- good-pos here)))
     (t
      (setq strategy 'forward
	      how-far (- here cache-pos)
	      start-point cache-pos)))

    ;; Might we be better off starting from the top level, two defuns back,
    ;; instead?
    (when (> how-far c-state-cache-too-far)
      (setq BOD-pos (c-get-fallback-scan-pos here)) ; somewhat EXPENSIVE!!!
      (if (< (- here BOD-pos) how-far)
	  (setq strategy 'BOD
		start-point BOD-pos)))

    (list
     strategy
     (and (memq strategy '(forward backward)) cache-pos)
     (and (memq strategy '(forward BOD)) start-point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines which change `c-state-cache' and associated values.
(defun c-renarrow-state-cache ()
  ;; The region (more precisely, point-min) has changed since we
  ;; calculated `c-state-cache'.  Amend `c-state-cache' accordingly.
  (if (< (point-min) c-state-point-min)
      ;; If point-min has MOVED BACKWARDS then we drop the state completely.
      ;; It would be possible to do a better job here and recalculate the top
      ;; only.
      (progn
	(c-state-mark-point-min-literal)
	(setq c-state-cache nil
	      c-state-cache-good-pos c-state-min-scan-pos
	      c-state-brace-pair-desert nil))

    ;; point-min has MOVED FORWARD.

    ;; Is the new point-min inside a (different) literal?
    (unless (and c-state-point-min-lit-start ; at prev. point-min
		 (< (point-min) (c-state-get-min-scan-pos)))
      (c-state-mark-point-min-literal))

    ;; Cut off a bit of the tail from `c-state-cache'.
    (let ((ptr (cons nil c-state-cache))
	  pa)
      (while (and (setq pa (c-state-cache-top-lparen (cdr ptr)))
		  (>= pa (point-min)))
	(setq ptr (cdr ptr)))

      (when (consp ptr)
	(if (eq (cdr ptr) c-state-cache)
	    (setq c-state-cache nil
		  c-state-cache-good-pos c-state-min-scan-pos)
	  (setcdr ptr nil)
	  (setq c-state-cache-good-pos (1+ (c-state-cache-top-lparen))))
	)))

  (setq c-state-point-min (point-min)))

(defun c-append-lower-brace-pair-to-state-cache (from &optional upper-lim)
  ;; If there is a brace pair preceding FROM in the buffer (not necessarily
  ;; immediately preceding), push a cons onto `c-state-cache' to represent it.
  ;; FROM must not be inside a literal.  If UPPER-LIM is non-nil, we append
  ;; the highest brace pair whose "}" is below UPPER-LIM.
  ;;
  ;; Return non-nil when this has been done.
  ;;
  ;; This routine should be fast.  Since it can get called a LOT, we maintain
  ;; `c-state-brace-pair-desert', a small cache of "failures", such that we
  ;; reduce the time wasted in repeated fruitless searches in brace deserts.
  (save-excursion
    (save-restriction
      (let ((bra from) ce		; Positions of "{" and "}".
	    new-cons
	    (cache-pos (c-state-cache-top-lparen)) ; might be nil.
	    (macro-start-or-from
	     (progn (goto-char from)
		    (c-beginning-of-macro)
		    (point))))
	(or upper-lim (setq upper-lim from))

	;; If we're essentially repeating a fruitless search, just give up.
	(unless (and c-state-brace-pair-desert
		     (eq cache-pos (car c-state-brace-pair-desert))
		     (<= from (cdr c-state-brace-pair-desert)))
	  ;; Only search what we absolutely need to:
	  (if (and c-state-brace-pair-desert
		   (eq cache-pos (car c-state-brace-pair-desert)))
	      (narrow-to-region (cdr c-state-brace-pair-desert) (point-max)))

	  ;; In the next pair of nested loops, the inner one moves back past a
	  ;; pair of (mis-)matching parens or brackets; the outer one moves
	  ;; back over a sequence of unmatched close brace/paren/bracket each
	  ;; time round.
	  (while
	      (progn
		(c-safe
		  (while
		      (and (setq ce (scan-lists bra -1 -1)) ; back past )/]/}; might signal
			   (setq bra (scan-lists ce -1 1)) ; back past (/[/{; might signal
			   (or (> ce upper-lim)
			       (not (eq (char-after bra) ?\{))
			       (and (goto-char bra)
				    (c-beginning-of-macro)
				    (< (point) macro-start-or-from))))))
		(and ce (< ce bra)))
	    (setq bra ce))	; If we just backed over an unbalanced closing
					; brace, ignore it.

	  (if (and ce (< bra ce) (eq (char-after bra) ?\{))
	      ;; We've found the desired brace-pair.
	      (progn
		(setq new-cons (cons bra (1+ ce)))
		(cond
		 ((consp (car c-state-cache))
		  (setcar c-state-cache new-cons))
		 ((and (numberp (car c-state-cache)) ; probably never happens
		       (< ce (car c-state-cache)))
		  (setcdr c-state-cache
			  (cons new-cons (cdr c-state-cache))))
		 (t (setq c-state-cache (cons new-cons c-state-cache)))))

	    ;; We haven't found a brace pair.  Record this.
	    (setq c-state-brace-pair-desert (cons cache-pos from))))))))

(defsubst c-state-push-any-brace-pair (bra+1 macro-start-or-here)
  ;; If BRA+1 is nil, do nothing.  Otherwise, BRA+1 is the buffer position
  ;; following a {, and that brace has a (mis-)matching } (or ]), and we
  ;; "push" "a" brace pair onto `c-state-cache'.
  ;;
  ;; Here "push" means overwrite the top element if it's itself a brace-pair,
  ;; otherwise push it normally.
  ;;
  ;; The brace pair we push is normally the one surrounding BRA+1, but if the
  ;; latter is inside a macro, not being a macro containing
  ;; MACRO-START-OR-HERE, we scan backwards through the buffer for a non-macro
  ;; base pair.  This latter case is assumed to be rare.
  ;;
  ;; Note: POINT is not preserved in this routine.
  (if bra+1
      (if (or (> bra+1 macro-start-or-here)
	      (progn (goto-char bra+1)
		     (not (c-beginning-of-macro))))
	  (setq c-state-cache
		(cons (cons (1- bra+1)
			    (scan-lists bra+1 1 1))
		      (if (consp (car c-state-cache))
			  (cdr c-state-cache)
			c-state-cache)))
	;; N.B.  This defsubst codes one method for the simple, normal case,
	;; and a more sophisticated, slower way for the general case.  Don't
	;; eliminate this defsubst - it's a speed optimization.
	(c-append-lower-brace-pair-to-state-cache (1- bra+1)))))

(defun c-append-to-state-cache (from)
  ;; Scan the buffer from FROM to (point-max), adding elements into
  ;; `c-state-cache' for braces etc.  Return a candidate for
  ;; `c-state-cache-good-pos'.
  ;;
  ;; FROM must be after the latest brace/paren/bracket in `c-state-cache', if
  ;; any.  Typically, it is immediately after it.  It must not be inside a
  ;; literal.
  (let ((here-bol (c-point 'bol (point-max)))
	(macro-start-or-here
	 (save-excursion (goto-char (point-max))
			 (if (c-beginning-of-macro)
			     (point)
			   (point-max))))
	pa+1		      ; pos just after an opening PAren (or brace).
	(ren+1 from)	      ; usually a pos just after an closing paREN etc.
			      ; Is actually the pos. to scan for a (/{/[ from,
			      ; which sometimes is after a silly )/}/].
	paren+1		      ; Pos after some opening or closing paren.
	paren+1s	      ; A list of `paren+1's; used to determine a
			      ; good-pos.
	bra+1 ce+1	      ; just after L/R bra-ces.
	bra+1s		      ; list of OLD values of bra+1.
	mstart)		      ; start of a macro.

    (save-excursion
      ;; Each time round the following loop, we enter a successively deeper
      ;; level of brace/paren nesting.  (Except sometimes we "continue at
      ;; the existing level".)  `pa+1' is a pos inside an opening
      ;; brace/paren/bracket, usually just after it.
      (while
	  (progn
	    ;; Each time round the next loop moves forward over an opening then
	    ;; a closing brace/bracket/paren.  This loop is white hot, so it
	    ;; plays ugly tricks to go fast.  DON'T PUT ANYTHING INTO THIS
	    ;; LOOP WHICH ISN'T ABSOLUTELY NECESSARY!!!  It terminates when a
	    ;; call of `scan-lists' signals an error, which happens when there
	    ;; are no more b/b/p's to scan.
	    (c-safe
	      (while t
		(setq pa+1 (scan-lists ren+1 1 -1) ; Into (/{/[; might signal
		      paren+1s (cons pa+1 paren+1s))
		(setq ren+1 (scan-lists pa+1 1 1)) ; Out of )/}/]; might signal
		(if (and (eq (char-before pa+1) ?{)) ; Check for a macro later.
		    (setq bra+1 pa+1))
		(setcar paren+1s ren+1)))

	    (if (and pa+1 (> pa+1 ren+1))
		;; We've just entered a deeper nesting level.
		(progn
		  ;; Insert the brace pair (if present) and the single open
		  ;; paren/brace/bracket into `c-state-cache' It cannot be
		  ;; inside a macro, except one around point, because of what
		  ;; `c-neutralize-syntax-in-CPP' has done.
		  (c-state-push-any-brace-pair bra+1 macro-start-or-here)
		  ;; Insert the opening brace/bracket/paren position.
		  (setq c-state-cache (cons (1- pa+1) c-state-cache))
		  ;; Clear admin stuff for the next more nested part of the scan.
		  (setq ren+1 pa+1  pa+1 nil  bra+1 nil  bra+1s nil)
		  t)			; Carry on the loop

	      ;; All open p/b/b's at this nesting level, if any, have probably
	      ;; been closed by matching/mismatching ones.  We're probably
	      ;; finished - we just need to check for having found an
	      ;; unmatched )/}/], which we ignore.  Such a )/}/] can't be in a
	      ;; macro, due the action of `c-neutralize-syntax-in-CPP'.
	      (c-safe (setq ren+1 (scan-lists ren+1 1 1)))))) ; acts as loop control.

      ;; Record the final, innermost, brace-pair if there is one.
      (c-state-push-any-brace-pair bra+1 macro-start-or-here)

      ;; Determine a good pos
      (while (and (setq paren+1 (car paren+1s))
		  (> (if (> paren+1 macro-start-or-here)
			 paren+1
		       (goto-char paren+1)
		       (setq mstart (and (c-beginning-of-macro)
					 (point)))
		       (or mstart paren+1))
		     here-bol))
	(setq paren+1s (cdr paren+1s)))
      (cond
       ((and paren+1 mstart)
	(min paren+1 mstart))
       (paren+1)
       (t from)))))

(defun c-remove-stale-state-cache (good-pos pps-point)
  ;; Remove stale entries from the `c-cache-state', i.e. those which will
  ;; not be in it when it is amended for position (point-max).
  ;; Additionally, the "outermost" open-brace entry before (point-max)
  ;; will be converted to a cons if the matching close-brace is scanned.
  ;;
  ;; GOOD-POS is a "maximal" "safe position" - there must be no open
  ;; parens/braces/brackets between GOOD-POS and (point-max).
  ;;
  ;; As a second thing, calculate the result of parse-partial-sexp at
  ;; PPS-POINT, w.r.t. GOOD-POS.  The motivation here is that
  ;; `c-state-cache-good-pos' may become PPS-POINT, but the caller may need to
  ;; adjust it to get outside a string/comment.  (Sorry about this!  The code
  ;; needs to be FAST).
  ;;
  ;; Return a list (GOOD-POS SCAN-BACK-POS PPS-STATE), where
  ;; o - GOOD-POS is a position where the new value `c-state-cache' is known
  ;;   to be good (we aim for this to be as high as possible);
  ;; o - SCAN-BACK-POS, if not nil, indicates there may be a brace pair
  ;;   preceding POS which needs to be recorded in `c-state-cache'.  It is a
  ;;   position to scan backwards from.
  ;; o - PPS-STATE is the parse-partial-sexp state at PPS-POINT.
  (save-restriction
    (narrow-to-region 1 (point-max))
    (save-excursion
      (let* ((in-macro-start   ; start of macro containing (point-max) or nil.
	      (save-excursion
		(goto-char (point-max))
		(and (c-beginning-of-macro)
		     (point))))
	     (good-pos-actual-macro-start ; Start of macro containing good-pos
					; or nil
	      (and (< good-pos (point-max))
		   (save-excursion
		     (goto-char good-pos)
		     (and (c-beginning-of-macro)
			  (point)))))
	     (good-pos-actual-macro-end	; End of this macro, (maybe
					; (point-max)), or nil.
	      (and good-pos-actual-macro-start
		   (save-excursion
		     (goto-char good-pos-actual-macro-start)
		     (c-end-of-macro)
		     (point))))
	     pps-state 			; Will be 9 or 10 elements long.
	     pos
	     upper-lim	   ; ,beyond which `c-state-cache' entries are removed
	     scan-back-pos
	     pair-beg pps-point-state target-depth)

	;; Remove entries beyond (point-max).  Also remove any entries inside
	;; a macro, unless (point-max) is in the same macro.
	(setq upper-lim
	      (if (or (null c-state-old-cpp-beg)
		      (and (> (point-max) c-state-old-cpp-beg)
			   (< (point-max) c-state-old-cpp-end)))
		  (point-max)
		(min (point-max) c-state-old-cpp-beg)))
	(while (and c-state-cache (>= (c-state-cache-top-lparen) upper-lim))
	  (setq c-state-cache (cdr c-state-cache)))
	;; If `upper-lim' is inside the last recorded brace pair, remove its
	;; RBrace and indicate we'll need to search backwards for a previous
	;; brace pair.
	(when (and c-state-cache
		   (consp (car c-state-cache))
		   (> (cdar c-state-cache) upper-lim))
	  (setcar c-state-cache (caar c-state-cache))
	  (setq scan-back-pos (car c-state-cache)))

	;; The next loop jumps forward out of a nested level of parens each
	;; time round; the corresponding elements in `c-state-cache' are
	;; removed.  `pos' is just after the brace-pair or the open paren at
	;; (car c-state-cache).  There can be no open parens/braces/brackets
	;; between `good-pos'/`good-pos-actual-macro-start' and (point-max),
	;; due to the interface spec to this function.
	(setq pos (if (and good-pos-actual-macro-end
			   (not (eq good-pos-actual-macro-start
				    in-macro-start)))
		      (1+ good-pos-actual-macro-end) ; get outside the macro as
					; marked by a `category' text property.
		    good-pos))
	(goto-char pos)
	(while (and c-state-cache
		    (< (point) (point-max)))
	  (cond
	   ((null pps-state)		; first time through
	    (setq target-depth -1))
	   ((eq (car pps-state) target-depth) ; found closing ),},]
	    (setq target-depth (1- (car pps-state))))
	   ;; Do nothing when we've merely reached pps-point.
	   )

	  ;; Scan!
	  (setq pps-state
		(parse-partial-sexp
		 (point) (if (< (point) pps-point) pps-point (point-max))
		 target-depth
		 nil pps-state))

	  (if (= (point) pps-point)
	      (setq pps-point-state pps-state))

	  (when (eq (car pps-state) target-depth)
	    (setq pos (point))	     ; POS is now just after an R-paren/brace.
	    (cond
	     ((and (consp (car c-state-cache))
		   (eq (point) (cdar c-state-cache)))
		;; We've just moved out of the paren pair containing the brace-pair
		;; at (car c-state-cache).  `pair-beg' is where the open paren is,
		;; and is potentially where the open brace of a cons in
		;; c-state-cache will be.
	      (setq pair-beg (car-safe (cdr c-state-cache))
		    c-state-cache (cdr-safe (cdr c-state-cache)))) ; remove {}pair + containing Lparen.
	     ((numberp (car c-state-cache))
	      (setq pair-beg (car c-state-cache)
		    c-state-cache (cdr c-state-cache))) ; remove this
					; containing Lparen
	     ((numberp (cadr c-state-cache))
	      (setq pair-beg (cadr c-state-cache)
		    c-state-cache (cddr c-state-cache))) ; Remove a paren pair
					; together with enclosed brace pair.
	     ;; (t nil)			; Ignore an unmated Rparen.
	     )))

	(if (< (point) pps-point)
	    (setq pps-state (parse-partial-sexp (point) pps-point
						nil nil ; TARGETDEPTH, STOPBEFORE
						pps-state)))

	;; If the last paren pair we moved out of was actually a brace pair,
	;; insert it into `c-state-cache'.
	(when (and pair-beg (eq (char-after pair-beg) ?{))
	  (if (consp (car-safe c-state-cache))
	      (setq c-state-cache (cdr c-state-cache)))
	  (setq c-state-cache (cons (cons pair-beg pos)
				    c-state-cache)))

	(list pos scan-back-pos pps-state)))))

(defun c-remove-stale-state-cache-backwards (here cache-pos)
  ;; Strip stale elements of `c-state-cache' by moving backwards through the
  ;; buffer, and inform the caller of the scenario detected.
  ;;
  ;; HERE is the position we're setting `c-state-cache' for.
  ;; CACHE-POS is just after the latest recorded position in `c-state-cache'
  ;;   before HERE, or a position at or near point-min which isn't in a
  ;;   literal.
  ;;
  ;; This function must only be called only when (> `c-state-cache-good-pos'
  ;; HERE).  Usually the gap between CACHE-POS and HERE is large.  It is thus
  ;; optimized to eliminate (or minimize) scanning between these two
  ;; positions.
  ;;
  ;; Return a three element list (GOOD-POS SCAN-BACK-POS FWD-FLAG), where:
  ;; o - GOOD-POS is a "good position", where `c-state-cache' is valid, or
  ;;   could become so after missing elements are inserted into
  ;;   `c-state-cache'.  This is JUST AFTER an opening or closing
  ;;   brace/paren/bracket which is already in `c-state-cache' or just before
  ;;   one otherwise.  exceptionally (when there's no such b/p/b handy) the BOL
  ;;   before `here''s line, or the start of the literal containing it.
  ;; o - SCAN-BACK-POS, if non-nil, indicates there may be a brace pair
  ;;   preceding POS which isn't recorded in `c-state-cache'.  It is a position
  ;;   to scan backwards from.
  ;; o - FWD-FLAG, if non-nil, indicates there may be parens/braces between
  ;;   POS and HERE which aren't recorded in `c-state-cache'.
  ;;
  ;; The comments in this defun use "paren" to mean parenthesis or square
  ;; bracket (as contrasted with a brace), and "(" and ")" likewise.
  ;;
  ;;    .   {..} (..) (..)  ( .. {   }  ) (...)    ( ....          .  ..)
  ;;    |                   |       |   |     |                    |
  ;;    CP                  E      here D     C                   good
  (let ((pos c-state-cache-good-pos)
	pa ren	       ; positions of "(" and ")"
	dropped-cons ; whether the last element dropped from `c-state-cache'
		     ; was a cons (representing a brace-pair)
	good-pos			; see above.
	lit	    ; (START . END) of a literal containing some point.
	here-lit-start here-lit-end	; bounds of literal containing `here'
					; or `here' itself.
	here- here+		     ; start/end of macro around HERE, or HERE
	(here-bol (c-point 'bol here))
	(too-far-back (max (- here c-state-cache-too-far) (point-min))))

    ;; Remove completely irrelevant entries from `c-state-cache'.
    (while (and c-state-cache
		(>= (setq pa (c-state-cache-top-lparen)) here))
      (setq dropped-cons (consp (car c-state-cache)))
      (setq c-state-cache (cdr c-state-cache))
      (setq pos pa))
    ;; At this stage, (> pos here);
    ;; (< (c-state-cache-top-lparen) here)  (or is nil).

    (cond
     ((and (consp (car c-state-cache))
	   (> (cdar c-state-cache) here))
      ;; CASE 1: The top of the cache is a brace pair which now encloses
      ;; `here'.  As good-pos, return the address. of the "{".  Since we've no
      ;; knowledge of what's inside these braces, we have no alternative but
      ;; to direct the caller to scan the buffer from the opening brace.
      (setq pos (caar c-state-cache))
      (setcar c-state-cache pos)
      (list (1+ pos) pos t)) ; return value.  We've just converted a brace pair
			     ; entry into a { entry, so the caller needs to
			     ; search for a brace pair before the {.

     ;; `here' might be inside a literal.  Check for this.
     ((progn
	(setq lit (c-state-literal-at here)
	      here-lit-start (or (car lit) here)
	      here-lit-end (or (cdr lit) here))
	;; Has `here' just "newly entered" a macro?
	(save-excursion
	  (goto-char here-lit-start)
	  (if (and (c-beginning-of-macro)
		   (or (null c-state-old-cpp-beg)
		       (not (= (point) c-state-old-cpp-beg))))
	      (progn
		(setq here- (point))
		(c-end-of-macro)
		(setq here+ (point)))
	    (setq here- here-lit-start
		  here+ here-lit-end)))

	;; `here' might be nested inside any depth of parens (or brackets but
	;; not braces).  Scan backwards to find the outermost such opening
	;; paren, if there is one.  This will be the scan position to return.
	(save-restriction
	  (narrow-to-region cache-pos (point-max))
	  (setq pos (c-state-balance-parens-backwards here- here+ pos)))
	nil))				; for the cond

     ((< pos here-lit-start)
      ;; CASE 2: Address of outermost ( or [ which now encloses `here', but
      ;; didn't enclose the (previous) `c-state-cache-good-pos'.  If there is
      ;; a brace pair preceding this, it will already be in `c-state-cache',
      ;; unless there was a brace pair after it, i.e. there'll only be one to
      ;; scan for if we've just deleted one.
      (list pos (and dropped-cons pos) t)) ; Return value.

      ;; `here' isn't enclosed in a (previously unrecorded) bracket/paren.
      ;; Further forward scanning isn't needed, but we still need to find a
      ;; GOOD-POS.  Step out of all enclosing "("s on HERE's line.
     ((progn
	(save-restriction
	  (narrow-to-region here-bol (point-max))
	  (setq pos here-lit-start)
	  (c-safe (while (setq pa (scan-lists pos -1 1))
		    (setq pos pa))))	; might signal
	nil))				; for the cond

     ((setq ren (c-safe-scan-lists pos -1 -1 too-far-back))
       ;; CASE 3: After a }/)/] before `here''s BOL.
      (list (1+ ren) (and dropped-cons pos) nil)) ; Return value

     (t
      ;; CASE 4; Best of a bad job: BOL before `here-bol', or beginning of
      ;; literal containing it.
      (setq good-pos (c-state-lit-beg (c-point 'bopl here-bol)))
      (list good-pos (and dropped-cons good-pos) nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Externally visible routines.

(defun c-state-cache-init ()
  (setq c-state-cache nil
	c-state-cache-good-pos 1
	c-state-nonlit-pos-cache nil
	c-state-nonlit-pos-cache-limit 1
	c-state-brace-pair-desert nil
	c-state-point-min 1
	c-state-point-min-lit-type nil
	c-state-point-min-lit-start nil
	c-state-min-scan-pos 1
	c-state-old-cpp-beg nil
	c-state-old-cpp-end nil)
  (c-state-mark-point-min-literal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging routines to dump `c-state-cache' in a "replayable" form.
;; (defmacro c-sc-de (elt) 		; "c-state-cache-dump-element"
;;   `(format ,(concat "(setq " (symbol-name elt) " %s)    ") ,elt))
;; (defmacro c-sc-qde (elt)		; "c-state-cache-quote-dump-element"
;;   `(format ,(concat "(setq " (symbol-name elt) " '%s)    ") ,elt))
;; (defun c-state-dump ()
;;   ;; For debugging.
;;   ;(message
;;   (concat
;;    (c-sc-qde c-state-cache)
;;    (c-sc-de c-state-cache-good-pos)
;;    (c-sc-qde c-state-nonlit-pos-cache)
;;    (c-sc-de c-state-nonlit-pos-cache-limit)
;;    (c-sc-qde c-state-brace-pair-desert)
;;    (c-sc-de c-state-point-min)
;;    (c-sc-de c-state-point-min-lit-type)
;;    (c-sc-de c-state-point-min-lit-start)
;;    (c-sc-de c-state-min-scan-pos)
;;    (c-sc-de c-state-old-cpp-beg)
;;    (c-sc-de c-state-old-cpp-end)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-invalidate-state-cache-1 (here)
  ;; Invalidate all info on `c-state-cache' that applies to the buffer at HERE
  ;; or higher and set `c-state-cache-good-pos' accordingly.  The cache is
  ;; left in a consistent state.
  ;;
  ;; This is much like `c-whack-state-after', but it never changes a paren
  ;; pair element into an open paren element.  Doing that would mean that the
  ;; new open paren wouldn't have the required preceding paren pair element.
  ;;
  ;; This function is called from c-after-change.

  ;; The caches of non-literals:
  (if (< here c-state-nonlit-pos-cache-limit)
      (setq c-state-nonlit-pos-cache-limit here))
  (if (< here c-state-semi-nonlit-pos-cache-limit)
      (setq c-state-semi-nonlit-pos-cache-limit here))

  ;; `c-state-cache':
  ;; Case 1: if `here' is in a literal containing point-min, everything
  ;; becomes (or is already) nil.
  (if (or (null c-state-cache-good-pos)
	  (< here (c-state-get-min-scan-pos)))
      (setq c-state-cache nil
	    c-state-cache-good-pos nil
	    c-state-min-scan-pos nil)

    ;; Truncate `c-state-cache' and set `c-state-cache-good-pos' to a value
    ;; below `here'.  To maintain its consistency, we may need to insert a new
    ;; brace pair.
    (let ((here-bol (c-point 'bol here))
	  too-high-pa		  ; recorded {/(/[ next above here, or nil.
	  dropped-cons		  ; was the last removed element a brace pair?
	  pa)
      ;; The easy bit - knock over-the-top bits off `c-state-cache'.
      (while (and c-state-cache
		  (>= (setq pa (c-state-cache-top-paren)) here))
	(setq dropped-cons (consp (car c-state-cache))
	      too-high-pa (c-state-cache-top-lparen)
	      c-state-cache (cdr c-state-cache)))

      ;; Do we need to add in an earlier brace pair, having lopped one off?
      (if (and dropped-cons
	       (< too-high-pa (+ here c-state-cache-too-far)))
	  (c-append-lower-brace-pair-to-state-cache too-high-pa here-bol))
      (setq c-state-cache-good-pos (or (c-state-cache-after-top-paren)
				       (c-state-get-min-scan-pos)))))

  ;; The brace-pair desert marker:
  (when (car c-state-brace-pair-desert)
    (if (< here (car c-state-brace-pair-desert))
	(setq c-state-brace-pair-desert nil)
      (if (< here (cdr c-state-brace-pair-desert))
	  (setcdr c-state-brace-pair-desert here)))))

(defun c-parse-state-1 ()
  ;; Find and record all noteworthy parens between some good point earlier in
  ;; the file and point.  That good point is at least the beginning of the
  ;; top-level construct we are in, or the beginning of the preceding
  ;; top-level construct if we aren't in one.
  ;;
  ;; The returned value is a list of the noteworthy parens with the last one
  ;; first.  If an element in the list is an integer, it's the position of an
  ;; open paren (of any type) which has not been closed before the point.  If
  ;; an element is a cons, it gives the position of a closed BRACE paren
  ;; pair[*]; the car is the start brace position and the cdr is the position
  ;; following the closing brace.  Only the last closed brace paren pair
  ;; before each open paren and before the point is recorded, and thus the
  ;; state never contains two cons elements in succession.  When a close brace
  ;; has no matching open brace (e.g., the matching brace is outside the
  ;; visible region), it is not represented in the returned value.
  ;;
  ;; [*] N.B. The close "brace" might be a mismatching close bracket or paren.
  ;; This defun explicitly treats mismatching parens/braces/brackets as
  ;; matching.  It is the open brace which makes it a "brace" pair.
  ;;
  ;; If POINT is within a macro, open parens and brace pairs within
  ;; THIS macro MIGHT be recorded.  This depends on whether their
  ;; syntactic properties have been suppressed by
  ;; `c-neutralize-syntax-in-CPP'.  This might need fixing (2008-12-11).
  ;;
  ;; Currently no characters which are given paren syntax with the
  ;; syntax-table property are recorded, i.e. angle bracket arglist
  ;; parens are never present here.  Note that this might change.
  ;;
  ;; BUG: This function doesn't cope entirely well with unbalanced
  ;; parens in macros.  (2008-12-11: this has probably been resolved
  ;; by the function `c-neutralize-syntax-in-CPP'.)  E.g. in the
  ;; following case the brace before the macro isn't balanced with the
  ;; one after it:
  ;;
  ;;     {
  ;;     #define X {
  ;;     }
  ;;
  ;; Note to maintainers: this function DOES get called with point
  ;; within comments and strings, so don't assume it doesn't!
  ;;
  ;; This function might do hidden buffer changes.
  (let* ((here (point))
	 (here-bopl (c-point 'bopl))
	 strategy	     ; 'forward, 'backward etc..
	 ;; Candidate positions to start scanning from:
	 cache-pos	     ; highest position below HERE already existing in
			     ; cache (or 1).
	 good-pos
	 start-point
	 bopl-state
	 res
	 scan-backward-pos scan-forward-p) ; used for 'backward.
    ;; If POINT-MIN has changed, adjust the cache
    (unless (= (point-min) c-state-point-min)
      (c-renarrow-state-cache))

    ;; Strategy?
    (setq res (c-parse-state-get-strategy here c-state-cache-good-pos)
	  strategy (car res)
	  cache-pos (cadr res)
	  start-point (nth 2 res))

    (when (eq strategy 'BOD)
      (setq c-state-cache nil
	    c-state-cache-good-pos start-point))

    ;; SCAN!
    (save-restriction
      (cond
       ((memq strategy '(forward BOD))
	(narrow-to-region (point-min) here)
	(setq res (c-remove-stale-state-cache start-point here-bopl))
	(setq cache-pos (car res)
	      scan-backward-pos (cadr res)
	      bopl-state (car (cddr res))) ; will be nil if (< here-bopl
					; start-point)
	(if scan-backward-pos
	    (c-append-lower-brace-pair-to-state-cache scan-backward-pos))
	(setq good-pos
	      (c-append-to-state-cache cache-pos))
	(setq c-state-cache-good-pos
	      (if (and bopl-state
		       (< good-pos (- here c-state-cache-too-far)))
		  (c-state-cache-non-literal-place here-bopl bopl-state)
		good-pos)))

       ((eq strategy 'backward)
	(setq res (c-remove-stale-state-cache-backwards here cache-pos)
	      good-pos (car res)
	      scan-backward-pos (cadr res)
	      scan-forward-p (car (cddr res)))
	(if scan-backward-pos
	    (c-append-lower-brace-pair-to-state-cache
	     scan-backward-pos))
	(setq c-state-cache-good-pos
	      (if scan-forward-p
		  (progn (narrow-to-region (point-min) here)
			 (c-append-to-state-cache good-pos))
		good-pos)))

       (t ; (eq strategy 'IN-LIT)
	(setq c-state-cache nil
	      c-state-cache-good-pos nil)))))

  c-state-cache)

(defun c-invalidate-state-cache (here)
  ;; This is a wrapper over `c-invalidate-state-cache-1'.
  ;;
  ;; It suppresses the syntactic effect of the < and > (template) brackets and
  ;; of all parens in preprocessor constructs, except for any such construct
  ;; containing point.  We can then call `c-invalidate-state-cache-1' without
  ;; worrying further about macros and template delimiters.
  (c-with-<->-as-parens-suppressed
   (if (and c-state-old-cpp-beg
	    (< c-state-old-cpp-beg here))
       (c-with-all-but-one-cpps-commented-out
	c-state-old-cpp-beg
	(min c-state-old-cpp-end here)
	(c-invalidate-state-cache-1 here))
     (c-with-cpps-commented-out
      (c-invalidate-state-cache-1 here)))))

(defun c-parse-state ()
  ;; This is a wrapper over `c-parse-state-1'.  See that function for a
  ;; description of the functionality and return value.
  ;;
  ;; It suppresses the syntactic effect of the < and > (template) brackets and
  ;; of all parens in preprocessor constructs, except for any such construct
  ;; containing point.  We can then call `c-parse-state-1' without worrying
  ;; further about macros and template delimiters.
  (let (here-cpp-beg here-cpp-end)
    (save-excursion
      (when (c-beginning-of-macro)
	(setq here-cpp-beg (point))
	(unless
	    (> (setq here-cpp-end (c-syntactic-end-of-macro))
	       here-cpp-beg)
	  (setq here-cpp-beg nil  here-cpp-end nil))))
    ;; FIXME!!! Put in a `condition-case' here to protect the integrity of the
    ;; subsystem.
    (prog1
	(c-with-<->-as-parens-suppressed
	 (if (and here-cpp-beg (> here-cpp-end here-cpp-beg))
	     (c-with-all-but-one-cpps-commented-out
	      here-cpp-beg here-cpp-end
	      (c-parse-state-1))
	   (c-with-cpps-commented-out
	    (c-parse-state-1))))
      (setq c-state-old-cpp-beg (and here-cpp-beg (copy-marker here-cpp-beg t))
	    c-state-old-cpp-end (and here-cpp-end (copy-marker here-cpp-end t)))
      )))

;; Debug tool to catch cache inconsistencies.  This is called from
;; 000tests.el.
(defvar c-debug-parse-state nil)
(unless (fboundp 'c-real-parse-state)
  (fset 'c-real-parse-state (symbol-function 'c-parse-state)))
(cc-bytecomp-defun c-real-parse-state)

(defvar c-parse-state-state nil)
(defun c-record-parse-state-state ()
  (setq c-parse-state-state
	(mapcar
	 (lambda (arg)
	   (cons arg (symbol-value arg)))
	 '(c-state-cache
	   c-state-cache-good-pos
	   c-state-nonlit-pos-cache
	   c-state-nonlit-pos-cache-limit
	   c-state-brace-pair-desert
	   c-state-point-min
	   c-state-point-min-lit-type
	   c-state-point-min-lit-start
	   c-state-min-scan-pos
	   c-state-old-cpp-beg
	   c-state-old-cpp-end))))
(defun c-replay-parse-state-state ()
  (message
   (concat "(setq "
    (mapconcat
     (lambda (arg)
       (format "%s %s%s" (car arg) (if (atom (cdr arg)) "" "'") (cdr arg)))
     c-parse-state-state "  ")
    ")")))

(defun c-debug-parse-state ()
  (let ((here (point)) (res1 (c-real-parse-state)) res2)
    (let ((c-state-cache nil)
	  (c-state-cache-good-pos 1)
	  (c-state-nonlit-pos-cache nil)
	  (c-state-nonlit-pos-cache-limit 1)
	  (c-state-brace-pair-desert nil)
	  (c-state-point-min 1)
	  (c-state-point-min-lit-type nil)
	  (c-state-point-min-lit-start nil)
	  (c-state-min-scan-pos 1)
	  (c-state-old-cpp-beg nil)
	  (c-state-old-cpp-end nil))
      (setq res2 (c-real-parse-state)))
    (unless (equal res1 res2)
      ;; The cache can actually go further back due to the ad-hoc way
      ;; the first paren is found, so try to whack off a bit of its
      ;; start before complaining.
      ;; (save-excursion
      ;; 	(goto-char (or (c-least-enclosing-brace res2) (point)))
      ;; 	(c-beginning-of-defun-1)
      ;; 	(while (not (or (bobp) (eq (char-after) ?{)))
      ;; 	  (c-beginning-of-defun-1))
      ;; 	(unless (equal (c-whack-state-before (point) res1) res2)
      ;; 	  (message (concat "c-parse-state inconsistency at %s: "
      ;; 			   "using cache: %s, from scratch: %s")
      ;; 		   here res1 res2)))
      (message (concat "c-parse-state inconsistency at %s: "
		       "using cache: %s, from scratch: %s")
	       here res1 res2)
      (message "Old state:")
      (c-replay-parse-state-state))
    (c-record-parse-state-state)
    res1))

(defun c-toggle-parse-state-debug (&optional arg)
  (interactive "P")
  (setq c-debug-parse-state (c-calculate-state arg c-debug-parse-state))
  (fset 'c-parse-state (symbol-function (if c-debug-parse-state
					    'c-debug-parse-state
					  'c-real-parse-state)))
  (c-keep-region-active))
(when c-debug-parse-state
  (c-toggle-parse-state-debug 1))


(defun c-whack-state-before (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies
  ;; before BUFPOS.  Not destructive on PAREN-STATE.
  (let* ((newstate (list nil))
	 (ptr newstate)
	 car)
    (while paren-state
      (setq car (car paren-state)
	    paren-state (cdr paren-state))
      (if (< (if (consp car) (car car) car) bufpos)
	  (setq paren-state nil)
	(setcdr ptr (list car))
	(setq ptr (cdr ptr))))
    (cdr newstate)))

(defun c-whack-state-after (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies at or
  ;; after BUFPOS.  Not destructive on PAREN-STATE.
  (catch 'done
    (while paren-state
      (let ((car (car paren-state)))
	(if (consp car)
	    ;; just check the car, because in a balanced brace
	    ;; expression, it must be impossible for the corresponding
	    ;; close brace to be before point, but the open brace to
	    ;; be after.
	    (if (<= bufpos (car car))
		nil			; whack it off
	      (if (< bufpos (cdr car))
		  ;; its possible that the open brace is before
		  ;; bufpos, but the close brace is after.  In that
		  ;; case, convert this to a non-cons element.  The
		  ;; rest of the state is before bufpos, so we're
		  ;; done.
		  (throw 'done (cons (car car) (cdr paren-state)))
		;; we know that both the open and close braces are
		;; before bufpos, so we also know that everything else
		;; on state is before bufpos.
		(throw 'done paren-state)))
	  (if (<= bufpos car)
	      nil			; whack it off
	    ;; it's before bufpos, so everything else should too.
	    (throw 'done paren-state)))
	(setq paren-state (cdr paren-state)))
      nil)))

(defun c-most-enclosing-brace (paren-state &optional bufpos)
  ;; Return the bufpos of the innermost enclosing open paren before
  ;; bufpos, or nil if none was found.
  (let (enclosingp)
    (or bufpos (setq bufpos 134217727))
    (while paren-state
      (setq enclosingp (car paren-state)
	    paren-state (cdr paren-state))
      (if (or (consp enclosingp)
	      (>= enclosingp bufpos))
	  (setq enclosingp nil)
	(setq paren-state nil)))
    enclosingp))

(defun c-least-enclosing-brace (paren-state)
  ;; Return the bufpos of the outermost enclosing open paren, or nil
  ;; if none was found.
  (let (pos elem)
    (while paren-state
      (setq elem (car paren-state)
	    paren-state (cdr paren-state))
      (if (integerp elem)
	  (setq pos elem)))
    pos))

(defun c-safe-position (bufpos paren-state)
  ;; Return the closest "safe" position recorded on PAREN-STATE that
  ;; is higher up than BUFPOS.  Return nil if PAREN-STATE doesn't
  ;; contain any.  Return nil if BUFPOS is nil, which is useful to
  ;; find the closest limit before a given limit that might be nil.
  ;;
  ;; A "safe" position is a position at or after a recorded open
  ;; paren, or after a recorded close paren.  The returned position is
  ;; thus either the first position after a close brace, or the first
  ;; position after an enclosing paren, or at the enclosing paren in
  ;; case BUFPOS is immediately after it.
  (when bufpos
    (let (elem)
      (catch 'done
	(while paren-state
	  (setq elem (car paren-state))
	  (if (consp elem)
	      (cond ((< (cdr elem) bufpos)
		     (throw 'done (cdr elem)))
		    ((< (car elem) bufpos)
		     ;; See below.
		     (throw 'done (min (1+ (car elem)) bufpos))))
	    (if (< elem bufpos)
		;; elem is the position at and not after the opening paren, so
		;; we can go forward one more step unless it's equal to
		;; bufpos.  This is useful in some cases avoid an extra paren
		;; level between the safe position and bufpos.
		(throw 'done (min (1+ elem) bufpos))))
	  (setq paren-state (cdr paren-state)))))))

(defun c-beginning-of-syntax ()
  ;; This is used for `font-lock-beginning-of-syntax-function'.  It
  ;; goes to the closest previous point that is known to be outside
  ;; any string literal or comment.  `c-state-cache' is used if it has
  ;; a position in the vicinity.
  (let* ((paren-state c-state-cache)
	 elem

	 (pos (catch 'done
		;; Note: Similar code in `c-safe-position'.  The
		;; difference is that we accept a safe position at
		;; the point and don't bother to go forward past open
		;; parens.
		(while paren-state
		  (setq elem (car paren-state))
		  (if (consp elem)
		      (cond ((<= (cdr elem) (point))
			     (throw 'done (cdr elem)))
			    ((<= (car elem) (point))
			     (throw 'done (car elem))))
		    (if (<= elem (point))
			(throw 'done elem)))
		  (setq paren-state (cdr paren-state)))
		(point-min))))

    (if (> pos (- (point) 4000))
	(goto-char pos)
      ;; The position is far back.  Try `c-beginning-of-defun-1'
      ;; (although we can't be entirely sure it will go to a position
      ;; outside a comment or string in current emacsen).  FIXME:
      ;; Consult `syntax-ppss' here.
      (c-beginning-of-defun-1)
      (if (< (point) pos)
	  (goto-char pos)))))


;; Tools for scanning identifiers and other tokens.

(defun c-on-identifier ()
  "Return non-nil if the point is on or directly after an identifier.
Keywords are recognized and not considered identifiers.  If an
identifier is detected, the returned value is its starting position.
If an identifier ends at the point and another begins at it \(can only
happen in Pike) then the point for the preceding one is returned.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  ;; FIXME: Shouldn't this function handle "operator" in C++?

  (save-excursion
    (skip-syntax-backward "w_")

    (or

     ;; Check for a normal (non-keyword) identifier.
     (and (looking-at c-symbol-start)
	  (not (looking-at c-keywords-regexp))
	  (point))

     (when (c-major-mode-is 'pike-mode)
       ;; Handle the `<operator> syntax in Pike.
       (let ((pos (point)))
	 (skip-chars-backward "-!%&*+/<=>^|~[]()")
	 (and (if (< (skip-chars-backward "`") 0)
		  t
		(goto-char pos)
		(eq (char-after) ?\`))
	      (looking-at c-symbol-key)
	      (>= (match-end 0) pos)
	      (point))))

     ;; Handle the "operator +" syntax in C++.
     (when (and c-overloadable-operators-regexp
		(= (c-backward-token-2 0) 0))

       (cond ((and (looking-at c-overloadable-operators-regexp)
		   (or (not c-opt-op-identifier-prefix)
		       (and (= (c-backward-token-2 1) 0)
			    (looking-at c-opt-op-identifier-prefix))))
	      (point))

	     ((save-excursion
		(and c-opt-op-identifier-prefix
		     (looking-at c-opt-op-identifier-prefix)
		     (= (c-forward-token-2 1) 0)
		     (looking-at c-overloadable-operators-regexp)))
	      (point))))

     )))

(defsubst c-simple-skip-symbol-backward ()
  ;; If the point is at the end of a symbol then skip backward to the
  ;; beginning of it.  Don't move otherwise.  Return non-nil if point
  ;; moved.
  ;;
  ;; This function might do hidden buffer changes.
  (or (< (skip-syntax-backward "w_") 0)
      (and (c-major-mode-is 'pike-mode)
	   ;; Handle the `<operator> syntax in Pike.
	   (let ((pos (point)))
	     (if (and (< (skip-chars-backward "-!%&*+/<=>^|~[]()") 0)
		      (< (skip-chars-backward "`") 0)
		      (looking-at c-symbol-key)
		      (>= (match-end 0) pos))
		 t
	       (goto-char pos)
	       nil)))))

(defun c-beginning-of-current-token (&optional back-limit)
  ;; Move to the beginning of the current token.  Do not move if not
  ;; in the middle of one.  BACK-LIMIT may be used to bound the
  ;; backward search; if given it's assumed to be at the boundary
  ;; between two tokens.  Return non-nil if the point is moved, nil
  ;; otherwise.
  ;;
  ;; This function might do hidden buffer changes.
    (let ((start (point)))
      (if (looking-at "\\w\\|\\s_")
	  (skip-syntax-backward "w_" back-limit)
	(when (< (skip-syntax-backward ".()" back-limit) 0)
	  (while (let ((pos (or (and (looking-at c-nonsymbol-token-regexp)
				     (match-end 0))
				;; `c-nonsymbol-token-regexp' should always match
				;; since we've skipped backward over punctuator
				;; or paren syntax, but consume one char in case
				;; it doesn't so that we don't leave point before
				;; some earlier incorrect token.
				(1+ (point)))))
		   (if (<= pos start)
		       (goto-char pos))))))
      (< (point) start)))

(defun c-end-of-current-token (&optional back-limit)
  ;; Move to the end of the current token.  Do not move if not in the
  ;; middle of one.  BACK-LIMIT may be used to bound the backward
  ;; search; if given it's assumed to be at the boundary between two
  ;; tokens.  Return non-nil if the point is moved, nil otherwise.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((start (point)))
    (cond ((< (skip-syntax-backward "w_" (1- start)) 0)
	   (skip-syntax-forward "w_"))
	  ((< (skip-syntax-backward ".()" back-limit) 0)
	   (while (progn
		    (if (looking-at c-nonsymbol-token-regexp)
			(goto-char (match-end 0))
		      ;; `c-nonsymbol-token-regexp' should always match since
		      ;; we've skipped backward over punctuator or paren
		      ;; syntax, but move forward in case it doesn't so that
		      ;; we don't leave point earlier than we started with.
		      (forward-char))
		    (< (point) start)))))
    (> (point) start)))

(defconst c-jump-syntax-balanced
  (if (memq 'gen-string-delim c-emacs-features)
      "\\w\\|\\s_\\|\\s\(\\|\\s\)\\|\\s\"\\|\\s|"
    "\\w\\|\\s_\\|\\s\(\\|\\s\)\\|\\s\""))

(defconst c-jump-syntax-unbalanced
  (if (memq 'gen-string-delim c-emacs-features)
      "\\w\\|\\s_\\|\\s\"\\|\\s|"
    "\\w\\|\\s_\\|\\s\""))

(defun c-forward-token-2 (&optional count balanced limit)
  "Move forward by tokens.
A token is defined as all symbols and identifiers which aren't
syntactic whitespace \(note that multicharacter tokens like \"==\" are
treated properly).  Point is always either left at the beginning of a
token or not moved at all.  COUNT specifies the number of tokens to
move; a negative COUNT moves in the opposite direction.  A COUNT of 0
moves to the next token beginning only if not already at one.  If
BALANCED is true, move over balanced parens, otherwise move into them.
Also, if BALANCED is true, never move out of an enclosing paren.

LIMIT sets the limit for the movement and defaults to the point limit.
The case when LIMIT is set in the middle of a token, comment or macro
is handled correctly, i.e. the point won't be left there.

Return the number of tokens left to move \(positive or negative).  If
BALANCED is true, a move over a balanced paren counts as one.  Note
that if COUNT is 0 and no appropriate token beginning is found, 1 will
be returned.  Thus, a return value of 0 guarantees that point is at
the requested position and a return value less \(without signs) than
COUNT guarantees that point is at the beginning of some token.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (or count (setq count 1))
  (if (< count 0)
      (- (c-backward-token-2 (- count) balanced limit))

    (let ((jump-syntax (if balanced
			   c-jump-syntax-balanced
			 c-jump-syntax-unbalanced))
	  (last (point))
	  (prev (point)))

      (if (zerop count)
	  ;; If count is zero we should jump if in the middle of a token.
	  (c-end-of-current-token))

      (save-restriction
	(if limit (narrow-to-region (point-min) limit))
	(if (/= (point)
		(progn (c-forward-syntactic-ws) (point)))
	    ;; Skip whitespace.  Count this as a move if we did in
	    ;; fact move.
	    (setq count (max (1- count) 0)))

	(if (eobp)
	    ;; Moved out of bounds.  Make sure the returned count isn't zero.
	    (progn
	      (if (zerop count) (setq count 1))
	      (goto-char last))

	  ;; Use `condition-case' to avoid having the limit tests
	  ;; inside the loop.
	  (condition-case nil
	      (while (and
		      (> count 0)
		      (progn
			(setq last (point))
			(cond ((looking-at jump-syntax)
			       (goto-char (scan-sexps (point) 1))
			       t)
			      ((looking-at c-nonsymbol-token-regexp)
			       (goto-char (match-end 0))
			       t)
			      ;; `c-nonsymbol-token-regexp' above should always
			      ;; match if there are correct tokens.  Try to
			      ;; widen to see if the limit was set in the
			      ;; middle of one, else fall back to treating
			      ;; the offending thing as a one character token.
			      ((and limit
				    (save-restriction
				      (widen)
				      (looking-at c-nonsymbol-token-regexp)))
			       nil)
			      (t
			       (forward-char)
			       t))))
		(c-forward-syntactic-ws)
		(setq prev last
		      count (1- count)))
	    (error (goto-char last)))

	  (when (eobp)
	    (goto-char prev)
	    (setq count (1+ count)))))

      count)))

(defun c-backward-token-2 (&optional count balanced limit)
  "Move backward by tokens.
See `c-forward-token-2' for details."

  (or count (setq count 1))
  (if (< count 0)
      (- (c-forward-token-2 (- count) balanced limit))

    (or limit (setq limit (point-min)))
    (let ((jump-syntax (if balanced
			   c-jump-syntax-balanced
			 c-jump-syntax-unbalanced))
	  (last (point)))

      (if (zerop count)
	  ;; The count is zero so try to skip to the beginning of the
	  ;; current token.
	  (if (> (point)
		 (progn (c-beginning-of-current-token) (point)))
	      (if (< (point) limit)
		  ;; The limit is inside the same token, so return 1.
		  (setq count 1))

	    ;; We're not in the middle of a token.  If there's
	    ;; whitespace after the point then we must move backward,
	    ;; so set count to 1 in that case.
	    (and (looking-at c-syntactic-ws-start)
		 ;; If we're looking at a '#' that might start a cpp
		 ;; directive then we have to do a more elaborate check.
		 (or (/= (char-after) ?#)
		     (not c-opt-cpp-prefix)
		     (save-excursion
		       (and (= (point)
			       (progn (beginning-of-line)
				      (looking-at "[ \t]*")
				      (match-end 0)))
			    (or (bobp)
				(progn (backward-char)
				       (not (eq (char-before) ?\\)))))))
		 (setq count 1))))

      ;; Use `condition-case' to avoid having to check for buffer
      ;; limits in `backward-char', `scan-sexps' and `goto-char' below.
      (condition-case nil
	  (while (and
		  (> count 0)
		  (progn
		    (c-backward-syntactic-ws)
		    (backward-char)
		    (if (looking-at jump-syntax)
			(goto-char (scan-sexps (1+ (point)) -1))
		      ;; This can be very inefficient if there's a long
		      ;; sequence of operator tokens without any separation.
		      ;; That doesn't happen in practice, anyway.
		      (c-beginning-of-current-token))
		    (>= (point) limit)))
	    (setq last (point)
		  count (1- count)))
	(error (goto-char last)))

      (if (< (point) limit)
	  (goto-char last))

      count)))

(defun c-forward-token-1 (&optional count balanced limit)
  "Like `c-forward-token-2' but doesn't treat multicharacter operator
tokens like \"==\" as single tokens, i.e. all sequences of symbol
characters are jumped over character by character.  This function is
for compatibility only; it's only a wrapper over `c-forward-token-2'."
  (let ((c-nonsymbol-token-regexp "\\s.\\|\\s\(\\|\\s\)"))
    (c-forward-token-2 count balanced limit)))

(defun c-backward-token-1 (&optional count balanced limit)
  "Like `c-backward-token-2' but doesn't treat multicharacter operator
tokens like \"==\" as single tokens, i.e. all sequences of symbol
characters are jumped over character by character.  This function is
for compatibility only; it's only a wrapper over `c-backward-token-2'."
  (let ((c-nonsymbol-token-regexp "\\s.\\|\\s\(\\|\\s\)"))
    (c-backward-token-2 count balanced limit)))


;; Tools for doing searches restricted to syntactically relevant text.

(defun c-syntactic-re-search-forward (regexp &optional bound noerror
				      paren-level not-inside-token
				      lookbehind-submatch)
  "Like `re-search-forward', but only report matches that are found
in syntactically significant text.  I.e. matches in comments, macros
or string literals are ignored.  The start point is assumed to be
outside any comment, macro or string literal, or else the content of
that region is taken as syntactically significant text.

If PAREN-LEVEL is non-nil, an additional restriction is added to
ignore matches in nested paren sexps.  The search will also not go
outside the current list sexp, which has the effect that if the point
should be moved to BOUND when no match is found \(i.e. NOERROR is
neither nil nor t), then it will be at the closing paren if the end of
the current list sexp is encountered first.

If NOT-INSIDE-TOKEN is non-nil, matches in the middle of tokens are
ignored.  Things like multicharacter operators and special symbols
\(e.g. \"`()\" in Pike) are handled but currently not floating point
constants.

If LOOKBEHIND-SUBMATCH is non-nil, it's taken as a number of a
subexpression in REGEXP.  The end of that submatch is used as the
position to check for syntactic significance.  If LOOKBEHIND-SUBMATCH
isn't used or if that subexpression didn't match then the start
position of the whole match is used instead.  The \"look behind\"
subexpression is never tested before the starting position, so it
might be a good idea to include \\=\\= as a match alternative in it.

Optimization note: Matches might be missed if the \"look behind\"
subexpression can match the end of nonwhite syntactic whitespace,
i.e. the end of comments or cpp directives.  This since the function
skips over such things before resuming the search.  It's on the other
hand not safe to assume that the \"look behind\" subexpression never
matches syntactic whitespace.

Bug: Unbalanced parens inside cpp directives are currently not handled
correctly \(i.e. they don't get ignored as they should) when
PAREN-LEVEL is set.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (or bound (setq bound (point-max)))
  (if paren-level (setq paren-level -1))

  ;;(message "c-syntactic-re-search-forward %s %s %S" (point) bound regexp)

  (let ((start (point))
	tmp
	;; Start position for the last search.
	search-pos
	;; The `parse-partial-sexp' state between the start position
	;; and the point.
	state
	;; The current position after the last state update.  The next
	;; `parse-partial-sexp' continues from here.
	(state-pos (point))
	;; The position at which to check the state and the state
	;; there.  This is separate from `state-pos' since we might
	;; need to back up before doing the next search round.
	check-pos check-state
	;; Last position known to end a token.
	(last-token-end-pos (point-min))
	;; Set when a valid match is found.
	found)

    (condition-case err
	(while
	    (and
	     (progn
	       (setq search-pos (point))
	       (re-search-forward regexp bound noerror))

	     (progn
	       (setq state (parse-partial-sexp
			    state-pos (match-beginning 0) paren-level nil state)
		     state-pos (point))
	       (if (setq check-pos (and lookbehind-submatch
					(or (not paren-level)
					    (>= (car state) 0))
					(match-end lookbehind-submatch)))
		   (setq check-state (parse-partial-sexp
				      state-pos check-pos paren-level nil state))
		 (setq check-pos state-pos
		       check-state state))

	       ;; NOTE: If we got a look behind subexpression and get
	       ;; an insignificant match in something that isn't
	       ;; syntactic whitespace (i.e. strings or in nested
	       ;; parentheses), then we can never skip more than a
	       ;; single character from the match start position
	       ;; (i.e. `state-pos' here) before continuing the
	       ;; search.  That since the look behind subexpression
	       ;; might match the end of the insignificant region in
	       ;; the next search.

	       (cond
		((elt check-state 7)
		 ;; Match inside a line comment.  Skip to eol.  Use
		 ;; `re-search-forward' instead of `skip-chars-forward' to get
		 ;; the right bound behavior.
		 (re-search-forward "[\n\r]" bound noerror))

		((elt check-state 4)
		 ;; Match inside a block comment.  Skip to the '*/'.
		 (search-forward "*/" bound noerror))

		((and (not (elt check-state 5))
		      (eq (char-before check-pos) ?/)
		      (not (c-get-char-property (1- check-pos) 'syntax-table))
		      (memq (char-after check-pos) '(?/ ?*)))
		 ;; Match in the middle of the opener of a block or line
		 ;; comment.
		 (if (= (char-after check-pos) ?/)
		     (re-search-forward "[\n\r]" bound noerror)
		   (search-forward "*/" bound noerror)))

		;; The last `parse-partial-sexp' above might have
		;; stopped short of the real check position if the end
		;; of the current sexp was encountered in paren-level
		;; mode.  The checks above are always false in that
		;; case, and since they can do better skipping in
		;; lookbehind-submatch mode, we do them before
		;; checking the paren level.

		((and paren-level
		      (/= (setq tmp (car check-state)) 0))
		 ;; Check the paren level first since we're short of the
		 ;; syntactic checking position if the end of the
		 ;; current sexp was encountered by `parse-partial-sexp'.
		 (if (> tmp 0)

		     ;; Inside a nested paren sexp.
		     (if lookbehind-submatch
			 ;; See the NOTE above.
			 (progn (goto-char state-pos) t)
		       ;; Skip out of the paren quickly.
		       (setq state (parse-partial-sexp state-pos bound 0 nil state)
			     state-pos (point)))

		   ;; Have exited the current paren sexp.
		   (if noerror
		       (progn
			 ;; The last `parse-partial-sexp' call above
			 ;; has left us just after the closing paren
			 ;; in this case, so we can modify the bound
			 ;; to leave the point at the right position
			 ;; upon return.
			 (setq bound (1- (point)))
			 nil)
		     (signal 'search-failed (list regexp)))))

		((setq tmp (elt check-state 3))
		 ;; Match inside a string.
		 (if (or lookbehind-submatch
			 (not (integerp tmp)))
		     ;; See the NOTE above.
		     (progn (goto-char state-pos) t)
		   ;; Skip to the end of the string before continuing.
		   (let ((ender (make-string 1 tmp)) (continue t))
		     (while (if (search-forward ender bound noerror)
				(progn
				  (setq state (parse-partial-sexp
					       state-pos (point) nil nil state)
					state-pos (point))
				  (elt state 3))
			      (setq continue nil)))
		     continue)))

		((save-excursion
		   (save-match-data
		     (c-beginning-of-macro start)))
		 ;; Match inside a macro.  Skip to the end of it.
		 (c-end-of-macro)
		 (cond ((<= (point) bound) t)
		       (noerror nil)
		       (t (signal 'search-failed (list regexp)))))

		((and not-inside-token
		      (or (< check-pos last-token-end-pos)
			  (< check-pos
			     (save-excursion
			       (goto-char check-pos)
			       (save-match-data
				 (c-end-of-current-token last-token-end-pos))
			       (setq last-token-end-pos (point))))))
		 ;; Inside a token.
		 (if lookbehind-submatch
		     ;; See the NOTE above.
		     (goto-char state-pos)
		   (goto-char (min last-token-end-pos bound))))

		(t
		 ;; A real match.
		 (setq found t)
		 nil)))

	     ;; Should loop to search again, but take care to avoid
	     ;; looping on the same spot.
	     (or (/= search-pos (point))
		 (if (= (point) bound)
		     (if noerror
			 nil
		       (signal 'search-failed (list regexp)))
		   (forward-char)
		   t))))

      (error
       (goto-char start)
       (signal (car err) (cdr err))))

    ;;(message "c-syntactic-re-search-forward done %s" (or (match-end 0) (point)))

    (if found
	(progn
	  (goto-char (match-end 0))
	  (match-end 0))

      ;; Search failed.  Set point as appropriate.
      (if (eq noerror t)
	  (goto-char start)
	(goto-char bound))
      nil)))

(defvar safe-pos-list)		  ; bound in c-syntactic-skip-backward

(defsubst c-ssb-lit-begin ()
  ;; Return the start of the literal point is in, or nil.
  ;; We read and write the variables `safe-pos', `safe-pos-list', `state'
  ;; bound in the caller.

  ;; Use `parse-partial-sexp' from a safe position down to the point to check
  ;; if it's outside comments and strings.
  (save-excursion
    (let ((pos (point)) safe-pos state pps-end-pos)
      ;; Pick a safe position as close to the point as possible.
      ;;
      ;; FIXME: Consult `syntax-ppss' here if our cache doesn't give a good
      ;; position.

      (while (and safe-pos-list
		  (> (car safe-pos-list) (point)))
	(setq safe-pos-list (cdr safe-pos-list)))
      (unless (setq safe-pos (car-safe safe-pos-list))
	(setq safe-pos (max (or (c-safe-position
				 (point) (or c-state-cache
					     (c-parse-state)))
				0)
			    (point-min))
	      safe-pos-list (list safe-pos)))

      ;; Cache positions along the way to use if we have to back up more.  We
      ;; cache every closing paren on the same level.  If the paren cache is
      ;; relevant in this region then we're typically already on the same
      ;; level as the target position.  Note that we might cache positions
      ;; after opening parens in case safe-pos is in a nested list.  That's
      ;; both uncommon and harmless.
      (while (progn
	       (setq state (parse-partial-sexp
			    safe-pos pos 0))
	       (< (point) pos))
	(setq safe-pos (point)
	      safe-pos-list (cons safe-pos safe-pos-list)))

      ;; If the state contains the start of the containing sexp we cache that
      ;; position too, so that parse-partial-sexp in the next run has a bigger
      ;; chance of starting at the same level as the target position and thus
      ;; will get more good safe positions into the list.
      (if (elt state 1)
	  (setq safe-pos (1+ (elt state 1))
		safe-pos-list (cons safe-pos safe-pos-list)))

      (if (or (elt state 3) (elt state 4))
	  ;; Inside string or comment.  Continue search at the
	  ;; beginning of it.
	  (elt state 8)))))

(defun c-syntactic-skip-backward (skip-chars &optional limit paren-level)
  "Like `skip-chars-backward' but only look at syntactically relevant chars,
i.e. don't stop at positions inside syntactic whitespace or string
literals.  Preprocessor directives are also ignored, with the exception
of the one that the point starts within, if any.  If LIMIT is given,
it's assumed to be at a syntactically relevant position.

If PAREN-LEVEL is non-nil, the function won't stop in nested paren
sexps, and the search will also not go outside the current paren sexp.
However, if LIMIT or the buffer limit is reached inside a nested paren
then the point will be left at the limit.

Non-nil is returned if the point moved, nil otherwise.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (let ((start (point))
	state-2
	;; A list of syntactically relevant positions in descending
	;; order.  It's used to avoid scanning repeatedly over
	;; potentially large regions with `parse-partial-sexp' to verify
	;; each position.  Used in `c-ssb-lit-begin'
	safe-pos-list
	;; The result from `c-beginning-of-macro' at the start position or the
	;; start position itself if it isn't within a macro.  Evaluated on
	;; demand.
	start-macro-beg
	;; The earliest position after the current one with the same paren
	;; level.  Used only when `paren-level' is set.
	lit-beg
	(paren-level-pos (point)))

    (while
	(progn
	  ;; The next loop "tries" to find the end point each time round,
	  ;; loops when it hasn't succeeded.
	  (while
	      (and
	       (< (skip-chars-backward skip-chars limit) 0)

	       (let ((pos (point)) state-2 pps-end-pos)

		 (cond
		  ;; Don't stop inside a literal
		  ((setq lit-beg (c-ssb-lit-begin))
		   (goto-char lit-beg)
		   t)

		  ((and paren-level
			(save-excursion
			  (setq state-2 (parse-partial-sexp
					 pos paren-level-pos -1)
				pps-end-pos (point))
			  (/= (car state-2) 0)))
		   ;; Not at the right level.

		   (if (and (< (car state-2) 0)
			    ;; We stop above if we go out of a paren.
			    ;; Now check whether it precedes or is
			    ;; nested in the starting sexp.
			    (save-excursion
			      (setq state-2
				    (parse-partial-sexp
				     pps-end-pos paren-level-pos
				     nil nil state-2))
			      (< (car state-2) 0)))

		       ;; We've stopped short of the starting position
		       ;; so the hit was inside a nested list.  Go up
		       ;; until we are at the right level.
		       (condition-case nil
			   (progn
			     (goto-char (scan-lists pos -1
						    (- (car state-2))))
			     (setq paren-level-pos (point))
			     (if (and limit (>= limit paren-level-pos))
				 (progn
				   (goto-char limit)
				   nil)
			       t))
			 (error
			  (goto-char (or limit (point-min)))
			  nil))

		     ;; The hit was outside the list at the start
		     ;; position.  Go to the start of the list and exit.
		     (goto-char (1+ (elt state-2 1)))
		     nil))

		  ((c-beginning-of-macro limit)
		   ;; Inside a macro.
		   (if (< (point)
			  (or start-macro-beg
			      (setq start-macro-beg
				    (save-excursion
				      (goto-char start)
				      (c-beginning-of-macro limit)
				      (point)))))
		       t

		     ;; It's inside the same macro we started in so it's
		     ;; a relevant match.
		     (goto-char pos)
		     nil))))))

	  (> (point)
	     (progn
	       ;; Skip syntactic ws afterwards so that we don't stop at the
	       ;; end of a comment if `skip-chars' is something like "^/".
	       (c-backward-syntactic-ws)
	       (point)))))

    ;; We might want to extend this with more useful return values in
    ;; the future.
    (/= (point) start)))

;; The following is an alternative implementation of
;; `c-syntactic-skip-backward' that uses backward movement to keep
;; track of the syntactic context.  It turned out to be generally
;; slower than the one above which uses forward checks from earlier
;; safe positions.
;;
;;(defconst c-ssb-stop-re
;;  ;; The regexp matching chars `c-syntactic-skip-backward' needs to
;;  ;; stop at to avoid going into comments and literals.
;;  (concat
;;   ;; Match comment end syntax and string literal syntax.  Also match
;;   ;; '/' for block comment endings (not covered by comment end
;;   ;; syntax).
;;   "\\s>\\|/\\|\\s\""
;;   (if (memq 'gen-string-delim c-emacs-features)
;;	 "\\|\\s|"
;;     "")
;;   (if (memq 'gen-comment-delim c-emacs-features)
;;	 "\\|\\s!"
;;     "")))
;;
;;(defconst c-ssb-stop-paren-re
;;  ;; Like `c-ssb-stop-re' but also stops at paren chars.
;;  (concat c-ssb-stop-re "\\|\\s(\\|\\s)"))
;;
;;(defconst c-ssb-sexp-end-re
;;  ;; Regexp matching the ending syntax of a complex sexp.
;;  (concat c-string-limit-regexp "\\|\\s)"))
;;
;;(defun c-syntactic-skip-backward (skip-chars &optional limit paren-level)
;;  "Like `skip-chars-backward' but only look at syntactically relevant chars,
;;i.e. don't stop at positions inside syntactic whitespace or string
;;literals.  Preprocessor directives are also ignored.  However, if the
;;point is within a comment, string literal or preprocessor directory to
;;begin with, its contents is treated as syntactically relevant chars.
;;If LIMIT is given, it limits the backward search and the point will be
;;left there if no earlier position is found.
;;
;;If PAREN-LEVEL is non-nil, the function won't stop in nested paren
;;sexps, and the search will also not go outside the current paren sexp.
;;However, if LIMIT or the buffer limit is reached inside a nested paren
;;then the point will be left at the limit.
;;
;;Non-nil is returned if the point moved, nil otherwise.
;;
;;Note that this function might do hidden buffer changes.  See the
;;comment at the start of cc-engine.el for more info."
;;
;;  (save-restriction
;;    (when limit
;;	(narrow-to-region limit (point-max)))
;;
;;    (let ((start (point)))
;;	(catch 'done
;;	  (while (let ((last-pos (point))
;;		       (stop-pos (progn
;;				   (skip-chars-backward skip-chars)
;;				   (point))))
;;
;;		   ;; Skip back over the same region as
;;		   ;; `skip-chars-backward' above, but keep to
;;		   ;; syntactically relevant positions.
;;		   (goto-char last-pos)
;;		   (while (and
;;			   ;; `re-search-backward' with a single char regexp
;;			   ;; should be fast.
;;			   (re-search-backward
;;			    (if paren-level c-ssb-stop-paren-re c-ssb-stop-re)
;;			    stop-pos 'move)
;;
;;			   (progn
;;			     (cond
;;			      ((looking-at "\\s(")
;;			       ;; `paren-level' is set and we've found the
;;			       ;; start of the containing paren.
;;			       (forward-char)
;;			       (throw 'done t))
;;
;;			      ((looking-at c-ssb-sexp-end-re)
;;			       ;; We're at the end of a string literal or paren
;;			       ;; sexp (if `paren-level' is set).
;;			       (forward-char)
;;			       (condition-case nil
;;				   (c-backward-sexp)
;;				 (error
;;				  (goto-char limit)
;;				  (throw 'done t))))
;;
;;			      (t
;;			       (forward-char)
;;			       ;; At the end of some syntactic ws or possibly
;;			       ;; after a plain '/' operator.
;;			       (let ((pos (point)))
;;				 (c-backward-syntactic-ws)
;;				 (if (= pos (point))
;;				     ;; Was a plain '/' operator.  Go past it.
;;				     (backward-char)))))
;;
;;			     (> (point) stop-pos))))
;;
;;		   ;; Now the point is either at `stop-pos' or at some
;;		   ;; position further back if `stop-pos' was at a
;;		   ;; syntactically irrelevant place.
;;
;;		   ;; Skip additional syntactic ws so that we don't stop
;;		   ;; at the end of a comment if `skip-chars' is
;;		   ;; something like "^/".
;;		   (c-backward-syntactic-ws)
;;
;;		   (< (point) stop-pos))))
;;
;;	;; We might want to extend this with more useful return values
;;	;; in the future.
;;	(/= (point) start))))


;; Tools for handling comments and string literals.

(defun c-in-literal (&optional lim detect-cpp)
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a C++
style comment, `string' if in a string literal, `pound' if DETECT-CPP
is non-nil and in a preprocessor line, or nil if somewhere else.
Optional LIM is used as the backward limit of the search.  If omitted,
or nil, `c-beginning-of-defun' is used.

The last point calculated is cached if the cache is enabled, i.e. if
`c-in-literal-cache' is bound to a two element vector.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (save-restriction
    (widen)
    (let* ((safe-place (c-state-semi-safe-place (point)))
	   (lit (c-state-pp-to-literal safe-place (point))))
      (or (cadr lit)
	  (and detect-cpp
	       (save-excursion (c-beginning-of-macro))
	       'pound)))))

(defun c-literal-limits (&optional lim near not-in-delimiter)
  "Return a cons of the beginning and end positions of the comment or
string surrounding point (including both delimiters), or nil if point
isn't in one.  If LIM is non-nil, it's used as the \"safe\" position
to start parsing from.  If NEAR is non-nil, then the limits of any
literal next to point is returned.  \"Next to\" means there's only
spaces and tabs between point and the literal.  The search for such a
literal is done first in forward direction.  If NOT-IN-DELIMITER is
non-nil, the case when point is inside a starting delimiter won't be
recognized.  This only has effect for comments which have starting
delimiters with more than one character.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (c-state-semi-safe-place pos)))
	   (pp-to-lit (save-restriction
			(widen)
			(c-state-pp-to-literal lim pos)))
	   (state (car pp-to-lit))
	   (lit-limits (car (cddr pp-to-lit))))

      (cond
       (lit-limits)
       ((and (not not-in-delimiter)
	     (not (elt state 5))
	     (eq (char-before) ?/)
	     (looking-at "[/*]")) ; FIXME!!! use c-line/block-comment-starter.  2008-09-28.
	;; We're standing in a comment starter.
	(backward-char 1)
	(cons (point) (progn (c-forward-single-comment) (point))))

       (near
	(goto-char pos)
	;; Search forward for a literal.
	(skip-chars-forward " \t")
	(cond
	 ((looking-at c-string-limit-regexp) ; String.
	  (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			    (point-max))))

	 ((looking-at c-comment-start-regexp) ; Line or block comment.
	  (cons (point) (progn (c-forward-single-comment) (point))))

	 (t
	  ;; Search backward.
	  (skip-chars-backward " \t")

	  (let ((end (point)) beg)
	    (cond
	     ((save-excursion
		(< (skip-syntax-backward c-string-syntax) 0)) ; String.
	      (setq beg (c-safe (c-backward-sexp 1) (point))))

	     ((and (c-safe (forward-char -2) t)
		   (looking-at "*/"))
	      ;; Block comment.  Due to the nature of line
	      ;; comments, they will always be covered by the
	      ;; normal case above.
	      (goto-char end)
	      (c-backward-single-comment)
	      ;; If LIM is bogus, beg will be bogus.
	      (setq beg (point))))

	    (if beg (cons beg end))))))
       ))))

;; In case external callers use this; it did have a docstring.
(defalias 'c-literal-limits-fast 'c-literal-limits)

(defun c-collect-line-comments (range)
  "If the argument is a cons of two buffer positions (such as returned by
`c-literal-limits'), and that range contains a C++ style line comment,
then an extended range is returned that contains all adjacent line
comments (i.e. all comments that starts in the same column with no
empty lines or non-whitespace characters between them).  Otherwise the
argument is returned.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (save-excursion
    (condition-case nil
	(if (and (consp range) (progn
				 (goto-char (car range))
				 (looking-at c-line-comment-starter)))
	    (let ((col (current-column))
		  (beg (point))
		  (bopl (c-point 'bopl))
		  (end (cdr range)))
	      ;; Got to take care in the backward direction to handle
	      ;; comments which are preceded by code.
	      (while (and (c-backward-single-comment)
			  (>= (point) bopl)
			  (looking-at c-line-comment-starter)
			  (= col (current-column)))
		(setq beg (point)
		      bopl (c-point 'bopl)))
	      (goto-char end)
	      (while (and (progn (skip-chars-forward " \t")
				 (looking-at c-line-comment-starter))
			  (= col (current-column))
			  (prog1 (zerop (forward-line 1))
			    (setq end (point)))))
	      (cons beg end))
	  range)
      (error range))))

(defun c-literal-type (range)
  "Convenience function that given the result of `c-literal-limits',
returns nil or the type of literal that the range surrounds, one
of the symbols 'c, 'c++ or 'string.  It's much faster than using
`c-in-literal' and is intended to be used when you need both the
type of a literal and its limits.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (if (consp range)
      (save-excursion
	(goto-char (car range))
	(cond ((looking-at c-string-limit-regexp) 'string)
	      ((or (looking-at "//") ; c++ line comment
		   (and (looking-at "\\s<") ; comment starter
			(looking-at "#"))) ; awk comment.
               'c++)
	      (t 'c)))			; Assuming the range is valid.
    range))

(defsubst c-determine-limit-get-base (start try-size)
  ;; Get a "safe place" approximately TRY-SIZE characters before START.
  ;; This doesn't preserve point.
  (let* ((pos (max (- start try-size) (point-min)))
	 (base (c-state-semi-safe-place pos))
	 (s (parse-partial-sexp base pos)))
    (if (or (nth 4 s) (nth 3 s))	; comment or string
	(nth 8 s)
      (point))))

(defun c-determine-limit (how-far-back &optional start try-size)
  ;; Return a buffer position HOW-FAR-BACK non-literal characters from START
  ;; (default point).  This is done by going back further in the buffer then
  ;; searching forward for literals.  The position found won't be in a
  ;; literal.  We start searching for the sought position TRY-SIZE (default
  ;; twice HOW-FAR-BACK) bytes back from START.  This function must be fast.
  ;; :-)
  (save-excursion
    (let* ((start (or start (point)))
	   (try-size (or try-size (* 2 how-far-back)))
	   (base (c-determine-limit-get-base start try-size))
	   (pos base)

	   (s (parse-partial-sexp pos pos)) ; null state.
	   stack elt size
	   (count 0))
      (while (< pos start)
	;; Move forward one literal each time round this loop.
	;; Move forward to the start of a comment or string.
	(setq s (parse-partial-sexp
		 pos
		 start
		 nil			; target-depth
		 nil			; stop-before
		 s			; state
		 'syntax-table))	; stop-comment

	;; Gather details of the non-literal-bit - starting pos and size.
	(setq size (- (if (or (nth 4 s) (nth 3 s))
			  (nth 8 s)
			(point))
		      pos))
	(if (> size 0)
	    (setq stack (cons (cons pos size) stack)))

	;; Move forward to the end of the comment/string.
	(if (or (nth 4 s) (nth 3 s))
	    (setq s (parse-partial-sexp
		     (point)
		     start
		     nil		; target-depth
		     nil		; stop-before
		     s			; state
		     'syntax-table)))	; stop-comment
	(setq pos (point)))
    
      ;; Now try and find enough non-literal characters recorded on the stack.
      ;; Go back one recorded literal each time round this loop.
      (while (and (< count how-far-back)
		  stack)
	(setq elt (car stack)
	      stack (cdr stack))
	(setq count (+ count (cdr elt))))

      ;; Have we found enough yet?
      (cond
       ((>= count how-far-back)
	(+ (car elt) (- count how-far-back)))
       ((eq base (point-min))
	(point-min))
       (t
	(c-determine-limit (- how-far-back count) base try-size))))))

(defun c-determine-+ve-limit (how-far &optional start-pos)
  ;; Return a buffer position about HOW-FAR non-literal characters forward
  ;; from START-POS (default point), which must not be inside a literal.
  (save-excursion
    (let ((pos (or start-pos (point)))
	  (count how-far)
	  (s (parse-partial-sexp (point) (point)))) ; null state
      (while (and (not (eobp))
		  (> count 0))
	;; Scan over counted characters.
	(setq s (parse-partial-sexp
		 pos
		 (min (+ pos count) (point-max))
		 nil			; target-depth
		 nil			; stop-before
		 s			; state
		 'syntax-table))	; stop-comment
	(setq count (- count (- (point) pos) 1)
	      pos (point))
	;; Scan over literal characters.
	(if (nth 8 s)
	    (setq s (parse-partial-sexp
		     pos
		     (point-max)
		     nil		; target-depth
		     nil		; stop-before
		     s			; state
		     'syntax-table)	; stop-comment
		  pos (point))))
      (point))))


;; `c-find-decl-spots' and accompanying stuff.

;; Variables used in `c-find-decl-spots' to cache the search done for
;; the first declaration in the last call.  When that function starts,
;; it needs to back up over syntactic whitespace to look at the last
;; token before the region being searched.  That can sometimes cause
;; moves back and forth over a quite large region of comments and
;; macros, which would be repeated for each changed character when
;; we're called during fontification, since font-lock refontifies the
;; current line for each change.  Thus it's worthwhile to cache the
;; first match.
;;
;; `c-find-decl-syntactic-pos' is a syntactically relevant position in
;; the syntactic whitespace less or equal to some start position.
;; There's no cached value if it's nil.
;;
;; `c-find-decl-match-pos' is the match position if
;; `c-find-decl-prefix-search' matched before the syntactic whitespace
;; at `c-find-decl-syntactic-pos', or nil if there's no such match.
(defvar c-find-decl-syntactic-pos nil)
(make-variable-buffer-local 'c-find-decl-syntactic-pos)
(defvar c-find-decl-match-pos nil)
(make-variable-buffer-local 'c-find-decl-match-pos)

(defsubst c-invalidate-find-decl-cache (change-min-pos)
  (and c-find-decl-syntactic-pos
       (< change-min-pos c-find-decl-syntactic-pos)
       (setq c-find-decl-syntactic-pos nil)))

; (defface c-debug-decl-spot-face
;   '((t (:background "Turquoise")))
;   "Debug face to mark the spots where `c-find-decl-spots' stopped.")
; (defface c-debug-decl-sws-face
;   '((t (:background "Khaki")))
;   "Debug face to mark the syntactic whitespace between the declaration
; spots and the preceding token end.")

(defmacro c-debug-put-decl-spot-faces (match-pos decl-pos)
  (when (facep 'c-debug-decl-spot-face)
    `(c-save-buffer-state ((match-pos ,match-pos) (decl-pos ,decl-pos))
       (c-debug-add-face (max match-pos (point-min)) decl-pos
			 'c-debug-decl-sws-face)
       (c-debug-add-face decl-pos (min (1+ decl-pos) (point-max))
			 'c-debug-decl-spot-face))))
(defmacro c-debug-remove-decl-spot-faces (beg end)
  (when (facep 'c-debug-decl-spot-face)
    `(c-save-buffer-state ()
       (c-debug-remove-face ,beg ,end 'c-debug-decl-spot-face)
       (c-debug-remove-face ,beg ,end 'c-debug-decl-sws-face))))

(defmacro c-find-decl-prefix-search ()
  ;; Macro used inside `c-find-decl-spots'.  It ought to be a defun,
  ;; but it contains lots of free variables that refer to things
  ;; inside `c-find-decl-spots'.  The point is left at `cfd-match-pos'
  ;; if there is a match, otherwise at `cfd-limit'.
  ;;
  ;; This macro might do hidden buffer changes.

  '(progn
     ;; Find the next property match position if we haven't got one already.
     (unless cfd-prop-match
       (save-excursion
	 (while (progn
		  (goto-char (next-single-property-change
			      (point) 'c-type nil cfd-limit))
		  (and (< (point) cfd-limit)
		       (not (eq (c-get-char-property (1- (point)) 'c-type)
				'c-decl-end)))))
	 (setq cfd-prop-match (point))))

     ;; Find the next `c-decl-prefix-or-start-re' match if we haven't
     ;; got one already.
     (unless cfd-re-match

       (if (> cfd-re-match-end (point))
	   (goto-char cfd-re-match-end))

       (while (if (setq cfd-re-match-end
			(re-search-forward c-decl-prefix-or-start-re
					   cfd-limit 'move))

		  ;; Match.  Check if it's inside a comment or string literal.
		  (c-got-face-at
		   (if (setq cfd-re-match (match-end 1))
		       ;; Matched the end of a token preceding a decl spot.
		       (progn
			 (goto-char cfd-re-match)
			 (1- cfd-re-match))
		     ;; Matched a token that start a decl spot.
		     (goto-char (match-beginning 0))
		     (point))
		   c-literal-faces)

		;; No match.  Finish up and exit the loop.
		(setq cfd-re-match cfd-limit)
		nil)

	 ;; Skip out of comments and string literals.
	 (while (progn
		  (goto-char (next-single-property-change
			      (point) 'face nil cfd-limit))
		  (and (< (point) cfd-limit)
		       (c-got-face-at (point) c-literal-faces)))))

       ;; If we matched at the decl start, we have to back up over the
       ;; preceding syntactic ws to set `cfd-match-pos' and to catch
       ;; any decl spots in the syntactic ws.
       (unless cfd-re-match
	 (c-backward-syntactic-ws)
	 (setq cfd-re-match (point))))

     ;; Choose whichever match is closer to the start.
     (if (< cfd-re-match cfd-prop-match)
	 (setq cfd-match-pos cfd-re-match
	       cfd-re-match nil)
       (setq cfd-match-pos cfd-prop-match
	     cfd-prop-match nil))

     (goto-char cfd-match-pos)

     (when (< cfd-match-pos cfd-limit)
       ;; Skip forward past comments only so we don't skip macros.
       (c-forward-comments)
       ;; Set the position to continue at.  We can avoid going over
       ;; the comments skipped above a second time, but it's possible
       ;; that the comment skipping has taken us past `cfd-prop-match'
       ;; since the property might be used inside comments.
       (setq cfd-continue-pos (if cfd-prop-match
				  (min cfd-prop-match (point))
				(point))))))

(defun c-find-decl-spots (cfd-limit cfd-decl-re cfd-face-checklist cfd-fun)
  ;; Call CFD-FUN for each possible spot for a declaration, cast or
  ;; label from the point to CFD-LIMIT.
  ;;
  ;; CFD-FUN is called with point at the start of the spot.  It's passed two
  ;; arguments: The first is the end position of the token preceding the spot,
  ;; or 0 for the implicit match at bob.  The second is a flag that is t when
  ;; the match is inside a macro.  Point should be moved forward by at least
  ;; one token.
  ;;
  ;; If CFD-FUN adds `c-decl-end' properties somewhere below the current spot,
  ;; it should return non-nil to ensure that the next search will find them.
  ;;
  ;; Such a spot is:
  ;; o  The first token after bob.
  ;; o  The first token after the end of submatch 1 in
  ;;    `c-decl-prefix-or-start-re' when that submatch matches.
  ;; o  The start of each `c-decl-prefix-or-start-re' match when
  ;;    submatch 1 doesn't match.
  ;; o  The first token after the end of each occurrence of the
  ;;    `c-type' text property with the value `c-decl-end', provided
  ;;    `c-type-decl-end-used' is set.
  ;;
  ;; Only a spot that match CFD-DECL-RE and whose face is in the
  ;; CFD-FACE-CHECKLIST list causes CFD-FUN to be called.  The face
  ;; check is disabled if CFD-FACE-CHECKLIST is nil.
  ;;
  ;; If the match is inside a macro then the buffer is narrowed to the
  ;; end of it, so that CFD-FUN can investigate the following tokens
  ;; without matching something that begins inside a macro and ends
  ;; outside it.  It's to avoid this work that the CFD-DECL-RE and
  ;; CFD-FACE-CHECKLIST checks exist.
  ;;
  ;; The spots are visited approximately in order from top to bottom.
  ;; It's however the positions where `c-decl-prefix-or-start-re'
  ;; matches and where `c-decl-end' properties are found that are in
  ;; order.  Since the spots often are at the following token, they
  ;; might be visited out of order insofar as more spots are reported
  ;; later on within the syntactic whitespace between the match
  ;; positions and their spots.
  ;;
  ;; It's assumed that comments and strings are fontified in the
  ;; searched range.
  ;;
  ;; This is mainly used in fontification, and so has an elaborate
  ;; cache to handle repeated calls from the same start position; see
  ;; the variables above.
  ;;
  ;; All variables in this function begin with `cfd-' to avoid name
  ;; collision with the (dynamically bound) variables used in CFD-FUN.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((cfd-start-pos (point))
	(cfd-buffer-end (point-max))
	;; The end of the token preceding the decl spot last found
	;; with `c-decl-prefix-or-start-re'.  `cfd-limit' if there's
	;; no match.
	cfd-re-match
	;; The end position of the last `c-decl-prefix-or-start-re'
	;; match.  If this is greater than `cfd-continue-pos', the
	;; next regexp search is started here instead.
	(cfd-re-match-end (point-min))
	;; The end of the last `c-decl-end' found by
	;; `c-find-decl-prefix-search'.  `cfd-limit' if there's no
	;; match.  If searching for the property isn't needed then we
	;; disable it by setting it to `cfd-limit' directly.
	(cfd-prop-match (unless c-type-decl-end-used cfd-limit))
	;; The end of the token preceding the decl spot last found by
	;; `c-find-decl-prefix-search'.  0 for the implicit match at
	;; bob.  `cfd-limit' if there's no match.  In other words,
	;; this is the minimum of `cfd-re-match' and `cfd-prop-match'.
	(cfd-match-pos cfd-limit)
	;; The position to continue searching at.
	cfd-continue-pos
	;; The position of the last "real" token we've stopped at.
	;; This can be greater than `cfd-continue-pos' when we get
	;; hits inside macros or at `c-decl-end' positions inside
	;; comments.
	(cfd-token-pos 0)
	;; The end position of the last entered macro.
	(cfd-macro-end 0))

    ;; Initialize by finding a syntactically relevant start position
    ;; before the point, and do the first `c-decl-prefix-or-start-re'
    ;; search unless we're at bob.

    (let (start-in-literal start-in-macro syntactic-pos)
      ;; Must back up a bit since we look for the end of the previous
      ;; statement or declaration, which is earlier than the first
      ;; returned match.

      (cond
       ;; First we need to move to a syntactically relevant position.
       ;; Begin by backing out of comment or string literals.
       ((and
	 (when (c-got-face-at (point) c-literal-faces)
	   ;; Try to use the faces to back up to the start of the
	   ;; literal.  FIXME: What if the point is on a declaration
	   ;; inside a comment?
	   (while (and (not (bobp))
		       (c-got-face-at (1- (point)) c-literal-faces))
	     (goto-char (previous-single-property-change
			 (point) 'face nil (point-min))))

	   ;; XEmacs doesn't fontify the quotes surrounding string
	   ;; literals.
	   (and (featurep 'xemacs)
		(eq (get-text-property (point) 'face)
		    'font-lock-string-face)
		(not (bobp))
		(progn (backward-char)
		       (not (looking-at c-string-limit-regexp)))
		(forward-char))

	   ;; Don't trust the literal to contain only literal faces
	   ;; (the font lock package might not have fontified the
	   ;; start of it at all, for instance) so check that we have
	   ;; arrived at something that looks like a start or else
	   ;; resort to `c-literal-limits'.
	   (unless (looking-at c-literal-start-regexp)
	     (let ((range (c-literal-limits)))
	       (if range (goto-char (car range)))))

	   (setq start-in-literal (point)))

	 ;; The start is in a literal.  If the limit is in the same
	 ;; one we don't have to find a syntactic position etc.  We
	 ;; only check that if the limit is at or before bonl to save
	 ;; time; it covers the by far most common case when font-lock
	 ;; refontifies the current line only.
	 (<= cfd-limit (c-point 'bonl cfd-start-pos))
	 (save-excursion
	   (goto-char cfd-start-pos)
	   (while (progn
		    (goto-char (next-single-property-change
				(point) 'face nil cfd-limit))
		    (and (< (point) cfd-limit)
			 (c-got-face-at (point) c-literal-faces))))
	   (= (point) cfd-limit)))

	;; Completely inside a literal.  Set up variables to trig the
	;; (< cfd-continue-pos cfd-start-pos) case below and it'll
	;; find a suitable start position.
	(setq cfd-continue-pos start-in-literal))

       ;; Check if the region might be completely inside a macro, to
       ;; optimize that like the completely-inside-literal above.
       ((save-excursion
	  (and (= (forward-line 1) 0)
	       (bolp)			; forward-line has funny behavior at eob.
	       (>= (point) cfd-limit)
	       (progn (backward-char)
		      (eq (char-before) ?\\))))
	;; (Maybe) completely inside a macro.  Only need to trig the
	;; (< cfd-continue-pos cfd-start-pos) case below to make it
	;; set things up.
	(setq cfd-continue-pos (1- cfd-start-pos)
	      start-in-macro t))

       (t
	;; Back out of any macro so we don't miss any declaration
	;; that could follow after it.
	(when (c-beginning-of-macro)
	  (setq start-in-macro t))

	;; Now we're at a proper syntactically relevant position so we
	;; can use the cache.  But first clear it if it applied
	;; further down.
	(c-invalidate-find-decl-cache cfd-start-pos)

	(setq syntactic-pos (point))
	(unless (eq syntactic-pos c-find-decl-syntactic-pos)
	  ;; Don't have to do this if the cache is relevant here,
	  ;; typically if the same line is refontified again.  If
	  ;; we're just some syntactic whitespace further down we can
	  ;; still use the cache to limit the skipping.
	  (c-backward-syntactic-ws c-find-decl-syntactic-pos))

	;; If we hit `c-find-decl-syntactic-pos' and
	;; `c-find-decl-match-pos' is set then we install the cached
	;; values.  If we hit `c-find-decl-syntactic-pos' and
	;; `c-find-decl-match-pos' is nil then we know there's no decl
	;; prefix in the whitespace before `c-find-decl-syntactic-pos'
	;; and so we can continue the search from this point.  If we
	;; didn't hit `c-find-decl-syntactic-pos' then we're now in
	;; the right spot to begin searching anyway.
	(if (and (eq (point) c-find-decl-syntactic-pos)
		 c-find-decl-match-pos)
	    (setq cfd-match-pos c-find-decl-match-pos
		  cfd-continue-pos syntactic-pos)

	  (setq c-find-decl-syntactic-pos syntactic-pos)

	  (when (if (bobp)
		    ;; Always consider bob a match to get the first
		    ;; declaration in the file.  Do this separately instead of
		    ;; letting `c-decl-prefix-or-start-re' match bob, so that
		    ;; regexp always can consume at least one character to
		    ;; ensure that we won't get stuck in an infinite loop.
		    (setq cfd-re-match 0)
		  (backward-char)
		  (c-beginning-of-current-token)
		  (< (point) cfd-limit))
	    ;; Do an initial search now.  In the bob case above it's
	    ;; only done to search for a `c-decl-end' spot.
	    (c-find-decl-prefix-search))

	  (setq c-find-decl-match-pos (and (< cfd-match-pos cfd-start-pos)
					   cfd-match-pos)))))

      ;; Advance `cfd-continue-pos' if it's before the start position.
      ;; The closest continue position that might have effect at or
      ;; after the start depends on what we started in.  This also
      ;; finds a suitable start position in the special cases when the
      ;; region is completely within a literal or macro.
      (when (and cfd-continue-pos (< cfd-continue-pos cfd-start-pos))

	(cond
	 (start-in-macro
	  ;; If we're in a macro then it's the closest preceding token
	  ;; in the macro.  Check this before `start-in-literal',
	  ;; since if we're inside a literal in a macro, the preceding
	  ;; token is earlier than any `c-decl-end' spot inside the
	  ;; literal (comment).
	  (goto-char (or start-in-literal cfd-start-pos))
	  ;; The only syntactic ws in macros are comments.
	  (c-backward-comments)
	  (backward-char)
	  (c-beginning-of-current-token))

	 (start-in-literal
	  ;; If we're in a comment it can only be the closest
	  ;; preceding `c-decl-end' position within that comment, if
	  ;; any.  Go back to the beginning of such a property so that
	  ;; `c-find-decl-prefix-search' will find the end of it.
	  ;; (Can't stop at the end and install it directly on
	  ;; `cfd-prop-match' since that variable might be cleared
	  ;; after `cfd-fun' below.)
	  ;;
	  ;; Note that if the literal is a string then the property
	  ;; search will simply skip to the beginning of it right
	  ;; away.
	  (if (not c-type-decl-end-used)
	      (goto-char start-in-literal)
	    (goto-char cfd-start-pos)
	    (while (progn
		     (goto-char (previous-single-property-change
				 (point) 'c-type nil start-in-literal))
		     (and (> (point) start-in-literal)
			  (not (eq (c-get-char-property (point) 'c-type)
				   'c-decl-end))))))

	  (when (= (point) start-in-literal)
	    ;; Didn't find any property inside the comment, so we can
	    ;; skip it entirely.  (This won't skip past a string, but
	    ;; that'll be handled quickly by the next
	    ;; `c-find-decl-prefix-search' anyway.)
	    (c-forward-single-comment)
	    (if (> (point) cfd-limit)
		(goto-char cfd-limit))))

	 (t
	  ;; If we started in normal code, the only match that might
	  ;; apply before the start is what we already got in
	  ;; `cfd-match-pos' so we can continue at the start position.
	  ;; (Note that we don't get here if the first match is below
	  ;; it.)
	  (goto-char cfd-start-pos)))

	;; Delete found matches if they are before our new continue
	;; position, so that `c-find-decl-prefix-search' won't back up
	;; to them later on.
	(setq cfd-continue-pos (point))
	(when (and cfd-re-match (< cfd-re-match cfd-continue-pos))
	  (setq cfd-re-match nil))
	(when (and cfd-prop-match (< cfd-prop-match cfd-continue-pos))
	  (setq cfd-prop-match nil)))

      (if syntactic-pos
	  ;; This is the normal case and we got a proper syntactic
	  ;; position.  If there's a match then it's always outside
	  ;; macros and comments, so advance to the next token and set
	  ;; `cfd-token-pos'.  The loop below will later go back using
	  ;; `cfd-continue-pos' to fix declarations inside the
	  ;; syntactic ws.
	  (when (and cfd-match-pos (< cfd-match-pos syntactic-pos))
	    (goto-char syntactic-pos)
	    (c-forward-syntactic-ws)
	    (and cfd-continue-pos
		 (< cfd-continue-pos (point))
		 (setq cfd-token-pos (point))))

	;; Have one of the special cases when the region is completely
	;; within a literal or macro.  `cfd-continue-pos' is set to a
	;; good start position for the search, so do it.
	(c-find-decl-prefix-search)))

    ;; Now loop.  Round what?  (ACM, 2006/7/5).  We already got the first match.

    (while (progn
	     (while (and
		     (< cfd-match-pos cfd-limit)

		     (or
		      ;; Kludge to filter out matches on the "<" that
		      ;; aren't open parens, for the sake of languages
		      ;; that got `c-recognize-<>-arglists' set.
		      (and (eq (char-before cfd-match-pos) ?<)
			   (not (c-get-char-property (1- cfd-match-pos)
						     'syntax-table)))

		      ;; If `cfd-continue-pos' is less or equal to
		      ;; `cfd-token-pos', we've got a hit inside a macro
		      ;; that's in the syntactic whitespace before the last
		      ;; "real" declaration we've checked.  If they're equal
		      ;; we've arrived at the declaration a second time, so
		      ;; there's nothing to do.
		      (= cfd-continue-pos cfd-token-pos)

		      (progn
			;; If `cfd-continue-pos' is less than `cfd-token-pos'
			;; we're still searching for declarations embedded in
			;; the syntactic whitespace.  In that case we need
			;; only to skip comments and not macros, since they
			;; can't be nested, and that's already been done in
			;; `c-find-decl-prefix-search'.
			(when (> cfd-continue-pos cfd-token-pos)
			  (c-forward-syntactic-ws)
			  (setq cfd-token-pos (point)))

			;; Continue if the following token fails the
			;; CFD-DECL-RE and CFD-FACE-CHECKLIST checks.
			(when (or (>= (point) cfd-limit)
				  (not (looking-at cfd-decl-re))
				  (and cfd-face-checklist
				       (not (c-got-face-at
					     (point) cfd-face-checklist))))
			  (goto-char cfd-continue-pos)
			  t)))

		     (< (point) cfd-limit))
	       (c-find-decl-prefix-search))

	     (< (point) cfd-limit))

      (when (and
	     (>= (point) cfd-start-pos)

	     (progn
	       ;; Narrow to the end of the macro if we got a hit inside
	       ;; one, to avoid recognizing things that start inside the
	       ;; macro and end outside it.
	       (when (> cfd-match-pos cfd-macro-end)
		 ;; Not in the same macro as in the previous round.
		 (save-excursion
		   (goto-char cfd-match-pos)
		   (setq cfd-macro-end
			 (if (save-excursion (and (c-beginning-of-macro)
						  (< (point) cfd-match-pos)))
			     (progn (c-end-of-macro)
				    (point))
			   0))))

	       (if (zerop cfd-macro-end)
		   t
		 (if (> cfd-macro-end (point))
		     (progn (narrow-to-region (point-min) cfd-macro-end)
			    t)
		   ;; The matched token was the last thing in the macro,
		   ;; so the whole match is bogus.
		   (setq cfd-macro-end 0)
		   nil))))

	(c-debug-put-decl-spot-faces cfd-match-pos (point))
	(if (funcall cfd-fun cfd-match-pos (/= cfd-macro-end 0))
	    (setq cfd-prop-match nil))

	(when (/= cfd-macro-end 0)
	  ;; Restore limits if we did macro narrowing above.
	  (narrow-to-region (point-min) cfd-buffer-end)))

      (goto-char cfd-continue-pos)
      (if (= cfd-continue-pos cfd-limit)
	  (setq cfd-match-pos cfd-limit)
	(c-find-decl-prefix-search))))) ; Moves point, sets cfd-continue-pos,
					; cfd-match-pos, etc.


;; A cache for found types.

;; Buffer local variable that contains an obarray with the types we've
;; found.  If a declaration is recognized somewhere we record the
;; fully qualified identifier in it to recognize it as a type
;; elsewhere in the file too.  This is not accurate since we do not
;; bother with the scoping rules of the languages, but in practice the
;; same name is seldom used as both a type and something else in a
;; file, and we only use this as a last resort in ambiguous cases (see
;; `c-forward-decl-or-cast-1').
;;
;; Not every type need be in this cache.  However, things which have
;; ceased to be types must be removed from it.
;;
;; Template types in C++ are added here too but with the template
;; arglist replaced with "<>" in references or "<" for the one in the
;; primary type.  E.g. the type "Foo<A,B>::Bar<C>" is stored as
;; "Foo<>::Bar<".  This avoids storing very long strings (since C++
;; template specs can be fairly sized programs in themselves) and
;; improves the hit ratio (it's a type regardless of the template
;; args; it's just not the same type, but we're only interested in
;; recognizing types, not telling distinct types apart).  Note that
;; template types in references are added here too; from the example
;; above there will also be an entry "Foo<".
(defvar c-found-types nil)
(make-variable-buffer-local 'c-found-types)

(defsubst c-clear-found-types ()
  ;; Clears `c-found-types'.
  (setq c-found-types (make-vector 53 0)))

(defun c-add-type (from to)
  ;; Add the given region as a type in `c-found-types'.  If the region
  ;; doesn't match an existing type but there is a type which is equal
  ;; to the given one except that the last character is missing, then
  ;; the shorter type is removed.  That's done to avoid adding all
  ;; prefixes of a type as it's being entered and font locked.  This
  ;; doesn't cover cases like when characters are removed from a type
  ;; or added in the middle.  We'd need the position of point when the
  ;; font locking is invoked to solve this well.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((type (c-syntactic-content from to c-recognize-<>-arglists)))
    (unless (intern-soft type c-found-types)
      (unintern (substring type 0 -1) c-found-types)
      (intern type c-found-types))))

(defun c-unfind-type (name)
  ;; Remove the "NAME" from c-found-types, if present.
  (unintern name c-found-types))

(defsubst c-check-type (from to)
  ;; Return non-nil if the given region contains a type in
  ;; `c-found-types'.
  ;;
  ;; This function might do hidden buffer changes.
  (intern-soft (c-syntactic-content from to c-recognize-<>-arglists)
	       c-found-types))

(defun c-list-found-types ()
  ;; Return all the types in `c-found-types' as a sorted list of
  ;; strings.
  (let (type-list)
    (mapatoms (lambda (type)
		(setq type-list (cons (symbol-name type)
				      type-list)))
	      c-found-types)
    (sort type-list 'string-lessp)))

;; Shut up the byte compiler.
(defvar c-maybe-stale-found-type)

(defun c-trim-found-types (beg end old-len)
  ;; An after change function which, in conjunction with the info in
  ;; c-maybe-stale-found-type (set in c-before-change), removes a type
  ;; from `c-found-types', should this type have become stale.  For
  ;; example, this happens to "foo" when "foo \n bar();" becomes
  ;; "foo(); \n bar();".  Such stale types, if not removed, foul up
  ;; the fontification.
  ;;
  ;; Have we, perhaps, added non-ws characters to the front/back of a found
  ;; type?
  (when (> end beg)
    (save-excursion
      (when (< end (point-max))
	(goto-char end)
	(if (and (c-beginning-of-current-token) ; only moves when we started in the middle
		 (progn (goto-char end)
			(c-end-of-current-token)))
	    (c-unfind-type (buffer-substring-no-properties
			    end (point)))))
      (when (> beg (point-min))
	(goto-char beg)
	(if (and (c-end-of-current-token) ; only moves when we started in the middle
		 (progn (goto-char beg)
			(c-beginning-of-current-token)))
	    (c-unfind-type (buffer-substring-no-properties
			    (point) beg))))))

  (if c-maybe-stale-found-type ; e.g. (c-decl-id-start "foo" 97 107 " (* ooka) " "o")
      (cond
       ;; Changing the amount of (already existing) whitespace - don't do anything.
       ((and (c-partial-ws-p beg end)
	     (or (= beg end)		; removal of WS
		 (string-match "^[ \t\n\r\f\v]*$" (nth 5 c-maybe-stale-found-type)))))

       ;; The syntactic relationship which defined a "found type" has been
       ;; destroyed.
       ((eq (car c-maybe-stale-found-type) 'c-decl-id-start)
	(c-unfind-type (cadr c-maybe-stale-found-type)))
;;        ((eq (car c-maybe-stale-found-type) 'c-decl-type-start)  FIXME!!!
	)))


;; Setting and removing syntax properties on < and > in languages (C++
;; and Java) where they can be template/generic delimiters as well as
;; their normal meaning of "less/greater than".

;; Normally, < and > have syntax 'punctuation'.  When they are found to
;; be delimiters, they are marked as such with the category properties
;; c-<-as-paren-syntax, c->-as-paren-syntax respectively.

;; STRATEGY:
;;
;; It is impossible to determine with certainty whether a <..> pair in
;; C++ is two comparison operators or is template delimiters, unless
;; one duplicates a lot of a C++ compiler.  For example, the following
;; code fragment:
;;
;;     foo (a < b, c > d) ;
;;
;; could be a function call with two integer parameters (each a
;; relational expression), or it could be a constructor for class foo
;; taking one parameter d of templated type "a < b, c >".  They are
;; somewhat easier to distinguish in Java.
;;
;; The strategy now (2010-01) adopted is to mark and unmark < and
;; > IN MATCHING PAIRS ONLY.  [Previously, they were marked
;; individually when their context so indicated.  This gave rise to
;; intractable problems when one of a matching pair was deleted, or
;; pulled into a literal.]
;;
;; At each buffer change, the syntax-table properties are removed in a
;; before-change function and reapplied, when needed, in an
;; after-change function.  It is far more important that the
;; properties get removed when they they are spurious than that they
;; be present when wanted.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c-clear-<-pair-props (&optional pos)
  ;; POS (default point) is at a < character.  If it is marked with
  ;; open paren syntax-table text property, remove the property,
  ;; together with the close paren property on the matching > (if
  ;; any).
  (save-excursion
    (if pos
	(goto-char pos)
      (setq pos (point)))
    (when (equal (c-get-char-property (point) 'syntax-table)
		 c-<-as-paren-syntax)
      (with-syntax-table c-no-parens-syntax-table ; ignore unbalanced [,{,(,..
	(c-go-list-forward))
      (when (equal (c-get-char-property (1- (point)) 'syntax-table)
		   c->-as-paren-syntax) ; should always be true.
	(c-clear-char-property (1- (point)) 'category))
      (c-clear-char-property pos 'category))))

(defun c-clear->-pair-props (&optional pos)
  ;; POS (default point) is at a > character.  If it is marked with
  ;; close paren syntax-table property, remove the property, together
  ;; with the open paren property on the matching < (if any).
  (save-excursion
    (if pos
	(goto-char pos)
      (setq pos (point)))
    (when (equal (c-get-char-property (point) 'syntax-table)
		 c->-as-paren-syntax)
      (with-syntax-table c-no-parens-syntax-table ; ignore unbalanced [,{,(,..
	(c-go-up-list-backward))
      (when (equal (c-get-char-property (point) 'syntax-table)
			c-<-as-paren-syntax) ; should always be true.
	(c-clear-char-property (point) 'category))
      (c-clear-char-property pos 'category))))

(defun c-clear-<>-pair-props (&optional pos)
  ;; POS (default point) is at a < or > character.  If it has an
  ;; open/close paren syntax-table property, remove this property both
  ;; from the current character and its partner (which will also be
  ;; thusly marked).
  (cond
   ((eq (char-after) ?\<)
    (c-clear-<-pair-props pos))
   ((eq (char-after) ?\>)
    (c-clear->-pair-props pos))
   (t (c-benign-error
       "c-clear-<>-pair-props called from wrong position"))))

(defun c-clear-<-pair-props-if-match-after (lim &optional pos)
  ;; POS (default point) is at a < character.  If it is both marked
  ;; with open/close paren syntax-table property, and has a matching >
  ;; (also marked) which is after LIM, remove the property both from
  ;; the current > and its partner.  Return t when this happens, nil
  ;; when it doesn't.
  (save-excursion
    (if pos
	(goto-char pos)
      (setq pos (point)))
    (when (equal (c-get-char-property (point) 'syntax-table)
		 c-<-as-paren-syntax)
      (with-syntax-table c-no-parens-syntax-table ; ignore unbalanced [,{,(,..
	(c-go-list-forward))
      (when (and (>= (point) lim)
		 (equal (c-get-char-property (1- (point)) 'syntax-table)
			c->-as-paren-syntax)) ; should always be true.
	(c-unmark-<->-as-paren (1- (point)))
	(c-unmark-<->-as-paren pos))
      t)))

(defun c-clear->-pair-props-if-match-before (lim &optional pos)
  ;; POS (default point) is at a > character.  If it is both marked
  ;; with open/close paren syntax-table property, and has a matching <
  ;; (also marked) which is before LIM, remove the property both from
  ;; the current < and its partner.  Return t when this happens, nil
  ;; when it doesn't.
  (save-excursion
    (if pos
	(goto-char pos)
      (setq pos (point)))
    (when (equal (c-get-char-property (point) 'syntax-table)
		 c->-as-paren-syntax)
      (with-syntax-table c-no-parens-syntax-table ; ignore unbalanced [,{,(,..
	(c-go-up-list-backward))
      (when (and (<= (point) lim)
		 (equal (c-get-char-property (point) 'syntax-table)
			c-<-as-paren-syntax)) ; should always be true.
	(c-unmark-<->-as-paren (point))
	(c-unmark-<->-as-paren pos))
      t)))

;; Set by c-common-init in cc-mode.el.
(defvar c-new-BEG)
(defvar c-new-END)

(defun c-before-change-check-<>-operators (beg end)
  ;; Unmark certain pairs of "< .... >" which are currently marked as
  ;; template/generic delimiters.  (This marking is via syntax-table
  ;; text properties).
  ;;
  ;; These pairs are those which are in the current "statement" (i.e.,
  ;; the region between the {, }, or ; before BEG and the one after
  ;; END), and which enclose any part of the interval (BEG END).
  ;;
  ;; Note that in C++ (?and Java), template/generic parens cannot
  ;; enclose a brace or semicolon, so we use these as bounds on the
  ;; region we must work on.
  ;;
  ;; This function is called from before-change-functions (via
  ;; c-get-state-before-change-functions).  Thus the buffer is widened,
  ;; and point is undefined, both at entry and exit.
  ;;
  ;; FIXME!!!  This routine ignores the possibility of macros entirely.
  ;; 2010-01-29.
  (save-excursion
    (let ((beg-lit-limits (progn (goto-char beg) (c-literal-limits)))
	  (end-lit-limits (progn (goto-char end) (c-literal-limits)))
	  new-beg new-end need-new-beg need-new-end)
      ;; Locate the barrier before the changed region
      (goto-char  (if beg-lit-limits (car beg-lit-limits) beg))
      (c-syntactic-skip-backward "^;{}" (c-determine-limit 512))
      (setq new-beg (point))

      ;; Remove the syntax-table properties from each pertinent <...> pair.
      ;; Firsly, the ones with the < before beg and > after beg.
      (while (c-search-forward-char-property 'category 'c-<-as-paren-syntax beg)
	(if (c-clear-<-pair-props-if-match-after beg (1- (point)))
	    (setq need-new-beg t)))

      ;; Locate the barrier after END.
      (goto-char (if end-lit-limits (cdr end-lit-limits) end))
      (c-syntactic-re-search-forward "[;{}]" (c-determine-+ve-limit 512) 'end)
      (setq new-end (point))

      ;; Remove syntax-table properties from the remaining pertinent <...>
      ;; pairs, those with a > after end and < before end.
      (while (c-search-backward-char-property 'category 'c->-as-paren-syntax end)
	(if (c-clear->-pair-props-if-match-before end)
	    (setq need-new-end t)))

      ;; Extend the fontification region, if needed.
      (when need-new-beg
	(goto-char new-beg)
	(c-forward-syntactic-ws)
	(and (< (point) c-new-BEG) (setq c-new-BEG (point))))

      (when need-new-end
	(and (> new-end c-new-END) (setq c-new-END new-end))))))



(defun c-after-change-check-<>-operators (beg end)
  ;; This is called from `after-change-functions' when
  ;; c-recognize-<>-arglists' is set.  It ensures that no "<" or ">"
  ;; chars with paren syntax become part of another operator like "<<"
  ;; or ">=".
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (goto-char beg)
    (when (or (looking-at "[<>]")
	      (< (skip-chars-backward "<>") 0))

      (goto-char beg)
      (c-beginning-of-current-token)
      (when (and (< (point) beg)
		 (looking-at c-<>-multichar-token-regexp)
		 (< beg (setq beg (match-end 0))))
	(while (progn (skip-chars-forward "^<>" beg)
		      (< (point) beg))
	  (c-clear-<>-pair-props)
	  (forward-char))))

    (when (< beg end)
      (goto-char end)
      (when (or (looking-at "[<>]")
		(< (skip-chars-backward "<>") 0))

	(goto-char end)
	(c-beginning-of-current-token)
	(when (and (< (point) end)
		   (looking-at c-<>-multichar-token-regexp)
		   (< end (setq end (match-end 0))))
	  (while (progn (skip-chars-forward "^<>" end)
			(< (point) end))
	    (c-clear-<>-pair-props)
	    (forward-char)))))))



;; Handling of small scale constructs like types and names.

;; Dynamically bound variable that instructs `c-forward-type' to also
;; treat possible types (i.e. those that it normally returns 'maybe or
;; 'found for) as actual types (and always return 'found for them).
;; This means that it records them in `c-record-type-identifiers' if
;; that is set, and that it adds them to `c-found-types'.
(defvar c-promote-possible-types nil)

;; Dynamically bound variable that instructs `c-forward-<>-arglist' to
;; mark up successfully parsed arglists with paren syntax properties on
;; the surrounding angle brackets and with `c-<>-arg-sep' in the
;; `c-type' property of each argument separating comma.
;;
;; Setting this variable also makes `c-forward-<>-arglist' recurse into
;; all arglists for side effects (i.e. recording types), otherwise it
;; exploits any existing paren syntax properties to quickly jump to the
;; end of already parsed arglists.
;;
;; Marking up the arglists is not the default since doing that correctly
;; depends on a proper value for `c-restricted-<>-arglists'.
(defvar c-parse-and-markup-<>-arglists nil)

;; Dynamically bound variable that instructs `c-forward-<>-arglist' to
;; not accept arglists that contain binary operators.
;;
;; This is primarily used to handle C++ template arglists.  C++
;; disambiguates them by checking whether the preceding name is a
;; template or not.  We can't do that, so we assume it is a template
;; if it can be parsed as one.  That usually works well since
;; comparison expressions on the forms "a < b > c" or "a < b, c > d"
;; in almost all cases would be pointless.
;;
;; However, in function arglists, e.g. in "foo (a < b, c > d)", we
;; should let the comma separate the function arguments instead.  And
;; in a context where the value of the expression is taken, e.g. in
;; "if (a < b || c > d)", it's probably not a template.
(defvar c-restricted-<>-arglists nil)

;; Dynamically bound variables that instructs
;; `c-forward-keyword-clause', `c-forward-<>-arglist',
;; `c-forward-name', `c-forward-type', `c-forward-decl-or-cast-1', and
;; `c-forward-label' to record the ranges of all the type and
;; reference identifiers they encounter.  They will build lists on
;; these variables where each element is a cons of the buffer
;; positions surrounding each identifier.  This recording is only
;; activated when `c-record-type-identifiers' is non-nil.
;;
;; All known types that can't be identifiers are recorded, and also
;; other possible types if `c-promote-possible-types' is set.
;; Recording is however disabled inside angle bracket arglists that
;; are encountered inside names and other angle bracket arglists.
;; Such occurrences are taken care of by `c-font-lock-<>-arglists'
;; instead.
;;
;; Only the names in C++ template style references (e.g. "tmpl" in
;; "tmpl<a,b>::foo") are recorded as references, other references
;; aren't handled here.
;;
;; `c-forward-label' records the label identifier(s) on
;; `c-record-ref-identifiers'.
(defvar c-record-type-identifiers nil)
(defvar c-record-ref-identifiers nil)

;; This variable will receive a cons cell of the range of the last
;; single identifier symbol stepped over by `c-forward-name' if it's
;; successful.  This is the range that should be put on one of the
;; record lists above by the caller.  It's assigned nil if there's no
;; such symbol in the name.
(defvar c-last-identifier-range nil)

(defmacro c-record-type-id (range)
  (if (eq (car-safe range) 'cons)
      ;; Always true.
      `(setq c-record-type-identifiers
	     (cons ,range c-record-type-identifiers))
    `(let ((range ,range))
       (if range
	   (setq c-record-type-identifiers
		 (cons range c-record-type-identifiers))))))

(defmacro c-record-ref-id (range)
  (if (eq (car-safe range) 'cons)
      ;; Always true.
      `(setq c-record-ref-identifiers
	     (cons ,range c-record-ref-identifiers))
    `(let ((range ,range))
       (if range
	   (setq c-record-ref-identifiers
		 (cons range c-record-ref-identifiers))))))

;; Dynamically bound variable that instructs `c-forward-type' to
;; record the ranges of types that only are found.  Behaves otherwise
;; like `c-record-type-identifiers'.
(defvar c-record-found-types nil)

(defmacro c-forward-keyword-prefixed-id (type)
  ;; Used internally in `c-forward-keyword-clause' to move forward
  ;; over a type (if TYPE is 'type) or a name (otherwise) which
  ;; possibly is prefixed by keywords and their associated clauses.
  ;; Try with a type/name first to not trip up on those that begin
  ;; with a keyword.  Return t if a known or found type is moved
  ;; over.  The point is clobbered if nil is returned.  If range
  ;; recording is enabled, the identifier is recorded on as a type
  ;; if TYPE is 'type or as a reference if TYPE is 'ref.
  ;;
  ;; This macro might do hidden buffer changes.
  `(let (res)
     (while (if (setq res ,(if (eq type 'type)
			       `(c-forward-type)
			     `(c-forward-name)))
		nil
	      (and (looking-at c-keywords-regexp)
		   (c-forward-keyword-clause 1))))
     (when (memq res '(t known found prefix))
       ,(when (eq type 'ref)
	  `(when c-record-type-identifiers
	     (c-record-ref-id c-last-identifier-range)))
       t)))

(defmacro c-forward-id-comma-list (type update-safe-pos)
  ;; Used internally in `c-forward-keyword-clause' to move forward
  ;; over a comma separated list of types or names using
  ;; `c-forward-keyword-prefixed-id'.
  ;;
  ;; This macro might do hidden buffer changes.
  `(while (and (progn
		 ,(when update-safe-pos
		    `(setq safe-pos (point)))
		 (eq (char-after) ?,))
	       (progn
		 (forward-char)
		 (c-forward-syntactic-ws)
		 (c-forward-keyword-prefixed-id ,type)))))

(defun c-forward-keyword-clause (match)
  ;; Submatch MATCH in the current match data is assumed to surround a
  ;; token.  If it's a keyword, move over it and any immediately
  ;; following clauses associated with it, stopping at the start of
  ;; the next token.  t is returned in that case, otherwise the point
  ;; stays and nil is returned.  The kind of clauses that are
  ;; recognized are those specified by `c-type-list-kwds',
  ;; `c-ref-list-kwds', `c-colon-type-list-kwds',
  ;; `c-paren-nontype-kwds', `c-paren-type-kwds', `c-<>-type-kwds',
  ;; and `c-<>-arglist-kwds'.
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; Note that for `c-colon-type-list-kwds', which doesn't necessary
  ;; apply directly after the keyword, the type list is moved over
  ;; only when there is no unaccounted token before it (i.e. a token
  ;; that isn't moved over due to some other keyword list).  The
  ;; identifier ranges in the list are still recorded if that should
  ;; be done, though.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((kwd-sym (c-keyword-sym (match-string match))) safe-pos pos
	;; The call to `c-forward-<>-arglist' below is made after
	;; `c-<>-sexp-kwds' keywords, so we're certain they actually
	;; are angle bracket arglists and `c-restricted-<>-arglists'
	;; should therefore be nil.
	(c-parse-and-markup-<>-arglists t)
	c-restricted-<>-arglists)

    (when kwd-sym
      (goto-char (match-end match))
      (c-forward-syntactic-ws)
      (setq safe-pos (point))

      (cond
       ((and (c-keyword-member kwd-sym 'c-type-list-kwds)
	     (c-forward-keyword-prefixed-id type))
	;; There's a type directly after a keyword in `c-type-list-kwds'.
	(c-forward-id-comma-list type t))

       ((and (c-keyword-member kwd-sym 'c-ref-list-kwds)
	     (c-forward-keyword-prefixed-id ref))
	;; There's a name directly after a keyword in `c-ref-list-kwds'.
	(c-forward-id-comma-list ref t))

       ((and (c-keyword-member kwd-sym 'c-paren-any-kwds)
	     (eq (char-after) ?\())
	;; There's an open paren after a keyword in `c-paren-any-kwds'.

	(forward-char)
	(when (and (setq pos (c-up-list-forward))
		   (eq (char-before pos) ?\)))
	  (when (and c-record-type-identifiers
		     (c-keyword-member kwd-sym 'c-paren-type-kwds))
	    ;; Use `c-forward-type' on every identifier we can find
	    ;; inside the paren, to record the types.
	    (while (c-syntactic-re-search-forward c-symbol-start pos t)
	      (goto-char (match-beginning 0))
	      (unless (c-forward-type)
		(looking-at c-symbol-key) ; Always matches.
		(goto-char (match-end 0)))))

	  (goto-char pos)
	  (c-forward-syntactic-ws)
	  (setq safe-pos (point))))

       ((and (c-keyword-member kwd-sym 'c-<>-sexp-kwds)
	     (eq (char-after) ?<)
	     (c-forward-<>-arglist (c-keyword-member kwd-sym 'c-<>-type-kwds)))
	(c-forward-syntactic-ws)
	(setq safe-pos (point)))

       ((and (c-keyword-member kwd-sym 'c-nonsymbol-sexp-kwds)
	     (not (looking-at c-symbol-start))
	     (c-safe (c-forward-sexp) t))
	(c-forward-syntactic-ws)
	(setq safe-pos (point))))

      (when (c-keyword-member kwd-sym 'c-colon-type-list-kwds)
	(if (eq (char-after) ?:)
	    ;; If we are at the colon already, we move over the type
	    ;; list after it.
	    (progn
	      (forward-char)
	      (c-forward-syntactic-ws)
	      (when (c-forward-keyword-prefixed-id type)
		(c-forward-id-comma-list type t)))
	  ;; Not at the colon, so stop here.  But the identifier
	  ;; ranges in the type list later on should still be
	  ;; recorded.
	  (and c-record-type-identifiers
	       (progn
		 ;; If a keyword matched both one of the types above and
		 ;; this one, we match `c-colon-type-list-re' after the
		 ;; clause matched above.
		 (goto-char safe-pos)
		 (looking-at c-colon-type-list-re))
	       (progn
		 (goto-char (match-end 0))
		 (c-forward-syntactic-ws)
		 (c-forward-keyword-prefixed-id type))
	       ;; There's a type after the `c-colon-type-list-re' match
	       ;; after a keyword in `c-colon-type-list-kwds'.
	       (c-forward-id-comma-list type nil))))

      (goto-char safe-pos)
      t)))

;; cc-mode requires cc-fonts.
(declare-function c-fontify-recorded-types-and-refs "cc-fonts" ())

(defun c-forward-<>-arglist (all-types)
  ;; The point is assumed to be at a "<".  Try to treat it as the open
  ;; paren of an angle bracket arglist and move forward to the
  ;; corresponding ">".  If successful, the point is left after the
  ;; ">" and t is returned, otherwise the point isn't moved and nil is
  ;; returned.  If ALL-TYPES is t then all encountered arguments in
  ;; the arglist that might be types are treated as found types.
  ;;
  ;; The variable `c-parse-and-markup-<>-arglists' controls how this
  ;; function handles text properties on the angle brackets and argument
  ;; separating commas.
  ;;
  ;; `c-restricted-<>-arglists' controls how lenient the template
  ;; arglist recognition should be.
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((start (point))
	;; If `c-record-type-identifiers' is set then activate
	;; recording of any found types that constitute an argument in
	;; the arglist.
	(c-record-found-types (if c-record-type-identifiers t)))
    (if (catch 'angle-bracket-arglist-escape
	  (setq c-record-found-types
		(c-forward-<>-arglist-recur all-types)))
	(progn
	  (when (consp c-record-found-types)
	    (setq c-record-type-identifiers
		  ;; `nconc' doesn't mind that the tail of
		  ;; `c-record-found-types' is t.
		  (nconc c-record-found-types c-record-type-identifiers)))
	    (if (c-major-mode-is 'java-mode) (c-fontify-recorded-types-and-refs))
	  t)

      (goto-char start)
      nil)))

(defun c-forward-<>-arglist-recur (all-types)
  ;; Recursive part of `c-forward-<>-arglist'.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((start (point)) res pos tmp
	;; Cover this so that any recorded found type ranges are
	;; automatically lost if it turns out to not be an angle
	;; bracket arglist.  It's propagated through the return value
	;; on successful completion.
	(c-record-found-types c-record-found-types)
	;; List that collects the positions after the argument
	;; separating ',' in the arglist.
	arg-start-pos)
    ;; If the '<' has paren open syntax then we've marked it as an angle
    ;; bracket arglist before, so skip to the end.
    (if (and (not c-parse-and-markup-<>-arglists)
	     (c-get-char-property (point) 'syntax-table))

	(progn
	  (forward-char)
	  (if (and (c-go-up-list-forward)
		   (eq (char-before) ?>))
	      t
	    ;; Got unmatched paren angle brackets.  We don't clear the paren
	    ;; syntax properties and retry, on the basis that it's very
	    ;; unlikely that paren angle brackets become operators by code
	    ;; manipulation.  It's far more likely that it doesn't match due
	    ;; to narrowing or some temporary change.
	    (goto-char start)
	    nil))

      (forward-char) ; Forward over the opening '<'.

      (unless (looking-at c-<-op-cont-regexp)
	;; go forward one non-alphanumeric character (group) per iteration of
	;; this loop.
	(while (and
		(progn
		  (c-forward-syntactic-ws)
		  (let ((orig-record-found-types c-record-found-types))
		    (when (or (and c-record-type-identifiers all-types)
			      (c-major-mode-is 'java-mode))
		      ;; All encountered identifiers are types, so set the
		      ;; promote flag and parse the type.
		      (progn
			(c-forward-syntactic-ws)
			(if (looking-at "\\?")
			    (forward-char)
			  (when (looking-at c-identifier-start)
			    (let ((c-promote-possible-types t)
				  (c-record-found-types t))
			      (c-forward-type))))

			(c-forward-syntactic-ws)

			(when (or (looking-at "extends")
				  (looking-at "super"))
			  (forward-word)
			  (c-forward-syntactic-ws)
			  (let ((c-promote-possible-types t)
				(c-record-found-types t))
			    (c-forward-type)
			    (c-forward-syntactic-ws))))))

		  (setq pos (point))    ; e.g. first token inside the '<'

		  ;; Note: These regexps exploit the match order in \| so
		  ;; that "<>" is matched by "<" rather than "[^>:-]>".
		  (c-syntactic-re-search-forward
		   ;; Stop on ',', '|', '&', '+' and '-' to catch
		   ;; common binary operators that could be between
		   ;; two comparison expressions "a<b" and "c>d".
		   "[<;{},|+&-]\\|[>)]"
		   nil t t))

		(cond
		 ((eq (char-before) ?>)
		  ;; Either an operator starting with '>' or the end of
		  ;; the angle bracket arglist.

		  (if (looking-at c->-op-cont-regexp)
		      (progn
			(goto-char (match-end 0))
			t)		; Continue the loop.

		    ;; The angle bracket arglist is finished.
		    (when c-parse-and-markup-<>-arglists
		      (while arg-start-pos
			(c-put-c-type-property (1- (car arg-start-pos))
					       'c-<>-arg-sep)
			(setq arg-start-pos (cdr arg-start-pos)))
		      (c-mark-<-as-paren start)
		      (c-mark->-as-paren (1- (point))))
		    (setq res t)
		    nil))		; Exit the loop.

		 ((eq (char-before) ?<)
		  ;; Either an operator starting with '<' or a nested arglist.
		  (setq pos (point))
		  (let (id-start id-end subres keyword-match)
                  (cond
		     ;; The '<' begins a multi-char operator.
		     ((looking-at c-<-op-cont-regexp)
		      (setq tmp (match-end 0))
		      (goto-char (match-end 0)))
		     ;; We're at a nested <.....>
		     ((progn
			(setq tmp pos)
			(backward-char) ; to the '<'
			(and
			 (save-excursion
			   ;; There's always an identifier before an angle
			   ;; bracket arglist, or a keyword in `c-<>-type-kwds'
			   ;; or `c-<>-arglist-kwds'.
			   (c-backward-syntactic-ws)
			   (setq id-end (point))
			   (c-simple-skip-symbol-backward)
			   (when (or (setq keyword-match
					   (looking-at c-opt-<>-sexp-key))
				     (not (looking-at c-keywords-regexp)))
			     (setq id-start (point))))
			 (setq subres
			       (let ((c-promote-possible-types t)
				     (c-record-found-types t))
				 (c-forward-<>-arglist-recur
				  (and keyword-match
				       (c-keyword-member
					(c-keyword-sym (match-string 1))
					'c-<>-type-kwds)))))))

		      ;; It was an angle bracket arglist.
		      (setq c-record-found-types subres)

		      ;; Record the identifier before the template as a type
		      ;; or reference depending on whether the arglist is last
		      ;; in a qualified identifier.
		      (when (and c-record-type-identifiers
				 (not keyword-match))
			(if (and c-opt-identifier-concat-key
				 (progn
				   (c-forward-syntactic-ws)
				   (looking-at c-opt-identifier-concat-key)))
			    (c-record-ref-id (cons id-start id-end))
                        (c-record-type-id (cons id-start id-end)))))

                   ;; At a "less than" operator.
                   (t
                    (forward-char)
                    )))
                t)                    ; carry on looping.

		 ((and (not c-restricted-<>-arglists)
		       (or (and (eq (char-before) ?&)
				(not (eq (char-after) ?&)))
			   (eq (char-before) ?,)))
		  ;; Just another argument.	 Record the position.  The
		  ;; type check stuff that made us stop at it is at
		  ;; the top of the loop.
		  (setq arg-start-pos (cons (point) arg-start-pos)))

		 (t
		  ;; Got a character that can't be in an angle bracket
		  ;; arglist argument.  Abort using `throw', since
		  ;; it's useless to try to find a surrounding arglist
		  ;; if we're nested.
		  (throw 'angle-bracket-arglist-escape nil))))))
      (if res
	  (or c-record-found-types t)))))

(defun c-backward-<>-arglist (all-types &optional limit)
  ;; The point is assumed to be directly after a ">".  Try to treat it
  ;; as the close paren of an angle bracket arglist and move back to
  ;; the corresponding "<".  If successful, the point is left at
  ;; the "<" and t is returned, otherwise the point isn't moved and
  ;; nil is returned.  ALL-TYPES is passed on to
  ;; `c-forward-<>-arglist'.
  ;;
  ;; If the optional LIMIT is given, it bounds the backward search.
  ;; It's then assumed to be at a syntactically relevant position.
  ;;
  ;; This is a wrapper around `c-forward-<>-arglist'.  See that
  ;; function for more details.

  (let ((start (point)))
    (backward-char)
    (if (and (not c-parse-and-markup-<>-arglists)
	     (c-get-char-property (point) 'syntax-table))

	(if (and (c-go-up-list-backward)
		 (eq (char-after) ?<))
	    t
	  ;; See corresponding note in `c-forward-<>-arglist'.
	  (goto-char start)
	  nil)

      (while (progn
	      (c-syntactic-skip-backward "^<;{}" limit t)

	      (and
	       (if (eq (char-before) ?<)
		   t
		 ;; Stopped at bob or a char that isn't allowed in an
		 ;; arglist, so we've failed.
		 (goto-char start)
		 nil)

	       (if (> (point)
		      (progn (c-beginning-of-current-token)
			     (point)))
		   ;; If we moved then the "<" was part of some
		   ;; multicharacter token.
		   t

		 (backward-char)
		 (let ((beg-pos (point)))
		   (if (c-forward-<>-arglist all-types)
		       (cond ((= (point) start)
			      ;; Matched the arglist.  Break the while.
			      (goto-char beg-pos)
			      nil)
			     ((> (point) start)
			      ;; We started from a non-paren ">" inside an
			      ;; arglist.
			      (goto-char start)
			      nil)
			     (t
			      ;; Matched a shorter arglist.  Can be a nested
			      ;; one so continue looking.
			      (goto-char beg-pos)
			      t))
		     t))))))

      (/= (point) start))))

(defun c-forward-name ()
  ;; Move forward over a complete name if at the beginning of one,
  ;; stopping at the next following token.  A keyword, as such,
  ;; doesn't count as a name.  If the point is not at something that
  ;; is recognized as a name then it stays put.
  ;;
  ;; A name could be something as simple as "foo" in C or something as
  ;; complex as "X<Y<class A<int>::B, BIT_MAX >> b>, ::operator<> ::
  ;; Z<(a>b)> :: operator const X<&foo>::T Q::G<unsigned short
  ;; int>::*volatile const" in C++ (this function is actually little
  ;; more than a `looking-at' call in all modes except those that,
  ;; like C++, have `c-recognize-<>-arglists' set).
  ;;
  ;; Return
  ;; o - nil if no name is found;
  ;; o - 'template if it's an identifier ending with an angle bracket
  ;;   arglist;
  ;; o - 'operator of it's an operator identifier;
  ;; o - t if it's some other kind of name.
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((pos (point)) (start (point)) res id-start id-end
	;; Turn off `c-promote-possible-types' here since we might
	;; call `c-forward-<>-arglist' and we don't want it to promote
	;; every suspect thing in the arglist to a type.  We're
	;; typically called from `c-forward-type' in this case, and
	;; the caller only wants the top level type that it finds to
	;; be promoted.
	c-promote-possible-types)
    (while
	(and
	 (looking-at c-identifier-key)

	 (progn
	   ;; Check for keyword.  We go to the last symbol in
	   ;; `c-identifier-key' first.
	   (goto-char (setq id-end (match-end 0)))
	   (c-simple-skip-symbol-backward)
	   (setq id-start (point))

	   (if (looking-at c-keywords-regexp)
	       (when (and (c-major-mode-is 'c++-mode)
			  (looking-at
			   (cc-eval-when-compile
			     (concat "\\(operator\\|\\(template\\)\\)"
				     "\\(" (c-lang-const c-nonsymbol-key c++)
				     "\\|$\\)")))
			  (if (match-beginning 2)
			      ;; "template" is only valid inside an
			      ;; identifier if preceded by "::".
			      (save-excursion
				(c-backward-syntactic-ws)
				(and (c-safe (backward-char 2) t)
				     (looking-at "::")))
			    t))

		 ;; Handle a C++ operator or template identifier.
		 (goto-char id-end)
		 (c-forward-syntactic-ws)
		 (cond ((eq (char-before id-end) ?e)
			;; Got "... ::template".
			(let ((subres (c-forward-name)))
			  (when subres
			    (setq pos (point)
				  res subres))))

		       ((looking-at c-identifier-start)
			;; Got a cast operator.
			(when (c-forward-type)
			  (setq pos (point)
				res 'operator)
			  ;; Now we should match a sequence of either
			  ;; '*', '&' or a name followed by ":: *",
			  ;; where each can be followed by a sequence
			  ;; of `c-opt-type-modifier-key'.
			  (while (cond ((looking-at "[*&]")
					(goto-char (match-end 0))
					t)
				       ((looking-at c-identifier-start)
					(and (c-forward-name)
					     (looking-at "::")
					     (progn
					       (goto-char (match-end 0))
					       (c-forward-syntactic-ws)
					       (eq (char-after) ?*))
					     (progn
					       (forward-char)
					       t))))
			    (while (progn
				     (c-forward-syntactic-ws)
				     (setq pos (point))
				     (looking-at c-opt-type-modifier-key))
			      (goto-char (match-end 1))))))

		       ((looking-at c-overloadable-operators-regexp)
			;; Got some other operator.
			(setq c-last-identifier-range
			      (cons (point) (match-end 0)))
			(goto-char (match-end 0))
			(c-forward-syntactic-ws)
			(setq pos (point)
			      res 'operator)))

		 nil)

	     ;; `id-start' is equal to `id-end' if we've jumped over
	     ;; an identifier that doesn't end with a symbol token.
	     ;; That can occur e.g. for Java import directives on the
	     ;; form "foo.bar.*".
	     (when (and id-start (/= id-start id-end))
	       (setq c-last-identifier-range
		     (cons id-start id-end)))
	     (goto-char id-end)
	     (c-forward-syntactic-ws)
	     (setq pos (point)
		   res t)))

	 (progn
	   (goto-char pos)
	   (when (or c-opt-identifier-concat-key
		     c-recognize-<>-arglists)

	     (cond
	      ((and c-opt-identifier-concat-key
		    (looking-at c-opt-identifier-concat-key))
	       ;; Got a concatenated identifier.  This handles the
	       ;; cases with tricky syntactic whitespace that aren't
	       ;; covered in `c-identifier-key'.
	       (goto-char (match-end 0))
	       (c-forward-syntactic-ws)
	       t)

	      ((and c-recognize-<>-arglists
		    (eq (char-after) ?<))
	       ;; Maybe an angle bracket arglist.
	       (when (let ((c-record-type-identifiers t)
			   (c-record-found-types t))
		       (c-forward-<>-arglist nil))

		 (c-add-type start (1+ pos))
		 (c-forward-syntactic-ws)
		 (setq pos (point)
		       c-last-identifier-range nil)

		 (if (and c-opt-identifier-concat-key
			  (looking-at c-opt-identifier-concat-key))

		     ;; Continue if there's an identifier concatenation
		     ;; operator after the template argument.
		     (progn
		       (when (and c-record-type-identifiers id-start)
			 (c-record-ref-id (cons id-start id-end)))
		       (forward-char 2)
		       (c-forward-syntactic-ws)
		       t)

		   (when (and c-record-type-identifiers id-start)
		     (c-record-type-id (cons id-start id-end)))
		   (setq res 'template)
		   nil)))
	      )))))

    (goto-char pos)
    res))

(defun c-forward-type (&optional brace-block-too)
  ;; Move forward over a type spec if at the beginning of one,
  ;; stopping at the next following token.  The keyword "typedef"
  ;; isn't part of a type spec here.
  ;;
  ;; BRACE-BLOCK-TOO, when non-nil, means move over the brace block in
  ;; constructs like "struct foo {...} bar ;" or "struct {...} bar;".
  ;; The current (2009-03-10) intention is to convert all uses of
  ;; `c-forward-type' to call with this parameter set, then to
  ;; eliminate it.
  ;;
  ;; Return
  ;;   o - t if it's a known type that can't be a name or other
  ;;     expression;
  ;;   o - 'known if it's an otherwise known type (according to
  ;;     `*-font-lock-extra-types');
  ;;   o - 'prefix if it's a known prefix of a type;
  ;;   o - 'found if it's a type that matches one in `c-found-types';
  ;;   o - 'maybe if it's an identifier that might be a type; or
  ;;   o -  nil if it can't be a type (the point isn't moved then).
  ;;
  ;; The point is assumed to be at the beginning of a token.
  ;;
  ;; Note that this function doesn't skip past the brace definition
  ;; that might be considered part of the type, e.g.
  ;; "enum {a, b, c} foo".
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.
  (when (and c-recognize-<>-arglists
	     (looking-at "<"))
    (c-forward-<>-arglist t)
    (c-forward-syntactic-ws))

  (let ((start (point)) pos res name-res id-start id-end id-range)

    ;; Skip leading type modifiers.  If any are found we know it's a
    ;; prefix of a type.
    (when c-opt-type-modifier-key ; e.g. "const" "volatile", but NOT "typedef"
      (while (looking-at c-opt-type-modifier-key)
	(goto-char (match-end 1))
	(c-forward-syntactic-ws)
	(setq res 'prefix)))

    (cond
     ((looking-at c-type-prefix-key) ; e.g. "struct", "class", but NOT
				     ; "typedef".
      (goto-char (match-end 1))
      (c-forward-syntactic-ws)
      (setq pos (point))

      (setq name-res (c-forward-name))
      (setq res (not (null name-res)))
      (when (eq name-res t)
	;; In many languages the name can be used without the
	;; prefix, so we add it to `c-found-types'.
	(c-add-type pos (point))
	(when (and c-record-type-identifiers
		   c-last-identifier-range)
	  (c-record-type-id c-last-identifier-range)))
      (when (and brace-block-too
		 (memq res '(t nil))
		 (eq (char-after) ?\{)
		 (save-excursion
		   (c-safe
		     (progn (c-forward-sexp)
			    (c-forward-syntactic-ws)
			    (setq pos (point))))))
	(goto-char pos)
	(setq res t))
      (unless res (goto-char start)))	; invalid syntax

     ((progn
	(setq pos nil)
	(if (looking-at c-identifier-start)
	    (save-excursion
	      (setq id-start (point)
		    name-res (c-forward-name))
	      (when name-res
		(setq id-end (point)
		      id-range c-last-identifier-range))))
	(and (cond ((looking-at c-primitive-type-key)
		    (setq res t))
		   ((c-with-syntax-table c-identifier-syntax-table
		      (looking-at c-known-type-key))
		    (setq res 'known)))
	     (or (not id-end)
		 (>= (save-excursion
		       (save-match-data
			 (goto-char (match-end 1))
			 (c-forward-syntactic-ws)
			 (setq pos (point))))
		     id-end)
		 (setq res nil))))
      ;; Looking at a primitive or known type identifier.  We've
      ;; checked for a name first so that we don't go here if the
      ;; known type match only is a prefix of another name.

      (setq id-end (match-end 1))

      (when (and c-record-type-identifiers
		 (or c-promote-possible-types (eq res t)))
	(c-record-type-id (cons (match-beginning 1) (match-end 1))))

      (if (and c-opt-type-component-key
	       (save-match-data
		 (looking-at c-opt-type-component-key)))
	  ;; There might be more keywords for the type.
	  (let (safe-pos)
	    (c-forward-keyword-clause 1)
	    (while (progn
		     (setq safe-pos (point))
		     (looking-at c-opt-type-component-key))
	      (when (and c-record-type-identifiers
			 (looking-at c-primitive-type-key))
		(c-record-type-id (cons (match-beginning 1)
					(match-end 1))))
	      (c-forward-keyword-clause 1))
	    (if (looking-at c-primitive-type-key)
		(progn
		  (when c-record-type-identifiers
		    (c-record-type-id (cons (match-beginning 1)
					    (match-end 1))))
		  (c-forward-keyword-clause 1)
		  (setq res t))
	      (goto-char safe-pos)
	      (setq res 'prefix)))
	(unless (save-match-data (c-forward-keyword-clause 1))
	  (if pos
	      (goto-char pos)
	    (goto-char (match-end 1))
	    (c-forward-syntactic-ws)))))

     (name-res
      (cond ((eq name-res t)
	     ;; A normal identifier.
	     (goto-char id-end)
	     (if (or res c-promote-possible-types)
		 (progn
		   (c-add-type id-start id-end)
		   (when (and c-record-type-identifiers id-range)
		     (c-record-type-id id-range))
		   (unless res
		     (setq res 'found)))
	       (setq res (if (c-check-type id-start id-end)
			     ;; It's an identifier that has been used as
			     ;; a type somewhere else.
			     'found
			   ;; It's an identifier that might be a type.
			   'maybe))))
	    ((eq name-res 'template)
	     ;; A template is a type.
	     (goto-char id-end)
	     (setq res t))
	    (t
	     ;; Otherwise it's an operator identifier, which is not a type.
	     (goto-char start)
	     (setq res nil)))))

    (when res
      ;; Skip trailing type modifiers.	If any are found we know it's
      ;; a type.
      (when c-opt-type-modifier-key
	(while (looking-at c-opt-type-modifier-key) ; e.g. "const", "volatile"
	  (goto-char (match-end 1))
	  (c-forward-syntactic-ws)
	  (setq res t)))
      ;; Step over any type suffix operator.  Do not let the existence
      ;; of these alter the classification of the found type, since
      ;; these operators typically are allowed in normal expressions
      ;; too.
      (when c-opt-type-suffix-key
	(while (looking-at c-opt-type-suffix-key)
	  (goto-char (match-end 1))
	  (c-forward-syntactic-ws)))

      (when c-opt-type-concat-key	; Only/mainly for pike.
	;; Look for a trailing operator that concatenates the type
	;; with a following one, and if so step past that one through
	;; a recursive call.  Note that we don't record concatenated
	;; types in `c-found-types' - it's the component types that
	;; are recorded when appropriate.
	(setq pos (point))
	(let* ((c-promote-possible-types (or (memq res '(t known))
					     c-promote-possible-types))
	       ;; If we can't promote then set `c-record-found-types' so that
	       ;; we can merge in the types from the second part afterwards if
	       ;; it turns out to be a known type there.
	       (c-record-found-types (and c-record-type-identifiers
					  (not c-promote-possible-types)))
	       subres)
	  (if (and (looking-at c-opt-type-concat-key)

		   (progn
		     (goto-char (match-end 1))
		     (c-forward-syntactic-ws)
		     (setq subres (c-forward-type))))

	      (progn
		;; If either operand certainly is a type then both are, but we
		;; don't let the existence of the operator itself promote two
		;; uncertain types to a certain one.
		(cond ((eq res t))
		      ((eq subres t)
		       (unless (eq name-res 'template)
			 (c-add-type id-start id-end))
		       (when (and c-record-type-identifiers id-range)
			 (c-record-type-id id-range))
		       (setq res t))
		      ((eq res 'known))
		      ((eq subres 'known)
		       (setq res 'known))
		      ((eq res 'found))
		      ((eq subres 'found)
		       (setq res 'found))
		      (t
		       (setq res 'maybe)))

		(when (and (eq res t)
			   (consp c-record-found-types))
		  ;; Merge in the ranges of any types found by the second
		  ;; `c-forward-type'.
		  (setq c-record-type-identifiers
			;; `nconc' doesn't mind that the tail of
			;; `c-record-found-types' is t.
			(nconc c-record-found-types
			       c-record-type-identifiers))))

	    (goto-char pos))))

      (when (and c-record-found-types (memq res '(known found)) id-range)
	(setq c-record-found-types
	      (cons id-range c-record-found-types))))

    ;;(message "c-forward-type %s -> %s: %s" start (point) res)

    res))

(defun c-forward-annotation ()
  ;; Used for Java code only at the moment.  Assumes point is on the
  ;; @, moves forward an annotation.  returns nil if there is no
  ;; annotation at point.
  (and (looking-at "@")
       (progn (forward-char) t)
       (c-forward-type)
       (progn (c-forward-syntactic-ws) t)
       (if (looking-at "(")
	   (c-go-list-forward)
         t)))


;; Handling of large scale constructs like statements and declarations.

;; Macro used inside `c-forward-decl-or-cast-1'.  It ought to be a
;; defsubst or perhaps even a defun, but it contains lots of free
;; variables that refer to things inside `c-forward-decl-or-cast-1'.
(defmacro c-fdoc-shift-type-backward (&optional short)
  ;; `c-forward-decl-or-cast-1' can consume an arbitrary length list
  ;; of types when parsing a declaration, which means that it
  ;; sometimes consumes the identifier in the declaration as a type.
  ;; This is used to "backtrack" and make the last type be treated as
  ;; an identifier instead.
  `(progn
     ,(unless short
	;; These identifiers are bound only in the inner let.
	'(setq identifier-type at-type
	       identifier-start type-start
	       got-parens nil
	       got-identifier t
	       got-suffix t
	       got-suffix-after-parens id-start
	       paren-depth 0))

     (if (setq at-type (if (eq backup-at-type 'prefix)
			   t
			 backup-at-type))
	 (setq type-start backup-type-start
	       id-start backup-id-start)
       (setq type-start start-pos
	     id-start start-pos))

     ;; When these flags already are set we've found specifiers that
     ;; unconditionally signal these attributes - backtracking doesn't
     ;; change that.  So keep them set in that case.
     (or at-type-decl
	 (setq at-type-decl backup-at-type-decl))
     (or maybe-typeless
	 (setq maybe-typeless backup-maybe-typeless))

     ,(unless short
	;; This identifier is bound only in the inner let.
	'(setq start id-start))))

(defun c-forward-decl-or-cast-1 (preceding-token-end context last-cast-end)
  ;; Move forward over a declaration or a cast if at the start of one.
  ;; The point is assumed to be at the start of some token.  Nil is
  ;; returned if no declaration or cast is recognized, and the point
  ;; is clobbered in that case.
  ;;
  ;; If a declaration is parsed:
  ;;
  ;;   The point is left at the first token after the first complete
  ;;   declarator, if there is one.  The return value is a cons where
  ;;   the car is the position of the first token in the declarator.  (See
  ;;   below for the cdr.)
  ;;   Some examples:
  ;;
  ;; 	 void foo (int a, char *b) stuff ...
  ;; 	  car ^                    ^ point
  ;; 	 float (*a)[], b;
  ;; 	   car ^     ^ point
  ;; 	 unsigned int a = c_style_initializer, b;
  ;; 		  car ^ ^ point
  ;; 	 unsigned int a (cplusplus_style_initializer), b;
  ;; 		  car ^                              ^ point (might change)
  ;; 	 class Foo : public Bar {}
  ;; 	   car ^   ^ point
  ;; 	 class PikeClass (int a, string b) stuff ...
  ;; 	   car ^                           ^ point
  ;; 	 enum bool;
  ;; 	  car ^   ^ point
  ;; 	 enum bool flag;
  ;; 	       car ^   ^ point
  ;;     void cplusplus_function (int x) throw (Bad);
  ;;      car ^                                     ^ point
  ;;     Foo::Foo (int b) : Base (b) {}
  ;; car ^                ^ point
  ;;
  ;;   The cdr of the return value is non-nil when a
  ;;   `c-typedef-decl-kwds' specifier is found in the declaration.
  ;;   Specifically it is a dotted pair (A . B) where B is t when a
  ;;   `c-typedef-kwds' ("typedef") is present, and A is t when some
  ;;   other `c-typedef-decl-kwds' (e.g. class, struct, enum)
  ;;   specifier is present.  I.e., (some of) the declared
  ;;   identifier(s) are types.
  ;;
  ;; If a cast is parsed:
  ;;
  ;;   The point is left at the first token after the closing paren of
  ;;   the cast.  The return value is `cast'.  Note that the start
  ;;   position must be at the first token inside the cast parenthesis
  ;;   to recognize it.
  ;;
  ;; PRECEDING-TOKEN-END is the first position after the preceding
  ;; token, i.e. on the other side of the syntactic ws from the point.
  ;; Use a value less than or equal to (point-min) if the point is at
  ;; the first token in (the visible part of) the buffer.
  ;;
  ;; CONTEXT is a symbol that describes the context at the point:
  ;; 'decl     In a comma-separated declaration context (typically
  ;;           inside a function declaration arglist).
  ;; '<>       In an angle bracket arglist.
  ;; 'arglist  Some other type of arglist.
  ;; nil       Some other context or unknown context.  Includes
  ;;           within the parens of an if, for, ... construct.
  ;;
  ;; LAST-CAST-END is the first token after the closing paren of a
  ;; preceding cast, or nil if none is known.  If
  ;; `c-forward-decl-or-cast-1' is used in succession, it should be
  ;; the position after the closest preceding call where a cast was
  ;; matched.  In that case it's used to discover chains of casts like
  ;; "(a) (b) c".
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  (let (;; `start-pos' is used below to point to the start of the
	;; first type, i.e. after any leading specifiers.  It might
	;; also point at the beginning of the preceding syntactic
	;; whitespace.
	(start-pos (point))
	;; Set to the result of `c-forward-type'.
	at-type
	;; The position of the first token in what we currently
	;; believe is the type in the declaration or cast, after any
	;; specifiers and their associated clauses.
	type-start
	;; The position of the first token in what we currently
	;; believe is the declarator for the first identifier.  Set
	;; when the type is found, and moved forward over any
	;; `c-decl-hangon-kwds' and their associated clauses that
	;; occurs after the type.
	id-start
	;; These store `at-type', `type-start' and `id-start' of the
	;; identifier before the one in those variables.  The previous
	;; identifier might turn out to be the real type in a
	;; declaration if the last one has to be the declarator in it.
	;; If `backup-at-type' is nil then the other variables have
	;; undefined values.
	backup-at-type backup-type-start backup-id-start
	;; Set if we've found a specifier (apart from "typedef") that makes
	;; the defined identifier(s) types.
	at-type-decl
	;; Set if we've a "typedef" keyword.
	at-typedef
	;; Set if we've found a specifier that can start a declaration
	;; where there's no type.
	maybe-typeless
	;; If a specifier is found that also can be a type prefix,
	;; these flags are set instead of those above.  If we need to
	;; back up an identifier, they are copied to the real flag
	;; variables.  Thus they only take effect if we fail to
	;; interpret it as a type.
	backup-at-type-decl backup-maybe-typeless
	;; Whether we've found a declaration or a cast.  We might know
	;; this before we've found the type in it.  It's 'ids if we've
	;; found two consecutive identifiers (usually a sure sign, but
	;; we should allow that in labels too), and t if we've found a
	;; specifier keyword (a 100% sure sign).
	at-decl-or-cast
	;; Set when we need to back up to parse this as a declaration
	;; but not as a cast.
	backup-if-not-cast
	;; For casts, the return position.
	cast-end
	;; Save `c-record-type-identifiers' and
	;; `c-record-ref-identifiers' since ranges are recorded
	;; speculatively and should be thrown away if it turns out
	;; that it isn't a declaration or cast.
	(save-rec-type-ids c-record-type-identifiers)
	(save-rec-ref-ids c-record-ref-identifiers))

    (while (c-forward-annotation)
      (c-forward-syntactic-ws))

    ;; Check for a type.  Unknown symbols are treated as possible
    ;; types, but they could also be specifiers disguised through
    ;; macros like __INLINE__, so we recognize both types and known
    ;; specifiers after them too.
    (while
	(let* ((start (point)) kwd-sym kwd-clause-end found-type)

	  ;; Look for a specifier keyword clause.
	  (when (or (looking-at c-prefix-spec-kwds-re)
		    (and (c-major-mode-is 'java-mode)
			 (looking-at "@[A-Za-z0-9]+")))
	    (if (looking-at c-typedef-key)
		(setq at-typedef t))
	    (setq kwd-sym (c-keyword-sym (match-string 1)))
	    (save-excursion
	      (c-forward-keyword-clause 1)
	      (setq kwd-clause-end (point))))

	  (when (setq found-type (c-forward-type t)) ; brace-block-too
	    ;; Found a known or possible type or a prefix of a known type.

	    (when at-type
	      ;; Got two identifiers with nothing but whitespace
	      ;; between them.  That can only happen in declarations.
	      (setq at-decl-or-cast 'ids)

	      (when (eq at-type 'found)
		;; If the previous identifier is a found type we
		;; record it as a real one; it might be some sort of
		;; alias for a prefix like "unsigned".
		(save-excursion
		  (goto-char type-start)
		  (let ((c-promote-possible-types t))
		    (c-forward-type)))))

	    (setq backup-at-type at-type
		  backup-type-start type-start
		  backup-id-start id-start
		  at-type found-type
		  type-start start
		  id-start (point)
		  ;; The previous ambiguous specifier/type turned out
		  ;; to be a type since we've parsed another one after
		  ;; it, so clear these backup flags.
		  backup-at-type-decl nil
		  backup-maybe-typeless nil))

	  (if kwd-sym
	      (progn
		;; Handle known specifier keywords and
		;; `c-decl-hangon-kwds' which can occur after known
		;; types.

		(if (c-keyword-member kwd-sym 'c-decl-hangon-kwds)
		    ;; It's a hang-on keyword that can occur anywhere.
		    (progn
		      (setq at-decl-or-cast t)
		      (if at-type
			  ;; Move the identifier start position if
			  ;; we've passed a type.
			  (setq id-start kwd-clause-end)
			;; Otherwise treat this as a specifier and
			;; move the fallback position.
			(setq start-pos kwd-clause-end))
		      (goto-char kwd-clause-end))

		  ;; It's an ordinary specifier so we know that
		  ;; anything before this can't be the type.
		  (setq backup-at-type nil
			start-pos kwd-clause-end)

		  (if found-type
		      ;; It's ambiguous whether this keyword is a
		      ;; specifier or a type prefix, so set the backup
		      ;; flags.  (It's assumed that `c-forward-type'
		      ;; moved further than `c-forward-keyword-clause'.)
		      (progn
			(when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
			  (setq backup-at-type-decl t))
			(when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
			  (setq backup-maybe-typeless t)))

		    (when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
		      ;; This test only happens after we've scanned a type.
		      ;; So, with valid syntax, kwd-sym can't be 'typedef.
		      (setq at-type-decl t))
		    (when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
		      (setq maybe-typeless t))

		    ;; Haven't matched a type so it's an unambiguous
		    ;; specifier keyword and we know we're in a
		    ;; declaration.
		    (setq at-decl-or-cast t)

		    (goto-char kwd-clause-end))))

	    ;; If the type isn't known we continue so that we'll jump
	    ;; over all specifiers and type identifiers.  The reason
	    ;; to do this for a known type prefix is to make things
	    ;; like "unsigned INT16" work.
	    (and found-type (not (eq found-type t))))))

    (cond
     ((eq at-type t)
      ;; If a known type was found, we still need to skip over any
      ;; hangon keyword clauses after it.  Otherwise it has already
      ;; been done in the loop above.
      (while (looking-at c-decl-hangon-key)
	(c-forward-keyword-clause 1))
      (setq id-start (point)))

     ((eq at-type 'prefix)
      ;; A prefix type is itself a primitive type when it's not
      ;; followed by another type.
      (setq at-type t))

     ((not at-type)
      ;; Got no type but set things up to continue anyway to handle
      ;; the various cases when a declaration doesn't start with a
      ;; type.
      (setq id-start start-pos))

     ((and (eq at-type 'maybe)
	   (c-major-mode-is 'c++-mode))
      ;; If it's C++ then check if the last "type" ends on the form
      ;; "foo::foo" or "foo::~foo", i.e. if it's the name of a
      ;; (con|de)structor.
      (save-excursion
	(let (name end-2 end-1)
	  (goto-char id-start)
	  (c-backward-syntactic-ws)
	  (setq end-2 (point))
	  (when (and
		 (c-simple-skip-symbol-backward)
		 (progn
		   (setq name
			 (buffer-substring-no-properties (point) end-2))
		   ;; Cheating in the handling of syntactic ws below.
		   (< (skip-chars-backward ":~ \t\n\r\v\f") 0))
		 (progn
		   (setq end-1 (point))
		   (c-simple-skip-symbol-backward))
		 (>= (point) type-start)
		 (equal (buffer-substring-no-properties (point) end-1)
			name))
	    ;; It is a (con|de)structor name.  In that case the
	    ;; declaration is typeless so zap out any preceding
	    ;; identifier(s) that we might have taken as types.
	    (goto-char type-start)
	    (setq at-type nil
		  backup-at-type nil
		  id-start type-start))))))

    ;; Check for and step over a type decl expression after the thing
    ;; that is or might be a type.  This can't be skipped since we
    ;; need the correct end position of the declarator for
    ;; `max-type-decl-end-*'.
    (let ((start (point)) (paren-depth 0) pos
	  ;; True if there's a non-open-paren match of
	  ;; `c-type-decl-prefix-key'.
	  got-prefix
	  ;; True if the declarator is surrounded by a parenthesis pair.
	  got-parens
	  ;; True if there is an identifier in the declarator.
	  got-identifier
	  ;; True if there's a non-close-paren match of
	  ;; `c-type-decl-suffix-key'.
	  got-suffix
	  ;; True if there's a prefix match outside the outermost
	  ;; paren pair that surrounds the declarator.
	  got-prefix-before-parens
	  ;; True if there's a suffix match outside the outermost
	  ;; paren pair that surrounds the declarator.  The value is
	  ;; the position of the first suffix match.
	  got-suffix-after-parens
	  ;; True if we've parsed the type decl to a token that is
	  ;; known to end declarations in this context.
	  at-decl-end
	  ;; The earlier values of `at-type' and `type-start' if we've
	  ;; shifted the type backwards.
	  identifier-type identifier-start
	  ;; If `c-parse-and-markup-<>-arglists' is set we need to
	  ;; turn it off during the name skipping below to avoid
	  ;; getting `c-type' properties that might be bogus.  That
	  ;; can happen since we don't know if
	  ;; `c-restricted-<>-arglists' will be correct inside the
	  ;; arglist paren that gets entered.
	  c-parse-and-markup-<>-arglists)

      (goto-char id-start)

      ;; Skip over type decl prefix operators.  (Note similar code in
      ;; `c-font-lock-declarators'.)
      (while (and (looking-at c-type-decl-prefix-key)
		  (if (and (c-major-mode-is 'c++-mode)
			   (match-beginning 3))
		      ;; If the second submatch matches in C++ then
		      ;; we're looking at an identifier that's a
		      ;; prefix only if it specifies a member pointer.
		      (when (setq got-identifier (c-forward-name))
			(if (looking-at "\\(::\\)")
			    ;; We only check for a trailing "::" and
			    ;; let the "*" that should follow be
			    ;; matched in the next round.
			    (progn (setq got-identifier nil) t)
			  ;; It turned out to be the real identifier,
			  ;; so stop.
			  nil))
		    t))

	(if (eq (char-after) ?\()
	    (progn
	      (setq paren-depth (1+ paren-depth))
	      (forward-char))
	  (unless got-prefix-before-parens
	    (setq got-prefix-before-parens (= paren-depth 0)))
	  (setq got-prefix t)
	  (goto-char (match-end 1)))
	(c-forward-syntactic-ws))

      (setq got-parens (> paren-depth 0))

      ;; Skip over an identifier.
      (or got-identifier
	  (and (looking-at c-identifier-start)
	       (setq got-identifier (c-forward-name))))

      ;; Skip over type decl suffix operators.
      (while (if (looking-at c-type-decl-suffix-key)

		 (if (eq (char-after) ?\))
		     (when (> paren-depth 0)
		       (setq paren-depth (1- paren-depth))
		       (forward-char)
		       t)
		   (when (if (save-match-data (looking-at "\\s\("))
			     (c-safe (c-forward-sexp 1) t)
			   (goto-char (match-end 1))
			   t)
		     (when (and (not got-suffix-after-parens)
				(= paren-depth 0))
		       (setq got-suffix-after-parens (match-beginning 0)))
		     (setq got-suffix t)))

	       ;; No suffix matched.  We might have matched the
	       ;; identifier as a type and the open paren of a
	       ;; function arglist as a type decl prefix.  In that
	       ;; case we should "backtrack": Reinterpret the last
	       ;; type as the identifier, move out of the arglist and
	       ;; continue searching for suffix operators.
	       ;;
	       ;; Do this even if there's no preceding type, to cope
	       ;; with old style function declarations in K&R C,
	       ;; (con|de)structors in C++ and `c-typeless-decl-kwds'
	       ;; style declarations.  That isn't applicable in an
	       ;; arglist context, though.
	       (when (and (= paren-depth 1)
			  (not got-prefix-before-parens)
			  (not (eq at-type t))
			  (or backup-at-type
			      maybe-typeless
			      backup-maybe-typeless
			      (when c-recognize-typeless-decls
				(not context)))
			  (setq pos (c-up-list-forward (point)))
			  (eq (char-before pos) ?\)))
		 (c-fdoc-shift-type-backward)
		 (goto-char pos)
		 t))

	(c-forward-syntactic-ws))

      (when (and (or maybe-typeless backup-maybe-typeless)
		 (not got-identifier)
		 (not got-prefix)
		 at-type)
	;; Have found no identifier but `c-typeless-decl-kwds' has
	;; matched so we know we're inside a declaration.  The
	;; preceding type must be the identifier instead.
	(c-fdoc-shift-type-backward))

      (setq
       at-decl-or-cast
       (catch 'at-decl-or-cast

	 ;; CASE 1
	(when (> paren-depth 0)
	  ;; Encountered something inside parens that isn't matched by
	  ;; the `c-type-decl-*' regexps, so it's not a type decl
	  ;; expression.  Try to skip out to the same paren depth to
	  ;; not confuse the cast check below.
	  (c-safe (goto-char (scan-lists (point) 1 paren-depth)))
	  ;; If we've found a specifier keyword then it's a
	  ;; declaration regardless.
	  (throw 'at-decl-or-cast (eq at-decl-or-cast t)))

	(setq at-decl-end
	      (looking-at (cond ((eq context '<>) "[,>]")
				(context "[,\)]")
				(t "[,;]"))))

	;; Now we've collected info about various characteristics of
	;; the construct we're looking at.  Below follows a decision
	;; tree based on that.  It's ordered to check more certain
	;; signs before less certain ones.

	(if got-identifier
	    (progn

	      ;; CASE 2
	      (when (and (or at-type maybe-typeless)
			 (not (or got-prefix got-parens)))
		;; Got another identifier directly after the type, so it's a
		;; declaration.
		(throw 'at-decl-or-cast t))

	      (when (and got-parens
			 (not got-prefix)
			 (not got-suffix-after-parens)
			 (or backup-at-type
			     maybe-typeless
			     backup-maybe-typeless))
		;; Got a declaration of the form "foo bar (gnu);" where we've
		;; recognized "bar" as the type and "gnu" as the declarator.
		;; In this case it's however more likely that "bar" is the
		;; declarator and "gnu" a function argument or initializer (if
		;; `c-recognize-paren-inits' is set), since the parens around
		;; "gnu" would be superfluous if it's a declarator.  Shift the
		;; type one step backward.
		(c-fdoc-shift-type-backward)))

	  ;; Found no identifier.

	  (if backup-at-type
	      (progn


		 ;; CASE 3
		 (when (= (point) start)
		   ;; Got a plain list of identifiers. If a colon follows it's
		   ;; a valid label, or maybe a bitfield.  Otherwise the last
		   ;; one probably is the declared identifier and we should
		   ;; back up to the previous type, providing it isn't a cast.
		   (if (and (eq (char-after) ?:)
			    (not (c-major-mode-is 'java-mode)))
		       (cond
			;; If we've found a specifier keyword then it's a
			;; declaration regardless.
			((eq at-decl-or-cast t)
			 (throw 'at-decl-or-cast t))
			((and c-has-bitfields
			      (eq at-decl-or-cast 'ids)) ; bitfield.
			 (setq backup-if-not-cast t)
			 (throw 'at-decl-or-cast t)))

		     (setq backup-if-not-cast t)
		     (throw 'at-decl-or-cast t)))

		;; CASE 4
		(when (and got-suffix
			   (not got-prefix)
			   (not got-parens))
		  ;; Got a plain list of identifiers followed by some suffix.
		  ;; If this isn't a cast then the last identifier probably is
		  ;; the declared one and we should back up to the previous
		  ;; type.
		  (setq backup-if-not-cast t)
		  (throw 'at-decl-or-cast t)))

	    ;; CASE 5
	    (when (eq at-type t)
	      ;; If the type is known we know that there can't be any
	      ;; identifier somewhere else, and it's only in declarations in
	      ;; e.g. function prototypes and in casts that the identifier may
	      ;; be left out.
	      (throw 'at-decl-or-cast t))

	    (when (= (point) start)
	      ;; Only got a single identifier (parsed as a type so far).
	      ;; CASE 6
	      (if (and
		   ;; Check that the identifier isn't at the start of an
		   ;; expression.
		   at-decl-end
		   (cond
		    ((eq context 'decl)
		     ;; Inside an arglist that contains declarations.  If K&R
		     ;; style declarations and parenthesis style initializers
		     ;; aren't allowed then the single identifier must be a
		     ;; type, else we require that it's known or found
		     ;; (primitive types are handled above).
		     (or (and (not c-recognize-knr-p)
			      (not c-recognize-paren-inits))
			 (memq at-type '(known found))))
		    ((eq context '<>)
		     ;; Inside a template arglist.  Accept known and found
		     ;; types; other identifiers could just as well be
		     ;; constants in C++.
		     (memq at-type '(known found)))))
		  (throw 'at-decl-or-cast t)
		;; CASE 7
		;; Can't be a valid declaration or cast, but if we've found a
		;; specifier it can't be anything else either, so treat it as
		;; an invalid/unfinished declaration or cast.
		(throw 'at-decl-or-cast at-decl-or-cast))))

	  (if (and got-parens
		   (not got-prefix)
		   (not context)
		   (not (eq at-type t))
		   (or backup-at-type
		       maybe-typeless
		       backup-maybe-typeless
		       (when c-recognize-typeless-decls
			 (or (not got-suffix)
			     (not (looking-at
				   c-after-suffixed-type-maybe-decl-key))))))
	      ;; Got an empty paren pair and a preceding type that probably
	      ;; really is the identifier.  Shift the type backwards to make
	      ;; the last one the identifier.  This is analogous to the
	      ;; "backtracking" done inside the `c-type-decl-suffix-key' loop
	      ;; above.
	      ;;
	      ;; Exception: In addition to the conditions in that
	      ;; "backtracking" code, do not shift backward if we're not
	      ;; looking at either `c-after-suffixed-type-decl-key' or "[;,]".
	      ;; Since there's no preceding type, the shift would mean that
	      ;; the declaration is typeless.  But if the regexp doesn't match
	      ;; then we will simply fall through in the tests below and not
	      ;; recognize it at all, so it's better to try it as an abstract
	      ;; declarator instead.
	      (c-fdoc-shift-type-backward)

	    ;; Still no identifier.
	    ;; CASE 8
	    (when (and got-prefix (or got-parens got-suffix))
	      ;; Require `got-prefix' together with either `got-parens' or
	      ;; `got-suffix' to recognize it as an abstract declarator:
	      ;; `got-parens' only is probably an empty function call.
	      ;; `got-suffix' only can build an ordinary expression together
	      ;; with the preceding identifier which we've taken as a type.
	      ;; We could actually accept on `got-prefix' only, but that can
	      ;; easily occur temporarily while writing an expression so we
	      ;; avoid that case anyway.  We could do a better job if we knew
	      ;; the point when the fontification was invoked.
	      (throw 'at-decl-or-cast t))

	    ;; CASE 9
	    (when (and at-type
		       (not got-prefix)
		       (not got-parens)
		       got-suffix-after-parens
		       (eq (char-after got-suffix-after-parens) ?\())
	      ;; Got a type, no declarator but a paren suffix. I.e. it's a
	      ;; normal function call after all (or perhaps a C++ style object
	      ;; instantiation expression).
	      (throw 'at-decl-or-cast nil))))

	;; CASE 10
	(when at-decl-or-cast
	  ;; By now we've located the type in the declaration that we know
	  ;; we're in.
	  (throw 'at-decl-or-cast t))

	;; CASE 11
	(when (and got-identifier
		   (not context)
		   (looking-at c-after-suffixed-type-decl-key)
		   (if (and got-parens
			    (not got-prefix)
			    (not got-suffix)
			    (not (eq at-type t)))
		       ;; Shift the type backward in the case that there's a
		       ;; single identifier inside parens.  That can only
		       ;; occur in K&R style function declarations so it's
		       ;; more likely that it really is a function call.
		       ;; Therefore we only do this after
		       ;; `c-after-suffixed-type-decl-key' has matched.
		       (progn (c-fdoc-shift-type-backward) t)
		     got-suffix-after-parens))
	  ;; A declaration according to `c-after-suffixed-type-decl-key'.
	  (throw 'at-decl-or-cast t))

	;; CASE 12
	(when (and (or got-prefix (not got-parens))
		   (memq at-type '(t known)))
	  ;; It's a declaration if a known type precedes it and it can't be a
	  ;; function call.
	  (throw 'at-decl-or-cast t))

	;; If we get here we can't tell if this is a type decl or a normal
	;; expression by looking at it alone.  (That's under the assumption
	;; that normal expressions always can look like type decl expressions,
	;; which isn't really true but the cases where it doesn't hold are so
	;; uncommon (e.g. some placements of "const" in C++) it's not worth
	;; the effort to look for them.)

	(unless (or at-decl-end (looking-at "=[^=]"))
	  ;; If this is a declaration it should end here or its initializer(*)
	  ;; should start here, so check for allowed separation tokens.  Note
	  ;; that this rule doesn't work e.g. with a K&R arglist after a
	  ;; function header.
	  ;;
	  ;; *) Don't check for C++ style initializers using parens
	  ;; since those already have been matched as suffixes.
	  ;;
	  ;; If `at-decl-or-cast' is then we've found some other sign that
	  ;; it's a declaration or cast, so then it's probably an
	  ;; invalid/unfinished one.
	  (throw 'at-decl-or-cast at-decl-or-cast))

	;; Below are tests that only should be applied when we're certain to
	;; not have parsed halfway through an expression.

	;; CASE 14
	(when (memq at-type '(t known))
	  ;; The expression starts with a known type so treat it as a
	  ;; declaration.
	  (throw 'at-decl-or-cast t))

	;; CASE 15
	(when (and (c-major-mode-is 'c++-mode)
		   ;; In C++ we check if the identifier is a known type, since
		   ;; (con|de)structors use the class name as identifier.
		   ;; We've always shifted over the identifier as a type and
		   ;; then backed up again in this case.
		   identifier-type
		   (or (memq identifier-type '(found known))
		       (and (eq (char-after identifier-start) ?~)
			    ;; `at-type' probably won't be 'found for
			    ;; destructors since the "~" is then part of the
			    ;; type name being checked against the list of
			    ;; known types, so do a check without that
			    ;; operator.
			    (or (save-excursion
				  (goto-char (1+ identifier-start))
				  (c-forward-syntactic-ws)
				  (c-with-syntax-table
				      c-identifier-syntax-table
				    (looking-at c-known-type-key)))
				(save-excursion
				  (goto-char (1+ identifier-start))
				  ;; We have already parsed the type earlier,
				  ;; so it'd be possible to cache the end
				  ;; position instead of redoing it here, but
				  ;; then we'd need to keep track of another
				  ;; position everywhere.
				  (c-check-type (point)
						(progn (c-forward-type)
						       (point))))))))
	  (throw 'at-decl-or-cast t))

	(if got-identifier
	    (progn
	      ;; CASE 16
	      (when (and got-prefix-before-parens
			 at-type
			 (or at-decl-end (looking-at "=[^=]"))
			 (not context)
			 (not got-suffix))
		;; Got something like "foo * bar;".  Since we're not inside an
		;; arglist it would be a meaningless expression because the
		;; result isn't used.  We therefore choose to recognize it as
		;; a declaration.  Do not allow a suffix since it could then
		;; be a function call.
		(throw 'at-decl-or-cast t))

	      ;; CASE 17
	      (when (and (or got-suffix-after-parens
			     (looking-at "=[^=]"))
			 (eq at-type 'found)
			 (not (eq context 'arglist)))
		;; Got something like "a (*b) (c);" or "a (b) = c;".  It could
		;; be an odd expression or it could be a declaration.  Treat
		;; it as a declaration if "a" has been used as a type
		;; somewhere else (if it's a known type we won't get here).
		(throw 'at-decl-or-cast t)))

	  ;; CASE 18
	  (when (and context
		     (or got-prefix
			 (and (eq context 'decl)
			      (not c-recognize-paren-inits)
			      (or got-parens got-suffix))))
	    ;; Got a type followed by an abstract declarator.  If `got-prefix'
	    ;; is set it's something like "a *" without anything after it.  If
	    ;; `got-parens' or `got-suffix' is set it's "a()", "a[]", "a()[]",
	    ;; or similar, which we accept only if the context rules out
	    ;; expressions.
	    (throw 'at-decl-or-cast t)))

	;; If we had a complete symbol table here (which rules out
	;; `c-found-types') we should return t due to the disambiguation rule
	;; (in at least C++) that anything that can be parsed as a declaration
	;; is a declaration.  Now we're being more defensive and prefer to
	;; highlight things like "foo (bar);" as a declaration only if we're
	;; inside an arglist that contains declarations.
	(eq context 'decl))))

    ;; The point is now after the type decl expression.

    (cond
     ;; Check for a cast.
     ((save-excursion
	(and
	 c-cast-parens

	 ;; Should be the first type/identifier in a cast paren.
	 (> preceding-token-end (point-min))
	 (memq (char-before preceding-token-end) c-cast-parens)

	 ;; The closing paren should follow.
	 (progn
	   (c-forward-syntactic-ws)
	   (looking-at "\\s\)"))

	 ;; There should be a primary expression after it.
	 (let (pos)
	   (forward-char)
	   (c-forward-syntactic-ws)
	   (setq cast-end (point))
	   (and (looking-at c-primary-expr-regexp)
		(progn
		  (setq pos (match-end 0))
		  (or
		   ;; Check if the expression begins with a prefix keyword.
		   (match-beginning 2)
		   (if (match-beginning 1)
		       ;; Expression begins with an ambiguous operator.  Treat
		       ;; it as a cast if it's a type decl or if we've
		       ;; recognized the type somewhere else.
		       (or at-decl-or-cast
			   (memq at-type '(t known found)))
		     ;; Unless it's a keyword, it's the beginning of a primary
		     ;; expression.
		     (not (looking-at c-keywords-regexp)))))
		;; If `c-primary-expr-regexp' matched a nonsymbol token, check
		;; that it matched a whole one so that we don't e.g. confuse
		;; the operator '-' with '->'.  It's ok if it matches further,
		;; though, since it e.g. can match the float '.5' while the
		;; operator regexp only matches '.'.
		(or (not (looking-at c-nonsymbol-token-regexp))
		    (<= (match-end 0) pos))))

	 ;; There should either be a cast before it or something that isn't an
	 ;; identifier or close paren.
	 (> preceding-token-end (point-min))
	 (progn
	   (goto-char (1- preceding-token-end))
	   (or (eq (point) last-cast-end)
	       (progn
		 (c-backward-syntactic-ws)
		 (if (< (skip-syntax-backward "w_") 0)
		     ;; It's a symbol.  Accept it only if it's one of the
		     ;; keywords that can precede an expression (without
		     ;; surrounding parens).
		     (looking-at c-simple-stmt-key)
		   (and
		    ;; Check that it isn't a close paren (block close is ok,
		    ;; though).
		    (not (memq (char-before) '(?\) ?\])))
		    ;; Check that it isn't a nonsymbol identifier.
		    (not (c-on-identifier)))))))))

      ;; Handle the cast.
      (when (and c-record-type-identifiers at-type (not (eq at-type t)))
	(let ((c-promote-possible-types t))
	  (goto-char type-start)
	  (c-forward-type)))

      (goto-char cast-end)
      'cast)

     (at-decl-or-cast
      ;; We're at a declaration.  Highlight the type and the following
      ;; declarators.

      (when backup-if-not-cast
	(c-fdoc-shift-type-backward t))

      (when (and (eq context 'decl) (looking-at ","))
	;; Make sure to propagate the `c-decl-arg-start' property to
	;; the next argument if it's set in this one, to cope with
	;; interactive refontification.
	(c-put-c-type-property (point) 'c-decl-arg-start))

      (when (and c-record-type-identifiers at-type (not (eq at-type t)))
	(let ((c-promote-possible-types t))
	  (save-excursion
	    (goto-char type-start)
	    (c-forward-type))))

      (cons id-start
	    (and (or at-type-decl at-typedef)
		 (cons at-type-decl at-typedef))))

     (t
      ;; False alarm.  Restore the recorded ranges.
      (setq c-record-type-identifiers save-rec-type-ids
	    c-record-ref-identifiers save-rec-ref-ids)
      nil))))

(defun c-forward-label (&optional assume-markup preceding-token-end limit)
  ;; Assuming that point is at the beginning of a token, check if it starts a
  ;; label and if so move over it and return non-nil (t in default situations,
  ;; specific symbols (see below) for interesting situations), otherwise don't
  ;; move and return nil.  "Label" here means "most things with a colon".
  ;;
  ;; More precisely, a "label" is regarded as one of:
  ;; (i) a goto target like "foo:" - returns the symbol `goto-target';
  ;; (ii) A case label - either the entire construct "case FOO:", or just the
  ;;   bare "case", should the colon be missing.  We return t;
  ;; (iii) a keyword which needs a colon, like "default:" or "private:";  We
  ;;   return t;
  ;; (iv) One of QT's "extended" C++ variants of
  ;;   "private:"/"protected:"/"public:"/"more:" looking like "public slots:".
  ;;   Returns the symbol `qt-2kwds-colon'.
  ;; (v) QT's construct "signals:".  Returns the symbol `qt-1kwd-colon'.
  ;; (vi) One of the keywords matched by `c-opt-extra-label-key' (without any
  ;;   colon).  Currently (2006-03), this applies only to Objective C's
  ;;   keywords "@private", "@protected", and "@public".  Returns t.
  ;;
  ;; One of the things which will NOT be recognized as a label is a bit-field
  ;; element of a struct, something like "int foo:5".
  ;;
  ;; The end of the label is taken to be just after the colon, or the end of
  ;; the first submatch in `c-opt-extra-label-key'.  The point is directly
  ;; after the end on return.  The terminating char gets marked with
  ;; `c-decl-end' to improve recognition of the following declaration or
  ;; statement.
  ;;
  ;; If ASSUME-MARKUP is non-nil, it's assumed that the preceding
  ;; label, if any, has already been marked up like that.
  ;;
  ;; If PRECEDING-TOKEN-END is given, it should be the first position
  ;; after the preceding token, i.e. on the other side of the
  ;; syntactic ws from the point.  Use a value less than or equal to
  ;; (point-min) if the point is at the first token in (the visible
  ;; part of) the buffer.
  ;;
  ;; The optional LIMIT limits the forward scan for the colon.
  ;;
  ;; This function records the ranges of the label symbols on
  ;; `c-record-ref-identifiers' if `c-record-type-identifiers' (!) is
  ;; non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((start (point))
	label-end
	qt-symbol-idx
	macro-start			; if we're in one.
	label-type
	kwd)
    (cond
     ;; "case" or "default" (Doesn't apply to AWK).
     ((looking-at c-label-kwds-regexp)
      (let ((kwd-end (match-end 1)))
	;; Record only the keyword itself for fontification, since in
	;; case labels the following is a constant expression and not
	;; a label.
	(when c-record-type-identifiers
	  (c-record-ref-id (cons (match-beginning 1) kwd-end)))

	;; Find the label end.
	(goto-char kwd-end)
	(setq label-type
	      (if (and (c-syntactic-re-search-forward
			;; Stop on chars that aren't allowed in expressions,
			;; and on operator chars that would be meaningless
			;; there.  FIXME: This doesn't cope with ?: operators.
			"[;{=,@]\\|\\(\\=\\|[^:]\\):\\([^:]\\|\\'\\)"
			limit t t nil 1)
		       (match-beginning 2))

		  (progn		; there's a proper :
		    (goto-char (match-beginning 2)) ; just after the :
		    (c-put-c-type-property (1- (point)) 'c-decl-end)
		    t)

	      ;; It's an unfinished label.  We consider the keyword enough
	      ;; to recognize it as a label, so that it gets fontified.
	      ;; Leave the point at the end of it, but don't put any
	      ;; `c-decl-end' marker.
		(goto-char kwd-end)
		t))))

     ;; @private, @protected, @public, in Objective C, or similar.
     ((and c-opt-extra-label-key
	   (looking-at c-opt-extra-label-key))
      ;; For a `c-opt-extra-label-key' match, we record the whole
      ;; thing for fontification.  That's to get the leading '@' in
      ;; Objective-C protection labels fontified.
      (goto-char (match-end 1))
      (when c-record-type-identifiers
	(c-record-ref-id (cons (match-beginning 1) (point))))
      (c-put-c-type-property (1- (point)) 'c-decl-end)
      (setq label-type t))

     ;; All other cases of labels.
     ((and c-recognize-colon-labels	; nil for AWK and IDL, otherwise t.

	   ;; A colon label must have something before the colon.
	   (not (eq (char-after) ?:))

	   ;; Check that we're not after a token that can't precede a label.
	   (or
	    ;; Trivially succeeds when there's no preceding token.
	    ;; Succeeds when we're at a virtual semicolon.
	    (if preceding-token-end
		(<= preceding-token-end (point-min))
	      (save-excursion
		(c-backward-syntactic-ws)
		(setq preceding-token-end (point))
		(or (bobp)
		    (c-at-vsemi-p))))

	    ;; Check if we're after a label, if we're after a closing
	    ;; paren that belong to statement, and with
	    ;; `c-label-prefix-re'.  It's done in different order
	    ;; depending on `assume-markup' since the checks have
	    ;; different expensiveness.
	    (if assume-markup
		(or
		 (eq (c-get-char-property (1- preceding-token-end) 'c-type)
		     'c-decl-end)

		 (save-excursion
		   (goto-char (1- preceding-token-end))
		   (c-beginning-of-current-token)
		   (or (looking-at c-label-prefix-re)
		       (looking-at c-block-stmt-1-key)))

		 (and (eq (char-before preceding-token-end) ?\))
		      (c-after-conditional)))

	      (or
	       (save-excursion
		 (goto-char (1- preceding-token-end))
		 (c-beginning-of-current-token)
		 (or (looking-at c-label-prefix-re)
		     (looking-at c-block-stmt-1-key)))

	       (cond
		((eq (char-before preceding-token-end) ?\))
		 (c-after-conditional))

		((eq (char-before preceding-token-end) ?:)
		 ;; Might be after another label, so check it recursively.
		 (save-restriction
		   (save-excursion
		     (goto-char (1- preceding-token-end))
		     ;; Essentially the same as the
		     ;; `c-syntactic-re-search-forward' regexp below.
		     (setq macro-start
			   (save-excursion (and (c-beginning-of-macro)
						(point))))
		     (if macro-start (narrow-to-region macro-start (point-max)))
		     (c-syntactic-skip-backward "^-]:?;}=*/%&|,<>!@+" nil t)
		     ;; Note: the following should work instead of the
		     ;; narrow-to-region above.  Investigate why not,
		     ;; sometime.  ACM, 2006-03-31.
		     ;; (c-syntactic-skip-backward "^-]:?;}=*/%&|,<>!@+"
		     ;;				    macro-start t)
		     (let ((pte (point))
			   ;; If the caller turned on recording for us,
			   ;; it shouldn't apply when we check the
			   ;; preceding label.
			   c-record-type-identifiers)
		       ;; A label can't start at a cpp directive.  Check for
		       ;; this, since c-forward-syntactic-ws would foul up on it.
		       (unless (and c-opt-cpp-prefix (looking-at c-opt-cpp-prefix))
			 (c-forward-syntactic-ws)
			 (c-forward-label nil pte start))))))))))

	   ;; Point is still at the beginning of the possible label construct.
	   ;;
	   ;; Check that the next nonsymbol token is ":", or that we're in one
	   ;; of QT's "slots" declarations.  Allow '(' for the sake of macro
	   ;; arguments.  FIXME: Should build this regexp from the language
	   ;; constants.
	   (cond
	    ;; public: protected: private:
	    ((and
	      (c-major-mode-is 'c++-mode)
	      (search-forward-regexp
	       "\\=p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\>[^_]" nil t)
	      (progn (backward-char)
		     (c-forward-syntactic-ws limit)
		     (looking-at ":\\([^:]\\|\\'\\)"))) ; A single colon.
	     (forward-char)
	     (setq label-type t))
	    ;; QT double keyword like "protected slots:" or goto target.
	    ((progn (goto-char start) nil))
	    ((when (c-syntactic-re-search-forward
		    "[ \t\n[:?;{=*/%&|,<>!@+-]" limit t t) ; not at EOB
	       (backward-char)
	       (setq label-end (point))
	       (setq qt-symbol-idx
		     (and (c-major-mode-is 'c++-mode)
			  (string-match
			   "\\(p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|more\\)\\>"
			   (buffer-substring start (point)))))
	       (c-forward-syntactic-ws limit)
	       (cond
		((looking-at ":\\([^:]\\|\\'\\)") ; A single colon.
		 (forward-char)
		 (setq label-type
		       (if (or (string= "signals" ; Special QT macro
					(setq kwd (buffer-substring-no-properties start label-end)))
			       (string= "Q_SIGNALS" kwd))
			   'qt-1kwd-colon
			 'goto-target)))
		((and qt-symbol-idx
		      (search-forward-regexp "\\=\\(slots\\|Q_SLOTS\\)\\>" limit t)
		      (progn (c-forward-syntactic-ws limit)
			     (looking-at ":\\([^:]\\|\\'\\)"))) ; A single colon
		 (forward-char)
		 (setq label-type 'qt-2kwds-colon)))))))

      (save-restriction
	(narrow-to-region start (point))

	;; Check that `c-nonlabel-token-key' doesn't match anywhere.
	(catch 'check-label
	  (goto-char start)
	  (while (progn
		   (when (looking-at c-nonlabel-token-key)
		     (goto-char start)
		     (setq label-type nil)
		     (throw 'check-label nil))
		   (and (c-safe (c-forward-sexp)
				(c-forward-syntactic-ws)
				t)
			(not (eobp)))))

	  ;; Record the identifiers in the label for fontification, unless
	  ;; it begins with `c-label-kwds' in which case the following
	  ;; identifiers are part of a (constant) expression that
	  ;; shouldn't be fontified.
	  (when (and c-record-type-identifiers
		     (progn (goto-char start)
			    (not (looking-at c-label-kwds-regexp))))
	    (while (c-syntactic-re-search-forward c-symbol-key nil t)
	      (c-record-ref-id (cons (match-beginning 0)
				     (match-end 0)))))

	  (c-put-c-type-property (1- (point-max)) 'c-decl-end)
	  (goto-char (point-max)))))

     (t
      ;; Not a label.
      (goto-char start)))
    label-type))

(defun c-forward-objc-directive ()
  ;; Assuming the point is at the beginning of a token, try to move
  ;; forward to the end of the Objective-C directive that starts
  ;; there.  Return t if a directive was fully recognized, otherwise
  ;; the point is moved as far as one could be successfully parsed and
  ;; nil is returned.
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

    (let ((start (point))
	  start-char
	  (c-promote-possible-types t)
	  lim
	  ;; Turn off recognition of angle bracket arglists while parsing
	  ;; types here since the protocol reference list might then be
	  ;; considered part of the preceding name or superclass-name.
	  c-recognize-<>-arglists)

      (if (or
	   (when (looking-at
		  (eval-when-compile
		    (c-make-keywords-re t
		      (append (c-lang-const c-protection-kwds objc)
			      '("@end"))
		      'objc-mode)))
	     (goto-char (match-end 1))
	     t)

	   (and
	    (looking-at
	     (eval-when-compile
	       (c-make-keywords-re t
		 '("@interface" "@implementation" "@protocol")
		 'objc-mode)))

	    ;; Handle the name of the class itself.
	    (progn
;	      (c-forward-token-2) ; 2006/1/13 This doesn't move if the token's
;	      at EOB.
	      (goto-char (match-end 0))
	      (setq lim (point))
	      (c-skip-ws-forward)
	      (c-forward-type))

	    (catch 'break
	      ;; Look for ": superclass-name" or "( category-name )".
	      (when (looking-at "[:\(]")
		(setq start-char (char-after))
		(forward-char)
		(c-forward-syntactic-ws)
		(unless (c-forward-type) (throw 'break nil))
		(when (eq start-char ?\()
		  (unless (eq (char-after) ?\)) (throw 'break nil))
		  (forward-char)
		  (c-forward-syntactic-ws)))

	      ;; Look for a protocol reference list.
	      (if (eq (char-after) ?<)
		  (let ((c-recognize-<>-arglists t)
			(c-parse-and-markup-<>-arglists t)
			c-restricted-<>-arglists)
		    (c-forward-<>-arglist t))
		t))))

	  (progn
	    (c-backward-syntactic-ws lim)
	    (c-clear-c-type-property start (1- (point)) 'c-decl-end)
	    (c-put-c-type-property (1- (point)) 'c-decl-end)
	    t)

	(c-clear-c-type-property start (point) 'c-decl-end)
	nil)))

(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  ;;
  ;; This function might do hidden buffer changes.
  (c-with-syntax-table c++-template-syntax-table
    (c-backward-token-2 0 t lim)
    (while (and (or (looking-at c-symbol-start)
		    (looking-at "[<,]\\|::"))
		(zerop (c-backward-token-2 1 t lim))))))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (beginning-of-line)
    (and c-opt-method-key
	 (looking-at c-opt-method-key)
	 (point))
    ))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-in-gcc-asm-p ()
  ;; Return non-nil if point is within a gcc \"asm\" block.
  ;;
  ;; This should be called with point inside an argument list.
  ;;
  ;; Only one level of enclosing parentheses is considered, so for
  ;; instance `nil' is returned when in a function call within an asm
  ;; operand.
  ;;
  ;; This function might do hidden buffer changes.

  (and c-opt-asm-stmt-key
       (save-excursion
	 (beginning-of-line)
	 (backward-up-list 1)
	 (c-beginning-of-statement-1 (point-min) nil t)
	 (looking-at c-opt-asm-stmt-key))))

(defun c-at-toplevel-p ()
  "Return a determination as to whether point is \"at the top level\".
Informally, \"at the top level\" is anywhere where you can write
a function.

More precisely, being at the top-level means that point is either
outside any enclosing block (such as a function definition), or
directly inside a class, namespace or other block that contains
another declaration level.

If point is not at the top-level (e.g. it is inside a method
definition), then nil is returned.  Otherwise, if point is at a
top-level not enclosed within a class definition, t is returned.
Otherwise, a 2-vector is returned where the zeroth element is the
buffer position of the start of the class declaration, and the first
element is the buffer position of the enclosing class's opening
brace.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (let ((paren-state (c-parse-state)))
    (or (not (c-most-enclosing-brace paren-state))
	(c-search-uplist-for-classkey paren-state))))

(defun c-just-after-func-arglist-p (&optional lim)
  ;; Return non-nil if the point is in the region after the argument
  ;; list of a function and its opening brace (or semicolon in case it
  ;; got no body).  If there are K&R style argument declarations in
  ;; that region, the point has to be inside the first one for this
  ;; function to recognize it.
  ;;
  ;; If successful, the point is moved to the first token after the
  ;; function header (see `c-forward-decl-or-cast-1' for details) and
  ;; the position of the opening paren of the function arglist is
  ;; returned.
  ;;
  ;; The point is clobbered if not successful.
  ;;
  ;; LIM is used as bound for backward buffer searches.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((beg (point)) end id-start)
    (and
     (eq (c-beginning-of-statement-1 lim) 'same)

     (not (and (c-major-mode-is 'objc-mode)
	       (c-forward-objc-directive)))

     (setq id-start
	   (car-safe (c-forward-decl-or-cast-1 (c-point 'bosws) nil nil)))
     (< id-start beg)

     ;; There should not be a '=' or ',' between beg and the
     ;; start of the declaration since that means we were in the
     ;; "expression part" of the declaration.
     (or (> (point) beg)
	 (not (looking-at "[=,]")))

     (save-excursion
       ;; Check that there's an arglist paren in the
       ;; declaration.
       (goto-char id-start)
       (cond ((eq (char-after) ?\()
	      ;; The declarator is a paren expression, so skip past it
	      ;; so that we don't get stuck on that instead of the
	      ;; function arglist.
	      (c-forward-sexp))
	     ((and c-opt-op-identifier-prefix
		   (looking-at c-opt-op-identifier-prefix))
	      ;; Don't trip up on "operator ()".
	      (c-forward-token-2 2 t)))
       (and (< (point) beg)
	    (c-syntactic-re-search-forward "(" beg t t)
	    (1- (point)))))))

(defun c-in-knr-argdecl (&optional lim)
  ;; Return the position of the first argument declaration if point is
  ;; inside a K&R style argument declaration list, nil otherwise.
  ;; `c-recognize-knr-p' is not checked.  If LIM is non-nil, it's a
  ;; position that bounds the backward search for the argument list.
  ;;
  ;; Point must be within a possible K&R region, e.g. just before a top-level
  ;; "{".  It must be outside of parens and brackets.  The test can return
  ;; false positives otherwise.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (save-restriction
      ;; If we're in a macro, our search range is restricted to it.  Narrow to
      ;; the searchable range.
      (let* ((macro-start (save-excursion (and (c-beginning-of-macro) (point))))
	     (macro-end (save-excursion (and macro-start (c-end-of-macro) (point))))
	     (low-lim (max (or lim (point-min))   (or macro-start (point-min))))
	     before-lparen after-rparen
	     (pp-count-out 20))	; Max number of paren/brace constructs before
				; we give up
	(narrow-to-region low-lim (or macro-end (point-max)))

	;; Search backwards for the defun's argument list.  We give up if we
	;; encounter a "}" (end of a previous defun) an "=" (which can't be in
	;; a knr region) or BOB.
	;;
	;; The criterion for a paren structure being the arg list is:
	;; o - there is non-WS stuff after it but before any "{"; AND
	;; o - the token after it isn't a ";" AND
	;; o - it is preceded by either an identifier (the function name) or
	;;   a macro expansion like "DEFUN (...)"; AND
	;; o - its content is a non-empty comma-separated list of identifiers
	;;   (an empty arg list won't have a knr region).
	;;
	;; The following snippet illustrates these rules:
	;; int foo (bar, baz, yuk)
	;;     int bar [] ;
	;;     int (*baz) (my_type) ;
	;;     int (*) (void) (*yuk) (void) ;
	;; {

	(catch 'knr
	  (while (> pp-count-out 0) ; go back one paren/bracket pair each time.
	    (setq pp-count-out (1- pp-count-out))
	    (c-syntactic-skip-backward "^)]}=")
	    (cond ((eq (char-before) ?\))
		   (setq after-rparen (point)))
		  ((eq (char-before) ?\])
		   (setq after-rparen nil))
		  (t ; either } (hit previous defun) or = or no more
		     ; parens/brackets.
		   (throw 'knr nil)))

	    (if after-rparen
	    ;; We're inside a paren.  Could it be our argument list....?
	      (if
		  (and
		   (progn
		     (goto-char after-rparen)
		     (unless (c-go-list-backward) (throw 'knr nil)) ;
		;; FIXME!!!  What about macros between the parens?  2007/01/20
		     (setq before-lparen (point)))

		   ;; It can't be the arg list if next token is ; or {
		   (progn (goto-char after-rparen)
			  (c-forward-syntactic-ws)
			  (not (memq (char-after) '(?\; ?\{ ?\=))))

		   ;; Is the thing preceding the list an identifier (the
		   ;; function name), or a macro expansion?
		   (progn
		     (goto-char before-lparen)
		     (eq (c-backward-token-2) 0)
		     (or (eq (c-on-identifier) (point))
			 (and (eq (char-after) ?\))
			      (c-go-up-list-backward)
			      (eq (c-backward-token-2) 0)
			      (eq (c-on-identifier) (point)))))

		   ;; Have we got a non-empty list of comma-separated
		   ;; identifiers?
		   (progn
		     (goto-char before-lparen)
		     (c-forward-token-2) ; to first token inside parens
		     (and
		      (c-on-identifier)
		      (c-forward-token-2)
		      (catch 'id-list
			(while (eq (char-after) ?\,)
			  (c-forward-token-2)
			  (unless (c-on-identifier) (throw 'id-list nil))
			  (c-forward-token-2))
			(eq (char-after) ?\))))))

		  ;; ...Yes.  We've identified the function's argument list.
		  (throw 'knr
		       (progn (goto-char after-rparen)
			      (c-forward-syntactic-ws)
			      (point)))

		;; ...No.  The current parens aren't the function's arg list.
		(goto-char before-lparen))

	      (or (c-go-list-backward)	; backwards over [ .... ]
		  (throw 'knr nil)))))))))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  ;;
  ;; This function might do hidden buffer changes.
  (c-forward-sexp (cond
		   ;; else if()
		   ((looking-at (concat "\\<else"
					"\\([ \t\n]\\|\\\\\n\\)+"
					"if\\>\\([^_]\\|$\\)"))
		    3)
		   ;; do, else, try, finally
		   ((looking-at (concat "\\<\\("
					"do\\|else\\|try\\|finally"
					"\\)\\>\\([^_]\\|$\\)"))
		    1)
		   ;; for, if, while, switch, catch, synchronized, foreach
		   (t 2))))

(defun c-after-conditional (&optional lim)
  ;; If looking at the token after a conditional then return the
  ;; position of its start, otherwise return nil.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (and (zerop (c-backward-token-2 1 t lim))
	 (or (looking-at c-block-stmt-1-key)
	     (and (eq (char-after) ?\()
		  (zerop (c-backward-token-2 1 t lim))
		  (looking-at c-block-stmt-2-key)))
	 (point))))

(defun c-after-special-operator-id (&optional lim)
  ;; If the point is after an operator identifier that isn't handled
  ;; like an ordinary symbol (i.e. like "operator =" in C++) then the
  ;; position of the start of that identifier is returned.  nil is
  ;; returned otherwise.  The point may be anywhere in the syntactic
  ;; whitespace after the last token of the operator identifier.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (and c-overloadable-operators-regexp
	 (zerop (c-backward-token-2 1 nil lim))
	 (looking-at c-overloadable-operators-regexp)
	 (or (not c-opt-op-identifier-prefix)
	     (and
	      (zerop (c-backward-token-2 1 nil lim))
	      (looking-at c-opt-op-identifier-prefix)))
	 (point))))

(defsubst c-backward-to-block-anchor (&optional lim)
  ;; Assuming point is at a brace that opens a statement block of some
  ;; kind, move to the proper anchor point for that block.  It might
  ;; need to be adjusted further by c-add-stmt-syntax, but the
  ;; position at return is suitable as start position for that
  ;; function.
  ;;
  ;; This function might do hidden buffer changes.
  (unless (= (point) (c-point 'boi))
    (let ((start (c-after-conditional lim)))
      (if start
	  (goto-char start)))))

(defsubst c-backward-to-decl-anchor (&optional lim)
  ;; Assuming point is at a brace that opens the block of a top level
  ;; declaration of some kind, move to the proper anchor point for
  ;; that block.
  ;;
  ;; This function might do hidden buffer changes.
  (unless (= (point) (c-point 'boi))
    (c-beginning-of-statement-1 lim)))

(defun c-search-decl-header-end ()
  ;; Search forward for the end of the "header" of the current
  ;; declaration.  That's the position where the definition body
  ;; starts, or the first variable initializer, or the ending
  ;; semicolon.  I.e. search forward for the closest following
  ;; (syntactically relevant) '{', '=' or ';' token.  Point is left
  ;; _after_ the first found token, or at point-max if none is found.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((base (point)))
    (if (c-major-mode-is 'c++-mode)

	;; In C++ we need to take special care to handle operator
	;; tokens and those pesky template brackets.
	(while (and
		(c-syntactic-re-search-forward "[;{<=]" nil 'move t t)
		(or
		 (c-end-of-current-token base)
		 ;; Handle operator identifiers, i.e. ignore any
		 ;; operator token preceded by "operator".
		 (save-excursion
		   (and (c-safe (c-backward-sexp) t)
			(looking-at c-opt-op-identifier-prefix)))
		 (and (eq (char-before) ?<)
		      (c-with-syntax-table c++-template-syntax-table
			(if (c-safe (goto-char (c-up-list-forward (point))))
			    t
			  (goto-char (point-max))
			  nil)))))
	  (setq base (point)))

      (while (and
	      (c-syntactic-re-search-forward "[;{=]" nil 'move t t)
	      (c-end-of-current-token base))
	(setq base (point))))))

(defun c-beginning-of-decl-1 (&optional lim)
  ;; Go to the beginning of the current declaration, or the beginning
  ;; of the previous one if already at the start of it.  Point won't
  ;; be moved out of any surrounding paren.  Return a cons cell of the
  ;; form (MOVE . KNR-POS).  MOVE is like the return value from
  ;; `c-beginning-of-statement-1'.  If point skipped over some K&R
  ;; style argument declarations (and they are to be recognized) then
  ;; KNR-POS is set to the start of the first such argument
  ;; declaration, otherwise KNR-POS is nil.  If LIM is non-nil, it's a
  ;; position that bounds the backward search.
  ;;
  ;; NB: Cases where the declaration continues after the block, as in
  ;; "struct foo { ... } bar;", are currently recognized as two
  ;; declarations, e.g. "struct foo { ... }" and "bar;" in this case.
  ;;
  ;; This function might do hidden buffer changes.
  (catch 'return
    (let* ((start (point))
	   (last-stmt-start (point))
	   (move (c-beginning-of-statement-1 lim nil t)))

      ;; `c-beginning-of-statement-1' stops at a block start, but we
      ;; want to continue if the block doesn't begin a top level
      ;; construct, i.e. if it isn't preceded by ';', '}', ':', bob,
      ;; or an open paren.
      (let ((beg (point)) tentative-move)
	;; Go back one "statement" each time round the loop until we're just
	;; after a ;, }, or :, or at BOB or the start of a macro or start of
	;; an ObjC method.  This will move over a multiple declaration whose
	;; components are comma separated.
	(while (and
		;; Must check with c-opt-method-key in ObjC mode.
		(not (and c-opt-method-key
			  (looking-at c-opt-method-key)))
		(/= last-stmt-start (point))
		(progn
		  (c-backward-syntactic-ws lim)
		  (not (memq (char-before) '(?\; ?} ?: nil))))
		(save-excursion
		  (backward-char)
		  (not (looking-at "\\s(")))
		;; Check that we don't move from the first thing in a
		;; macro to its header.
		(not (eq (setq tentative-move
			       (c-beginning-of-statement-1 lim nil t))
			 'macro)))
	  (setq last-stmt-start beg
		beg (point)
		move tentative-move))
	(goto-char beg))

      (when c-recognize-knr-p
	(let ((fallback-pos (point)) knr-argdecl-start)
	  ;; Handle K&R argdecls.  Back up after the "statement" jumped
	  ;; over by `c-beginning-of-statement-1', unless it was the
	  ;; function body, in which case we're sitting on the opening
	  ;; brace now.  Then test if we're in a K&R argdecl region and
	  ;; that we started at the other side of the first argdecl in
	  ;; it.
	  (unless (eq (char-after) ?{)
	    (goto-char last-stmt-start))
	  (if (and (setq knr-argdecl-start (c-in-knr-argdecl lim))
		   (< knr-argdecl-start start)
		   (progn
		     (goto-char knr-argdecl-start)
		     (not (eq (c-beginning-of-statement-1 lim nil t) 'macro))))
	      (throw 'return
		     (cons (if (eq (char-after fallback-pos) ?{)
			       'previous
			     'same)
			   knr-argdecl-start))
	    (goto-char fallback-pos))))

      ;; `c-beginning-of-statement-1' counts each brace block as a separate
      ;; statement, so the result will be 'previous if we've moved over any.
      ;; So change our result back to 'same if necessary.
      ;;
      ;; If they were brace list initializers we might not have moved over a
      ;; declaration boundary though, so change it to 'same if we've moved
      ;; past a '=' before '{', but not ';'.  (This ought to be integrated
      ;; into `c-beginning-of-statement-1', so we avoid this extra pass which
      ;; potentially can search over a large amount of text.).  Take special
      ;; pains not to get mislead by C++'s "operator=", and the like.
      (if (and (eq move 'previous)
	       (c-with-syntax-table (if (c-major-mode-is 'c++-mode)
					c++-template-syntax-table
				      (syntax-table))
		 (save-excursion
		   (and
		    (progn
		      (while  ; keep going back to "[;={"s until we either find
			    ; no more, or get to one which isn't an "operator ="
			  (and (c-syntactic-re-search-forward "[;={]" start t t t)
			       (eq (char-before) ?=)
			       c-overloadable-operators-regexp
			       c-opt-op-identifier-prefix
			       (save-excursion
				 (eq (c-backward-token-2) 0)
				 (looking-at c-overloadable-operators-regexp)
				 (eq (c-backward-token-2) 0)
				 (looking-at c-opt-op-identifier-prefix))))
		      (eq (char-before) ?=))
		    (c-syntactic-re-search-forward "[;{]" start t t)
		    (eq (char-before) ?{)
		    (c-safe (goto-char (c-up-list-forward (point))) t)
		    (not (c-syntactic-re-search-forward ";" start t t))))))
	  (cons 'same nil)
	(cons move nil)))))

(defun c-end-of-decl-1 ()
  ;; Assuming point is at the start of a declaration (as detected by
  ;; e.g. `c-beginning-of-decl-1'), go to the end of it.  Unlike
  ;; `c-beginning-of-decl-1', this function handles the case when a
  ;; block is followed by identifiers in e.g. struct declarations in C
  ;; or C++.  If a proper end was found then t is returned, otherwise
  ;; point is moved as far as possible within the current sexp and nil
  ;; is returned.  This function doesn't handle macros; use
  ;; `c-end-of-macro' instead in those cases.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((start (point))
	(decl-syntax-table (if (c-major-mode-is 'c++-mode)
			       c++-template-syntax-table
			     (syntax-table))))
    (catch 'return
      (c-search-decl-header-end)

      (when (and c-recognize-knr-p
		 (eq (char-before) ?\;)
		 (c-in-knr-argdecl start))
	;; Stopped at the ';' in a K&R argdecl section which is
	;; detected using the same criteria as in
	;; `c-beginning-of-decl-1'.  Move to the following block
	;; start.
	(c-syntactic-re-search-forward "{" nil 'move t))

      (when (eq (char-before) ?{)
	;; Encountered a block in the declaration.  Jump over it.
	(condition-case nil
	    (goto-char (c-up-list-forward (point)))
	  (error (goto-char (point-max))
		 (throw 'return nil)))
	(if (or (not c-opt-block-decls-with-vars-key)
		(save-excursion
		  (c-with-syntax-table decl-syntax-table
		    (let ((lim (point)))
		      (goto-char start)
		      (not (and
			    ;; Check for `c-opt-block-decls-with-vars-key'
			    ;; before the first paren.
			    (c-syntactic-re-search-forward
			     (concat "[;=\(\[{]\\|\\("
				     c-opt-block-decls-with-vars-key
				     "\\)")
			     lim t t t)
			    (match-beginning 1)
			    (not (eq (char-before) ?_))
			    ;; Check that the first following paren is
			    ;; the block.
			    (c-syntactic-re-search-forward "[;=\(\[{]"
							   lim t t t)
			    (eq (char-before) ?{)))))))
	    ;; The declaration doesn't have any of the
	    ;; `c-opt-block-decls-with-vars' keywords in the
	    ;; beginning, so it ends here at the end of the block.
	    (throw 'return t)))

      (c-with-syntax-table decl-syntax-table
	(while (progn
		 (if (eq (char-before) ?\;)
		     (throw 'return t))
		 (c-syntactic-re-search-forward ";" nil 'move t))))
      nil)))

(defun c-looking-at-decl-block (containing-sexp goto-start &optional limit)
  ;; Assuming the point is at an open brace, check if it starts a
  ;; block that contains another declaration level, i.e. that isn't a
  ;; statement block or a brace list, and if so return non-nil.
  ;;
  ;; If the check is successful, the return value is the start of the
  ;; keyword that tells what kind of construct it is, i.e. typically
  ;; what `c-decl-block-key' matched.  Also, if GOTO-START is set then
  ;; the point will be at the start of the construct, before any
  ;; leading specifiers, otherwise it's at the returned position.
  ;;
  ;; The point is clobbered if the check is unsuccessful.
  ;;
  ;; CONTAINING-SEXP is the position of the open of the surrounding
  ;; paren, or nil if none.
  ;;
  ;; The optional LIMIT limits the backward search for the start of
  ;; the construct.  It's assumed to be at a syntactically relevant
  ;; position.
  ;;
  ;; If any template arglists are found in the searched region before
  ;; the open brace, they get marked with paren syntax.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((open-brace (point)) kwd-start first-specifier-pos)
    (c-syntactic-skip-backward c-block-prefix-charset limit t)

    (when (and c-recognize-<>-arglists
	       (eq (char-before) ?>))
      ;; Could be at the end of a template arglist.
      (let ((c-parse-and-markup-<>-arglists t)
	    (c-disallow-comma-in-<>-arglists
	     (and containing-sexp
		  (not (eq (char-after containing-sexp) ?{)))))
	(while (and
		(c-backward-<>-arglist nil limit)
		(progn
		  (c-syntactic-skip-backward c-block-prefix-charset limit t)
		  (eq (char-before) ?>))))))

    ;; Note: Can't get bogus hits inside template arglists below since they
    ;; have gotten paren syntax above.
    (when (and
	   ;; If `goto-start' is set we begin by searching for the
	   ;; first possible position of a leading specifier list.
	   ;; The `c-decl-block-key' search continues from there since
	   ;; we know it can't match earlier.
	   (if goto-start
	       (when (c-syntactic-re-search-forward c-symbol-start
						    open-brace t t)
		 (goto-char (setq first-specifier-pos (match-beginning 0)))
		 t)
	     t)

	   (cond
	    ((c-syntactic-re-search-forward c-decl-block-key open-brace t t t)
	     (goto-char (setq kwd-start (match-beginning 0)))
	     (or

	      ;; Found a keyword that can't be a type?
	      (match-beginning 1)

	      ;; Can be a type too, in which case it's the return type of a
	      ;; function (under the assumption that no declaration level
	      ;; block construct starts with a type).
	      (not (c-forward-type))

	      ;; Jumped over a type, but it could be a declaration keyword
	      ;; followed by the declared identifier that we've jumped over
	      ;; instead (e.g. in "class Foo {").  If it indeed is a type
	      ;; then we should be at the declarator now, so check for a
	      ;; valid declarator start.
	      ;;
	      ;; Note: This doesn't cope with the case when a declared
	      ;; identifier is followed by e.g. '(' in a language where '('
	      ;; also might be part of a declarator expression.  Currently
	      ;; there's no such language.
	      (not (or (looking-at c-symbol-start)
		       (looking-at c-type-decl-prefix-key)))))

	    ;; In Pike a list of modifiers may be followed by a brace
	    ;; to make them apply to many identifiers.  Note that the
	    ;; match data will be empty on return in this case.
	    ((and (c-major-mode-is 'pike-mode)
		  (progn
		    (goto-char open-brace)
		    (= (c-backward-token-2) 0))
		  (looking-at c-specifier-key)
		  ;; Use this variant to avoid yet another special regexp.
		  (c-keyword-member (c-keyword-sym (match-string 1))
				    'c-modifier-kwds))
	     (setq kwd-start (point))
	     t)))

      ;; Got a match.

      (if goto-start
	  ;; Back up over any preceding specifiers and their clauses
	  ;; by going forward from `first-specifier-pos', which is the
	  ;; earliest possible position where the specifier list can
	  ;; start.
	  (progn
	    (goto-char first-specifier-pos)

	    (while (< (point) kwd-start)
	      (if (looking-at c-symbol-key)
		  ;; Accept any plain symbol token on the ground that
		  ;; it's a specifier masked through a macro (just
		  ;; like `c-forward-decl-or-cast-1' skip forward over
		  ;; such tokens).
		  ;;
		  ;; Could be more restrictive wrt invalid keywords,
		  ;; but that'd only occur in invalid code so there's
		  ;; no use spending effort on it.
		  (let ((end (match-end 0)))
		    (unless (c-forward-keyword-clause 0)
		      (goto-char end)
		      (c-forward-syntactic-ws)))

		;; Can't parse a declaration preamble and is still
		;; before `kwd-start'.  That means `first-specifier-pos'
		;; was in some earlier construct.  Search again.
		(if (c-syntactic-re-search-forward c-symbol-start
						   kwd-start 'move t)
		    (goto-char (setq first-specifier-pos (match-beginning 0)))
		  ;; Got no preamble before the block declaration keyword.
		  (setq first-specifier-pos kwd-start))))

	    (goto-char first-specifier-pos))
	(goto-char kwd-start))

      kwd-start)))

(defun c-search-uplist-for-classkey (paren-state)
  ;; Check if the closest containing paren sexp is a declaration
  ;; block, returning a 2 element vector in that case.  Aref 0
  ;; contains the bufpos at boi of the class key line, and aref 1
  ;; contains the bufpos of the open brace.  This function is an
  ;; obsolete wrapper for `c-looking-at-decl-block'.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((open-paren-pos (c-most-enclosing-brace paren-state)))
    (when open-paren-pos
      (save-excursion
	(goto-char open-paren-pos)
	(when (and (eq (char-after) ?{)
		   (c-looking-at-decl-block
		    (c-safe-position open-paren-pos paren-state)
		    nil))
	  (back-to-indentation)
	  (vector (point) open-paren-pos))))))

(defmacro c-pull-open-brace (ps)
  ;; Pull the next open brace from PS (which has the form of paren-state),
  ;; skipping over any brace pairs.  Returns NIL when PS is exhausted.
  `(progn
     (while (consp (car ,ps))
       (setq ,ps (cdr ,ps)))
     (prog1 (car ,ps)
       (setq ,ps (cdr ,ps)))))

(defun c-most-enclosing-decl-block (paren-state)
  ;; Return the buffer position of the most enclosing decl-block brace (in the
  ;; sense of c-looking-at-decl-block) in the PAREN-STATE structure, or nil if
  ;; none was found.
  (let* ((open-brace (c-pull-open-brace paren-state))
	 (next-open-brace (c-pull-open-brace paren-state)))
    (while (and open-brace
		(save-excursion
		  (goto-char open-brace)
		  (not (c-looking-at-decl-block next-open-brace nil))))
      (setq open-brace next-open-brace
	    next-open-brace (c-pull-open-brace paren-state)))
    open-brace))

(defun c-cheap-inside-bracelist-p (paren-state)
  ;; Return the position of the L-brace if point is inside a brace list
  ;; initialization of an array, etc.  This is an approximate function,
  ;; designed for speed over accuracy.  It will not find every bracelist, but
  ;; a non-nil result is reliable.  We simply search for "= {" (naturally with
  ;; syntactic whitespace allowed).  PAREN-STATE is the normal thing that it
  ;; is everywhere else.
  (let (b-pos)
    (save-excursion
      (while
	  (and (setq b-pos (c-pull-open-brace paren-state))
	       (progn (goto-char b-pos)
		      (c-backward-sws)
		      (c-backward-token-2)
		      (not (looking-at "=")))))
      b-pos)))

(defun c-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  PAREN-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; placed in inconvenient locations.  It's a trade-off we make for
  ;; speed.
  ;;
  ;; This function might do hidden buffer changes.
  (or
   ;; This will pick up brace list declarations.
   (c-safe
    (save-excursion
      (goto-char containing-sexp)
      (c-forward-sexp -1)
      (let (bracepos)
	(if (and (or (looking-at c-brace-list-key)
		     (progn (c-forward-sexp -1)
			    (looking-at c-brace-list-key)))
		 (setq bracepos (c-down-list-forward (point)))
		 (not (c-crosses-statement-barrier-p (point)
						     (- bracepos 2))))
	    (point)))))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
	    ;; Pike can have class definitions anywhere, so we must
	    ;; check for the class key here.
	    (and (c-major-mode-is 'pike-mode)
		 c-decl-block-key))
	   bufpos braceassignp lim next-containing)
       (while (and (not bufpos)
		   containing-sexp)
	   (when paren-state
	     (if (consp (car paren-state))
		 (setq lim (cdr (car paren-state))
		       paren-state (cdr paren-state))
	       (setq lim (car paren-state)))
	     (when paren-state
	       (setq next-containing (car paren-state)
		     paren-state (cdr paren-state))))
	   (goto-char containing-sexp)
	   (if (c-looking-at-inexpr-block next-containing next-containing)
	       ;; We're in an in-expression block of some kind.  Do not
	       ;; check nesting.  We deliberately set the limit to the
	       ;; containing sexp, so that c-looking-at-inexpr-block
	       ;; doesn't check for an identifier before it.
	       (setq containing-sexp nil)
	     ;; see if the open brace is preceded by = or [...] in
	     ;; this statement, but watch out for operator=
	     (setq braceassignp 'dontknow)
	     (c-backward-token-2 1 t lim)
	     ;; Checks to do only on the first sexp before the brace.
	     (when (and c-opt-inexpr-brace-list-key
			(eq (char-after) ?\[))
	       ;; In Java, an initialization brace list may follow
	       ;; directly after "new Foo[]", so check for a "new"
	       ;; earlier.
	       (while (eq braceassignp 'dontknow)
		 (setq braceassignp
		       (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
			     ((looking-at c-opt-inexpr-brace-list-key) t)
			     ((looking-at "\\sw\\|\\s_\\|[.[]")
			      ;; Carry on looking if this is an
			      ;; identifier (may contain "." in Java)
			      ;; or another "[]" sexp.
			      'dontknow)
			     (t nil)))))
	     ;; Checks to do on all sexps before the brace, up to the
	     ;; beginning of the statement.
	     (while (eq braceassignp 'dontknow)
	       (cond ((eq (char-after) ?\;)
		      (setq braceassignp nil))
		     ((and class-key
			   (looking-at class-key))
		      (setq braceassignp nil))
		     ((eq (char-after) ?=)
		      ;; We've seen a =, but must check earlier tokens so
		      ;; that it isn't something that should be ignored.
		      (setq braceassignp 'maybe)
		      (while (and (eq braceassignp 'maybe)
				  (zerop (c-backward-token-2 1 t lim)))
			(setq braceassignp
			      (cond
			       ;; Check for operator =
			       ((and c-opt-op-identifier-prefix
				     (looking-at c-opt-op-identifier-prefix))
				nil)
			       ;; Check for `<opchar>= in Pike.
			       ((and (c-major-mode-is 'pike-mode)
				     (or (eq (char-after) ?`)
					 ;; Special case for Pikes
					 ;; `[]=, since '[' is not in
					 ;; the punctuation class.
					 (and (eq (char-after) ?\[)
					      (eq (char-before) ?`))))
				nil)
			       ((looking-at "\\s.") 'maybe)
			       ;; make sure we're not in a C++ template
			       ;; argument assignment
			       ((and
				 (c-major-mode-is 'c++-mode)
				 (save-excursion
				   (let ((here (point))
					 (pos< (progn
						 (skip-chars-backward "^<>")
						 (point))))
				     (and (eq (char-before) ?<)
					  (not (c-crosses-statement-barrier-p
						pos< here))
					  (not (c-in-literal))
					  ))))
				nil)
			       (t t))))))
	       (if (and (eq braceassignp 'dontknow)
			(/= (c-backward-token-2 1 t lim) 0))
		   (setq braceassignp nil)))
	     (if (not braceassignp)
		 (if (eq (char-after) ?\;)
		     ;; Brace lists can't contain a semicolon, so we're done.
		     (setq containing-sexp nil)
		   ;; Go up one level.
		   (setq containing-sexp next-containing
			 lim nil
			 next-containing nil))
	       ;; we've hit the beginning of the aggregate list
	       (c-beginning-of-statement-1
		(c-most-enclosing-brace paren-state))
	       (setq bufpos (point))))
	   )
       bufpos))
   ))

(defun c-looking-at-special-brace-list (&optional lim)
  ;; If we're looking at the start of a pike-style list, ie `({ })',
  ;; `([ ])', `(< >)' etc, a cons of a cons of its starting and ending
  ;; positions and its entry in c-special-brace-lists is returned, nil
  ;; otherwise.  The ending position is nil if the list is still open.
  ;; LIM is the limit for forward search.  The point may either be at
  ;; the `(' or at the following paren character.  Tries to check the
  ;; matching closer, but assumes it's correct if no balanced paren is
  ;; found (i.e. the case `({ ... } ... )' is detected as _not_ being
  ;; a special brace list).
  ;;
  ;; This function might do hidden buffer changes.
  (if c-special-brace-lists
      (condition-case ()
	  (save-excursion
	    (let ((beg (point))
		  inner-beg end type)
	      (c-forward-syntactic-ws)
	      (if (eq (char-after) ?\()
		  (progn
		    (forward-char 1)
		    (c-forward-syntactic-ws)
		    (setq inner-beg (point))
		    (setq type (assq (char-after) c-special-brace-lists)))
		(if (setq type (assq (char-after) c-special-brace-lists))
		    (progn
		      (setq inner-beg (point))
		      (c-backward-syntactic-ws)
		      (forward-char -1)
		      (setq beg (if (eq (char-after) ?\()
				    (point)
				  nil)))))
	      (if (and beg type)
		  (if (and (c-safe
			     (goto-char beg)
			     (c-forward-sexp 1)
			     (setq end (point))
			     (= (char-before) ?\)))
			   (c-safe
			     (goto-char inner-beg)
			     (if (looking-at "\\s(")
				 ;; Check balancing of the inner paren
				 ;; below.
				 (progn
				   (c-forward-sexp 1)
				   t)
			       ;; If the inner char isn't a paren then
			       ;; we can't check balancing, so just
			       ;; check the char before the outer
			       ;; closing paren.
			       (goto-char end)
			       (backward-char)
			       (c-backward-syntactic-ws)
			       (= (char-before) (cdr type)))))
		      (if (or (/= (char-syntax (char-before)) ?\))
			      (= (progn
				   (c-forward-syntactic-ws)
				   (point))
				 (1- end)))
			  (cons (cons beg end) type))
		    (cons (list beg) type)))))
	(error nil))))

(defun c-looking-at-bos (&optional lim)
  ;; Return non-nil if between two statements or declarations, assuming
  ;; point is not inside a literal or comment.
  ;;
  ;; Obsolete - `c-at-statement-start-p' or `c-at-expression-start-p'
  ;; are recommended instead.
  ;;
  ;; This function might do hidden buffer changes.
  (c-at-statement-start-p))
(make-obsolete 'c-looking-at-bos 'c-at-statement-start-p "22.1")

(defun c-looking-at-inexpr-block (lim containing-sexp &optional check-at-end)
  ;; Return non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.
  ;;
  ;; LIM limits the backward search.  CONTAINING-SEXP is the start
  ;; position of the closest containing list.  If it's nil, the
  ;; containing paren isn't used to decide whether we're inside an
  ;; expression or not.  If both LIM and CONTAINING-SEXP are used, LIM
  ;; needs to be farther back.
  ;;
  ;; If CHECK-AT-END is non-nil then extra checks at the end of the
  ;; brace block might be done.  It should only be used when the
  ;; construct can be assumed to be complete, i.e. when the original
  ;; starting position was further down than that.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (let ((res 'maybe) passed-paren
	  (closest-lim (or containing-sexp lim (point-min)))
	  ;; Look at the character after point only as a last resort
	  ;; when we can't disambiguate.
	  (block-follows (and (eq (char-after) ?{) (point))))

      (while (and (eq res 'maybe)
		  (progn (c-backward-syntactic-ws)
			 (> (point) closest-lim))
		  (not (bobp))
		  (progn (backward-char)
			 (looking-at "[\]\).]\\|\\w\\|\\s_"))
		  (c-safe (forward-char)
			  (goto-char (scan-sexps (point) -1))))

	(setq res
	      (if (looking-at c-keywords-regexp)
		  (let ((kw-sym (c-keyword-sym (match-string 1))))
		    (cond
		     ((and block-follows
			   (c-keyword-member kw-sym 'c-inexpr-class-kwds))
		      (and (not (eq passed-paren ?\[))
			   (or (not (looking-at c-class-key))
			       ;; If the class definition is at the start of
			       ;; a statement, we don't consider it an
			       ;; in-expression class.
			       (let ((prev (point)))
				 (while (and
					 (= (c-backward-token-2 1 nil closest-lim) 0)
					 (eq (char-syntax (char-after)) ?w))
				   (setq prev (point)))
				 (goto-char prev)
				 (not (c-at-statement-start-p)))
			       ;; Also, in Pike we treat it as an
			       ;; in-expression class if it's used in an
			       ;; object clone expression.
			       (save-excursion
				 (and check-at-end
				      (c-major-mode-is 'pike-mode)
				      (progn (goto-char block-follows)
					     (zerop (c-forward-token-2 1 t)))
				      (eq (char-after) ?\())))
			   (cons 'inexpr-class (point))))
		     ((c-keyword-member kw-sym 'c-inexpr-block-kwds)
		      (when (not passed-paren)
			(cons 'inexpr-statement (point))))
		     ((c-keyword-member kw-sym 'c-lambda-kwds)
		      (when (or (not passed-paren)
				(eq passed-paren ?\())
			(cons 'inlambda (point))))
		     ((c-keyword-member kw-sym 'c-block-stmt-kwds)
		      nil)
		     (t
		      'maybe)))

		(if (looking-at "\\s(")
		    (if passed-paren
			(if (and (eq passed-paren ?\[)
				 (eq (char-after) ?\[))
			    ;; Accept several square bracket sexps for
			    ;; Java array initializations.
			    'maybe)
		      (setq passed-paren (char-after))
		      'maybe)
		  'maybe))))

      (if (eq res 'maybe)
	  (when (and c-recognize-paren-inexpr-blocks
		     block-follows
		     containing-sexp
		     (eq (char-after containing-sexp) ?\())
	    (goto-char containing-sexp)
	    (if (or (save-excursion
		      (c-backward-syntactic-ws lim)
		      (and (> (point) (or lim (point-min)))
			   (c-on-identifier)))
		    (and c-special-brace-lists
			 (c-looking-at-special-brace-list)))
		nil
	      (cons 'inexpr-statement (point))))

	res))))

(defun c-looking-at-inexpr-block-backward (paren-state)
  ;; Returns non-nil if we're looking at the end of an in-expression
  ;; block, otherwise the same as `c-looking-at-inexpr-block'.
  ;; PAREN-STATE is the paren state relevant at the current position.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    ;; We currently only recognize a block.
    (let ((here (point))
	  (elem (car-safe paren-state))
	  containing-sexp)
      (when (and (consp elem)
		 (progn (goto-char (cdr elem))
			(c-forward-syntactic-ws here)
			(= (point) here)))
	(goto-char (car elem))
	(if (setq paren-state (cdr paren-state))
	    (setq containing-sexp (car-safe paren-state)))
	(c-looking-at-inexpr-block (c-safe-position containing-sexp
						    paren-state)
				   containing-sexp)))))

(defun c-at-macro-vsemi-p (&optional pos)
  ;; Is there a "virtual semicolon" at POS or point?
  ;; (See cc-defs.el for full details of "virtual semicolons".)
  ;;
  ;; This is true when point is at the last non syntactic WS position on the
  ;; line, there is a macro call last on the line, and this particular macro's
  ;; name is defined by the regexp `c-vs-macro-regexp' as not needing a
  ;; semicolon.
  (save-excursion
    (save-restriction
      (widen)
      (if pos
	  (goto-char pos)
	(setq pos (point)))
      (and
       c-macro-with-semi-re
       (eq (skip-chars-backward " \t") 0)

       ;; Check we've got nothing after this except comments and empty lines
       ;; joined by escaped EOLs.
       (skip-chars-forward " \t")	; always returns non-nil.
       (progn
	 (while			      ; go over 1 block comment per iteration.
	     (and
	      (looking-at "\\(\\\\[\n\r][ \t]*\\)*")
	      (goto-char (match-end 0))
	      (cond
	       ((looking-at c-block-comment-start-regexp)
		(and (forward-comment 1)
		     (skip-chars-forward " \t"))) ; always returns non-nil
	       ((looking-at c-line-comment-start-regexp)
		(end-of-line)
		nil)
	       (t nil))))
	 (eolp))

       (goto-char pos)
       (progn (c-backward-syntactic-ws)
	      (eq (point) pos))

       ;; Check for one of the listed macros being before point.
       (or (not (eq (char-before) ?\)))
	   (when (c-go-list-backward)
	     (c-backward-syntactic-ws)
	     t))
       (c-simple-skip-symbol-backward)
       (looking-at c-macro-with-semi-re)
       (goto-char pos)
       (not (c-in-literal))))))		; The most expensive check last. 

(defun c-macro-vsemi-status-unknown-p () t) ; See cc-defs.el.


;; `c-guess-basic-syntax' and the functions that precedes it below
;; implements the main decision tree for determining the syntactic
;; analysis of the current line of code.

;; Dynamically bound to t when `c-guess-basic-syntax' is called during
;; auto newline analysis.
(defvar c-auto-newline-analysis nil)

(defun c-brace-anchor-point (bracepos)
  ;; BRACEPOS is the position of a brace in a construct like "namespace
  ;; Bar {".  Return the anchor point in this construct; this is the
  ;; earliest symbol on the brace's line which isn't earlier than
  ;; "namespace".
  ;;
  ;; Currently (2007-08-17), "like namespace" means "matches
  ;; c-other-block-decl-kwds".  It doesn't work with "class" or "struct"
  ;; or anything like that.
  (save-excursion
    (let ((boi (c-point 'boi bracepos)))
      (goto-char bracepos)
      (while (and (> (point) boi)
		  (not (looking-at c-other-decl-block-key)))
	(c-backward-token-2))
      (if (> (point) boi) (point) boi))))

(defsubst c-add-syntax (symbol &rest args)
  ;; A simple function to prepend a new syntax element to
  ;; `c-syntactic-context'.  Using `setq' on it is unsafe since it
  ;; should always be dynamically bound but since we read it first
  ;; we'll fail properly anyway if this function is misused.
  (setq c-syntactic-context (cons (cons symbol args)
				  c-syntactic-context)))

(defsubst c-append-syntax (symbol &rest args)
  ;; Like `c-add-syntax' but appends to the end of the syntax list.
  ;; (Normally not necessary.)
  (setq c-syntactic-context (nconc c-syntactic-context
				   (list (cons symbol args)))))

(defun c-add-stmt-syntax (syntax-symbol
			  syntax-extra-args
			  stop-at-boi-only
			  containing-sexp
			  paren-state)
  ;; Add the indicated SYNTAX-SYMBOL to `c-syntactic-context', extending it as
  ;; needed with further syntax elements of the types `substatement',
  ;; `inexpr-statement', `arglist-cont-nonempty', `statement-block-intro', and
  ;; `defun-block-intro'.
  ;;
  ;; Do the generic processing to anchor the given syntax symbol on
  ;; the preceding statement: Skip over any labels and containing
  ;; statements on the same line, and then search backward until we
  ;; find a statement or block start that begins at boi without a
  ;; label or comment.
  ;;
  ;; Point is assumed to be at the prospective anchor point for the
  ;; given SYNTAX-SYMBOL.  More syntax entries are added if we need to
  ;; skip past open parens and containing statements.  Most of the added
  ;; syntax elements will get the same anchor point - the exception is
  ;; for an anchor in a construct like "namespace"[*] - this is as early
  ;; as possible in the construct but on the same line as the {.
  ;;
  ;; [*] i.e. with a keyword matching c-other-block-decl-kwds.
  ;;
  ;; SYNTAX-EXTRA-ARGS are a list of the extra arguments for the
  ;; syntax symbol.  They are appended after the anchor point.
  ;;
  ;; If STOP-AT-BOI-ONLY is nil, we can stop in the middle of the line
  ;; if the current statement starts there.
  ;;
  ;; Note: It's not a problem if PAREN-STATE "overshoots"
  ;; CONTAINING-SEXP, i.e. contains info about parens further down.
  ;;
  ;; This function might do hidden buffer changes.

  (if (= (point) (c-point 'boi))
      ;; This is by far the most common case, so let's give it special
      ;; treatment.
      (apply 'c-add-syntax syntax-symbol (point) syntax-extra-args)

    (let ((syntax-last c-syntactic-context)
	  (boi (c-point 'boi))
	  ;; Set when we're on a label, so that we don't stop there.
	  ;; FIXME: To be complete we should check if we're on a label
	  ;; now at the start.
	  on-label)

      ;; Use point as the anchor point for "namespace", "extern", etc.
      (apply 'c-add-syntax syntax-symbol
	     (if (rassq syntax-symbol c-other-decl-block-key-in-symbols-alist)
		 (point) nil)
	     syntax-extra-args)

      ;; Loop while we have to back out of containing blocks.
      (while
	  (and
	   (catch 'back-up-block

	     ;; Loop while we have to back up statements.
	     (while (or (/= (point) boi)
			on-label
			(looking-at c-comment-start-regexp))

	       ;; Skip past any comments that stands between the
	       ;; statement start and boi.
	       (let ((savepos (point)))
		 (while (and (/= savepos boi)
			     (c-backward-single-comment))
		   (setq savepos (point)
			 boi (c-point 'boi)))
		 (goto-char savepos))

	       ;; Skip to the beginning of this statement or backward
	       ;; another one.
	       (let ((old-pos (point))
		     (old-boi boi)
		     (step-type (c-beginning-of-statement-1 containing-sexp)))
		 (setq boi (c-point 'boi)
		       on-label (eq step-type 'label))

		 (cond ((= (point) old-pos)
			;; If we didn't move we're at the start of a block and
			;; have to continue outside it.
			(throw 'back-up-block t))

		       ((and (eq step-type 'up)
			     (>= (point) old-boi)
			     (looking-at "else\\>[^_]")
			     (save-excursion
			       (goto-char old-pos)
			       (looking-at "if\\>[^_]")))
			;; Special case to avoid deeper and deeper indentation
			;; of "else if" clauses.
			)

		       ((and (not stop-at-boi-only)
			     (/= old-pos old-boi)
			     (memq step-type '(up previous)))
			;; If stop-at-boi-only is nil, we shouldn't back up
			;; over previous or containing statements to try to
			;; reach boi, so go back to the last position and
			;; exit.
			(goto-char old-pos)
			(throw 'back-up-block nil))

		       (t
			(if (and (not stop-at-boi-only)
				 (memq step-type '(up previous beginning)))
			    ;; If we've moved into another statement then we
			    ;; should no longer try to stop in the middle of a
			    ;; line.
			    (setq stop-at-boi-only t))

			;; Record this as a substatement if we skipped up one
			;; level.
			(when (eq step-type 'up)
			  (c-add-syntax 'substatement nil))))
		 )))

	   containing-sexp)

	;; Now we have to go out of this block.
	(goto-char containing-sexp)

	;; Don't stop in the middle of a special brace list opener
	;; like "({".
	(when c-special-brace-lists
	  (let ((special-list (c-looking-at-special-brace-list)))
	    (when (and special-list
		       (< (car (car special-list)) (point)))
	      (setq containing-sexp (car (car special-list)))
	      (goto-char containing-sexp))))

	(setq paren-state (c-whack-state-after containing-sexp paren-state)
	      containing-sexp (c-most-enclosing-brace paren-state)
	      boi (c-point 'boi))

	;; Analyze the construct in front of the block we've stepped out
	;; from and add the right syntactic element for it.
	(let ((paren-pos (point))
	      (paren-char (char-after))
	      step-type)

	  (if (eq paren-char ?\()
	      ;; Stepped out of a parenthesis block, so we're in an
	      ;; expression now.
	      (progn
		(when (/= paren-pos boi)
		  (if (and c-recognize-paren-inexpr-blocks
			   (progn
			     (c-backward-syntactic-ws containing-sexp)
			     (or (not (looking-at "\\>"))
				 (not (c-on-identifier))))
			   (save-excursion
			     (goto-char (1+ paren-pos))
			     (c-forward-syntactic-ws)
			     (eq (char-after) ?{)))
		      ;; Stepped out of an in-expression statement.  This
		      ;; syntactic element won't get an anchor pos.
		      (c-add-syntax 'inexpr-statement)

		    ;; A parenthesis normally belongs to an arglist.
		    (c-add-syntax 'arglist-cont-nonempty nil paren-pos)))

		(goto-char (max boi
				(if containing-sexp
				    (1+ containing-sexp)
				  (point-min))))
		(setq step-type 'same
		      on-label nil))

	    ;; Stepped out of a brace block.
	    (setq step-type (c-beginning-of-statement-1 containing-sexp)
		  on-label (eq step-type 'label))

	    (if (and (eq step-type 'same)
		     (/= paren-pos (point)))
		(let (inexpr)
		  (cond
		   ((save-excursion
		      (goto-char paren-pos)
		      (setq inexpr (c-looking-at-inexpr-block
				    (c-safe-position containing-sexp paren-state)
				    containing-sexp)))
		    (c-add-syntax (if (eq (car inexpr) 'inlambda)
				      'defun-block-intro
				    'statement-block-intro)
				  nil))
		   ((looking-at c-other-decl-block-key)
		    (c-add-syntax
		     (cdr (assoc (match-string 1)
				 c-other-decl-block-key-in-symbols-alist))
		     (max (c-point 'boi paren-pos) (point))))
		   (t (c-add-syntax 'defun-block-intro nil))))

		 (c-add-syntax 'statement-block-intro nil)))

	  (if (= paren-pos boi)
	      ;; Always done if the open brace was at boi.  The
	      ;; c-beginning-of-statement-1 call above is necessary
	      ;; anyway, to decide the type of block-intro to add.
	      (goto-char paren-pos)
	    (setq boi (c-point 'boi)))
	  ))

      ;; Fill in the current point as the anchor for all the symbols
      ;; added above.
      (let ((p c-syntactic-context) q)
	(while (not (eq p syntax-last))
	  (setq q (cdr (car p))) ; e.g. (nil 28) [from (arglist-cont-nonempty nil 28)]
	  (while q
	    (unless (car q)
	      (setcar q (point)))
	    (setq q (cdr q)))
	  (setq p (cdr p))))
      )))

(defun c-add-class-syntax (symbol
			   containing-decl-open
			   containing-decl-start
			   containing-decl-kwd
			   paren-state)
  ;; The inclass and class-close syntactic symbols are added in
  ;; several places and some work is needed to fix everything.
  ;; Therefore it's collected here.
  ;;
  ;; This function might do hidden buffer changes.
  (goto-char containing-decl-open)
  (if (and (eq symbol 'inclass) (= (point) (c-point 'boi)))
      (progn
	(c-add-syntax symbol containing-decl-open)
	containing-decl-open)
    (goto-char containing-decl-start)
    ;; Ought to use `c-add-stmt-syntax' instead of backing up to boi
    ;; here, but we have to do like this for compatibility.
    (back-to-indentation)
    (c-add-syntax symbol (point))
    (if (and (c-keyword-member containing-decl-kwd
			       'c-inexpr-class-kwds)
	     (/= containing-decl-start (c-point 'boi containing-decl-start)))
	(c-add-syntax 'inexpr-class))
    (point)))

(defun c-guess-continued-construct (indent-point
				    char-after-ip
				    beg-of-same-or-containing-stmt
				    containing-sexp
				    paren-state)
  ;; This function contains the decision tree reached through both
  ;; cases 18 and 10.  It's a continued statement or top level
  ;; construct of some kind.
  ;;
  ;; This function might do hidden buffer changes.

  (let (special-brace-list placeholder)
    (goto-char indent-point)
    (skip-chars-forward " \t")

    (cond
     ;; (CASE A removed.)
     ;; CASE B: open braces for class or brace-lists
     ((setq special-brace-list
	    (or (and c-special-brace-lists
		     (c-looking-at-special-brace-list))
		(eq char-after-ip ?{)))

      (cond
       ;; CASE B.1: class-open
       ((save-excursion
	  (and (eq (char-after) ?{)
	       (c-looking-at-decl-block containing-sexp t)
	       (setq beg-of-same-or-containing-stmt (point))))
	(c-add-syntax 'class-open beg-of-same-or-containing-stmt))

       ;; CASE B.2: brace-list-open
       ((or (consp special-brace-list)
	    (save-excursion
	      (goto-char beg-of-same-or-containing-stmt)
	      (c-syntactic-re-search-forward "=\\([^=]\\|$\\)"
					     indent-point t t t)))
	;; The most semantically accurate symbol here is
	;; brace-list-open, but we normally report it simply as a
	;; statement-cont.  The reason is that one normally adjusts
	;; brace-list-open for brace lists as top-level constructs,
	;; and brace lists inside statements is a completely different
	;; context.  C.f. case 5A.3.
	(c-beginning-of-statement-1 containing-sexp)
	(c-add-stmt-syntax (if c-auto-newline-analysis
			       ;; Turn off the dwim above when we're
			       ;; analyzing the nature of the brace
			       ;; for the auto newline feature.
			       'brace-list-open
			     'statement-cont)
			   nil nil
			   containing-sexp paren-state))

       ;; CASE B.3: The body of a function declared inside a normal
       ;; block.  Can occur e.g. in Pike and when using gcc
       ;; extensions, but watch out for macros followed by blocks.
       ;; C.f. cases E, 16F and 17G.
       ((and (not (c-at-statement-start-p))
	     (eq (c-beginning-of-statement-1 containing-sexp nil nil t)
		 'same)
	     (save-excursion
	       (let ((c-recognize-typeless-decls nil))
		 ;; Turn off recognition of constructs that lacks a
		 ;; type in this case, since that's more likely to be
		 ;; a macro followed by a block.
		 (c-forward-decl-or-cast-1 (c-point 'bosws) nil nil))))
	(c-add-stmt-syntax 'defun-open nil t
			   containing-sexp paren-state))

       ;; CASE B.4: Continued statement with block open.  The most
       ;; accurate analysis is perhaps `statement-cont' together with
       ;; `block-open' but we play DWIM and use `substatement-open'
       ;; instead.  The rationale is that this typically is a macro
       ;; followed by a block which makes it very similar to a
       ;; statement with a substatement block.
       (t
	(c-add-stmt-syntax 'substatement-open nil nil
			   containing-sexp paren-state))
       ))

     ;; CASE C: iostream insertion or extraction operator
     ((and (looking-at "\\(<<\\|>>\\)\\([^=]\\|$\\)")
	   (save-excursion
	     (goto-char beg-of-same-or-containing-stmt)
	     ;; If there is no preceding streamop in the statement
	     ;; then indent this line as a normal statement-cont.
	     (when (c-syntactic-re-search-forward
		    "\\(<<\\|>>\\)\\([^=]\\|$\\)" indent-point 'move t t)
	       (c-add-syntax 'stream-op (c-point 'boi))
	       t))))

     ;; CASE E: In the "K&R region" of a function declared inside a
     ;; normal block.  C.f. case B.3.
     ((and (save-excursion
	     ;; Check that the next token is a '{'.  This works as
	     ;; long as no language that allows nested function
	     ;; definitions allows stuff like member init lists, K&R
	     ;; declarations or throws clauses there.
	     ;;
	     ;; Note that we do a forward search for something ahead
	     ;; of the indentation line here.  That's not good since
	     ;; the user might not have typed it yet.  Unfortunately
	     ;; it's exceedingly tricky to recognize a function
	     ;; prototype in a code block without resorting to this.
	     (c-forward-syntactic-ws)
	     (eq (char-after) ?{))
	   (not (c-at-statement-start-p))
	   (eq (c-beginning-of-statement-1 containing-sexp nil nil t)
	       'same)
	   (save-excursion
	     (let ((c-recognize-typeless-decls nil))
	       ;; Turn off recognition of constructs that lacks a
	       ;; type in this case, since that's more likely to be
	       ;; a macro followed by a block.
	       (c-forward-decl-or-cast-1 (c-point 'bosws) nil nil))))
      (c-add-stmt-syntax 'func-decl-cont nil t
			 containing-sexp paren-state))

     ;;CASE F: continued statement and the only preceding items are
     ;;annotations.
     ((and (c-major-mode-is 'java-mode)
	   (setq placeholder (point))
            (c-beginning-of-statement-1)
            (progn
              (while (and (c-forward-annotation)
                          (< (point) placeholder))
                (c-forward-syntactic-ws))
              t)
            (prog1
                (>= (point) placeholder)
              (goto-char placeholder)))
       (c-beginning-of-statement-1 containing-sexp)
       (c-add-syntax 'annotation-var-cont (point)))

     ;; CASE G: a template list continuation?
     ;; Mostly a duplication of case 5D.3 to fix templates-19:
     ((and (c-major-mode-is 'c++-mode)
	   (save-excursion
	     (goto-char indent-point)
	     (c-with-syntax-table c++-template-syntax-table
	       (setq placeholder (c-up-list-backward)))
	     (and placeholder
		  (eq (char-after placeholder) ?<)
		  (/= (char-before placeholder) ?<)
		  (progn
		    (goto-char (1+ placeholder))
		    (not (looking-at c-<-op-cont-regexp))))))
      (c-with-syntax-table c++-template-syntax-table
	(goto-char placeholder)
	(c-beginning-of-statement-1 containing-sexp t)
	(if (save-excursion
	      (c-backward-syntactic-ws containing-sexp)
	      (eq (char-before) ?<))
	    ;; In a nested template arglist.
	    (progn
	      (goto-char placeholder)
	      (c-syntactic-skip-backward "^,;" containing-sexp t)
	      (c-forward-syntactic-ws))
	  (back-to-indentation)))
      ;; FIXME: Should use c-add-stmt-syntax, but it's not yet
      ;; template aware.
      (c-add-syntax 'template-args-cont (point) placeholder))

     ;; CASE D: continued statement.
     (t
      (c-beginning-of-statement-1 containing-sexp)
      (c-add-stmt-syntax 'statement-cont nil nil
			 containing-sexp paren-state))
     )))

;; The next autoload was added by RMS on 2005/8/9 - don't know why (ACM,
;; 2005/11/29).
;;;###autoload
(defun c-guess-basic-syntax ()
  "Return the syntactic context of the current line."
  (save-excursion
    (beginning-of-line)
    (c-save-buffer-state
	((indent-point (point))
	 (case-fold-search nil)
	 ;; A whole ugly bunch of various temporary variables.  Have
	 ;; to declare them here since it's not possible to declare
	 ;; a variable with only the scope of a cond test and the
	 ;; following result clauses, and most of this function is a
	 ;; single gigantic cond. :P
	 literal char-before-ip before-ws-ip char-after-ip macro-start
	 in-macro-expr c-syntactic-context placeholder c-in-literal-cache
	 step-type tmpsymbol keyword injava-inher special-brace-list tmp-pos
	 containing-<
	 ;; The following record some positions for the containing
	 ;; declaration block if we're directly within one:
	 ;; `containing-decl-open' is the position of the open
	 ;; brace.  `containing-decl-start' is the start of the
	 ;; declaration.  `containing-decl-kwd' is the keyword
	 ;; symbol of the keyword that tells what kind of block it
	 ;; is.
	 containing-decl-open
	 containing-decl-start
	 containing-decl-kwd
	 ;; The open paren of the closest surrounding sexp or nil if
	 ;; there is none.
	 containing-sexp
	 ;; The position after the closest preceding brace sexp
	 ;; (nested sexps are ignored), or the position after
	 ;; `containing-sexp' if there is none, or (point-min) if
	 ;; `containing-sexp' is nil.
	 lim
	 ;; The paren state outside `containing-sexp', or at
	 ;; `indent-point' if `containing-sexp' is nil.
	 (paren-state (c-parse-state))
	 ;; There's always at most one syntactic element which got
	 ;; an anchor pos.  It's stored in syntactic-relpos.
	 syntactic-relpos
	 (c-stmt-delim-chars c-stmt-delim-chars))

      ;; Check if we're directly inside an enclosing declaration
      ;; level block.
      (when (and (setq containing-sexp
		       (c-most-enclosing-brace paren-state))
		 (progn
		   (goto-char containing-sexp)
		   (eq (char-after) ?{))
		 (setq placeholder
		       (c-looking-at-decl-block
			(c-most-enclosing-brace paren-state
						containing-sexp)
			t)))
	(setq containing-decl-open containing-sexp
	      containing-decl-start (point)
	      containing-sexp nil)
	(goto-char placeholder)
	(setq containing-decl-kwd (and (looking-at c-keywords-regexp)
				       (c-keyword-sym (match-string 1)))))

      ;; Init some position variables.
      (if c-state-cache
	  (progn
	    (setq containing-sexp (car paren-state)
		  paren-state (cdr paren-state))
	    (if (consp containing-sexp)
		(progn
		  (setq lim (cdr containing-sexp))
		  (if (cdr c-state-cache)
		      ;; Ignore balanced paren.  The next entry
		      ;; can't be another one.
		      (setq containing-sexp (car (cdr c-state-cache))
			    paren-state (cdr paren-state))
		    ;; If there is no surrounding open paren then
		    ;; put the last balanced pair back on paren-state.
		    (setq paren-state (cons containing-sexp paren-state)
			  containing-sexp nil)))
	      (setq lim (1+ containing-sexp))))
	(setq lim (point-min)))
      (when (c-beginning-of-macro)
	(goto-char indent-point)
	(let ((lim1 (c-determine-limit 2000)))
	  (setq lim (max lim lim1))))

      ;; If we're in a parenthesis list then ',' delimits the
      ;; "statements" rather than being an operator (with the
      ;; exception of the "for" clause).  This difference is
      ;; typically only noticeable when statements are used in macro
      ;; arglists.
      (when (and containing-sexp
		 (eq (char-after containing-sexp) ?\())
	(setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))
      ;; cache char before and after indent point, and move point to
      ;; the most likely position to perform the majority of tests
      (goto-char indent-point)
      (c-backward-syntactic-ws lim)
      (setq before-ws-ip (point)
	    char-before-ip (char-before))
      (goto-char indent-point)
      (skip-chars-forward " \t")
      (setq char-after-ip (char-after))

      ;; are we in a literal?
      (setq literal (c-in-literal lim))

      ;; now figure out syntactic qualities of the current line
      (cond

       ;; CASE 1: in a string.
       ((eq literal 'string)
	(c-add-syntax 'string (c-point 'bopl)))

       ;; CASE 2: in a C or C++ style comment.
       ((and (memq literal '(c c++))
	     ;; This is a kludge for XEmacs where we use
	     ;; `buffer-syntactic-context', which doesn't correctly
	     ;; recognize "\*/" to end a block comment.
	     ;; `parse-partial-sexp' which is used by
	     ;; `c-literal-limits' will however do that in most
	     ;; versions, which results in that we get nil from
	     ;; `c-literal-limits' even when `c-in-literal' claims
	     ;; we're inside a comment.
	     (setq placeholder (c-literal-limits lim)))
	(c-add-syntax literal (car placeholder)))

       ;; CASE 3: in a cpp preprocessor macro continuation.
       ((and (save-excursion
	       (when (c-beginning-of-macro)
		 (setq macro-start (point))))
	     (/= macro-start (c-point 'boi))
	     (progn
	       (setq tmpsymbol 'cpp-macro-cont)
	       (or (not c-syntactic-indentation-in-macros)
		   (save-excursion
		     (goto-char macro-start)
		     ;; If at the beginning of the body of a #define
		     ;; directive then analyze as cpp-define-intro
		     ;; only.  Go on with the syntactic analysis
		     ;; otherwise.  in-macro-expr is set if we're in a
		     ;; cpp expression, i.e. before the #define body
		     ;; or anywhere in a non-#define directive.
		     (if (c-forward-to-cpp-define-body)
			 (let ((indent-boi (c-point 'boi indent-point)))
			   (setq in-macro-expr (> (point) indent-boi)
				 tmpsymbol 'cpp-define-intro)
			   (= (point) indent-boi))
		       (setq in-macro-expr t)
		       nil)))))
	(c-add-syntax tmpsymbol macro-start)
	(setq macro-start nil))

       ;; CASE 11: an else clause?
       ((looking-at "else\\>[^_]")
	(c-beginning-of-statement-1 containing-sexp)
	(c-add-stmt-syntax 'else-clause nil t
			   containing-sexp paren-state))

       ;; CASE 12: while closure of a do/while construct?
       ((and (looking-at "while\\>[^_]")
	     (save-excursion
	       (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
			  'beginning)
		 (setq placeholder (point)))))
	(goto-char placeholder)
	(c-add-stmt-syntax 'do-while-closure nil t
			   containing-sexp paren-state))

       ;; CASE 13: A catch or finally clause?  This case is simpler
       ;; than if-else and do-while, because a block is required
       ;; after every try, catch and finally.
       ((save-excursion
	  (and (cond ((c-major-mode-is 'c++-mode)
		      (looking-at "catch\\>[^_]"))
		     ((c-major-mode-is 'java-mode)
		      (looking-at "\\(catch\\|finally\\)\\>[^_]")))
	       (and (c-safe (c-backward-syntactic-ws)
			    (c-backward-sexp)
			    t)
		    (eq (char-after) ?{)
		    (c-safe (c-backward-syntactic-ws)
			    (c-backward-sexp)
			    t)
		    (if (eq (char-after) ?\()
			(c-safe (c-backward-sexp) t)
		      t))
	       (looking-at "\\(try\\|catch\\)\\>[^_]")
	       (setq placeholder (point))))
	(goto-char placeholder)
	(c-add-stmt-syntax 'catch-clause nil t
			   containing-sexp paren-state))

       ;; CASE 18: A substatement we can recognize by keyword.
       ((save-excursion
	  (and c-opt-block-stmt-key
	       (not (eq char-before-ip ?\;))
	       (not (c-at-vsemi-p before-ws-ip))
	       (not (memq char-after-ip '(?\) ?\] ?,)))
	       (or (not (eq char-before-ip ?}))
		   (c-looking-at-inexpr-block-backward c-state-cache))
	       (> (point)
		  (progn
		    ;; Ought to cache the result from the
		    ;; c-beginning-of-statement-1 calls here.
		    (setq placeholder (point))
		    (while (eq (setq step-type
				     (c-beginning-of-statement-1 lim))
			       'label))
		    (if (eq step-type 'previous)
			(goto-char placeholder)
		      (setq placeholder (point))
		      (if (and (eq step-type 'same)
			       (not (looking-at c-opt-block-stmt-key)))
			  ;; Step up to the containing statement if we
			  ;; stayed in the same one.
			  (let (step)
			    (while (eq
				    (setq step
					  (c-beginning-of-statement-1 lim))
				    'label))
			    (if (eq step 'up)
				(setq placeholder (point))
			      ;; There was no containing statement after all.
			      (goto-char placeholder)))))
		    placeholder))
	       (if (looking-at c-block-stmt-2-key)
		   ;; Require a parenthesis after these keywords.
		   ;; Necessary to catch e.g. synchronized in Java,
		   ;; which can be used both as statement and
		   ;; modifier.
		   (and (zerop (c-forward-token-2 1 nil))
			(eq (char-after) ?\())
		 (looking-at c-opt-block-stmt-key))))

	(if (eq step-type 'up)
	    ;; CASE 18A: Simple substatement.
	    (progn
	      (goto-char placeholder)
	      (cond
	       ((eq char-after-ip ?{)
		(c-add-stmt-syntax 'substatement-open nil nil
				   containing-sexp paren-state))
	       ((save-excursion
		  (goto-char indent-point)
		  (back-to-indentation)
		  (c-forward-label))
		(c-add-stmt-syntax 'substatement-label nil nil
				   containing-sexp paren-state))
	       (t
		(c-add-stmt-syntax 'substatement nil nil
				   containing-sexp paren-state))))

	  ;; CASE 18B: Some other substatement.  This is shared
	  ;; with case 10.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       lim
				       paren-state)))

       ;; CASE 14: A case or default label
       ((looking-at c-label-kwds-regexp)
	(if containing-sexp
	    (progn
	      (goto-char containing-sexp)
	      (setq lim (c-most-enclosing-brace c-state-cache
						containing-sexp))
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'case-label nil t lim paren-state))
	  ;; Got a bogus label at the top level.  In lack of better
	  ;; alternatives, anchor it on (point-min).
	  (c-add-syntax 'case-label (point-min))))

       ;; CASE 15: any other label
       ((save-excursion
	  (back-to-indentation)
	  (and (not (looking-at c-syntactic-ws-start))
	       (c-forward-label)))
	(cond (containing-decl-open
	       (setq placeholder (c-add-class-syntax 'inclass
						     containing-decl-open
						     containing-decl-start
						     containing-decl-kwd
						     paren-state))
	       ;; Append access-label with the same anchor point as
	       ;; inclass gets.
	       (c-append-syntax 'access-label placeholder))

	      (containing-sexp
	       (goto-char containing-sexp)
	       (setq lim (c-most-enclosing-brace c-state-cache
						 containing-sexp))
	       (save-excursion
		 (setq tmpsymbol
		       (if (and (eq (c-beginning-of-statement-1 lim) 'up)
				(looking-at "switch\\>[^_]"))
			   ;; If the surrounding statement is a switch then
			   ;; let's analyze all labels as switch labels, so
			   ;; that they get lined up consistently.
			   'case-label
			 'label)))
	       (c-backward-to-block-anchor lim)
	       (c-add-stmt-syntax tmpsymbol nil t lim paren-state))

	      (t
	       ;; A label on the top level.  Treat it as a class
	       ;; context.  (point-min) is the closest we get to the
	       ;; class open brace.
	       (c-add-syntax 'access-label (point-min)))))

       ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
       ;; 17E.
       ((setq placeholder (c-looking-at-inexpr-block
			   (c-safe-position containing-sexp paren-state)
			   containing-sexp
			   ;; Have to turn on the heuristics after
			   ;; the point even though it doesn't work
			   ;; very well.  C.f. test case class-16.pike.
			   t))
	(setq tmpsymbol (assq (car placeholder)
			      '((inexpr-class . class-open)
				(inexpr-statement . block-open))))
	(if tmpsymbol
	    ;; It's a statement block or an anonymous class.
	    (setq tmpsymbol (cdr tmpsymbol))
	  ;; It's a Pike lambda.  Check whether we are between the
	  ;; lambda keyword and the argument list or at the defun
	  ;; opener.
	  (setq tmpsymbol (if (eq char-after-ip ?{)
			      'inline-open
			    'lambda-intro-cont)))
	(goto-char (cdr placeholder))
	(back-to-indentation)
	(c-add-stmt-syntax tmpsymbol nil t
			   (c-most-enclosing-brace c-state-cache (point))
			   paren-state)
	(unless (eq (point) (cdr placeholder))
	  (c-add-syntax (car placeholder))))

       ;; CASE 5: Line is inside a declaration level block or at top level.
       ((or containing-decl-open (null containing-sexp))
	(cond

	 ;; CASE 5A: we are looking at a defun, brace list, class,
	 ;; or inline-inclass method opening brace
	 ((setq special-brace-list
		(or (and c-special-brace-lists
			 (c-looking-at-special-brace-list))
		    (eq char-after-ip ?{)))
	  (cond

	   ;; CASE 5A.1: Non-class declaration block open.
	   ((save-excursion
	      (let (tmp)
		(and (eq char-after-ip ?{)
		     (setq tmp (c-looking-at-decl-block containing-sexp t))
		     (progn
		       (setq placeholder (point))
		       (goto-char tmp)
		       (looking-at c-symbol-key))
		     (c-keyword-member
		      (c-keyword-sym (setq keyword (match-string 0)))
		      'c-other-block-decl-kwds))))
	    (goto-char placeholder)
	    (c-add-stmt-syntax
	     (if (string-equal keyword "extern")
		 ;; Special case for extern-lang-open.
		 'extern-lang-open
	       (intern (concat keyword "-open")))
	     nil t containing-sexp paren-state))

	   ;; CASE 5A.2: we are looking at a class opening brace
	   ((save-excursion
	      (goto-char indent-point)
	      (skip-chars-forward " \t")
	      (and (eq (char-after) ?{)
		   (c-looking-at-decl-block containing-sexp t)
		   (setq placeholder (point))))
	    (c-add-syntax 'class-open placeholder))

	   ;; CASE 5A.3: brace list open
	   ((save-excursion
	      (c-beginning-of-decl-1 lim)
	      (while (looking-at c-specifier-key)
		(goto-char (match-end 1))
		(c-forward-syntactic-ws indent-point))
	      (setq placeholder (c-point 'boi))
	      (or (consp special-brace-list)
		  (and (or (save-excursion
			     (goto-char indent-point)
			     (setq tmpsymbol nil)
			     (while (and (> (point) placeholder)
					 (zerop (c-backward-token-2 1 t))
					 (/= (char-after) ?=))
			       (and c-opt-inexpr-brace-list-key
				    (not tmpsymbol)
				    (looking-at c-opt-inexpr-brace-list-key)
				    (setq tmpsymbol 'topmost-intro-cont)))
			     (eq (char-after) ?=))
			   (looking-at c-brace-list-key))
		       (save-excursion
			 (while (and (< (point) indent-point)
				     (zerop (c-forward-token-2 1 t))
				     (not (memq (char-after) '(?\; ?\()))))
			 (not (memq (char-after) '(?\; ?\()))
			 ))))
	    (if (and (not c-auto-newline-analysis)
		     (c-major-mode-is 'java-mode)
		     (eq tmpsymbol 'topmost-intro-cont))
		;; We're in Java and have found that the open brace
		;; belongs to a "new Foo[]" initialization list,
		;; which means the brace list is part of an
		;; expression and not a top level definition.  We
		;; therefore treat it as any topmost continuation
		;; even though the semantically correct symbol still
		;; is brace-list-open, on the same grounds as in
		;; case B.2.
		(progn
		  (c-beginning-of-statement-1 lim)
		  (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	      (c-add-syntax 'brace-list-open placeholder)))

	   ;; CASE 5A.4: inline defun open
	   ((and containing-decl-open
		 (not (c-keyword-member containing-decl-kwd
					'c-other-block-decl-kwds)))
	    (c-add-syntax 'inline-open)
	    (c-add-class-syntax 'inclass
				containing-decl-open
				containing-decl-start
				containing-decl-kwd
				paren-state))

	   ;; CASE 5A.5: ordinary defun open
	   (t
	    (save-excursion
	      (c-beginning-of-decl-1 lim)
	      (while (looking-at c-specifier-key)
		(goto-char (match-end 1))
		(c-forward-syntactic-ws indent-point))
	      (c-add-syntax 'defun-open (c-point 'boi))
	      ;; Bogus to use bol here, but it's the legacy.  (Resolved,
	      ;; 2007-11-09)
	      ))))

	 ;; CASE 5B: After a function header but before the body (or
	 ;; the ending semicolon if there's no body).
	 ((save-excursion
	    (when (setq placeholder (c-just-after-func-arglist-p
				     (max lim (c-determine-limit 500))))
	      (setq tmp-pos (point))))
	  (cond

	   ;; CASE 5B.1: Member init list.
	   ((eq (char-after tmp-pos) ?:)
	    (if (or (>= tmp-pos indent-point)
		    (= (c-point 'bosws) (1+ tmp-pos)))
		(progn
		  ;; There is no preceding member init clause.
		  ;; Indent relative to the beginning of indentation
		  ;; for the topmost-intro line that contains the
		  ;; prototype's open paren.
		  (goto-char placeholder)
		  (c-add-syntax 'member-init-intro (c-point 'boi)))
	      ;; Indent relative to the first member init clause.
	      (goto-char (1+ tmp-pos))
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))))

	   ;; CASE 5B.2: K&R arg decl intro
	   ((and c-recognize-knr-p
		 (c-in-knr-argdecl lim))
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	    (if containing-decl-open
		(c-add-class-syntax 'inclass
				    containing-decl-open
				    containing-decl-start
				    containing-decl-kwd
				    paren-state)))

	   ;; CASE 5B.4: Nether region after a C++ or Java func
	   ;; decl, which could include a `throws' declaration.
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'func-decl-cont (c-point 'boi))
	    )))

	 ;; CASE 5C: inheritance line. could be first inheritance
	 ;; line, or continuation of a multiple inheritance
	 ((or (and (c-major-mode-is 'c++-mode)
		   (progn
		     (when (eq char-after-ip ?,)
		       (skip-chars-forward " \t")
		       (forward-char))
		     (looking-at c-opt-postfix-decl-spec-key)))
	      (and (or (eq char-before-ip ?:)
		       ;; watch out for scope operator
		       (save-excursion
			 (and (eq char-after-ip ?:)
			      (c-safe (forward-char 1) t)
			      (not (eq (char-after) ?:))
			      )))
		   (save-excursion
		     (c-backward-syntactic-ws lim)
		     (if (eq char-before-ip ?:)
			 (progn
			   (forward-char -1)
			   (c-backward-syntactic-ws lim)))
		     (back-to-indentation)
		     (looking-at c-class-key)))
	      ;; for Java
	      (and (c-major-mode-is 'java-mode)
		   (let ((fence (save-excursion
				  (c-beginning-of-statement-1 lim)
				  (point)))
			 cont done)
		     (save-excursion
		       (while (not done)
			 (cond ((looking-at c-opt-postfix-decl-spec-key)
				(setq injava-inher (cons cont (point))
				      done t))
			       ((or (not (c-safe (c-forward-sexp -1) t))
				    (<= (point) fence))
				(setq done t))
			       )
			 (setq cont t)))
		     injava-inher)
		   (not (c-crosses-statement-barrier-p (cdr injava-inher)
						       (point)))
		   ))
	  (cond

	   ;; CASE 5C.1: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'inher-intro (c-point 'boi))
	    ;; don't add inclass symbol since relative point already
	    ;; contains any class offset
	    )

	   ;; CASE 5C.2: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'inher-intro (c-point 'boi))
	    (if containing-decl-open
		(c-add-class-syntax 'inclass
				    containing-decl-open
				    containing-decl-start
				    containing-decl-kwd
				    paren-state)))

	   ;; CASE 5C.3: in a Java implements/extends
	   (injava-inher
	    (let ((where (cdr injava-inher))
		  (cont (car injava-inher)))
	      (goto-char where)
	      (cond ((looking-at "throws\\>[^_]")
		     (c-add-syntax 'func-decl-cont
				   (progn (c-beginning-of-statement-1 lim)
					  (c-point 'boi))))
		    (cont (c-add-syntax 'inher-cont where))
		    (t (c-add-syntax 'inher-intro
				     (progn (goto-char (cdr injava-inher))
					    (c-beginning-of-statement-1 lim)
					    (point))))
		    )))

	   ;; CASE 5C.4: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    ;; don't add inclass symbol since relative point already
	    ;; contains any class offset
	    )))

	 ;; CASE 5D: this could be a top-level initialization, a
	 ;; member init list continuation, or a template argument
	 ;; list continuation.
	 ((save-excursion
	    ;; Note: We use the fact that lim is always after any
	    ;; preceding brace sexp.
	    (if c-recognize-<>-arglists
		(while (and
			(progn
			  (c-syntactic-skip-backward "^;,=<>" lim t)
			  (> (point) lim))
			(or
			 (when c-overloadable-operators-regexp
			   (when (setq placeholder (c-after-special-operator-id lim))
			     (goto-char placeholder)
			     t))
			 (cond
			  ((eq (char-before) ?>)
			   (or (c-backward-<>-arglist nil lim)
			       (backward-char))
			   t)
			  ((eq (char-before) ?<)
			   (backward-char)
			   (if (save-excursion
				 (c-forward-<>-arglist nil))
			       (progn (forward-char)
				      nil)
			     t))
			  (t nil)))))
	      ;; NB: No c-after-special-operator-id stuff in this
	      ;; clause - we assume only C++ needs it.
	      (c-syntactic-skip-backward "^;,=" lim t))
	    (memq (char-before) '(?, ?= ?<)))
	  (cond

	   ;; CASE 5D.3: perhaps a template list continuation?
	   ((and (c-major-mode-is 'c++-mode)
		 (save-excursion
		   (save-restriction
		     (c-with-syntax-table c++-template-syntax-table
		       (goto-char indent-point)
		       (setq placeholder (c-up-list-backward))
		       (and placeholder
			    (eq (char-after placeholder) ?<))))))
	    (c-with-syntax-table c++-template-syntax-table
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim t)
	      (if (save-excursion
		    (c-backward-syntactic-ws lim)
		    (eq (char-before) ?<))
		  ;; In a nested template arglist.
		  (progn
		    (goto-char placeholder)
		    (c-syntactic-skip-backward "^,;" lim t)
		    (c-forward-syntactic-ws))
		(back-to-indentation)))
	    ;; FIXME: Should use c-add-stmt-syntax, but it's not yet
	    ;; template aware.
	    (c-add-syntax 'template-args-cont (point) placeholder))

	   ;; CASE 5D.4: perhaps a multiple inheritance line?
	   ((and (c-major-mode-is 'c++-mode)
		 (save-excursion
		   (c-beginning-of-statement-1 lim)
		   (setq placeholder (point))
		   (if (looking-at "static\\>[^_]")
		       (c-forward-token-2 1 nil indent-point))
		   (and (looking-at c-class-key)
			(zerop (c-forward-token-2 2 nil indent-point))
			(if (eq (char-after) ?<)
			    (c-with-syntax-table c++-template-syntax-table
			      (zerop (c-forward-token-2 1 t indent-point)))
			  t)
			(eq (char-after) ?:))))
	    (goto-char placeholder)
	    (c-add-syntax 'inher-cont (c-point 'boi)))

	   ;; CASE 5D.5: Continuation of the "expression part" of a
	   ;; top level construct.  Or, perhaps, an unrecognized construct.
	   (t
	    (while (and (setq placeholder (point))
			(eq (car (c-beginning-of-decl-1 containing-sexp)) ; Can't use `lim' here.
			    'same)
			(save-excursion
			  (c-backward-syntactic-ws)
			  (eq (char-before) ?}))
			(< (point) placeholder)))
	    (c-add-stmt-syntax
	     (cond
	      ((eq (point) placeholder) 'statement) ; unrecognized construct
	      ;; A preceding comma at the top level means that a
	      ;; new variable declaration starts here.  Use
	      ;; topmost-intro-cont for it, for consistency with
	      ;; the first variable declaration.  C.f. case 5N.
	      ((eq char-before-ip ?,) 'topmost-intro-cont)
	      (t 'statement-cont))
	     nil nil containing-sexp paren-state))
	   ))

	 ;; CASE 5F: Close of a non-class declaration level block.
	 ((and (eq char-after-ip ?})
	       (c-keyword-member containing-decl-kwd
				 'c-other-block-decl-kwds))
	  ;; This is inconsistent: Should use `containing-decl-open'
	  ;; here if it's at boi, like in case 5J.
	  (goto-char containing-decl-start)
	  (c-add-stmt-syntax
	   (if (string-equal (symbol-name containing-decl-kwd) "extern")
	       ;; Special case for compatibility with the
	       ;; extern-lang syntactic symbols.
	       'extern-lang-close
	     (intern (concat (symbol-name containing-decl-kwd)
			     "-close")))
	   nil t
	   (c-most-enclosing-brace paren-state (point))
	   paren-state))

	 ;; CASE 5G: we are looking at the brace which closes the
	 ;; enclosing nested class decl
	 ((and containing-sexp
	       (eq char-after-ip ?})
	       (eq containing-decl-open containing-sexp))
	  (c-add-class-syntax 'class-close
			      containing-decl-open
			      containing-decl-start
			      containing-decl-kwd
			      paren-state))

	 ;; CASE 5H: we could be looking at subsequent knr-argdecls
	 ((and c-recognize-knr-p
	       (not containing-sexp)	; can't be knr inside braces.
	       (not (eq char-before-ip ?}))
	       (save-excursion
		 (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		 (and placeholder
		      ;; Do an extra check to avoid tripping up on
		      ;; statements that occur in invalid contexts
		      ;; (e.g. in macro bodies where we don't really
		      ;; know the context of what we're looking at).
		      (not (and c-opt-block-stmt-key
				(looking-at c-opt-block-stmt-key)))))
	       (< placeholder indent-point))
	  (goto-char placeholder)
	  (c-add-syntax 'knr-argdecl (point)))

	 ;; CASE 5I: ObjC method definition.
	 ((and c-opt-method-key
	       (looking-at c-opt-method-key))
	  (c-beginning-of-statement-1 nil t)
	  (if (= (point) indent-point)
	      ;; Handle the case when it's the first (non-comment)
	      ;; thing in the buffer.  Can't look for a 'same return
	      ;; value from cbos1 since ObjC directives currently
	      ;; aren't recognized fully, so that we get 'same
	      ;; instead of 'previous if it moved over a preceding
	      ;; directive.
	      (goto-char (point-min)))
	  (c-add-syntax 'objc-method-intro (c-point 'boi)))

	 ;; CASE 5P: AWK pattern or function or continuation
	 ;; thereof.
	 ((c-major-mode-is 'awk-mode)
	  (setq placeholder (point))
	  (c-add-stmt-syntax
	   (if (and (eq (c-beginning-of-statement-1) 'same)
		    (/= (point) placeholder))
	       'topmost-intro-cont
	     'topmost-intro)
	   nil nil
	   containing-sexp paren-state))

	 ;; CASE 5N: At a variable declaration that follows a class
	 ;; definition or some other block declaration that doesn't
	 ;; end at the closing '}'.  C.f. case 5D.5.
	 ((progn
	    (c-backward-syntactic-ws lim)
	    (and (eq (char-before) ?})
		 (save-excursion
		   (let ((start (point)))
		     (if (and c-state-cache
			      (consp (car c-state-cache))
			      (eq (cdar c-state-cache) (point)))
			 ;; Speed up the backward search a bit.
			 (goto-char (caar c-state-cache)))
		     (c-beginning-of-decl-1 containing-sexp) ; Can't use `lim' here.
		     (setq placeholder (point))
		     (if (= start (point))
			 ;; The '}' is unbalanced.
			 nil
		       (c-end-of-decl-1)
		       (>= (point) indent-point))))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'topmost-intro-cont nil nil
			     containing-sexp paren-state))

	 ;; NOTE: The point is at the end of the previous token here.

	 ;; CASE 5J: we are at the topmost level, make
	 ;; sure we skip back past any access specifiers
	 ((and
	   ;; A macro continuation line is never at top level.
	   (not (and macro-start
		     (> indent-point macro-start)))
	   (save-excursion
	     (setq placeholder (point))
	     (or (memq char-before-ip '(?\; ?{ ?} nil))
		 (c-at-vsemi-p before-ws-ip)
		 (when (and (eq char-before-ip ?:)
			    (eq (c-beginning-of-statement-1 lim)
				'label))
		   (c-backward-syntactic-ws lim)
		   (setq placeholder (point)))
		 (and (c-major-mode-is 'objc-mode)
		      (catch 'not-in-directive
			(c-beginning-of-statement-1 lim)
			(setq placeholder (point))
			(while (and (c-forward-objc-directive)
				    (< (point) indent-point))
			  (c-forward-syntactic-ws)
			  (if (>= (point) indent-point)
			      (throw 'not-in-directive t))
			  (setq placeholder (point)))
			nil)))))
	  ;; For historic reasons we anchor at bol of the last
	  ;; line of the previous declaration.  That's clearly
	  ;; highly bogus and useless, and it makes our lives hard
	  ;; to remain compatible.  :P
	  (goto-char placeholder)
	  (c-add-syntax 'topmost-intro (c-point 'bol))
	  (if containing-decl-open
	      (if (c-keyword-member containing-decl-kwd
				    'c-other-block-decl-kwds)
		  (progn
		    (goto-char (c-brace-anchor-point containing-decl-open))
		    (c-add-stmt-syntax
		     (if (string-equal (symbol-name containing-decl-kwd)
				       "extern")
			 ;; Special case for compatibility with the
			 ;; extern-lang syntactic symbols.
			 'inextern-lang
		       (intern (concat "in"
				       (symbol-name containing-decl-kwd))))
		     nil t
		     (c-most-enclosing-brace paren-state (point))
		     paren-state))
		(c-add-class-syntax 'inclass
				    containing-decl-open
				    containing-decl-start
				    containing-decl-kwd
				    paren-state)))
	  (when (and c-syntactic-indentation-in-macros
		     macro-start
		     (/= macro-start (c-point 'boi indent-point)))
	    (c-add-syntax 'cpp-define-intro)
	    (setq macro-start nil)))

	 ;; CASE 5K: we are at an ObjC method definition
	 ;; continuation line.
	 ((and c-opt-method-key
	       (save-excursion
		 (c-beginning-of-statement-1 lim)
		 (beginning-of-line)
		 (when (looking-at c-opt-method-key)
		   (setq placeholder (point)))))
	  (c-add-syntax 'objc-method-args-cont placeholder))

	 ;; CASE 5L: we are at the first argument of a template
	 ;; arglist that begins on the previous line.
	 ((and c-recognize-<>-arglists
	       (eq (char-before) ?<)
	       (not (and c-overloadable-operators-regexp
			 (c-after-special-operator-id lim))))
	  (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	  (c-add-syntax 'template-args-cont (c-point 'boi)))

	 ;; CASE 5Q: we are at a statement within a macro.
	 (macro-start
	  (c-beginning-of-statement-1 containing-sexp)
	  (c-add-stmt-syntax 'statement nil t containing-sexp paren-state))

	 ;;CASE 5N: We are at a topmost continuation line and the only
	 ;;preceding items are annotations.
	 ((and (c-major-mode-is 'java-mode)
	       (setq placeholder (point))
	       (c-beginning-of-statement-1)
	       (progn
		 (while (and (c-forward-annotation))
		   (c-forward-syntactic-ws))
		 t)
	       (prog1
		   (>= (point) placeholder)
		 (goto-char placeholder)))
	  (c-add-syntax 'annotation-top-cont (c-point 'boi)))

	 ;; CASE 5M: we are at a topmost continuation line
	 (t
	  (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	  (when (c-major-mode-is 'objc-mode)
	    (setq placeholder (point))
	    (while (and (c-forward-objc-directive)
			(< (point) indent-point))
	      (c-forward-syntactic-ws)
	      (setq placeholder (point)))
	    (goto-char placeholder))
	  (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	 ))


       ;; (CASE 6 has been removed.)

       ;; CASE 7: line is an expression, not a statement.  Most
       ;; likely we are either in a function prototype or a function
       ;; call argument list
       ((not (or (and c-special-brace-lists
		      (save-excursion
			(goto-char containing-sexp)
			(c-looking-at-special-brace-list)))
		 (eq (char-after containing-sexp) ?{)))
	(cond

	 ;; CASE 7A: we are looking at the arglist closing paren.
	 ;; C.f. case 7F.
	 ((memq char-after-ip '(?\) ?\]))
	  (goto-char containing-sexp)
	  (setq placeholder (c-point 'boi))
	  (if (and (c-safe (backward-up-list 1) t)
		   (>= (point) placeholder))
	      (progn
		(forward-char)
		(skip-chars-forward " \t"))
	    (goto-char placeholder))
	  (c-add-stmt-syntax 'arglist-close (list containing-sexp) t
			     (c-most-enclosing-brace paren-state (point))
			     paren-state))

	 ;; CASE 7B: Looking at the opening brace of an
	 ;; in-expression block or brace list.	C.f. cases 4, 16A
	 ;; and 17E.
	 ((and (eq char-after-ip ?{)
	       (progn
		 (setq placeholder (c-inside-bracelist-p (point)
							 paren-state))
		 (if placeholder
		     (setq tmpsymbol '(brace-list-open . inexpr-class))
		   (setq tmpsymbol '(block-open . inexpr-statement)
			 placeholder
			 (cdr-safe (c-looking-at-inexpr-block
				    (c-safe-position containing-sexp
						     paren-state)
				    containing-sexp)))
		   ;; placeholder is nil if it's a block directly in
		   ;; a function arglist.  That makes us skip out of
		   ;; this case.
		   )))
	  (goto-char placeholder)
	  (back-to-indentation)
	  (c-add-stmt-syntax (car tmpsymbol) nil t
			     (c-most-enclosing-brace paren-state (point))
			     paren-state)
	  (if (/= (point) placeholder)
	      (c-add-syntax (cdr tmpsymbol))))

	 ;; CASE 7C: we are looking at the first argument in an empty
	 ;; argument list. Use arglist-close if we're actually
	 ;; looking at a close paren or bracket.
	 ((memq char-before-ip '(?\( ?\[))
	  (goto-char containing-sexp)
	  (setq placeholder (c-point 'boi))
	  (if (and (c-safe (backward-up-list 1) t)
		   (>= (point) placeholder))
	      (progn
		(forward-char)
		(skip-chars-forward " \t"))
	    (goto-char placeholder))
	  (c-add-stmt-syntax 'arglist-intro (list containing-sexp) t
			     (c-most-enclosing-brace paren-state (point))
			     paren-state))

	 ;; CASE 7D: we are inside a conditional test clause. treat
	 ;; these things as statements
	 ((progn
	    (goto-char containing-sexp)
	    (and (c-safe (c-forward-sexp -1) t)
		 (looking-at "\\<for\\>[^_]")))
	  (goto-char (1+ containing-sexp))
	  (c-forward-syntactic-ws indent-point)
	  (if (eq char-before-ip ?\;)
	      (c-add-syntax 'statement (point))
	    (c-add-syntax 'statement-cont (point))
	    ))

	 ;; CASE 7E: maybe a continued ObjC method call. This is the
	 ;; case when we are inside a [] bracketed exp, and what
	 ;; precede the opening bracket is not an identifier.
	 ((and c-opt-method-key
	       (eq (char-after containing-sexp) ?\[)
	       (progn
		 (goto-char (1- containing-sexp))
		 (c-backward-syntactic-ws (c-point 'bod))
		 (if (not (looking-at c-symbol-key))
		     (c-add-syntax 'objc-method-call-cont containing-sexp))
		 )))

	 ;; CASE 7F: we are looking at an arglist continuation line,
	 ;; but the preceding argument is on the same line as the
	 ;; opening paren.  This case includes multi-line
	 ;; mathematical paren groupings, but we could be on a
	 ;; for-list continuation line.  C.f. case 7A.
	 ((progn
	    (goto-char (1+ containing-sexp))
	    (< (save-excursion
		 (c-forward-syntactic-ws)
		 (point))
	       (c-point 'bonl)))
	  (goto-char containing-sexp)	; paren opening the arglist
	  (setq placeholder (c-point 'boi))
	  (if (and (c-safe (backward-up-list 1) t)
		   (>= (point) placeholder))
	      (progn
		(forward-char)
		(skip-chars-forward " \t"))
	    (goto-char placeholder))
	  (c-add-stmt-syntax 'arglist-cont-nonempty (list containing-sexp) t
			     (c-most-enclosing-brace c-state-cache (point))
			     paren-state))

	 ;; CASE 7G: we are looking at just a normal arglist
	 ;; continuation line
	 (t (c-forward-syntactic-ws indent-point)
	    (c-add-syntax 'arglist-cont (c-point 'boi)))
	 ))

       ;; CASE 8: func-local multi-inheritance line
       ((and (c-major-mode-is 'c++-mode)
	     (save-excursion
	       (goto-char indent-point)
	       (skip-chars-forward " \t")
	       (looking-at c-opt-postfix-decl-spec-key)))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(cond

	 ;; CASE 8A: non-hanging colon on an inher intro
	 ((eq char-after-ip ?:)
	  (c-backward-syntactic-ws lim)
	  (c-add-syntax 'inher-intro (c-point 'boi)))

	 ;; CASE 8B: hanging colon on an inher intro
	 ((eq char-before-ip ?:)
	  (c-add-syntax 'inher-intro (c-point 'boi)))

	 ;; CASE 8C: a continued inheritance line
	 (t
	  (c-beginning-of-inheritance-list lim)
	  (c-add-syntax 'inher-cont (point))
	  )))

       ;; CASE 9: we are inside a brace-list
       ((and (not (c-major-mode-is 'awk-mode)) ; Maybe this isn't needed (ACM, 2002/3/29)
	     (setq special-brace-list
		   (or (and c-special-brace-lists ;;;; ALWAYS NIL FOR AWK!!
			    (save-excursion
			      (goto-char containing-sexp)
			      (c-looking-at-special-brace-list)))
		       (c-inside-bracelist-p containing-sexp paren-state))))
	(cond

	 ;; CASE 9A: In the middle of a special brace list opener.
	 ((and (consp special-brace-list)
	       (save-excursion
		 (goto-char containing-sexp)
		 (eq (char-after) ?\())
	       (eq char-after-ip (car (cdr special-brace-list))))
	  (goto-char (car (car special-brace-list)))
	  (skip-chars-backward " \t")
	  (if (and (bolp)
		   (assoc 'statement-cont
			  (setq placeholder (c-guess-basic-syntax))))
	      (setq c-syntactic-context placeholder)
	    (c-beginning-of-statement-1
	     (c-safe-position (1- containing-sexp) paren-state))
	    (c-forward-token-2 0)
	    (while (looking-at c-specifier-key)
	      (goto-char (match-end 1))
	      (c-forward-syntactic-ws))
	    (c-add-syntax 'brace-list-open (c-point 'boi))))

	 ;; CASE 9B: brace-list-close brace
	 ((if (consp special-brace-list)
	      ;; Check special brace list closer.
	      (progn
		(goto-char (car (car special-brace-list)))
		(save-excursion
		  (goto-char indent-point)
		  (back-to-indentation)
		  (or
		   ;; We were between the special close char and the `)'.
		   (and (eq (char-after) ?\))
			(eq (1+ (point)) (cdr (car special-brace-list))))
		   ;; We were before the special close char.
		   (and (eq (char-after) (cdr (cdr special-brace-list)))
			(zerop (c-forward-token-2))
			(eq (1+ (point)) (cdr (car special-brace-list)))))))
	    ;; Normal brace list check.
	    (and (eq char-after-ip ?})
		 (c-safe (goto-char (c-up-list-backward (point))) t)
		 (= (point) containing-sexp)))
	  (if (eq (point) (c-point 'boi))
	      (c-add-syntax 'brace-list-close (point))
	    (setq lim (c-most-enclosing-brace c-state-cache (point)))
	    (c-beginning-of-statement-1 lim)
	    (c-add-stmt-syntax 'brace-list-close nil t lim paren-state)))

	 (t
	  ;; Prepare for the rest of the cases below by going to the
	  ;; token following the opening brace
	  (if (consp special-brace-list)
	      (progn
		(goto-char (car (car special-brace-list)))
		(c-forward-token-2 1 nil indent-point))
	    (goto-char containing-sexp))
	  (forward-char)
	  (let ((start (point)))
	    (c-forward-syntactic-ws indent-point)
	    (goto-char (max start (c-point 'bol))))
	  (c-skip-ws-forward indent-point)
	  (cond

	   ;; CASE 9C: we're looking at the first line in a brace-list
	   ((= (point) indent-point)
	    (if (consp special-brace-list)
		(goto-char (car (car special-brace-list)))
	      (goto-char containing-sexp))
	    (if (eq (point) (c-point 'boi))
		(c-add-syntax 'brace-list-intro (point))
	      (setq lim (c-most-enclosing-brace c-state-cache (point)))
	      (c-beginning-of-statement-1 lim)
	      (c-add-stmt-syntax 'brace-list-intro nil t lim paren-state)))

	   ;; CASE 9D: this is just a later brace-list-entry or
	   ;; brace-entry-open
	   (t (if (or (eq char-after-ip ?{)
		      (and c-special-brace-lists
			   (save-excursion
			     (goto-char indent-point)
			     (c-forward-syntactic-ws (c-point 'eol))
			     (c-looking-at-special-brace-list (point)))))
		  (c-add-syntax 'brace-entry-open (point))
		(c-add-syntax 'brace-list-entry (point))
		))
	   ))))

       ;; CASE 10: A continued statement or top level construct.
       ((and (not (memq char-before-ip '(?\; ?:)))
	     (not (c-at-vsemi-p before-ws-ip))
	     (or (not (eq char-before-ip ?}))
		 (c-looking-at-inexpr-block-backward c-state-cache))
	     (> (point)
		(save-excursion
		  (c-beginning-of-statement-1 containing-sexp)
		  (setq placeholder (point))))
	     (/= placeholder containing-sexp))
	;; This is shared with case 18.
	(c-guess-continued-construct indent-point
				     char-after-ip
				     placeholder
				     containing-sexp
				     paren-state))

       ;; CASE 16: block close brace, possibly closing the defun or
       ;; the class
       ((eq char-after-ip ?})
	;; From here on we have the next containing sexp in lim.
	(setq lim (c-most-enclosing-brace paren-state))
	(goto-char containing-sexp)
	(cond

	 ;; CASE 16E: Closing a statement block?  This catches
	 ;; cases where it's preceded by a statement keyword,
	 ;; which works even when used in an "invalid" context,
	 ;; e.g. a macro argument.
	 ((c-after-conditional)
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'block-close nil t lim paren-state))

	 ;; CASE 16A: closing a lambda defun or an in-expression
	 ;; block?  C.f. cases 4, 7B and 17E.
	 ((setq placeholder (c-looking-at-inexpr-block
			     (c-safe-position containing-sexp paren-state)
			     nil))
	  (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
			      'inline-close
			    'block-close))
	  (goto-char containing-sexp)
	  (back-to-indentation)
	  (if (= containing-sexp (point))
	      (c-add-syntax tmpsymbol (point))
	    (goto-char (cdr placeholder))
	    (back-to-indentation)
	    (c-add-stmt-syntax tmpsymbol nil t
			       (c-most-enclosing-brace paren-state (point))
			       paren-state)
	    (if (/= (point) (cdr placeholder))
		(c-add-syntax (car placeholder)))))

	 ;; CASE 16B: does this close an inline or a function in
	 ;; a non-class declaration level block?
	 ((save-excursion
	    (and lim
		 (progn
		   (goto-char lim)
		   (c-looking-at-decl-block
		    (c-most-enclosing-brace paren-state lim)
		    nil))
		 (setq placeholder (point))))
	  (c-backward-to-decl-anchor lim)
	  (back-to-indentation)
	  (if (save-excursion
		(goto-char placeholder)
		(looking-at c-other-decl-block-key))
	      (c-add-syntax 'defun-close (point))
	    (c-add-syntax 'inline-close (point))))

	 ;; CASE 16F: Can be a defun-close of a function declared
	 ;; in a statement block, e.g. in Pike or when using gcc
	 ;; extensions, but watch out for macros followed by
	 ;; blocks.  Let it through to be handled below.
	 ;; C.f. cases B.3 and 17G.
	 ((save-excursion
	    (and (not (c-at-statement-start-p))
		 (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		 (setq placeholder (point))
		 (let ((c-recognize-typeless-decls nil))
		   ;; Turn off recognition of constructs that
		   ;; lacks a type in this case, since that's more
		   ;; likely to be a macro followed by a block.
		   (c-forward-decl-or-cast-1 (c-point 'bosws) nil nil))))
	  (back-to-indentation)
	  (if (/= (point) containing-sexp)
	      (goto-char placeholder))
	  (c-add-stmt-syntax 'defun-close nil t lim paren-state))

	 ;; CASE 16C: If there is an enclosing brace then this is
	 ;; a block close since defun closes inside declaration
	 ;; level blocks have been handled above.
	 (lim
	  ;; If the block is preceded by a case/switch label on
	  ;; the same line, we anchor at the first preceding label
	  ;; at boi.  The default handling in c-add-stmt-syntax
	  ;; really fixes it better, but we do like this to keep
	  ;; the indentation compatible with version 5.28 and
	  ;; earlier.  C.f. case 17H.
	  (while (and (/= (setq placeholder (point)) (c-point 'boi))
		      (eq (c-beginning-of-statement-1 lim) 'label)))
	  (goto-char placeholder)
	  (if (looking-at c-label-kwds-regexp)
	      (c-add-syntax 'block-close (point))
	    (goto-char containing-sexp)
	    ;; c-backward-to-block-anchor not necessary here; those
	    ;; situations are handled in case 16E above.
	    (c-add-stmt-syntax 'block-close nil t lim paren-state)))

	 ;; CASE 16D: Only top level defun close left.
	 (t
	  (goto-char containing-sexp)
	  (c-backward-to-decl-anchor lim)
	  (c-add-stmt-syntax 'defun-close nil nil
			     (c-most-enclosing-brace paren-state)
			     paren-state))
	 ))

       ;; CASE 19: line is an expression, not a statement, and is directly
       ;; contained by a template delimiter.	Most likely, we are in a
       ;; template arglist within a statement.  This case is based on CASE
       ;; 7.	At some point in the future, we may wish to create more
       ;; syntactic symbols such as `template-intro',
       ;; `template-cont-nonempty', etc., and distinguish between them as we
       ;; do for `arglist-intro' etc. (2009-12-07).
       ((and c-recognize-<>-arglists
 	     (setq containing-< (c-up-list-backward indent-point containing-sexp))
 	     (eq (char-after containing-<) ?\<))
 	(setq placeholder (c-point 'boi containing-<))
 	(goto-char containing-sexp)	; Most nested Lbrace/Lparen (but not
 					; '<') before indent-point.
 	(if (>= (point) placeholder)
 	    (progn
 	      (forward-char)
 	      (skip-chars-forward " \t"))
 	  (goto-char placeholder))
 	(c-add-stmt-syntax 'template-args-cont (list containing-<) t
 			   (c-most-enclosing-brace c-state-cache (point))
 			   paren-state))

       ;; CASE 17: Statement or defun catchall.
       (t
	(goto-char indent-point)
	;; Back up statements until we find one that starts at boi.
	(while (let* ((prev-point (point))
		      (last-step-type (c-beginning-of-statement-1
				       containing-sexp)))
		 (if (= (point) prev-point)
		     (progn
		       (setq step-type (or step-type last-step-type))
		       nil)
		   (setq step-type last-step-type)
		   (/= (point) (c-point 'boi)))))
	(cond

	 ;; CASE 17B: continued statement
	 ((and (eq step-type 'same)
	       (/= (point) indent-point))
	  (c-add-stmt-syntax 'statement-cont nil nil
			     containing-sexp paren-state))

	 ;; CASE 17A: After a case/default label?
	 ((progn
	    (while (and (eq step-type 'label)
			(not (looking-at c-label-kwds-regexp)))
	      (setq step-type
		    (c-beginning-of-statement-1 containing-sexp)))
	    (eq step-type 'label))
	  (c-add-stmt-syntax (if (eq char-after-ip ?{)
				 'statement-case-open
			       'statement-case-intro)
			     nil t containing-sexp paren-state))

	 ;; CASE 17D: any old statement
	 ((progn
	    (while (eq step-type 'label)
	      (setq step-type
		    (c-beginning-of-statement-1 containing-sexp)))
	    (eq step-type 'previous))
	  (c-add-stmt-syntax 'statement nil t
			     containing-sexp paren-state)
	  (if (eq char-after-ip ?{)
	      (c-add-syntax 'block-open)))

	 ;; CASE 17I: Inside a substatement block.
	 ((progn
	    ;; The following tests are all based on containing-sexp.
	    (goto-char containing-sexp)
	    ;; From here on we have the next containing sexp in lim.
	    (setq lim (c-most-enclosing-brace paren-state containing-sexp))
	    (c-after-conditional))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'statement-block-intro nil t
			     lim paren-state)
	  (if (eq char-after-ip ?{)
	      (c-add-syntax 'block-open)))

	 ;; CASE 17E: first statement in an in-expression block.
	 ;; C.f. cases 4, 7B and 16A.
	 ((setq placeholder (c-looking-at-inexpr-block
			     (c-safe-position containing-sexp paren-state)
			     nil))
	  (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
			      'defun-block-intro
			    'statement-block-intro))
	  (back-to-indentation)
	  (if (= containing-sexp (point))
	      (c-add-syntax tmpsymbol (point))
	    (goto-char (cdr placeholder))
	    (back-to-indentation)
	    (c-add-stmt-syntax tmpsymbol nil t
			       (c-most-enclosing-brace c-state-cache (point))
			       paren-state)
	    (if (/= (point) (cdr placeholder))
		(c-add-syntax (car placeholder))))
	  (if (eq char-after-ip ?{)
	      (c-add-syntax 'block-open)))

	 ;; CASE 17F: first statement in an inline, or first
	 ;; statement in a top-level defun. we can tell this is it
	 ;; if there are no enclosing braces that haven't been
	 ;; narrowed out by a class (i.e. don't use bod here).
	 ((save-excursion
	    (or (not (setq placeholder (c-most-enclosing-brace
					paren-state)))
		(and (progn
		       (goto-char placeholder)
		       (eq (char-after) ?{))
		     (c-looking-at-decl-block (c-most-enclosing-brace
					       paren-state (point))
					      nil))))
	  (c-backward-to-decl-anchor lim)
	  (back-to-indentation)
	  (c-add-syntax 'defun-block-intro (point)))

	 ;; CASE 17G: First statement in a function declared inside
	 ;; a normal block.  This can occur in Pike and with
	 ;; e.g. the gcc extensions, but watch out for macros
	 ;; followed by blocks.  C.f. cases B.3 and 16F.
	 ((save-excursion
	    (and (not (c-at-statement-start-p))
		 (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		 (setq placeholder (point))
		 (let ((c-recognize-typeless-decls nil))
		   ;; Turn off recognition of constructs that lacks
		   ;; a type in this case, since that's more likely
		   ;; to be a macro followed by a block.
		   (c-forward-decl-or-cast-1 (c-point 'bosws) nil nil))))
	  (back-to-indentation)
	  (if (/= (point) containing-sexp)
	      (goto-char placeholder))
	  (c-add-stmt-syntax 'defun-block-intro nil t
			     lim paren-state))

	 ;; CASE 17H: First statement in a block.
	 (t
	  ;; If the block is preceded by a case/switch label on the
	  ;; same line, we anchor at the first preceding label at
	  ;; boi.  The default handling in c-add-stmt-syntax is
	  ;; really fixes it better, but we do like this to keep the
	  ;; indentation compatible with version 5.28 and earlier.
	  ;; C.f. case 16C.
	  (while (and (/= (setq placeholder (point)) (c-point 'boi))
		      (eq (c-beginning-of-statement-1 lim) 'label)))
	  (goto-char placeholder)
	  (if (looking-at c-label-kwds-regexp)
	      (c-add-syntax 'statement-block-intro (point))
	    (goto-char containing-sexp)
	    ;; c-backward-to-block-anchor not necessary here; those
	    ;; situations are handled in case 17I above.
	    (c-add-stmt-syntax 'statement-block-intro nil t
			       lim paren-state))
	  (if (eq char-after-ip ?{)
	      (c-add-syntax 'block-open)))
	 ))
       )

      ;; now we need to look at any modifiers
      (goto-char indent-point)
      (skip-chars-forward " \t")

      ;; are we looking at a comment only line?
      (when (and (looking-at c-comment-start-regexp)
		 (/= (c-forward-token-2 0 nil (c-point 'eol)) 0))
	(c-append-syntax 'comment-intro))

      ;; we might want to give additional offset to friends (in C++).
      (when (and c-opt-friend-key
		 (looking-at c-opt-friend-key))
	(c-append-syntax 'friend))

      ;; Set syntactic-relpos.
      (let ((p c-syntactic-context))
	(while (and p
		    (if (integerp (c-langelem-pos (car p)))
			(progn
			  (setq syntactic-relpos (c-langelem-pos (car p)))
			  nil)
		      t))
	  (setq p (cdr p))))

      ;; Start of or a continuation of a preprocessor directive?
      (if (and macro-start
	       (eq macro-start (c-point 'boi))
	       (not (and (c-major-mode-is 'pike-mode)
			 (eq (char-after (1+ macro-start)) ?\"))))
	  (c-append-syntax 'cpp-macro)
	(when (and c-syntactic-indentation-in-macros macro-start)
	  (if in-macro-expr
	      (when (or
		     (< syntactic-relpos macro-start)
		     (not (or
			   (assq 'arglist-intro c-syntactic-context)
			   (assq 'arglist-cont c-syntactic-context)
			   (assq 'arglist-cont-nonempty c-syntactic-context)
			   (assq 'arglist-close c-syntactic-context))))
		;; If inside a cpp expression, i.e. anywhere in a
		;; cpp directive except a #define body, we only let
		;; through the syntactic analysis that is internal
		;; in the expression.  That means the arglist
		;; elements, if they are anchored inside the cpp
		;; expression.
		(setq c-syntactic-context nil)
		(c-add-syntax 'cpp-macro-cont macro-start))
	    (when (and (eq macro-start syntactic-relpos)
		       (not (assq 'cpp-define-intro c-syntactic-context))
		       (save-excursion
			 (goto-char macro-start)
			 (or (not (c-forward-to-cpp-define-body))
			     (<= (point) (c-point 'boi indent-point)))))
	      ;; Inside a #define body and the syntactic analysis is
	      ;; anchored on the start of the #define.  In this case
	      ;; we add cpp-define-intro to get the extra
	      ;; indentation of the #define body.
	      (c-add-syntax 'cpp-define-intro)))))

      ;; return the syntax
      c-syntactic-context)))


;; Indentation calculation.

(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  ;;
  ;; This function might do hidden buffer changes.
  (let ((res
	 (cond
	  ((numberp offset) offset)
	  ((vectorp offset) offset)
	  ((null offset)    nil)

	  ((eq offset '+)   c-basic-offset)
	  ((eq offset '-)   (- c-basic-offset))
	  ((eq offset '++)  (* 2 c-basic-offset))
	  ((eq offset '--)  (* 2 (- c-basic-offset)))
	  ((eq offset '*)   (/ c-basic-offset 2))
	  ((eq offset '/)   (/ (- c-basic-offset) 2))

	  ((functionp offset)
	   (c-evaluate-offset
	    (funcall offset
		     (cons (c-langelem-sym langelem)
			   (c-langelem-pos langelem)))
	    langelem symbol))

	  ((listp offset)
	   (cond
	    ((eq (car offset) 'quote)
	     (c-benign-error "The offset %S for %s was mistakenly quoted"
			     offset symbol)
	     nil)

	    ((memq (car offset) '(min max))
	     (let (res val (method (car offset)))
	       (setq offset (cdr offset))
	       (while offset
		 (setq val (c-evaluate-offset (car offset) langelem symbol))
		 (cond
		  ((not val))
		  ((not res)
		   (setq res val))
		  ((integerp val)
		   (if (vectorp res)
		       (c-benign-error "\
Error evaluating offset %S for %s: \
Cannot combine absolute offset %S with relative %S in `%s' method"
				       (car offset) symbol res val method)
		     (setq res (funcall method res val))))
		  (t
		   (if (integerp res)
		       (c-benign-error "\
Error evaluating offset %S for %s: \
Cannot combine relative offset %S with absolute %S in `%s' method"
				       (car offset) symbol res val method)
		     (setq res (vector (funcall method (aref res 0)
						(aref val 0)))))))
		 (setq offset (cdr offset)))
	       res))

	    ((eq (car offset) 'add)
	     (let (res val)
	       (setq offset (cdr offset))
	       (while offset
		 (setq val (c-evaluate-offset (car offset) langelem symbol))
		 (cond
		  ((not val))
		  ((not res)
		   (setq res val))
		  ((integerp val)
		   (if (vectorp res)
		       (setq res (vector (+ (aref res 0) val)))
		     (setq res (+ res val))))
		  (t
		   (if (vectorp res)
		       (c-benign-error "\
Error evaluating offset %S for %s: \
Cannot combine absolute offsets %S and %S in `add' method"
				       (car offset) symbol res val)
		     (setq res val))))	; Override.
		 (setq offset (cdr offset)))
	       res))

	    (t
	     (let (res)
	       (when (eq (car offset) 'first)
		 (setq offset (cdr offset)))
	       (while (and (not res) offset)
		 (setq res (c-evaluate-offset (car offset) langelem symbol)
		       offset (cdr offset)))
	       res))))

	  ((and (symbolp offset) (boundp offset))
	   (symbol-value offset))

	  (t
	   (c-benign-error "Unknown offset format %S for %s" offset symbol)
	   nil))))

    (if (or (null res) (integerp res)
	    (and (vectorp res) (= (length res) 1) (integerp (aref res 0))))
	res
      (c-benign-error "Error evaluating offset %S for %s: Got invalid value %S"
		      offset symbol res)
      nil)))

(defun c-calc-offset (langelem)
  ;; Get offset from LANGELEM which is a list beginning with the
  ;; syntactic symbol and followed by any analysis data it provides.
  ;; That data may be zero or more elements, but if at least one is
  ;; given then the first is the anchor position (or nil).  The symbol
  ;; is matched against `c-offsets-alist' and the offset calculated
  ;; from that is returned.
  ;;
  ;; This function might do hidden buffer changes.
  (let* ((symbol (c-langelem-sym langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if match
	(setq offset (c-evaluate-offset offset langelem symbol))
      (if c-strict-syntax-p
	  (c-benign-error "No offset found for syntactic symbol %s" symbol))
      (setq offset 0))
    (if (vectorp offset)
	offset
      (or (and (numberp offset) offset)
	  (and (symbolp offset) (symbol-value offset))
	  0))
    ))

(defun c-get-offset (langelem)
  ;; This is a compatibility wrapper for `c-calc-offset' in case
  ;; someone is calling it directly.  It takes an old style syntactic
  ;; element on the form (SYMBOL . ANCHOR-POS) and converts it to the
  ;; new list form.
  ;;
  ;; This function might do hidden buffer changes.
  (if (c-langelem-pos langelem)
      (c-calc-offset (list (c-langelem-sym langelem)
			   (c-langelem-pos langelem)))
    (c-calc-offset langelem)))

(defun c-get-syntactic-indentation (langelems)
  ;; Calculate the syntactic indentation from a syntactic description
  ;; as returned by `c-guess-syntax'.
  ;;
  ;; Note that topmost-intro always has an anchor position at bol, for
  ;; historical reasons.  It's often used together with other symbols
  ;; that has more sane positions.  Since we always use the first
  ;; found anchor position, we rely on that these other symbols always
  ;; precede topmost-intro in the LANGELEMS list.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((indent 0) anchor)

    (while langelems
      (let* ((c-syntactic-element (car langelems))
	     (res (c-calc-offset c-syntactic-element)))

	(if (vectorp res)
	    ;; Got an absolute column that overrides any indentation
	    ;; we've collected so far, but not the relative
	    ;; indentation we might get for the nested structures
	    ;; further down the langelems list.
	    (setq indent (elt res 0)
		  anchor (point-min))	; A position at column 0.

	  ;; Got a relative change of the current calculated
	  ;; indentation.
	  (setq indent (+ indent res))

	  ;; Use the anchor position from the first syntactic
	  ;; element with one.
	  (unless anchor
	    (setq anchor (c-langelem-pos (car langelems)))))

	(setq langelems (cdr langelems))))

    (if anchor
	(+ indent (save-excursion
		    (goto-char anchor)
		    (current-column)))
      indent)))


(cc-provide 'cc-engine)

;;; cc-engine.el ends here
