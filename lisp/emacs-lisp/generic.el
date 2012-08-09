;;; generic.el --- defining simple major modes with comment and font-lock
;;
;; Copyright (C) 1997, 1999, 2001-2012 Free Software Foundation, Inc.
;;
;; Author:  Peter Breton <pbreton@cs.umb.edu>
;; Created: Fri Sep 27 1996
;; Keywords: generic, comment, font-lock
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

;; INTRODUCTION:
;;
;; The macro `define-generic-mode' can be used to define small modes
;; which provide basic comment and font-lock support.  These modes are
;; intended for the many configuration files and such which are too
;; small for a "real" mode, but still have a regular syntax, comment
;; characters and the like.
;;
;; Each generic mode can define the following:
;;
;; * List of comment-characters.  The elements of this list should be
;;   either a character, a one or two character string, or a cons
;;   cell.  If the entry is a character or a string, it is added to
;;   the mode's syntax table with "comment starter" syntax.  If the
;;   entry is a cons cell, the `car' and `cdr' of the pair are
;;   considered the "comment starter" and "comment ender"
;;   respectively.  (The latter should be nil if you want comments to
;;   end at the end of the line.)  Emacs does not support comment
;;   strings of more than two characters in length.
;;
;; * List of keywords to font-lock.  Each keyword should be a string.
;;   If you have additional keywords which should be highlighted in a
;;   face different from `font-lock-keyword-face', you can use the
;;   convenience function `generic-make-keywords-list' (which see),
;;   and add the result to the following list:
;;
;; * Additional expressions to font-lock.  This should be a list of
;;   expressions, each of which should be of the same form as those in
;;   `font-lock-keywords'.
;;
;; * List of regular expressions to be placed in auto-mode-alist.
;;
;; * List of functions to call to do some additional setup
;;
;; This should pretty much cover basic functionality; if you need much
;; more than this, or you find yourself writing extensive customizations,
;; perhaps you should be writing a major mode instead!
;;
;; EXAMPLE:
;;
;; You can use `define-generic-mode' like this:
;;
;;   (define-generic-mode 'foo-generic-mode
;;     (list ?%)
;;     (list "keyword")
;;     nil
;;     (list "\\.FOO\\'")
;;     (list 'foo-setup-function))
;;
;; to define a new generic-mode `foo-generic-mode', which has '%' as a
;; comment character, and "keyword" as a keyword.  When files which
;; end in '.FOO' are loaded, Emacs will go into foo-generic-mode and
;; call foo-setup-function.  You can also use the function
;; `foo-generic-mode' (which is interactive) to put a buffer into
;; foo-generic-mode.
;;
;; GOTCHAS:
;;
;; Be careful that your font-lock definitions are correct.  Getting
;; them wrong can cause Emacs to continually attempt to fontify! This
;; problem is not specific to generic-mode.

;; Credit for suggestions, brainstorming, help with debugging:
;;   ACorreir@pervasive-sw.com (Alfred Correira)
;; Extensive cleanup by:
;;   Stefan Monnier (monnier+gnu/emacs@flint.cs.yale.edu)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar generic-font-lock-keywords nil
  "Keywords for `font-lock-defaults' in a generic mode.")
(make-variable-buffer-local 'generic-font-lock-keywords)
(define-obsolete-variable-alias 'generic-font-lock-defaults 'generic-font-lock-keywords "22.1")

;;;###autoload
(defvar generic-mode-list nil
  "A list of mode names for `generic-mode'.
Do not add entries to this list directly; use `define-generic-mode'
instead (which see).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defmacro define-generic-mode (mode comment-list keyword-list
				    font-lock-list auto-mode-list
				    function-list &optional docstring)
  "Create a new generic mode MODE.

MODE is the name of the command for the generic mode; don't quote it.
The optional DOCSTRING is the documentation for the mode command.  If
you do not supply it, `define-generic-mode' uses a default
documentation string instead.

COMMENT-LIST is a list in which each element is either a character, a
string of one or two characters, or a cons cell.  A character or a
string is set up in the mode's syntax table as a \"comment starter\".
If the entry is a cons cell, the `car' is set up as a \"comment
starter\" and the `cdr' as a \"comment ender\".  (Use nil for the
latter if you want comments to end at the end of the line.)  Note that
the syntax table has limitations about what comment starters and
enders are actually possible.

KEYWORD-LIST is a list of keywords to highlight with
`font-lock-keyword-face'.  Each keyword should be a string.

FONT-LOCK-LIST is a list of additional expressions to highlight.  Each
element of this list should have the same form as an element of
`font-lock-keywords'.

AUTO-MODE-LIST is a list of regular expressions to add to
`auto-mode-alist'.  These regular expressions are added when Emacs
runs the macro expansion.

FUNCTION-LIST is a list of functions to call to do some additional
setup.  The mode command calls these functions just before it runs the
mode hook `MODE-hook'.

See the file generic-x.el for some examples of `define-generic-mode'."
  (declare (debug (sexp def-form def-form def-form form def-form
			[&optional stringp] &rest [keywordp form]))
	   (indent 1))

  ;; Backward compatibility.
  (when (eq (car-safe mode) 'quote)
    (setq mode (eval mode)))

  (let* ((name (symbol-name mode))
	 (pretty-name (capitalize (replace-regexp-in-string
				   "-mode\\'" "" name))))

    `(progn
       ;; Add a new entry.
       (add-to-list 'generic-mode-list ,name)

       ;; Add it to auto-mode-alist
       (dolist (re ,auto-mode-list)
	 (add-to-list 'auto-mode-alist (cons re ',mode)))

       (defun ,mode ()
	 ,(or docstring
	      (concat pretty-name " mode.\n"
		      "This a generic mode defined with `define-generic-mode'.\n"
		      "It runs `" name "-hook' as the last thing it does."))
	 (interactive)
	 (generic-mode-internal ',mode ,comment-list ,keyword-list
				,font-lock-list ,function-list)))))

;;;###autoload
(defun generic-mode-internal (mode comment-list keyword-list
				   font-lock-list function-list)
  "Go into the generic mode MODE."
  (let* ((name (symbol-name mode))
	 (pretty-name (capitalize (replace-regexp-in-string
				   "-mode\\'" "" name)))
	 (mode-hook (intern (concat name "-hook"))))

    (kill-all-local-variables)

    (setq major-mode mode
	  mode-name pretty-name)

    (generic-mode-set-comments comment-list)

    ;; Font-lock functionality.
    ;; Font-lock-defaults is always set even if there are no keywords
    ;; or font-lock expressions, so comments can be highlighted.
    (setq generic-font-lock-keywords font-lock-list)
    (when keyword-list
      (push (concat "\\_<" (regexp-opt keyword-list t) "\\_>")
	    generic-font-lock-keywords))
    (setq font-lock-defaults '(generic-font-lock-keywords))

    ;; Call a list of functions
    (mapc 'funcall function-list)

    (run-mode-hooks mode-hook)))

;;;###autoload
(defun generic-mode (mode)
  "Enter generic mode MODE.

Generic modes provide basic comment and font-lock functionality
for \"generic\" files.  (Files which are too small to warrant their
own mode, but have comment characters, keywords, and the like.)

To define a generic-mode, use the function `define-generic-mode'.
Some generic modes are defined in `generic-x.el'."
  (interactive
   (list (completing-read "Generic mode: " generic-mode-list nil t)))
  (funcall (intern mode)))

;;; Comment Functionality
(defun generic-mode-set-comments (comment-list)
  "Set up comment functionality for generic mode."
  (let ((st (make-syntax-table))
	(chars nil)
	(comstyles))
    (make-local-variable 'comment-start)
    (make-local-variable 'comment-start-skip)
    (make-local-variable 'comment-end)

    ;; Go through all the comments
    (dolist (start comment-list)
      (let (end (comstyle ""))
	;; Normalize
	(when (consp start)
	  (setq end (cdr start))
	  (setq start (car start)))
	(when (characterp start) (setq start (char-to-string start)))
	(cond
	 ((characterp end)   (setq end (char-to-string end)))
	 ((zerop (length end)) (setq end "\n")))

	;; Setup the vars for `comment-region'
	(if comment-start
	    ;; We have already setup a comment-style, so use style b
	    (progn
	      (setq comstyle "b")
	      (setq comment-start-skip
		    (concat comment-start-skip "\\|" (regexp-quote start) "+\\s-*")))
	  ;; First comment-style
	  (setq comment-start start)
	  (setq comment-end (if (string-equal end "\n") "" end))
	  (setq comment-start-skip (concat (regexp-quote start) "+\\s-*")))

       ;; Reuse comstyles if necessary
       (setq comstyle
             (or (cdr (assoc start comstyles))
                 (cdr (assoc end comstyles))
                 comstyle))
       (push (cons start comstyle) comstyles)
       (push (cons end comstyle) comstyles)

	;; Setup the syntax table
	(if (= (length start) 1)
	    (modify-syntax-entry (string-to-char start)
				 (concat "< " comstyle) st)
	  (let ((c0 (elt start 0)) (c1 (elt start 1)))
	    ;; Store the relevant info but don't update yet
	    (push (cons c0 (concat (cdr (assoc c0 chars)) "1")) chars)
	    (push (cons c1 (concat (cdr (assoc c1 chars))
				   (concat "2" comstyle))) chars)))
	(if (= (length end) 1)
	    (modify-syntax-entry (string-to-char end)
				 (concat ">" comstyle) st)
	  (let ((c0 (elt end 0)) (c1 (elt end 1)))
	    ;; Store the relevant info but don't update yet
	    (push (cons c0 (concat (cdr (assoc c0 chars))
				   (concat "3" comstyle))) chars)
	    (push (cons c1 (concat (cdr (assoc c1 chars)) "4")) chars)))))

    ;; Process the chars that were part of a 2-char comment marker
    (dolist (cs (nreverse chars))
      (modify-syntax-entry (car cs)
			   (concat (char-to-string (char-syntax (car cs)))
				   " " (cdr cs))
			   st))
    (set-syntax-table st)))

(defun generic-bracket-support ()
  "Imenu support for [KEYWORD] constructs found in INF, INI and Samba files."
  (setq imenu-generic-expression
	'((nil "^\\[\\(.*\\)\\]" 1))
        imenu-case-fold-search t))

;;;###autoload
(defun generic-make-keywords-list (keyword-list face &optional prefix suffix)
  "Return a `font-lock-keywords' construct that highlights KEYWORD-LIST.
KEYWORD-LIST is a list of keyword strings that should be
highlighted with face FACE.  This function calculates a regular
expression that matches these keywords and concatenates it with
PREFIX and SUFFIX.  Then it returns a construct based on this
regular expression that can be used as an element of
`font-lock-keywords'."
  (unless (listp keyword-list)
    (error "Keywords argument must be a list of strings"))
  (list (concat prefix "\\_<"
		;; Use an optimized regexp.
		(regexp-opt keyword-list t)
		"\\_>" suffix)
	1
	face))

(provide 'generic)

;;; generic.el ends here
