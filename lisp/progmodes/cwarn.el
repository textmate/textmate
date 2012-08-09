;;; cwarn.el --- highlight suspicious C and C++ constructions

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Anders Lindgren <andersl@andersl.com>
;; Keywords: c, languages, faces
;; X-Url: http://www.andersl.com/emacs
;; Version: 1.3.1

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

;;{{{ Documentation

;; Description:
;;
;; CWarn is a package that highlights suspicious C and C++ constructions.
;;
;; For example, take a look at the following piece of C code:
;;
;;     if (x = 0);
;;       foo();
;;
;; The code contains two, possibly fatal, bugs.  The first is that the
;; assignment operator "=" is used as part of the test; the user
;; probably meant to use the comparison operator "==".
;;
;; The second problem is that an extra semicolon is placed after
;; closing parenthesis of the test expression.  This makes the body of
;; the if statement to be an empty statement, not the call to the
;; function "foo", as the user probably intended.
;;
;; This package is capable of highlighting the following C and C++
;; constructions:
;;
;; * Assignments inside expressions, including variations like "+=".
;; * Semicolon following immediately after `if', `for', and `while'
;;   (except, of course, after a `do .. while' statement).
;; * C++ functions with reference parameters.
;;
;; Note that none of the constructions highlighted (especially not C++
;; reference parameters) are considered errors by the language
;; definitions.

;; Usage:
;;
;; CWarn is implemented as two minor modes: `cwarn-mode' and
;; `global-cwarn-mode'.  The former can be applied to individual buffers
;; and the latter to all buffers.
;;
;; Activate this package by Customize, or by placing the following line
;; into the appropriate init file:
;;
;;    (global-cwarn-mode 1)
;;
;; Also, `font-lock-mode' or `global-font-lock-mode' must be enabled.

;; Afterthought:
;;
;; After using this package for several weeks it feels as though I
;; find stupid typo-style bugs while editing rather than at compile-
;; or run-time, if I ever find them.
;;
;; On the other hand, I find myself using assignments inside
;; expressions much more often than I used to do.  The reason is that
;; there is no risk of interpreting an assignment operator as a
;; comparison ("hey, the assignment operator is red, duh!").

;; Reporting bugs:
;;
;;     Out of the last ten bugs you found, how many did you report?
;;
;; When reporting a bug, please:
;;
;; * Send a mail the maintainer of the package, or to the author
;;   if no maintainer exists.
;; * Include the name of the package in the title of the mail, to
;;   simplify for the recipient.
;; * State exactly what you did, what happened, and what you expected
;;   to see when you found the bug.
;; * If the bug cause an error, set the variable `debug-on-error' to t,
;;   repeat the operations that triggered the error and include
;;   the backtrace in the letter.
;; * If possible, include an example that activates the bug.
;; * Should you speculate about the cause of the problem, please
;;   state explicitly that you are guessing.

;;}}}

;;; Code:

;;{{{ Dependencies

(eval-when-compile (require 'cl))

(require 'custom)
(require 'font-lock)
(require 'cc-mode)

;;}}}
;;{{{ Variables

(defgroup cwarn nil
  "Highlight suspicious C and C++ constructions."
  :version "21.1"
  :group 'faces)

(defvar cwarn-mode nil
  "*Non-nil when Cwarn mode is active.

Never set this variable directly, use the command `cwarn-mode'
instead.")

(defcustom cwarn-configuration
  '((c-mode (not reference))
    (c++-mode t))
  "List of items each describing which features are enable for a mode.
Each item is on the form (mode featurelist), where featurelist can be
on one of three forms:

* A list of enabled features.
* A list starting with the atom `not' followed by the features
  which are not enabled.
* The atom t, that represent that all features are enabled.

See variable `cwarn-font-lock-feature-keywords-alist' for available
features."
  :type '(repeat sexp)
  :group 'cwarn)

(defcustom cwarn-font-lock-feature-keywords-alist
  '((assign    . cwarn-font-lock-assignment-keywords)
    (semicolon . cwarn-font-lock-semicolon-keywords)
    (reference . cwarn-font-lock-reference-keywords))
  "An alist mapping a CWarn feature to font-lock keywords.
The keywords could either a font-lock keyword list or a symbol.
If it is a symbol it is assumed to be a variable containing a font-lock
keyword list."
  :type '(alist :key-type (choice (const assign)
				  (const semicolon)
				  (const reference))
		:value-type (sexp :tag "Value"))
  :group 'cwarn)

(defcustom cwarn-verbose t
  "When nil, CWarn mode will not generate any messages.

Currently, messages are generated when the mode is activated and
deactivated."
  :group 'cwarn
  :type 'boolean)

(defcustom cwarn-mode-text " CWarn"
  "String to display in the mode line when CWarn mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "CWarn mode text"                ; To separate it from `global-...'
  :group 'cwarn
  :type 'string)

(defcustom cwarn-load-hook nil
  "Functions to run when CWarn mode is first loaded."
  :tag "Load Hook"
  :group 'cwarn
  :type 'hook)

;;}}}
;;{{{ The modes

;;;###autoload
(define-minor-mode cwarn-mode
  "Minor mode that highlights suspicious C and C++ constructions.

Suspicious constructs are highlighted using `font-lock-warning-face'.

Note, in addition to enabling this minor mode, the major mode must
be included in the variable `cwarn-configuration'.  By default C and
C++ modes are included.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :group 'cwarn :lighter cwarn-mode-text
  (cwarn-font-lock-keywords cwarn-mode)
  (if font-lock-mode (font-lock-fontify-buffer)))

;;;###autoload
(defun turn-on-cwarn-mode ()
  "Turn on CWarn mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-cwarn-mode)"
  (cwarn-mode 1))
(make-obsolete 'turn-on-cwarn-mode 'cwarn-mode "24.1")

;;}}}
;;{{{ Help functions

(defun cwarn-is-enabled (mode &optional feature)
  "Non-nil if CWarn FEATURE is enabled for MODE.
FEATURE is an atom representing one construction to highlight.

Check if any feature is enabled for MODE if no feature is specified.

The valid features are described by the variable
`cwarn-font-lock-feature-keywords-alist'."
  (let ((mode-configuration (assq mode cwarn-configuration)))
    (and mode-configuration
	 (or (null feature)
	     (let ((list-or-t (nth 1 mode-configuration)))
	       (or (eq list-or-t t)
		   (if (eq (car-safe list-or-t) 'not)
		       (not (memq feature (cdr list-or-t)))
		     (memq feature list-or-t))))))))

(defun cwarn-inside-macro ()
  "True if point is inside a C macro definition."
  (save-excursion
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (forward-line -1))
    (back-to-indentation)
    (eq (char-after) ?#)))

(defun cwarn-font-lock-keywords (addp)
  "Install/remove keywords into current buffer.
If ADDP is non-nil, install else remove."
  (dolist (pair cwarn-font-lock-feature-keywords-alist)
    (let ((feature (car pair))
	  (keywords (cdr pair)))
      (if (not (listp keywords))
	  (setq keywords (symbol-value keywords)))
      (if (cwarn-is-enabled major-mode feature)
	  (funcall (if addp 'font-lock-add-keywords 'font-lock-remove-keywords)
		   nil keywords)))))

;;}}}
;;{{{ Font-lock keywords and match functions

;; This section contains font-lock keywords.  A font lock keyword can
;; either contain a regular expression or a match function.  All
;; keywords defined here use match functions since the C and C++
;; constructions highlighted by CWarn are too complex to be matched by
;; regular expressions.
;;
;; A match function should act like a normal forward search.  They
;; should return non-nil if they found a candidate and the match data
;; should correspond to the highlight part of the font-lock keyword.
;; The functions should not generate errors, in that case font-lock
;; will fail to highlight the buffer.  A match function takes one
;; argument, LIMIT, that represent the end of area to be searched.
;;
;; The variable `cwarn-font-lock-feature-keywords-alist' contains a
;; mapping from CWarn features to the font-lock keywords defined
;; below.

(defmacro cwarn-font-lock-match (re &rest body)
  "Match RE but only if BODY holds."
  `(let ((res nil))
     (while
	 (progn
	   (setq res (re-search-forward ,re limit t))
	   (and res
		(save-excursion
		  (when (match-beginning 1) (goto-char (match-beginning 1)))
		  (condition-case nil	; In case something barfs.
		      (not (save-match-data
			     ,@body))
		    (error t))))))
     res))

;;{{{ Assignment in expressions

(defconst cwarn-font-lock-assignment-keywords
  '((cwarn-font-lock-match-assignment-in-expression
     (1 font-lock-warning-face))))

(defun cwarn-font-lock-match-assignment-in-expression (limit)
  "Match assignments inside expressions."
  (cwarn-font-lock-match
   "[^!<>=]\\(\\([-+*/%&^|]\\|<<\\|>>\\)?=\\)[^=]"
   (backward-up-list 1)
   (and (memq (following-char) '(?\( ?\[))
	(not (progn
	       (skip-chars-backward " ")
	       (skip-chars-backward "a-zA-Z0-9_")
	       (or
		;; Default parameter of function.
		(c-at-toplevel-p)
		(looking-at "for\\>")))))))

;;}}}
;;{{{ Semicolon

(defconst cwarn-font-lock-semicolon-keywords
  '((cwarn-font-lock-match-dangerous-semicolon (0 font-lock-warning-face))))

(defun cwarn-font-lock-match-dangerous-semicolon (limit)
  "Match semicolons directly after `for', `while', and `if'.
The semicolon after a `do { ... } while (x);' construction is not matched."
  (cwarn-font-lock-match
   ";"
   (backward-sexp 2)			; Expression and keyword.
   (or (looking-at "\\(for\\|if\\)\\>")
       (and (looking-at "while\\>")
	    (condition-case nil
		(progn
		  (backward-sexp 2)	; Body and "do".
		  (not (looking-at "do\\>")))
	      (error t))))))

;;}}}
;;{{{ Reference

(defconst cwarn-font-lock-reference-keywords
  '((cwarn-font-lock-match-reference (1 font-lock-warning-face))))

(defun cwarn-font-lock-match-reference (limit)
  "Font-lock matcher for C++ reference parameters."
  (cwarn-font-lock-match
   "[^&]\\(&\\)[^&=]"
   (backward-up-list 1)
   (and (eq (following-char) ?\()
	(not (cwarn-inside-macro))
	(c-at-toplevel-p))))

;;}}}

;;}}}
;;{{{ The end

(defun turn-on-cwarn-mode-if-enabled ()
  "Turn on CWarn mode in the current buffer if applicable.
The mode is turned if some feature is enabled for the current
`major-mode' in `cwarn-configuration'."
  (when (cwarn-is-enabled major-mode) (cwarn-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-cwarn-mode
  cwarn-mode turn-on-cwarn-mode-if-enabled)

(provide 'cwarn)

(run-hooks 'cwarn-load-hook)

;;}}}

;;; cwarn.el ends here
