;;; cap-words.el --- minor mode for motion in CapitalizedWordIdentifiers

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages

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

;; Provides Capitalized Words minor mode for word movement in
;; identifiers CapitalizedLikeThis.

;; Note that the same effect could be obtained by frobbing the
;; category of upper case characters to produce word boundaries, but
;; the necessary processing isn't done for ASCII characters.

;; Fixme: This doesn't work properly for mouse double clicks.

;;; Code:

(defun capitalized-find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'.
Looks for word boundaries before capitals."
  (save-excursion
    (goto-char pos)
    (let (case-fold-search)
      (if (<= pos limit)
	  ;; Fixme: Are these regexps the best?
	  (or (and (re-search-forward "\\=.\\w*[[:upper:]]"
				      limit t)
		   (progn (backward-char)
			  t))
	      (re-search-forward "\\>" limit t))
	(or (re-search-backward "[[:upper:]]\\w*\\=" limit t)
	    (re-search-backward "\\<" limit t))))
    (point)))


(defconst capitalized-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'capitalized-find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in Capitalized Words mode.")

;;;###autoload
(define-minor-mode capitalized-words-mode
  "Toggle Capitalized Words mode.
With a prefix argument ARG, enable Capitalized Words mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Capitalized Words mode is a buffer-local minor mode.  When
enabled, a word boundary occurs immediately before an uppercase
letter in a symbol.  This is in addition to all the normal
boundaries given by the syntax and category tables.  There is no
restriction to ASCII.

E.g. the beginning of words in the following identifier are as marked:

  capitalizedWorDD
  ^          ^  ^^

Note that these word boundaries only apply for word motion and
marking commands such as \\[forward-word].  This mode does not affect word
boundaries found by regexp matching (`\\>', `\\w' &c).

This style of identifiers is common in environments like Java ones,
where underscores aren't trendy enough.  Capitalization rules are
sometimes part of the language, e.g. Haskell, which may thus encourage
such a style.  It is appropriate to add `capitalized-words-mode' to
the mode hook for programming language modes in which you encounter
variables like this, e.g. `java-mode-hook'.  It's unlikely to cause
trouble if such identifiers aren't used.

See also `glasses-mode' and `studlify-word'.
Obsoletes `c-forward-into-nomenclature'."
  nil " Caps" nil :group 'programming
  (set (make-local-variable 'find-word-boundary-function-table)
       capitalized-find-word-boundary-function-table))

(provide 'cap-words)

;;; cap-words.el ends here
