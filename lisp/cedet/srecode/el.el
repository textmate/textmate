;;; srecode/el.el --- Emacs Lisp specific arguments

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Emacs Lisp specific handlers.  To use these handlers in your
;; template, add the :name part to your template argument list.
;;
;; Error if not in a Emacs Lisp mode

;;; Code:

(require 'srecode)
(require 'srecode/semantic)

(declare-function semanticdb-brute-find-tags-by-class "semantic/db-find")

;;;###autoload
(defun srecode-semantic-handle-:el (dict)
  "Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  PRENAME - The common name prefix of this file."
  (let* ((names (append (semantic-find-tags-by-class 'function (current-buffer))
			(semantic-find-tags-by-class 'variable (current-buffer)))
		)
	 (common (try-completion "" names)))

    (srecode-dictionary-set-value dict "PRENAME" common)
    ))

;;;###autoload
(defun srecode-semantic-handle-:el-custom (dict)
  "Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  GROUP - The 'defgroup' name we guess you want for variables.
  FACEGROUP - The `defgroup' name you might want for faces."
  (require 'semantic/db-find)
  (let ((groups (semanticdb-strip-find-results
		 (semanticdb-brute-find-tags-by-class 'customgroup)))
	(varg nil)
	(faceg nil)
	)

    ;; Pick the best group
    (while groups
      (cond ((string-match "face" (semantic-tag-name (car groups)))
	     (setq faceg (car groups)))
	    ((not varg)
	     (setq varg (car groups)))
	    (t
	     ;; What about other groups?
	     ))
      (setq groups (cdr groups)))

    ;; Double check the facegroup.
    (setq faceg (or faceg varg))

    ;; Setup some variables
    (srecode-dictionary-set-value dict "GROUP" (semantic-tag-name varg))
    (srecode-dictionary-set-value dict "FACEGROUP" (semantic-tag-name faceg))

    ))

(define-mode-local-override srecode-semantic-apply-tag-to-dict
  emacs-lisp-mode (tagobj dict)
  "Apply Emacs Lisp specific features from TAGOBJ into DICT.
Calls `srecode-semantic-apply-tag-to-dict-default' first."
  (srecode-semantic-apply-tag-to-dict-default tagobj dict)

  ;; Pull out the tag for the individual pieces.
  (let* ((tag (oref tagobj :prime))
	 (doc (semantic-tag-docstring tag)))

    ;; It is much more common to have doc on ELisp.
    (srecode-dictionary-set-value dict "DOC" doc)

    (cond
     ;;
     ;; FUNCTION
     ;;
     ((eq (semantic-tag-class tag) 'function)
      (if (semantic-tag-get-attribute tag :user-visible-flag)
	  (srecode-dictionary-set-value dict "INTERACTIVE" "  (interactive)\n  ")
	(srecode-dictionary-set-value dict "INTERACTIVE" ""))))))


(provide 'srecode/el)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/el"
;; End:

;;; srecode/el.el ends here
