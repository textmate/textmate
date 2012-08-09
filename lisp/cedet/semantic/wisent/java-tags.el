;;; semantic/wisent/java-tags.el --- Java LALR parser for Emacs

;; Copyright (C) 2001-2006, 2009-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax

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

;;; History:
;;

;;; Code:

(require 'semantic/wisent)
(require 'semantic/wisent/javat-wy)
(require 'semantic/java)

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-java-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Local context
;;;;

(define-mode-local-override semantic-get-local-variables
  java-mode ()
  "Get local values from a specific context.
Parse the current context for `field_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration
                       0 t)
                      vars))))
    vars))

;;;;
;;;; Semantic integration of the Java LALR parser
;;;;

;; In semantic/imenu.el, not part of Emacs.
(defvar semantic-imenu-summary-function)

;;;###autoload
(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser."
  (wisent-java-tags-wy--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression semantic-java-number-regexp
   semantic-lex-analyzer 'wisent-java-tags-lexer
   ;; Parsing
   semantic-tag-expand-function 'semantic-java-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Imports")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   )
  ;; Setup javadoc stuff
  (semantic-java-doc-setup))

(provide 'semantic/wisent/java-tags)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/wisent/java-tags"
;; End:

;;; semantic/wisent/java-tags.el ends here
