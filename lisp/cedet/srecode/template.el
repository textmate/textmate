;;; srecode/template.el --- SRecoder template language parser support.

;; Copyright (C) 2005, 2007-2012  Free Software Foundation, Inc.

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
;; Parser setup for the semantic recoder template parser.

;;; Code:
(require 'semantic)
(require 'semantic/ctxt)
(require 'semantic/wisent)
(require 'srecode/srt-wy)

(define-mode-local-override semantic-tag-components
  srecode-template-mode (tag)
  "Return sectiondictionary tags."
  (when (semantic-tag-of-class-p tag 'function)
    (let ((dicts (semantic-tag-get-attribute tag :dictionaries))
	  (ans nil))
      (while dicts
	(setq ans (append ans (cdr (car dicts))))
	(setq dicts (cdr dicts)))
      ans)
    ))

;; In semantic-imenu.el, not part of Emacs.
(defvar semantic-imenu-summary-function)

;;;###autoload
(defun srecode-template-setup-parser ()
  "Setup buffer for parse."
  (srecode-template-wy--install-parser)

  (setq
   ;; Lexical Analysis
   semantic-lex-analyzer 'wisent-srecode-template-lexer
   ;; Parsing
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character "\n"
   semantic-lex-comment-regex ";;"
   ;; Speedbar
   semantic-symbol->name-assoc-list
   '((function . "Template")
     (variable . "Variable")
     )
   ;; Navigation
   senator-step-at-tag-classes '(function variable)
   ))

(provide 'srecode/template)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/template"
;; End:

;;; srecode/template.el ends here
