;;; semantic/bovine/scm.el --- Semantic details for Scheme (guile)

;;; Copyright (C) 2001-2004, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Use the Semantic Bovinator for Scheme (guile)

(require 'semantic)
(require 'semantic/bovine/scm-by)
(require 'semantic/format)
(require 'semantic/dep)

;;; Code:

(defcustom-mode-local-semantic-dependency-system-include-path
  scheme-mode semantic-default-scheme-path
  '("/usr/share/guile/")
  "Default set of include paths for scheme (guile) code.
This should probably do some sort of search to see what is
actually on the local machine.")

(define-mode-local-override semantic-format-tag-prototype scheme-mode (tag)
  "Return a prototype for the Emacs Lisp nonterminal TAG."
  (let* ((tok (semantic-tag-class tag))
	 (args (semantic-tag-components tag))
	 )
    (if (eq tok 'function)
	(concat (semantic-tag-name tag) " ("
		(mapconcat (lambda (a) a) args " ")
		")")
      (semantic-format-tag-prototype-default tag))))

(define-mode-local-override semantic-documentation-for-tag scheme-mode (tag &optional nosnarf)
  "Return the documentation string for TAG.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-tag-docstring tag)))
    (if (and d (> (length d) 0) (= (aref d 0) ?*))
	(substring d 1)
      d)))

(define-mode-local-override semantic-insert-foreign-tag scheme-mode (tag tagfile)
  "Insert TAG from TAGFILE at point.
Attempts a simple prototype for calling or using TAG."
  (cond ((eq (semantic-tag-class tag) 'function)
	 (insert "(" (semantic-tag-name tag) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-tag-name tag)))))

;; Note: Analyzer from Henry S. Thompson
(define-lex-regex-analyzer semantic-lex-scheme-symbol
  "Detect and create symbol and keyword tokens."
  "\\(\\sw\\([:]\\|\\sw\\|\\s_\\)+\\)"
  ;; (message (format "symbol: %s" (match-string 0)))
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0)) 'symbol)
    (match-beginning 0) (match-end 0))))


(define-lex semantic-scheme-lexer
  "A simple lexical analyzer that handles simple buffers.
This lexer ignores comments and whitespace, and will return
syntax as specified by the syntax table."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-scheme-symbol
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-number
  semantic-lex-default-action)

;;;###autoload
(defun semantic-default-scheme-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  (semantic-scm-by--install-parser)
  (setq semantic-symbol->name-assoc-list '( (variable . "Variables")
                                            ;;(type     . "Types")
                                            (function . "Functions")
                                            (include  . "Loads")
                                            (package  . "DefineModule"))
        imenu-create-index-function 'semantic-create-imenu-index
        imenu-create-index-function 'semantic-create-imenu-index
        )
  (setq semantic-lex-analyzer #'semantic-scheme-lexer)
  )

(provide 'semantic/bovine/scm)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/scm"
;; End:

;;; semantic/bovine/scm.el ends here
