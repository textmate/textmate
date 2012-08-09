;;; semantic/bovine/make-by.el --- Generated parser support file

;; Copyright (C) 1999-2004, 2008-2012  Free Software Foundation, Inc.

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
;; This file was generated from etc/grammars/make.by.

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))


;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-make-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("if" . IF)
     ("ifdef" . IFDEF)
     ("ifndef" . IFNDEF)
     ("ifeq" . IFEQ)
     ("ifneq" . IFNEQ)
     ("else" . ELSE)
     ("endif" . ENDIF)
     ("include" . INCLUDE))
   '(("include" summary "Macro: include filename1 filename2 ...")
     ("ifneq" summary "Conditional: ifneq (expression) ... else ... endif")
     ("ifeq" summary "Conditional: ifeq (expression) ... else ... endif")
     ("ifndef" summary "Conditional: ifndef (expression) ... else ... endif")
     ("ifdef" summary "Conditional: ifdef (expression) ... else ... endif")
     ("endif" summary "Conditional: if (expression) ... else ... endif")
     ("else" summary "Conditional: if (expression) ... else ... endif")
     ("if" summary "Conditional: if (expression) ... else ... endif")))
  "Table of language keywords.")

(defconst semantic-make-by--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (BACKSLASH . "\\`[\\]\\'")
      (DOLLAR . "\\`[$]\\'")
      (EQUAL . "\\`[=]\\'")
      (PLUS . "\\`[+]\\'")
      (COLON . "\\`[:]\\'")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-make-by--parse-table
  `(
    (bovine-toplevel
     (Makefile)
     ) ;; end bovine-toplevel

    (Makefile
     (bol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (bol
      variable
      ,(semantic-lambda
	(nth 1 vals))
      )
     (bol
      rule
      ,(semantic-lambda
	(nth 1 vals))
      )
     (bol
      conditional
      ,(semantic-lambda
	(nth 1 vals))
      )
     (bol
      include
      ,(semantic-lambda
	(nth 1 vals))
      )
     (whitespace
      ,(semantic-lambda
	(list nil))
      )
     (newline
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end Makefile

    (variable
     (symbol
      opt-whitespace
      equals
      opt-whitespace
      element-list
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil
	 (nth 4 vals)))
      )
     ) ;; end variable

    (rule
     (targets
      opt-whitespace
      colons
      opt-whitespace
      element-list
      commands
      ,(semantic-lambda
	(semantic-tag-new-function
	 (nth 0 vals) nil
	 (nth 4 vals)))
      )
     ) ;; end rule

    (targets
     (target
      opt-whitespace
      targets
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))
	 (car
	  (nth 2 vals))))
      )
     (target
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))))
      )
     ) ;; end targets

    (target
     (sub-target
      target
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  (car
	   (nth 2 vals)))))
      )
     (sub-target
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))))
      )
     ) ;; end target

    (sub-target
     (symbol)
     (string)
     (varref)
     ) ;; end sub-target

    (conditional
     (IF
      some-whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFDEF
      some-whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFNDEF
      some-whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFEQ
      some-whitespace
      expression
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFNEQ
      some-whitespace
      expression
      newline
      ,(semantic-lambda
	(list nil))
      )
     (ELSE
      newline
      ,(semantic-lambda
	(list nil))
      )
     (ENDIF
      newline
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end conditional

    (expression
     (semantic-list)
     ) ;; end expression

    (include
     (INCLUDE
      some-whitespace
      element-list
      ,(semantic-lambda
	(semantic-tag-new-include
	 (nth 2 vals) nil))
      )
     ) ;; end include

    (equals
     (punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[+]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     ) ;; end equals

    (colons
     (punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[:]\\'"
      ,(semantic-lambda)
      )
     ) ;; end colons

    (element-list
     (elements
      newline
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end element-list

    (elements
     (element
      some-whitespace
      elements
      ,(semantic-lambda
	(nth 0 vals)
	(nth 2 vals))
      )
     (element
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end elements

    (element
     (sub-element
      element
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  (car
	   (nth 1 vals)))))
      )
     ( ;;EMPTY
      )
     ) ;; end element

    (sub-element
     (symbol)
     (string)
     (punctuation)
     (semantic-list
      ,(semantic-lambda
	(list
	 (buffer-substring-no-properties
	  (identity start)
	  (identity end))))
      )
     ) ;; end sub-element

    (varref
     (punctuation
      "\\`[$]\\'"
      semantic-list
      ,(semantic-lambda
	(list
	 (buffer-substring-no-properties
	  (identity start)
	  (identity end))))
      )
     ) ;; end varref

    (commands
     (bol
      shell-command
      newline
      commands
      ,(semantic-lambda
	(list
	 (nth 0 vals))
	(nth 1 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end commands

    (opt-whitespace
     (some-whitespace
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-whitespace

    (some-whitespace
     (whitespace
      some-whitespace
      ,(semantic-lambda
	(list nil))
      )
     (whitespace
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end some-whitespace
    )
  "Parser table.")

(defun semantic-make-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-make-by--parse-table
	semantic-debug-parser-source "make.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-flex-keywords-obarray semantic-make-by--keyword-table
	))

(provide 'semantic/bovine/make-by)

;;; semantic/bovine/make-by.el ends here
