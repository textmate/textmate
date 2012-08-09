;;; semantic/grammar-wy.el --- Generated parser support file

;; Copyright (C) 2002-2004, 2009-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
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
;; This file was generated from admin/grammars/grammar.wy.

;;; Code:

(require 'semantic/lex)
(defvar semantic-grammar-lex-c-char-re)

;; Current parsed nonterminal name.
(defvar semantic-grammar-wy--nterm nil)
;; Index of rule in a nonterminal clause.
(defvar semantic-grammar-wy--rindx nil)

;;; Declarations
;;
(defconst semantic-grammar-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("%default-prec" . DEFAULT-PREC)
     ("%no-default-prec" . NO-DEFAULT-PREC)
     ("%keyword" . KEYWORD)
     ("%languagemode" . LANGUAGEMODE)
     ("%left" . LEFT)
     ("%nonassoc" . NONASSOC)
     ("%package" . PACKAGE)
     ("%prec" . PREC)
     ("%put" . PUT)
     ("%quotemode" . QUOTEMODE)
     ("%right" . RIGHT)
     ("%scopestart" . SCOPESTART)
     ("%start" . START)
     ("%token" . TOKEN)
     ("%type" . TYPE)
     ("%use-macros" . USE-MACROS))
   'nil)
  "Table of language keywords.")

(defconst semantic-grammar-wy--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (GT . ">")
      (LT . "<")
      (OR . "|")
      (SEMI . ";")
      (COLON . ":"))
     ("close-paren"
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
     ("code"
      (EPILOGUE . "%%...EOF")
      (PROLOGUE . "%{...%}"))
     ("sexp"
      (SEXP))
     ("qlist"
      (PREFIXED_LIST))
     ("char"
      (CHARACTER))
     ("symbol"
      (PERCENT_PERCENT . "\\`%%\\'")
      (SYMBOL))
     ("string"
      (STRING)))
   '(("punctuation" :declared t)
     ("block" :declared t)
     ("sexp" matchdatatype sexp)
     ("sexp" syntax "\\=")
     ("sexp" :declared t)
     ("qlist" matchdatatype sexp)
     ("qlist" syntax "\\s'\\s-*(")
     ("qlist" :declared t)
     ("char" syntax semantic-grammar-lex-c-char-re)
     ("char" :declared t)
     ("symbol" syntax ":?\\(\\sw\\|\\s_\\)+")
     ("symbol" :declared t)
     ("string" :declared t)
     ("keyword" :declared t)))
  "Table of lexical tokens.")

(defconst semantic-grammar-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((DEFAULT-PREC NO-DEFAULT-PREC KEYWORD LANGUAGEMODE LEFT NONASSOC PACKAGE PREC PUT QUOTEMODE RIGHT SCOPESTART START TOKEN TYPE USE-MACROS STRING SYMBOL PERCENT_PERCENT CHARACTER PREFIXED_LIST SEXP PROLOGUE EPILOGUE PAREN_BLOCK BRACE_BLOCK LPAREN RPAREN LBRACE RBRACE COLON SEMI OR LT GT)
       nil
       (grammar
	((prologue))
	((epilogue))
	((declaration))
	((nonterminal))
	((PERCENT_PERCENT)))
       (prologue
	((PROLOGUE)
	 (wisent-raw-tag
	  (semantic-tag-new-code "prologue" nil))))
       (epilogue
	((EPILOGUE)
	 (wisent-raw-tag
	  (semantic-tag-new-code "epilogue" nil))))
       (declaration
	((decl)
	 (eval $1)))
       (decl
	((default_prec_decl))
	((no_default_prec_decl))
	((languagemode_decl))
	((package_decl))
	((precedence_decl))
	((put_decl))
	((quotemode_decl))
	((scopestart_decl))
	((start_decl))
	((keyword_decl))
	((token_decl))
	((type_decl))
	((use_macros_decl)))
       (default_prec_decl
	 ((DEFAULT-PREC)
	  `(wisent-raw-tag
	    (semantic-tag "default-prec" 'assoc :value
			  '("t")))))
       (no_default_prec_decl
	((NO-DEFAULT-PREC)
	 `(wisent-raw-tag
	   (semantic-tag "default-prec" 'assoc :value
			 '("nil")))))
       (languagemode_decl
	((LANGUAGEMODE symbols)
	 `(wisent-raw-tag
	   (semantic-tag ',(car $2)
			 'languagemode :rest ',(cdr $2)))))
       (package_decl
	((PACKAGE SYMBOL)
	 `(wisent-raw-tag
	   (semantic-tag-new-package ',$2 nil))))
       (precedence_decl
	((associativity token_type_opt items)
	 `(wisent-raw-tag
	   (semantic-tag ',$1 'assoc :type ',$2 :value ',$3))))
       (associativity
	((LEFT)
	 (progn "left"))
	((RIGHT)
	 (progn "right"))
	((NONASSOC)
	 (progn "nonassoc")))
       (put_decl
	((PUT put_name put_value)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'put :value ',(list $3))))
	((PUT put_name put_value_list)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'put :value ',$3)))
	((PUT put_name_list put_value)
	 `(wisent-raw-tag
	   (semantic-tag ',(car $2)
			 'put :rest ',(cdr $2)
			 :value ',(list $3))))
	((PUT put_name_list put_value_list)
	 `(wisent-raw-tag
	   (semantic-tag ',(car $2)
			 'put :rest ',(cdr $2)
			 :value ',$3))))
       (put_name_list
	((BRACE_BLOCK)
	 (mapcar 'semantic-tag-name
		 (semantic-parse-region
		  (car $region1)
		  (cdr $region1)
		  'put_names 1))))
       (put_names
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((put_name)
	 (wisent-raw-tag
	  (semantic-tag $1 'put-name))))
       (put_name
	((SYMBOL))
	((token_type)))
       (put_value_list
	((BRACE_BLOCK)
	 (mapcar 'semantic-tag-code-detail
		 (semantic-parse-region
		  (car $region1)
		  (cdr $region1)
		  'put_values 1))))
       (put_values
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((put_value)
	 (wisent-raw-tag
	  (semantic-tag-new-code "put-value" $1))))
       (put_value
	((SYMBOL any_value)
	 (cons $1 $2)))
       (scopestart_decl
	((SCOPESTART SYMBOL)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'scopestart))))
       (quotemode_decl
	((QUOTEMODE SYMBOL)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'quotemode))))
       (start_decl
	((START symbols)
	 `(wisent-raw-tag
	   (semantic-tag ',(car $2)
			 'start :rest ',(cdr $2)))))
       (keyword_decl
	((KEYWORD SYMBOL string_value)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'keyword :value ',$3))))
       (token_decl
	((TOKEN token_type_opt SYMBOL string_value)
	 `(wisent-raw-tag
	   (semantic-tag ',$3 ',(if $2 'token 'keyword)
			 :type ',$2 :value ',$4)))
	((TOKEN token_type_opt symbols)
	 `(wisent-raw-tag
	   (semantic-tag ',(car $3)
			 'token :type ',$2 :rest ',(cdr $3)))))
       (token_type_opt
	(nil)
	((token_type)))
       (token_type
	((LT SYMBOL GT)
	 (progn $2)))
       (type_decl
	((TYPE token_type plist_opt)
	 `(wisent-raw-tag
	   (semantic-tag ',$2 'type :value ',$3))))
       (plist_opt
	(nil)
	((plist)))
       (plist
	((plist put_value)
	 (append
	  (list $2)
	  $1))
	((put_value)
	 (list $1)))
       (use_name_list
	((BRACE_BLOCK)
	 (mapcar 'semantic-tag-name
		 (semantic-parse-region
		  (car $region1)
		  (cdr $region1)
		  'use_names 1))))
       (use_names
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((SYMBOL)
	 (wisent-raw-tag
	  (semantic-tag $1 'use-name))))
       (use_macros_decl
	((USE-MACROS SYMBOL use_name_list)
	 `(wisent-raw-tag
	   (semantic-tag "macro" 'macro :type ',$2 :value ',$3))))
       (string_value
	((STRING)
	 (read $1)))
       (any_value
	((SYMBOL))
	((STRING))
	((PAREN_BLOCK))
	((PREFIXED_LIST))
	((SEXP)))
       (symbols
	((lifo_symbols)
	 (nreverse $1)))
       (lifo_symbols
	((lifo_symbols SYMBOL)
	 (cons $2 $1))
	((SYMBOL)
	 (list $1)))
       (nonterminal
	((SYMBOL
	  (setq semantic-grammar-wy--nterm $1 semantic-grammar-wy--rindx 0)
	  COLON rules SEMI)
	 (wisent-raw-tag
	  (semantic-tag $1 'nonterminal :children $4))))
       (rules
	((lifo_rules)
	 (apply 'nconc
		(nreverse $1))))
       (lifo_rules
	((lifo_rules OR rule)
	 (cons $3 $1))
	((rule)
	 (list $1)))
       (rule
	((rhs)
	 (let*
	     ((nterm semantic-grammar-wy--nterm)
	      (rindx semantic-grammar-wy--rindx)
	      (rhs $1)
	      comps prec action elt)
	   (setq semantic-grammar-wy--rindx
		 (1+ semantic-grammar-wy--rindx))
	   (while rhs
	     (setq elt
		   (car rhs)
		   rhs
		   (cdr rhs))
	     (cond
	      ((vectorp elt)
	       (if prec
		   (error "Duplicate %%prec in `%s:%d' rule" nterm rindx))
	       (setq prec
		     (aref elt 0)))
	      ((consp elt)
	       (if
		   (or action comps)
		   (setq comps
			 (cons elt comps)
			 semantic-grammar-wy--rindx
			 (1+ semantic-grammar-wy--rindx))
		 (setq action
		       (car elt))))
	      (t
	       (setq comps
		     (cons elt comps)))))
	   (wisent-cook-tag
	    (wisent-raw-tag
	     (semantic-tag
	      (format "%s:%d" nterm rindx)
	      'rule :type
	      (if comps "group" "empty")
	      :value comps :prec prec :expr action))))))
       (rhs
	(nil)
	((rhs item)
	 (cons $2 $1))
	((rhs action)
	 (cons
	  (list $2)
	  $1))
	((rhs PREC item)
	 (cons
	  (vector $3)
	  $1)))
       (action
	((PAREN_BLOCK))
	((PREFIXED_LIST))
	((BRACE_BLOCK)
	 (format "(progn\n%s)"
		 (let
		     ((s $1))
		   (if
		       (string-match "^{[\n	 ]*" s)
		       (setq s
			     (substring s
					(match-end 0))))
		   (if
		       (string-match "[\n	 ]*}$" s)
		       (setq s
			     (substring s 0
					(match-beginning 0))))
		   s))))
       (items
	((lifo_items)
	 (nreverse $1)))
       (lifo_items
	((lifo_items item)
	 (cons $2 $1))
	((item)
	 (list $1)))
       (item
	((SYMBOL))
	((CHARACTER))))
     '(grammar prologue epilogue declaration nonterminal rule put_names put_values use_names)))
  "Parser table.")

(defun semantic-grammar-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table semantic-grammar-wy--parse-table
	semantic-debug-parser-source "semantic-grammar.wy"
	semantic-flex-keywords-obarray semantic-grammar-wy--keyword-table
	semantic-lex-types-obarray semantic-grammar-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers

(define-lex-sexp-type-analyzer semantic-grammar-wy--<sexp>-sexp-analyzer
  "sexp analyzer for <sexp> tokens."
  "\\="
  'SEXP)

(define-lex-sexp-type-analyzer semantic-grammar-wy--<qlist>-sexp-analyzer
  "sexp analyzer for <qlist> tokens."
  "\\s'\\s-*("
  'PREFIXED_LIST)

(define-lex-keyword-type-analyzer semantic-grammar-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")

(define-lex-block-type-analyzer semantic-grammar-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK))
    (")" RPAREN)
    ("}" RBRACE))
  )

(define-lex-regex-type-analyzer semantic-grammar-wy--<char>-regexp-analyzer
  "regexp analyzer for <char> tokens."
  semantic-grammar-lex-c-char-re
  nil
  'CHARACTER)

(define-lex-sexp-type-analyzer semantic-grammar-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING)

(define-lex-regex-type-analyzer semantic-grammar-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  ":?\\(\\sw\\|\\s_\\)+"
  '((PERCENT_PERCENT . "\\`%%\\'"))
  'SYMBOL)

(define-lex-string-type-analyzer semantic-grammar-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((GT . ">")
    (LT . "<")
    (OR . "|")
    (SEMI . ";")
    (COLON . ":"))
  'punctuation)

(provide 'semantic/grammar-wy)

;;; semantic/grammar-wy.el ends here
