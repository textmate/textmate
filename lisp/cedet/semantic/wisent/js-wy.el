;;; semantic/wisent/js-wy.el --- Generated parser support file

;; Copyright (C) 2005, 2009-2012  Free Software Foundation, Inc.
;; Copyright (C) 1998-2011 Ecma International.

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
;; This file was generated from admin/grammars/js.wy.

;; It is derived from the grammar in the ECMAScript Language
;; Specification published at
;;
;; http://www.ecma-international.org/publications/standards/Ecma-262.htm
;;
;; and redistributed under the following license:
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.
;;
;; 3. Neither the name of the authors nor Ecma International may be
;; used to endorse or promote products derived from this software
;; without specific prior written permission.  THIS SOFTWARE IS
;; PROVIDED BY THE ECMA INTERNATIONAL "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL ECMA INTERNATIONAL BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;; DAMAGE.

;;; Code:

(require 'semantic/lex)

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-javascript-jv-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("if" . IF)
     ("break" . BREAK)
     ("continue" . CONTINUE)
     ("else" . ELSE)
     ("for" . FOR)
     ("function" . FUNCTION)
     ("this" . THIS)
     ("return" . RETURN)
     ("while" . WHILE)
     ("void" . VOID_SYMBOL)
     ("new" . NEW)
     ("delete" . DELETE)
     ("var" . VAR)
     ("with" . WITH)
     ("typeof" . TYPEOF)
     ("in" . IN))
   '(("in" summary "in something")
     ("typeof" summary "typeof ")
     ("with" summary "with ")
     ("var" summary "var <variablename> [= value];")
     ("delete" summary "delete(<objectreference>) - Deletes the object.")
     ("new" summary "new <objecttype> - Creates a new object.")
     ("void" summary "Method return type: void <name> ...")
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("return" summary "return [<expr>] ;")
     ("this" summary "this")
     ("function" summary "function declaration blah blah")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("continue" summary "continue [<label>] ;")
     ("break" summary "break [<label>] ;")
     ("if" summary "if (<expr>) <stmt> [else <stmt>] (jv)")))
  "Table of language keywords.")

(defconst wisent-javascript-jv-wy--token-table
  (semantic-lex-make-type-table
   '(("<no-type>"
      (NULL_TOKEN)
      (QUERY)
      (TRUE)
      (FALSE))
     ("number"
      (NUMBER))
     ("string"
      (STRING))
     ("symbol"
      (VARIABLE))
     ("close-paren"
      (CLOSE_SQ_BRACKETS . "]")
      (END_BLOCK . "}")
      (CLOSE_PARENTHESIS . ")"))
     ("open-paren"
      (OPEN_SQ_BRACKETS . "[")
      (START_BLOCK . "{")
      (OPEN_PARENTHESIS . "("))
     ("block"
      (BRACK_BLOCK . "(OPEN_SQ_BRACKETS CLOSE_SQ_BRACKETS)")
      (BRACE_BLOCK . "(START_BLOCK END_BLOCK)")
      (PAREN_BLOCK . "(OPEN_PARENTHESIS CLOSE_PARENTHESIS)"))
     ("punctuation"
      (ONES_COMPLIMENT . "~")
      (SEMICOLON . ";")
      (LINE_TERMINATOR . "\n")
      (LESS_THAN . "<")
      (DOT . ".")
      (COMMA . ",")
      (COLON . ":")
      (DIV . "/")
      (DECREMENT . "--")
      (INCREMENT . "++")
      (PLUS_EQUALS . "+=")
      (PLUS . "+")
      (MULTIPLY_EQUALS . "*=")
      (MULTIPLY . "*")
      (MOD_EQUALS . "%=")
      (MOD . "%")
      (MINUS_EQUALS . "-=")
      (MINUS . "-")
      (LS_EQUAL . "<=")
      (LOGICAL_NOT . "!!")
      (LOGICAL_OR . "||")
      (LOGICAL_AND . "&&")
      (GT_EQUAL . ">=")
      (GREATER_THAN . ">")
      (EQUALS . "==")
      (DIV_EQUALS . "/=")
      (NOT_EQUAL . "!=")
      (BITWISE_SHIFT_RIGHT_ZERO_FILL_EQUALS . ">>>=")
      (BITWISE_SHIFT_RIGHT_ZERO_FILL . ">>>")
      (BITWISE_SHIFT_RIGHT_EQUALS . ">>=")
      (BITWISE_SHIFT_RIGHT . ">>")
      (BITWISE_SHIFT_LEFT_EQUALS . "<<=")
      (BITWISE_SHIFT_LEFT . "<<")
      (BITWISE_OR_EQUALS . "|=")
      (BITWISE_OR . "|")
      (BITWISE_EXCLUSIVE_OR_EQUALS . "^=")
      (BITWISE_EXCLUSIVE_OR . "^")
      (BITWISE_AND_EQUALS . "&=")
      (BITWISE_AND . "&")
      (ASSIGN_SYMBOL . "=")))
   '(("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("keyword" :declared t)
     ("block" :declared t)
     ("punctuation" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-javascript-jv-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((ASSIGN_SYMBOL BITWISE_AND BITWISE_AND_EQUALS BITWISE_EXCLUSIVE_OR BITWISE_EXCLUSIVE_OR_EQUALS BITWISE_OR BITWISE_OR_EQUALS BITWISE_SHIFT_LEFT BITWISE_SHIFT_LEFT_EQUALS BITWISE_SHIFT_RIGHT BITWISE_SHIFT_RIGHT_EQUALS BITWISE_SHIFT_RIGHT_ZERO_FILL BITWISE_SHIFT_RIGHT_ZERO_FILL_EQUALS NOT_EQUAL DIV_EQUALS EQUALS GREATER_THAN GT_EQUAL LOGICAL_AND LOGICAL_OR LOGICAL_NOT LS_EQUAL MINUS MINUS_EQUALS MOD MOD_EQUALS MULTIPLY MULTIPLY_EQUALS PLUS PLUS_EQUALS INCREMENT DECREMENT DIV COLON COMMA DOT LESS_THAN LINE_TERMINATOR SEMICOLON ONES_COMPLIMENT PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK OPEN_PARENTHESIS CLOSE_PARENTHESIS START_BLOCK END_BLOCK OPEN_SQ_BRACKETS CLOSE_SQ_BRACKETS IF BREAK CONTINUE ELSE FOR FUNCTION THIS RETURN WHILE VOID_SYMBOL NEW DELETE VAR WITH TYPEOF IN VARIABLE STRING NUMBER FALSE TRUE QUERY NULL_TOKEN)
       ((left PLUS MINUS)
	(left MULTIPLY DIV MOD)
	(nonassoc FALSE)
	(nonassoc HIGHER_THAN_FALSE)
	(nonassoc ELSE)
	(nonassoc LOWER_THAN_CLOSE_PARENTHESIS)
	(nonassoc CLOSE_PARENTHESIS))
       (Program
	((SourceElement)))
       (SourceElement
	((Statement))
	((FunctionDeclaration)))
       (Statement
	((Block))
	((VariableStatement))
	((EmptyStatement))
	((ExpressionStatement))
	((IfStatement))
	((IterationExpression))
	((ContinueStatement))
	((BreakStatement))
	((ReturnStatement))
	((WithStatement)))
       (FunctionDeclaration
	((FUNCTION VARIABLE FormalParameterListBlock Block)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 nil $3))))
       (FormalParameterListBlock
	((PAREN_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'FormalParameterList 1)))
       (FormalParameterList
	((OPEN_PARENTHESIS)
	 nil)
	((VARIABLE)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil)))
	((CLOSE_PARENTHESIS)
	 nil)
	((COMMA)
	 nil))
       (StatementList
	((Statement))
	((StatementList Statement)))
       (Block
	((BRACE_BLOCK)))
       (BlockExpand
	((START_BLOCK StatementList END_BLOCK))
	((START_BLOCK END_BLOCK)))
       (VariableStatement
	((VAR VariableDeclarationList SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 nil nil))))
       (VariableDeclarationList
	((VariableDeclaration)
	 (list $1))
	((VariableDeclarationList COMMA VariableDeclaration)
	 (append $1
		 (list $3))))
       (VariableDeclaration
	((VARIABLE)
	 (append
	  (list $1 nil)
	  $region))
	((VARIABLE Initializer)
	 (append
	  (cons $1 $2)
	  $region)))
       (Initializer
	((ASSIGN_SYMBOL AssignmentExpression)
	 (list $2)))
       (EmptyStatement
	((SEMICOLON)))
       (ExpressionStatement
	((Expression SEMICOLON)))
       (IfStatement
	((IF OPEN_PARENTHESIS Expression CLOSE_PARENTHESIS Statement)
	 [HIGHER_THAN_FALSE])
	((IF OPEN_PARENTHESIS Expression CLOSE_PARENTHESIS Statement ELSE Statement))
	((IF OPEN_PARENTHESIS FALSE CLOSE_PARENTHESIS Statement))
	((IF OPEN_PARENTHESIS LeftHandSideExpression AssignmentOperator AssignmentExpression CLOSE_PARENTHESIS Statement)))
       (IterationExpression
	((WHILE OPEN_PARENTHESIS Expression CLOSE_PARENTHESIS Statement)
	 [HIGHER_THAN_FALSE])
	((WHILE OPEN_PARENTHESIS FALSE CLOSE_PARENTHESIS Statement))
	((WHILE OPEN_PARENTHESIS LeftHandSideExpression AssignmentOperator AssignmentExpression CLOSE_PARENTHESIS Statement))
	((FOR OPEN_PARENTHESIS OptionalExpression SEMICOLON OptionalExpression SEMICOLON OptionalExpression CLOSE_PARENTHESIS Statement))
	((FOR OPEN_PARENTHESIS VAR VariableDeclarationList SEMICOLON OptionalExpression SEMICOLON OptionalExpression CLOSE_PARENTHESIS Statement))
	((FOR OPEN_PARENTHESIS LeftHandSideExpression IN Expression CLOSE_PARENTHESIS Statement))
	((FOR OPEN_PARENTHESIS VAR VARIABLE OptionalInitializer IN Expression CLOSE_PARENTHESIS Statement)))
       (ContinueStatement
	((CONTINUE SEMICOLON)))
       (BreakStatement
	((BREAK SEMICOLON)))
       (ReturnStatement
	((RETURN Expression SEMICOLON))
	((RETURN SEMICOLON)))
       (WithStatement
	((WITH OPEN_PARENTHESIS Expression CLOSE_PARENTHESIS Statement)))
       (OptionalInitializer
	((Initializer))
	(nil))
       (PrimaryExpression
	((THIS))
	((VARIABLE))
	((NUMBER))
	((STRING))
	((NULL_TOKEN))
	((TRUE))
	((FALSE))
	((OPEN_PARENTHESIS Expression CLOSE_PARENTHESIS)))
       (MemberExpression
	((PrimaryExpression))
	((MemberExpression OPEN_SQ_BRACKETS Expression CLOSE_SQ_BRACKETS))
	((MemberExpression DOT VARIABLE))
	((NEW MemberExpression Arguments)))
       (NewExpression
	((MemberExpression))
	((NEW NewExpression)))
       (CallExpression
	((MemberExpression Arguments))
	((CallExpression Arguments))
	((CallExpression OPEN_SQ_BRACKETS Expression CLOSE_SQ_BRACKETS))
	((CallExpression DOT VARIABLE)))
       (Arguments
	((OPEN_PARENTHESIS CLOSE_PARENTHESIS))
	((OPEN_PARENTHESIS ArgumentList CLOSE_PARENTHESIS)))
       (ArgumentList
	((AssignmentExpression))
	((ArgumentList COMMA AssignmentExpression)))
       (LeftHandSideExpression
	((NewExpression))
	((CallExpression)))
       (PostfixExpression
	((LeftHandSideExpression))
	((LeftHandSideExpression INCREMENT))
	((LeftHandSideExpression DECREMENT)))
       (UnaryExpression
	((PostfixExpression))
	((DELETE UnaryExpression))
	((VOID_SYMBOL UnaryExpression))
	((TYPEOF UnaryExpression))
	((INCREMENT UnaryExpression))
	((DECREMENT UnaryExpression))
	((PLUS UnaryExpression))
	((MINUS UnaryExpression))
	((ONES_COMPLIMENT UnaryExpression))
	((LOGICAL_NOT UnaryExpression)))
       (MultiplicativeExpression
	((UnaryExpression))
	((MultiplicativeExpression MULTIPLY UnaryExpression))
	((MultiplicativeExpression DIV UnaryExpression))
	((MultiplicativeExpression MOD UnaryExpression)))
       (AdditiveExpression
	((MultiplicativeExpression))
	((AdditiveExpression PLUS MultiplicativeExpression))
	((AdditiveExpression MINUS MultiplicativeExpression)))
       (ShiftExpression
	((AdditiveExpression))
	((ShiftExpression BITWISE_SHIFT_LEFT AdditiveExpression))
	((ShiftExpression BITWISE_SHIFT_RIGHT AdditiveExpression))
	((ShiftExpression BITWISE_SHIFT_RIGHT_ZERO_FILL AdditiveExpression)))
       (RelationalExpression
	((ShiftExpression))
	((RelationalExpression LESS_THAN ShiftExpression))
	((RelationalExpression GREATER_THAN ShiftExpression))
	((RelationalExpression LS_EQUAL ShiftExpression))
	((RelationalExpression GT_EQUAL ShiftExpression)))
       (EqualityExpression
	((RelationalExpression))
	((EqualityExpression EQUALS RelationalExpression))
	((EqualityExpression NOT_EQUAL RelationalExpression)))
       (BitwiseANDExpression
	((EqualityExpression))
	((BitwiseANDExpression BITWISE_AND EqualityExpression)))
       (BitwiseXORExpression
	((BitwiseANDExpression))
	((BitwiseXORExpression BITWISE_EXCLUSIVE_OR BitwiseANDExpression)))
       (BitwiseORExpression
	((BitwiseXORExpression))
	((BitwiseORExpression BITWISE_OR BitwiseXORExpression)))
       (LogicalANDExpression
	((BitwiseORExpression))
	((LogicalANDExpression LOGICAL_AND BitwiseORExpression)))
       (LogicalORExpression
	((LogicalANDExpression))
	((LogicalORExpression LOGICAL_OR LogicalANDExpression)))
       (ConditionalExpression
	((LogicalORExpression))
	((LogicalORExpression QUERY AssignmentExpression COLON AssignmentExpression)))
       (AssignmentExpression
	((ConditionalExpression))
	((LeftHandSideExpression AssignmentOperator AssignmentExpression)
	 [LOWER_THAN_CLOSE_PARENTHESIS]))
       (AssignmentOperator
	((ASSIGN_SYMBOL))
	((MULTIPLY_EQUALS))
	((DIV_EQUALS))
	((MOD_EQUALS))
	((PLUS_EQUALS))
	((MINUS_EQUALS))
	((BITWISE_SHIFT_LEFT_EQUALS))
	((BITWISE_SHIFT_RIGHT_EQUALS))
	((BITWISE_SHIFT_RIGHT_ZERO_FILL_EQUALS))
	((BITWISE_AND_EQUALS))
	((BITWISE_EXCLUSIVE_OR_EQUALS))
	((BITWISE_OR_EQUALS)))
       (Expression
	((AssignmentExpression))
	((Expression COMMA AssignmentExpression)))
       (OptionalExpression
	((Expression))
	(nil)))
     '(Program FormalParameterList)))
  "Parser table.")

(defun wisent-javascript-jv-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-javascript-jv-wy--parse-table
	semantic-debug-parser-source "js.wy"
	semantic-flex-keywords-obarray wisent-javascript-jv-wy--keyword-table
	semantic-lex-types-obarray wisent-javascript-jv-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers

(define-lex-string-type-analyzer wisent-javascript-jv-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((ONES_COMPLIMENT . "~")
    (SEMICOLON . ";")
    (LINE_TERMINATOR . "\n")
    (LESS_THAN . "<")
    (DOT . ".")
    (COMMA . ",")
    (COLON . ":")
    (DIV . "/")
    (DECREMENT . "--")
    (INCREMENT . "++")
    (PLUS_EQUALS . "+=")
    (PLUS . "+")
    (MULTIPLY_EQUALS . "*=")
    (MULTIPLY . "*")
    (MOD_EQUALS . "%=")
    (MOD . "%")
    (MINUS_EQUALS . "-=")
    (MINUS . "-")
    (LS_EQUAL . "<=")
    (LOGICAL_NOT . "!!")
    (LOGICAL_OR . "||")
    (LOGICAL_AND . "&&")
    (GT_EQUAL . ">=")
    (GREATER_THAN . ">")
    (EQUALS . "==")
    (DIV_EQUALS . "/=")
    (NOT_EQUAL . "!=")
    (BITWISE_SHIFT_RIGHT_ZERO_FILL_EQUALS . ">>>=")
    (BITWISE_SHIFT_RIGHT_ZERO_FILL . ">>>")
    (BITWISE_SHIFT_RIGHT_EQUALS . ">>=")
    (BITWISE_SHIFT_RIGHT . ">>")
    (BITWISE_SHIFT_LEFT_EQUALS . "<<=")
    (BITWISE_SHIFT_LEFT . "<<")
    (BITWISE_OR_EQUALS . "|=")
    (BITWISE_OR . "|")
    (BITWISE_EXCLUSIVE_OR_EQUALS . "^=")
    (BITWISE_EXCLUSIVE_OR . "^")
    (BITWISE_AND_EQUALS . "&=")
    (BITWISE_AND . "&")
    (ASSIGN_SYMBOL . "="))
  'punctuation)

(define-lex-block-type-analyzer wisent-javascript-jv-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" OPEN_PARENTHESIS PAREN_BLOCK)
     ("{" START_BLOCK BRACE_BLOCK)
     ("[" OPEN_SQ_BRACKETS BRACK_BLOCK))
    (")" CLOSE_PARENTHESIS)
    ("}" END_BLOCK)
    ("]" CLOSE_SQ_BRACKETS))
  )

(define-lex-regex-type-analyzer wisent-javascript-jv-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'VARIABLE)

(define-lex-regex-type-analyzer wisent-javascript-jv-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER)

(define-lex-sexp-type-analyzer wisent-javascript-jv-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING)

(define-lex-keyword-type-analyzer wisent-javascript-jv-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
;;here something like:
;;(define-lex wisent-java-tags-lexer
;; should go
(define-lex javascript-lexer-jv
"javascript thingy"
;;std stuff
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments

  ;;stuff generated from the wy file(one for each "type" declaration)
  wisent-javascript-jv-wy--<number>-regexp-analyzer
  wisent-javascript-jv-wy--<string>-sexp-analyzer

  wisent-javascript-jv-wy--<keyword>-keyword-analyzer

  wisent-javascript-jv-wy--<symbol>-regexp-analyzer
  wisent-javascript-jv-wy--<punctuation>-string-analyzer
  wisent-javascript-jv-wy--<block>-block-analyzer


  ;;;;more std stuff
  semantic-lex-default-action
  )

(provide 'semantic/wisent/js-wy)

;;; semantic/wisent/js-wy.el ends here
