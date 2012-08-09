;;; semantic/wisent/javat-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2007, 2009-2012  Free Software Foundation, Inc.

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
;; This file was generated from admin/grammars/java-tags.wy.

;;; Code:

(require 'semantic/lex)

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-java-tags-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("abstract" . ABSTRACT)
     ("boolean" . BOOLEAN)
     ("break" . BREAK)
     ("byte" . BYTE)
     ("case" . CASE)
     ("catch" . CATCH)
     ("char" . CHAR)
     ("class" . CLASS)
     ("const" . CONST)
     ("continue" . CONTINUE)
     ("default" . DEFAULT)
     ("do" . DO)
     ("double" . DOUBLE)
     ("else" . ELSE)
     ("extends" . EXTENDS)
     ("final" . FINAL)
     ("finally" . FINALLY)
     ("float" . FLOAT)
     ("for" . FOR)
     ("goto" . GOTO)
     ("if" . IF)
     ("implements" . IMPLEMENTS)
     ("import" . IMPORT)
     ("instanceof" . INSTANCEOF)
     ("int" . INT)
     ("interface" . INTERFACE)
     ("long" . LONG)
     ("native" . NATIVE)
     ("new" . NEW)
     ("package" . PACKAGE)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("public" . PUBLIC)
     ("return" . RETURN)
     ("short" . SHORT)
     ("static" . STATIC)
     ("strictfp" . STRICTFP)
     ("super" . SUPER)
     ("switch" . SWITCH)
     ("synchronized" . SYNCHRONIZED)
     ("this" . THIS)
     ("throw" . THROW)
     ("throws" . THROWS)
     ("transient" . TRANSIENT)
     ("try" . TRY)
     ("void" . VOID)
     ("volatile" . VOLATILE)
     ("while" . WHILE)
     ("@author" . _AUTHOR)
     ("@version" . _VERSION)
     ("@param" . _PARAM)
     ("@return" . _RETURN)
     ("@exception" . _EXCEPTION)
     ("@throws" . _THROWS)
     ("@see" . _SEE)
     ("@since" . _SINCE)
     ("@serial" . _SERIAL)
     ("@serialData" . _SERIALDATA)
     ("@serialField" . _SERIALFIELD)
     ("@deprecated" . _DEPRECATED))
   '(("@deprecated" javadoc
      (seq 12 usage
	   (type function variable)
	   opt t))
     ("@serialField" javadoc
      (seq 11 usage
	   (variable)
	   opt t))
     ("@serialData" javadoc
      (seq 10 usage
	   (function)
	   opt t))
     ("@serial" javadoc
      (seq 9 usage
	   (variable)
	   opt t))
     ("@since" javadoc
      (seq 8 usage
	   (type function variable)
	   opt t))
     ("@see" javadoc
      (seq 7 usage
	   (type function variable)
	   opt t with-ref t))
     ("@throws" javadoc
      (seq 6 usage
	   (function)
	   with-name t))
     ("@exception" javadoc
      (seq 5 usage
	   (function)
	   with-name t))
     ("@return" javadoc
      (seq 4 usage
	   (function)))
     ("@param" javadoc
      (seq 3 usage
	   (function)
	   with-name t))
     ("@version" javadoc
      (seq 2 usage
	   (type)))
     ("@author" javadoc
      (seq 1 usage
	   (type)))
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("volatile" summary "Field declaration modifier: volatile <type> <name> ...")
     ("void" summary "Method return type: void <name> ...")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("transient" summary "Field declaration modifier: transient <type> <name> ...")
     ("throws" summary "Method|Constructor declaration: throws <classType>, ...")
     ("throw" summary "throw <expr> ;")
     ("synchronized" summary "synchronized (<expr>) ... | Method decl. modifier: synchronized <type> <name> ...")
     ("switch" summary "switch(<expr>) {[case <const-expr>: <stmts> ...] [default: <stmts>]}")
     ("strictfp" summary "Declaration modifier: strictfp {class|interface|<type>} <name> ...")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("short" summary "Integral primitive type (-32768 to 32767)")
     ("return" summary "return [<expr>] ;")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("package" summary "Package declaration: package <name>")
     ("native" summary "Method declaration modifier: native <type> <name> ...")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("interface" summary "Interface declaration: interface <name>")
     ("int" summary "Integral primitive type (-2147483648 to 2147483647)")
     ("import" summary "Import package declarations: import <package>")
     ("implements" summary "Class SuperInterfaces declaration: implements <name> [, ...]")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("goto" summary "Unused reserved word")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("finally" summary "try {<stmts>} ... finally {<stmts>}")
     ("final" summary "Class|Member declaration modifier: final {class|<type>} <name> ...")
     ("extends" summary "SuperClass|SuperInterfaces declaration: extends <name> [, ...]")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("do" summary "do <stmt> while (<expr>);")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("continue" summary "continue [<label>] ;")
     ("const" summary "Unused reserved word")
     ("class" summary "Class declaration: class <name>")
     ("char" summary "Integral primitive type ('\000' to '￿') (0 to 65535)")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("byte" summary "Integral primitive type (-128 to 127)")
     ("break" summary "break [<label>] ;")
     ("boolean" summary "Primitive logical quantity type (true or false)")
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")))
  "Table of language keywords.")

(defconst wisent-java-tags-wy--token-table
  (semantic-lex-make-type-table
   '(("unicode"
      (unicodecharacter))
     ("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER))
     ("punctuation"
      (COMP . "~")
      (OROR . "||")
      (OREQ . "|=")
      (OR . "|")
      (XOREQ . "^=")
      (XOR . "^")
      (QUESTION . "?")
      (URSHIFTEQ . ">>>=")
      (URSHIFT . ">>>")
      (RSHIFTEQ . ">>=")
      (RSHIFT . ">>")
      (GTEQ . ">=")
      (GT . ">")
      (EQEQ . "==")
      (EQ . "=")
      (LTEQ . "<=")
      (LSHIFTEQ . "<<=")
      (LSHIFT . "<<")
      (LT . "<")
      (SEMICOLON . ";")
      (COLON . ":")
      (DIVEQ . "/=")
      (DIV . "/")
      (DOT . ".")
      (MINUSEQ . "-=")
      (MINUSMINUS . "--")
      (MINUS . "-")
      (COMMA . ",")
      (PLUSEQ . "+=")
      (PLUSPLUS . "++")
      (PLUS . "+")
      (MULTEQ . "*=")
      (MULT . "*")
      (ANDEQ . "&=")
      (ANDAND . "&&")
      (AND . "&")
      (MODEQ . "%=")
      (MOD . "%")
      (NOTEQ . "!=")
      (NOT . "!"))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)")))
   '(("keyword" :declared t)
     ("unicode" syntax "\\\\u[0-9a-f][0-9a-f][0-9a-f][0-9a-f]")
     ("unicode" :declared t)
     ("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("punctuation" :declared t)
     ("block" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-java-tags-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ URSHIFT URSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP IDENTIFIER STRING_LITERAL NUMBER_LITERAL unicodecharacter ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE _AUTHOR _VERSION _PARAM _RETURN _EXCEPTION _THROWS _SEE _SINCE _SERIAL _SERIALDATA _SERIALFIELD _DEPRECATED)
       nil
       (compilation_unit
	((package_declaration))
	((import_declaration))
	((type_declaration)))
       (package_declaration
	((PACKAGE qualified_name SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-package $2 nil))))
       (import_declaration
	((IMPORT qualified_name SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-include $2 nil)))
	((IMPORT qualified_name DOT MULT SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-include
	   (concat $2 $3 $4)
	   nil))))
       (type_declaration
	((SEMICOLON)
	 nil)
	((class_declaration))
	((interface_declaration)))
       (class_declaration
	((modifiers_opt CLASS qualified_name superc_opt interfaces_opt class_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $3 $2 $6
				 (if
				     (or $4 $5)
				     (cons $4 $5))
				 :typemodifiers $1))))
       (superc_opt
	(nil)
	((EXTENDS qualified_name)
	 (identity $2)))
       (interfaces_opt
	(nil)
	((IMPLEMENTS qualified_name_list)
	 (nreverse $2)))
       (class_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'class_member_declaration 1)))
       (class_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((static_initializer)
	 nil)
	((constructor_declaration))
	((interface_declaration))
	((class_declaration))
	((method_declaration))
	((field_declaration)))
       (interface_declaration
	((modifiers_opt INTERFACE IDENTIFIER extends_interfaces_opt interface_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $3 $2 $5
				 (if $4
				     (cons nil $4))
				 :typemodifiers $1))))
       (extends_interfaces_opt
	(nil)
	((EXTENDS qualified_name_list)
	 (identity $2)))
       (interface_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'interface_member_declaration 1)))
       (interface_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((interface_declaration))
	((class_declaration))
	((method_declaration))
	((field_declaration)))
       (static_initializer
	((STATIC block)))
       (constructor_declaration
	((modifiers_opt constructor_declarator throwsc_opt constructor_body)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $2)
	   nil
	   (cdr $2)
	   :typemodifiers $1 :throws $3 :constructor-flag t))))
       (constructor_declarator
	((IDENTIFIER formal_parameter_list)
	 (cons $1 $2)))
       (constructor_body
	((block)))
       (method_declaration
	((modifiers_opt VOID method_declarator throwsc_opt method_body)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $3)
	   $2
	   (cdr $3)
	   :typemodifiers $1 :throws $4)))
	((modifiers_opt type method_declarator throwsc_opt method_body)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $3)
	   $2
	   (cdr $3)
	   :typemodifiers $1 :throws $4))))
       (method_declarator
	((IDENTIFIER formal_parameter_list dims_opt)
	 (cons
	  (concat $1 $3)
	  $2)))
       (throwsc_opt
	(nil)
	((THROWS qualified_name_list)
	 (nreverse $2)))
       (qualified_name_list
	((qualified_name_list COMMA qualified_name)
	 (cons $3 $1))
	((qualified_name)
	 (list $1)))
       (method_body
	((SEMICOLON))
	((block)))
       (block
	((BRACE_BLOCK)))
       (formal_parameter_list
	((PAREN_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'formal_parameters 1)))
       (formal_parameters
	((LPAREN)
	 nil)
	((RPAREN)
	 nil)
	((formal_parameter COMMA))
	((formal_parameter RPAREN)))
       (formal_parameter
	((formal_parameter_modifier_opt type variable_declarator_id)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $3 $2 nil :typemodifiers $1))))
       (formal_parameter_modifier_opt
	(nil)
	((FINAL)
	 (list $1)))
       (field_declaration
	((modifiers_opt type variable_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $3 $2 nil :typemodifiers $1))))
       (variable_declarators
	((variable_declarators COMMA variable_declarator)
	 (progn
	   (setcdr
	    (cdr
	     (car $1))
	    (cdr $region2))
	   (cons $3 $1)))
	((variable_declarator)
	 (list $1)))
       (variable_declarator
	((variable_declarator_id EQ variable_initializer)
	 (cons $1 $region))
	((variable_declarator_id)
	 (cons $1 $region)))
       (variable_declarator_id
	((IDENTIFIER dims_opt)
	 (concat $1 $2)))
       (variable_initializer
	((expression)))
       (expression
	((expression term))
	((term)))
       (term
	((literal))
	((operator))
	((primitive_type))
	((IDENTIFIER))
	((BRACK_BLOCK))
	((PAREN_BLOCK))
	((BRACE_BLOCK))
	((NEW))
	((CLASS))
	((THIS))
	((SUPER)))
       (literal
	((STRING_LITERAL))
	((NUMBER_LITERAL)))
       (operator
	((NOT))
	((PLUS))
	((PLUSPLUS))
	((MINUS))
	((MINUSMINUS))
	((NOTEQ))
	((MOD))
	((MODEQ))
	((AND))
	((ANDAND))
	((ANDEQ))
	((MULT))
	((MULTEQ))
	((PLUSEQ))
	((MINUSEQ))
	((DOT))
	((DIV))
	((DIVEQ))
	((COLON))
	((LT))
	((LSHIFT))
	((LSHIFTEQ))
	((LTEQ))
	((EQ))
	((EQEQ))
	((GT))
	((GTEQ))
	((RSHIFT))
	((RSHIFTEQ))
	((URSHIFT))
	((URSHIFTEQ))
	((QUESTION))
	((XOR))
	((XOREQ))
	((OR))
	((OREQ))
	((OROR))
	((COMP))
	((INSTANCEOF)))
       (primitive_type
	((BOOLEAN))
	((CHAR))
	((LONG))
	((INT))
	((SHORT))
	((BYTE))
	((DOUBLE))
	((FLOAT)))
       (modifiers_opt
	(nil)
	((modifiers)
	 (nreverse $1)))
       (modifiers
	((modifiers modifier)
	 (cons $2 $1))
	((modifier)
	 (list $1)))
       (modifier
	((STRICTFP))
	((VOLATILE))
	((TRANSIENT))
	((SYNCHRONIZED))
	((NATIVE))
	((FINAL))
	((ABSTRACT))
	((STATIC))
	((PRIVATE))
	((PROTECTED))
	((PUBLIC)))
       (type
	((qualified_name dims_opt)
	 (concat $1 $2))
	((primitive_type dims_opt)
	 (concat $1 $2)))
       (qualified_name
	((qualified_name DOT IDENTIFIER)
	 (concat $1 $2 $3))
	((IDENTIFIER)))
       (dims_opt
	(nil
	 (identity ""))
	((dims)))
       (dims
	((dims BRACK_BLOCK)
	 (concat $1 "[]"))
	((BRACK_BLOCK)
	 (identity "[]"))))
     '(compilation_unit package_declaration import_declaration class_declaration field_declaration method_declaration formal_parameter constructor_declaration interface_declaration class_member_declaration interface_member_declaration formal_parameters)))
  "Parser table.")

(defun wisent-java-tags-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-java-tags-wy--parse-table
	semantic-debug-parser-source "java-tags.wy"
	semantic-flex-keywords-obarray wisent-java-tags-wy--keyword-table
	semantic-lex-types-obarray wisent-java-tags-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers

(define-lex-block-type-analyzer wisent-java-tags-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-string-type-analyzer wisent-java-tags-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((COMP . "~")
    (OROR . "||")
    (OREQ . "|=")
    (OR . "|")
    (XOREQ . "^=")
    (XOR . "^")
    (QUESTION . "?")
    (URSHIFTEQ . ">>>=")
    (URSHIFT . ">>>")
    (RSHIFTEQ . ">>=")
    (RSHIFT . ">>")
    (GTEQ . ">=")
    (GT . ">")
    (EQEQ . "==")
    (EQ . "=")
    (LTEQ . "<=")
    (LSHIFTEQ . "<<=")
    (LSHIFT . "<<")
    (LT . "<")
    (SEMICOLON . ";")
    (COLON . ":")
    (DIVEQ . "/=")
    (DIV . "/")
    (DOT . ".")
    (MINUSEQ . "-=")
    (MINUSMINUS . "--")
    (MINUS . "-")
    (COMMA . ",")
    (PLUSEQ . "+=")
    (PLUSPLUS . "++")
    (PLUS . "+")
    (MULTEQ . "*=")
    (MULT . "*")
    (ANDEQ . "&=")
    (ANDAND . "&&")
    (AND . "&")
    (MODEQ . "%=")
    (MOD . "%")
    (NOTEQ . "!=")
    (NOT . "!"))
  'punctuation)

(define-lex-regex-type-analyzer wisent-java-tags-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'IDENTIFIER)

(define-lex-regex-type-analyzer wisent-java-tags-wy--<unicode>-regexp-analyzer
  "regexp analyzer for <unicode> tokens."
  "\\\\u[0-9a-f][0-9a-f][0-9a-f][0-9a-f]"
  nil
  'unicodecharacter)

(define-lex-regex-type-analyzer wisent-java-tags-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-sexp-type-analyzer wisent-java-tags-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING_LITERAL)

(define-lex-keyword-type-analyzer wisent-java-tags-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
;; Define the lexer for this grammar
(define-lex wisent-java-tags-lexer
  "Lexical analyzer that handles Java buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ;;;; Auto-generated analyzers.
  wisent-java-tags-wy--<number>-regexp-analyzer
  wisent-java-tags-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-java-tags-wy--<keyword>-keyword-analyzer
  wisent-java-tags-wy--<symbol>-regexp-analyzer
  wisent-java-tags-wy--<punctuation>-string-analyzer
  wisent-java-tags-wy--<block>-block-analyzer
  ;; In theory, Unicode chars should be turned into normal chars
  ;; and then combined into regular ascii keywords and text.  This
  ;; analyzer just keeps these things from making the lexer go boom.
  wisent-java-tags-wy--<unicode>-regexp-analyzer
  ;;;;
  semantic-lex-default-action)

(provide 'semantic/wisent/javat-wy)

;;; semantic/wisent/javat-wy.el ends here
