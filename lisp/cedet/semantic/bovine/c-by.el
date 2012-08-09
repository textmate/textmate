;;; semantic/bovine/c-by.el --- Generated parser support file

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

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
;; This file was generated from etc/grammars/c.by.

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

(declare-function semantic-c-reconstitute-token "semantic/bovine/c")
(declare-function semantic-c-reconstitute-template "semantic/bovine/c")
(declare-function semantic-expand-c-tag "semantic/bovine/c")

(defconst semantic-c-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("extern" . EXTERN)
     ("static" . STATIC)
     ("const" . CONST)
     ("volatile" . VOLATILE)
     ("register" . REGISTER)
     ("signed" . SIGNED)
     ("unsigned" . UNSIGNED)
     ("inline" . INLINE)
     ("virtual" . VIRTUAL)
     ("mutable" . MUTABLE)
     ("struct" . STRUCT)
     ("union" . UNION)
     ("enum" . ENUM)
     ("typedef" . TYPEDEF)
     ("class" . CLASS)
     ("typename" . TYPENAME)
     ("namespace" . NAMESPACE)
     ("using" . USING)
     ("new" . NEW)
     ("delete" . DELETE)
     ("template" . TEMPLATE)
     ("throw" . THROW)
     ("reentrant" . REENTRANT)
     ("try" . TRY)
     ("catch" . CATCH)
     ("operator" . OPERATOR)
     ("public" . PUBLIC)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("friend" . FRIEND)
     ("if" . IF)
     ("else" . ELSE)
     ("do" . DO)
     ("while" . WHILE)
     ("for" . FOR)
     ("switch" . SWITCH)
     ("case" . CASE)
     ("default" . DEFAULT)
     ("return" . RETURN)
     ("break" . BREAK)
     ("continue" . CONTINUE)
     ("sizeof" . SIZEOF)
     ("void" . VOID)
     ("char" . CHAR)
     ("wchar_t" . WCHAR)
     ("short" . SHORT)
     ("int" . INT)
     ("long" . LONG)
     ("float" . FLOAT)
     ("double" . DOUBLE)
     ("bool" . BOOL)
     ("_P" . UNDERP)
     ("__P" . UNDERUNDERP))
   '(("__P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ("_P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ("bool" summary "Primitive boolean type")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("int" summary "Integral Primitive Type: (-2147483648 to 2147483647)")
     ("short" summary "Integral Primitive Type: (-32768 to 32767)")
     ("wchar_t" summary "Wide Character Type")
     ("char" summary "Integral Character Type: (0 to 256)")
     ("void" summary "Built in typeless type: void")
     ("sizeof" summary "Compile time macro: sizeof(<type or variable>) // size in bytes")
     ("continue" summary "Non-local continue within a loop (for, do/while): continue;")
     ("break" summary "Non-local exit within a loop or switch (for, do/while, switch): break;")
     ("return" summary "return <value>;")
     ("default" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("case" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("switch" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("for" summary "for(<init>; <condition>; <increment>) { code }")
     ("while" summary "do { code } while (<condition>); or while (<condition>) { code };")
     ("do" summary " do { code } while (<condition>);")
     ("else" summary "if (<condition>) { code } [ else { code } ]")
     ("if" summary "if (<condition>) { code } [ else { code } ]")
     ("friend" summary "friend class <CLASSNAME>")
     ("catch" summary "try { <body> } catch { <catch code> }")
     ("try" summary "try { <body> } catch { <catch code> }")
     ("reentrant" summary "<type> <methoddef> (<method args>) reentrant ...")
     ("throw" summary "<type> <methoddef> (<method args>) throw (<exception>) ...")
     ("template" summary "template <class TYPE ...> TYPE_OR_FUNCTION")
     ("delete" summary "delete <object>;")
     ("new" summary "new <classname>();")
     ("using" summary "using <namespace>;")
     ("namespace" summary "Namespace Declaration: namespace <name> { ... };")
     ("typename" summary "typename is used to handle a qualified name as a typename;")
     ("class" summary "Class Declaration: class <name>[:parents] { ... };")
     ("typedef" summary "Arbitrary Type Declaration: typedef <typedeclaration> <name>;")
     ("enum" summary "Enumeration Type Declaration: enum [name] { ... };")
     ("union" summary "Union Type Declaration: union [name] { ... };")
     ("struct" summary "Structure Type Declaration: struct [name] { ... };")
     ("mutable" summary "Member Declaration Modifier: mutable <type> <name> ...")
     ("virtual" summary "Method Modifier: virtual <type> <name>(...) ...")
     ("inline" summary "Function Modifier: inline <return  type> <name>(...) {...};")
     ("unsigned" summary "Numeric Type Modifier: unsigned <numeric type> <name> ...")
     ("signed" summary "Numeric Type Modifier: signed <numeric type> <name> ...")
     ("register" summary "Declaration Modifier: register <type> <name> ...")
     ("volatile" summary "Declaration Modifier: volatile <type> <name> ...")
     ("const" summary "Declaration Modifier: const <type> <name> ...")
     ("static" summary "Declaration Modifier: static <type> <name> ...")
     ("extern" summary "Declaration Modifier: extern <type> <name> ...")))
  "Table of language keywords.")

(defconst semantic-c-by--token-table
  (semantic-lex-make-type-table
   '(("semantic-list"
      (BRACKETS . "\\[\\]")
      (PARENS . "()")
      (VOID_BLCK . "^(void)$")
      (BRACE_BLCK . "^{")
      (PAREN_BLCK . "^(")
      (BRACK_BLCK . "\\[.*\\]$"))
     ("close-paren"
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACE . "{")
      (LPAREN . "("))
     ("symbol"
      (RESTRICT . "\\<\\(__\\)?restrict\\>"))
     ("number"
      (ZERO . "^0$"))
     ("string"
      (CPP . "\"C\\+\\+\"")
      (C . "\"C\""))
     ("punctuation"
      (OR . "\\`[|]\\'")
      (HAT . "\\`\\^\\'")
      (MOD . "\\`[%]\\'")
      (TILDE . "\\`[~]\\'")
      (COMA . "\\`[,]\\'")
      (GREATER . "\\`[>]\\'")
      (LESS . "\\`[<]\\'")
      (EQUAL . "\\`[=]\\'")
      (BANG . "\\`[!]\\'")
      (MINUS . "\\`[-]\\'")
      (PLUS . "\\`[+]\\'")
      (DIVIDE . "\\`[/]\\'")
      (AMPERSAND . "\\`[&]\\'")
      (STAR . "\\`[*]\\'")
      (SEMICOLON . "\\`[;]\\'")
      (COLON . "\\`[:]\\'")
      (PERIOD . "\\`[.]\\'")
      (HASH . "\\`[#]\\'")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-c-by--parse-table
  `(
    (bovine-toplevel
     (declaration)
     ) ;; end bovine-toplevel

    (bovine-inner-scope
     (codeblock)
     ) ;; end bovine-inner-scope

    (declaration
     (macro)
     (type)
     (define)
     (var-or-fun)
     (extern-c)
     (template)
     (using)
     ) ;; end declaration

    (codeblock
     (define)
     (codeblock-var-or-fun)
     (type)
     (using)
     ) ;; end codeblock

    (extern-c-contents
     (open-paren
      ,(semantic-lambda
	(list nil))
      )
     (declaration)
     (close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end extern-c-contents

    (extern-c
     (EXTERN
      string
      "\"C\""
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 "C"
	 'extern :members
	 (semantic-parse-region
	  (car
	   (nth 2 vals))
	  (cdr
	   (nth 2 vals))
	  'extern-c-contents
	  1)))
      )
     (EXTERN
      string
      "\"C\\+\\+\""
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 "C"
	 'extern :members
	 (semantic-parse-region
	  (car
	   (nth 2 vals))
	  (cdr
	   (nth 2 vals))
	  'extern-c-contents
	  1)))
      )
     (EXTERN
      string
      "\"C\""
      ,(semantic-lambda
	(list nil))
      )
     (EXTERN
      string
      "\"C\\+\\+\""
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end extern-c

    (macro
     (spp-macro-def
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil nil :constant-flag t))
      )
     (spp-system-include
      ,(semantic-lambda
	(semantic-tag-new-include
	 (nth 0 vals) t))
      )
     (spp-include
      ,(semantic-lambda
	(semantic-tag-new-include
	 (nth 0 vals) nil))
      )
     ) ;; end macro

    (define
      (spp-macro-def
       ,(semantic-lambda
	 (semantic-tag-new-variable
	  (nth 0 vals) nil nil :constant-flag t))
       )
      (spp-macro-undef
       ,(semantic-lambda
	 (list nil))
       )
      ) ;; end define

    (unionparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'classsubparts
	 1))
      )
     ) ;; end unionparts

    (opt-symbol
     (symbol)
     ( ;;EMPTY
      )
     ) ;; end opt-symbol

    (classsubparts
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (class-protection
      opt-symbol
      punctuation
      "\\`[:]\\'"
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 0 vals))
	 'label))
      )
     (var-or-fun)
     (FRIEND
      func-decl
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 1 vals))
	 'friend))
      )
     (FRIEND
      CLASS
      symbol
      ,(semantic-lambda
	(semantic-tag
	 (nth 2 vals)
	 'friend))
      )
     (type)
     (define)
     (template)
     ( ;;EMPTY
      )
     ) ;; end classsubparts

    (opt-class-parents
     (punctuation
      "\\`[:]\\'"
      class-parents
      opt-template-specifier
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-class-parents

    (one-class-parent
     (opt-class-protection
      opt-class-declmods
      namespace-symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 "class" nil nil :protection
	 (car
	  (nth 0 vals))))
      )
     (opt-class-declmods
      opt-class-protection
      namespace-symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 "class" nil nil :protection
	 (car
	  (nth 1 vals))))
      )
     ) ;; end one-class-parent

    (class-parents
     (one-class-parent
      punctuation
      "\\`[,]\\'"
      class-parents
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (one-class-parent
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end class-parents

    (opt-class-declmods
     (class-declmods
      opt-class-declmods
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-class-declmods

    (class-declmods
     (VIRTUAL)
     ) ;; end class-declmods

    (class-protection
     (PUBLIC)
     (PRIVATE)
     (PROTECTED)
     ) ;; end class-protection

    (opt-class-protection
     (class-protection
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 "unspecified"))
      )
     ) ;; end opt-class-protection

    (namespaceparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'namespacesubparts
	 1))
      )
     ) ;; end namespaceparts

    (namespacesubparts
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (type)
     (var-or-fun)
     (define)
     (class-protection
      punctuation
      "\\`[:]\\'"
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 0 vals))
	 'label))
      )
     (template)
     (using)
     ( ;;EMPTY
      )
     ) ;; end namespacesubparts

    (enumparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'enumsubparts
	 1))
      )
     ) ;; end enumparts

    (enumsubparts
     (symbol
      opt-assign
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals)
	 "int"
	 (car
	  (nth 1 vals)) :constant-flag t))
      )
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\`[,]\\'"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end enumsubparts

    (opt-name
     (symbol)
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 ""))
      )
     ) ;; end opt-name

    (typesimple
     (struct-or-class
      opt-class
      opt-name
      opt-template-specifier
      opt-class-parents
      semantic-list
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (car
	  (nth 0 vals))
	 (let
	     (
	      (semantic-c-classname
	       (cons
		(car
		 (nth 2 vals))
		(car
		 (nth 0 vals)))))
	   (semantic-parse-region
	    (car
	     (nth 5 vals))
	    (cdr
	     (nth 5 vals))
	    'classsubparts
	    1))
	 (nth 4 vals) :template-specifier
	 (nth 3 vals) :parent
	 (car
	  (nth 1 vals))))
      )
     (struct-or-class
      opt-class
      opt-name
      opt-template-specifier
      opt-class-parents
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (car
	  (nth 0 vals)) nil
	 (nth 4 vals) :template-specifier
	 (nth 3 vals) :prototype t :parent
	 (car
	  (nth 1 vals))))
      )
     (UNION
      opt-class
      opt-name
      unionparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (nth 0 vals)
	 (nth 3 vals) nil :parent
	 (car
	  (nth 1 vals))))
      )
     (ENUM
      opt-class
      opt-name
      enumparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (nth 0 vals)
	 (nth 3 vals) nil :parent
	 (car
	  (nth 1 vals))))
      )
     (TYPEDEF
      declmods
      typeformbase
      cv-declmods
      typedef-symbol-list
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 4 vals)
	 (nth 0 vals) nil
	 (list
	  (nth 2 vals))))
      )
     ) ;; end typesimple

    (typedef-symbol-list
     (typedefname
      punctuation
      "\\`[,]\\'"
      typedef-symbol-list
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (typedefname
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end typedef-symbol-list

    (typedefname
     (opt-stars
      symbol
      opt-bits
      opt-array
      ,(semantic-lambda
	(list
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ) ;; end typedefname

    (struct-or-class
     (STRUCT)
     (CLASS)
     ) ;; end struct-or-class

    (type
     (typesimple
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(nth 0 vals))
      )
     (NAMESPACE
      symbol
      namespaceparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals)
	 (nth 2 vals) nil))
      )
     (NAMESPACE
      namespaceparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 "unnamed"
	 (nth 0 vals)
	 (nth 1 vals) nil))
      )
     (NAMESPACE
      symbol
      punctuation
      "\\`[=]\\'"
      typeformbase
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals)
	 (list
	  (semantic-tag-new-type
	   (car
	    (nth 3 vals))
	   (nth 0 vals) nil nil)) nil :kind
	 'alias))
      )
     ) ;; end type

    (using
     (USING
      usingname
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 1 vals))
	 'using :type
	 (nth 1 vals)))
      )
     ) ;; end using

    (usingname
     (typeformbase
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 0 vals))
	 "class" nil nil :prototype t))
      )
     (NAMESPACE
      typeformbase
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 1 vals))
	 "namespace" nil nil :prototype t))
      )
     ) ;; end usingname

    (template
     (TEMPLATE
      template-specifier
      opt-friend
      template-definition
      ,(semantic-lambda
	(semantic-c-reconstitute-template
	 (nth 3 vals)
	 (nth 1 vals)))
      )
     ) ;; end template

    (opt-friend
     (FRIEND)
     ( ;;EMPTY
      )
     ) ;; end opt-friend

    (opt-template-specifier
     (template-specifier
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-template-specifier

    (template-specifier
     (punctuation
      "\\`[<]\\'"
      template-specifier-types
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end template-specifier

    (template-specifier-types
     (template-var
      template-specifier-type-list
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      )
     ) ;; end template-specifier-types

    (template-specifier-type-list
     (punctuation
      "\\`[,]\\'"
      template-specifier-types
      ,(semantic-lambda
	(nth 1 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end template-specifier-type-list

    (template-var
     (template-type
      opt-template-equal
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))))
      )
     (string
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (number
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (opt-stars
      opt-ref
      namespace-symbol
      ,(semantic-lambda
	(nth 2 vals))
      )
     (semantic-list
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (SIZEOF
      semantic-list
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ) ;; end template-var

    (opt-template-equal
     (punctuation
      "\\`[=]\\'"
      symbol
      punctuation
      "\\`[<]\\'"
      template-specifier-types
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     (punctuation
      "\\`[=]\\'"
      symbol
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-template-equal

    (template-type
     (CLASS
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "class" nil nil))
      )
     (STRUCT
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "struct" nil nil))
      )
     (TYPENAME
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "class" nil nil))
      )
     (declmods
      typeformbase
      cv-declmods
      opt-stars
      opt-ref
      variablearg-opt-name
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 1 vals)) nil nil nil :constant-flag
	 (if
	     (member
	      "const"
	      (append
	       (nth 0 vals)
	       (nth 2 vals))) t nil) :typemodifiers
	 (delete
	  "const"
	  (append
	   (nth 0 vals)
	   (nth 2 vals))) :reference
	 (car
	  (nth 4 vals)) :pointer
	 (car
	  (nth 3 vals))))
      )
     ) ;; end template-type

    (template-definition
     (type
      ,(semantic-lambda
	(nth 0 vals))
      )
     (var-or-fun
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end template-definition

    (opt-stars
     (punctuation
      "\\`[*]\\'"
      opt-starmod
      opt-stars
      ,(semantic-lambda
	(list
	 (1+
	  (car
	   (nth 2 vals)))))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 0))
      )
     ) ;; end opt-stars

    (opt-starmod
     (STARMOD
      opt-starmod
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-starmod

    (STARMOD
     (CONST)
     ) ;; end STARMOD

    (declmods
     (DECLMOD
      declmods
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     (DECLMOD
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end declmods

    (DECLMOD
     (EXTERN)
     (STATIC)
     (CVDECLMOD)
     (INLINE)
     (REGISTER)
     (FRIEND)
     (TYPENAME)
     (METADECLMOD)
     (VIRTUAL)
     ) ;; end DECLMOD

    (metadeclmod
     (METADECLMOD
      ,(semantic-lambda)
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end metadeclmod

    (CVDECLMOD
     (CONST)
     (VOLATILE)
     ) ;; end CVDECLMOD

    (cv-declmods
     (CVDECLMOD
      cv-declmods
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     (CVDECLMOD
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end cv-declmods

    (METADECLMOD
     (VIRTUAL)
     (MUTABLE)
     ) ;; end METADECLMOD

    (opt-ref
     (punctuation
      "\\`[&]\\'"
      ,(semantic-lambda
	(list
	 1))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 0))
      )
     ) ;; end opt-ref

    (typeformbase
     (typesimple
      ,(semantic-lambda
	(nth 0 vals))
      )
     (STRUCT
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (UNION
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (ENUM
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (builtintype
      ,(semantic-lambda
	(nth 0 vals))
      )
     (symbol
      template-specifier
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 0 vals)
	 "class" nil nil :template-specifier
	 (nth 1 vals)))
      )
     (namespace-symbol-for-typeformbase
      opt-template-specifier
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 0 vals))
	 "class" nil nil :template-specifier
	 (nth 1 vals)))
      )
     (symbol
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end typeformbase

    (signedmod
     (UNSIGNED)
     (SIGNED)
     ) ;; end signedmod

    (builtintype-types
     (VOID)
     (CHAR)
     (WCHAR)
     (SHORT
      INT
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (SHORT)
     (INT)
     (LONG
      INT
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (FLOAT)
     (DOUBLE)
     (BOOL)
     (LONG
      DOUBLE
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (LONG
      LONG
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (LONG)
     ) ;; end builtintype-types

    (builtintype
     (signedmod
      builtintype-types
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  " "
	  (car
	   (nth 1 vals)))))
      )
     (builtintype-types
      ,(semantic-lambda
	(nth 0 vals))
      )
     (signedmod
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  " int")))
      )
     ) ;; end builtintype

    (codeblock-var-or-fun
     (declmods
      typeformbase
      declmods
      opt-ref
      var-or-func-decl
      ,(semantic-lambda
	(semantic-c-reconstitute-token
	 (nth 4 vals)
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ) ;; end codeblock-var-or-fun

    (var-or-fun
     (codeblock-var-or-fun
      ,(semantic-lambda
	(nth 0 vals))
      )
     (declmods
      var-or-func-decl
      ,(semantic-lambda
	(semantic-c-reconstitute-token
	 (nth 1 vals)
	 (nth 0 vals) nil))
      )
     ) ;; end var-or-fun

    (var-or-func-decl
     (func-decl
      ,(semantic-lambda
	(nth 0 vals))
      )
     (var-decl
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end var-or-func-decl

    (func-decl
     (opt-stars
      opt-class
      opt-destructor
      functionname
      opt-template-specifier
      opt-under-p
      arg-list
      opt-post-fcn-modifiers
      opt-throw
      opt-initializers
      fun-or-proto-end
      ,(semantic-lambda
	(nth 3 vals)
	(list
	 'function
	 (nth 1 vals)
	 (nth 2 vals)
	 (nth 6 vals)
	 (nth 8 vals)
	 (nth 7 vals))
	(nth 0 vals)
	(nth 10 vals)
	(list
	 (nth 4 vals))
	(nth 9 vals))
      )
     (opt-stars
      opt-class
      opt-destructor
      functionname
      opt-template-specifier
      opt-under-p
      opt-post-fcn-modifiers
      opt-throw
      opt-initializers
      fun-try-end
      ,(semantic-lambda
	(nth 3 vals)
	(list
	 'function
	 (nth 1 vals)
	 (nth 2 vals) nil
	 (nth 7 vals)
	 (nth 6 vals))
	(nth 0 vals)
	(nth 9 vals)
	(list
	 (nth 4 vals))
	(nth 8 vals))
      )
     ) ;; end func-decl

    (var-decl
     (varnamelist
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(list
	 (nth 0 vals)
	 'variable))
      )
     ) ;; end var-decl

    (opt-under-p
     (UNDERP
      ,(semantic-lambda
	(list nil))
      )
     (UNDERUNDERP
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-under-p

    (opt-initializers
     (punctuation
      "\\`[:]\\'"
      namespace-symbol
      semantic-list
      opt-initializers)
     (punctuation
      "\\`[,]\\'"
      namespace-symbol
      semantic-list
      opt-initializers)
     ( ;;EMPTY
      )
     ) ;; end opt-initializers

    (opt-post-fcn-modifiers
     (post-fcn-modifiers
      opt-post-fcn-modifiers
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-post-fcn-modifiers

    (post-fcn-modifiers
     (REENTRANT)
     (CONST)
     ) ;; end post-fcn-modifiers

    (opt-throw
     (THROW
      semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 1 vals))
	  (cdr
	   (nth 1 vals))
	  'throw-exception-list))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-throw

    (throw-exception-list
     (namespace-symbol
      punctuation
      "\\`[,]\\'"
      throw-exception-list
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 2 vals)))
      )
     (namespace-symbol
      close-paren
      ")"
      ,(semantic-lambda
	(nth 0 vals))
      )
     (symbol
      close-paren
      ")"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (open-paren
      "("
      throw-exception-list
      ,(semantic-lambda
	(nth 1 vals))
      )
     (close-paren
      ")"
      ,(semantic-lambda)
      )
     ) ;; end throw-exception-list

    (opt-bits
     (punctuation
      "\\`[:]\\'"
      number
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-bits

    (opt-array
     (semantic-list
      "\\[.*\\]$"
      opt-array
      ,(semantic-lambda
	(list
	 (cons
	  1
	  (car
	   (nth 1 vals)))))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-array

    (opt-assign
     (punctuation
      "\\`[=]\\'"
      expression
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-assign

    (opt-restrict
     (symbol
      "\\<\\(__\\)?restrict\\>")
     ( ;;EMPTY
      )
     ) ;; end opt-restrict

    (varname
     (opt-stars
      opt-restrict
      namespace-symbol
      opt-bits
      opt-array
      ,(semantic-lambda
	(nth 2 vals)
	(nth 0 vals)
	(nth 3 vals)
	(nth 4 vals))
      )
     ) ;; end varname

    (variablearg
     (declmods
      typeformbase
      cv-declmods
      opt-ref
      variablearg-opt-name
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (list
	  (nth 4 vals))
	 (nth 1 vals) nil :constant-flag
	 (if
	     (member
	      "const"
	      (append
	       (nth 0 vals)
	       (nth 2 vals))) t nil) :typemodifiers
	 (delete
	  "const"
	  (append
	   (nth 0 vals)
	   (nth 2 vals))) :reference
	 (car
	  (nth 3 vals))))
      )
     ) ;; end variablearg

    (variablearg-opt-name
     (varname
      ,(semantic-lambda
	(nth 0 vals))
      )
     (opt-stars
      ,(semantic-lambda
	(list
	 "")
	(nth 0 vals)
	(list nil nil nil))
      )
     ) ;; end variablearg-opt-name

    (varname-opt-initializer
     (semantic-list)
     (opt-assign)
     ( ;;EMPTY
      )
     ) ;; end varname-opt-initializer

    (varnamelist
     (opt-ref
      varname
      varname-opt-initializer
      punctuation
      "\\`[,]\\'"
      varnamelist
      ,(semantic-lambda
	(cons
	 (nth 1 vals)
	 (nth 4 vals)))
      )
     (opt-ref
      varname
      varname-opt-initializer
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ) ;; end varnamelist

    (namespace-symbol
     (symbol
      opt-template-specifier
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      namespace-symbol
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  "::"
	  (car
	   (nth 4 vals)))))
      )
     (symbol
      opt-template-specifier
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end namespace-symbol

    (namespace-symbol-for-typeformbase
     (symbol
      opt-template-specifier
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      namespace-symbol-for-typeformbase
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  "::"
	  (car
	   (nth 4 vals)))))
      )
     (symbol
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end namespace-symbol-for-typeformbase

    (namespace-opt-class
     (symbol
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      namespace-opt-class
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  "::"
	  (car
	   (nth 3 vals)))))
      )
     (symbol
      opt-template-specifier
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end namespace-opt-class

    (opt-class
     (namespace-opt-class
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-class

    (opt-destructor
     (punctuation
      "\\`[~]\\'"
      ,(semantic-lambda
	(list t))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-destructor

    (arg-list
     (semantic-list
      "^("
      knr-arguments
      ,(semantic-lambda
	(nth 1 vals))
      )
     (semantic-list
      "^("
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'arg-sub-list
	 1))
      )
     (semantic-list
      "^(void)$"
      ,(semantic-lambda)
      )
     ) ;; end arg-list

    (knr-varnamelist
     (varname
      punctuation
      "\\`[,]\\'"
      knr-varnamelist
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (varname
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end knr-varnamelist

    (knr-one-variable-decl
     (declmods
      typeformbase
      cv-declmods
      knr-varnamelist
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nreverse
	  (nth 3 vals))
	 (nth 1 vals) nil :constant-flag
	 (if
	     (member
	      "const"
	      (append
	       (nth 2 vals))) t nil) :typemodifiers
	 (delete
	  "const"
	  (nth 2 vals))))
      )
     ) ;; end knr-one-variable-decl

    (knr-arguments
     (knr-one-variable-decl
      punctuation
      "\\`[;]\\'"
      knr-arguments
      ,(semantic-lambda
	(append
	 (semantic-expand-c-tag
	  (nth 0 vals))
	 (nth 2 vals)))
      )
     (knr-one-variable-decl
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(semantic-expand-c-tag
	 (nth 0 vals)))
      )
     ) ;; end knr-arguments

    (arg-sub-list
     (variablearg
      ,(semantic-lambda
	(nth 0 vals))
      )
     (punctuation
      "\\`[.]\\'"
      punctuation
      "\\`[.]\\'"
      punctuation
      "\\`[.]\\'"
      close-paren
      ")"
      ,(semantic-lambda
	(semantic-tag-new-variable
	 "..."
	 "vararg" nil))
      )
     (punctuation
      "\\`[,]\\'"
      ,(semantic-lambda
	(list nil))
      )
     (open-paren
      "("
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      ")"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end arg-sub-list

    (operatorsym
     (punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "<<="))
      )
     (punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 ">>="))
      )
     (punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[<]\\'"
      ,(semantic-lambda
	(list
	 "<<"))
      )
     (punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(list
	 ">>"))
      )
     (punctuation
      "\\`[=]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "=="))
      )
     (punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "<="))
      )
     (punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 ">="))
      )
     (punctuation
      "\\`[!]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "!="))
      )
     (punctuation
      "\\`[+]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "+="))
      )
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "-="))
      )
     (punctuation
      "\\`[*]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "*="))
      )
     (punctuation
      "\\`[/]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "/="))
      )
     (punctuation
      "\\`[%]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "%="))
      )
     (punctuation
      "\\`[&]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "&="))
      )
     (punctuation
      "\\`[|]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "|="))
      )
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[*]\\'"
      ,(semantic-lambda
	(list
	 "->*"))
      )
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(list
	 "->"))
      )
     (semantic-list
      "()"
      ,(semantic-lambda
	(list
	 "()"))
      )
     (semantic-list
      "\\[\\]"
      ,(semantic-lambda
	(list
	 "[]"))
      )
     (punctuation
      "\\`[<]\\'")
     (punctuation
      "\\`[>]\\'")
     (punctuation
      "\\`[*]\\'")
     (punctuation
      "\\`[+]\\'"
      punctuation
      "\\`[+]\\'"
      ,(semantic-lambda
	(list
	 "++"))
      )
     (punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[-]\\'"
      ,(semantic-lambda
	(list
	 "--"))
      )
     (punctuation
      "\\`[-]\\'")
     (punctuation
      "\\`[&]\\'"
      punctuation
      "\\`[&]\\'"
      ,(semantic-lambda
	(list
	 "&&"))
      )
     (punctuation
      "\\`[&]\\'")
     (punctuation
      "\\`[|]\\'"
      punctuation
      "\\`[|]\\'"
      ,(semantic-lambda
	(list
	 "||"))
      )
     (punctuation
      "\\`[|]\\'")
     (punctuation
      "\\`[/]\\'")
     (punctuation
      "\\`[=]\\'")
     (punctuation
      "\\`[!]\\'")
     (punctuation
      "\\`[~]\\'")
     (punctuation
      "\\`[%]\\'")
     (punctuation
      "\\`[,]\\'")
     (punctuation
      "\\`\\^\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "^="))
      )
     (punctuation
      "\\`\\^\\'")
     ) ;; end operatorsym

    (functionname
     (OPERATOR
      operatorsym
      ,(semantic-lambda
	(nth 1 vals))
      )
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'function-pointer))
      )
     (symbol
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end functionname

    (function-pointer
     (open-paren
      "("
      punctuation
      "\\`[*]\\'"
      symbol
      close-paren
      ")"
      ,(semantic-lambda
	(list
	 (concat
	  "*"
	  (nth 2 vals))))
      )
     ) ;; end function-pointer

    (fun-or-proto-end
     (punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(list t))
      )
     (semantic-list
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\`[=]\\'"
      number
      "^0$"
      punctuation
      "\\`[;]\\'"
      ,(semantic-lambda
	(list ':pure-virtual-flag))
      )
     (fun-try-end
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end fun-or-proto-end

    (fun-try-end
     (TRY
      opt-initializers
      semantic-list
      "^{"
      fun-try-several-catches
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end fun-try-end

    (fun-try-several-catches
     (CATCH
      semantic-list
      "^("
      semantic-list
      "^{"
      fun-try-several-catches
      ,(semantic-lambda)
      )
     (CATCH
      semantic-list
      "^{"
      fun-try-several-catches
      ,(semantic-lambda)
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end fun-try-several-catches

    (type-cast
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'type-cast-list))
      )
     ) ;; end type-cast

    (type-cast-list
     (open-paren
      typeformbase
      close-paren)
     ) ;; end type-cast-list

    (opt-stuff-after-symbol
     (semantic-list
      "^(")
     (semantic-list
      "\\[.*\\]$")
     ( ;;EMPTY
      )
     ) ;; end opt-stuff-after-symbol

    (multi-stage-dereference
     (namespace-symbol
      opt-stuff-after-symbol
      punctuation
      "\\`[.]\\'"
      multi-stage-dereference)
     (namespace-symbol
      opt-stuff-after-symbol
      punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[>]\\'"
      multi-stage-dereference)
     (namespace-symbol
      opt-stuff-after-symbol)
     ) ;; end multi-stage-dereference

    (string-seq
     (string
      string-seq
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  (car
	   (nth 1 vals)))))
      )
     (string
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end string-seq

    (expr-start
     (punctuation
      "\\`[-]\\'")
     (punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[*]\\'")
     (punctuation
      "\\`[&]\\'")
     ) ;; end expr-start

    (expr-binop
     (punctuation
      "\\`[-]\\'")
     (punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[*]\\'")
     (punctuation
      "\\`[/]\\'")
     (punctuation
      "\\`[&]\\'"
      punctuation
      "\\`[&]\\'")
     (punctuation
      "\\`[&]\\'")
     (punctuation
      "\\`[|]\\'"
      punctuation
      "\\`[|]\\'")
     (punctuation
      "\\`[|]\\'")
     ) ;; end expr-binop

    (expression
     (unaryexpression
      expr-binop
      unaryexpression
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (unaryexpression
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     ) ;; end expression

    (unaryexpression
     (number)
     (multi-stage-dereference)
     (NEW
      multi-stage-dereference)
     (NEW
      builtintype-types
      semantic-list)
     (namespace-symbol)
     (string-seq)
     (type-cast
      expression)
     (semantic-list
      expression)
     (semantic-list)
     (expr-start
      expression)
     ) ;; end unaryexpression
    )
  "Parser table.")

(defun semantic-c-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-c-by--parse-table
	semantic-debug-parser-source "c.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-flex-keywords-obarray semantic-c-by--keyword-table
	semantic-equivalent-major-modes '(c-mode c++-mode)
	))

;;; Epilogue
;;

(provide 'semantic/bovine/c-by)

;;; semantic/bovine/c-by.el ends here
