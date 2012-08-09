;;; semantic/lex.el --- Lexical Analyzer builder

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

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
;; This file handles the creation of lexical analyzers for different
;; languages in Emacs Lisp.  The purpose of a lexical analyzer is to
;; convert a buffer into a list of lexical tokens.  Each token
;; contains the token class (such as 'number, 'symbol, 'IF, etc) and
;; the location in the buffer it was found.  Optionally, a token also
;; contains a string representing what is at the designated buffer
;; location.
;;
;; Tokens are pushed onto a token stream, which is basically a list of
;; all the lexical tokens from the analyzed region.  The token stream
;; is then handed to the grammar which parsers the file.
;;
;;; How it works
;;
;; Each analyzer specifies a condition and forms.  These conditions
;; and forms are assembled into a function by `define-lex' that does
;; the lexical analysis.
;;
;; In the lexical analyzer created with `define-lex', each condition
;; is tested for a given point.  When the condition is true, the forms
;; run.
;;
;; The forms can push a lexical token onto the token stream.  The
;; analyzer forms also must move the current analyzer point.  If the
;; analyzer point is moved without pushing a token, then the matched
;; syntax is effectively ignored, or skipped.
;;
;; Thus, starting at the beginning of a region to be analyzed, each
;; condition is tested.  One will match, and a lexical token might be
;; pushed, and the point is moved to the end of the lexical token
;; identified.  At the new position, the process occurs again until
;; the end of the specified region is reached.
;;
;;; How to use semantic-lex
;;
;; To create a lexer for a language, use the `define-lex' macro.
;;
;; The `define-lex' macro accepts a list of lexical analyzers.  Each
;; analyzer is created with `define-lex-analyzer', or one of the
;; derivative macros.  A single analyzer defines a regular expression
;; to match text in a buffer, and a short segment of code to create
;; one lexical token.
;;
;; Each analyzer has a NAME, DOC, a CONDITION, and possibly some
;; FORMS.  The NAME is the name used in `define-lex'.  The DOC
;; describes what the analyzer should do.
;;
;; The CONDITION evaluates the text at the current point in the
;; current buffer.  If CONDITION is true, then the FORMS will be
;; executed.
;;
;; The purpose of the FORMS is to push new lexical tokens onto the
;; list of tokens for the current buffer, and to move point after the
;; matched text.
;;
;; Some macros for creating one analyzer are:
;;
;;   define-lex-analyzer - A generic analyzer associating any style of
;;              condition to forms.
;;   define-lex-regex-analyzer - Matches a regular expression.
;;   define-lex-simple-regex-analyzer - Matches a regular expressions,
;;              and pushes the match.
;;   define-lex-block-analyzer - Matches list syntax, and defines
;;              handles open/close delimiters.
;;
;; These macros are used by the grammar compiler when lexical
;; information is specified in a grammar:
;;   define-lex- * -type-analyzer - Matches syntax specified in
;;              a grammar, and pushes one token for it.  The * would
;;              be `sexp' for things like lists or strings, and
;;              `string' for things that need to match some special
;;              string, such as "\\." where a literal match is needed.
;;
;;; Lexical Tables
;;
;; There are tables of different symbols managed in semantic-lex.el.
;; They are:
;;
;;   Lexical keyword table - A Table of symbols declared in a grammar
;;           file with the %keyword declaration.
;;           Keywords are used by `semantic-lex-symbol-or-keyword'
;;           to create lexical tokens based on the keyword.
;;
;;   Lexical type table - A table of symbols declared in a grammar
;;           file with the %type declaration.
;;           The grammar compiler uses the type table to create new
;;           lexical analyzers.  These analyzers are then used to when
;;           a new lexical analyzer is made for a language.
;;
;;; Lexical Types
;;
;; A lexical type defines a kind of lexical analyzer that will be
;; automatically generated from a grammar file based on some
;; predetermined attributes.  For now these two attributes are
;; recognized :
;;
;; * matchdatatype : define the kind of lexical analyzer.  That is :
;;
;;   - regexp : define a regexp analyzer (see
;;     `define-lex-regex-type-analyzer')
;;
;;   - string : define a string analyzer (see
;;     `define-lex-string-type-analyzer')
;;
;;   - block : define a block type analyzer (see
;;     `define-lex-block-type-analyzer')
;;
;;   - sexp : define a sexp analyzer (see
;;     `define-lex-sexp-type-analyzer')
;;
;;   - keyword : define a keyword analyzer (see
;;     `define-lex-keyword-type-analyzer')
;;
;; * syntax : define the syntax that matches a syntactic
;;   expression.  When syntax is matched the corresponding type
;;   analyzer is entered and the resulting match data will be
;;   interpreted based on the kind of analyzer (see matchdatatype
;;   above).
;;
;; The following lexical types are predefined :
;;
;; +-------------+---------------+--------------------------------+
;; | type        | matchdatatype | syntax                         |
;; +-------------+---------------+--------------------------------+
;; | punctuation | string        | "\\(\\s.\\|\\s$\\|\\s'\\)+"    |
;; | keyword     | keyword       | "\\(\\sw\\|\\s_\\)+"           |
;; | symbol      | regexp        | "\\(\\sw\\|\\s_\\)+"           |
;; | string      | sexp          | "\\s\""                        |
;; | number      | regexp        | semantic-lex-number-expression |
;; | block       | block         | "\\s(\\|\\s)"                  |
;; +-------------+---------------+--------------------------------+
;;
;; In a grammar you must use a %type expression to automatically generate
;; the corresponding analyzers of that type.
;;
;; Here is an example to auto-generate punctuation analyzers
;; with 'matchdatatype and 'syntax predefined (see table above)
;;
;; %type <punctuation> ;; will auto-generate this kind of analyzers
;;
;; It is equivalent to write :
;;
;; %type  <punctuation> syntax "\\(\\s.\\|\\s$\\|\\s'\\)+" matchdatatype string
;;
;; ;; Some punctuation based on the type defines above
;;
;; %token <punctuation> NOT         "!"
;; %token <punctuation> NOTEQ       "!="
;; %token <punctuation> MOD         "%"
;; %token <punctuation> MODEQ       "%="
;;

;;; On the Semantic 1.x lexer
;;
;; In semantic 1.x, the lexical analyzer was an all purpose routine.
;; To boost efficiency, the analyzer is now a series of routines that
;; are constructed at build time into a single routine.  This will
;; eliminate unneeded if statements to speed the lexer.

(require 'semantic/fw)

;;; Code:

;;; Semantic 2.x lexical analysis
;;
(defun semantic-lex-map-symbols (fun table &optional property)
  "Call function FUN on every symbol in TABLE.
If optional PROPERTY is non-nil, call FUN only on every symbol which
as a PROPERTY value.  FUN receives a symbol as argument."
  (if (arrayp table)
      (mapatoms
       #'(lambda (symbol)
           (if (or (null property) (get symbol property))
               (funcall fun symbol)))
       table)))

;;; Lexical keyword table handling.
;;
;; These keywords are keywords defined for using in a grammar with the
;; %keyword declaration, and are not keywords used in Emacs Lisp.

(defvar semantic-flex-keywords-obarray nil
  "Buffer local keyword obarray for the lexical analyzer.
These keywords are matched explicitly, and converted into special symbols.")
(make-variable-buffer-local 'semantic-flex-keywords-obarray)

(defmacro semantic-lex-keyword-invalid (name)
  "Signal that NAME is an invalid keyword name."
  `(signal 'wrong-type-argument '(semantic-lex-keyword-p ,name)))

(defsubst semantic-lex-keyword-symbol (name)
  "Return keyword symbol with NAME or nil if not found."
  (and (arrayp semantic-flex-keywords-obarray)
       (stringp name)
       (intern-soft name semantic-flex-keywords-obarray)))

(defsubst semantic-lex-keyword-p (name)
  "Return non-nil if a keyword with NAME exists in the keyword table.
Return nil otherwise."
  (and (setq name (semantic-lex-keyword-symbol name))
       (symbol-value name)))

(defsubst semantic-lex-keyword-set (name value)
  "Set value of keyword with NAME to VALUE and return VALUE."
  (set (intern name semantic-flex-keywords-obarray) value))

(defsubst semantic-lex-keyword-value (name)
  "Return value of keyword with NAME.
Signal an error if a keyword with NAME does not exist."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (symbol-value keyword)
      (semantic-lex-keyword-invalid name))))

(defsubst semantic-lex-keyword-put (name property value)
  "For keyword with NAME, set its PROPERTY to VALUE."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (put keyword property value)
      (semantic-lex-keyword-invalid name))))

(defsubst semantic-lex-keyword-get (name property)
  "For keyword with NAME, return its PROPERTY value."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (get keyword property)
      (semantic-lex-keyword-invalid name))))

(defun semantic-lex-make-keyword-table (specs &optional propspecs)
  "Convert keyword SPECS into an obarray and return it.
SPECS must be a list of (NAME . TOKSYM) elements, where:

  NAME is the name of the keyword symbol to define.
  TOKSYM is the lexical token symbol of that keyword.

If optional argument PROPSPECS is non nil, then interpret it, and
apply those properties.
PROPSPECS must be a list of (NAME PROPERTY VALUE) elements."
  ;; Create the symbol hash table
  (let ((semantic-flex-keywords-obarray (make-vector 13 0))
        spec)
    ;; fill it with stuff
    (while specs
      (setq spec  (car specs)
            specs (cdr specs))
      (semantic-lex-keyword-set (car spec) (cdr spec)))
    ;; Apply all properties
    (while propspecs
      (setq spec (car propspecs)
            propspecs (cdr propspecs))
      (semantic-lex-keyword-put (car spec) (nth 1 spec) (nth 2 spec)))
    semantic-flex-keywords-obarray))

(defsubst semantic-lex-map-keywords (fun &optional property)
  "Call function FUN on every lexical keyword.
If optional PROPERTY is non-nil, call FUN only on every keyword which
as a PROPERTY value.  FUN receives a lexical keyword as argument."
  (semantic-lex-map-symbols
   fun semantic-flex-keywords-obarray property))

(defun semantic-lex-keywords (&optional property)
  "Return a list of lexical keywords.
If optional PROPERTY is non-nil, return only keywords which have a
PROPERTY set."
  (let (keywords)
    (semantic-lex-map-keywords
     #'(lambda (symbol) (setq keywords (cons symbol keywords)))
     property)
    keywords))

;;; Inline functions:

(defvar semantic-lex-unterminated-syntax-end-function)
(defvar semantic-lex-analysis-bounds)
(defvar semantic-lex-end-point)

(defsubst semantic-lex-token-bounds (token)
  "Fetch the start and end locations of the lexical token TOKEN.
Return a pair (START . END)."
  (if (not (numberp (car (cdr token))))
      (cdr (cdr token))
    (cdr token)))

(defsubst semantic-lex-token-start (token)
  "Fetch the start position of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (car (semantic-lex-token-bounds token)))

(defsubst semantic-lex-token-end (token)
  "Fetch the end position of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (cdr (semantic-lex-token-bounds token)))

(defsubst semantic-lex-unterminated-syntax-detected (syntax)
  "Inside a lexical analyzer, use this when unterminated syntax was found.
Argument SYNTAX indicates the type of syntax that is unterminated.
The job of this function is to move (point) to a new logical location
so that analysis can continue, if possible."
  (goto-char
   (funcall semantic-lex-unterminated-syntax-end-function
	    syntax
	    (car semantic-lex-analysis-bounds)
	    (cdr semantic-lex-analysis-bounds)
	    ))
  (setq semantic-lex-end-point (point)))

;;; Type table handling.
;;
;; The lexical type table manages types that occur in a grammar file
;; with the %type declaration.  Types represent different syntaxes.
;; See code for `semantic-lex-preset-default-types' for the classic
;; types of syntax.
(defvar semantic-lex-types-obarray nil
  "Buffer local types obarray for the lexical analyzer.")
(make-variable-buffer-local 'semantic-lex-types-obarray)

(defmacro semantic-lex-type-invalid (type)
  "Signal that TYPE is an invalid lexical type name."
  `(signal 'wrong-type-argument '(semantic-lex-type-p ,type)))

(defsubst semantic-lex-type-symbol (type)
  "Return symbol with TYPE or nil if not found."
  (and (arrayp semantic-lex-types-obarray)
       (stringp type)
       (intern-soft type semantic-lex-types-obarray)))

(defsubst semantic-lex-type-p (type)
  "Return non-nil if a symbol with TYPE name exists."
  (and (setq type (semantic-lex-type-symbol type))
       (symbol-value type)))

(defsubst semantic-lex-type-set (type value)
  "Set value of symbol with TYPE name to VALUE and return VALUE."
  (set (intern type semantic-lex-types-obarray) value))

(defsubst semantic-lex-type-value (type &optional noerror)
  "Return value of symbol with TYPE name.
If optional argument NOERROR is non-nil return nil if a symbol with
TYPE name does not exist.  Otherwise signal an error."
  (let ((sym (semantic-lex-type-symbol type)))
    (if sym
        (symbol-value sym)
      (unless noerror
        (semantic-lex-type-invalid type)))))

(defsubst semantic-lex-type-put (type property value &optional add)
  "For symbol with TYPE name, set its PROPERTY to VALUE.
If optional argument ADD is non-nil, create a new symbol with TYPE
name if it does not already exist.  Otherwise signal an error."
  (let ((sym (semantic-lex-type-symbol type)))
    (unless sym
      (or add (semantic-lex-type-invalid type))
      (semantic-lex-type-set type nil)
      (setq sym (semantic-lex-type-symbol type)))
    (put sym property value)))

(defsubst semantic-lex-type-get (type property &optional noerror)
  "For symbol with TYPE name, return its PROPERTY value.
If optional argument NOERROR is non-nil return nil if a symbol with
TYPE name does not exist.  Otherwise signal an error."
  (let ((sym (semantic-lex-type-symbol type)))
    (if sym
        (get sym property)
      (unless noerror
        (semantic-lex-type-invalid type)))))

(defun semantic-lex-preset-default-types ()
  "Install useful default properties for well known types."
  (semantic-lex-type-put "punctuation" 'matchdatatype 'string t)
  (semantic-lex-type-put "punctuation" 'syntax "\\(\\s.\\|\\s$\\|\\s'\\)+")
  (semantic-lex-type-put "keyword" 'matchdatatype 'keyword t)
  (semantic-lex-type-put "keyword" 'syntax "\\(\\sw\\|\\s_\\)+")
  (semantic-lex-type-put "symbol"  'matchdatatype 'regexp t)
  (semantic-lex-type-put "symbol"  'syntax "\\(\\sw\\|\\s_\\)+")
  (semantic-lex-type-put "string"  'matchdatatype 'sexp t)
  (semantic-lex-type-put "string"  'syntax "\\s\"")
  (semantic-lex-type-put "number"  'matchdatatype 'regexp t)
  (semantic-lex-type-put "number"  'syntax 'semantic-lex-number-expression)
  (semantic-lex-type-put "block"   'matchdatatype 'block t)
  (semantic-lex-type-put "block"   'syntax "\\s(\\|\\s)")
  )

(defun semantic-lex-make-type-table (specs &optional propspecs)
  "Convert type SPECS into an obarray and return it.
SPECS must be a list of (TYPE . TOKENS) elements, where:

  TYPE is the name of the type symbol to define.
  TOKENS is an list of (TOKSYM . MATCHER) elements, where:

    TOKSYM is any lexical token symbol.
    MATCHER is a string or regexp a text must match to be a such
    lexical token.

If optional argument PROPSPECS is non nil, then interpret it, and
apply those properties.
PROPSPECS must be a list of (TYPE PROPERTY VALUE)."
  ;; Create the symbol hash table
  (let* ((semantic-lex-types-obarray (make-vector 13 0))
         spec type tokens token alist default)
    ;; fill it with stuff
    (while specs
      (setq spec   (car specs)
            specs  (cdr specs)
            type   (car spec)
            tokens (cdr spec)
            default nil
            alist   nil)
      (while tokens
        (setq token  (car tokens)
              tokens (cdr tokens))
        (if (cdr token)
            (setq alist (cons token alist))
          (setq token (car token))
          (if default
              (message
               "*Warning* default value of <%s> tokens changed to %S, was %S"
               type default token))
          (setq default token)))
      ;; Ensure the default matching spec is the first one.
      (semantic-lex-type-set type (cons default (nreverse alist))))
    ;; Install useful default types & properties
    (semantic-lex-preset-default-types)
    ;; Apply all properties
    (while propspecs
      (setq spec (car propspecs)
            propspecs (cdr propspecs))
      ;; Create the type if necessary.
      (semantic-lex-type-put (car spec) (nth 1 spec) (nth 2 spec) t))
    semantic-lex-types-obarray))

(defsubst semantic-lex-map-types (fun &optional property)
  "Call function FUN on every lexical type.
If optional PROPERTY is non-nil, call FUN only on every type symbol
which as a PROPERTY value.  FUN receives a type symbol as argument."
  (semantic-lex-map-symbols
   fun semantic-lex-types-obarray property))

(defun semantic-lex-types (&optional property)
  "Return a list of lexical type symbols.
If optional PROPERTY is non-nil, return only type symbols which have
PROPERTY set."
  (let (types)
    (semantic-lex-map-types
     #'(lambda (symbol) (setq types (cons symbol types)))
     property)
    types))

;;; Lexical Analyzer framework settings
;;

(defvar semantic-lex-analyzer 'semantic-flex
  "The lexical analyzer used for a given buffer.
See `semantic-lex' for documentation.
For compatibility with Semantic 1.x it defaults to `semantic-flex'.")
(make-variable-buffer-local 'semantic-lex-analyzer)

(defvar semantic-lex-tokens
  '(
    (bol)
    (charquote)
    (close-paren)
    (comment)
    (newline)
    (open-paren)
    (punctuation)
    (semantic-list)
    (string)
    (symbol)
    (whitespace)
    )
  "An alist of semantic token types.
As of December 2001 (semantic 1.4beta13), this variable is not used in
any code.  The only use is to refer to the doc-string from elsewhere.

The key to this alist is the symbol representing token type that
\\[semantic-flex] returns.  These are

  - bol:           Empty string matching a beginning of line.
                   This token is produced with
                   `semantic-lex-beginning-of-line'.

  - charquote:     String sequences that match `\\s\\+' regexp.
                   This token is produced with `semantic-lex-charquote'.

  - close-paren:   Characters that match `\\s)' regexp.
                   These are typically `)', `}', `]', etc.
                   This token is produced with
                   `semantic-lex-close-paren'.

  - comment:       A comment chunk.  These token types are not
                   produced by default.
                   This token is produced with `semantic-lex-comments'.
                   Comments are ignored with `semantic-lex-ignore-comments'.
                   Comments are treated as whitespace with
                   `semantic-lex-comments-as-whitespace'.

  - newline        Characters matching `\\s-*\\(\n\\|\\s>\\)' regexp.
                   This token is produced with `semantic-lex-newline'.

  - open-paren:    Characters that match `\\s(' regexp.
                   These are typically `(', `{', `[', etc.
                   If `semantic-lex-paren-or-list' is used,
                   then `open-paren' is not usually generated unless
                   the `depth' argument to \\[semantic-lex] is
                   greater than 0.
                   This token is always produced if the analyzer
                   `semantic-lex-open-paren' is used.

  - punctuation:   Characters matching `{\\(\\s.\\|\\s$\\|\\s'\\)'
                   regexp.
                   This token is produced with `semantic-lex-punctuation'.
                   Always specify this analyzer after the comment
                   analyzer.

  - semantic-list: String delimited by matching parenthesis, braces,
                   etc.  that the lexer skipped over, because the
                   `depth' parameter to \\[semantic-flex] was not high
                   enough.
                   This token is produced with `semantic-lex-paren-or-list'.

  - string:        Quoted strings, i.e., string sequences that start
                   and end with characters matching `\\s\"'
                   regexp.  The lexer relies on @code{forward-sexp} to
                   find the matching end.
                   This token is produced with `semantic-lex-string'.

  - symbol:        String sequences that match `\\(\\sw\\|\\s_\\)+'
                   regexp.
                   This token is produced with
                   `semantic-lex-symbol-or-keyword'.  Always add this analyzer
                   after `semantic-lex-number', or other analyzers that
                   match its regular expression.

  - whitespace:    Characters that match `\\s-+' regexp.
                   This token is produced with `semantic-lex-whitespace'.")

(defvar semantic-lex-syntax-modifications nil
  "Changes to the syntax table for this buffer.
These changes are active only while the buffer is being flexed.
This is a list where each element has the form:
  (CHAR CLASS)
CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what syntax class CHAR has.")
(make-variable-buffer-local 'semantic-lex-syntax-modifications)

(defvar semantic-lex-syntax-table nil
  "Syntax table used by lexical analysis.
See also `semantic-lex-syntax-modifications'.")
(make-variable-buffer-local 'semantic-lex-syntax-table)

(defvar semantic-lex-comment-regex nil
  "Regular expression for identifying comment start during lexical analysis.
This may be automatically set when semantic initializes in a mode, but
may need to be overridden for some special languages.")
(make-variable-buffer-local 'semantic-lex-comment-regex)

(defvar semantic-lex-number-expression
  ;; This expression was written by David Ponce for Java, and copied
  ;; here for C and any other similar language.
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Regular expression for matching a number.
If this value is nil, no number extraction is done during lex.
This expression tries to match C and Java like numbers.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")
(make-variable-buffer-local 'semantic-lex-number-expression)

(defvar semantic-lex-depth 0
  "Default lexing depth.
This specifies how many lists to create tokens in.")
(make-variable-buffer-local 'semantic-lex-depth)

(defvar semantic-lex-unterminated-syntax-end-function
  (lambda (syntax syntax-start lex-end) lex-end)
  "Function called when unterminated syntax is encountered.
This should be set to one function.  That function should take three
parameters.  The SYNTAX, or type of syntax which is unterminated.
SYNTAX-START where the broken syntax begins.
LEX-END is where the lexical analysis was asked to end.
This function can be used for languages that can intelligently fix up
broken syntax, or the exit lexical analysis via `throw' or `signal'
when finding unterminated syntax.")

;;; Interactive testing commands

(declare-function semantic-elapsed-time "semantic")

(defun semantic-lex-test (arg)
  "Test the semantic lexer in the current buffer.
If universal argument ARG, then try the whole buffer."
  (interactive "P")
  (require 'semantic)
  (let* ((start (current-time))
	 (result (semantic-lex
		  (if arg (point-min) (point))
		  (point-max)))
	 (end (current-time)))
    (message "Elapsed Time: %.2f seconds."
	     (semantic-elapsed-time start end))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))
    ))

(defvar semantic-lex-debug nil
  "When non-nil, debug the local lexical analyzer.")

(defun semantic-lex-debug (arg)
  "Debug the semantic lexer in the current buffer.
Argument ARG specifies of the analyze the whole buffer, or start at point.
While engaged, each token identified by the lexer will be highlighted
in the target buffer   A description of the current token will be
displayed in the minibuffer.  Press SPC to move to the next lexical token."
  (interactive "P")
  (require 'semantic/debug)
  (let ((semantic-lex-debug t))
    (semantic-lex-test arg)))

(defun semantic-lex-highlight-token (token)
  "Highlight the lexical TOKEN.
TOKEN is a lexical token with a START And END position.
Return the overlay."
  (let ((o (semantic-make-overlay (semantic-lex-token-start token)
				  (semantic-lex-token-end token))))
    (semantic-overlay-put o 'face 'highlight)
    o))

(defsubst semantic-lex-debug-break (token)
  "Break during lexical analysis at TOKEN."
  (when semantic-lex-debug
    (let ((o nil))
      (unwind-protect
	  (progn
	    (when token
	      (setq o (semantic-lex-highlight-token token)))
	    (semantic-read-event
	     (format "%S :: SPC - continue" token))
	    )
	(when o
	  (semantic-overlay-delete o))))))

;;; Lexical analyzer creation
;;
;; Code for creating a lex function from lists of analyzers.
;;
;; A lexical analyzer is created from a list of individual analyzers.
;; Each individual analyzer specifies a single match, and code that
;; goes with it.
;;
;; Creation of an analyzer assembles these analyzers into a new function
;; with the behaviors of all the individual analyzers.
;;
(defmacro semantic-lex-one-token (analyzers)
  "Calculate one token from the current buffer at point.
Uses locally bound variables from `define-lex'.
Argument ANALYZERS is the list of analyzers being used."
  (cons 'cond (mapcar #'symbol-value analyzers)))

(defvar semantic-lex-end-point nil
  "The end point as tracked through lexical functions.")

(defvar semantic-lex-current-depth nil
  "The current depth as tracked through lexical functions.")

(defvar semantic-lex-maximum-depth nil
  "The maximum depth of parenthesis as tracked through lexical functions.")

(defvar semantic-lex-token-stream nil
  "The current token stream we are collecting.")

(defvar semantic-lex-analysis-bounds nil
  "The bounds of the current analysis.")

(defvar semantic-lex-block-streams nil
  "Streams of tokens inside collapsed blocks.
This is an alist of (ANCHOR . STREAM) elements where ANCHOR is the
start position of the block, and STREAM is the list of tokens in that
block.")

(defvar semantic-lex-reset-hooks nil
  "Abnormal hook used by major-modes to reset lexical analyzers.
Hook functions are called with START and END values for the
current lexical pass.  Should be set with `add-hook', specifying
a LOCAL option.")

;; Stack of nested blocks.
(defvar semantic-lex-block-stack nil)
;;(defvar semantic-lex-timeout 5
;;  "*Number of sections of lexing before giving up.")

(defmacro define-lex (name doc &rest analyzers)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
ANALYZERS are small code snippets of analyzers to use when
building the new NAMED analyzer.  Only use analyzers which
are written to be used in `define-lex'.
Each analyzer should be an analyzer created with `define-lex-analyzer'.
Note: The order in which analyzers are listed is important.
If two analyzers can match the same text, it is important to order the
analyzers so that the one you want to match first occurs first.  For
example, it is good to put a number analyzer in front of a symbol
analyzer which might mistake a number for as a symbol."
  `(defun ,name  (start end &optional depth length)
     ,(concat doc "\nSee `semantic-lex' for more information.")
     ;; Make sure the state of block parsing starts over.
     (setq semantic-lex-block-streams nil)
     ;; Allow specialty reset items.
     (run-hook-with-args 'semantic-lex-reset-hooks start end)
     ;; Lexing state.
     (let* (;(starttime (current-time))
	    (starting-position (point))
            (semantic-lex-token-stream nil)
            (semantic-lex-block-stack nil)
	    (tmp-start start)
            (semantic-lex-end-point start)
            (semantic-lex-current-depth 0)
            ;; Use the default depth when not specified.
            (semantic-lex-maximum-depth
	     (or depth semantic-lex-depth))
	    ;; Bounds needed for unterminated syntax
	    (semantic-lex-analysis-bounds (cons start end))
	    ;; This entry prevents text properties from
	    ;; confusing our lexical analysis.  See Emacs 22 (CVS)
	    ;; version of C++ mode with template hack text properties.
	    (parse-sexp-lookup-properties nil)
	    )
       ;; Maybe REMOVE THIS LATER.
       ;; Trying to find incremental parser bug.
       (when (> end (point-max))
         (error ,(format "%s: end (%%d) > point-max (%%d)" name)
                end (point-max)))
       (with-syntax-table semantic-lex-syntax-table
         (goto-char start)
         (while (and (< (point) end)
                     (or (not length)
			 (<= (length semantic-lex-token-stream) length)))
           (semantic-lex-one-token ,analyzers)
	   (when (eq semantic-lex-end-point tmp-start)
	     (error ,(format "%s: endless loop at %%d, after %%S" name)
                    tmp-start (car semantic-lex-token-stream)))
	   (setq tmp-start semantic-lex-end-point)
           (goto-char semantic-lex-end-point)
	   ;;(when (> (semantic-elapsed-time starttime (current-time))
	   ;;	    semantic-lex-timeout)
	   ;;  (error "Timeout during lex at char %d" (point)))
	   (semantic-throw-on-input 'lex)
	   (semantic-lex-debug-break (car semantic-lex-token-stream))
	   ))
       ;; Check that there is no unterminated block.
       (when semantic-lex-block-stack
         (let* ((last (pop semantic-lex-block-stack))
                (blk last))
           (while blk
             (message
              ,(format "%s: `%%s' block from %%S is unterminated" name)
              (car blk) (cadr blk))
             (setq blk (pop semantic-lex-block-stack)))
           (semantic-lex-unterminated-syntax-detected (car last))))
       ;; Return to where we started.
       ;; Do not wrap in protective stuff so that if there is an error
       ;; thrown, the user knows where.
       (goto-char starting-position)
       ;; Return the token stream
       (nreverse semantic-lex-token-stream))))

;;; Collapsed block tokens delimited by any tokens.
;;
(defun semantic-lex-start-block (syntax)
  "Mark the last read token as the beginning of a SYNTAX block."
  (if (or (not semantic-lex-maximum-depth)
          (< semantic-lex-current-depth semantic-lex-maximum-depth))
      (setq semantic-lex-current-depth (1+ semantic-lex-current-depth))
    (push (list syntax (car semantic-lex-token-stream))
          semantic-lex-block-stack)))

(defun semantic-lex-end-block (syntax)
  "Process the end of a previously marked SYNTAX block.
That is, collapse the tokens inside that block, including the
beginning and end of block tokens, into a high level block token of
class SYNTAX.
The token at beginning of block is the one marked by a previous call
to `semantic-lex-start-block'.  The current token is the end of block.
The collapsed tokens are saved in `semantic-lex-block-streams'."
  (if (null semantic-lex-block-stack)
      (setq semantic-lex-current-depth (1- semantic-lex-current-depth))
    (let* ((stream semantic-lex-token-stream)
           (blk (pop semantic-lex-block-stack))
           (bstream (cdr blk))
           (first (car bstream))
           (last (pop stream)) ;; The current token mark the EOBLK
           tok)
      (if (not (eq (car blk) syntax))
          ;; SYNTAX doesn't match the syntax of the current block in
          ;; the stack. So we encountered the end of the SYNTAX block
          ;; before the end of the current one in the stack which is
          ;; signaled unterminated.
          (semantic-lex-unterminated-syntax-detected (car blk))
        ;; Move tokens found inside the block from the main stream
        ;; into a separate block stream.
        (while (and stream (not (eq (setq tok (pop stream)) first)))
          (push tok bstream))
        ;; The token marked as beginning of block was not encountered.
        ;; This should not happen!
        (or (eq tok first)
            (error "Token %S not found at beginning of block `%s'"
                   first syntax))
        ;; Save the block stream for future reuse, to avoid to redo
        ;; the lexical analysis of the block content!
        ;; Anchor the block stream with its start position, so we can
        ;; use: (cdr (assq start semantic-lex-block-streams)) to
        ;; quickly retrieve the lexical stream associated to a block.
        (setcar blk (semantic-lex-token-start first))
        (setcdr blk (nreverse bstream))
        (push blk semantic-lex-block-streams)
        ;; In the main stream, replace the tokens inside the block by
        ;; a high level block token of class SYNTAX.
        (setq semantic-lex-token-stream stream)
        (semantic-lex-push-token
         (semantic-lex-token
          syntax (car blk) (semantic-lex-token-end last)))
        ))))

;;; Lexical token API
;;
;; Functions for accessing parts of a token.  Use these functions
;; instead of accessing the list structure directly because the
;; contents of the lexical may change.
;;
(defmacro semantic-lex-token (symbol start end &optional str)
  "Create a lexical token.
SYMBOL is a symbol representing the class of syntax found.
START and END define the bounds of the token in the current buffer.
Optional STR is the string for the token only if the bounds in
the buffer do not cover the string they represent.  (As from
macro expansion.)"
  ;; This if statement checks the existence of a STR argument at
  ;; compile time, where STR is some symbol or constant.  If the
  ;; variable STr (runtime) is nil, this will make an incorrect decision.
  ;;
  ;; It is like this to maintain the original speed of the compiled
  ;; code.
  (if str
      `(cons ,symbol (cons ,str (cons ,start ,end)))
    `(cons ,symbol (cons ,start ,end))))

(defun semantic-lex-token-p (thing)
  "Return non-nil if THING is a semantic lex token.
This is an exhaustively robust check."
  (and (consp thing)
       (symbolp (car thing))
       (or (and (numberp (nth 1 thing))
		(numberp (nthcdr 2 thing)))
	   (and (stringp (nth 1 thing))
		(numberp (nth 2 thing))
		(numberp (nthcdr 3 thing)))
	   ))
  )

(defun semantic-lex-token-with-text-p (thing)
  "Return non-nil if THING is a semantic lex token.
This is an exhaustively robust check."
  (and (consp thing)
       (symbolp (car thing))
       (= (length thing) 4)
       (stringp (nth 1 thing))
       (numberp (nth 2 thing))
       (numberp (nth 3 thing)))
  )

(defun semantic-lex-token-without-text-p (thing)
  "Return non-nil if THING is a semantic lex token.
This is an exhaustively robust check."
  (and (consp thing)
       (symbolp (car thing))
       (= (length thing) 3)
       (numberp (nth 1 thing))
       (numberp (nth 2 thing)))
  )

(eval-and-compile

(defun semantic-lex-expand-block-specs (specs)
  "Expand block specifications SPECS into a Lisp form.
SPECS is a list of (BLOCK BEGIN END) elements where BLOCK, BEGIN, and
END are token class symbols that indicate to produce one collapsed
BLOCK token from tokens found between BEGIN and END ones.
BLOCK must be a non-nil symbol, and at least one of the BEGIN or END
symbols must be non-nil too.
When BEGIN is non-nil, generate a call to `semantic-lex-start-block'
when a BEGIN token class is encountered.
When END is non-nil, generate a call to `semantic-lex-end-block' when
an END token class is encountered."
  (let ((class (make-symbol "class"))
        (form nil))
    (dolist (spec specs)
      (when (car spec)
        (when (nth 1 spec)
          (push `((eq ',(nth 1 spec) ,class)
                  (semantic-lex-start-block ',(car spec)))
                form))
        (when (nth 2 spec)
          (push `((eq ',(nth 2 spec) ,class)
                  (semantic-lex-end-block ',(car spec)))
                form))))
    (when form
      `((let ((,class (semantic-lex-token-class
                       (car semantic-lex-token-stream))))
          (cond ,@(nreverse form))))
      )))
)

(defmacro semantic-lex-push-token (token &rest blockspecs)
  "Push TOKEN in the lexical analyzer token stream.
Return the lexical analysis current end point.
If optional arguments BLOCKSPECS is non-nil, it specifies to process
collapsed block tokens.  See `semantic-lex-expand-block-specs' for
more details.
This macro should only be called within the bounds of
`define-lex-analyzer'.  It changes the values of the lexical analyzer
variables `token-stream' and `semantic-lex-end-point'.  If you need to
move `semantic-lex-end-point' somewhere else, just modify this
variable after calling `semantic-lex-push-token'."
  `(progn
     (push ,token semantic-lex-token-stream)
     ,@(semantic-lex-expand-block-specs blockspecs)
     (setq semantic-lex-end-point
           (semantic-lex-token-end (car semantic-lex-token-stream)))
     ))

(defsubst semantic-lex-token-class (token)
  "Fetch the class of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (car token))

(defsubst semantic-lex-token-text (token)
  "Fetch the text associated with the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (if (stringp (car (cdr token)))
      (car (cdr token))
    (buffer-substring-no-properties
     (semantic-lex-token-start token)
     (semantic-lex-token-end   token))))

(defun semantic-lex-init ()
  "Initialize any lexical state for this buffer."
  (unless semantic-lex-comment-regex
    (setq semantic-lex-comment-regex
	  (if comment-start-skip
	      (concat "\\(\\s<\\|" comment-start-skip "\\)")
	    "\\(\\s<\\)")))
  ;; Setup the lexer syntax-table
  (setq semantic-lex-syntax-table (copy-syntax-table (syntax-table)))
  (dolist (mod semantic-lex-syntax-modifications)
    (modify-syntax-entry
     (car mod) (nth 1 mod) semantic-lex-syntax-table)))

;;;###autoload
(define-overloadable-function semantic-lex (start end &optional depth length)
  "Lexically analyze text in the current buffer between START and END.
Optional argument DEPTH indicates at what level to scan over entire
lists.  The last argument, LENGTH specifies that `semantic-lex'
should only return LENGTH tokens.  The return value is a token stream.
Each element is a list, such of the form
  (symbol start-expression .  end-expression)
where SYMBOL denotes the token type.
See `semantic-lex-tokens' variable for details on token types.  END
does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'."
  (funcall semantic-lex-analyzer start end depth length))

(defsubst semantic-lex-buffer (&optional depth)
  "Lex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-lex (point-min) (point-max) depth))

(defsubst semantic-lex-list (semlist depth)
  "Lex the body of SEMLIST to DEPTH."
  (semantic-lex (semantic-lex-token-start semlist)
                (semantic-lex-token-end   semlist)
                depth))

;;; Analyzer creation macros
;;
;; An individual analyzer is a condition and code that goes with it.
;;
;; Created analyzers become variables with the code associated with them
;; as the symbol value.  These analyzers are assembled into a lexer
;; to create new lexical analyzers.

(defcustom semantic-lex-debug-analyzers nil
  "Non nil means to debug analyzers with syntax protection.
Only in effect if `debug-on-error' is also non-nil."
  :group 'semantic
  :type 'boolean)

(defmacro semantic-lex-unterminated-syntax-protection (syntax &rest forms)
  "For SYNTAX, execute FORMS with protection for unterminated syntax.
If FORMS throws an error, treat this as a syntax problem, and
execute the unterminated syntax code.  FORMS should return a position.
Irregardless of an error, the cursor should be moved to the end of
the desired syntax, and a position returned.
If `debug-on-error' is set, errors are not caught, so that you can
debug them.
Avoid using a large FORMS since it is duplicated."
  `(if (and debug-on-error semantic-lex-debug-analyzers)
       (progn ,@forms)
     (condition-case nil
         (progn ,@forms)
       (error
        (semantic-lex-unterminated-syntax-detected ,syntax)))))
(put 'semantic-lex-unterminated-syntax-protection
     'lisp-indent-function 1)

(defmacro define-lex-analyzer (name doc condition &rest forms)
  "Create a single lexical analyzer NAME with DOC.
When an analyzer is called, the current buffer and point are
positioned in a buffer at the location to be analyzed.
CONDITION is an expression which returns t if FORMS should be run.
Within the bounds of CONDITION and FORMS, the use of backquote
can be used to evaluate expressions at compile time.
While forms are running, the following variables will be locally bound:
  `semantic-lex-analysis-bounds' - The bounds of the current analysis.
                  of the form (START . END)
  `semantic-lex-maximum-depth' - The maximum depth of semantic-list
                  for the current analysis.
  `semantic-lex-current-depth' - The current depth of `semantic-list' that has
                  been descended.
  `semantic-lex-end-point' - End Point after match.
                   Analyzers should set this to a buffer location if their
                   match string does not represent the end of the matched text.
  `semantic-lex-token-stream' - The token list being collected.
                   Add new lexical tokens to this list.
Proper action in FORMS is to move the value of `semantic-lex-end-point' to
after the location of the analyzed entry, and to add any discovered tokens
at the beginning of `semantic-lex-token-stream'.
This can be done by using `semantic-lex-push-token'."
  `(eval-and-compile
     (defvar ,name nil ,doc)
     (defun ,name nil)
     ;; Do this part separately so that re-evaluation rebuilds this code.
     (setq ,name '(,condition ,@forms))
     ;; Build a single lexical analyzer function, so the doc for
     ;; function help is automatically provided, and perhaps the
     ;; function could be useful for testing and debugging one
     ;; analyzer.
     (fset ',name (lambda () ,doc
		    (let ((semantic-lex-token-stream nil)
			  (semantic-lex-end-point (point))
			  (semantic-lex-analysis-bounds
			   (cons (point) (point-max)))
			  (semantic-lex-current-depth 0)
			  (semantic-lex-maximum-depth
			   semantic-lex-depth)
			  )
		      (when ,condition ,@forms)
		      semantic-lex-token-stream)))
     ))

(defmacro define-lex-regex-analyzer (name doc regexp &rest forms)
  "Create a lexical analyzer with NAME and DOC that will match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers."
  `(define-lex-analyzer ,name
     ,doc
     (looking-at ,regexp)
     ,@forms
     ))

(defmacro define-lex-simple-regex-analyzer (name doc regexp toksym
						 &optional index
						 &rest forms)
  "Create a lexical analyzer with NAME and DOC that match REGEXP.
TOKSYM is the symbol to use when creating a semantic lexical token.
INDEX is the index into the match that defines the bounds of the token.
Index should be a plain integer, and not specified in the macro as an
expression.
FORMS are evaluated upon a successful match BEFORE the new token is
created.  It is valid to ignore FORMS.
See `define-lex-analyzer' for more about analyzers."
  `(define-lex-analyzer ,name
     ,doc
     (looking-at ,regexp)
     ,@forms
     (semantic-lex-push-token
      (semantic-lex-token ,toksym
			  (match-beginning ,(or index 0))
			  (match-end ,(or index 0))))
     ))

(defmacro define-lex-block-analyzer (name doc spec1 &rest specs)
  "Create a lexical analyzer NAME for paired delimiters blocks.
It detects a paired delimiters block or the corresponding open or
close delimiter depending on the value of the variable
`semantic-lex-current-depth'.  DOC is the documentation string of the lexical
analyzer.  SPEC1 and SPECS specify the token symbols and open, close
delimiters used.  Each SPEC has the form:

\(BLOCK-SYM (OPEN-DELIM OPEN-SYM) (CLOSE-DELIM CLOSE-SYM))

where BLOCK-SYM is the symbol returned in a block token.  OPEN-DELIM
and CLOSE-DELIM are respectively the open and close delimiters
identifying a block.  OPEN-SYM and CLOSE-SYM are respectively the
symbols returned in open and close tokens."
  (let ((specs (cons spec1 specs))
        spec open olist clist)
    (while specs
      (setq spec  (car specs)
            specs (cdr specs)
            open  (nth 1 spec)
            ;; build alist ((OPEN-DELIM OPEN-SYM BLOCK-SYM) ...)
            olist (cons (list (car open) (cadr open) (car spec)) olist)
            ;; build alist ((CLOSE-DELIM CLOSE-SYM) ...)
            clist (cons (nth 2 spec) clist)))
    `(define-lex-analyzer ,name
       ,doc
       (and
        (looking-at "\\(\\s(\\|\\s)\\)")
        (let ((text (match-string 0)) match)
          (cond
           ((setq match (assoc text ',olist))
            (if (or (not semantic-lex-maximum-depth)
		    (< semantic-lex-current-depth semantic-lex-maximum-depth))
                (progn
                  (setq semantic-lex-current-depth (1+ semantic-lex-current-depth))
		  (semantic-lex-push-token
		   (semantic-lex-token
		    (nth 1 match)
		    (match-beginning 0) (match-end 0))))
	      (semantic-lex-push-token
	       (semantic-lex-token
		(nth 2 match)
		(match-beginning 0)
		(save-excursion
		  (semantic-lex-unterminated-syntax-protection (nth 2 match)
		    (forward-list 1)
		    (point)))
		))
	      ))
           ((setq match (assoc text ',clist))
            (setq semantic-lex-current-depth (1- semantic-lex-current-depth))
	    (semantic-lex-push-token
	     (semantic-lex-token
	      (nth 1 match)
	      (match-beginning 0) (match-end 0)))))))
       )))

;;; Analyzers
;;
;; Pre-defined common analyzers.
;;
(define-lex-analyzer semantic-lex-default-action
  "The default action when no other lexical actions match text.
This action will just throw an error."
  t
  (error "Unmatched Text during Lexical Analysis"))

(define-lex-analyzer semantic-lex-beginning-of-line
  "Detect and create a beginning of line token (BOL)."
  (and (bolp)
       ;; Just insert a (bol N . N) token in the token stream,
       ;; without moving the point.  N is the point at the
       ;; beginning of line.
       (semantic-lex-push-token (semantic-lex-token 'bol (point) (point)))
       nil) ;; CONTINUE
  ;; We identify and add the BOL token onto the stream, but since
  ;; semantic-lex-end-point doesn't move, we always fail CONDITION, and have no
  ;; FORMS body.
  nil)

(define-lex-simple-regex-analyzer semantic-lex-newline
  "Detect and create newline tokens."
  "\\s-*\\(\n\\|\\s>\\)"  'newline 1)

(define-lex-regex-analyzer semantic-lex-newline-as-whitespace
  "Detect and create newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters) AND when you want whitespace tokens."
  "\\s-*\\(\n\\|\\s>\\)"
  ;; Language wants whitespaces.  Create a token for it.
  (if (eq (semantic-lex-token-class (car semantic-lex-token-stream))
	  'whitespace)
      ;; Merge whitespace tokens together if they are adjacent.  Two
      ;; whitespace tokens may be separated by a comment which is not in
      ;; the token stream.
      (setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream))
              (match-end 0))
    (semantic-lex-push-token
     (semantic-lex-token
      'whitespace (match-beginning 0) (match-end 0)))))

(define-lex-regex-analyzer semantic-lex-ignore-newline
  "Detect and ignore newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters)."
  "\\s-*\\(\n\\|\\s>\\)"
  (setq semantic-lex-end-point (match-end 0)))

(define-lex-regex-analyzer semantic-lex-whitespace
  "Detect and create whitespace tokens."
  ;; catch whitespace when needed
  "\\s-+"
  ;; Language wants whitespaces.  Create a token for it.
  (if (eq (semantic-lex-token-class (car semantic-lex-token-stream))
	  'whitespace)
      ;; Merge whitespace tokens together if they are adjacent.  Two
      ;; whitespace tokens may be separated by a comment which is not in
      ;; the token stream.
      (progn
        (setq semantic-lex-end-point (match-end 0))
        (setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream))
                semantic-lex-end-point))
    (semantic-lex-push-token
     (semantic-lex-token
      'whitespace (match-beginning 0) (match-end 0)))))

(define-lex-regex-analyzer semantic-lex-ignore-whitespace
  "Detect and skip over whitespace tokens."
  ;; catch whitespace when needed
  "\\s-+"
  ;; Skip over the detected whitespace, do not create a token for it.
  (setq semantic-lex-end-point (match-end 0)))

(define-lex-simple-regex-analyzer semantic-lex-number
  "Detect and create number tokens.
See `semantic-lex-number-expression' for details on matching numbers,
and number formats."
  semantic-lex-number-expression 'number)

(define-lex-regex-analyzer semantic-lex-symbol-or-keyword
  "Detect and create symbol and keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0)) 'symbol)
    (match-beginning 0) (match-end 0))))

(define-lex-simple-regex-analyzer semantic-lex-charquote
  "Detect and create charquote tokens."
  ;; Character quoting characters (ie, \n as newline)
  "\\s\\+" 'charquote)

(define-lex-simple-regex-analyzer semantic-lex-punctuation
  "Detect and create punctuation tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)" 'punctuation)

(define-lex-analyzer semantic-lex-punctuation-type
  "Detect and create a punctuation type token.
Recognized punctuation is defined in the current table of lexical
types, as the value of the `punctuation' token type."
  (and (looking-at "\\(\\s.\\|\\s$\\|\\s'\\)+")
       (let* ((key (match-string 0))
              (pos (match-beginning 0))
              (end (match-end 0))
              (len (- end pos))
              (lst (semantic-lex-type-value "punctuation" t))
              (def (car lst)) ;; default lexical symbol or nil
              (lst (cdr lst)) ;; alist of (LEX-SYM . PUNCT-STRING)
              (elt nil))
         (if lst
             ;; Starting with the longest one, search if the
             ;; punctuation string is defined for this language.
             (while (and (> len 0) (not (setq elt (rassoc key lst))))
               (setq len (1- len)
                     key (substring key 0 len))))
         (if elt ;; Return the punctuation token found
             (semantic-lex-push-token
	      (semantic-lex-token (car elt) pos (+ pos len)))
           (if def ;; Return a default generic token
               (semantic-lex-push-token
		(semantic-lex-token def pos end))
             ;; Nothing match
             )))))

(define-lex-regex-analyzer semantic-lex-paren-or-list
  "Detect open parenthesis.
Return either a paren token or a semantic list token depending on
`semantic-lex-current-depth'."
  "\\s("
  (if (or (not semantic-lex-maximum-depth)
	  (< semantic-lex-current-depth semantic-lex-maximum-depth))
      (progn
	(setq semantic-lex-current-depth (1+ semantic-lex-current-depth))
	(semantic-lex-push-token
	 (semantic-lex-token
	  'open-paren (match-beginning 0) (match-end 0))))
    (semantic-lex-push-token
     (semantic-lex-token
      'semantic-list (match-beginning 0)
      (save-excursion
	(semantic-lex-unterminated-syntax-protection 'semantic-list
	  (forward-list 1)
	  (point))
	)))
    ))

(define-lex-simple-regex-analyzer semantic-lex-open-paren
  "Detect and create an open parenthesis token."
  "\\s(" 'open-paren 0  (setq semantic-lex-current-depth (1+ semantic-lex-current-depth)))

(define-lex-simple-regex-analyzer semantic-lex-close-paren
  "Detect and create a close parenthesis token."
  "\\s)" 'close-paren 0 (setq semantic-lex-current-depth (1- semantic-lex-current-depth)))

(define-lex-regex-analyzer semantic-lex-string
  "Detect and create a string token."
  "\\s\""
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'string (point)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'string
	(forward-sexp 1)
	(point))
      ))))

(define-lex-regex-analyzer semantic-lex-comments
  "Detect and create a comment token."
  semantic-lex-comment-regex
  (save-excursion
    (forward-comment 1)
    ;; Generate newline token if enabled
    (if (bolp) (backward-char 1))
    (setq semantic-lex-end-point (point))
    ;; Language wants comments or want them as whitespaces,
    ;; link them together.
    (if (eq (semantic-lex-token-class (car semantic-lex-token-stream)) 'comment)
	(setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream))
		semantic-lex-end-point)
      (semantic-lex-push-token
       (semantic-lex-token
	'comment (match-beginning 0) semantic-lex-end-point)))))

(define-lex-regex-analyzer semantic-lex-comments-as-whitespace
  "Detect comments and create a whitespace token."
  semantic-lex-comment-regex
  (save-excursion
    (forward-comment 1)
    ;; Generate newline token if enabled
    (if (bolp) (backward-char 1))
    (setq semantic-lex-end-point (point))
    ;; Language wants comments or want them as whitespaces,
    ;; link them together.
    (if (eq (semantic-lex-token-class (car semantic-lex-token-stream)) 'whitespace)
	(setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream))
		semantic-lex-end-point)
      (semantic-lex-push-token
       (semantic-lex-token
	'whitespace (match-beginning 0) semantic-lex-end-point)))))

(define-lex-regex-analyzer semantic-lex-ignore-comments
  "Detect and create a comment token."
  semantic-lex-comment-regex
  (let ((comment-start-point (point)))
    (forward-comment 1)
    (if (eq (point) comment-start-point)
	;; In this case our start-skip string failed
	;; to work properly.  Lets try and move over
	;; whatever white space we matched to begin
	;; with.
	(skip-syntax-forward "-.'" (point-at-eol))
      ;; We may need to back up so newlines or whitespace is generated.
      (if (bolp)
	  (backward-char 1)))
    (if (eq (point) comment-start-point)
	(error "Strange comment syntax prevents lexical analysis"))
    (setq semantic-lex-end-point (point))))

;;; Comment lexer
;;
;; Predefined lexers that could be used instead of creating new
;; analyzers.

(define-lex semantic-comment-lexer
  "A simple lexical analyzer that handles comments.
This lexer will only return comment tokens.  It is the default lexer
used by `semantic-find-doc-snarf-comment' to snarf up the comment at
point."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-comments
  semantic-lex-default-action)

;;; Test Lexer
;;
(define-lex semantic-simple-lexer
  "A simple lexical analyzer that handles simple buffers.
This lexer ignores comments and whitespace, and will return
syntax as specified by the syntax table."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-number
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

;;; Analyzers generated from grammar.
;;
;; Some analyzers are hand written.  Analyzers created with these
;; functions are generated from the grammar files.

(defmacro define-lex-keyword-type-analyzer (name doc syntax)
  "Define a keyword type analyzer NAME with DOC string.
SYNTAX is the regexp that matches a keyword syntactic expression."
  (let ((key (make-symbol "key")))
    `(define-lex-analyzer ,name
       ,doc
       (and (looking-at ,syntax)
            (let ((,key (semantic-lex-keyword-p (match-string 0))))
              (when ,key
                (semantic-lex-push-token
                 (semantic-lex-token
                  ,key (match-beginning 0) (match-end 0)))))))
    ))

(defmacro define-lex-sexp-type-analyzer (name doc syntax token)
  "Define a sexp type analyzer NAME with DOC string.
SYNTAX is the regexp that matches the beginning of the s-expression.
TOKEN is the lexical token returned when SYNTAX matches."
  `(define-lex-regex-analyzer ,name
     ,doc
     ,syntax
     (semantic-lex-push-token
      (semantic-lex-token
       ,token (point)
       (save-excursion
         (semantic-lex-unterminated-syntax-protection ,token
           (forward-sexp 1)
           (point))))))
  )

(defmacro define-lex-regex-type-analyzer (name doc syntax matches default)
  "Define a regexp type analyzer NAME with DOC string.
SYNTAX is the regexp that matches a syntactic expression.
MATCHES is an alist of lexical elements used to refine the syntactic
expression.
DEFAULT is the default lexical token returned when no MATCHES."
  (if matches
      (let* ((val (make-symbol "val"))
             (lst (make-symbol "lst"))
             (elt (make-symbol "elt"))
             (pos (make-symbol "pos"))
             (end (make-symbol "end")))
        `(define-lex-analyzer ,name
           ,doc
           (and (looking-at ,syntax)
                (let* ((,val (match-string 0))
                       (,pos (match-beginning 0))
                       (,end (match-end 0))
                       (,lst ,matches)
                       ,elt)
                  (while (and ,lst (not ,elt))
                    (if (string-match (cdar ,lst) ,val)
                        (setq ,elt (caar ,lst))
                      (setq ,lst (cdr ,lst))))
                  (semantic-lex-push-token
                   (semantic-lex-token (or ,elt ,default) ,pos ,end))))
           ))
    `(define-lex-simple-regex-analyzer ,name
       ,doc
       ,syntax ,default)
    ))

(defmacro define-lex-string-type-analyzer (name doc syntax matches default)
  "Define a string type analyzer NAME with DOC string.
SYNTAX is the regexp that matches a syntactic expression.
MATCHES is an alist of lexical elements used to refine the syntactic
expression.
DEFAULT is the default lexical token returned when no MATCHES."
  (if matches
      (let* ((val (make-symbol "val"))
             (lst (make-symbol "lst"))
             (elt (make-symbol "elt"))
             (pos (make-symbol "pos"))
             (end (make-symbol "end"))
             (len (make-symbol "len")))
        `(define-lex-analyzer ,name
           ,doc
           (and (looking-at ,syntax)
                (let* ((,val (match-string 0))
                       (,pos (match-beginning 0))
                       (,end (match-end 0))
                       (,len (- ,end ,pos))
                       (,lst ,matches)
                       ,elt)
               ;; Starting with the longest one, search if a lexical
               ;; value match a token defined for this language.
               (while (and (> ,len 0) (not (setq ,elt (rassoc ,val ,lst))))
                 (setq ,len (1- ,len)
                       ,val (substring ,val 0 ,len)))
               (when ,elt ;; Adjust token end position.
                 (setq ,elt (car ,elt)
                       ,end (+ ,pos ,len)))
               (semantic-lex-push-token
                (semantic-lex-token (or ,elt ,default) ,pos ,end))))
           ))
    `(define-lex-simple-regex-analyzer ,name
       ,doc
       ,syntax ,default)
    ))

(defmacro define-lex-block-type-analyzer (name doc syntax matches)
  "Define a block type analyzer NAME with DOC string.

SYNTAX is the regexp that matches block delimiters,  typically the
open (`\\\\s(') and close (`\\\\s)') parenthesis syntax classes.

MATCHES is a pair (OPEN-SPECS . CLOSE-SPECS) that defines blocks.

  OPEN-SPECS is a list of (OPEN-DELIM OPEN-TOKEN BLOCK-TOKEN) elements
  where:

    OPEN-DELIM is a string: the block open delimiter character.

    OPEN-TOKEN is the lexical token class associated to the OPEN-DELIM
    delimiter.

    BLOCK-TOKEN is the lexical token class associated to the block
    that starts at the OPEN-DELIM delimiter.

  CLOSE-SPECS is a list of (CLOSE-DELIM CLOSE-TOKEN) elements where:

    CLOSE-DELIM is a string: the block end delimiter character.

    CLOSE-TOKEN is the lexical token class associated to the
    CLOSE-DELIM delimiter.

Each element in OPEN-SPECS must have a corresponding element in
CLOSE-SPECS.

The lexer will return a BLOCK-TOKEN token when the value of
`semantic-lex-current-depth' is greater than or equal to the maximum
depth of parenthesis tracking (see also the function `semantic-lex').
Otherwise it will return OPEN-TOKEN and CLOSE-TOKEN tokens.

TO DO: Put the following in the developer's guide and just put a
reference here.

In the grammar:

The value of a block token must be a string that contains a readable
sexp of the form:

  \"(OPEN-TOKEN CLOSE-TOKEN)\"

OPEN-TOKEN and CLOSE-TOKEN represent the block delimiters, and must be
lexical tokens of respectively `open-paren' and `close-paren' types.
Their value is the corresponding delimiter character as a string.

Here is a small example to analyze a parenthesis block:

  %token <block>       PAREN_BLOCK \"(LPAREN RPAREN)\"
  %token <open-paren>  LPAREN      \"(\"
  %token <close-paren> RPAREN      \")\"

When the lexer encounters the open-paren delimiter \"(\":

 - If the maximum depth of parenthesis tracking is not reached (that
   is, current depth < max depth), it returns a (LPAREN start .  end)
   token, then continue analysis inside the block.  Later, when the
   corresponding close-paren delimiter \")\" will be encountered, it
   will return a (RPAREN start . end) token.

 - If the maximum depth of parenthesis tracking is reached (current
   depth >= max depth), it returns the whole parenthesis block as
   a (PAREN_BLOCK start . end) token."
  (let* ((val (make-symbol "val"))
         (lst (make-symbol "lst"))
         (elt (make-symbol "elt")))
    `(define-lex-analyzer ,name
       ,doc
       (and
        (looking-at ,syntax) ;; "\\(\\s(\\|\\s)\\)"
        (let ((,val (match-string 0))
              (,lst ,matches)
              ,elt)
          (cond
           ((setq ,elt (assoc ,val (car ,lst)))
            (if (or (not semantic-lex-maximum-depth)
                    (< semantic-lex-current-depth semantic-lex-maximum-depth))
                (progn
                  (setq semantic-lex-current-depth (1+ semantic-lex-current-depth))
                  (semantic-lex-push-token
                   (semantic-lex-token
                    (nth 1 ,elt)
                    (match-beginning 0) (match-end 0))))
              (semantic-lex-push-token
               (semantic-lex-token
                (nth 2 ,elt)
                (match-beginning 0)
                (save-excursion
                  (semantic-lex-unterminated-syntax-protection (nth 2 ,elt)
                    (forward-list 1)
                    (point)))))))
           ((setq ,elt (assoc ,val (cdr ,lst)))
            (setq semantic-lex-current-depth (1- semantic-lex-current-depth))
            (semantic-lex-push-token
             (semantic-lex-token
              (nth 1 ,elt)
              (match-beginning 0) (match-end 0))))
           ))))
    ))

;;; Lexical Safety
;;
;; The semantic lexers, unlike other lexers, can throw errors on
;; unbalanced syntax.  Since editing is all about changing text
;; we need to provide a convenient way to protect against syntactic
;; inequalities.

(defmacro semantic-lex-catch-errors (symbol &rest forms)
  "Using SYMBOL, execute FORMS catching lexical errors.
If FORMS results in a call to the parser that throws a lexical error,
the error will be caught here without the buffer's cache being thrown
out of date.
If there is an error, the syntax that failed is returned.
If there is no error, then the last value of FORMS is returned."
  (let ((ret (make-symbol "ret"))
        (syntax (make-symbol "syntax"))
        (start (make-symbol "start"))
        (end (make-symbol "end")))
    `(let* ((semantic-lex-unterminated-syntax-end-function
             (lambda (,syntax ,start ,end)
               (throw ',symbol ,syntax)))
            ;; Delete the below when semantic-flex is fully retired.
            (semantic-flex-unterminated-syntax-end-function
             semantic-lex-unterminated-syntax-end-function)
            (,ret (catch ',symbol
                    (save-excursion
                      ,@forms
                      nil))))
       ;; Great Sadness.  Assume that FORMS execute within the
       ;; confines of the current buffer only!  Mark this thing
       ;; unparsable iff the special symbol was thrown.  This
       ;; will prevent future calls from parsing, but will allow
       ;; then to still return the cache.
       (when ,ret
	 ;; Leave this message off.  If an APP using this fcn wants
	 ;; a message, they can do it themselves.  This cleans up
	 ;; problems with the idle scheduler obscuring useful data.
         ;;(message "Buffer not currently parsable (%S)." ,ret)
         (semantic-parse-tree-unparseable))
       ,ret)))
(put 'semantic-lex-catch-errors 'lisp-indent-function 1)


;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()

     (def-edebug-spec define-lex
       (&define name stringp (&rest symbolp))
       )
     (def-edebug-spec define-lex-analyzer
       (&define name stringp form def-body)
       )
     (def-edebug-spec define-lex-regex-analyzer
       (&define name stringp form def-body)
       )
     (def-edebug-spec define-lex-simple-regex-analyzer
       (&define name stringp form symbolp [ &optional form ] def-body)
       )
     (def-edebug-spec define-lex-block-analyzer
       (&define name stringp form (&rest form))
       )
     (def-edebug-spec semantic-lex-catch-errors
       (symbolp def-body)
       )

     ))

;;; Compatibility with Semantic 1.x lexical analysis
;;
;; NOTE: DELETE THIS SOMEDAY SOON

(semantic-alias-obsolete 'semantic-flex-start 'semantic-lex-token-start "23.2")
(semantic-alias-obsolete 'semantic-flex-end 'semantic-lex-token-end "23.2")
(semantic-alias-obsolete 'semantic-flex-text 'semantic-lex-token-text "23.2")
(semantic-alias-obsolete 'semantic-flex-make-keyword-table 'semantic-lex-make-keyword-table "23.2")
(semantic-alias-obsolete 'semantic-flex-keyword-p 'semantic-lex-keyword-p "23.2")
(semantic-alias-obsolete 'semantic-flex-keyword-put 'semantic-lex-keyword-put "23.2")
(semantic-alias-obsolete 'semantic-flex-keyword-get 'semantic-lex-keyword-get "23.2")
(semantic-alias-obsolete 'semantic-flex-map-keywords 'semantic-lex-map-keywords "23.2")
(semantic-alias-obsolete 'semantic-flex-keywords 'semantic-lex-keywords "23.2")
(semantic-alias-obsolete 'semantic-flex-buffer 'semantic-lex-buffer "23.2")
(semantic-alias-obsolete 'semantic-flex-list 'semantic-lex-list "23.2")

;; This simple scanner uses the syntax table to generate a stream of
;; simple tokens of the form:
;;
;;  (SYMBOL START . END)
;;
;; Where symbol is the type of thing it is.  START and END mark that
;; objects boundary.

(defvar semantic-flex-tokens semantic-lex-tokens
  "An alist of semantic token types.
See variable `semantic-lex-tokens'.")

(defvar semantic-flex-unterminated-syntax-end-function
  (lambda (syntax syntax-start flex-end) flex-end)
  "Function called when unterminated syntax is encountered.
This should be set to one function.  That function should take three
parameters.  The SYNTAX, or type of syntax which is unterminated.
SYNTAX-START where the broken syntax begins.
FLEX-END is where the lexical analysis was asked to end.
This function can be used for languages that can intelligently fix up
broken syntax, or the exit lexical analysis via `throw' or `signal'
when finding unterminated syntax.")

(defvar semantic-flex-extensions nil
  "Buffer local extensions to the lexical analyzer.
This should contain an alist with a key of a regex and a data element of
a function.  The function should both move point, and return a lexical
token of the form:
  ( TYPE START .  END)
nil is also a valid return value.
TYPE can be any type of symbol, as long as it doesn't occur as a
nonterminal in the language definition.")
(make-variable-buffer-local 'semantic-flex-extensions)

(defvar semantic-flex-syntax-modifications nil
  "Changes to the syntax table for this buffer.
These changes are active only while the buffer is being flexed.
This is a list where each element has the form:
  (CHAR CLASS)
CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what syntax class CHAR has.")
(make-variable-buffer-local 'semantic-flex-syntax-modifications)

(defvar semantic-ignore-comments t
  "Default comment handling.
The value t means to strip comments when flexing; nil means
to keep comments as part of the token stream.")
(make-variable-buffer-local 'semantic-ignore-comments)

(defvar semantic-flex-enable-newlines nil
  "When flexing, report 'newlines as syntactic elements.
Useful for languages where the newline is a special case terminator.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-newlines)

(defvar semantic-flex-enable-whitespace nil
  "When flexing, report 'whitespace as syntactic elements.
Useful for languages where the syntax is whitespace dependent.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-whitespace)

(defvar semantic-flex-enable-bol nil
  "When flexing, report beginning of lines as syntactic elements.
Useful for languages like python which are indentation sensitive.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-bol)

(defvar semantic-number-expression semantic-lex-number-expression
  "See variable `semantic-lex-number-expression'.")
(make-variable-buffer-local 'semantic-number-expression)

(defvar semantic-flex-depth 0
  "Default flexing depth.
This specifies how many lists to create tokens in.")
(make-variable-buffer-local 'semantic-flex-depth)

(defun semantic-flex (start end &optional depth length)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element is a list, such of
the form (symbol start-expression .  end-expression) where SYMBOL
denotes the token type.
See `semantic-flex-tokens' variable for details on token types.
END does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.
The last argument, LENGTH specifies that `semantic-flex' should only
return LENGTH tokens."
  (message "`semantic-flex' is an obsolete function.  Use `define-lex' to create lexers.")
  (if (not semantic-flex-keywords-obarray)
      (setq semantic-flex-keywords-obarray [ nil ]))
  (let ((ts nil)
        (pos (point))
        (ep nil)
        (curdepth 0)
        (cs (if comment-start-skip
                (concat "\\(\\s<\\|" comment-start-skip "\\)")
              (concat "\\(\\s<\\)")))
        (newsyntax (copy-syntax-table (syntax-table)))
        (mods semantic-flex-syntax-modifications)
        ;; Use the default depth if it is not specified.
        (depth (or depth semantic-flex-depth)))
    ;; Update the syntax table
    (while mods
      (modify-syntax-entry (car (car mods)) (car (cdr (car mods))) newsyntax)
      (setq mods (cdr mods)))
    (with-syntax-table newsyntax
      (goto-char start)
      (while (and (< (point) end) (or (not length) (<= (length ts) length)))
        (cond
         ;; catch beginning of lines when needed.
         ;; Must be done before catching any other tokens!
         ((and semantic-flex-enable-bol
               (bolp)
               ;; Just insert a (bol N . N) token in the token stream,
               ;; without moving the point.  N is the point at the
               ;; beginning of line.
               (setq ts (cons (cons 'bol (cons (point) (point))) ts))
               nil)) ;; CONTINUE
         ;; special extensions, includes whitespace, nl, etc.
         ((and semantic-flex-extensions
               (let ((fe semantic-flex-extensions)
                     (r nil))
                 (while fe
                   (if (looking-at (car (car fe)))
                       (setq ts (cons (funcall (cdr (car fe))) ts)
                             r t
                             fe nil
                             ep (point)))
                   (setq fe (cdr fe)))
                 (if (and r (not (car ts))) (setq ts (cdr ts)))
                 r)))
         ;; catch newlines when needed
         ((looking-at "\\s-*\\(\n\\|\\s>\\)")
          (if semantic-flex-enable-newlines
              (setq ep (match-end 1)
                    ts (cons (cons 'newline
                                   (cons (match-beginning 1) ep))
                             ts))))
         ;; catch whitespace when needed
         ((looking-at "\\s-+")
          (if semantic-flex-enable-whitespace
              ;; Language wants whitespaces, link them together.
              (if (eq (car (car ts)) 'whitespace)
                  (setcdr (cdr (car ts)) (match-end 0))
                (setq ts (cons (cons 'whitespace
                                     (cons (match-beginning 0)
                                           (match-end 0)))
                               ts)))))
         ;; numbers
         ((and semantic-number-expression
               (looking-at semantic-number-expression))
          (setq ts (cons (cons 'number
                               (cons (match-beginning 0)
                                     (match-end 0)))
                         ts)))
         ;; symbols
         ((looking-at "\\(\\sw\\|\\s_\\)+")
          (setq ts (cons (cons
                          ;; Get info on if this is a keyword or not
                          (or (semantic-lex-keyword-p (match-string 0))
                              'symbol)
                          (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Character quoting characters (ie, \n as newline)
         ((looking-at "\\s\\+")
          (setq ts (cons (cons 'charquote
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Open parens, or semantic-lists.
         ((looking-at "\\s(")
          (if (or (not depth) (< curdepth depth))
              (progn
                (setq curdepth (1+ curdepth))
                (setq ts (cons (cons 'open-paren
                                     (cons (match-beginning 0) (match-end 0)))
                               ts)))
            (setq ts (cons
                      (cons 'semantic-list
                            (cons (match-beginning 0)
                                  (save-excursion
                                    (condition-case nil
                                        (forward-list 1)
                                      ;; This case makes flex robust
                                      ;; to broken lists.
                                      (error
                                       (goto-char
                                        (funcall
                                         semantic-flex-unterminated-syntax-end-function
                                         'semantic-list
                                         start end))))
                                    (setq ep (point)))))
                      ts))))
         ;; Close parens
         ((looking-at "\\s)")
          (setq ts (cons (cons 'close-paren
                               (cons (match-beginning 0) (match-end 0)))
                         ts))
          (setq curdepth (1- curdepth)))
         ;; String initiators
         ((looking-at "\\s\"")
          ;; Zing to the end of this string.
          (setq ts (cons (cons 'string
                               (cons (match-beginning 0)
                                     (save-excursion
                                       (condition-case nil
                                           (forward-sexp 1)
                                         ;; This case makes flex
                                         ;; robust to broken strings.
                                         (error
                                          (goto-char
                                           (funcall
                                            semantic-flex-unterminated-syntax-end-function
                                            'string
                                            start end))))
                                       (setq ep (point)))))
                         ts)))
         ;; comments
         ((looking-at cs)
          (if (and semantic-ignore-comments
                   (not semantic-flex-enable-whitespace))
              ;; If the language doesn't deal with comments nor
              ;; whitespaces, ignore them here.
              (let ((comment-start-point (point)))
                (forward-comment 1)
                (if (eq (point) comment-start-point)
                    ;; In this case our start-skip string failed
                    ;; to work properly.  Lets try and move over
                    ;; whatever white space we matched to begin
                    ;; with.
                    (skip-syntax-forward "-.'" (point-at-eol))
                  ;;(forward-comment 1)
                  ;; Generate newline token if enabled
                  (if (and semantic-flex-enable-newlines
                           (bolp))
                      (backward-char 1)))
                (if (eq (point) comment-start-point)
                    (error "Strange comment syntax prevents lexical analysis"))
                (setq ep (point)))
            (let ((tk (if semantic-ignore-comments 'whitespace 'comment)))
              (save-excursion
                (forward-comment 1)
                ;; Generate newline token if enabled
                (if (and semantic-flex-enable-newlines
                         (bolp))
                    (backward-char 1))
                (setq ep (point)))
              ;; Language wants comments or want them as whitespaces,
              ;; link them together.
              (if (eq (car (car ts)) tk)
                  (setcdr (cdr (car ts)) ep)
                (setq ts (cons (cons tk (cons (match-beginning 0) ep))
                               ts))))))
         ;; punctuation
         ((looking-at "\\(\\s.\\|\\s$\\|\\s'\\)")
          (setq ts (cons (cons 'punctuation
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; unknown token
         (t
          (error "What is that?")))
        (goto-char (or ep (match-end 0)))
        (setq ep nil)))
    ;; maybe catch the last beginning of line when needed
    (and semantic-flex-enable-bol
         (= (point) end)
         (bolp)
         (setq ts (cons (cons 'bol (cons (point) (point))) ts)))
    (goto-char pos)
    ;;(message "Flexing muscles...done")
    (nreverse ts)))

(provide 'semantic/lex)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/lex"
;; End:

;;; semantic/lex.el ends here
