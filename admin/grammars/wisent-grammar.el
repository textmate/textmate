;;; wisent-grammar.el --- Wisent's input grammar mode

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 26 Aug 2002
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
;; Major mode for editing Wisent's input grammar (.wy) files.

;;; Code:
(require 'semantic)
(require 'semantic/grammar)
(require 'semantic/find)
(require 'semantic/lex)
(require 'semantic/wisent)
(require 'semantic/bovine)

(defsubst wisent-grammar-region-placeholder (symb)
  "Given a $N placeholder symbol in SYMB, return a $regionN symbol.
Return nil if $N is not a valid placeholder symbol."
  (let ((n (symbol-name symb)))
    (if (string-match "^[$]\\([1-9][0-9]*\\)$" n)
        (intern (concat "$region" (match-string 1 n))))))

(defun wisent-grammar-EXPAND (symb nonterm)
  "Expand call to EXPAND grammar macro.
Return the form to parse from within a nonterminal.
SYMB is a $I placeholder symbol that gives the bounds of the area to
parse.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-bovinate-from-nonterminal
          (car ,$ri) (cdr ,$ri) ',nonterm)
      (error "Invalid form (EXPAND %s %s)" symb nonterm))))

(defun wisent-grammar-EXPANDFULL (symb nonterm)
  "Expand call to EXPANDFULL grammar macro.
Return the form to recursively parse an area.
SYMB is a $I placeholder symbol that gives the bounds of the area.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-parse-region
          (car ,$ri) (cdr ,$ri) ',nonterm 1)
      (error "Invalid form (EXPANDFULL %s %s)" symb nonterm))))

(defun wisent-grammar-TAG (name class &rest attributes)
  "Expand call to TAG grammar macro.
Return the form to create a generic semantic tag.
See the function `semantic-tag' for the meaning of arguments NAME,
CLASS and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag ,name ,class ,@attributes)))

(defun wisent-grammar-VARIABLE-TAG (name type default-value &rest attributes)
  "Expand call to VARIABLE-TAG grammar macro.
Return the form to create a semantic tag of class variable.
See the function `semantic-tag-new-variable' for the meaning of
arguments NAME, TYPE, DEFAULT-VALUE and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-variable ,name ,type ,default-value ,@attributes)))

(defun wisent-grammar-FUNCTION-TAG (name type arg-list &rest attributes)
  "Expand call to FUNCTION-TAG grammar macro.
Return the form to create a semantic tag of class function.
See the function `semantic-tag-new-function' for the meaning of
arguments NAME, TYPE, ARG-LIST and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-function ,name ,type ,arg-list ,@attributes)))

(defun wisent-grammar-TYPE-TAG (name type members parents &rest attributes)
  "Expand call to TYPE-TAG grammar macro.
Return the form to create a semantic tag of class type.
See the function `semantic-tag-new-type' for the meaning of arguments
NAME, TYPE, MEMBERS, PARENTS and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-type ,name ,type ,members ,parents ,@attributes)))

(defun wisent-grammar-INCLUDE-TAG (name system-flag &rest attributes)
  "Expand call to INCLUDE-TAG grammar macro.
Return the form to create a semantic tag of class include.
See the function `semantic-tag-new-include' for the meaning of
arguments NAME, SYSTEM-FLAG and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-include ,name ,system-flag ,@attributes)))

(defun wisent-grammar-PACKAGE-TAG (name detail &rest attributes)
  "Expand call to PACKAGE-TAG grammar macro.
Return the form to create a semantic tag of class package.
See the function `semantic-tag-new-package' for the meaning of
arguments NAME, DETAIL and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-package ,name ,detail ,@attributes)))

(defun wisent-grammar-CODE-TAG (name detail &rest attributes)
  "Expand call to CODE-TAG grammar macro.
Return the form to create a semantic tag of class code.
See the function `semantic-tag-new-code' for the meaning of arguments
NAME, DETAIL and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-code ,name ,detail ,@attributes)))

(defun wisent-grammar-ALIAS-TAG (name aliasclass definition &rest attributes)
  "Expand call to ALIAS-TAG grammar macro.
Return the form to create a semantic tag of class alias.
See the function `semantic-tag-new-alias' for the meaning of arguments
NAME, ALIASCLASS, DEFINITION and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-alias ,name ,aliasclass ,definition ,@attributes)))

(defun wisent-grammar-EXPANDTAG (raw-tag)
  "Expand call to EXPANDTAG grammar macro.
Return the form to produce a list of cooked tags from raw form of
Semantic tag RAW-TAG."
  `(wisent-cook-tag ,raw-tag))

(defun wisent-grammar-AST-ADD (ast &rest nodes)
  "Expand call to AST-ADD grammar macro.
Return the form to update the abstract syntax tree AST with NODES.
See also the function `semantic-ast-add'."
  `(semantic-ast-add ,ast ,@nodes))

(defun wisent-grammar-AST-PUT (ast &rest nodes)
  "Expand call to AST-PUT grammar macro.
Return the form to update the abstract syntax tree AST with NODES.
See also the function `semantic-ast-put'."
  `(semantic-ast-put ,ast ,@nodes))

(defun wisent-grammar-AST-GET (ast node)
  "Expand call to AST-GET grammar macro.
Return the form to get, from the abstract syntax tree AST, the value
of NODE.
See also the function `semantic-ast-get'."
  `(semantic-ast-get ,ast ,node))

(defun wisent-grammar-AST-GET1 (ast node)
  "Expand call to AST-GET1 grammar macro.
Return the form to get, from the abstract syntax tree AST, the first
value of NODE.
See also the function `semantic-ast-get1'."
  `(semantic-ast-get1 ,ast ,node))

(defun wisent-grammar-AST-GET-STRING (ast node)
  "Expand call to AST-GET-STRING grammar macro.
Return the form to get, from the abstract syntax tree AST, the value
of NODE as a string.
See also the function `semantic-ast-get-string'."
  `(semantic-ast-get-string ,ast ,node))

(defun wisent-grammar-AST-MERGE (ast1 ast2)
  "Expand call to AST-MERGE grammar macro.
Return the form to merge the abstract syntax trees AST1 and AST2.
See also the function `semantic-ast-merge'."
  `(semantic-ast-merge ,ast1 ,ast2))

(defun wisent-grammar-SKIP-BLOCK (&optional symb)
  "Expand call to SKIP-BLOCK grammar macro.
Return the form to skip a parenthesized block.
Optional argument SYMB is a $I placeholder symbol that gives the
bounds of the block to skip.  By default, skip the block at `$1'.
See also the function `wisent-skip-block'."
  (let ($ri)
    (when symb
      (unless (setq $ri (wisent-grammar-region-placeholder symb))
        (error "Invalid form (SKIP-BLOCK %s)" symb)))
    `(wisent-skip-block ,$ri)))

(defun wisent-grammar-SKIP-TOKEN ()
  "Expand call to SKIP-TOKEN grammar macro.
Return the form to skip the lookahead token.
See also the function `wisent-skip-token'."
  `(wisent-skip-token))

(defun wisent-grammar-assocs ()
  "Return associativity and precedence level definitions."
  (mapcar
   #'(lambda (tag)
       (cons (intern (semantic-tag-name tag))
             (mapcar #'semantic-grammar-item-value
                     (semantic-tag-get-attribute tag :value))))
   (semantic-find-tags-by-class 'assoc (current-buffer))))

(defun wisent-grammar-terminals ()
  "Return the list of terminal symbols.
Keep order of declaration in the WY file without duplicates."
  (let (terms)
    (mapcar
     #'(lambda (tag)
         (mapcar #'(lambda (name)
                     (add-to-list 'terms (intern name)))
                 (cons (semantic-tag-name tag)
                       (semantic-tag-get-attribute tag :rest))))
     (semantic--find-tags-by-function
      #'(lambda (tag)
          (memq (semantic-tag-class tag) '(token keyword)))
      (current-buffer)))
    (nreverse terms)))

;; Cache of macro definitions currently in use.
(defvar wisent--grammar-macros nil)

(defun wisent-grammar-expand-macros (expr)
  "Expand expression EXPR into a form without grammar macros.
Return the expanded expression."
  (if (or (atom expr) (semantic-grammar-quote-p (car expr)))
      expr ;; Just return atom or quoted expression.
    (let* ((expr  (mapcar 'wisent-grammar-expand-macros expr))
           (macro (assq (car expr) wisent--grammar-macros)))
      (if macro ;; Expand Semantic built-in.
          (apply (cdr macro) (cdr expr))
        expr))))

(defun wisent-grammar-nonterminals ()
  "Return the list form of nonterminal definitions."
  (let ((nttags (semantic-find-tags-by-class
                 'nonterminal (current-buffer)))
        ;; Setup the cache of macro definitions.
        (wisent--grammar-macros (semantic-grammar-macros))
        rltags nterms rules rule elems elem actn sexp prec)
    (while nttags
      (setq rltags (semantic-tag-components (car nttags))
            rules  nil)
      (while rltags
        (setq elems (semantic-tag-get-attribute (car rltags) :value)
              prec  (semantic-tag-get-attribute (car rltags) :prec)
              actn  (semantic-tag-get-attribute (car rltags) :expr)
              rule  nil)
        (when elems ;; not an EMPTY rule
          (while elems
            (setq elem  (car elems)
                  elems (cdr elems))
            (setq elem (if (consp elem) ;; mid-rule action
                           (wisent-grammar-expand-macros (read (car elem)))
                         (semantic-grammar-item-value elem)) ;; item
                  rule (cons elem rule)))
          (setq rule (nreverse rule)))
        (if prec
            (setq prec (vector (semantic-grammar-item-value prec))))
        (if actn
            (setq sexp (wisent-grammar-expand-macros (read actn))))
        (setq rule (if actn
                       (if prec
                           (list rule prec sexp)
                         (list rule sexp))
                     (if prec
                         (list rule prec)
                       (list rule))))
        (setq rules (cons rule rules)
              rltags (cdr rltags)))
      (setq nterms (cons (cons (intern (semantic-tag-name (car nttags)))
                               (nreverse rules))
                         nterms)
            nttags (cdr nttags)))
    (nreverse nterms)))

(defun wisent-grammar-grammar ()
  "Return Elisp form of the grammar."
  (let* ((terminals    (wisent-grammar-terminals))
         (nonterminals (wisent-grammar-nonterminals))
         (assocs       (wisent-grammar-assocs)))
    (cons terminals (cons assocs nonterminals))))

(defun wisent-grammar-parsetable-builder ()
  "Return the value of the parser table."
  `(progn
     ;; Ensure that the grammar [byte-]compiler is available.
     (eval-when-compile (require 'semantic/wisent/comp))
     (wisent-compile-grammar
      ',(wisent-grammar-grammar)
      ',(semantic-grammar-start))))

(defun wisent-grammar-setupcode-builder ()
  "Return the parser setup code."
  (format
   "(semantic-install-function-overrides\n\
      '((parse-stream . wisent-parse-stream)))\n\
    (setq semantic-parser-name \"LALR\"\n\
          semantic--parse-table %s\n\
          semantic-debug-parser-source %S\n\
          semantic-flex-keywords-obarray %s\n\
          semantic-lex-types-obarray %s)\n\
    ;; Collect unmatched syntax lexical tokens\n\
    (semantic-make-local-hook 'wisent-discarding-token-functions)\n\
    (add-hook 'wisent-discarding-token-functions\n\
              'wisent-collect-unmatched-syntax nil t)"
   (semantic-grammar-parsetable)
   (buffer-name)
   (semantic-grammar-keywordtable)
   (semantic-grammar-tokentable)))

(defvar wisent-grammar-menu
  '("WY Grammar"
    ["LALR Compiler Verbose" wisent-toggle-verbose-flag
     :style toggle :active (boundp 'wisent-verbose-flag)
     :selected (and (boundp 'wisent-verbose-flag)
                    wisent-verbose-flag)]
    )
  "WY mode specific grammar menu.
Menu items are appended to the common grammar menu.")

(define-derived-mode wisent-grammar-mode semantic-grammar-mode "WY"
  "Major mode for editing Wisent grammars."
  (semantic-grammar-setup-menu wisent-grammar-menu)
  (semantic-install-function-overrides
   '((grammar-parsetable-builder . wisent-grammar-parsetable-builder)
     (grammar-setupcode-builder  . wisent-grammar-setupcode-builder)
     )))

(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisent-grammar-mode))

(defvar-mode-local wisent-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . wisent-grammar-EXPAND)
    (EXPANDFULL     . wisent-grammar-EXPANDFULL)
    (TAG            . wisent-grammar-TAG)
    (VARIABLE-TAG   . wisent-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . wisent-grammar-FUNCTION-TAG)
    (TYPE-TAG       . wisent-grammar-TYPE-TAG)
    (INCLUDE-TAG    . wisent-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . wisent-grammar-PACKAGE-TAG)
    (EXPANDTAG      . wisent-grammar-EXPANDTAG)
    (CODE-TAG       . wisent-grammar-CODE-TAG)
    (ALIAS-TAG      . wisent-grammar-ALIAS-TAG)
    (AST-ADD        . wisent-grammar-AST-ADD)
    (AST-PUT        . wisent-grammar-AST-PUT)
    (AST-GET        . wisent-grammar-AST-GET)
    (AST-GET1       . wisent-grammar-AST-GET1)
    (AST-GET-STRING . wisent-grammar-AST-GET-STRING)
    (AST-MERGE      . wisent-grammar-AST-MERGE)
    (SKIP-BLOCK     . wisent-grammar-SKIP-BLOCK)
    (SKIP-TOKEN     . wisent-grammar-SKIP-TOKEN)
    )
  "Semantic grammar macros used in wisent grammars.")

(defvar wisent-make-parsers--emacs-license
  ";; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.")

(defvar wisent-make-parsers--python-license
  ";; It is derived in part from the Python grammar, used under the
;; following license:
;;
;; PYTHON SOFTWARE FOUNDATION LICENSE VERSION 2
;; --------------------------------------------
;; 1. This LICENSE AGREEMENT is between the Python Software Foundation
;; (\"PSF\"), and the Individual or Organization (\"Licensee\") accessing
;; and otherwise using this software (\"Python\") in source or binary
;; form and its associated documentation.
;;
;; 2. Subject to the terms and conditions of this License Agreement,
;; PSF hereby grants Licensee a nonexclusive, royalty-free, world-wide
;; license to reproduce, analyze, test, perform and/or display
;; publicly, prepare derivative works, distribute, and otherwise use
;; Python alone or in any derivative version, provided, however, that
;; PSF's License Agreement and PSF's notice of copyright, i.e.,
;; \"Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
;; 2009, 2010 Python Software Foundation; All Rights Reserved\" are
;; retained in Python alone or in any derivative version prepared by
;; Licensee.
;;
;; 3. In the event Licensee prepares a derivative work that is based
;; on or incorporates Python or any part thereof, and wants to make
;; the derivative work available to others as provided herein, then
;; Licensee hereby agrees to include in any such work a brief summary
;; of the changes made to Python.
;;
;; 4. PSF is making Python available to Licensee on an \"AS IS\"
;; basis.  PSF MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
;; IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PSF MAKES NO AND
;; DISCLAIMS ANY REPRESENTATION OR WARRANTY OF MERCHANTABILITY OR FITNESS
;; FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF PYTHON WILL NOT
;; INFRINGE ANY THIRD PARTY RIGHTS.
;;
;; 5. PSF SHALL NOT BE LIABLE TO LICENSEE OR ANY OTHER USERS OF PYTHON
;; FOR ANY INCIDENTAL, SPECIAL, OR CONSEQUENTIAL DAMAGES OR LOSS AS A
;; RESULT OF MODIFYING, DISTRIBUTING, OR OTHERWISE USING PYTHON, OR
;; ANY DERIVATIVE THEREOF, EVEN IF ADVISED OF THE POSSIBILITY THEREOF.
;;
;; 6. This License Agreement will automatically terminate upon a
;; material breach of its terms and conditions.
;;
;; 7. Nothing in this License Agreement shall be deemed to create any
;; relationship of agency, partnership, or joint venture between PSF
;; and Licensee.  This License Agreement does not grant permission to
;; use PSF trademarks or trade name in a trademark sense to endorse or
;; promote products or services of Licensee, or any third party.
;;
;; 8. By copying, installing or otherwise using Python, Licensee
;; agrees to be bound by the terms and conditions of this License
;; Agreement.")

(defvar wisent-make-parsers--ecmascript-license
  "\n;; It is derived from the grammar in the ECMAScript Language
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
;; PROVIDED BY THE ECMA INTERNATIONAL \"AS IS\" AND ANY EXPRESS OR
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
;; DAMAGE.")

(defvar wisent-make-parsers--parser-file-name
  `(("semantic-grammar-wy.el"
     "semantic/grammar-wy")
    ("srecode-template-wy.el"
     "srecode/srt-wy")
    ("wisent-javascript-jv-wy.el"
     "semantic/wisent/js-wy"
     "Copyright (C) 1998-2011 Ecma International."
     ,wisent-make-parsers--ecmascript-license)
    ("wisent-java-tags-wy.el"
     "semantic/wisent/javat-wy")
    ("wisent-python-wy.el"
     "semantic/wisent/python-wy"
     "Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Python Software Foundation; All Rights Reserved."
     ,wisent-make-parsers--python-license)))

(defun wisent-make-parsers ()
  "Generate Emacs' built-in Wisent-based parser files."
  (semantic-mode 1)
  ;; Loop through each .wy file in current directory, and run
  ;; `semantic-grammar-batch-build-one-package' to build the grammar.
  (dolist (f (directory-files default-directory nil "\\.wy\\'"))
    (let ((packagename
           (condition-case err
               (with-current-buffer (find-file-noselect f)
                 (semantic-grammar-create-package))
             (error (message "%s" (error-message-string err)) nil)))
	  output-data)
      (when (setq output-data (assoc packagename wisent-make-parsers--parser-file-name))
	(let ((require-name         (nth 1 output-data))
	      (additional-copyright (nth 2 output-data))
	      (additional-license   (nth 3 output-data))
	      copyright-end)
	  ;; Touch up the generated parsers for Emacs integration.
	  (with-temp-buffer
	    (insert-file-contents packagename)
	    ;; Fix copyright header:
	    (goto-char (point-min))
	    (when additional-copyright
	      (re-search-forward "Copyright (C).*$")
	      (insert "\n;; " additional-copyright))
	    (re-search-forward "^;; Author:")
	    (setq copyright-end (match-beginning 0))
	    (re-search-forward "^;;; Code:\n")
	    (delete-region copyright-end (match-end 0))
	    (goto-char copyright-end)
	    (insert wisent-make-parsers--emacs-license)
	    (insert "\n\n;;; Commentary:
;;
;; This file was generated from admin/grammars/"
			f ".")
	    (when additional-license
	      (insert "\n" additional-license))
	    (insert "\n\n;;; Code:\n
\(require 'semantic/lex)\n")
	    (goto-char (point-min))
	    (delete-region (point-min) (line-end-position))
	    (insert ";;; " require-name
		    ".el --- Generated parser support file")
	    (delete-trailing-whitespace)
	    (re-search-forward ";;\n(require 'semantic/lex)\n")
	    (delete-region (match-beginning 0) (match-end 0))
	    ;; Fix footer:
	    (goto-char (point-max))
	    (re-search-backward "^(provide")
	    (delete-region (match-beginning 0) (point-max))
	    (goto-char (point-max))
	    (insert "(provide '" require-name ")\n\n")
	    (insert ";;; " require-name ".el ends here\n")
	    (write-region nil nil (expand-file-name packagename))))))))

;;; wisent-grammar.el ends here
