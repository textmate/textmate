;;; bovine-grammar.el --- Bovine's input grammar mode
;;
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
;; Major mode for editing Bovine's input grammar (.by) files.

;;; History:

;;; Code:
(require 'semantic)
(require 'semantic/grammar)
(require 'semantic/find)
(require 'semantic/lex)
(require 'semantic/wisent)
(require 'semantic/bovine)

(defun bovine-grammar-EXPAND (bounds nonterm)
  "Expand call to EXPAND grammar macro.
Return the form to parse from within a nonterminal between BOUNDS.
NONTERM is the nonterminal symbol to start with."
  `(semantic-bovinate-from-nonterminal
    (car ,bounds) (cdr ,bounds) ',nonterm))

(defun bovine-grammar-EXPANDFULL (bounds nonterm)
  "Expand call to EXPANDFULL grammar macro.
Return the form to recursively parse the area between BOUNDS.
NONTERM is the nonterminal symbol to start with."
  `(semantic-parse-region
    (car ,bounds) (cdr ,bounds) ',nonterm 1))

(defun bovine-grammar-TAG (name class &rest attributes)
  "Expand call to TAG grammar macro.
Return the form to create a generic semantic tag.
See the function `semantic-tag' for the meaning of arguments NAME,
CLASS and ATTRIBUTES."
  `(semantic-tag ,name ,class ,@attributes))

(defun bovine-grammar-VARIABLE-TAG (name type default-value &rest attributes)
  "Expand call to VARIABLE-TAG grammar macro.
Return the form to create a semantic tag of class variable.
See the function `semantic-tag-new-variable' for the meaning of
arguments NAME, TYPE, DEFAULT-VALUE and ATTRIBUTES."
  `(semantic-tag-new-variable ,name ,type ,default-value ,@attributes))

(defun bovine-grammar-FUNCTION-TAG (name type arg-list &rest attributes)
  "Expand call to FUNCTION-TAG grammar macro.
Return the form to create a semantic tag of class function.
See the function `semantic-tag-new-function' for the meaning of
arguments NAME, TYPE, ARG-LIST and ATTRIBUTES."
  `(semantic-tag-new-function ,name ,type ,arg-list ,@attributes))

(defun bovine-grammar-TYPE-TAG (name type members parents &rest attributes)
  "Expand call to TYPE-TAG grammar macro.
Return the form to create a semantic tag of class type.
See the function `semantic-tag-new-type' for the meaning of arguments
NAME, TYPE, MEMBERS, PARENTS and ATTRIBUTES."
  `(semantic-tag-new-type ,name ,type ,members ,parents ,@attributes))

(defun bovine-grammar-INCLUDE-TAG (name system-flag &rest attributes)
  "Expand call to INCLUDE-TAG grammar macro.
Return the form to create a semantic tag of class include.
See the function `semantic-tag-new-include' for the meaning of
arguments NAME, SYSTEM-FLAG and ATTRIBUTES."
  `(semantic-tag-new-include ,name ,system-flag ,@attributes))

(defun bovine-grammar-PACKAGE-TAG (name detail &rest attributes)
  "Expand call to PACKAGE-TAG grammar macro.
Return the form to create a semantic tag of class package.
See the function `semantic-tag-new-package' for the meaning of
arguments NAME, DETAIL and ATTRIBUTES."
  `(semantic-tag-new-package ,name ,detail ,@attributes))

(defun bovine-grammar-CODE-TAG (name detail &rest attributes)
  "Expand call to CODE-TAG grammar macro.
Return the form to create a semantic tag of class code.
See the function `semantic-tag-new-code' for the meaning of arguments
NAME, DETAIL and ATTRIBUTES."
  `(semantic-tag-new-code ,name ,detail ,@attributes))

(defun bovine-grammar-ALIAS-TAG (name aliasclass definition &rest attributes)
  "Expand call to ALIAS-TAG grammar macro.
Return the form to create a semantic tag of class alias.
See the function `semantic-tag-new-alias' for the meaning of arguments
NAME, ALIASCLASS, DEFINITION and ATTRIBUTES."
  `(semantic-tag-new-alias ,name ,aliasclass ,definition ,@attributes))

;; Cache of macro definitions currently in use.
(defvar bovine--grammar-macros nil)

;; Detect if we have an Emacs with newstyle unquotes allowed outside
;; of backquote.
;; This should probably be changed to a test to (= emacs-major-version 24)
;; when it is released, but at the moment it might be possible that people
;; are using an older snapshot.
(defvar bovine--grammar-newstyle-unquote
  (equal '(\, test) (read ",test")))

(defun bovine-grammar-expand-form (form quotemode &optional inplace)
  "Expand FORM into a new one suitable to the bovine parser.
FORM is a list in which we are substituting.
Argument QUOTEMODE is non-nil if we are in backquote mode.
When non-nil, optional argument INPLACE indicates that FORM is being
expanded from elsewhere."
  (when (eq (car form) 'quote)
    (setq form (cdr form))
    (cond
     ((and (= (length form) 1) (listp (car form)))
      (insert "\n(append")
      (bovine-grammar-expand-form (car form) quotemode nil)
      (insert ")")
      (setq form nil inplace nil)
      )
     ((and (= (length form) 1) (symbolp (car form)))
      (insert "\n'" (symbol-name (car form)))
      (setq form nil inplace nil)
      )
     (t
      (insert "\n(list")
      (setq inplace t)
      )))
  (let ((macro (assq (car form) bovine--grammar-macros))
        inlist first n q x)
    (if macro
        (bovine-grammar-expand-form
         (apply (cdr macro) (cdr form))
         quotemode t)
      (if inplace (insert "\n("))
      (while form
        (setq first (car form)
              form  (cdr form))
	;; Hack for dealing with new reading of unquotes outside of
	;; backquote (introduced in rev. 102591 in emacs-bzr).
	(when (and bovine--grammar-newstyle-unquote
		   (listp first)
		   (or (equal (car first) '\,)
		       (equal (car first) '\,@)))
	  (if (listp (cadr first))
	      (setq form (append (cdr first) form)
		    first (car first))
	    (setq first (intern (concat (symbol-name (car first))
					(symbol-name (cadr first)))))))
        (cond
         ((eq first nil)
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          (insert " nil")
          )
         ((listp first)
          ;;(let ((fn (and (symbolp (caar form)) (fboundp (caar form)))))
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          ;;(if (and inplace (not fn) (not (eq (caar form) 'EXPAND)))
          ;;    (insert " (append"))
          (bovine-grammar-expand-form
           first quotemode t) ;;(and fn (not (eq fn 'quote))))
          ;;(if (and inplace (not fn) (not (eq (caar form) 'EXPAND)))
          ;;    (insert  ")"))
          ;;)
          )
         ((symbolp first)
          (setq n (symbol-name first)   ;the name
                q quotemode             ;implied quote flag
                x nil)                  ;expand flag
          (if (eq (aref n 0) ?,)
              (if quotemode
                  ;; backquote mode needs the @
                  (if (eq (aref n 1) ?@)
                      (setq n (substring n 2)
                            q nil
                            x t)
                    ;; non backquote mode behaves normally.
                    (setq n (substring n 1)
                          q nil))
                (setq n (substring n 1)
                      x t)))
          (if (string= n "")
              (progn
                ;; We expand only the next item in place (a list?)
                ;; A regular inline-list...
                (bovine-grammar-expand-form (car form) quotemode t)
                (setq form (cdr form)))
            (if (and (eq (aref n 0) ?$)
                     ;; Don't expand $ tokens in implied quote mode.
                     ;; This acts like quoting in other symbols.
                     (not q))
                (progn
                  (cond
                   ((and (not x) (not inlist) (not inplace))
                    (insert "\n(list"))
                   ((and x inlist (not inplace))
                    (insert ")")
                    (setq inlist nil)))
                  (insert "\n(nth " (int-to-string
                                     (1- (string-to-number
                                          (substring n 1))))
                          " vals)")
                  (and (not x) (not inplace)
                       (setq inlist t)))

              (when (and (not inlist) (not inplace))
                (insert "\n(list")
                (setq inlist t))
              (or (char-equal (char-before) ?\()
                  (insert " "))
              (insert (if (or inplace (eq first t))
                          "" "'")
                      n))) ;; " "
          )
         (t
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          (insert (format "\n%S" first))
          )
         ))
      (if inlist (insert ")"))
      (if inplace (insert ")")))
    ))

(defun bovine-grammar-expand-action (textform quotemode)
  "Expand semantic action string TEXTFORM into Lisp code.
QUOTEMODE is the mode in which quoted symbols are slurred."
  (if (string= "" textform)
      nil
    (let ((sexp (read textform)))
      ;; We converted the lambda string into a list.  Now write it
      ;; out as the bovine lambda expression, and do macro-like
      ;; conversion upon it.
      (insert "\n")
      (cond
       ((eq (car sexp) 'EXPAND)
        (insert ",(lambda (vals start end)")
        ;; The EXPAND macro definition is mandatory
        (bovine-grammar-expand-form
         (apply (cdr (assq 'EXPAND bovine--grammar-macros)) (cdr sexp))
         quotemode t)
        )
       ((and (listp (car sexp)) (eq (caar sexp) 'EVAL))
        ;; The user wants to evaluate the following args.
        ;; Use a simpler expander
        )
       (t
        (insert ",(semantic-lambda")
        (bovine-grammar-expand-form sexp quotemode)
        ))
      (insert ")\n")))
)

(defun bovine-grammar-parsetable-builder ()
  "Return the parser table expression as a string value.
The format of a bovine parser table is:

 ( ( NONTERMINAL-SYMBOL1 MATCH-LIST1 )
   ( NONTERMINAL-SYMBOL2 MATCH-LIST2 )
   ...
   ( NONTERMINAL-SYMBOLn MATCH-LISTn )

Where each NONTERMINAL-SYMBOL is an artificial symbol which can appear
in any child state.  As a starting place, one of the NONTERMINAL-SYMBOLS
must be `bovine-toplevel'.

A MATCH-LIST is a list of possible matches of the form:

 ( STATE-LIST1
   STATE-LIST2
   ...
   STATE-LISTN )

where STATE-LIST is of the form:
  ( TYPE1 [ \"VALUE1\" ] TYPE2 [ \"VALUE2\" ] ... LAMBDA )

where TYPE is one of the returned types of the token stream.
VALUE is a value, or range of values to match against.  For
example, a SYMBOL might need to match \"foo\".  Some TYPES will not
have matching criteria.

LAMBDA is a lambda expression which is evalled with the text of the
type when it is found.  It is passed the list of all buffer text
elements found since the last lambda expression.  It should return a
semantic element (see below.)

For consistency between languages, try to use common return values
from your parser.  Please reference the chapter \"Writing Parsers\" in
the \"Language Support Developer's Guide -\" in the semantic texinfo
manual."
  (let* ((start      (semantic-grammar-start))
         (scopestart (semantic-grammar-scopestart))
         (quotemode  (semantic-grammar-quotemode))
         (tags       (semantic-find-tags-by-class
                      'token (current-buffer)))
         (nterms     (semantic-find-tags-by-class
                      'nonterminal (current-buffer)))
         ;; Setup the cache of macro definitions.
         (bovine--grammar-macros (semantic-grammar-macros))
         nterm rules items item actn prec tag type regex)

    ;; Check some trivial things
    (cond
     ((null nterms)
      (error "Bad input grammar"))
     (start
      (if (cdr start)
          (message "Extra start symbols %S ignored" (cdr start)))
      (setq start (symbol-name (car start)))
      (unless (semantic-find-first-tag-by-name start nterms)
        (error "start symbol `%s' has no rule" start)))
     (t
      ;; Default to the first grammar rule.
      (setq start (semantic-tag-name (car nterms)))))
    (when scopestart
      (setq scopestart (symbol-name scopestart))
      (unless (semantic-find-first-tag-by-name scopestart nterms)
        (error "scopestart symbol `%s' has no rule" scopestart)))

    ;; Generate the grammar Lisp form.
    (with-temp-buffer
      (erase-buffer)
      (insert "`(")
      ;; Insert the start/scopestart rules
      (insert "\n(bovine-toplevel \n("
              start
              ")\n) ;; end bovine-toplevel\n")
      (when scopestart
        (insert "\n(bovine-inner-scope \n("
                scopestart
                ")\n) ;; end bovine-inner-scope\n"))
      ;; Process each nonterminal
      (while nterms
        (setq nterm  (car nterms)
              ;; We can't use the override form because the current buffer
              ;; is not the originator of the tag.
              rules  (semantic-tag-components-semantic-grammar-mode nterm)
              nterm  (semantic-tag-name nterm)
              nterms (cdr nterms))
        (when (member nterm '("bovine-toplevel" "bovine-inner-scope"))
          (error "`%s' is a reserved internal name" nterm))
        (insert "\n(" nterm)
        ;; Process each rule
        (while rules
          (setq items (semantic-tag-get-attribute (car rules) :value)
                prec  (semantic-tag-get-attribute (car rules) :prec)
                actn  (semantic-tag-get-attribute (car rules) :expr)
                rules (cdr rules))
          ;; Process each item
          (insert "\n(")
          (if (null items)
              ;; EMPTY rule
              (insert ";;EMPTY" (if actn "" "\n"))
            ;; Expand items
            (while items
              (setq item  (car items)
                    items (cdr items))
              (if (consp item) ;; mid-rule action
                  (message "Mid-rule action %S ignored" item)
                (or (char-equal (char-before) ?\()
                    (insert "\n"))
                (cond
                 ((member item '("bovine-toplevel" "bovine-inner-scope"))
                  (error "`%s' is a reserved internal name" item))
                 ;; Replace ITEM by its %token definition.
                 ;; If a '%token TYPE ITEM [REGEX]' definition exists
                 ;; in the grammar, ITEM is replaced by TYPE [REGEX].
                 ((setq tag (semantic-find-first-tag-by-name
                             item tags)
                        type  (semantic-tag-get-attribute tag :type))
                  (insert type)
                  (if (setq regex (semantic-tag-get-attribute tag :value))
                      (insert (format "\n%S" regex))))
                 ;; Don't change ITEM
                 (t
                  (insert (semantic-grammar-item-text item)))
                 ))))
          (if prec
              (message "%%prec %S ignored" prec))
          (if actn
              (bovine-grammar-expand-action actn quotemode))
          (insert ")"))
        (insert "\n) ;; end " nterm "\n"))
      (insert ")\n")
      (buffer-string))))

(defun bovine-grammar-setupcode-builder ()
  "Return the text of the setup code."
  (format
   "(setq semantic--parse-table %s\n\
          semantic-debug-parser-source %S\n\
          semantic-debug-parser-class 'semantic-bovine-debug-parser
          semantic-flex-keywords-obarray %s\n\
          %s)"
   (semantic-grammar-parsetable)
   (buffer-name)
   (semantic-grammar-keywordtable)
   (let ((mode (semantic-grammar-languagemode)))
     ;; Is there more than one major mode?
     (if (and (listp mode) (> (length mode) 1))
         (format "semantic-equivalent-major-modes '%S\n" mode)
       ""))))

(defvar bovine-grammar-menu
  '("BY Grammar"
    )
  "BY mode specific grammar menu.
Menu items are appended to the common grammar menu.")

(define-derived-mode bovine-grammar-mode semantic-grammar-mode "BY"
  "Major mode for editing Bovine grammars."
  (semantic-grammar-setup-menu bovine-grammar-menu)
  (semantic-install-function-overrides
   '((grammar-parsetable-builder . bovine-grammar-parsetable-builder)
     (grammar-setupcode-builder  . bovine-grammar-setupcode-builder)
     )))

(add-to-list 'auto-mode-alist '("\\.by\\'" . bovine-grammar-mode))

(defvar-mode-local bovine-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . bovine-grammar-EXPAND)
    (EXPANDFULL     . bovine-grammar-EXPANDFULL)
    (TAG            . bovine-grammar-TAG)
    (VARIABLE-TAG   . bovine-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . bovine-grammar-FUNCTION-TAG)
    (TYPE-TAG       . bovine-grammar-TYPE-TAG)
    (INCLUDE-TAG    . bovine-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . bovine-grammar-PACKAGE-TAG)
    (CODE-TAG       . bovine-grammar-CODE-TAG)
    (ALIAS-TAG      . bovine-grammar-ALIAS-TAG)
    )
  "Semantic grammar macros used in bovine grammars.")

(provide 'semantic/bovine/grammar)

(defun bovine-make-parsers ()
  "Generate Emacs' built-in Bovine-based parser files."
  (semantic-mode 1)
  ;; Loop through each .by file in current directory, and run
  ;; `semantic-grammar-batch-build-one-package' to build the grammar.
  (dolist (f (directory-files default-directory nil "\\.by\\'"))
    (let ((packagename
           (condition-case err
               (with-current-buffer (find-file-noselect f)
                 (semantic-grammar-create-package))
             (error (message "%s" (error-message-string err)) nil)))
	  lang)
      (when (and packagename
		 (string-match "^semantic-\\(.*\\)-by\\.el\\'" packagename))
	(setq lang (match-string 1 packagename))
	(with-temp-buffer
	  (insert-file-contents packagename)
	  (setq buffer-file-name (expand-file-name packagename))
	  ;; Fix copyright header:
	  (goto-char (point-min))
	  (re-search-forward "^;; Author:")
	  (setq copyright-end (match-beginning 0))
	  (re-search-forward "^;;; Code:\n")
	  (delete-region copyright-end (match-end 0))
	  (goto-char copyright-end)
	  (insert ";; This file is part of GNU Emacs.

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
;; This file was generated from admin/grammars/"
		  lang ".by.

;;; Code:

\(require 'semantic/lex)
\(eval-when-compile (require 'semantic/bovine))\n")
	  (goto-char (point-min))
	  (delete-region (point-min) (line-end-position))
	  (insert ";;; semantic/bovine/" lang
		  "-by.el --- Generated parser support file")
	  (delete-trailing-whitespace)
	  ;; Fix footer:
	  (goto-char (point-max))
	  (re-search-backward ".\n;;; Analyzers")
	  (delete-region (point) (point-max))
	  (insert "(provide 'semantic/bovine/" lang "-by)\n\n")
	  (insert ";;; semantic/bovine/" lang "-by.el ends here\n")
	  (save-buffer))))))

;;; bovine-grammar.el ends here
