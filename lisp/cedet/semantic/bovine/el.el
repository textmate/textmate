;;; semantic/bovine/el.el --- Semantic details for Emacs Lisp

;; Copyright (C) 1999-2005, 2007-2012  Free Software Foundation, Inc.

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
;; Use the Semantic Bovinator for Emacs Lisp

(require 'semantic)
(require 'semantic/bovine)
(require 'find-func)

(require 'semantic/ctxt)
(require 'semantic/format)
(require 'thingatpt)

;;; Code:

;;; Lexer
;;
(define-lex semantic-emacs-lisp-lexer
  "A simple lexical analyzer for Emacs Lisp.
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

;;; Parser
;;
(defvar semantic--elisp-parse-table
  `((bovine-toplevel
     (semantic-list
      ,(lambda (vals start end)
         (let ((tag (semantic-elisp-use-read (car vals))))
	   (cond
	    ((and (listp tag) (semantic-tag-p (car tag)))
	     ;; We got a list of tags back.  This list is
	     ;; returned here in the correct order, but this
	     ;; list gets reversed later, putting the correctly ordered
	     ;; items into reverse order later.
	     (nreverse tag))
	    ((semantic--tag-expanded-p tag)
	     ;; At this point, if `semantic-elisp-use-read' returned an
	     ;; already expanded tag (from definitions parsed inside an
	     ;; eval and compile wrapper), just pass it!
	     tag)
	    (t
	     ;; We got the basics of a single tag.
	     (append tag (list start end))))))))
    )
  "Top level bovination table for elisp.")

(defun semantic-elisp-desymbolify (arglist)
  "Convert symbols to strings for ARGLIST."
  (let ((out nil))
    (while arglist
      (setq out
	    (cons
	     (if (symbolp (car arglist))
		 (symbol-name (car arglist))
	       (if (and (listp (car arglist))
			(symbolp (car (car arglist))))
		   (symbol-name (car (car arglist)))
		 (format "%S" (car arglist))))
	     out)
	    arglist (cdr arglist)))
    (nreverse out)))

(defun semantic-elisp-desymbolify-args (arglist)
  "Convert symbols to strings for ARGLIST."
  (let ((in (semantic-elisp-desymbolify arglist))
	(out nil))
    (dolist (T in)
      (when (not (string-match "^&" T))
	(push T out)))
    (nreverse out)))

(defun semantic-elisp-clos-slot-property-string (slot property)
  "For SLOT, a string representing PROPERTY."
  (let ((p (member property slot)))
    (if (not p)
	nil
      (setq p (cdr p))
      (cond
       ((stringp (car p))
	(car p))
       ((or (symbolp (car p))
	    (listp (car p))
	    (numberp (car p)))
	(format "%S" (car p)))
       (t nil)))))

(defun semantic-elisp-clos-args-to-semantic (partlist)
  "Convert a list of CLOS class slot PARTLIST to `variable' tags."
  (let (vars part v)
    (while partlist
      (setq part (car partlist)
            partlist (cdr partlist)
            v (semantic-tag-new-variable
               (symbol-name (car part))
               (semantic-elisp-clos-slot-property-string part :type)
               (semantic-elisp-clos-slot-property-string part :initform)
               ;; Attributes
               :protection (semantic-elisp-clos-slot-property-string
                            part :protection)
               :static-flag (equal (semantic-elisp-clos-slot-property-string
                                    part :allocation)
                                   ":class")
               :documentation (semantic-elisp-clos-slot-property-string
                               part :documentation))
            vars (cons v vars)))
    (nreverse vars)))

(defun semantic-elisp-form-to-doc-string (form)
  "After reading a form FORM, convert it to a doc string.
For Emacs Lisp, sometimes that string is non-existent.
Sometimes it is a form which is evaluated at compile time, permitting
compound strings."
  (cond ((stringp form) form)
	((and (listp form) (eq (car form) 'concat)
	      (stringp (nth 1 form)))
	 (nth 1 form))
	(t nil)))

(defvar semantic-elisp-store-documentation-in-tag nil
  "*When non-nil, store documentation strings in the created tags.")

(defun semantic-elisp-do-doc (str)
  "Return STR as a documentation string IF they are enabled."
  (when semantic-elisp-store-documentation-in-tag
    (semantic-elisp-form-to-doc-string str)))

(defmacro semantic-elisp-setup-form-parser (parser &rest symbols)
  "Install the function PARSER as the form parser for SYMBOLS.
SYMBOLS is a list of symbols identifying the forms to parse.
PARSER is called on every forms whose first element (car FORM) is
found in SYMBOLS.  It is passed the parameters FORM, START, END,
where:

- FORM is an Elisp form read from the current buffer.
- START and END are the beginning and end location of the
  corresponding data in the current buffer."
  (let ((sym (make-symbol "sym")))
    `(dolist (,sym ',symbols)
       (put ,sym 'semantic-elisp-form-parser #',parser))))
(put 'semantic-elisp-setup-form-parser 'lisp-indent-function 1)

(defmacro semantic-elisp-reuse-form-parser (symbol &rest symbols)
  "Reuse the form parser of SYMBOL for forms identified by SYMBOLS.
See also `semantic-elisp-setup-form-parser'."
  (let ((parser (make-symbol "parser"))
        (sym (make-symbol "sym")))
    `(let ((,parser (get ',symbol 'semantic-elisp-form-parser)))
       (or ,parser
           (signal 'wrong-type-argument
                   '(semantic-elisp-form-parser ,symbol)))
       (dolist (,sym ',symbols)
         (put ,sym 'semantic-elisp-form-parser ,parser)))))

(defun semantic-elisp-use-read (sl)
  "Use `read' on the semantic list SL.
Return a bovination list to use."
  (let* ((start (car sl))
         (end   (cdr sl))
         (form  (read (buffer-substring-no-properties start end))))
    (cond
     ;; If the first elt is a list, then it is some arbitrary code.
     ((listp (car form))
      (semantic-tag-new-code "anonymous" nil)
      )
     ;; A special form parser is provided, use it.
     ((and (car form) (symbolp (car form))
           (get (car form) 'semantic-elisp-form-parser))
      (funcall (get (car form) 'semantic-elisp-form-parser)
               form start end))
     ;; Produce a generic code tag by default.
     (t
      (semantic-tag-new-code (format "%S" (car form)) nil)
      ))))

;;; Form parsers
;;
(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-function
       (symbol-name (nth 2 form))
       nil
       '("form" "start" "end")
       :form-parser t
       ))
  semantic-elisp-setup-form-parser)

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((tags
             (condition-case foo
                 (semantic-parse-region start end nil 1)
               (error (message "MUNGE: %S" foo)
                      nil))))
        (if (semantic-tag-p (car-safe tags))
            tags
          (semantic-tag-new-code (format "%S" (car form)) nil))))
  eval-and-compile
  eval-when-compile
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-function
       (symbol-name (nth 1 form))
       nil
       (semantic-elisp-desymbolify-args (nth 2 form))
       :user-visible-flag (eq (car-safe (nth 4 form)) 'interactive)
       :documentation (semantic-elisp-do-doc (nth 3 form))
       :overloadable (or (eq (car form) 'define-overload)
			 (eq (car form) 'define-overloadable-function))
       ))
  defun
  defun*
  defsubst
  defmacro
  define-overload ;; @todo - remove after cleaning up semantic.
  define-overloadable-function
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((doc (semantic-elisp-form-to-doc-string (nth 3 form))))
        (semantic-tag-new-variable
         (symbol-name (nth 1 form))
         nil
         (nth 2 form)
         :user-visible-flag (and doc
                                 (> (length doc) 0)
                                 (= (aref doc 0) ?*))
         :constant-flag (eq (car form) 'defconst)
         :documentation (semantic-elisp-do-doc doc)
         )))
  defvar
  defconst
  defcustom
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((doc (semantic-elisp-form-to-doc-string (nth 3 form))))
        (semantic-tag-new-variable
         (symbol-name (nth 1 form))
         "face"
         (nth 2 form)
         :user-visible-flag (and doc
                                 (> (length doc) 0)
                                 (= (aref doc 0) ?*))
         :documentation (semantic-elisp-do-doc doc)
         )))
  defface
  )


(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((doc (semantic-elisp-form-to-doc-string (nth 3 form))))
        (semantic-tag-new-variable
         (symbol-name (nth 1 form))
         "image"
         (nth 2 form)
         :user-visible-flag (and doc
                                 (> (length doc) 0)
                                 (= (aref doc 0) ?*))
         :documentation (semantic-elisp-do-doc doc)
         )))
  defimage
  defezimage
  )


(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((doc (semantic-elisp-form-to-doc-string (nth 3 form))))
        (semantic-tag
         (symbol-name (nth 1 form))
         'customgroup
         :value (nth 2 form)
         :user-visible-flag t
         :documentation (semantic-elisp-do-doc doc)
         )))
  defgroup
  )


(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-function
       (symbol-name (cadr (cadr form)))
       nil nil
       :user-visible-flag (and (nth 4 form)
                               (not (eq (nth 4 form) 'nil)))
       :prototype-flag t
       :documentation (semantic-elisp-do-doc (nth 3 form))))
  autoload
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let* ((a2 (nth 2 form))
             (a3 (nth 3 form))
             (args (if (listp a2) a2 a3))
             (doc (nth (if (listp a2) 3 4) form)))
        (semantic-tag-new-function
         (symbol-name (nth 1 form))
         nil
         (if (listp (car args))
             (cons (symbol-name (caar args))
                   (semantic-elisp-desymbolify-args (cdr args)))
           (semantic-elisp-desymbolify-args (cdr args)))
         :parent (if (listp (car args)) (symbol-name (cadr (car args))) nil)
         :documentation (semantic-elisp-do-doc doc)
         )))
  defmethod
  defgeneric
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-function
       (symbol-name (nth 1 form))
       nil
       (semantic-elisp-desymbolify (nth 2 form))
       ))
  defadvice
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((docpart (nthcdr 4 form)))
	(semantic-tag-new-type
	 (symbol-name (nth 1 form))
         "class"
	 (semantic-elisp-clos-args-to-semantic (nth 3 form))
	 (semantic-elisp-desymbolify (nth 2 form))
	 :typemodifiers (semantic-elisp-desymbolify
			 (unless (stringp (car docpart)) docpart))
	 :documentation (semantic-elisp-do-doc
                         (if (stringp (car docpart))
                             (car docpart)
                           (cadr (member :documentation docpart))))
	 )))
  defclass
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((slots (nthcdr 2 form)))
        ;; Skip doc string if present.
        (and (stringp (car slots))
             (setq slots (cdr slots)))
        (semantic-tag-new-type
         (symbol-name (if (consp (nth 1 form))
                          (car (nth 1 form))
                        (nth 1 form)))
         "struct"
         (semantic-elisp-desymbolify slots)
         (cons nil nil)
         )))
  defstruct
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-function
       (symbol-name (nth 1 form))
       nil nil
       :lexical-analyzer-flag t
       :documentation (semantic-elisp-do-doc (nth 2 form))
       ))
  define-lex
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((args (nth 3 form)))
	(semantic-tag-new-function
	 (symbol-name (nth 1 form))
         nil
	 (and (listp args) (semantic-elisp-desymbolify args))
	 :override-function-flag t
	 :parent (symbol-name (nth 2 form))
	 :documentation (semantic-elisp-do-doc (nth 4 form))
	 )))
  define-mode-overload-implementation ;; obsoleted
  define-mode-local-override
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (semantic-tag-new-variable
       (symbol-name (nth 2 form))
       nil
       (nth 3 form)                     ; default value
       :override-variable-flag t
       :parent (symbol-name (nth 1 form))
       :documentation (semantic-elisp-do-doc (nth 4 form))
       ))
  defvar-mode-local
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((name (nth 1 form)))
        (semantic-tag-new-include
         (symbol-name (if (eq (car-safe name) 'quote)
                          (nth 1 name)
                        name))
         nil
         :directory (nth 2 form))))
  require
  )

(semantic-elisp-setup-form-parser
    (lambda (form start end)
      (let ((name (nth 1 form)))
        (semantic-tag-new-package
         (symbol-name (if (eq (car-safe name) 'quote)
                          (nth 1 name)
                        name))
         (nth 3 form))))
  provide
  )

;;; Mode setup
;;
(define-mode-local-override semantic-dependency-tag-file
  emacs-lisp-mode (tag)
  "Find the file BUFFER depends on described by TAG."
  (if (fboundp 'find-library-name)
      (condition-case nil
	  ;; Try an Emacs 22 fcn.  This throws errors.
	  (find-library-name (semantic-tag-name tag))
	(error
	 (message "semantic: cannot find source file %s"
		  (semantic-tag-name tag))))
    ;; No handy function available.  (Older Emacsen)
    (let* ((lib (locate-library (semantic-tag-name tag)))
	   (name (if lib (file-name-sans-extension lib) nil))
	   (nameel (concat name ".el")))
      (cond
       ((and name (file-exists-p nameel)) nameel)
       ((and name (file-exists-p (concat name ".el.gz")))
	;; This is the linux distro case.
	(concat name ".el.gz"))
       ;; source file does not exists
       (name
	(message "semantic: cannot find source file %s" (concat name ".el")))
       (t
	nil)))))

;;; DOC Strings
;;
(defun semantic-emacs-lisp-overridable-doc (tag)
  "Return the documentation string generated for overloadable functions.
Fetch the item for TAG.  Only returns info about what symbols can be
used to perform the override."
  (if (and (eq (semantic-tag-class tag) 'function)
	   (semantic-tag-get-attribute tag :overloadable))
      ;; Calc the doc to use for the overloadable symbols.
      (overload-docstring-extension (intern (semantic-tag-name tag)))
    ""))

(defun semantic-emacs-lisp-obsoleted-doc (tag)
  "Indicate that TAG is a new name that has obsoleted some old name.
Unfortunately, this requires that the tag in question has been loaded
into Emacs Lisp's memory."
  (let ((obsoletethis (intern-soft (semantic-tag-name tag)))
	(obsoleter nil))
    ;; This asks if our tag is available in the Emacs name space for querying.
    (when obsoletethis
      (mapatoms (lambda (a)
		  (let ((oi (get a 'byte-obsolete-info)))
		    (if (and oi (eq (car oi) obsoletethis))
			(setq obsoleter a)))))
      (if obsoleter
	  (format "\n@obsolete{%s,%s}" obsoleter (semantic-tag-name tag))
	""))))

(define-mode-local-override semantic-documentation-for-tag
  emacs-lisp-mode (tag &optional nosnarf)
  "Return the documentation string for TAG.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-tag-docstring tag)))
    (when (not d)
      (cond ((semantic-tag-with-position-p tag)
	     ;; Doc isn't in the tag itself.  Let's pull it out of the
	     ;; sources.
	     (let ((semantic-elisp-store-documentation-in-tag t))
	       (setq tag (with-current-buffer (semantic-tag-buffer tag)
			   (goto-char (semantic-tag-start tag))
			   (semantic-elisp-use-read
			    ;; concoct a lexical token.
			    (cons (semantic-tag-start tag)
				  (semantic-tag-end tag))))
		     d (semantic-tag-docstring tag))))
	    ;; The tag may be the result of a system search.
	    ((intern-soft (semantic-tag-name tag))
	     (let ((sym (intern-soft (semantic-tag-name tag))))
	       ;; Query into the global table o stuff.
	       (cond ((eq (semantic-tag-class tag) 'function)
		      (setq d (documentation sym)))
		     (t
		      (setq d (documentation-property
			       sym 'variable-documentation)))))
	     ;; Label it as system doc. perhaps just for debugging
	     ;; purposes.
	     (if d (setq d (concat "System Doc: \n" d)))
	     ))
      )

    (when d
      (concat
       (substitute-command-keys
        (if (and (> (length d) 0) (= (aref d 0) ?*))
            (substring d 1)
          d))
       (semantic-emacs-lisp-overridable-doc tag)
       (semantic-emacs-lisp-obsoleted-doc tag)))))

;;; Tag Features
;;
(define-mode-local-override semantic-tag-include-filename emacs-lisp-mode
  (tag)
  "Return the name of the tag with .el appended.
If there is a detail, prepend that directory."
  (let ((name (semantic-tag-name tag))
	(detail (semantic-tag-get-attribute tag :directory)))
    (concat (expand-file-name name detail) ".el")))

(define-mode-local-override semantic-insert-foreign-tag
  emacs-lisp-mode (tag)
  "Insert TAG at point.
Attempts a simple prototype for calling or using TAG."
  (cond ((semantic-tag-of-class-p tag 'function)
	 (insert "(" (semantic-tag-name tag) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-tag-name tag)))))

(define-mode-local-override semantic-tag-protection
  emacs-lisp-mode (tag &optional parent)
  "Return the protection of TAG in PARENT.
Override function for `semantic-tag-protection'."
  (let ((prot (semantic-tag-get-attribute tag :protection)))
    (cond
     ;; If a protection is not specified, AND there is a parent
     ;; data type, then it is public.
     ((and (not prot) parent) 'public)
     ((string= prot ":public") 'public)
     ((string= prot "public") 'public)
     ((string= prot ":private") 'private)
     ((string= prot "private") 'private)
     ((string= prot ":protected") 'protected)
     ((string= prot "protected") 'protected))))

(define-mode-local-override semantic-tag-static-p
  emacs-lisp-mode (tag &optional parent)
  "Return non-nil if TAG is static in PARENT class.
Overrides `semantic-nonterminal-static'."
  ;; This can only be true (theoretically) in a class where it is assigned.
  (semantic-tag-get-attribute tag :static-flag))

;;; Context parsing
;;
;; Emacs lisp is very different from C,C++ which most context parsing
;; functions are written.  Support them here.
(define-mode-local-override semantic-up-context emacs-lisp-mode
  (&optional point bounds-type)
  "Move up one context in an Emacs Lisp function.
A Context in many languages is a block with its own local variables.
In Emacs, we will move up lists and stop when one starts with one of
the following context specifiers:
  `let', `let*', `defun', `with-slots'
Returns non-nil it is not possible to go up a context."
  (let ((last-up (semantic-up-context-default)))
  (while
      (and (not (looking-at
		 "(\\(let\\*?\\|def\\(un\\|method\\|generic\\|\
define-mode-overload\\)\
\\|with-slots\\)"))
	   (not last-up))
    (setq last-up (semantic-up-context-default)))
  last-up))


(define-mode-local-override semantic-ctxt-current-function emacs-lisp-mode
  (&optional point same-as-symbol-return)
  "Return a string which is the current function being called."
  (save-excursion
    (if point (goto-char point) (setq point (point)))
    ;; (semantic-beginning-of-command)
    (if (condition-case nil
	    (and (save-excursion
		   (up-list -2)
		   (looking-at "(("))
		 (save-excursion
		   (up-list -3)
		   (looking-at "(let")))
	  (error nil))
	;; This is really a let statement, not a function.
	nil
      (let ((fun (condition-case nil
		     (save-excursion
		       (up-list -1)
		       (forward-char 1)
		       (buffer-substring-no-properties
			(point) (progn (forward-sexp 1)
				       (point))))
		   (error nil))
		 ))
	(when fun
	  ;; Do not return FUN IFF the cursor is on FUN.
	  ;; Huh?  Thats because if cursor is on fun, it is
	  ;; the current symbol, and not the current function.
	  (if (save-excursion
		(condition-case nil
		    (progn (forward-sexp -1)
			   (and
			    (looking-at (regexp-quote fun))
			    (<= point (+ (point) (length fun))))
			   )
		  (error t)))
	      ;; Go up and try again.
	      same-as-symbol-return
	    ;; We are ok, so get it.
	    (list fun))
	  ))
      )))


(define-mode-local-override semantic-get-local-variables emacs-lisp-mode
  (&optional point)
  "Return a list of local variables for POINT.
Scan backwards from point at each successive function.  For all occurrences
of `let' or `let*', grab those variable names."
  (let* ((vars nil)
	 (fn nil))
    (save-excursion
      (while (setq fn (car (semantic-ctxt-current-function-emacs-lisp-mode
			    (point) (list t))))
	(cond
	 ((eq fn t)
	  nil)
	 ((member fn '("let" "let*" "with-slots"))
	  ;; Snarf variables
	  (up-list -1)
	  (forward-char 1)
	  (forward-symbol 1)
	  (skip-chars-forward "* \t\n")
	  (let ((varlst (read (buffer-substring-no-properties
			       (point)
			       (save-excursion
				 (forward-sexp 1)
				 (point))))))
	    (while varlst
	      (let* ((oneelt (car varlst))
		     (name (if (symbolp oneelt)
			       oneelt
			     (car oneelt))))
		(setq vars (cons (semantic-tag-new-variable
				  (symbol-name name)
				  nil nil)
				 vars)))
	      (setq varlst (cdr varlst)))
	    ))
	 ((string= fn "lambda")
	  ;; Snart args...
	  (up-list -1)
	  (forward-char 1)
	  (forward-word 1)
	  (skip-chars-forward "* \t\n")
	  (let ((arglst (read (buffer-substring-no-properties
			       (point)
			       (save-excursion
				 (forward-sexp 1)
				 (point))))))
	    (while arglst
	      (let* ((name (car arglst)))
		(when (/= ?& (aref (symbol-name name) 0))
		  (setq vars (cons (semantic-tag-new-variable
				    (symbol-name name)
				    nil nil)
				   vars))))
	      (setq arglst (cdr arglst)))
	    ))
	 )
	(up-list -1)))
    (nreverse vars)))

(define-mode-local-override semantic-end-of-command emacs-lisp-mode
  ()
  "Move cursor to the end of the current command.
In Emacs Lisp this is easily defined by parenthesis bounding."
  (condition-case nil
      (up-list 1)
    (error nil)))

(define-mode-local-override semantic-beginning-of-command emacs-lisp-mode
  ()
  "Move cursor to the beginning of the current command.
In Emacs Lisp this is easily defined by parenthesis bounding."
  (condition-case nil
      (progn
        (up-list -1)
        (forward-char 1))
    (error nil)))

(define-mode-local-override semantic-ctxt-current-symbol emacs-lisp-mode
  (&optional point)
  "List the symbol under point."
  (save-excursion
    (if point (goto-char point))
    (require 'thingatpt)
    (let ((sym (thing-at-point 'symbol)))
      (if sym (list sym)))
    ))


(define-mode-local-override semantic-ctxt-current-assignment emacs-lisp-mode
  (&optional point)
  "What is the variable being assigned into at POINT?"
  (save-excursion
    (if point (goto-char point))
    (let ((fn (semantic-ctxt-current-function point))
	  (point (point)))
      ;; We should never get lists from here.
      (if fn (setq fn (car fn)))
      (cond
       ;; SETQ
       ((and fn (or (string= fn "setq") (string= fn "set")))
	(save-excursion
	  (condition-case nil
	      (let ((count 0)
		    (lastodd nil)
		    (start nil))
		(up-list -1)
		(down-list 1)
		(forward-sexp 1)
		;; Skip over sexp until we pass point.
		(while (< (point) point)
		  (setq count (1+ count))
		  (forward-comment 1)
		  (setq start (point))
		  (forward-sexp 1)
		  (if (= (% count 2) 1)
		      (setq lastodd
			    (buffer-substring-no-properties start (point))))
		  )
		(if lastodd (list lastodd))
		)
	    (error nil))))
       ;; This obscure thing finds let statements.
       ((condition-case nil
	    (and
	     (save-excursion
	       (up-list -2)
	       (looking-at "(("))
	     (save-excursion
	       (up-list -3)
	       (looking-at "(let")))
	  (error nil))
	(save-excursion
	  (semantic-beginning-of-command)
	  ;; Use func finding code, since it is the same format.
	  (semantic-ctxt-current-symbol)))
       ;;
       ;; DEFAULT- nothing
       (t nil))
      )))

(define-mode-local-override semantic-ctxt-current-argument emacs-lisp-mode
  (&optional point)
  "Return the index into the argument the cursor is in, or nil."
  (save-excursion
    (if point (goto-char point))
    (if (looking-at "\\<\\w")
	(forward-char 1))
    (let ((count 0))
      (while (condition-case nil
		 (progn
		   (forward-sexp -1)
		   t)
	       (error nil))
	(setq count (1+ count)))
      (cond ((= count 0)
	     0)
	    (t (1- count))))
    ))

(define-mode-local-override semantic-ctxt-current-class-list emacs-lisp-mode
  (&optional point)
  "Return a list of tag classes allowed at POINT.
Emacs Lisp knows much more about the class of the tag needed to perform
completion than some languages.  We distinctly know if we are to be a
function name, variable name, or any type of symbol.  We could identify
fields and such to, but that is for some other day."
  (save-excursion
    (if point (goto-char point))
    (setq point (point))
    (condition-case nil
	(let ((count 0))
	  (up-list -1)
	  (forward-char 1)
	  (while (< (point) point)
	    (setq count (1+ count))
	    (forward-sexp 1))
	  (if (= count 1)
	      '(function)
	    '(variable))
	  )
      (error '(variable)))
    ))

;;; Formatting
;;
(define-mode-local-override semantic-format-tag-abbreviate emacs-lisp-mode
  (tag &optional parent color)
  "Return an abbreviated string describing tag."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	)
    (cond
     ((eq class 'function)
      (concat "(" name ")"))
     (t
      (semantic-format-tag-abbreviate-default tag parent color)))))

(define-mode-local-override semantic-format-tag-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a prototype string describing tag.
In Emacs Lisp, a prototype for something may start (autoload ...).
This is certainly not expected if this is used to display a summary.
Make up something else.  When we go to write something that needs
a real Emacs Lisp prototype, we can fix it then."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	)
    (cond
     ((eq class 'function)
      (let* ((args  (semantic-tag-function-arguments tag))
	     (argstr (semantic--format-tag-arguments args
						     #'identity
						     color)))
	(concat "(" name (if args " " "")
		argstr
		")")))
     (t
      (semantic-format-tag-prototype-default tag parent color)))))

(define-mode-local-override semantic-format-tag-concise-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a concise prototype string describing tag.
See `semantic-format-tag-prototype' for Emacs Lisp for more details."
  (semantic-format-tag-prototype tag parent color))

(define-mode-local-override semantic-format-tag-uml-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a uml prototype string describing tag.
See `semantic-format-tag-prototype' for Emacs Lisp for more details."
  (semantic-format-tag-prototype tag parent color))

;;; IA Commands
;;
(define-mode-local-override semantic-ia-insert-tag
  emacs-lisp-mode (tag)
  "Insert TAG into the current buffer based on completion."
  ;; This function by David <de_bb@...> is a tweaked version of the original.
  (insert (semantic-tag-name tag))
  (let ((tt (semantic-tag-class tag))
	(args (semantic-tag-function-arguments tag)))
    (cond ((eq tt 'function)
	   (if args
	       (insert " ")
	     (insert ")")))
	  (t nil))))

;;; Lexical features and setup
;;
(defvar-mode-local emacs-lisp-mode semantic-lex-analyzer
  'semantic-emacs-lisp-lexer)

(defvar-mode-local emacs-lisp-mode semantic--parse-table
  semantic--elisp-parse-table)

(defvar-mode-local emacs-lisp-mode semantic-function-argument-separator
  " ")

(defvar-mode-local emacs-lisp-mode semantic-function-argument-separation-character
  " ")

(defvar-mode-local emacs-lisp-mode semantic-symbol->name-assoc-list
  '(
    (type     . "Types")
    (variable . "Variables")
    (function . "Defuns")
    (include  . "Requires")
    (package  . "Provides")
    ))

(defvar-mode-local emacs-lisp-mode imenu-create-index-function
  'semantic-create-imenu-index)

(defvar-mode-local emacs-lisp-mode semantic-stickyfunc-sticky-classes
  '(function type variable)
  "Add variables.
ELisp variables can be pretty long, so track this one too.")

(define-child-mode lisp-mode emacs-lisp-mode
  "Make `lisp-mode' inherit mode local behavior from `emacs-lisp-mode'.")

(defun semantic-default-elisp-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  )

(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

;;; LISP MODE
;;
;; @TODO: Lisp supports syntaxes that Emacs Lisp does not.
;;        Write a Lisp only parser someday.
;;
;; See this syntax:
;; (defun foo () /#A)
;;
(add-hook 'lisp-mode-hook 'semantic-default-elisp-setup)

(eval-after-load "semanticdb"
  '(require 'semantic/db-el)
  )

(provide 'semantic/bovine/el)

;;; semantic/bovine/el.el ends here
