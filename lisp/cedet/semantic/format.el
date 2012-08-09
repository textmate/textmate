;;; semantic/format.el --- Routines for formatting tags

;; Copyright (C) 1999-2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
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
;; Once a language file has been parsed into a TAG, it is often useful
;; then display that tag information in browsers, completion engines, or
;; help routines.  The functions and setup in this file provide ways
;; to reformat a tag into different standard output types.
;;
;; In addition, macros for setting up customizable variables that let
;; the user choose their default format type are also provided.
;;

;;; Code:
(eval-when-compile (require 'font-lock))
(require 'semantic)
(require 'semantic/tag-ls)
(require 'ezimage)

(eval-when-compile (require 'semantic/find))

;;; Tag to text overload functions
;;
;; abbreviations, prototypes, and coloring support.
(defvar semantic-format-tag-functions
  '(semantic-format-tag-name
    semantic-format-tag-canonical-name
    semantic-format-tag-abbreviate
    semantic-format-tag-summarize
    semantic-format-tag-summarize-with-file
    semantic-format-tag-short-doc
    semantic-format-tag-prototype
    semantic-format-tag-concise-prototype
    semantic-format-tag-uml-abbreviate
    semantic-format-tag-uml-prototype
    semantic-format-tag-uml-concise-prototype
    semantic-format-tag-prin1
    )
  "List of functions which convert a tag to text.
Each function must take the parameters TAG &optional PARENT COLOR.
TAG is the tag to convert.
PARENT is a parent tag or name which refers to the structure
or class which contains TAG.  PARENT is NOT a class which a TAG
would claim as a parent.
COLOR indicates that the generated text should be colored using
`font-lock'.")

(defvar semantic-format-tag-custom-list
  (append '(radio)
	  (mapcar (lambda (f) (list 'const f))
		  semantic-format-tag-functions)
	  '(function))
  "A List used by customizable variables to choose a tag to text function.
Use this variable in the :type field of a customizable variable.")

(defcustom semantic-format-use-images-flag ezimage-use-images
  "Non-nil means semantic format functions use images.
Images can be used as icons instead of some types of text strings."
  :group 'semantic
  :type 'boolean)

(defvar semantic-function-argument-separator ","
  "Text used to separate arguments when creating text from tags.")
(make-variable-buffer-local 'semantic-function-argument-separator)

(defvar semantic-format-parent-separator "::"
  "Text used to separate names when between namespaces/classes and functions.")
(make-variable-buffer-local 'semantic-format-parent-separator)

(defvar semantic-format-face-alist
  `( (function . font-lock-function-name-face)
     (variable . font-lock-variable-name-face)
     (type . font-lock-type-face)
     ;; These are different between Emacsen.
     (include . ,(if (featurep 'xemacs)
		     'font-lock-preprocessor-face
		   'font-lock-constant-face))
     (package . ,(if (featurep 'xemacs)
		     'font-lock-preprocessor-face
		   'font-lock-constant-face))
     ;; Not a tag, but instead a feature of output
     (label . font-lock-string-face)
     (comment . font-lock-comment-face)
     (keyword . font-lock-keyword-face)
     (abstract . italic)
     (static . underline)
     (documentation . font-lock-doc-face)
     )
  "Face used to colorize tags of different types.
Override the value locally if a language supports other tag types.
When adding new elements, try to use symbols also returned by the parser.
The form of an entry in this list is of the form:
 ( SYMBOL .  FACE )
where SYMBOL is a tag type symbol used with semantic.  FACE
is a symbol representing a face.
Faces used are generated in `font-lock' for consistency, and will not
be used unless font lock is a feature.")


;;; Coloring Functions
;;
(defun semantic--format-colorize-text (text face-class)
  "Apply onto TEXT a color associated with FACE-CLASS.
FACE-CLASS is a tag type found in `semantic-format-face-alist'.
See that variable for details on adding new types."
  (if (featurep 'font-lock)
      (let ((face (cdr-safe (assoc face-class semantic-format-face-alist)))
	    (newtext (concat text)))
	(put-text-property 0 (length text) 'face face newtext)
	newtext)
    text))

(defun semantic--format-colorize-merge-text (precoloredtext face-class)
  "Apply onto PRECOLOREDTEXT a color associated with FACE-CLASS.
FACE-CLASS is a tag type found in `semantic-formatface-alist'.
See that variable for details on adding new types."
  (let ((face (cdr-safe (assoc face-class semantic-format-face-alist)))
	(newtext (concat precoloredtext))
	)
    (if (featurep 'xemacs)
	(add-text-properties 0 (length newtext) (list 'face face) newtext)
      (alter-text-property 0 (length newtext) 'face
			   (lambda (current-face)
			     (let ((cf
				    (cond ((facep current-face)
					   (list current-face))
					  ((listp current-face)
					   current-face)
					  (t nil)))
				   (nf
				    (cond ((facep face)
					   (list face))
					  ((listp face)
					   face)
					  (t nil))))
			       (append cf nf)))
			   newtext))
    newtext))

;;; Function Arguments
;;
(defun semantic--format-tag-arguments (args formatter color)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."
  (let ((out nil))
    (while args
      (push (if (and formatter
		     (semantic-tag-p (car args))
		     (not (string= (semantic-tag-name (car args)) ""))
		     )
		(funcall formatter (car args) nil color)
	      (semantic-format-tag-name-from-anything
	       (car args) nil color 'variable))
	    out)
      (setq args (cdr args)))
    (mapconcat 'identity (nreverse out) semantic-function-argument-separator)
    ))

;;; Data Type
(define-overloadable-function semantic-format-tag-type (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
It is presumed that TYPE is a string or semantic tag.")

(defun semantic-format-tag-type-default (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
Argument COLOR specifies to colorize the text."
  (let* ((type (semantic-tag-type tag))
	 (out (cond ((semantic-tag-p type)
		     (let* ((typetype (semantic-tag-type type))
			    (name (semantic-tag-name type))
			    (str (if typetype
				     (concat typetype " " name)
				   name)))
		       (if color
			   (semantic--format-colorize-text
			    str
			    'type)
			 str)))
		    ((and (listp type)
			  (stringp (car type)))
		     (car type))
		    ((stringp type)
		     type)
		    (t nil))))
    (if (and color out)
	(setq out (semantic--format-colorize-text out 'type))
      out)
    ))


;;; Abstract formatting functions
;;

(defun semantic-format-tag-prin1 (tag &optional parent color)
  "Convert TAG to a string that is the print name for TAG.
PARENT and COLOR are ignored."
  (format "%S" tag))

(defun semantic-format-tag-name-from-anything (anything &optional
							parent color
							colorhint)
  "Convert just about anything into a name like string.
Argument ANYTHING is the thing to be converted.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.
Optional COLORHINT is the type of color to use if ANYTHING is not a tag
with a tag class.  See `semantic--format-colorize-text' for a definition
of FACE-CLASS for which this is used."
  (cond ((stringp anything)
	 (semantic--format-colorize-text anything colorhint))
	((semantic-tag-p anything)
	 (let ((ans (semantic-format-tag-name anything parent color)))
	   ;; If ANS is empty string or nil, then the name wasn't
	   ;; supplied.  The implication is as in C where there is a data
	   ;; type but no name for a prototype from an include file, or
	   ;; an argument just wasn't used in the body of the fcn.
	   (if (or (null ans) (string= ans ""))
	       (setq ans (semantic-format-tag-type anything color)))
	   ans))
	((and (listp anything)
	      (stringp (car anything)))
	 (semantic--format-colorize-text (car anything) colorhint))))

;;;###autoload
(define-overloadable-function semantic-format-tag-name (tag &optional parent color)
  "Return the name string describing TAG.
The name is the shortest possible representation.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-name-default (tag &optional parent color)
  "Return an abbreviated string describing TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let ((name (semantic-tag-name tag))
	(destructor
	 (if (eq (semantic-tag-class tag) 'function)
	     (semantic-tag-function-destructor-p tag))))
    (when destructor
      (setq name (concat "~" name)))
    (if color
	(setq name (semantic--format-colorize-text name (semantic-tag-class tag))))
    name))

(declare-function semantic-go-to-tag "semantic/tag-file")

(defun semantic--format-tag-parent-tree (tag parent)
  "Under Consideration.

Return a list of parents for TAG.
PARENT is the first parent, or nil.  If nil, then an attempt to
determine PARENT is made.
Once PARENT is identified, additional parents are looked for.
The return list first element is the nearest parent, and the last
item is the first parent which may be a string.  The root parent may
not be the actual first parent as there may just be a failure to find
local definitions."
  ;; First, validate the PARENT argument.
  (unless parent
    ;; All mechanisms here must be fast as often parent
    ;; is nil because there isn't one.
    (setq parent (or (semantic-tag-function-parent tag)
		     (save-excursion
		       (require 'semantic/tag-file)
		       (semantic-go-to-tag tag)
		       (semantic-current-tag-parent)))))
  (when (stringp parent)
    (setq parent (semantic-find-first-tag-by-name
		  parent (current-buffer))))
  ;; Try and find a trail of parents from PARENT
  (let ((rlist (list parent))
	)
    ;; IMPLEMENT ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (reverse rlist)))

(define-overloadable-function semantic-format-tag-canonical-name (tag &optional parent color)
  "Return a canonical name for TAG.
A canonical name includes the names of any parents or namespaces preceding
the tag.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-canonical-name-default (tag &optional parent color)
  "Return a canonical name for TAG.
A canonical name includes the names of any parents or namespaces preceding
the tag with colons separating them.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let ((parent-input-str
	 (if (and parent
		  (semantic-tag-p parent)
		  (semantic-tag-of-class-p parent 'type))
	     (concat
	      ;; Choose a class of 'type as the default parent for something.
	      ;; Just a guess though.
	      (semantic-format-tag-name-from-anything parent nil color 'type)
	      ;; Default separator between class/namespace and others.
	      semantic-format-parent-separator)
	   ""))
	(tag-parent-str
	 (or (when (and (semantic-tag-of-class-p tag 'function)
			(semantic-tag-function-parent tag))
	       (concat (semantic-tag-function-parent tag)
		       semantic-format-parent-separator))
	     ""))
	)
    (concat parent-input-str
	    tag-parent-str
	    (semantic-format-tag-name tag parent color))
    ))

(define-overloadable-function semantic-format-tag-abbreviate (tag &optional parent color)
  "Return an abbreviated string describing TAG.
The abbreviation is to be short, with possible symbols indicating
the type of tag, or other information.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-abbreviate-default (tag &optional parent color)
  "Return an abbreviated string describing TAG.
Optional argument PARENT is a parent tag in the tag hierarchy.
In this case PARENT refers to containment, not inheritance.
Optional argument COLOR means highlight the prototype with font-lock colors.
This is a simple C like default."
  ;; Do lots of complex stuff here.
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-canonical-name tag parent color))
	(suffix "")
	(prefix "")
	str)
    (cond ((eq class 'function)
	   (setq suffix "()"))
	  ((eq class 'include)
	   (setq suffix "<>"))
	  ((eq class 'variable)
	   (setq suffix (if (semantic-tag-variable-default tag)
			    "=" "")))
	  ((eq class 'label)
	   (setq suffix ":"))
	  ((eq class 'code)
	   (setq prefix "{"
		 suffix "}"))
	  ((eq class 'type)
	   (setq suffix "{}"))
	  )
    (setq str (concat prefix name suffix))
    str))

;;;###autoload
(define-overloadable-function semantic-format-tag-summarize (tag &optional parent color)
  "Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-summarize-default (tag &optional parent color)
  "Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((proto (semantic-format-tag-prototype tag nil color))
	 (names (if parent
		    semantic-symbol->name-assoc-list-for-type-parts
		  semantic-symbol->name-assoc-list))
	 (tsymb (semantic-tag-class tag))
	 (label (capitalize (or (cdr-safe (assoc tsymb names))
				(symbol-name tsymb)))))
    (if color
	(setq label (semantic--format-colorize-text label 'label)))
    (concat label ": " proto)))

(define-overloadable-function semantic-format-tag-summarize-with-file (tag &optional parent color)
  "Like `semantic-format-tag-summarize', but with the file name.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-summarize-with-file-default (tag &optional parent color)
  "Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((proto (semantic-format-tag-prototype tag nil color))
	 (file (semantic-tag-file-name tag))
	 )
    ;; Nothing for tag?  Try parent.
    (when (and (not file) (and parent))
      (setq file (semantic-tag-file-name parent)))
    ;; Don't include the file name if we can't find one, or it is the
    ;; same as the current buffer.
    (if (or (not file)
	    (string= file (buffer-file-name (current-buffer))))
	proto
      (setq file (file-name-nondirectory file))
      (when color
	(setq file (semantic--format-colorize-text file 'label)))
      (concat file ": " proto))))

(define-overloadable-function semantic-format-tag-short-doc (tag &optional parent color)
  "Display a short form of TAG's documentation. (Comments, or docstring.)
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(declare-function semantic-documentation-for-tag "semantic/doc")

(defun semantic-format-tag-short-doc-default (tag &optional parent color)
  "Display a short form of TAG's documentation.  (Comments, or docstring.)
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((fname (or (semantic-tag-file-name tag)
		    (when parent (semantic-tag-file-name parent))))
	 (buf (or (semantic-tag-buffer tag)
		  (when parent (semantic-tag-buffer parent))))
	 (doc (semantic-tag-docstring tag buf)))
    (when (and (not doc) (not buf) fname)
      ;; If there is no doc, and no buffer, but we have a filename,
      ;; let's try again.
      (save-match-data
	(setq buf (find-file-noselect fname)))
      (setq doc (semantic-tag-docstring tag buf)))
    (when (not doc)
      (require 'semantic/doc)
      (setq doc (semantic-documentation-for-tag tag))
      )
    (setq doc
	  (if (not doc)
	      ;; No doc, use summarize.
	      (semantic-format-tag-summarize tag parent color)
	    ;; We have doc.  Can we devise a single line?
	    (if (string-match "$" doc)
		(substring doc 0 (match-beginning 0))
	      doc)
	    ))
    (when color
      (setq doc (semantic--format-colorize-text doc 'documentation)))
    doc
    ))

;;; Prototype generation
;;
;;;###autoload
(define-overloadable-function semantic-format-tag-prototype (tag &optional parent color)
  "Return a prototype for TAG.
This function should be overloaded, though it need not be used.
This is because it can be used to create code by language independent
tools.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-prototype-default (tag &optional parent color)
  "Default method for returning a prototype for TAG.
This will work for C like languages.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (name (semantic-format-tag-name tag parent color))
	 (type (if (member class '(function variable type))
		   (semantic-format-tag-type tag color)))
	 (args (if (member class '(function type))
		   (semantic--format-tag-arguments
		    (if (eq class 'function)
			(semantic-tag-function-arguments tag)
		      (list "")
		      ;;(semantic-tag-type-members tag)
		      )
		    #'semantic-format-tag-prototype
		    color)))
	 (const (semantic-tag-get-attribute tag :constant-flag))
	 (tm (semantic-tag-get-attribute tag :typemodifiers))
	 (mods (append
		(if const '("const") nil)
		(cond ((stringp tm) (list tm))
		      ((consp tm) tm)
		      (t nil))
		))
	 (array (if (eq class 'variable)
		    (let ((deref
			   (semantic-tag-get-attribute
 			    tag :dereference))
 			  (r ""))
 		      (while (and deref (/= deref 0))
 			(setq r (concat r "[]")
 			      deref (1- deref)))
 		      r)))
 	 )
    (if args
	(setq args
	      (concat " "
		      (if (eq class 'type) "{" "(")
		      args
		      (if (eq class 'type) "}" ")"))))
    (when mods
      (setq mods (concat (mapconcat 'identity mods " ") " ")))
    (concat (or mods "")
	    (if type (concat type " "))
	    name
	    (or args "")
	    (or array ""))))

;;;###autoload
(define-overloadable-function semantic-format-tag-concise-prototype (tag &optional parent color)
  "Return a concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-concise-prototype-default (tag &optional parent color)
  "Return a concise prototype for TAG.
This default function will make a cheap concise prototype using C like syntax.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let ((class (semantic-tag-class tag)))
    (cond
     ((eq class 'type)
      (concat (semantic-format-tag-name tag parent color) "{}"))
     ((eq class 'function)
      (concat (semantic-format-tag-name tag parent color)
	      " ("
	      (semantic--format-tag-arguments
	       (semantic-tag-function-arguments tag)
	       'semantic-format-tag-concise-prototype
	       color)
	      ")"))
     ((eq class 'variable)
      (let* ((deref (semantic-tag-get-attribute
		     tag :dereference))
	     (array "")
	     )
	(while (and deref (/= deref 0))
	  (setq array (concat array "[]")
		deref (1- deref)))
	(concat (semantic-format-tag-name tag parent color)
		array)))
     (t
      (semantic-format-tag-abbreviate tag parent color)))))

;;; UML display styles
;;
(defcustom semantic-uml-colon-string " : "
  "*String used as a color separator between parts of a UML string.
In UML, a variable may appear as `varname : type'.
Change this variable to change the output separator."
  :group 'semantic
  :type 'string)

(defcustom semantic-uml-no-protection-string ""
  "*String used to describe when no protection is specified.
Used by `semantic-format-tag-uml-protection-to-string'."
  :group 'semantic
  :type 'string)

(defun semantic--format-uml-post-colorize (text tag parent)
  "Add color to TEXT created from TAG and PARENT.
Adds augmentation for `abstract' and `static' entries."
  (if (semantic-tag-abstract-p tag parent)
      (setq text (semantic--format-colorize-merge-text text 'abstract)))
  (if (semantic-tag-static-p tag parent)
      (setq text (semantic--format-colorize-merge-text text 'static)))
  text
  )

(defun semantic-uml-attribute-string (tag &optional parent)
  "Return a string for TAG, a child of PARENT representing a UML attribute.
UML attribute strings are things like {abstract} or {leaf}."
  (cond ((semantic-tag-abstract-p tag parent)
	 "{abstract}")
	((semantic-tag-leaf-p tag parent)
	 "{leaf}")
	))

(defvar semantic-format-tag-protection-image-alist
  '(("+" . ezimage-unlock)
    ("#" . ezimage-key)
    ("-" . ezimage-lock)
    )
  "Association of protection strings, and images to use.")

(defvar semantic-format-tag-protection-symbol-to-string-assoc-list
  '((public . "+")
    (protected . "#")
    (private . "-")
    )
  "Association list of the form (SYMBOL . \"STRING\") for protection symbols.
This associates a symbol, such as 'public with the st ring \"+\".")

(define-overloadable-function semantic-format-tag-uml-protection-to-string (protection-symbol color)
  "Convert PROTECTION-SYMBOL to a string for UML.
By default, uses `semantic-format-tag-protection-symbol-to-string-assoc-list'
to convert.
By default character returns are:
  public    -- +
  private   -- -
  protected -- #.
If PROTECTION-SYMBOL is unknown, then the return value is
`semantic-uml-no-protection-string'.
COLOR indicates if we should use an image on the text.")

(defun semantic-format-tag-uml-protection-to-string-default (protection-symbol color)
  "Convert PROTECTION-SYMBOL to a string for UML.
Uses `semantic-format-tag-protection-symbol-to-string-assoc-list' to convert.
If PROTECTION-SYMBOL is unknown, then the return value is
`semantic-uml-no-protection-string'.
COLOR indicates if we should use an image on the text."
  (let* ((ezimage-use-images (and semantic-format-use-images-flag color))
	 (key (assoc protection-symbol
		     semantic-format-tag-protection-symbol-to-string-assoc-list))
	 (str (or (cdr-safe key) semantic-uml-no-protection-string)))
    (ezimage-image-over-string
     (copy-sequence str)  ; make a copy to keep the original pristine.
     semantic-format-tag-protection-image-alist)))

(defsubst semantic-format-tag-uml-protection (tag parent color)
  "Retrieve the protection string for TAG with PARENT.
Argument COLOR specifies that color should be added to the string as
needed."
  (semantic-format-tag-uml-protection-to-string
   (semantic-tag-protection tag parent)
   color))

(defun semantic--format-tag-uml-type (tag color)
  "Format the data type of TAG to a string usable for formatting.
COLOR indicates if it should be colorized."
  (let ((str (semantic-format-tag-type tag color)))
    (if str
	(concat semantic-uml-colon-string str))))

(define-overloadable-function semantic-format-tag-uml-abbreviate (tag &optional parent color)
  "Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-abbreviate-default (tag &optional parent color)
  "Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((name (semantic-format-tag-name tag parent color))
	 (type  (semantic--format-tag-uml-type tag color))
	 (protstr (semantic-format-tag-uml-protection tag parent color))
	 (text nil))
    (setq text
	  (concat
	   protstr
	   (if type (concat name type)
	     name)))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text))

(define-overloadable-function semantic-format-tag-uml-prototype (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-prototype-default (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (cp (semantic-format-tag-name tag parent color))
	 (type (semantic--format-tag-uml-type tag color))
	 (prot (semantic-format-tag-uml-protection tag parent color))
	 (argtext
	  (cond ((eq class 'function)
		 (concat
		  " ("
		  (semantic--format-tag-arguments
		   (semantic-tag-function-arguments tag)
		   #'semantic-format-tag-uml-prototype
		   color)
		  ")"))
		((eq class 'type)
		 "{}")))
	 (text nil))
    (setq text (concat prot cp argtext type))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text
    ))

(define-overloadable-function semantic-format-tag-uml-concise-prototype (tag &optional parent color)
  "Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-concise-prototype-default (tag &optional parent color)
  "Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((cp (semantic-format-tag-concise-prototype tag parent color))
	 (type (semantic--format-tag-uml-type tag color))
	 (prot (semantic-format-tag-uml-protection tag parent color))
	 (text nil)
	 )
    (setq text (concat prot cp type))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text))

(provide 'semantic/format)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/format"
;; End:

;;; semantic/format.el ends here
