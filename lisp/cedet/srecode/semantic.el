;;; srecode/semantic.el --- Semantic specific extensions to SRecode.

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Semantic specific extensions to the Semantic Recoder.
;;
;; I realize it is the "Semantic Recoder", but most of srecode
;; is a template library and set of user interfaces unrelated to
;; semantic in the specific.
;;
;; This file defines the following:
;;   - :tag argument handling.
;;   - <more goes here>

;;; Code:

(require 'srecode/insert)
(require 'srecode/dictionary)
(require 'semantic/find)
(require 'semantic/format)
(require 'semantic/senator)
(require 'ring)


;;; The SEMANTIC TAG inserter
;;
;; Put a tag into the dictionary that can be used w/ arbitrary
;; lisp expressions.

(defclass srecode-semantic-tag (srecode-dictionary-compound-value)
  ((prime :initarg :prime
	  :type semantic-tag
	  :documentation
	  "This is the primary insertion tag.")
   )
  "Wrap up a collection of semantic tag information.
This class will be used to derive dictionary values.")

(defmethod srecode-compound-toString((cp srecode-semantic-tag)
				     function
				     dictionary)
  "Convert the compound dictionary value CP to a string.
If FUNCTION is non-nil, then FUNCTION is somehow applied to an
aspect of the compound value."
  (if (not function)
      ;; Just format it in some handy dandy way.
      (semantic-format-tag-prototype (oref cp :prime))
    ;; Otherwise, apply the function to the tag itself.
    (funcall function (oref cp :prime))
    ))


;;; Managing the `current' tag
;;

(defvar srecode-semantic-selected-tag nil
  "The tag selected by a :tag template argument.
If this is nil, then `senator-tag-ring' is used.")

(defun srecode-semantic-tag-from-kill-ring ()
  "Create an `srecode-semantic-tag' from the senator kill ring."
  (if (ring-empty-p senator-tag-ring)
      (error "You must use `senator-copy-tag' to provide a tag to this template"))
  (ring-ref senator-tag-ring 0))


;;; TAG in a DICTIONARY
;;
(defvar srecode-semantic-apply-tag-augment-hook nil
  "A function called for each tag added to a dictionary.
The hook is called with two arguments, the TAG and DICT
to be augmented.")

(define-overload srecode-semantic-apply-tag-to-dict (tagobj dict)
  "Insert features of TAGOBJ into the dictionary DICT.
TAGOBJ is an object of class `srecode-semantic-tag'.  This class
is a compound inserter value.
DICT is a dictionary object.
At a minimum, this function will create dictionary macro for NAME.
It is also likely to create macros for TYPE (data type), function arguments,
variable default values, and other things."
  )

(defun srecode-semantic-apply-tag-to-dict-default (tagobj dict)
  "Insert features of TAGOBJ into dictionary DICT."
  ;; Store the sst into the dictionary.
  (srecode-dictionary-set-value dict "TAG" tagobj)

  ;; Pull out the tag for the individual pieces.
  (let ((tag (oref tagobj :prime)))

    (srecode-dictionary-set-value dict "NAME" (semantic-tag-name tag))
    (srecode-dictionary-set-value dict "TYPE" (semantic-format-tag-type tag nil))

    (run-hook-with-args 'srecode-semantic-apply-tag-augment-hook tag dict)

    (cond
     ;;
     ;; FUNCTION
     ;;
     ((eq (semantic-tag-class tag) 'function)
      ;; FCN ARGS
      (let ((args (semantic-tag-function-arguments tag)))
	(while args
	  (let ((larg (car args))
		(subdict (srecode-dictionary-add-section-dictionary
			  dict "ARGS")))
	    ;; Clean up elements in the arg list.
	    (if (stringp larg)
		(setq larg (semantic-tag-new-variable
			    larg nil nil)))
	    ;; Apply the sub-argument to the subdictionary.
	    (srecode-semantic-apply-tag-to-dict
	     (srecode-semantic-tag (semantic-tag-name larg)
				   :prime larg)
	     subdict)
	    )
	  ;; Next!
	  (setq args (cdr args))))
      ;; PARENTS
      (let ((p (semantic-tag-function-parent tag)))
	(when p
	  (srecode-dictionary-set-value dict "PARENT" p)
	  ))
      ;; EXCEPTIONS (java/c++)
      (let ((exceptions (semantic-tag-get-attribute tag :throws)))
	(while exceptions
	  (let ((subdict (srecode-dictionary-add-section-dictionary
			  dict "THROWS")))
	    (srecode-dictionary-set-value subdict "NAME" (car exceptions))
	    )
	  (setq exceptions (cdr exceptions)))
	)
      )
     ;;
     ;; VARIABLE
     ;;
     ((eq (semantic-tag-class tag) 'variable)
      (when (semantic-tag-variable-default tag)
	(let ((subdict (srecode-dictionary-add-section-dictionary
			dict "HAVEDEFAULT")))
	  (srecode-dictionary-set-value
	   subdict "VALUE" (semantic-tag-variable-default tag))))
      )
     ;;
     ;; TYPE
     ;;
     ((eq (semantic-tag-class tag) 'type)
      (dolist (p (semantic-tag-type-superclasses tag))
	(let ((sd (srecode-dictionary-add-section-dictionary
		   dict "PARENTS")))
	  (srecode-dictionary-set-value sd "NAME" p)
	  ))
      (dolist (i (semantic-tag-type-interfaces tag))
	(let ((sd (srecode-dictionary-add-section-dictionary
		   dict "INTERFACES")))
	  (srecode-dictionary-set-value sd "NAME" i)
	  ))
; NOTE : The members are too complicated to do via a template.
;        do it via the insert-tag solution instead.
;
;      (dolist (mem (semantic-tag-type-members tag))
;	(let ((subdict (srecode-dictionary-add-section-dictionary
;			dict "MEMBERS")))
;	  (when (stringp mem)
;	    (setq mem (semantic-tag-new-variable mem nil nil)))
;	  (srecode-semantic-apply-tag-to-dict
;	   (srecode-semantic-tag (semantic-tag-name mem)
;				 :prime mem)
;	   subdict)))
      ))))


;;; ARGUMENT HANDLERS

;;; :tag ARGUMENT HANDLING
;;
;; When a :tag argument is required, identify the current :tag,
;; and apply its parts into the dictionary.
(defun srecode-semantic-handle-:tag (dict)
  "Add macros into the dictionary DICT based on the current :tag."
  ;; We have a tag, start adding "stuff" into the dictionary.
  (let ((tag (or srecode-semantic-selected-tag
		 (srecode-semantic-tag-from-kill-ring))))
    (when (not tag)
      "No tag for current template.  Use the semantic kill-ring.")
    (srecode-semantic-apply-tag-to-dict
     (srecode-semantic-tag (semantic-tag-name tag)
			   :prime tag)
     dict)))

;;; :tagtype ARGUMENT HANDLING
;;
;; When a :tagtype argument is required, identify the current tag, of
;; cf class 'type.  Apply those parameters to the dictionary.

(defun srecode-semantic-handle-:tagtype (dict)
  "Add macros into the dictionary DICT based on a tag of class type at point.
Assumes the cursor is in a tag of class type.  If not, throw an error."
  (let ((typetag (or srecode-semantic-selected-tag
		     (semantic-current-tag-of-class 'type))))
    (when (not typetag)
      (error "Cursor is not in a TAG of class 'type"))
    (srecode-semantic-apply-tag-to-dict
     typetag
     dict)))


;;; INSERT A TAG API
;;
;; Routines that take a tag, and insert into a buffer.
(define-overload srecode-semantic-find-template (class prototype ctxt)
  "Find a template for a tag of class CLASS based on context.
PROTOTYPE is non-nil if we want a prototype template instead."
  )

(defun srecode-semantic-find-template-default (class prototype ctxt)
  "Find a template for tag CLASS based on context.
PROTOTYPE is non-nil if we need a prototype.
CTXT is the pre-calculated context."
  (let* ((top (car ctxt))
	 (tname (if (stringp class)
		    class
		  (symbol-name class)))
	 (temp nil)
	 )
    ;; Try to find a template.
    (setq temp (or
		(when prototype
		  (srecode-template-get-table (srecode-table)
					      (concat tname "-tag-prototype")
					      top))
		(when prototype
		  (srecode-template-get-table (srecode-table)
					      (concat tname "-prototype")
					      top))
		(srecode-template-get-table (srecode-table)
					    (concat tname "-tag")
					    top)
		(srecode-template-get-table (srecode-table)
					    tname
					    top)
		(when (and (not (string= top "declaration"))
			   prototype)
		  (srecode-template-get-table (srecode-table)
					      (concat tname "-prototype")
					      "declaration"))
		(when (and (not (string= top "declaration"))
			   prototype)
		  (srecode-template-get-table (srecode-table)
					      (concat tname "-tag-prototype")
					      "declaration"))
		(when (not (string= top "declaration"))
		  (srecode-template-get-table (srecode-table)
					      (concat tname "-tag")
					      "declaration"))
		(when (not (string= top "declaration"))
		  (srecode-template-get-table (srecode-table)
					      tname
					      "declaration"))
		))
    temp))

(defun srecode-semantic-insert-tag (tag &optional style-option
					point-insert-fcn
					&rest dict-entries)
  "Insert TAG into a buffer using srecode templates at point.

Optional STYLE-OPTION is a list of minor configuration of styles,
such as the symbol 'prototype for prototype functions, or
'system for system includes, and 'doxygen, for a doxygen style
comment.

Optional third argument POINT-INSERT-FCN is a hook that is run after
TAG is inserted that allows an opportunity to fill in the body of
some thing.  This hook function is called with one argument, the TAG
being inserted.

The rest of the arguments are DICT-ENTRIES.  DICT-ENTRIES
is of the form ( NAME1 VALUE1 NAME2 VALUE2 ... NAMEn VALUEn).

The exact template used is based on the current context.
The template used is found within the toplevel context as calculated
by `srecode-calculate-context', such as `declaration', `classdecl',
or `code'.

For various conditions, this function looks for a template with
the name CLASS-tag, where CLASS is the tag class.  If it cannot
find that, it will look for that template in the `declaration'
context (if the current context was not `declaration').

If PROTOTYPE is specified, it will first look for templates with
the name CLASS-tag-prototype, or CLASS-prototype as above.

See `srecode-semantic-apply-tag-to-dict' for details on what is in
the dictionary when the templates are called.

This function returns to location in the buffer where the
inserted tag ENDS, and will leave point inside the inserted
text based on any occurrence of a point-inserter.  Templates such
as `function' will leave point where code might be inserted."
  (srecode-load-tables-for-mode major-mode)
  (let* ((ctxt (srecode-calculate-context))
	 (top (car ctxt))
	 (tname (symbol-name (semantic-tag-class tag)))
	 (dict (srecode-create-dictionary))
	 (temp nil)
	 (errtype tname)
	 (prototype (memq 'prototype style-option))
	 )
    ;; Try some special cases.
    (cond ((and (semantic-tag-of-class-p tag 'function)
		(semantic-tag-get-attribute tag :constructor-flag))
	   (setq temp (srecode-semantic-find-template
		       "constructor" prototype ctxt))
	   )

	  ((and (semantic-tag-of-class-p tag 'function)
		(semantic-tag-get-attribute tag :destructor-flag))
	   (setq temp (srecode-semantic-find-template
		       "destructor" prototype ctxt))
	   )

	  ((and (semantic-tag-of-class-p tag 'function)
		(semantic-tag-function-parent tag))
	   (setq temp (srecode-semantic-find-template
		       "method" prototype ctxt))
	   )

	  ((and (semantic-tag-of-class-p tag 'variable)
		(semantic-tag-get-attribute tag :constant-flag))
	   (setq temp (srecode-semantic-find-template
		       "variable-const" prototype ctxt))
	   )
	  )

    (when (not temp)
      ;; Try the basics
      (setq temp (srecode-semantic-find-template
		  tname prototype ctxt)))

    ;; Try some backup template names.
    (when (not temp)
      (cond
       ;; Types might split things up based on the type's type.
       ((and (eq (semantic-tag-class tag) 'type)
	     (semantic-tag-type tag))
	(setq temp (srecode-semantic-find-template
		    (semantic-tag-type tag) prototype ctxt))
	(setq errtype (concat errtype " or " (semantic-tag-type tag)))
	)
       ;; A function might be an externally declared method.
       ((and (eq (semantic-tag-class tag) 'function)
	     (semantic-tag-function-parent tag))
	(setq temp (srecode-semantic-find-template
		    "method" prototype ctxt)))
       (t
	nil)
       ))

    ;; Can't find one?  Drat!
    (when (not temp)
      (error "Cannot find template %s in %s for inserting tag %S"
	     errtype top (semantic-format-tag-summarize tag)))

    ;; Resolve arguments
    (let ((srecode-semantic-selected-tag tag))
      (srecode-resolve-arguments temp dict))

    ;; Resolve TAG into the dictionary.  We may have a :tag arg
    ;; from the macro such that we don't need to do this.
    (when (not (srecode-dictionary-lookup-name dict "TAG"))
      (let ((tagobj (srecode-semantic-tag (semantic-tag-name tag) :prime tag))
	    )
	(srecode-semantic-apply-tag-to-dict tagobj dict)))

    ;; Insert dict-entries into the dictionary LAST so that previous
    ;; items can be overridden.
    (let ((entries dict-entries))
      (while entries
	(srecode-dictionary-set-value dict
				      (car entries)
				      (car (cdr entries)))
	(setq entries (cdr (cdr entries)))))

    ;; Insert the template.
    (let ((endpt (srecode-insert-fcn temp dict nil t)))

      (run-hook-with-args 'point-insert-fcn tag)
      ;;(sit-for 1)

      (cond
       ((semantic-tag-of-class-p tag 'type)
	;; Insert all the members at the current insertion point.
	(dolist (m (semantic-tag-type-members tag))

	  (when (stringp m)
	    (setq m (semantic-tag-new-variable m nil nil)))

	  ;; We do prototypes w/in the class decl?
	  (let ((me (srecode-semantic-insert-tag m '(prototype))))
	    (goto-char me))

	  ))
       )

      endpt)
    ))

(provide 'srecode/semantic)

;;; srecode/semantic.el ends here
