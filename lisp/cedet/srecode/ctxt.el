;;; srecode/ctxt.el --- Derive a context from the source buffer.

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
;; Manage context calculations for Semantic Recoder.
;;
;; SRecode templates are always bound to a context.  By calculating
;; the current context, we can narrow down the selection of possible
;; templates to something reasonable.
;;
;; Alternately, code here will find a context for templates that
;; require different pieces of code placed in multiple areas.

(require 'semantic)
(require 'semantic/tag-ls)

(declare-function srecode-dictionary-show-section "srecode/dictionary")
(declare-function srecode-dictionary-set-value "srecode/dictionary")

;;; Code:

(define-overload srecode-calculate-context ()
  "Calculate the context at the current point.
The returned context is a list, with the top-most context first.
Each returned context is a string that would show up in a `context'
statement in an `.srt' file.

Some useful context values used by the provided srecode templates are:
  \"file\" - Templates that for a file (such as an empty file.)
     \"empty\" - The file is empty
  \"declaration\" - Top-level declarations in a file.
     \"include\" - In or near include statements
     \"package\" - In or near provide statements
     \"function\" - In or near function statements
         \"NAME\" - Near functions within NAME namespace or class
     \"variable\" - In or near variable statements.
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment
  \"classdecl\" - Declarations within a class/struct/etc.
     \"variable\" - In or near class fields
     \"function\" - In or near methods/functions
        \"virtual\" - Nearby items are virtual
           \"pure\" - and those virtual items are pure virtual
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment in a block of code
     -- these items show up at the end of the context list. --
     \"public\", \"protected\", \"private\" -
                  In or near a section of public/protected/private entries.
  \"code\" - In a block of code.
     \"string\" - In a string in a block of code
     \"comment\"  - In a comment in a block of code

    ... More later."
  )

(defun srecode-calculate-nearby-things ()
  ;; NOTE: May need to add bounds to this FCN
  "Calculate the CONTEXT type items nearby the current point.
Assume that what we want to insert next is based on what is just
before point.  If there is nothing, then assume it is whatever is
after point."
  ;; @todo - ADD BOUNDS TO THE PREV/NEXT TAG SEARCH
  ;;         thus classdecl "near" stuff cannot be
  ;;         outside the bounds of the type in question.
  (let ((near (semantic-find-tag-by-overlay-prev))
	(prot nil)
	(ans nil))
    (if (not near)
	(setq near (semantic-find-tag-by-overlay-next)))
    (when near
      ;; Calculate the type of thing we are near.
      (if (not (semantic-tag-of-class-p near 'function))
	  (setq ans (cons (symbol-name (semantic-tag-class near)) ans))
	;; if the symbol NEAR has a parent,
	(let ((p (semantic-tag-function-parent near)))
	  (setq ans (cons (symbol-name (semantic-tag-class near)) ans))
	  (cond ((semantic-tag-p p)
		 (setq ans (cons (semantic-tag-name p) ans)))
		((stringp p)
		 (setq ans (cons p ans)))
		(t nil)))
	;; Was it virtual?
	(when (semantic-tag-get-attribute near :virtual)
	  (setq ans (cons "virtual" ans)))
	;; Was it pure?
	(when (semantic-tag-get-attribute near :pure-virtual-flag)
	  (setq ans (cons "pure" ans)))
      )
      ;; Calculate the protection
      (setq prot (semantic-tag-protection near))
      (when (and prot (not (eq prot 'unknown)))
	(setq ans (cons (symbol-name prot) ans)))
      )
    (nreverse ans)))

(defun srecode-calculate-context-font-lock ()
  "Calculate an srecode context by using font-lock."
  (let ((face (get-text-property (point) 'face))
	)
    (cond ((member face '(font-lock-string-face
			  font-lock-doc-face))
	   (list "string"))
	  ((member face '(font-lock-comment-face
			  font-lock-comment-delimiter-face))
	   (list "comment"))
	  )
    ))

(defun srecode-calculate-context-default ()
  "Generic method for calculating a context for srecode."
  (if (= (point-min) (point-max))
      (list "file" "empty")

    (semantic-fetch-tags)
    (let ((ct (semantic-find-tag-by-overlay))
	  )
      (cond ((or (not ct)
		 ;; Ok, below is a bit C specific.
		 (and (eq (semantic-tag-class (car ct)) 'type)
		      (string= (semantic-tag-type (car ct)) "namespace")))
	     (cons "declaration"
		   (or (srecode-calculate-context-font-lock)
		       (srecode-calculate-nearby-things)
		       ))
	     )
	    ((eq (semantic-tag-class (car ct)) 'function)
	     (cons "code" (srecode-calculate-context-font-lock))
	     )
	    ((eq (semantic-tag-class (car ct)) 'type) ; We know not namespace
	     (cons "classdecl"
		   (or (srecode-calculate-context-font-lock)
		       (srecode-calculate-nearby-things)))
	     )
	    ((and (car (cdr ct))
		  (eq (semantic-tag-class (car (cdr ct))) 'type))
	     (list "classdecl"
		   (symbol-name (semantic-tag-class (car ct))))
	     )
	    )
      )))


;;; HANDLERS
;;
;; The calculated context is one thing, but more info is often available.
;; The context handlers can add info into the active dictionary that is
;; based on the context, such as a method parent name, protection scheme,
;; or other feature.

(defun srecode-semantic-handle-:ctxt (dict &optional template)
  "Add macros into the dictionary DICT based on the current Emacs Lisp file.
Argument TEMPLATE is the template object adding context dictionary
entries.
This might add the following:
   VIRTUAL - show a section if a function is virtual
   PURE - show a section if a function is pure virtual.
   PARENT - The name of a parent type for functions.
   PROTECTION - Show a protection section, and what the protection is."
  (require 'srecode/dictionary)
  (when template

    (let ((name (oref template object-name))
	  (cc (if (boundp 'srecode-insertion-start-context)
		  srecode-insertion-start-context))
	  ;(context (oref template context))
	  )

;      (when (and cc
;		 (null (string= (car cc) context))
;		 )
;	;; No current context, or the base is different, then
;	;; this is the section where we need to recalculate
;	;; the context based on user choice, if possible.
;	;;
;	;; The recalculation is complex, as there are many possibilities
;	;; that need to be divined.  Set "cc" to the new context
;	;; at the end.
;	;;
;	;; @todo -
;
;	)

      ;; The various context all have different features.
      (let ((ct (nth 0 cc))
	    (it (nth 1 cc))
	    (last (last cc))
	    (parent nil)
	    )
	(cond ((string= it "function")
	       (setq parent (nth 2 cc))
	       (when parent
		 (cond ((string= parent "virtual")
			(srecode-dictionary-show-section dict "VIRTUAL")
			(when (nth 3 cc)
			  (srecode-dictionary-show-section dict "PURE"))
			)
		       (t
			(srecode-dictionary-set-value dict "PARENT" parent))))
	       )
	      ((and (string= it "type")
		    (or (string= name "function") (string= name "method")))
	       ;; If we have a type, but we insert a fcn, then use that type
	       ;; as the function parent.
	       (let ((near (semantic-find-tag-by-overlay-prev)))
		 (when (and near (semantic-tag-of-class-p near 'type))
		   (srecode-dictionary-set-value
		    dict "PARENT" (semantic-tag-name near))))
	       )
	      ((string= ct "code")
	       ;;(let ((analyzer (semantic-analyze-current-context)))
	       ;; @todo - Use the analyze to setup things like local
	       ;;         variables we might use or something.
	       nil
	       ;;)
	       )
	      (t
	       nil))
	(when (member last '("public" "private" "protected"))
	  ;; Hey, fancy that, we can do both.
	  (srecode-dictionary-set-value dict "PROTECTION" parent)
	  (srecode-dictionary-show-section dict "PROTECTION"))
	))
    ))


(provide 'srecode/ctxt)

;;; srecode/ctxt.el ends here
