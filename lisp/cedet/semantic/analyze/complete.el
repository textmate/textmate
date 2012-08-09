;;; semantic/analyze/complete.el --- Smart Completions

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

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
;; Calculate smart completions.
;;
;; Uses the analyzer context routine to determine the best possible
;; list of completions.
;;
;;; History:
;;
;; Code was moved here from semantic/analyze.el

(require 'semantic/analyze)

;; For semantic-find-* macros:
(eval-when-compile (require 'semantic/find))

;;; Code:

;;; Helper Fcns
;;
;;
;;;###autoload
(define-overloadable-function semantic-analyze-type-constants (type)
  "For the tag TYPE, return any constant symbols of TYPE.
Used as options when completing.")

(defun semantic-analyze-type-constants-default (type)
  "Do nothing with TYPE."
  nil)

(defun semantic-analyze-tags-of-class-list (tags classlist)
  "Return the tags in TAGS that are of classes in CLASSLIST."
  (let ((origc tags))
    ;; Accept only tags that are of the datatype specified by
    ;; the desired classes.
    (setq tags (apply 'nconc ;; All input lists are permutable.
		      (mapcar (lambda (class)
				(semantic-find-tags-by-class class origc))
			      classlist)))
    tags))

;;; MAIN completion calculator
;;
;;;###autoload
(define-overloadable-function semantic-analyze-possible-completions (context &rest flags)
  "Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
The remaining FLAGS arguments are passed to the mode specific completion engine.
Bad flags should be ignored by modes that don't use them.
See `semantic-analyze-possible-completions-default' for details on the default FLAGS.

Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
  (interactive "d")
  ;; In theory, we don't need the below since the context will
  ;; do it for us.
  ;;(semantic-refresh-tags-safe)
  (with-syntax-table semantic-lex-syntax-table
    (let* ((context (if (semantic-analyze-context-child-p context)
                        context
                      (semantic-analyze-current-context context)))
	   (ans (if (not context)
		    (error "Nothing to complete")
		  (:override))))
      ;; If interactive, display them.
      (when (called-interactively-p 'any)
	(with-output-to-temp-buffer "*Possible Completions*"
	  (semantic-analyze-princ-sequence ans "" (current-buffer)))
	(shrink-window-if-larger-than-buffer
	 (get-buffer-window "*Possible Completions*")))
      ans)))

(defun semantic-analyze-possible-completions-default (context &optional flags)
  "Default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context.
The optional argument FLAGS changes which return options are returned.
FLAGS can be any number of:
  'no-tc     - do not apply data-type constraint.
  'no-unique - do not apply unique by name filtering."
  (let* ((a context)
	 (desired-type (semantic-analyze-type-constraint a))
	 (desired-class (oref a prefixclass))
	 (prefix (oref a prefix))
	 (prefixtypes (oref a prefixtypes))
	 (completetext nil)
	 (completetexttype nil)
	 (scope (oref a scope))
	 (localvar (when scope (oref scope localvar)))
	 (origc nil)
	 (c nil)
	 (any nil)
	 (do-typeconstraint (not (memq 'no-tc flags)))
	 (do-unique (not (memq 'no-unique flags)))
	 )

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-tag-p completetext)
	(setq completetext (semantic-tag-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (when (or (not completetexttype)
	      (not (and (semantic-tag-p completetexttype)
			(eq (semantic-tag-class completetexttype) 'type))))
      ;; What should I do here?  I think this is an error condition.
      (setq completetexttype nil)
      ;; If we had something that was a completetexttype but it wasn't
      ;; valid, then express our dismay!
      (when (> (length prefix) 1)
	(let* ((errprefix (car (cdr (reverse prefix)))))
	  (error "Cannot find types for `%s'"
		 (cond ((semantic-tag-p errprefix)
			(semantic-format-tag-prototype errprefix))
		       (t
			(format "%S" errprefix)))))
	))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if completetexttype

	(setq c (semantic-find-tags-for-completion
		 completetext
		 (semantic-analyze-scoped-type-parts completetexttype scope)
		 ))

      ;; No type based on the completetext.  This is a free-range
      ;; var or function.  We need to expand our search beyond this
      ;; scope into semanticdb, etc.
      (setq c (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope
	       (semantic-find-tags-for-completion completetext (when scope (oref scope fullscope)))
	       ;; The world
	       (semantic-analyze-find-tags-by-prefix completetext))
	    )
      )

    (let ((loopc c)
	  (dtname (semantic-tag-name desired-type)))

      ;; Save off our first batch of completions
      (setq origc c)

      ;; Reset c.
      (setq c nil)

      ;; Loop over all the found matches, and categorize them
      ;; as being possible features.
      (while (and loopc do-typeconstraint)

	(cond
	 ;; Strip operators
	 ((semantic-tag-get-attribute (car loopc) :operator-flag)
	  nil
	  )

	 ;; If we are completing from within some prefix,
	 ;; then we want to exclude constructors and destructors
	 ((and completetexttype
	       (or (semantic-tag-get-attribute (car loopc) :constructor-flag)
		   (semantic-tag-get-attribute (car loopc) :destructor-flag)))
	  nil
	  )

	 ;; If there is a desired type, we need a pair of restrictions
	 (desired-type

	  (cond
	   ;; Ok, we now have a completion list based on the text we found
	   ;; we want to complete on.  Now filter that stream against the
	   ;; type we want to search for.
	   ((string= dtname (semantic-analyze-type-to-name (semantic-tag-type (car loopc))))
	    (setq c (cons (car loopc) c))
	    )

	   ;; Now anything that is a compound type which could contain
	   ;; additional things which are of the desired type
	   ((semantic-tag-type (car loopc))
	    (let ((att (semantic-analyze-tag-type (car loopc) scope))
		)
	      (if (and att (semantic-tag-type-members att))
		  (setq c (cons (car loopc) c))))
	    )

	   ) ; cond
	  ); desired type

	 ;; No desired type, no other restrictions.  Just add.
	 (t
	  (setq c (cons (car loopc) c)))

	 ); cond

	(setq loopc (cdr loopc)))

      (when desired-type
	;; Some types, like the enum in C, have special constant values that
	;; we could complete with.  Thus, if the target is an enum, we can
	;; find possible symbol values to fill in that value.
	(let ((constants
	       (semantic-analyze-type-constants desired-type)))
	  (if constants
	      (progn
		;; Filter
		(setq constants
		      (semantic-find-tags-for-completion
		       completetext constants))
		;; Add to the list
		(setq c (nconc c constants)))
	    )))
      )

    (when desired-class
      (setq c (semantic-analyze-tags-of-class-list c desired-class)))

    (if do-unique
	(if c
	    ;; Pull out trash.
	    ;; NOTE TO SELF: Is this too slow?
	    (setq c (semantic-unique-tag-table-by-name c))
	  (setq c (semantic-unique-tag-table-by-name origc)))
      (when (not c)
	(setq c origc)))

    ;; All done!
    c))

(provide 'semantic/analyze/complete)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/analyze/complete"
;; End:

;;; semantic/analyze/complete.el ends here
