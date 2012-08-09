;;; srecode/getset.el --- Package for inserting new get/set methods.

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
;; SRecoder application for inserting new get/set methods into a class.

(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/find)
(require 'srecode/insert)
(require 'srecode/dictionary)

;;; Code:
(defvar srecode-insert-getset-fully-automatic-flag nil
  "Non-nil means accept choices srecode comes up with without asking.")

;;;###autoload
(defun srecode-insert-getset (&optional class-in field-in)
  "Insert get/set methods for the current class.
CLASS-IN is the semantic tag of the class to update.
FIELD-IN is the semantic tag, or string name, of the field to add.
If you do not specify CLASS-IN or FIELD-IN then a class and field
will be derived."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'getset)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (if (not (srecode-template-get-table (srecode-table)
				       "getset-in-class"
				       "declaration"
				       'getset))
      (error "No templates for inserting get/set"))

  ;; Step 1: Try to derive the tag for the class we will use
  (semantic-fetch-tags)
  (let* ((class (or class-in (srecode-auto-choose-class (point))))
	 (tagstart (when class (semantic-tag-start class)))
	 (inclass (eq (semantic-current-tag-of-class 'type) class))
	 (field nil)
	 )

    (when (not class)
      (error "Move point to a class and try again"))

    ;; Step 2: Select a name for the field we will use.
    (when field-in
      (setq field field-in))

    (when (and inclass (not field))
      (setq field (srecode-auto-choose-field (point))))

    (when (not field)
      (setq field (srecode-query-for-field class)))

    ;; Step 3: Insert a new field if needed
    (when (stringp field)

      (goto-char (point))
      (srecode-position-new-field class inclass)

      (let* ((dict (srecode-create-dictionary))
	     (temp (srecode-template-get-table (srecode-table)
					       "getset-field"
					       "declaration"
					       'getset))
	     )
	(when (not temp)
	  (error "Getset templates for %s not loaded!" major-mode))
	(srecode-resolve-arguments temp dict)
	(srecode-dictionary-set-value dict "NAME" field)
	(when srecode-insert-getset-fully-automatic-flag
	  (srecode-dictionary-set-value dict "TYPE" "int"))
	(srecode-insert-fcn temp dict)

	(semantic-fetch-tags)
	(save-excursion
	  (goto-char tagstart)
	  ;; Refresh our class tag.
	  (setq class (srecode-auto-choose-class (point)))
	  )

	(let ((tmptag (semantic-deep-find-tags-by-name-regexp
		       field (current-buffer))))
	  (setq tmptag (semantic-find-tags-by-class 'variable tmptag))

	  (if tmptag
	      (setq field (car tmptag))
	    (error "Could not find new field %s" field)))
	)

      ;; Step 3.5: Insert an initializer if needed.
      ;; ...


      ;; Set up for the rest.
      )

    (if (not (semantic-tag-p field))
	(error "Must specify field for get/set.  (parts may not be impl'd yet.)"))

    ;; Set 4: Position for insertion of methods
    (srecode-position-new-methods class field)

    ;; Step 5: Insert the get/set methods
    (if (not (eq (semantic-current-tag) class))
	;; We are positioned on top of something else.
	;; insert a /n
	(insert "\n"))

    (let* ((dict (srecode-create-dictionary))
	   (srecode-semantic-selected-tag field)
	   (temp (srecode-template-get-table (srecode-table)
					     "getset-in-class"
					     "declaration"
					     'getset))
	   )
      (if (not temp)
	  (error "Getset templates for %s not loaded!" major-mode))
      (srecode-resolve-arguments temp dict)
      (srecode-dictionary-set-value dict "GROUPNAME"
				    (concat (semantic-tag-name field)
					    " Accessors"))
      (srecode-dictionary-set-value dict "NICENAME"
				    (srecode-strip-fieldname
				     (semantic-tag-name field)))
      (srecode-insert-fcn temp dict)
      )))

(defun srecode-strip-fieldname (name)
  "Strip the fieldname NAME of polish notation things."
  (cond ((string-match "[a-z]\\([A-Z]\\w+\\)" name)
	 (substring name (match-beginning 1)))
	;; Add more rules here.
	(t
	 name)))

(defun srecode-position-new-methods (class field)
  "Position the cursor in CLASS where new getset methods should go.
FIELD is the field for the get sets.
INCLASS specifies if the cursor is already in CLASS or not."
  (semantic-go-to-tag field)

  (let ((prev (semantic-find-tag-by-overlay-prev))
	(next (semantic-find-tag-by-overlay-next))
	(setname nil)
	(aftertag nil)
	)
    (cond
     ((and prev (semantic-tag-of-class-p prev 'variable))
      (setq setname
	    (concat "set"
		    (srecode-strip-fieldname (semantic-tag-name prev))))
      )
     ((and next (semantic-tag-of-class-p next 'variable))
      (setq setname
	    (concat "set"
		    (srecode-strip-fieldname (semantic-tag-name prev)))))
     (t nil))

    (setq aftertag (semantic-find-first-tag-by-name
		    setname (semantic-tag-type-members class)))

    (when (not aftertag)
      (setq aftertag (car-safe
		      (semantic--find-tags-by-macro
		       (semantic-tag-get-attribute (car tags) :destructor-flag)
		       (semantic-tag-type-members class))))
      ;; Make sure the tag is public
      (when (not (eq (semantic-tag-protection aftertag class) 'public))
	(setq aftertag nil))
      )

    (if (not aftertag)
	(setq aftertag (car-safe
			(semantic--find-tags-by-macro
			 (semantic-tag-get-attribute (car tags) :constructor-flag)
			 (semantic-tag-type-members class))))
      ;; Make sure the tag is public
      (when (not (eq (semantic-tag-protection aftertag class) 'public))
	(setq aftertag nil))
      )

    (when (not aftertag)
      (setq aftertag (semantic-find-first-tag-by-name
		      "public" (semantic-tag-type-members class))))

    (when (not aftertag)
      (setq aftertag (car (semantic-tag-type-members class))))

    (if aftertag
	(let ((te (semantic-tag-end aftertag)))
	  (when (not te)
	    (message "Unknown location for tag-end in %s:" (semantic-tag-name aftertag)))
	  (goto-char te)
	  ;; If there is a comment immediately after aftertag, skip over it.
	  (when (looking-at (concat "\\s-*\n?\\s-*" semantic-lex-comment-regex))
	    (let ((pos (point))
		  (rnext (semantic-find-tag-by-overlay-next (point))))
	      (forward-comment 1)
	      ;; Make sure the comment we skipped didn't say anything about
	      ;; the rnext tag.
	      (when (and rnext
			 (re-search-backward
			  (regexp-quote (semantic-tag-name rnext)) pos t))
		;; It did mention rnext, so go back to our starting position.
		(goto-char pos)
		)
	      ))
	  )

      ;; At the very beginning of the class.
      (goto-char (semantic-tag-end class))
      (forward-sexp -1)
      (forward-char 1)

      )

    (end-of-line)
    (forward-char 1)
    ))

(defun srecode-position-new-field (class inclass)
  "Select a position for a new field for CLASS.
If INCLASS is non-nil, then the cursor is already in the class
and should not be moved during point selection."

  ;; If we aren't in the class, get the cursor there, pronto!
  (when (not inclass)

    (error "You must position the cursor where to insert the new field")

    (let ((kids (semantic-find-tags-by-class
		 'variable (semantic-tag-type-members class))))
      (cond (kids
	     (semantic-go-to-tag (car kids) class))
	    (t
	     (semantic-go-to-tag class)))
      )

    (switch-to-buffer (current-buffer))

    ;; Once the cursor is in our class, ask the user to position
    ;; the cursor to keep going.
    )

  (if (or srecode-insert-getset-fully-automatic-flag
	  (y-or-n-p "Insert new field here? "))
      nil
    (error "You must position the cursor where to insert the new field first"))
  )



(defun srecode-auto-choose-field (point)
  "Choose a field for the get/set methods.
Base selection on the field related to POINT."
  (save-excursion
    (when point
      (goto-char point))

    (let ((field (semantic-current-tag-of-class 'variable)))

      ;; If we get a field, make sure the user gets a chance to choose.
      (when field
	(if srecode-insert-getset-fully-automatic-flag
	    nil
	  (when (not (y-or-n-p
		      (format "Use field %s? " (semantic-tag-name field))))
	    (setq field nil))
	  ))
      field)))

(defun srecode-query-for-field (class)
  "Query for a field in CLASS."
  (let* ((kids (semantic-find-tags-by-class
		'variable (semantic-tag-type-members class)))
	 (sel (completing-read "Use Field: " kids))
	 )

    (or (semantic-find-tags-by-name sel kids)
	sel)
    ))

(defun srecode-auto-choose-class (point)
  "Choose a class based on location of POINT."
  (save-excursion
    (when point
      (goto-char point))

    (let ((tag (semantic-current-tag-of-class 'type)))

      (when (or (not tag)
		(not (string= (semantic-tag-type tag) "class")))
	;; The current tag is not a class.  Are we in a fcn
	;; that is a method?
	(setq tag (semantic-current-tag-of-class 'function))

	(when (and tag
		   (semantic-tag-function-parent tag))
	  (let ((p (semantic-tag-function-parent tag)))
	    ;; @TODO : Copied below out of semantic-analyze
	    ;;         Turn into a routine.

	    (let* ((searchname (cond ((stringp p) p)
				     ((semantic-tag-p p)
				      (semantic-tag-name p))
				     ((and (listp p) (stringp (car p)))
				      (car p))))
		   (ptag (semantic-analyze-find-tag searchname
						    'type nil)))
	      (when ptag (setq tag ptag ))
	      ))))

      (when (or (not tag)
		(not (semantic-tag-of-class-p tag 'type))
		(not (string= (semantic-tag-type tag) "class")))
	;; We are not in a class that needs a get/set method.
	;; Analyze the current context, and derive a class name.
	(let* ((ctxt (semantic-analyze-current-context))
	       (pfix nil)
	       (ans nil))
	  (when ctxt
	    (setq pfix (reverse (oref ctxt prefix)))
	    (while (and (not ans) pfix)
	      ;; Start at the end and back up to the first class.
	      (when (and (semantic-tag-p (car pfix))
			 (semantic-tag-of-class-p (car pfix) 'type)
			 (string= (semantic-tag-type (car pfix))
				  "class"))
		(setq ans (car pfix)))
	      (setq pfix (cdr pfix))))
	  (setq tag ans)))

      tag)))

(provide 'srecode/getset)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/getset"
;; End:

;;; srecode/getset.el ends here
