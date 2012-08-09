;;; eieio-custom.el -- eieio object customization

;; Copyright (C) 1999-2001, 2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2
;; Keywords: OO, lisp
;; Package: eieio

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
;;   This contains support customization of eieio objects.  Enabling
;; your object to be customizable requires use of the slot attribute
;; `:custom'.

(require 'eieio)
(require 'widget)
(require 'wid-edit)
(require 'custom)

;;; Compatibility

;; (eval-and-compile
;;   (if (featurep 'xemacs)
;;       (defalias 'eieio-overlay-lists (lambda () (list (extent-list))))
;;     (defalias 'eieio-overlay-lists 'overlay-lists)))

;;; Code:
(defclass eieio-widget-test-class nil
  ((a-string :initarg :a-string
	     :initform "The moose is loose"
	     :custom string
	     :label "Amorphous String"
	     :group (default foo)
	     :documentation "A string for testing custom.
This is the next line of documentation.")
   (listostuff :initarg :listostuff
	       :initform ("1" "2" "3")
	       :type list
	       :custom (repeat (string :tag "Stuff"))
	       :label "List of Strings"
	       :group foo
	       :documentation "A list of stuff.")
   (uninitialized :initarg :uninitialized
		  :type string
		  :custom string
		  :documentation "This slot is not initialized.
Used to make sure that custom doesn't barf when it encounters one
of these.")
   (a-number :initarg :a-number
	     :initform 2
	     :custom integer
	     :documentation "A number of thingies."))
  "A class for testing the widget on.")

(defcustom eieio-widget-test (eieio-widget-test-class "Foo")
  "Test variable for editing an object."
  :type 'object
  :group 'eieio)

(defface eieio-custom-slot-tag-face '((((class color)
					(background dark))
				       (:foreground "light blue"))
				      (((class color)
					(background light))
				       (:foreground "blue"))
				      (t (:italic t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)

(defvar eieio-wo nil
  "Buffer local variable in object customize buffers for the current widget.")
(defvar eieio-co nil
  "Buffer local variable in object customize buffers for the current obj.")
(defvar eieio-cog nil
  "Buffer local variable in object customize buffers for the current group.")

 (defvar eieio-custom-ignore-eieio-co nil
   "When true, all customizable slots of the current object are updated.
Updates occur regardless of the current customization group.")

(define-widget 'object-slot 'group
  "Abstractly modify a single slot in an object."
  :tag "Slot"
  :format "%t %v%h\n"
  :convert-widget 'widget-types-convert-widget
  :value-create 'eieio-slot-value-create
  :value-get 'eieio-slot-value-get
  :value-delete 'widget-children-value-delete
  :validate 'widget-children-validate
  :match 'eieio-object-match ;; same
  )

(defun eieio-slot-value-create (widget)
  "Create the value of WIDGET."
  (let ((chil nil))
    (setq chil (cons
		(widget-create-child-and-convert
		 widget (widget-get widget :childtype)
		 :tag ""
		 :value (widget-get widget :value))
		chil))
    (widget-put widget :children chil)))

(defun eieio-slot-value-get (widget)
  "Get the value of WIDGET."
  (widget-value (car (widget-get widget :children))))

(defun eieio-custom-toggle-hide (widget)
  "Toggle visibility of WIDGET."
  (let ((vc (car (widget-get widget :children))))
    (cond ((eq (widget-get vc :eieio-custom-state) 'hidden)
	   (widget-put vc :eieio-custom-state 'visible)
	   (widget-put vc :value-face (widget-get vc :orig-face)))
	  (t
	   (widget-put vc :eieio-custom-state 'hidden)
	   (widget-put vc :orig-face (widget-get vc :value-face))
	   (widget-put vc :value-face 'invisible)
	   ))
    (widget-value-set vc (widget-value vc))))

(defun eieio-custom-toggle-parent (widget &rest ignore)
  "Toggle visibility of parent of WIDGET.
Optional argument IGNORE is an extraneous parameter."
  (eieio-custom-toggle-hide (widget-get widget :parent)))

(define-widget 'object-edit 'group
  "Abstractly modify a CLOS object."
  :tag "Object"
  :format "%v"
  :convert-widget 'widget-types-convert-widget
  :value-create 'eieio-object-value-create
  :value-get 'eieio-object-value-get
  :value-delete 'widget-children-value-delete
  :validate 'widget-children-validate
  :match 'eieio-object-match
  :clone-object-children nil
  )

(defun eieio-object-match (widget value)
  "Match info for WIDGET against VALUE."
  ;; Write me
  t)

(defun eieio-filter-slot-type (widget slottype)
  "Filter WIDGETs SLOTTYPE."
  (if (widget-get widget :clone-object-children)
      slottype
    (cond ((eq slottype 'object)
	   'object-edit)
	  ((and (listp slottype)
		(eq (car slottype) 'object))
	   (cons 'object-edit (cdr slottype)))
	  ((equal slottype '(repeat object))
	   '(repeat object-edit))
	  ((and (listp slottype)
		(equal (car slottype) 'repeat)
		(listp (car (cdr slottype)))
		(equal (car (car (cdr slottype))) 'object))
	   (list 'repeat
		 (cons 'object-edit
		       (cdr (car (cdr slottype))))))
	  (t slottype))))

(defun eieio-object-value-create (widget)
  "Create the value of WIDGET."
  (if (not (widget-get widget :value))
      (widget-put widget
		  :value (cond ((widget-get widget :objecttype)
				(funcall (class-constructor
					  (widget-get widget :objecttype))
					 "Custom-new"))
			       ((widget-get widget :objectcreatefcn)
				(funcall (widget-get widget :objectcreatefcn)))
			       (t (error "No create method specified")))))
  (let* ((chil nil)
	 (obj (widget-get widget :value))
	 (master-group (widget-get widget :eieio-group))
	 (cv (class-v (object-class-fast obj)))
	 (slots (aref cv class-public-a))
	 (flabel (aref cv class-public-custom-label))
	 (fgroup (aref cv class-public-custom-group))
	 (fdoc (aref cv class-public-doc))
	 (fcust (aref cv class-public-custom)))
    ;; First line describes the object, but may not editable.
    (if (widget-get widget :eieio-show-name)
	(setq chil (cons (widget-create-child-and-convert
			  widget 'string :tag "Object "
			  :sample-face 'bold
			  (object-name-string obj))
			 chil)))
    ;; Display information about the group being shown
    (when master-group
      (let ((groups (class-option (object-class-fast obj) :custom-groups)))
	(widget-insert "Groups:")
	(while groups
	  (widget-insert "  ")
	  (if (eq (car groups) master-group)
	      (widget-insert "*" (capitalize (symbol-name master-group)) "*")
	    (widget-create 'push-button
			   :thing (cons obj (car groups))
			   :notify (lambda (widget &rest stuff)
				     (eieio-customize-object
				      (car (widget-get widget :thing))
				      (cdr (widget-get widget :thing))))
			   (capitalize (symbol-name (car groups)))))
	  (setq groups (cdr groups)))
	(widget-insert "\n\n")))
    ;; Loop over all the slots, creating child widgets.
    (while slots
      ;; Output this slot if it has a customize flag associated with it.
      (when (and (car fcust)
		 (or (not master-group) (member master-group (car fgroup)))
		 (slot-boundp obj (car slots)))
	;; In this case, this slot has a custom type.  Create its
	;; children widgets.
	(let ((type (eieio-filter-slot-type widget (car fcust)))
	      (stuff nil))
	  ;; This next bit is an evil hack to get some EDE functions
	  ;; working the way I like.
	  (if (and (listp type)
		   (setq stuff (member :slotofchoices type)))
	      (let ((choices (eieio-oref obj (car (cdr stuff))))
		    (newtype nil))
		(while (not (eq (car type) :slotofchoices))
		  (setq newtype (cons (car type) newtype)
			type (cdr type)))
		(while choices
		  (setq newtype (cons (list 'const (car choices))
				      newtype)
			choices (cdr choices)))
		(setq type (nreverse newtype))))
	  (setq chil (cons (widget-create-child-and-convert
			    widget 'object-slot
			    :childtype type
			    :sample-face 'eieio-custom-slot-tag-face
			    :tag
			    (concat
			     (make-string
			      (or (widget-get widget :indent) 0)
			      ? )
			     (if (car flabel)
				 (car flabel)
			       (let ((s (symbol-name
					 (or
					  (class-slot-initarg
					   (object-class-fast obj)
					   (car slots))
					  (car slots)))))
				 (capitalize
				  (if (string-match "^:" s)
				      (substring s (match-end 0))
				    s)))))
			    :value (slot-value obj (car slots))
			    :doc  (if (car fdoc) (car fdoc)
				    "Slot not Documented.")
			    :eieio-custom-visibility 'visible
			    )
			   chil))
	  )
	)
      (setq slots (cdr slots)
	    fdoc (cdr fdoc)
	    fcust (cdr fcust)
	    flabel (cdr flabel)
	    fgroup (cdr fgroup)))
    (widget-put widget :children (nreverse chil))
    ))

(defun eieio-object-value-get (widget)
  "Get the value of WIDGET."
  (let* ((obj (widget-get widget :value))
	 (master-group eieio-cog)
	 (cv (class-v (object-class-fast obj)))
	 (fgroup (aref cv class-public-custom-group))
	 (wids (widget-get widget :children))
	 (name (if (widget-get widget :eieio-show-name)
		   (car (widget-apply (car wids) :value-inline))
		 nil))
	 (chil (if (widget-get widget :eieio-show-name)
		   (nthcdr 1 wids) wids))
	 (cv (class-v (object-class-fast obj)))
	 (slots (aref cv class-public-a))
	 (fcust (aref cv class-public-custom)))
    ;; If there are any prefix widgets, clear them.
    ;; -- None yet
    ;; Create a batch of initargs for each slot.
    (while (and slots chil)
      (if (and (car fcust)
	       (or eieio-custom-ignore-eieio-co
		   (not master-group) (member master-group (car fgroup)))
	       (slot-boundp obj (car slots)))
	  (progn
	    ;; Only customized slots have widgets
	    (let ((eieio-custom-ignore-eieio-co t))
	      (eieio-oset obj (car slots)
			  (car (widget-apply (car chil) :value-inline))))
	    (setq chil (cdr chil))))
      (setq slots (cdr slots)
	    fgroup (cdr fgroup)
	    fcust (cdr fcust)))
    ;; Set any name updates on it.
    (if name (aset obj object-name name))
    ;; This is the same object we had before.
    obj))

(defmethod eieio-done-customizing ((obj eieio-default-superclass))
  "When applying change to a widget, call this method.
This method is called by the default widget-edit commands.
User made commands should also call this method when applying changes.
Argument OBJ is the object that has been customized."
  nil)

;;;###autoload
(defun customize-object (obj &optional group)
  "Customize OBJ in a custom buffer.
Optional argument GROUP is the sub-group of slots to display."
  (eieio-customize-object obj group))

(defmethod eieio-customize-object ((obj eieio-default-superclass)
				   &optional group)
  "Customize OBJ in a specialized custom buffer.
To override call the `eieio-custom-widget-insert' to just insert the
object widget.
Optional argument GROUP specifies a subgroup of slots to edit as a symbol.
These groups are specified with the `:group' slot flag."
  ;; Insert check for multiple edits here.
  (let* ((g (or group 'default)))
    (switch-to-buffer (get-buffer-create
		       (concat "*CUSTOMIZE "
			       (object-name obj) " "
			       (symbol-name g) "*")))
    (toggle-read-only -1)
    (kill-all-local-variables)
    (erase-buffer)
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapc 'delete-overlay (car all))
      (mapc 'delete-overlay (cdr all)))
    ;; Add an apply reset option at the top of the buffer.
    (eieio-custom-object-apply-reset obj)
    (widget-insert "\n\n")
    (widget-insert "Edit object " (object-name obj) "\n\n")
    ;; Create the widget editing the object.
    (make-local-variable 'eieio-wo)
    (setq eieio-wo (eieio-custom-widget-insert obj :eieio-group g))
    ;;Now generate the apply buttons
    (widget-insert "\n")
    (eieio-custom-object-apply-reset obj)
    ;; Now initialize the buffer
    (use-local-map widget-keymap)
    (widget-setup)
    ;;(widget-minor-mode)
    (goto-char (point-min))
    (widget-forward 3)
    (make-local-variable 'eieio-co)
    (setq eieio-co obj)
    (make-local-variable 'eieio-cog)
    (setq eieio-cog group)))

(defmethod eieio-custom-object-apply-reset ((obj eieio-default-superclass))
  "Insert an Apply and Reset button into the object editor.
Argument OBJ is the object being customized."
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (widget-apply eieio-wo :value-get)
			   (eieio-done-customizing eieio-co)
			   (bury-buffer))
		 "Accept")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   ;; I think the act of getting it sets
			   ;; its value through the get function.
			   (message "Applying Changes...")
			   (widget-apply eieio-wo :value-get)
			   (eieio-done-customizing eieio-co)
			   (message "Applying Changes...Done"))
		 "Apply")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (message "Resetting")
			   (eieio-customize-object eieio-co eieio-cog))
		 "Reset")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bury-buffer))
		 "Cancel"))

(defmethod eieio-custom-widget-insert ((obj eieio-default-superclass)
				       &rest flags)
  "Insert the widget used for editing object OBJ in the current buffer.
Arguments FLAGS are widget compatible flags.
Must return the created widget."
  (apply 'widget-create 'object-edit :value obj flags))

(define-widget 'object 'object-edit
  "Instance of a CLOS class."
  :format "%{%t%}:\n%v"
  :value-to-internal 'eieio-object-value-to-abstract
  :value-to-external 'eieio-object-abstract-to-value
  :clone-object-children t
  )

(defun eieio-object-value-to-abstract (widget value)
  "For WIDGET, convert VALUE to an abstract /safe/ representation."
  (if (eieio-object-p value) value
    (if (null value) value
      nil)))

(defun eieio-object-abstract-to-value (widget value)
  "For WIDGET, convert VALUE from an abstract /safe/ representation."
  value)


;;; customization group functions
;;
;; These functions provide the ability to create dynamic menus to
;; customize specific sections of an object.  They do not hook directly
;; into a filter, but can be used to create easymenu vectors.
(defmethod eieio-customize-object-group ((obj eieio-default-superclass))
  "Create a list of vectors for customizing sections of OBJ."
  (mapcar (lambda (group)
	    (vector (concat "Group " (symbol-name group))
		    (list 'customize-object obj (list 'quote group))
		    t))
	  (class-option (object-class-fast obj) :custom-groups)))

(defvar eieio-read-custom-group-history nil
  "History for the custom group reader.")

(defmethod eieio-read-customization-group ((obj eieio-default-superclass))
  "Do a completing read on the name of a customization group in OBJ.
Return the symbol for the group, or nil"
  (let ((g (class-option (object-class-fast obj) :custom-groups)))
    (if (= (length g) 1)
	(car g)
      ;; Make the association list
      (setq g (mapcar (lambda (g) (cons (symbol-name g) g)) g))
      (cdr (assoc
	    (completing-read (concat (oref obj name)  " Custom Group: ")
			     g nil t nil 'eieio-read-custom-group-history)
	    g)))))

(provide 'eieio-custom)

;; Local variables:
;; generated-autoload-file: "eieio.el"
;; End:

;;; eieio-custom.el ends here
