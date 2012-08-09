;;; eieio-speedbar.el -- Classes for managing speedbar displays.

;; Copyright (C) 1999-2002, 2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2
;; Keywords: OO, tools
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
;;  This provides some classes that can be used as a parent which
;; will automatically provide SPEEDBAR support for any list of objects
;; of that type.
;;
;; This file requires speedbar version 0.10 or later.

;;; Creating a new speedbar mode based on a pre-existing object hierarchy
;;
;; To create a new speedbar mode based on lists of objects is easier
;; than creating a whole new speedbar mode from scratch.
;;
;; 1) Objects that will have lists of items that can be expanded
;;    should also inherit from the classes:
;;  * `eieio-speedbar'                  - specify your own button behavior
;;  * `eieio-speedbar-directory-button' - objects that behave like directories
;;  * `eieio-speedbar-file-button'      - objects that behave like files
;;
;; 2) Objects that have lists of children should implement the method
;;    `eieio-speedbar-object-children' which returns a list of more
;;    objects, or a list of strings.
;;
;; 3) Objects that return a list of strings should also implement these
;;    methods:
;;  * `eieio-speedbar-child-make-tag-lines' - make tag lines for a child.
;;  * `eieio-speedbar-child-description' - describe non-object children
;;
;; 4) Objects which have expanded information should implement the method
;;    `eieio-speedbar-description' to produce more information.
;;
;; 5) Objects that are associated with a directory should implement
;;    the method `eieio-speedbar-derive-line-path' which returns a
;;    path.
;;
;; 6) Objects that have a specialized behavior when clicked should
;;    define the method `eieio-speedbar-handle-click'.
;;
;; To initialize a new eieio based speedbar display, do the following.
;;
;; 1) Create a keymap variable `foo-speedbar-key-map'.
;;    This keymap variable should be initialized in a function.
;;    If you have no special needs, use `eieio-speedbar-key-map'
;;
;; 2) Create a variable containing an easymenu definition compatible
;;    with speedbar.  if you have no special needs, use
;;    `eieio-speedbar-menu'.
;;
;; 3) Create a function which returns the top-level list of children
;;    objects to be displayed in speedbar.
;;
;; 4) Call `eieio-speedbar-create' as specified in it's documentation
;;    string.   This will automatically handle cases when speedbar is
;;    not already loaded, and specifying all overload functions.
;;
;; 5) Create an initializer function which looks like this:
;;
;; (defun my-speedbar-mode-initialize ()
;;   "documentation"
;;   (interactive)
;;   (speedbar-frame-mode 1)
;;   (speedbar-change-initial-expansion-list mymodename)
;;   (speedbar-get-focus))
;;
;; where `mymodename' is the same value as passed to `eieio-speedbar-create'
;; as the MODENAME parameter.

;; @todo - Can we make this ECB friendly?

;;; Code:
(require 'eieio)
(require 'eieio-custom)
(require 'speedbar)

;;; Support a way of adding generic object based modes into speedbar.
;;
(defun eieio-speedbar-make-map ()
  "Make the generic object based speedbar keymap."
  (let ((map (speedbar-make-specialized-keymap)))

    ;; General viewing things
    (define-key map "\C-m" 'speedbar-edit-line)
    (define-key map "+" 'speedbar-expand-line)
    (define-key map "=" 'speedbar-expand-line)
    (define-key map "-" 'speedbar-contract-line)

    ;; Some object based things
    (define-key map "C" 'eieio-speedbar-customize-line)
    map))

(defvar eieio-speedbar-key-map (eieio-speedbar-make-map)
  "A generic object based speedbar display keymap.")

(defvar eieio-speedbar-menu
  '([ "Edit Object/Field" speedbar-edit-line t]
    [ "Expand Object" speedbar-expand-line
      (save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *.\\+. "))]
    [ "Contract Object" speedbar-contract-line
      (save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *.-. "))]
    "---"
    [ "Customize Object" eieio-speedbar-customize-line
      (eieio-object-p (speedbar-line-token)) ]
    )
  "Menu part in easymenu format used in speedbar while browsing objects.")

;; Note to self:  Fix this silly thing!
(defalias 'eieio-speedbar-customize-line  'speedbar-edit-line)

(defun eieio-speedbar-create (map-fn map-var menu-var modename fetcher)
  "Create a speedbar mode for displaying an object hierarchy.
MAP-FN is the keymap generator function used for extra keys.
MAP-VAR is the keymap variable used.
MENU-VAR is the symbol containing an easymenu compatible menu part to use.
MODENAME is a string used to identify this browser mode.
FETCHER is a generic function used to fetch the base object list used when
creating the speedbar display."
  (if (not (featurep 'speedbar))
      (add-hook 'speedbar-load-hook
		(list 'lambda nil
		      (list 'eieio-speedbar-create-engine
			    map-fn map-var menu-var modename fetcher)))
    (eieio-speedbar-create-engine map-fn map-var menu-var modename fetcher)))

(defun eieio-speedbar-create-engine (map-fn map-var menu-var modename fetcher)
  "Create a speedbar mode for displaying an object hierarchy.
Called from `eieio-speedbar-create', or the speedbar load-hook.
MAP-FN, MAP-VAR, MENU-VAR, MODENAME, and FETCHER are the same as in
`eieio-speedbar-create'."
  ;; make sure the keymap exists
  (funcall map-fn)
  ;; Add to the expansion list.
  (speedbar-add-expansion-list
   (list modename
	 menu-var
	 map-var
	 (list 'lambda '(dir depth)
	       (list 'eieio-speedbar-buttons 'dir 'depth
		     (list 'quote fetcher)))))
  ;; Set the special functions.
  (speedbar-add-mode-functions-list
   (list modename
	 '(speedbar-item-info . eieio-speedbar-item-info)
	 '(speedbar-line-directory . eieio-speedbar-line-path))))

(defun eieio-speedbar-buttons (dir-or-object depth fetcher)
  "Create buttons for the speedbar display.
Start in directory DIR-OR-OBJECT.  If it is an object, just display that
object's subelements.
Argument DEPTH specifies how far down we have already been displayed.
If it is a directory, use FETCHER to fetch all objects associated with
that path."
  (let ((objlst (cond ((eieio-object-p dir-or-object)
		       (list dir-or-object))
		      ((stringp dir-or-object)
		       (funcall fetcher dir-or-object))
		      (t dir-or-object))))
    (if (not objlst)
	(speedbar-make-tag-line nil nil nil nil "Empty display" nil nil nil
				depth)
      ;; Dump all objects into speedbar
      (while objlst
	(eieio-speedbar-make-tag-line (car objlst) depth)
	(setq objlst (cdr objlst))))))


;;; DEFAULT SUPERCLASS baseline methods
;;
;; First, define methods onto the superclass so all classes
;; will have some minor support.

(defmethod eieio-speedbar-description ((object eieio-default-superclass))
  "Return a string describing OBJECT."
  (object-name-string object))

(defmethod eieio-speedbar-derive-line-path ((object eieio-default-superclass))
  "Return the path which OBJECT has something to do with."
  nil)

(defmethod eieio-speedbar-object-buttonname ((object eieio-default-superclass))
  "Return a string to use as a speedbar button for OBJECT."
  (object-name-string object))

(defmethod eieio-speedbar-make-tag-line ((object eieio-default-superclass)
					 depth)
  "Insert a tag line into speedbar at point for OBJECT.
By default, all objects appear as simple TAGS with no need to inherit from
the special `eieio-speedbar' classes.  Child classes should redefine this
method to create more accurate tag lines.
Argument DEPTH is the depth at which the tag line is inserted."
  (speedbar-make-tag-line nil nil nil nil
			  (eieio-speedbar-object-buttonname object)
			  'eieio-speedbar-object-click
			  object
			  'speedbar-tag-face
			  depth))

(defmethod eieio-speedbar-handle-click ((object eieio-default-superclass))
  "Handle a click action on OBJECT in speedbar.
Any object can be represented as a tag in SPEEDBAR without special
attributes.  These default objects will be pulled up in a custom
object edit buffer doing an in-place edit.

If your object represents some other item, override this method
and take the appropriate action."
  (require 'eieio-custom)
  (speedbar-with-attached-buffer
   (eieio-customize-object object))
  (speedbar-maybee-jump-to-attached-frame))


;;; Class definitions
;;
;; Now define a special speedbar class with some
;; variables with :allocation class which can be attached into
;; object hierarchies.
;;
;; These more complex types are for objects which wish to display
;; lists of children buttons.

(defclass eieio-speedbar nil
  ((buttontype :initform nil
	       :type symbol
	       :documentation
	       "The type of expansion button used for objects of this class.
Possible values are those symbols supported by the `exp-button-type' argument
to `speedbar-make-tag-line'."
	       :allocation :class)
   (buttonface :initform speedbar-tag-face
	       :type (or symbol face)
	       :documentation
	       "The face used on the textual part of the button for this class.
See `speedbar-make-tag-line' for details."
	       :allocation :class)
   (expanded :initform nil
	     :type boolean
	     :documentation
	     "State of an object being expanded in speedbar.")
   )
  "Class which provides basic speedbar support for child classes.
Add one of the child classes to this class to the parent list of a class."
  :method-invocation-order :depth-first
  :abstract t)

(defclass eieio-speedbar-directory-button (eieio-speedbar)
  ((buttontype :initform angle)
   (buttonface :initform speedbar-directory-face))
  "Class providing support for objects which behave like a directory."
  :method-invocation-order :depth-first
  :abstract t)

(defclass eieio-speedbar-file-button (eieio-speedbar)
  ((buttontype :initform bracket)
   (buttonface :initform speedbar-file-face))
  "Class providing support for objects which behave like a file."
  :method-invocation-order :depth-first
  :abstract t)


;;; Methods to eieio-speedbar-* which do not need to be overridden
;;
(defmethod eieio-speedbar-make-tag-line ((object eieio-speedbar)
					 depth)
  "Insert a tag line into speedbar at point for OBJECT.
All objects a child of symbol `eieio-speedbar' can be created from
this method.  Override this if you need non-traditional tag lines.
Argument DEPTH is the depth at which the tag line is inserted."
  (let ((children (eieio-speedbar-object-children object))
	(exp (oref object expanded)))
    (if (not children)
	(if (eq (oref object buttontype) 'expandtag)
	    (speedbar-make-tag-line 'statictag
				    ?  nil nil
				    (eieio-speedbar-object-buttonname object)
				    'eieio-speedbar-object-click
				    object
				    (oref object buttonface)
				    depth)
	  (speedbar-make-tag-line (oref object buttontype)
				  ?  nil nil
				  (eieio-speedbar-object-buttonname object)
				  'eieio-speedbar-object-click
				  object
				  (oref object buttonface)
				  depth))
      (speedbar-make-tag-line (oref object buttontype)
			      (if exp ?- ?+)
			      'eieio-speedbar-object-expand
			      object
			      (eieio-speedbar-object-buttonname object)
			      'eieio-speedbar-object-click
			      object
			      (oref object buttonface)
			      depth)
      (if exp
	  (eieio-speedbar-expand object (1+ depth))))))

(defmethod eieio-speedbar-child-make-tag-lines ((object eieio-speedbar) depth)
  "Base method for creating tag lines for non-object children."
  (error "You must implement `eieio-speedbar-child-make-tag-lines' for %s"
	 (object-name object)))

(defmethod eieio-speedbar-expand ((object eieio-speedbar) depth)
  "Expand OBJECT at indentation DEPTH.
Inserts a list of new tag lines representing expanded elements within
OBJECT."
  (let ((children (eieio-speedbar-object-children object)))
    (cond ((eieio-object-p (car children))
	   (mapcar (lambda (car)
		     (eieio-speedbar-make-tag-line car depth))
		   children))
	  (children (eieio-speedbar-child-make-tag-lines object depth)))))


;;; Speedbar specific function callbacks.
;;
(defun eieio-speedbar-object-click (text token indent)
  "Handle a user click on TEXT representing object TOKEN.
The object is at indentation level INDENT."
  (eieio-speedbar-handle-click token))

(defun eieio-speedbar-object-expand (text token indent)
  "Expand object represented by TEXT.
TOKEN is the object.  INDENT is the current indentation level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (oset token expanded t)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (eieio-speedbar-expand token (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (oset token expanded nil)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defmethod eieio-speedbar-child-description ((obj eieio-speedbar))
  "Return a description for a child of OBJ which is not an object."
  (error "You must implement `eieio-speedbar-child-description' for %s"
	 (object-name obj)))

(defun eieio-speedbar-item-info ()
  "Display info for the current line when in EDE display mode."
  ;; Switch across the types of the tokens.
  (let ((tok (speedbar-line-token)))
    (cond ((eieio-object-p tok)
	   (message (eieio-speedbar-description tok)))
	  (t
	   (let ((no (eieio-speedbar-find-nearest-object)))
	     (if no
		 (eieio-speedbar-child-description no)))))))

(defun eieio-speedbar-find-nearest-object (&optional depth)
  "Search backwards to the first line associated with an object.
Optional argument DEPTH is the current depth of the search."
  (save-excursion
    (if (not depth)
	(progn
	  (beginning-of-line)
	  (when (looking-at "^\\([0-9]+\\):")
	    (setq depth (string-to-number (match-string 1))))))
    (when depth
      (while (and (not (eieio-object-p (speedbar-line-token)))
		  (> depth 0))
	(setq depth (1- depth))
	(re-search-backward (format "^%d:" depth) nil t))
      (speedbar-line-token))))

(defun eieio-speedbar-line-path (&optional depth)
  "If applicable, return the path to the file the cursor is on.
Optional DEPTH is the depth we start at."
  (save-match-data
    (if (not depth)
	(progn
	  (beginning-of-line)
	  (looking-at "^\\([0-9]+\\):")
	  (setq depth (string-to-number (match-string 1)))))
    ;; This whole function is presently bogus.  Make it better later.
    (let ((tok (eieio-speedbar-find-nearest-object depth)))
      (if (eieio-object-p tok)
	  (eieio-speedbar-derive-line-path tok)
	default-directory))))


;;; Methods to the eieio-speedbar-* classes which need to be overridden.
;;
(defmethod eieio-speedbar-object-children ((object eieio-speedbar))
  "Return a list of children to be displayed in speedbar.
If the return value is a list of OBJECTs, then those objects are
queried for details.  If the return list is made of strings,
then this object will be queried for the details needed
to create a speedbar button."
  nil)

(provide 'eieio-speedbar)

;;; eieio-speedbar.el ends here
