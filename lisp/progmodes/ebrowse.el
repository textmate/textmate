;;; ebrowse.el --- Emacs C++ class browser & tags facility

;; Copyright (C) 1992-2012  Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: FSF
;; Keywords: C++ tags tools

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

;; This package implements

;; - A class browser for C++
;; - A complete set of tags-like functions working on class trees
;; - An electric buffer list showing class browser buffers only

;; Documentation is found in a separate Info file.

;;; Code:

(require 'easymenu)
(require 'view)
(require 'ebuff-menu)

(eval-when-compile
  (require 'cl)
  (require 'helper))


;;; User-options

(defgroup ebrowse nil
  "Settings for the C++ class browser."
  :group 'tools)


(defcustom ebrowse-search-path nil
  "*List of directories to search for source files in a class tree.
Elements should be directory names; nil as an element means to try
to find source files relative to the location of the BROWSE file loaded."
  :group 'ebrowse
  :type '(repeat (choice (const :tag "Default" nil)
                         (string :tag "Directory"))))


(defcustom ebrowse-view/find-hook nil
  "*Hooks run after finding or viewing a member or class."
  :group 'ebrowse
  :type 'hook)


(defcustom ebrowse-not-found-hook nil
  "*Hooks run when finding or viewing a member or class was not successful."
  :group 'ebrowse
  :type 'hook)


(defcustom ebrowse-electric-list-mode-hook nil
  "*Hook called by `ebrowse-electric-position-mode'."
  :group 'ebrowse
  :type 'hook)


(defcustom ebrowse-max-positions 50
  "*Number of markers saved on electric position stack."
  :group 'ebrowse
  :type 'integer)



(defgroup ebrowse-tree nil
  "Settings for class tree buffers."
  :group 'ebrowse)


(defcustom ebrowse-tree-mode-hook nil
  "*Hook run in each new tree buffer."
  :group 'ebrowse-tree
  :type 'hook)


(defcustom ebrowse-tree-buffer-name "*Tree*"
  "*The default name of class tree buffers."
  :group 'ebrowse-tree
  :type 'string)


(defcustom ebrowse--indentation 4
  "*The amount by which subclasses are indented in the tree."
  :group 'ebrowse-tree
  :type 'integer)


(defcustom ebrowse-source-file-column 40
  "*The column in which source file names are displayed in the tree."
  :group 'ebrowse-tree
  :type 'integer)


(defcustom ebrowse-tree-left-margin 2
  "*Amount of space left at the left side of the tree display.
This space is used to display markers."
  :group 'ebrowse-tree
  :type 'integer)



(defgroup ebrowse-member nil
  "Settings for member buffers."
  :group 'ebrowse)


(defcustom ebrowse-default-declaration-column 25
  "*The column in which member declarations are displayed in member buffers."
  :group 'ebrowse-member
  :type 'integer)


(defcustom ebrowse-default-column-width 25
  "*The width of the columns in member buffers (short display form)."
  :group 'ebrowse-member
  :type 'integer)


(defcustom ebrowse-member-buffer-name "*Members*"
  "*The name of the buffer for member display."
  :group 'ebrowse-member
  :type 'string)


(defcustom ebrowse-member-mode-hook nil
  "*Run in each new member buffer."
  :group 'ebrowse-member
  :type 'hook)



(defgroup ebrowse-faces nil
  "Faces used by Ebrowse."
  :group 'ebrowse)


(defface ebrowse-tree-mark
  '((((min-colors 88)) (:foreground "red1"))
    (t (:foreground "red")))
  "*The face used for the mark character in the tree."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-tree-mark-face 'ebrowse-tree-mark "22.1")


(defface ebrowse-root-class
  '((((min-colors 88)) (:weight bold :foreground "blue1"))
    (t (:weight bold :foreground "blue")))
  "*The face used for root classes in the tree."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-root-class-face 'ebrowse-root-class "22.1")


(defface ebrowse-file-name
  '((t (:italic t)))
  "*The face for filenames displayed in the tree."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-file-name-face 'ebrowse-file-name "22.1")


(defface ebrowse-default
  '((t nil))
  "*Face for everything else in the tree not having other faces."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-default-face 'ebrowse-default "22.1")


(defface ebrowse-member-attribute
  '((((min-colors 88)) (:foreground "red1"))
    (t (:foreground "red")))
  "*Face used to display member attributes."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-member-attribute-face
  'ebrowse-member-attribute "22.1")


(defface ebrowse-member-class
  '((t (:foreground "purple")))
  "*Face used to display the class title in member buffers."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-member-class-face
  'ebrowse-member-class "22.1")


(defface ebrowse-progress
  '((((min-colors 88)) (:background "blue1"))
    (t (:background "blue")))
  "*Face for progress indicator."
  :group 'ebrowse-faces)
(define-obsolete-face-alias 'ebrowse-progress-face 'ebrowse-progress "22.1")



;;; Utilities.

(defun ebrowse-some (predicate vector)
  "Return true if PREDICATE is true of some element of VECTOR.
If so, return the value returned by PREDICATE."
  (let ((length (length vector))
	(i 0)
	result)
    (while (and (< i length) (not result))
      (setq result (funcall predicate (aref vector i))
	    i (1+ i)))
    result))


(defun ebrowse-every (predicate vector)
  "Return true if PREDICATE is true of every element of VECTOR."
  (let ((length (length vector))
	(i 0)
	(result t))
    (while (and (< i length) result)
      (setq result (funcall predicate (aref vector i))
	    i (1+ i)))
    result))


(defun ebrowse-position (item list &optional test)
  "Return the position of ITEM in LIST or nil if not found.
Compare items with `eq' or TEST if specified."
  (let ((i 0) found)
    (cond (test
	   (while list
	     (when (funcall test item (car list))
	       (setq found i list nil))
	     (setq list (cdr list) i (1+ i))))
	  (t
	   (while list
	     (when (eq item (car list))
	       (setq found i list nil))
	     (setq list (cdr list) i (1+ i)))))
    found))


(defun ebrowse-delete-if-not (predicate list)
  "Remove elements not satisfying PREDICATE from LIST and return the result.
This is a destructive operation."
  (let (result)
    (while list
      (let ((next (cdr list)))
	(when (funcall predicate (car list))
	  (setq result (nconc result list))
	  (setf (cdr list) nil))
	(setq list next)))
    result))


(defmacro ebrowse-output (&rest body)
  "Eval BODY with a writable current buffer.
Preserve buffer's modified state."
  (let ((modified (make-symbol "--ebrowse-output--")))
    `(let (buffer-read-only (,modified (buffer-modified-p)))
       (unwind-protect
	   (progn ,@body)
	 (set-buffer-modified-p ,modified)))))


(defmacro ebrowse-ignoring-completion-case (&rest body)
  "Eval BODY with `completion-ignore-case' bound to t."
  `(let ((completion-ignore-case t))
     ,@body))


(defmacro ebrowse-save-selective (&rest body)
  "Eval BODY with `selective-display' restored at the end."
  (let ((var (make-symbol "var")))
    `(let ((,var selective-display))
       (unwind-protect
	   (progn ,@body)
	 (setq selective-display ,var)))))


(defmacro ebrowse-for-all-trees (spec &rest body)
  "For all trees in SPEC, eval BODY."
  (let ((var (make-symbol "var"))
	(spec-var (car spec))
	(array (cadr spec)))
    `(loop for ,var being the symbols of ,array
	   as ,spec-var = (get ,var 'ebrowse-root) do
	   (when (vectorp ,spec-var)
	     ,@body))))

;;; Set indentation for macros above.

(put 'ebrowse-output 'lisp-indent-hook 0)
(put 'ebrowse-ignoring-completion-case 'lisp-indent-hook 0)
(put 'ebrowse-save-selective 'lisp-indent-hook 0)
(put 'ebrowse-for-all-trees 'lisp-indent-hook 1)


(defsubst ebrowse-set-face (start end face)
  "Set face of a region START END to FACE."
  (overlay-put (make-overlay start end) 'face face))


(defun ebrowse-completing-read-value (prompt table initial-input)
  "Read a string in the minibuffer, with completion.
Case is ignored in completions.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
TABLE can also be a function to do the completion itself.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
If it is (STRING . POSITION), the initial input
is STRING, but point is placed POSITION characters into the string."
  (ebrowse-ignoring-completion-case
    (completing-read prompt table nil t initial-input)))


(defun ebrowse-value-in-buffer (sym buffer)
  "Return the value of SYM in BUFFER."
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer buffer)
          (symbol-value sym))
      (set-buffer old-buffer))))


(defun ebrowse-rename-buffer (new-name)
  "Rename current buffer to NEW-NAME.
If a buffer with name NEW-NAME already exists, delete it first."
  (let ((old-buffer (get-buffer new-name)))
    (unless (eq old-buffer (current-buffer))
      (when old-buffer
        (save-excursion (kill-buffer old-buffer)))
      (rename-buffer new-name))))


(defun ebrowse-trim-string (string)
  "Return a copy of STRING with leading white space removed.
Replace sequences of newlines with a single space."
  (when (string-match "^[ \t\n\r]+" string)
    (setq string (substring string (match-end 0))))
  (loop while (string-match "[\n]+" string)
        finally return string do
	(setq string (replace-match " " nil t string))))


(defun ebrowse-width-of-drawable-area ()
  "Return the width of the display area for the current buffer.
If buffer is displayed in a window, use that window's width,
otherwise use the current frame's width."
  (let ((window (get-buffer-window (current-buffer))))
    (if window
        (window-width window)
      (frame-width))))


;;; Structure definitions

(defstruct (ebrowse-hs (:type vector) :named)
  "Header structure found at the head of BROWSE files."
  ;; A version string that is compared against the version number of
  ;; the Lisp package when the file is loaded.  This is done to
  ;; detect file format changes.
  version
  ;; Command line options used for producing the BROWSE file.
  command-line-options
  ;; The following slot is currently not used.  It's kept to keep
  ;; the file format compatible.
  unused
  ;; A slot that is filled out after the tree is loaded.  This slot is
  ;; set to a hash table mapping members to lists of classes in which
  ;; they are defined.
  member-table)


(defstruct (ebrowse-ts (:type vector) :named)
  "Tree structure.
Following the header structure, a BROWSE file contains a number
of `ebrowse-ts' structures, each one describing one root class of
the class hierarchy with all its subclasses."
  ;; A `ebrowse-cs' structure describing the root class.
  class
  ;; A list of `ebrowse-ts' structures for all subclasses.
  subclasses
  ;; Lists of `ebrowse-ms' structures for each member in a group of
  ;; members.
  member-variables member-functions static-variables static-functions
  friends types
  ;; List of `ebrowse-ts' structures for base classes.  This slot is
  ;; filled at load time.
  base-classes
  ;; A marker slot used in the tree buffer (can be saved back to disk.
  mark)


(defstruct (ebrowse-bs (:type vector) :named)
  "Common sub-structure.
A common structure defining an occurrence of some name in the
source files."
  ;; The class or member name as a string constant
  name
  ;; An optional string for the scope of nested classes or for
  ;; namespaces.
  scope
  ;; Various flags describing properties of classes/members, e.g. is
  ;; template, is const etc.
  flags
  ;; File in which the entity is found.  If this is part of a
  ;; `ebrowse-ms' member description structure, and FILE is nil, then
  ;; search for the name in the SOURCE-FILE of the members class.
  file
  ;; Regular expression to search for.  This slot can be a number in
  ;; which case the number is the file position at which the regular
  ;; expression is found in a separate regexp file (see the header
  ;; structure).  This slot can be nil in which case the regular
  ;; expression will be generated from the class/member name.
  pattern
  ;; The buffer position at which the search for the class or member
  ;; will start.
  point)


(defstruct (ebrowse-cs (:include ebrowse-bs) (:type vector) :named)
  "Class structure.
This is the structure stored in the CLASS slot of a `ebrowse-ts'
structure.  It describes the location of the class declaration."
  source-file)


(defstruct (ebrowse-ms (:include ebrowse-bs) (:type vector) :named)
  "Member structure.
This is the structure describing a single member.  The `ebrowse-ts'
structure contains various lists for the different types of
members."
  ;; Public, protected, private
  visibility
  ;; The file in which the member's definition can be found.
  definition-file
  ;; Same as PATTERN above, but for the member definition.
  definition-pattern
  ;; Same as POINT above but for member definition.
  definition-point)



;;; Some macros to access the FLAGS slot of a MEMBER.

(defsubst ebrowse-member-bit-set-p (member bit)
  "Value is non-nil if MEMBER's bit BIT is set."
  (/= 0 (logand (ebrowse-bs-flags member) bit)))


(defsubst ebrowse-virtual-p (member)
  "Value is non-nil if MEMBER is virtual."
  (ebrowse-member-bit-set-p member 1))


(defsubst ebrowse-inline-p (member)
  "Value is non-nil if MEMBER is inline."
  (ebrowse-member-bit-set-p member 2))


(defsubst ebrowse-const-p (member)
  "Value is non-nil if MEMBER is const."
  (ebrowse-member-bit-set-p member 4))


(defsubst ebrowse-pure-virtual-p (member)
  "Value is non-nil if MEMBER is a pure virtual function."
  (ebrowse-member-bit-set-p member 8))


(defsubst ebrowse-mutable-p (member)
  "Value is non-nil if MEMBER is mutable."
  (ebrowse-member-bit-set-p member 16))


(defsubst ebrowse-template-p (member)
  "Value is non-nil if MEMBER is a template."
  (ebrowse-member-bit-set-p member 32))


(defsubst ebrowse-explicit-p (member)
  "Value is non-nil if MEMBER is explicit."
  (ebrowse-member-bit-set-p member 64))


(defsubst ebrowse-throw-list-p (member)
  "Value is non-nil if MEMBER has a throw specification."
  (ebrowse-member-bit-set-p member 128))


(defsubst ebrowse-extern-c-p (member)
  "Value is non-nil if MEMBER.is `extern \"C\"'."
  (ebrowse-member-bit-set-p member 256))


(defsubst ebrowse-define-p (member)
  "Value is non-nil if MEMBER is a define."
  (ebrowse-member-bit-set-p member 512))


(defconst ebrowse-version-string "ebrowse 5.0"
  "Version string expected in BROWSE files.")


(defconst ebrowse-globals-name "*Globals*"
  "The name used for the surrogate class.containing global entities.
This must be the same that `ebrowse' uses.")


(defvar ebrowse--last-regexp nil
  "Last regular expression searched for in tree and member buffers.
Each tree and member buffer maintains its own search history.")
(make-variable-buffer-local 'ebrowse--last-regexp)


(defconst ebrowse-member-list-accessors
  '(ebrowse-ts-member-variables
    ebrowse-ts-member-functions
    ebrowse-ts-static-variables
    ebrowse-ts-static-functions
    ebrowse-ts-friends
    ebrowse-ts-types)
  "List of accessors for member lists.
Each element is the symbol of an accessor function.
The nth element must be the accessor for the nth member list
in an `ebrowse-ts' structure.")


;;; FIXME: Add more doc strings for the buffer-local variables below.

(defvar ebrowse--tree-obarray nil
  "Obarray holding all `ebrowse-ts' structures of a class tree.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--tags-file-name nil
  "File from which BROWSE file was loaded.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--header nil
  "Header structure of type `ebrowse-hs' of a class tree.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--frozen-flag nil
  "Non-nil means an Ebrowse buffer won't be reused.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--show-file-names-flag nil
  "Non-nil means show file names in a tree buffer.
Buffer-local in Ebrowse tree buffers.")


(defvar ebrowse--long-display-flag nil
  "Non-nil means show members in long display form.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--n-columns nil
  "Number of columns to display for short member display form.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--column-width nil
  "Width of a columns to display for short member display form.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--virtual-display-flag nil
  "Non-nil means display virtual members in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--inline-display-flag nil
  "Non-nil means display inline members in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--const-display-flag nil
  "Non-nil means display const members in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--pure-display-flag nil
  "Non-nil means display pure virtual members in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--filters nil
  "Filter for display of public, protected, and private members.
This is a vector of three elements.  An element nil means the
corresponding members are not shown.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--show-inherited-flag nil
  "Non-nil means display inherited members in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--attributes-flag nil
  "Non-nil means display member attributes in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--source-regexp-flag nil
  "Non-nil means display member regexps in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--displayed-class nil
  "Class displayed in a member buffer, a `ebrowse-ts' structure.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--accessor nil
  "Member list displayed in a member buffer.
This is a symbol whose function definition is an accessor for the
member list in `ebrowse-cs' structures.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--member-list nil
  "The list of `ebrowse-ms' structures displayed in a member buffer.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--decl-column nil
  "Column in which declarations are displayed in member buffers.
Buffer-local in Ebrowse member buffers.")


(defvar ebrowse--frame-configuration nil
  "Frame configuration saved when viewing a class/member in another frame.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--view-exit-action nil
  "Action to perform after viewing a class/member.
Either `kill-buffer' or nil.
Buffer-local in Ebrowse buffers.")


(defvar ebrowse--tree nil
  "Class tree.
Buffer-local in Ebrowse buffers.")


;;; Temporaries used to communicate with `ebrowse-find-pattern'.

(defvar ebrowse-temp-position-to-view nil)
(defvar ebrowse-temp-info-to-view nil)


(defvar ebrowse-tree-mode-map ()
  "The keymap used in tree mode buffers.")


(defvar ebrowse--member-mode-strings nil
  "Strings displayed in the mode line of member buffers.")


(defvar ebrowse-member-mode-map ()
  "The keymap used in the member buffers.")


;;; Define mode line titles for each member list.

(put 'ebrowse-ts-member-variables 'ebrowse-title "Member Variables")
(put 'ebrowse-ts-member-functions 'ebrowse-title "Member Functions")
(put 'ebrowse-ts-static-variables 'ebrowse-title "Static Variables")
(put 'ebrowse-ts-static-functions 'ebrowse-title "Static Functions")
(put 'ebrowse-ts-friends 'ebrowse-title "Friends")
(put 'ebrowse-ts-types 'ebrowse-title "Types")

(put 'ebrowse-ts-member-variables 'ebrowse-global-title "Global Variables")
(put 'ebrowse-ts-member-functions 'ebrowse-global-title "Global Functions")
(put 'ebrowse-ts-static-variables 'ebrowse-global-title "Static Variables")
(put 'ebrowse-ts-static-functions 'ebrowse-global-title "Static Functions")
(put 'ebrowse-ts-friends 'ebrowse-global-title "Defines")
(put 'ebrowse-ts-types 'ebrowse-global-title "Types")



;;; Operations on `ebrowse-ts' structures

(defun ebrowse-files-table (&optional marked-only)
  "Return an obarray containing all files mentioned in the current tree.
The tree is expected in the buffer-local variable `ebrowse--tree-obarray'.
MARKED-ONLY non-nil means include marked classes only."
  (let ((files (make-hash-table :test 'equal))
	(i -1))
    (ebrowse-for-all-trees (tree ebrowse--tree-obarray)
      (when (or (not marked-only) (ebrowse-ts-mark tree))
	(let ((class (ebrowse-ts-class tree)))
	  (when (zerop (% (incf i) 20))
	    (ebrowse-show-progress "Preparing file list" (zerop i)))
	  ;; Add files mentioned in class description
	  (let ((source-file (ebrowse-cs-source-file class))
		(file        (ebrowse-cs-file class)))
	    (when source-file
	      (puthash source-file source-file files))
	    (when file
	      (puthash file file files))
	    ;; For all member lists in this class
	    (loop for accessor in ebrowse-member-list-accessors do
		  (loop for m in (funcall accessor tree)
			for file = (ebrowse-ms-file m)
			for def-file = (ebrowse-ms-definition-file m) do
			(when file
			  (puthash file file files))
			(when def-file
			  (puthash def-file def-file files))))))))
    files))


(defun ebrowse-files-list (&optional marked-only)
  "Return a list containing all files mentioned in a tree.
MARKED-ONLY non-nil means include marked classes only."
  (let (list)
    (maphash (lambda (file _dummy) (setq list (cons file list)))
	     (ebrowse-files-table marked-only))
    list))


(defun* ebrowse-marked-classes-p ()
  "Value is non-nil if any class in the current class tree is marked."
  (ebrowse-for-all-trees (tree ebrowse--tree-obarray)
    (when (ebrowse-ts-mark tree)
      (return-from ebrowse-marked-classes-p tree))))


(defsubst ebrowse-globals-tree-p (tree)
  "Return t if TREE is the one for global entities."
  (string= (ebrowse-bs-name (ebrowse-ts-class tree))
	   ebrowse-globals-name))


(defsubst ebrowse-qualified-class-name (class)
  "Return the name of CLASS with scope prepended, if any."
  (if (ebrowse-cs-scope class)
      (concat (ebrowse-cs-scope class) "::" (ebrowse-cs-name class))
    (ebrowse-cs-name class)))


(defun ebrowse-tree-obarray-as-alist (&optional qualified-names-p)
  "Return an alist describing all classes in a tree.
Each elements in the list has the form (CLASS-NAME . TREE).
CLASS-NAME is the name of the class.  TREE is the
class tree whose root is QUALIFIED-CLASS-NAME.
QUALIFIED-NAMES-P non-nil means return qualified names as CLASS-NAME.
The class tree is found in the buffer-local variable `ebrowse--tree-obarray'."
  (let (alist)
    (if qualified-names-p
	(ebrowse-for-all-trees (tree ebrowse--tree-obarray)
	  (setq alist
		(acons (ebrowse-qualified-class-name (ebrowse-ts-class tree))
		       tree alist)))
      (ebrowse-for-all-trees (tree ebrowse--tree-obarray)
	(setq alist
	      (acons (ebrowse-cs-name (ebrowse-ts-class tree))
		     tree alist))))
    alist))


(defun ebrowse-sort-tree-list (list)
  "Sort a LIST of `ebrowse-ts' structures by qualified class names."
  (sort list
	(lambda (a b)
	  (string< (ebrowse-qualified-class-name (ebrowse-ts-class a))
		   (ebrowse-qualified-class-name (ebrowse-ts-class b))))))


(defun ebrowse-class-in-tree (class tree)
  "Search for a class with name CLASS in TREE.
If CLASS is found, return the tail of TREE starting at CLASS.  This function
is used during the load phase where classes appended to a file replace older
class information."
  (let ((tclass (ebrowse-ts-class class))
	found)
    (while (and tree (not found))
      (let ((root-ptr tree))
	(when (string= (ebrowse-qualified-class-name (ebrowse-ts-class (car root-ptr)))
		       (ebrowse-qualified-class-name tclass))
	  (setq found root-ptr))
	(setq tree (cdr tree))))
    found))


(defun ebrowse-base-classes (tree)
  "Return list of base-classes of TREE by searching subclass lists.
This function must be used instead of the struct slot
`base-classes' to access the base-class list directly because it
computes this information lazily."
  (or (ebrowse-ts-base-classes tree)
      (setf (ebrowse-ts-base-classes tree)
	    (loop with to-search = (list tree)
		  with result = nil
		  as search = (pop to-search)
		  while search finally return result
		  do (ebrowse-for-all-trees (ti ebrowse--tree-obarray)
		       (when (memq search (ebrowse-ts-subclasses ti))
			 (unless (memq ti result)
			   (setq result (nconc result (list ti))))
			 (push ti to-search)))))))


(defun ebrowse-direct-base-classes (tree)
  "Return the list of direct super classes of TREE."
  (let (result)
    (dolist (s (ebrowse-base-classes tree))
      (when (memq tree (ebrowse-ts-subclasses s))
	(setq result (cons s result))))
    result))



;;; Operations on MEMBER structures/lists

(defun ebrowse-name/accessor-alist (tree accessor)
  "Return an alist containing all members of TREE in group ACCESSOR.
ACCESSOR is the accessor function for the member list.
Elements of the result have the form (NAME . ACCESSOR), where NAME
is the member name."
  (loop for member in (funcall accessor tree)
	collect (cons (ebrowse-ms-name member) accessor)))


(defun ebrowse-name/accessor-alist-for-visible-members ()
  "Return an alist describing all members visible in the current buffer.
Each element of the list has the form (MEMBER-NAME . ACCESSOR),
where MEMBER-NAME is the member's name, and ACCESSOR is the struct
accessor with which the member's list can be accessed in an `ebrowse-ts'
structure.  The list includes inherited members if these are visible."
  (let* ((list (ebrowse-name/accessor-alist ebrowse--displayed-class
					    ebrowse--accessor)))
    (if ebrowse--show-inherited-flag
	(nconc list
	       (loop for tree in (ebrowse-base-classes
				  ebrowse--displayed-class)
		     nconc (ebrowse-name/accessor-alist
			    tree ebrowse--accessor)))
      list)))


(defun ebrowse-name/accessor-alist-for-class-members ()
  "Like `ebrowse-name/accessor-alist-for-visible-members'.
This function includes members of base classes if base class members
are visible in the buffer."
  (let (list)
    (dolist (func ebrowse-member-list-accessors list)
      (setq list (nconc list (ebrowse-name/accessor-alist
			      ebrowse--displayed-class func)))
      (when ebrowse--show-inherited-flag
	(dolist (class (ebrowse-base-classes ebrowse--displayed-class))
	  (setq list
		(nconc list (ebrowse-name/accessor-alist class func))))))))


;;; Progress indication

(defvar ebrowse-n-boxes 0)
(defconst ebrowse-max-boxes 60)

(defun ebrowse-show-progress (title &optional start)
  "Display a progress indicator.
TITLE is the title of the progress message.  START non-nil means
this is the first progress message displayed."
  (let (message-log-max)
    (when start (setq ebrowse-n-boxes 0))
    (setq ebrowse-n-boxes (mod (1+ ebrowse-n-boxes) ebrowse-max-boxes))
    (message "%s: %s" title
	     (propertize (make-string ebrowse-n-boxes
				      (if (display-color-p) ?\  ?+))
			 'face 'ebrowse-progress))))


;;; Reading a tree from disk

(defun ebrowse-read ()
  "Read `ebrowse-hs' and `ebrowse-ts' structures in the current buffer.
Return a list (HEADER TREE) where HEADER is the file header read
and TREE is a list of `ebrowse-ts' structures forming the class tree."
  (let ((header (condition-case nil
		    (read (current-buffer))
		  (error (error "No Ebrowse file header found"))))
	tree)
    ;; Check file format.
    (unless (ebrowse-hs-p header)
      (error "No Ebrowse file header found"))
    (unless (string= (ebrowse-hs-version header) ebrowse-version-string)
      (error "File has wrong version `%s' (`%s' expected)"
	     (ebrowse-hs-version header) ebrowse-version-string))
    ;; Read Lisp objects.  Temporarily increase `gc-cons-threshold' to
    ;; prevent a GC that would not free any memory.
    (let ((gc-cons-threshold 2000000))
      (while (not (progn (skip-chars-forward " \t\n\r") (eobp)))
	(let* ((root (read (current-buffer)))
	       (old-root-ptr (ebrowse-class-in-tree root tree)))
	  (ebrowse-show-progress "Reading data" (null tree))
	  (if old-root-ptr
	      (setcar old-root-ptr root)
	    (push root tree)))))
    (garbage-collect)
    (list header tree)))


(defun ebrowse-revert-tree-buffer-from-file (_ignore-auto-save noconfirm)
  "Function installed as `revert-buffer-function' in tree buffers.
See that variable's documentation for the meaning of IGNORE-AUTO-SAVE and
NOCONFIRM."
  (when (or noconfirm (yes-or-no-p "Revert tree from disk? "))
    (loop for member-buffer in (ebrowse-same-tree-member-buffer-list)
	  do (kill-buffer member-buffer))
    (erase-buffer)
    (with-no-warnings
      (insert-file (or buffer-file-name ebrowse--tags-file-name)))
    (ebrowse-tree-mode)
    (current-buffer)))


(defun ebrowse-create-tree-buffer (tree tags-file header classes pop)
  "Create a new tree buffer for tree TREE.
The tree was loaded from file TAGS-FILE.
HEADER is the header structure of the file.
CLASSES is an obarray with a symbol for each class in the tree.
POP non-nil means popup the buffer up at the end.
Return the buffer created."
  (let ((name ebrowse-tree-buffer-name))
    (set-buffer (get-buffer-create name))
    (ebrowse-tree-mode)
    (setq ebrowse--tree tree
	  ebrowse--tags-file-name tags-file
	  ebrowse--tree-obarray classes
	  ebrowse--header header
	  ebrowse--frozen-flag nil)
    (ebrowse-redraw-tree)
    (set-buffer-modified-p nil)
    (case pop
      (switch (switch-to-buffer name))
      (pop (pop-to-buffer name)))
    (current-buffer)))



;;; Operations for member obarrays

(defun ebrowse-fill-member-table ()
  "Return an obarray holding all members of all classes in the current tree.

For each member, a symbol is added to the obarray.  Members are
extracted from the buffer-local tree `ebrowse--tree-obarray'.

Each symbol has its property `ebrowse-info' set to a list (TREE MEMBER-LIST
MEMBER) where TREE is the tree in which the member is defined,
MEMBER-LIST is a symbol describing the member list in which the member
is found, and MEMBER is a MEMBER structure describing the member.

The slot `member-table' of the buffer-local header structure of
type `ebrowse-hs' is set to the resulting obarray."
  (let ((members (make-hash-table :test 'equal))
	(i -1))
    (setf (ebrowse-hs-member-table ebrowse--header) nil)
    (garbage-collect)
    ;; For all classes...
    (ebrowse-for-all-trees (c ebrowse--tree-obarray)
      (when (zerop (% (incf i) 10))
	(ebrowse-show-progress "Preparing member lookup" (zerop i)))
      (loop for f in ebrowse-member-list-accessors do
	    (loop for m in (funcall f c) do
		  (let* ((member-name (ebrowse-ms-name m))
			 (value (gethash member-name members)))
		    (push (list c f m) value)
		    (puthash member-name value members)))))
    (setf (ebrowse-hs-member-table ebrowse--header) members)))


(defun ebrowse-member-table (header)
  "Return the member obarray.  Build it if it hasn't been set up yet.
HEADER is the tree header structure of the class tree."
  (when (null (ebrowse-hs-member-table header))
    (loop for buffer in (ebrowse-browser-buffer-list)
	  until (eq header (ebrowse-value-in-buffer 'ebrowse--header buffer))
	  finally do
	  (with-current-buffer buffer
	    (ebrowse-fill-member-table))))
  (ebrowse-hs-member-table header))



;;; Operations on TREE obarrays

(defun ebrowse-build-tree-obarray (tree)
  "Make sure every class in TREE is represented by a unique object.
Build obarray of all classes in TREE."
  (let ((classes (make-vector 127 0)))
    ;; Add root classes...
    (loop for root in tree
	  as sym =
	  (intern (ebrowse-qualified-class-name (ebrowse-ts-class root)) classes)
	  do (unless (get sym 'ebrowse-root)
	       (setf (get sym 'ebrowse-root) root)))
    ;; Process subclasses
    (ebrowse-insert-supers tree classes)
    classes))


(defun ebrowse-insert-supers (tree classes)
  "Build base class lists in class tree TREE.
CLASSES is an obarray used to collect classes.

Helper function for `ebrowse-build-tree-obarray'.  Base classes should
be ordered so that immediate base classes come first, then the base
class of the immediate base class and so on.  This means that we must
construct the base-class list top down with adding each level at the
beginning of the base-class list.

We have to be cautious here not to end up in an infinite recursion
if for some reason a circle is in the inheritance graph."
  (loop for class in tree
	as subclasses = (ebrowse-ts-subclasses class) do
	;; Make sure every class is represented by a unique object
	(loop for subclass on subclasses
	      as sym = (intern
			(ebrowse-qualified-class-name (ebrowse-ts-class (car subclass)))
			classes)
	      as next = nil
	      do
	      ;; Replace the subclass tree with the one found in
	      ;; CLASSES if there is already an entry for that class
	      ;; in it. Otherwise make a new entry.
	      ;;
	      ;; CAVEAT: If by some means (e.g., use of the
	      ;; preprocessor in class declarations, a name is marked
	      ;; as a subclass of itself on some path, we would end up
	      ;; in an endless loop. We have to omit subclasses from
	      ;; the recursion that already have been processed.
	      (if (get sym 'ebrowse-root)
		  (setf (car subclass) (get sym 'ebrowse-root))
		(setf (get sym 'ebrowse-root) (car subclass))))
	;; Process subclasses
	(ebrowse-insert-supers subclasses classes)))


;;; Tree buffers

(unless ebrowse-tree-mode-map
  (let ((map (make-keymap)))
    (setf ebrowse-tree-mode-map map)
    (suppress-keymap map)

    (when (display-mouse-p)
      (define-key map [down-mouse-3] 'ebrowse-mouse-3-in-tree-buffer)
      (define-key map [mouse-2] 'ebrowse-mouse-2-in-tree-buffer)
      (define-key map [down-mouse-1] 'ebrowse-mouse-1-in-tree-buffer))

    (let ((map1 (make-sparse-keymap)))
      (suppress-keymap map1 t)
      (define-key map "L" map1)
      (define-key map1 "d" 'ebrowse-tree-command:show-friends)
      (define-key map1 "f" 'ebrowse-tree-command:show-member-functions)
      (define-key map1 "F" 'ebrowse-tree-command:show-static-member-functions)
      (define-key map1 "t" 'ebrowse-tree-command:show-types)
      (define-key map1 "v" 'ebrowse-tree-command:show-member-variables)
      (define-key map1 "V" 'ebrowse-tree-command:show-static-member-variables))

    (let ((map1 (make-sparse-keymap)))
      (suppress-keymap map1 t)
      (define-key map "M" map1)
      (define-key map1 "a" 'ebrowse-mark-all-classes)
      (define-key map1 "t" 'ebrowse-toggle-mark-at-point))

    (let ((map1 (make-sparse-keymap)))
      (suppress-keymap map1 t)
      (define-key map "T" map1)
      (define-key map1 "f" 'ebrowse-toggle-file-name-display)
      (define-key map1 "s" 'ebrowse-show-file-name-at-point)
      (define-key map1 "w" 'ebrowse-set-tree-indentation)
      (define-key map "x" 'ebrowse-statistics))

    (define-key map "n" 'ebrowse-repeat-member-search)
    (define-key map "q" 'bury-buffer)
    (define-key map "*" 'ebrowse-expand-all)
    (define-key map "+" 'ebrowse-expand-branch)
    (define-key map "-" 'ebrowse-collapse-branch)
    (define-key map "/" 'ebrowse-read-class-name-and-go)
    (define-key map " " 'ebrowse-view-class-declaration)
    (define-key map "?" 'describe-mode)
    (define-key map "\C-i" 'ebrowse-pop/switch-to-member-buffer-for-same-tree)
    (define-key map "\C-k" 'ebrowse-remove-class-at-point)
    (define-key map "\C-l" 'ebrowse-redraw-tree)
    (define-key map "\C-m" 'ebrowse-find-class-declaration)))



;;; Tree-mode - mode for tree buffers

;;;###autoload
(define-derived-mode ebrowse-tree-mode special-mode "Ebrowse-Tree"
  "Major mode for Ebrowse class tree buffers.
Each line corresponds to a class in a class tree.
Letters do not insert themselves, they are commands.
File operations in the tree buffer work on class tree data structures.
E.g.\\[save-buffer] writes the tree to the file it was loaded from.

Tree mode key bindings:
\\{ebrowse-tree-mode-map}"
  (let* ((ident (propertized-buffer-identification "C++ Tree"))
	 (inhibit-read-only t)
         header tree)

    (buffer-disable-undo)

    (unless (zerop (buffer-size))
      (goto-char (point-min))
      (multiple-value-setq (header tree) (values-list (ebrowse-read)))
      (message "Sorting. Please be patient...")
      (setq tree (ebrowse-sort-tree-list tree))
      (erase-buffer)
      (message nil))

    (set (make-local-variable 'ebrowse--show-file-names-flag) nil)
    (set (make-local-variable 'ebrowse--tree-obarray) (make-vector 127 0))
    (set (make-local-variable 'ebrowse--frozen-flag) nil)
    (setq mode-line-buffer-identification ident)
    (setq buffer-read-only t)
    (setq selective-display t)
    (setq selective-display-ellipses t)
    (set (make-local-variable 'revert-buffer-function)
         #'ebrowse-revert-tree-buffer-from-file)
    (set (make-local-variable 'ebrowse--header) header)
    (set (make-local-variable 'ebrowse--tree) tree)
    (set (make-local-variable 'ebrowse--tags-file-name) buffer-file-name)
    (set (make-local-variable 'ebrowse--tree-obarray)
         (and tree (ebrowse-build-tree-obarray tree)))
    (set (make-local-variable 'ebrowse--frozen-flag) nil)

    (add-hook 'local-write-file-hooks 'ebrowse-write-file-hook-fn nil t)
    (modify-syntax-entry ?_ (char-to-string (char-syntax ?a)))
    (when tree
      (ebrowse-redraw-tree)
      (set-buffer-modified-p nil))))



(defun ebrowse-update-tree-buffer-mode-line ()
  "Update the tree buffer mode line."
  (ebrowse-rename-buffer (if ebrowse--frozen-flag
			     (ebrowse-frozen-tree-buffer-name
			      ebrowse--tags-file-name)
			   ebrowse-tree-buffer-name))
  (force-mode-line-update))



;;; Removing classes from trees

(defun ebrowse-remove-class-and-kill-member-buffers (tree class)
  "Remove from TREE class CLASS.
Kill all member buffers still containing a reference to the class."
  (let ((sym (intern-soft (ebrowse-cs-name (ebrowse-ts-class class))
			  ebrowse--tree-obarray)))
    (setf tree (delq class tree)
	  (get sym 'ebrowse-root) nil)
    (dolist (root tree)
      (setf (ebrowse-ts-subclasses root)
	    (delq class (ebrowse-ts-subclasses root))
	    (ebrowse-ts-base-classes root) nil)
      (ebrowse-remove-class-and-kill-member-buffers
       (ebrowse-ts-subclasses root) class))
    (ebrowse-kill-member-buffers-displaying class)
    tree))


(defun ebrowse-remove-class-at-point (forced)
  "Remove the class point is on from the class tree.
Do not ask for confirmation if FORCED is non-nil."
  (interactive "P")
  (let* ((class (ebrowse-tree-at-point))
	 (class-name (ebrowse-cs-name (ebrowse-ts-class class)))
	 (subclasses (ebrowse-ts-subclasses class)))
    (cond ((or forced
	       (y-or-n-p (concat "Delete class " class-name "? ")))
	   (setf ebrowse--tree (ebrowse-remove-class-and-kill-member-buffers
				ebrowse--tree class))
	   (set-buffer-modified-p t)
	   (message "%s %sdeleted." class-name
		    (if subclasses "and derived classes " ""))
	   (ebrowse-redraw-tree))
	  (t (message "Aborted")))))



;;; Marking classes in the tree buffer

(defun ebrowse-toggle-mark-at-point (&optional n-times)
  "Toggle mark for class cursor is on.
If given a numeric N-TIMES argument, mark that many classes."
  (interactive "p")
  (let (to-change)
    ;; Get the classes whose mark must be toggled. Note that
    ;; ebrowse-tree-at-point might issue an error.
    (ignore-errors
      (loop repeat (or n-times 1)
	    as tree = (ebrowse-tree-at-point)
	    do (progn
		 (setf (ebrowse-ts-mark tree) (not (ebrowse-ts-mark tree)))
		 (forward-line 1)
		 (push tree to-change))))
    (save-excursion
      ;; For all these classes, reverse the mark char in the display
      ;; by a regexp replace over the whole buffer. The reason for this
      ;; is that classes might have multiple base classes. If this is
      ;; the case, they are displayed more than once in the tree.
      (ebrowse-output
	(loop for tree in to-change
	      as regexp = (concat "^.*\\b"
				  (regexp-quote
				   (ebrowse-cs-name (ebrowse-ts-class tree)))
				  "\\b")
	      do
	      (goto-char (point-min))
	      (loop while (re-search-forward regexp nil t)
		    do (progn
			 (goto-char (match-beginning 0))
			 (delete-char 1)
			 (insert-char (if (ebrowse-ts-mark tree) ?> ? ) 1)
			 (ebrowse-set-mark-props (1- (point)) (point) tree)
			 (goto-char (match-end 0)))))))))


(defun ebrowse-mark-all-classes (prefix)
  "Unmark, with PREFIX mark, all classes in the tree."
  (interactive "P")
  (ebrowse-for-all-trees (tree ebrowse--tree-obarray)
    (setf (ebrowse-ts-mark tree) prefix))
  (ebrowse-redraw-marks (point-min) (point-max)))


(defun ebrowse-redraw-marks (start end)
  "Display class marker signs in the tree between START and END."
  (interactive)
  (save-excursion
    (ebrowse-output
      (catch 'end
	(goto-char (point-min))
	(dolist (root ebrowse--tree)
	  (ebrowse-draw-marks-fn root start end))))
    (ebrowse-update-tree-buffer-mode-line)))


(defun ebrowse-draw-marks-fn (tree start end)
  "Display class marker signs in TREE between START and END."
  (when (>= (point) start)
    (delete-char 1)
    (insert (if (ebrowse-ts-mark tree) ?> ? ))
    (ebrowse-set-mark-props (1- (point)) (point) tree))
  (forward-line 1)
  (when (> (point) end)
    (throw 'end nil))
  (dolist (sub (ebrowse-ts-subclasses tree))
    (ebrowse-draw-marks-fn sub start end)))



;;; File name display in tree buffers

(defun ebrowse-show-file-name-at-point (prefix)
  "Show filename in the line point is in.
With PREFIX, insert that many filenames."
  (interactive "p")
  (unless ebrowse--show-file-names-flag
    (ebrowse-output
      (dotimes (i prefix)
	(let ((tree (ebrowse-tree-at-point))
	      start
	      file-name-existing)
	  (beginning-of-line)
	  (skip-chars-forward " \t*a-zA-Z0-9_")
	  (setq start (point)
		file-name-existing (looking-at "("))
	  (delete-region start (line-end-position))
	  (unless file-name-existing
	    (indent-to ebrowse-source-file-column)
	    (insert "(" (or (ebrowse-cs-file
			     (ebrowse-ts-class tree))
			    "unknown")
		    ")"))
	  (ebrowse-set-face start (point) 'ebrowse-file-name)
	  (beginning-of-line)
	  (forward-line 1))))))


(defun ebrowse-toggle-file-name-display ()
  "Toggle display of filenames in tree buffer."
  (interactive)
  (setf ebrowse--show-file-names-flag (not ebrowse--show-file-names-flag))
  (let ((old-line (count-lines (point-min) (point))))
    (ebrowse-redraw-tree)
    (goto-char (point-min))
    (forward-line (1- old-line))))



;;; General member and tree buffer functions

(defun ebrowse-member-buffer-p (buffer)
  "Value is non-nil if BUFFER is a member buffer."
  ;; FIXME: Why not (buffer-local-value 'major-mode buffer)?
  (eq (cdr (assoc 'major-mode (buffer-local-variables buffer)))
      'ebrowse-member-mode))


(defun ebrowse-tree-buffer-p (buffer)
  "Value is non-nil if BUFFER is a class tree buffer."
  (eq (cdr (assoc 'major-mode (buffer-local-variables buffer)))
      'ebrowse-tree-mode))


(defun ebrowse-buffer-p (buffer)
  "Value is non-nil if BUFFER is a tree or member buffer."
  (memq (cdr (assoc 'major-mode (buffer-local-variables buffer)))
	'(ebrowse-tree-mode ebrowse-member-mode)))


(defun ebrowse-browser-buffer-list ()
  "Return a list of all tree or member buffers."
  (ebrowse-delete-if-not 'ebrowse-buffer-p (buffer-list)))


(defun ebrowse-member-buffer-list ()
  "Return a list of all member buffers."
  (ebrowse-delete-if-not 'ebrowse-member-buffer-p (buffer-list)))


(defun ebrowse-tree-buffer-list ()
  "Return a list of all tree buffers."
  (ebrowse-delete-if-not 'ebrowse-tree-buffer-p (buffer-list)))


(defun ebrowse-known-class-trees-buffer-list ()
  "Return a list of buffers containing class trees.
The list will contain, for each class tree loaded,
one buffer.  Prefer tree buffers over member buffers."
  (let ((buffers (nconc (ebrowse-tree-buffer-list)
			(ebrowse-member-buffer-list)))
	(set (make-hash-table))
	result)
    (dolist (buffer buffers)
      (let ((tree (ebrowse-value-in-buffer 'ebrowse--tree buffer)))
	(unless (gethash tree set)
	  (push buffer result))
	(puthash tree t set)))
    result))


(defun ebrowse-same-tree-member-buffer-list ()
  "Return a list of members buffers with same tree as current buffer."
  (ebrowse-delete-if-not
   (lambda (buffer)
     (eq (ebrowse-value-in-buffer 'ebrowse--tree buffer)
	 ebrowse--tree))
   (ebrowse-member-buffer-list)))



(defun ebrowse-pop/switch-to-member-buffer-for-same-tree (arg)
  "Pop to the buffer displaying members.
Switch to buffer if prefix ARG.
If no member buffer exists, make one."
  (interactive "P")
  (let ((buf (or (first (ebrowse-same-tree-member-buffer-list))
		 (get-buffer ebrowse-member-buffer-name)
		 (ebrowse-tree-command:show-member-functions))))
    (when buf
      (if arg
	  (switch-to-buffer buf)
	(pop-to-buffer buf)))
    buf))


(defun ebrowse-switch-to-next-member-buffer ()
  "Switch to next member buffer."
  (interactive)
  (let* ((list (ebrowse-member-buffer-list))
	 (next-list (cdr (memq (current-buffer) list)))
	 (next-buffer (if next-list (car next-list) (car list))))
    (if (eq next-buffer (current-buffer))
	(error "No next buffer")
      (bury-buffer)
      (switch-to-buffer next-buffer))))


(defun ebrowse-kill-member-buffers-displaying (tree)
  "Kill all member buffers displaying TREE."
  (loop for buffer in (ebrowse-member-buffer-list)
	as class = (ebrowse-value-in-buffer 'ebrowse--displayed-class buffer)
	when (eq class tree) do (kill-buffer buffer)))


(defun ebrowse-frozen-tree-buffer-name (tags-file)
  "Return the buffer name of a tree which is associated TAGS-FILE."
  (concat ebrowse-tree-buffer-name " (" tags-file ")"))


(defun ebrowse-pop-to-browser-buffer (arg)
  "Pop to a browser buffer from any other buffer.
Pop to member buffer if no prefix ARG, to tree buffer otherwise."
  (interactive "P")
  (let ((buffer (get-buffer (if arg
				ebrowse-tree-buffer-name
			      ebrowse-member-buffer-name))))
    (unless buffer
      (setq buffer
	    (get-buffer (if arg
			    ebrowse-member-buffer-name
			  ebrowse-tree-buffer-name))))
    (unless buffer
      (error "No browser buffer found"))
    (pop-to-buffer buffer)))



;;; Misc tree buffer commands

(defun ebrowse-set-tree-indentation ()
  "Set the indentation width of the tree display."
  (interactive)
  (let ((width (string-to-number (read-string
                                  (concat "Indentation (default "
                                          (int-to-string ebrowse--indentation)
                                          "): ")
                                  nil nil ebrowse--indentation))))
    (when (plusp width)
      (set (make-local-variable 'ebrowse--indentation) width)
      (ebrowse-redraw-tree))))


(defun ebrowse-read-class-name-and-go (&optional class)
  "Position cursor on CLASS.
Read a class name from the minibuffer if CLASS is nil."
  (interactive)
  (ebrowse-ignoring-completion-case
    ;; If no class specified, read the class name from mini-buffer
    (unless class
      (setf class
	    (completing-read "Goto class: "
			     (ebrowse-tree-obarray-as-alist) nil t)))
    (ebrowse-save-selective
      (goto-char (point-min))
      (widen)
      (setf selective-display nil)
      (setq ebrowse--last-regexp (concat "\\b" class "\\b"))
      (if (re-search-forward ebrowse--last-regexp nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (ebrowse-unhide-base-classes))
	(error "Not found")))))



;;; Showing various kinds of member buffers

(defun ebrowse-tree-command:show-member-variables (arg)
  "Display member variables; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-member-variables arg))


(defun ebrowse-tree-command:show-member-functions (&optional arg)
  "Display member functions; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-member-functions arg))


(defun ebrowse-tree-command:show-static-member-variables (arg)
  "Display static member variables; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-static-variables arg))


(defun ebrowse-tree-command:show-static-member-functions (arg)
  "Display static member functions; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-static-functions arg))


(defun ebrowse-tree-command:show-friends (arg)
  "Display friend functions; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-friends arg))


(defun ebrowse-tree-command:show-types (arg)
  "Display types defined in a class; with prefix ARG in frozen member buffer."
  (interactive "P")
  (ebrowse-display-member-buffer 'ebrowse-ts-types arg))



;;; Viewing or finding a class declaration

(defun ebrowse-tree-at-point ()
  "Return the class structure for the class point is on."
  (or (get-text-property (point) 'ebrowse-tree)
      (error "Not on a class")))


(defun* ebrowse-view/find-class-declaration (&key view where)
  "View or find the declarator of the class point is on.
VIEW non-nil means view it.  WHERE is additional position info."
  (let* ((class (ebrowse-ts-class (ebrowse-tree-at-point)))
	 (file (ebrowse-cs-file class))
	 (browse-struct (make-ebrowse-bs
			 :name (ebrowse-cs-name class)
			 :pattern (ebrowse-cs-pattern class)
			 :flags (ebrowse-cs-flags class)
			 :file (ebrowse-cs-file class)
			 :point (ebrowse-cs-point class))))
    (ebrowse-view/find-file-and-search-pattern
     browse-struct
     (list ebrowse--header class nil)
     file
     ebrowse--tags-file-name
     view
     where)))


(defun ebrowse-find-class-declaration (prefix)
  "Find a class declaration and position cursor on it.
PREFIX 4 means find it in another window.
PREFIX 5 means find it in another frame."
  (interactive "p")
  (ebrowse-view/find-class-declaration
   :view nil
   :where (cond ((= prefix 4) 'other-window)
		((= prefix 5) 'other-frame)
		(t            'this-window))))


(defun ebrowse-view-class-declaration (prefix)
  "View class declaration and position cursor on it.
PREFIX 4 means view it in another window.
PREFIX 5 means view it in another frame."
  (interactive "p")
  (ebrowse-view/find-class-declaration
   :view 'view
   :where (cond ((= prefix 4) 'other-window)
		((= prefix 5) 'other-frame)
		(t            'this-window))))



;;; The FIND engine

(defun ebrowse-find-source-file (file tags-file)
  "Find source file FILE.
Source files are searched for (a) relative to TAGS-FILE
which is the path of the BROWSE file from which the class tree was loaded,
and (b) in the directories named in `ebrowse-search-path'."
  (let (file-name
	(try-file (expand-file-name file
				    (file-name-directory tags-file))))
    (if (file-readable-p try-file)
	(setq file-name try-file)
      (let ((search-in ebrowse-search-path))
	(while (and search-in
		    (null file-name))
	  (let ((try-file (expand-file-name file (car search-in))))
	    (if (file-readable-p try-file)
		(setq file-name try-file))
	    (setq search-in (cdr search-in))))))
    (unless file-name
      (error "File `%s' not found" file))
    file-name))


(defun ebrowse-view-exit-fn (buffer)
  "Function called when exiting View mode in BUFFER.
Restore frame configuration active before viewing the file,
and possibly kill the viewed buffer."
  (let (exit-action original-frame-configuration)
    (with-current-buffer buffer
      (setq original-frame-configuration ebrowse--frame-configuration
	    exit-action ebrowse--view-exit-action))
    ;; Delete the frame in which we viewed.
    (mapc 'delete-frame
	  (loop for frame in (frame-list)
	     when (not (assq frame original-frame-configuration))
	     collect frame))
    (when exit-action
      (funcall exit-action buffer))))


(defun ebrowse-view-file-other-frame (file)
  "View a file FILE in another frame.
The new frame is deleted when you quit viewing the file in that frame."
  (interactive)
  (let ((old-frame-configuration (current-frame-configuration))
	(had-a-buf (get-file-buffer file))
	(buf-to-view (find-file-noselect file)))
    (switch-to-buffer-other-frame buf-to-view)
    (set (make-local-variable 'ebrowse--frame-configuration)
         old-frame-configuration)
    (set (make-local-variable 'ebrowse--view-exit-action)
         (and (not had-a-buf)
              (not (buffer-modified-p buf-to-view))
              'kill-buffer))
    (view-mode-enter (cons (selected-window) (cons (selected-window) t))
		     'ebrowse-view-exit-fn)))

(defun ebrowse-view/find-file-and-search-pattern
  (struc info file tags-file &optional view where)
  "Find or view a member or class.
STRUC is an `ebrowse-bs' structure (or a structure including that)
describing what to search.
INFO is a list (HEADER MEMBER-OR-CLASS ACCESSOR).  HEADER is the
header structure of a class tree.  MEMBER-OR-CLASS is either an
`ebrowse-ms' or `ebrowse-cs' structure depending on what is searched.
ACCESSOR is an accessor function for the member list of a member
if MEMBER-OR-CLASS is an `ebrowse-ms'.
FILE is the file to search the member in.
FILE is not taken out of STRUC here because the filename in STRUC
may be nil in which case the filename of the class description is used.
TAGS-FILE is the name of the BROWSE file from which the
tree was loaded.
If VIEW is non-nil, view file else find the file.
WHERE is either `other-window', `other-frame' or `this-window' and
specifies where to find/view the result."
  (unless file
    (error "Sorry, no file information available for %s"
	   (ebrowse-bs-name struc)))
  ;; Get the source file to view or find.
  (setf file (ebrowse-find-source-file file tags-file))
  ;; If current window is dedicated, use another frame.
  (when (window-dedicated-p (selected-window))
    (setf where 'other-window))
  (cond (view
	 (setf ebrowse-temp-position-to-view struc
	       ebrowse-temp-info-to-view info)
	 (unless (boundp 'view-mode-hook)
	   (setq view-mode-hook nil))
	 (push 'ebrowse-find-pattern view-mode-hook)
	 (case where
	   (other-window (view-file-other-window file))
	   (other-frame  (ebrowse-view-file-other-frame file))
	   (t            (view-file file))))
	(t
	 (case where
	   (other-window (find-file-other-window file))
	   (other-frame  (find-file-other-frame file))
	   (t            (find-file file)))
	 (ebrowse-find-pattern struc info))))


(defun ebrowse-symbol-regexp (name)
  "Generate a suitable regular expression for a member or class NAME.
This is `regexp-quote' for most symbols, except for operator names
which may contain whitespace.  For these symbols, replace white
space in the symbol name (generated by BROWSE) with a regular
expression matching any number of whitespace characters."
  (loop with regexp = (regexp-quote name)
	with start = 0
	finally return regexp
	while (string-match "[ \t]+" regexp start)
	do (setq regexp (concat (substring regexp 0 (match-beginning 0))
				"[ \t]*"
				(substring regexp (match-end 0)))
		 start (+ (match-beginning 0) 5))))


(defun ebrowse-class-declaration-regexp (name)
  "Construct a regexp for a declaration of class NAME."
  (concat "^[ \t]*\\(template[ \t\n]*<.*>\\)?"
	  "[ \t\n]*\\(class\\|struct\\|union\\).*\\S_"
	  (ebrowse-symbol-regexp name)
	  "\\S_"))


(defun ebrowse-variable-declaration-regexp (name)
  "Construct a regexp for matching a variable NAME."
  (concat "\\S_" (ebrowse-symbol-regexp name) "\\S_"))


(defun ebrowse-function-declaration/definition-regexp (name)
  "Construct a regexp for matching a function NAME."
  (concat "^[a-zA-Z0-9_:*&<>, \t]*\\S_"
	  (ebrowse-symbol-regexp name)
	  "[ \t\n]*("))


(defun ebrowse-pp-define-regexp (name)
  "Construct a regexp matching a define of NAME."
  (concat "^[ \t]*#[ \t]*define[ \t]+" (regexp-quote name)))


(defun* ebrowse-find-pattern (&optional position info &aux viewing)
  "Find a pattern.

This is a kluge: Ebrowse allows you to find or view a file containing
a pattern.  To be able to do a search in a viewed buffer,
`view-mode-hook' is temporarily set to this function;
`ebrowse-temp-position-to-view' holds what to search for.

INFO is a list (TREE-HEADER TREE-OR-MEMBER MEMBER-LIST)."
  (unless position
    (pop view-mode-hook)
    (setf viewing t
	  position ebrowse-temp-position-to-view
	  info ebrowse-temp-info-to-view))
  (widen)
  (let* ((pattern (ebrowse-bs-pattern position))
	 (start (ebrowse-bs-point position))
	 (offset 100)
	 found)
    (destructuring-bind (header class-or-member member-list) info
      ;; If no pattern is specified, construct one from the member name.
      (when (stringp pattern)
	(setq pattern (concat "^.*" (regexp-quote pattern))))
      ;; Construct a regular expression if none given.
      (unless pattern
	(typecase class-or-member
	  (ebrowse-ms
	   (case member-list
	     ((ebrowse-ts-member-variables
	       ebrowse-ts-static-variables
	       ebrowse-ts-types)
	      (setf pattern (ebrowse-variable-declaration-regexp
			     (ebrowse-bs-name position))))
	     (otherwise
	      (if (ebrowse-define-p class-or-member)
		  (setf pattern (ebrowse-pp-define-regexp (ebrowse-bs-name position)))
		(setf pattern (ebrowse-function-declaration/definition-regexp
			       (ebrowse-bs-name position)))))))
	  (ebrowse-cs
	   (setf pattern (ebrowse-class-declaration-regexp
			  (ebrowse-bs-name position))))))
      ;; Begin searching some OFFSET from the original point where the
      ;; regular expression was found by the parse, and step forward.
      ;; When there is no regular expression in the database and a
      ;; member definition/declaration was not seen by the parser,
      ;; START will be 0.
      (when (and (boundp 'ebrowse-debug)
		 (symbol-value 'ebrowse-debug))
	(y-or-n-p (format "start = %d? " start))
	(y-or-n-p pattern))
      (setf found
	    (loop do (goto-char (max (point-min) (- start offset)))
		  when (re-search-forward pattern (+ start offset) t) return t
		  never (bobp)
		  do (incf offset offset)))
      (cond (found
	     (beginning-of-line)
	     (run-hooks 'ebrowse-view/find-hook))
	    ((numberp (ebrowse-bs-pattern position))
	     (goto-char start)
	     (if ebrowse-not-found-hook
		 (run-hooks 'ebrowse-not-found-hook)
	       (message "Not found")
	       (sit-for 2)))
	    (t
	     (if ebrowse-not-found-hook
		 (run-hooks 'ebrowse-not-found-hook)
	       (unless viewing
		 (error "Not found"))
	       (message "Not found")
	       (sit-for 2)))))))


;;; Drawing the tree

(defun ebrowse-redraw-tree (&optional quietly)
  "Redisplay the complete tree.
QUIETLY non-nil means don't display progress messages."
  (interactive)
  (or quietly (message "Displaying..."))
  (save-excursion
    (ebrowse-output
      (erase-buffer)
      (ebrowse-draw-tree-fn)))
  (ebrowse-update-tree-buffer-mode-line)
  (or quietly (message nil)))


(defun ebrowse-set-mark-props (start end tree)
  "Set text properties for class marker signs between START and END.
TREE denotes the class shown."
  (add-text-properties
   start end
   `(mouse-face highlight ebrowse-what mark ebrowse-tree ,tree
		help-echo "double-mouse-1: mark/unmark"))
  (ebrowse-set-face start end 'ebrowse-tree-mark))


(defun* ebrowse-draw-tree-fn (&aux stack1 stack2 start)
  "Display a single class and recursively its subclasses.
This function may look weird, but this is faster than recursion."
  (setq stack1 (make-list (length ebrowse--tree) 0)
	stack2 (copy-sequence ebrowse--tree))
  (loop while stack2
	as level = (pop stack1)
	as tree = (pop stack2)
	as class = (ebrowse-ts-class tree) do
	(let ((start-of-line (point))
	      start-of-class-name end-of-class-name)
	  ;; Insert mark
	  (insert (if (ebrowse-ts-mark tree) ">" " "))

	  ;; Indent and insert class name
	  (indent-to (+ (* level ebrowse--indentation)
			ebrowse-tree-left-margin))
	  (setq start (point))
	  (insert (ebrowse-qualified-class-name class))

	  ;; If template class, add <>
	  (when (ebrowse-template-p class)
	    (insert "<>"))
	  (ebrowse-set-face start (point) (if (zerop level)
					      'ebrowse-root-class
					    'ebrowse-default))
	  (setf start-of-class-name start
		end-of-class-name (point))
	  ;; If filenames are to be displayed...
	  (when ebrowse--show-file-names-flag
	    (indent-to ebrowse-source-file-column)
	    (setq start (point))
	    (insert "("
		    (or (ebrowse-cs-file class)
			"unknown")
		    ")")
	    (ebrowse-set-face start (point) 'ebrowse-file-name))
	  (ebrowse-set-mark-props start-of-line (1+ start-of-line) tree)
	  (add-text-properties
	   start-of-class-name end-of-class-name
	   `(mouse-face highlight ebrowse-what class-name
			ebrowse-tree ,tree
			help-echo "double-mouse-1: (un)expand tree; mouse-2: member functions, mouse-3: menu"))
	  (insert "\n"))
	;; Push subclasses, if any.
	(when (ebrowse-ts-subclasses tree)
	  (setq stack2
		(nconc (copy-sequence (ebrowse-ts-subclasses tree)) stack2)
		stack1
		(nconc (make-list (length (ebrowse-ts-subclasses tree))
				  (1+ level)) stack1)))))



;;; Expanding/ collapsing tree branches

(defun ebrowse-expand-branch (arg)
  "Expand a sub-tree that has been previously collapsed.
With prefix ARG, expand all sub-trees."
  (interactive "P")
  (if arg
      (ebrowse-expand-all arg)
    (ebrowse-collapse-fn nil)))


(defun ebrowse-collapse-branch (arg)
  "Fold (do no longer display) the subclasses of the current class.
\(The class cursor is on.)  With prefix ARG, fold all trees in the buffer."
  (interactive "P")
  (if arg
      (ebrowse-expand-all (not arg))
    (ebrowse-collapse-fn t)))


(defun ebrowse-expand-all (collapse)
  "Expand or fold all trees in the buffer.
COLLAPSE non-nil means fold them."
  (interactive "P")
  (let ((line-end  (if collapse "^\n" "^\r"))
	(insertion (if collapse "\r"  "\n")))
    (ebrowse-output
      (save-excursion
	(goto-char (point-min))
	(while (not (progn (skip-chars-forward line-end) (eobp)))
	  (when (or (not collapse)
		    (looking-at "\n "))
	    (delete-char 1)
	    (insert insertion))
	  (when collapse
	    (skip-chars-forward "\n ")))))))


(defun ebrowse-unhide-base-classes ()
  "Unhide the line the cursor is on and all base classes."
  (ebrowse-output
    (save-excursion
      (let (indent last-indent)
	(skip-chars-backward "^\r\n")
	(when (not (looking-at "[\r\n][^ \t]"))
	  (skip-chars-forward "\r\n \t")
	  (while (and (or (null last-indent) ;first time
			  (> indent 1))	;not root class
		      (re-search-backward "[\r\n][ \t]*" nil t))
	    (setf indent (- (match-end 0)
			    (match-beginning 0)))
	    (when (or (null last-indent)
		      (< indent last-indent))
	      (setf last-indent indent)
	      (when (looking-at "\r")
		(delete-char 1)
		(insert 10)))
	    (backward-char 1)))))))


(defun ebrowse-hide-line (collapse)
  "Hide/show a single line in the tree.
COLLAPSE non-nil means hide."
  (save-excursion
    (ebrowse-output
      (skip-chars-forward "^\r\n")
      (delete-char 1)
      (insert (if collapse 13 10)))))


(defun ebrowse-collapse-fn (collapse)
  "Collapse or expand a branch of the tree.
COLLAPSE non-nil means collapse the branch."
  (ebrowse-output
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "> \t")
      (let ((indentation (current-column)))
	(while (and (not (eobp))
		    (save-excursion
		      (skip-chars-forward "^\r\n")
		      (goto-char (1+ (point)))
		      (skip-chars-forward "> \t")
		      (> (current-column) indentation)))
	  (ebrowse-hide-line collapse)
	  (skip-chars-forward "^\r\n")
	  (goto-char (1+ (point))))))))


;;; Electric tree selection

(defvar ebrowse-electric-list-mode-map ()
  "Keymap used in electric Ebrowse buffer list window.")


(unless ebrowse-electric-list-mode-map
  (let ((map (make-keymap))
	(submap (make-keymap)))
    (setq ebrowse-electric-list-mode-map map)
    (fillarray (car (cdr map)) 'ebrowse-electric-list-undefined)
    (fillarray (car (cdr submap)) 'ebrowse-electric-list-undefined)
    (define-key map "\e" submap)
    (define-key map "\C-z" 'suspend-frame)
    (define-key map "\C-h" 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'ebrowse-electric-list-quit)
    (define-key map "q" 'ebrowse-electric-list-quit)
    (define-key map " " 'ebrowse-electric-list-select)
    (define-key map "\C-l" 'recenter)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "v" 'ebrowse-electric-view-buffer)
    (define-key map "\C-v" 'scroll-up-command)
    (define-key map "\ev" 'scroll-down-command)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (define-key map "\e>" 'end-of-buffer)))

(put 'ebrowse-electric-list-mode 'mode-class 'special)
(put 'ebrowse-electric-list-undefined 'suppress-keymap t)


(define-derived-mode ebrowse-electric-list-mode
  fundamental-mode "Electric Position Menu"
  "Mode for electric tree list mode."
  (setq mode-line-buffer-identification "Electric Tree Menu")
  (when (memq 'mode-name mode-line-format)
    (setq mode-line-format (copy-sequence mode-line-format))
    (setcar (memq 'mode-name mode-line-format) "Tree Buffers"))
  (set (make-local-variable 'Helper-return-blurb) "return to buffer editing")
  (setq truncate-lines t
	buffer-read-only t))


(defun ebrowse-list-tree-buffers ()
  "Display a list of all tree buffers."
  (set-buffer (get-buffer-create "*Tree Buffers*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Tree\n" "----\n")
  (dolist (buffer (ebrowse-known-class-trees-buffer-list))
    (insert (buffer-name buffer) "\n"))
  (setq buffer-read-only t))


;;;###autoload
(defun ebrowse-electric-choose-tree ()
  "Return a buffer containing a tree or nil if no tree found or canceled."
  (interactive)
  (unless (car (ebrowse-known-class-trees-buffer-list))
    (error "No tree buffers"))
  (let (select buffer window)
    (save-window-excursion
      (save-window-excursion (ebrowse-list-tree-buffers))
      (setq window (Electric-pop-up-window "*Tree Buffers*")
	    buffer (window-buffer window))
      (shrink-window-if-larger-than-buffer window)
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (ebrowse-electric-list-mode)
	    (setq select
		  (catch 'ebrowse-electric-list-select
		    (message "<<< Press Space to bury the list >>>")
		    (let ((first (progn (goto-char (point-min))
					(forward-line 2)
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      (goto-char first)
		      (Electric-command-loop 'ebrowse-electric-list-select
					     nil
					     t
					     'ebrowse-electric-list-looper
					     (cons first last))))))
	(set-buffer buffer)
	(bury-buffer buffer)
	(message nil)))
    (when select
      (set-buffer buffer)
      (setq select (ebrowse-electric-get-buffer select)))
    (kill-buffer buffer)
    select))


(defun ebrowse-electric-list-looper (state condition)
  "Prevent cursor from moving beyond the buffer end.
Don't let it move into the title lines.
See 'Electric-command-loop' for a description of STATE and CONDITION."
  (cond ((and condition
	      (not (memq (car condition)
			 '(buffer-read-only end-of-buffer
					    beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (forward-line 2))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1)))))


(defun ebrowse-electric-list-undefined ()
  "Function called for keys that are undefined."
  (interactive)
  (message "Type C-h for help, ? for commands, q to quit, Space to select.")
  (sit-for 4))


(defun ebrowse-electric-list-quit ()
  "Discard the buffer list."
  (interactive)
  (throw 'ebrowse-electric-list-select nil))


(defun ebrowse-electric-list-select ()
  "Select a buffer from the buffer list."
  (interactive)
  (throw 'ebrowse-electric-list-select (point)))


(defun ebrowse-electric-get-buffer (point)
  "Get a buffer corresponding to the line POINT is in."
  (let ((index (- (count-lines (point-min) point) 2)))
    (nth index (ebrowse-known-class-trees-buffer-list))))


;;; View a buffer for a tree.

(defun ebrowse-electric-view-buffer ()
  "View buffer point is on."
  (interactive)
  (let ((buffer (ebrowse-electric-get-buffer (point))))
    (cond (buffer
	   (view-buffer buffer))
	  (t
	   (error "Buffer no longer exists")))))


(defun ebrowse-choose-from-browser-buffers ()
  "Read a browser buffer name from the minibuffer and return that buffer."
  (let* ((buffers (ebrowse-known-class-trees-buffer-list)))
    (if buffers
	(if (not (second buffers))
	    (first buffers)
	  (or (ebrowse-electric-choose-tree) (error "No tree buffer")))
      (let* ((insert-default-directory t)
	     (file (read-file-name "Find tree: " nil nil t)))
	(save-excursion
	  (find-file file))
	(find-buffer-visiting file)))))


;;; Member buffers

(unless ebrowse-member-mode-map
  (let ((map (make-keymap)))
    (setf ebrowse-member-mode-map map)
    (suppress-keymap map)

  (when (display-mouse-p)
    (define-key map [down-mouse-3] 'ebrowse-member-mouse-3)
    (define-key map [mouse-2] 'ebrowse-member-mouse-2))

  (let ((map1 (make-sparse-keymap)))
    (suppress-keymap map1 t)
    (define-key map "C" map1)
    (define-key map1 "b" 'ebrowse-switch-member-buffer-to-base-class)
    (define-key map1 "c" 'ebrowse-switch-member-buffer-to-any-class)
    (define-key map1 "d" 'ebrowse-switch-member-buffer-to-derived-class)
    (define-key map1 "n" 'ebrowse-switch-member-buffer-to-next-sibling-class)
    (define-key map1 "p" 'ebrowse-switch-member-buffer-to-previous-sibling-class))

  (let ((map1 (make-sparse-keymap)))
    (suppress-keymap map1 t)
    (define-key map "D" map1)
    (define-key map1 "a" 'ebrowse-toggle-member-attributes-display)
    (define-key map1 "b" 'ebrowse-toggle-base-class-display)
    (define-key map1 "f" 'ebrowse-freeze-member-buffer)
    (define-key map1 "l" 'ebrowse-toggle-long-short-display)
    (define-key map1 "r" 'ebrowse-toggle-regexp-display)
    (define-key map1 "w" 'ebrowse-set-member-buffer-column-width))

  (let ((map1 (make-sparse-keymap)))
    (suppress-keymap map1 t)
    (define-key map "F" map1)
    (let ((map2 (make-sparse-keymap)))
      (suppress-keymap map2 t)
      (define-key map1 "a" map2)
      (define-key map2 "i" 'ebrowse-toggle-private-member-filter)
      (define-key map2 "o" 'ebrowse-toggle-protected-member-filter)
      (define-key map2 "u" 'ebrowse-toggle-public-member-filter))
    (define-key map1 "c" 'ebrowse-toggle-const-member-filter)
    (define-key map1 "i" 'ebrowse-toggle-inline-member-filter)
    (define-key map1 "p" 'ebrowse-toggle-pure-member-filter)
    (define-key map1 "r" 'ebrowse-remove-all-member-filters)
    (define-key map1 "v" 'ebrowse-toggle-virtual-member-filter))

  (let ((map1 (make-sparse-keymap)))
    (suppress-keymap map1 t)
    (define-key map "L" map1)
    (define-key map1 "d" 'ebrowse-display-friends-member-list)
    (define-key map1 "f" 'ebrowse-display-function-member-list)
    (define-key map1 "F" 'ebrowse-display-static-functions-member-list)
    (define-key map1 "n" 'ebrowse-display-next-member-list)
    (define-key map1 "p" 'ebrowse-display-previous-member-list)
    (define-key map1 "t" 'ebrowse-display-types-member-list)
    (define-key map1 "v" 'ebrowse-display-variables-member-list)
    (define-key map1 "V" 'ebrowse-display-static-variables-member-list))

  (let ((map1 (make-sparse-keymap)))
    (suppress-keymap map1 t)
    (define-key map "G" map1)
    (define-key map1 "m" 'ebrowse-goto-visible-member/all-member-lists)
    (define-key map1 "n" 'ebrowse-repeat-member-search)
    (define-key map1 "v" 'ebrowse-goto-visible-member))

  (define-key map "f" 'ebrowse-find-member-declaration)
  (define-key map "m" 'ebrowse-switch-to-next-member-buffer)
  (define-key map "q" 'bury-buffer)
  (define-key map "t" 'ebrowse-show-displayed-class-in-tree)
  (define-key map "v" 'ebrowse-view-member-declaration)
  (define-key map " " 'ebrowse-view-member-definition)
  (define-key map "?" 'describe-mode)
  (define-key map "\C-i" 'ebrowse-pop-from-member-to-tree-buffer)
  (define-key map "\C-l" 'ebrowse-redisplay-member-buffer)
  (define-key map "\C-m" 'ebrowse-find-member-definition)))



;;; Member mode

;;;###autoload
(define-derived-mode ebrowse-member-mode special-mode "Ebrowse-Members"
  "Major mode for Ebrowse member buffers."
  (mapc 'make-local-variable
	'(ebrowse--decl-column	        ;display column
	  ebrowse--n-columns		;number of short columns
	  ebrowse--column-width	        ;width of columns above
	  ebrowse--show-inherited-flag  ;include inherited members?
	  ebrowse--filters		;public, protected, private
	  ebrowse--accessor		;vars, functions, friends
	  ebrowse--displayed-class	;class displayed
	  ebrowse--long-display-flag	;display with regexps?
	  ebrowse--source-regexp-flag	;show source regexp?
	  ebrowse--attributes-flag	;show `virtual' and `inline'
	  ebrowse--member-list          ;list of members displayed
	  ebrowse--tree		        ;the class tree
	  ebrowse--member-mode-strings  ;part of mode line
	  ebrowse--tags-file-name	;
	  ebrowse--header
	  ebrowse--tree-obarray
	  ebrowse--virtual-display-flag
	  ebrowse--inline-display-flag
	  ebrowse--const-display-flag
	  ebrowse--pure-display-flag
	  ebrowse--frozen-flag))	;buffer not automagically reused
  (setq mode-line-buffer-identification
	(propertized-buffer-identification "C++ Members")
	buffer-read-only t
	ebrowse--long-display-flag nil
	ebrowse--attributes-flag t
	ebrowse--show-inherited-flag t
	ebrowse--source-regexp-flag nil
	ebrowse--filters [0 1 2]
	ebrowse--decl-column ebrowse-default-declaration-column
	ebrowse--column-width ebrowse-default-column-width
	ebrowse--virtual-display-flag nil
	ebrowse--inline-display-flag nil
	ebrowse--const-display-flag nil
	ebrowse--pure-display-flag nil)
  (modify-syntax-entry ?_ (char-to-string (char-syntax ?a))))



;;; Member mode mode line

(defsubst ebrowse-class-name-displayed-in-member-buffer ()
  "Return the name of the class displayed in the member buffer."
  (ebrowse-cs-name (ebrowse-ts-class ebrowse--displayed-class)))


(defsubst ebrowse-member-list-name ()
  "Return a string describing what is displayed in the member buffer."
  (get ebrowse--accessor (if (ebrowse-globals-tree-p ebrowse--displayed-class)
			     'ebrowse-global-title
			   'ebrowse-title)))


(defun ebrowse-update-member-buffer-mode-line ()
  "Update the mode line of member buffers."
  (let* ((name (when ebrowse--frozen-flag
		 (concat (ebrowse-class-name-displayed-in-member-buffer)
			 " ")))
	 (ident (concat name (ebrowse-member-list-name))))
    (setq mode-line-buffer-identification
	  (propertized-buffer-identification ident))
    (ebrowse-rename-buffer (if name ident ebrowse-member-buffer-name))
    (force-mode-line-update)))


;;; Misc member buffer commands

(defun ebrowse-freeze-member-buffer ()
  "Toggle frozen status of current buffer."
  (interactive)
  (setq ebrowse--frozen-flag (not ebrowse--frozen-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-show-displayed-class-in-tree (arg)
  "Show the currently displayed class in the tree window.
With prefix ARG, switch to the tree buffer else pop to it."
  (interactive "P")
  (let ((class-name (ebrowse-class-name-displayed-in-member-buffer)))
    (when (ebrowse-pop-from-member-to-tree-buffer arg)
      (ebrowse-read-class-name-and-go class-name))))


(defun ebrowse-set-member-buffer-column-width ()
  "Set the column width of the member display.
The new width is read from the minibuffer."
  (interactive)
  (let ((width (string-to-number
		(read-from-minibuffer
		 (concat "Column width ("
			 (int-to-string (if ebrowse--long-display-flag
					    ebrowse--decl-column
					  ebrowse--column-width))
			 "): ")))))
    (when (plusp width)
      (if ebrowse--long-display-flag
	  (setq ebrowse--decl-column width)
	(setq ebrowse--column-width width))
      (ebrowse-redisplay-member-buffer))))


(defun ebrowse-pop-from-member-to-tree-buffer (arg)
  "Pop from a member buffer to the matching tree buffer.
Switch to the buffer if prefix ARG.  If no tree buffer exists,
make one."
  (interactive "P")
  (let ((buf (or (get-buffer (ebrowse-frozen-tree-buffer-name
			      ebrowse--tags-file-name))
		 (get-buffer ebrowse-tree-buffer-name)
		 (ebrowse-create-tree-buffer ebrowse--tree
					     ebrowse--tags-file-name
					     ebrowse--header
					     ebrowse--tree-obarray
					     'pop))))
    (and buf
	 (funcall (if arg 'switch-to-buffer 'pop-to-buffer) buf))
    buf))



;;; Switching between member lists

(defun ebrowse-display-member-list-for-accessor (accessor)
  "Switch the member buffer to display the member list for ACCESSOR."
  (setf ebrowse--accessor accessor
	ebrowse--member-list (funcall accessor ebrowse--displayed-class))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-cyclic-display-next/previous-member-list (incr)
  "Switch buffer to INCR'th next/previous list of members."
  (let ((index (ebrowse-position ebrowse--accessor
				 ebrowse-member-list-accessors)))
    (setf ebrowse--accessor
	  (cond ((plusp incr)
		 (or (nth (1+ index)
			  ebrowse-member-list-accessors)
		     (first ebrowse-member-list-accessors)))
		((minusp incr)
		 (or (and (>= (decf index) 0)
			  (nth index
			       ebrowse-member-list-accessors))
		     (first (last ebrowse-member-list-accessors))))))
    (ebrowse-display-member-list-for-accessor ebrowse--accessor)))


(defun ebrowse-display-next-member-list ()
  "Switch buffer to next member list."
  (interactive)
  (ebrowse-cyclic-display-next/previous-member-list 1))


(defun ebrowse-display-previous-member-list ()
  "Switch buffer to previous member list."
  (interactive)
  (ebrowse-cyclic-display-next/previous-member-list -1))


(defun ebrowse-display-function-member-list ()
  "Display the list of member functions."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-member-functions))


(defun ebrowse-display-variables-member-list ()
  "Display the list of member variables."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-member-variables))


(defun ebrowse-display-static-variables-member-list ()
  "Display the list of static member variables."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-static-variables))


(defun ebrowse-display-static-functions-member-list ()
  "Display the list of static member functions."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-static-functions))


(defun ebrowse-display-friends-member-list ()
  "Display the list of friends."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-friends))


(defun ebrowse-display-types-member-list ()
  "Display the list of types."
  (interactive)
  (ebrowse-display-member-list-for-accessor 'ebrowse-ts-types))



;;; Filters and other display attributes

(defun ebrowse-toggle-member-attributes-display ()
  "Toggle display of `virtual', `inline', `const' etc."
  (interactive)
  (setq ebrowse--attributes-flag (not ebrowse--attributes-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-base-class-display ()
  "Toggle the display of members inherited from base classes."
  (interactive)
  (setf ebrowse--show-inherited-flag (not ebrowse--show-inherited-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-pure-member-filter ()
  "Toggle display of pure virtual members."
  (interactive)
  (setf ebrowse--pure-display-flag (not ebrowse--pure-display-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-const-member-filter ()
  "Toggle display of const members."
  (interactive)
  (setf ebrowse--const-display-flag (not ebrowse--const-display-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-inline-member-filter ()
  "Toggle display of inline members."
  (interactive)
  (setf ebrowse--inline-display-flag (not ebrowse--inline-display-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-virtual-member-filter ()
  "Toggle display of virtual members."
  (interactive)
  (setf ebrowse--virtual-display-flag (not ebrowse--virtual-display-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-remove-all-member-filters ()
  "Remove all filters."
  (interactive)
  (dotimes (i 3)
    (aset ebrowse--filters i i))
  (setq ebrowse--pure-display-flag nil
	ebrowse--const-display-flag nil
	ebrowse--virtual-display-flag nil
	ebrowse--inline-display-flag nil)
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-public-member-filter ()
  "Toggle visibility of public members."
  (interactive)
  (ebrowse-set-member-access-visibility 0)
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-protected-member-filter ()
  "Toggle visibility of protected members."
  (interactive)
  (ebrowse-set-member-access-visibility 1)
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-private-member-filter ()
  "Toggle visibility of private members."
  (interactive)
  (ebrowse-set-member-access-visibility 2)
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-set-member-access-visibility (vis)
  (setf (aref ebrowse--filters vis)
	(if (aref ebrowse--filters vis) nil vis)))


(defun ebrowse-toggle-long-short-display ()
  "Toggle between long and short display form of member buffers."
  (interactive)
  (setf ebrowse--long-display-flag (not ebrowse--long-display-flag))
  (ebrowse-redisplay-member-buffer))


(defun ebrowse-toggle-regexp-display ()
  "Toggle declaration/definition regular expression display.
Used in member buffers showing the long display form."
  (interactive)
  (setf ebrowse--source-regexp-flag (not ebrowse--source-regexp-flag))
  (ebrowse-redisplay-member-buffer))



;;; Viewing/finding members

(defun ebrowse-find-member-definition (&optional prefix)
  "Find the file containing a member definition.
With PREFIX 4. find file in another window, with prefix 5
find file in another frame."
  (interactive "p")
  (ebrowse-view/find-member-declaration/definition prefix nil t))


(defun ebrowse-view-member-definition (prefix)
  "View the file containing a member definition.
With PREFIX 4. find file in another window, with prefix 5
find file in another frame."
  (interactive "p")
  (ebrowse-view/find-member-declaration/definition prefix t t))


(defun ebrowse-find-member-declaration (prefix)
  "Find the file containing a member's declaration.
With PREFIX 4. find file in another window, with prefix 5
find file in another frame."
  (interactive "p")
  (ebrowse-view/find-member-declaration/definition prefix nil))


(defun ebrowse-view-member-declaration (prefix)
  "View the file containing a member's declaration.
With PREFIX 4. find file in another window, with prefix 5
find file in another frame."
  (interactive "p")
  (ebrowse-view/find-member-declaration/definition prefix t))


(defun* ebrowse-view/find-member-declaration/definition
    (prefix view &optional definition info header tags-file)
  "Find or view a member declaration or definition.
With PREFIX 4. find file in another window, with prefix 5
find file in another frame.
DEFINITION non-nil means find the definition, otherwise find the
declaration.
INFO is a list (TREE ACCESSOR MEMBER) describing the member to
search.
TAGS-FILE is the file name of the BROWSE file."
  (unless header
    (setq header ebrowse--header))
  (unless tags-file
    (setq tags-file ebrowse--tags-file-name))
  (let (tree member accessor file on-class
	     (where (if (= prefix 4) 'other-window
		      (if (= prefix 5) 'other-frame 'this-window))))
    ;; If not given as parameters, get the necessary information
    ;; out of the member buffer.
    (if info
	(setq tree (first info)
	      accessor (second info)
	      member (third info))
      (multiple-value-setq (tree member on-class)
	(values-list (ebrowse-member-info-from-point)))
      (setq accessor ebrowse--accessor))
    ;; View/find class if on a line containing a class name.
    (when on-class
      (return-from ebrowse-view/find-member-declaration/definition
	(ebrowse-view/find-file-and-search-pattern
	 (ebrowse-ts-class tree)
	 (list ebrowse--header (ebrowse-ts-class tree) nil)
	 (ebrowse-cs-file (ebrowse-ts-class tree))
	 tags-file view where)))
    ;; For some member lists, it doesn't make sense to search for
    ;; a definition. If this is requested, silently search for the
    ;; declaration.
    (when (and definition
	       (eq accessor 'ebrowse-ts-member-variables))
      (setq definition nil))
    ;; Construct a suitable `browse' struct for definitions.
    (when definition
      (setf member (make-ebrowse-ms
		    :name (ebrowse-ms-name member)
		    :file (ebrowse-ms-definition-file member)
		    :pattern (ebrowse-ms-definition-pattern
			      member)
		    :flags (ebrowse-ms-flags member)
		    :point (ebrowse-ms-definition-point
			    member))))
    ;; When no file information in member, use that of the class
    (setf file (or (ebrowse-ms-file member)
		   (if definition
		       (ebrowse-cs-source-file (ebrowse-ts-class tree))
		     (ebrowse-cs-file (ebrowse-ts-class tree)))))
    ;; When we have no regular expressions in the database the only
    ;; indication that the parser hasn't seen a definition/declaration
    ;; is that the search start point will be zero.
    (if (or (null file) (zerop (ebrowse-ms-point member)))
	(if (y-or-n-p (concat "No information about "
			      (if definition "definition" "declaration")
			      ".  Search for "
			      (if definition "declaration" "definition")
			      " of `"
			      (ebrowse-ms-name member)
			      "'? "))
	    (progn
	      (message nil)
	      ;; Recurse with new info.
	      (ebrowse-view/find-member-declaration/definition
	       prefix view (not definition) info header tags-file))
	  (error "Search canceled"))
      ;; Find that thing.
      (ebrowse-view/find-file-and-search-pattern
       (make-ebrowse-bs :name (ebrowse-ms-name member)
			:pattern (ebrowse-ms-pattern member)
			:file (ebrowse-ms-file member)
			:flags (ebrowse-ms-flags member)
			:point (ebrowse-ms-point member))
       (list header member accessor)
       file
       tags-file
       view
       where))))



;;; Drawing the member buffer

(defun ebrowse-redisplay-member-buffer ()
  "Force buffer redisplay."
  (interactive)
  (let ((display-fn (if ebrowse--long-display-flag
			'ebrowse-draw-member-long-fn
		      'ebrowse-draw-member-short-fn)))
    (ebrowse-output
      (erase-buffer)
      ;; Show this class
      (ebrowse-draw-member-buffer-class-line)
      (funcall display-fn ebrowse--member-list ebrowse--displayed-class)
      ;; Show inherited members if corresponding switch is on
      (when ebrowse--show-inherited-flag
	(dolist (super (ebrowse-base-classes ebrowse--displayed-class))
	  (goto-char (point-max))
	  (insert (if (bolp) "\n\n" "\n"))
	  (ebrowse-draw-member-buffer-class-line super)
	  (funcall display-fn (funcall ebrowse--accessor super) super)))
      (ebrowse-update-member-buffer-mode-line))))


(defun ebrowse-draw-member-buffer-class-line (&optional class)
  "Display the title line for a class section in the member buffer.
CLASS non-nil means display that class' title.  Otherwise use
the class cursor is on."
  (let ((start (point))
	(tree  (or class ebrowse--displayed-class))
	class-name-start
	class-name-end)
    (insert "class ")
    (setq class-name-start (point))
    (insert (ebrowse-qualified-class-name (ebrowse-ts-class tree)))
    (when (ebrowse-template-p (ebrowse-ts-class tree))
      (insert "<>"))
    (setq class-name-end (point))
    (insert ":\n\n")
    (ebrowse-set-face start (point) 'ebrowse-member-class)
    (add-text-properties
     class-name-start class-name-end
     '(ebrowse-what class-name
		    mouse-face highlight
		    help-echo "mouse-3: menu"))
    (put-text-property start class-name-end 'ebrowse-tree tree)))


(defun ebrowse-display-member-buffer (list &optional stand-alone class)
  "Start point for member buffer creation.
LIST is the member list to display.  STAND-ALONE non-nil
means the member buffer is standalone.  CLASS is its class."
  (let* ((classes ebrowse--tree-obarray)
	 (tree ebrowse--tree)
	 (tags-file ebrowse--tags-file-name)
	 (header ebrowse--header)
	 temp-buffer-setup-hook
	 (temp-buffer (get-buffer ebrowse-member-buffer-name)))
    ;; Get the class description from the name the cursor
    ;; is on if not specified as an argument.
    (unless class
      (setq class (ebrowse-tree-at-point)))
    (save-selected-window
      (if temp-buffer
	  (pop-to-buffer temp-buffer)
	(pop-to-buffer (get-buffer-create ebrowse-member-buffer-name))
	;; If new buffer, set the mode and initial values of locals
	(ebrowse-member-mode))
      ;; Set local variables
      (setq ebrowse--member-list (funcall list class)
	    ebrowse--displayed-class class
	    ebrowse--accessor list
	    ebrowse--tree-obarray classes
	    ebrowse--frozen-flag stand-alone
	    ebrowse--tags-file-name tags-file
	    ebrowse--header header
	    ebrowse--tree tree
	    buffer-read-only t)
      (ebrowse-redisplay-member-buffer)
      (current-buffer))))


(defun ebrowse-member-display-p (member)
  "Return t if MEMBER must be displayed under the current filter settings."
  (if (and (aref ebrowse--filters (ebrowse-ms-visibility member))
	   (or (null ebrowse--const-display-flag)
	       (ebrowse-const-p member))
	   (or (null ebrowse--inline-display-flag)
	       (ebrowse-inline-p member))
	   (or (null ebrowse--pure-display-flag)
	       (ebrowse-bs-p member))
	   (or (null ebrowse--virtual-display-flag)
	       (ebrowse-virtual-p member)))
      member))


(defun ebrowse-draw-member-attributes (member)
  "Insert a string for the attributes of MEMBER."
  (insert (if (ebrowse-template-p member) "T" "-")
	  (if (ebrowse-extern-c-p member) "C" "-")
	  (if (ebrowse-virtual-p member) "v" "-")
	  (if (ebrowse-inline-p member) "i" "-")
	  (if (ebrowse-const-p member) "c" "-")
	  (if (ebrowse-pure-virtual-p member) "0" "-")
	  (if (ebrowse-mutable-p member) "m" "-")
	  (if (ebrowse-explicit-p member) "e" "-")
	  (if (ebrowse-throw-list-p member) "t" "-")))


(defun ebrowse-draw-member-regexp (member-struc)
  "Insert a string for the regular expression matching MEMBER-STRUC."
  (let ((pattern (if ebrowse--source-regexp-flag
		     (ebrowse-ms-definition-pattern
		      member-struc)
		   (ebrowse-ms-pattern member-struc))))
    (cond ((stringp pattern)
	   (insert (ebrowse-trim-string pattern) "...\n")
	   (beginning-of-line 0)
	   (move-to-column (+ 4 ebrowse--decl-column))
	   (while (re-search-forward "[ \t]+" nil t)
	     (delete-region (match-beginning 0) (match-end 0))
	     (insert " "))
	   (beginning-of-line 2))
	  (t
	   (insert "[not recorded or unknown]\n")))))


(defun ebrowse-draw-member-long-fn (member-list tree)
  "Display member buffer for MEMBER-LIST in long form.
TREE is the class tree of MEMBER-LIST."
  (dolist (member-struc (mapcar 'ebrowse-member-display-p member-list))
    (when member-struc
      (let ((name (ebrowse-ms-name member-struc))
	    (start (point)))
	;; Insert member name truncated to the right length
	(insert (substring name
			   0
			   (min (length name)
				(1- ebrowse--decl-column))))
	(add-text-properties
	 start (point)
	 `(mouse-face highlight ebrowse-what member-name
		      ebrowse-member ,member-struc
		      ebrowse-tree ,tree
		      help-echo "mouse-2: view definition; mouse-3: menu"))
	;; Display virtual, inline, and const status
	(setf start (point))
	(indent-to ebrowse--decl-column)
	(put-text-property start (point) 'mouse-face nil)
	(when ebrowse--attributes-flag
	  (let ((start (point)))
	    (insert "<")
	    (ebrowse-draw-member-attributes member-struc)
	    (insert ">")
	    (ebrowse-set-face start (point)
			      'ebrowse-member-attribute)))
	(insert " ")
	(ebrowse-draw-member-regexp member-struc))))
  (insert "\n")
  (goto-char (point-min)))


(defun ebrowse-draw-member-short-fn (member-list tree)
  "Display MEMBER-LIST in short form.
TREE is the class tree in which the members are found."
  (let ((i 0)
	(column-width (+ ebrowse--column-width
			 (if ebrowse--attributes-flag 12 0))))
    ;; Get the number of columns to draw.
    (setq ebrowse--n-columns
	  (max 1 (/ (ebrowse-width-of-drawable-area) column-width)))
    (dolist (member (mapcar #'ebrowse-member-display-p member-list))
      (when member
	(let ((name (ebrowse-ms-name member))
	      start-of-entry
	      (start-of-column (point))
	      start-of-name)
	  (indent-to (* i column-width))
	  (put-text-property start-of-column (point) 'mouse-face nil)
	  (setq start-of-entry (point))
	  ;; Show various attributes
	  (when ebrowse--attributes-flag
	    (insert "<")
	    (ebrowse-draw-member-attributes member)
	    (insert "> ")
	    (ebrowse-set-face start-of-entry (point)
			      'ebrowse-member-attribute))
	  ;; insert member name truncated to column width
	  (setq start-of-name (point))
	  (insert (substring name 0
			     (min (length name)
				  (1- ebrowse--column-width))))
	  ;; set text properties
	  (add-text-properties
	   start-of-name (point)
	   `(ebrowse-what member-name
			  ebrowse-member ,member
			  mouse-face highlight
			  ebrowse-tree ,tree
			  help-echo "mouse-2: view definition; mouse-3: menu"))
	  (incf i)
	  (when (>= i ebrowse--n-columns)
	    (setf i 0)
	    (insert "\n")))))
    (when (plusp i)
      (insert "\n"))
    (goto-char (point-min))))



;;; Killing members from tree

(defun ebrowse-member-info-from-point ()
  "Ger information about the member at point.
The result has the form (TREE MEMBER NULL-P).  TREE is the tree
we're in, MEMBER is the member we're on.  NULL-P is t if MEMBER
is nil."
  (let ((tree (or (get-text-property (point) 'ebrowse-tree)
		  (error "No information at point")))
	(member (get-text-property (point) 'ebrowse-member)))
    (list tree member (null member))))



;;; Switching member buffer to display a selected member

(defun ebrowse-goto-visible-member/all-member-lists (_prefix)
  "Position cursor on a member read from the minibuffer.
With PREFIX, search all members in the tree.  Otherwise consider
only members visible in the buffer."
  (interactive "p")
  (ebrowse-ignoring-completion-case
    (let* ((completion-list (ebrowse-name/accessor-alist-for-class-members))
	   (member (completing-read "Goto member: " completion-list nil t))
	   (accessor (cdr (assoc member completion-list))))
      (unless accessor
	(error "`%s' not found" member))
      (unless (eq accessor ebrowse--accessor)
	(setf ebrowse--accessor accessor
	      ebrowse--member-list (funcall accessor ebrowse--displayed-class))
	(ebrowse-redisplay-member-buffer))
      (ebrowse-move-point-to-member member))))


(defun ebrowse-goto-visible-member (repeat)
  "Position point on a member.
Read the member's name from the minibuffer.  Consider only members
visible in the member buffer.
REPEAT non-nil means repeat the search that number of times."
  (interactive "p")
  (ebrowse-ignoring-completion-case
    ;; Read member name
    (let* ((completion-list (ebrowse-name/accessor-alist-for-visible-members))
	   (member (completing-read "Goto member: " completion-list nil t)))
      (ebrowse-move-point-to-member member repeat))))



;;; Searching a member in the member buffer

(defun ebrowse-repeat-member-search (repeat)
  "Repeat the last regular expression search.
REPEAT, if specified, says repeat the search REPEAT times."
  (interactive "p")
  (unless ebrowse--last-regexp
    (error "No regular expression remembered"))
  ;; Skip over word the point is on
  (skip-chars-forward "^ \t\n")
  ;; Search for regexp from point
  (if (re-search-forward ebrowse--last-regexp nil t repeat)
      (progn
	(goto-char (match-beginning 0))
	(skip-chars-forward " \t\n"))
    ;; If not found above, repeat search from buffer start
    (goto-char (point-min))
    (if (re-search-forward ebrowse--last-regexp nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (skip-chars-forward " \t\n"))
      (error "Not found"))))


(defun* ebrowse-move-point-to-member (name &optional count &aux member)
  "Set point on member NAME in the member buffer
COUNT, if specified, says search the COUNT'th member with the same name."
  (goto-char (point-min))
  (widen)
  (setq member
	(substring name 0 (min (length name) (1- ebrowse--column-width)))
	ebrowse--last-regexp
	(concat "[ \t\n]" (regexp-quote member) "[ \n\t]"))
  (if (re-search-forward ebrowse--last-regexp nil t count)
      (goto-char (1+ (match-beginning 0)))
    (error "Not found")))



;;; Switching member buffer to another class.

(defun ebrowse-switch-member-buffer-to-other-class (title compl-list)
  "Switch member buffer to a class read from the minibuffer.
Use TITLE as minibuffer prompt.
COMPL-LIST is a completion list to use."
  (let* ((initial (unless (second compl-list)
		    (first (first compl-list))))
	 (class (or (ebrowse-completing-read-value title compl-list initial)
		    (error "Not found"))))
    (setf ebrowse--displayed-class class
	  ebrowse--member-list (funcall ebrowse--accessor ebrowse--displayed-class))
    (ebrowse-redisplay-member-buffer)))


(defun ebrowse-switch-member-buffer-to-any-class ()
  "Switch member buffer to a class read from the minibuffer."
  (interactive)
  (ebrowse-switch-member-buffer-to-other-class
   "Goto class: " (ebrowse-tree-obarray-as-alist)))


(defun ebrowse-switch-member-buffer-to-base-class (arg)
  "Switch buffer to ARG'th base class."
  (interactive "P")
  (let ((supers (or (ebrowse-direct-base-classes ebrowse--displayed-class)
		    (error "No base classes"))))
    (if (and arg (second supers))
	(let ((alist (loop for s in supers
			   collect (cons (ebrowse-qualified-class-name
					  (ebrowse-ts-class s))
					 s))))
	  (ebrowse-switch-member-buffer-to-other-class
	   "Goto base class: " alist))
      (setq ebrowse--displayed-class (first supers)
	    ebrowse--member-list
	    (funcall ebrowse--accessor ebrowse--displayed-class))
      (ebrowse-redisplay-member-buffer))))

(defun ebrowse-switch-member-buffer-to-next-sibling-class (arg)
  "Move to ARG'th next sibling."
  (interactive "p")
  (ebrowse-switch-member-buffer-to-sibling-class arg))


(defun ebrowse-switch-member-buffer-to-previous-sibling-class (arg)
  "Move to ARG'th previous sibling."
  (interactive "p")
  (ebrowse-switch-member-buffer-to-sibling-class (- arg)))


(defun ebrowse-switch-member-buffer-to-sibling-class (inc)
  "Switch member display to nth sibling class.
Prefix arg INC specifies which one."
  (interactive "p")
  (let ((containing-list ebrowse--tree)
	index cls
	(supers (ebrowse-direct-base-classes ebrowse--displayed-class)))
    (flet ((trees-alist (trees)
			(loop for tr in trees
			      collect (cons (ebrowse-cs-name
					     (ebrowse-ts-class tr)) tr))))
      (when supers
	(let ((tree (if (second supers)
			(ebrowse-completing-read-value
			 "Relative to base class: "
			 (trees-alist supers) nil)
		      (first supers))))
	  (unless tree (error "Not found"))
	  (setq containing-list (ebrowse-ts-subclasses tree)))))
    (setq index (+ inc (ebrowse-position ebrowse--displayed-class
					 containing-list)))
    (cond ((minusp index) (message "No previous class"))
	  ((null (nth index containing-list)) (message "No next class")))
    (setq index (max 0 (min index (1- (length containing-list)))))
    (setq cls (nth index containing-list))
    (setf ebrowse--displayed-class cls
	  ebrowse--member-list (funcall ebrowse--accessor cls))
    (ebrowse-redisplay-member-buffer)))


(defun ebrowse-switch-member-buffer-to-derived-class (arg)
  "Switch member display to nth derived class.
Prefix arg ARG says which class should be displayed.  Default is
the first derived class."
  (interactive "P")
  (flet ((ebrowse-tree-obarray-as-alist ()
					(loop for s in (ebrowse-ts-subclasses
							ebrowse--displayed-class)
					      collect (cons (ebrowse-cs-name
							     (ebrowse-ts-class s)) s))))
    (let ((subs (or (ebrowse-ts-subclasses ebrowse--displayed-class)
		    (error "No derived classes"))))
      (if (and arg (second subs))
	  (ebrowse-switch-member-buffer-to-other-class
	   "Goto derived class: " (ebrowse-tree-obarray-as-alist))
	(setq ebrowse--displayed-class (first subs)
	      ebrowse--member-list
	      (funcall ebrowse--accessor ebrowse--displayed-class))
	(ebrowse-redisplay-member-buffer)))))



;;; Member buffer mouse functions

(defun ebrowse-displaying-functions ()
  (eq ebrowse--accessor 'ebrowse-ts-member-functions))
(defun ebrowse-displaying-variables ()
  (eq ebrowse--accessor 'ebrowse-ts-member-variables))
(defun ebrowse-displaying-static-functions ()
  )
(defun ebrowse-displaying-static-variables ()
  )
(defun ebrowse-displaying-types ()
  (eq ebrowse--accessor 'ebrowse-ts-types))
(defun ebrowse-displaying-friends ()
  (eq ebrowse--accessor 'ebrowse-ts-friends))

(easy-menu-define
 ebrowse-member-buffer-object-menu ebrowse-member-mode-map
 "Object menu for the member buffer itself."
 '("Members"
   ("Members List"
    ["Functions" ebrowse-display-function-member-list
     :help "Show the list of member functions"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-member-functions)
     :active t]
    ["Variables" ebrowse-display-variables-member-list
     :help "Show the list of member variables"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-member-variables)
     :active t]
    ["Static Functions" ebrowse-display-static-functions-member-list
     :help "Show the list of static member functions"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-static-functions)
     :active t]
    ["Static Variables" ebrowse-display-static-variables-member-list
     :help "Show the list of static member variables"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-static-variables)
     :active t]
    ["Types" ebrowse-display-types-member-list
     :help "Show the list of nested types"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-types)
     :active t]
    ["Friends/Defines" ebrowse-display-friends-member-list
     :help "Show the list of friends or defines"
     :style radio
     :selected (eq ebrowse--accessor 'ebrowse-ts-friends)
     :active t])
   ("Class"
    ["Up" ebrowse-switch-member-buffer-to-base-class
     :help "Show the base class of this class"
     :active t]
    ["Down" ebrowse-switch-member-buffer-to-derived-class
     :help "Show a derived class class of this class"
     :active t]
    ["Next Sibling" ebrowse-switch-member-buffer-to-next-sibling-class
     :help "Show the next sibling class"
     :active t]
    ["Previous Sibling" ebrowse-switch-member-buffer-to-previous-sibling-class
     :help "Show the previous sibling class"
     :active t])
   ("Member"
    ["Show in Tree" ebrowse-show-displayed-class-in-tree
     :help "Show this class in the class tree"
     :active t]
    ["Find in this Class" ebrowse-goto-visible-member
     :help "Search for a member of this class"
     :active t]
    ["Find in Tree" ebrowse-goto-visible-member/all-member-lists
     :help "Search for a member in any class"
     :active t])
   ("Display"
    ["Inherited" ebrowse-toggle-base-class-display
     :help "Toggle display of inherited members"
     :style toggle
     :selected ebrowse--show-inherited-flag
     :active t]
    ["Attributes" ebrowse-toggle-member-attributes-display
     :help "Show member attributes"
     :style toggle
     :selected ebrowse--attributes-flag
     :active t]
    ["Long Display" ebrowse-toggle-long-short-display
     :help "Toggle the member display format"
     :style toggle
     :selected ebrowse--long-display-flag
     :active t]
    ["Column Width" ebrowse-set-member-buffer-column-width
     :help "Set the display's column width"
     :active t])
   ("Filter"
    ["Public" ebrowse-toggle-public-member-filter
     :help "Toggle the visibility of public members"
     :style toggle
     :selected (not (aref ebrowse--filters 0))
     :active t]
    ["Protected" ebrowse-toggle-protected-member-filter
     :help "Toggle the visibility of protected members"
     :style toggle
     :selected (not (aref ebrowse--filters 1))
     :active t]
    ["Private" ebrowse-toggle-private-member-filter
     :help "Toggle the visibility of private members"
     :style toggle
     :selected (not (aref ebrowse--filters 2))
     :active t]
    ["Virtual" ebrowse-toggle-virtual-member-filter
     :help "Toggle the visibility of virtual members"
     :style toggle
     :selected ebrowse--virtual-display-flag
     :active t]
    ["Inline" ebrowse-toggle-inline-member-filter
     :help "Toggle the visibility of inline members"
     :style toggle
     :selected ebrowse--inline-display-flag
     :active t]
    ["Const" ebrowse-toggle-const-member-filter
     :help "Toggle the visibility of const members"
     :style toggle
     :selected ebrowse--const-display-flag
     :active t]
    ["Pure" ebrowse-toggle-pure-member-filter
     :help "Toggle the visibility of pure virtual members"
     :style toggle
     :selected ebrowse--pure-display-flag
     :active t]
    "-----------------"
    ["Show all" ebrowse-remove-all-member-filters
     :help "Remove any display filters"
     :active t])
   ("Buffer"
    ["Tree" ebrowse-pop-from-member-to-tree-buffer
     :help "Pop to the class tree buffer"
     :active t]
    ["Next Member Buffer" ebrowse-switch-to-next-member-buffer
     :help "Switch to the next member buffer of this class tree"
     :active t]
    ["Freeze" ebrowse-freeze-member-buffer
     :help "Freeze (do not reuse) this member buffer"
     :active t])))


(defun ebrowse-on-class-name ()
  "Value is non-nil if point is on a class name."
  (eq (get-text-property (point) 'ebrowse-what) 'class-name))


(defun ebrowse-on-member-name ()
  "Value is non-nil if point is on a member name."
  (eq (get-text-property (point) 'ebrowse-what) 'member-name))


(easy-menu-define
 ebrowse-member-class-name-object-menu ebrowse-member-mode-map
 "Object menu for class names in member buffer."
 '("Class"
   ["Find" ebrowse-find-member-definition
    :help "Find this class in the source files"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]
   ["View" ebrowse-view-member-definition
    :help "View this class in the source files"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]))


(easy-menu-define
 ebrowse-member-name-object-menu ebrowse-member-mode-map
 "Object menu for member names"
 '("Ebrowse"
   ["Find Definition" ebrowse-find-member-definition
    :help "Find this member's definition in the source files"
    :active (ebrowse-on-member-name)]
   ["Find Declaration" ebrowse-find-member-declaration
    :help "Find this member's declaration in the source files"
    :active (ebrowse-on-member-name)]
   ["View Definition" ebrowse-view-member-definition
    :help "View this member's definition in the source files"
    :active (ebrowse-on-member-name)]
   ["View Declaration" ebrowse-view-member-declaration
    :help "View this member's declaration in the source files"
    :active (ebrowse-on-member-name)]))


(defun ebrowse-member-mouse-3 (event)
  "Handle `mouse-3' events in member buffers.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (case (event-click-count event)
    (2 (ebrowse-find-member-definition))
    (1 (case (get-text-property (posn-point (event-start event))
				'ebrowse-what)
	 (member-name
	  (ebrowse-popup-menu ebrowse-member-name-object-menu event))
	 (class-name
	  (ebrowse-popup-menu ebrowse-member-class-name-object-menu event))
	 (t
	  (ebrowse-popup-menu ebrowse-member-buffer-object-menu event))))))


(defun ebrowse-member-mouse-2 (event)
  "Handle `mouse-2' events in member buffers.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (case (event-click-count event)
    (2 (ebrowse-find-member-definition))
    (1 (case (get-text-property (posn-point (event-start event))
				'ebrowse-what)
	 (member-name
	  (ebrowse-view-member-definition 0))))))



;;; Tags view/find

(defun ebrowse-class-alist-for-member (tree-header name)
  "Return information about a member in a class tree.
TREE-HEADER is the header structure of the class tree.
NAME is the name of the member.
Value is an alist of elements (CLASS-NAME . (CLASS LIST NAME)),
where each element describes one occurrence of member NAME in the tree.
CLASS-NAME is the qualified name of the class in which the
member was found.  The CDR of the acons is described in function
`ebrowse-class/index/member-for-member'."
  (let ((table (ebrowse-member-table tree-header))
	known-classes
	alist)
    (when name
      (dolist (info (gethash name table) alist)
	(unless (memq (first info) known-classes)
	  (setf alist (acons (ebrowse-qualified-class-name
			      (ebrowse-ts-class (first info)))
			     info alist)
		known-classes (cons (first info) known-classes)))))))


(defun ebrowse-choose-tree ()
  "Choose a class tree to use.
If there's more than one class tree loaded, let the user choose
the one he wants.  Value is (TREE HEADER BUFFER), with TREE being
the class tree, HEADER the header structure of the tree, and BUFFER
being the tree or member buffer containing the tree."
  (let* ((buffer (ebrowse-choose-from-browser-buffers)))
    (if buffer (list (ebrowse-value-in-buffer 'ebrowse--tree buffer)
		     (ebrowse-value-in-buffer 'ebrowse--header buffer)
		     buffer))))


(defun ebrowse-tags-read-name (header prompt)
  "Read a C++ identifier from the minibuffer.
HEADER is the `ebrowse-hs' structure of the class tree.
Prompt with PROMPT.  Insert into the minibuffer a C++ identifier read
from point as default.  Value is a list (CLASS-NAME MEMBER-NAME)."
  (save-excursion
    (let ((members (ebrowse-member-table header)))
      (multiple-value-bind (class-name member-name)
	  (values-list (ebrowse-tags-read-member+class-name))
	(unless member-name
	  (error "No member name at point"))
	(if members
	    (let* ((name (ebrowse-ignoring-completion-case
			   (completing-read prompt members nil nil member-name)))
		   (completion-result (try-completion name members)))
	      ;; Cannot rely on `try-completion' returning t for exact
	      ;; matches!  It returns the name as a string.
	      (unless (gethash name members)
		(if (y-or-n-p "No exact match found.  Try substrings? ")
		    (setq name
			  (or (first (ebrowse-list-of-matching-members
				      members (regexp-quote name) name))
			      (error "Sorry, nothing found")))
		  (error "Canceled")))
	      (list class-name name))
	  (list class-name (read-from-minibuffer prompt member-name)))))))


(defun ebrowse-tags-read-member+class-name ()
  "Read a C++ identifier from point.
Value is (CLASS-NAME MEMBER-NAME).
CLASS-NAME is the name of the class if the identifier was qualified.
It is nil otherwise.
MEMBER-NAME is the name of the member found."
  (save-excursion
    (skip-chars-backward "a-zA-Z0-9_")
    (let* ((start (point))
	   (name (progn (skip-chars-forward "a-zA-Z0-9_")
			(buffer-substring start (point))))
	   class)
      (list class name))))


(defun ebrowse-tags-choose-class (_tree header name initial-class-name)
  "Read a class name for a member from the minibuffer.
TREE is the class tree we operate on.
HEADER is its header structure.
NAME is the name of the member.
INITIAL-CLASS-NAME is an initial class name to insert in the minibuffer.
Value is a list (TREE ACCESSOR MEMBER) for the member."
  (let ((alist (or (ebrowse-class-alist-for-member header name)
		   (error "No classes with member `%s' found" name))))
    (ebrowse-ignoring-completion-case
      (if (null (second alist))
	  (cdr (first alist))
	(push ?\? unread-command-events)
	(cdr (assoc (completing-read "In class: "
				     alist nil t initial-class-name)
		    alist))))))


(defun* ebrowse-tags-view/find-member-decl/defn
    (prefix &key view definition member-name)
  "If VIEW is t, view, else find an occurrence of MEMBER-NAME.

If DEFINITION is t, find or view the member definition else its
declaration.  This function reads the member's name from the
current buffer like FIND-TAG.  It then prepares a completion list
of all classes containing a member with the given name and lets
the user choose the class to use.  As a last step, a tags search
is performed that positions point on the member declaration or
definition."
  (multiple-value-bind
      (tree header tree-buffer) (values-list (ebrowse-choose-tree))
    (unless tree (error "No class tree"))
    (let* ((marker (point-marker))
	   class-name
	   (name member-name)
	   info)
      (unless name
	(multiple-value-setq (class-name name)
	  (values-list
	   (ebrowse-tags-read-name
	    header
	    (concat (if view "View" "Find") " member "
		    (if definition "definition" "declaration") ": ")))))
      (setq info (ebrowse-tags-choose-class tree header name class-name))
      (ebrowse-push-position marker info)
      ;; Goto the occurrence of the member
      (ebrowse-view/find-member-declaration/definition
       prefix view definition info
       header
       (ebrowse-value-in-buffer 'ebrowse--tags-file-name tree-buffer))
      ;; Record position jumped to
      (ebrowse-push-position (point-marker) info t))))


;;;###autoload
(defun ebrowse-tags-view-declaration ()
  "View declaration of member at point."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 0 :view t :definition nil))


;;;###autoload
(defun ebrowse-tags-find-declaration ()
  "Find declaration of member at point."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 0 :view nil :definition nil))


;;;###autoload
(defun ebrowse-tags-view-definition ()
  "View definition of member at point."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 0 :view t :definition t))


;;;###autoload
(defun ebrowse-tags-find-definition ()
  "Find definition of member at point."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 0 :view nil :definition t))


(defun ebrowse-tags-view-declaration-other-window ()
  "View declaration of member at point in other window."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 4 :view t :definition nil))


;;;###autoload
(defun ebrowse-tags-find-declaration-other-window ()
  "Find declaration of member at point in other window."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 4 :view nil :definition nil))


;;;###autoload
(defun ebrowse-tags-view-definition-other-window ()
  "View definition of member at point in other window."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 4 :view t :definition t))


;;;###autoload
(defun ebrowse-tags-find-definition-other-window ()
  "Find definition of member at point in other window."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 4 :view nil :definition t))


(defun ebrowse-tags-view-declaration-other-frame ()
  "View definition of member at point in other frame."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 5 :view t :definition nil))


;;;###autoload
(defun ebrowse-tags-find-declaration-other-frame ()
  "Find definition of member at point in other frame."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 5 :view nil :definition nil))


;;;###autoload
(defun ebrowse-tags-view-definition-other-frame ()
  "View definition of member at point in other frame."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 5 :view t :definition t))


;;;###autoload
(defun ebrowse-tags-find-definition-other-frame ()
  "Find definition of member at point in other frame."
  (interactive)
  (ebrowse-tags-view/find-member-decl/defn 5 :view nil :definition t))


(defun ebrowse-tags-select/create-member-buffer (tree-buffer info)
  "Select or create member buffer.
TREE-BUFFER specifies the tree to use.  INFO describes the member.
It is a list (TREE ACCESSOR MEMBER)."
  (let ((buffer (get-buffer ebrowse-member-buffer-name)))
    (cond ((null buffer)
	   (set-buffer tree-buffer)
	   (switch-to-buffer (ebrowse-display-member-buffer
			      (second info) nil (first info))))
	  (t
	   (switch-to-buffer buffer)
	   (setq ebrowse--displayed-class (first info)
		 ebrowse--accessor (second info)
		 ebrowse--member-list (funcall ebrowse--accessor ebrowse--displayed-class))
	   (ebrowse-redisplay-member-buffer)))
    (ebrowse-move-point-to-member (ebrowse-ms-name (third info)))))


(defun ebrowse-tags-display-member-buffer (&optional fix-name)
  "Display a member buffer for a member.
FIX-NAME non-nil means display the buffer for that member.
Otherwise read a member name from point."
  (interactive)
  (multiple-value-bind
      (tree header tree-buffer) (values-list (ebrowse-choose-tree))
    (unless tree (error "No class tree"))
    (let* ((marker (point-marker)) class-name (name fix-name) info)
      (unless name
	(multiple-value-setq (class-name name)
	  (values-list
	   (ebrowse-tags-read-name header
				   (concat "Find member list of: ")))))
      (setq info (ebrowse-tags-choose-class tree header name class-name))
      (ebrowse-push-position marker info)
      (ebrowse-tags-select/create-member-buffer tree-buffer info))))


(defun ebrowse-list-of-matching-members (members regexp &optional name)
  "Return a list of members in table MEMBERS matching REGEXP or NAME.
Both NAME and REGEXP may be nil in which case exact or regexp matches
are not performed."
  (let (list)
    (when (or name regexp)
      (maphash (lambda (member-name _info)
		 (when (or (and name (string= name member-name))
			   (and regexp (string-match regexp member-name)))
		   (setq list (cons member-name list))))
	       members))
    list))


(defun ebrowse-tags-apropos ()
  "Display a list of members matching a regexp read from the minibuffer."
  (interactive)
  (let* ((buffer (or (ebrowse-choose-from-browser-buffers)
		     (error "No tree buffer")))
	 (header (ebrowse-value-in-buffer 'ebrowse--header buffer))
	 (members (ebrowse-member-table header))
	 temp-buffer-setup-hook
	 (regexp (read-from-minibuffer "List members matching regexp: ")))
    (with-output-to-temp-buffer (concat "*Apropos Members*")
      (set-buffer standard-output)
      (erase-buffer)
      (insert "Members matching `" regexp "'\n\n")
      (loop for s in (ebrowse-list-of-matching-members members regexp) do
	    (loop for info in (gethash s members) do
		  (ebrowse-draw-file-member-info info))))))


(defun ebrowse-tags-list-members-in-file ()
  "Display a list of members found in a file.
The file name is read from the minibuffer."
  (interactive)
  (let* ((buffer (or (ebrowse-choose-from-browser-buffers)
		     (error "No tree buffer")))
	 (files (with-current-buffer buffer (ebrowse-files-table)))
	 (file (completing-read "List members in file: " files nil t))
	 (header (ebrowse-value-in-buffer 'ebrowse--header buffer))
	 temp-buffer-setup-hook
	 (members (ebrowse-member-table header)))
    (with-output-to-temp-buffer (concat "*Members in file " file "*")
      (set-buffer standard-output)
      (maphash
       (lambda (_member-name list)
	 (loop for info in list
	       as member = (third info)
	       as class = (ebrowse-ts-class (first info))
		when (or (and (null (ebrowse-ms-file member))
			      (string= (ebrowse-cs-file class) file))
			 (string= file (ebrowse-ms-file member)))
	      do (ebrowse-draw-file-member-info info "decl.")
	      when (or (and (null (ebrowse-ms-definition-file member))
			    (string= (ebrowse-cs-source-file class) file))
		       (string= file (ebrowse-ms-definition-file member)))
	      do (ebrowse-draw-file-member-info info "defn.")))
       members))))


(defun* ebrowse-draw-file-member-info (info &optional (kind ""))
  "Display a line in the members info buffer.
INFO describes the member.  It has the form (TREE ACCESSOR MEMBER).
TREE is the class of the member to display.
ACCESSOR is the accessor symbol of its member list.
MEMBER is the member structure.
KIND is an additional string printed in the buffer."
  (let* ((tree (first info))
	 (globals-p (ebrowse-globals-tree-p tree)))
    (unless globals-p
      (insert (ebrowse-cs-name (ebrowse-ts-class tree))))
    (insert "::" (ebrowse-ms-name (third info)))
    (indent-to 40)
    (insert kind)
    (indent-to 50)
    (insert (case (second info)
	      (ebrowse-ts-member-functions "member function")
	      (ebrowse-ts-member-variables "member variable")
	      (ebrowse-ts-static-functions "static function")
	      (ebrowse-ts-static-variables "static variable")
	      (ebrowse-ts-friends (if globals-p "define" "friend"))
	      (ebrowse-ts-types "type")
	      (t "unknown"))
	    "\n")))

(defvar ebrowse-last-completion nil
  "Text inserted by the last completion operation.")


(defvar ebrowse-last-completion-start nil
  "String which was the basis for the last completion operation.")


(defvar ebrowse-last-completion-location nil
  "Buffer position at which the last completion operation was initiated.")


(defvar ebrowse-last-completion-obarray nil
  "Member used in last completion operation.")


(make-variable-buffer-local 'ebrowse-last-completion-obarray)
(make-variable-buffer-local 'ebrowse-last-completion-location)
(make-variable-buffer-local 'ebrowse-last-completion)
(make-variable-buffer-local 'ebrowse-last-completion-start)



(defun ebrowse-some-member-table ()
  "Return a hash table containing all members of a tree.
If there's only one tree loaded, use that.  Otherwise let the
use choose a tree."
  (let* ((buffers (ebrowse-known-class-trees-buffer-list))
	 (buffer (cond ((and (first buffers) (not (second buffers)))
			(first buffers))
		       (t (or (ebrowse-electric-choose-tree)
			      (error "No tree buffer")))))
	 (header (ebrowse-value-in-buffer 'ebrowse--header buffer)))
    (ebrowse-member-table header)))


(defun ebrowse-cyclic-successor-in-string-list (string list)
  "Return the item following STRING in LIST.
If STRING is the last element, return the first element as successor."
  (or (nth (1+ (ebrowse-position string list 'string=)) list)
      (first list)))


;;; Symbol completion

;;;###autoload
(defun* ebrowse-tags-complete-symbol (prefix)
  "Perform completion on the C++ symbol preceding point.
A second call of this function without changing point inserts the next match.
A call with prefix PREFIX reads the symbol to insert from the minibuffer with
completion."
  (interactive "P")
  (let* ((end (point))
	 (begin (save-excursion (skip-chars-backward "a-zA-Z_0-9") (point)))
	 (pattern (buffer-substring begin end))
	 list completion)
    (cond
     ;; With prefix, read name from minibuffer with completion.
     (prefix
      (let* ((members (ebrowse-some-member-table))
	     (completion (completing-read "Insert member: "
					  members nil t pattern)))
	(when completion
	  (setf ebrowse-last-completion-location nil)
	  (delete-region begin end)
	  (insert completion))))
     ;; If this function is called at the same point the last
     ;; expansion ended, insert the next expansion.
     ((eq (point) ebrowse-last-completion-location)
      (setf list (all-completions ebrowse-last-completion-start
				  ebrowse-last-completion-obarray)
	    completion (ebrowse-cyclic-successor-in-string-list
			ebrowse-last-completion list))
      (cond ((null completion)
	     (error "No completion"))
	    ((string= completion pattern)
	     (error "No further completion"))
	    (t
	     (delete-region begin end)
	     (insert completion)
	     (setf ebrowse-last-completion completion
		   ebrowse-last-completion-location (point)))))
     ;; First time the function is called at some position in the
     ;; buffer: Start new completion.
     (t
      (let* ((members (ebrowse-some-member-table))
	     (completion (first (all-completions pattern members nil))))
	(cond ((eq completion t))
	      ((null completion)
	       (error "Can't find completion for `%s'" pattern))
	      (t
	       (delete-region begin end)
	       (insert completion)

	       (setf ebrowse-last-completion-location (point)
		     ebrowse-last-completion-start pattern
		     ebrowse-last-completion completion
		     ebrowse-last-completion-obarray members))))))))


;;; Tags query replace & search

(defvar ebrowse-tags-loop-form ()
  "Form for `ebrowse-loop-continue'.
Evaluated for each file in the tree.  If it returns nil, proceed
with the next file.")

(defvar ebrowse-tags-next-file-list ()
  "A list of files to be processed.")


(defvar ebrowse-tags-next-file-path nil
  "The path relative to which files have to be searched.")


(defvar ebrowse-tags-loop-last-file nil
  "The last file visited via `ebrowse-tags-loop'.")


(defun ebrowse-tags-next-file (&optional initialize tree-buffer)
  "Select next file among files in current tag table.
Non-nil argument INITIALIZE (prefix arg, if interactive) initializes
to the beginning of the list of files in the tag table.
TREE-BUFFER specifies the class tree we operate on."
  (interactive "P")
  ;; Call with INITIALIZE non-nil initializes the files list.
  ;; If more than one tree buffer is loaded, let the user choose
  ;; on which tree (s)he wants to operate.
  (when initialize
    (let ((buffer (or tree-buffer (ebrowse-choose-from-browser-buffers))))
      (with-current-buffer buffer
	(setq ebrowse-tags-next-file-list
	      (ebrowse-files-list (ebrowse-marked-classes-p))
	      ebrowse-tags-loop-last-file
	      nil
	      ebrowse-tags-next-file-path
	      (file-name-directory ebrowse--tags-file-name)))))
  ;; End of the loop if the stack of files is empty.
  (unless ebrowse-tags-next-file-list
    (error "All files processed"))
  ;; ebrowse-tags-loop-last-file is the last file that was visited due
  ;; to a call to BROWSE-LOOP (see below). If that file is still
  ;; in memory, and it wasn't modified, throw its buffer away to
  ;; prevent cluttering up the buffer list.
  (when ebrowse-tags-loop-last-file
    (let ((buffer (get-file-buffer ebrowse-tags-loop-last-file)))
      (when (and buffer
		 (not (buffer-modified-p buffer)))
	(kill-buffer buffer))))
  ;; Remember this buffer file name for later deletion, if it
  ;; wasn't visited by other means.
  (let ((file (expand-file-name (car ebrowse-tags-next-file-list)
				ebrowse-tags-next-file-path)))
    (setq ebrowse-tags-loop-last-file (if (get-file-buffer file) nil file))
    ;; Find the file and pop the file list. Pop has to be done
    ;; before the file is loaded because FIND-FILE might encounter
    ;; an error, and we want to be able to proceed with the next
    ;; file in this case.
    (pop ebrowse-tags-next-file-list)
    (find-file file)))


;;;###autoload
(defun ebrowse-tags-loop-continue (&optional first-time tree-buffer)
  "Repeat last operation on files in tree.
FIRST-TIME non-nil means this is not a repetition, but the first time.
TREE-BUFFER if indirectly specifies which files to loop over."
  (interactive)
  (when first-time
    (ebrowse-tags-next-file first-time tree-buffer)
    (goto-char (point-min)))
  (while (not (eval ebrowse-tags-loop-form))
    (ebrowse-tags-next-file)
    (message "Scanning file `%s'..." buffer-file-name)
    (goto-char (point-min))))


;;;###autoload
(defun ebrowse-tags-search (regexp)
  "Search for REGEXP in all files in a tree.
If marked classes exist, process marked classes, only.
If regular expression is nil, repeat last search."
  (interactive "sTree search (regexp): ")
  (if (and (string= regexp "")
	   (eq (car ebrowse-tags-loop-form) 're-search-forward))
      (ebrowse-tags-loop-continue)
    (setq ebrowse-tags-loop-form (list 're-search-forward regexp nil t))
    (ebrowse-tags-loop-continue 'first-time)))


;;;###autoload
(defun ebrowse-tags-query-replace (from to)
  "Query replace FROM with TO in all files of a class tree.
With prefix arg, process files of marked classes only."
  (interactive
   "sTree query replace (regexp): \nsTree query replace %s by: ")
  (setq ebrowse-tags-loop-form
	(list 'and (list 'save-excursion
			 (list 're-search-forward from nil t))
	      (list 'not (list 'perform-replace from to t t nil))))
  (ebrowse-tags-loop-continue 'first-time))


;;;###autoload
(defun ebrowse-tags-search-member-use (&optional fix-name)
  "Search for call sites of a member.
If FIX-NAME is specified, search uses of that member.
Otherwise, read a member name from the minibuffer.
Searches in all files mentioned in a class tree for something that
looks like a function call to the member."
  (interactive)
  ;; Choose the tree to use if there is more than one.
  (multiple-value-bind (tree header tree-buffer)
      (values-list (ebrowse-choose-tree))
    (unless tree
      (error "No class tree"))
    ;; Get the member name NAME (class-name is ignored).
    (let ((name fix-name) class-name regexp)
      (unless name
	(multiple-value-setq (class-name name)
	  (values-list (ebrowse-tags-read-name header "Find calls of: "))))
      ;; Set tags loop form to search for member and begin loop.
      (setq regexp (concat "\\<" name "[ \t]*(")
	    ebrowse-tags-loop-form (list 're-search-forward regexp nil t))
      (ebrowse-tags-loop-continue 'first-time tree-buffer))))



;;; Tags position management

;;; Structures of this kind are the elements of the position stack.

(defstruct (ebrowse-position (:type vector) :named)
  file-name				; in which file
  point					; point in file
  target				; t if target of a jump
  info)					; (CLASS FUNC MEMBER) jumped to


(defvar ebrowse-position-stack ()
  "Stack of `ebrowse-position' structured.")


(defvar ebrowse-position-index 0
  "Current position in position stack.")


(defun ebrowse-position-name (position)
  "Return an identifying string for POSITION.
The string is printed in the electric position list buffer."
  (let ((info (ebrowse-position-info position)))
    (concat (if (ebrowse-position-target position) "at " "to ")
	    (ebrowse-cs-name (ebrowse-ts-class (first info)))
	    "::" (ebrowse-ms-name (third info)))))


(defun ebrowse-view/find-position (position &optional view)
  "Position point on POSITION.
If VIEW is non-nil, view the position, otherwise find it."
  (cond ((not view)
	 (find-file (ebrowse-position-file-name position))
	 (goto-char (ebrowse-position-point position)))
	(t
	 (unwind-protect
	     (progn
	       (push (function
		      (lambda ()
			(goto-char (ebrowse-position-point position))))
		     view-mode-hook)
	       (view-file (ebrowse-position-file-name position)))
	   (pop view-mode-hook)))))


(defun ebrowse-push-position (marker info &optional target)
  "Push current position on position stack.
MARKER is the marker to remember as position.
INFO is a list (CLASS FUNC MEMBER) specifying what we jumped to.
TARGET non-nil means we performed a jump.
Positions in buffers that have no file names are not saved."
  (when (buffer-file-name (marker-buffer marker))
    (let ((too-much (- (length ebrowse-position-stack)
		       ebrowse-max-positions)))
      ;; Do not let the stack grow to infinity.
      (when (plusp too-much)
	(setq ebrowse-position-stack
	      (butlast ebrowse-position-stack too-much)))
      ;; Push the position.
      (push (make-ebrowse-position
	     :file-name (buffer-file-name (marker-buffer marker))
	     :point (marker-position marker)
	     :target target
	     :info info)
	    ebrowse-position-stack))))


(defun ebrowse-move-in-position-stack (increment)
  "Move by INCREMENT in the position stack."
  (let ((length (length ebrowse-position-stack)))
    (when (zerop length)
      (error "No positions remembered"))
    (setq ebrowse-position-index
	  (mod (+ increment ebrowse-position-index) length))
    (message "Position %d of %d " ebrowse-position-index length)
    (ebrowse-view/find-position (nth ebrowse-position-index
				     ebrowse-position-stack))))


;;;###autoload
(defun ebrowse-back-in-position-stack (arg)
  "Move backward in the position stack.
Prefix arg ARG says how much."
  (interactive "p")
  (ebrowse-move-in-position-stack (max 1 arg)))


;;;###autoload
(defun ebrowse-forward-in-position-stack (arg)
  "Move forward in the position stack.
Prefix arg ARG says how much."
  (interactive "p")
  (ebrowse-move-in-position-stack (min -1 (- arg))))



;;; Electric position list

(defvar ebrowse-electric-position-mode-map ()
  "Keymap used in electric position stack window.")


(defvar ebrowse-electric-position-mode-hook nil
  "If non-nil, its value is called by `ebrowse-electric-position-mode'.")


(unless ebrowse-electric-position-mode-map
  (let ((map (make-keymap))
	(submap (make-keymap)))
    (setq ebrowse-electric-position-mode-map map)
    (fillarray (car (cdr map)) 'ebrowse-electric-position-undefined)
    (fillarray (car (cdr submap)) 'ebrowse-electric-position-undefined)
    (define-key map "\e" submap)
    (define-key map "\C-z" 'suspend-frame)
    (define-key map "\C-h" 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'ebrowse-electric-position-quit)
    (define-key map "q" 'ebrowse-electric-position-quit)
    (define-key map " " 'ebrowse-electric-select-position)
    (define-key map "\C-l" 'recenter)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "v" 'ebrowse-electric-view-position)
    (define-key map "\C-v" 'scroll-up-command)
    (define-key map "\ev" 'scroll-down-command)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (define-key map "\e>" 'end-of-buffer)))

(put 'ebrowse-electric-position-mode 'mode-class 'special)
(put 'ebrowse-electric-position-undefined 'suppress-keymap t)


(define-derived-mode ebrowse-electric-position-mode
  fundamental-mode "Electric Position Menu"
  "Mode for electric position buffers.
Runs the hook `ebrowse-electric-position-mode-hook'."
  (setq mode-line-buffer-identification "Electric Position Menu")
  (when (memq 'mode-name mode-line-format)
    (setq mode-line-format (copy-sequence mode-line-format))
    (setcar (memq 'mode-name mode-line-format) "Positions"))
  (set (make-local-variable 'Helper-return-blurb) "return to buffer editing")
  (setq truncate-lines t
	buffer-read-only t))


(defun ebrowse-draw-position-buffer ()
  "Display positions in buffer *Positions*."
  (set-buffer (get-buffer-create "*Positions*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "File           Point  Description\n"
	  "----           -----  -----------\n")
  (dolist (position ebrowse-position-stack)
    (insert (file-name-nondirectory (ebrowse-position-file-name position)))
    (indent-to 15)
    (insert (int-to-string (ebrowse-position-point position)))
    (indent-to 22)
    (insert (ebrowse-position-name position) "\n"))
  (setq buffer-read-only t))


;;;###autoload
(defun ebrowse-electric-position-menu ()
  "List positions in the position stack in an electric buffer."
  (interactive)
  (unless ebrowse-position-stack
    (error "No positions remembered"))
  (let (select buffer window)
    (save-window-excursion
      (save-window-excursion (ebrowse-draw-position-buffer))
      (setq window (Electric-pop-up-window "*Positions*")
	    buffer (window-buffer window))
      (shrink-window-if-larger-than-buffer window)
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (ebrowse-electric-position-mode)
	    (setq select
		  (catch 'ebrowse-electric-select-position
		    (message "<<< Press Space to bury the list >>>")
		    (let ((first (progn (goto-char (point-min))
					(forward-line 2)
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      (goto-char first)
		      (Electric-command-loop 'ebrowse-electric-select-position
					     nil t
					     'ebrowse-electric-position-looper
					     (cons first last))))))
	(set-buffer buffer)
	(bury-buffer buffer)
	(message nil)))
    (when select
      (set-buffer buffer)
      (ebrowse-electric-find-position select))
    (kill-buffer buffer)))


(defun ebrowse-electric-position-looper (state condition)
  "Prevent moving point on invalid lines.
Called from `Electric-command-loop'.  See there for the meaning
of STATE and CONDITION."
  (cond ((and condition
	      (not (memq (car condition) '(buffer-read-only
					   end-of-buffer
					   beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (forward-line 2))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1)))))


(defun ebrowse-electric-position-undefined ()
  "Function called for undefined keys."
  (interactive)
  (message "Type C-h for help, ? for commands, q to quit, Space to execute")
  (sit-for 4))


(defun ebrowse-electric-position-quit ()
  "Leave the electric position list."
  (interactive)
  (throw 'ebrowse-electric-select-position nil))


(defun ebrowse-electric-select-position ()
  "Select a position from the list."
  (interactive)
  (throw 'ebrowse-electric-select-position (point)))


(defun ebrowse-electric-find-position (point &optional view)
  "View/find what is described by the line at POINT.
If VIEW is non-nil, view else find source files."
  (let ((index (- (count-lines (point-min) point) 2)))
    (ebrowse-view/find-position (nth index
				     ebrowse-position-stack) view)))


(defun ebrowse-electric-view-position ()
  "View the position described by the line point is in."
  (interactive)
  (ebrowse-electric-find-position (point) t))



;;; Saving trees to disk

(defun ebrowse-write-file-hook-fn ()
  "Write current buffer as a class tree.
Installed on `local-write-file-hooks'."
  (ebrowse-save-tree)
  t)


;;;###autoload
(defun ebrowse-save-tree ()
  "Save current tree in same file it was loaded from."
  (interactive)
  (ebrowse-save-tree-as (or buffer-file-name ebrowse--tags-file-name)))


;;;###autoload
(defun ebrowse-save-tree-as (&optional file-name)
  "Write the current tree data structure to a file.
Read the file name from the minibuffer if interactive.
Otherwise, FILE-NAME specifies the file to save the tree in."
  (interactive "FSave tree as: ")
  (let ((temp-buffer (get-buffer-create "*Tree Output"))
	(old-standard-output standard-output)
	(header (copy-ebrowse-hs ebrowse--header))
	(tree ebrowse--tree))
    (unwind-protect
	(with-current-buffer (setq standard-output temp-buffer)
	  (erase-buffer)
	  (setf (ebrowse-hs-member-table header) nil)
	  (insert (prin1-to-string header) " ")
	  (mapc 'ebrowse-save-class tree)
	  (write-file file-name)
	  (message "Tree written to file `%s'" file-name))
      (kill-buffer temp-buffer)
      (set-buffer-modified-p nil)
      (ebrowse-update-tree-buffer-mode-line)
      (setq standard-output old-standard-output))))


(defun ebrowse-save-class (class)
  "Write single class CLASS to current buffer."
  (message "%s..." (ebrowse-cs-name (ebrowse-ts-class class)))
  (insert "[ebrowse-ts ")
  (prin1 (ebrowse-ts-class class))	;class name
  (insert "(")				;list of subclasses
  (mapc 'ebrowse-save-class (ebrowse-ts-subclasses class))
  (insert ")")
  (dolist (func ebrowse-member-list-accessors)
    (prin1 (funcall func class))
    (insert "\n"))
  (insert "()")				;base-classes slot
  (prin1 (ebrowse-ts-mark class))
  (insert "]\n"))



;;; Statistics

;;;###autoload
(defun ebrowse-statistics ()
  "Display statistics for a class tree."
  (interactive)
  (let ((tree-file (buffer-file-name))
	temp-buffer-setup-hook)
    (with-output-to-temp-buffer "*Tree Statistics*"
      (multiple-value-bind (classes member-functions member-variables
				    static-functions static-variables)
	  (values-list (ebrowse-gather-statistics))
	(set-buffer standard-output)
	(erase-buffer)
	(insert "STATISTICS FOR TREE " (or tree-file "unknown") ":\n\n")
	(ebrowse-print-statistics-line "Number of classes:" classes)
	(ebrowse-print-statistics-line "Number of member functions:"
				       member-functions)
	(ebrowse-print-statistics-line "Number of member variables:"
				       member-variables)
	(ebrowse-print-statistics-line "Number of static functions:"
				       static-functions)
	(ebrowse-print-statistics-line "Number of static variables:"
				       static-variables)))))


(defun ebrowse-print-statistics-line (title value)
  "Print a line in the statistics buffer.
TITLE is the title of the line, VALUE is a number to be printed
after that."
  (insert title)
  (indent-to 40)
  (insert (format "%d\n" value)))


(defun ebrowse-gather-statistics ()
  "Return statistics for a class tree.
The result is a list (NUMBER-OF-CLASSES NUMBER-OF-MEMBER-FUNCTIONS
NUMBER-OF-INSTANCE-VARIABLES NUMBER-OF-STATIC-FUNCTIONS
NUMBER-OF-STATIC-VARIABLES:"
  (let ((classes 0) (member-functions 0) (member-variables 0)
	(static-functions 0) (static-variables 0))
    (ebrowse-for-all-trees (tree ebrowse--tree-obarray)
      (incf classes)
      (incf member-functions (length (ebrowse-ts-member-functions tree)))
      (incf member-variables (length (ebrowse-ts-member-variables tree)))
      (incf static-functions (length (ebrowse-ts-static-functions tree)))
      (incf static-variables (length (ebrowse-ts-static-variables tree))))
    (list classes member-functions member-variables
	  static-functions static-variables)))



;;; Global key bindings

;; The following can be used to bind key sequences starting with
;; prefix `\C-c\C-m' to browse commands.

(defvar ebrowse-global-map nil
  "*Keymap for Ebrowse commands.")


(defvar ebrowse-global-prefix-key "\C-c\C-m"
  "Prefix key for Ebrowse commands.")


(defvar ebrowse-global-submap-4 nil
  "Keymap used for `ebrowse-global-prefix' followed by `4'.")


(defvar ebrowse-global-submap-5 nil
  "Keymap used for `ebrowse-global-prefix' followed by `5'.")


(unless ebrowse-global-map
  (setq ebrowse-global-map (make-sparse-keymap))
  (setq ebrowse-global-submap-4 (make-sparse-keymap))
  (setq ebrowse-global-submap-5 (make-sparse-keymap))
  (define-key ebrowse-global-map "a" 'ebrowse-tags-apropos)
  (define-key ebrowse-global-map "b" 'ebrowse-pop-to-browser-buffer)
  (define-key ebrowse-global-map "-" 'ebrowse-back-in-position-stack)
  (define-key ebrowse-global-map "+" 'ebrowse-forward-in-position-stack)
  (define-key ebrowse-global-map "l" 'ebrowse-tags-list-members-in-file)
  (define-key ebrowse-global-map "m" 'ebrowse-tags-display-member-buffer)
  (define-key ebrowse-global-map "n" 'ebrowse-tags-next-file)
  (define-key ebrowse-global-map "p" 'ebrowse-electric-position-menu)
  (define-key ebrowse-global-map "s" 'ebrowse-tags-search)
  (define-key ebrowse-global-map "u" 'ebrowse-tags-search-member-use)
  (define-key ebrowse-global-map "v" 'ebrowse-tags-view-definition)
  (define-key ebrowse-global-map "V" 'ebrowse-tags-view-declaration)
  (define-key ebrowse-global-map "%" 'ebrowse-tags-query-replace)
  (define-key ebrowse-global-map "." 'ebrowse-tags-find-definition)
  (define-key ebrowse-global-map "f" 'ebrowse-tags-find-definition)
  (define-key ebrowse-global-map "F" 'ebrowse-tags-find-declaration)
  (define-key ebrowse-global-map "," 'ebrowse-tags-loop-continue)
  (define-key ebrowse-global-map " " 'ebrowse-electric-buffer-list)
  (define-key ebrowse-global-map "\t" 'ebrowse-tags-complete-symbol)
  (define-key ebrowse-global-map "4" ebrowse-global-submap-4)
  (define-key ebrowse-global-submap-4 "." 'ebrowse-tags-find-definition-other-window)
  (define-key ebrowse-global-submap-4 "f" 'ebrowse-tags-find-definition-other-window)
  (define-key ebrowse-global-submap-4 "v" 'ebrowse-tags-find-declaration-other-window)
  (define-key ebrowse-global-submap-4 "F" 'ebrowse-tags-view-definition-other-window)
  (define-key ebrowse-global-submap-4 "V" 'ebrowse-tags-view-declaration-other-window)
  (define-key ebrowse-global-map "5" ebrowse-global-submap-5)
  (define-key ebrowse-global-submap-5 "." 'ebrowse-tags-find-definition-other-frame)
  (define-key ebrowse-global-submap-5 "f" 'ebrowse-tags-find-definition-other-frame)
  (define-key ebrowse-global-submap-5 "v" 'ebrowse-tags-find-declaration-other-frame)
  (define-key ebrowse-global-submap-5 "F" 'ebrowse-tags-view-definition-other-frame)
  (define-key ebrowse-global-submap-5 "V" 'ebrowse-tags-view-declaration-other-frame)
  (define-key global-map ebrowse-global-prefix-key ebrowse-global-map))



;;; Electric C++ browser buffer menu

;; Electric buffer menu customization to display only some buffers
;; (in this case Tree buffers).  There is only one problem with this:
;; If the very first character typed in the buffer menu is a space,
;; this will select the buffer from which the buffer menu was
;; invoked.  But this buffer is not displayed in the buffer list if
;; it isn't a tree buffer.  I therefore let the buffer menu command
;; loop read the command `p' via `unread-command-char'.  This command
;; has no effect since we are on the first line of the buffer.

(defvar electric-buffer-menu-mode-hook nil)


(defun ebrowse-hack-electric-buffer-menu ()
  "Hack the electric buffer menu to display browser buffers."
  (let (non-empty)
    (unwind-protect
	(save-excursion
	  (setq buffer-read-only nil)
	  (goto-char 1)
	  (forward-line 2)
	  (while (not (eobp))
	    (let ((b (Buffer-menu-buffer nil)))
	      (if (or (ebrowse-buffer-p b)
		      (string= (buffer-name b) "*Apropos Members*"))
		  (progn (forward-line 1)
			 (setq non-empty t))
		(delete-region (point)
			       (save-excursion (end-of-line)
					       (min (point-max)
						    (1+ (point)))))))))
      (unless non-empty
	(error "No tree buffers"))
      (setf unread-command-events (listify-key-sequence "p"))
      (shrink-window-if-larger-than-buffer (selected-window))
      (setq buffer-read-only t))))


(defun ebrowse-select-1st-to-9nth ()
  "Select the nth entry in the list by the keys 1..9."
  (interactive)
  (let* ((maxlin (count-lines (point-min) (point-max)))
	 (n (min maxlin (+ 2 (string-to-number (this-command-keys))))))
    (goto-char (point-min))
    (forward-line (1- n))
    (throw 'electric-buffer-menu-select (point))))


(defun ebrowse-install-1-to-9-keys ()
  "Define keys 1..9 to select the 1st to 9nth entry in the list."
  (dotimes (i 9)
    (define-key (current-local-map) (char-to-string (+ i ?1))
      'ebrowse-select-1st-to-9nth)))


(defun ebrowse-electric-buffer-list ()
  "Display an electric list of Ebrowse buffers."
  (interactive)
  (unwind-protect
      (progn
	(add-hook 'electric-buffer-menu-mode-hook
		  'ebrowse-hack-electric-buffer-menu)
	(add-hook 'electric-buffer-menu-mode-hook
		  'ebrowse-install-1-to-9-keys)
	(call-interactively 'electric-buffer-list))
    (remove-hook 'electric-buffer-menu-mode-hook
		 'ebrowse-hack-electric-buffer-menu)))


;;; Mouse support

(defun ebrowse-mouse-find-member (event)
  "Find the member clicked on in another frame.
EVENT is a mouse button event."
  (interactive "e")
  (mouse-set-point event)
  (let (start name)
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_")
      (setq start (point))
      (skip-chars-forward "a-zA-Z0-9_")
      (setq name (buffer-substring start (point))))
    (ebrowse-tags-view/find-member-decl/defn
     5 :view nil :definition t :member-name name)))


(defun ebrowse-popup-menu (menu event)
  "Pop up MENU and perform an action if something was selected.
EVENT is the mouse event."
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let ((selection (x-popup-menu event menu)) binding)
      (while selection
	(setq binding (lookup-key (or binding menu) (vector (car selection)))
	      selection (cdr selection)))
      (when binding
	(call-interactively binding)))))


(easy-menu-define
 ebrowse-tree-buffer-class-object-menu ebrowse-tree-mode-map
 "Object menu for classes in the tree buffer"
 '("Class"
   ["Functions" ebrowse-tree-command:show-member-functions
    :help "Display a list of member functions"
    :active t]
   ["Variables" ebrowse-tree-command:show-member-variables
    :help "Display a list of member variables"
    :active t]
   ["Static Functions" ebrowse-tree-command:show-static-member-functions
    :help "Display a list of static member functions"
    :active t]
   ["Static Variables" ebrowse-tree-command:show-static-member-variables
    :help "Display a list of static member variables"
    :active t]
   ["Friends/ Defines" ebrowse-tree-command:show-friends
    :help "Display a list of friends of a class"
    :active t]
   ["Types" ebrowse-tree-command:show-types
    :help "Display a list of types defined in a class"
    :active t]
   "-----------------"
   ["View" ebrowse-view-class-declaration
    :help "View class declaration"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]
   ["Find" ebrowse-find-class-declaration
    :help "Find class declaration in file"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]
   "-----------------"
   ["Mark" ebrowse-toggle-mark-at-point
    :help "Mark class point is on"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]
   "-----------------"
   ["Collapse" ebrowse-collapse-branch
    :help "Collapse subtree under class point is on"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]
   ["Expand" ebrowse-expand-branch
    :help "Expand subtree under class point is on"
    :active (eq (get-text-property (point) 'ebrowse-what) 'class-name)]))


(easy-menu-define
 ebrowse-tree-buffer-object-menu ebrowse-tree-mode-map
 "Object menu for tree buffers"
 '("Ebrowse"
   ["Filename Display" ebrowse-toggle-file-name-display
    :help "Toggle display of source files names"
    :style toggle
    :selected ebrowse--show-file-names-flag
    :active t]
   ["Tree Indentation" ebrowse-set-tree-indentation
    :help "Set the tree's indentation"
    :active t]
   ["Unmark All Classes" ebrowse-mark-all-classes
    :help "Unmark all classes in the class tree"
    :active t]
   ["Expand All" ebrowse-expand-all
    :help "Expand all subtrees in the class tree"
    :active t]
   ["Statistics" ebrowse-statistics
    :help "Show a buffer with class hierarchy statistics"
    :active t]
   ["Find Class" ebrowse-read-class-name-and-go
    :help "Find a class in the tree"
    :active t]
   ["Member Buffer" ebrowse-pop/switch-to-member-buffer-for-same-tree
    :help "Show a member buffer for this class tree"
    :active t]))


(defun ebrowse-mouse-3-in-tree-buffer (event)
  "Perform mouse actions in tree buffers.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (let* ((where (posn-point (event-start event)))
	 (property (get-text-property where 'ebrowse-what)))
    (case (event-click-count event)
      (1
       (case property
	 (class-name
	  (ebrowse-popup-menu ebrowse-tree-buffer-class-object-menu event))
	 (t
	  (ebrowse-popup-menu ebrowse-tree-buffer-object-menu event)))))))


(defun ebrowse-mouse-2-in-tree-buffer (event)
  "Perform mouse actions in tree buffers.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (let* ((where (posn-point (event-start event)))
	 (property (get-text-property where 'ebrowse-what)))
    (case (event-click-count event)
      (1 (case property
	   (class-name
	    (ebrowse-tree-command:show-member-functions)))))))


(defun ebrowse-mouse-1-in-tree-buffer (event)
  "Perform mouse actions in tree buffers.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (let* ((where (posn-point (event-start event)))
	 (property (get-text-property where 'ebrowse-what)))
    (case (event-click-count event)
      (2 (case property
	   (class-name
	    (let ((collapsed (save-excursion (skip-chars-forward "^\r\n")
					     (looking-at "\r"))))
	      (ebrowse-collapse-fn (not collapsed))))
	   (mark
	    (ebrowse-toggle-mark-at-point 1)))))))



(provide 'ebrowse)

;; Local variables:
;; eval:(put 'ebrowse-output 'lisp-indent-hook 0)
;; eval:(put 'ebrowse-ignoring-completion-case 'lisp-indent-hook 0)
;; eval:(put 'ebrowse-save-selective 'lisp-indent-hook 0)
;; eval:(put 'ebrowse-for-all-trees 'lisp-indent-hook 1)
;; End:

;;; ebrowse.el ends here
