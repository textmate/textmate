;;; semantic/imenu.el --- Use Semantic as an imenu tag generator

;;; Copyright (C) 2000-2005, 2007-2008, 2010-2012
;;   Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Maintainer: Eric Ludlam

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
;; This support function can be used in any buffer which supports
;; the bovinator to create the imenu index.
;;
;; To use this in a buffer, do this in a hook.
;;
;; (add-hook 'mode-hook
;;           (lambda ()
;;             (setq imenu-create-index-function 'semantic-create-imenu-index)
;;             ))

(require 'semantic)
(require 'semantic/format)
(require 'semantic/db)
(require 'semantic/db-file)
(require 'semantic/sort)
(require 'imenu)

(declare-function pulse-momentary-highlight-one-line "pulse" (o &optional face))
(declare-function semanticdb-semantic-init-hook-fcn "db-mode")

;; Because semantic imenu tags will hose the current imenu handling
;; code in speedbar, force semantic/sb in.
(if (featurep 'speedbar)
    (require 'semantic/sb)
  (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb))))

(defgroup semantic-imenu nil
  "Semantic interface to Imenu."
  :group 'semantic
  :group 'imenu
  )

;;;###autoload
(defcustom semantic-imenu-summary-function 'semantic-format-tag-abbreviate
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'."
  :group 'semantic-imenu
  :type semantic-format-tag-custom-list)
(make-variable-buffer-local 'semantic-imenu-summary-function)

;;;###autoload
(defcustom semantic-imenu-bucketize-file t
  "*Non-nil if tags in a file are to be grouped into buckets."
  :group 'semantic-imenu
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-bucketize-file)

(defcustom semantic-imenu-adopt-external-members t
  "*Non-nil if types in a file should adopt externally defined members.
C++ and CLOS can define methods that are not in the body of a class
definition."
  :group 'semantic-imenu
  :type 'boolean)

(defcustom semantic-imenu-buckets-to-submenu t
  "*Non-nil if buckets of tags are to be turned into submenus.
This option is ignored if `semantic-imenu-bucketize-file' is nil."
  :group 'semantic-imenu
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-buckets-to-submenu)

;;;###autoload
(defcustom semantic-imenu-expand-type-members t
  "*Non-nil if types should have submenus with members in them."
  :group 'semantic-imenu
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-expand-type-members)
(semantic-varalias-obsolete 'semantic-imenu-expand-type-parts
                            'semantic-imenu-expand-type-members "23.2")

(defcustom semantic-imenu-bucketize-type-members t
  "*Non-nil if members of a type should be grouped into buckets.
A nil value means to keep them in the same order.
Overridden to nil if `semantic-imenu-bucketize-file' is nil."
  :group 'semantic-imenu
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-bucketize-type-members)
(semantic-varalias-obsolete 'semantic-imenu-bucketize-type-parts
                            'semantic-imenu-bucketize-type-members "23.2")

(defcustom semantic-imenu-sort-bucket-function nil
  "*Function to use when sorting tags in the buckets of functions.
See `semantic-bucketize' and the FILTER argument for more details on this function."
  :group 'semantic-imenu
  :type '(radio (const :tag "No Sorting" nil)
		(const semantic-sort-tags-by-name-increasing)
		(const semantic-sort-tags-by-name-decreasing)
		(const semantic-sort-tags-by-type-increasing)
		(const semantic-sort-tags-by-type-decreasing)
		(const semantic-sort-tags-by-name-increasing-ci)
		(const semantic-sort-tags-by-name-decreasing-ci)
		(const semantic-sort-tags-by-type-increasing-ci)
		(const semantic-sort-tags-by-type-decreasing-ci)
		(function)))
(make-variable-buffer-local 'semantic-imenu-sort-bucket-function)

(defcustom semantic-imenu-index-directory nil
  "*Non nil to index the entire directory for tags.
Doesn't actually parse the entire directory, but displays tags for all files
currently listed in the current Semantic database.
This variable has no meaning if semanticdb is not active."
  :group 'semantic-imenu
  :type 'boolean)

(defcustom semantic-imenu-auto-rebuild-directory-indexes nil
  "*If non-nil automatically rebuild directory index imenus.
That is when a directory index imenu is updated, automatically rebuild
other buffer local ones based on the same semanticdb."
  :group 'semantic-imenu
  :type 'boolean)

(defvar semantic-imenu-directory-current-file nil
  "When building a file index, this is the file name currently being built.")

(defvar semantic-imenu-auto-rebuild-running nil
  "Non-nil if `semantic-imenu-rebuild-directory-indexes' is running.")

;;;###autoload
(defvar semantic-imenu-expandable-tag-classes '(type)
  "List of expandable tag classes.
Tags of those classes will be given submenu with children.
By default, a `type' has interesting children.  In Texinfo, however, a
`section' has interesting children.")
(make-variable-buffer-local 'semantic-imenu-expandable-tag-classes)
(semantic-varalias-obsolete 'semantic-imenu-expandable-token
                            'semantic-imenu-expandable-tag-classes "23.2")

;;; Code:
(defun semantic-imenu-tag-overlay (tag)
  "Return the overlay belonging to tag.
If TAG doesn't have an overlay, and instead as a vector of positions,
concoct a combination of file name, and position."
  (let ((o (semantic-tag-overlay tag)))
    (if (not (semantic-overlay-p o))
	(let ((v (make-vector 3 nil)))
	  (aset v 0 semantic-imenu-directory-current-file)
	  (aset v 1 (aref o 0))
	  (aset v 2 (aref o 1))
	  v)
      o)))


(defun semantic-imenu-goto-function (name position &optional rest)
  "Move point associated with NAME to POSITION.
Used to override function `imenu-default-goto-function' so that
we can continue to use overlays to maintain the current position.
Optional argument REST is some extra stuff."
  (require 'pulse)
  (if (semantic-overlay-p position)
      (let ((os (semantic-overlay-start position))
	    (ob (semantic-overlay-buffer position)))
	(if os
	    (progn
	      (if (not (eq ob (current-buffer)))
		  (switch-to-buffer ob))
	      (imenu-default-goto-function name os rest)
	      (pulse-momentary-highlight-one-line (point))
	      )
	  ;; This should never happen, but check anyway.
	  (message "Imenu is out of date, try again. (internal bug)")
	  (setq imenu--index-alist nil)))
    ;; When the POSITION is actually a pair of numbers in an array, then
    ;; the file isn't loaded into the current buffer.
    (if (vectorp position)
	(let ((file (aref position 0))
	      (pos (aref position 1)))
	  (and file (find-file file))
	  (imenu-default-goto-function name pos rest)
	  (pulse-momentary-highlight-one-line (point))
	  )
      ;; When the POSITION is the symbol 'file-only' it means that this
      ;; is a directory index entry and there is no tags in this
      ;; file. So just jump to the beginning of the file.
      (if (eq position 'file-only)
	  (progn
	    (find-file name)
	    (imenu-default-goto-function name (point-min) rest)
	    (pulse-momentary-highlight-one-line (point))
	    )
        ;; Probably POSITION don't came from a semantic imenu.  Try
        ;; the default imenu goto function.
        (condition-case nil
	    (progn
	      (imenu-default-goto-function name position rest)
	      (pulse-momentary-highlight-one-line (point))
	      )
          (error
           (message "Semantic Imenu override problem. (Internal bug)")
           (setq imenu--index-alist nil)))))
    ))

(defun semantic-imenu-flush-fcn (&optional ignore)
  "This function is called as a hook to clear the imenu cache.
It is cleared after any parsing.
IGNORE arguments."
  (if (eq imenu-create-index-function 'semantic-create-imenu-index)
      (setq imenu--index-alist nil
            imenu-menubar-modified-tick 0))
  (remove-hook 'semantic-after-toplevel-cache-change-hook
               'semantic-imenu-flush-fcn t)
  (remove-hook 'semantic-after-partial-cache-change-hook
               'semantic-imenu-flush-fcn t)
  )

;;;###autoload
(defun semantic-create-imenu-index (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic parser to create the index.
Optional argument STREAM is an optional stream of tags used to create menus."
  (setq imenu-default-goto-function 'semantic-imenu-goto-function)
  (prog1
      (if (and semantic-imenu-index-directory
               (featurep 'semantic/db)
               (semanticdb-minor-mode-p))
          (semantic-create-imenu-directory-index
	   (or stream (semantic-fetch-tags-fast)))
        (semantic-create-imenu-index-1
	 (or stream (semantic-fetch-tags-fast)) nil))
    (semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
    (add-hook 'semantic-after-toplevel-cache-change-hook
              'semantic-imenu-flush-fcn nil t)
    (semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
    (add-hook 'semantic-after-partial-cache-change-hook
              'semantic-imenu-flush-fcn nil t)))

(defun semantic-create-imenu-directory-index (&optional stream)
  "Create an imenu tag index based on all files active in semanticdb.
Optional argument STREAM is the stream of tags for the current buffer."
  (if (not semanticdb-current-database)
      (semantic-create-imenu-index-1 stream nil)
    ;; We have a database, list all files, with the current file on top.
    (let ((index (list
		  (cons (oref semanticdb-current-table file)
			(or (semantic-create-imenu-index-1 stream nil)
			    ;; No tags in this file
			    'file-only))))
	  (tables (semanticdb-get-database-tables semanticdb-current-database)))
	(while tables
	  (let ((semantic-imenu-directory-current-file
		 (oref (car tables) file))
		tags)
	    (when (and (not (eq (car tables) semanticdb-current-table))
		       (semanticdb-live-p (car tables))
		       (semanticdb-equivalent-mode (car tables))
		       )
	      (setq tags (oref (car tables) tags)
		    index (cons (cons semantic-imenu-directory-current-file
				      (or (and tags
					       ;; don't pass nil stream because
					       ;; it will use the current
					       ;; buffer
					       (semantic-create-imenu-index-1
						(oref (car tables) tags)
						nil))
					  ;; no tags in the file
					  'file-only))
				index)))
	    (setq tables (cdr tables))))

      ;; If enabled automatically rebuild other imenu directory
      ;; indexes based on the same Semantic database
      (or (not semantic-imenu-auto-rebuild-directory-indexes)
          ;; If auto rebuild already in progress does nothing
          semantic-imenu-auto-rebuild-running
          (unwind-protect
              (progn
                (setq semantic-imenu-auto-rebuild-running t)
                (semantic-imenu-rebuild-directory-indexes
                 semanticdb-current-database))
            (setq semantic-imenu-auto-rebuild-running nil)))

      (nreverse index))))

(defun semantic-create-imenu-index-1 (stream &optional parent)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic parser to create the index.
STREAM is a stream of tags used to create menus.
Optional argument PARENT is a tag parent of STREAM."
  (let ((tags stream)
	(semantic-imenu-adopt-external-members
	 semantic-imenu-adopt-external-members))
    ;; If we should regroup, do so.
    (if semantic-imenu-adopt-external-members
 	(setq tags (semantic-adopt-external-members tags)
	      ;; Don't allow recursion here.
	      semantic-imenu-adopt-external-members nil))
    ;; Test for bucketing vs not.
    (if semantic-imenu-bucketize-file
	(let ((buckets (semantic-bucketize
			tags parent
			semantic-imenu-sort-bucket-function))
	      item name
	      index)
	  (cond
	   ((null buckets)
	    nil)
	   ((or (cdr-safe buckets) ;; if buckets has more than one item in it.
                (not semantic-imenu-buckets-to-submenu)) ;; to force separators between buckets
	    (while buckets
	      (setq name (car (car buckets))
		    item (cdr (car buckets)))
	      (if semantic-imenu-buckets-to-submenu
		  (progn
		    ;; Make submenus
		    (if item
			(setq index
			      (cons (cons name
					  (semantic-create-imenu-subindex item))
				    index))))
		;; Glom everything together with "---" between
		(if item
		    (setq index
			  (append index
				  ;; do not create a menu separator in the parent menu
				  ;; when creating a sub-menu
				  (if (memq (semantic-tag-class (car item))
                                            semantic-imenu-expandable-tag-classes)
				      (semantic-create-imenu-subindex item)
				    (cons
				     '("---")
				     (semantic-create-imenu-subindex item)))))
		  ))
	      (setq buckets (cdr buckets)))
	    (if semantic-imenu-buckets-to-submenu
		(nreverse index)
	      index))
	   (t
	    (setq name (car (car buckets))
		  item (cdr (car buckets)))
	    (semantic-create-imenu-subindex item))))
      ;; Else, group everything together
      (semantic-create-imenu-subindex tags))))

(defun semantic-create-imenu-subindex (tags)
  "From TAGS, create an imenu index of interesting things."
  (let ((notypecheck (not semantic-imenu-expand-type-members))
	children index tag parts)
    (while tags
      (setq tag (car tags)
	    children (semantic-tag-components-with-overlays tag))
      (if (and (not notypecheck)
               (memq (semantic-tag-class tag)
                     semantic-imenu-expandable-tag-classes)
	       children
               )
          ;; to keep an homogeneous menu organization, type menu items
          ;; always have a sub-menu with at least the *definition*
          ;; item (even if the tag has no type components)
	  (progn
	    (setq parts children)
	    ;; There is options which create the submenu
	    ;;  * Type has an overlay, but children do.
	    ;; The type doesn't have to have it's own overlay,
	    ;; but a type with no overlay and no children should be
	    ;; invalid.
	    (setq index
		  (cons
		   (cons
		    (funcall semantic-imenu-summary-function tag)
		    ;; Add a menu for getting at the type definitions
		    (if (and parts
			     ;; Note to self: enable menu items for
			     ;; sub parts even if they are not proper
			     ;; tags.
			     (semantic-tag-p (car parts)))
			(let ((submenu
			       (if (and semantic-imenu-bucketize-type-members
					semantic-imenu-bucketize-file)
				   (semantic-create-imenu-index-1 parts tag)
				 (semantic-create-imenu-subindex parts))))
			  ;; Only add a *definition* if we have a position
			  ;; in that type tag.
			  (if (semantic-tag-with-position-p tag)
			      (cons
			       (cons "*definition*"
				     (semantic-imenu-tag-overlay tag))
			       submenu)
			    submenu))
		      ;; There were no parts, or something like that, so
		      ;; instead just put the definition here.
		      (if (semantic-tag-with-position-p tag)
			  (semantic-imenu-tag-overlay tag)
			nil)
		      ))
		   index)))
	(if (semantic-tag-with-position-p tag)
	    (setq index (cons
			 (cons
			  (funcall semantic-imenu-summary-function tag)
			  (semantic-imenu-tag-overlay tag))
			 index))))
      (setq tags (cdr tags)))
    ;; `imenu--split-submenus' sort submenus according to
    ;; `imenu-sort-function' setting and split them up if they are
    ;; longer than `imenu-max-items'.
    (imenu--split-submenus (nreverse index))))

;;; directory imenu rebuilding.
;;
(defun semantic-imenu-rebuild-directory-indexes (db)
  "Rebuild directory index imenus based on Semantic database DB."
  (let ((l (buffer-list))
        b)
    (while l
      (setq b (car l)
            l (cdr l))
      (if (and (not (eq b (current-buffer)))
               (buffer-live-p b))
          (with-current-buffer b
            ;; If there is a buffer local Semantic index directory
            ;; imenu
            (when (and (eq imenu-create-index-function
                           'semantic-create-imenu-index)
                       semanticdb-current-database
                       (eq semanticdb-current-database db))
              ;; Rebuild the imenu
              (imenu--cleanup)
              (setq imenu--index-alist nil)
              (funcall
               (if (fboundp 'imenu-menu-filter)
                   ;; XEmacs imenu
                   'imenu-menu-filter
                 ;; Emacs imenu
                 'imenu-update-menubar))))))))

(defun semantic-imenu-semanticdb-hook ()
  "Function to be called from `semanticdb-mode-hook'.
Clears all imenu menus that may be depending on the database."
  (require 'semantic/db-mode)
  (semantic-map-buffers
   #'(lambda ()
       ;; Set up semanticdb environment if enabled.
       (if (semanticdb-minor-mode-p)
           (semanticdb-semantic-init-hook-fcn))
       ;; Clear imenu cache to redraw the imenu.
       (semantic-imenu-flush-fcn))))

(add-hook 'semanticdb-mode-hook 'semantic-imenu-semanticdb-hook)

;;; Interactive Utilities
;;
(defun semantic-imenu-toggle-bucketize-file ()
  "Toggle the ability of imenu to bucketize the current file."
  (interactive)
  (setq semantic-imenu-bucketize-file (not semantic-imenu-bucketize-file))
  ;; Force a rescan
  (setq imenu--index-alist nil))

(defun semantic-imenu-toggle-buckets-to-submenu ()
  "Toggle the ability of imenu to turn buckets into submenus."
  (interactive)
  (setq semantic-imenu-buckets-to-submenu (not semantic-imenu-buckets-to-submenu))
  ;; Force a rescan
  (setq imenu--index-alist nil))

(defun semantic-imenu-toggle-bucketize-type-parts ()
  "Toggle the ability of imenu to bucketize the current file."
  (interactive)
  (setq semantic-imenu-bucketize-type-members (not semantic-imenu-bucketize-type-members))
  ;; Force a rescan
  (setq imenu--index-alist nil))

;;; Which function support
;;
;; The which-function library will display the current function in the
;; mode line.  It tries do do this through imenu.  With a semantic parsed
;; buffer, there is a much more efficient way of doing this.
;; Advise `which-function' so that we optionally use semantic tags
;; instead, and get better stuff.
(require 'advice)

(defvar semantic-which-function 'semantic-default-which-function
  "Function to convert semantic tags into `which-function' text.")

(defcustom semantic-which-function-use-color nil
  "*Use color when displaying the current function with `which-function'."
  :group 'semantic-imenu
  :type 'boolean)

(defun semantic-default-which-function (taglist)
  "Convert TAGLIST into a string usable by `which-function'.
Returns the first tag name in the list, unless it is a type,
in which case it concatenates them together."
  (cond ((eq (length taglist) 1)
	 (semantic-format-tag-abbreviate
          (car taglist) nil semantic-which-function-use-color))
	((memq (semantic-tag-class (car taglist))
               semantic-imenu-expandable-tag-classes)
	 (concat (semantic-format-tag-name
                  (car taglist) nil semantic-which-function-use-color)
		 (car semantic-type-relation-separator-character)
		 ;; recurse until we no longer have a type
		 ;; or any tags left.
		 (semantic-default-which-function (cdr taglist))))
	(t (semantic-format-tag-abbreviate
            (car taglist) nil semantic-which-function-use-color))))

;; (defadvice which-function (around semantic-which activate)
;;   "Choose the function to display via semantic if it is currently active."
;;   (if (and (featurep 'semantic) semantic--buffer-cache)
;;       (let ((ol (semantic-find-tag-by-overlay)))
;; 	(setq ad-return-value (funcall semantic-which-function ol)))
;;     ad-do-it))

(provide 'semantic/imenu)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/imenu"
;; End:

;;; semantic/imenu.el ends here
