;;; semantic/decorate/include.el --- Decoration modes for include statements

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Highlight any include that is in a state the user may care about.
;; The basic idea is to have the state be highly visible so users will
;; as 'what is this?" and get the info they need to fix problems that
;; are otherwise transparent when trying to get smart completion
;; working.

(require 'semantic/decorate/mode)
(require 'semantic/db)
(require 'semantic/db-ref)
(require 'semantic/db-find)

(eval-when-compile
  (require 'semantic/find))

(defvar semantic-dependency-system-include-path)
(declare-function ede-get-locator-object "ede/files")
(declare-function ede-system-include-path "ede/cpp-root")

;;; Code:

;;; FACES AND KEYMAPS
(defvar semantic-decoratiton-mouse-3 (if (featurep 'xemacs) [ button3 ] [ mouse-3 ])
  "The keybinding lisp object to use for binding the right mouse button.")

;;; Includes that are in a happy state!
;;
(defface semantic-decoration-on-includes
  nil
  "*Overlay Face used on includes that are not in some other state.
Used by the decoration style: `semantic-decoration-on-includes'."
  :group 'semantic-faces)

(defvar semantic-decoration-on-include-map
  (let ((km (make-sparse-keymap)))
    (define-key km semantic-decoratiton-mouse-3 'semantic-decoration-include-menu)
    km)
  "Keymap used on includes.")


(defvar semantic-decoration-on-include-menu nil
  "Menu used for include headers.")

(easy-menu-define
  semantic-decoration-on-include-menu
  semantic-decoration-on-include-map
  "Include Menu"
  (list
   "Include"
   (semantic-menu-item
    ["What Is This?" semantic-decoration-include-describe
     :active t
     :help "Describe why this include has been marked this way." ])
   (semantic-menu-item
    ["Visit This Include" semantic-decoration-include-visit
     :active t
     :help "Visit this include file." ])
   "---"
   (semantic-menu-item
    ["Summarize includes current buffer" semantic-decoration-all-include-summary
     :active t
     :help "Show a summary for the current buffer containing this include." ])
   (semantic-menu-item
    ["List found includes (load unparsed)" semanticdb-find-test-translate-path
     :active t
     :help "List all includes found for this file, and parse unparsed files." ])
   (semantic-menu-item
    ["List found includes (no loading)" semanticdb-find-test-translate-path-no-loading
     :active t
     :help "List all includes found for this file, do not parse unparsed files." ])
   (semantic-menu-item
    ["List all unknown includes" semanticdb-find-adebug-lost-includes
     :active t
     :help "Show a list of all includes semantic cannot find for this file." ])
   "---"
   (semantic-menu-item
    ["Customize System Include Path" semantic-customize-system-include-path
     :active (get 'semantic-dependency-system-include-path major-mode)
     :help "Run customize for the system include path for this major mode." ])
   (semantic-menu-item
    ["Add a System Include Path" semantic-add-system-include
     :active t
     :help "Add an include path for this session." ])
   (semantic-menu-item
    ["Remove a System Include Path" semantic-remove-system-include
     :active t
     :help "Add an include path for this session." ])
   ;;["" semantic-decoration-include-
   ;; :active t
   ;; :help "" ]
   ))

;;; Unknown Includes!
;;
(defface semantic-decoration-on-unknown-includes
  '((((class color) (background dark))
     (:background "#900000"))
    (((class color) (background light))
     (:background "#fff0f0")))
  "*Face used to show includes that cannot be found.
Used by the decoration style: `semantic-decoration-on-unknown-includes'."
  :group 'semantic-faces)

(defvar semantic-decoration-on-unknown-include-map
  (let ((km (make-sparse-keymap)))
    ;(define-key km [ mouse-2 ] 'semantic-decoration-unknown-include-describe)
    (define-key km semantic-decoratiton-mouse-3 'semantic-decoration-unknown-include-menu)
    km)
  "Keymap used on unparsed includes.")

(defvar semantic-decoration-on-unknown-include-menu nil
  "Menu used for unparsed include headers.")

(easy-menu-define
  semantic-decoration-on-unknown-include-menu
  semantic-decoration-on-unknown-include-map
  "Unknown Include Menu"
  (list
   "Unknown Include"
   (semantic-menu-item
    ["What Is This?" semantic-decoration-unknown-include-describe
     :active t
     :help "Describe why this include has been marked this way." ])
   (semantic-menu-item
    ["List all unknown includes" semanticdb-find-adebug-lost-includes
     :active t
     :help "Show a list of all includes semantic cannot find for this file." ])
   "---"
   (semantic-menu-item
    ["Summarize includes current buffer" semantic-decoration-all-include-summary
     :active t
     :help "Show a summary for the current buffer containing this include." ])
   (semantic-menu-item
    ["List found includes (load unparsed)" semanticdb-find-test-translate-path
     :active t
     :help "List all includes found for this file, and parse unparsed files." ])
   (semantic-menu-item
    ["List found includes (no loading)" semanticdb-find-test-translate-path-no-loading
     :active t
     :help "List all includes found for this file, do not parse unparsed files." ])
   "---"
   (semantic-menu-item
    ["Customize System Include Path" semantic-customize-system-include-path
     :active (get 'semantic-dependency-system-include-path major-mode)
     :help "Run customize for the system include path for this major mode." ])
   (semantic-menu-item
    ["Add a System Include Path" semantic-add-system-include
     :active t
     :help "Add an include path for this session." ])
   (semantic-menu-item
    ["Remove a System Include Path" semantic-remove-system-include
     :active t
     :help "Add an include path for this session." ])
   ))

;;; Includes that need to be parsed.
;;
(defface semantic-decoration-on-unparsed-includes
  '((((class color) (background dark))
     (:background "#555500"))
    (((class color) (background light))
     (:background "#ffff55")))
  "*Face used to show includes that have not yet been parsed.
Used by the decoration style: `semantic-decoration-on-unparsed-includes'."
  :group 'semantic-faces)

(defvar semantic-decoration-on-unparsed-include-map
  (let ((km (make-sparse-keymap)))
    (define-key km semantic-decoratiton-mouse-3 'semantic-decoration-unparsed-include-menu)
    km)
  "Keymap used on unparsed includes.")


(defvar semantic-decoration-on-unparsed-include-menu nil
  "Menu used for unparsed include headers.")

(easy-menu-define
  semantic-decoration-on-unparsed-include-menu
  semantic-decoration-on-unparsed-include-map
  "Unparsed Include Menu"
  (list
   "Unparsed Include"
   (semantic-menu-item
    ["What Is This?" semantic-decoration-unparsed-include-describe
     :active t
     :help "Describe why this include has been marked this way." ])
   (semantic-menu-item
    ["Visit This Include" semantic-decoration-include-visit
     :active t
     :help "Visit this include file so that header file's tags can be used." ])
   (semantic-menu-item
    ["Parse This Include" semantic-decoration-unparsed-include-parse-include
     :active t
     :help "Parse this include file so that header file's tags can be used." ])
   (semantic-menu-item
    ["Parse All Includes" semantic-decoration-unparsed-include-parse-all-includes
     :active t
     :help "Parse all the includes so the contents can be used." ])
   "---"
   (semantic-menu-item
    ["Summarize includes current buffer" semantic-decoration-all-include-summary
     :active t
     :help "Show a summary for the current buffer containing this include." ])
   (semantic-menu-item
    ["List found includes (load unparsed)" semanticdb-find-test-translate-path
     :active t
     :help "List all includes found for this file, and parse unparsed files." ])
   (semantic-menu-item
    ["List found includes (no loading)" semanticdb-find-test-translate-path-no-loading
     :active t
     :help "List all includes found for this file, do not parse unparsed files." ])
   (semantic-menu-item
    ["List all unknown includes" semanticdb-find-adebug-lost-includes
     :active t
     :help "Show a list of all includes semantic cannot find for this file." ])
   "---"
   (semantic-menu-item
    ["Customize System Include Path" semantic-customize-system-include-path
     :active (get 'semantic-dependency-system-include-path major-mode)
     :help "Run customize for the system include path for this major mode." ])
   (semantic-menu-item
    ["Add a System Include Path" semantic-add-system-include
     :active t
     :help "Add an include path for this session." ])
   (semantic-menu-item
    ["Remove a System Include Path" semantic-remove-system-include
     :active t
     :help "Add an include path for this session." ])
   ;;["" semantic-decoration-unparsed-include-
   ;; :active t
   ;; :help "" ]
   ))


;;; MODES

;;; Include statement Decorate Mode
;;
;; This mode handles the three states of an include statements
;;
(define-semantic-decoration-style semantic-decoration-on-includes
  "Highlight class members that are includes.
This mode provides a nice context menu on the include statements."
  :enabled t)

(defun semantic-decoration-on-includes-p-default (tag)
  "Return non-nil if TAG has is an includes that can't be found."
  (semantic-tag-of-class-p tag 'include))

(defun semantic-decoration-on-includes-highlight-default (tag)
  "Highlight the include TAG to show that semantic can't find it."
  (let* ((file (semantic-dependency-tag-file tag))
	 (table (when file
		  (semanticdb-file-table-object file t)))
	 (face nil)
	 (map nil)
	 )
    (cond
     ((not file)
      ;; Cannot find this header.
      (setq face 'semantic-decoration-on-unknown-includes
	    map semantic-decoration-on-unknown-include-map)
      )
     ((and table (number-or-marker-p (oref table pointmax)))
      ;; A found and parsed file.
      (setq face 'semantic-decoration-on-includes
	    map semantic-decoration-on-include-map)
      )
     (t
      ;; An unparsed file.
      (setq face 'semantic-decoration-on-unparsed-includes
	    map semantic-decoration-on-unparsed-include-map)
      (when table
	;; Set ourselves up for synchronization
	(semanticdb-cache-get
	 table 'semantic-decoration-unparsed-include-cache)
	;; Add a dependency.
	(let ((table semanticdb-current-table))
	  (semanticdb-add-reference table tag))
	)
      ))

    ;; @TODO - if not a tag w/ a position, we need to get one.  How?

    (when (semantic-tag-with-position-p tag)
      (let ((ol (semantic-decorate-tag tag
				       (semantic-tag-start tag)
				       (semantic-tag-end tag)
				       face))
	    )
	(semantic-overlay-put ol 'mouse-face 'highlight)
	(semantic-overlay-put ol 'keymap map)
	(semantic-overlay-put ol 'help-echo
			      "Header File : mouse-3 - Context menu")
	))))

;;; Regular Include Functions
;;
(defun semantic-decoration-include-describe ()
  "Describe what unparsed includes are in the current buffer.
Argument EVENT is the mouse clicked event."
  (interactive)
  (let* ((tag (or (semantic-current-tag)
		  (error "No tag under point")))
	 (file (semantic-dependency-tag-file tag))
	 (table (when file
		  (semanticdb-file-table-object file t))))
    (with-output-to-temp-buffer (help-buffer) ; "*Help*"
      (help-setup-xref (list #'semantic-decoration-include-describe)
		       (called-interactively-p 'interactive))
      (princ "Include File: ")
      (princ (semantic-format-tag-name tag nil t))
      (princ "\n")
      (princ "This include file was found at:\n  ")
      (princ (semantic-dependency-tag-file tag))
      (princ "\n\n")
      (princ "Semantic knows where this include file is, and has parsed
its contents.

")
      (let ((inc (semantic-find-tags-by-class 'include table))
	    (ok 0)
	    (unknown 0)
	    (unparsed 0)
	    (all 0))
	(dolist (i inc)
	  (let* ((fileinner (semantic-dependency-tag-file i))
		 )
	    (cond ((not fileinner)
		   (setq unknown (1+ unknown)))
		  ((number-or-marker-p (oref table pointmax))
		   (setq ok (1+ ok)))
		  (t
		   (setq unparsed (1+ unparsed))))))
	(setq all (+ ok unknown unparsed))
	(if (= 0 all)
	    (princ "There are no other includes in this file.\n")
	  (princ (format "There are %d more includes in this file.\n"
			 all))
	  (princ (format "   Unknown Includes:  %d\n" unknown))
	  (princ (format "   Unparsed Includes: %d\n" unparsed))
	  (princ (format "   Parsed Includes:   %d\n" ok)))
	)
      ;; Get the semanticdb statement, and display it's contents.
      (princ "\nDetails for header file...\n")
      (princ "\nMajor Mode:          ")
      (princ (oref table :major-mode))
      (princ "\nTags:                ")
      (princ (format "%s entries" (length (oref table :tags))))
      (princ "\nFile Size:           ")
      (princ (format "%s chars" (oref table :pointmax)))
      (princ "\nSave State:          ")
      (cond ((oref table dirty)
	     (princ "Table needs to be saved."))
	    (t
	     (princ "Table is saved on disk."))
	    )
      (princ "\nExternal References:")
      (dolist (r (oref table db-refs))
	(princ "\n    ")
	(princ (oref r file)))
      )))

;;;###autoload
(defun semantic-decoration-include-visit ()
  "Visit the included file at point."
  (interactive)
  (let ((tag  (semantic-current-tag)))
    (unless (eq (semantic-tag-class tag) 'include)
      (error "Point is not on an include tag"))
    (let ((file (semantic-dependency-tag-file tag)))
      (cond
       ((or (not file) (not (file-exists-p file)))
	(error "Could not location include %s"
	       (semantic-tag-name tag)))
       ((get-file-buffer file)
	(switch-to-buffer (get-file-buffer file)))
       ((stringp file)
	(find-file file))
       ))))

(defun semantic-decoration-include-menu (event)
  "Popup a menu that can help a user understand unparsed includes.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (semantic-event-window event))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (sit-for 0)
      (semantic-popup-menu semantic-decoration-on-include-menu)
      )
    (select-window startwin)))


;;; Unknown Include functions
;;
(defun semantic-decoration-unknown-include-describe ()
  "Describe what unknown includes are in the current buffer.
Argument EVENT is the mouse clicked event."
  (interactive)
  (let ((tag (semantic-current-tag))
	(mm major-mode))
    (with-output-to-temp-buffer (help-buffer) ; "*Help*"
      (help-setup-xref (list #'semantic-decoration-unknown-include-describe)
		       (called-interactively-p 'interactive))
      (princ "Include File: ")
      (princ (semantic-format-tag-name tag nil t))
      (princ "\n\n")
      (princ "This header file has been marked \"Unknown\".
This means that Semantic has not been able to locate this file on disk.

When Semantic cannot find an include file, this means that the
idle summary mode and idle completion modes cannot use the contents of
that file to provide coding assistance.

If this is a system header and you want it excluded from Semantic's
searches (which may be desirable for speed reasons) then you can
safely ignore this state.

If this is a system header, and you want to include it in Semantic's
searches, then you will need to use:

M-x semantic-add-system-include RET /path/to/includes RET

or, in your .emacs file do:

  (semantic-add-system-include \"/path/to/include\" '")
      (princ (symbol-name mm))
      (princ ")

to add the path to Semantic's search.

If this is an include file that belongs to your project, then you may
need to update `semanticdb-project-roots' or better yet, use `ede'
to manage your project.  See the ede manual for projects that will
wrap existing project code for Semantic's benefit.
")

      (when (or (eq mm 'c++-mode) (eq mm 'c-mode))
	(princ "
For C/C++ includes located within a project, you can use a special
EDE project that will wrap an existing build system.  You can do that
like this in your .emacs file:

  (ede-cpp-root-project \"NAME\" :file \"FILENAME\" :locate-fcn 'MYFCN)

See the CEDET manual, the EDE manual, or the commentary in
ede/cpp-root.el for more.

If you think this header tag is marked in error, you may need to do:

C-u M-x bovinate RET

to refresh the tags in this buffer, and recalculate the state."))

      (princ "
See the Semantic manual node on SemanticDB for more about search paths.")
      )))

(defun semantic-decoration-unknown-include-menu (event)
  "Popup a menu that can help a user understand unparsed includes.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 ;; This line has an issue in XEmacs.
	 (win (semantic-event-window event))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (sit-for 0)
      (semantic-popup-menu semantic-decoration-on-unknown-include-menu)
      )
    (select-window startwin)))


;;; Interactive parts of unparsed includes
;;
(defun semantic-decoration-unparsed-include-describe ()
  "Describe what unparsed includes are in the current buffer.
Argument EVENT is the mouse clicked event."
  (interactive)
  (let ((tag (semantic-current-tag)))
    (with-output-to-temp-buffer (help-buffer); "*Help*"
      (help-setup-xref (list #'semantic-decoration-unparsed-include-describe)
		       (called-interactively-p 'interactive))

      (princ "Include File: ")
      (princ (semantic-format-tag-name tag nil t))
      (princ "\n")
      (princ "This include file was found at:\n  ")
      (princ (semantic-dependency-tag-file tag))
      (princ "\n\n")
      (princ "This header file has been marked \"Unparsed\".
This means that Semantic has located this header file on disk
but has not yet opened and parsed this file.

So long as this header file is unparsed, idle summary and
idle completion will not be able to reference the details in this
header.

To resolve this, use the context menu to parse this include file,
or all include files referred to in ")
      (princ (buffer-name))
      (princ ".
This can take a while in large projects.

Alternately, you can call:

M-x semanticdb-find-test-translate-path RET

to search path Semantic uses to perform completion.


If you think this header tag is marked in error, you may need to do:

C-u M-x bovinate RET

to refresh the tags in this buffer, and recalculate the state.
If you find a repeatable case where a header is marked in error,
report it to cedet-devel@lists.sf.net.") )))


(defun semantic-decoration-unparsed-include-menu (event)
  "Popup a menu that can help a user understand unparsed includes.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (semantic-event-window event))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (sit-for 0)
      (semantic-popup-menu semantic-decoration-on-unparsed-include-menu)
      )
    (select-window startwin)))

(defun semantic-decoration-unparsed-include-parse-include ()
  "Parse the include file the user menu-selected from."
  (interactive)
  (let* ((file (semantic-dependency-tag-file (semantic-current-tag))))
    (semanticdb-file-table-object file)
    (semantic-decoration-unparsed-include-do-reset)))


(defun semantic-decoration-unparsed-include-parse-all-includes ()
  "Parse the include file the user menu-selected from."
  (interactive)
  (semanticdb-find-translate-path nil nil)
  )


;;; General Includes Information
;;
(defun semantic-decoration-all-include-summary ()
  "Provide a general summary for the state of all includes."
  (interactive)
  (require 'semantic/dep)
  (let* ((table semanticdb-current-table)
	 (tags (semantic-fetch-tags))
	 (inc (semantic-find-tags-by-class 'include table))
	 )
    (with-output-to-temp-buffer (help-buffer) ;"*Help*"
      (help-setup-xref (list #'semantic-decoration-all-include-summary)
		       (called-interactively-p 'interactive))

      (princ "Include Summary for File: ")
      (princ (file-truename (buffer-file-name)))
      (princ "\n")

      (when (oref table db-refs)
	(princ "\nExternal Database References to this buffer:")
	(dolist (r (oref table db-refs))
	  (princ "\n    ")
	  (princ (oref r file)))
	)

      (princ (format "\nThis file contains %d tags, %d of which are includes.\n"
		     (length tags) (length inc)))
      (let ((ok 0)
	    (unknown 0)
	    (unparsed 0)
	    (all 0))
	(dolist (i inc)
	  (let* ((fileinner (semantic-dependency-tag-file i))
		 (tableinner (when fileinner
			       (semanticdb-file-table-object fileinner t))))
	    (cond ((not fileinner)
		   (setq unknown (1+ unknown)))
		  ((number-or-marker-p (oref tableinner pointmax))
		   (setq ok (1+ ok)))
		  (t
		   (setq unparsed (1+ unparsed))))))
	(setq all (+ ok unknown unparsed))
	(when (not (= 0 all))
	  (princ (format "   Unknown Includes:  %d\n" unknown))
	  (princ (format "   Unparsed Includes: %d\n" unparsed))
	  (princ (format "   Parsed Includes:   %d\n" ok)))
	)

      (princ "\nInclude Path Summary:\n\n")
      (when (and (boundp 'ede-object)
		 (boundp 'ede-object-project)
		 ede-object)
	(princ "  This file's project include search is handled by the EDE object:\n")
	(princ "    Buffer Target:  ")
	(princ (object-print ede-object))
	(princ "\n")
	(when (not (eq ede-object ede-object-project))
	  (princ "    Buffer Project: ")
	  (princ (object-print ede-object-project))
	  (princ "\n")
	  )
	(when ede-object-project
	  (let ((loc (ede-get-locator-object ede-object-project)))
	    (princ "    Backup in-project Locator: ")
	    (princ (object-print loc))
	    (princ "\n")))
	(let ((syspath (ede-system-include-path ede-object-project)))
	  (if (not syspath)
	      (princ "    EDE Project system include path: Empty\n")
	    (princ "    EDE Project system include path:\n")
	    (dolist (dir syspath)
	      (princ "        ")
	      (princ dir)
	      (princ "\n"))
	    )))

      (princ "\n  This file's system include path is:\n")
      (dolist (dir semantic-dependency-system-include-path)
	(princ "    ")
	(princ dir)
	(princ "\n"))

      (let ((unk semanticdb-find-lost-includes))
	(when unk
	  (princ "\nAll unknown includes:\n")
	  (dolist (tag unk)
	    (princ "  ")
	    (princ (semantic-tag-name tag))
	    (princ "\n"))
	  ))

      (let* ((semanticdb-find-default-throttle
	      (if (featurep 'semantic/db-find)
		  (remq 'unloaded semanticdb-find-default-throttle)
		nil))
	     (path (semanticdb-find-translate-path nil nil)))
	(if (<= (length path) (length inc))
	    (princ "\nThere are currently no includes found recursively.\n")
	  ;; List the full include list.
	  (princ "\nSummary of all includes needed by ")
	  (princ (buffer-name))
	  (dolist (p path)
	    (if (slot-boundp p 'tags)
		(princ (format "\n  %s :\t%d tags, %d are includes. %s"
			       (object-name-string p)
			       (length (oref p tags))
			       (length (semantic-find-tags-by-class
					'include p))
			       (cond
				((condition-case nil
				     (oref p dirty)
				   (error nil))
				 " dirty.")
				((not (number-or-marker-p (oref table pointmax)))
				 "  Needs to be parsed.")
				(t ""))))
	      (princ (format "\n  %s :\tUnparsed"
			     (object-name-string p))))
	    )))
      )))


;;; Unparsed Include Features
;;
;; This section handles changing states of unparsed include
;; decorations base on what happens in other files.
;;

(defclass semantic-decoration-unparsed-include-cache (semanticdb-abstract-cache)
  ()
  "Class used to reset decorated includes.
When an include's referring file is parsed, we need to undecorate
any decorated referring includes.")


(defmethod semantic-reset ((obj semantic-decoration-unparsed-include-cache))
  "Reset OBJ back to it's empty settings."
  (let ((table (oref obj table)))
    ;; This is a hack.  Add in something better?
    (semanticdb-notify-references
     table (lambda (tab me)
	     (semantic-decoration-unparsed-include-refrence-reset tab)
	     ))
    ))

(defmethod semanticdb-partial-synchronize ((cache semantic-decoration-unparsed-include-cache)
					   new-tags)
  "Synchronize CACHE with some NEW-TAGS."
  (if (semantic-find-tags-by-class 'include new-tags)
      (semantic-reset cache)))

(defmethod semanticdb-synchronize ((cache semantic-decoration-unparsed-include-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  (semantic-reset cache))

(defun semantic-decoration-unparsed-include-refrence-reset (table)
  "Refresh any highlighting in buffers referred to by TABLE.
If TABLE is not in a buffer, do nothing."
  ;; This cache removal may seem odd in that we are "creating one", but
  ;; since we can't get in the fcn unless one exists, this ought to be
  ;; ok.
  (let ((c (semanticdb-cache-get
	    table 'semantic-decoration-unparsed-include-cache)))
    (semanticdb-cache-remove table c))

  (let ((buf (semanticdb-in-buffer-p table)))
    (when buf
      (semantic-decorate-add-pending-decoration
       'semantic-decoration-unparsed-include-do-reset
       buf)
      )))

;;;###autoload
(defun semantic-decoration-unparsed-include-do-reset ()
  "Do a reset of unparsed includes in the current buffer."
  (let* ((style (assoc "semantic-decoration-on-includes"
		       semantic-decoration-styles)))
    (when (cdr style)
      (let ((allinc (semantic-find-tags-included
		     (semantic-fetch-tags-fast))))
	;; This will do everything, but it should be speedy since it
	;; would have been done once already.
	(semantic-decorate-add-decorations allinc)
	))))


(provide 'semantic/decorate/include)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/decorate/include"
;; End:

;;; semantic/decorate/include.el ends here
