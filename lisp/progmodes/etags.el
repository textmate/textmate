;;; etags.el --- etags facility for Emacs

;; Copyright (C) 1985-1986, 1988-1989, 1992-1996, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Maintainer: FSF
;; Keywords: tools

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

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ring)
(require 'button)

;;;###autoload
(defvar tags-file-name nil
  "*File name of tags table.
To switch to a new tags table, setting this variable is sufficient.
If you set this variable, do not also set `tags-table-list'.
Use the `etags' program to make a tags table file.")
;; Make M-x set-variable tags-file-name like M-x visit-tags-table.
;;;###autoload (put 'tags-file-name 'variable-interactive (purecopy "fVisit tags table: "))
;;;###autoload (put 'tags-file-name 'safe-local-variable 'stringp)

(defgroup etags nil "Tags tables."
  :group 'tools)

;;;###autoload
(defcustom tags-case-fold-search 'default
  "*Whether tags operations should be case-sensitive.
A value of t means case-insensitive, a value of nil means case-sensitive.
Any other value means use the setting of `case-fold-search'."
  :group 'etags
  :type '(choice (const :tag "Case-sensitive" nil)
		 (const :tag "Case-insensitive" t)
		 (other :tag "Use default" default))
  :version "21.1")

;;;###autoload
;; Use `visit-tags-table-buffer' to cycle through tags tables in this list.
(defcustom tags-table-list nil
  "*List of file names of tags tables to search.
An element that is a directory means the file \"TAGS\" in that directory.
To switch to a new list of tags tables, setting this variable is sufficient.
If you set this variable, do not also set `tags-file-name'.
Use the `etags' program to make a tags table file."
  :group 'etags
  :type '(repeat file))

;;;###autoload
(defcustom tags-compression-info-list
  (purecopy '("" ".Z" ".bz2" ".gz" ".xz" ".tgz"))
  "*List of extensions tried by etags when jka-compr is used.
An empty string means search the non-compressed file.
These extensions will be tried only if jka-compr was activated
\(i.e. via customize of `auto-compression-mode' or by calling the function
`auto-compression-mode')."
  :version "24.1"			; added xz
  :type  '(repeat string)
  :group 'etags)

;; !!! tags-compression-info-list should probably be replaced by access
;; to directory list and matching jka-compr-compression-info-list. Currently,
;; this implementation forces each modification of
;; jka-compr-compression-info-list to be reflected in this var.
;; An alternative could be to say that introducing a special
;; element in this list (e.g. t) means : try at this point
;; using directory listing and regexp matching using
;; jka-compr-compression-info-list.


;;;###autoload
(defcustom tags-add-tables 'ask-user
  "*Control whether to add a new tags table to the current list.
t means do; nil means don't (always start a new list).
Any other value means ask the user whether to add a new tags table
to the current list (as opposed to starting a new list)."
  :group 'etags
  :type '(choice (const :tag "Do" t)
		 (const :tag "Don't" nil)
		 (other :tag "Ask" ask-user)))

(defcustom tags-revert-without-query nil
  "*Non-nil means reread a TAGS table without querying, if it has changed."
  :group 'etags
  :type 'boolean)

(defvar tags-table-computed-list nil
  "List of tags tables to search, computed from `tags-table-list'.
This includes tables implicitly included by other tables.  The list is not
always complete: the included tables of a table are not known until that
table is read into core.  An element that is t is a placeholder
indicating that the preceding element is a table that has not been read
into core and might contain included tables to search.
See `tags-table-check-computed-list'.")

(defvar tags-table-computed-list-for nil
  "Value of `tags-table-list' that `tags-table-computed-list' corresponds to.
If `tags-table-list' changes, `tags-table-computed-list' is thrown away and
recomputed; see `tags-table-check-computed-list'.")

(defvar tags-table-list-pointer nil
  "Pointer into `tags-table-computed-list' for the current state of searching.
Use `visit-tags-table-buffer' to cycle through tags tables in this list.")

(defvar tags-table-list-started-at nil
  "Pointer into `tags-table-computed-list', where the current search started.")

(defvar tags-table-set-list nil
  "List of sets of tags table which have been used together in the past.
Each element is a list of strings which are file names.")

;;;###autoload
(defcustom find-tag-hook nil
  "*Hook to be run by \\[find-tag] after finding a tag.  See `run-hooks'.
The value in the buffer in which \\[find-tag] is done is used,
not the value in the buffer \\[find-tag] goes to."
  :group 'etags
  :type 'hook)

;;;###autoload
(defcustom find-tag-default-function nil
  "*A function of no arguments used by \\[find-tag] to pick a default tag.
If nil, and the symbol that is the value of `major-mode'
has a `find-tag-default-function' property (see `put'), that is used.
Otherwise, `find-tag-default' is used."
  :group 'etags
  :type '(choice (const nil) function))

(defcustom find-tag-marker-ring-length 16
  "*Length of marker rings `find-tag-marker-ring' and `tags-location-ring'."
  :group 'etags
  :type 'integer
  :version "20.3")

(defcustom tags-tag-face 'default
  "*Face for tags in the output of `tags-apropos'."
  :group 'etags
  :type 'face
  :version "21.1")

(defcustom tags-apropos-verbose nil
  "If non-nil, print the name of the tags file in the *Tags List* buffer."
  :group 'etags
  :type 'boolean
  :version "21.1")

(defcustom tags-apropos-additional-actions nil
  "Specify additional actions for `tags-apropos'.

If non-nil, value should be a list of triples (TITLE FUNCTION
TO-SEARCH).  For each triple, `tags-apropos' processes TO-SEARCH and
lists tags from it.  TO-SEARCH should be an alist, obarray, or symbol.
If it is a symbol, the symbol's value is used.
TITLE, a string, is a title used to label the additional list of tags.
FUNCTION is a function to call when a symbol is selected in the
*Tags List* buffer.  It will be called with one argument SYMBOL which
is the symbol being selected.

Example value:

  '((\"Emacs Lisp\" Info-goto-emacs-command-node obarray)
    (\"Common Lisp\" common-lisp-hyperspec common-lisp-hyperspec-obarray)
    (\"SCWM\" scwm-documentation scwm-obarray))"
  :group 'etags
  :type '(repeat (list (string :tag "Title")
		       function
		       (sexp :tag "Tags to search")))
  :version "21.1")

(defvar find-tag-marker-ring (make-ring find-tag-marker-ring-length)
  "Ring of markers which are locations from which \\[find-tag] was invoked.")

(defvar default-tags-table-function nil
  "If non-nil, a function to choose a default tags file for a buffer.
This function receives no arguments and should return the default
tags table file to use for the current buffer.")

(defvar tags-location-ring (make-ring find-tag-marker-ring-length)
  "Ring of markers which are locations visited by \\[find-tag].
Pop back to the last location with \\[negative-argument] \\[find-tag].")

;; Tags table state.
;; These variables are local in tags table buffers.

(defvar tags-table-files nil
  "List of file names covered by current tags table.
nil means it has not yet been computed; use `tags-table-files' to do so.")

(defvar tags-completion-table nil
  "Obarray of tag names defined in current tags table.")

(defvar tags-included-tables nil
  "List of tags tables included by the current tags table.")

(defvar next-file-list nil
  "List of files for \\[next-file] to process.")

;; Hooks for file formats.

(defvar tags-table-format-functions '(etags-recognize-tags-table
				      tags-recognize-empty-tags-table)
  "Hook to be called in a tags table buffer to identify the type of tags table.
The functions are called in order, with no arguments,
until one returns non-nil.  The function should make buffer-local bindings
of the format-parsing tags function variables if successful.")

(defvar file-of-tag-function nil
  "Function to do the work of `file-of-tag' (which see).
One optional argument, a boolean specifying to return complete path (nil) or
relative path (non-nil).")
(defvar tags-table-files-function nil
  "Function to do the work of `tags-table-files' (which see).")
(defvar tags-completion-table-function nil
  "Function to build the `tags-completion-table'.")
(defvar snarf-tag-function nil
  "Function to get info about a matched tag for `goto-tag-location-function'.
One optional argument, specifying to use explicit tag (non-nil) or not (nil).
The default is nil.")
(defvar goto-tag-location-function nil
  "Function of to go to the location in the buffer specified by a tag.
One argument, the tag info returned by `snarf-tag-function'.")
(defvar find-tag-regexp-search-function nil
  "Search function passed to `find-tag-in-order' for finding a regexp tag.")
(defvar find-tag-regexp-tag-order nil
  "Tag order passed to `find-tag-in-order' for finding a regexp tag.")
(defvar find-tag-regexp-next-line-after-failure-p nil
  "Flag passed to `find-tag-in-order' for finding a regexp tag.")
(defvar find-tag-search-function nil
  "Search function passed to `find-tag-in-order' for finding a tag.")
(defvar find-tag-tag-order nil
  "Tag order passed to `find-tag-in-order' for finding a tag.")
(defvar find-tag-next-line-after-failure-p nil
  "Flag passed to `find-tag-in-order' for finding a tag.")
(defvar list-tags-function nil
  "Function to do the work of `list-tags' (which see).")
(defvar tags-apropos-function nil
  "Function to do the work of `tags-apropos' (which see).")
(defvar tags-included-tables-function nil
  "Function to do the work of `tags-included-tables' (which see).")
(defvar verify-tags-table-function nil
  "Function to return t if current buffer contains valid tags file.")

(defun initialize-new-tags-table ()
  "Initialize the tags table in the current buffer.
Return non-nil if it is a valid tags table, and
in that case, also make the tags table state variables
buffer-local and set them to nil."
  (set (make-local-variable 'tags-table-files) nil)
  (set (make-local-variable 'tags-completion-table) nil)
  (set (make-local-variable 'tags-included-tables) nil)
  ;; We used to initialize find-tag-marker-ring and tags-location-ring
  ;; here, to new empty rings.  But that is wrong, because those
  ;; are global.

  ;; Value is t if we have found a valid tags table buffer.
  (run-hook-with-args-until-success 'tags-table-format-functions))

;;;###autoload
(defun tags-table-mode ()
  "Major mode for tags table file buffers."
  (interactive)
  (setq major-mode 'tags-table-mode     ;FIXME: Use define-derived-mode.
        mode-name "Tags Table"
        buffer-undo-list t)
  (initialize-new-tags-table))

;;;###autoload
(defun visit-tags-table (file &optional local)
  "Tell tags commands to use tags table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.

Normally \\[visit-tags-table] sets the global value of `tags-file-name'.
With a prefix arg, set the buffer-local value instead.
When you find a tag with \\[find-tag], the buffer it finds the tag
in is given a local value of this variable which is the name of the tags
file the tag was in."
  (interactive (list (read-file-name "Visit tags table (default TAGS): "
				     default-directory
				     (expand-file-name "TAGS"
						       default-directory)
				     t)
		     current-prefix-arg))
  (or (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
  ;; Bind tags-file-name so we can control below whether the local or
  ;; global value gets set.
  ;; Calling visit-tags-table-buffer with tags-file-name set to FILE will
  ;; initialize a buffer for FILE and set tags-file-name to the
  ;; fully-expanded name.
  (let ((tags-file-name file))
    (save-excursion
      (or (visit-tags-table-buffer file)
	  (signal 'file-error (list "Visiting tags table"
				    "file does not exist"
				    file)))
      ;; Set FILE to the expanded name.
      (setq file tags-file-name)))
  (if local
      ;; Set the local value of tags-file-name.
      (set (make-local-variable 'tags-file-name) file)
    ;; Set the global value of tags-file-name.
    (setq-default tags-file-name file)))

(defun tags-table-check-computed-list ()
  "Compute `tags-table-computed-list' from `tags-table-list' if necessary."
  (let ((expanded-list (mapcar 'tags-expand-table-name tags-table-list)))
    (or (equal tags-table-computed-list-for expanded-list)
	;; The list (or default-directory) has changed since last computed.
	(let* ((compute-for (mapcar 'copy-sequence expanded-list))
	       (tables (copy-sequence compute-for)) ;Mutated in the loop.
	       (computed nil)
	       table-buffer)

	  (while tables
	    (setq computed (cons (car tables) computed)
		  table-buffer (get-file-buffer (car tables)))
	    (if (and table-buffer
		     ;; There is a buffer visiting the file.  Now make sure
		     ;; it is initialized as a tag table buffer.
		     (save-excursion
		       (tags-verify-table (buffer-file-name table-buffer))))
		(with-current-buffer table-buffer
		  (if (tags-included-tables)
		      ;; Insert the included tables into the list we
		      ;; are processing.
		      (setcdr tables (nconc (mapcar 'tags-expand-table-name
						    (tags-included-tables))
					    (cdr tables)))))
	      ;; This table is not in core yet.  Insert a placeholder
	      ;; saying we must read it into core to check for included
	      ;; tables before searching the next table in the list.
	      (setq computed (cons t computed)))
	    (setq tables (cdr tables)))

	  ;; Record the tags-table-list value (and the context of the
	  ;; current directory) we computed from.
	  (setq tags-table-computed-list-for compute-for
		tags-table-computed-list (nreverse computed))))))

(defun tags-table-extend-computed-list ()
  "Extend `tags-table-computed-list' to remove the first t placeholder.

An element of the list that is t is a placeholder indicating that the
preceding element is a table that has not been read in and might
contain included tables to search.  This function reads in the first
such table and puts its included tables into the list."
  (let ((list tags-table-computed-list))
    (while (not (eq (nth 1 list) t))
      (setq list (cdr list)))
    (save-excursion
      (if (tags-verify-table (car list))
	  ;; We are now in the buffer visiting (car LIST).  Extract its
	  ;; list of included tables and insert it into the computed list.
	  (let ((tables (tags-included-tables))
		(computed nil)
		table-buffer)
	    (while tables
	      (setq computed (cons (car tables) computed)
		    table-buffer (get-file-buffer (car tables)))
	      (if table-buffer
		  (with-current-buffer table-buffer
		    (if (tags-included-tables)
			;; Insert the included tables into the list we
			;; are processing.
			(setcdr tables (append (tags-included-tables)
					       tables))))
		;; This table is not in core yet.  Insert a placeholder
		;; saying we must read it into core to check for included
		;; tables before searching the next table in the list.
		(setq computed (cons t computed)))
	      (setq tables (cdr tables)))
	    (setq computed (nreverse computed))
	    ;; COMPUTED now contains the list of included tables (and
	    ;; tables included by them, etc.).  Now splice this into the
	    ;; current list.
	    (setcdr list (nconc computed (cdr (cdr list)))))
	;; It was not a valid table, so just remove the following placeholder.
	(setcdr list (cdr (cdr list)))))))

(defun tags-expand-table-name (file)
  "Expand tags table name FILE into a complete file name."
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (expand-file-name "TAGS" file)
    file))

;; Like member, but comparison is done after tags-expand-table-name on both
;; sides and elements of LIST that are t are skipped.
(defun tags-table-list-member (file list)
  "Like (member FILE LIST) after applying `tags-expand-table-name'.
More precisely, apply `tags-expand-table-name' to FILE
and each element of LIST, returning the link whose car is the first match.
If an element of LIST is t, ignore it."
  (setq file (tags-expand-table-name file))
  (while (and list
	      (or (eq (car list) t)
		  (not (string= file (tags-expand-table-name (car list))))))
    (setq list (cdr list)))
  list)

(defun tags-verify-table (file)
  "Read FILE into a buffer and verify that it is a valid tags table.
Sets the current buffer to one visiting FILE (if it exists).
Returns non-nil if it is a valid table."
  (if (get-file-buffer file)
      ;; The file is already in a buffer.  Check for the visited file
      ;; having changed since we last used it.
      (progn
	(set-buffer (get-file-buffer file))
        (or verify-tags-table-function (tags-table-mode))
	(if (or (verify-visited-file-modtime (current-buffer))
		;; Decide whether to revert the file.
		;; revert-without-query can say to revert
		;; or the user can say to revert.
		(not (or (let ((tail revert-without-query)
			       (found nil))
			   (while tail
			     (if (string-match (car tail) buffer-file-name)
				 (setq found t))
			     (setq tail (cdr tail)))
			   found)
			 tags-revert-without-query
			 (yes-or-no-p
			  (format "Tags file %s has changed, read new contents? "
				  file)))))
	    (and verify-tags-table-function
		 (funcall verify-tags-table-function))
	  (revert-buffer t t)
	  (tags-table-mode)))
    (when (file-exists-p file)
      (let* ((buf (find-file-noselect file))
             (newfile (buffer-file-name buf)))
        (unless (string= file newfile)
          ;; find-file-noselect has changed the file name.
          ;; Propagate the change to tags-file-name and tags-table-list.
          (let ((tail (member file tags-table-list)))
            (if tail (setcar tail newfile)))
          (if (eq file tags-file-name) (setq tags-file-name newfile)))
        ;; Only change buffer now that we're done using potentially
        ;; buffer-local variables.
        (set-buffer buf)
        (tags-table-mode)))))

;; Subroutine of visit-tags-table-buffer.  Search the current tags tables
;; for one that has tags for THIS-FILE (or that includes a table that
;; does).  Return the name of the first table listing THIS-FILE; if
;; the table is one included by another table, it is the master table that
;; we return.  If CORE-ONLY is non-nil, check only tags tables that are
;; already in buffers--don't visit any new files.
(defun tags-table-including (this-file core-only)
  "Search current tags tables for tags for THIS-FILE.
Subroutine of `visit-tags-table-buffer'.
Looks for a tags table that has such tags or that includes a table
that has them.  Returns the name of the first such table.
Non-nil CORE-ONLY means check only tags tables that are already in
buffers.  If CORE-ONLY is nil, it is ignored."
  (let ((tables tags-table-computed-list)
	(found nil))
    ;; Loop over the list, looking for a table containing tags for THIS-FILE.
    (while (and (not found)
		tables)

      (if core-only
	  ;; Skip tables not in core.
	  (while (eq (nth 1 tables) t)
	    (setq tables (cdr (cdr tables))))
	(if (eq (nth 1 tables) t)
	    ;; This table has not been read into core yet.  Read it in now.
	    (tags-table-extend-computed-list)))

      (if tables
	  ;; Select the tags table buffer and get the file list up to date.
	  (let ((tags-file-name (car tables)))
	    (visit-tags-table-buffer 'same)
	    (if (member this-file (mapcar 'expand-file-name
					  (tags-table-files)))
		;; Found it.
		(setq found tables))))
      (setq tables (cdr tables)))
    (if found
	;; Now determine if the table we found was one included by another
	;; table, not explicitly listed.  We do this by checking each
	;; element of the computed list to see if it appears in the user's
	;; explicit list; the last element we will check is FOUND itself.
	;; Then we return the last one which did in fact appear in
	;; tags-table-list.
	(let ((could-be nil)
	      (elt tags-table-computed-list))
	  (while (not (eq elt (cdr found)))
	    (if (tags-table-list-member (car elt) tags-table-list)
		;; This table appears in the user's list, so it could be
		;; the one which includes the table we found.
		(setq could-be (car elt)))
	    (setq elt (cdr elt))
	    (if (eq t (car elt))
		(setq elt (cdr elt))))
	  ;; The last element we found in the computed list before FOUND
	  ;; that appears in the user's list will be the table that
	  ;; included the one we found.
	  could-be))))

(defun tags-next-table ()
  "Move `tags-table-list-pointer' along and set `tags-file-name'.
Subroutine of `visit-tags-table-buffer'.\
Returns nil when out of tables."
  ;; If there is a placeholder element next, compute the list to replace it.
  (while (eq (nth 1 tags-table-list-pointer) t)
    (tags-table-extend-computed-list))

  ;; Go to the next table in the list.
  (setq tags-table-list-pointer (cdr tags-table-list-pointer))
  (or tags-table-list-pointer
      ;; Wrap around.
      (setq tags-table-list-pointer tags-table-computed-list))

  (if (eq tags-table-list-pointer tags-table-list-started-at)
      ;; We have come full circle.  No more tables.
      (setq tags-table-list-pointer nil)
    ;; Set tags-file-name to the name from the list.  It is already expanded.
    (setq tags-file-name (car tags-table-list-pointer))))

;;;###autoload
(defun visit-tags-table-buffer (&optional cont)
  "Select the buffer containing the current tags table.
If optional arg is a string, visit that file as a tags table.
If optional arg is t, visit the next table in `tags-table-list'.
If optional arg is the atom `same', don't look for a new table;
 just select the buffer visiting `tags-file-name'.
If arg is nil or absent, choose a first buffer from information in
 `tags-file-name', `tags-table-list', `tags-table-list-pointer'.
Returns t if it visits a tags table, or nil if there are no more in the list."

  ;; Set tags-file-name to the tags table file we want to visit.
  (cond ((eq cont 'same)
	 ;; Use the ambient value of tags-file-name.
	 (or tags-file-name
	     (error "%s"
		    (substitute-command-keys
		     (concat "No tags table in use; "
			     "use \\[visit-tags-table] to select one")))))

	((eq t cont)
	 ;; Find the next table.
	 (if (tags-next-table)
	     ;; Skip over nonexistent files.
	     (while (and (not (or (get-file-buffer tags-file-name)
				  (file-exists-p tags-file-name)))
			 (tags-next-table)))))

	(t
	 ;; Pick a table out of our hat.
	 (tags-table-check-computed-list) ;Get it up to date, we might use it.
	 (setq tags-file-name
	       (or
		;; If passed a string, use that.
		(if (stringp cont)
		    (prog1 cont
		      (setq cont nil)))
		;; First, try a local variable.
		(cdr (assq 'tags-file-name (buffer-local-variables)))
		;; Second, try a user-specified function to guess.
		(and default-tags-table-function
		     (funcall default-tags-table-function))
		;; Third, look for a tags table that contains tags for the
		;; current buffer's file.  If one is found, the lists will
		;; be frobnicated, and CONT will be set non-nil so we don't
		;; do it below.
		(and buffer-file-name
		     (or
		      ;; First check only tables already in buffers.
		      (tags-table-including buffer-file-name t)
		      ;; Since that didn't find any, now do the
		      ;; expensive version: reading new files.
		      (tags-table-including buffer-file-name nil)))
		;; Fourth, use the user variable tags-file-name, if it is
		;; not already in the current list.
		(and tags-file-name
		     (not (tags-table-list-member tags-file-name
						  tags-table-computed-list))
		     tags-file-name)
		;; Fifth, use the user variable giving the table list.
		;; Find the first element of the list that actually exists.
		(let ((list tags-table-list)
		      file)
		  (while (and list
			      (setq file (tags-expand-table-name (car list)))
			      (not (get-file-buffer file))
			      (not (file-exists-p file)))
		    (setq list (cdr list)))
		  (car list))
		;; Finally, prompt the user for a file name.
		(expand-file-name
		 (read-file-name "Visit tags table (default TAGS): "
				 default-directory
				 "TAGS"
				 t))))))

  ;; Expand the table name into a full file name.
  (setq tags-file-name (tags-expand-table-name tags-file-name))

  (unless (and (eq cont t) (null tags-table-list-pointer))
    ;; Verify that tags-file-name names a valid tags table.
    ;; Bind another variable with the value of tags-file-name
    ;; before we switch buffers, in case tags-file-name is buffer-local.
    (let ((curbuf (current-buffer))
	  (local-tags-file-name tags-file-name))
      (if (tags-verify-table local-tags-file-name)

	  ;; We have a valid tags table.
	  (progn
	    ;; Bury the tags table buffer so it
	    ;; doesn't get in the user's way.
	    (bury-buffer (current-buffer))

	    ;; If this was a new table selection (CONT is nil), make
	    ;; sure tags-table-list includes the chosen table, and
	    ;; update the list pointer variables.
	    (or cont
		;; Look in the list for the table we chose.
		(let ((found (tags-table-list-member
			      local-tags-file-name
			      tags-table-computed-list)))
		  (if found
		      ;; There it is.  Just switch to it.
		      (setq tags-table-list-pointer found
			    tags-table-list-started-at found)

		    ;; The table is not in the current set.
		    ;; Try to find it in another previously used set.
		    (let ((sets tags-table-set-list))
		      (while (and sets
				  (not (tags-table-list-member
					local-tags-file-name
					(car sets))))
			(setq sets (cdr sets)))
		      (if sets
			  ;; Found in some other set.  Switch to that set.
			  (progn
			    (or (memq tags-table-list tags-table-set-list)
				;; Save the current list.
				(setq tags-table-set-list
				      (cons tags-table-list
					    tags-table-set-list)))
			    (setq tags-table-list (car sets)))

			;; Not found in any existing set.
			(if (and tags-table-list
				 (or (eq t tags-add-tables)
				     (and tags-add-tables
					  (y-or-n-p
					   (concat "Keep current list of "
						   "tags tables also? ")))))
			    ;; Add it to the current list.
			    (setq tags-table-list (cons local-tags-file-name
							tags-table-list))

			  ;; Make a fresh list, and store the old one.
			  (message "Starting a new list of tags tables")
			  (or (null tags-table-list)
			      (memq tags-table-list tags-table-set-list)
			      (setq tags-table-set-list
				    (cons tags-table-list
					  tags-table-set-list)))
			  ;; Clear out buffers holding old tables.
			  (dolist (table tags-table-list)
			    ;; The list can contain items t.
			    (if (stringp table)
				(let ((buffer (find-buffer-visiting table)))
			      (if buffer
				  (kill-buffer buffer)))))
			  (setq tags-table-list (list local-tags-file-name))))

		      ;; Recompute tags-table-computed-list.
		      (tags-table-check-computed-list)
		      ;; Set the tags table list state variables to start
		      ;; over from tags-table-computed-list.
		      (setq tags-table-list-started-at tags-table-computed-list
			    tags-table-list-pointer
			    tags-table-computed-list)))))

	    ;; Return of t says the tags table is valid.
	    t)

	;; The buffer was not valid.  Don't use it again.
	(set-buffer curbuf)
	(kill-local-variable 'tags-file-name)
	(if (eq local-tags-file-name tags-file-name)
	    (setq tags-file-name nil))
	(error "File %s is not a valid tags table" local-tags-file-name)))))

(defun tags-reset-tags-tables ()
  "Reset tags state to cancel effect of any previous \\[visit-tags-table] or \\[find-tag]."
  (interactive)
  ;; Clear out the markers we are throwing away.
  (let ((i 0))
    (while (< i find-tag-marker-ring-length)
      (if (aref (cddr tags-location-ring) i)
	  (set-marker (aref (cddr tags-location-ring) i) nil))
      (if (aref (cddr find-tag-marker-ring) i)
	  (set-marker (aref (cddr find-tag-marker-ring) i) nil))
      (setq i (1+ i))))
  (setq tags-file-name nil
	tags-location-ring (make-ring find-tag-marker-ring-length)
	find-tag-marker-ring (make-ring find-tag-marker-ring-length)
	tags-table-list nil
	tags-table-computed-list nil
	tags-table-computed-list-for nil
	tags-table-list-pointer nil
	tags-table-list-started-at nil
	tags-table-set-list nil))

(defun file-of-tag (&optional relative)
  "Return the file name of the file whose tags point is within.
Assumes the tags table is the current buffer.
If RELATIVE is non-nil, file name returned is relative to tags
table file's directory. If RELATIVE is nil, file name returned
is complete."
  (funcall file-of-tag-function relative))

;;;###autoload
(defun tags-table-files ()
  "Return a list of files in the current tags table.
Assumes the tags table is the current buffer.  The file names are returned
as they appeared in the `etags' command that created the table, usually
without directory names."
  (or tags-table-files
      (setq tags-table-files
	    (funcall tags-table-files-function))))

(defun tags-included-tables ()
  "Return a list of tags tables included by the current table.
Assumes the tags table is the current buffer."
  (or tags-included-tables
      (setq tags-included-tables (funcall tags-included-tables-function))))

(defun tags-completion-table ()
  "Build `tags-completion-table' on demand.
The tags included in the completion table are those in the current
tags table and its (recursively) included tags tables."
  (or tags-completion-table
      ;; No cached value for this buffer.
      (condition-case ()
	  (let (current-table combined-table)
	    (message "Making tags completion table for %s..." buffer-file-name)
	    (save-excursion
	      ;; Iterate over the current list of tags tables.
	      (while (visit-tags-table-buffer (and combined-table t))
		;; Find possible completions in this table.
		(setq current-table (funcall tags-completion-table-function))
		;; Merge this buffer's completions into the combined table.
		(if combined-table
		    (mapatoms
		     (lambda (sym) (intern (symbol-name sym) combined-table))
		     current-table)
		  (setq combined-table current-table))))
	    (message "Making tags completion table for %s...done"
		     buffer-file-name)
	    ;; Cache the result in a buffer-local variable.
	    (setq tags-completion-table combined-table))
	(quit (message "Tags completion table construction aborted.")
	      (setq tags-completion-table nil)))))

(defun tags-lazy-completion-table ()
  (lexical-let ((buf (current-buffer)))
    (lambda (string pred action)
      (with-current-buffer buf
        (save-excursion
          ;; If we need to ask for the tag table, allow that.
          (let ((enable-recursive-minibuffers t))
            (visit-tags-table-buffer))
          (complete-with-action action (tags-completion-table) string pred))))))

;;;###autoload (defun tags-completion-at-point-function ()
;;;###autoload   (if (or tags-table-list tags-file-name)
;;;###autoload       (progn
;;;###autoload         (load "etags")
;;;###autoload         (tags-completion-at-point-function))))

(defun tags-completion-at-point-function ()
  "Using tags, return a completion table for the text around point.
If no tags table is loaded, do nothing and return nil."
  (when (or tags-table-list tags-file-name)
    (let ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
				      tags-case-fold-search
				    case-fold-search))
	  (pattern (funcall (or find-tag-default-function
				(get major-mode 'find-tag-default-function)
				'find-tag-default)))
	  beg)
      (when pattern
	(save-excursion
          (forward-char (1- (length pattern)))
          (search-backward pattern)
          (setq beg (point))
          (forward-char (length pattern))
          (list beg (point) (tags-lazy-completion-table) :exclusive 'no))))))

(defun find-tag-tag (string)
  "Read a tag name, with defaulting and completion."
  (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
				     tags-case-fold-search
				   case-fold-search))
	 (default (funcall (or find-tag-default-function
			       (get major-mode 'find-tag-default-function)
			       'find-tag-default)))
	 (spec (completing-read (if default
				    (format "%s (default %s): "
					    (substring string 0 (string-match "[ :]+\\'" string))
					    default)
				  string)
				(tags-lazy-completion-table)
				nil nil nil nil default)))
    (if (equal spec "")
	(or default (error "There is no default tag"))
      spec)))

(defvar last-tag nil
  "Last tag found by \\[find-tag].")

(defun find-tag-interactive (prompt &optional no-default)
  "Get interactive arguments for tag functions.
The functions using this are `find-tag-noselect',
`find-tag-other-window', and `find-tag-regexp'."
  (if (and current-prefix-arg last-tag)
      (list nil (if (< (prefix-numeric-value current-prefix-arg) 0)
		    '-
		  t))
    (list (if no-default
	      (read-string prompt)
	    (find-tag-tag prompt)))))

(defvar find-tag-history nil) ; Doc string?

;; Dynamic bondage:
(defvar etags-case-fold-search)
(defvar etags-syntax-table)
(defvar local-find-tag-hook)

;;;###autoload
(defun find-tag-noselect (tagname &optional next-p regexp-p)
  "Find tag (in current tags table) whose name contains TAGNAME.
Returns the buffer containing the tag's definition and moves its point there,
but does not select the buffer.
The default for TAGNAME is the expression in the buffer near point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is the atom `-' (interactively, with prefix arg that is a negative number
or just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag: "))

  (setq find-tag-history (cons tagname find-tag-history))
  ;; Save the current buffer's value of `find-tag-hook' before
  ;; selecting the tags table buffer.  For the same reason, save value
  ;; of `tags-file-name' in case it has a buffer-local value.
  (let ((local-find-tag-hook find-tag-hook))
    (if (eq '- next-p)
	;; Pop back to a previous location.
	(if (ring-empty-p tags-location-ring)
	    (error "No previous tag locations")
	  (let ((marker (ring-remove tags-location-ring 0)))
	    (prog1
		;; Move to the saved location.
		(set-buffer (or (marker-buffer marker)
                                (error "The marked buffer has been deleted")))
	      (goto-char (marker-position marker))
	      ;; Kill that marker so it doesn't slow down editing.
	      (set-marker marker nil nil)
	      ;; Run the user's hook.  Do we really want to do this for pop?
	      (run-hooks 'local-find-tag-hook))))
      ;; Record whence we came.
      (ring-insert find-tag-marker-ring (point-marker))
      (if (and next-p last-tag)
	  ;; Find the same table we last used.
	  (visit-tags-table-buffer 'same)
	;; Pick a table to use.
	(visit-tags-table-buffer)
	;; Record TAGNAME for a future call with NEXT-P non-nil.
	(setq last-tag tagname))
      ;; Record the location so we can pop back to it later.
      (let ((marker (make-marker)))
	(with-current-buffer
            ;; find-tag-in-order does the real work.
            (find-tag-in-order
             (if (and next-p last-tag) last-tag tagname)
             (if regexp-p
                 find-tag-regexp-search-function
               find-tag-search-function)
             (if regexp-p
                 find-tag-regexp-tag-order
               find-tag-tag-order)
             (if regexp-p
                 find-tag-regexp-next-line-after-failure-p
               find-tag-next-line-after-failure-p)
             (if regexp-p "matching" "containing")
             (or (not next-p) (not last-tag)))
	  (set-marker marker (point))
	  (run-hooks 'local-find-tag-hook)
	  (ring-insert tags-location-ring marker)
	  (current-buffer))))))

;;;###autoload
(defun find-tag (tagname &optional next-p regexp-p)
  "Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition, and move point there.
The default for TAGNAME is the expression in the buffer around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is the atom `-' (interactively, with prefix arg that is a negative number
or just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag: "))
  (let* ((buf (find-tag-noselect tagname next-p regexp-p))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (goto-char pos)))
;;;###autoload (define-key esc-map "." 'find-tag)

;;;###autoload
(defun find-tag-other-window (tagname &optional next-p regexp-p)
  "Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition in another window, and
move point there.  The default for TAGNAME is the expression in the buffer
around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag other window: "))

  ;; This hair is to deal with the case where the tag is found in the
  ;; selected window's buffer; without the hair, point is moved in both
  ;; windows.  To prevent this, we save the selected window's point before
  ;; doing find-tag-noselect, and restore it after.
  (let* ((window-point (window-point (selected-window)))
	 (tagbuf (find-tag-noselect tagname next-p regexp-p))
	 (tagpoint (progn (set-buffer tagbuf) (point))))
    (set-window-point (prog1
			  (selected-window)
			(switch-to-buffer-other-window tagbuf)
			;; We have to set this new window's point; it
			;; might already have been displaying a
			;; different portion of tagbuf, in which case
			;; switch-to-buffer-other-window doesn't set
			;; the window's point from the buffer.
			(set-window-point (selected-window) tagpoint))
		      window-point)))
;;;###autoload (define-key ctl-x-4-map "." 'find-tag-other-window)

;;;###autoload
(defun find-tag-other-frame (tagname &optional next-p)
  "Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition in another frame, and
move point there.  The default for TAGNAME is the expression in the buffer
around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag other frame: "))
  (let ((pop-up-frames t))
    (find-tag-other-window tagname next-p)))
;;;###autoload (define-key ctl-x-5-map "." 'find-tag-other-frame)

;;;###autoload
(defun find-tag-regexp (regexp &optional next-p other-window)
  "Find tag (in current tags table) whose name matches REGEXP.
Select the buffer containing the tag's definition and move point there.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg OTHER-WINDOW is non-nil, select the buffer in another window.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag regexp: " t))
  ;; We go through find-tag-other-window to do all the display hair there.
  (funcall (if other-window 'find-tag-other-window 'find-tag)
	   regexp next-p t))
;;;###autoload (define-key esc-map [?\C-.] 'find-tag-regexp)

;;;###autoload (define-key esc-map "*" 'pop-tag-mark)

;;;###autoload
(defun pop-tag-mark ()
  "Pop back to where \\[find-tag] was last invoked.

This is distinct from invoking \\[find-tag] with a negative argument
since that pops a stack of markers at which tags were found, not from
where they were found."
  (interactive)
  (if (ring-empty-p find-tag-marker-ring)
      (error "No previous locations for find-tag invocation"))
  (let ((marker (ring-remove find-tag-marker-ring 0)))
    (switch-to-buffer (or (marker-buffer marker)
                          (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))

(defvar tag-lines-already-matched nil
  "Matches remembered between calls.") ; Doc string: calls to what?

(defun find-tag-in-order (pattern
			  search-forward-func
			  order
			  next-line-after-failure-p
			  matching
			  first-search)
  "Internal tag-finding function.
PATTERN is a string to pass to arg SEARCH-FORWARD-FUNC, and to any
member of the function list ORDER.  If ORDER is nil, use saved state
to continue a previous search.

Arg NEXT-LINE-AFTER-FAILURE-P is non-nil if after a failed match,
point should be moved to the next line.

Arg MATCHING is a string, an English `-ing' word, to be used in an
error message."
;; Algorithm is as follows:
;; For each qualifier-func in ORDER, go to beginning of tags file, and
;; perform inner loop: for each naive match for PATTERN found using
;; SEARCH-FORWARD-FUNC, qualify the naive match using qualifier-func.  If
;; it qualifies, go to the specified line in the specified source file
;; and return.  Qualified matches are remembered to avoid repetition.
;; State is saved so that the loop can be continued.
  (let (file				;name of file containing tag
	tag-info			;where to find the tag in FILE
	(first-table t)
	(tag-order order)
	(match-marker (make-marker))
	goto-func
	(case-fold-search (if (memq tags-case-fold-search '(nil t))
			      tags-case-fold-search
			    case-fold-search))
	)
    (save-excursion

      (if first-search
	  ;; This is the start of a search for a fresh tag.
	  ;; Clear the list of tags matched by the previous search.
	  ;; find-tag-noselect has already put us in the first tags table
	  ;; buffer before we got called.
	  (setq tag-lines-already-matched nil)
	;; Continuing to search for the tag specified last time.
	;; tag-lines-already-matched lists locations matched in previous
	;; calls so we don't visit the same tag twice if it matches twice
	;; during two passes with different qualification predicates.
	;; Switch to the current tags table buffer.
	(visit-tags-table-buffer 'same))

      ;; Get a qualified match.
      (catch 'qualified-match-found

	;; Iterate over the list of tags tables.
	(while (or first-table
		   (visit-tags-table-buffer t))

	  (and first-search first-table
	       ;; Start at beginning of tags file.
	       (goto-char (point-min)))

	  (setq first-table nil)

	  ;; Iterate over the list of ordering predicates.
	  (while order
	    (while (funcall search-forward-func pattern nil t)
	      ;; Naive match found.  Qualify the match.
	      (and (funcall (car order) pattern)
		   ;; Make sure it is not a previous qualified match.
		   (not (member (set-marker match-marker (point-at-bol))
				tag-lines-already-matched))
		   (throw 'qualified-match-found nil))
	      (if next-line-after-failure-p
		  (forward-line 1)))
	    ;; Try the next flavor of match.
	    (setq order (cdr order))
	    (goto-char (point-min)))
	  (setq order tag-order))
	;; We throw out on match, so only get here if there were no matches.
	;; Clear out the markers we use to avoid duplicate matches so they
	;; don't slow down editing and are immediately available for GC.
	(while tag-lines-already-matched
	  (set-marker (car tag-lines-already-matched) nil nil)
	  (setq tag-lines-already-matched (cdr tag-lines-already-matched)))
	(set-marker match-marker nil nil)
	(error "No %stags %s %s" (if first-search "" "more ")
	       matching pattern))

      ;; Found a tag; extract location info.
      (beginning-of-line)
      (setq tag-lines-already-matched (cons match-marker
					    tag-lines-already-matched))
      ;; Expand the filename, using the tags table buffer's default-directory.
      ;; We should be able to search for file-name backwards in file-of-tag:
      ;; the beginning-of-line is ok except when positioned on a "file-name" tag.
      (setq file (expand-file-name
		  (if (memq (car order) '(tag-exact-file-name-match-p
					  tag-file-name-match-p
					  tag-partial-file-name-match-p))
                      (save-excursion (forward-line 1)
                                      (file-of-tag))
                    (file-of-tag)))
	    tag-info (funcall snarf-tag-function))

      ;; Get the local value in the tags table buffer before switching buffers.
      (setq goto-func goto-tag-location-function)
      (tag-find-file-of-tag-noselect file)
      (widen)
      (push-mark)
      (funcall goto-func tag-info)

      ;; Return the buffer where the tag was found.
      (current-buffer))))

(defun tag-find-file-of-tag-noselect (file)
  "Find the right line in the specified FILE."
  ;; If interested in compressed-files, search files with extensions.
  ;; Otherwise, search only the real file.
  (let* ((buffer-search-extensions (if (featurep 'jka-compr)
				       tags-compression-info-list
				     '("")))
	 the-buffer
	 (file-search-extensions buffer-search-extensions))
    ;; search a buffer visiting the file with each possible extension
    ;; Note: there is a small inefficiency in find-buffer-visiting :
    ;;   truename is computed even if not needed. Not too sure about this
    ;;   but I suspect truename computation accesses the disk.
    ;;   It is maybe a good idea to optimize this find-buffer-visiting.
    ;; An alternative would be to use only get-file-buffer
    ;; but this looks less "sure" to find the buffer for the file.
    (while (and (not the-buffer) buffer-search-extensions)
      (setq the-buffer (find-buffer-visiting (concat file (car buffer-search-extensions))))
      (setq buffer-search-extensions (cdr buffer-search-extensions)))
    ;; if found a buffer but file modified, ensure we re-read !
    (if (and the-buffer (not (verify-visited-file-modtime the-buffer)))
	(find-file-noselect (buffer-file-name the-buffer)))
    ;; if no buffer found, search for files with possible extensions on disk
    (while (and (not the-buffer) file-search-extensions)
      (if (not (file-exists-p (concat file (car file-search-extensions))))
	  (setq file-search-extensions (cdr file-search-extensions))
	(setq the-buffer (find-file-noselect (concat file (car file-search-extensions))))))
    (if (not the-buffer)
	(if (featurep 'jka-compr)
	    (error "File %s (with or without extensions %s) not found" file tags-compression-info-list)
	  (error "File %s not found" file))
      (set-buffer the-buffer))))

(defun tag-find-file-of-tag (file) ; Doc string?
  (let ((buf (tag-find-file-of-tag-noselect file)))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))))

;; `etags' TAGS file format support.

(defun etags-recognize-tags-table ()
  "If `etags-verify-tags-table', make buffer-local format variables.
If current buffer is a valid etags TAGS file, then give it
buffer-local values of tags table format variables."
  (and (etags-verify-tags-table)
       ;; It is annoying to flash messages on the screen briefly,
       ;; and this message is not useful.  -- rms
       ;; (message "%s is an `etags' TAGS file" buffer-file-name)
       (mapc (lambda (elt) (set (make-local-variable (car elt)) (cdr elt)))
	     '((file-of-tag-function . etags-file-of-tag)
	       (tags-table-files-function . etags-tags-table-files)
	       (tags-completion-table-function . etags-tags-completion-table)
	       (snarf-tag-function . etags-snarf-tag)
	       (goto-tag-location-function . etags-goto-tag-location)
	       (find-tag-regexp-search-function . re-search-forward)
	       (find-tag-regexp-tag-order . (tag-re-match-p))
	       (find-tag-regexp-next-line-after-failure-p . t)
	       (find-tag-search-function . search-forward)
	       (find-tag-tag-order . (tag-exact-file-name-match-p
                                      tag-file-name-match-p
				      tag-exact-match-p
				      tag-implicit-name-match-p
				      tag-symbol-match-p
				      tag-word-match-p
				      tag-partial-file-name-match-p
				      tag-any-match-p))
	       (find-tag-next-line-after-failure-p . nil)
	       (list-tags-function . etags-list-tags)
	       (tags-apropos-function . etags-tags-apropos)
	       (tags-included-tables-function . etags-tags-included-tables)
	       (verify-tags-table-function . etags-verify-tags-table)
	       ))))

(defun etags-verify-tags-table ()
  "Return non-nil if the current buffer is a valid etags TAGS file."
  ;; Use eq instead of = in case char-after returns nil.
  (eq (char-after (point-min)) ?\f))

(defun etags-file-of-tag (&optional relative) ; Doc string?
  (save-excursion
    (re-search-backward "\f\n\\([^\n]+\\),[0-9]*\n")
    (let ((str (convert-standard-filename
                (buffer-substring (match-beginning 1) (match-end 1)))))
      (if relative
	  str
	(expand-file-name str (file-truename default-directory))))))


(defun etags-tags-completion-table () ; Doc string?
  (let ((table (make-vector 511 0))
	(progress-reporter
	 (make-progress-reporter
	  (format "Making tags completion table for %s..." buffer-file-name)
	  (point-min) (point-max))))
    (save-excursion
      (goto-char (point-min))
      ;; This monster regexp matches an etags tag line.
      ;;   \1 is the string to match;
      ;;   \2 is not interesting;
      ;;   \3 is the guessed tag name; XXX guess should be better eg DEFUN
      ;;   \4 is not interesting;
      ;;   \5 is the explicitly-specified tag name.
      ;;   \6 is the line to start searching at;
      ;;   \7 is the char to start searching at.
      (while (re-search-forward
	      "^\\(\\([^\177]+[^-a-zA-Z0-9_+*$:\177]+\\)?\
\\([-a-zA-Z0-9_+*$?:]+\\)[^-a-zA-Z0-9_+*$?:\177]*\\)\177\
\\(\\([^\n\001]+\\)\001\\)?\\([0-9]+\\)?,\\([0-9]+\\)?\n"
	      nil t)
	(intern	(prog1 (if (match-beginning 5)
			   ;; There is an explicit tag name.
			   (buffer-substring (match-beginning 5) (match-end 5))
			 ;; No explicit tag name.  Best guess.
			 (buffer-substring (match-beginning 3) (match-end 3)))
		  (progress-reporter-update progress-reporter (point)))
		table)))
    table))

(defun etags-snarf-tag (&optional use-explicit) ; Doc string?
  (let (tag-text line startpos explicit-start)
    (if (save-excursion
	  (forward-line -1)
	  (looking-at "\f\n"))
	;; The match was for a source file name, not any tag within a file.
	;; Give text of t, meaning to go exactly to the location we specify,
	;; the beginning of the file.
	(setq tag-text t
	      line nil
	      startpos (point-min))

      ;; Find the end of the tag and record the whole tag text.
      (search-forward "\177")
      (setq tag-text (buffer-substring (1- (point)) (point-at-bol)))
      ;; If use-explicit is non nil and explicit tag is present, use it as part of
      ;; return value. Else just skip it.
      (setq explicit-start (point))
      (when (and (search-forward "\001" (point-at-bol 2) t)
		 use-explicit)
	(setq tag-text (buffer-substring explicit-start (1- (point)))))


      (if (looking-at "[0-9]")
	  (setq line (string-to-number (buffer-substring
                                        (point)
                                        (progn (skip-chars-forward "0-9")
                                               (point))))))
      (search-forward ",")
      (if (looking-at "[0-9]")
	  (setq startpos (string-to-number (buffer-substring
                                            (point)
                                            (progn (skip-chars-forward "0-9")
                                                   (point)))))))
    ;; Leave point on the next line of the tags file.
    (forward-line 1)
    (cons tag-text (cons line startpos))))

(defun etags-goto-tag-location (tag-info)
  "Go to location of tag specified by TAG-INFO.
TAG-INFO is a cons (TEXT LINE . POSITION).
TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present.

If the tag isn't exactly at the given position, then look near that
position using a search window that expands progressively until it
hits the start of file."
  (let ((startpos (cdr (cdr tag-info)))
	(line (car (cdr tag-info)))
	offset found pat)
    (if (eq (car tag-info) t)
	;; Direct file tag.
	(cond (line (progn (goto-char (point-min))
			   (forward-line (1- line))))
	      (startpos (goto-char startpos))
	      (t (error "etags.el BUG: bogus direct file tag")))
      ;; This constant is 1/2 the initial search window.
      ;; There is no sense in making it too small,
      ;; since just going around the loop once probably
      ;; costs about as much as searching 2000 chars.
      (setq offset 1000
	    found nil
	    pat (concat (if (eq selective-display t)
			    "\\(^\\|\^m\\)" "^")
			(regexp-quote (car tag-info))))
      ;; The character position in the tags table is 0-origin.
      ;; Convert it to a 1-origin Emacs character position.
      (if startpos (setq startpos (1+ startpos)))
      ;; If no char pos was given, try the given line number.
      (or startpos
	  (if line
	      (setq startpos (progn (goto-char (point-min))
				    (forward-line (1- line))
				    (point)))))
      (or startpos
	  (setq startpos (point-min)))
      ;; First see if the tag is right at the specified location.
      (goto-char startpos)
      (setq found (looking-at pat))
      (while (and (not found)
		  (progn
		    (goto-char (- startpos offset))
		    (not (bobp))))
	(setq found
	      (re-search-forward pat (+ startpos offset) t)
	      offset (* 3 offset)))	; expand search window
      (or found
	  (re-search-forward pat nil t)
	  (error "Rerun etags: `%s' not found in %s"
		 pat buffer-file-name)))
    ;; Position point at the right place
    ;; if the search string matched an extra Ctrl-m at the beginning.
    (and (eq selective-display t)
	 (looking-at "\^m")
	 (forward-char 1))
    (beginning-of-line)))

(defun etags-list-tags (file) ; Doc string?
  (goto-char (point-min))
  (when (re-search-forward (concat "\f\n" "\\(" file "\\)" ",") nil t)
    (let ((path (save-excursion (forward-line 1) (file-of-tag)))
	  ;; Get the local value in the tags table
	  ;; buffer before switching buffers.
	  (goto-func goto-tag-location-function)
	  tag tag-info pt)
    (forward-line 1)
    (while (not (or (eobp) (looking-at "\f")))
      ;; We used to use explicit tags when available, but the current goto-func
      ;; can only handle implicit tags.
      (setq tag-info (save-excursion (funcall snarf-tag-function nil))
	    tag (car tag-info)
	    pt (with-current-buffer standard-output (point)))
      (princ tag)
      (when (= (aref tag 0) ?\() (princ " ...)"))
      (with-current-buffer standard-output
	(make-text-button pt (point)
			  'tag-info tag-info
			  'file-path path
			  'goto-func goto-func
			  'action (lambda (button)
				    (let ((tag-info (button-get button 'tag-info))
					  (goto-func (button-get button 'goto-func)))
				      (tag-find-file-of-tag (button-get button 'file-path))
				      (widen)
				      (funcall goto-func tag-info)))
			  'follow-link t
			  'face tags-tag-face
			  'type 'button))
      (terpri)
      (forward-line 1))
    t)))

(defmacro tags-with-face (face &rest body)
  "Execute BODY, give output to `standard-output' face FACE."
  (let ((pp (make-symbol "start")))
    `(let ((,pp (with-current-buffer standard-output (point))))
       ,@body
       (put-text-property ,pp (with-current-buffer standard-output (point))
			  'face ,face standard-output))))

(defun etags-tags-apropos-additional (regexp)
  "Display tags matching REGEXP from `tags-apropos-additional-actions'."
  (with-current-buffer standard-output
    (dolist (oba tags-apropos-additional-actions)
      (princ "\n\n")
      (tags-with-face 'highlight (princ (car oba)))
      (princ":\n\n")
      (let* ((beg (point))
	     (symbs (car (cddr oba)))
             (ins-symb (lambda (sy)
                         (let ((sn (symbol-name sy)))
                           (when (string-match regexp sn)
                             (make-text-button (point)
					  (progn (princ sy) (point))
					  'action-internal(cadr oba)
					  'action (lambda (button) (funcall
								    (button-get button 'action-internal)
								    (button-get button 'item)))
					  'item sn
					  'face tags-tag-face
					  'follow-link t
					  'type 'button)
                             (terpri))))))
        (when (symbolp symbs)
          (if (boundp symbs)
	      (setq symbs (symbol-value symbs))
	    (insert "symbol `" (symbol-name symbs) "' has no value\n")
	    (setq symbs nil)))
        (if (vectorp symbs)
	    (mapatoms ins-symb symbs)
	  (dolist (sy symbs)
	    (funcall ins-symb (car sy))))
        (sort-lines nil beg (point))))))

(defun etags-tags-apropos (string) ; Doc string?
  (when tags-apropos-verbose
    (princ "Tags in file `")
    (tags-with-face 'highlight (princ buffer-file-name))
    (princ "':\n\n"))
  (goto-char (point-min))
  (let ((progress-reporter (make-progress-reporter
			    (format "Making tags apropos buffer for `%s'..."
				    string)
			    (point-min) (point-max))))
    (while (re-search-forward string nil t)
      (progress-reporter-update progress-reporter (point))
      (beginning-of-line)

      (let* ( ;; Get the local value in the tags table
	     ;; buffer before switching buffers.
	     (goto-func goto-tag-location-function)
	     (tag-info (save-excursion (funcall snarf-tag-function)))
	     (tag (if (eq t (car tag-info)) nil (car tag-info)))
	     (file-path (save-excursion (if tag (file-of-tag)
					  (save-excursion (forward-line 1)
							  (file-of-tag)))))
	     (file-label (if tag (file-of-tag t)
			   (save-excursion (forward-line 1)
					   (file-of-tag t))))
	     (pt (with-current-buffer standard-output (point))))
	(if tag
	    (progn
	      (princ (format "[%s]: " file-label))
	      (princ tag)
	      (when (= (aref tag 0) ?\() (princ " ...)"))
	      (with-current-buffer standard-output
		(make-text-button pt (point)
				  'tag-info tag-info
				  'file-path file-path
				  'goto-func goto-func
				  'action (lambda (button)
					    (let ((tag-info (button-get button 'tag-info))
						  (goto-func (button-get button 'goto-func)))
					      (tag-find-file-of-tag (button-get button 'file-path))
					      (widen)
					      (funcall goto-func tag-info)))
				  'follow-link t
				  'face tags-tag-face
				  'type 'button)))
	  (princ (format "- %s" file-label))
	  (with-current-buffer standard-output
	    (make-text-button pt (point)
			      'file-path file-path
			      'action (lambda (button)
					(tag-find-file-of-tag (button-get button 'file-path))
					;; Get the local value in the tags table
					;; buffer before switching buffers.
					(goto-char (point-min)))
			      'follow-link t
			      'face tags-tag-face
			      'type 'button))))
      (terpri)
      (forward-line 1))
    (message nil))
  (when tags-apropos-verbose (princ "\n")))

(defun etags-tags-table-files () ; Doc string?
  (let ((files nil)
	beg)
    (goto-char (point-min))
    (while (search-forward "\f\n" nil t)
      (setq beg (point))
      (end-of-line)
      (skip-chars-backward "^," beg)
      (or (looking-at "include$")
	  (push (convert-standard-filename
                 (buffer-substring beg (1- (point))))
                files)))
    (nreverse files)))

(defun etags-tags-included-tables () ; Doc string?
  (let ((files nil)
	beg)
    (goto-char (point-min))
    (while (search-forward "\f\n" nil t)
      (setq beg (point))
      (end-of-line)
      (skip-chars-backward "^," beg)
      (when (looking-at "include$")
        ;; Expand in the default-directory of the tags table buffer.
        (push (expand-file-name (convert-standard-filename
                                 (buffer-substring beg (1- (point)))))
              files)))
    (nreverse files)))

;; Empty tags file support.

(defun tags-recognize-empty-tags-table ()
  "Return non-nil if current buffer is empty.
If empty, make buffer-local values of the tags table format variables
that do nothing."
  (and (zerop (buffer-size))
       (mapc (lambda (sym) (set (make-local-variable sym) 'ignore))
	     '(tags-table-files-function
	       tags-completion-table-function
	       find-tag-regexp-search-function
	       find-tag-search-function
	       tags-apropos-function
	       tags-included-tables-function))
       (set (make-local-variable 'verify-tags-table-function)
            (lambda () (zerop (buffer-size))))))

;; Match qualifier functions for tagnames.
;; These functions assume the etags file format defined in etc/ETAGS.EBNF.

;; This might be a neat idea, but it's too hairy at the moment.
;;(defmacro tags-with-syntax (&rest body)
;;   `(with-syntax-table
;;        (with-current-buffer (find-file-noselect (file-of-tag))
;;          (syntax-table))
;;      ,@body))
;;(put 'tags-with-syntax 'edebug-form-spec '(&rest form))

;; exact file name match, i.e. searched tag must match complete file
;; name including directories parts if there are some.
(defun tag-exact-file-name-match-p (tag)
  "Return non-nil if TAG matches complete file name.
Any directory part of the file name is also matched."
  (and (looking-at ",[0-9\n]")
       (save-excursion (backward-char (+ 2 (length tag)))
		       (looking-at "\f\n"))))

;; file name match as above, but searched tag must match the file
;; name not including the directories if there are some.
(defun tag-file-name-match-p (tag)
  "Return non-nil if TAG matches file name, excluding directory part."
  (and (looking-at ",[0-9\n]")
       (save-excursion (backward-char (1+ (length tag)))
		       (looking-at "/"))))

;; this / to detect we are after a directory separator is ok for unix,
;; is there a variable that contains the regexp for directory separator
;; on whatever operating system ?
;; Looks like ms-win will lose here :).

;; t if point is at a tag line that matches TAG exactly.
;; point should be just after a string that matches TAG.
(defun tag-exact-match-p (tag)
  "Return non-nil if current tag line matches TAG exactly.
Point should be just after a string that matches TAG."
  ;; The match is really exact if there is an explicit tag name.
  (or (and (eq (char-after (point)) ?\001)
	   (eq (char-after (- (point) (length tag) 1)) ?\177))
      ;; We are not on the explicit tag name, but perhaps it follows.
      (looking-at (concat "[^\177\n]*\177" (regexp-quote tag) "\001"))))

;; t if point is at a tag line that has an implicit name.
;; point should be just after a string that matches TAG.
(defun tag-implicit-name-match-p (tag)
  "Return non-nil if current tag line has an implicit name.
Point should be just after a string that matches TAG."
  ;; Look at the comment of the make_tag function in lib-src/etags.c for
  ;; a textual description of the four rules.
  (and (string-match "^[^ \t()=,;]+$" tag) ;rule #1
       (looking-at "[ \t()=,;]?\177")	;rules #2 and #4
       (save-excursion
	 (backward-char (1+ (length tag)))
	 (looking-at "[\n \t()=,;]"))))	;rule #3

;; t if point is at a tag line that matches TAG as a symbol.
;; point should be just after a string that matches TAG.
(defun tag-symbol-match-p (tag)
  "Return non-nil if current tag line matches TAG as a symbol.
Point should be just after a string that matches TAG."
  (and (looking-at "\\Sw.*\177") (looking-at "\\S_.*\177")
       (save-excursion
	 (backward-char (1+ (length tag)))
	 (and (looking-at "\\Sw") (looking-at "\\S_")))))

;; t if point is at a tag line that matches TAG as a word.
;; point should be just after a string that matches TAG.
(defun tag-word-match-p (tag)
  "Return non-nil if current tag line matches TAG as a word.
Point should be just after a string that matches TAG."
  (and (looking-at "\\b.*\177")
       (save-excursion (backward-char (length tag))
		       (looking-at "\\b"))))

;; partial file name match, i.e. searched tag must match a substring
;; of the file name (potentially including a directory separator).
(defun tag-partial-file-name-match-p (_tag)
  "Return non-nil if current tag matches file name.
This is a substring match, and it can include directory separators.
Point should be just after a string that matches TAG."
  (and (looking-at ".*,[0-9\n]")
       (save-excursion (beginning-of-line)
                       (backward-char 2)
  		       (looking-at "\f\n"))))

;; t if point is in a tag line with a tag containing TAG as a substring.
(defun tag-any-match-p (_tag)
  "Return non-nil if current tag line contains TAG as a substring."
  (looking-at ".*\177"))

;; t if point is at a tag line that matches RE as a regexp.
(defun tag-re-match-p (re)
  "Return non-nil if current tag line matches regexp RE."
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (and (search-forward "\177" (line-end-position) t)
	   (re-search-backward re bol t)))))

(defcustom tags-loop-revert-buffers nil
  "*Non-nil means tags-scanning loops should offer to reread changed files.
These loops normally read each file into Emacs, but when a file
is already visited, they use the existing buffer.
When this flag is non-nil, they offer to revert the existing buffer
in the case where the file has changed since you visited it."
  :type 'boolean
  :group 'etags)

;;;###autoload
(defun next-file (&optional initialize novisit)
  "Select next file among files in current tags table.

A first argument of t (prefix arg, if interactive) initializes to the
beginning of the list of files in the tags table.  If the argument is
neither nil nor t, it is evalled to initialize the list of files.

Non-nil second argument NOVISIT means use a temporary buffer
 to save time and avoid uninteresting warnings.

Value is nil if the file was already visited;
if the file was newly read in, the value is the filename."
  ;; Make the interactive arg t if there was any prefix arg.
  (interactive (list (if current-prefix-arg t)))
  (cond ((not initialize)
	 ;; Not the first run.
	 )
	((eq initialize t)
	 ;; Initialize the list from the tags table.
	 (save-excursion
	   ;; Visit the tags table buffer to get its list of files.
	   (visit-tags-table-buffer)
	   ;; Copy the list so we can setcdr below, and expand the file
	   ;; names while we are at it, in this buffer's default directory.
	   (setq next-file-list (mapcar 'expand-file-name (tags-table-files)))
	   ;; Iterate over all the tags table files, collecting
	   ;; a complete list of referenced file names.
	   (while (visit-tags-table-buffer t)
	     ;; Find the tail of the working list and chain on the new
	     ;; sublist for this tags table.
	     (let ((tail next-file-list))
	       (while (cdr tail)
		 (setq tail (cdr tail)))
	       ;; Use a copy so the next loop iteration will not modify the
	       ;; list later returned by (tags-table-files).
	       (if tail
		   (setcdr tail (mapcar 'expand-file-name (tags-table-files)))
		 (setq next-file-list (mapcar 'expand-file-name
					      (tags-table-files))))))))
	(t
	 ;; Initialize the list by evalling the argument.
	 (setq next-file-list (eval initialize))))
  (unless next-file-list
    (and novisit
	 (get-buffer " *next-file*")
	 (kill-buffer " *next-file*"))
    (error "All files processed"))
  (let* ((next (car next-file-list))
	 (buffer (get-file-buffer next))
	 (new (not buffer)))
    ;; Advance the list before trying to find the file.
    ;; If we get an error finding the file, don't get stuck on it.
    (setq next-file-list (cdr next-file-list))
    ;; Optionally offer to revert buffers
    ;; if the files have changed on disk.
    (and buffer tags-loop-revert-buffers
	 (not (verify-visited-file-modtime buffer))
	 (y-or-n-p
	  (format
	   (if (buffer-modified-p buffer)
	       "File %s changed on disk.  Discard your edits? "
	     "File %s changed on disk.  Reread from disk? ")
	   next))
	 (with-current-buffer buffer
	   (revert-buffer t t)))
    (if (not (and new novisit))
	(find-file next novisit)
      ;; Like find-file, but avoids random warning messages.
      (switch-to-buffer (get-buffer-create " *next-file*"))
      (kill-all-local-variables)
      (erase-buffer)
      (setq new next)
      (insert-file-contents new nil))
    new))

(defvar tags-loop-operate nil
  "Form for `tags-loop-continue' to eval to change one file.")

(defvar tags-loop-scan
  '(error "%s"
	  (substitute-command-keys
	   "No \\[tags-search] or \\[tags-query-replace] in progress"))
  "Form for `tags-loop-continue' to eval to scan one file.
If it returns non-nil, this file needs processing by evalling
\`tags-loop-operate'.  Otherwise, move on to the next file.")

(defun tags-loop-eval (form)
  "Evaluate FORM and return its result.
Bind `case-fold-search' during the evaluation, depending on the value of
`tags-case-fold-search'."
  (let ((case-fold-search (if (memq tags-case-fold-search '(t nil))
			      tags-case-fold-search
			    case-fold-search)))
    (eval form)))


;;;###autoload
(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).

Two variables control the processing we do on each file: the value of
`tags-loop-scan' is a form to be executed on each file to see if it is
interesting (it returns non-nil if so) and `tags-loop-operate' is a form to
evaluate to operate on an interesting file.  If the latter evaluates to
nil, we exit; otherwise we scan the next file."
  (interactive)
  (let (new
	;; Non-nil means we have finished one file
	;; and should not scan it again.
	file-finished
	original-point
	(messaged nil))
    (while
	(progn
	  ;; Scan files quickly for the first or next interesting one.
	  ;; This starts at point in the current buffer.
	  (while (or first-time file-finished
		     (save-restriction
		       (widen)
		       (not (tags-loop-eval tags-loop-scan))))
	    ;; If nothing was found in the previous file, and
	    ;; that file isn't in a temp buffer, restore point to
	    ;; where it was.
	    (when original-point
	      (goto-char original-point))

	    (setq file-finished nil)
	    (setq new (next-file first-time t))

	    ;; If NEW is non-nil, we got a temp buffer,
	    ;; and NEW is the file name.
	    (when (or messaged
		      (and (not first-time)
			   (> baud-rate search-slow-speed)
			   (setq messaged t)))
	      (message "Scanning file %s..." (or new buffer-file-name)))

	    (setq first-time nil)
	    (setq original-point (if new nil (point)))
	    (goto-char (point-min)))

	  ;; If we visited it in a temp buffer, visit it now for real.
	  (if new
	      (let ((pos (point)))
		(erase-buffer)
		(set-buffer (find-file-noselect new))
		(setq new nil)		;No longer in a temp buffer.
		(widen)
		(goto-char pos))
	    (push-mark original-point t))

	  (switch-to-buffer (current-buffer))

	  ;; Now operate on the file.
	  ;; If value is non-nil, continue to scan the next file.
	  (tags-loop-eval tags-loop-operate))
      (setq file-finished t))
    (and messaged
	 (null tags-loop-operate)
	 (message "Scanning file %s...found" buffer-file-name))))
;;;###autoload (define-key esc-map "," 'tags-loop-continue)

;;;###autoload
(defun tags-search (regexp &optional file-list-form)
  "Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

If FILE-LIST-FORM is non-nil, it should be a form that, when
evaluated, will return a list of file names.  The search will be
restricted to these files.

Aleso see the documentation of the `tags-file-name' variable."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-scan) 're-search-forward)
	   (null tags-loop-operate))
      ;; Continue last tags-search as if by M-,.
      (tags-loop-continue nil)
    (setq tags-loop-scan `(re-search-forward ',regexp nil t)
	  tags-loop-operate nil)
    (tags-loop-continue (or file-list-form t))))

;;;###autoload
(defun tags-query-replace (from to &optional delimited file-list-form)
  "Do `query-replace-regexp' of FROM with TO on all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].
Fourth arg FILE-LIST-FORM non-nil means initialize the replacement loop.
Fifth and sixth arguments START and END are accepted, for compatibility
with `query-replace-regexp', and ignored.

If FILE-LIST-FORM is non-nil, it is a form to evaluate to
produce the list of files to search.

See also the documentation of the variable `tags-file-name'."
  (interactive (query-replace-read-args "Tags query replace (regexp)" t t))
  (setq tags-loop-scan `(let ,(unless (equal from (downcase from))
				'((case-fold-search nil)))
			  (if (re-search-forward ',from nil t)
			      ;; When we find a match, move back
			      ;; to the beginning of it so perform-replace
			      ;; will see it.
			      (goto-char (match-beginning 0))))
	tags-loop-operate `(perform-replace ',from ',to t t ',delimited
					    nil multi-query-replace-map))
  (tags-loop-continue (or file-list-form t)))

(defun tags-complete-tags-table-file (string predicate what) ; Doc string?
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (if (eq what t)
	(all-completions string (tags-table-files) predicate)
      (try-completion string (tags-table-files) predicate))))

;;;###autoload
(defun list-tags (file &optional _next-match)
  "Display list of tags in file FILE.
This searches only the first table in the list, and no included tables.
FILE should be as it appeared in the `etags' command, usually without a
directory specification."
  (interactive (list (completing-read "List tags in file: "
				      'tags-complete-tags-table-file
				      nil t nil)))
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags in file `")
    (tags-with-face 'highlight (princ file))
    (princ "':\n\n")
    (save-excursion
      (let ((first-time t)
	    (gotany nil))
	(while (visit-tags-table-buffer (not first-time))
	  (setq first-time nil)
	  (if (funcall list-tags-function file)
	      (setq gotany t)))
	(or gotany
	    (error "File %s not in current tags tables" file)))))
  (with-current-buffer "*Tags List*"
    (require 'apropos)
    (with-no-warnings
      (apropos-mode))
    (setq buffer-read-only t)))

;;;###autoload
(defun tags-apropos (regexp)
  "Display list of all tags in tags table REGEXP matches."
  (interactive "sTags apropos (regexp): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Click mouse-2 to follow tags.\n\nTags matching regexp `")
    (tags-with-face 'highlight (princ regexp))
    (princ "':\n\n")
    (save-excursion
      (let ((first-time t))
	(while (visit-tags-table-buffer (not first-time))
	  (setq first-time nil)
	  (funcall tags-apropos-function regexp))))
    (etags-tags-apropos-additional regexp))
  (with-current-buffer "*Tags List*"
    (eval-and-compile (require 'apropos))
    (apropos-mode)
    ;; apropos-mode is derived from fundamental-mode and it kills
    ;; all local variables.
    (setq buffer-read-only t)))

;; XXX Kludge interface.

(define-button-type 'tags-select-tags-table
  'action 'select-tags-table-select
  'follow-link t
  'help-echo "RET, t or mouse-2: select tags table")

;; XXX If a file is in multiple tables, selection may get the wrong one.
;;;###autoload
(defun select-tags-table ()
  "Select a tags table file from a menu of those you have already used.
The list of tags tables to select from is stored in `tags-table-set-list';
see the doc of that variable if you want to add names to the list."
  (interactive)
  (pop-to-buffer "*Tags Table List*")
  (setq buffer-read-only nil
	buffer-undo-list t)
  (erase-buffer)
  (let ((set-list tags-table-set-list)
	(desired-point nil)
	b)
    (when tags-table-list
      (setq desired-point (point-marker))
      (setq b (point))
      (princ (mapcar 'abbreviate-file-name tags-table-list) (current-buffer))
      (make-text-button b (point) 'type 'tags-select-tags-table
                        'etags-table (car tags-table-list))
      (insert "\n"))
    (while set-list
      (unless (eq (car set-list) tags-table-list)
	(setq b (point))
	(princ (mapcar 'abbreviate-file-name (car set-list)) (current-buffer))
	(make-text-button b (point) 'type 'tags-select-tags-table
                          'etags-table (car (car set-list)))
	(insert "\n"))
      (setq set-list (cdr set-list)))
    (when tags-file-name
      (or desired-point
          (setq desired-point (point-marker)))
      (setq b (point))
      (insert (abbreviate-file-name tags-file-name))
      (make-text-button b (point) 'type 'tags-select-tags-table
                        'etags-table tags-file-name)
      (insert "\n"))
    (setq set-list (delete tags-file-name
			   (apply 'nconc (cons (copy-sequence tags-table-list)
					       (mapcar 'copy-sequence
						       tags-table-set-list)))))
    (while set-list
      (setq b (point))
      (insert (abbreviate-file-name (car set-list)))
      (make-text-button b (point) 'type 'tags-select-tags-table
                          'etags-table (car set-list))
      (insert "\n")
      (setq set-list (delete (car set-list) set-list)))
    (goto-char (point-min))
    (insert-before-markers
     "Type `t' to select a tags table or set of tags tables:\n\n")
    (if desired-point
	(goto-char desired-point))
    (set-window-start (selected-window) 1 t))
  (set-buffer-modified-p nil)
  (select-tags-table-mode))

(defvar select-tags-table-mode-map ; Doc string?
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "t" 'push-button)
    (define-key map " " 'next-line)
    (define-key map "\^?" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'select-tags-table-quit)
    map))

(define-derived-mode select-tags-table-mode special-mode "Select Tags Table"
  "Major mode for choosing a current tags table among those already loaded."
  (setq buffer-read-only t))

(defun select-tags-table-select (button)
  "Select the tags table named on this line."
  (interactive (list (or (button-at (line-beginning-position))
                         (error "No tags table on current line"))))
  (let ((name (button-get button 'etags-table)))
    (visit-tags-table name)
    (select-tags-table-quit)
    (message "Tags table now %s" name)))

(defun select-tags-table-quit ()
  "Kill the buffer and delete the selected window."
  (interactive)
  (quit-window t (selected-window)))

;;;###autoload
(defun complete-tag ()
  "Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)."
  (interactive)
  (or tags-table-list
      tags-file-name
      (error "%s"
	     (substitute-command-keys
	      "No tags table loaded; try \\[visit-tags-table]")))
  (let ((comp-data (tags-completion-at-point-function)))
    (if (null comp-data)
	(error "Nothing to complete")
      (completion-in-region (car comp-data) (cadr comp-data)
			    (nth 2 comp-data)
			    (plist-get (nthcdr 3 comp-data) :predicate)))))

(dolist (x '("^No tags table in use; use .* to select one$"
	     "^There is no default tag$"
	     "^No previous tag locations$"
	     "^File .* is not a valid tags table$"
	     "^No \\(more \\|\\)tags \\(matching\\|containing\\) "
	     "^Rerun etags: `.*' not found in "
	     "^All files processed$"
	     "^No .* or .* in progress$"
	     "^File .* not in current tags tables$"
	     "^No tags table loaded"
	     "^Nothing to complete$"))
	(add-to-list 'debug-ignored-errors x))

(provide 'etags)

;;; etags.el ends here
