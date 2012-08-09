;;; semantic/db-ebrowse.el --- Semanticdb backend using ebrowse.

;; Copyright (C) 2005-2012  Free Software Foundation, Inc.

;; Authors: Eric M. Ludlam <zappo@gnu.org>
;;	Joakim Verona
;; Keywords: tags

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
;; This program was started by Eric Ludlam, and Joakim Verona finished
;; the implementation by adding searches and fixing bugs.
;;
;; Read in custom-created ebrowse BROWSE files into a semanticdb back
;; end.
;;
;; Add these databases to the 'system' search.
;; Possibly use ebrowse for local parsing too.
;;
;; When real details are needed out of the tag system from ebrowse,
;; we will need to delve into the originating source and parse those
;; files the usual way.
;;
;; COMMANDS:
;; `semanticdb-create-ebrowse-database' - Call EBROWSE to create a
;;       system database for some directory.  In general, use this for
;;       system libraries, such as /usr/include, or include directories
;;       large software projects.
;;       Customize `semanticdb-ebrowse-file-match' to make sure the correct
;;       file extensions are matched.
;;
;; `semanticdb-load-ebrowse-caches' - Load all the EBROWSE caches from
;;       your semanticdb system database directory.  Once they are
;;       loaded, they become searchable as omnipotent databases for
;;       all C++ files.  This is called automatically by semantic-load.
;;       Call it a second time to refresh the Emacs DB with the file.
;;

(require 'ebrowse)
(require 'semantic)
(require 'semantic/db-file)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  (require 'semantic/find))

(declare-function semantic-add-system-include "semantic/dep")

;;; Code:
(defvar semanticdb-ebrowse-default-file-name "BROWSE"
  "The EBROWSE file name used for system caches.")

(defcustom semanticdb-ebrowse-file-match "\\.\\(hh?\\|HH?\\|hpp\\)"
  "Regular expression matching file names for ebrowse to parse.
This expression should exclude C++ headers that have no extension.
By default, include only headers since the semantic use of EBrowse
is only for searching via semanticdb, and thus only headers would
be searched."
  :group 'semanticdb
  :type 'string)

;;; SEMANTIC Database related Code
;;; Classes:
(defclass semanticdb-table-ebrowse (semanticdb-table)
  ((major-mode :initform c++-mode)
   (ebrowse-tree :initform nil
		 :initarg :ebrowse-tree
		 :documentation
		 "The raw ebrowse tree for this file."
		 )
   (global-extract :initform nil
		   :initarg :global-extract
		   :documentation
		   "Table of ebrowse tags specific to this file.
This table is composited from the ebrowse *Globals* section.")
   )
  "A table for returning search results from ebrowse.")

(defclass semanticdb-project-database-ebrowse
  (semanticdb-project-database)
  ((new-table-class :initform semanticdb-table-ebrowse
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (system-include-p :initform nil
		     :initarg :system-include
		     :documentation
		     "Flag indicating this database represents a system include directory.")
   (ebrowse-struct :initform nil
		   :initarg :ebrowse-struct
		   )
   )
  "Semantic Database deriving tags using the EBROWSE tool.
EBROWSE is a C/C++ parser for use with `ebrowse' Emacs program.")


(defun semanticdb-ebrowse-C-file-p (file)
  "Is FILE a C or C++ file?"
  (or (string-match semanticdb-ebrowse-file-match file)
      (and (string-match "/\\w+$" file)
	   (not (file-directory-p file))
	   (let ((tmp (get-buffer-create "*semanticdb-ebrowse-tmp*")))
	     (with-current-buffer tmp
	       (condition-case nil
		   (insert-file-contents file nil 0 100 t)
		 (error (insert-file-contents file nil nil nil t)))
	       (goto-char (point-min))
	       (looking-at "\\s-*/\\(\\*\\|/\\)")
	       ))
	   )))

(defun semanticdb-create-ebrowse-database (dir)
  "Create an EBROWSE database for directory DIR.
The database file is stored in ~/.semanticdb, or whichever directory
is specified by `semanticdb-default-save-directory'."
  (interactive "DDirectory: ")
  (setq dir (file-name-as-directory dir)) ;; for / on end
  (let* ((savein (semanticdb-ebrowse-file-for-directory dir))
	 (filebuff (get-buffer-create "*SEMANTICDB EBROWSE TMP*"))
	 (files (directory-files (expand-file-name dir) t))
	 (mma auto-mode-alist)
	 (regexp nil)
	 )
    ;; Create the input to the ebrowse command
    (with-current-buffer filebuff
      (buffer-disable-undo filebuff)
      (setq default-directory (expand-file-name dir))

      ;;; @TODO - convert to use semanticdb-collect-matching-filenames
      ;; to get the file names.


      (mapc (lambda (f)
	      (when (semanticdb-ebrowse-C-file-p f)
		(insert f)
		(insert "\n")))
	    files)
      ;; Cleanup the ebrowse output buffer.
      (with-current-buffer (get-buffer-create "*EBROWSE OUTPUT*")
	(erase-buffer))
      ;; Call the EBROWSE command.
      (message "Creating ebrowse file: %s ..." savein)
      (call-process-region (point-min) (point-max)
			   "ebrowse" nil "*EBROWSE OUTPUT*" nil
			   (concat "--output-file=" savein)
			   "--very-verbose")
      )
    ;; Create a short LOADER program for loading in this database.
    (let* ((lfn (concat savein "-load.el"))
	   (lf (find-file-noselect lfn)))
      (with-current-buffer lf
	(erase-buffer)
	(insert "(semanticdb-ebrowse-load-helper \""
		(expand-file-name dir)
		"\")\n")
	(save-buffer)
	(kill-buffer (current-buffer)))
      (message "Creating ebrowse file: %s ... done" savein)
      ;; Reload that database
      (load lfn nil t)
      )))

(defun semanticdb-load-ebrowse-caches ()
  "Load all semanticdb controlled EBROWSE caches."
  (interactive)
  (let ((f (directory-files semanticdb-default-save-directory
			    t (concat semanticdb-ebrowse-default-file-name "-load.el$") t)))
    (while f
      (load (car f) nil t)
      (setq f (cdr f)))
    ))

(defun semanticdb-ebrowse-load-helper (directory)
  "Create the semanticdb database via ebrowse for directory.
If DIRECTORY is found to be defunct, it won't load the DB, and will
warn instead."
  (if (file-directory-p directory)
      (semanticdb-create-database semanticdb-project-database-ebrowse
				  directory)
    (let* ((BF (semanticdb-ebrowse-file-for-directory directory))
	   (BFL (concat BF "-load.el"))
	   (BFLB (concat BF "-load.el~")))
      (save-window-excursion
	(with-output-to-temp-buffer "*FILES TO DELETE*"
	  (princ "The following BROWSE files are obsolete.\n\n")
	  (princ BF)
	  (princ "\n")
	  (princ BFL)
	  (princ "\n")
	  (when (file-exists-p BFLB)
	    (princ BFLB)
	    (princ "\n"))
	  )
	(when (y-or-n-p (format
			 "Warning: Obsolete BROWSE file for: %s\nDelete? "
			 directory))
	  (delete-file BF)
	  (delete-file BFL)
	  (when (file-exists-p BFLB)
	    (delete-file BFLB))
	  )))))

;JAVE this just instantiates a default empty ebrowse struct?
; how would new instances wind up here?
; the ebrowse class isn't singleton, unlike the emacs lisp one
(defvar-mode-local c++-mode semanticdb-project-system-databases
  ()
  "Search Ebrowse for symbols.")

(defmethod semanticdb-needs-refresh-p ((table semanticdb-table-ebrowse))
  "EBROWSE database do not need to be refreshed.

JAVE: stub for needs-refresh, because, how do we know if BROWSE files
      are out of date?

EML: Our database should probably remember the timestamp/checksum of
     the most recently read EBROWSE file, and use that."
  nil
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; EBROWSE code
;;
;; These routines deal with part of the ebrowse interface.
(defun semanticdb-ebrowse-file-for-directory (dir)
  "Return the file name for DIR where the ebrowse BROWSE file is.
This file should reside in `semanticdb-default-save-directory'."
  (let* ((semanticdb-default-save-directory
	  semanticdb-default-save-directory)
	 (B (semanticdb-file-name-directory
	     'semanticdb-project-database-file
	     (concat (expand-file-name dir)
		     semanticdb-ebrowse-default-file-name)))
	 )
    B))

(defun semanticdb-ebrowse-get-ebrowse-structure (dir)
  "Return the ebrowse structure for directory DIR.
This assumes semantic manages the BROWSE files, so they are assumed to live
where semantic cache files live, depending on your settings.

For instance: /home/<username>/.semanticdb/!usr!include!BROWSE"
  (let* ((B (semanticdb-ebrowse-file-for-directory dir))
	 (buf (get-buffer-create "*semanticdb ebrowse*")))
    (message "semanticdb-ebrowse %s" B)
    (when (file-exists-p B)
      (set-buffer buf)
      (buffer-disable-undo buf)
      (erase-buffer)
      (insert-file-contents B)
      (let ((ans nil)
	    (efcn (symbol-function 'ebrowse-show-progress)))
	(fset 'ebrowse-show-progress #'(lambda (&rest junk) nil))
	(unwind-protect ;; Protect against errors w/ ebrowse
	    (setq ans (list B (ebrowse-read)))
	  ;; These items must always happen
	  (erase-buffer)
	  (fset 'ebrowse-show-fcn efcn)
	  )
	ans))))

;;; Methods for creating a database or tables
;;
(defmethod semanticdb-create-database :STATIC ((dbeC semanticdb-project-database-ebrowse)
					       directory)
  "Create a new semantic database for DIRECTORY based on ebrowse.
If there is no database for DIRECTORY available, then
{not implemented yet} create one.  Return nil if that is not possible."
  ;; MAKE SURE THAT THE FILE LOADED DOESN'T ALREADY EXIST.
  (require 'semantic/dep)
  (let ((dbs semanticdb-database-list)
	(found nil))
    (while (and (not found) dbs)
      (when (semanticdb-project-database-ebrowse-p (car dbs))
	(when (string= (oref (car dbs) reference-directory) directory)
	  (setq found (car dbs))))
      (setq dbs (cdr dbs)))
    ;;STATIC means DBE can't be used as object, only as a class
    (let* ((ebrowse-data (semanticdb-ebrowse-get-ebrowse-structure directory))
	   (dat (car (cdr ebrowse-data)))
	   (ebd (car dat))
	   (db nil)
	   (default-directory directory)
	   )
      (if found
	  (setq db found)
	(setq db (make-instance
		  dbeC
		  directory
		  :ebrowse-struct ebd
		  ))
	(oset db reference-directory directory))

      ;; Once we recycle or make a new DB, refresh the
      ;; contents from the BROWSE file.
      (oset db tables nil)
      ;; only possible after object creation, tables inited to nil.
      (semanticdb-ebrowse-strip-trees db dat)

      ;; Once our database is loaded, if we are a system DB, we
      ;; add ourselves to the include list for C++.
      (semantic-add-system-include directory 'c++-mode)
      (semantic-add-system-include directory 'c-mode)

      db)))

(defmethod semanticdb-ebrowse-strip-trees  ((dbe semanticdb-project-database-ebrowse)
						    data)
  "For the ebrowse database DBE, strip all tables from DATA."
;JAVE what it actually seems to do is split the original tree in "tables" associated with files
; im not sure it actually works:
;   the filename slot sometimes gets to be nil,
;      apparently for classes which definition can't be found, yet needs to be included in the tree
;      like library baseclasses
;   a file can define several classes
  (let ((T (car (cdr data))));1st comes a header, then the tree
    (while T

      (let* ((tree (car T))
	     (class (ebrowse-ts-class tree)); root class of tree
	     ;; Something funny going on with this file thing...
             (filename (or (ebrowse-cs-source-file class)
			   (ebrowse-cs-file class)))
	     )
	(cond
	 ((ebrowse-globals-tree-p tree)
	  ;; We have the globals tree.. save this special.
	  (semanticdb-ebrowse-add-globals-to-table dbe tree)
	  )
	 (t
	  ;; ebrowse will collect all the info from multiple files
	  ;; into one tree.  Semantic wants all the bits to be tied
	  ;; into different files.  We need to do a full dissociation
	  ;; into semantic parsable tables.
	  (semanticdb-ebrowse-add-tree-to-table dbe tree)
	  ))
      (setq T (cdr T))))
    ))

;;; Filename based methods
;;
(defun semanticdb-ebrowse-add-globals-to-table (dbe tree)
  "For database DBE, add the ebrowse TREE into the table."
  (if (or (not (ebrowse-ts-p tree))
	  (not (ebrowse-globals-tree-p tree)))
      (signal 'wrong-type-argument (list 'ebrowse-ts-p tree)))

  (let* ((class (ebrowse-ts-class tree))
	 (fname (or (ebrowse-cs-source-file class)
		    (ebrowse-cs-file class)
		    ;; Not def'd here, assume our current
		    ;; file
		    (concat default-directory "/unknown-proxy.hh")))
	 (vars (ebrowse-ts-member-functions tree))
	 (fns (ebrowse-ts-member-variables tree))
	 (toks nil)
	 )
    (while vars
      (let ((nt (semantic-tag (ebrowse-ms-name (car vars))
			      'variable))
	    (defpoint (ebrowse-bs-point class)))
	(when defpoint
	  (semantic--tag-set-overlay nt
				     (vector defpoint defpoint)))
	(setq toks (cons nt toks)))
      (setq vars (cdr vars)))
    (while fns
      (let ((nt (semantic-tag (ebrowse-ms-name (car fns))
			      'function))
	    (defpoint (ebrowse-bs-point class)))
	(when defpoint
	  (semantic--tag-set-overlay nt
				     (vector defpoint defpoint)))
	(setq toks (cons nt toks)))
      (setq fns (cdr fns)))

    ))

(defun semanticdb-ebrowse-add-tree-to-table (dbe tree &optional fname baseclasses)
  "For database DBE, add the ebrowse TREE into the table for FNAME.
Optional argument BASECLASSES specifies a baseclass to the tree being provided."
  (if (not (ebrowse-ts-p tree))
      (signal 'wrong-type-argument (list 'ebrowse-ts-p tree)))

  ;; Strategy overview:
  ;; 1) Calculate the filename for this tree.
  ;; 2) Find a matching namespace in TAB, or create a new one.
  ;; 3) Fabricate a tag proxy for CLASS
  ;; 4) Add it to the namespace
  ;; 5) Add subclasses

  ;; 1 - Find the filename
  (if (not fname)
      (setq fname (or (ebrowse-cs-source-file (ebrowse-ts-class tree))
		      (ebrowse-cs-file (ebrowse-ts-class tree))
		      ;; Not def'd here, assume our current
		      ;; file
		      (concat default-directory "/unknown-proxy.hh"))))

  (let* ((tab (or (semanticdb-file-table dbe fname)
		  (semanticdb-create-table dbe fname)))
	 (class (ebrowse-ts-class tree))
	 (scope (ebrowse-cs-scope class))
	 (ns (when scope (split-string scope ":" t)))
	 (nst nil)
	 (cls nil)
	 )

    ;; 2 - Get the namespace tag
    (when ns
      (let ((taglst (if (slot-boundp tab 'tags) (oref tab tags) nil)))
	(setq nst (semantic-find-first-tag-by-name (car ns) taglst))
	(when (not nst)
	  (setq nst (semantic-tag (car ns) 'type :type "namespace"))
	  (oset tab tags (cons nst taglst))
	  )))

    ;; 3 - Create a proxy tg.
    (setq cls (semantic-tag (ebrowse-cs-name class)
			    'type
			    :type "class"
			    :superclasses baseclasses
			    :faux t
			    :filename fname
			    ))
    (let ((defpoint (ebrowse-bs-point class)))
      (when defpoint
	(semantic--tag-set-overlay cls
				   (vector defpoint defpoint))))

    ;; 4 - add to namespace
    (if nst
	(semantic-tag-put-attribute
	 nst :members (cons cls (semantic-tag-get-attribute nst :members)))
      (oset tab tags (cons cls (when (slot-boundp tab 'tags)
				 (oref tab tags)))))

    ;; 5 - Subclasses
    (let* ((subclass (ebrowse-ts-subclasses tree))
	   (pname (ebrowse-cs-name class)))
      (when (ebrowse-cs-scope class)
	(setq pname (concat (mapconcat (lambda (a) a) (cdr ns) "::") "::" pname)))

      (while subclass
	(let* ((scc (ebrowse-ts-class (car subclass)))
	       (fname (or (ebrowse-cs-source-file scc)
			  (ebrowse-cs-file scc)
			  ;; Not def'd here, assume our current
			  ;; file
			  fname
			  )))
	  (when fname
	    (semanticdb-ebrowse-add-tree-to-table
	     dbe (car subclass) fname pname)))
	(setq subclass (cdr subclass))))
    ))

;;;
;; Overload for converting the simple faux tag into something better.
;;
(defmethod semanticdb-normalize-tags ((obj semanticdb-table-ebrowse) tags)
  "Convert in Ebrowse database OBJ a list of TAGS into a complete tag.
The default tag provided by searches exclude many features of a
semantic parsed tag.  Look up the file for OBJ, and match TAGS
against a semantic parsed tag that has all the info needed, and
return that."
  (let ((tagret nil)
	)
    ;; SemanticDB will automatically create a regular database
    ;; on top of the file just loaded by ebrowse during the set
    ;; buffer.  Fetch that table, and use it's tag list to look
    ;; up the tag we just got, and thus turn it into a full semantic
    ;; tag.
    (while tags
      (let ((tag (car tags)))
	(save-excursion
	  (semanticdb-set-buffer obj)
	  (let ((ans nil))
	    ;; Gee, it would be nice to do this, but ebrowse LIES.  Oi.
	    (when (semantic-tag-with-position-p tag)
	      (goto-char (semantic-tag-start tag))
	      (let ((foundtag (semantic-current-tag)))
		;; Make sure the discovered tag is the same as what we started with.
		(when (string= (semantic-tag-name tag)
			       (semantic-tag-name foundtag))
		  ;; We have a winner!
		  (setq ans foundtag))))
	    ;; Sometimes ebrowse lies.  Do a generic search
	    ;; to find it within this file.
	    (when (not ans)
	      ;; We might find multiple hits for this tag, and we have no way
	      ;; of knowing which one the user wanted.  Return the first one.
	      (setq ans (semantic-deep-find-tags-by-name
			 (semantic-tag-name tag)
			 (semantic-fetch-tags))))
	    (if (semantic-tag-p ans)
		(setq tagret (cons ans tagret))
	      (setq tagret (append ans tagret)))
	    ))
	(setq tags (cdr tags))))
    tagret))

(defmethod semanticdb-normalize-one-tag ((obj semanticdb-table-ebrowse) tag)
  "Convert in Ebrowse database OBJ one TAG into a complete tag.
The default tag provided by searches exclude many features of a
semantic parsed tag.  Look up the file for OBJ, and match TAG
against a semantic parsed tag that has all the info needed, and
return that."
  (let ((tagret nil)
	(objret nil))
    ;; SemanticDB will automatically create a regular database
    ;; on top of the file just loaded by ebrowse during the set
    ;; buffer.  Fetch that table, and use it's tag list to look
    ;; up the tag we just got, and thus turn it into a full semantic
    ;; tag.
    (save-excursion
      (semanticdb-set-buffer obj)
      (setq objret semanticdb-current-table)
      (when (not objret)
	;; What to do??
	(debug))
      (let ((ans nil))
	;; Gee, it would be nice to do this, but ebrowse LIES.  Oi.
	(when (semantic-tag-with-position-p tag)
	  (goto-char (semantic-tag-start tag))
	  (let ((foundtag (semantic-current-tag)))
	    ;; Make sure the discovered tag is the same as what we started with.
	    (when (string= (semantic-tag-name tag)
			   (semantic-tag-name foundtag))
	      ;; We have a winner!
	      (setq ans foundtag))))
	;; Sometimes ebrowse lies.  Do a generic search
	;; to find it within this file.
	(when (not ans)
	  ;; We might find multiple hits for this tag, and we have no way
	  ;; of knowing which one the user wanted.  Return the first one.
	  (setq ans (semantic-deep-find-tags-by-name
		     (semantic-tag-name tag)
		     (semantic-fetch-tags))))
	(if (semantic-tag-p ans)
	    (setq tagret ans)
	  (setq tagret (car ans)))
	))
    (cons objret tagret)))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-ebrowse) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  ;;(message "semanticdb-find-tags-by-name-method name -- %s" name)
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; If we ever need to do something special, add here.
    ;; Since ebrowse tags are converted into semantic tags, we can
    ;; get away with this sort of thing.
    (call-next-method)
    )
  )

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-ebrowse) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (call-next-method)
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-ebrowse) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (call-next-method)
    ))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-ebrowse) class &optional tags)
  "In TABLE, find all occurrences of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;

(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-ebrowse) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for ebrowse."
  ;;(semanticdb-find-tags-by-name-method table name tags)
  (call-next-method))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-ebrowse) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for ebrowse."
  ;;(semanticdb-find-tags-by-name-regexp-method table regex tags)
  (call-next-method))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-ebrowse) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for ebrowse."
  ;;(semanticdb-find-tags-for-completion-method table prefix tags)
  (call-next-method))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-ebrowse) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; Ebrowse collects all this type of stuff together for us.
    ;; but we can't use it.... yet.
    nil
    ))

(provide 'semantic/db-ebrowse)

;;; semantic/db-ebrowse.el ends here
