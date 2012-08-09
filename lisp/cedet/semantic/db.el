;;; semantic/db.el --- Semantic tag database manager

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
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
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.
;;

;;; Code:

(require 'eieio-base)
(require 'semantic)

(declare-function semantic-lex-spp-save-table "semantic/lex-spp")

;;; Variables:
(defgroup semanticdb nil
  "Parser Generator Persistent Database interface."
  :group 'semantic)

(defvar semanticdb-database-list nil
  "List of all active databases.")

(defvar semanticdb-new-database-class 'semanticdb-project-database-file
  "The default type of database created for new files.
This can be changed on a per file basis, so that some directories
are saved using one mechanism, and some directories via a different
mechanism.")
(make-variable-buffer-local 'semanticdb-new-database-class)

(defvar semanticdb-default-find-index-class 'semanticdb-find-search-index
  "The default type of search index to use for a `semanticdb-table's.
This can be changed to try out new types of search indices.")
(make-variable-buffer-local 'semanticdb-default-find=index-class)

;;;###autoload
(defvar semanticdb-current-database nil
  "For a given buffer, this is the currently active database.")
(make-variable-buffer-local 'semanticdb-current-database)

;;;###autoload
(defvar semanticdb-current-table nil
  "For a given buffer, this is the currently active database table.")
(make-variable-buffer-local 'semanticdb-current-table)

;;; ABSTRACT CLASSES
;;
(defclass semanticdb-abstract-table ()
  ((parent-db ;; :initarg :parent-db
    ;; Do not set an initarg, or you get circular writes to disk.
	      :documentation "Database Object containing this table.")
   (major-mode :initarg :major-mode
	       :initform nil
	       :documentation "Major mode this table belongs to.
Sometimes it is important for a program to know if a given table has the
same major mode as the current buffer.")
   (tags :initarg :tags
	 :accessor semanticdb-get-tags
	 :printer semantic-tag-write-list-slot-value
	 :documentation "The tags belonging to this table.")
   (index :type semanticdb-abstract-search-index
	  :documentation "The search index.
Used by semanticdb-find to store additional information about
this table for searching purposes.

Note: This index will not be saved in a persistent file.")
   (cache :type list
	  :initform nil
	  :documentation "List of cache information for tools.
Any particular tool can cache data to a database at runtime
with `semanticdb-cache-get'.

Using a semanticdb cache does not save any information to a file,
so your cache will need to be recalculated at runtime.  Caches can be
referenced even when the file is not in a buffer.

Note: This index will not be saved in a persistent file.")
   )
  "A simple table for semantic tags.
This table is the root of tables, and contains the minimum needed
for a new table not associated with a buffer."
  :abstract t)

(defmethod semanticdb-in-buffer-p ((obj semanticdb-abstract-table))
  "Return a nil, meaning abstract table OBJ is not in a buffer."
  nil)

(defmethod semanticdb-get-buffer ((obj semanticdb-abstract-table))
  "Return a buffer associated with OBJ.
If the buffer is not in memory, load it with `find-file-noselect'."
  nil)

(defmethod semanticdb-full-filename ((obj semanticdb-abstract-table))
  "Fetch the full filename that OBJ refers to.
Abstract tables do not have file names associated with them."
  nil)

(defmethod semanticdb-dirty-p ((obj semanticdb-abstract-table))
  "Return non-nil if OBJ is 'dirty'."
  nil)

(defmethod semanticdb-set-dirty ((obj semanticdb-abstract-table))
  "Mark the abstract table OBJ dirty.
Abstract tables can not be marked dirty, as there is nothing
for them to synchronize against."
  ;; The abstract table can not be dirty.
  nil)

(defmethod semanticdb-normalize-tags ((obj semanticdb-abstract-table) tags)
  "For the table OBJ, convert a list of TAGS, into standardized form.
The default is to return TAGS.
Some databases may default to searching and providing simplified tags
based on whichever technique used.  This method provides a hook for
them to convert TAG into a more complete form."
  tags)

(defmethod semanticdb-normalize-one-tag ((obj semanticdb-abstract-table) tag)
  "For the table OBJ, convert a TAG, into standardized form.
This method returns a list of the form (DATABASE . NEWTAG).

The default is to just return (OBJ TAG).

Some databases may default to searching and providing simplified tags
based on whichever technique used.  This method provides a hook for
them to convert TAG into a more complete form."
  (cons obj tag))

(defmethod object-print ((obj semanticdb-abstract-table) &rest strings)
  "Pretty printer extension for `semanticdb-table'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d tags)"
		       (length (semanticdb-get-tags obj))
		       )
	       strings)))

;;; Index Cache
;;
(defclass semanticdb-abstract-search-index ()
  ((table :initarg :table
	  :type semanticdb-abstract-table
	  :documentation "XRef to the table this belongs to.")
   )
  "A place where semanticdb-find can store search index information.
The search index will store data about which other tables might be
needed, or perhaps create hash or index tables for the current buffer."
  :abstract t)

(defmethod semanticdb-get-table-index ((obj semanticdb-abstract-table))
  "Return the search index for the table OBJ.
If one doesn't exist, create it."
  (if (slot-boundp obj 'index)
      (oref obj index)
    (let ((idx nil))
      (setq idx (funcall semanticdb-default-find-index-class
			 (concat (object-name obj) " index")
			 ;; Fill in the defaults
		         :table obj
			 ))
      (oset obj index idx)
      idx)))

(defmethod semanticdb-synchronize ((idx semanticdb-abstract-search-index)
				   new-tags)
  "Synchronize the search index IDX with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((idx semanticdb-abstract-search-index)
					   new-tags)
  "Synchronize the search index IDX with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )


;;; SEARCH RESULTS TABLE
;;
;; Needed for system databases that may not provide
;; a semanticdb-table associated with a file.
;;
(defclass semanticdb-search-results-table (semanticdb-abstract-table)
  (
   )
  "Table used for search results when there is no file or table association.
Examples include search results from external sources such as from
Emacs's own symbol table, or from external libraries.")

(defmethod semanticdb-refresh-table ((obj semanticdb-search-results-table) &optional force)
  "If the tag list associated with OBJ is loaded, refresh it.
This will call `semantic-fetch-tags' if that file is in memory."
  nil)

;;; CONCRETE TABLE CLASSES
;;
(defclass semanticdb-table (semanticdb-abstract-table)
  ((file :initarg :file
	 :documentation "File name relative to the parent database.
This is for the file whose tags are stored in this TABLE object.")
   (buffer :initform nil
	   :documentation "The buffer associated with this table.
If nil, the table's buffer is no in Emacs.  If it has a value, then
it is in Emacs.")
   (dirty :initform nil
	  :documentation
	  "Non nil if this table needs to be `Saved'.")
   (db-refs :initform nil
	    :documentation
	    "List of `semanticdb-table' objects referring to this one.
These aren't saved, but are instead recalculated after load.
See the file semantic/db-ref.el for how this slot is used.")
   (pointmax :initarg :pointmax
	     :initform nil
	     :documentation "Size of buffer when written to disk.
Checked on retrieval to make sure the file is the same.")
   (fsize :initarg :fsize
	  :initform nil
	  :documentation "Size of the file when it was last referenced.
Checked when deciding if a loaded table needs updating from changes
outside of Semantic's control.")
   (lastmodtime :initarg :lastmodtime
		:initform nil
		:documentation "Last modification time of the file referenced.
Checked when deciding if a loaded table needs updating from changes outside of
Semantic's control.")
   ;; @todo - need to add `last parsed time', so we can also have
   ;; refresh checks if spp tables or the parser gets rebuilt.
   (unmatched-syntax :initarg :unmatched-syntax
		     :documentation
		     "List of vectors specifying unmatched syntax.")

   (lexical-table :initarg :lexical-table
		  :initform nil
		  :printer semantic-lex-spp-table-write-slot-value
		  :documentation
		  "Table that might be needed by the lexical analyzer.
For C/C++, the C preprocessor macros can be saved here.")
   )
  "A single table of tags derived from file.")

(defmethod semanticdb-in-buffer-p ((obj semanticdb-table))
  "Return a buffer associated with OBJ.
If the buffer is in memory, return that buffer."
  (let ((buff (oref obj buffer)))
    (if (buffer-live-p buff)
	buff
      (oset obj buffer nil))))

(defmethod semanticdb-get-buffer ((obj semanticdb-table))
  "Return a buffer associated with OBJ.
If the buffer is in memory, return that buffer.
If the buffer is not in memory, load it with `find-file-noselect'."
  (or (semanticdb-in-buffer-p obj)
      ;; Save match data to protect against odd stuff in mode hooks.
      (save-match-data
	(find-file-noselect (semanticdb-full-filename obj) t))))

(defmethod semanticdb-set-buffer ((obj semanticdb-table))
  "Set the current buffer to be a buffer owned by OBJ.
If OBJ's file is not loaded, read it in first."
  (set-buffer (semanticdb-get-buffer obj)))

(defmethod semanticdb-full-filename ((obj semanticdb-table))
  "Fetch the full filename that OBJ refers to."
  (expand-file-name (oref obj file)
		    (oref (oref obj parent-db) reference-directory)))

(defmethod semanticdb-dirty-p ((obj semanticdb-table))
  "Return non-nil if OBJ is 'dirty'."
  (oref obj dirty))

(defmethod semanticdb-set-dirty ((obj semanticdb-table))
  "Mark the abstract table OBJ dirty."
  (oset obj dirty t)
  )

(defmethod object-print ((obj semanticdb-table) &rest strings)
  "Pretty printer extension for `semanticdb-table'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (if (oref obj dirty) ", DIRTY" "") strings)))

;;; DATABASE BASE CLASS
;;
(defclass semanticdb-project-database (eieio-instance-tracker)
  ((tracking-symbol :initform semanticdb-database-list)
   (reference-directory :type string
			:documentation "Directory this database refers to.
When a cache directory is specified, then this refers to the directory
this database contains symbols for.")
   (new-table-class :initform semanticdb-table
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (cache :type list
	  :initform nil
	  :documentation "List of cache information for tools.
Any particular tool can cache data to a database at runtime
with `semanticdb-cache-get'.

Using a semanticdb cache does not save any information to a file,
so your cache will need to be recalculated at runtime.

Note: This index will not be saved in a persistent file.")
   (tables :initarg :tables
	   :type list
	   ;; Need this protection so apps don't try to access
	   ;; the tables without using the accessor.
	   :accessor semanticdb-get-database-tables
	   :protection :protected
	   :documentation "List of `semantic-db-table' objects."))
  "Database of file tables.")

(defmethod semanticdb-full-filename ((obj semanticdb-project-database))
  "Fetch the full filename that OBJ refers to.
Abstract tables do not have file names associated with them."
  nil)

(defmethod semanticdb-dirty-p ((DB semanticdb-project-database))
  "Return non-nil if DB is 'dirty'.
A database is dirty if the state of the database changed in a way
where it may need to resynchronize with some persistent storage."
  (let ((dirty nil)
	(tabs (oref DB tables)))
    (while (and (not dirty) tabs)
      (setq dirty (semanticdb-dirty-p (car tabs)))
      (setq tabs (cdr tabs)))
    dirty))

(defmethod object-print ((obj semanticdb-project-database) &rest strings)
  "Pretty printer extension for `semanticdb-project-database'.
Adds the number of tables in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d tables%s)"
		       (length (semanticdb-get-database-tables obj))
		       (if (semanticdb-dirty-p obj)
			   " DIRTY" "")
		       )
	       strings)))

(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database) directory)
  "Create a new semantic database of class DBC for DIRECTORY and return it.
If a database for DIRECTORY has already been created, return it.
If DIRECTORY doesn't exist, create a new one."
  (let ((db (semanticdb-directory-loaded-p directory)))
    (unless db
      (setq db (semanticdb-project-database
		(file-name-nondirectory directory)
		:tables nil))
      ;; Set this up here.   We can't put it in the constructor because it
      ;; would be saved, and we want DB files to be portable.
      (oset db reference-directory (file-truename directory)))
    db))

(defmethod semanticdb-flush-database-tables ((db semanticdb-project-database))
  "Reset the tables in DB to be empty."
  (oset db tables nil))

(defmethod semanticdb-create-table ((db semanticdb-project-database) file)
  "Create a new table in DB for FILE and return it.
The class of DB contains the class name for the type of table to create.
If the table for FILE exists, return it.
If the table for FILE does not exist, create one."
  (let ((newtab (semanticdb-file-table db file)))
    (unless newtab
      ;; This implementation will satisfy autoloaded classes
      ;; for tables.
      (setq newtab (funcall (oref db new-table-class)
			    (file-name-nondirectory file)
			    :file (file-name-nondirectory file)
			    ))
      (oset newtab parent-db db)
      (object-add-to-list db 'tables newtab t))
    newtab))

(defmethod semanticdb-file-table ((obj semanticdb-project-database) filename)
  "From OBJ, return FILENAME's associated table object."
  (object-assoc (file-relative-name (file-truename filename)
  				    (oref obj reference-directory))
		'file (oref obj tables)))

;; DATABASE FUNCTIONS
(defun semanticdb-get-database (filename)
  "Get a database for FILENAME.
If one isn't found, create one."
  (semanticdb-create-database semanticdb-new-database-class (file-truename filename)))

(defun semanticdb-directory-loaded-p (path)
  "Return the project belonging to PATH if it was already loaded."
  (eieio-instance-tracker-find path 'reference-directory 'semanticdb-database-list))

(defun semanticdb-create-table-for-file (filename)
  "Initialize a database table for FILENAME, and return it.
If FILENAME exists in the database already, return that.
If there is no database for the table to live in, create one."
  (let ((cdb nil)
	(tbl nil)
	(dd (file-name-directory filename))
	)
    ;; Allow a database override function
    (setq cdb (semanticdb-create-database semanticdb-new-database-class
					  dd))
    ;; Get a table for this file.
    (setq tbl (semanticdb-create-table cdb filename))

    ;; Return the pair.
    (cons cdb tbl)
    ))

;;; Cache Cache.
;;
(defclass semanticdb-abstract-cache ()
  ((table :initarg :table
	  :type semanticdb-abstract-table
	  :documentation
	  "Cross reference to the table this belongs to.")
   )
  "Abstract baseclass for tools to use to cache information in semanticdb.
Tools needing a per-file cache must subclass this, and then get one as
needed.  Cache objects are identified in semanticdb by subclass.
In order to keep your cache up to date, be sure to implement
`semanticdb-synchronize', and `semanticdb-partial-synchronize'.
See the file semantic/scope.el for an example."
  :abstract t)

(defmethod semanticdb-cache-get ((table semanticdb-abstract-table)
				 desired-class)
  "Get a cache object on TABLE of class DESIRED-CLASS.
This method will create one if none exists with no init arguments
other than :table."
  (unless (child-of-class-p desired-class 'semanticdb-abstract-cache)
    (error "Invalid SemanticDB cache"))
  (let ((cache (oref table cache))
	(obj nil))
    (while (and (not obj) cache)
      (if (eq (object-class-fast (car cache)) desired-class)
	  (setq obj (car cache)))
      (setq cache (cdr cache)))
    (if obj
	obj ;; Just return it.
      ;; No object, let's create a new one and return that.
      (setq obj (funcall desired-class "Cache" :table table))
      (object-add-to-list table 'cache obj)
      obj)))

(defmethod semanticdb-cache-remove ((table semanticdb-abstract-table)
				    cache)
  "Remove from TABLE the cache object CACHE."
  (object-remove-from-list table 'cache cache))

(defmethod semanticdb-synchronize ((cache semanticdb-abstract-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((cache semanticdb-abstract-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defclass semanticdb-abstract-db-cache ()
  ((db :initarg :db
       :type semanticdb-project-database
       :documentation
       "Cross reference to the database this belongs to.")
   )
  "Abstract baseclass for tools to use to cache information in semanticdb.
Tools needing a database cache must subclass this, and then get one as
needed.  Cache objects are identified in semanticdb by subclass.
In order to keep your cache up to date, be sure to implement
`semanticdb-synchronize', and `semanticdb-partial-synchronize'.
See the file semantic/scope.el for an example."
  :abstract t)

(defmethod semanticdb-cache-get ((db semanticdb-project-database)
				 desired-class)
  "Get a cache object on DB of class DESIRED-CLASS.
This method will create one if none exists with no init arguments
other than :table."
  (unless (child-of-class-p desired-class 'semanticdb-abstract-cache)
    (error "Invalid SemanticDB cache"))
  (let ((cache (oref db cache))
	(obj nil))
    (while (and (not obj) cache)
      (if (eq (object-class-fast (car cache)) desired-class)
	  (setq obj (car cache)))
      (setq cache (cdr cache)))
    (if obj
	obj ;; Just return it.
      ;; No object, let's create a new one and return that.
      (setq obj (funcall desired-class "Cache" :db db))
      (object-add-to-list db 'cache obj)
      obj)))

(defmethod semanticdb-cache-remove ((db semanticdb-project-database)
				    cache)
  "Remove from TABLE the cache object CACHE."
  (object-remove-from-list db 'cache cache))


(defmethod semanticdb-synchronize ((cache semanticdb-abstract-db-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((cache semanticdb-abstract-db-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

;;; REFRESH

(defmethod semanticdb-refresh-table ((obj semanticdb-table) &optional force)
  "If the tag list associated with OBJ is loaded, refresh it.
Optional argument FORCE will force a refresh even if the file in question
is not in a buffer.  Avoid using FORCE for most uses, as an old cache
may be sufficient for the general case.  Forced updates can be slow.
This will call `semantic-fetch-tags' if that file is in memory."
  (cond
   ;;
   ;; Already in a buffer, just do it.
   ((semanticdb-in-buffer-p obj)
    (semanticdb-set-buffer obj)
    (semantic-fetch-tags))
   ;;
   ;; Not in a buffer.  Forcing a load.
   (force
    ;; Patch from Iain Nicol. --
    ;; @TODO: I wonder if there is a way to recycle
    ;;        semanticdb-create-table-for-file-not-in-buffer
    (save-excursion
      (let ((buff (semantic-find-file-noselect
		   (semanticdb-full-filename obj))))
	(set-buffer buff)
	(semantic-fetch-tags)
	;; Kill off the buffer if it didn't exist when we were called.
	(kill-buffer buff))))))

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table))
  "Return non-nil of OBJ's tag list is out of date.
The file associated with OBJ does not need to be in a buffer."
  (let* ((ff (semanticdb-full-filename obj))
	 (buff (semanticdb-in-buffer-p obj))
	 )
    (if buff
	(with-current-buffer buff
	  ;; Use semantic's magic tracker to determine of the buffer is up
	  ;; to date or not.
	  (not (semantic-parse-tree-up-to-date-p))
	  ;; We assume that semanticdb is keeping itself up to date.
	  ;; via all the clever hooks
	  )
      ;; Buffer isn't loaded.  The only clue we have is if the file
      ;; is somehow different from our mark in the semanticdb table.
      (let* ((stats (file-attributes ff))
	     (actualsize (nth 7 stats))
	     (actualmod (nth 5 stats))
	     )

	(or (not (slot-boundp obj 'tags))
	    ;; (not (oref obj tags)) -->  not needed anymore?
	    (/= (or (oref obj fsize) 0) actualsize)
	    (not (equal (oref obj lastmodtime) actualmod))
	    )
	))))


;;; Synchronization
;;
(defmethod semanticdb-synchronize ((table semanticdb-abstract-table)
				   new-tags)
  "Synchronize the table TABLE with some NEW-TAGS."
  (oset table tags new-tags)
  (oset table pointmax (point-max))
  (let ((fattr (file-attributes (semanticdb-full-filename table))))
    (oset table fsize (nth 7 fattr))
    (oset table lastmodtime (nth 5 fattr))
    )
  ;; Assume it is now up to date.
  (oset table unmatched-syntax semantic-unmatched-syntax-cache)
  ;; The lexical table should be good too.
  (when (featurep 'semantic/lex-spp)
    (oset table lexical-table (semantic-lex-spp-save-table)))
  ;; this implies dirtiness
  (semanticdb-set-dirty table)

  ;; Synchronize the index
  (when (slot-boundp table 'index)
    (let ((idx (oref table index)))
      (when idx (semanticdb-synchronize idx new-tags))))

  ;; Synchronize application caches.
  (dolist (C (oref table cache))
    (semanticdb-synchronize C new-tags)
    )

  ;; Update cross references
  ;; (semanticdb-refresh-references table)
  )

(defmethod semanticdb-partial-synchronize ((table semanticdb-abstract-table)
					   new-tags)
  "Synchronize the table TABLE where some NEW-TAGS changed."
  ;; You might think we need to reset the tags, but since the partial
  ;; parser splices the lists, we don't need to do anything
  ;;(oset table tags new-tags)
  ;; We do need to mark ourselves dirty.
  (semanticdb-set-dirty table)

  ;; The lexical table may be modified.
  (when (featurep 'semantic/lex-spp)
    (oset table lexical-table (semantic-lex-spp-save-table)))

  ;; Incremental parser doesn't monkey around with this.
  (oset table unmatched-syntax semantic-unmatched-syntax-cache)

  ;; Synchronize the index
  (when (slot-boundp table 'index)
    (let ((idx (oref table index)))
      (when idx (semanticdb-partial-synchronize idx new-tags))))

  ;; Synchronize application caches.
  (dolist (C (oref table cache))
    (semanticdb-synchronize C new-tags)
    )

  ;; Update cross references
  ;;(when (semantic-find-tags-by-class 'include new-tags)
  ;;  (semanticdb-refresh-references table))
  )

;;; SAVE/LOAD
;;
(defmethod semanticdb-save-db ((DB semanticdb-project-database)
			       &optional suppress-questions)
  "Cause a database to save itself.
The database base class does not save itself persistently.
Subclasses could save themselves to a file, or to a database, or other
form."
  nil)

(defun semanticdb-save-current-db ()
  "Save the current tag database."
  (interactive)
  (message "Saving current tag summaries...")
  (semanticdb-save-db semanticdb-current-database)
  (message "Saving current tag summaries...done"))

;; This prevents Semanticdb from querying multiple times if the users
;; answers "no" to creating the Semanticdb directory.
(defvar semanticdb--inhibit-create-file-directory)

(defun semanticdb-save-all-db ()
  "Save all semantic tag databases."
  (interactive)
  (message "Saving tag summaries...")
  (let ((semanticdb--inhibit-make-directory nil))
    (mapc 'semanticdb-save-db semanticdb-database-list))
  (message "Saving tag summaries...done"))

(defun semanticdb-save-all-db-idle ()
  "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."
  (semantic-safe "Auto-DB Save: %S"
    (semantic-exit-on-input 'semanticdb-idle-save
      (mapc (lambda (db)
	      (semantic-throw-on-input 'semanticdb-idle-save)
	      (semanticdb-save-db db t))
	    semanticdb-database-list))
    ))

;;; Directory Project support
;;
(defvar semanticdb-project-predicate-functions nil
  "List of predicates to try that indicate a directory belongs to a project.
This list is used when `semanticdb-persistent-path' contains the value
'project.  If the predicate list is nil, then presume all paths are valid.

Project Management software (such as EDE and JDE) should add their own
predicates with `add-hook' to this variable, and semanticdb will save tag
caches in directories controlled by them.")

(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  nil)

;;; Utilities
;;
;; What is the current database, are two tables of an equivalent mode,
;; and what databases are a part of the same project.
(defun semanticdb-current-database ()
  "Return the currently active database."
  (or semanticdb-current-database
      (and default-directory
	   (semanticdb-create-database semanticdb-new-database-class
				       default-directory)
	   )
      nil))

(defvar semanticdb-match-any-mode nil
  "Non-nil to temporarily search any major mode for a tag.
If a particular major mode wants to search any mode, put the
`semantic-match-any-mode' symbol onto the symbol of that major mode.
Do not set the value of this variable permanently.")

(defmacro semanticdb-with-match-any-mode (&rest body)
  "A Semanticdb search occurring withing BODY will search tags in all modes.
This temporarily sets `semanticdb-match-any-mode' while executing BODY."
  `(let ((semanticdb-match-any-mode t))
     ,@body))
(put 'semanticdb-with-match-any-mode 'lisp-indent-function 0)

(defmethod semanticdb-equivalent-mode-for-search (table &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
See `semanticdb-equivalent-mode' for details.
This version is used during searches.  Major-modes that opt
to set the `semantic-match-any-mode' property will be able to search
all files of any type."
  (or (get major-mode 'semantic-match-any-mode)
      semanticdb-match-any-mode
      (semanticdb-equivalent-mode table buffer))
  )

(defmethod semanticdb-equivalent-mode ((table semanticdb-abstract-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (if buffer (set-buffer buffer))
    (or
     ;; nil major mode in table means we don't know yet.  Assume yes for now?
     (null (oref table major-mode))
     ;; nil means the same as major-mode
     (and (not semantic-equivalent-major-modes)
	  (mode-local-use-bindings-p major-mode (oref table major-mode)))
     (and semantic-equivalent-major-modes
	  (member (oref table major-mode) semantic-equivalent-major-modes))
     )
    ))


;;; Associations
;;
;; These routines determine associations between a file, and multiple
;; associated databases.

(defcustom semanticdb-project-roots nil
  "*List of directories, where each directory is the root of some project.
All subdirectories of a root project are considered a part of one project.
Values in this string can be overridden by project management programs
via the `semanticdb-project-root-functions' variable."
  :group 'semanticdb
  :type '(repeat string))

(defvar semanticdb-project-root-functions nil
  "List of functions used to determine a given directories project root.
Functions in this variable can override `semanticdb-project-roots'.
Functions set in the variable are given one argument (a directory) and
must return a string, (the root directory) or a list of strings (multiple
root directories in a more complex system).  This variable should be used
by project management programs like EDE or JDE.")

(defvar semanticdb-project-system-databases nil
  "List of databases containing system library information.
Mode authors can create their own system databases which know
detailed information about the system libraries for querying purposes.
Put those into this variable as a buffer-local, or mode-local
value.")
(make-variable-buffer-local 'semanticdb-project-system-databases)

(defvar semanticdb-search-system-databases t
  "Non nil if search routines are to include a system database.")

(defun semanticdb-current-database-list (&optional dir)
  "Return a list of databases associated with the current buffer.
If optional argument DIR is non-nil, then use DIR as the starting directory.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'.
Always append `semanticdb-project-system-databases' if
`semanticdb-search-system' is non-nil."
  (let ((root nil)			; found root directory
	(dbs nil)			; collected databases
	(roots semanticdb-project-roots) ;all user roots
	(dir (file-truename (or dir default-directory)))
	)
    ;; Find the root based on project functions.
    (setq root (run-hook-with-args-until-success
		'semanticdb-project-root-functions
		dir))
    (if root
	(setq root (file-truename root))
      ;; Else, Find roots based on strings
      (while roots
	(let ((r (file-truename (car roots))))
	  (if (string-match (concat "^" (regexp-quote r)) dir)
	      (setq root r)))
	(setq roots (cdr roots))))

    ;; If no roots are found, use this directory.
    (unless root (setq root dir))

    ;; Find databases based on the root directory.
    (when root
      ;; The rootlist allows the root functions to possibly
      ;; return several roots which are in different areas but
      ;; all apart of the same system.
      (let ((regexp (concat "^" (regexp-quote root)))
	    (adb semanticdb-database-list) ; all databases
	    )
	(while adb
	  ;; I don't like this part, but close enough.
	  (if (and (slot-boundp (car adb) 'reference-directory)
		   (string-match regexp (oref (car adb) reference-directory)))
	      (setq dbs (cons (car adb) dbs)))
	  (setq adb (cdr adb))))
      )
    ;; Add in system databases
    (when semanticdb-search-system-databases
      (setq dbs (nconc dbs semanticdb-project-system-databases)))
    ;; Return
    dbs))


;;; Generic Accessor Routines
;;
;; These routines can be used to get at tags in files w/out
;; having to know a lot about semanticDB.
(defvar semanticdb-file-table-hash (make-hash-table :test 'equal)
  "Hash table mapping file names to database tables.")

(defun semanticdb-file-table-object-from-hash (file)
  "Retrieve a DB table from the hash for FILE.
Does not use `file-truename'."
  (gethash file semanticdb-file-table-hash 'no-hit))

(defun semanticdb-file-table-object-put-hash (file dbtable)
  "For FILE, associate DBTABLE in the hash table."
  (puthash file dbtable semanticdb-file-table-hash))

;;;###autoload
(defun semanticdb-file-table-object (file &optional dontload)
  "Return a semanticdb table belonging to FILE, make it up to date.
If file has database tags available in the database, return it.
If file does not have tags available, and DONTLOAD is nil,
then load the tags for FILE, and create a new table object for it.
DONTLOAD does not affect the creation of new database objects."
  ;; (message "Object Translate: %s" file)
  (when (and file (file-exists-p file))
    (let* ((default-directory (file-name-directory file))
	   (tab (semanticdb-file-table-object-from-hash file))
	   (fullfile nil))

      ;; If it is not in the cache, then extract the more traditional
      ;; way by getting the database, and finding a table in that database.
      ;; Once we have a table, add it to the hash.
      (when (eq tab 'no-hit)
	(setq fullfile (file-truename file))
	(let ((db (or ;; This line will pick up system databases.
		   (semanticdb-directory-loaded-p default-directory)
		   ;; this line will make a new one if needed.
		   (semanticdb-get-database default-directory))))
	  (setq tab (semanticdb-file-table db fullfile))
	  (when tab
	    (semanticdb-file-table-object-put-hash file tab)
	    (when (not (string= fullfile file))
	      (semanticdb-file-table-object-put-hash fullfile tab)
	    ))
	  ))

      (cond
       ((and tab
	     ;; Is this in a buffer?
	     ;;(find-buffer-visiting (semanticdb-full-filename tab))
	     (semanticdb-in-buffer-p tab)
	     )
	(save-excursion
	  ;;(set-buffer (find-buffer-visiting (semanticdb-full-filename tab)))
	  (semanticdb-set-buffer tab)
	  (semantic-fetch-tags)
	  ;; Return the table.
	  tab))
       ((and tab dontload)
	;; If we have table, and we don't want to load it, just return it.
	tab)
       ((and tab
	     ;; Is table fully loaded, or just a proxy?
	     (number-or-marker-p (oref tab pointmax))
	     ;; Is this table up to date with the file?
	     (not (semanticdb-needs-refresh-p tab)))
	;; A-ok!
	tab)
       ((or (and fullfile (get-file-buffer fullfile))
	    (get-file-buffer file))
	;; are these two calls this faster than `find-buffer-visiting'?

	;; If FILE is being visited, but none of the above state is
	;; true (meaning, there is no table object associated with it)
	;; then it is a file not supported by Semantic, and can be safely
	;; ignored.
	nil)
       ((not dontload) ;; We must load the file.
	;; Full file should have been set by now.  Debug why not?
	(when (and (not tab) (not fullfile))
	  ;; This case is if a 'nil is erroneously put into the hash table.  This
	  ;; would need fixing
	  (setq fullfile (file-truename file))
	  )

	;; If we have a table, but no fullfile, that's ok.  Let's get the filename
	;; from the table which is pre-truenamed.
	(when (and (not fullfile) tab)
	  (setq fullfile (semanticdb-full-filename tab)))

	(setq tab (semanticdb-create-table-for-file-not-in-buffer fullfile))

	;; Save the new table.
	(semanticdb-file-table-object-put-hash file tab)
	(when (not (string= fullfile file))
	  (semanticdb-file-table-object-put-hash fullfile tab)
	  )
	;; Done!
	tab)
       (t
	;; Full file should have been set by now.  Debug why not?
	;; One person found this.  Is it a file that failed to parse
	;; in the past?
	(when (not fullfile)
	  (setq fullfile (file-truename file)))

	;; We were asked not to load the file in and parse it.
	;; Instead just create a database table with no tags
	;; and a claim of being empty.
	;;
	;; This will give us a starting point for storing
	;; database cross-references so when it is loaded,
	;; the cross-references will fire and caches will
	;; be cleaned.
	(let ((ans (semanticdb-create-table-for-file file)))
	  (setq tab (cdr ans))

	  ;; Save the new table.
	  (semanticdb-file-table-object-put-hash file tab)
	  (when (not (string= fullfile file))
	    (semanticdb-file-table-object-put-hash fullfile tab)
	    )
	  ;; Done!
	  tab))
       )
      )))

(defvar semanticdb-out-of-buffer-create-table-fcn nil
  "When non-nil, a function for creating a semanticdb table.
This should take a filename to be parsed.")
(make-variable-buffer-local 'semanticdb-out-of-buffer-create-table-fcn)

(defun semanticdb-create-table-for-file-not-in-buffer (filename)
  "Create a table for the file FILENAME.
If there are no language specific configurations, this
function will read in the buffer, parse it, and kill the buffer."
  (if (and semanticdb-out-of-buffer-create-table-fcn
	   (not (file-remote-p filename)))
      ;; Use external parser only of the file is accessible to the
      ;; local file system.
      (funcall semanticdb-out-of-buffer-create-table-fcn filename)
    (save-excursion
      (let* ( ;; Remember the buffer to kill
	     (kill-buffer-flag (find-buffer-visiting filename))
	     (buffer-to-kill (or kill-buffer-flag
				 (semantic-find-file-noselect filename t))))

	;; This shouldn't ever be set.  Debug some issue here?
	;; (when kill-buffer-flag (debug))

	(set-buffer buffer-to-kill)
	;; Find file should automatically do this for us.
	;; Sometimes the DB table doesn't contains tags and needs
	;; a refresh.  For example, when the file is loaded for
	;; the first time, and the idle scheduler didn't get a
	;; chance to trigger a parse before the file buffer is
	;; killed.
	(when semanticdb-current-table
	  (semantic-fetch-tags))
	(prog1
	    semanticdb-current-table
	  (when (not kill-buffer-flag)
	    ;; If we had to find the file, then we should kill it
	    ;; to keep the master buffer list clean.
	    (kill-buffer buffer-to-kill)
	    )))))
  )

(defun semanticdb-file-stream (file)
  "Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them."
  (let ((table (semanticdb-file-table-object file)))
    (when table
      (semanticdb-get-tags table))))

(provide 'semantic/db)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db"
;; End:

;;; semantic/db.el ends here
