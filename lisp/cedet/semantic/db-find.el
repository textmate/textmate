;;; semantic/db-find.el --- Searching through semantic databases.

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
;; Databases of various forms can all be searched.
;; There are a few types of searches that can be done:
;;
;;   Basic Name Search:
;;    These searches scan a database table  collection for tags based
;;    on name.
;;
;;   Basic Attribute Search:
;;    These searches allow searching on specific attributes of tags,
;;    such as name, type, or other attribute.
;;
;;   Advanced Search:
;;    These are searches that were needed to accomplish some
;;    specialized tasks as discovered in utilities.  Advanced searches
;;    include matching methods defined outside some parent class.
;;
;;    The reason for advanced searches are so that external
;;    repositories such as the Emacs obarray, or java .class files can
;;    quickly answer these needed questions without dumping the entire
;;    symbol list into Emacs for additional refinement searches via
;;    regular semanticdb search.
;;
;; How databases are decided upon is another important aspect of a
;; database search.  When it comes to searching for a name, there are
;; these types of searches:
;;
;;   Basic Search:
;;    Basic search means that tags looking for a given name start
;;    with a specific search path.  Names are sought on that path
;;    until it is empty or items on the path can no longer be found.
;;    Use `semanticdb-dump-all-table-summary' to test this list.
;;    Use `semanticdb-find-throttle-custom-list' to refine this list.
;;
;;   Deep Search:
;;    A deep search will search more than just the global namespace.
;;    It will recurse into tags that contain more tags, and search
;;    those too.
;;
;;   Brute Search:
;;    Brute search means that all tables in all databases in a given
;;    project are searched.  Brute searches are the search style as
;;    written for semantic version 1.x.
;;
;; How does the search path work?
;;
;;  A basic search starts with three parameters:
;;
;;     (FINDME &optional PATH FIND-FILE-MATCH)
;;
;;  FINDME is key to be searched for dependent on the type of search.
;;  PATH is an indicator of which tables are to be searched.
;;  FIND-FILE-MATCH indicates that any time a match is found, the
;;  file associated with the tag should be read into a file.
;;
;;  The PATH argument is then the most interesting argument.  It can
;;  have these values:
;;
;;    nil - Take the current buffer, and use its include list
;;    buffer - Use that buffer's include list.
;;    filename - Use that file's include list.  If the file is not
;;        in a buffer, see of there is a semanticdb table for it.  If
;;        not, read that file into a buffer.
;;    tag - Get that tag's buffer of file file.  See above.
;;    table - Search that table, and its include list.
;;
;; Search Results:
;;
;;   Semanticdb returns the results in a specific format.  There are a
;;   series of routines for using those results, and results can be
;;   passed in as a search-path for refinement searches with
;;   semanticdb.  Apropos for semanticdb.*find-result for more.
;;
;; Application:
;;
;; Here are applications where different searches are needed which
;; exist as of semantic 1.4.x
;;
;; eldoc - popup help
;;   => Requires basic search using default path.  (Header files ok)
;; tag jump - jump to a named tag
;;   => Requires a brute search using whole project.  (Source files only)
;; completion - Completing symbol names in a smart way
;;   => Basic search (headers ok)
;; type analysis - finding type definitions for variables & fcns
;;   => Basic search (headers ok)
;; Class browser - organize types into some structure
;;   => Brute search, or custom navigation.

;; TODO:
;;  During a search, load any unloaded DB files based on paths in the
;;  current project.

(require 'semantic/db)
(require 'semantic/db-ref)
(eval-when-compile
  (require 'semantic/find))

;;; Code:

(defvar data-debug-thing-alist)
(declare-function data-debug-insert-stuff-list "data-debug")
;;;(declare-function data-debug-insert-tag-list "adebug")
(declare-function semantic-scope-reset-cache "semantic/scope")
(declare-function semanticdb-typecache-notify-reset "semantic/db-typecache")
(declare-function ede-current-project "ede")

(defvar semanticdb-find-throttle-custom-list
  '(repeat (radio (const 'local)
		  (const 'project)
		  (const 'unloaded)
		  (const 'system)
		  (const 'recursive)
		  (const 'omniscience)))
  "Customization values for semanticdb find throttle.
See `semanticdb-find-throttle' for details.")

;;;###autoload
(defcustom semanticdb-find-default-throttle
  '(local project unloaded system recursive)
  "The default throttle for `semanticdb-find' routines.
The throttle controls how detailed the list of database
tables is for a symbol lookup.  The value is a list with
the following keys:
  `file'       - The file the search is being performed from.
                 This option is here for completeness only, and
                 is assumed to always be on.
  `local'      - Tables from the same local directory are included.
                 This includes files directly referenced by a file name
                 which might be in a different directory.
  `project'    - Tables from the same local project are included
                 If `project' is specified, then `local' is assumed.
  `unloaded'   - If a table is not in memory, load it.  If it is not cached
                 on disk either, get the source, parse it, and create
                 the table.
  `system'     - Tables from system databases.  These are specifically
                 tables from system header files, or language equivalent.
  `recursive'  - For include based searches, includes tables referenced
                 by included files.
  `omniscience' - Included system databases which are omniscience, or
                 somehow know everything.  Omniscience databases are found
                 in `semanticdb-project-system-databases'.
                 The Emacs Lisp system DB is an omniscience database."
  :group 'semanticdb
  :type semanticdb-find-throttle-custom-list)

(defun semanticdb-find-throttle-active-p (access-type)
  "Non-nil if ACCESS-TYPE is an active throttle type."
  (or (memq access-type semanticdb-find-default-throttle)
      (eq access-type 'file)
      (and (eq access-type 'local)
	   (memq 'project semanticdb-find-default-throttle))
      ))

;;; Index Class
;;
;; The find routines spend a lot of time looking stuff up.
;; Use this handy search index to cache data between searches.
;; This should allow searches to start running faster.
(defclass semanticdb-find-search-index (semanticdb-abstract-search-index)
  ((include-path :initform nil
		 :documentation
		 "List of semanticdb tables from the include path.")
   (type-cache :initform nil
	       :documentation
	       "Cache of all the data types accessible from this file.
Includes all types from all included files, merged namespaces, and
expunge duplicates.")
   )
  "Concrete search index for `semanticdb-find'.
This class will cache data derived during various searches.")

(defmethod semantic-reset ((idx semanticdb-find-search-index))
  "Reset the object IDX."
  (require 'semantic/scope)
  ;; Clear the include path.
  (oset idx include-path nil)
  (when (oref idx type-cache)
    (semantic-reset (oref idx type-cache)))
  ;; Clear the scope.  Scope doesn't have the data it needs to track
  ;; its own reset.
  (semantic-scope-reset-cache)
  )

(defmethod semanticdb-synchronize ((idx semanticdb-find-search-index)
				   new-tags)
  "Synchronize the search index IDX with some NEW-TAGS."
  ;; Reset our parts.
  (semantic-reset idx)
  ;; Notify dependants by clearing their indices.
  (semanticdb-notify-references
   (oref idx table)
   (lambda (tab me)
     (semantic-reset (semanticdb-get-table-index tab))))
  )

(defmethod semanticdb-partial-synchronize ((idx semanticdb-find-search-index)
					   new-tags)
  "Synchronize the search index IDX with some changed NEW-TAGS."
  ;; Only reset if include statements changed.
  (if (semantic-find-tags-by-class 'include new-tags)
      (progn
	(semantic-reset idx)
	;; Notify dependants by clearing their indices.
	(semanticdb-notify-references
	 (oref idx table)
	 (lambda (tab me)
	   (semantic-reset (semanticdb-get-table-index tab))))
	)
    ;; Else, not an include, by just a type.
    (when (oref idx type-cache)
      (when (semanticdb-partial-synchronize (oref idx type-cache) new-tags)
	;; If the synchronize returns true, we need to notify.
	;; Notify dependants by clearing their indices.
	(semanticdb-notify-references
	 (oref idx table)
	 (lambda (tab me)
	   (let ((tab-idx (semanticdb-get-table-index tab)))
	     ;; Not a full reset?
	     (when (oref tab-idx type-cache)
	       (require 'db-typecache)
	       (semanticdb-typecache-notify-reset
		(oref tab-idx type-cache)))
	     )))
	))
  ))


;;; Path Translations
;;
;;; OVERLOAD Functions
;;
;; These routines needed to be overloaded by specific language modes.
;; They are needed for translating an INCLUDE tag into a semanticdb
;; TABLE object.
;;;###autoload
(define-overloadable-function semanticdb-find-translate-path (path brutish)
  "Translate PATH into a list of semantic tables.
Path translation involves identifying the PATH input argument
in one of the following ways:
  nil - Take the current buffer, and use its include list
  buffer - Use that buffer's include list.
  filename - Use that file's include list.  If the file is not
      in a buffer, see of there is a semanticdb table for it.  If
      not, read that file into a buffer.
  tag - Get that tag's buffer of file file.  See above.
  table - Search that table, and its include list.
  find result - Search the results of a previous find.

In addition, once the base path is found, there is the possibility of
each added table adding yet more tables to the path, so this routine
can return a lengthy list.

If argument BRUTISH is non-nil, then instead of using the include
list, use all tables found in the parent project of the table
identified by translating PATH.  Such searches use brute force to
scan every available table.

The return value is a list of objects of type `semanticdb-table' or
their children.  In the case of passing in a find result, the result
is returned unchanged.

This routine uses `semanticdb-find-table-for-include' to translate
specific include tags into a semanticdb table.

Note: When searching using a non-brutish method, the list of
included files will be cached between runs.  Database-references
are used to track which files need to have their include lists
refreshed when things change.  See `semanticdb-ref-test'.

Note for overloading:  If you opt to overload this function for your
major mode, and your routine takes a long time, be sure to call

 (semantic-throw-on-input 'your-symbol-here)

so that it can be called from the idle work handler."
  )

(defun semanticdb-find-translate-path-default (path brutish)
  "Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'."
  (if (semanticdb-find-results-p path)
      ;; nil means perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      (semanticdb-find-translate-path-includes-default path))))

;;;###autoload
(define-overloadable-function semanticdb-find-table-for-include (includetag &optional table)
  "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE is a semanticdb table that identifies where INCLUDETAG came from.
TABLE is optional if INCLUDETAG has an overlay of :filename attribute."
  )

(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
	 (cond ((null path) semanticdb-current-database)
	       ((semanticdb-table-p path) (oref path parent-db))
	       (t (let ((tt (semantic-something-to-tag-table path)))
		    (if tt
			;; @todo - What does this DO ??!?!
			(with-current-buffer (semantic-tag-buffer (car tt))
			  semanticdb-current-database)
		      semanticdb-current-database))))))
    (apply
     #'nconc
     (mapcar
      (lambda (db)
	(let ((tabs (semanticdb-get-database-tables db))
	      (ret nil))
	  ;; Only return tables of the same language (major-mode)
	  ;; as the current search environment.
	  (while tabs

	    (semantic-throw-on-input 'translate-path-brutish)

	    (if (semanticdb-equivalent-mode-for-search (car tabs)
						       (current-buffer))
		(setq ret (cons (car tabs) ret)))
	    (setq tabs (cdr tabs)))
	  ret))
      ;; FIXME:
      ;; This should scan the current project directory list for all
      ;; semanticdb files, perhaps handling proxies for them.
      (semanticdb-current-database-list
       (if basedb (oref basedb reference-directory)
	 default-directory))))
    ))

(defun semanticdb-find-incomplete-cache-entries-p (cache)
  "Are there any incomplete entries in CACHE?"
  (let ((ans nil))
    (dolist (tab cache)
      (when (and (semanticdb-table-child-p tab)
		 (not (number-or-marker-p (oref tab pointmax))))
	(setq ans t))
      )
    ans))

(defun semanticdb-find-need-cache-update-p (table)
  "Non-nil if the semanticdb TABLE cache needs to be updated."
  ;; If we were passed in something related to a TABLE,
  ;; do a caching lookup.
  (let* ((index (semanticdb-get-table-index table))
	 (cache (when index (oref index include-path)))
	 (incom (semanticdb-find-incomplete-cache-entries-p cache))
	 (unl (semanticdb-find-throttle-active-p 'unloaded))
	 )
    (if (and
	 cache ;; Must have a cache
	 (or
	  ;; If all entries are "full", or if 'unloaded
	  ;; OR
	  ;; is not in the throttle, it is ok to use the cache.
	  (not incom) (not unl)
	  ))
	nil
      ;;cache
      ;; ELSE
      ;;
      ;; We need an update.
      t))
  )

(defun semanticdb-find-translate-path-includes-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((table (cond ((null path)
		      semanticdb-current-table)
		     ((bufferp path)
		      (semantic-buffer-local-value 'semanticdb-current-table path))
		     ((and (stringp path) (file-exists-p path))
		      (semanticdb-file-table-object path t))
		     ((semanticdb-abstract-table-child-p path)
		      path)
		     (t nil))))
    (if table
	;; If we were passed in something related to a TABLE,
	;; do a caching lookup.
	(let ((index (semanticdb-get-table-index table)))
	  (if (semanticdb-find-need-cache-update-p table)
	      ;; Let's go look up our indices.
	      (let ((ans (semanticdb-find-translate-path-includes--internal path)))
		(oset index include-path ans)
		;; Once we have our new indices set up, notify those
		;; who depend on us if we found something for them to
		;; depend on.
		(when ans (semanticdb-refresh-references table))
		ans)
	    ;; ELSE
	    ;;
	    ;; Just return the cache.
	    (oref index include-path)))
      ;; If we were passed in something like a tag list, or other boring
      ;; searchable item, then instead do the regular thing without caching.
      (semanticdb-find-translate-path-includes--internal path))))

(defvar semanticdb-find-lost-includes nil
  "Include files that we cannot find associated with this buffer.")
(make-variable-buffer-local 'semanticdb-find-lost-includes)

(defvar semanticdb-find-scanned-include-tags nil
  "All include tags scanned, plus action taken on the tag.
Each entry is an alist:
  (ACTION . TAG)
where ACTION is one of 'scanned, 'duplicate, 'lost
and TAG is a clone of the include tag that was found.")
(make-variable-buffer-local 'semanticdb-find-scanned-include-tags)

(defvar semanticdb-implied-include-tags nil
  "Include tags implied for all files of a given mode.
Set this variable with `defvar-mode-local' for a particular mode so
that any symbols that exist for all files for that mode are included.

Note: This could be used as a way to write a file in a language
to declare all the built-ins for that language.")

(defun semanticdb-find-translate-path-includes--internal (path)
  "Internal implementation of `semanticdb-find-translate-path-includes-default'.
This routine does not depend on the cache, but will always derive
a new path from the provided PATH."
  (let ((includetags nil)
	(curtable nil)
	(matchedtables (list semanticdb-current-table))
	(matchedincludes nil)
	(lostincludes nil)
	(scannedincludes nil)
	(incfname nil)
	nexttable)
    (cond ((null path)
	   (semantic-refresh-tags-safe)
	   (setq includetags (append
			      (semantic-find-tags-included (current-buffer))
			      semanticdb-implied-include-tags)
		 curtable semanticdb-current-table
		 incfname (buffer-file-name))
	   )
	  ((semanticdb-table-p path)
	   (setq includetags (semantic-find-tags-included path)
		 curtable path
		 incfname (semanticdb-full-filename path))
	   )
	  ((bufferp path)
	   (with-current-buffer path
	     (semantic-refresh-tags-safe))
	   (setq includetags (semantic-find-tags-included path)
		 curtable (with-current-buffer path
                            semanticdb-current-table)
		 incfname (buffer-file-name path)))
	  (t
	   (setq includetags (semantic-find-tags-included path))
	   (when includetags
	     ;; If we have some tags, derive a table from them.
	     ;; else we will do nothing, so the table is useless.

	     ;; @todo - derive some tables
	     (message "Need to derive tables for %S in translate-path-includes--default."
		      path)
	   )))

    ;; Make sure each found include tag has an originating file name associated
    ;; with it.
    (when incfname
      (dolist (it includetags)
	(semantic--tag-put-property it :filename incfname)))

    ;; Loop over all include tags adding to matchedtables
    (while includetags
      (semantic-throw-on-input 'semantic-find-translate-path-includes-default)

      ;; If we've seen this include string before, lets skip it.
      (if (member (semantic-tag-name (car includetags)) matchedincludes)
	  (progn
	    (setq nexttable nil)
	    (push (cons 'duplicate (semantic-tag-clone (car includetags)))
		  scannedincludes)
	    )
	(setq nexttable (semanticdb-find-table-for-include (car includetags) curtable))
	(when (not nexttable)
	  ;; Save the lost include.
	  (push (car includetags) lostincludes)
	  (push (cons 'lost (semantic-tag-clone (car includetags)))
		scannedincludes)
	  )
	)

      ;; Push the include file, so if we can't find it, we only
      ;; can't find it once.
      (push (semantic-tag-name (car includetags)) matchedincludes)

      ;; (message "Scanning %s" (semantic-tag-name (car includetags)))
      (when (and nexttable
		 (not (memq nexttable matchedtables))
		 (semanticdb-equivalent-mode-for-search nexttable
							(current-buffer))
		 )
	;; Add to list of tables
	(push nexttable matchedtables)

	;; Queue new includes to list
	(if (semanticdb-find-throttle-active-p 'recursive)
	    ;; @todo - recursive includes need to have the originating
	    ;;         buffer's location added to the path.
	    (let ((newtags
		   (cond
		    ((semanticdb-table-p nexttable)
		     (semanticdb-refresh-table nexttable)
		     ;; Use the method directly, or we will recurse
		     ;; into ourselves here.
		     (semanticdb-find-tags-by-class-method
		      nexttable 'include))
		    (t ;; @todo - is this ever possible???
		     (message "semanticdb-ftp - how did you do that?")
		     (semantic-find-tags-included
		      (semanticdb-get-tags nexttable)))
		    ))
		  (newincfname (semanticdb-full-filename nexttable))
		  )

	      (push (cons 'scanned (semantic-tag-clone (car includetags)))
		    scannedincludes)

	      ;; Setup new tags so we know where they are.
	      (dolist (it newtags)
		(semantic--tag-put-property it :filename
					    newincfname))

	      (setq includetags (nconc includetags newtags)))
	  ;; ELSE - not recursive throttle
	  (push (cons 'scanned-no-recurse
		      (semantic-tag-clone (car includetags)))
		scannedincludes)
	  )
	)
      (setq includetags (cdr includetags)))

    (setq semanticdb-find-lost-includes lostincludes)
    (setq semanticdb-find-scanned-include-tags (reverse scannedincludes))

    ;; Find all the omniscient databases for this major mode, and
    ;; add them if needed
    (when (and (semanticdb-find-throttle-active-p 'omniscience)
	       semanticdb-search-system-databases)
      ;; We can append any mode-specific omniscience databases into
      ;; our search list here.
      (let ((systemdb semanticdb-project-system-databases)
	    (ans nil))
	(while systemdb
	  (setq ans (semanticdb-file-table
		     (car systemdb)
		     ;; I would expect most omniscient to return the same
		     ;; thing regardless of filename, but we may have
		     ;; one that can return a table of all things the
		     ;; current file needs.
		     (buffer-file-name (current-buffer))))
	  (when (not (memq ans matchedtables))
	    (setq matchedtables (cons ans matchedtables)))
	  (setq systemdb (cdr systemdb))))
      )
    (nreverse matchedtables)))

(define-overloadable-function semanticdb-find-load-unloaded (filename)
  "Create a database table for FILENAME if it hasn't been parsed yet.
Assumes that FILENAME exists as a source file.
Assumes that a preexisting table does not exist, even if it
isn't in memory yet."
  (if (semanticdb-find-throttle-active-p 'unloaded)
      (:override)
    (semanticdb-file-table-object filename t)))

(defun semanticdb-find-load-unloaded-default (filename)
  "Load an unloaded file in FILENAME using the default semanticdb loader."
  (semanticdb-file-table-object filename))

;; The creation of the overload occurs above.
(defun semanticdb-find-table-for-include-default (includetag &optional table)
  "Default implementation of `semanticdb-find-table-for-include'.
Uses `semanticdb-current-database-list' as the search path.
INCLUDETAG and TABLE are documented in `semanticdb-find-table-for-include'.
Included databases are filtered based on `semanticdb-find-default-throttle'."
  (if (not (eq (semantic-tag-class includetag) 'include))
      (signal 'wrong-type-argument (list includetag 'include)))

  (let ((name
	 ;; Note, some languages (like Emacs or Java) use include tag names
	 ;; that don't represent files!  We want to have file names.
	 (semantic-tag-include-filename includetag))
	(originfiledir nil)
	(roots nil)
	(tmp nil)
	(ans nil))

    ;; INCLUDETAG should have some way to reference where it came
    ;; from!  If not, TABLE should provide the way.  Each time we
    ;; look up a tag, we may need to find it in some relative way
    ;; and must set our current buffer eto the origin of includetag
    ;; or nothing may work.
    (setq originfiledir
	  (cond ((semantic-tag-file-name includetag)
		 ;; A tag may have a buffer, or a :filename property.
		 (file-name-directory (semantic-tag-file-name includetag)))
		(table
		 (file-name-directory (semanticdb-full-filename table)))
		(t
		 ;; @todo - what to do here?  Throw an error maybe
		 ;; and fix usage bugs?
		 default-directory)))

    (cond
     ;; Step 1: Relative path name
     ;;
     ;; If the name is relative, then it should be findable as relative
     ;; to the source file that this tag originated in, and be fast.
     ;;
     ((and (semanticdb-find-throttle-active-p 'local)
	   (file-exists-p (expand-file-name name originfiledir)))

      (setq ans (semanticdb-find-load-unloaded
		 (expand-file-name name originfiledir)))
      )
     ;; Step 2: System or Project level includes
     ;;
     ((or
       ;; First, if it a system include, we can investigate that tags
       ;; dependency file
       (and (semanticdb-find-throttle-active-p 'system)

	    ;; Sadly, not all languages make this distinction.
	    ;;(semantic-tag-include-system-p includetag)

	    ;; Here, we get local and system files.
	    (setq tmp (semantic-dependency-tag-file includetag))
	    )
       ;; Second, project files are active, we and we have EDE,
       ;; we can find it using the same tool.
       (and (semanticdb-find-throttle-active-p 'project)
	    ;; Make sure EDE is available, and we have a project
	    (featurep 'ede) (ede-current-project originfiledir)
	    ;; The EDE query is hidden in this call.
	    (setq tmp (semantic-dependency-tag-file includetag))
	    )
       )
      (setq ans (semanticdb-find-load-unloaded tmp))
      )
     ;; Somewhere in our project hierarchy
     ;;
     ;; Remember: Roots includes system databases which can create
     ;; specialized tables we can search.
     ;;
     ;; NOTE: Not used if EDE is active!
     ((and (semanticdb-find-throttle-active-p 'project)
	   ;; And don't do this if it is a system include.  Not supported by all languages,
	   ;; but when it is, this is a nice fast way to skip this step.
	   (not (semantic-tag-include-system-p includetag))
	   ;; Don't do this if we have an EDE project.
	   (not (and (featurep 'ede)
		     ;; Note: We don't use originfiledir here because
		     ;; we want to know about the source file we are
		     ;; starting from.
		     (ede-current-project)))
	   )

      (setq roots (semanticdb-current-database-list))

      (while (and (not ans) roots)
	(let* ((ref (if (slot-boundp (car roots) 'reference-directory)
			(oref (car roots) reference-directory)))
	       (fname (cond ((null ref) nil)
			    ((file-exists-p (expand-file-name name ref))
			     (expand-file-name name ref))
			    ((file-exists-p (expand-file-name (file-name-nondirectory name) ref))
			     (expand-file-name (file-name-nondirectory name) ref)))))
	  (when (and ref fname)
	    ;; There is an actual file.  Grab it.
	    (setq ans (semanticdb-find-load-unloaded fname)))

	  ;; ELSE
	  ;;
	  ;; NOTE: We used to look up omniscient databases here, but that
	  ;; is now handled one layer up.
	  ;;
	  ;; Missing: a database that knows where missing files are.  Hmm.
	  ;; perhaps I need an override function for that?

	  )

	(setq roots (cdr roots))))
     )
    ans))


;;; Perform interactive tests on the path/search mechanisms.
;;
;;;###autoload
(defun semanticdb-find-test-translate-path (&optional arg)
  "Call and output results of `semanticdb-find-translate-path'.
With ARG non-nil, specify a BRUTISH translation.
See `semanticdb-find-default-throttle' and `semanticdb-project-roots'
for details on how this list is derived."
  (interactive "P")
  (semantic-fetch-tags)
  (require 'data-debug)
  (let ((start (current-time))
	(p (semanticdb-find-translate-path nil arg))
	(end (current-time))
	)
    (data-debug-new-buffer "*SEMANTICDB FTP ADEBUG*")
    (message "Search of tags took %.2f seconds."
	     (semantic-elapsed-time start end))

    (data-debug-insert-stuff-list p "*")))

(defun semanticdb-find-test-translate-path-no-loading (&optional arg)
  "Call and output results of `semanticdb-find-translate-path'.
With ARG non-nil, specify a BRUTISH translation.
See `semanticdb-find-default-throttle' and `semanticdb-project-roots'
for details on how this list is derived."
  (interactive "P")
  (semantic-fetch-tags)
  (require 'data-debug)
  (let* ((semanticdb-find-default-throttle
	  (if (featurep 'semantic/db-find)
	      (remq 'unloaded semanticdb-find-default-throttle)
	    nil))
	 (start (current-time))
	 (p (semanticdb-find-translate-path nil arg))
	 (end (current-time))
	 )
    (data-debug-new-buffer "*SEMANTICDB FTP ADEBUG*")
    (message "Search of tags took %.2f seconds."
	     (semantic-elapsed-time start end))

    (data-debug-insert-stuff-list p "*")))

;;;###autoload
(defun semanticdb-find-adebug-lost-includes ()
  "Translate the current path, then display the lost includes.
Examines the variable `semanticdb-find-lost-includes'."
  (interactive)
  (require 'data-debug)
  (semanticdb-find-translate-path nil nil)
  (let ((lost semanticdb-find-lost-includes)
	)

    (if (not lost)
	(message "There are no unknown includes for %s"
		 (buffer-name))

      (data-debug-new-buffer "*SEMANTICDB lost-includes ADEBUG*")
      ;; (data-debug-insert-tag-list lost "*")
      )))

(defun semanticdb-find-adebug-insert-scanned-tag-cons (consdata prefix prebuttontext)
  "Insert a button representing scanned include CONSDATA.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the overlay button."
  (let* ((start (point))
	 (end nil)
	 (mode (car consdata))
	 (tag (cdr consdata))
	 (name (semantic-tag-name tag))
	 (file (semantic-tag-file-name tag))
	 (str1 (format "%S %s" mode name))
	 (str2 (format " : %s" file))
	 (tip nil))
    (insert prefix prebuttontext str1)
    (setq end (point))
    (insert str2)
    (put-text-property start end 'face
		       (cond ((eq mode 'scanned)
			      'font-lock-function-name-face)
			     ((eq mode 'duplicate)
			      'font-lock-comment-face)
			     ((eq mode 'lost)
			      'font-lock-variable-name-face)
			     ((eq mode 'scanned-no-recurse)
			      'font-lock-type-face)))
    (put-text-property start end 'ddebug (cdr consdata))
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-tag-parts-from-point)
    (insert "\n")
    )
  )

(defun semanticdb-find-adebug-scanned-includes ()
  "Translate the current path, then display the lost includes.
Examines the variable `semanticdb-find-lost-includes'."
  (interactive)
  (require 'data-debug)
  (semanticdb-find-translate-path nil nil)
  (let ((scanned semanticdb-find-scanned-include-tags)
	(data-debug-thing-alist
	 (cons
	  '((lambda (thing) (and (consp thing)
				 (symbolp (car thing))
				 (memq (car thing)
				       '(scanned scanned-no-recurse
						 lost duplicate))))
	    . semanticdb-find-adebug-insert-scanned-tag-cons)
	  data-debug-thing-alist))
	)

    (if (not scanned)
	(message "There are no includes scanned %s"
		 (buffer-name))

      (data-debug-new-buffer "*SEMANTICDB scanned-includes ADEBUG*")
      (data-debug-insert-stuff-list scanned "*")
      )))

;;; API Functions
;;
;; Once you have a search result, use these routines to operate
;; on the search results at a higher level

;;;###autoload
(defun semanticdb-strip-find-results (results &optional find-file-match)
  "Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
Optional FIND-FILE-MATCH loads all files associated with RESULTS
into buffers.  This has the side effect of enabling `semantic-tag-buffer' to
return a value.
If FIND-FILE-MATCH is 'name, then only the filename is stored
in each tag instead of loading each file into a buffer.
If the input RESULTS are not going to be used again, and if
FIND-FILE-MATCH is nil, you can use `semanticdb-fast-strip-find-results'
instead."
  (if find-file-match
      ;; Load all files associated with RESULTS.
      (let ((tmp results)
	    (output nil))
	(while tmp
	  (let ((tab (car (car tmp)))
		(tags (cdr (car tmp))))
	    (dolist (T tags)
	      ;; Normalization gives specialty database tables a chance
	      ;; to convert into a more stable tag format.
	      (let* ((norm (semanticdb-normalize-one-tag tab T))
		     (ntab (car norm))
		     (ntag (cdr norm))
		     (nametable ntab))

		;; If it didn't normalize, use what we had.
		(if (not norm)
		    (setq nametable tab)
		  (setq output (append output (list ntag))))

		;; Find-file-match allows a tool to make sure the tag is
		;; 'live', somewhere in a buffer.
		(cond ((eq find-file-match 'name)
		       (let ((f (semanticdb-full-filename nametable)))
			 (semantic--tag-put-property ntag :filename f)))
		      ((and find-file-match ntab)
		       (semanticdb-get-buffer ntab))
		      )
		))
	    )
	  (setq tmp (cdr tmp)))
	output)
    ;; @todo - I could use nconc, but I don't know what the caller may do with
    ;;         RESULTS after this is called.  Right now semantic-complete will
    ;;         recycling the input after calling this routine.
    (apply #'append (mapcar #'cdr results))))

(defun semanticdb-fast-strip-find-results (results)
  "Destructively strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
This is like `semanticdb-strip-find-results', except the input list RESULTS
will be changed."
  (apply #'nconc (mapcar #'cdr results)))

(defun semanticdb-find-results-p (resultp)
  "Non-nil if RESULTP is in the form of a semanticdb search result.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (semanticdb-abstract-table-child-p (car (car resultp)))
       (or (semantic-tag-p (car (cdr (car resultp))))
	   (null (car (cdr (car resultp)))))))

(defun semanticdb-find-result-prin1-to-string (result)
  "Presuming RESULT satisfies `semanticdb-find-results-p', provide a short PRIN1 output."
  (if (< (length result) 2)
      (concat "#<FIND RESULT "
	      (mapconcat (lambda (a)
			   (concat "(" (object-name (car a) ) " . "
				   "#<TAG LIST " (number-to-string (length (cdr a))) ">)"))
			 result
			 " ")
	      ">")
    ;; Longer results should have an abbreviated form.
    (format "#<FIND RESULT %d TAGS in %d FILES>"
	    (semanticdb-find-result-length result)
	    (length result))))

(defun semanticdb-find-result-with-nil-p (resultp)
  "Non-nil of RESULTP is in the form of a semanticdb search result.
The value nil is valid where a TABLE usually is, but only if the TAG
results include overlays.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (let ((tag-to-test (car-safe (cdr (car resultp)))))
	 (or (and (semanticdb-abstract-table-child-p (car (car resultp)))
		  (or (semantic-tag-p tag-to-test)
		      (null tag-to-test)))
	     (and (null (car (car resultp)))
		  (or (semantic-tag-with-position-p tag-to-test)
		      (null tag-to-test))))
	 )))

;;;###autoload
(defun semanticdb-find-result-length (result)
  "Number of tags found in RESULT."
  (let ((count 0))
    (mapc (lambda (onetable)
	    (setq count (+ count (1- (length onetable)))))
	  result)
    count))

;;;###autoload
(defun semanticdb-find-result-nth (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the Nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil."
  (let ((ans nil)
	(anstable nil))
    ;; Loop over each single table hit.
    (while (and (not ans) result)
      ;; For each table result, get local length, and modify
      ;; N to be that much less.
      (let ((ll (length (cdr (car result))))) ;; local length
	(if (> ll n)
	    ;; We have a local match.
	    (setq ans (nth n (cdr (car result)))
		  anstable (car (car result)))
	  ;; More to go.  Decrement N.
	  (setq n (- n ll))))
      ;; Keep moving.
      (setq result (cdr result)))
    (cons ans anstable)))

(defun semanticdb-find-result-test (result)
  "Test RESULT by accessing all the tags in the list."
  (if (not (semanticdb-find-results-p result))
      (error "Does not pass `semanticdb-find-results-p.\n"))
  (let ((len (semanticdb-find-result-length result))
	(i 0))
    (while (< i len)
      (let ((tag (semanticdb-find-result-nth result i)))
	(if (not (semantic-tag-p (car tag)))
	    (error "%d entry is not a tag" i)))
      (setq i (1+ i)))))

;;;###autoload
(defun semanticdb-find-result-nth-in-buffer (result n)
  "In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current."
  (let* ((ret (semanticdb-find-result-nth result n))
	 (ans (car ret))
	 (anstable (cdr ret)))
    ;; If we have a hit, double-check the find-file
    ;; entry.  If the file must be loaded, then gat that table's
    ;; source file into a buffer.

    (if anstable
	(let ((norm (semanticdb-normalize-one-tag anstable ans)))
	  (when norm
	    ;; The normalized tags can now be found based on that
	    ;; tags table.
	    (condition-case foo
		(progn
		  (semanticdb-set-buffer (car norm))
		  ;; Now reset ans
		  (setq ans (cdr norm)))
	      ;; Don't error for this case, but don't store
	      ;; the thing either.
	      (no-method-definition nil))
	    ))
      )
    ;; Return the tag.
    ans))

(defun semanticdb-find-result-mapc (fcn result)
  "Apply FCN to each element of find RESULT for side-effects only.
FCN takes two arguments.  The first is a TAG, and the
second is a DB from whence TAG originated.
Returns result."
  (mapc (lambda (sublst-icky)
	  (mapc (lambda (tag-icky)
		  (funcall fcn tag-icky (car sublst-icky)))
		(cdr sublst-icky)))
	result)
  result)

;;; Search Logging
;;
;; Basic logging to see what the search routines are doing.
(defvar semanticdb-find-log-flag nil
  "Non-nil means log the process of searches.")

(defvar semanticdb-find-log-buffer-name "*SemanticDB Find Log*"
  "The name of the logging buffer.")

(defun semanticdb-find-toggle-logging ()
  "Toggle semanticdb logging."
  (interactive)
  (setq semanticdb-find-log-flag (null semanticdb-find-log-flag))
  (message "Semanticdb find logging is %sabled"
	   (if semanticdb-find-log-flag "en" "dis")))

(defun semanticdb-reset-log ()
  "Reset the log buffer."
  (interactive)
  (when semanticdb-find-log-flag
    (with-current-buffer (get-buffer-create semanticdb-find-log-buffer-name)
      (erase-buffer)
      )))

(defun semanticdb-find-log-move-to-end ()
  "Move to the end of the semantic log."
  (let ((cb (current-buffer))
	(cw (selected-window)))
    (unwind-protect
	(progn
	  (set-buffer semanticdb-find-log-buffer-name)
	  (if (get-buffer-window (current-buffer) 'visible)
	      (select-window (get-buffer-window (current-buffer) 'visible)))
	  (goto-char (point-max)))
      (if cw (select-window cw))
      (set-buffer cb))))

(defun semanticdb-find-log-new-search (forwhat)
  "Start a new search FORWHAT."
  (when semanticdb-find-log-flag
    (with-current-buffer (get-buffer-create semanticdb-find-log-buffer-name)
      (insert (format "New Search: %S\n" forwhat))
      )
    (semanticdb-find-log-move-to-end)))

(defun semanticdb-find-log-activity (table result)
  "Log that TABLE has been searched and RESULT was found."
  (when semanticdb-find-log-flag
    (with-current-buffer semanticdb-find-log-buffer-name
      (insert "Table: " (object-print table)
	      " Result: " (int-to-string (length result)) " tags"
	      "\n")
      )
    (semanticdb-find-log-move-to-end)))

;;; Semanticdb find API functions
;; These are the routines actually used to perform searches.
;;
(defun semanticdb-find-tags-collector (function &optional path find-file-match
						brutish)
  "Collect all tags returned by FUNCTION over PATH.
The FUNCTION must take two arguments.  The first is TABLE,
which is a semanticdb table containing tags.  The second argument
to FUNCTION is TAGS.  TAGS may be a list of tags.  If TAGS is non-nil,
then FUNCTION should search the TAG list, not through TABLE.

See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

Note: You should leave FIND-FILE-MATCH as nil.  It is far more
efficient to take the results from any search and use
`semanticdb-strip-find-results' instead.  This argument is here
for backward compatibility.

If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree."
  (let (found match)
    (save-excursion
      ;; If path is a buffer, set ourselves up in that buffer
      ;; so that the override methods work correctly.
      (when (bufferp path) (set-buffer path))
      (if (semanticdb-find-results-p path)
	  ;; When we get find results, loop over that.
	  (dolist (tableandtags path)
	    (semantic-throw-on-input 'semantic-find-translate-path)
	    ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	    ;; `semanticdb-search-results-table', since those are system
	    ;; databases and not associated with a file.
	    (unless (and find-file-match
			 (obj-of-class-p
			  (car tableandtags) semanticdb-search-results-table))
	      (when (setq match (funcall function
					 (car tableandtags) (cdr tableandtags)))
		(when find-file-match
		  (save-excursion (semanticdb-set-buffer (car tableandtags))))
		(push (cons (car tableandtags) match) found)))
    	    )
	;; Only log searches across data bases.
	(semanticdb-find-log-new-search nil)
	;; If we get something else, scan the list of tables resulting
	;; from translating it into a list of objects.
	(dolist (table (semanticdb-find-translate-path path brutish))
	  (semantic-throw-on-input 'semantic-find-translate-path)
	  ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	  ;; `semanticdb-search-results-table', since those are system
	  ;; databases and not associated with a file.
	  (unless (and find-file-match
		       (obj-of-class-p table semanticdb-search-results-table))
	    (when (and table (setq match (funcall function table nil)))
	      (semanticdb-find-log-activity table match)
	      (when find-file-match
		(save-excursion (semanticdb-set-buffer table)))
	      (push (cons table match) found))))))
    ;; At this point, FOUND has had items pushed onto it.
    ;; This means items are being returned in REVERSE order
    ;; of the tables searched, so if you just get th CAR, then
    ;; too-bad, you may have some system-tag that has no
    ;; buffer associated with it.

    ;; It must be reversed.
    (nreverse found)))

;;;###autoload
(defun semanticdb-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-method table name tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match))

;;; Deep Searches
(defun semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match))

(defun semanticdb-deep-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

(defun semanticdb-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;; Brutish Search Routines
;;
(defun semanticdb-brute-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match t))

(defun semanticdb-brute-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match t))

(defun semanticdb-brute-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match t))

;;; Specialty Search Routines
(defun semanticdb-find-tags-external-children-of-type
  (type &optional path find-file-match)
  "Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-external-children-of-type-method table type tags))
   path find-file-match))

(defun semanticdb-find-tags-subclasses-of-type
  (type &optional path find-file-match)
  "Search for all tags of class type defined that subclass TYPE.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-subclasses-of-type-method table type tags))
   path find-file-match t))

;;; METHODS
;;
;; Default methods for semanticdb database and table objects.
;; Override these with system databases to as new types of back ends.

;;; Top level Searches
(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-abstract-table) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name name (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-abstract-table) regexp &optional tags)
  "In TABLE, find all occurrences of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-abstract-table) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-class-method ((table semanticdb-abstract-table) class &optional tags)
  "In TABLE, find all occurrences of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-class class (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-external-children-of-type-method ((table semanticdb-abstract-table) parent &optional tags)
   "In TABLE, find all occurrences of tags whose parent is the PARENT type.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
   (require 'semantic/find)
   (semantic-find-tags-external-children-of-type parent (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-subclasses-of-type-method ((table semanticdb-abstract-table) parent &optional tags)
   "In TABLE, find all occurrences of tags whose parent is the PARENT type.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
   (require 'semantic/find)
   (semantic-find-tags-subclasses-of-type parent (or tags (semanticdb-get-tags table))))

;;; Deep Searches
(defmethod semanticdb-deep-find-tags-by-name-method ((table semanticdb-abstract-table) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name name (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method ((table semanticdb-abstract-table) regexp &optional tags)
  "In TABLE, find all occurrences of tags matching REGEXP.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-for-completion-method ((table semanticdb-abstract-table) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-for-completion prefix (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(provide 'semantic/db-find)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-find"
;; End:

;;; semantic/db-find.el ends here
