;;; semantic/db-file.el --- Save a semanticdb to a cache file.

;;; Copyright (C) 2000-2005, 2007-2012  Free Software Foundation, Inc.

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
;; A set of semanticdb classes for persistently saving caches on disk.
;;

(require 'semantic)
(require 'semantic/db)
(require 'cedet-files)

(defvar semanticdb-file-version semantic-version
  "Version of semanticdb we are writing files to disk with.")
(defvar semanticdb-file-incompatible-version "1.4"
  "Version of semanticdb we are not reverse compatible with.")

;;; Settings
;;
(defcustom semanticdb-default-file-name "semantic.cache"
  "File name of the semantic tag cache."
  :group 'semanticdb
  :type 'string)

(defcustom semanticdb-default-save-directory
  (locate-user-emacs-file "semanticdb" ".semanticdb")
  "Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory."
  :group 'semanticdb
  :type '(choice :tag "Default-Directory"
                 :menu-tag "Default-Directory"
                 (const :tag "Use current directory" :value nil)
                 (directory)))

(defcustom semanticdb-persistent-path '(always)
  "List of valid paths that semanticdb will cache tags to.
When `global-semanticdb-minor-mode' is active, tag lists will
be saved to disk when Emacs exits.  Not all directories will have
tags that should be saved.
The value should be a list of valid paths.  A path can be a string,
indicating a directory in which to save a variable.  An element in the
list can also be a symbol.  Valid symbols are `never', which will
disable any saving anywhere, `always', which enables saving
everywhere, or `project', which enables saving in any directory that
passes a list of predicates in `semanticdb-project-predicate-functions'."
  :group 'semanticdb
  :type nil)

(defcustom semanticdb-save-database-hooks nil
  "Abnormal hook run after a database is saved.
Each function is called with one argument, the object representing
the database recently written."
  :group 'semanticdb
  :type 'hook)

(defvar semanticdb-dir-sep-char (if (boundp 'directory-sep-char)
				    (symbol-value 'directory-sep-char)
				  ?/)
  "Character used for directory separation.
Obsoleted in some versions of Emacs.  Needed in others.
NOTE: This should get deleted from semantic soon.")

(defun semanticdb-fix-pathname (dir)
  "If DIR is broken, fix it.
Force DIR to end with a /.
Note: Same as `file-name-as-directory'.
NOTE: This should get deleted from semantic soon."
  (file-name-as-directory dir))
;; I didn't initially know about the above fcn.  Keep the below as a
;; reference.  Delete it someday once I've proven everything is the same.
;;  (if (not (= semanticdb-dir-sep-char (aref path (1- (length path)))))
;;      (concat path (list semanticdb-dir-sep-char))
;;    path))

;;; Classes
;;
;;;###autoload
(defclass semanticdb-project-database-file (semanticdb-project-database
					    eieio-persistent)
  ((file-header-line :initform ";; SEMANTICDB Tags save file")
   (do-backups :initform nil)
   (semantic-tag-version :initarg :semantic-tag-version
			 :initform "1.4"
			 :documentation
			 "The version of the tags saved.
The default value is 1.4.  In semantic 1.4 there was no versioning, so
when those files are loaded, this becomes the version number.
To save the version number, we must hand-set this version string.")
   (semanticdb-version :initarg :semanticdb-version
		       :initform "1.4"
		       :documentation
		       "The version of the object system saved.
The default value is 1.4.  In semantic 1.4, there was no versioning,
so when those files are loaded, this becomes the version number.
To save the version number, we must hand-set this version string.")
   )
  "Database of file tables saved to disk.")

;;; Code:
;;
(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database-file)
					       directory)
  "Create a new semantic database for DIRECTORY and return it.
If a database for DIRECTORY has already been loaded, return it.
If a database for DIRECTORY exists, then load that database, and return it.
If DIRECTORY doesn't exist, create a new one."
  ;; Make sure this is fully expanded so we don't get duplicates.
  (setq directory (file-truename directory))
  (let* ((fn (semanticdb-cache-filename dbc directory))
	 (db (or (semanticdb-file-loaded-p fn)
		 (if (file-exists-p fn)
		     (progn
		       (semanticdb-load-database fn))))))
    (unless db
      (setq db (make-instance
		dbc  ; Create the database requested.  Perhaps
		(concat (file-name-nondirectory
			 (directory-file-name
			  directory))
			"/")
		:file fn :tables nil
		:semantic-tag-version semantic-version
		:semanticdb-version semanticdb-file-version)))
    ;; Set this up here.   We can't put it in the constructor because it
    ;; would be saved, and we want DB files to be portable.
    (oset db reference-directory directory)
    db))

;;; File IO

(declare-function inversion-test "inversion")

(defun semanticdb-load-database (filename)
  "Load the database FILENAME."
  (condition-case foo
      (let* ((r (eieio-persistent-read filename))
	     (c (semanticdb-get-database-tables r))
	     (tv (oref r semantic-tag-version))
	     (fv (oref r semanticdb-version))
	     )
	;; Restore the parent-db connection
	(while c
	  (oset (car c) parent-db r)
	  (setq c (cdr c)))
	(unless (and (equal semanticdb-file-version fv)
		     (equal semantic-tag-version tv))
	  ;; Try not to load inversion unless we need it:
	  (require 'inversion)
	  (if (not (inversion-test 'semanticdb-file fv))
	      (when (inversion-test 'semantic-tag tv)
		;; Incompatible version.  Flush tables.
		(semanticdb-flush-database-tables r)
		;; Reset the version to new version.
		(oset r semantic-tag-version semantic-tag-version)
		;; Warn user
		(message "Semanticdb file is old.  Starting over for %s"
			 filename))
	    ;; Version is not ok.  Flush whole system
	    (message "semanticdb file is old.  Starting over for %s"
		     filename)
	    ;; This database is so old, we need to replace it.
	    ;; We also need to delete it from the instance tracker.
	    (delete-instance r)
	    (setq r nil)))
	r)
    (error (message "Cache Error: [%s] %s, Restart"
		    filename foo)
	   nil)))

(defun semanticdb-file-loaded-p (filename)
  "Return the project belonging to FILENAME if it was already loaded."
  (eieio-instance-tracker-find filename 'file 'semanticdb-database-list))

(defmethod semanticdb-file-directory-exists-p ((DB semanticdb-project-database-file)
					       &optional suppress-questions)
  "Does the directory the database DB needs to write to exist?
If SUPPRESS-QUESTIONS, then do not ask to create the directory."
  (let ((dest (file-name-directory (oref DB file)))
	)
    (cond ((null dest)
	   ;; @TODO - If it was never set up... what should we do ?
	   nil)
	  ((file-exists-p dest) t)
	  ((or suppress-questions
	       (and (boundp 'semanticdb--inhibit-make-directory)
		    semanticdb--inhibit-make-directory))
	   nil)
	  ((y-or-n-p (format "Create directory %s for SemanticDB? " dest))
	   (make-directory dest t)
	   t)
	  (t
	   (if (boundp 'semanticdb--inhibit-make-directory)
	       (setq semanticdb--inhibit-make-directory t))
	   nil))))

(defmethod semanticdb-save-db ((DB semanticdb-project-database-file)
			       &optional
			       suppress-questions)
  "Write out the database DB to its file.
If DB is not specified, then use the current database."
  (let ((objname (oref DB file)))
    (when (and (semanticdb-dirty-p DB)
	       (semanticdb-live-p DB)
	       (semanticdb-file-directory-exists-p DB suppress-questions)
	       (semanticdb-write-directory-p DB)
	       )
      ;;(message "Saving tag summary for %s..." objname)
      (condition-case foo
	  (eieio-persistent-save (or DB semanticdb-current-database))
	(file-error		    ; System error saving?  Ignore it.
	 (message "%S: %s" foo objname))
	(error
	 (cond
	  ((and (listp foo)
		(stringp (nth 1 foo))
		(string-match "write[- ]protected" (nth 1 foo)))
	   (message (nth 1 foo)))
	  ((and (listp foo)
		(stringp (nth 1 foo))
		(string-match "no such directory" (nth 1 foo)))
	   (message (nth 1 foo)))
	  (t
	   ;; @todo - It should ask if we are not called from a hook.
	   ;;         How?
	   (if (or suppress-questions
		   (y-or-n-p (format "Skip Error: %s ?" (car (cdr foo)))))
	       (message "Save Error: %S: %s" (car (cdr foo))
			objname)
	     (error "%S" (car (cdr foo))))))))
      (run-hook-with-args 'semanticdb-save-database-hooks
			  (or DB semanticdb-current-database))
      ;;(message "Saving tag summary for %s...done" objname)
      )
    ))

(defmethod semanticdb-live-p ((obj semanticdb-project-database))
  "Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories."
  (and (slot-boundp obj 'reference-directory)
       (file-exists-p (oref obj reference-directory))))

(defmethod semanticdb-live-p ((obj semanticdb-table))
  "Return non-nil if the file associated with OBJ is live.
Live files are either buffers in Emacs, or files existing on the filesystem."
  (let ((full-filename (semanticdb-full-filename obj)))
    (or (find-buffer-visiting full-filename)
	(file-exists-p full-filename))))

(defvar semanticdb-data-debug-on-write-error nil
  "Run the data debugger on tables that issue errors.
This variable is set to nil after the first error is encountered
to prevent overload.")

(declare-function data-debug-insert-thing "data-debug")

(defmethod object-write ((obj semanticdb-table))
  "When writing a table, we have to make sure we deoverlay it first.
Restore the overlays after writing.
Argument OBJ is the object to write."
  (when (semanticdb-live-p obj)
    (when (semanticdb-in-buffer-p obj)
      (with-current-buffer (semanticdb-in-buffer-p obj)

	;; Make sure all our tag lists are up to date.
	(semantic-fetch-tags)

	;; Try to get an accurate unmatched syntax table.
	(when (and (boundp semantic-show-unmatched-syntax-mode)
		   semantic-show-unmatched-syntax-mode)
	  ;; Only do this if the user runs unmatched syntax
	  ;; mode display entries.
	  (oset obj unmatched-syntax
		(semantic-show-unmatched-lex-tokens-fetch))
	  )

	;; Make sure pointmax is up to date
	(oset obj pointmax (point-max))
	))

    ;; Make sure that the file size and other attributes are
    ;; up to date.
    (let ((fattr (file-attributes (semanticdb-full-filename obj))))
      (oset obj fsize (nth 7 fattr))
      (oset obj lastmodtime (nth 5 fattr))
      )

    ;; Do it!
    (condition-case tableerror
	(call-next-method)
      (error
       (when semanticdb-data-debug-on-write-error
	 (require 'data-debug)
	 (data-debug-new-buffer (concat "*SEMANTICDB ERROR*"))
	 (data-debug-insert-thing obj "*" "")
	 (setq semanticdb-data-debug-on-write-error nil))
       (message "Error Writing Table: %s" (object-name obj))
       (error "%S" (car (cdr tableerror)))))

    ;; Clear the dirty bit.
    (oset obj dirty nil)
    ))

;;; State queries
;;
(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database-file))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  (let ((path semanticdb-persistent-path))
    (catch 'found
      (while path
	(cond ((stringp (car path))
	       (if (string= (oref obj reference-directory) (car path))
		   (throw 'found t)))
	      ((eq (car path) 'project)
	       ;; @TODO - EDE causes us to go in here and disable
	       ;; the old default 'always save' setting.
	       ;;
	       ;; With new default 'always' should I care?
	       (if semanticdb-project-predicate-functions
		   (if (run-hook-with-args-until-success
			'semanticdb-project-predicate-functions
			(oref obj reference-directory))
		       (throw 'found t))
		 ;; If the mode is 'project, and there are no project
		 ;; modes, then just always save the file.  If users
		 ;; wish to restrict the search, modify
		 ;; `semanticdb-persistent-path' to include desired paths.
		 (if (= (length semanticdb-persistent-path) 1)
		     (throw 'found t))
		 ))
	      ((eq (car path) 'never)
	       (throw 'found nil))
	      ((eq (car path) 'always)
	       (throw 'found t))
	      (t (error "Invalid path %S" (car path))))
	(setq path (cdr path)))
      (call-next-method))
    ))

;;; Filename manipulation
;;
(defmethod semanticdb-file-table ((obj semanticdb-project-database-file) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; Cheater option.  In this case, we always have files directly
  ;; under ourselves.  The main project type may not.
  (object-assoc (file-name-nondirectory filename) 'file (oref obj tables)))

(defmethod semanticdb-file-name-non-directory :STATIC
  ((dbclass semanticdb-project-database-file))
  "Return the file name DBCLASS will use.
File name excludes any directory part."
  semanticdb-default-file-name)

(defmethod semanticdb-file-name-directory :STATIC
  ((dbclass semanticdb-project-database-file) directory)
  "Return the relative directory to where DBCLASS will save its cache file.
The returned path is related to DIRECTORY."
  (if semanticdb-default-save-directory
      (let ((file (cedet-directory-name-to-file-name directory)))
        ;; Now create a filename for the cache file in
        ;; ;`semanticdb-default-save-directory'.
	(expand-file-name
	 file (file-name-as-directory semanticdb-default-save-directory)))
    directory))

(defmethod semanticdb-cache-filename :STATIC
  ((dbclass semanticdb-project-database-file) path)
  "For DBCLASS, return a file to a cache file belonging to PATH.
This could be a cache file in the current directory, or an encoded file
name in a secondary directory."
  ;; Use concat and not expand-file-name, because the dir part
  ;; may include some of the file name.
  (concat (semanticdb-file-name-directory dbclass path)
	  (semanticdb-file-name-non-directory dbclass)))

(defmethod semanticdb-full-filename ((obj semanticdb-project-database-file))
  "Fetch the full filename that OBJ refers to."
  (oref obj file))

;;; FLUSH OLD FILES
;;
(defun semanticdb-cleanup-cache-files (&optional noerror)
  "Cleanup any cache files associated with directories that no longer exist.
Optional NOERROR prevents errors from being displayed."
  (interactive)
  (when (and (not semanticdb-default-save-directory)
	     (not noerror))
    (error "No default save directory for semantic-save files"))

  (when semanticdb-default-save-directory

    ;; Calculate all the cache files we have.
    (let* ((regexp (regexp-quote semanticdb-default-file-name))
	   (files (directory-files semanticdb-default-save-directory
				   t regexp))
	   (orig nil)
	   (to-delete nil))
      (dolist (F files)
	(setq orig (cedet-file-name-to-directory-name
		    (file-name-nondirectory F)))
	(when (not (file-exists-p (file-name-directory orig)))
	  (setq to-delete (cons F to-delete))
	  ))
      (if to-delete
	(save-window-excursion
	  (let ((buff (get-buffer-create "*Semanticdb Delete*")))
	    (with-current-buffer buff
	      (erase-buffer)
	      (insert "The following Cache files appear to be obsolete.\n\n")
	      (dolist (F to-delete)
		(insert F "\n")))
	    (pop-to-buffer buff t t)
	    (fit-window-to-buffer (get-buffer-window buff) nil 1)
	    (when (y-or-n-p "Delete Old Cache Files? ")
	      (mapc (lambda (F)
		      (message "Deleting to %s..." F)
		      (delete-file F))
		    to-delete)
	      (message "done."))
	    ))
	;; No files to delete
	(when (not noerror)
	  (message "No obsolete semanticdb.cache files."))
	))))

(provide 'semantic/db-file)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-file"
;; End:

;;; semantic/db-file.el ends here
