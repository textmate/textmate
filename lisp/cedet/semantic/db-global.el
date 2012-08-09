;;; semantic/db-global.el --- Semantic database extensions for GLOBAL

;; Copyright (C) 2002-2006, 2008-2012  Free Software Foundation, Inc.

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
;; Use GNU Global for by-name database searches.
;;
;; This will work as an "omniscient" database for a given project.
;;

(require 'cedet-global)
(require 'semantic/db-find)
(require 'semantic/symref/global)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )

;;; Code:

;;;###autoload
(defun semanticdb-enable-gnu-global-databases (mode)
  "Enable the use of the GNU Global SemanticDB back end for all files of MODE.
This will add an instance of a GNU Global database to each buffer
in a GNU Global supported hierarchy."
  (interactive
   (list (completing-read
          "Enable in Mode: " obarray
          #'(lambda (s) (get s 'mode-local-symbol-table))
          t (symbol-name major-mode))))

  ;; First, make sure the version is ok.
  (cedet-gnu-global-version-check)

  ;; Make sure mode is a symbol.
  (when (stringp mode)
    (setq mode (intern mode)))

  (let ((ih (mode-local-value mode 'semantic-init-mode-hook)))
    (eval `(setq-mode-local
	    ,mode semantic-init-mode-hook
	    (cons 'semanticdb-enable-gnu-global-hook ih))))

  )

(defun semanticdb-enable-gnu-global-hook ()
  "Add support for GNU Global in the current buffer via `semantic-init-hook'.
MODE is the major mode to support."
  (semanticdb-enable-gnu-global-in-buffer t))

(defclass semanticdb-project-database-global
  ;; @todo - convert to one DB per directory.
  (semanticdb-project-database eieio-instance-tracker)
  ()
  "Database representing a GNU Global tags file.")

(defun semanticdb-enable-gnu-global-in-buffer (&optional dont-err-if-not-available)
  "Enable a GNU Global database in the current buffer.
When GNU Global is not available for this directory, display a message
if optional DONT-ERR-IF-NOT-AVAILABLE is non-nil; else throw an error."
  (interactive "P")
  (if (cedet-gnu-global-root)
      (setq
       ;; Add to the system database list.
       semanticdb-project-system-databases
       (cons (semanticdb-project-database-global "global")
	     semanticdb-project-system-databases)
       ;; Apply the throttle.
       semanticdb-find-default-throttle
       (append semanticdb-find-default-throttle
	       '(omniscience))
       )
    (if dont-err-if-not-available
	nil; (message "No Global support in %s" default-directory)
      (error "No Global support in %s" default-directory))
    ))

;;; Classes:
(defclass semanticdb-table-global (semanticdb-search-results-table)
  ((major-mode :initform nil)
   )
  "A table for returning search results from GNU Global.")

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-global) &optional buffer)
  "Return t, pretend that this table's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  ;; @todo - hack alert!
  t)

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-global))
  "For a global database, there are no explicit tables.
For each file hit, get the traditional semantic table from that file."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-global "GNU Global Search Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))

  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-global) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; We pass in "don't load".  I wonder if we need to avoid that or not?
  (car (semanticdb-get-database-tables obj))
  )

;;; Search Overrides
;;
;; Only NAME based searches work with GLOBAL as that is all it tracks.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-global) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; Call out to GNU Global for some results.
    (let* ((semantic-symref-tool 'global)
	   (result (semantic-symref-find-tags-by-name name 'project))
	   )
      (when result
	;; We could ask to keep the buffer open, but that annoys
	;; people.
	(semantic-symref-result-get-tags result))
      )))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-global) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'global)
	   (result (semantic-symref-find-tags-by-regexp regex 'project))
	   )
      (when result
	(semantic-symref-result-get-tags result))
      )))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-global) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'global)
	   (result (semantic-symref-find-tags-by-completion prefix 'project))
	   (faketags nil)
	   )
      (when result
	(dolist (T (oref result :hit-text))
	  ;; We should look up each tag one at a time, but I'm lazy!
	  ;; Doing this may be good enough.
	  (setq faketags (cons
			  (semantic-tag T 'function :faux t)
			  faketags))
	  )
	faketags))))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-global) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-global) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-global) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for global."
  (semanticdb-find-tags-for-completion-method table prefix tags))

(provide 'semantic/db-global)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-global"
;; End:

;;; semantic/db-global.el ends here
