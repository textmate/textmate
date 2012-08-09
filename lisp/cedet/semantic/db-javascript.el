;;; semantic/db-javascript.el --- Semantic database extensions for javascript

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Joakim Verona

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
;; Semanticdb database for Javascript.
;;
;; This is an omniscient database with a hard-coded list of symbols for
;; Javascript.  See the doc at the end of this file for adding or modifying
;; the list of tags.
;;

(require 'semantic/db)
(require 'semantic/db-find)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt))

;;; Code:
(defvar semanticdb-javascript-tags
  '(("eval" function
     (:arguments
      (("x" variable nil nil nil)))
     nil nil)
    ("parseInt" function
     (:arguments
      (("string" variable nil nil nil)
       ("radix" variable nil nil nil)))
     nil nil)
    ("parseFloat" function
     (:arguments
      (("string" variable nil nil nil)))
     nil nil)
    ("isNaN" function
     (:arguments
      (("number" variable nil nil nil)))
     nil nil)
    ("isFinite" function
     (:arguments
      (("number" variable nil nil nil)))
     nil nil)
    ("decodeURI" function
     (:arguments
      (("encodedURI" variable nil nil nil)))
     nil nil)
    ("decodeURIComponent" function
     (:arguments
      (("encodedURIComponent" variable nil nil nil)))
     nil nil)
    ("encodeURI" function
     (:arguments
      (("uri" variable nil nil nil)))
     nil nil)
    ("encodeURIComponent" function
     (:arguments
      (("uriComponent" variable nil nil nil)))
     nil nil))
  "Hard-coded list of javascript tags for semanticdb.
See bottom of this file for instructions on managing this list.")

;;; Classes:
(defclass semanticdb-table-javascript (semanticdb-search-results-table)
  ((major-mode :initform javascript-mode)
   )
  "A table for returning search results from javascript.")

(defclass semanticdb-project-database-javascript
  (semanticdb-project-database
   eieio-singleton ;this db is for js globals, so singleton is appropriate
   )
  ((new-table-class :initform semanticdb-table-javascript
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing javascript.")

;; Create the database, and add it to searchable databases for javascript mode.
(defvar-mode-local javascript-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-javascript "Javascript"))
  "Search javascript for symbols.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
(defvar-mode-local javascript-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-javascript))
  "For a javascript database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; NOTE: This method overrides an accessor for the `tables' slot in
  ;;       a database.  You can either construct your own (like tmp here
  ;;       or you can manage any number of tables.

  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-javascript "tmp")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method)
  )

(defmethod semanticdb-file-table ((obj semanticdb-project-database-javascript) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See not for `semanticdb-get-database-tables'.
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-javascript ))
  "Return the list of tags belonging to TABLE."
  ;; NOTE: Omniscient databases probably don't want to keep large tables
  ;;       lolly-gagging about.  Keep internal Emacs tables empty and
  ;;       refer to alternate databases when you need something.
  semanticdb-javascript-tags)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-javascript) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer buffer
    (eq (or mode-local-active-mode major-mode) 'javascript-mode)))

;;; Usage
;;
;; Unlike other tables, an omniscient database does not need to
;; be associated with a path.  Use this routine to always add ourselves
;; to a search list.
(define-mode-local-override semanticdb-find-translate-path javascript-mode
  (path brutish)
  "Return a list of semanticdb tables associated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
	  ;; so that our Javascript database only shows up once when
	  ;; we append it in this iteration.
	  (let ((semanticdb-search-system-databases nil)
		)
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
	default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defun semanticdb-javascript-regexp-search (regexp)
  "Search for REGEXP in our fixed list of javascript tags."
  (let* ((tags semanticdb-javascript-tags)
	 (result nil))
    (while tags
      (if (string-match regexp (caar tags))
	  (setq result (cons (car tags) result)))
      (setq tags (cdr tags)))
    result))

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-javascript) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    (assoc-string name  semanticdb-javascript-tags)
    ))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-javascript) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (semanticdb-javascript-regexp-search regex)

    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-javascript) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (semanticdb-javascript-regexp-search (concat "^" prefix ".*"))
    ))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-javascript) class &optional tags)
  "In TABLE, find all occurrences of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; Note: This search method could be considered optional in an
    ;;       omniscient database.  It may be unwise to return all tags
    ;;       that exist for a language that are a variable or function.
    ;;
    ;; If it is optional, you can just delete this method.
    nil))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-javascript) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for javascript."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-javascript) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for javascript."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-javascript) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for javascript."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-javascript) type &optional tags)
  "Find all nonterminals which are child elements of TYPE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; OPTIONAL: This could be considered an optional function.  It is
    ;;       used for `semantic-adopt-external-members' and may not
    ;;       be possible to do in your language.
    ;;
    ;; If it is optional, you can just delete this method.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun semanticdb-javascript-strip-tags (tags)
  "Strip TAGS from overlays and reparse symbols."
  (cond ((and (consp tags) (eq 'reparse-symbol (car tags)))
	 nil)
	((overlayp tags) nil)
	((atom tags) tags)
	(t (cons (semanticdb-javascript-strip-tags
		  (car tags)) (semanticdb-javascript-strip-tags
			       (cdr tags))))))

;this list was made from a javascript file, and the above function
;; function eval(x){}
;; function parseInt(string,radix){}
;; function parseFloat(string){}
;; function isNaN(number){}
;; function isFinite(number){}
;; function decodeURI(encodedURI){}
;; function decodeURIComponent (encodedURIComponent){}
;; function encodeURI (uri){}
;; function encodeURIComponent (uriComponent){}

(provide 'semantic/db-javascript)

;;; semantic/db-javascript.el ends here
