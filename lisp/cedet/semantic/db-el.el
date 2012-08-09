;;; semantic/db-el.el --- Semantic database extensions for Emacs Lisp

;;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

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
;; There are a lot of Emacs Lisp functions and variables available for
;; the asking.  This adds on to the semanticdb programming interface to
;; allow all loaded Emacs Lisp functions to be queried via semanticdb.
;;
;; This allows you to use programs written for Semantic using the database
;; to also work in Emacs Lisp with no compromises.
;;

(require 'semantic/db)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  (require 'eieio-base))

(declare-function semantic-elisp-desymbolify "semantic/bovine/el")

;;; Code:

;;; Classes:
(defclass semanticdb-table-emacs-lisp (semanticdb-abstract-table)
  ((major-mode :initform emacs-lisp-mode)
   )
  "A table for returning search results from Emacs.")

(defmethod semanticdb-refresh-table ((obj semanticdb-table-emacs-lisp) &optional force)
  "Do not refresh Emacs Lisp table.
It does not need refreshing."
  nil)

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table-emacs-lisp))
  "Return nil, we never need a refresh."
  nil)

(defclass semanticdb-project-database-emacs-lisp
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-emacs-lisp
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Emacs core.")

;; Create the database, and add it to searchable databases for Emacs Lisp mode.
(defvar-mode-local emacs-lisp-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-emacs-lisp "Emacs"))
  "Search Emacs core for symbols.")

(defvar-mode-local emacs-lisp-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-emacs-lisp))
  "For an Emacs Lisp database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-emacs-lisp "Emacs System Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-emacs-lisp) filename)
  "From OBJ, return FILENAME's associated table object.
For Emacs Lisp, creates a specialized table."
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-emacs-lisp ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-emacs-lisp) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer buffer
    (eq (or mode-local-active-mode major-mode) 'emacs-lisp-mode)))

(defmethod semanticdb-full-filename ((obj semanticdb-table-emacs-lisp))
  "Fetch the full filename that OBJ refers to.
For Emacs Lisp system DB, there isn't one."
  nil)

;;; Conversion
;;
(defmethod semanticdb-normalize-tags ((obj semanticdb-table-emacs-lisp) tags)
  "Convert tags, originating from Emacs OBJ, into standardized form."
  (let ((newtags nil))
    (dolist (T tags)
      (let* ((ot (semanticdb-normalize-one-tag obj T))
	     (tag (cdr ot)))
	(setq newtags (cons tag newtags))))
    ;; There is no promise to have files associated.
    (nreverse newtags)))

(defmethod semanticdb-normalize-one-tag ((obj semanticdb-table-emacs-lisp) tag)
  "Convert one TAG, originating from Emacs OBJ, into standardized form.
If Emacs cannot resolve this symbol to a particular file, then return nil."
  ;; Here's the idea.  For each tag, get the name, then use
  ;; Emacs's `symbol-file' to get the source.  Once we have that,
  ;; we can use more typical semantic searching techniques to
  ;; get a regularly parsed tag.
  (let* ((type (cond ((semantic-tag-of-class-p tag 'function)
		      'defun)
		     ((semantic-tag-of-class-p tag 'variable)
		      'defvar)
		     ))
	 (sym (intern (semantic-tag-name tag)))
	 (file (condition-case err
		   (symbol-file sym type)
		 ;; Older [X]Emacs don't have a 2nd argument.
		 (error (symbol-file sym))))
	 )
    (if (or (not file) (not (file-exists-p file)))
	;; The file didn't exist.  Return nil.
	;; We can't normalize this tag.  Fake it out.
	(cons obj tag)
      (when (string-match "\\.elc" file)
	(setq file (concat (file-name-sans-extension file)
			   ".el"))
	(when (and (not (file-exists-p file))
		   (file-exists-p (concat file ".gz")))
	  ;; Is it a .gz file?
	  (setq file (concat file ".gz"))))

      (let* ((tab (semanticdb-file-table-object file))
	     (alltags (semanticdb-get-tags tab))
	     (newtags (semanticdb-find-tags-by-name-method
		       tab (semantic-tag-name tag)))
	     (match nil))
	;; Find the best match.
	(dolist (T newtags)
	  (when (semantic-tag-similar-p T tag)
	    (setq match T)))
	;; Backup system.
	(when (not match)
	    (setq match (car newtags)))
	;; Return it.
	(cons tab match)))))

(defun semanticdb-elisp-sym-function-arglist (sym)
  "Get the argument list for SYM.
Deal with all different forms of function.
This was snarfed out of eldoc."
  (let* ((prelim-def
	  (let ((sd (and (fboundp sym)
			 (symbol-function sym))))
	    (and (symbolp sd)
		 (condition-case err
		     (setq sd (indirect-function sym))
		   (error (setq sd nil))))
	    sd))
         (def (if (eq (car-safe prelim-def) 'macro)
                  (cdr prelim-def)
                prelim-def))
         (arglist (cond ((null def) nil)
			((byte-code-function-p def)
			 ;; This is an eieio compatibility function.
			 ;; We depend on EIEIO, so use this.
			 (eieio-compiled-function-arglist def))
                        ((eq (car-safe def) 'lambda)
                         (nth 1 def))
                        (t nil))))
    arglist))

(defun semanticdb-elisp-sym->tag (sym &optional toktype)
  "Convert SYM into a semantic tag.
TOKTYPE is a hint to the type of tag desired."
  (if (stringp sym)
      (setq sym (intern-soft sym)))
  (when sym
    (cond ((and (eq toktype 'function) (fboundp sym))
	   (require 'semantic/bovine/el)
	   (semantic-tag-new-function
	    (symbol-name sym)
	    nil	;; return type
	    (semantic-elisp-desymbolify
	     (semanticdb-elisp-sym-function-arglist sym)) ;; arg-list
	    :user-visible-flag (condition-case nil
				   (interactive-form sym)
				 (error nil))
	    ))
	  ((and (eq toktype 'variable) (boundp sym))
	   (semantic-tag-new-variable
	    (symbol-name sym)
	    nil	;; type
	    nil	;; value - ignore for now
	    ))
	  ((and (eq toktype 'type) (class-p sym))
	   (semantic-tag-new-type
	    (symbol-name sym)
	    "class"
	    (semantic-elisp-desymbolify
	     (aref (class-v semanticdb-project-database)
		   class-public-a)) ;; slots
	    (semantic-elisp-desymbolify (class-parents sym)) ;; parents
	    ))
	  ((not toktype)
	   ;; Figure it out on our own.
	   (cond ((class-p sym)
		  (semanticdb-elisp-sym->tag sym 'type))
		 ((fboundp sym)
		  (semanticdb-elisp-sym->tag sym 'function))
		 ((boundp sym)
		  (semanticdb-elisp-sym->tag sym 'variable))
		 (t nil))
	   )
	  (t nil))))

;;; Search Overrides
;;
(defvar semanticdb-elisp-mapatom-collector nil
  "Variable used to collect `mapatoms' output.")

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-emacs-lisp) name &optional tags)
  "Find all tags named NAME in TABLE.
Uses `intern-soft' to match NAME to Emacs symbols.
Return a list of tags."
  (if tags (call-next-method)
    ;; No need to search.  Use `intern-soft' which does the same thing for us.
    (let* ((sym (intern-soft name))
	   (fun (semanticdb-elisp-sym->tag sym 'function))
	   (var (semanticdb-elisp-sym->tag sym 'variable))
	   (typ (semanticdb-elisp-sym->tag sym 'type))
	   (taglst nil)
	   )
      (when (or fun var typ)
	;; If the symbol is any of these things, build the search table.
	(when var	(setq taglst (cons var taglst)))
	(when typ	(setq taglst (cons typ taglst)))
	(when fun	(setq taglst (cons fun taglst)))
	taglst
	))))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-emacs-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Uses `apropos-internal' to find matches.
Return a list of tags."
  (if tags (call-next-method)
    (delq nil (mapcar 'semanticdb-elisp-sym->tag
		      (apropos-internal regex)))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-emacs-lisp) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (delq nil (mapcar 'semanticdb-elisp-sym->tag
		      (all-completions prefix obarray)))))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-emacs-lisp) class &optional tags)
  "In TABLE, find all occurrences of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; We could implement this, but it could be messy.
    nil))

;;; Deep Searches
;;
;; For Emacs Lisp deep searches are like top level searches.
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-emacs-lisp) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-emacs-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-emacs-lisp) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for Emacs Lisp."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-emacs-lisp) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; EIEIO is the only time this matters
    (when (featurep 'eieio)
      (let* ((class (intern-soft type))
	     (taglst (when class
		       (delq nil
			     (mapcar 'semanticdb-elisp-sym->tag
				     ;; Fancy eieio function that knows all about
				     ;; built in methods belonging to CLASS.
				     (eieio-all-generic-functions class)))))
	     )
	taglst))))

(provide 'semantic/db-el)

;;; semantic/db-el.el ends here
