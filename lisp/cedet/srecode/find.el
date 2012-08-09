;;;; srecode/find.el --- Tools for finding templates in the database.

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Various routines that search through various template tables
;; in search of the right template.

(require 'srecode/ctxt)
(require 'srecode/table)
(require 'srecode/map)

(declare-function srecode-compile-file "srecode/compile")

;;; Code:

(defun srecode-table (&optional mode)
  "Return the currently active Semantic Recoder table for this buffer.
Optional argument MODE specifies the mode table to use."
  (let* ((modeq (or mode major-mode))
	 (table (srecode-get-mode-table modeq)))

    ;; If there isn't one, keep searching backwards for a table.
    (while (and (not table) (setq modeq (get-mode-local-parent modeq)))
      (setq table (srecode-get-mode-table modeq)))

    ;; Last ditch effort.
    (when (not table)
      (setq table (srecode-get-mode-table 'default)))

    table))

;;; TRACKER
;;
;; Template file tracker for between sessions.
;;
(defun srecode-load-tables-for-mode (mmode &optional appname)
  "Load all the template files for MMODE.
Templates are found in the SRecode Template Map.
See `srecode-get-maps' for more.
APPNAME is the name of an application.  In this case,
all template files for that application will be loaded."
  (require 'srecode/compile)
  (let ((files
	 (if appname
	     (apply 'append
		    (mapcar
		     (lambda (map)
		       (srecode-map-entries-for-app-and-mode map appname mmode))
		     (srecode-get-maps)))
	   (apply 'append
		  (mapcar
		   (lambda (map)
		     (srecode-map-entries-for-mode map mmode))
		   (srecode-get-maps)))))
	)
    ;; Don't recurse if we are already the 'default state.
    (when (not (eq mmode 'default))
      ;; Are we a derived mode?  If so, get the parent mode's
      ;; templates loaded too.
      (if (get-mode-local-parent mmode)
	  (srecode-load-tables-for-mode (get-mode-local-parent mmode)
					appname)
	;; No parent mode, all templates depend on the defaults being
	;; loaded in, so get that in instead.
	(srecode-load-tables-for-mode 'default appname)))

    ;; Load in templates for our major mode.
    (dolist (f files)
      (let ((mt (srecode-get-mode-table mmode))
	    )
	  (when (or (not mt) (not (srecode-mode-table-find mt (car f))))
	    (srecode-compile-file (car f)))
	))
    ))

;;; PROJECT
;;
;; Find if a template table has a project set, and if so, is the
;; current buffer in that project.
(defmethod srecode-template-table-in-project-p ((tab srecode-template-table))
  "Return non-nil if the table TAB can be used in the current project.
If TAB has a :project set, check that the directories match.
If TAB is nil, then always return t."
  (let ((proj (oref tab :project)))
    ;; Return t if the project wasn't set.
    (if (not proj) t
      ;; If the project directory was set, let's check it.
      (let ((dd (expand-file-name default-directory))
	    (projexp (regexp-quote (directory-file-name proj))))
	(if (string-match (concat "^" projexp) dd)
	    t nil)))))

;;; SEARCH
;;
;; Find a given template based on name, and features of the current
;; buffer.
(defmethod srecode-template-get-table ((tab srecode-template-table)
				       template-name &optional
				       context application)
  "Find in the template in table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies that the template should part
of a particular context.
The APPLICATION argument is unused."
  (when (srecode-template-table-in-project-p tab)
    (if context
	;; If a context is specified, then look it up there.
	(let ((ctxth (gethash context (oref tab contexthash))))
	  (when ctxth
	    (gethash template-name ctxth)))
      ;; No context, perhaps a merged name?
      (gethash template-name (oref tab namehash)))))

(defmethod srecode-template-get-table ((tab srecode-mode-table)
				       template-name &optional
				       context application)
  "Find in the template in mode table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched."
  (let* ((mt tab)
	 (tabs (oref mt :tables))
	 (ans nil))
    (while (and (not ans) tabs)
      (let ((app (oref (car tabs) :application)))
	(when (or (and (not application) (null app))
		  (and application (eq app application)))
	  (setq ans (srecode-template-get-table (car tabs) template-name
						context)))
	(setq tabs (cdr tabs))))
    (or ans
	;; Recurse to the default.
	(when (not (equal (oref tab :major-mode) 'default))
	  (srecode-template-get-table (srecode-get-mode-table 'default)
				      template-name context application)))))

;;
;; Find a given template based on a key binding.
;;
(defmethod srecode-template-get-table-for-binding
  ((tab srecode-template-table) binding &optional context)
  "Find in the template name in table TAB, the template with BINDING.
Optional argument CONTEXT specifies that the template should part
of a particular context."
  (when (srecode-template-table-in-project-p tab)
    (let* ((keyout nil)
	   (hashfcn (lambda (key value)
		      (when (and (slot-boundp value 'binding)
				 (oref value binding)
				 (= (aref (oref value binding) 0) binding))
			(setq keyout key))))
	   (contextstr (cond ((listp context)
			      (car-safe context))
			     ((stringp context)
			      context)
			     (t nil)))
	   )
      (if context
	  (let ((ctxth (gethash contextstr (oref tab contexthash))))
	    (when ctxth
	      ;; If a context is specified, then look it up there.
	      (maphash hashfcn ctxth)
	      ;; Context hashes EXCLUDE the context prefix which
	      ;; we need to include, so concat it here
	      (when keyout
		(setq keyout (concat contextstr ":" keyout)))
	      )))
      (when (not keyout)
	;; No context, or binding in context.  Try full hash.
	(maphash hashfcn (oref tab namehash)))
      keyout)))

(defmethod srecode-template-get-table-for-binding
  ((tab srecode-mode-table) binding &optional context application)
  "Find in the template name in mode table TAB, the template with BINDING.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched."
  (let* ((mt tab)
	 (tabs (oref mt :tables))
	 (ans nil))
    (while (and (not ans) tabs)
      (let ((app (oref (car tabs) :application)))
	(when (or (and (not application) (null app))
		  (and application (eq app application)))
	  (setq ans (srecode-template-get-table-for-binding
		     (car tabs) binding context)))
	(setq tabs (cdr tabs))))
    (or ans
	;; Recurse to the default.
	(when (not (equal (oref tab :major-mode) 'default))
	  (srecode-template-get-table-for-binding
	   (srecode-get-mode-table 'default) binding context)))))
;;; Interactive
;;
;; Interactive queries into the template data.
;;
(defvar srecode-read-template-name-history nil
  "History for completing reads for template names.")

(defun srecode-all-template-hash (&optional mode hash)
  "Create a hash table of all the currently available templates.
Optional argument MODE is the major mode to look for.
Optional argument HASH is the hash table to fill in."
  (let* ((mhash (or hash (make-hash-table :test 'equal)))
	 (mmode (or mode major-mode))
	 (mp (get-mode-local-parent mmode))
	 )
    ;; Get the parent hash table filled into our current hash.
    (when (not (eq mode 'default))
      (if mp
	  (srecode-all-template-hash mp mhash)
	(srecode-all-template-hash 'default mhash)))
    ;; Load up the hash table for our current mode.
    (let* ((mt (srecode-get-mode-table mmode))
	   (tabs (when mt (oref mt :tables)))
	   )
      (while tabs
	;; Exclude templates for a particular application.
	(when (and (not (oref (car tabs) :application))
		   (srecode-template-table-in-project-p (car tabs)))
	  (maphash (lambda (key temp)
		     (puthash key temp mhash)
		     )
		   (oref (car tabs) namehash)))
	(setq tabs (cdr tabs)))
      mhash)))

(defun srecode-calculate-default-template-string (hash)
  "Calculate the name of the template to use as a DEFAULT.
Templates are read from HASH.
Context into which the template is inserted is calculated
with `srecode-calculate-context'."
  (let* ((ctxt (srecode-calculate-context))
	 (ans (concat (nth 0 ctxt) ":" (nth 1 ctxt))))
    (if (gethash ans hash)
	ans
      ;; No hash at the specifics, at least offer
      ;; the prefix for the completing read
      (concat (nth 0 ctxt) ":"))))

(defun srecode-read-template-name (prompt &optional initial hist default)
  "Completing read for Semantic Recoder template names.
PROMPT is used to query for the name of the template desired.
INITIAL is the initial string to use.
HIST is a history variable to use.
DEFAULT is what to use if the user presses RET."
  (srecode-load-tables-for-mode major-mode)
  (let* ((hash (srecode-all-template-hash))
	 (def (or initial
		  (srecode-calculate-default-template-string hash))))
    (completing-read prompt hash
		     nil t def
		     (or hist
			 'srecode-read-template-name-history))))

(provide 'srecode/find)

;;; srecode/find.el ends here
