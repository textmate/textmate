;;; rng-loc.el --- locate the schema to use for validation

;; Copyright (C) 2003, 2007-2012  Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

(require 'nxml-util)
(require 'nxml-parse)
(require 'rng-parse)
(require 'rng-uri)
(require 'rng-util)
(require 'xmltok)

(defvar rng-current-schema-file-name nil
  "Filename of schema being used for current buffer.
It is nil if using a vacuous schema.")
(make-variable-buffer-local 'rng-current-schema-file-name)

(defvar rng-schema-locating-files-default
  (list "schemas.xml" (expand-file-name "schema/schemas.xml" data-directory))
  "Default value for variable `rng-schema-locating-files'.")

(defvar rng-schema-locating-file-schema-file
  (expand-file-name "schema/locate.rnc" data-directory)
  "File containing schema for schema locating files.")

(defvar rng-schema-locating-file-schema nil
  "Schema for schema locating files or nil if not yet loaded.")

(defcustom rng-schema-locating-files rng-schema-locating-files-default
  "List of schema locating files."
  :type '(repeat file)
  :group 'relax-ng)

(defvar rng-schema-loader-alist '(("rnc" . rng-c-load-schema))
  "Alist of schema extensions vs schema loader functions.")

(defvar rng-cached-document-element nil)

(defvar rng-document-type-history nil)

(defun rng-set-document-type (type-id)
  (interactive (list (rng-read-type-id)))
  (condition-case err
      (when (not (string= type-id ""))
	(let ((schema-file (rng-locate-schema-file type-id)))
	  (unless schema-file
	    (error "Could not locate schema for type id `%s'" type-id))
	  (rng-set-schema-file-1 schema-file))
	(rng-save-schema-location-1 t type-id)
	(rng-what-schema))
    (nxml-file-parse-error
     (nxml-display-file-parse-error err))))

(defun rng-read-type-id ()
  (condition-case err
      (let ((type-ids (rng-possible-type-ids))
	    (completion-ignore-case nil))
	(completing-read "Document type id: "
			 (mapcar (lambda (x) (cons x nil))
				 type-ids)
			 nil
			 t
			 nil
			 'rng-document-type-history))
    (nxml-file-parse-error
     (nxml-display-file-parse-error err))))

(defun rng-set-schema-file (filename)
  "Set the schema for the current buffer to the schema in FILENAME.
FILENAME must be the name of a file containing a schema.
The extension of FILENAME is used to determine what kind of schema it
is.  The variable `rng-schema-loader-alist' maps from schema
extensions to schema loader functions.  The function
`rng-c-load-schema' is the loader for RELAX NG compact syntax.  The
association is between the buffer and the schema: the association is
lost when the buffer is killed."
  (interactive "fSchema file: ")
  (condition-case err
      (progn
	(rng-set-schema-file-1 filename)
	(rng-save-schema-location-1 t))
    (nxml-file-parse-error
     (nxml-display-file-parse-error err))))

(defun rng-set-vacuous-schema ()
  "Set the schema for the current buffer to allow any well-formed XML."
  (interactive)
  (rng-set-schema-file-1 nil)
  (rng-what-schema))

(defun rng-set-schema-file-1 (filename)
  (setq filename (and filename (expand-file-name filename)))
  (setq rng-current-schema
	(if filename
	    (rng-load-schema filename)
	  rng-any-element))
  (setq rng-current-schema-file-name filename)
  (run-hooks 'rng-schema-change-hook))

(defun rng-load-schema (filename)
  (let* ((extension (file-name-extension filename))
	 (loader (cdr (assoc extension rng-schema-loader-alist))))
    (or loader
	(if extension
	    (error "No schema loader available for file extension `%s'"
		   extension)
	  (error "No schema loader available for null file extension")))
    (funcall loader filename)))

(defun rng-what-schema ()
  "Display a message saying what schema `rng-validate-mode' is using."
  (interactive)
  (if rng-current-schema-file-name
      (message "Using schema %s"
	       (abbreviate-file-name rng-current-schema-file-name))
    (message "Using vacuous schema")))

(defun rng-auto-set-schema (&optional no-display-error)
  "Set the schema for this buffer based on the buffer's contents and file-name."
  (interactive)
  (condition-case err
      (progn
	(rng-set-schema-file-1 (rng-locate-schema-file))
	(rng-what-schema))
    (nxml-file-parse-error
     (if no-display-error
	 (error "%s at position %s in %s"
		(nth 3 err)
		(nth 2 err)
		(abbreviate-file-name (nth 1 err)))
       (nxml-display-file-parse-error err)))))

(defun rng-locate-schema-file (&optional type-id)
  "Return the file-name of the schema to use for the current buffer.
Return nil if no schema could be located.
If TYPE-ID is non-nil, then locate the schema for this TYPE-ID."
  (let* ((rng-cached-document-element nil)
	 (schema
	  (if type-id
	      (cons type-id nil)
	    (rng-locate-schema-file-using rng-schema-locating-files)))
	 files type-ids)
    (while (consp schema)
      (setq files rng-schema-locating-files)
      (setq type-id (car schema))
      (setq schema nil)
      (when (member type-id type-ids)
	(error "Type-id loop for type-id `%s'" type-id))
      (setq type-ids (cons type-id type-ids))
      (while (and files (not schema))
	(setq schema
	      (rng-locate-schema-file-from-type-id type-id
						   (car files)))
	(setq files (cdr files))))
    (and schema
	 (rng-uri-file-name schema))))

(defun rng-possible-type-ids ()
  "Return a list of the known type IDs."
  (let ((files rng-schema-locating-files)
	type-ids)
    (while files
      (setq type-ids (rng-possible-type-ids-using (car files) type-ids))
      (setq files (cdr files)))
    (rng-uniquify-equal (sort type-ids 'string<))))

(defun rng-locate-schema-file-using (files)
  "Locate a schema using the schema locating files FILES.
FILES is a list of file-names.
Return either a URI, a list (TYPE-ID) where TYPE-ID is a string,
or nil."
  (let (rules
	;; List of types that override normal order-based
	;; priority, most important first
	preferred-types
	;; Best result found so far; same form as return value.
	best-so-far)
    (while (and (progn
		  (while (and (not rules) files)
		    (setq rules (rng-get-parsed-schema-locating-file
				 (car files)))
		    (setq files (cdr files)))
		  rules)
		(or (not best-so-far) preferred-types))
      (let* ((rule (car rules))
	     (rule-type (car rule))
	     (rule-matcher (get rule-type 'rng-rule-matcher)))
	(setq rules (cdr rules))
	(cond (rule-matcher
	       (when (and (or (not best-so-far)
			      (memq rule-type preferred-types)))
			  (setq best-so-far
				(funcall rule-matcher (cdr rule)))
			  preferred-types)
		 (setq preferred-types
		       (nbutlast preferred-types
				 (length (memq rule-type preferred-types)))))
	      ((eq rule-type 'applyFollowingRules)
	       (when (not best-so-far)
		 (let ((prefer (cdr (assq 'ruleType (cdr rule)))))
		   (when (and prefer
			      (not (memq (setq prefer (intern prefer))
					 preferred-types)))
		     (setq preferred-types
			   (nconc preferred-types (list prefer)))))))
	      ((eq rule-type 'include)
	       (let ((uri (cdr (assq 'rules (cdr rule)))))
		 (when uri
		   (setq rules
			 (append (rng-get-parsed-schema-locating-file
				  (rng-uri-file-name uri))
				 rules))))))))
    best-so-far))

(put 'documentElement 'rng-rule-matcher 'rng-match-document-element-rule)
(put 'namespace 'rng-rule-matcher 'rng-match-namespace-rule)
(put 'uri 'rng-rule-matcher 'rng-match-uri-rule)
(put 'transformURI 'rng-rule-matcher 'rng-match-transform-uri-rule)
(put 'default 'rng-rule-matcher 'rng-match-default-rule)

(defun rng-match-document-element-rule (props)
  (let ((document-element (rng-document-element))
	(prefix (cdr (assq 'prefix props)))
	(local-name (cdr (assq 'localName props))))
    (and (or (not prefix)
	     (if (= (length prefix) 0)
		 (not (nth 1 document-element))
	       (string= prefix (nth 1 document-element))))
	 (or (not local-name)
	     (string= local-name
		      (nth 2 document-element)))
	 (rng-match-default-rule props))))

(defun rng-match-namespace-rule (props)
  (let ((document-element (rng-document-element))
	(ns (cdr (assq 'ns props))))
    (and document-element
	 ns
	 (eq (nth 0 document-element)
	     (if (string= ns "")
		 nil
	       (nxml-make-namespace ns)))
	 (rng-match-default-rule props))))

(defun rng-document-element ()
  "Return a list (NS PREFIX LOCAL-NAME).
NS is t if the document has a non-nil, but not otherwise known namespace."
  (or rng-cached-document-element
      (setq rng-cached-document-element
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(let (xmltok-dtd)
		  (xmltok-save
		   (xmltok-forward-prolog)
		   (xmltok-forward)
		   (when (memq xmltok-type '(start-tag
					     partial-start-tag
					     empty-element
					     partial-empty-element))
		     (list (rng-get-start-tag-namespace)
			   (xmltok-start-tag-prefix)
			   (xmltok-start-tag-local-name))))))))))

(defun rng-get-start-tag-namespace ()
  (let ((prefix (xmltok-start-tag-prefix))
	namespace att value)
    (while xmltok-namespace-attributes
      (setq att (car xmltok-namespace-attributes))
      (setq xmltok-namespace-attributes (cdr xmltok-namespace-attributes))
      (when (if prefix
		(and (xmltok-attribute-prefix att)
		     (string= (xmltok-attribute-local-name att)
			      prefix))
	      (not (xmltok-attribute-prefix att)))
	(setq value (xmltok-attribute-value att))
	(setq namespace (if value (nxml-make-namespace value) t))))
    (if (and prefix (not namespace))
	t
      namespace)))

(defun rng-match-transform-uri-rule (props)
  (let ((from-pattern (cdr (assq 'fromPattern props)))
	(to-pattern (cdr (assq 'toPattern props)))
	(file-name (buffer-file-name)))
    (and file-name
	 (setq file-name (expand-file-name file-name))
	 (rng-file-name-matches-uri-pattern-p file-name from-pattern)
	 (condition-case ()
	     (let ((new-file-name
		    (replace-match
		     (save-match-data
		       (rng-uri-pattern-file-name-replace-match to-pattern))
		     t
		     nil
		     file-name)))
	       (and (file-name-absolute-p new-file-name)
		    (file-exists-p new-file-name)
		    (rng-file-name-uri new-file-name)))
	     (rng-uri-error nil)))))

(defun rng-match-uri-rule (props)
  (let ((resource (cdr (assq 'resource props)))
	(pattern (cdr (assq 'pattern props)))
	(file-name (buffer-file-name)))
    (and file-name
	 (setq file-name (expand-file-name file-name))
	 (cond (resource
		(condition-case ()
		    (eq (compare-strings (rng-uri-file-name resource)
					 0
					 nil
					 (expand-file-name file-name)
					 0
					 nil
					 nxml-file-name-ignore-case)
			t)
		  (rng-uri-error nil)))
	       (pattern
		(rng-file-name-matches-uri-pattern-p file-name
						     pattern)))
	 (rng-match-default-rule props))))

(defun rng-file-name-matches-uri-pattern-p (file-name pattern)
  (condition-case ()
      (and (let ((case-fold-search nxml-file-name-ignore-case))
	     (string-match (rng-uri-pattern-file-name-regexp pattern)
			   file-name))
	   t)
    (rng-uri-error nil)))

(defun rng-match-default-rule (props)
  (or (cdr (assq 'uri props))
      (let ((type-id (cdr (assq 'typeId props))))
	(and type-id
	     (cons (rng-collapse-space type-id) nil)))))

(defun rng-possible-type-ids-using (file type-ids)
  (let ((rules (rng-get-parsed-schema-locating-file file))
	rule)
    (while rules
      (setq rule (car rules))
      (setq rules (cdr rules))
      (cond ((eq (car rule) 'typeId)
	     (let ((id (cdr (assq 'id (cdr rule)))))
	       (when id
		 (setq type-ids
		       (cons (rng-collapse-space id)
			     type-ids)))))
	    ((eq (car rule) 'include)
	     (let ((uri (cdr (assq 'rules (cdr rule)))))
	       (when uri
		 (setq type-ids
		       (rng-possible-type-ids-using
			(rng-get-parsed-schema-locating-file
			 (rng-uri-file-name uri))
			type-ids)))))))
    type-ids))

(defun rng-locate-schema-file-from-type-id (type-id file)
  "Locate the schema for type id TYPE-ID using schema locating file FILE.
Return either a URI, a list (TYPE-ID) where TYPE-ID is a string,
or nil."
  (let ((rules (rng-get-parsed-schema-locating-file file))
	schema rule)
    (while (and rules (not schema))
      (setq rule (car rules))
      (setq rules (cdr rules))
      (cond ((and (eq (car rule) 'typeId)
		  (let ((id (assq 'id (cdr rule))))
		    (and id
			 (string= (rng-collapse-space (cdr id)) type-id))))
	     (setq schema (rng-match-default-rule (cdr rule))))
	    ((eq (car rule) 'include)
	     (let ((uri (cdr (assq 'rules (cdr rule)))))
	       (when uri
		 (setq schema
		       (rng-locate-schema-file-from-type-id
			type-id
			(rng-uri-file-name uri))))))))
    schema))

(defvar rng-schema-locating-file-alist nil)

(defun rng-get-parsed-schema-locating-file (file)
  "Return a list of rules for the schema locating file FILE."
  (setq file (expand-file-name file))
  (let ((cached (assoc file rng-schema-locating-file-alist))
	(mtime (nth 5 (file-attributes file)))
	parsed)
    (cond ((not mtime)
	   (when cached
	     (setq rng-schema-locating-file-alist
		   (delq cached rng-schema-locating-file-alist)))
	   nil)
	  ((and cached (equal (nth 1 cached) mtime))
	   (nth 2 cached))
	  (t
	   (setq parsed (rng-parse-schema-locating-file file))
	   (if cached
	       (setcdr cached (list mtime parsed))
	     (setq rng-schema-locating-file-alist
		   (cons (list file mtime parsed)
			 rng-schema-locating-file-alist)))
	   parsed))))

(defconst rng-locate-namespace-uri
  (nxml-make-namespace "http://thaiopensource.com/ns/locating-rules/1.0"))

(defun rng-parse-schema-locating-file (file)
  "Return list of rules.
Each rule has the form (TYPE (ATTR . VAL) ...), where
TYPE is a symbol for the element name, ATTR is a symbol for the attribute
and VAL is a string for the value.
Attribute values representing URIs are made absolute and xml:base
attributes are removed."
  (when (and (not rng-schema-locating-file-schema)
	     rng-schema-locating-file-schema-file)
    (setq rng-schema-locating-file-schema
	  (rng-load-schema rng-schema-locating-file-schema-file)))
  (let* ((element
	  (if rng-schema-locating-file-schema
	      (rng-parse-validate-file rng-schema-locating-file-schema
				       file)
	    (nxml-parse-file file)))
	 (children (cddr element))
	 (base-uri (rng-file-name-uri file))
	 child name rules atts att props prop-name prop-value)
    (when (equal (car element)
		 (cons rng-locate-namespace-uri "locatingRules"))
      (while children
	(setq child (car children))
	(setq children (cdr children))
	(when (consp child)
	  (setq name (car child))
	  (when (eq (car name) rng-locate-namespace-uri)
	    (setq atts (cadr child))
	    (setq props nil)
	    (while atts
	      (setq att (car atts))
	      (when (stringp (car att))
		(setq prop-name (intern (car att)))
		(setq prop-value (cdr att))
		(when (memq prop-name '(uri rules resource))
		  (setq prop-value
			(rng-uri-resolve prop-value base-uri)))
		(setq props (cons (cons prop-name prop-value)
				  props)))
	      (setq atts (cdr atts)))
	    (setq rules
		  (cons (cons (intern (cdr name)) (nreverse props))
			rules))))))
    (nreverse rules)))

(defun rng-save-schema-location ()
  "Save the association between the buffer's file and the current schema.
This ensures that the schema that is currently being used will be used
if the file is edited in a future session.  The association will be
saved to the first writable file in `rng-schema-locating-files'."
  (interactive)
  (rng-save-schema-location-1 nil))

(defun rng-save-schema-location-1 (prompt &optional type-id)
  (unless (or rng-current-schema-file-name type-id)
    (error "Buffer is using a vacuous schema"))
  (let ((files rng-schema-locating-files)
	(document-file-name (buffer-file-name))
	(schema-file-name rng-current-schema-file-name)
	file)
    (while (and files (not file))
      (if (file-writable-p (car files))
	  (setq file (expand-file-name (car files)))
	(setq files (cdr files))))
    (cond ((not file)
	   (if prompt
	       nil
	     (error "No writable schema locating file configured")))
	  ((not document-file-name)
	   (if prompt
	       nil
	     (error "Buffer does not have a filename")))
	  ((and prompt
		(not (y-or-n-p (format "Save %s to %s "
				       (if type-id
					   "type identifier"
					 "schema location")
				       file)))))
	  (t
	   (with-current-buffer (find-file-noselect file)
	     (let ((modified (buffer-modified-p)))
	       (if (> (buffer-size) 0)
		   (let (xmltok-dtd)
		     (goto-char (point-min))
		     (xmltok-save
		       (xmltok-forward-prolog)
		       (xmltok-forward)
		       (unless (eq xmltok-type 'start-tag)
			 (error "Locating file `%s' invalid" file))))
		 (insert "<?xml version=\"1.0\"?>\n"
			 "<locatingRules xmlns=\""
			 (nxml-namespace-name rng-locate-namespace-uri)
			 "\">")
		 (let ((pos (point)))
		   (insert "\n</locatingRules>\n")
		   (goto-char pos)))
	       (insert "\n")
	       (insert (let ((locating-file-uri (rng-file-name-uri file)))
			 (format "<uri resource=\"%s\" %s=\"%s\"/>"
				 (rng-escape-string
				  (rng-relative-uri
				   (rng-file-name-uri document-file-name)
				   locating-file-uri))
				 (if type-id "typeId" "uri")
				 (rng-escape-string
				  (or type-id
				      (rng-relative-uri
				       (rng-file-name-uri schema-file-name)
				       locating-file-uri))))))
	       (indent-according-to-mode)
	       (when (or (not modified)
			 (y-or-n-p (format "Save file %s "
					   (buffer-file-name))))
		 (save-buffer))))))))

(provide 'rng-loc)

;;; rng-loc.el ends here
