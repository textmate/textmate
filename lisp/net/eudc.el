;;; eudc.el --- Emacs Unified Directory Client

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: Pavel Janík <Pavel@Janik.cz>
;; Keywords: comm

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
;;    This package provides a common interface to query directory servers using
;;    different protocols such as LDAP, CCSO PH/QI or BBDB.  Queries can be
;;    made through an interactive form or inline. Inline query strings in
;;    buffers are expanded with appropriately formatted query results
;;    (especially used to expand email addresses in message buffers).  EUDC
;;    also interfaces with the BBDB package to let you register query results
;;    into your own BBDB database.

;;; Usage:
;;    EUDC comes with an extensive documentation, please refer to it.
;;
;;    The main entry points of EUDC are:
;;      `eudc-query-form': Query a directory server from a query form
;;      `eudc-expand-inline': Query a directory server for the e-mail address
;;                            of the name before cursor and insert it in the
;;                            buffer
;;      `eudc-get-phone': Get a phone number from a directory server
;;      `eudc-get-email': Get an e-mail address from a directory server
;;      `eudc-customize': Customize various aspects of EUDC

;;; Code:

(require 'wid-edit)

(eval-and-compile
  (if (not (fboundp 'make-overlay))
      (require 'overlay))
  (if (not (fboundp 'unless))
      (require 'cl)))

(unless (fboundp 'custom-menu-create)
  (autoload 'custom-menu-create "cus-edit"))

(require 'eudc-vars)



;;{{{      Internal cooking

;;{{{      Internal variables and compatibility tricks

(defvar eudc-form-widget-list nil)

(defvar eudc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-this-buffer)
    (define-key map "x" 'kill-this-buffer)
    (define-key map "f" 'eudc-query-form)
    (define-key map "b" 'eudc-try-bbdb-insert)
    (define-key map "n" 'eudc-move-to-next-record)
    (define-key map "p" 'eudc-move-to-previous-record)
    map))
(set-keymap-parent eudc-mode-map widget-keymap)

(defvar mode-popup-menu)

;; List of known servers
;; Alist of (SERVER . PROTOCOL)
(defvar eudc-server-hotlist nil)

;; List of variables that have server- or protocol-local bindings
(defvar eudc-local-vars nil)

;; Protocol local. Query function
(defvar eudc-query-function nil)

;; Protocol local.  A function that retrieves a list of valid attribute names
(defvar eudc-list-attributes-function nil)

;; Protocol local. A mapping between EUDC attribute names and corresponding
;; protocol specific names.  The following names are defined by EUDC and may be
;; included in that list: `name' , `firstname', `email', `phone'
(defvar eudc-protocol-attributes-translation-alist nil)

;; Protocol local. Mapping between protocol attribute names and BBDB field
;; names
(defvar eudc-bbdb-conversion-alist nil)

;; Protocol/Server local. Hook called upon switching to that server
(defvar eudc-switch-to-server-hook nil)

;; Protocol/Server local. Hook called upon switching from that server
(defvar eudc-switch-from-server-hook nil)

;; Protocol local. Whether the protocol supports queries with no specified
;; attribute name
(defvar eudc-protocol-has-default-query-attributes nil)

(defun eudc-cadr (obj)
  (car (cdr obj)))

(defun eudc-cdar (obj)
  (cdr (car obj)))

(defun eudc-caar (obj)
  (car (car obj)))

(defun eudc-cdaar (obj)
  (cdr (car (car obj))))

(defun eudc-plist-member (plist prop)
  "Return t if PROP has a value specified in PLIST."
  (if (not (= 0 (% (length plist) 2)))
      (error "Malformed plist"))
  (catch 'found
    (while plist
      (if (eq prop (car plist))
	  (throw 'found t))
      (setq plist (cdr (cdr plist))))
    nil))

;; Emacs's plist-get lacks third parameter
(defun eudc-plist-get (plist prop &optional default)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or DEFAULT if PROP is not
one of the properties on the list."
  (if (eudc-plist-member plist prop)
      (plist-get plist prop)
    default))

(defun eudc-lax-plist-get (plist prop &optional default)
  "Extract a value from a lax property list.

PLIST is a lax property list, which is a list of the form (PROP1
VALUE1 PROP2 VALUE2...), where comparisons between properties are done
using `equal' instead of `eq'.  This function returns the value
corresponding to PROP, or DEFAULT if PROP is not one of the
properties on the list."
  (if (not (= 0 (% (length plist) 2)))
      (error "Malformed plist"))
  (catch 'found
    (while plist
      (if (equal prop (car plist))
	  (throw 'found (car (cdr plist))))
      (setq plist (cdr (cdr plist))))
    default))

(if (not (fboundp 'split-string))
    (defun split-string (string &optional pattern)
      "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  (let (parts (start 0))
    (when (string-match pattern string 0)
      (if (> (match-beginning 0) 0)
	  (setq parts (cons (substring string 0 (match-beginning 0)) nil)))
      (setq start (match-end 0))
      (while (and (string-match pattern string start)
		  (> (match-end 0) start))
	(setq parts (cons (substring string start (match-beginning 0)) parts)
	      start (match-end 0))))
    (nreverse (if (< start (length string))
		  (cons (substring string start) parts)
		parts)))))

(defun eudc-replace-in-string (str regexp newtext)
  "Replace all matches in STR for REGEXP with NEWTEXT.
Value is the new string."
  (let ((rtn-str "")
	(start 0)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat rtn-str
		    (substring str prev-start match)
		    newtext)))
    (concat rtn-str (substring str start))))

;;}}}

;;{{{ Server and Protocol Variable Routines

(defun eudc-server-local-variable-p (var)
  "Return non-nil if VAR has server-local bindings."
  (eudc-plist-member (get var 'eudc-locals) 'server))

(defun eudc-protocol-local-variable-p (var)
  "Return non-nil if VAR has protocol-local bindings."
  (eudc-plist-member (get var 'eudc-locals) 'protocol))

(defun eudc-default-set (var val)
  "Set the EUDC default value of VAR to VAL.
The current binding of VAR is not changed."
  (put var 'eudc-locals
       (plist-put (get var 'eudc-locals) 'default val))
  (add-to-list 'eudc-local-vars var))

(defun eudc-protocol-set (var val &optional protocol)
  "Set the PROTOCOL-local binding of VAR to VAL.
If omitted PROTOCOL defaults to the current value of `eudc-protocol'.
The current binding of VAR is changed only if PROTOCOL is omitted."
  (if (eq 'unbound (eudc-variable-default-value var))
      (eudc-default-set var (symbol-value var)))
  (let* ((eudc-locals (get var 'eudc-locals))
	 (protocol-locals (eudc-plist-get eudc-locals 'protocol)))
    (setq protocol-locals (plist-put protocol-locals (or protocol
							 eudc-protocol) val))
    (setq eudc-locals
	  (plist-put eudc-locals 'protocol protocol-locals))
    (put var 'eudc-locals eudc-locals)
    (add-to-list 'eudc-local-vars var)
    (unless protocol
      (eudc-update-variable var))))

(defun eudc-server-set (var val &optional server)
  "Set the SERVER-local binding of VAR to VAL.
If omitted SERVER defaults to the current value of `eudc-server'.
The current binding of VAR is changed only if SERVER is omitted."
  (if (eq 'unbound (eudc-variable-default-value var))
      (eudc-default-set var (symbol-value var)))
  (let* ((eudc-locals (get var 'eudc-locals))
	 (server-locals (eudc-plist-get eudc-locals 'server)))
    (setq server-locals (plist-put server-locals (or server
						     eudc-server) val))
    (setq eudc-locals
	  (plist-put eudc-locals 'server server-locals))
    (put var 'eudc-locals eudc-locals)
    (add-to-list 'eudc-local-vars var)
    (unless server
      (eudc-update-variable var))))


(defun eudc-set (var val)
  "Set the most local (server, protocol or default) binding of VAR to VAL.
The current binding of VAR is also set to VAL"
  (cond
   ((not (eq 'unbound (eudc-variable-server-value var)))
    (eudc-server-set var val))
   ((not (eq 'unbound (eudc-variable-protocol-value var)))
    (eudc-protocol-set var val))
   (t
    (eudc-default-set var val)))
  (set var val))

(defun eudc-variable-default-value (var)
  "Return the default binding of VAR.
Return `unbound' if VAR has no EUDC default value."
  (let ((eudc-locals (get var 'eudc-locals)))
    (if (and (boundp var)
	     eudc-locals)
	(eudc-plist-get eudc-locals 'default 'unbound)
      'unbound)))

(defun eudc-variable-protocol-value (var &optional protocol)
  "Return the value of VAR local to PROTOCOL.
Return `unbound' if VAR has no value local to PROTOCOL.
PROTOCOL defaults to `eudc-protocol'"
  (let* ((eudc-locals (get var 'eudc-locals))
	 protocol-locals)
    (if (not (and  (boundp var)
		   eudc-locals
		   (eudc-plist-member eudc-locals 'protocol)))
	'unbound
      (setq protocol-locals (eudc-plist-get eudc-locals 'protocol))
      (eudc-lax-plist-get protocol-locals
			  (or protocol
			      eudc-protocol) 'unbound))))

(defun eudc-variable-server-value (var &optional server)
  "Return the value of VAR local to SERVER.
Return `unbound' if VAR has no value local to SERVER.
SERVER defaults to `eudc-server'"
  (let* ((eudc-locals (get var 'eudc-locals))
	 server-locals)
    (if (not (and (boundp var)
		  eudc-locals
		  (eudc-plist-member eudc-locals 'server)))
	'unbound
      (setq server-locals (eudc-plist-get eudc-locals 'server))
      (eudc-lax-plist-get server-locals
			  (or server
			      eudc-server) 'unbound))))

(defun eudc-update-variable (var)
  "Set the value of VAR according to its locals.
If the VAR has a server- or protocol-local value corresponding
to the current `eudc-server' and `eudc-protocol' then it is set
accordingly. Otherwise it is set to its EUDC default binding"
  (let (val)
    (cond
     ((not (eq 'unbound (setq val (eudc-variable-server-value var))))
      (set var val))
     ((not (eq 'unbound (setq val (eudc-variable-protocol-value var))))
      (set var val))
     ((not (eq 'unbound (setq val (eudc-variable-default-value var))))
      (set var val)))))

(defun eudc-update-local-variables ()
  "Update all EUDC variables according to their local settings."
  (interactive)
  (mapcar 'eudc-update-variable eudc-local-vars))

(eudc-default-set 'eudc-query-function nil)
(eudc-default-set 'eudc-list-attributes-function nil)
(eudc-default-set 'eudc-protocol-attributes-translation-alist nil)
(eudc-default-set 'eudc-bbdb-conversion-alist nil)
(eudc-default-set 'eudc-switch-to-server-hook nil)
(eudc-default-set 'eudc-switch-from-server-hook nil)
(eudc-default-set 'eudc-protocol-has-default-query-attributes nil)
(eudc-default-set 'eudc-attribute-display-method-alist nil)

;;}}}


;; Add PROTOCOL to the list of supported protocols
(defun eudc-register-protocol (protocol)
  (unless (memq protocol eudc-supported-protocols)
    (setq eudc-supported-protocols
	  (cons protocol eudc-supported-protocols))
    (put 'eudc-protocol 'custom-type
	 `(choice :menu-tag "Protocol"
		  ,@(mapcar (lambda (s)
			      (list 'string ':tag (symbol-name s)))
			    eudc-supported-protocols))))
  (or (memq protocol eudc-known-protocols)
      (setq eudc-known-protocols
	    (cons protocol eudc-known-protocols))))


(defun eudc-translate-query (query)
  "Translate attribute names of QUERY.
The translation is done according to
`eudc-protocol-attributes-translation-alist'."
  (if eudc-protocol-attributes-translation-alist
      (mapcar (lambda (attribute)
                (let ((trans (assq (car attribute)
                                   (symbol-value eudc-protocol-attributes-translation-alist))))
                  (if trans
                      (cons (cdr trans) (cdr attribute))
                    attribute)))
	      query)
    query))

(defun eudc-translate-attribute-list (list)
  "Translate a list of attribute names LIST.
The translation is done according to
`eudc-protocol-attributes-translation-alist'."
  (if eudc-protocol-attributes-translation-alist
      (let (trans)
	(mapcar (lambda (attribute)
		   (setq trans (assq attribute
				     (symbol-value eudc-protocol-attributes-translation-alist)))
		   (if trans
		       (cdr trans)
		     attribute))
		list))
    list))

(defun eudc-select (choices beg end)
  "Choose one from CHOICES using a completion.
BEG and END delimit the text which is to be replaced."
  (let ((replacement))
   (setq replacement
	 (completing-read "Multiple matches found; choose one: "
			  (mapcar 'list choices)))
   (delete-region beg end)
   (insert replacement)))

(defun eudc-query (query &optional return-attributes no-translation)
   "Query the current directory server with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTR is an attribute
name and VALUE the corresponding value.
If NO-TRANSLATION is non-nil, ATTR is translated according to
`eudc-protocol-attributes-translation-alist'.
RETURN-ATTRIBUTES is a list of attributes to return defaulting to
`eudc-default-return-attributes'."
   (unless eudc-query-function
     (error "Don't know how to perform the query"))
   (if no-translation
       (funcall eudc-query-function query (or return-attributes
					      eudc-default-return-attributes))

     (funcall eudc-query-function
	      (eudc-translate-query query)
	      (cond
	       (return-attributes
		(eudc-translate-attribute-list return-attributes))
	       ((listp eudc-default-return-attributes)
		(eudc-translate-attribute-list eudc-default-return-attributes))
	       (t
		eudc-default-return-attributes)))))

(defun eudc-format-attribute-name-for-display (attribute)
  "Format a directory attribute name for display.
ATTRIBUTE is looked up in `eudc-user-attribute-names-alist' and replaced
by the corresponding user name if any.  Otherwise it is capitalized and
underscore characters are replaced by spaces."
  (let ((match (assq attribute eudc-user-attribute-names-alist)))
    (if match
	(cdr match)
      (capitalize
       (mapconcat 'identity
		  (split-string (symbol-name attribute) "_")
		  " ")))))

(defun eudc-print-attribute-value (field)
  "Insert the value of the directory FIELD at point.
The directory attribute name in car of FIELD is looked up in
`eudc-attribute-display-method-alist' and the corresponding method,
if any, is called to print the value in cdr of FIELD."
  (let ((match (assoc (downcase (car field))
		      eudc-attribute-display-method-alist))
	(col (current-column))
	(val (cdr field)))
    (if match
	(progn
	  (eval (list (cdr match) val))
	  (insert "\n"))
      (mapcar
       (function
	(lambda (val-elem)
	  (indent-to col)
	  (insert val-elem "\n")))
       (cond
	((listp val) val)
	((stringp val) (split-string val "\n"))
	((null val) '(""))
	(t (list val)))))))

(defun eudc-print-record-field (field column-width)
  "Print the record field FIELD.
FIELD is a list (ATTR VALUE1 VALUE2 ...) or cons-cell (ATTR . VAL)
COLUMN-WIDTH is the width of the first display column containing the
attribute name ATTR."
  (let ((field-beg (point)))
;; The record field that is passed to this function has already been processed
;; by `eudc-format-attribute-name-for-display' so we don't need to call it
;; again to display the attribute name
    (insert (format (concat "%" (int-to-string column-width) "s: ")
		    (car field)))
    (put-text-property field-beg (point) 'face 'bold)
    (indent-to (+ 2 column-width))
    (eudc-print-attribute-value field)))

(defun eudc-display-records (records &optional raw-attr-names)
  "Display the record list RECORDS in a formatted buffer.
If RAW-ATTR-NAMES is non-nil, the raw attribute names are displayed
otherwise they are formatted according to `eudc-user-attribute-names-alist'."
  (let (inhibit-read-only
	precords
	(width 0)
	beg
	first-record
	attribute-name)
    (with-output-to-temp-buffer "*Directory Query Results*"
      (with-current-buffer standard-output
	(setq buffer-read-only t)
	(setq inhibit-read-only t)
	(erase-buffer)
	(insert "Directory Query Result\n")
	(insert "======================\n\n\n")
	(if (null records)
	    (insert "No match found.\n"
		    (if eudc-strict-return-matches
			"Try setting `eudc-strict-return-matches' to nil or change `eudc-default-return-attributes'.\n"
		      ""))
	  ;; Replace field names with user names, compute max width
	  (setq precords
		(mapcar
		 (function
		  (lambda (record)
		    (mapcar
		     (function
		      (lambda (field)
			(setq attribute-name
			      (if raw-attr-names
				  (symbol-name (car field))
				(eudc-format-attribute-name-for-display (car field))))
			(if (> (length attribute-name) width)
			    (setq width (length attribute-name)))
			(cons attribute-name (cdr field))))
		     record)))
		 records))
	  ;; Display the records
	  (setq first-record (point))
	  (mapc
	   (function
	    (lambda (record)
	      (setq beg (point))
	      ;; Map over the record fields to print the attribute/value pairs
	      (mapc (function
		     (lambda (field)
		       (eudc-print-record-field field width)))
		    record)
	      ;; Store the record internal format in some convenient place
	      (overlay-put (make-overlay beg (point))
			   'eudc-record
			   (car records))
	      (setq records (cdr records))
	      (insert "\n")))
	   precords))
	(insert "\n")
	(widget-create 'push-button
		       :notify (lambda (&rest ignore)
				 (eudc-query-form))
		       "New query")
	(widget-insert " ")
	(widget-create 'push-button
		       :notify (lambda (&rest ignore)
				 (kill-this-buffer))
		       "Quit")
	(eudc-mode)
	(widget-setup)
	(if first-record
	    (goto-char first-record))))))

(defun eudc-process-form ()
  "Process the query form in current buffer and display the results."
  (let (query-alist
	value)
    (if (not (and (boundp 'eudc-form-widget-list)
		  eudc-form-widget-list))
	(error "Not in a directory query form buffer")
      (mapc (function
	     (lambda (wid-field)
	       (setq value (widget-value (cdr wid-field)))
	       (if (not (string= value ""))
		   (setq query-alist (cons (cons (car wid-field) value)
					   query-alist)))))
	    eudc-form-widget-list)
      (kill-buffer (current-buffer))
      (eudc-display-records (eudc-query query-alist) eudc-use-raw-directory-names))))


(defun eudc-filter-duplicate-attributes (record)
  "Filter RECORD according to `eudc-duplicate-attribute-handling-method'."
  (let ((rec record)
	unique
	duplicates
	result)

    ;; Search for multiple records
    (while (and rec
		(not (listp (eudc-cdar rec))))
      (setq rec (cdr rec)))

    (if (null (eudc-cdar rec))
	(list record)			; No duplicate attrs in this record
      (mapc (function
	     (lambda (field)
	       (if (listp (cdr field))
		   (setq duplicates (cons field duplicates))
		 (setq unique (cons field unique)))))
	    record)
      (setq result (list unique))
      ;; Map over the record fields that have multiple values
      (mapc
       (function
	(lambda (field)
	  (let ((method (if (consp eudc-duplicate-attribute-handling-method)
			    (cdr
			     (assq
			      (or
			       (car
				(rassq
				 (car field)
				 (symbol-value
				  eudc-protocol-attributes-translation-alist)))
			       (car field))
			      eudc-duplicate-attribute-handling-method))
			  eudc-duplicate-attribute-handling-method)))
	    (cond
	     ((or (null method) (eq 'list method))
	      (setq result
		    (eudc-add-field-to-records field result)))
	     ((eq 'first method)
	      (setq result
		    (eudc-add-field-to-records (cons (car field)
						     (eudc-cadr field))
					       result)))
	     ((eq 'concat method)
	      (setq result
		    (eudc-add-field-to-records (cons (car field)
						     (mapconcat
						      'identity
						      (cdr field)
						      "\n")) result)))
	     ((eq 'duplicate method)
	      (setq result
		    (eudc-distribute-field-on-records field result)))))))
       duplicates)
      result)))

(defun eudc-filter-partial-records (records attrs)
  "Eliminate records that do not contain all ATTRS from RECORDS."
  (delq nil
	(mapcar
	 (function
	  (lambda (rec)
	    (if (eval (cons 'and
		       (mapcar
			(function
			 (lambda (attr)
			   (consp (assq attr rec))))
			attrs)))
		rec)))
	 records)))

(defun eudc-add-field-to-records (field records)
  "Add FIELD to each individual record in RECORDS and return the resulting list."
  (mapcar (function
	   (lambda (r)
	     (cons field r)))
	  records))

(defun eudc-distribute-field-on-records (field records)
  "Duplicate each individual record in RECORDS according to value of FIELD.
Each copy is added a new field containing one of the values of FIELD."
  (let (result
	(values (cdr field)))
    ;; Uniquify values first
    (while values
      (setcdr values (delete (car values) (cdr values)))
      (setq values (cdr values)))
    (mapc
     (function
      (lambda (value)
	(let ((result-list (copy-sequence records)))
	  (setq result-list (eudc-add-field-to-records
			     (cons (car field) value)
			     result-list))
	  (setq result (append result-list result))
		 )))
	    (cdr field))
    result))


(defun eudc-mode ()
  "Major mode used in buffers displaying the results of directory queries.
There is no sense in calling this command from a buffer other than
one containing the results of a directory query.

These are the special commands of EUDC mode:
    q -- Kill this buffer.
    f -- Display a form to query the current directory server.
    n -- Move to next record.
    p -- Move to previous record.
    b -- Insert record at point into the BBDB database."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'eudc-mode)
  (setq mode-name "EUDC")
  (use-local-map eudc-mode-map)
  (if (not (featurep 'xemacs))
      (easy-menu-define eudc-emacs-menu eudc-mode-map "" (eudc-menu))
    (setq mode-popup-menu (eudc-menu)))
  (run-mode-hooks 'eudc-mode-hook))

;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-customize ()
  "Customize the EUDC package."
  (interactive)
  (customize-group 'eudc))

;;;###autoload
(defun eudc-set-server (server protocol &optional no-save)
  "Set the directory server to SERVER using PROTOCOL.
Unless NO-SAVE is non-nil, the server is saved as the default
server for future sessions."
  (interactive (list
		(read-from-minibuffer "Directory Server: ")
		(intern (completing-read "Protocol: "
					 (mapcar (lambda (elt)
						    (cons (symbol-name elt)
							  elt))
						 eudc-known-protocols)))))
  (unless (or (member protocol
		      eudc-supported-protocols)
	      (load (concat "eudcb-" (symbol-name protocol)) t))
    (error "Unsupported protocol: %s" protocol))
  (run-hooks 'eudc-switch-from-server-hook)
  (setq eudc-protocol protocol)
  (setq eudc-server server)
  (eudc-update-local-variables)
  (run-hooks 'eudc-switch-to-server-hook)
  (if (called-interactively-p 'interactive)
      (message "Current directory server is now %s (%s)" eudc-server eudc-protocol))
  (if (null no-save)
      (eudc-save-options)))

;;;###autoload
(defun eudc-get-email (name &optional error)
  "Get the email field of NAME from the directory server.
If ERROR is non-nil, report an error if there is none."
  (interactive "sName: \np")
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((result (eudc-query (list (cons 'name name)) '(email)))
	email)
    (if (null (cdr result))
	(setq email (eudc-cdaar result))
      (error "Multiple match--use the query form"))
    (if error
	(if email
	    (message "%s" email)
	  (error "No record matching %s" name)))
    email))

;;;###autoload
(defun eudc-get-phone (name &optional error)
  "Get the phone field of NAME from the directory server.
If ERROR is non-nil, report an error if there is none."
  (interactive "sName: \np")
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((result (eudc-query (list (cons 'name name)) '(phone)))
	phone)
    (if (null (cdr result))
	(setq phone (eudc-cdaar result))
      (error "Multiple match--use the query form"))
    (if error
	(if phone
	    (message "%s" phone)
	  (error "No record matching %s" name)))
    phone))

(defun eudc-get-attribute-list ()
  "Return a list of valid attributes for the current server.
When called interactively the list is formatted in a dedicated buffer
otherwise a list of symbols is returned."
  (interactive)
  (if eudc-list-attributes-function
      (let ((entries (funcall eudc-list-attributes-function
			      (called-interactively-p 'interactive))))
	(if entries
	    (if (called-interactively-p 'interactive)
		(eudc-display-records entries t)
	      entries)))
    (error "The %s protocol has no support for listing attributes" eudc-protocol)))

(defun eudc-format-query (words format)
  "Use FORMAT to build a EUDC query from WORDS."
  (let (query
	query-alist
	key val cell)
    (if format
	(progn
	  (while (and words format)
	    (setq query-alist (cons (cons (car format) (car words))
				    query-alist))
	    (setq words (cdr words)
		  format (cdr format)))
	  ;; If the same attribute appears more than once, merge
	  ;; the corresponding values
	  (setq query-alist (nreverse query-alist))
	  (while query-alist
	    (setq key (eudc-caar query-alist)
		  val (eudc-cdar query-alist)
		  cell (assq key query))
	    (if cell
		(setcdr cell (concat (cdr cell) " " val))
	      (setq query (cons (car query-alist) query)))
	    (setq query-alist (cdr query-alist)))
	  query)
      (if eudc-protocol-has-default-query-attributes
	  (mapconcat 'identity words " ")
	(list (cons 'name (mapconcat 'identity words " ")))))))

(defun eudc-extract-n-word-formats (format-list n)
  "Extract a list of N-long formats from FORMAT-LIST.
If none try N - 1 and so forth."
  (let (formats)
    (while (and (null formats)
		(> n 0))
      (setq formats
	    (delq nil
		  (mapcar (lambda (format)
			     (if (= n
				    (length format))
				 format
			       nil))
			  format-list)))
      (setq n (1- n)))
    formats))


;;;###autoload
(defun eudc-expand-inline (&optional replace)
  "Query the directory server, and expand the query string before point.
The query string consists of the buffer substring from the point back to
the preceding comma, colon or beginning of line.
The variable `eudc-inline-query-format' controls how to associate the
individual inline query words with directory attribute names.
After querying the server for the given string, the expansion specified by
`eudc-inline-expansion-format' is inserted in the buffer at point.
If REPLACE is non-nil, then this expansion replaces the name in the buffer.
`eudc-expansion-overwrites-query' being non-nil inverts the meaning of REPLACE.
Multiple servers can be tried with the same query until one finds a match,
see `eudc-inline-expansion-servers'"
  (interactive)
  (if (memq eudc-inline-expansion-servers
	    '(current-server server-then-hotlist))
      (or eudc-server
	  (call-interactively 'eudc-set-server))
    (or eudc-server-hotlist
	(error "No server in the hotlist")))
  (let* ((end (point))
	 (beg (save-excursion
		(if (re-search-backward "\\([:,]\\|^\\)[ \t]*"
					(point-at-bol) 'move)
		    (goto-char (match-end 0)))
		(point)))
	 (query-words (split-string (buffer-substring beg end) "[ \t]+"))
	 query-formats
	 response
	 response-string
	 response-strings
	 (eudc-former-server eudc-server)
	 (eudc-former-protocol eudc-protocol)
	 servers)

    ;; Prepare the list of servers to query
    (setq servers (copy-sequence eudc-server-hotlist))
    (setq servers
	  (cond
	   ((eq eudc-inline-expansion-servers 'hotlist)
	    eudc-server-hotlist)
	   ((eq eudc-inline-expansion-servers 'server-then-hotlist)
	    (cons (cons eudc-server eudc-protocol)
		  (delete (cons eudc-server eudc-protocol) servers)))
	   ((eq eudc-inline-expansion-servers 'current-server)
	    (list (cons eudc-server eudc-protocol)))
	   (t
	    (error "Wrong value for `eudc-inline-expansion-servers': %S"
		   eudc-inline-expansion-servers))))
    (if (and eudc-max-servers-to-query
	     (> (length servers) eudc-max-servers-to-query))
	(setcdr (nthcdr (1- eudc-max-servers-to-query) servers) nil))

    (condition-case signal
	(progn
	  (setq response
		(catch 'found
		  ;; Loop on the servers
		  (while servers
		    (eudc-set-server (eudc-caar servers) (eudc-cdar servers) t)

		    ;; Determine which formats apply in the query-format list
		    (setq query-formats
			  (or
			   (eudc-extract-n-word-formats eudc-inline-query-format
							(length query-words))
			   (if (null eudc-protocol-has-default-query-attributes)
			       '(name))))

		    ;; Loop on query-formats
		    (while query-formats
		      (setq response
			    (eudc-query
			     (eudc-format-query query-words (car query-formats))
			     (eudc-translate-attribute-list
			      (cdr eudc-inline-expansion-format))))
		      (if response
			  (throw 'found response))
		      (setq query-formats (cdr query-formats)))
		    (setq servers (cdr servers)))
		  ;; No more servers to try... no match found
		  nil))


	  (if (null response)
	      (error "No match")

	    ;; Process response through eudc-inline-expansion-format
	    (while response
	      (setq response-string (apply 'format
					   (car eudc-inline-expansion-format)
					   (mapcar (function
						    (lambda (field)
						      (or (cdr (assq field (car response)))
							  "")))
						   (eudc-translate-attribute-list
						    (cdr eudc-inline-expansion-format)))))
	      (if (> (length response-string) 0)
		  (setq response-strings
			(cons response-string response-strings)))
	      (setq response (cdr response)))

	    (if (or
		 (and replace (not eudc-expansion-overwrites-query))
		 (and (not replace) eudc-expansion-overwrites-query))
		(kill-ring-save beg end))
	    (cond
	     ((or (= (length response-strings) 1)
		  (null eudc-multiple-match-handling-method)
		  (eq eudc-multiple-match-handling-method 'first))
	      (delete-region beg end)
	      (insert (car response-strings)))
	     ((eq eudc-multiple-match-handling-method 'select)
	      (eudc-select response-strings beg end))
	     ((eq eudc-multiple-match-handling-method 'all)
	      (delete-region beg end)
	      (insert (mapconcat 'identity response-strings ", ")))
	     ((eq eudc-multiple-match-handling-method 'abort)
	      (error "There is more than one match for the query"))))
	  (or (and (equal eudc-server eudc-former-server)
		   (equal eudc-protocol eudc-former-protocol))
	      (eudc-set-server eudc-former-server eudc-former-protocol t)))
      (error
       (or (and (equal eudc-server eudc-former-server)
		(equal eudc-protocol eudc-former-protocol))
	   (eudc-set-server eudc-former-server eudc-former-protocol t))
       (signal (car signal) (cdr signal))))))

;;;###autoload
(defun eudc-query-form (&optional get-fields-from-server)
  "Display a form to query the directory server.
If given a non-nil argument GET-FIELDS-FROM-SERVER, the function first
queries the server for the existing fields and displays a corresponding form."
  (interactive "P")
  (let ((fields (or (and get-fields-from-server
			 (eudc-get-attribute-list))
		    eudc-query-form-attributes))
	(buffer (get-buffer-create "*Directory Query Form*"))
	prompts
	widget
	(width 0)
	inhibit-read-only
	pt)
    (switch-to-buffer buffer)
    (setq inhibit-read-only t)
    (erase-buffer)
    (kill-all-local-variables)
    (make-local-variable 'eudc-form-widget-list)
    (widget-insert "Directory Query Form\n")
    (widget-insert "====================\n\n")
    (widget-insert "Current server is: " (or eudc-server
					     (progn
					       (call-interactively 'eudc-set-server)
					       eudc-server))
					     "\n")
    (widget-insert "Protocol         : " (symbol-name eudc-protocol) "\n")
    ;; Build the list of prompts
    (setq prompts (if eudc-use-raw-directory-names
		      (mapcar 'symbol-name (eudc-translate-attribute-list fields))
		    (mapcar (function
			     (lambda (field)
			       (or (and (assq field eudc-user-attribute-names-alist)
					(cdr (assq field eudc-user-attribute-names-alist)))
				   (capitalize (symbol-name field)))))
			    fields)))
    ;; Loop over prompt strings to find the longest one
    (mapc (function
	   (lambda (prompt)
	     (if (> (length prompt) width)
		 (setq width (length prompt)))))
	  prompts)
    ;; Insert the first widget out of the mapcar to leave the cursor
    ;; in the first field
    (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
    (setq pt (point))
    (setq widget (widget-create 'editable-field :size 15))
    (setq eudc-form-widget-list (cons (cons (car fields) widget)
				      eudc-form-widget-list))
    (setq fields (cdr fields))
    (setq prompts (cdr prompts))
    (mapc (function
	   (lambda (field)
	     (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
	     (setq widget (widget-create 'editable-field
					 :size 15))
	     (setq eudc-form-widget-list (cons (cons field widget)
					       eudc-form-widget-list))
	     (setq prompts (cdr prompts))))
	  fields)
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-process-form))
		   "Query Server")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-query-form))
		   "Reset Form")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-this-buffer))
		   "Quit")
    (goto-char pt)
    (use-local-map widget-keymap)
    (widget-setup))
  )

(defun eudc-bookmark-server (server protocol)
  "Add SERVER using PROTOCOL to the EUDC `servers' hotlist."
  (interactive "sDirectory server: \nsProtocol: ")
  (if (member (cons server protocol) eudc-server-hotlist)
      (error "%s:%s is already in the hotlist" protocol server)
    (setq eudc-server-hotlist (cons (cons server protocol) eudc-server-hotlist))
    (eudc-install-menu)
    (eudc-save-options)))

(defun eudc-bookmark-current-server ()
  "Add current server to the EUDC `servers' hotlist."
  (interactive)
  (eudc-bookmark-server eudc-server eudc-protocol))

(defun eudc-save-options ()
  "Save options to `eudc-options-file'."
  (interactive)
  (with-current-buffer (find-file-noselect eudc-options-file t)
    (goto-char (point-min))
    ;; delete the previous setq
    (let ((standard-output (current-buffer))
	  provide-p
	  set-hotlist-p
	  set-server-p)
      (catch 'found
	(while t
	  (let ((sexp (condition-case nil
			  (read (current-buffer))
			(end-of-file (throw 'found nil)))))
	    (if (listp sexp)
		(cond
		 ((eq (car sexp)  'eudc-set-server)
		  (delete-region (save-excursion
				   (backward-sexp)
				   (point))
				 (point))
		  (setq set-server-p t))
		 ((and (eq (car sexp)  'setq)
		       (eq (eudc-cadr sexp) 'eudc-server-hotlist))
		  (delete-region (save-excursion
				   (backward-sexp)
				   (point))
				 (point))
		  (setq set-hotlist-p t))
		 ((and (eq (car sexp)  'provide)
		       (equal (eudc-cadr sexp) '(quote eudc-options-file)))
		  (setq provide-p t)))
	      (if (and provide-p
		       set-hotlist-p
		       set-server-p)
		  (throw 'found t))))))
      (if (eq (point-min) (point-max))
	  (princ ";; This file was automatically generated by eudc.el.\n\n"))
      (or provide-p
	  (princ "(provide 'eudc-options-file)\n"))
      (or (bolp)
	  (princ "\n"))
      (delete-blank-lines)
      (princ "(eudc-set-server ")
      (prin1 eudc-server)
      (princ " '")
      (prin1 eudc-protocol)
      (princ " t)\n")
      (princ "(setq eudc-server-hotlist '")
      (prin1 eudc-server-hotlist)
      (princ ")\n")
      (save-buffer))))

(defun eudc-move-to-next-record ()
  "Move to next record, in a buffer displaying directory query results."
  (interactive)
  (if (not (eq major-mode 'eudc-mode))
      (error "Not in a EUDC buffer")
    (let ((pt (next-overlay-change (point))))
      (if (< pt (point-max))
	  (goto-char (1+ pt))
	(error "No more records after point")))))

(defun eudc-move-to-previous-record ()
  "Move to previous record, in a buffer displaying directory query results."
  (interactive)
  (if (not (eq major-mode 'eudc-mode))
      (error "Not in a EUDC buffer")
    (let ((pt (previous-overlay-change (point))))
      (if (> pt (point-min))
	  (goto-char pt)
	(error "No more records before point")))))

;;}}}

;;{{{      Menus and keymaps

(require 'easymenu)

(defconst eudc-custom-generated-menu (cdr (custom-menu-create 'eudc)))

(defconst eudc-tail-menu
  `(["---" nil nil]
    ["Query with Form" eudc-query-form
     :help "Display a form to query the directory server"]
    ["Expand Inline Query" eudc-expand-inline
     :help "Query the directory server, and expand the query string before point"]
    ["Insert Record into BBDB" eudc-insert-record-at-point-into-bbdb
     (and (or (featurep 'bbdb)
	      (prog1 (locate-library "bbdb") (message "")))
	  (overlays-at (point))
	  (overlay-get (car (overlays-at (point))) 'eudc-record))
     :help "Insert record at point into the BBDB database"]
    ["Insert All Records into BBDB" eudc-batch-export-records-to-bbdb
     (and (eq major-mode 'eudc-mode)
	  (or (featurep 'bbdb)
	      (prog1 (locate-library "bbdb") (message ""))))
     :help "Insert all the records returned by a directory query into BBDB"]
    ["---" nil nil]
    ["Get Email" eudc-get-email
     :help "Get the email field of NAME from the directory server"]
    ["Get Phone" eudc-get-phone
     :help "Get the phone field of name from the directory server"]
    ["List Valid Attribute Names" eudc-get-attribute-list
     :help "Return a list of valid attributes for the current server"]
    ["---" nil nil]
    ,(cons "Customize" eudc-custom-generated-menu)))


(defconst eudc-server-menu
  '(["---" nil nil]
    ["Bookmark Current Server" eudc-bookmark-current-server
     :help "Add current server to the EUDC `servers' hotlist"]
    ["Edit Server List" eudc-edit-hotlist
     :help "Edit the hotlist of directory servers in a specialized buffer"]
    ["New Server" eudc-set-server
     :help "Set the directory server to SERVER using PROTOCOL"]))

(defun eudc-menu ()
  (let (command)
    (append '("Directory Search")
	    (list
	     (append
	      '("Server")
	      (mapcar
	       (function
		(lambda (servspec)
		  (let* ((server (car servspec))
			 (protocol (cdr servspec))
			 (proto-name (symbol-name protocol)))
		    (setq command (intern (concat "eudc-set-server-"
						  server
						  "-"
						  proto-name)))
		    (if (not (fboundp command))
			(fset command
			      `(lambda ()
				 (interactive)
				 (eudc-set-server ,server (quote ,protocol))
				 (message "Selected directory server is now %s (%s)"
					  ,server
					  ,proto-name))))
		    (vector (format "%s (%s)" server proto-name)
			    command
			    :style 'radio
			    :selected `(equal eudc-server ,server)))))
	       eudc-server-hotlist)
	      eudc-server-menu))
	    eudc-tail-menu)))

(defun eudc-install-menu ()
  (cond
   ((and (featurep 'xemacs) (featurep 'menubar))
    (add-submenu '("Tools") (eudc-menu)))
   ((not (featurep 'xemacs))
    (cond
     ((fboundp 'easy-menu-create-menu)
      (define-key
	global-map
	[menu-bar tools directory-search]
	(cons "Directory Search"
	      (easy-menu-create-menu "Directory Search" (cdr (eudc-menu))))))
     ((fboundp 'easy-menu-add-item)
      (let ((menu (eudc-menu)))
	(easy-menu-add-item nil '("tools") (easy-menu-create-menu (car menu)
								  (cdr menu)))))
     ((fboundp 'easy-menu-create-keymaps)
      (easy-menu-define eudc-menu-map eudc-mode-map "Directory Client Menu" (eudc-menu))
      (define-key
	global-map
	[menu-bar tools eudc]
	(cons "Directory Search"
	      (easy-menu-create-keymaps "Directory Search" (cdr (eudc-menu))))))
     (t
      (error "Unknown version of easymenu"))))
   ))


;;; Load time initializations :

;;; Load the options file
(if (and (not noninteractive)
	 (and (locate-library eudc-options-file)
	      (progn (message "") t))   ; Remove modeline message
	 (not (featurep 'eudc-options-file)))
    (load eudc-options-file))

;;; Install the full menu
(unless (featurep 'infodock)
  (eudc-install-menu))


;;; The following installs a short menu for EUDC at XEmacs startup.

;;;###autoload
(defun eudc-load-eudc ()
  "Load the Emacs Unified Directory Client.
This does nothing except loading eudc by autoload side-effect."
  (interactive)
  nil)

;;;###autoload
(cond
 ((not (featurep 'xemacs))
  (defvar eudc-tools-menu
    (let ((map (make-sparse-keymap "Directory Search")))
      (define-key map [phone]
	`(menu-item ,(purecopy "Get Phone") eudc-get-phone
		    :help ,(purecopy "Get the phone field of name from the directory server")))
      (define-key map [email]
	`(menu-item ,(purecopy "Get Email") eudc-get-email
		    :help ,(purecopy "Get the email field of NAME from the directory server")))
      (define-key map [separator-eudc-email] menu-bar-separator)
      (define-key map [expand-inline]
	`(menu-item ,(purecopy "Expand Inline Query") eudc-expand-inline
		    :help ,(purecopy "Query the directory server, and expand the query string before point")))
      (define-key map [query]
	`(menu-item ,(purecopy "Query with Form") eudc-query-form
		    :help ,(purecopy "Display a form to query the directory server")))
      (define-key map [separator-eudc-query] menu-bar-separator)
      (define-key map [new]
	`(menu-item ,(purecopy "New Server") eudc-set-server
		    :help ,(purecopy "Set the directory server to SERVER using PROTOCOL")))
      (define-key map [load]
	`(menu-item ,(purecopy "Load Hotlist of Servers") eudc-load-eudc
		    :help ,(purecopy "Load the Emacs Unified Directory Client")))
      map))
  (fset 'eudc-tools-menu (symbol-value 'eudc-tools-menu)))
 (t
  (let ((menu  '("Directory Search"
		 ["Load Hotlist of Servers" eudc-load-eudc t]
		 ["New Server" eudc-set-server t]
		 ["---" nil nil]
		 ["Query with Form" eudc-query-form t]
		 ["Expand Inline Query" eudc-expand-inline t]
		 ["---" nil nil]
		 ["Get Email" eudc-get-email t]
		 ["Get Phone" eudc-get-phone t])))
    (if (not (featurep 'eudc-autoloads))
	(if (featurep 'xemacs)
	    (if (and (featurep 'menubar)
		     (not (featurep 'infodock)))
		(add-submenu '("Tools") menu))
	  (require 'easymenu)
	  (cond
	   ((fboundp 'easy-menu-add-item)
	    (easy-menu-add-item nil '("tools")
				(easy-menu-create-menu (car menu)
						       (cdr menu))))
	   ((fboundp 'easy-menu-create-keymaps)
	    (define-key
	      global-map
	      [menu-bar tools eudc]
	      (cons "Directory Search"
		    (easy-menu-create-keymaps "Directory Search"
					      (cdr menu)))))))))))

;;}}}

(provide 'eudc)

;;; eudc.el ends here
