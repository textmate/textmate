;;; eudcb-ph.el --- Emacs Unified Directory Client - CCSO PH/QI Backend

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: Pavel Janík <Pavel@Janik.cz>
;; Keywords: comm
;; Package: eudc

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

;;    This library provides specific CCSO PH/QI protocol support for the
;;    Emacs Unified Directory Client package.

;;; Code:

(require 'eudc)

;;{{{      Internal cooking

(eudc-protocol-set 'eudc-bbdb-conversion-alist 'eudc-ph-bbdb-conversion-alist 'ph)
(eudc-protocol-set 'eudc-query-function 'eudc-ph-query-internal 'ph)
(eudc-protocol-set 'eudc-list-attributes-function 'eudc-ph-get-field-list 'ph)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes t 'ph)

(defvar eudc-ph-process-buffer nil)
(defvar eudc-ph-read-point)

(defconst eudc-ph-default-server-port 105
  "Default TCP port for CCSO PH/QI directory services.")

(defun eudc-ph-query-internal (query &optional return-fields)
  "Query the PH/QI server with QUERY.
QUERY can be a string NAME or a list made of strings NAME
and/or cons cells (KEY . VALUE) where KEYs should be valid
CCSO database keys.  NAME is equivalent to (DEFAULT . NAME),
where DEFAULT is the default key of the database.
RETURN-FIELDS is a list of database fields to return,
defaulting to `eudc-default-return-attributes'."
  (let (request)
    (if (null return-fields)
	(setq return-fields eudc-default-return-attributes))
    (if (eq 'all return-fields)
	(setq return-fields '(all)))
    (setq request
	  (concat "query "
		  (if (stringp query)
		      query
		    (mapconcat (function (lambda (elt)
					   (if (stringp elt) elt)
					   (format "%s=%s" (car elt) (cdr elt))))
			       query
			       " "))
		  (if return-fields
		      (concat " return " (mapconcat 'symbol-name return-fields " ")))))
    (and (> (length request) 6)
	 (eudc-ph-do-request request)
	 (eudc-ph-parse-query-result return-fields))))

(defun eudc-ph-get-field-list (full-records)
  "Return a list of valid field names for the current server.
If FULL-RECORDS is non-nil, full records including field description
are returned"
  (interactive)
  (eudc-ph-do-request "fields")
  (if full-records
      (eudc-ph-parse-query-result)
    (mapcar 'eudc-caar (eudc-ph-parse-query-result))))

(defun eudc-ph-parse-query-result (&optional fields)
  "Return a list of alists of key/values from in `eudc-ph-process-buffer'.
Fields not in FIELDS are discarded."
  (let (record
	records
	line-regexp
	current-key
	key
	value
	ignore)
    (save-excursion
      (message "Parsing results...")
      (set-buffer eudc-ph-process-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(-[0-9]+\\):\\([0-9]+\\):" nil t)
	(catch 'ignore
	  (setq line-regexp (concat "^\\(-[0-9]+\\):" (match-string 2) ":[ \t]*\\([-a-zA-Z_]*\\)?:[ \t]*\\(.*\\)$"))
	  (beginning-of-line)
	  (setq record nil
		ignore nil
		current-key nil)
	  (while (re-search-forward line-regexp nil t)
	    (catch 'skip-line
	      (if (string= "-508" (match-string 1))
		  ;; A field is missing in this entry.  Skip it or skip the
		  ;; whole record (see `eudc-strict-return-matches')
		  (if (not eudc-strict-return-matches)
		      (throw 'skip-line t)
		    (while (re-search-forward line-regexp nil t))
		    (setq ignore t)
		    (throw 'ignore t)))
	      (setq key   (and (not (string= (match-string 2) ""))
			       (intern (match-string 2)))
		    value (match-string 3))
	      (if (and current-key
		       (eq key current-key))
		  (setq key nil)
		(setq current-key key))
	      (if (or (null fields)
		      (eq 'all fields)
		      (memq current-key fields))
		  (if key
		      (setq record (cons (cons key value) record)) ; New key
		    (setcdr (car record) (if (listp (eudc-cdar record))
					     (append (eudc-cdar record) (list value))
					   (list (eudc-cdar record) value))))))))
	(and (not ignore)
	     (or (null fields)
		 (eq 'all fields)
		 (setq record (nreverse record)))
	     (setq record (if (not (eq 'list eudc-duplicate-attribute-handling-method))
			      (eudc-filter-duplicate-attributes record)
			    (list record)))
	     (setq records (append record records)))))
    (message "Done")
    records))

(defun eudc-ph-do-request (request)
  "Send REQUEST to the server.
Wait for response and return the buffer containing it."
  (let (process
	buffer)
    (unwind-protect
	(progn
	  (message "Contacting server...")
	  (setq process (eudc-ph-open-session))
	  (if process
	      (with-current-buffer (setq buffer (process-buffer process))
		(eudc-ph-send-command process request)
		(message "Request sent, waiting for reply...")
		(eudc-ph-read-response process))))
      (if process
	  (eudc-ph-close-session process)))
    buffer))

(defun eudc-ph-open-session (&optional server)
  "Open a connection to the given CCSO/QI SERVER.
SERVER is either a string naming the server or a list (NAME PORT)."
  (let (process
	host
	port)
    (catch 'done
      (if (null server)
	  (setq server (or eudc-server
			   (call-interactively 'eudc-ph-set-server))))
      (string-match "\\(.*\\)\\(:\\(.*\\)\\)?" server)
      (setq host (match-string 1 server))
      (setq port (or (match-string 3 server)
		     eudc-ph-default-server-port))
      (setq eudc-ph-process-buffer (get-buffer-create (format " *PH-%s*" host)))
      (with-current-buffer eudc-ph-process-buffer
	(erase-buffer)
	(setq eudc-ph-read-point (point))
	(and (featurep 'xemacs) (featurep 'mule)
	     (set-buffer-file-coding-system 'binary t)))
      (setq process (open-network-stream "ph" eudc-ph-process-buffer host port))
      (if (null process)
	  (throw 'done nil))
      (set-process-query-on-exit-flag process t)
      process)))

(defun eudc-ph-close-session (process)
  (with-current-buffer (process-buffer process)
    (eudc-ph-send-command process "quit")
    (eudc-ph-read-response process)
    (run-at-time 2 nil 'delete-process process)))

(defun eudc-ph-send-command (process command)
  (goto-char (point-max))
  (process-send-string process command)
  (process-send-string process "\r\n")
  )

(defun eudc-ph-read-response (process &optional return-response)
  "Read a response from the PH/QI query process PROCESS.
Returns nil if response starts with an error code.  If the
response is successful the return code or the response itself is returned
depending on RETURN-RESPONSE."
  (let ((case-fold-search nil)
	return-code
	match-end)
    (goto-char eudc-ph-read-point)
    ;; CCSO protocol : response complete if status >= 200
    (while (not (re-search-forward "^\\(^[2-5].*\\):.*\n" nil t))
      (accept-process-output process)
      (goto-char eudc-ph-read-point))
    (setq match-end (point))
    (goto-char eudc-ph-read-point)
    (if (and (setq return-code (match-string 1))
	     (setq return-code (string-to-number return-code))
	     (>= (abs return-code) 300))
	(progn (setq eudc-ph-read-point match-end) nil)
      (setq eudc-ph-read-point match-end)
      (if return-response
	  (buffer-substring (point) match-end)
	return-code))))

;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-ph-customize ()
  "Customize the EUDC PH support."
  (interactive)
  (customize-group 'eudc-ph))

(defun eudc-ph-set-server (server)
  "Set the PH server to SERVER."
  (interactive "sNew PH/QI Server: ")
  (message "Selected PH/QI server is now %s" server)
  (eudc-set-server server 'ph))

;;}}}

(eudc-register-protocol 'ph)

(provide 'eudcb-ph)

;;; eudcb-ph.el ends here
