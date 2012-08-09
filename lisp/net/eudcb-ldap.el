;;; eudcb-ldap.el --- Emacs Unified Directory Client - LDAP Backend

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
;;    This library provides specific LDAP protocol support for the
;;    Emacs Unified Directory Client package

;;; Installation:
;;    Install EUDC first. See EUDC documentation.

;;; Code:

(require 'eudc)
(require 'ldap)


;;{{{      Internal cooking

(eval-and-compile
  (if (fboundp 'ldap-get-host-parameter)
      (fset 'eudc-ldap-get-host-parameter 'ldap-get-host-parameter)
    (defun eudc-ldap-get-host-parameter (host parameter)
      "Get the value of PARAMETER for HOST in `ldap-host-parameters-alist'."
      (plist-get (cdr (assoc host ldap-host-parameters-alist))
		 parameter))))

(defvar eudc-ldap-attributes-translation-alist
  '((name . sn)
    (firstname . givenname)
    (email . mail)
    (phone . telephonenumber))
  "Alist mapping EUDC attribute names to LDAP names.")

(eudc-protocol-set 'eudc-query-function 'eudc-ldap-simple-query-internal
		   'ldap)
(eudc-protocol-set 'eudc-list-attributes-function 'eudc-ldap-get-field-list
		   'ldap)
(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
		   'eudc-ldap-attributes-translation-alist 'ldap)
(eudc-protocol-set 'eudc-bbdb-conversion-alist
		   'eudc-ldap-bbdb-conversion-alist
		   'ldap)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes nil 'ldap)
(eudc-protocol-set 'eudc-attribute-display-method-alist
		   '(("jpegphoto" . eudc-display-jpeg-inline)
		     ("labeledurl" . eudc-display-url)
		     ("audio" . eudc-display-sound)
		     ("labeleduri" . eudc-display-url)
		     ("mail" . eudc-display-mail)
		     ("url" . eudc-display-url))
		   'ldap)
(eudc-protocol-set 'eudc-switch-to-server-hook
		   '(eudc-ldap-check-base)
		   'ldap)

(defun eudc-ldap-cleanup-record-simple (record)
  "Do some cleanup in a RECORD to make it suitable for EUDC."
  (mapcar
   (function
    (lambda (field)
      (cons (intern (car field))
	    (if (cdr (cdr field))
		(cdr field)
	      (car (cdr field))))))
   record))

(defun eudc-filter-$ (string)
  (mapconcat 'identity (split-string string "\\$") "\n"))

;; Cleanup a LDAP record to make it suitable for EUDC:
;;   Make the record a cons-cell instead of a list if it is single-valued
;;   Filter the $ character in addresses into \n if not done by the LDAP lib
(defun eudc-ldap-cleanup-record-filtering-addresses (record)
  (mapcar
   (function
    (lambda (field)
      (let ((name (intern (car field)))
	    (value (cdr field)))
	(if (memq name '(postaladdress registeredaddress))
	    (setq value (mapcar 'eudc-filter-$ value)))
	(cons name
	      (if (cdr value)
		  value
		(car value))))))
   record))

(defun eudc-ldap-simple-query-internal (query &optional return-attrs)
  "Query the LDAP server with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTRs should be valid
LDAP attribute names.
RETURN-ATTRS is a list of attributes to return, defaulting to
`eudc-default-return-attributes'."
  (let ((result (ldap-search (eudc-ldap-format-query-as-rfc1558 query)
			     eudc-server
			     (if (listp return-attrs)
				 (mapcar 'symbol-name return-attrs))))
	final-result)
    (if (or (not (boundp 'ldap-ignore-attribute-codings))
	    ldap-ignore-attribute-codings)
	(setq result
	      (mapcar 'eudc-ldap-cleanup-record-filtering-addresses result))
      (setq result (mapcar 'eudc-ldap-cleanup-record-simple result)))

    (if (and eudc-strict-return-matches
	     return-attrs
	     (not (eq 'all return-attrs)))
	(setq result (eudc-filter-partial-records result return-attrs)))
    ;; Apply eudc-duplicate-attribute-handling-method
    (if (not (eq 'list eudc-duplicate-attribute-handling-method))
	(mapc
	 (function (lambda (record)
		     (setq final-result
			   (append (eudc-filter-duplicate-attributes record)
				   final-result))))
	 result))
    final-result))

(defun eudc-ldap-get-field-list (dummy &optional objectclass)
  "Return a list of valid attribute names for the current server.
OBJECTCLASS is the LDAP object class for which the valid
attribute names are returned. Default to `person'"
  (interactive)
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((ldap-host-parameters-alist
	 (list (cons eudc-server
		     '(scope subtree sizelimit 1)))))
    (mapcar 'eudc-ldap-cleanup-record-simple
	    (ldap-search
	     (eudc-ldap-format-query-as-rfc1558
	      (list (cons "objectclass"
			  (or objectclass
			      "person"))))
	     eudc-server nil t))))

(defun eudc-ldap-escape-query-special-chars (string)
  "Value is STRING with characters forbidden in LDAP queries escaped."
;; Note that * should also be escaped but in most situations I suppose
;; the user doesn't want this
  (eudc-replace-in-string
   (eudc-replace-in-string
    (eudc-replace-in-string
      (eudc-replace-in-string
       string
       "\\\\" "\\5c")
      "(" "\\28")
     ")" "\\29")
   (char-to-string ?\0) "\\00"))

(defun eudc-ldap-format-query-as-rfc1558 (query)
  "Format the EUDC QUERY list as a RFC1558 LDAP search filter."
  (format "(&%s)"
	  (apply 'concat
		 (mapcar (lambda (item)
                           (format "(%s=%s)"
                                   (car item)
                                   (eudc-ldap-escape-query-special-chars (cdr item))))
			 query))))


;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-ldap-customize ()
  "Customize the EUDC LDAP support."
  (interactive)
  (customize-group 'eudc-ldap))

(defun eudc-ldap-check-base ()
  "Check if the current LDAP server has a configured search base."
  (unless (or (eudc-ldap-get-host-parameter eudc-server 'base)
	      ldap-default-base
	      (null (y-or-n-p "No search base defined. Configure it now? ")))
    ;; If the server is not in ldap-host-parameters-alist we add it for the
    ;; user
    (if (null (assoc eudc-server ldap-host-parameters-alist))
	(setq ldap-host-parameters-alist
	      (cons (list eudc-server) ldap-host-parameters-alist)))
    (customize-variable 'ldap-host-parameters-alist)))

;;}}}


(eudc-register-protocol 'ldap)

(provide 'eudcb-ldap)

;;; eudcb-ldap.el ends here
