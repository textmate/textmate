;;; url-ldap.el --- LDAP Uniform Resource Locator retrieval code

;; Copyright (C) 1998-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
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

(require 'url-vars)
(require 'url-parse)
(require 'url-util)
(require 'ldap)
(autoload 'tls-certificate-information "tls")

;; This has been implemented from RFC2255 'The LDAP URL Format' (Dec 1997)
;;
;; basic format is: ldap://host:port/dn?attributes?scope?filter?extensions
;;
;; Test URLs:
;; ldap://ldap.itd.umich.edu/cn%3Dumbflabmanager%2C%20ou%3DUser%20Groups%2C%20ou%3DGroups%2C%20o%3DUniversity%20of%20Michigan%2C%20c%3DUS
;; ldap://ldap.itd.umich.edu/o=University%20of%20Michigan,c=US
;;
;; For simple queries, I have verified compatibility with Netscape
;; Communicator v4.5 under GNU/Linux.
;;
;; For anything _useful_ though, like specifying the attributes,
;; scope, filter, or extensions, netscape claims the URL format is
;; unrecognized.  So I don't think it supports anything other than the
;; defaults (scope=base,attributes=*,filter=(objectClass=*)

(defconst url-ldap-default-port 389 "Default LDAP port.")
(defalias 'url-ldap-expand-file-name 'url-default-expander)

(defvar url-ldap-pretty-names
  '(("l"           . "City")
    ("objectclass" . "Object Class")
    ("o"           . "Organization")
    ("ou"          . "Organizational Unit")
    ("cn"          . "Name")
    ("sn"          . "Last Name")
    ("givenname"   . "First Name")
    ("mail"        . "Email")
    ("title"       . "Title")
    ("c"           . "Country")
    ("postalcode"  . "ZIP Code")
    ("telephonenumber"          . "Phone Number")
    ("facsimiletelephonenumber" . "Fax")
    ("postaladdress"            . "Mailing Address")
    ("description"              . "Notes"))
  "*An assoc list mapping LDAP attribute names to pretty descriptions of them.")

(defvar url-ldap-attribute-formatters
  '(("mail"       . (lambda (x) (format "<a href='mailto:%s'>%s</a>" x x)))
    ("owner"      . url-ldap-dn-formatter)
    ("creatorsname" . url-ldap-dn-formatter)
    ("jpegphoto"     . url-ldap-image-formatter)
    ("usercertificate" . url-ldap-certificate-formatter)
    ("modifiersname" . url-ldap-dn-formatter)
    ("namingcontexts" . url-ldap-dn-formatter)
    ("defaultnamingcontext" . url-ldap-dn-formatter)
    ("member"     . url-ldap-dn-formatter))
  "*An assoc list mapping LDAP attribute names to pretty formatters for them.")

(defsubst url-ldap-attribute-pretty-name (n)
  (or (cdr-safe (assoc (downcase n) url-ldap-pretty-names)) n))

(defsubst url-ldap-attribute-pretty-desc (n v)
  (if (string-match "^\\([^;]+\\);" n)
      (setq n (match-string 1 n)))
  (funcall (or (cdr-safe (assoc (downcase n) url-ldap-attribute-formatters)) 'identity) v))

(defun url-ldap-dn-formatter (dn)
  (concat "<a href='/"
	  (url-hexify-string dn)
	  "'>" dn "</a>"))

(defun url-ldap-certificate-formatter (data)
  (condition-case ()
      (require 'ssl)
    (error nil))
  (let ((vals (if (fboundp 'ssl-certificate-information)
		  (ssl-certificate-information data)
		(tls-certificate-information data))))
    (if (not vals)
	"<b>Unable to parse certificate</b>"
      (concat "<table border=0>\n"
	      (mapconcat
	       (lambda (ava)
		 (format "<tr><td>%s</td><td>%s</td></tr>\n" (car ava) (cdr ava)))
	       vals "\n")
	      "</table>\n"))))

(defun url-ldap-image-formatter (data)
  (format "<img alt='JPEG Photo' src='data:image/jpeg;base64,%s'>"
	  (url-hexify-string (base64-encode-string data))))

;;;###autoload
(defun url-ldap (url)
  "Perform an LDAP search specified by URL.
The return value is a buffer displaying the search results in HTML.
URL can be a URL string, or a URL vector of the type returned by
`url-generic-parse-url'."
  (if (stringp url)
      (setq url (url-generic-parse-url (url-unhex-string url)))
    (if (not (vectorp url))
        (error "Argument is not a valid URL")))
  (with-current-buffer (generate-new-buffer " *url-ldap*")
    (setq url-current-object url)
    (insert "Content-type: text/html\r\n\r\n")
    (if (not (fboundp 'ldap-search-internal))
	(insert "<html>\n"
		" <head>\n"
		"  <title>LDAP Not Supported</title>\n"
		"  <base href='" (url-recreate-url url) "'>\n"
		" </head>\n"
		" <body>\n"
		"  <h1>LDAP Not Supported</h1>\n"
		"  <p>\n"
		"    This version of Emacs does not support LDAP.\n"
		"  </p>\n"
		" </body>\n"
		"</html>\n")
      (let* ((binddn nil)
	     (data (url-filename url))
	     (host (url-host url))
	     (port (url-port url))
	     (base-object nil)
	     (attributes nil)
	     (scope nil)
	     (filter nil)
	     (extensions nil)
	     (results nil))

	;; Get rid of leading /
	(if (string-match "^/" data)
	    (setq data (substring data 1)))

	(setq data (mapcar (lambda (x) (if (/= (length x) 0) x nil)) (split-string data "\\?"))
	      base-object (nth 0 data)
	      attributes (nth 1 data)
	      scope (nth 2 data)
	      filter (nth 3 data)
	      extensions (nth 4 data))

	;; fill in the defaults
	(setq base-object (url-unhex-string (or base-object ""))
	      scope (intern (url-unhex-string (or scope "base")))
	      filter (url-unhex-string (or filter "(objectClass=*)")))

	(if (not (memq scope '(base one sub)))
	    (error "Malformed LDAP URL: Unknown scope: %S" scope))

	;; Convert to the internal LDAP support scoping names.
	(setq scope (cdr (assq scope '((base . base) (one . onelevel) (sub . subtree)))))

	(if attributes
	    (setq attributes (mapcar 'url-unhex-string (split-string attributes ","))))

	;; Parse out the extensions.
	(if extensions
	    (setq extensions (mapcar (lambda (ext)
				       (if (string-match "\\([^=]*\\)=\\(.*\\)" ext)
					   (cons (match-string 1 ext) (match-string 2 ext))
					 (cons ext ext)))
				     (split-string extensions ","))
		  extensions (mapcar (lambda (ext)
				       (cons (url-unhex-string (car ext))
					     (url-unhex-string (cdr ext))))
				     extensions)))

	(setq binddn (cdr-safe (or (assoc "bindname" extensions)
				   (assoc "!bindname" extensions))))

	;; Now, let's actually do something with it.
	(setq results (cdr (ldap-search-internal
		       (list 'host (concat host ":" (number-to-string port))
			     'base base-object
			     'attributes attributes
			     'scope scope
			     'filter filter
			     'binddn binddn))))

	(insert "<html>\n"
		" <head>\n"
		"  <title>LDAP Search Results</title>\n"
		"  <base href='" (url-recreate-url url) "'>\n"
		" </head>\n"
		" <body>\n"
		"  <h1>" (int-to-string (length results)) " matches</h1>\n")

	(mapc (lambda (obj)
		(insert "  <hr>\n"
			"  <table border=1>\n")
		(mapc (lambda (attr)
			(if (= (length (cdr attr)) 1)
			    ;; single match, easy
			    (insert "   <tr><td>"
				    (url-ldap-attribute-pretty-name (car attr))
				    "</td><td>"
				    (url-ldap-attribute-pretty-desc (car attr) (car (cdr attr)))
				    "</td></tr>\n")
			  ;; Multiple matches, slightly uglier
			  (insert "   <tr>\n"
				  (format "    <td valign=top>")
				  (url-ldap-attribute-pretty-name (car attr)) "</td><td>"
				  (mapconcat (lambda (x)
					       (url-ldap-attribute-pretty-desc (car attr) x))
					     (cdr attr)
					     "<br>\n")
				  "</td>"
				  "   </tr>\n")))
                      obj)
		(insert "  </table>\n"))
	      results)

	(insert "  <hr>\n"
		" </body>\n"
		"</html>\n")))
    (current-buffer)))

(provide 'url-ldap)

;;; url-ldap.el ends here
