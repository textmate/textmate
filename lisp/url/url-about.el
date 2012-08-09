;;; url-about.el --- Show internal URLs

;; Copyright (C) 2001, 2004-2012  Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

(require 'url-util)
(require 'url-parse)

(defun url-probe-protocols ()
  "Return a list of all potential URL schemes."
  (or (get 'url-extension-protocols 'probed)
      (mapc (lambda (s) (url-scheme-get-property s 'name))
	    (or (get 'url-extension-protocols 'schemes)
		(let ((schemes '("info" "man" "rlogin" "telnet"
				 "tn3270" "data" "snews")))
		  (mapc (lambda (d)
			  (mapc (lambda (f)
				  (if (string-match "url-\\(.*\\).el$" f)
				      (push (match-string 1 f) schemes)))
				(directory-files d nil "^url-.*\\.el$")))
			load-path)
		  (put 'url-extension-protocols 'schemes schemes)
		  schemes)))))

(defvar url-scheme-registry)

(defun url-about-protocols (url)
  (url-probe-protocols)
  (insert "<html>\n"
	  " <head>\n"
	  "  <title>Supported Protocols</title>\n"
	  " </head>\n"
	  " <body>\n"
	  "  <h1>Supported Protocols - URL v" url-version "</h1>\n"
	  "  <table width='100%' border='1'>\n"
	  "   <tr>\n"
	  "    <td>Protocol\n"
	  "    <td>Properties\n"
	  "    <td>Description\n"
	  "   </tr>\n")
  (mapc (lambda (k)
	  (if (string= k "proxy")
	      ;; Ignore the proxy setting... its magic!
	      nil
	    (insert "   <tr>\n")
	    ;; The name of the protocol
	    (insert "    <td valign=top>" (or (url-scheme-get-property k 'name) k) "\n")

	    ;; Now the properties.  Currently just asynchronous
	    ;; status, default port number, and proxy status.
	    (insert "    <td valign=top>"
		    (if (url-scheme-get-property k 'asynchronous-p) "As" "S")
		    "ynchronous<br>\n"
		    (if (url-scheme-get-property k 'default-port)
			(format "Default Port: %d<br>\n"
				(url-scheme-get-property k 'default-port)) "")
		    (if (assoc k url-proxy-services)
			(format "Proxy: %s<br>\n" (assoc k url-proxy-services)) ""))
	    ;; Now the description...
	    (insert "    <td valign=top>"
		    (or (url-scheme-get-property k 'description) "N/A"))))
	(sort (let (x) (maphash (lambda (k v) (push k x)) url-scheme-registry) x) 'string-lessp))
  (insert "  </table>\n"
	  " </body>\n"
	  "</html>\n"))

(defun url-about (url)
  "Show internal URLs."
  (let* ((item (downcase (url-filename url)))
	 (func (intern (format "url-about-%s" item))))
    (if (fboundp func)
	(progn
	  (set-buffer (generate-new-buffer " *about-data*"))
	  (insert "Content-type: text/plain\n\n")
	  (funcall func url)
	  (current-buffer))
      (error "URL does not know about `%s'" item))))

(provide 'url-about)

;;; url-about.el ends here
