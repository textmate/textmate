;;; url-methods.el --- Load URL schemes as needed

;; Copyright (C) 1996-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

(eval-when-compile
  (require 'cl))

;; This loads up some of the small, silly URLs that I really don't
;; want to bother putting in their own separate files.
(require 'url-parse)

(defvar url-scheme-registry (make-hash-table :size 7 :test 'equal))

(defconst url-scheme-methods
  '((default-port      . variable)
    (asynchronous-p    . variable)
    (expand-file-name  . function)
    (file-exists-p     . function)
    (file-attributes   . function)
    (parse-url         . function)
    (file-symlink-p    . function)
    (file-writable-p   . function)
    (file-directory-p  . function)
    (file-executable-p . function)
    (directory-files   . function)
    (file-truename     . function))
  "Assoc-list of methods that each URL loader can provide.")

(defconst url-scheme-default-properties
  (list 'name "unknown"
	'loader 'url-scheme-default-loader
	'default-port 0
	'expand-file-name 'url-identity-expander
	'parse-url 'url-generic-parse-url
	'asynchronous-p nil
	'file-directory-p 'ignore
	'file-truename (lambda (&rest args)
			 (url-recreate-url (car args)))
	'file-exists-p 'ignore
	'file-attributes 'ignore))

(defun url-scheme-default-loader (url &optional callback cbargs)
  "Signal an error for an unknown URL scheme."
  (error "Unknown URL scheme: %s" (url-type url)))

(defvar url-scheme--registering-proxy nil)

(defun url-scheme-register-proxy (scheme)
  "Automatically find a proxy for SCHEME and put it in `url-proxy-services'."
  (let* ((env-var (concat scheme "_proxy"))
	 (env-proxy (or (getenv (upcase env-var))
			(getenv (downcase env-var))))
	 (cur-proxy (assoc scheme url-proxy-services))
	 (urlobj nil)
	 (url-scheme--registering-proxy t))

    ;; If env-proxy is an empty string, treat it as if it were nil
    (when (and (stringp env-proxy)
	       (string= env-proxy ""))
      (setq env-proxy nil))

    ;; Store any proxying information - this will not overwrite an old
    ;; entry, so that people can still set this information in their
    ;; .emacs file
    (cond
     (cur-proxy nil)			; Keep their old settings
     ((null env-proxy) nil)		; No proxy setup
     ;; First check if its something like hostname:port
     ((string-match "^\\([^:]+\\):\\([0-9]+\\)$" env-proxy)
      (setq urlobj (url-generic-parse-url nil)) ; Get a blank object
      (setf (url-type urlobj) "http")
      (setf (url-host urlobj) (match-string 1 env-proxy))
      (setf (url-port urlobj) (string-to-number (match-string 2 env-proxy))))
     ;; Then check if its a fully specified URL
     ((string-match url-nonrelative-link env-proxy)
      (setq urlobj (url-generic-parse-url env-proxy))
      (setf (url-type urlobj) "http")
      (setf (url-target urlobj) nil))
     ;; Finally, fall back on the assumption that its just a hostname
     (t
      (setq urlobj (url-generic-parse-url nil)) ; Get a blank object
      (setf (url-type urlobj) "http")
      (setf (url-host urlobj) env-proxy)))

     (if (and (not cur-proxy) urlobj)
	 (progn
	   (setq url-proxy-services
		 (cons (cons scheme (format "%s:%d" (url-host urlobj)
					    (url-port urlobj)))
		       url-proxy-services))
	   (message "Using a proxy for %s..." scheme)))))

(defun url-scheme-get-property (scheme property)
  "Get PROPERTY of a URL SCHEME.
Will automatically try to load a backend from url-SCHEME.el if
it has not already been loaded."
  (setq scheme (downcase scheme))
  (let ((desc (gethash scheme url-scheme-registry)))
    (if (not desc)
	(let* ((stub (concat "url-" scheme))
	       (loader (intern stub)))
	  (condition-case ()
	      (require loader)
	    (error nil))
	  (if (fboundp loader)
	      (progn
		;; Found the module to handle <scheme> URLs
		(unless url-scheme--registering-proxy
		  (url-scheme-register-proxy scheme))
		(setq desc (list 'name scheme
				 'loader loader))
		(dolist (cell url-scheme-methods)
		  (let ((symbol (intern-soft (format "%s-%s" stub (car cell))))
			(type (cdr cell)))
		    (if symbol
			(case type
			  (function
			   ;; Store the symbol name of a function
			   (if (fboundp symbol)
			       (setq desc (plist-put desc (car cell) symbol))))
			  (variable
			   ;; Store the VALUE of a variable
			   (if (boundp symbol)
			       (setq desc (plist-put desc (car cell)
						     (symbol-value symbol)))))
			  (otherwise
			   (error "Malformed url-scheme-methods entry: %S"
				  cell))))))
		(puthash scheme desc url-scheme-registry)))))
    (or (plist-get desc property)
	(plist-get url-scheme-default-properties property))))

(provide 'url-methods)

;;; url-methods.el ends here
