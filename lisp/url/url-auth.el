;;; url-auth.el --- Uniform Resource Locator authorization modules

;; Copyright (C) 1996-1999, 2004-2012  Free Software Foundation, Inc.

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

;;; Code:

(require 'url-vars)
(require 'url-parse)
(autoload 'url-warn "url")
(autoload 'auth-source-search "auth-source")

(defsubst url-auth-user-prompt (url realm)
  "String to usefully prompt for a username."
  (concat "Username [for "
	  (or realm (url-truncate-url-for-viewing
		     (url-recreate-url url)
		     (- (window-width) 10 20)))
	  "]: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic authorization code
;;; ------------------------
;;; This implements the BASIC authorization type.  See the online
;;; documentation at
;;; http://www.w3.org/hypertext/WWW/AccessAuthorization/Basic.html
;;; for the complete documentation on this type.
;;;
;;; This is very insecure, but it works as a proof-of-concept
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-basic-auth-storage 'url-http-real-basic-auth-storage
  "Where usernames and passwords are stored.

Must be a symbol pointing to another variable that will actually store
the information.  The value of this variable is an assoc list of assoc
lists.  The first assoc list is keyed by the server name.  The cdr of
this is an assoc list based on the 'directory' specified by the URL we
are looking up.")

(defun url-basic-auth (url &optional prompt overwrite realm args)
  "Get the username/password for the specified URL.
If optional argument PROMPT is non-nil, ask for the username/password
to use for the url and its descendants.  If optional third argument
OVERWRITE is non-nil, overwrite the old username/password pair if it
is found in the assoc list.  If REALM is specified, use that as the realm
instead of the filename inheritance method."
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (server (url-host href))
	 (type (url-type href))
	 (port (url-port href))
	 (file (url-filename href))
	 (user (url-user href))
	 (pass (url-password href))
	 (enable-recursive-minibuffers t) ; for url-handler-mode (bug#10298)
	 byserv retval data)
    (setq server (format "%s:%d" server port)
	  file (cond
		(realm realm)
		((string= "" file) "/")
		((string-match "/$" file) file)
		(t (url-file-directory file)))
	  byserv (cdr-safe (assoc server
				  (symbol-value url-basic-auth-storage))))
    (cond
     ((and prompt (not byserv))
      (setq user (or
		  (url-do-auth-source-search server type :user)
		  (read-string (url-auth-user-prompt url realm)
			       (or user (user-real-login-name))))
	    pass (or
		  (url-do-auth-source-search server type :secret)
		  (read-passwd "Password: " nil (or pass ""))))
      (set url-basic-auth-storage
	   (cons (list server
		       (cons file
			     (setq retval
				   (base64-encode-string
				    (format "%s:%s" user
					    (encode-coding-string pass 'utf-8))))))
		 (symbol-value url-basic-auth-storage))))
     (byserv
      (setq retval (cdr-safe (assoc file byserv)))
      (if (and (not retval)
	       (string-match "/" file))
 	  (while (and byserv (not retval))
	    (setq data (car (car byserv)))
	    (if (or (not (string-match "/" data)) ; It's a realm - take it!
		    (and
		     (>= (length file) (length data))
		     (string= data (substring file 0 (length data)))))
		(setq retval (cdr (car byserv))))
	    (setq byserv (cdr byserv))))
      (if (or (and (not retval) prompt) overwrite)
	  (progn
	    (setq user (or
			(url-do-auth-source-search server type :user)
			(read-string (url-auth-user-prompt url realm)
				     (user-real-login-name)))
		  pass (or
			(url-do-auth-source-search server type :secret)
			(read-passwd "Password: "))
		  retval (base64-encode-string (format "%s:%s" user pass))
		  byserv (assoc server (symbol-value url-basic-auth-storage)))
	    (setcdr byserv
		    (cons (cons file retval) (cdr byserv))))))
     (t (setq retval nil)))
    (if retval (setq retval (concat "Basic " retval)))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Digest authorization code
;;; ------------------------
;;; This implements the DIGEST authorization type.  See the internet draft
;;; ftp://ds.internic.net/internet-drafts/draft-ietf-http-digest-aa-01.txt
;;; for the complete documentation on this type.
;;;
;;; This is very secure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-digest-auth-storage nil
  "Where usernames and passwords are stored.
Its value is an assoc list of assoc lists.  The first assoc list is
keyed by the server name.  The cdr of this is an assoc list based
on the 'directory' specified by the url we are looking up.")

(defun url-digest-auth-create-key (username password realm method uri)
  "Create a key for digest authentication method"
  (let* ((info (if (stringp uri)
		   (url-generic-parse-url uri)
		 uri))
	 (a1 (md5 (concat username ":" realm ":" password)))
	 (a2 (md5 (concat method ":" (url-filename info)))))
    (list a1 a2)))

(defun url-digest-auth (url &optional prompt overwrite realm args)
  "Get the username/password for the specified URL.
If optional argument PROMPT is non-nil, ask for the username/password
to use for the URL and its descendants.  If optional third argument
OVERWRITE is non-nil, overwrite the old username/password pair if it
is found in the assoc list.  If REALM is specified, use that as the realm
instead of hostname:portnum."
  (if args
      (let* ((href (if (stringp url)
		       (url-generic-parse-url url)
		     url))
	     (server (url-host href))
	     (type (url-type href))
	     (port (url-port href))
	     (file (url-filename href))
	     (enable-recursive-minibuffers t)
	     user pass byserv retval data)
	(setq file (cond
		    (realm realm)
		    ((string-match "/$" file) file)
		    (t (url-file-directory file)))
	      server (format "%s:%d" server port)
	      byserv (cdr-safe (assoc server url-digest-auth-storage)))
	(cond
	 ((and prompt (not byserv))
	  (setq user (or
		      (url-do-auth-source-search server type :user)
		      (read-string (url-auth-user-prompt url realm)
				   (user-real-login-name)))
		pass (or
		      (url-do-auth-source-search server type :secret)
		      (read-passwd "Password: "))
		url-digest-auth-storage
		(cons (list server
			    (cons file
				  (setq retval
					(cons user
					      (url-digest-auth-create-key
					       user pass realm
					       (or url-request-method "GET")
					       url)))))
		      url-digest-auth-storage)))
	 (byserv
	  (setq retval (cdr-safe (assoc file byserv)))
	  (if (and (not retval)		; no exact match, check directories
		   (string-match "/" file)) ; not looking for a realm
	      (while (and byserv (not retval))
		(setq data (car (car byserv)))
		(if (or (not (string-match "/" data))
			(and
			 (>= (length file) (length data))
			 (string= data (substring file 0 (length data)))))
		    (setq retval (cdr (car byserv))))
		(setq byserv (cdr byserv))))
	  (if overwrite
	      (if (and (not retval) prompt)
		  (setq user (or
			      (url-do-auth-source-search server type :user)
			      (read-string (url-auth-user-prompt url realm)
					   (user-real-login-name)))
			pass (or
			      (url-do-auth-source-search server type :secret)
			      (read-passwd "Password: "))
			retval (setq retval
				     (cons user
					   (url-digest-auth-create-key
					    user pass realm
					    (or url-request-method "GET")
					    url)))
			byserv (assoc server url-digest-auth-storage))
		(setcdr byserv
			(cons (cons file retval) (cdr byserv))))))
	 (t (setq retval nil)))
	(if retval
	    (if (cdr-safe (assoc "opaque" args))
		(let ((nonce (or (cdr-safe (assoc "nonce" args)) "nonegiven"))
		      (opaque (cdr-safe (assoc "opaque" args))))
		  (format
		   (concat "Digest username=\"%s\", realm=\"%s\","
			   "nonce=\"%s\", uri=\"%s\","
			   "response=\"%s\", opaque=\"%s\"")
		   (nth 0 retval) realm nonce (url-filename href)
		   (md5 (concat (nth 1 retval) ":" nonce ":"
				(nth 2 retval))) opaque))
	      (let ((nonce (or (cdr-safe (assoc "nonce" args)) "nonegiven")))
		(format
		 (concat "Digest username=\"%s\", realm=\"%s\","
			 "nonce=\"%s\", uri=\"%s\","
			 "response=\"%s\"")
		 (nth 0 retval) realm nonce (url-filename href)
		 (md5 (concat (nth 1 retval) ":" nonce ":"
			      (nth 2 retval))))))))))

(defvar url-registered-auth-schemes nil
  "A list of the registered authorization schemes and various and sundry
information associated with them.")

(defun url-do-auth-source-search (server type parameter)
  (let* ((auth-info (auth-source-search :max 1 :host server :port type))
         (auth-info (nth 0 auth-info))
         (token (plist-get auth-info parameter))
         (token (if (functionp token) (funcall token) token)))
    token))

;;;###autoload
(defun url-get-authentication (url realm type prompt &optional args)
  "Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol 'any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol 'any'
       to specify that any authentication is acceptable.  If requesting 'any'
       the strongest matching authentication will be returned.  If this is
       wrong, it's no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache"
  (if (not realm)
      (setq realm (cdr-safe (assoc "realm" args))))
  (if (stringp url)
      (setq url (url-generic-parse-url url)))
  (if (or (null type) (eq type 'any))
      ;; Whooo doogies!
      ;; Go through and get _all_ the authorization strings that could apply
      ;; to this URL, store them along with the 'rating' we have in the list
      ;; of schemes, then sort them so that the 'best' is at the front of the
      ;; list, then get the car, then get the cdr.
      ;; Zooom zooom zoooooom
      (cdr-safe
       (car-safe
	(sort
	 (mapcar
	  (function
	   (lambda (scheme)
	     (if (fboundp (car (cdr scheme)))
		 (cons (cdr (cdr scheme))
		       (funcall (car (cdr scheme)) url nil nil realm))
	       (cons 0 nil))))
	  url-registered-auth-schemes)
	 (function
	  (lambda (x y)
	    (cond
	     ((null (cdr x)) nil)
	     ((and (cdr x) (null (cdr y))) t)
	     ((and (cdr x) (cdr y))
	      (>= (car x) (car y)))
	     (t nil)))))))
    (if (symbolp type) (setq type (symbol-name type)))
    (let* ((scheme (car-safe
		    (cdr-safe (assoc (downcase type)
				     url-registered-auth-schemes)))))
      (if (and scheme (fboundp scheme))
	  (funcall scheme url prompt
		   (and prompt
			(funcall scheme url nil nil realm args))
		   realm args)))))

;;;###autoload
(defun url-register-auth-scheme (type &optional function rating)
  "Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.
         This should be the same thing you expect to get returned in
         an Authenticate header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.
         This defaults to `url-?-auth', where ? is TYPE.
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned."
  (let* ((type (cond
		((stringp type) (downcase type))
		((symbolp type) (downcase (symbol-name type)))
		(t (error "Bad call to `url-register-auth-scheme'"))))
	 (function (or function (intern (concat "url-" type "-auth"))))
	 (rating (cond
		  ((null rating) 2)
		  ((stringp rating) (string-to-number rating))
		  (t rating)))
	 (node (assoc type url-registered-auth-schemes)))
    (if (not (fboundp function))
	(url-warn 'security
		  (format (concat
			   "Tried to register `%s' as an auth scheme"
			   ", but it is not a function!") function)))

    (if node
	(setcdr node (cons function rating))
      (setq url-registered-auth-schemes
	    (cons (cons type (cons function rating))
		  url-registered-auth-schemes)))))

(defun url-auth-registered (scheme)
  "Return non-nil if SCHEME is registered as an auth type."
  (assoc scheme url-registered-auth-schemes))

(provide 'url-auth)

;;; url-auth.el ends here
