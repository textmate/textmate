;;; url-http.el --- HTTP retrieval routines

;; Copyright (C) 1999, 2001, 2004-2012  Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
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

(eval-when-compile (require 'cl))
(defvar url-http-extra-headers)
(defvar url-http-target-url)
(defvar url-http-no-retry)
(defvar url-http-proxy)
(defvar url-http-connection-opened)
(require 'url-gw)
(require 'url-util)
(require 'url-parse)
(require 'url-cookie)
(require 'mail-parse)
(require 'url-auth)
(require 'url)
(autoload 'url-cache-create-filename "url-cache")

(defconst url-http-default-port 80 "Default HTTP port.")
(defconst url-http-asynchronous-p t "HTTP retrievals are asynchronous.")
(defalias 'url-http-expand-file-name 'url-default-expander)

(defvar url-http-real-basic-auth-storage nil)
(defvar url-http-proxy-basic-auth-storage nil)

(defvar url-http-open-connections (make-hash-table :test 'equal
						   :size 17)
  "A hash table of all open network connections.")

(defvar url-http-version "1.1"
  "What version of HTTP we advertise, as a string.
Valid values are 1.1 and 1.0.
This is only useful when debugging the HTTP subsystem.

Setting this to 1.0 will tell servers not to send chunked encoding,
and other HTTP/1.1 specific features.")

(defvar url-http-attempt-keepalives t
  "Whether to use a single TCP connection multiple times in HTTP.
This is only useful when debugging the HTTP subsystem.  Setting to
nil will explicitly close the connection to the server after every
request.")

(defconst url-http-codes
  '((100 continue                        "Continue with request")
    (101 switching-protocols             "Switching protocols")
    (102 processing                      "Processing (Added by DAV)")
    (200 OK                              "OK")
    (201 created                         "Created")
    (202 accepted                        "Accepted")
    (203 non-authoritative               "Non-authoritative information")
    (204 no-content                      "No content")
    (205 reset-content                   "Reset content")
    (206 partial-content                 "Partial content")
    (207 multi-status                    "Multi-status (Added by DAV)")
    (300 multiple-choices                "Multiple choices")
    (301 moved-permanently               "Moved permanently")
    (302 found                           "Found")
    (303 see-other                       "See other")
    (304 not-modified                    "Not modified")
    (305 use-proxy                       "Use proxy")
    (307 temporary-redirect              "Temporary redirect")
    (400 bad-request                     "Bad Request")
    (401 unauthorized                    "Unauthorized")
    (402 payment-required                "Payment required")
    (403 forbidden                       "Forbidden")
    (404 not-found                       "Not found")
    (405 method-not-allowed              "Method not allowed")
    (406 not-acceptable                  "Not acceptable")
    (407 proxy-authentication-required   "Proxy authentication required")
    (408 request-timeout                 "Request time-out")
    (409 conflict                        "Conflict")
    (410 gone                            "Gone")
    (411 length-required                 "Length required")
    (412 precondition-failed             "Precondition failed")
    (413 request-entity-too-large        "Request entity too large")
    (414 request-uri-too-large           "Request-URI too large")
    (415 unsupported-media-type          "Unsupported media type")
    (416 requested-range-not-satisfiable "Requested range not satisfiable")
    (417 expectation-failed              "Expectation failed")
    (422 unprocessable-entity            "Unprocessable Entity (Added by DAV)")
    (423 locked                          "Locked")
    (424 failed-Dependency               "Failed Dependency")
    (500 internal-server-error           "Internal server error")
    (501 not-implemented                 "Not implemented")
    (502 bad-gateway                     "Bad gateway")
    (503 service-unavailable             "Service unavailable")
    (504 gateway-timeout                 "Gateway time-out")
    (505 http-version-not-supported      "HTTP version not supported")
    (507 insufficient-storage            "Insufficient storage")
"The HTTP return codes and their text."))

;(eval-when-compile
;; These are all macros so that they are hidden from external sight
;; when the file is byte-compiled.
;;
;; This allows us to expose just the entry points we want.

;; These routines will allow us to implement persistent HTTP
;; connections.
(defsubst url-http-debug (&rest args)
  (if quit-flag
      (let ((proc (get-buffer-process (current-buffer))))
	;; The user hit C-g, honor it!  Some things can get in an
	;; incredibly tight loop (chunked encoding)
	(if proc
	    (progn
	      (set-process-sentinel proc nil)
	      (set-process-filter proc nil)))
	(error "Transfer interrupted!")))
  (apply 'url-debug 'http args))

(defun url-http-mark-connection-as-busy (host port proc)
  (url-http-debug "Marking connection as busy: %s:%d %S" host port proc)
  (set-process-query-on-exit-flag proc t)
  (puthash (cons host port)
	      (delq proc (gethash (cons host port) url-http-open-connections))
	      url-http-open-connections)
  proc)

(defun url-http-mark-connection-as-free (host port proc)
  (url-http-debug "Marking connection as free: %s:%d %S" host port proc)
  (when (memq (process-status proc) '(open run connect))
    (set-process-buffer proc nil)
    (set-process-sentinel proc 'url-http-idle-sentinel)
    (set-process-query-on-exit-flag proc nil)
    (puthash (cons host port)
	     (cons proc (gethash (cons host port) url-http-open-connections))
	     url-http-open-connections))
  nil)

(defun url-http-find-free-connection (host port)
  (let ((conns (gethash (cons host port) url-http-open-connections))
	(connection nil))
    (while (and conns (not connection))
      (if (not (memq (process-status (car conns)) '(run open connect)))
	  (progn
	    (url-http-debug "Cleaning up dead process: %s:%d %S"
			    host port (car conns))
	    (url-http-idle-sentinel (car conns) nil))
	(setq connection (car conns))
	(url-http-debug "Found existing connection: %s:%d %S" host port connection))
      (pop conns))
    (if connection
	(url-http-debug "Reusing existing connection: %s:%d" host port)
      (url-http-debug "Contacting host: %s:%d" host port))
    (url-lazy-message "Contacting host: %s:%d" host port)

    (unless connection
      (let ((buf (generate-new-buffer " *url-http-temp*")))
	;; `url-open-stream' needs a buffer in which to do things
	;; like authentication.  But we use another buffer afterwards.
	(unwind-protect
	    (let ((proc (url-open-stream host buf host port)))
	      ;; url-open-stream might return nil.
	      (when (processp proc)
		;; Drop the temp buffer link before killing the buffer.
		(set-process-buffer proc nil)
		(setq connection proc)))
	  ;; If there was an error on connect, make sure we don't
	  ;; get queried.
	  (when (get-buffer-process buf)
	    (set-process-query-on-exit-flag (get-buffer-process buf) nil))
	  (kill-buffer buf))))

    (if connection
	(url-http-mark-connection-as-busy host port connection))))

;; Building an HTTP request
(defun url-http-user-agent-string ()
  (if (or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level)
	       (memq 'agent url-privacy-level)))
      ""
    (format "User-Agent: %sURL/%s%s\r\n"
	    (if url-package-name
		(concat url-package-name "/" url-package-version " ")
	      "")
	    url-version
	    (cond
	     ((and url-os-type url-system-type)
	      (concat " (" url-os-type "; " url-system-type ")"))
	     ((or url-os-type url-system-type)
	      (concat " (" (or url-system-type url-os-type) ")"))
	     (t "")))))

(defun url-http-create-request (&optional ref-url)
  "Create an HTTP request for `url-http-target-url', referred to by REF-URL."
  (declare (special proxy-info
		    url-http-method url-http-data
		    url-http-extra-headers))
  (let* ((extra-headers)
	 (request nil)
	 (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
	 (using-proxy url-http-proxy)
	 (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
					      url-http-extra-headers))
			     (not using-proxy))
			 nil
		       (let ((url-basic-auth-storage
			      'url-http-proxy-basic-auth-storage))
			 (url-get-authentication url-http-target-url nil 'any nil))))
	 (real-fname (concat (url-filename url-http-target-url)
			     (url-recreate-url-attributes url-http-target-url)))
	 (host (url-host url-http-target-url))
	 (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
		   nil
		 (url-get-authentication (or
					  (and (boundp 'proxy-info)
					       proxy-info)
					  url-http-target-url) nil 'any nil))))
    (if (equal "" real-fname)
	(setq real-fname "/"))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
	(setq auth (concat "Authorization: " auth "\r\n")))
    (if proxy-auth
	(setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

    ;; Protection against stupid values in the referrer
    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
					   (string= ref-url "")))
	(setq ref-url nil))

    ;; We do not want to expose the referrer if the user is paranoid.
    (if (or (memq url-privacy-level '(low high paranoid))
	    (and (listp url-privacy-level)
		 (memq 'lastloc url-privacy-level)))
	(setq ref-url nil))

    ;; url-http-extra-headers contains an assoc-list of
    ;; header/value pairs that we need to put into the request.
    (setq extra-headers (mapconcat
			 (lambda (x)
			   (concat (car x) ": " (cdr x)))
			 url-http-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
	(setq extra-headers (concat extra-headers "\r\n")))

    ;; This was done with a call to `format'.  Concatenating parts has
    ;; the advantage of keeping the parts of each header together and
    ;; allows us to elide null lines directly, at the cost of making
    ;; the layout less clear.
    (setq request
          ;; We used to concat directly, but if one of the strings happens
          ;; to being multibyte (even if it only contains pure ASCII) then
          ;; every string gets converted with `string-MAKE-multibyte' which
          ;; turns the 127-255 codes into things like latin-1 accented chars
          ;; (it would work right if it used `string-TO-multibyte' instead).
          ;; So to avoid the problem we force every string to be unibyte.
          (mapconcat
           ;; FIXME: Instead of `string-AS-unibyte' we'd want
           ;; `string-to-unibyte', so as to properly signal an error if one
           ;; of the strings contains a multibyte char.
           'string-as-unibyte
           (delq nil
            (list
             ;; The request
             (or url-http-method "GET") " "
             (if using-proxy (url-recreate-url url-http-target-url) real-fname)
             " HTTP/" url-http-version "\r\n"
             ;; Version of MIME we speak
             "MIME-Version: 1.0\r\n"
             ;; (maybe) Try to keep the connection open
             "Connection: " (if (or using-proxy
                                    (not url-http-attempt-keepalives))
                                "close" "keep-alive") "\r\n"
                                ;; HTTP extensions we support
             (if url-extensions-header
                 (format
                  "Extension: %s\r\n" url-extensions-header))
             ;; Who we want to talk to
             (if (/= (url-port url-http-target-url)
                     (url-scheme-get-property
                      (url-type url-http-target-url) 'default-port))
                 (format
                  "Host: %s:%d\r\n" host (url-port url-http-target-url))
               (format "Host: %s\r\n" host))
             ;; Who its from
             (if url-personal-mail-address
                 (concat
                  "From: " url-personal-mail-address "\r\n"))
             ;; Encodings we understand
             (if url-mime-encoding-string
                 (concat
                  "Accept-encoding: " url-mime-encoding-string "\r\n"))
             (if url-mime-charset-string
                 (concat
                  "Accept-charset: " url-mime-charset-string "\r\n"))
             ;; Languages we understand
             (if url-mime-language-string
                 (concat
                  "Accept-language: " url-mime-language-string "\r\n"))
             ;; Types we understand
             "Accept: " (or url-mime-accept-string "*/*") "\r\n"
             ;; User agent
             (url-http-user-agent-string)
             ;; Proxy Authorization
             proxy-auth
             ;; Authorization
             auth
             ;; Cookies
	     (when (url-use-cookies url-http-target-url)
	       (url-cookie-generate-header-lines
		host real-fname
		(equal "https" (url-type url-http-target-url))))
             ;; If-modified-since
             (if (and (not no-cache)
                      (member url-http-method '("GET" nil)))
                 (let ((tm (url-is-cached url-http-target-url)))
                   (if tm
                       (concat "If-modified-since: "
                               (url-get-normalized-date tm) "\r\n"))))
             ;; Whence we came
             (if ref-url (concat
                          "Referer: " ref-url "\r\n"))
             extra-headers
             ;; Length of data
             (if url-http-data
                 (concat
                  "Content-length: " (number-to-string
                                      (length url-http-data))
                  "\r\n"))
             ;; End request
             "\r\n"
             ;; Any data
             url-http-data
	     ;; If `url-http-data' is nil, avoid two CRLFs (Bug#8931).
	     (if url-http-data "\r\n")))
           ""))
    (url-http-debug "Request is: \n%s" request)
    request))

;; Parsing routines
(defun url-http-clean-headers ()
  "Remove trailing \r from header lines.
This allows us to use `mail-fetch-field', etc.
Return the number of characters removed."
  (declare (special url-http-end-of-headers))
  (let ((end (marker-position url-http-end-of-headers)))
    (goto-char (point-min))
    (while (re-search-forward "\r$" url-http-end-of-headers t)
      (replace-match ""))
    (- end url-http-end-of-headers)))

(defun url-http-handle-authentication (proxy)
  (declare (special status success url-http-method url-http-data
		    url-callback-function url-callback-arguments))
  (url-http-debug "Handling %s authentication" (if proxy "proxy" "normal"))
  (let ((auths (or (nreverse
		    (mail-fetch-field
		     (if proxy "proxy-authenticate" "www-authenticate")
		     nil nil t))
		  '("basic")))
	(type nil)
	(url (url-recreate-url url-current-object))
	(auth-url (url-recreate-url
		   (if (and proxy (boundp 'url-http-proxy))
		       url-http-proxy
		     url-current-object)))
	(url-basic-auth-storage (if proxy
				    ;; Cheating, but who cares? :)
				    'url-http-proxy-basic-auth-storage
				  'url-http-real-basic-auth-storage))
	auth
	(strength 0))

    ;; find strongest supported auth
    (dolist (this-auth auths)
      (setq this-auth (url-eat-trailing-space
		       (url-strip-leading-spaces
			this-auth)))
      (let* ((this-type
	      (if (string-match "[ \t]" this-auth)
		  (downcase (substring this-auth 0 (match-beginning 0)))
		(downcase this-auth)))
	     (registered (url-auth-registered this-type))
	     (this-strength (cddr registered)))
	(when (and registered (> this-strength strength))
	  (setq auth this-auth
		type this-type
		strength this-strength))))

    (if (not (url-auth-registered type))
	(progn
	  (widen)
	  (goto-char (point-max))
	  (insert "<hr>Sorry, but I do not know how to handle " type
		  " authentication.  If you'd like to write it,"
		  " send it to " url-bug-address ".<hr>")
	  (setq status t))
      (let* ((args (url-parse-args (subst-char-in-string ?, ?\; auth)))
	     (auth (url-get-authentication auth-url
					   (cdr-safe (assoc "realm" args))
					   type t args)))
	(if (not auth)
	    (setq success t)
	  (push (cons (if proxy "Proxy-Authorization" "Authorization") auth)
		url-http-extra-headers)
	  (let ((url-request-method url-http-method)
		(url-request-data url-http-data)
		(url-request-extra-headers url-http-extra-headers))
	    (url-retrieve-internal url url-callback-function
				   url-callback-arguments)))))))

(defun url-http-parse-response ()
  "Parse just the response code."
  (declare (special url-http-end-of-headers url-http-response-status
		    url-http-response-version))
  (if (not url-http-end-of-headers)
      (error "Trying to parse HTTP response code in odd buffer: %s" (buffer-name)))
  (url-http-debug "url-http-parse-response called in (%s)" (buffer-name))
  (goto-char (point-min))
  (skip-chars-forward " \t\n")		; Skip any blank crap
  (skip-chars-forward "HTTP/")		; Skip HTTP Version
  (setq url-http-response-version
	(buffer-substring (point)
			  (progn
			    (skip-chars-forward "[0-9].")
			    (point))))
  (setq url-http-response-status (read (current-buffer))))

(defun url-http-handle-cookies ()
  "Handle all set-cookie / set-cookie2 headers in an HTTP response.
The buffer must already be narrowed to the headers, so `mail-fetch-field' will
work correctly."
  (let ((cookies (nreverse (mail-fetch-field "Set-Cookie" nil nil t)))
	(cookies2 (nreverse (mail-fetch-field "Set-Cookie2" nil nil t))))
    (and cookies (url-http-debug "Found %d Set-Cookie headers" (length cookies)))
    (and cookies2 (url-http-debug "Found %d Set-Cookie2 headers" (length cookies2)))
    (while cookies
      (url-cookie-handle-set-cookie (pop cookies)))
;;;     (while cookies2
;;;       (url-cookie-handle-set-cookie2 (pop cookies)))
    )
  )

(defun url-http-parse-headers ()
 "Parse and handle HTTP specific headers.
Return t if and only if the current buffer is still active and
should be shown to the user."
  ;; The comments after each status code handled are taken from RFC
  ;; 2616 (HTTP/1.1)
  (declare (special url-http-end-of-headers url-http-response-status
		    url-http-response-version
		    url-http-method url-http-data url-http-process
		    url-callback-function url-callback-arguments))

  (url-http-mark-connection-as-free (url-host url-current-object)
				    (url-port url-current-object)
				    url-http-process)

  (if (or (not (boundp 'url-http-end-of-headers))
	  (not url-http-end-of-headers))
      (error "Trying to parse headers in odd buffer: %s" (buffer-name)))
  (goto-char (point-min))
  (url-http-debug "url-http-parse-headers called in (%s)" (buffer-name))
  (url-http-parse-response)
  (mail-narrow-to-head)
  ;;(narrow-to-region (point-min) url-http-end-of-headers)
  (let ((connection (mail-fetch-field "Connection")))
    ;; In HTTP 1.0, keep the connection only if there is a
    ;; "Connection: keep-alive" header.
    ;; In HTTP 1.1 (and greater), keep the connection unless there is a
    ;; "Connection: close" header
    (cond
     ((string= url-http-response-version "1.0")
      (unless (and connection
		   (string= (downcase connection) "keep-alive"))
	(delete-process url-http-process)))
     (t
      (when (and connection
		 (string= (downcase connection) "close"))
	(delete-process url-http-process)))))
  (let ((buffer (current-buffer))
	(class nil)
	(success nil)
	;; other status symbols: jewelry and luxury cars
	(status-symbol (cadr (assq url-http-response-status url-http-codes)))
	;; The filename part of a URL could be in remote file syntax,
	;; see Bug#6717 for an example.  We disable file name
	;; handlers, therefore.
	(file-name-handler-alist nil))
    (setq class (/ url-http-response-status 100))
    (url-http-debug "Parsed HTTP headers: class=%d status=%d" class url-http-response-status)
    (when (url-use-cookies url-http-target-url)
      (url-http-handle-cookies))

    (case class
      ;; Classes of response codes
      ;;
      ;; 5xx = Server Error
      ;; 4xx = Client Error
      ;; 3xx = Redirection
      ;; 2xx = Successful
      ;; 1xx = Informational
      (1				; Information messages
       ;; 100 = Continue with request
       ;; 101 = Switching protocols
       ;; 102 = Processing (Added by DAV)
       (url-mark-buffer-as-dead buffer)
       (error "HTTP responses in class 1xx not supported (%d)" url-http-response-status))
      (2				; Success
       ;; 200 Ok
       ;; 201 Created
       ;; 202 Accepted
       ;; 203 Non-authoritative information
       ;; 204 No content
       ;; 205 Reset content
       ;; 206 Partial content
       ;; 207 Multi-status (Added by DAV)
       (case status-symbol
	 ((no-content reset-content)
	  ;; No new data, just stay at the same document
	  (url-mark-buffer-as-dead buffer)
	  (setq success t))
	 (otherwise
	  ;; Generic success for all others.  Store in the cache, and
	  ;; mark it as successful.
	  (widen)
	  (if (and url-automatic-caching (equal url-http-method "GET"))
	      (url-store-in-cache buffer))
	  (setq success t))))
      (3				; Redirection
       ;; 300 Multiple choices
       ;; 301 Moved permanently
       ;; 302 Found
       ;; 303 See other
       ;; 304 Not modified
       ;; 305 Use proxy
       ;; 307 Temporary redirect
       (let ((redirect-uri (or (mail-fetch-field "Location")
			       (mail-fetch-field "URI"))))
	 (case status-symbol
	   (multiple-choices	    ; 300
	    ;; Quoth the spec (section 10.3.1)
	    ;; -------------------------------
	    ;; The requested resource corresponds to any one of a set of
	    ;; representations, each with its own specific location and
	    ;; agent-driven negotiation information is being provided so
	    ;; that the user can select a preferred representation and
	    ;; redirect its request to that location.
	    ;; [...]
	    ;; If the server has a preferred choice of representation, it
	    ;; SHOULD include the specific URI for that representation in
	    ;; the Location field; user agents MAY use the Location field
	    ;; value for automatic redirection.
	    ;; -------------------------------
	    ;; We do not support agent-driven negotiation, so we just
	    ;; redirect to the preferred URI if one is provided.
	    nil)
	   ((moved-permanently found temporary-redirect) ; 301 302 307
	    ;; If the 301|302 status code is received in response to a
	    ;; request other than GET or HEAD, the user agent MUST NOT
	    ;; automatically redirect the request unless it can be
	    ;; confirmed by the user, since this might change the
	    ;; conditions under which the request was issued.
	    (unless (member url-http-method '("HEAD" "GET"))
	      (setq redirect-uri nil)))
	   (see-other			; 303
	    ;; The response to the request can be found under a different
	    ;; URI and SHOULD be retrieved using a GET method on that
	    ;; resource.
	    (setq url-http-method "GET"
		  url-http-data nil))
	   (not-modified		; 304
	    ;; The 304 response MUST NOT contain a message-body.
	    (url-http-debug "Extracting document from cache... (%s)"
			    (url-cache-create-filename (url-view-url t)))
	    (url-cache-extract (url-cache-create-filename (url-view-url t)))
	    (setq redirect-uri nil
		  success t))
	   (use-proxy			; 305
	    ;; The requested resource MUST be accessed through the
	    ;; proxy given by the Location field.  The Location field
	    ;; gives the URI of the proxy.  The recipient is expected
	    ;; to repeat this single request via the proxy.  305
	    ;; responses MUST only be generated by origin servers.
	    (error "Redirection thru a proxy server not supported: %s"
		   redirect-uri))
	   (otherwise
	    ;; Treat everything like '300'
	    nil))
	 (when redirect-uri
	   ;; Clean off any whitespace and/or <...> cruft.
	   (if (string-match "\\([^ \t]+\\)[ \t]" redirect-uri)
	       (setq redirect-uri (match-string 1 redirect-uri)))
	   (if (string-match "^<\\(.*\\)>$" redirect-uri)
	       (setq redirect-uri (match-string 1 redirect-uri)))

	   ;; Some stupid sites (like sourceforge) send a
	   ;; non-fully-qualified URL (ie: /), which royally confuses
	   ;; the URL library.
	   (if (not (string-match url-nonrelative-link redirect-uri))
               ;; Be careful to use the real target URL, otherwise we may
               ;; compute the redirection relative to the URL of the proxy.
	       (setq redirect-uri
		     (url-expand-file-name redirect-uri url-http-target-url)))
           (let ((url-request-method url-http-method)
		 (url-request-data url-http-data)
		 (url-request-extra-headers url-http-extra-headers))
	     ;; Check existing number of redirects
	     (if (or (< url-max-redirections 0)
		     (and (> url-max-redirections 0)
			  (let ((events (car url-callback-arguments))
				(old-redirects 0))
			    (while events
			      (if (eq (car events) :redirect)
				  (setq old-redirects (1+ old-redirects)))
			      (and (setq events (cdr events))
				   (setq events (cdr events))))
			    (< old-redirects url-max-redirections))))
		 ;; url-max-redirections hasn't been reached, so go
		 ;; ahead and redirect.
		 (progn
		   ;; Remember that the request was redirected.
		   (setf (car url-callback-arguments)
			 (nconc (list :redirect redirect-uri)
				(car url-callback-arguments)))
		   ;; Put in the current buffer a forwarding pointer to the new
		   ;; destination buffer.
		   ;; FIXME: This is a hack to fix url-retrieve-synchronously
		   ;; without changing the API.  Instead url-retrieve should
		   ;; either simply not return the "destination" buffer, or it
		   ;; should take an optional `dest-buf' argument.
		   (set (make-local-variable 'url-redirect-buffer)
			(url-retrieve-internal
			 redirect-uri url-callback-function
			 url-callback-arguments
			 (url-silent url-current-object)
			 (not (url-use-cookies url-current-object))))
		   (url-mark-buffer-as-dead buffer))
	       ;; We hit url-max-redirections, so issue an error and
	       ;; stop redirecting.
	       (url-http-debug "Maximum redirections reached")
	       (setf (car url-callback-arguments)
		     (nconc (list :error (list 'error 'http-redirect-limit
					       redirect-uri))
			    (car url-callback-arguments)))
	       (setq success t))))))
      (4				; Client error
       ;; 400 Bad Request
       ;; 401 Unauthorized
       ;; 402 Payment required
       ;; 403 Forbidden
       ;; 404 Not found
       ;; 405 Method not allowed
       ;; 406 Not acceptable
       ;; 407 Proxy authentication required
       ;; 408 Request time-out
       ;; 409 Conflict
       ;; 410 Gone
       ;; 411 Length required
       ;; 412 Precondition failed
       ;; 413 Request entity too large
       ;; 414 Request-URI too large
       ;; 415 Unsupported media type
       ;; 416 Requested range not satisfiable
       ;; 417 Expectation failed
       ;; 422 Unprocessable Entity (Added by DAV)
       ;; 423 Locked
       ;; 424 Failed Dependency
       (case status-symbol
	 (unauthorized			; 401
	  ;; The request requires user authentication.  The response
	  ;; MUST include a WWW-Authenticate header field containing a
	  ;; challenge applicable to the requested resource.  The
	  ;; client MAY repeat the request with a suitable
	  ;; Authorization header field.
	  (url-http-handle-authentication nil))
	 (payment-required              ; 402
	  ;; This code is reserved for future use
	  (url-mark-buffer-as-dead buffer)
	  (error "Somebody wants you to give them money"))
	 (forbidden			; 403
	  ;; The server understood the request, but is refusing to
	  ;; fulfill it.  Authorization will not help and the request
	  ;; SHOULD NOT be repeated.
	  (setq success t))
	 (not-found			; 404
	  ;; Not found
	  (setq success t))
	 (method-not-allowed		; 405
	  ;; The method specified in the Request-Line is not allowed
	  ;; for the resource identified by the Request-URI.  The
	  ;; response MUST include an Allow header containing a list of
	  ;; valid methods for the requested resource.
	  (setq success t))
	 (not-acceptable		; 406
	  ;; The resource identified by the request is only capable of
	  ;; generating response entities which have content
	  ;; characteristics not acceptable according to the accept
	  ;; headers sent in the request.
	  (setq success t))
	 (proxy-authentication-required ; 407
	  ;; This code is similar to 401 (Unauthorized), but indicates
	  ;; that the client must first authenticate itself with the
	  ;; proxy.  The proxy MUST return a Proxy-Authenticate header
	  ;; field containing a challenge applicable to the proxy for
	  ;; the requested resource.
	  (url-http-handle-authentication t))
	 (request-timeout		; 408
	  ;; The client did not produce a request within the time that
	  ;; the server was prepared to wait.  The client MAY repeat
	  ;; the request without modifications at any later time.
	  (setq success t))
	 (conflict			; 409
	  ;; The request could not be completed due to a conflict with
	  ;; the current state of the resource.  This code is only
	  ;; allowed in situations where it is expected that the user
	  ;; might be able to resolve the conflict and resubmit the
	  ;; request.  The response body SHOULD include enough
	  ;; information for the user to recognize the source of the
	  ;; conflict.
	  (setq success t))
	 (gone                          ; 410
	  ;; The requested resource is no longer available at the
	  ;; server and no forwarding address is known.
	  (setq success t))
	 (length-required		; 411
	  ;; The server refuses to accept the request without a defined
	  ;; Content-Length.  The client MAY repeat the request if it
	  ;; adds a valid Content-Length header field containing the
	  ;; length of the message-body in the request message.
	  ;;
	  ;; NOTE - this will never happen because
	  ;; `url-http-create-request' automatically calculates the
	  ;; content-length.
	  (setq success t))
	 (precondition-failed		; 412
	  ;; The precondition given in one or more of the
	  ;; request-header fields evaluated to false when it was
	  ;; tested on the server.
	  (setq success t))
	 ((request-entity-too-large request-uri-too-large) ; 413 414
	  ;; The server is refusing to process a request because the
	  ;; request entity|URI is larger than the server is willing or
	  ;; able to process.
	  (setq success t))
	 (unsupported-media-type	; 415
	  ;; The server is refusing to service the request because the
	  ;; entity of the request is in a format not supported by the
	  ;; requested resource for the requested method.
	  (setq success t))
	 (requested-range-not-satisfiable ; 416
	  ;; A server SHOULD return a response with this status code if
	  ;; a request included a Range request-header field, and none
	  ;; of the range-specifier values in this field overlap the
	  ;; current extent of the selected resource, and the request
	  ;; did not include an If-Range request-header field.
	  (setq success t))
	 (expectation-failed		; 417
	  ;; The expectation given in an Expect request-header field
	  ;; could not be met by this server, or, if the server is a
	  ;; proxy, the server has unambiguous evidence that the
	  ;; request could not be met by the next-hop server.
	  (setq success t))
	 (otherwise
	  ;; The request could not be understood by the server due to
	  ;; malformed syntax.  The client SHOULD NOT repeat the
	  ;; request without modifications.
	  (setq success t)))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
	 (setf (car url-callback-arguments)
	       (nconc (list :error (list 'error 'http url-http-response-status))
		      (car url-callback-arguments)))))
      (5
       ;; 500 Internal server error
       ;; 501 Not implemented
       ;; 502 Bad gateway
       ;; 503 Service unavailable
       ;; 504 Gateway time-out
       ;; 505 HTTP version not supported
       ;; 507 Insufficient storage
       (setq success t)
       (case url-http-response-status
	 (not-implemented		; 501
	  ;; The server does not support the functionality required to
	  ;; fulfill the request.
	  nil)
	 (bad-gateway			; 502
	  ;; The server, while acting as a gateway or proxy, received
	  ;; an invalid response from the upstream server it accessed
	  ;; in attempting to fulfill the request.
	  nil)
	 (service-unavailable		; 503
	  ;; The server is currently unable to handle the request due
	  ;; to a temporary overloading or maintenance of the server.
	  ;; The implication is that this is a temporary condition
	  ;; which will be alleviated after some delay.  If known, the
	  ;; length of the delay MAY be indicated in a Retry-After
	  ;; header.  If no Retry-After is given, the client SHOULD
	  ;; handle the response as it would for a 500 response.
	  nil)
	 (gateway-timeout		; 504
	  ;; The server, while acting as a gateway or proxy, did not
	  ;; receive a timely response from the upstream server
	  ;; specified by the URI (e.g. HTTP, FTP, LDAP) or some other
	  ;; auxiliary server (e.g. DNS) it needed to access in
	  ;; attempting to complete the request.
	  nil)
	 (http-version-not-supported	; 505
	  ;; The server does not support, or refuses to support, the
	  ;; HTTP protocol version that was used in the request
	  ;; message.
	  nil)
	 (insufficient-storage		; 507 (DAV)
	  ;; The method could not be performed on the resource
	  ;; because the server is unable to store the representation
	  ;; needed to successfully complete the request.  This
	  ;; condition is considered to be temporary.  If the request
	  ;; which received this status code was the result of a user
	  ;; action, the request MUST NOT be repeated until it is
	  ;; requested by a separate user action.
	  nil))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
	 (setf (car url-callback-arguments)
	       (nconc (list :error (list 'error 'http url-http-response-status))
		      (car url-callback-arguments)))))
      (otherwise
       (error "Unknown class of HTTP response code: %d (%d)"
	      class url-http-response-status)))
    (if (not success)
	(url-mark-buffer-as-dead buffer))
    (url-http-debug "Finished parsing HTTP headers: %S" success)
    (widen)
    success))

;; Miscellaneous
(defun url-http-activate-callback ()
  "Activate callback specified when this buffer was created."
  (declare (special url-http-process
		    url-callback-function
		    url-callback-arguments))
  (url-http-mark-connection-as-free (url-host url-current-object)
				    (url-port url-current-object)
				    url-http-process)
  (url-http-debug "Activating callback in buffer (%s)" (buffer-name))
  (apply url-callback-function url-callback-arguments))

;; )

;; These unfortunately cannot be macros... please ignore them!
(defun url-http-idle-sentinel (proc why)
  "Remove (now defunct) process PROC from the list of open connections."
  (maphash (lambda (key val)
		(if (memq proc val)
		    (puthash key (delq proc val) url-http-open-connections)))
	      url-http-open-connections))

(defun url-http-end-of-document-sentinel (proc why)
  ;; Sentinel used to handle (i) terminated old HTTP/0.9 connections,
  ;; and (ii) closed connection due to reusing a HTTP connection which
  ;; we believed was still alive, but which the server closed on us.
  ;; We handle case (ii) by calling `url-http' again.
  (url-http-debug "url-http-end-of-document-sentinel in buffer (%s)"
		  (process-buffer proc))
  (url-http-idle-sentinel proc why)
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (cond ((not (looking-at "HTTP/"))
	     (if url-http-no-retry
		 ;; HTTP/0.9 just gets passed back no matter what
		 (url-http-activate-callback)
	       ;; Call `url-http' again if our connection expired.
	       (erase-buffer)
	       (url-http url-current-object url-callback-function
			 url-callback-arguments (current-buffer))))
	    ((url-http-parse-headers)
	     (url-http-activate-callback))))))

(defun url-http-simple-after-change-function (st nd length)
  ;; Function used when we do NOT know how long the document is going to be
  ;; Just _very_ simple 'downloaded %d' type of info.
  (declare (special url-http-end-of-headers))
  (url-lazy-message "Reading %s..." (url-pretty-length nd)))

(defun url-http-content-length-after-change-function (st nd length)
  "Function used when we DO know how long the document is going to be.
More sophisticated percentage downloaded, etc.
Also does minimal parsing of HTTP headers and will actually cause
the callback to be triggered."
  (declare (special url-current-object
		    url-http-end-of-headers
		    url-http-content-length
		    url-http-content-type
		    url-http-process))
  (if url-http-content-type
      (url-display-percentage
       "Reading [%s]... %s of %s (%d%%)"
       (url-percentage (- nd url-http-end-of-headers)
		       url-http-content-length)
       url-http-content-type
       (url-pretty-length (- nd url-http-end-of-headers))
       (url-pretty-length url-http-content-length)
       (url-percentage (- nd url-http-end-of-headers)
		       url-http-content-length))
    (url-display-percentage
     "Reading... %s of %s (%d%%)"
     (url-percentage (- nd url-http-end-of-headers)
		     url-http-content-length)
     (url-pretty-length (- nd url-http-end-of-headers))
     (url-pretty-length url-http-content-length)
     (url-percentage (- nd url-http-end-of-headers)
		     url-http-content-length)))

  (if (> (- nd url-http-end-of-headers) url-http-content-length)
      (progn
	;; Found the end of the document!  Wheee!
	(url-display-percentage nil nil)
	(url-lazy-message "Reading... done.")
	(if (url-http-parse-headers)
	    (url-http-activate-callback)))))

(defun url-http-chunked-encoding-after-change-function (st nd length)
  "Function used when dealing with 'chunked' encoding.
Cannot give a sophisticated percentage, but we need a different
function to look for the special 0-length chunk that signifies
the end of the document."
  (declare (special url-current-object
		    url-http-end-of-headers
		    url-http-content-type
		    url-http-chunked-length
		    url-http-chunked-counter
		    url-http-process url-http-chunked-start))
  (save-excursion
    (goto-char st)
    (let ((read-next-chunk t)
	  (case-fold-search t)
	  (regexp nil)
	  (no-initial-crlf nil))
      ;; We need to loop thru looking for more chunks even within
      ;; one after-change-function call.
      (while read-next-chunk
	(setq no-initial-crlf (= 0 url-http-chunked-counter))
	(if url-http-content-type
	    (url-display-percentage nil
	     "Reading [%s]... chunk #%d"
	     url-http-content-type url-http-chunked-counter)
	  (url-display-percentage nil
	   "Reading... chunk #%d"
	   url-http-chunked-counter))
	(url-http-debug "Reading chunk %d (%d %d %d)"
			url-http-chunked-counter st nd length)
	(setq regexp (if no-initial-crlf
			 "\\([0-9a-z]+\\).*\r?\n"
		       "\r?\n\\([0-9a-z]+\\).*\r?\n"))

	(if url-http-chunked-start
	    ;; We know how long the chunk is supposed to be, skip over
	    ;; leading crap if possible.
	    (if (> nd (+ url-http-chunked-start url-http-chunked-length))
		(progn
		  (url-http-debug "Got to the end of chunk #%d!"
				  url-http-chunked-counter)
		  (goto-char (+ url-http-chunked-start
				url-http-chunked-length)))
	      (url-http-debug "Still need %d bytes to hit end of chunk"
			      (- (+ url-http-chunked-start
				    url-http-chunked-length)
				 nd))
	      (setq read-next-chunk nil)))
	(if (not read-next-chunk)
	    (url-http-debug "Still spinning for next chunk...")
	  (if no-initial-crlf (skip-chars-forward "\r\n"))
	  (if (not (looking-at regexp))
	      (progn
	   ;; Must not have received the entirety of the chunk header,
		;; need to spin some more.
		(url-http-debug "Did not see start of chunk @ %d!" (point))
		(setq read-next-chunk nil))
 	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'start-open t
				       'end-open t
				       'chunked-encoding t
				       'face 'cursor
				       'invisible t))
	    (setq url-http-chunked-length (string-to-number (buffer-substring
                                                             (match-beginning 1)
                                                             (match-end 1))
                                                            16)
		  url-http-chunked-counter (1+ url-http-chunked-counter)
		  url-http-chunked-start (set-marker
					  (or url-http-chunked-start
					      (make-marker))
					  (match-end 0)))
;	    (if (not url-http-debug)
		(delete-region (match-beginning 0) (match-end 0));)
	    (url-http-debug "Saw start of chunk %d (length=%d, start=%d"
			    url-http-chunked-counter url-http-chunked-length
			    (marker-position url-http-chunked-start))
	    (if (= 0 url-http-chunked-length)
		(progn
		  ;; Found the end of the document!  Wheee!
		  (url-http-debug "Saw end of stream chunk!")
		  (setq read-next-chunk nil)
		  (url-display-percentage nil nil)
		  ;; Every chunk, even the last 0-length one, is
		  ;; terminated by CRLF.  Skip it.
		  (when (looking-at "\r?\n")
		    (url-http-debug "Removing terminator of last chunk")
		    (delete-region (match-beginning 0) (match-end 0)))
		  (if (re-search-forward "^\r*$" nil t)
		      (url-http-debug "Saw end of trailers..."))
		  (if (url-http-parse-headers)
		      (url-http-activate-callback))))))))))

(defun url-http-wait-for-headers-change-function (st nd length)
  ;; This will wait for the headers to arrive and then splice in the
  ;; next appropriate after-change-function, etc.
  (declare (special url-current-object
		    url-http-end-of-headers
		    url-http-content-type
		    url-http-content-length
		    url-http-transfer-encoding
		    url-callback-function
		    url-callback-arguments
		    url-http-process
		    url-http-method
		    url-http-after-change-function
		    url-http-response-status))
  (url-http-debug "url-http-wait-for-headers-change-function (%s)"
		  (buffer-name))
  (let ((end-of-headers nil)
	(old-http nil)
	(process-buffer (current-buffer))
	(content-length nil))
    (when (not (bobp))
      (goto-char (point-min))
      (if (and (looking-at ".*\n")	; have one line at least
	       (not (looking-at "^HTTP/[1-9]\\.[0-9]")))
	  ;; Not HTTP/x.y data, must be 0.9
	  ;; God, I wish this could die.
	  (setq end-of-headers t
		url-http-end-of-headers 0
		old-http t)
	(when (re-search-forward "^\r*$" nil t)
	  ;; Saw the end of the headers
	  (url-http-debug "Saw end of headers... (%s)" (buffer-name))
	  (setq url-http-end-of-headers (set-marker (make-marker)
						    (point))
		end-of-headers t)
	  (setq nd (- nd (url-http-clean-headers)))))

      (if (not end-of-headers)
	  ;; Haven't seen the end of the headers yet, need to wait
	  ;; for more data to arrive.
	  nil
	(unless old-http
	  (url-http-parse-response)
	  (mail-narrow-to-head)
	  (setq url-http-transfer-encoding (mail-fetch-field
					    "transfer-encoding")
		url-http-content-type (mail-fetch-field "content-type"))
	  (if (mail-fetch-field "content-length")
	      (setq url-http-content-length
		    (string-to-number (mail-fetch-field "content-length"))))
	  (widen))
	(when url-http-transfer-encoding
	  (setq url-http-transfer-encoding
		(downcase url-http-transfer-encoding)))

	(cond
	 ((null url-http-response-status)
	  ;; We got back a headerless malformed response from the
	  ;; server.
	  (url-http-activate-callback))
	 ((or (= url-http-response-status 204)
	      (= url-http-response-status 205))
	  (url-http-debug "%d response must have headers only (%s)."
			  url-http-response-status (buffer-name))
	  (when (url-http-parse-headers)
	    (url-http-activate-callback)))
	 ((string= "HEAD" url-http-method)
	  ;; A HEAD request is _ALWAYS_ terminated by the header
	  ;; information, regardless of any entity headers,
	  ;; according to section 4.4 of the HTTP/1.1 draft.
	  (url-http-debug "HEAD request must have headers only (%s)."
			  (buffer-name))
	  (when (url-http-parse-headers)
	    (url-http-activate-callback)))
	 ((string= "CONNECT" url-http-method)
	  ;; A CONNECT request is finished, but we cannot stick this
	  ;; back on the free connection list
	  (url-http-debug "CONNECT request must have headers only.")
	  (when (url-http-parse-headers)
	    (url-http-activate-callback)))
	 ((equal url-http-response-status 304)
	  ;; Only allowed to have a header section.  We have to handle
	  ;; this here instead of in url-http-parse-headers because if
	  ;; you have a cached copy of something without a known
	  ;; content-length, and try to retrieve it from the cache, we'd
	  ;; fall into the 'being dumb' section and wait for the
	  ;; connection to terminate, which means we'd wait for 10
	  ;; seconds for the keep-alives to time out on some servers.
	  (when (url-http-parse-headers)
	    (url-http-activate-callback)))
	 (old-http
	  ;; HTTP/0.9 always signaled end-of-connection by closing the
	  ;; connection.
	  (url-http-debug
	   "Saw HTTP/0.9 response, connection closed means end of document.")
	  (setq url-http-after-change-function
		'url-http-simple-after-change-function))
	 ((equal url-http-transfer-encoding "chunked")
	  (url-http-debug "Saw chunked encoding.")
	  (setq url-http-after-change-function
		'url-http-chunked-encoding-after-change-function)
	  (when (> nd url-http-end-of-headers)
	    (url-http-debug
	     "Calling initial chunked-encoding for extra data at end of headers")
	    (url-http-chunked-encoding-after-change-function
	     (marker-position url-http-end-of-headers) nd
	     (- nd url-http-end-of-headers))))
	 ((integerp url-http-content-length)
	  (url-http-debug
	   "Got a content-length, being smart about document end.")
	  (setq url-http-after-change-function
		'url-http-content-length-after-change-function)
	  (cond
	   ((= 0 url-http-content-length)
	    ;; We got a NULL body!  Activate the callback
	    ;; immediately!
	    (url-http-debug
	     "Got 0-length content-length, activating callback immediately.")
	    (when (url-http-parse-headers)
	      (url-http-activate-callback)))
	   ((> nd url-http-end-of-headers)
	    ;; Have some leftover data
	    (url-http-debug "Calling initial content-length for extra data at end of headers")
	    (url-http-content-length-after-change-function
	     (marker-position url-http-end-of-headers)
	     nd
	     (- nd url-http-end-of-headers)))
	   (t
	    nil)))
	 (t
	  (url-http-debug "No content-length, being dumb.")
	  (setq url-http-after-change-function
		'url-http-simple-after-change-function)))))
    ;; We are still at the beginning of the buffer... must just be
    ;; waiting for a response.
    (url-http-debug "Spinning waiting for headers...")
    (when (eq process-buffer (current-buffer))
      (goto-char (point-max)))))

;;;###autoload
(defun url-http (url callback cbargs &optional retry-buffer)
  "Retrieve URL via HTTP asynchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.
When retrieval is completed, the function CALLBACK is executed with
CBARGS as the arguments.

Optional arg RETRY-BUFFER, if non-nil, specifies the buffer of a
previous `url-http' call, which is being re-attempted."
  (check-type url vector "Need a pre-parsed URL.")
  (declare (special url-current-object
		    url-http-end-of-headers
		    url-http-content-type
		    url-http-content-length
		    url-http-transfer-encoding
		    url-http-after-change-function
		    url-callback-function
		    url-callback-arguments
		    url-show-status
		    url-http-method
		    url-http-extra-headers
		    url-http-data
		    url-http-chunked-length
		    url-http-chunked-start
		    url-http-chunked-counter
		    url-http-process))
  (let* ((host (url-host (or url-using-proxy url)))
	 (port (url-port (or url-using-proxy url)))
	 (connection (url-http-find-free-connection host port))
	 (buffer (or retry-buffer
		     (generate-new-buffer (format " *http %s:%d*" host port)))))
    (if (not connection)
	;; Failed to open the connection for some reason
	(progn
	  (kill-buffer buffer)
	  (setq buffer nil)
	  (error "Could not create connection to %s:%d" host port))
      (with-current-buffer buffer
	(mm-disable-multibyte)
	(setq url-current-object url
	      mode-line-format "%b [%s]")

	(dolist (var '(url-http-end-of-headers
		       url-http-content-type
		       url-http-content-length
		       url-http-transfer-encoding
		       url-http-after-change-function
		       url-http-response-version
		       url-http-response-status
		       url-http-chunked-length
		       url-http-chunked-counter
		       url-http-chunked-start
		       url-callback-function
		       url-callback-arguments
		       url-show-status
		       url-http-process
		       url-http-method
		       url-http-extra-headers
		       url-http-data
		       url-http-target-url
		       url-http-no-retry
		       url-http-connection-opened
		       url-http-proxy))
	  (set (make-local-variable var) nil))

	(setq url-http-method (or url-request-method "GET")
	      url-http-extra-headers url-request-extra-headers
	      url-http-data url-request-data
	      url-http-process connection
	      url-http-chunked-length nil
	      url-http-chunked-start nil
	      url-http-chunked-counter 0
	      url-callback-function callback
	      url-callback-arguments cbargs
	      url-http-after-change-function 'url-http-wait-for-headers-change-function
	      url-http-target-url url-current-object
	      url-http-no-retry retry-buffer
	      url-http-connection-opened nil
	      url-http-proxy url-using-proxy)

	(set-process-buffer connection buffer)
	(set-process-filter connection 'url-http-generic-filter)
	(let ((status (process-status connection)))
	  (cond
	   ((eq status 'connect)
	    ;; Asynchronous connection
	    (set-process-sentinel connection 'url-http-async-sentinel))
	   ((eq status 'failed)
	    ;; Asynchronous connection failed
	    (error "Could not create connection to %s:%d" host port))
	   (t
	    (set-process-sentinel connection 'url-http-end-of-document-sentinel)
	    (process-send-string connection (url-http-create-request)))))))
    buffer))

(defun url-http-async-sentinel (proc why)
  (declare (special url-callback-arguments))
  ;; We are performing an asynchronous connection, and a status change
  ;; has occurred.
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (cond
       (url-http-connection-opened
	(setq url-http-no-retry t)
	(url-http-end-of-document-sentinel proc why))
       ((string= (substring why 0 4) "open")
	(setq url-http-connection-opened t)
	(condition-case error
	    (process-send-string proc (url-http-create-request))
	  (file-error
	   (setq url-http-connection-opened nil)
	   (message "HTTP error: %s" error))))
       (t
	(setf (car url-callback-arguments)
	      (nconc (list :error (list 'error 'connection-failed why
					:host (url-host (or url-http-proxy url-current-object))
					:service (url-port (or url-http-proxy url-current-object))))
		     (car url-callback-arguments)))
	(url-http-activate-callback))))))

;; Since Emacs 19/20 does not allow you to change the
;; `after-change-functions' hook in the midst of running them, we fake
;; an after change by hooking into the process filter and inserting
;; the data ourselves.  This is slightly less efficient, but there
;; were tons of weird ways the after-change code was biting us in the
;; shorts.
;; FIXME this can probably be simplified since the above is no longer true.
(defun url-http-generic-filter (proc data)
  ;; Sometimes we get a zero-length data chunk after the process has
  ;; been changed to 'free', which means it has no buffer associated
  ;; with it.  Do nothing if there is no buffer, or 0 length data.
  (declare (special url-http-after-change-function))
  (and (process-buffer proc)
       (/= (length data) 0)
       (with-current-buffer (process-buffer proc)
	 (url-http-debug "Calling after change function `%s' for `%S'" url-http-after-change-function proc)
	 (funcall url-http-after-change-function
		  (point-max)
		  (progn
		    (goto-char (point-max))
		    (insert data)
		    (point-max))
		  (length data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file-name-handler stuff from here on out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'url-http-symbol-value-in-buffer
  (if (fboundp 'symbol-value-in-buffer)
      'symbol-value-in-buffer
    (lambda (symbol buffer &optional unbound-value)
      "Return the value of SYMBOL in BUFFER, or UNBOUND-VALUE if it is unbound."
      (with-current-buffer buffer
        (if (not (boundp symbol))
            unbound-value
          (symbol-value symbol))))))

(defun url-http-head (url)
  (let ((url-request-method "HEAD")
	(url-request-data nil))
    (url-retrieve-synchronously url)))

;;;###autoload
(defun url-http-file-exists-p (url)
  (let ((status nil)
	(exists nil)
	(buffer (url-http-head url)))
    (if (not buffer)
	(setq exists nil)
      (setq status (url-http-symbol-value-in-buffer 'url-http-response-status
						    buffer 500)
	    exists (and (integerp status)
			(>= status 200) (< status 300)))
      (kill-buffer buffer))
    exists))

;;;###autoload
(defalias 'url-http-file-readable-p 'url-http-file-exists-p)

(defun url-http-head-file-attributes (url &optional id-format)
  (let ((buffer (url-http-head url)))
    (when buffer
      (prog1
          (list
           nil                          ;dir / link / normal file
           1                            ;number of links to file.
           0 0                          ;uid ; gid
           nil nil nil                  ;atime ; mtime ; ctime
           (url-http-symbol-value-in-buffer 'url-http-content-length
                                            buffer -1)
           (eval-when-compile (make-string 10 ?-))
           nil nil nil)          ;whether gid would change ; inode ; device.
        (kill-buffer buffer)))))

(declare-function url-dav-file-attributes "url-dav" (url &optional id-format))

;;;###autoload
(defun url-http-file-attributes (url &optional id-format)
  (if (url-dav-supported-p url)
      (url-dav-file-attributes url id-format)
    (url-http-head-file-attributes url id-format)))

;;;###autoload
(defun url-http-options (url)
  "Return a property list describing options available for URL.
This list is retrieved using the `OPTIONS' HTTP method.

Property list members:

methods
  A list of symbols specifying what HTTP methods the resource
  supports.

dav
  A list of numbers specifying what DAV protocol/schema versions are
  supported.

dasl
  A list of supported DASL search types supported (string form)

ranges
  A list of the units available for use in partial document fetches.

p3p
  The `Platform For Privacy Protection' description for the resource.
  Currently this is just the raw header contents.  This is likely to
  change once P3P is formally supported by the URL package or
  Emacs/W3."
  (let* ((url-request-method "OPTIONS")
	 (url-request-data nil)
	 (buffer (url-retrieve-synchronously url))
	 (header nil)
	 (options nil))
    (when (and buffer (= 2 (/ (url-http-symbol-value-in-buffer
			       'url-http-response-status buffer 0) 100)))
      ;; Only parse the options if we got a 2xx response code!
      (with-current-buffer buffer
	(save-restriction
	  (save-match-data
	    (mail-narrow-to-head)

	    ;; Figure out what methods are supported.
	    (when (setq header (mail-fetch-field "allow"))
	      (setq options (plist-put
			     options 'methods
			     (mapcar 'intern (split-string header "[ ,]+")))))

	    ;; Check for DAV
	    (when (setq header (mail-fetch-field "dav"))
	      (setq options (plist-put
			     options 'dav
			     (delq 0
				   (mapcar 'string-to-number
					   (split-string header "[, ]+"))))))

	    ;; Now for DASL
	    (when (setq header (mail-fetch-field "dasl"))
	      (setq options (plist-put
			     options 'dasl
			     (split-string header "[, ]+"))))

	    ;; P3P - should get more detailed here.  FIXME
	    (when (setq header (mail-fetch-field "p3p"))
	      (setq options (plist-put options 'p3p header)))

	    ;; Check for whether they accept byte-range requests.
	    (when (setq header (mail-fetch-field "accept-ranges"))
	      (setq options (plist-put
			     options 'ranges
			     (delq 'none
				   (mapcar 'intern
					   (split-string header "[, ]+"))))))
	    ))))
    (if buffer (kill-buffer buffer))
    options))

;; HTTPS.  This used to be in url-https.el, but that file collides
;; with url-http.el on systems with 8-character file names.
(require 'tls)

;;;###autoload
(defconst url-https-default-port 443 "Default HTTPS port.")
;;;###autoload
(defconst url-https-asynchronous-p t "HTTPS retrievals are asynchronous.")

;; FIXME what is the point of this alias being an autoload?
;; Trying to use it will not cause url-http to be loaded,
;; since the full alias just gets dumped into loaddefs.el.

;;;###autoload (autoload 'url-default-expander "url-expand")
;;;###autoload
(defalias 'url-https-expand-file-name 'url-default-expander)

(defmacro url-https-create-secure-wrapper (method args)
  `(defun ,(intern (format (if method "url-https-%s" "url-https") method)) ,args
    ,(format "HTTPS wrapper around `%s' call." (or method "url-http"))
    (let ((url-gateway-method 'tls))
      (,(intern (format (if method "url-http-%s" "url-http") method))
       ,@(remove '&rest (remove '&optional args))))))

;;;###autoload (autoload 'url-https "url-http")
(url-https-create-secure-wrapper nil (url callback cbargs))
;;;###autoload (autoload 'url-https-file-exists-p "url-http")
(url-https-create-secure-wrapper file-exists-p (url))
;;;###autoload (autoload 'url-https-file-readable-p "url-http")
(url-https-create-secure-wrapper file-readable-p (url))
;;;###autoload (autoload 'url-https-file-attributes "url-http")
(url-https-create-secure-wrapper file-attributes (url &optional id-format))

(provide 'url-http)

;;; url-http.el ends here
