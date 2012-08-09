;;; url-dav.el --- WebDAV support

;; Copyright (C) 2001, 2004-2012  Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Maintainer: Bill Perry <wmperry@gnu.org>
;; Keywords: url, vc

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

;; DAV is in RFC 2518.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'xml)
(require 'url-util)
(require 'url-handlers)

(defvar url-dav-supported-protocols '(1 2)
  "List of supported DAV versions.")

(defun url-intersection (l1 l2)
  "Return a list of the elements occurring in both of the lists L1 and L2."
  (if (null l2)
      l2
    (let (result)
      (while l1
	(if (member (car l1) l2)
	    (setq result (cons (pop l1) result))
	  (pop l1)))
      (nreverse result))))

;;;###autoload
(defun url-dav-supported-p (url)
  (and (featurep 'xml)
       (fboundp 'xml-expand-namespace)
       (url-intersection url-dav-supported-protocols
			 (plist-get (url-http-options url) 'dav))))

(defun url-dav-node-text (node)
  "Return the text data from the XML node NODE."
  (mapconcat (lambda (txt)
	       (if (stringp txt)
		   txt
		 "")) (xml-node-children node) " "))


;;; Parsing routines for the actual node contents.
;;
;; I am not incredibly happy with how this code looks/works right
;; now, but it DOES work, and if we get the API right, our callers
;; won't have to worry about the internal representation.

(defconst url-dav-datatype-attribute
  'urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/dt)

(defun url-dav-process-integer-property (node)
  (truncate (string-to-number (url-dav-node-text node))))

(defun url-dav-process-number-property (node)
  (string-to-number (url-dav-node-text node)))

(defconst url-dav-iso8601-regexp
  (let* ((dash "-?")
	 (colon ":?")
	 (4digit "\\([0-9][0-9][0-9][0-9]\\)")
	 (2digit "\\([0-9][0-9]\\)")
	 (date-fullyear 4digit)
	 (date-month 2digit)
	 (date-mday 2digit)
	 (time-hour 2digit)
	 (time-minute 2digit)
	 (time-second 2digit)
	 (time-secfrac "\\(\\.[0-9]+\\)?")
	 (time-numoffset (concat "[-+]\\(" time-hour "\\):" time-minute))
	 (time-offset (concat "Z" time-numoffset))
	 (partial-time (concat time-hour colon time-minute colon time-second
			       time-secfrac))
	 (full-date (concat date-fullyear dash date-month dash date-mday))
	 (full-time (concat partial-time time-offset))
	 (date-time (concat full-date "T" full-time)))
    (list (concat "^" full-date)
	  (concat "T" partial-time)
	  (concat "Z" time-numoffset)))
  "List of regular expressions matching ISO 8601 dates.
1st regular expression matches the date.
2nd regular expression matches the time.
3rd regular expression matches the (optional) timezone specification.")

(defun url-dav-process-date-property (node)
  (require 'parse-time)
  (let* ((date-re (nth 0 url-dav-iso8601-regexp))
	 (time-re (nth 1 url-dav-iso8601-regexp))
	 (tz-re (nth 2 url-dav-iso8601-regexp))
	 (date-string (url-dav-node-text node))
	 re-start
	 time seconds minute hour fractional-seconds
	 day month year day-of-week dst tz)
    ;; We need to populate 'time' with
    ;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)

    ;; Nobody else handles iso8601 correctly, let's do it ourselves.
    (when (string-match date-re date-string re-start)
      (setq year (string-to-number (match-string 1 date-string))
	    month (string-to-number (match-string 2 date-string))
	    day (string-to-number (match-string 3 date-string))
	    re-start (match-end 0))
      (when (string-match time-re date-string re-start)
	(setq hour (string-to-number (match-string 1 date-string))
	      minute (string-to-number (match-string 2 date-string))
	      seconds (string-to-number (match-string 3 date-string))
	      fractional-seconds (string-to-number (or
                                                    (match-string 4 date-string)
                                                    "0"))
	      re-start (match-end 0))
	(when (string-match tz-re date-string re-start)
	  (setq tz (match-string 1 date-string)))
	(url-debug 'dav "Parsed iso8601%s date" (if tz "tz" ""))
	(setq time (list seconds minute hour day month year day-of-week dst tz))))

    ;; Fall back to having Gnus do fancy things for us.
    (when (not time)
      (setq time (parse-time-string date-string)))

    (if time
	(setq time (apply 'encode-time time))
      (url-debug 'dav "Unable to decode date (%S) (%s)"
		 (xml-node-name node) date-string))
    time))

(defun url-dav-process-boolean-property (node)
  (/= 0 (string-to-number (url-dav-node-text node))))

(defun url-dav-process-uri-property (node)
  ;; Returns a parsed representation of the URL...
  (url-generic-parse-url (url-dav-node-text node)))

(defun url-dav-find-parser (node)
  "Find a function to parse the XML node NODE."
  (or (get (xml-node-name node) 'dav-parser)
      (let ((fn (intern (format "url-dav-process-%s" (xml-node-name node)))))
	(if (not (fboundp fn))
	    (setq fn 'url-dav-node-text)
	  (put (xml-node-name node) 'dav-parser fn))
	fn)))

(defmacro url-dav-dispatch-node (node)
  `(funcall (url-dav-find-parser ,node) ,node))

(defun url-dav-process-DAV:prop (node)
  ;; A prop node has content model of ANY
  ;;
  ;; Some predefined nodes have special meanings though.
  ;;
  ;; DAV:supportedlock    - list of DAV:lockentry
  ;; DAV:source
  ;; DAV:iscollection     - boolean
  ;; DAV:getcontentlength - integer
  ;; DAV:ishidden         - boolean
  ;; DAV:getcontenttype   - string
  ;; DAV:resourcetype     - node who's name is the resource type
  ;; DAV:getlastmodified  - date
  ;; DAV:creationdate     - date
  ;; DAV:displayname      - string
  ;; DAV:getetag          - unknown
  (let ((children (xml-node-children node))
	(node-type nil)
	(props nil)
	(value nil)
	(handler-func nil))
    (when (not children)
      (error "No child nodes in DAV:prop"))

    (while children
      (setq node (car children)
	    node-type (intern
		       (or
			(cdr-safe (assq url-dav-datatype-attribute
					(xml-node-attributes node)))
			"unknown"))
	    value nil)

      (case node-type
	((dateTime.iso8601tz
	  dateTime.iso8601
	  dateTime.tz
	  dateTime.rfc1123
	  dateTime
	  date)				; date is our 'special' one...
	 ;; Some type of date/time string.
	 (setq value (url-dav-process-date-property node)))
	(int
	 ;; Integer type...
	 (setq value (url-dav-process-integer-property node)))
	((number float)
	 (setq value (url-dav-process-number-property node)))
	(boolean
	 (setq value (url-dav-process-boolean-property node)))
	(uri
	 (setq value (url-dav-process-uri-property node)))
	(otherwise
	 (if (not (eq node-type 'unknown))
	     (url-debug 'dav "Unknown data type in url-dav-process-prop: %s"
			node-type))
	 (setq value (url-dav-dispatch-node node))))

      (setq props (plist-put props (xml-node-name node) value)
	    children (cdr children)))
    props))

(defun url-dav-process-DAV:supportedlock (node)
  ;; DAV:supportedlock is a list of DAV:lockentry items.
  ;; DAV:lockentry in turn contains a DAV:lockscope and DAV:locktype.
  ;; The DAV:lockscope must have a single node beneath it, ditto for
  ;; DAV:locktype.
  (let ((children (xml-node-children node))
	(results nil)
	scope type)
    (while children
      (when (and (not (stringp (car children)))
		 (eq (xml-node-name (car children)) 'DAV:lockentry))
	(setq scope (assq 'DAV:lockscope (xml-node-children (car children)))
	      type (assq 'DAV:locktype (xml-node-children (car children))))
	(when (and scope type)
	  (setq scope (xml-node-name (car (xml-node-children scope)))
		type (xml-node-name (car (xml-node-children type))))
	  (push (cons type scope) results)))
      (setq children (cdr children)))
    results))

(defun url-dav-process-subnode-property (node)
  ;; Returns a list of child node names.
  (delq nil (mapcar 'car-safe (xml-node-children node))))

(defalias 'url-dav-process-DAV:depth 'url-dav-process-integer-property)
(defalias 'url-dav-process-DAV:resourcetype 'url-dav-process-subnode-property)
(defalias 'url-dav-process-DAV:locktype 'url-dav-process-subnode-property)
(defalias 'url-dav-process-DAV:lockscope 'url-dav-process-subnode-property)
(defalias 'url-dav-process-DAV:getcontentlength 'url-dav-process-integer-property)
(defalias 'url-dav-process-DAV:getlastmodified 'url-dav-process-date-property)
(defalias 'url-dav-process-DAV:creationdate 'url-dav-process-date-property)
(defalias 'url-dav-process-DAV:iscollection 'url-dav-process-boolean-property)
(defalias 'url-dav-process-DAV:ishidden 'url-dav-process-boolean-property)

(defun url-dav-process-DAV:locktoken (node)
  ;; DAV:locktoken can have one or more DAV:href children.
  (delq nil (mapcar (lambda (n)
		      (if (stringp n)
			  n
			(url-dav-dispatch-node n)))
		    (xml-node-children node))))

(defun url-dav-process-DAV:owner (node)
  ;; DAV:owner can contain anything.
  (delq nil (mapcar (lambda (n)
		      (if (stringp n)
			  n
			(url-dav-dispatch-node n)))
		    (xml-node-children node))))

(defun url-dav-process-DAV:activelock (node)
  ;; DAV:activelock can contain:
  ;;  DAV:lockscope
  ;;  DAV:locktype
  ;;  DAV:depth
  ;;  DAV:owner (optional)
  ;;  DAV:timeout (optional)
  ;;  DAV:locktoken (optional)
  (let ((children (xml-node-children node))
	(results nil))
    (while children
      (if (listp (car children))
	  (push (cons (xml-node-name (car children))
		      (url-dav-dispatch-node (car children)))
		results))
      (setq children (cdr children)))
    results))

(defun url-dav-process-DAV:lockdiscovery (node)
  ;; Can only contain a list of DAV:activelock objects.
  (let ((children (xml-node-children node))
	(results nil))
    (while children
      (cond
       ((stringp (car children))
	;; text node? why?
	nil)
       ((eq (xml-node-name (car children)) 'DAV:activelock)
	(push (url-dav-dispatch-node (car children)) results))
       (t
	;; Ignore unknown nodes...
	nil))
      (setq children (cdr children)))
    results))

(defun url-dav-process-DAV:status (node)
  ;; The node contains a standard HTTP/1.1 response line... we really
  ;; only care about the numeric status code.
  (let ((status (url-dav-node-text node)))
    (if (string-match "\\`[ \r\t\n]*HTTP/[0-9.]+ \\([0-9]+\\)" status)
	(string-to-number (match-string 1 status))
      500)))

(defun url-dav-process-DAV:propstat (node)
  ;; A propstate node can have the following children...
  ;;
  ;; DAV:prop - a list of properties and values
  ;; DAV:status - An HTTP/1.1 status line
  (let ((children (xml-node-children node))
	(props nil)
	(status nil))
    (when (not children)
      (error "No child nodes in DAV:propstat"))

    (setq props (url-dav-dispatch-node (assq 'DAV:prop children))
	  status (url-dav-dispatch-node (assq 'DAV:status children)))

    ;; Need to parse out the HTTP status
    (setq props (plist-put props 'DAV:status status))
    props))

(defun url-dav-process-DAV:response (node)
  (let ((children (xml-node-children node))
	(propstat nil)
	(href))
    (when (not children)
      (error "No child nodes in DAV:response"))

    ;; A response node can have the following children...
    ;;
    ;; DAV:href     - URL the response is for.
    ;; DAV:propstat - see url-dav-process-propstat
    ;; DAV:responsedescription - text description of the response
    (setq propstat (assq 'DAV:propstat children)
	  href (assq 'DAV:href children))

    (when (not href)
      (error "No href in DAV:response"))

    (when (not propstat)
      (error "No propstat in DAV:response"))

    (setq propstat (url-dav-dispatch-node propstat)
	  href (url-dav-dispatch-node href))
    (cons href propstat)))

(defun url-dav-process-DAV:multistatus (node)
  (let ((children (xml-node-children node))
	(results nil))
    (while children
      (push (url-dav-dispatch-node (car children)) results)
      (setq children (cdr children)))
    results))


;;; DAV request/response generation/processing
(defun url-dav-process-response (buffer url)
  "Parse a WebDAV response from BUFFER, interpreting it relative to URL.

The buffer must have been retrieved by HTTP or HTTPS and contain an
XML document."
  (declare (special url-http-content-type
		    url-http-response-status
		    url-http-end-of-headers))
  (let ((tree nil)
	(overall-status nil))
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (goto-char url-http-end-of-headers)
	    (setq overall-status url-http-response-status)

	    ;; XML documents can be transferred as either text/xml or
	    ;; application/xml, and we are required to accept both of
	    ;; them.
	    (if (and
		 url-http-content-type
		 (string-match "\\`\\(text\\|application\\)/xml"
			       url-http-content-type))
		(setq tree (xml-parse-region (point) (point-max)))))
	;; Clean up after ourselves.
	(kill-buffer buffer)))

    ;; We should now be
    (if (eq (xml-node-name (car tree)) 'DAV:multistatus)
	(url-dav-dispatch-node (car tree))
      (url-debug 'dav "Got back singleton response for URL(%S)" url)
      (let ((properties (url-dav-dispatch-node (car tree))))
	;; We need to make sure we have a DAV:status node in there for
	;; higher-level code;
	(setq properties (plist-put properties 'DAV:status overall-status))
	;; Make this look like a DAV:multistatus parse tree so that
	;; nobody but us needs to know the difference.
	(list (cons url properties))))))

(defun url-dav-request (url method tag body
				 &optional depth headers namespaces)
  "Perform WebDAV operation METHOD on URL.  Return the parsed responses.
Automatically creates an XML request body if TAG is non-nil.
BODY is the XML document fragment to be enclosed by <TAG></TAG>.

DEPTH is how deep the request should propagate.  Default is 0, meaning
it should apply only to URL.  A negative number means to use
`Infinity' for the depth.  Not all WebDAV servers support this depth
though.

HEADERS is an assoc list of extra headers to send in the request.

NAMESPACES is an assoc list of (NAMESPACE . EXPANSION), and these are
added to the <TAG> element.  The DAV=DAV: namespace is automatically
added to this list, so most requests can just pass in nil."
  ;; Take care of the default value for depth...
  (setq depth (or depth 0))

  ;; Now let's translate it into something webdav can understand.
  (if (< depth 0)
      (setq depth "Infinity")
    (setq depth (int-to-string depth)))
  (if (not (assoc "DAV" namespaces))
      (setq namespaces (cons '("DAV" . "DAV:") namespaces)))

  (let* ((url-request-extra-headers `(("Depth" . ,depth)
				      ("Content-type" . "text/xml")
				      ,@headers))
	 (url-request-method method)
	 (url-request-data
	  (if tag
	      (concat
	       "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
	       "<" (symbol-name tag) " "
	       ;; add in the appropriate namespaces...
	       (mapconcat (lambda (ns)
			    (concat "xmlns:" (car ns) "='" (cdr ns) "'"))
			  namespaces "\n    ")
	       ">\n"
	       body
	       "</" (symbol-name tag) ">\n"))))
    (url-dav-process-response (url-retrieve-synchronously url) url)))

(defun url-dav-get-properties (url &optional attributes depth namespaces)
  "Return properties for URL, up to DEPTH levels deep.

Returns an assoc list, where the key is the filename (possibly a full
URI), and the value is a standard property list of DAV property
names (ie: DAV:resourcetype)."
  (url-dav-request url "PROPFIND" 'DAV:propfind
		   (if attributes
		       (mapconcat (lambda (attr)
				    (concat "<DAV:prop><"
					    (symbol-name attr)
					    "/></DAV:prop>"))
				  attributes "\n  ")
		     "  <DAV:allprop/>")
		   depth nil namespaces))

(defmacro url-dav-http-success-p (status)
  "Return whether STATUS was the result of a successful DAV request."
  `(= (/ (or ,status 500) 100) 2))


;;; Locking support
(defvar url-dav-lock-identifier (concat "mailto:" user-mail-address)
  "*URL used as contact information when creating locks in DAV.
This will be used as the contents of the DAV:owner/DAV:href tag to
identify the owner of a LOCK when requesting it.  This will be shown
to other users when the DAV:lockdiscovery property is requested, so
make sure you are comfortable with it leaking to the outside world.")

(defun url-dav-lock-resource (url exclusive &optional depth)
  "Request a lock on URL.  If EXCLUSIVE is non-nil, get an exclusive lock.
Optional 3rd argument DEPTH says how deep the lock should go, default is 0
\(lock only the resource and none of its children\).

Returns a cons-cell of (SUCCESSFUL-RESULTS . FAILURE-RESULTS).
SUCCESSFUL-RESULTS is a list of (URL STATUS locktoken).
FAILURE-RESULTS is a list of (URL STATUS)."
  (setq	exclusive (if exclusive "<DAV:exclusive/>" "<DAV:shared/>"))
  (let* ((body
	  (concat
	   "  <DAV:lockscope>" exclusive "</DAV:lockscope>\n"
	   "  <DAV:locktype> <DAV:write/> </DAV:locktype>\n"
	   "  <DAV:owner>\n"
	   "    <DAV:href>" url-dav-lock-identifier "</DAV:href>\n"
	   "  </DAV:owner>\n"))
	 (response nil)		  ; Responses to the LOCK request
	 (result nil)		  ; For walking thru the response list
	 (child-url nil)
	 (child-status nil)
	 (failures nil)		  ; List of failure cases (URL . STATUS)
	 (successes nil))	  ; List of success cases (URL . STATUS)
    (setq response (url-dav-request url "LOCK" 'DAV:lockinfo body
				    depth '(("Timeout" . "Infinite"))))

    ;; Get the parent URL ready for expand-file-name
    (if (not (vectorp url))
	(setq url (url-generic-parse-url url)))

    ;; Walk thru the response list, fully expand the URL, and grab the
    ;; status code.
    (while response
      (setq result (pop response)
	    child-url (url-expand-file-name (pop result) url)
	    child-status (or (plist-get result 'DAV:status) 500))
      (if (url-dav-http-success-p child-status)
	  (push (list url child-status "huh") successes)
	(push (list url child-status) failures)))
    (cons successes failures)))

(defun url-dav-active-locks (url &optional depth)
  "Return an assoc list of all active locks on URL."
  (let ((response (url-dav-get-properties url '(DAV:lockdiscovery) depth))
	(properties nil)
	(child nil)
	(child-url nil)
	(child-results nil)
	(results nil))
    (if (not (vectorp url))
	(setq url (url-generic-parse-url url)))

    (while response
      (setq child (pop response)
	    child-url (pop child)
	    child-results nil)
      (when (and (url-dav-http-success-p (plist-get child 'DAV:status))
		 (setq child (plist-get child 'DAV:lockdiscovery)))
	;; After our parser has had its way with it, The
	;; DAV:lockdiscovery property is a list of DAV:activelock
	;; objects, which are comprised of DAV:activelocks, which
	;; assoc lists of properties and values.
	(while child
	  (if (assq 'DAV:locktoken (car child))
	      (let ((tokens (cdr (assq 'DAV:locktoken (car child))))
		    (owners (cdr (assq 'DAV:owner (car child)))))
		(dolist (token tokens)
		  (dolist (owner owners)
		    (push (cons token owner) child-results)))))
	  (pop child)))
      (if child-results
	  (push (cons (url-expand-file-name child-url url) child-results)
		results)))
    results))

(defun url-dav-unlock-resource (url lock-token)
  "Release the lock on URL represented by LOCK-TOKEN.
Returns t if the lock was successfully released."
  (declare (special url-http-response-status))
  (let* ((url-request-extra-headers (list (cons "Lock-Token"
						(concat "<" lock-token ">"))))
	 (url-request-method "UNLOCK")
	 (url-request-data nil)
	 (buffer (url-retrieve-synchronously url))
	 (result nil))
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (setq result (url-dav-http-success-p url-http-response-status)))
	(kill-buffer buffer)))
    result))


;;; file-name-handler stuff
(defun url-dav-file-attributes-mode-string (properties)
  (let ((modes (make-string 10 ?-))
	(supported-locks (plist-get properties 'DAV:supportedlock))
	(executable-p (equal (plist-get properties 'http://apache.org/dav/props/executable)
			     "T"))
	(directory-p (memq 'DAV:collection (plist-get properties 'DAV:resourcetype)))
	(readable t)
	(lock nil))
    ;; Assume we can read this, otherwise the PROPFIND would have
    ;; failed.
    (when readable
      (aset modes 1 ?r)
      (aset modes 4 ?r)
      (aset modes 7 ?r))

    (when directory-p
      (aset modes 0 ?d))

    (when executable-p
      (aset modes 3 ?x)
      (aset modes 6 ?x)
      (aset modes 9 ?x))

    (while supported-locks
      (setq lock (car supported-locks)
	    supported-locks (cdr supported-locks))
      (case (car lock)
	(DAV:write
	 (case (cdr lock)
	   (DAV:shared			; group permissions (possibly world)
	    (aset modes 5 ?w))
	   (DAV:exclusive
	    (aset modes 2 ?w))		; owner permissions?
	   (otherwise
	    (url-debug 'dav "Unrecognized DAV:lockscope (%S)" (cdr lock)))))
	(otherwise
	 (url-debug 'dav "Unrecognized DAV:locktype (%S)" (car lock)))))
    modes))

(autoload 'url-http-head-file-attributes "url-http")

(defun url-dav-file-attributes (url &optional id-format)
  (let ((properties (cdar (url-dav-get-properties url))))
    (if (and properties
	     (url-dav-http-success-p (plist-get properties 'DAV:status)))
	;; We got a good DAV response back..
        (list
         ;; t for directory, string for symbolic link, or nil
         ;; Need to support DAV Bindings to figure out the
         ;; symbolic link issues.
         (if (memq 'DAV:collection (plist-get properties 'DAV:resourcetype)) t nil)

         ;; Number of links to file... Needs DAV Bindings.
         1

         ;; File uid - no way to figure out?
         0

         ;; File gid - no way to figure out?
         0

         ;; Last access time - ???
         nil

         ;; Last modification time
         (plist-get properties 'DAV:getlastmodified)

         ;; Last status change time... just reuse last-modified
         ;; for now.
         (plist-get properties 'DAV:getlastmodified)

         ;; size in bytes
         (or (plist-get properties 'DAV:getcontentlength) 0)

         ;; file modes as a string like `ls -l'
         ;;
         ;; Should be able to build this up from the
         ;; DAV:supportedlock attribute pretty easily.  Getting
         ;; the group info could be impossible though.
         (url-dav-file-attributes-mode-string properties)

         ;; t if file's gid would change if it were deleted &
         ;; recreated.  No way for us to know that thru DAV.
         nil

         ;; inode number - meaningless
         nil

         ;; device number - meaningless
         nil)
      ;; Fall back to just the normal http way of doing things.
      (url-http-head-file-attributes url id-format))))

(defun url-dav-save-resource (url obj &optional content-type lock-token)
  "Save OBJ as URL using WebDAV.
URL must be a fully qualified URL.
OBJ may be a buffer or a string."
  (declare (special url-http-response-status))
  (let ((buffer nil)
	(result nil)
	(url-request-extra-headers nil)
	(url-request-method "PUT")
	(url-request-data
	 (cond
	  ((bufferp obj)
	   (with-current-buffer obj
	     (buffer-string)))
	  ((stringp obj)
	   obj)
	  (t
	   (error "Invalid object to url-dav-save-resource")))))

    (if lock-token
	(push
	 (cons "If" (concat "(<" lock-token ">)"))
	 url-request-extra-headers))

    ;; Everything must always have a content-type when we submit it.
    (push
     (cons "Content-type" (or content-type "application/octet-stream"))
     url-request-extra-headers)

    ;; Do the save...
    (setq buffer (url-retrieve-synchronously url))

    ;; Sanity checking
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (setq result (url-dav-http-success-p url-http-response-status)))
	(kill-buffer buffer)))
    result))

(eval-when-compile
  (defmacro url-dav-delete-something (url lock-token &rest error-checking)
    "Delete URL completely, with no sanity checking whatsoever.  DO NOT USE.
This is defined as a macro that will not be visible from compiled files.
Use with care, and even then think three times."
    `(progn
       ,@error-checking
       (url-dav-request ,url "DELETE" nil nil -1
			(if ,lock-token
			    (list
			     (cons "If"
				   (concat "(<" ,lock-token ">)"))))))))


(defun url-dav-delete-directory (url &optional recursive lock-token)
  "Delete the WebDAV collection URL.
If optional second argument RECURSIVE is non-nil, then delete all
files in the collection as well."
  (let ((status nil)
	(props nil)
	(props nil))
    (setq props (url-dav-delete-something
		 url lock-token
		 (setq props (url-dav-get-properties url '(DAV:getcontenttype) 1))
		 (if (and (not recursive)
			  (/= (length props) 1))
		     (signal 'file-error (list "Removing directory"
					       "directory not empty" url)))))

     (mapc (lambda (result)
	     (setq status (plist-get (cdr result) 'DAV:status))
	     (if (not (url-dav-http-success-p status))
		 (signal 'file-error (list "Removing directory"
					   "Error removing"
					   (car result) status))))
	   props))
  nil)

(defun url-dav-delete-file (url &optional lock-token)
  "Delete file named URL."
  (let ((props nil)
	(status nil))
    (setq props (url-dav-delete-something
		 url lock-token
		 (setq props (url-dav-get-properties url))
		 (if (eq (plist-get (cdar props) 'DAV:resourcetype) 'DAV:collection)
		     (signal 'file-error (list "Removing old name" "is a collection" url)))))

    (mapc (lambda (result)
	    (setq status (plist-get (cdr result) 'DAV:status))
	    (if (not (url-dav-http-success-p status))
		(signal 'file-error (list "Removing old name"
					  "Error removing"
					  (car result) status))))
	  props))
  nil)

(defun url-dav-directory-files (url &optional full match nosort files-only)
  "Return a list of names of files in URL.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself."
  (let ((properties (url-dav-get-properties url '(DAV:resourcetype) 1))
	(child-url nil)
	(child-props nil)
	(files nil)
	(parsed-url (url-generic-parse-url url)))

    (if (= (length properties) 1)
	(signal 'file-error (list "Opening directory" "not a directory" url)))

    (while properties
      (setq child-props (pop properties)
	    child-url (pop child-props))
      (if (and (eq (plist-get child-props 'DAV:resourcetype) 'DAV:collection)
	       files-only)
	  ;; It is a directory, and we were told to return just files.
	  nil

	;; Fully expand the URL and then rip off the beginning if we
	;; are not supposed to return fully-qualified names.
	(setq child-url (url-expand-file-name child-url parsed-url))
	(if (not full)
	    (setq child-url (substring child-url (length url))))

	;; We don't want '/' as the last character in filenames...
	(if (string-match "/$" child-url)
	    (setq child-url (substring child-url 0 -1)))

	;; If we have a match criteria, then apply it.
	(if (or (and match (not (string-match match child-url)))
		(string= child-url "")
		(string= child-url url))
	    nil
	  (push child-url files))))

    (if nosort
	files
      (sort files 'string-lessp))))

(defun url-dav-file-directory-p (url)
  "Return t if URL names an existing DAV collection."
  (let ((properties (cdar (url-dav-get-properties url '(DAV:resourcetype)))))
    (eq (plist-get properties 'DAV:resourcetype) 'DAV:collection)))

(defun url-dav-make-directory (url &optional parents)
  "Create the directory DIR and any nonexistent parent dirs."
  (declare (special url-http-response-status))
  (let* ((url-request-extra-headers nil)
	 (url-request-method "MKCOL")
	 (url-request-data nil)
	 (buffer (url-retrieve-synchronously url))
	 (result nil))
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (case url-http-response-status
	      (201			; Collection created in its entirety
	       (setq result t))
	      (403			; Forbidden
	       nil)
	      (405			; Method not allowed
	       nil)
	      (409			; Conflict
	       nil)
	      (415 			; Unsupported media type (WTF?)
	       nil)
	      (507			; Insufficient storage
	       nil)
	      (otherwise
	       nil)))
	(kill-buffer buffer)))
    result))

(defun url-dav-rename-file (oldname newname &optional overwrite)
  (if (not (and (string-match url-handler-regexp oldname)
		(string-match url-handler-regexp newname)))
      (signal 'file-error
	      (list "Cannot rename between different URL backends"
		    oldname newname)))

  (let* ((headers nil)
	 (props nil)
	 (status nil)
	 (directory-p (url-dav-file-directory-p oldname))
	 (exists-p (url-http-file-exists-p newname)))

    (if (and exists-p
	     (or
	      (null overwrite)
	      (and (numberp overwrite)
		   (not (yes-or-no-p
			 (format "File %s already exists; rename to it anyway? "
				 newname))))))
	(signal 'file-already-exists (list "File already exists" newname)))

    ;; Honor the overwrite flag...
    (if overwrite (push '("Overwrite" . "T") headers))

    ;; Have to tell them where to copy it to!
    (push (cons "Destination" newname) headers)

    ;; Always send a depth of -1 in case we are moving a collection.
    (setq props (url-dav-request oldname "MOVE" nil nil (if directory-p -1 0)
				 headers))

    (mapc (lambda (result)
	    (setq status (plist-get (cdr result) 'DAV:status))

	    (if (not (url-dav-http-success-p status))
		(signal 'file-error (list "Renaming" oldname newname status))))
	  props)
    t))

(defun url-dav-file-name-all-completions (file url)
  "Return a list of all completions of file name FILE in URL.
These are all file names in URL which begin with FILE."
  (url-dav-directory-files url nil (concat "^" file ".*")))

(defun url-dav-file-name-completion (file url)
  "Complete file name FILE in URL.
Returns the longest string common to all file names in URL
that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if URL contains no name starting with FILE."
  (let ((matches (url-dav-file-name-all-completions file url))
	(result nil))
    (cond
     ((null matches)
      ;; No matches
      nil)
     ((and (= (length matches) 1)
	   (string= file (car matches)))
      ;; Only one file and FILE matches it exactly...
      t)
     (t
      ;; Need to figure out the longest string that they have in common
      (setq matches (sort matches (lambda (a b) (> (length a) (length b)))))
      (let ((n (length file))
	    (searching t)
	    (regexp nil)
	    (failed nil))
	(while (and searching
		    (< n (length (car matches))))
	  (setq regexp (concat "^" (substring (car matches) 0 (1+ n)))
		failed nil)
	  (dolist (potential matches)
	    (if (not (string-match regexp potential))
		(setq failed t)))
	  (if failed
	      (setq searching nil)
	    (incf n)))
	(substring (car matches) 0 n))))))

(defun url-dav-register-handler (op)
  (put op 'url-file-handlers (intern-soft (format "url-dav-%s" op))))

(mapc 'url-dav-register-handler
      ;; These handlers are disabled because they incorrectly presume that
      ;; the URL specifies an HTTP location and thus break FTP URLs.
      '(;; file-name-all-completions
	;; file-name-completion
	;; rename-file
	;; make-directory
	;; file-directory-p
	;; directory-files
	;; delete-file
	;; delete-directory
	;; file-attributes
	))


;;; Version Control backend cruft

;(put 'vc-registered 'url-file-handlers 'url-dav-vc-registered)

;;;###autoload
(defun url-dav-vc-registered (url)
  (if (and (string-match "\\`https?" url)
	   (plist-get (url-http-options url) 'dav))
      (progn
	(vc-file-setprop url 'vc-backend 'dav)
	t)))


;;; Miscellaneous stuff.

(provide 'url-dav)

;;; url-dav.el ends here
