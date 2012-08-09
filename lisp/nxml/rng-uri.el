;;; rng-uri.el --- URI parsing and manipulation

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML

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

(defun rng-file-name-uri (f)
  "Return a URI for the filename F.
Multibyte characters are left as is.  Use `rng-uri-escape-multibyte' to
escape them using %HH."
  (setq f (expand-file-name f))
  (let ((url
	 (replace-regexp-in-string "[\000-\032\177<>#%\"{}|\\^[]`%?;]"
				   'rng-percent-encode
				   f)))
    (concat "file:"
	    (if (and (> (length url) 0)
		     (= (aref url 0) ?/))
		"//"
	      "///")
	    url)))

(defun rng-uri-escape-multibyte (uri)
  "Escape multibyte characters in URI."
  (replace-regexp-in-string "[:nonascii:]"
			    'rng-percent-encode
			    (encode-coding-string uri 'utf-8)))

(defun rng-percent-encode (str)
  (apply 'concat
	 (mapcar (lambda (ch)
		   (format "%%%x%x" (/ ch 16) (% ch 16)))
		 (string-to-list str))))


(defun rng-uri-file-name (uri)
  "Return the filename represented by a URI.
Signal an error if URI is not a valid file URL."
  (rng-uri-file-name-1 uri nil))

(defun rng-uri-pattern-file-name-regexp (pattern)
  "Return a regexp for filenames represented by URIs that match PATTERN."
  (rng-uri-file-name-1 pattern 'match))

(defun rng-uri-pattern-file-name-replace-match (pattern)
  (rng-uri-file-name-1 pattern 'replace))

;; pattern is either nil or match or replace
(defun rng-uri-file-name-1 (uri pattern)
  (unless (string-match "\\`\\(?:[^%]\\|%[0-9a-fA-F]{2}\\)*\\'" uri)
    (rng-uri-error "Bad escapes in URI `%s'" uri))
  (setq uri (rng-uri-unescape-multibyte uri))
  (let* ((components
	  (or (rng-uri-split uri)
	      (rng-uri-error "Cannot split URI `%s' into its components" uri)))
	 (scheme (nth 0 components))
	 (authority (nth 1 components))
	 (path (nth 2 components))
	 (absolutep (string-match "\\`/" path))
	 (query (nth 3 components))
	 (fragment-id (nth 4 components)))
    (cond ((not scheme)
	   (unless pattern
	     (rng-uri-error "URI `%s' does not have a scheme" uri)))
	  ((not (string= (downcase scheme) "file"))
	   (rng-uri-error "URI `%s' does not use the `file:' scheme" uri)))
    (when (not (member authority
		       (cons system-name '(nil "" "localhost"))))
      (rng-uri-error "URI `%s' does not start with `file:///' or `file://localhost/'"
	     uri))
    (when query
      (rng-uri-error "`?' not escaped in file URI `%s'" uri))
    (when fragment-id
      (rng-uri-error "URI `%s' has a fragment identifier" uri))
    (when (string-match ";" path)
      (rng-uri-error "`;' not escaped in URI `%s'" uri))
    (when (string-match "%2[fF]" path) ;; 2f is hex code of slash
      (rng-uri-error "Escaped slash in URI `%s'" uri))
    (when (and (eq system-type 'windows-nt)
	       absolutep
	       (file-name-absolute-p (substring path 1)))
      (setq path (substring path 1)))
    (when (and pattern (string-match "\\`\\./" path))
      (setq path (substring path 2)))
    (setq path
	  (cond ((eq pattern 'match)
		 (rng-uri-unescape-unibyte-match path))
		((eq pattern 'replace)
		 (rng-uri-unescape-unibyte-replace path 2))
		(t
		 (rng-uri-unescape-unibyte path))))
    (when (string-match "\000" path)
      (rng-uri-error "URI `%s' has NUL character in path" uri))
    (when (eq pattern 'match)
      (setq path
	    (concat (if absolutep
			"\\(\\)"
		      "\\(\\(?:[^/]*/\\)*\\)")
		    path)))
    (cond ((eq pattern 'match)
	   (concat "\\`" path "\\'"))
	  ((and (eq pattern 'replace)
		(not absolutep))
	   (concat "\\1" path))
	  (t path))))

(defun rng-uri-error (&rest args)
  (signal 'rng-uri-error (list (apply 'format args))))

(put 'rng-uri-error 'error-conditions '(error rng-uri-error))
(put 'rng-uri-error 'error-message "Invalid URI")

(defun rng-uri-split (str)
  (and (string-match "\\`\\(?:\\([^:/?#]+\\):\\)?\
\\(?://\\([^/?#]*\\)\\)?\
\\([^?#]*\\)\
\\(?:\\?\\([^#]*\\)\\)?\
\\(?:#\\(\\(?:.\\|\n\\)*\\)\\)?\\'"
		     str)
       (list (match-string 1 str)
	     (match-string 2 str)
	     (match-string 3 str)
	     (match-string 4 str)
	     (match-string 5 str))))

(defun rng-uri-join (scheme authority path &optional query fragment-id)
  (when path
    (let (parts)
      (when fragment-id
	(setq parts (list "#" fragment-id)))
      (when query
	(setq parts
	      (cons "?"
		    (cons query parts))))
      (setq parts (cons path parts))
      (when authority
	(setq parts
	      (cons "//"
		    (cons authority parts))))
      (when scheme
	(setq parts
	      (cons scheme
		    (cons ":" parts))))
      (apply 'concat parts))))

(defun rng-uri-resolve (uri-ref base-uri)
  "Resolve a possibly relative URI reference into absolute form.
URI-REF is the URI reference to be resolved.
BASE-URI is the base URI to use for resolving it.
The algorithm is specified by RFC 2396.
If there is some problem with URI-REF or BASE-URI, then
URI-REF will be returned."
  (let* ((components (rng-uri-split uri-ref))
	 (scheme (nth 0 components))
	 (authority (nth 1 components))
	 (path (nth 2 components))
	 (query (nth 3 components))
	 (fragment-id (nth 4 components))
	 (base-components (rng-uri-split base-uri)))
    (if (or (not components)
	    scheme
	    (not base-components)
	    (not (nth 0 base-components)))
	uri-ref
      (setq scheme (nth 0 base-components))
      (when (not authority)
	(setq authority (nth 1 base-components))
	(if (and (equal path "") (not query))
	    ;; Handle same document reference by returning
	    ;; same URI (RFC 2396bis does this too).
	    (setq path (nth 2 base-components)
		  query (nth 3 base-components))
	  (setq path (rng-resolve-path path (nth 2 base-components)))))
      (rng-uri-join scheme
		    authority
		    path
		    query
		    fragment-id))))

;; See RFC 2396 5.2, steps 5 and 6
(defun rng-resolve-path (path base-path)
  ;; Step 5
  (if (or (string-match "\\`/" path)
	  (not (string-match "\\`/" base-path)))
      path
    ;; Step 6
    ;; (a), (b)
    (let ((segments (rng-split-path path))
	  (base-segments (rng-split-path base-path)))
      (if (> (length base-segments) 1)
	  (setq segments (nconc (nbutlast base-segments)
				segments))
	(setcar segments
		(concat (car base-segments) (car segments))))
      ;; (d)
      (let ((last-segment (last segments)))
	(when (equal (car last-segment) ".")
	  (setcar last-segment "")))
      ;; (c)
      (setq segments (delete "." segments))
      ;; (e)
      (let (iter matched)
	(while (progn
		 (setq matched nil)
		 (setq iter (cdr segments))
		 (while (and iter (not matched))
		   (if (or (not (equal (cadr iter) ".."))
			   (equal (car iter) ".."))
		       (setq iter (cdr iter))
		     (setcar iter nil)
		     (setcar (cdr iter)
			     ;; (f)
			     (if (cddr iter) nil ""))
		     (setq matched t)
		     (setq segments (delq nil segments))))
		 matched)))
      (rng-join-path segments))))

(defun rng-relative-uri (full base)
  "Return a URI that relative to BASE is equivalent to FULL.
The returned URI will be relative if possible.
Both FULL and BASE must be absolute URIs."
  (let* ((components (rng-uri-split full))
	 (scheme (nth 0 components))
	 (authority (nth 1 components))
	 (path (nth 2 components))
	 (query (nth 3 components))
	 (fragment-id (nth 4 components))
	 (base-components (rng-uri-split base)))
    (if (and components
	     base-components
	     scheme
	     (equal scheme
		    (nth 0 base-components)))
	(progn
	  (setq scheme nil)
	  (when (and authority
		     (equal authority
			    (nth 1 base-components)))
	    (setq authority nil)
	    (setq path (rng-relative-path path (nth 2 base-components))))
	  (rng-uri-join scheme authority path query fragment-id))
      full)))

(defun rng-relative-path (path base-path)
  (let ((segments (rng-split-path path))
	(base-segments (rng-split-path base-path)))
    (when (> (length base-segments) 1)
      (setq base-segments (nbutlast base-segments)))
    (if (or (member "." segments)
	    (member ".." segments)
	    (member "." base-segments)
	    (member ".." base-segments))
	path
      (while (and segments
		  base-segments
		  (string= (car segments)
			   (car base-segments)))
	(setq segments (cdr segments))
	(setq base-segments (cdr base-segments)))
      (while base-segments
	(setq base-segments (cdr base-segments))
	(setq segments (cons ".." segments)))
      (when (equal (car segments) "")
	(setq segments (cons "." segments)))
      (rng-join-path segments))))

(defun rng-split-path (path)
  (let ((start 0)
	segments)
    (while (string-match "/" path start)
      (setq segments (cons (substring path start (match-beginning 0))
			   segments))
      (setq start (match-end 0)))
    (nreverse (cons (substring path start) segments))))

(defun rng-join-path (segments)
  (and segments
       (mapconcat 'identity segments "/")))

(defun rng-uri-unescape-multibyte (str)
  (replace-regexp-in-string "\\(?:%[89a-fA-F][0-9a-fA-F]\\)+"
			    'rng-multibyte-percent-decode
			    str))

(defun rng-multibyte-percent-decode (str)
  (decode-coding-string  (apply 'string
				(mapcar (lambda (h) (string-to-number h 16))
					(split-string str "%")))
			 'utf-8))

(defun rng-uri-unescape-unibyte (str)
  (replace-regexp-in-string "%[0-7][0-9a-fA-F]"
			    (lambda (h)
			      (string-to-number (substring h 1) 16))
			    str
			    t
			    t))

(defun rng-uri-unescape-unibyte-match (str)
  (replace-regexp-in-string "%[0-7][0-9a-fA-F]\\|[^%]"
			    (lambda (match)
			      (if (string= match "*")
				  "\\([^/]*\\)"
				(regexp-quote
				 (if (= (length match) 1)
				     match
				   (string-to-number (substring match 1)
						     16)))))
			    str
			    t
			    t))

(defun rng-uri-unescape-unibyte-replace (str next-match-index)
  (replace-regexp-in-string
   "%[0-7][0-9a-fA-F]\\|[^%]"
   (lambda (match)
     (if (string= match "*")
	 (let ((n next-match-index))
	   (setq next-match-index (1+ n))
	   (format "\\%s" n))
       (let ((ch (if (= (length match) 1)
		     (aref match 0)
		   (string-to-number (substring match 1)
				     16))))
	 (if (eq ch ?\\)
	     (string ?\\ ?\\)
	   (string ch)))))
     str
     t
     t))

(provide 'rng-uri)

;;; rng-uri.el ends here
