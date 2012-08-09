;;; mailheader.el --- mail header parsing, merging, formatting

;; Copyright (C) 1996, 2001-2012  Free Software Foundation, Inc.

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: tools, mail, news
;; Package: mail-utils

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

;; This package provides an abstraction to RFC822-style messages, used in
;; mail, news, and some other systems.  The simple syntactic rules for such
;; headers, such as quoting and line folding, are routinely reimplemented
;; in many individual packages.  This package removes the need for this
;; redundancy by representing message headers as association lists,
;; offering functions to extract the set of headers from a message, to
;; parse individual headers, to merge sets of headers, and to format a set
;; of headers.

;; The car of each element in the message-header alist is a symbol whose
;; print name is the name of the header, in all lower-case.  The cdr of an
;; element depends on the operation.  After extracting headers from a
;; message, it is a string, the value of the header.  An extracted set of
;; headers may be parsed further, which may turn it into a list, whose car
;; is the original value and whose subsequent elements depend on the
;; header.  For formatting, it is evaluated to obtain the strings to be
;; inserted.  For merging, one set of headers consists of strings, while
;; the other set will be evaluated with the symbols in the first set of
;; headers bound to their respective values.

;;; Code:

(eval-when-compile
  (require 'cl))

(defun mail-header-extract ()
  "Extract headers from current buffer after point.
Returns a header alist, where each element is a cons cell (name . value),
where NAME is a symbol, and VALUE is the string value of the header having
that name."
  (let ((message-headers ()) (top (point))
	start end)
    (while (and (setq start (point))
		(> (skip-chars-forward "^\0- :") 0)
		(= (following-char) ?:)
		(setq end (point))
		(progn (forward-char)
		       (> (skip-chars-forward " \t") 0)))
      (let ((header (intern (downcase (buffer-substring start end))))
	    (value (list (buffer-substring
			  (point) (progn (end-of-line) (point))))))
	(while (progn (forward-char) (> (skip-chars-forward " \t") 0))
	  (push (buffer-substring (point) (progn (end-of-line) (point)))
		value))
	(push (if (cdr value)
		  (cons header (mapconcat #'identity (nreverse value) " "))
		  (cons header (car value)))
	      message-headers)))
    (goto-char top)
    (nreverse message-headers)))

(defun mail-header-extract-no-properties ()
  "Extract headers from current buffer after point, without properties.
Returns a header alist, where each element is a cons cell (name . value),
where NAME is a symbol, and VALUE is the string value of the header having
that name."
  (mapcar
   (lambda (elt)
     (set-text-properties 0 (length (cdr elt)) nil (cdr elt))
     elt)
   (mail-header-extract)))

(defun mail-header-parse (parsing-rules headers)
  "Apply PARSING-RULES to HEADERS.
PARSING-RULES is an alist whose keys are header names (symbols) and whose
value is a parsing function.  The function takes one argument, a string,
and return a list of values, which will destructively replace the value
associated with the key in HEADERS, after being prepended with the original
value."
  (dolist (rule parsing-rules)
    (let ((header (assq (car rule) headers)))
      (when header
	(if (consp (cdr header))
	    (setf (cddr header) (funcall (cdr rule) (cadr header)))
	  (setf (cdr header)
		(cons (cdr header) (funcall (cdr rule) (cdr header))))))))
  headers)

;; Advertised part of the interface; see mail-header, mail-header-set.
(defvar headers)

(defsubst mail-header (header &optional header-alist)
  "Return the value associated with header HEADER in HEADER-ALIST.
If the value is a string, it is the original value of the header.  If the
value is a list, its first element is the original value of the header,
with any subsequent elements being the result of parsing the value.
If HEADER-ALIST is nil, the dynamically bound variable `headers' is used."
  (cdr (assq header (or header-alist headers))))

(defun mail-header-set (header value &optional header-alist)
  "Set the value associated with header HEADER to VALUE in HEADER-ALIST.
HEADER-ALIST defaults to the dynamically bound variable `headers' if nil.
See `mail-header' for the semantics of VALUE."
  (let* ((alist (or header-alist headers))
	(entry (assq header alist)))
    (if entry
	(setf (cdr entry) value)
	(nconc alist (list (cons header value)))))
  value)

(defsetf mail-header (header &optional header-alist) (value)
  `(mail-header-set ,header ,value ,header-alist))

(defun mail-header-merge (merge-rules headers)
  "Return a new header alist with MERGE-RULES applied to HEADERS.
MERGE-RULES is an alist whose keys are header names (symbols) and whose
values are forms to evaluate, the results of which are the new headers.  It
should be a string or a list of string.  The first element may be nil to
denote that the formatting functions must use the remaining elements, or
skip the header altogether if there are no other elements.
  The macro `mail-header' can be used to access headers in HEADERS."
  (mapcar
   (lambda (rule)
     (cons (car rule) (eval (cdr rule))))
   merge-rules))

(defvar mail-header-format-function
  (lambda (header value)
    "Function to format headers without a specified formatting function."
    (insert (capitalize (symbol-name header))
	    ": "
	    (if (consp value) (car value) value)
	    "\n")))

(defun mail-header-format (format-rules headers)
  "Use FORMAT-RULES to format HEADERS and insert into current buffer.
HEADERS should be an alist of the form (HEADER . VALUE),
where HEADER is a header field name (a symbol or a string),
and VALUE is the contents for that header field.

FORMAT-RULES is an alist of elements (HEADER . FUNCTION) Here HEADER
is a header field name (a symbol), and FUNCTION is how to format that
header field, if it appears in HEADERS.  Each FUNCTION should take two
arguments: the header symbol, and the value of that header.  The value
returned by FUNCTION is inserted in the buffer unless it is nil.

If the function for a header field is nil, or if no function is
specified for a particular header field, the default action is to
insert the value of the header, unless it is nil.

The headers are inserted in the order of the FORMAT-RULES.
A key of t in FORMAT-RULES represents any otherwise unmentioned headers.
A key of nil has as its value a list of defaulted headers to ignore."
  (let ((ignore (append (cdr (assq nil format-rules))
			(mapcar #'car format-rules))))
    (dolist (rule format-rules)
      (let* ((header (car rule))
	    (value (mail-header header)))
	(if (stringp header)
	    (setq header (intern header)))
	(cond ((null header) 'ignore)
	      ((eq header t)
	       (dolist (defaulted headers)
		 (unless (memq (car defaulted) ignore)
		   (let* ((header (car defaulted))
			  (value (cdr defaulted)))
		     (if (cdr rule)
			 (funcall (cdr rule) header value)
		       (funcall mail-header-format-function header value))))))
	      (value
	       (if (cdr rule)
		   (funcall (cdr rule) header value)
		 (funcall mail-header-format-function header value))))))
    (insert "\n")))

(provide 'mailheader)

;;; mailheader.el ends here
