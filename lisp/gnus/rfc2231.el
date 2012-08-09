;;; rfc2231.el --- Functions for decoding rfc2231 headers

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(eval-when-compile (require 'cl))
(require 'ietf-drums)
(require 'rfc2047)
(autoload 'mm-encode-body "mm-bodies")
(autoload 'mail-header-remove-whitespace "mail-parse")
(autoload 'mail-header-remove-comments "mail-parse")

(defun rfc2231-get-value (ct attribute)
  "Return the value of ATTRIBUTE from CT."
  (cdr (assq attribute (cdr ct))))

(defun rfc2231-parse-qp-string (string)
  "Parse QP-encoded string using `rfc2231-parse-string'.
N.B.  This is in violation with RFC2047, but it seem to be in common use."
  (rfc2231-parse-string (rfc2047-decode-string string)))

(defun rfc2231-parse-string (string &optional signal-error)
  "Parse STRING and return a list.
The list will be on the form
 `(name (attribute . value) (attribute . value)...)'.

If the optional SIGNAL-ERROR is non-nil, signal an error when this
function fails in parsing of parameters.  Otherwise, this function
must never cause a Lisp error."
  (with-temp-buffer
    (let ((ttoken (ietf-drums-token-to-list ietf-drums-text-token))
	  (stoken (ietf-drums-token-to-list ietf-drums-tspecials))
	  (ntoken (ietf-drums-token-to-list "0-9"))
	  c type attribute encoded number parameters value)
      (ietf-drums-init
       (condition-case nil
	   (mail-header-remove-whitespace
	    (mail-header-remove-comments string))
	 ;; The most likely cause of an error is unbalanced parentheses
	 ;; or double-quotes.  If all parentheses and double-quotes are
	 ;; quoted meaninglessly with backslashes, removing them might
	 ;; make it parsable.  Let's try...
	 (error
	  (let (mod)
	    (when (and (string-match "\\\\\"" string)
		       (not (string-match "\\`\"\\|[^\\]\"" string)))
	      (setq string (mm-replace-in-string string "\\\\\"" "\"")
		    mod t))
	    (when (and (string-match "\\\\(" string)
		       (string-match "\\\\)" string)
		       (not (string-match "\\`(\\|[^\\][()]" string)))
	      (setq string (mm-replace-in-string string "\\\\\\([()]\\)" "\\1")
		    mod t))
	    (or (and mod
		     (ignore-errors
		       (mail-header-remove-whitespace
			(mail-header-remove-comments string))))
		;; Finally, attempt to extract only type.
		(if (string-match
		     (concat "\\`[\t\n ]*\\([^" ietf-drums-tspecials "\t\n ]+"
			     "\\(?:/[^" ietf-drums-tspecials
			     "\t\n ]+\\)?\\)\\(?:[\t\n ;]\\|\\'\\)")
		     string)
		    (match-string 1 string)
		  ""))))))
      (let ((table (copy-syntax-table ietf-drums-syntax-table)))
	(modify-syntax-entry ?\' "w" table)
	(modify-syntax-entry ?* " " table)
	(modify-syntax-entry ?\; " " table)
	(modify-syntax-entry ?= " " table)
	;; The following isn't valid, but one should be liberal
	;; in what one receives.
	(modify-syntax-entry ?\: "w" table)
	(set-syntax-table table))
      (setq c (char-after))
      (when (and (memq c ttoken)
		 (not (memq c stoken))
		 (setq type (ignore-errors
			      (downcase
			       (buffer-substring (point) (progn
							   (forward-sexp 1)
							   (point)))))))
	;; Do the params
	(condition-case err
	    (progn
	      (while (not (eobp))
		(setq c (char-after))
		(unless (eq c ?\;)
		  (error "Invalid header: %s" string))
		(forward-char 1)
		;; If c in nil, then this is an invalid header, but
		;; since elm generates invalid headers on this form,
		;; we allow it.
		(when (setq c (char-after))
		  (if (and (memq c ttoken)
			   (not (memq c stoken)))
		      (setq attribute
			    (intern
			     (downcase
			      (buffer-substring
			       (point) (progn (forward-sexp 1) (point))))))
		    (error "Invalid header: %s" string))
		  (setq c (char-after))
		  (if (eq c ?*)
		      (progn
			(forward-char 1)
			(setq c (char-after))
			(if (not (memq c ntoken))
			    (setq encoded t
				  number nil)
			  (setq number
				(string-to-number
				 (buffer-substring
				  (point) (progn (forward-sexp 1) (point)))))
			  (setq c (char-after))
			  (when (eq c ?*)
			    (setq encoded t)
			    (forward-char 1)
			    (setq c (char-after)))))
		    (setq number nil
			  encoded nil))
		  (unless (eq c ?=)
		    (error "Invalid header: %s" string))
		  (forward-char 1)
		  (setq c (char-after))
		  (cond
		   ((eq c ?\")
		    (setq value (buffer-substring (1+ (point))
						  (progn
						    (forward-sexp 1)
						    (1- (point)))))
		    (when encoded
		      (setq value (mapconcat (lambda (c) (format "%%%02x" c))
					     value ""))))
		   ((and (or (memq c ttoken)
			     ;; EXTENSION: Support non-ascii chars.
			     (> c ?\177))
			 (not (memq c stoken)))
		    (setq value
			  (buffer-substring
			   (point)
			   (progn
			     ;; Jump over asterisk, non-ASCII
			     ;; and non-boundary characters.
			     (while (and c
					 (or (eq c ?*)
					     (> c ?\177)
					     (not (eq (char-syntax c) ? ))))
			       (forward-char 1)
			       (setq c (char-after)))
			     (point)))))
		   (t
		    (error "Invalid header: %s" string)))
		  (push (list attribute value number encoded)
			parameters))))
	  (error
	   (setq parameters nil)
	   (when signal-error
	     (signal (car err) (cdr err)))))

	;; Now collect and concatenate continuation parameters.
	(let ((cparams nil)
	      elem)
	  (loop for (attribute value part encoded)
		in (sort parameters (lambda (e1 e2)
				      (< (or (caddr e1) 0)
					 (or (caddr e2) 0))))
		do (cond
		    ;; First part.
		    ((or (not (setq elem (assq attribute cparams)))
			 (and (numberp part)
			      (zerop part)))
		     (push (list attribute value encoded) cparams))
		    ;; Repetition of a part; do nothing.
		    ((and elem
			  (null number))
		     )
		    ;; Concatenate continuation parts.
		    (t
		     (setcar (cdr elem) (concat (cadr elem) value)))))
	  ;; Finally decode encoded values.
	  (cons type (mapcar
		      (lambda (elem)
			(cons (car elem)
			      (if (nth 2 elem)
				  (rfc2231-decode-encoded-string (nth 1 elem))
				(nth 1 elem))))
		      (nreverse cparams))))))))

(defun rfc2231-decode-encoded-string (string)
  "Decode an RFC2231-encoded string.
These look like:
 \"us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A\",
 \"us-ascii''This%20is%20%2A%2A%2Afun%2A%2A%2A\",
 \"'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A\",
 \"''This%20is%20%2A%2A%2Afun%2A%2A%2A\", or
 \"This is ***fun***\"."
  (string-match "\\`\\(?:\\([^']+\\)?'\\([^']+\\)?'\\)?\\(.+\\)" string)
  (let ((coding-system (mm-charset-to-coding-system (match-string 1 string)))
	;;(language (match-string 2 string))
	(value (match-string 3 string)))
    (mm-with-unibyte-buffer
      (insert value)
      (goto-char (point-min))
      (while (re-search-forward "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)" nil t)
	(insert
	 (prog1
	     (string-to-number (match-string 1) 16)
	   (delete-region (match-beginning 0) (match-end 0)))))
      ;; Decode using the charset, if any.
      (if (memq coding-system '(nil ascii))
	  (buffer-string)
	(mm-decode-coding-string (buffer-string) coding-system)))))

(defun rfc2231-encode-string (param value)
  "Return and PARAM=VALUE string encoded according to RFC2231.
Use `mml-insert-parameter' or `mml-insert-parameter-string' to insert
the result of this function."
  (let ((control (ietf-drums-token-to-list ietf-drums-no-ws-ctl-token))
	(tspecial (ietf-drums-token-to-list ietf-drums-tspecials))
	(special (ietf-drums-token-to-list "*'%\n\t"))
	(ascii (ietf-drums-token-to-list ietf-drums-text-token))
	(num -1)
	;; Don't make lines exceeding 76 column.
	(limit (- 74 (length param)))
	spacep encodep charsetp charset broken)
    (mm-with-multibyte-buffer
      (insert value)
      (goto-char (point-min))
      (while (not (eobp))
	(cond
	 ((or (memq (following-char) control)
	      (memq (following-char) tspecial)
	      (memq (following-char) special))
	  (setq encodep t))
	 ((eq (following-char) ? )
	  (setq spacep t))
	 ((not (memq (following-char) ascii))
	  (setq charsetp t)))
	(forward-char 1))
      (when charsetp
	(setq charset (mm-encode-body)))
      (mm-disable-multibyte)
      (cond
       ((or encodep charsetp
	    (progn
	      (end-of-line)
	      (> (current-column) (if spacep (- limit 2) limit))))
	(setq limit (- limit 6))
	(goto-char (point-min))
	(insert (symbol-name (or charset 'us-ascii)) "''")
	(while (not (eobp))
	  (if (or (not (memq (following-char) ascii))
		  (memq (following-char) control)
		  (memq (following-char) tspecial)
		  (memq (following-char) special)
		  (eq (following-char) ? ))
	      (progn
		(when (>= (current-column) (1- limit))
		  (insert ";\n")
		  (setq broken t))
		(insert "%" (format "%02x" (following-char)))
		(delete-char 1))
	    (when (> (current-column) limit)
	      (insert ";\n")
	      (setq broken t))
	    (forward-char 1)))
	(goto-char (point-min))
	(if (not broken)
	    (insert param "*=")
	  (while (not (eobp))
	    (insert (if (>= num 0) " " "")
		    param "*" (format "%d" (incf num)) "*=")
	    (forward-line 1))))
       (spacep
	(goto-char (point-min))
	(insert param "=\"")
	(goto-char (point-max))
	(insert "\""))
       (t
	(goto-char (point-min))
	(insert param "=")))
      (buffer-string))))

(provide 'rfc2231)

;;; rfc2231.el ends here
