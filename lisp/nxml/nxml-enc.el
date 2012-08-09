;;; nxml-enc.el --- XML encoding auto-detection

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

;; User entry points are nxml-start-auto-coding and
;; nxml-stop-auto-coding.  This is separate from nxml-mode, because
;; this cannot be autoloaded.  It may use
;; `xmltok-get-declared-encoding-position' which can be autoloaded.
;; It's separate from rng-auto.el so it can be byte-compiled, and
;; because it provides independent, useful functionality.

;;; Code:

(defvar nxml-file-name-ignore-case
  (memq system-type '(windows-nt)))

(defvar nxml-cached-file-name-auto-coding-regexp nil)
(defvar nxml-cached-auto-mode-alist nil)

(defun nxml-file-name-auto-coding-regexp ()
  "Return regexp for filenames for which XML auto-coding should be done."
  (if (eq auto-mode-alist nxml-cached-auto-mode-alist)
      nxml-cached-file-name-auto-coding-regexp
    (let ((alist auto-mode-alist)
	  (case-fold-search nxml-file-name-ignore-case)
	  regexps)
      (setq nxml-cached-auto-mode-alist alist)
      (while alist
	(when (eq (cdar alist) 'nxml-mode)
	  (setq regexps (cons (caar alist) regexps)))
	(setq alist (cdr alist)))
      (setq nxml-cached-file-name-auto-coding-regexp
	    (if (null (cdr regexps))
		(car regexps)
	      (mapconcat (lambda (r)
			   (concat "\\(?:" r "\\)"))
			 regexps
			 "\\|"))))))

(defvar nxml-non-xml-set-auto-coding-function nil
  "The function that `set-auto-coding-function' should call for non-XML files.")
(defun nxml-set-auto-coding (file-name size)
  (if (let ((case-fold-search nxml-file-name-ignore-case)
	    (regexp (nxml-file-name-auto-coding-regexp)))
	(and regexp
	     (string-match regexp file-name)))
      (nxml-set-xml-coding file-name size)
    (and nxml-non-xml-set-auto-coding-function
	 (funcall nxml-non-xml-set-auto-coding-function file-name size))))

(defun nxml-set-xml-coding (file-name size)
  "Function to use as `set-auto-coding-function' when file is known to be XML."
  (nxml-detect-coding-system (+ (point) (min size 1024))))

(declare-function xmltok-get-declared-encoding-position "xmltok"
                  (&optional limit))    ; autoloaded

(defun nxml-detect-coding-system (limit)
  (if (< limit (+ (point) 2))
      (if (eq (char-after) 0) 'no-conversion 'utf-8)
    (let ((first-two-chars (list (char-after)
				 (char-after (1+ (point))))))
      (cond ((equal first-two-chars '(#xFE #xFF))
	     (and (coding-system-p 'utf-16-be) 'utf-16-be))
	    ((equal first-two-chars '(#xFF #xFE))
	     (and (coding-system-p 'utf-16-le) 'utf-16-le))
	    ((memq 0 first-two-chars)
	     ;; Certainly not well-formed XML;
	     ;; perhaps UTF-16 without BOM.
	     ;; In any case, we can't handle it.
	     ;; no-conversion gives the user a chance to fix it.
	     'no-conversion)
	    ;; There are other things we might try here in the future
	    ;; eg UTF-8 BOM, UTF-16 with no BOM 
	    ;; translate to EBCDIC
	    (t
	     (let ((enc-pos (xmltok-get-declared-encoding-position limit)))
	       (cond ((consp enc-pos)
		      (or (nxml-mime-charset-coding-system
			   (buffer-substring-no-properties (car enc-pos)
							   (cdr enc-pos)))
			  ;; We have an encoding whose name we don't recognize.
			  ;; What to do?
			  ;; raw-text seems the best bet: since we got
			  ;; the XML decl it must be a superset of ASCII,
			  ;; so we don't need to go to no-conversion
			  'raw-text))
		     (enc-pos 'utf-8)
		     ;; invalid XML declaration
		     (t nil))))))))

(defun nxml-mime-charset-coding-system (charset)
  (let ((charset-sym (intern (downcase charset)))
	(coding-systems (coding-system-list t))
	coding-system ret)
    (while (and coding-systems (not ret))
      (setq coding-system (car coding-systems))
      (if (eq (coding-system-get coding-system 'mime-charset)
	      charset-sym)
	  (setq ret coding-system)
	(setq coding-systems (cdr coding-systems))))
    ret))

(defun nxml-start-auto-coding ()
  "Do encoding auto-detection as specified in the XML standard.
Applied to any files that `auto-mode-alist' says should be handled by
`nxml-mode'."
  (interactive)
  (unless (eq set-auto-coding-function 'nxml-set-auto-coding)
    (let ((inhibit-quit t))
      (setq nxml-non-xml-set-auto-coding-function set-auto-coding-function)
      (setq set-auto-coding-function 'nxml-set-auto-coding))))

(defun nxml-stop-auto-coding ()
  "Stop doing encoding auto-detection as specified in the XML standard."
  (interactive)
  (when (eq set-auto-coding-function 'nxml-set-auto-coding)
    (let ((inhibit-quit t))
      (setq set-auto-coding-function nxml-non-xml-set-auto-coding-function)
      (setq nxml-non-xml-set-auto-coding-function nil))))

;; Emacs 22 makes us-ascii an alias for iso-safe without
;; giving it a mime-charset property.
(unless (coding-system-get 'us-ascii 'mime-charset)
  (coding-system-put 'us-ascii 'mime-charset 'us-ascii))

(provide 'nxml-enc)

;;; nxml-enc.el ends here
