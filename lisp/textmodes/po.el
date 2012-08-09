;;; po.el --- basic support of PO translation files -*- coding: latin-1; -*-

;; Copyright (C) 1995-1998, 2000-2012 Free Software Foundation, Inc.

;; Authors: François Pinard <pinard@iro.umontreal.ca>,
;;          Greg McGary <gkm@magilla.cichlid.com>,
;;          Bruno Haible <bruno@clisp.org>.
;; Keywords: i18n, files

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

;; This package makes sure visiting PO files decodes them correctly,
;; according to the Charset= header in the PO file.  For more support
;; for editing PO files, see po-mode.el.

;;; Code:

(defconst po-content-type-charset-alist
  '(("ASCII" . undecided)
    ("ANSI_X3.4-1968" . undecided)
    ("US-ASCII" . undecided))
  "Alist of coding system versus GNU libc/libiconv canonical charset name.
Contains canonical charset names that don't correspond to coding systems.")

(defun po-find-charset (filename)
  "Return PO charset value for FILENAME.
If FILENAME is a cons cell, its CDR is a buffer that already contains
the PO file (but not yet decoded)."
  (let ((charset-regexp
	 "^\"Content-Type:[ \t]*text/plain;[ \t]*charset=\\(.*\\)\\\\n\"")
	(buf (and (consp filename) (cdr filename)))
	(short-read nil))
    (when buf
      (set-buffer buf)
      (goto-char (point-min)))
    ;; Try the first 4096 bytes.  In case we cannot find the charset value
    ;; within the first 4096 bytes (the PO file might start with a long
    ;; comment) try the next 4096 bytes repeatedly until we'll know for sure
    ;; we've checked the empty header entry entirely.
    (while (not (or short-read (re-search-forward "^msgid" nil t) buf))
      (save-excursion
        (goto-char (point-max))
	(let ((pair (insert-file-contents-literally filename nil
						    (1- (point))
						    (1- (+ (point) 4096)))))
	  (setq short-read (< (nth 1 pair) 4096)))))
    (cond ((re-search-forward charset-regexp nil t) (match-string 1))
	  ((or short-read buf) nil)
	  ;; We've found the first msgid; maybe, only a part of the msgstr
	  ;; value was loaded.  Load the next 1024 bytes; if charset still
	  ;; isn't available, give up.
	  (t (save-excursion
	       (goto-char (point-max))
	       (insert-file-contents-literally filename nil
					       (1- (point))
					       (1- (+ (point) 1024))))
	     (if (re-search-forward charset-regexp nil t)
		 (match-string 1))))))

(defun po-find-file-coding-system-guts (operation filename)
  "Return a (DECODING . ENCODING) pair for OPERATION on PO file FILENAME.
Do so according to FILENAME's declared charset.
FILENAME may be a cons (NAME . BUFFER).  In that case, detect charset
in BUFFER."
  (and
   (eq operation 'insert-file-contents)
   (or (if (consp filename) (buffer-live-p (cdr filename)))
       (file-exists-p filename))
   (with-temp-buffer
     (let* ((coding-system-for-read 'no-conversion)
	    (charset (or (po-find-charset filename) "ascii"))
	    assoc)
       (list (cond
	      ((setq assoc
		     (assoc-string charset
                                   po-content-type-charset-alist
                                   t))
	       (cdr assoc))
	      ((or (setq assoc (assoc-string charset coding-system-alist t))
		   (setq assoc
			 (assoc-string (subst-char-in-string ?_ ?-
                                                             charset)
                                       coding-system-alist t)))
	       (intern (car assoc)))
	      ;; In principle we should also check the `mime-charset'
	      ;; property of everything in the base coding system
	      ;; list, but there should always be a coding system
	      ;; corresponding to the MIME name.
	      ((featurep 'code-pages)
	       ;; Give up.
	       'raw-text)
	      (t
	       ;; Try again with code-pages loaded.  Maybe it's best
	       ;; to require it initially?
	       (require 'code-pages nil t)
	       (if (or
		    (setq assoc (assoc-string charset coding-system-alist t))
		    (setq assoc (assoc-string (subst-char-in-string
                                               ?_ ?- charset)
                                              coding-system-alist t)))
		   (intern (car assoc))
		 'raw-text))))))))

;;;###autoload
(defun po-find-file-coding-system (arg-list)
  "Return a (DECODING . ENCODING) pair, according to PO file's charset.
Called through `file-coding-system-alist', before the file is visited for real."
  (po-find-file-coding-system-guts (car arg-list) (car (cdr arg-list))))
;; This is for XEmacs.
;(defun po-find-file-coding-system (operation filename)
;  "\
;Return a Mule (DECODING . ENCODING) pair, according to PO file charset.
;Called through file-coding-system-alist, before the file is visited for real."
;  (po-find-file-coding-system-guts operation filename))

(provide 'po)

;;; po.el ends here
