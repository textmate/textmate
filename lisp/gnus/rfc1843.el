;;; rfc1843.el --- HZ (rfc1843) decoding

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: news HZ HZ+ mail i18n

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

;; Usage:
;; (require 'rfc1843)
;; (rfc1843-gnus-setup)
;;
;; Test:
;; (rfc1843-decode-string  "~{<:Ky2;S{#,NpJ)l6HK!#~}")

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))
(require 'mm-util)

(defvar gnus-decode-encoded-word-function)
(defvar gnus-decode-header-function)
(defvar gnus-newsgroup-name)

(defvar rfc1843-word-regexp
  "~\\({\\([\041-\167][\041-\176]\\| \\)+\\)\\(~}\\|$\\)")

(defvar rfc1843-word-regexp-strictly
  "~\\({\\([\041-\167][\041-\176]\\)+\\)\\(~}\\|$\\)")

(defvar rfc1843-hzp-word-regexp
  "~\\({\\([\041-\167][\041-\176]\\| \\)+\\|\
\[<>]\\([\041-\175][\041-\176]\\| \\)+\\)\\(~}\\|$\\)")

(defvar rfc1843-hzp-word-regexp-strictly
  "~\\({\\([\041-\167][\041-\176]\\)+\\|\
\[<>]\\([\041-\175][\041-\176]\\)+\\)\\(~}\\|$\\)")

(defcustom rfc1843-decode-loosely nil
  "Loosely check HZ encoding if non-nil.
When it is set non-nil, only buffers or strings with strictly
HZ-encoded are decoded."
  :type 'boolean
  :group 'mime)

(defcustom rfc1843-decode-hzp t
  "HZ+ decoding support if non-nil.
HZ+ specification (also known as HZP) is to provide a standardized
7-bit representation of mixed Big5, GB, and ASCII text for convenient
e-mail transmission, news posting, etc.
The document of HZ+ 0.78 specification can be found at
ftp://ftp.math.psu.edu/pub/simpson/chinese/hzp/hzp.doc"
  :type 'boolean
  :group 'mime)

(defcustom rfc1843-newsgroups-regexp "chinese\\|hz"
  "Regexp of newsgroups in which might be HZ encoded."
  :type 'string
  :group 'mime)

(defun rfc1843-decode-region (from to)
  "Decode HZ in the region between FROM and TO."
  (interactive "r")
  (let (str firstc)
    (save-excursion
      (goto-char from)
      (if (or rfc1843-decode-loosely
	      (re-search-forward (if rfc1843-decode-hzp
				     rfc1843-hzp-word-regexp-strictly
				   rfc1843-word-regexp-strictly) to t))
	  (save-restriction
	    (narrow-to-region from to)
	    (goto-char (point-min))
	    (while (re-search-forward (if rfc1843-decode-hzp
					  rfc1843-hzp-word-regexp
					rfc1843-word-regexp) (point-max) t)
	      ;;; Text with extents may cause XEmacs crash
	      (setq str (buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1)))
	      (setq firstc (aref str 0))
	      (insert (mm-decode-coding-string
		       (rfc1843-decode
			(prog1
			    (substring str 1)
			  (delete-region (match-beginning 0) (match-end 0)))
			firstc)
		       (if (eq firstc ?{) 'cn-gb-2312 'cn-big5))))
	    (goto-char (point-min))
	    (while (search-forward "~" (point-max) t)
	      (cond ((eq (char-after) ?\n)
		     (delete-char -1)
		     (delete-char 1))
		    ((eq (char-after) ?~)
		     (delete-char 1)))))))))

(defun rfc1843-decode-string (string)
  "Decode HZ STRING and return the results."
  (let ((m (mm-multibyte-p)))
    (with-temp-buffer
      (when m
	(mm-enable-multibyte))
      (insert string)
      (inline
	(rfc1843-decode-region (point-min) (point-max)))
      (buffer-string))))

(defun rfc1843-decode (word &optional firstc)
  "Decode HZ WORD and return it."
  (let ((i -1) (s (substring word 0)) v)
    (if (or (not firstc) (eq firstc ?{))
	(while (< (incf i) (length s))
	  (if (eq (setq v (aref s i)) ? ) nil
	    (aset s i (+ 128 v))))
      (while (< (incf i) (length s))
	(if (eq (setq v (aref s i)) ? ) nil
	  (setq v (+ (* 94 v) (aref s (1+ i)) -3135))
	  (aset s i (+ (/ v 157) (if (eq firstc ?<) 201 161)))
	  (setq v (% v 157))
	  (aset s (incf i) (+ v (if (< v 63) 64 98))))))
    s))

(autoload 'mail-header-parse-content-type "mail-parse")
(autoload 'message-narrow-to-head "message")
(declare-function message-fetch-field "message" (header &optional not-all))

(defun rfc1843-decode-article-body ()
  "Decode HZ encoded text in the article body."
  (if (string-match (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		    (or gnus-newsgroup-name ""))
      (save-excursion
	(save-restriction
	  (message-narrow-to-head)
	  (let* ((inhibit-point-motion-hooks t)
		 (case-fold-search t)
		 (ct (message-fetch-field "Content-Type" t))
		 (ctl (and ct (mail-header-parse-content-type ct))))
	    (if (and ctl (not (string-match "/" (car ctl))))
		(setq ctl nil))
	    (goto-char (point-max))
	    (widen)
	    (forward-line 1)
	    (narrow-to-region (point) (point-max))
	    (when (or (not ctl)
		      (equal (car ctl) "text/plain"))
	      (rfc1843-decode-region (point) (point-max))))))))

(defvar gnus-decode-header-methods)
(defvar gnus-decode-encoded-word-methods)

(defun rfc1843-gnus-setup ()
  "Setup HZ decoding for Gnus."
  (require 'gnus-art)
  (require 'gnus-sum)
  (add-hook 'gnus-article-decode-hook 'rfc1843-decode-article-body t)
  (setq gnus-decode-encoded-word-function
	'gnus-multi-decode-encoded-word-string
	gnus-decode-header-function
	'gnus-multi-decode-header
	gnus-decode-encoded-word-methods
	(nconc gnus-decode-encoded-word-methods
	       (list
		(cons (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		      'rfc1843-decode-string)))
	gnus-decode-header-methods
	(nconc gnus-decode-header-methods
	       (list
		(cons (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		      'rfc1843-decode-region)))))

(provide 'rfc1843)

;;; rfc1843.el ends here
