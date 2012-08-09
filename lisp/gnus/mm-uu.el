;;; mm-uu.el --- Return uu stuff as mm handles

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: postscript uudecode binhex shar forward gnatsweb pgp

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
(require 'mail-parse)
(require 'nnheader)
(require 'mm-decode)
(require 'mailcap)
(require 'mml2015)

(autoload 'uudecode-decode-region "uudecode")
(autoload 'uudecode-decode-region-external "uudecode")
(autoload 'uudecode-decode-region-internal "uudecode")

(autoload 'binhex-decode-region "binhex")
(autoload 'binhex-decode-region-external "binhex")
(autoload 'binhex-decode-region-internal "binhex")

(autoload 'yenc-decode-region "yenc")
(autoload 'yenc-extract-filename "yenc")

(defcustom mm-uu-decode-function 'uudecode-decode-region
  "*Function to uudecode.
Internal function is done in Lisp by default, therefore decoding may
appear to be horribly slow.  You can make Gnus use an external
decoder, such as uudecode."
  :type '(choice
	  (function-item :tag "Auto detect" uudecode-decode-region)
	  (function-item :tag "Internal" uudecode-decode-region-internal)
	  (function-item :tag "External" uudecode-decode-region-external))
  :group 'gnus-article-mime)

(defcustom mm-uu-binhex-decode-function 'binhex-decode-region
  "*Function to binhex decode.
Internal function is done in elisp by default, therefore decoding may
appear to be horribly slow . You can make Gnus use the external Unix
decoder, such as hexbin."
  :type '(choice (function-item :tag "Auto detect" binhex-decode-region)
		 (function-item :tag "Internal" binhex-decode-region-internal)
		 (function-item :tag "External" binhex-decode-region-external))
  :group 'gnus-article-mime)

(defvar mm-uu-yenc-decode-function 'yenc-decode-region)

(defvar mm-uu-beginning-regexp nil)

(defvar mm-dissect-disposition "inline"
  "The default disposition of uu parts.
This can be either \"inline\" or \"attachment\".")

(defcustom mm-uu-emacs-sources-regexp "\\.emacs\\.sources"
  "The regexp of Emacs sources groups."
  :version "22.1"
  :type 'regexp
  :group 'gnus-article-mime)

(defcustom mm-uu-diff-groups-regexp
  "\\(gmane\\|gnu\\)\\..*\\(diff\\|commit\\|cvs\\|bug\\|devel\\)"
  "Regexp matching diff groups."
  :version "22.1"
  :type 'regexp
  :group 'gnus-article-mime)

(defcustom mm-uu-tex-groups-regexp "\\.tex\\>"
  "*Regexp matching TeX groups."
  :version "23.1"
  :type 'regexp
  :group 'gnus-article-mime)

(defvar mm-uu-type-alist
  '((postscript
     "^%!PS-"
     "^%%EOF$"
     mm-uu-postscript-extract
     nil)
    (uu ;; Maybe we should have a more strict test here.
     "^begin[ \t]+0?[0-7][0-7][0-7][ \t]+"
     "^end[ \t]*$"
     mm-uu-uu-extract
     mm-uu-uu-filename)
    (binhex
     "^:.\\{63,63\\}$"
     ":$"
     mm-uu-binhex-extract
     nil
     mm-uu-binhex-filename)
    (yenc
     "^=ybegin.*size=[0-9]+.*name=.*$"
     "^=yend.*size=[0-9]+"
     mm-uu-yenc-extract
     mm-uu-yenc-filename)
    (shar
     "^#! */bin/sh"
     "^exit 0$"
     mm-uu-shar-extract)
    (forward
     ;; Thanks to Edward J. Sabol <sabol@alderaan.gsfc.nasa.gov> and
     ;; Peter von der Ah\'e <pahe@daimi.au.dk>
     "^-+ \\(Start of \\)?Forwarded message"
     "^-+ End \\(of \\)?forwarded message"
     mm-uu-forward-extract
     nil
     mm-uu-forward-test)
    (gnatsweb
     "^----gnatsweb-attachment----"
     nil
     mm-uu-gnatsweb-extract)
    (pgp-signed
     "^-----BEGIN PGP SIGNED MESSAGE-----"
     "^-----END PGP SIGNATURE-----"
     mm-uu-pgp-signed-extract
     nil
     nil)
    (pgp-encrypted
     "^-----BEGIN PGP MESSAGE-----"
     "^-----END PGP MESSAGE-----"
     mm-uu-pgp-encrypted-extract
     nil
     nil)
    (pgp-key
     "^-----BEGIN PGP PUBLIC KEY BLOCK-----"
     "^-----END PGP PUBLIC KEY BLOCK-----"
     mm-uu-pgp-key-extract
     mm-uu-gpg-key-skip-to-last
     nil)
    (emacs-sources
     "^;;;?[ \t]*[^ \t]+\\.el[ \t]*--"
     "^;;;?[ \t]*\\([^ \t]+\\.el\\)[ \t]+ends here"
     mm-uu-emacs-sources-extract
     nil
     mm-uu-emacs-sources-test)
    (diff
     "^Index: "
     nil
     mm-uu-diff-extract
     nil
     mm-uu-diff-test)
    (diff
     "^=== modified file "
     nil
     mm-uu-diff-extract
     nil
     mm-uu-diff-test)
    (git-format-patch
     "^diff --git "
     "^-- "
     mm-uu-diff-extract
     nil
     mm-uu-diff-test)
    (message-marks
     ;; Text enclosed with tags similar to `message-mark-insert-begin' and
     ;; `message-mark-insert-end'.  Don't use those variables to avoid
     ;; dependency on `message.el'.
     "^-+[8<>]*-\\{9,\\}[a-z ]+-\\{9,\\}[a-z ]+-\\{9,\\}[8<>]*-+$"
     "^-+[8<>]*-\\{9,\\}[a-z ]+-\\{9,\\}[a-z ]+-\\{9,\\}[8<>]*-+$"
     (lambda () (mm-uu-verbatim-marks-extract 0 0 1 -1))
     nil)
    ;; Omitting [a-z8<] leads to false positives (bogus signature separators
    ;; and mailing list banners).
    (insert-marks
     "^ *\\(-\\|_\\)\\{30,\\}.*[a-z8<].*\\(-\\|_\\)\\{30,\\} *$"
     "^ *\\(-\\|_\\)\\{30,\\}.*[a-z8<].*\\(-\\|_\\)\\{30,\\} *$"
     (lambda () (mm-uu-verbatim-marks-extract 0 0 1 -1))
     nil)
    (verbatim-marks
     ;; slrn-style verbatim marks, see
     ;; http://slrn.sourceforge.net/docs/slrn-manual-6.html#process_verbatim_marks
     "^#v\\+"
     "^#v\\-$"
     (lambda () (mm-uu-verbatim-marks-extract 0 0))
     nil)
    (LaTeX
     "^\\([\\\\%][^\n]+\n\\)*\\\\documentclass.*[[{%]"
     "^\\\\end{document}"
     mm-uu-latex-extract
     nil
     mm-uu-latex-test)
    (org-src-code-block
     "^[ \t]*#\\+begin_"
     "^[ \t]*#\\+end_"
     mm-uu-org-src-code-block-extract)
    (org-meta-line
     "^[ \t]*#\\+[[:alpha:]]+: "
     "$"
     mm-uu-org-src-code-block-extract))
  "A list of specifications for non-MIME attachments.
Each element consist of the following entries: label,
start-regexp, end-regexp, extract-function, test-function.

After modifying this list you must run \\[mm-uu-configure].

You can disable elements from this list by customizing
`mm-uu-configure-list'.")

(defcustom mm-uu-configure-list '((shar . disabled))
  "A list of mm-uu configuration.
To disable dissecting shar codes, for instance, add
`(shar . disabled)' to this list."
  :type 'alist
  :options (mapcar (lambda (entry)
		     (list (car entry) '(const disabled)))
		   mm-uu-type-alist)
  :group 'gnus-article-mime)

(defvar mm-uu-text-plain-type '("text/plain" (charset . gnus-decoded))
  "MIME type and parameters for text/plain parts.
`gnus-decoded' is a fake charset, which means no further decoding.")

;; functions

(defsubst mm-uu-type (entry)
  (car entry))

(defsubst mm-uu-beginning-regexp (entry)
  (nth 1 entry))

(defsubst mm-uu-end-regexp (entry)
  (nth 2 entry))

(defsubst mm-uu-function-extract (entry)
  (nth 3 entry))

(defsubst mm-uu-function-1 (entry)
  (nth 4 entry))

(defsubst mm-uu-function-2 (entry)
  (nth 5 entry))

;; In Emacs 22, we could use `min-colors' in the face definition.  But Emacs
;; 21 and XEmacs don't support it.
(defcustom mm-uu-hide-markers
  (< 16 (or (and (fboundp 'defined-colors)
		 (length (defined-colors)))
	    (and (fboundp 'device-color-cells)
		 (device-color-cells))
	    0))
  "If non-nil, hide verbatim markers.
The value should be nil on displays where the face
`mm-uu-extract' isn't distinguishable to the face `default'."
  :type '(choice (const :tag "Hide" t)
		 (const :tag "Don't hide" nil))
  :version "23.1" ;; No Gnus
  :group 'gnus-article-mime)

(defface mm-uu-extract '(;; Inspired by `gnus-cite-3'
			 (((type tty)
			   (class color)
			   (background dark))
			  (:background "dark blue"))
			 (((class color)
			   (background dark))
			  (:foreground "light yellow"
			   :background "dark green"))
			 (((type tty)
			   (class color)
			   (background light))
			  (:foreground "dark blue"))
			 (((class color)
			   (background light))
			  (:foreground "dark green"
			   :background "light yellow"))
			 (t
			  ()))
  "Face for extracted buffers."
  ;; See `mm-uu-verbatim-marks-extract'.
  :version "23.1" ;; No Gnus
  :group 'gnus-article-mime)

(defun mm-uu-copy-to-buffer (&optional from to properties)
  "Copy the contents of the current buffer to a fresh buffer.
Return that buffer.

If PROPERTIES is non-nil, PROPERTIES are applied to the buffer,
see `set-text-properties'.  If PROPERTIES equals t, this means to
apply the face `mm-uu-extract'."
  (let ((obuf (current-buffer))
        (multi (and (boundp 'enable-multibyte-characters)
                    enable-multibyte-characters))
	(coding-system
         ;; Might not exist in non-MULE XEmacs
         (when (boundp 'buffer-file-coding-system)
           buffer-file-coding-system)))
    (with-current-buffer (generate-new-buffer " *mm-uu*")
      (if multi (mm-enable-multibyte) (mm-disable-multibyte))
      (setq buffer-file-coding-system coding-system)
      (insert-buffer-substring obuf from to)
      (cond ((eq properties  t)
	     (set-text-properties (point-min) (point-max)
				  '(face mm-uu-extract)))
	    (properties
	     (set-text-properties (point-min) (point-max) properties)))
      (current-buffer))))

(defun mm-uu-configure-p  (key val)
  (member (cons key val) mm-uu-configure-list))

(defun mm-uu-configure (&optional symbol value)
  "Configure detection of non-MIME attachments."
  (interactive)
  (if symbol (set-default symbol value))
  (setq mm-uu-beginning-regexp nil)
  (mapcar (lambda (entry)
	     (if (mm-uu-configure-p (mm-uu-type entry) 'disabled)
		 nil
	       (setq mm-uu-beginning-regexp
		     (concat mm-uu-beginning-regexp
			     (if mm-uu-beginning-regexp "\\|")
			     (mm-uu-beginning-regexp entry)))))
	  mm-uu-type-alist))

(mm-uu-configure)

(defvar file-name)
(defvar start-point)
(defvar end-point)
(defvar entry)

(defun mm-uu-uu-filename ()
  (if (looking-at ".+")
      (setq file-name
	    (let ((nnheader-file-name-translation-alist
		   '((?/ . ?,) (?\  . ?_) (?* . ?_) (?$ . ?_))))
	      (nnheader-translate-file-chars (match-string 0))))))

(defun mm-uu-binhex-filename ()
  (setq file-name
	(ignore-errors
	  (binhex-decode-region start-point end-point t))))

(defun mm-uu-yenc-filename ()
  (goto-char start-point)
  (setq file-name
	(ignore-errors
	  (yenc-extract-filename))))

(defun mm-uu-forward-test ()
  (save-excursion
    (goto-char start-point)
    (forward-line)
    (looking-at "[\r\n]*[a-zA-Z][a-zA-Z0-9-]*:")))

(defun mm-uu-postscript-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/postscript")))

(defun mm-uu-verbatim-marks-extract (start-offset end-offset
						  &optional
						  start-hide
						  end-hide)
  (let ((start (or (and mm-uu-hide-markers
			start-hide)
		   start-offset
		   1))
	(end   (or (and mm-uu-hide-markers
			end-hide)
		   end-offset
		   -1)))
    (mm-make-handle
     (mm-uu-copy-to-buffer
      (progn (goto-char start-point)
	     (forward-line start)
	     (point))
      (progn (goto-char end-point)
	   (forward-line end)
	   (point))
      t)
     '("text/x-verbatim" (charset . gnus-decoded)))))

(defun mm-uu-latex-extract ()
  (mm-make-handle
   (mm-uu-copy-to-buffer start-point end-point t)
   ;; application/x-tex?
   '("text/x-verbatim" (charset . gnus-decoded))))

(defun mm-uu-emacs-sources-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/emacs-lisp" (charset . gnus-decoded))
		  nil nil
		  (list mm-dissect-disposition
			(cons 'filename file-name))))

(defun mm-uu-org-src-code-block-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
                  '("text/x-org")))

(defvar gnus-newsgroup-name)

(defun mm-uu-emacs-sources-test ()
  (setq file-name (match-string 1))
  (and gnus-newsgroup-name
       mm-uu-emacs-sources-regexp
       (string-match mm-uu-emacs-sources-regexp gnus-newsgroup-name)))

(defun mm-uu-diff-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("text/x-patch" (charset . gnus-decoded))))

(defun mm-uu-diff-test ()
  (and gnus-newsgroup-name
       mm-uu-diff-groups-regexp
       (string-match mm-uu-diff-groups-regexp gnus-newsgroup-name)))

(defun mm-uu-latex-test ()
  (and gnus-newsgroup-name
       mm-uu-tex-groups-regexp
       (string-match mm-uu-tex-groups-regexp gnus-newsgroup-name)))

(defun mm-uu-forward-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer
		   (progn
		     (goto-char start-point)
		     (forward-line)
		     (skip-chars-forward "\n")
		     (point))
		   (progn (goto-char end-point) (forward-line -1) (point)))
		  '("message/rfc822" (charset . gnus-decoded))))

(defun mm-uu-uu-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  (list (or (and file-name
				 (string-match "\\.[^\\.]+$"
					       file-name)
				 (mailcap-extension-to-mime
				  (match-string 0 file-name)))
			    "application/octet-stream"))
		  'x-uuencode nil
		  (if (and file-name (not (equal file-name "")))
		      (list mm-dissect-disposition
			    (cons 'filename file-name)))))

(defun mm-uu-binhex-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  (list (or (and file-name
				 (string-match "\\.[^\\.]+$" file-name)
				 (mailcap-extension-to-mime
				  (match-string 0 file-name)))
			    "application/octet-stream"))
		  'x-binhex nil
		  (if (and file-name (not (equal file-name "")))
		      (list mm-dissect-disposition
			    (cons 'filename file-name)))))

(defvar gnus-original-article-buffer)   ; gnus.el

(defun mm-uu-yenc-extract ()
  ;; This might not be exactly correct, but we sure can't get the
  ;; binary data from the article buffer, since that's already in a
  ;; non-binary charset.  So get it from the original article buffer.
  (mm-make-handle (with-current-buffer gnus-original-article-buffer
		    (mm-uu-copy-to-buffer start-point end-point))
		  (list (or (and file-name
				 (string-match "\\.[^\\.]+$" file-name)
				 (mailcap-extension-to-mime
				  (match-string 0 file-name)))
			    "application/octet-stream"))
		  'x-yenc nil
		  (if (and file-name (not (equal file-name "")))
		      (list mm-dissect-disposition
			    (cons 'filename file-name)))))


(defun mm-uu-shar-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/x-shar")))

(defun mm-uu-gnatsweb-extract ()
  (save-restriction
    (goto-char start-point)
    (forward-line)
    (narrow-to-region (point) end-point)
    (mm-dissect-buffer t)))

(defun mm-uu-pgp-signed-test (&rest rest)
  (and
   mml2015-use
   (mml2015-clear-verify-function)
   (cond
    ((eq mm-verify-option 'never) nil)
    ((eq mm-verify-option 'always) t)
    ((eq mm-verify-option 'known) t)
    (t (prog1
	   (y-or-n-p "Verify pgp signed part? ")
	 (message ""))))))

(defvar gnus-newsgroup-charset)

(defun mm-uu-pgp-signed-extract-1 (handles ctl)
  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max))))
    (with-current-buffer buf
      (if (mm-uu-pgp-signed-test)
	  (progn
	    (mml2015-clean-buffer)
	    (let ((coding-system-for-write (or buffer-file-coding-system
					       gnus-newsgroup-charset
					       'iso-8859-1))
		  (coding-system-for-read (or buffer-file-coding-system
					      gnus-newsgroup-charset
					      'iso-8859-1)))
	      (funcall (mml2015-clear-verify-function))))
	(when (and mml2015-use (null (mml2015-clear-verify-function)))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-details
	   (format "Clear verification not supported by `%s'.\n" mml2015-use)))
	(mml2015-extract-cleartext-signature))
      (list (mm-make-handle buf mm-uu-text-plain-type)))))

(defun mm-uu-pgp-signed-extract ()
  (let ((mm-security-handle (list (format "multipart/signed"))))
    (mm-set-handle-multipart-parameter
     mm-security-handle 'protocol "application/x-gnus-pgp-signature")
    (save-restriction
      (narrow-to-region start-point end-point)
      (add-text-properties 0 (length (car mm-security-handle))
			   (list 'buffer (mm-uu-copy-to-buffer))
			   (car mm-security-handle))
      (setcdr mm-security-handle
	      (mm-uu-pgp-signed-extract-1 nil
					  mm-security-handle)))
    mm-security-handle))

(defun mm-uu-pgp-encrypted-test (&rest rest)
  (and
   mml2015-use
   (mml2015-clear-decrypt-function)
   (cond
    ((eq mm-decrypt-option 'never) nil)
    ((eq mm-decrypt-option 'always) t)
    ((eq mm-decrypt-option 'known) t)
    (t (prog1
	   (y-or-n-p "Decrypt pgp encrypted part? ")
	 (message ""))))))

(defun mm-uu-pgp-encrypted-extract-1 (handles ctl)
  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max)))
	(first t)
	charset)
    ;; Make sure there's a blank line between header and body.
    (with-current-buffer buf
      (goto-char (point-min))
      (while (prog2
		 (forward-line 1)
		 (if first
		     (looking-at "[^\t\n ]+:")
		   (looking-at "[^\t\n ]+:\\|[\t ]"))
	       (setq first nil)))
      (unless (memq (char-after) '(?\n nil))
	(insert "\n"))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq charset (mail-fetch-field "charset")))
      (if (and (mm-uu-pgp-encrypted-test)
	       (progn
		 (mml2015-clean-buffer)
		 (funcall (mml2015-clear-decrypt-function))
		 (equal (mm-handle-multipart-ctl-parameter mm-security-handle
							   'gnus-info)
			"OK")))
	  (progn
	    ;; Decode charset.
	    (if (and (or charset
			 (setq charset gnus-newsgroup-charset))
		     (setq charset (mm-charset-to-coding-system charset))
		     (not (eq charset 'ascii)))
		;; Assume that buffer's multibyteness is turned off.
		;; See `mml2015-pgg-clear-decrypt'.
		(insert (mm-decode-coding-string (prog1
						     (buffer-string)
						   (erase-buffer)
						   (mm-enable-multibyte))
						 charset))
	      (mm-enable-multibyte))
	    (list (mm-make-handle buf mm-uu-text-plain-type)))
	(list (mm-make-handle buf '("application/pgp-encrypted")))))))

(defun mm-uu-pgp-encrypted-extract ()
  (let ((mm-security-handle (list (format "multipart/encrypted"))))
    (mm-set-handle-multipart-parameter
     mm-security-handle 'protocol "application/x-gnus-pgp-encrypted")
    (save-restriction
      (narrow-to-region start-point end-point)
      (add-text-properties 0 (length (car mm-security-handle))
			   (list 'buffer (mm-uu-copy-to-buffer))
			   (car mm-security-handle))
      (setcdr mm-security-handle
	      (mm-uu-pgp-encrypted-extract-1 nil
					     mm-security-handle)))
    mm-security-handle))

(defun mm-uu-gpg-key-skip-to-last ()
  (let ((point (point))
	(end-regexp (mm-uu-end-regexp entry))
	(beginning-regexp (mm-uu-beginning-regexp entry)))
    (when (and end-regexp
	       (not (mm-uu-configure-p (mm-uu-type entry) 'disabled)))
      (while (re-search-forward end-regexp nil t)
	(skip-chars-forward " \t\n\r")
	(if (looking-at beginning-regexp)
	    (setq point (match-end 0)))))
    (goto-char point)))

(defun mm-uu-pgp-key-extract ()
  (let ((buf (mm-uu-copy-to-buffer start-point end-point)))
    (mm-make-handle buf
		    '("application/pgp-keys"))))

;;;###autoload
(defun mm-uu-dissect (&optional noheader mime-type)
  "Dissect the current buffer and return a list of uu handles.
The optional NOHEADER means there's no header in the buffer.
MIME-TYPE specifies a MIME type and parameters, which defaults to the
value of `mm-uu-text-plain-type'."
  (let ((case-fold-search t)
	(mm-uu-text-plain-type (or mime-type mm-uu-text-plain-type))
	text-start start-point end-point file-name result entry func)
    (save-excursion
      (goto-char (point-min))
      (cond
       (noheader)
       ((looking-at "\n")
	(forward-line))
       ((search-forward "\n\n" nil t)
	t)
       (t (goto-char (point-max))))
      (setq text-start (point))
      (while (re-search-forward mm-uu-beginning-regexp nil t)
	(setq start-point (match-beginning 0)
	      entry nil)
	(let ((alist mm-uu-type-alist)
	      (beginning-regexp (match-string 0)))
	  (while (not entry)
	    (if (string-match (mm-uu-beginning-regexp (car alist))
			      beginning-regexp)
		(setq entry (car alist))
	      (pop alist))))
	(if (setq func (mm-uu-function-1 entry))
	    (funcall func))
	(forward-line);; in case of failure
	(when (and (not (mm-uu-configure-p (mm-uu-type entry) 'disabled))
		   (let ((end-regexp (mm-uu-end-regexp entry)))
		     (if (not end-regexp)
			 (or (setq end-point (point-max)) t)
		       (prog1
			   (re-search-forward end-regexp nil t)
			 (forward-line)
			 (setq end-point (point)))))
		   (or (not (setq func (mm-uu-function-2 entry)))
		       (funcall func)))
	  (if (and (> start-point text-start)
		   (progn
		     (goto-char text-start)
		     (re-search-forward "." start-point t)))
	      (push
	       (mm-make-handle (mm-uu-copy-to-buffer text-start start-point)
			       mm-uu-text-plain-type)
	       result))
	  (push
	   (funcall (mm-uu-function-extract entry))
	   result)
	  (goto-char (setq text-start end-point))))
      (when result
	(if (and (> (point-max) (1+ text-start))
		 (save-excursion
		   (goto-char text-start)
		   (re-search-forward "." nil t)))
	    (push
	     (mm-make-handle (mm-uu-copy-to-buffer text-start (point-max))
			     mm-uu-text-plain-type)
	     result))
	(setq result (cons "multipart/mixed" (nreverse result))))
      result)))

;;;###autoload
(defun mm-uu-dissect-text-parts (handle &optional decoded)
  "Dissect text parts and put uu handles into HANDLE.
Assume text has been decoded if DECODED is non-nil."
  (let ((buffer (mm-handle-buffer handle)))
    (cond ((stringp buffer)
	   (dolist (elem (cdr handle))
	     (mm-uu-dissect-text-parts elem decoded)))
	  ((bufferp buffer)
	   (let ((type (mm-handle-media-type handle))
		 (case-fold-search t) ;; string-match
		 children charset encoding)
	     (when (and
		    (stringp type)
		    ;; Mutt still uses application/pgp even though
		    ;; it has already been withdrawn.
		    (string-match "\\`text/\\|\\`application/pgp\\'" type)
                    (equal (car (mm-handle-disposition handle))
                           "inline")
		    (setq
		     children
		     (with-current-buffer buffer
		       (cond
			((or decoded
			     (eq (setq charset (mail-content-type-get
						(mm-handle-type handle)
						'charset))
				 'gnus-decoded))
			 (setq decoded t)
			 (mm-uu-dissect
			  t (cons type '((charset . gnus-decoded)))))
			(charset
			 (setq decoded t)
			 (mm-with-multibyte-buffer
			   (insert (mm-decode-string (mm-get-part handle)
						     charset))
			   (mm-uu-dissect
			    t (cons type '((charset . gnus-decoded))))))
			((setq encoding (mm-handle-encoding handle))
			 (setq decoded nil)
			 ;; Inherit the multibyteness of the `buffer'.
			 (with-temp-buffer
			   (insert-buffer-substring buffer)
			   (mm-decode-content-transfer-encoding
			    encoding type)
			   (mm-uu-dissect t (list type))))
			(t
			 (setq decoded nil)
			 (mm-uu-dissect t (list type)))))))
	       ;; Ignore it if a given part is dissected into a single
	       ;; part of which the type is the same as the given one.
	       (if (and (<= (length children) 2)
			(string-equal (mm-handle-media-type (cadr children))
				      type))
		   (kill-buffer (mm-handle-buffer (cadr children)))
		 (kill-buffer buffer)
		 (setcdr handle (cdr children))
		 (setcar handle (car children)) ;; "multipart/mixed"
		 (dolist (elem (cdr children))
		   (mm-uu-dissect-text-parts elem decoded))))))
	  (t
	   (dolist (elem handle)
	     (mm-uu-dissect-text-parts elem decoded))))))

(provide 'mm-uu)

;;; mm-uu.el ends here
