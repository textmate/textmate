;;; mailcap.el --- MIME media types configuration

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: William M. Perry <wmperry@aventail.com>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail, multimedia

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

;; Provides configuration of MIME media types from directly from Lisp
;; and via the usual mailcap mechanism (RFC 1524).  Deals with
;; mime.types similarly.

;;; Code:

(eval-when-compile (require 'cl))
(autoload 'mail-header-parse-content-type "mail-parse")

;; `mm-delete-duplicates' is an alias for `delete-dups' in Emacs 22.
(defalias 'mailcap-delete-duplicates
  (if (fboundp 'delete-dups)
      'delete-dups
    (autoload 'mm-delete-duplicates "mm-util")
    'mm-delete-duplicates))

;; `mailcap-replace-in-string' is an alias like `gnus-replace-in-string'.
(eval-and-compile
  (cond
   ((fboundp 'replace-regexp-in-string)
    (defun mailcap-replace-in-string  (string regexp newtext &optional literal)
      "Replace all matches for REGEXP with NEWTEXT in STRING.
If LITERAL is non-nil, insert NEWTEXT literally.  Return a new
string containing the replacements.
This is a compatibility function for different Emacsen."
      (replace-regexp-in-string regexp newtext string nil literal)))
   ((fboundp 'replace-in-string)
    (defalias 'mailcap-replace-in-string 'replace-in-string))))

(defgroup mailcap nil
  "Definition of viewers for MIME types."
  :version "21.1"
  :group 'mime)

(defvar mailcap-parse-args-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?{ "(" table)
    (modify-syntax-entry ?} ")" table)
    table)
  "A syntax table for parsing SGML attributes.")

(eval-and-compile
  (when (featurep 'xemacs)
    (condition-case nil
	(require 'lpr)
      (error nil))))

(defvar mailcap-print-command
  (mapconcat 'identity
	     (cons (if (boundp 'lpr-command)
		       lpr-command
		     "lpr")
		   (when (boundp 'lpr-switches)
		     (if (stringp lpr-switches)
			 (list lpr-switches)
		       lpr-switches)))
	     " ")
  "Shell command (including switches) used to print PostScript files.")

;; Postpone using defcustom for this as it's so big and we essentially
;; have to have two copies of the data around then.  Perhaps just
;; customize the Lisp viewers and rely on the normal configuration
;; files for the rest?  -- fx
(defvar mailcap-mime-data
  `(("application"
     ("vnd\\.ms-excel"
      (viewer . "gnumeric %s")
      (test   . (getenv "DISPLAY"))
      (type . "application/vnd.ms-excel"))
     ("x-x509-ca-cert"
      (viewer . ssl-view-site-cert)
      (test . (fboundp 'ssl-view-site-cert))
      (type . "application/x-x509-ca-cert"))
     ("x-x509-user-cert"
      (viewer . ssl-view-user-cert)
      (test . (fboundp 'ssl-view-user-cert))
      (type . "application/x-x509-user-cert"))
     ("octet-stream"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type . "application/octet-stream"))
     ("dvi"
      (viewer . "xdvi -safer %s")
      (test   . (eq window-system 'x))
      ("needsx11")
      (type   . "application/dvi")
      ("print" . "dvips -qRP %s"))
     ("dvi"
      (viewer . "dvitty %s")
      (test   . (not (getenv "DISPLAY")))
      (type   . "application/dvi")
      ("print" . "dvips -qRP %s"))
     ("emacs-lisp"
      (viewer . mailcap-maybe-eval)
      (type   . "application/emacs-lisp"))
     ("x-emacs-lisp"
      (viewer . mailcap-maybe-eval)
      (type   . "application/x-emacs-lisp"))
     ("x-tar"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type   . "application/x-tar"))
     ("x-latex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/x-latex"))
     ("x-tex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/x-tex"))
     ("latex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/latex"))
     ("tex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/tex"))
     ("texinfo"
      (viewer . texinfo-mode)
      (test   . (fboundp 'texinfo-mode))
      (type   . "application/tex"))
     ("zip"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type   . "application/zip")
      ("copiousoutput"))
     ("pdf"
      (viewer . "gv -safer %s")
      (type . "application/pdf")
      (test . window-system)
      ("print" . ,(concat "pdf2ps %s - | " mailcap-print-command)))
     ("pdf"
      (viewer . "gpdf %s")
      (type . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      (test . (eq window-system 'x)))
     ("pdf"
      (viewer . "xpdf %s")
      (type . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      (test . (eq window-system 'x)))
     ("pdf"
      (viewer . ,(concat "pdftotext %s -"))
      (type   . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      ("copiousoutput"))
     ("postscript"
      (viewer . "gv -safer %s")
      (type . "application/postscript")
      (test   . window-system)
      ("print" . ,(concat mailcap-print-command " %s"))
      ("needsx11"))
     ("postscript"
      (viewer . "ghostview -dSAFER %s")
      (type . "application/postscript")
      (test   . (eq window-system 'x))
      ("print" . ,(concat mailcap-print-command " %s"))
      ("needsx11"))
     ("postscript"
      (viewer . "ps2ascii %s")
      (type . "application/postscript")
      (test . (not (getenv "DISPLAY")))
      ("print" . ,(concat mailcap-print-command " %s"))
      ("copiousoutput"))
     ("sieve"
      (viewer . sieve-mode)
      (test   . (fboundp 'sieve-mode))
      (type   . "application/sieve"))
     ("pgp-keys"
      (viewer . "gpg --import --interactive --verbose")
      (type   . "application/pgp-keys")
      ("needsterminal")))
    ("audio"
     ("x-mpeg"
      (viewer . "maplay %s")
      (type   . "audio/x-mpeg"))
     (".*"
      (viewer . "showaudio")
      (type   . "audio/*")))
    ("message"
     ("rfc-*822"
      (viewer . mm-view-message)
      (test   . (and (featurep 'gnus)
		     (gnus-alive-p)))
      (type   . "message/rfc822"))
     ("rfc-*822"
      (viewer . vm-mode)
      (test   . (fboundp 'vm-mode))
      (type   . "message/rfc822"))
     ("rfc-*822"
      (viewer . w3-mode)
      (test   . (fboundp 'w3-mode))
      (type   . "message/rfc822"))
     ("rfc-*822"
      (viewer . view-mode)
      (type   . "message/rfc822")))
    ("image"
     ("x-xwd"
      (viewer  . "xwud -in %s")
      (type    . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test    . (eq window-system 'x))
      ("needsx11"))
     ("x11-dump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq window-system 'x))
      ("needsx11"))
     ("windowdump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq window-system 'x))
      ("needsx11"))
     (".*"
      (viewer . "display %s")
      (type . "image/*")
      (test   . (eq window-system 'x))
      ("needsx11"))
     (".*"
      (viewer . "ee %s")
      (type . "image/*")
      (test   . (eq window-system 'x))
      ("needsx11")))
    ("text"
     ("plain"
      (viewer  . w3-mode)
      (test    . (fboundp 'w3-mode))
      (type    . "text/plain"))
     ("plain"
      (viewer  . view-mode)
      (test    . (fboundp 'view-mode))
      (type    . "text/plain"))
     ("plain"
      (viewer  . fundamental-mode)
      (type    . "text/plain"))
     ("enriched"
      (viewer . enriched-decode)
      (test   . (fboundp 'enriched-decode))
      (type   . "text/enriched"))
     ("html"
      (viewer . mm-w3-prepare-buffer)
      (test   . (fboundp 'w3-prepare-buffer))
      (type   . "text/html"))
     ("dns"
      (viewer . dns-mode)
      (test   . (fboundp 'dns-mode))
      (type   . "text/dns")))
    ("video"
     ("mpeg"
      (viewer . "mpeg_play %s")
      (type   . "video/mpeg")
      (test   . (eq window-system 'x))
      ("needsx11")))
    ("x-world"
     ("x-vrml"
      (viewer  . "webspace -remote %s -URL %u")
      (type    . "x-world/x-vrml")
      ("description"
       "VRML document")))
    ("archive"
     ("tar"
      (viewer . tar-mode)
      (type . "archive/tar")
      (test . (fboundp 'tar-mode)))))
  "The mailcap structure is an assoc list of assoc lists.
1st assoc list is keyed on the major content-type
2nd assoc list is keyed on the minor content-type (which can be a regexp)

Which looks like:
-----------------
 ((\"application\"
   (\"postscript\" . <info>))
  (\"text\"
   (\"plain\" . <info>)))

Where <info> is another assoc list of the various information
related to the mailcap RFC 1524.  This is keyed on the lowercase
attribute name (viewer, test, etc).  This looks like:
 ((viewer . VIEWERINFO)
  (test   . TESTINFO)
  (xxxx   . \"STRING\")
  FLAG)

Where VIEWERINFO specifies how the content-type is viewed.  Can be
a string, in which case it is run through a shell, with
appropriate parameters, or a symbol, in which case the symbol is
`funcall'ed, with the buffer as an argument.

TESTINFO is a test for the viewer's applicability, or nil.  If nil, it
means the viewer is always valid.  If it is a Lisp function, it is
called with a list of items from any extra fields from the
Content-Type header as argument to return a boolean value for the
validity.  Otherwise, if it is a non-function Lisp symbol or list
whose car is a symbol, it is `eval'led to yield the validity.  If it
is a string or list of strings, it represents a shell command to run
to return a true or false shell value for the validity.")
(put 'mailcap-mime-data 'risky-local-variable t)

(defcustom mailcap-download-directory nil
  "*Directory to which `mailcap-save-binary-file' downloads files by default.
nil means your home directory."
  :type '(choice (const :tag "Home directory" nil)
		 directory)
  :group 'mailcap)

(defvar mailcap-poor-system-types
  '(ms-dos windows-nt)
  "Systems that don't have a Unix-like directory hierarchy.")

;;;
;;; Utility functions
;;;

(defun mailcap-save-binary-file ()
  (goto-char (point-min))
  (unwind-protect
      (let ((file (read-file-name
		   "Filename to save as: "
		   (or mailcap-download-directory "~/")))
	    (require-final-newline nil))
	(write-region (point-min) (point-max) file))
    (kill-buffer (current-buffer))))

(defvar mailcap-maybe-eval-warning
  "*** WARNING ***

This MIME part contains untrusted and possibly harmful content.
If you evaluate the Emacs Lisp code contained in it, a lot of nasty
things can happen.  Please examine the code very carefully before you
instruct Emacs to evaluate it.  You can browse the buffer containing
the code using \\[scroll-other-window].

If you are unsure what to do, please answer \"no\"."
  "Text of warning message displayed by `mailcap-maybe-eval'.
Make sure that this text consists only of few text lines.  Otherwise,
Gnus might fail to display all of it.")

(defun mailcap-maybe-eval ()
  "Maybe evaluate a buffer of Emacs Lisp code."
  (let ((lisp-buffer (current-buffer)))
    (goto-char (point-min))
    (when
	(save-window-excursion
	  (delete-other-windows)
	  (let ((buffer (get-buffer-create (generate-new-buffer-name
					    "*Warning*"))))
	    (unwind-protect
		(with-current-buffer buffer
		  (insert (substitute-command-keys
			   mailcap-maybe-eval-warning))
		  (goto-char (point-min))
		  (display-buffer buffer)
		  (yes-or-no-p "This is potentially dangerous emacs-lisp code, evaluate it? "))
	      (kill-buffer buffer))))
      (eval-buffer (current-buffer)))
    (when (buffer-live-p lisp-buffer)
      (with-current-buffer lisp-buffer
	(emacs-lisp-mode)))))


;;;
;;; The mailcap parser
;;;

(defun mailcap-replace-regexp (regexp to-string)
  ;; Quiet replace-regexp.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defvar mailcap-parsed-p nil)

(defun mailcap-parse-mailcaps (&optional path force)
  "Parse out all the mailcaps specified in a path string PATH.
Components of PATH are separated by the `path-separator' character
appropriate for this system.  If FORCE, re-parse even if already
parsed.  If PATH is omitted, use the value of environment variable
MAILCAPS if set; otherwise (on Unix) use the path from RFC 1524, plus
/usr/local/etc/mailcap."
  (interactive (list nil t))
  (when (or (not mailcap-parsed-p)
	    force)
    (cond
     (path nil)
     ((getenv "MAILCAPS") (setq path (getenv "MAILCAPS")))
     ((memq system-type mailcap-poor-system-types)
      (setq path '("~/.mailcap" "~/mail.cap" "~/etc/mail.cap")))
     (t (setq path
	      ;; This is per RFC 1524, specifically
	      ;; with /usr before /usr/local.
	      '("~/.mailcap" "/etc/mailcap" "/usr/etc/mailcap"
		"/usr/local/etc/mailcap"))))
    (let ((fnames (reverse
		   (if (stringp path)
		       (split-string path path-separator t)
		     path)))
	  fname)
      (while fnames
	(setq fname (car fnames))
	(if (and (file-readable-p fname)
		 (file-regular-p fname))
	    (mailcap-parse-mailcap fname))
	(setq fnames (cdr fnames))))
      (setq mailcap-parsed-p t)))

(defun mailcap-parse-mailcap (fname)
  "Parse out the mailcap file specified by FNAME."
  (let (major				; The major mime type (image/audio/etc)
	minor				; The minor mime type (gif, basic, etc)
	save-pos			; Misc saved positions used in parsing
	viewer				; How to view this mime type
	info				; Misc info about this mime type
	)
    (with-temp-buffer
      (insert-file-contents fname)
      (set-syntax-table mailcap-parse-args-syntax-table)
      (mailcap-replace-regexp "#.*" "")	; Remove all comments
      (mailcap-replace-regexp "\\\\[ \t]*\n" " ") ; And collapse spaces
      (mailcap-replace-regexp "\n+" "\n") ; And blank lines
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (while (not (bobp))
	(skip-chars-backward " \t\n")
	(beginning-of-line)
	(setq save-pos (point)
	      info nil)
	(skip-chars-forward "^/; \t\n")
	(downcase-region save-pos (point))
	(setq major (buffer-substring save-pos (point)))
	(skip-chars-forward " \t")
	(setq minor "")
	(when (eq (char-after) ?/)
	  (forward-char)
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^; \t\n")
	  (downcase-region save-pos (point))
	  (setq minor
		(cond
		 ((eq ?* (or (char-after save-pos) 0)) ".*")
		 ((= (point) save-pos) ".*")
		 (t (regexp-quote (buffer-substring save-pos (point)))))))
	(skip-chars-forward " \t")
	;;; Got the major/minor chunks, now for the viewers/etc
	;;; The first item _must_ be a viewer, according to the
	;;; RFC for mailcap files (#1524)
	(setq viewer "")
	(when (eq (char-after) ?\;)
	  (forward-char)
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^;\n")
	  ;; skip \;
	  (while (eq (char-before) ?\\)
	    (backward-delete-char 1)
	    (forward-char)
	    (skip-chars-forward "^;\n"))
	  (if (eq (or (char-after save-pos) 0) ?')
	      (setq viewer (progn
			     (narrow-to-region (1+ save-pos) (point))
			     (goto-char (point-min))
			     (prog1
				 (read (current-buffer))
			       (goto-char (point-max))
			       (widen))))
	    (setq viewer (buffer-substring save-pos (point)))))
	(setq save-pos (point))
	(end-of-line)
	(unless (equal viewer "")
	  (setq info (nconc (list (cons 'viewer viewer)
				  (cons 'type (concat major "/"
						      (if (string= minor ".*")
							  "*" minor))))
			    (mailcap-parse-mailcap-extras save-pos (point))))
	  (mailcap-mailcap-entry-passes-test info)
	  (mailcap-add-mailcap-entry major minor info))
	(beginning-of-line)))))

(defun mailcap-parse-mailcap-extras (st nd)
  "Grab all the extra stuff from a mailcap entry."
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	done				; Found end of \'d ;s?
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (skip-chars-forward " \n\t;")
      (while (not (eobp))
	(setq done nil)
	(setq name-pos (point))
	(skip-chars-forward "^ \n\t=;")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \t\n")
	(if (not (eq (char-after (point)) ?=)) ; There is no value
	    (setq value t)
	  (skip-chars-forward " \t\n=")
	  (setq val-pos (point))
	  (if (memq (char-after val-pos) '(?\" ?'))
	      (progn
		(setq val-pos (1+ val-pos))
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      (backward-char 1))
		  (error (goto-char (point-max)))))
	    (while (not done)
	      (skip-chars-forward "^;")
	      (if (eq (char-after (1- (point))) ?\\ )
		  (progn
		    (subst-char-in-region (1- (point)) (point) ?\\ ? )
		    (skip-chars-forward ";"))
		(setq done t))))
	  (setq	value (buffer-substring val-pos (point))))
	;; `test' as symbol, others like "copiousoutput" and "needsx11" as
	;; strings
	(setq results (cons (cons (if (string-equal name "test")
                                      'test
                                    name)
                                  value) results))
	(skip-chars-forward " \";\n\t"))
      results)))

(defun mailcap-mailcap-entry-passes-test (info)
  "Return non-nil if mailcap entry INFO passes its test clause.
Also return non-nil if no test clause is present."
  (let ((test (assq 'test info))	; The test clause
	status)
    (setq status (and test (split-string (cdr test) " ")))
    (if (and (or (assoc "needsterm" info)
		 (assoc "needsterminal" info)
		 (assoc "needsx11" info))
	     (not (getenv "DISPLAY")))
	(setq status nil)
      (cond
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-n")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") t nil)))
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-z")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") nil t)))
       (test nil)
       (t nil)))
    (and test (listp test) (setcdr test status))))

;;;
;;; The action routines.
;;;

(defun mailcap-possible-viewers (major minor)
  "Return a list of possible viewers from MAJOR for minor type MINOR."
  (let ((exact '())
	(wildcard '()))
    (while major
      (cond
       ((equal (car (car major)) minor)
	(setq exact (cons (cdr (car major)) exact)))
       ((and minor (string-match (concat "^" (car (car major)) "$") minor))
	(setq wildcard (cons (cdr (car major)) wildcard))))
      (setq major (cdr major)))
    (nconc exact wildcard)))

(defun mailcap-unescape-mime-test (test type-info)
  (let (save-pos save-chr subst)
    (cond
     ((symbolp test) test)
     ((and (listp test) (symbolp (car test))) test)
     ((or (stringp test)
	  (and (listp test) (stringp (car test))
	       (setq test (mapconcat 'identity test " "))))
      (with-temp-buffer
	(insert test)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "^%")
	  (if (/= (- (point)
		     (progn (skip-chars-backward "\\\\")
			    (point)))
		  0)			; It is an escaped %
	      (progn
		(delete-char 1)
		(skip-chars-forward "%."))
	    (setq save-pos (point))
	    (skip-chars-forward "%")
	    (setq save-chr (char-after (point)))
	    ;; Escapes:
	    ;; %s: name of a file for the body data
	    ;; %t: content-type
	    ;; %{<parameter name}: value of parameter in mailcap entry
	    ;; %n: number of sub-parts for multipart content-type
	    ;; %F: a set of content-type/filename pairs for multiparts
	    (cond
	     ((null save-chr) nil)
	     ((= save-chr ?t)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert (or (cdr (assq 'type type-info)) "\"\"")))
	     ((memq save-chr '(?M ?n ?F))
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?{)
	      (forward-char 1)
	      (skip-chars-forward "^}")
	      (downcase-region (+ 2 save-pos) (point))
	      (setq subst (buffer-substring (+ 2 save-pos) (point)))
	      (delete-region save-pos (1+ (point)))
	      (insert (or (cdr (assoc subst type-info)) "\"\"")))
	     (t nil))))
	(buffer-string)))
     (t (error "Bad value to mailcap-unescape-mime-test: %s" test)))))

(defvar mailcap-viewer-test-cache nil)

(defun mailcap-viewer-passes-test (viewer-info type-info)
  "Return non-nil if viewer specified by VIEWER-INFO passes its test clause.
Also return non-nil if it has no test clause.  TYPE-INFO is an argument
to supply to the test."
  (let* ((test-info (assq 'test viewer-info))
	 (test (cdr test-info))
	 (otest test)
	 (viewer (cdr (assoc 'viewer viewer-info)))
	 (default-directory (expand-file-name "~/"))
	 status parsed-test cache result)
    (cond ((setq cache (assoc test mailcap-viewer-test-cache))
	   (cadr cache))
	  ((not test-info) t)		; No test clause
	  (t
	   (setq
	    result
	    (cond
	     ((not test) nil)		; Already failed test
	     ((eq test t) t)		; Already passed test
	     ((functionp test)		; Lisp function as test
	      (funcall test type-info))
	     ((and (symbolp test)	; Lisp variable as test
		   (boundp test))
	      (symbol-value test))
	     ((and (listp test)		; List to be eval'd
		   (symbolp (car test)))
	      (eval test))
	     (t
	      (setq test (mailcap-unescape-mime-test test type-info)
		    test (list shell-file-name nil nil nil
			       shell-command-switch test)
		    status (apply 'call-process test))
	      (eq 0 status))))
	   (push (list otest result) mailcap-viewer-test-cache)
	   result))))

(defun mailcap-add-mailcap-entry (major minor info)
  (let ((old-major (assoc major mailcap-mime-data)))
    (if (null old-major)		; New major area
	(setq mailcap-mime-data
	      (cons (cons major (list (cons minor info)))
		    mailcap-mime-data))
      (let ((cur-minor (assoc minor old-major)))
	(cond
	 ((or (null cur-minor)		; New minor area, or
	      (assq 'test info))	; Has a test, insert at beginning
	  (setcdr old-major (cons (cons minor info) (cdr old-major))))
	 ((and (not (assq 'test info))	; No test info, replace completely
	       (not (assq 'test cur-minor))
	       (equal (assq 'viewer info)  ; Keep alternative viewer
		      (assq 'viewer cur-minor)))
	  (setcdr cur-minor info))
	 (t
	  (setcdr old-major (cons (cons minor info) (cdr old-major))))))
      )))

(defun mailcap-add (type viewer &optional test)
  "Add VIEWER as a handler for TYPE.
If TEST is not given, it defaults to t."
  (let ((tl (split-string type "/")))
    (when (or (not (car tl))
	      (not (cadr tl)))
      (error "%s is not a valid MIME type" type))
    (mailcap-add-mailcap-entry
     (car tl) (cadr tl)
     `((viewer . ,viewer)
       (test . ,(if test test t))
       (type . ,type)))))

;;;
;;; The main whabbo
;;;

(defun mailcap-viewer-lessp (x y)
  "Return t if viewer X is more desirable than viewer Y."
  (let ((x-wild (string-match "[*?]" (or (cdr-safe (assq 'type x)) "")))
	(y-wild (string-match "[*?]" (or (cdr-safe (assq 'type y)) "")))
	(x-lisp (not (stringp (or (cdr-safe (assq 'viewer x)) ""))))
	(y-lisp (not (stringp (or (cdr-safe (assq 'viewer y)) "")))))
    (cond
     ((and x-wild (not y-wild))
      nil)
     ((and (not x-wild) y-wild)
      t)
     ((and (not y-lisp) x-lisp)
      t)
     (t nil))))

(defun mailcap-mime-info (string &optional request no-decode)
  "Get the MIME viewer command for STRING, return nil if none found.
Expects a complete content-type header line as its argument.

Second argument REQUEST specifies what information to return.  If it is
nil or the empty string, the viewer (second field of the mailcap
entry) will be returned.  If it is a string, then the mailcap field
corresponding to that string will be returned (print, description,
whatever).  If a number, then all the information for this specific
viewer is returned.  If `all', then all possible viewers for
this type is returned.

If NO-DECODE is non-nil, don't decode STRING."
  ;; NO-DECODE avoids calling `mail-header-parse-content-type' from
  ;; `mail-parse.el'
  (let (
	major				; Major encoding (text, etc)
	minor				; Minor encoding (html, etc)
	info				; Other info
	save-pos			; Misc. position during parse
	major-info			; (assoc major mailcap-mime-data)
	minor-info			; (assoc minor major-info)
	test				; current test proc.
	viewers				; Possible viewers
	passed				; Viewers that passed the test
	viewer				; The one and only viewer
	ctl)
    (save-excursion
      (setq ctl
	    (if no-decode
		(list (or string "text/plain"))
	      (mail-header-parse-content-type (or string "text/plain"))))
      (setq major (split-string (car ctl) "/"))
      (setq minor (cadr major)
	    major (car major))
      (when (setq major-info (cdr (assoc major mailcap-mime-data)))
	(when (setq viewers (mailcap-possible-viewers major-info minor))
	  (setq info (mapcar (lambda (a) (cons (symbol-name (car a))
					       (cdr a)))
			     (cdr ctl)))
	  (while viewers
	    (if (mailcap-viewer-passes-test (car viewers) info)
		(setq passed (cons (car viewers) passed)))
	    (setq viewers (cdr viewers)))
	  (setq passed (sort passed 'mailcap-viewer-lessp))
	  (setq viewer (car passed))))
      (when (and (stringp (cdr (assq 'viewer viewer)))
		 passed)
	(setq viewer (car passed)))
      (cond
       ((and (null viewer) (not (equal major "default")) request)
	(mailcap-mime-info "default" request no-decode))
       ((or (null request) (equal request ""))
	(mailcap-unescape-mime-test (cdr (assq 'viewer viewer)) info))
       ((stringp request)
	(mailcap-unescape-mime-test
	 (cdr-safe (assoc request viewer)) info))
       ((eq request 'all)
	passed)
       (t
	;; MUST make a copy *sigh*, else we modify mailcap-mime-data
	(setq viewer (copy-sequence viewer))
	(let ((view (assq 'viewer viewer))
	      (test (assq 'test viewer)))
	  (if view (setcdr view (mailcap-unescape-mime-test (cdr view) info)))
	  (if test (setcdr test (mailcap-unescape-mime-test (cdr test) info))))
	viewer)))))

;;;
;;; Experimental MIME-types parsing
;;;

(defvar mailcap-mime-extensions
  '((""       . "text/plain")
    (".1"     . "text/plain")  ;; Manual pages
    (".3"     . "text/plain")
    (".8"     . "text/plain")
    (".abs"   . "audio/x-mpeg")
    (".aif"   . "audio/aiff")
    (".aifc"  . "audio/aiff")
    (".aiff"  . "audio/aiff")
    (".ano"   . "application/x-annotator")
    (".au"    . "audio/ulaw")
    (".avi"   . "video/x-msvideo")
    (".bcpio" . "application/x-bcpio")
    (".bin"   . "application/octet-stream")
    (".cdf"   . "application/x-netcdr")
    (".cpio"  . "application/x-cpio")
    (".csh"   . "application/x-csh")
    (".css"   . "text/css")
    (".dvi"   . "application/x-dvi")
    (".diff"  . "text/x-patch")
    (".dpatch". "test/x-patch")
    (".el"    . "application/emacs-lisp")
    (".eps"   . "application/postscript")
    (".etx"   . "text/x-setext")
    (".exe"   . "application/octet-stream")
    (".fax"   . "image/x-fax")
    (".gif"   . "image/gif")
    (".hdf"   . "application/x-hdf")
    (".hqx"   . "application/mac-binhex40")
    (".htm"   . "text/html")
    (".html"  . "text/html")
    (".icon"  . "image/x-icon")
    (".ief"   . "image/ief")
    (".jpg"   . "image/jpeg")
    (".macp"  . "image/x-macpaint")
    (".man"   . "application/x-troff-man")
    (".me"    . "application/x-troff-me")
    (".mif"   . "application/mif")
    (".mov"   . "video/quicktime")
    (".movie" . "video/x-sgi-movie")
    (".mp2"   . "audio/x-mpeg")
    (".mp3"   . "audio/x-mpeg")
    (".mp2a"  . "audio/x-mpeg2")
    (".mpa"   . "audio/x-mpeg")
    (".mpa2"  . "audio/x-mpeg2")
    (".mpe"   . "video/mpeg")
    (".mpeg"  . "video/mpeg")
    (".mpega" . "audio/x-mpeg")
    (".mpegv" . "video/mpeg")
    (".mpg"   . "video/mpeg")
    (".mpv"   . "video/mpeg")
    (".ms"    . "application/x-troff-ms")
    (".nc"    . "application/x-netcdf")
    (".nc"    . "application/x-netcdf")
    (".oda"   . "application/oda")
    (".patch" . "text/x-patch")
    (".pbm"   . "image/x-portable-bitmap")
    (".pdf"   . "application/pdf")
    (".pgm"   . "image/portable-graymap")
    (".pict"  . "image/pict")
    (".png"   . "image/png")
    (".pnm"   . "image/x-portable-anymap")
    (".pod"   . "text/plain")
    (".ppm"   . "image/portable-pixmap")
    (".ps"    . "application/postscript")
    (".qt"    . "video/quicktime")
    (".ras"   . "image/x-raster")
    (".rgb"   . "image/x-rgb")
    (".rtf"   . "application/rtf")
    (".rtx"   . "text/richtext")
    (".sh"    . "application/x-sh")
    (".sit"   . "application/x-stuffit")
    (".siv"   . "application/sieve")
    (".snd"   . "audio/basic")
    (".soa"   . "text/dns")
    (".src"   . "application/x-wais-source")
    (".tar"   . "archive/tar")
    (".tcl"   . "application/x-tcl")
    (".tex"   . "application/x-tex")
    (".texi"  . "application/texinfo")
    (".tga"   . "image/x-targa")
    (".tif"   . "image/tiff")
    (".tiff"  . "image/tiff")
    (".tr"    . "application/x-troff")
    (".troff" . "application/x-troff")
    (".tsv"   . "text/tab-separated-values")
    (".txt"   . "text/plain")
    (".vbs"   . "video/mpeg")
    (".vox"   . "audio/basic")
    (".vrml"  . "x-world/x-vrml")
    (".wav"   . "audio/x-wav")
    (".xls"   . "application/vnd.ms-excel")
    (".wrl"   . "x-world/x-vrml")
    (".xbm"   . "image/xbm")
    (".xpm"   . "image/xpm")
    (".xwd"   . "image/windowdump")
    (".zip"   . "application/zip")
    (".ai"    . "application/postscript")
    (".jpe"   . "image/jpeg")
    (".jpeg"  . "image/jpeg")
    (".org"   . "text/x-org"))
  "An alist of file extensions and corresponding MIME content-types.
This exists for you to customize the information in Lisp.  It is
merged with values from mailcap files by `mailcap-parse-mimetypes'.")

(defvar mailcap-mimetypes-parsed-p nil)

(defun mailcap-parse-mimetypes (&optional path force)
  "Parse out all the mimetypes specified in a Unix-style path string PATH.
Components of PATH are separated by the `path-separator' character
appropriate for this system.  If PATH is omitted, use the value of
environment variable MIMETYPES if set; otherwise use a default path.
If FORCE, re-parse even if already parsed."
  (interactive (list nil t))
  (when (or (not mailcap-mimetypes-parsed-p)
	    force)
    (cond
     (path nil)
     ((getenv "MIMETYPES") (setq path (getenv "MIMETYPES")))
     ((memq system-type mailcap-poor-system-types)
      (setq path '("~/mime.typ" "~/etc/mime.typ")))
     (t (setq path
	      ;; mime.types seems to be the normal name, definitely so
	      ;; on current GNUish systems.  The search order follows
	      ;; that for mailcap.
	      '("~/.mime.types"
		"/etc/mime.types"
		"/usr/etc/mime.types"
		"/usr/local/etc/mime.types"
		"/usr/local/www/conf/mime.types"
		"~/.mime-types"
		"/etc/mime-types"
		"/usr/etc/mime-types"
		"/usr/local/etc/mime-types"
		"/usr/local/www/conf/mime-types"))))
    (let ((fnames (reverse (if (stringp path)
			       (split-string path path-separator t)
			     path)))
	  fname)
      (while fnames
	(setq fname (car fnames))
	(if (and (file-readable-p fname))
	    (mailcap-parse-mimetype-file fname))
	(setq fnames (cdr fnames))))
    (setq mailcap-mimetypes-parsed-p t)))

(defun mailcap-parse-mimetype-file (fname)
  "Parse out a mime-types file FNAME."
  (let (type				; The MIME type for this line
	extns				; The extensions for this line
	save-pos			; Misc. saved buffer positions
	)
    (with-temp-buffer
      (insert-file-contents fname)
      (mailcap-replace-regexp "#.*" "")
      (mailcap-replace-regexp "\n+" "\n")
      (mailcap-replace-regexp "[ \t]+$" "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^ \t\n")
	(downcase-region save-pos (point))
	(setq type (buffer-substring save-pos (point)))
	(while (not (eolp))
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^ \t\n")
	  (setq extns (cons (buffer-substring save-pos (point)) extns)))
	(while extns
	  (setq mailcap-mime-extensions
		(cons
		 (cons (if (= (string-to-char (car extns)) ?.)
			   (car extns)
			 (concat "." (car extns))) type)
		 mailcap-mime-extensions)
		extns (cdr extns)))))))

(defun mailcap-extension-to-mime (extn)
  "Return the MIME content type of the file extensions EXTN."
  (mailcap-parse-mimetypes)
  (if (and (stringp extn)
	   (not (eq (string-to-char extn) ?.)))
      (setq extn (concat "." extn)))
  (cdr (assoc (downcase extn) mailcap-mime-extensions)))

;; Unused?
(defalias 'mailcap-command-p 'executable-find)

(defun mailcap-mime-types ()
  "Return a list of MIME media types."
  (mailcap-parse-mimetypes)
  (mailcap-delete-duplicates
   (nconc
    (mapcar 'cdr mailcap-mime-extensions)
    (apply
     'nconc
     (mapcar
      (lambda (l)
	(delq nil
	      (mapcar
	       (lambda (m)
		 (let ((type (cdr (assq 'type (cdr m)))))
		   (if (equal (cadr (split-string type "/"))
			      "*")
		       nil
		     type)))
	       (cdr l))))
      mailcap-mime-data)))))

;;;
;;; Useful supplementary functions
;;;

(defun mailcap-file-default-commands (files)
  "Return a list of default commands for FILES."
  (mailcap-parse-mailcaps)
  (mailcap-parse-mimetypes)
  (let* ((all-mime-type
	  ;; All unique MIME types from file extensions
	  (mailcap-delete-duplicates
	   (mapcar (lambda (file)
		     (mailcap-extension-to-mime
		      (file-name-extension file t)))
		   files)))
	 (all-mime-info
	  ;; All MIME info lists
	  (mailcap-delete-duplicates
	   (mapcar (lambda (mime-type)
		     (mailcap-mime-info mime-type 'all))
		   all-mime-type)))
	 (common-mime-info
	  ;; Intersection of mime-infos from different mime-types;
	  ;; or just the first MIME info for a single MIME type
	  (if (cdr all-mime-info)
	      (delq nil (mapcar (lambda (mi1)
				  (unless (memq nil (mapcar
						     (lambda (mi2)
						       (member mi1 mi2))
						     (cdr all-mime-info)))
				    mi1))
				(car all-mime-info)))
	    (car all-mime-info)))
	 (commands
	  ;; Command strings from `viewer' field of the MIME info
	  (mailcap-delete-duplicates
	   (delq nil (mapcar (lambda (mime-info)
			       (let ((command (cdr (assoc 'viewer mime-info))))
				 (if (stringp command)
				     (mailcap-replace-in-string
				      ;; Replace mailcap's `%s' placeholder
				      ;; with dired's `?' placeholder
				      (mailcap-replace-in-string
				       ;; Remove the final filename placeholder
				       command "[ \t\n]*\\('\\)?%s\\1?[ \t\n]*\\'" "" t)
				      "%s" "?" t))))
			     common-mime-info)))))
    commands))

(provide 'mailcap)

;;; mailcap.el ends here
