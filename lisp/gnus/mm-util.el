;;; mm-util.el --- Utility functions for Mule and low level things

;; Copyright (C) 1998-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))
(require 'mail-prsvr)

(eval-and-compile
  (if (featurep 'xemacs)
      (unless (ignore-errors
		(require 'timer-funcs))
	(require 'timer))
    (require 'timer)))

(defvar mm-mime-mule-charset-alist )
;; Note this is not presently used on Emacs >= 23, which is good,
;; since it means standalone message-mode (which requires mml and
;; hence mml-util) does not load gnus-util.
(autoload 'gnus-completing-read "gnus-util")

;; Emulate functions that are not available in every (X)Emacs version.
;; The name of a function is prefixed with mm-, like `mm-char-int' for
;; `char-int' that is a native XEmacs function, not available in Emacs.
;; Gnus programs all should use mm- functions, not the original ones.
(eval-and-compile
  (mapc
   (lambda (elem)
     (let ((nfunc (intern (format "mm-%s" (car elem)))))
       (if (fboundp (car elem))
	   (defalias nfunc (car elem))
	 (defalias nfunc (cdr elem)))))
   `(;; `coding-system-list' is not available in XEmacs 21.4 built
     ;; without the `file-coding' feature.
     (coding-system-list . ignore)
     ;; `char-int' is an XEmacs function, not available in Emacs.
     (char-int . identity)
     ;; `coding-system-equal' is an Emacs function, not available in XEmacs.
     (coding-system-equal . equal)
     ;; `annotationp' is an XEmacs function, not available in Emacs.
     (annotationp . ignore)
     ;; `set-buffer-file-coding-system' is not available in XEmacs 21.4
     ;; built without the `file-coding' feature.
     (set-buffer-file-coding-system . ignore)
     ;; `read-charset' is an Emacs function, not available in XEmacs.
     (read-charset
      . ,(lambda (prompt)
	   "Return a charset."
	   (intern
	    (gnus-completing-read
	     prompt
	     (mapcar (lambda (e) (symbol-name (car e)))
		     mm-mime-mule-charset-alist)
	     t))))
     ;; `subst-char-in-string' is not available in XEmacs 21.4.
     (subst-char-in-string
      . ,(lambda (from to string &optional inplace)
	   ;; stolen (and renamed) from nnheader.el
	   "Replace characters in STRING from FROM to TO.
	  Unless optional argument INPLACE is non-nil, return a new string."
	   (let ((string (if inplace string (copy-sequence string)))
		 (len (length string))
		 (idx 0))
	     ;; Replace all occurrences of FROM with TO.
	     (while (< idx len)
	       (when (= (aref string idx) from)
		 (aset string idx to))
	       (setq idx (1+ idx)))
	     string)))
     ;; `replace-in-string' is an XEmacs function, not available in Emacs.
     (replace-in-string
      . ,(lambda (string regexp rep &optional literal)
	   "See `replace-regexp-in-string', only the order of args differs."
	   (replace-regexp-in-string regexp rep string nil literal)))
     ;; `string-as-unibyte' is an Emacs function, not available in XEmacs.
     (string-as-unibyte . identity)
     ;; `string-make-unibyte' is an Emacs function, not available in XEmacs.
     (string-make-unibyte . identity)
     ;; string-as-multibyte often doesn't really do what you think it does.
     ;; Example:
     ;;    (aref (string-as-multibyte "\201") 0) -> 129 (aka ?\201)
     ;;    (aref (string-as-multibyte "\300") 0) -> 192 (aka ?\300)
     ;;    (aref (string-as-multibyte "\300\201") 0) -> 192 (aka ?\300)
     ;;    (aref (string-as-multibyte "\300\201") 1) -> 129 (aka ?\201)
     ;; but
     ;;    (aref (string-as-multibyte "\201\300") 0) -> 2240
     ;;    (aref (string-as-multibyte "\201\300") 1) -> <error>
     ;; Better use string-to-multibyte or encode-coding-string.
     ;; If you really need string-as-multibyte somewhere it's usually
     ;; because you're using the internal emacs-mule representation (maybe
     ;; because you're using string-as-unibyte somewhere), which is
     ;; generally a problem in itself.
     ;; Here is an approximate equivalence table to help think about it:
     ;; (string-as-multibyte s)   ~= (decode-coding-string s 'emacs-mule)
     ;; (string-to-multibyte s)   ~= (decode-coding-string s 'binary)
     ;; (string-make-multibyte s) ~= (decode-coding-string s locale-coding-system)
     ;; `string-as-multibyte' is an Emacs function, not available in XEmacs.
     (string-as-multibyte . identity)
     ;; `multibyte-string-p' is an Emacs function, not available in XEmacs.
     (multibyte-string-p . ignore)
     ;; `insert-byte' is available only in Emacs 23.1 or greater.
     (insert-byte . insert-char)
     ;; `multibyte-char-to-unibyte' is an Emacs function, not available
     ;; in XEmacs.
     (multibyte-char-to-unibyte . identity)
     ;; `set-buffer-multibyte' is an Emacs function, not available in XEmacs.
     (set-buffer-multibyte . ignore)
     ;; `special-display-p' is an Emacs function, not available in XEmacs.
     (special-display-p
      . ,(lambda (buffer-name)
	   "Returns non-nil if a buffer named BUFFER-NAME gets a special frame."
	   (and special-display-function
		(or (and (member buffer-name special-display-buffer-names) t)
		    (cdr (assoc buffer-name special-display-buffer-names))
		    (catch 'return
		      (dolist (elem special-display-regexps)
			(and (stringp elem)
			     (string-match elem buffer-name)
			     (throw 'return t))
			(and (consp elem)
			     (stringp (car elem))
			     (string-match (car elem) buffer-name)
			     (throw 'return (cdr elem)))))))))
     ;; `substring-no-properties' is available only in Emacs 22.1 or greater.
     (substring-no-properties
      . ,(lambda (string &optional from to)
	   "Return a substring of STRING, without text properties.
It starts at index FROM and ending before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties."
	   (setq string (substring string (or from 0) to))
	   (set-text-properties 0 (length string) nil string)
	   string))
     ;; `line-number-at-pos' is available only in Emacs 22.1 or greater
     ;; and XEmacs 21.5.
     (line-number-at-pos
      . ,(lambda (&optional pos)
	   "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
Counting starts at (point-min), so the value refers
to the contents of the accessible portion of the buffer."
	   (let ((opoint (or pos (point))) start)
	     (save-excursion
	       (goto-char (point-min))
	       (setq start (point))
	       (goto-char opoint)
	       (forward-line 0)
	       (1+ (count-lines start (point))))))))))

;; `decode-coding-string', `encode-coding-string', `decode-coding-region'
;; and `encode-coding-region' are available in Emacs and XEmacs built with
;; the `file-coding' feature, but the XEmacs versions treat nil, that is
;; given as the `coding-system' argument, as the `binary' coding system.
(eval-and-compile
  (if (featurep 'xemacs)
      (if (featurep 'file-coding)
	  (progn
	    (defun mm-decode-coding-string (str coding-system)
	      (if coding-system
		  (decode-coding-string str coding-system)
		str))
	    (defun mm-encode-coding-string (str coding-system)
	      (if coding-system
		  (encode-coding-string str coding-system)
		str))
	    (defun mm-decode-coding-region (start end coding-system)
	      (if coding-system
		  (decode-coding-region start end coding-system)))
	    (defun mm-encode-coding-region (start end coding-system)
	      (if coding-system
		  (encode-coding-region start end coding-system))))
	(defun mm-decode-coding-string (str coding-system) str)
	(defun mm-encode-coding-string (str coding-system) str)
	(defalias 'mm-decode-coding-region 'ignore)
	(defalias 'mm-encode-coding-region 'ignore))
    (defalias 'mm-decode-coding-string 'decode-coding-string)
    (defalias 'mm-encode-coding-string 'encode-coding-string)
    (defalias 'mm-decode-coding-region 'decode-coding-region)
    (defalias 'mm-encode-coding-region 'encode-coding-region)))

;; `string-to-multibyte' is available only in Emacs.
(defalias 'mm-string-to-multibyte (if (featurep 'xemacs)
				      'identity
				    'string-to-multibyte))

;; `char-or-char-int-p' is an XEmacs function, not available in Emacs.
(eval-and-compile
  (defalias 'mm-char-or-char-int-p
    (cond
     ((fboundp 'char-or-char-int-p) 'char-or-char-int-p)
     ((fboundp 'char-valid-p) 'char-valid-p)
     (t 'identity))))

;; `ucs-to-char' is a function that Mule-UCS provides.
(eval-and-compile
  (if (featurep 'xemacs)
      (cond ((and (fboundp 'unicode-to-char) ;; XEmacs 21.5.
		  (subrp (symbol-function 'unicode-to-char)))
	     (if (featurep 'mule)
		 (defalias 'mm-ucs-to-char 'unicode-to-char)
	       (defun mm-ucs-to-char (codepoint)
		 "Convert Unicode codepoint to character."
		 (or (unicode-to-char codepoint) ?#))))
	    ((featurep 'mule)
	     (defun mm-ucs-to-char (codepoint)
	       "Convert Unicode codepoint to character."
	       (if (fboundp 'ucs-to-char) ;; Mule-UCS is loaded.
		   (progn
		     (defalias 'mm-ucs-to-char
		       (lambda (codepoint)
			 "Convert Unicode codepoint to character."
			 (condition-case nil
			     (or (ucs-to-char codepoint) ?#)
			   (error ?#))))
		     (mm-ucs-to-char codepoint))
		 (condition-case nil
		     (or (int-to-char codepoint) ?#)
		   (error ?#)))))
	    (t
	     (defun mm-ucs-to-char (codepoint)
	       "Convert Unicode codepoint to character."
	       (condition-case nil
		   (or (int-to-char codepoint) ?#)
		 (error ?#)))))
    (if (let ((char (make-char 'japanese-jisx0208 36 34)))
	  (eq char (decode-char 'ucs char)))
	;; Emacs 23.
	(defalias 'mm-ucs-to-char 'identity)
      (defun mm-ucs-to-char (codepoint)
	"Convert Unicode codepoint to character."
	(or (decode-char 'ucs codepoint) ?#)))))

;; Fixme:  This seems always to be used to read a MIME charset, so it
;; should be re-named and fixed (in Emacs) to offer completion only on
;; proper charset names (base coding systems which have a
;; mime-charset defined).  XEmacs doesn't believe in mime-charset;
;; test with
;;   `(or (coding-system-get 'iso-8859-1 'mime-charset)
;;        (coding-system-get 'iso-8859-1 :mime-charset))'
;; Actually, there should be an `mm-coding-system-mime-charset'.
(eval-and-compile
  (defalias 'mm-read-coding-system
    (if (featurep 'emacs) 'read-coding-system
      (cond
       ((fboundp 'read-coding-system)
	(if (and (featurep 'xemacs)
		 (<= (string-to-number emacs-version) 21.1))
	    (lambda (prompt &optional default-coding-system)
	      (read-coding-system prompt))
	  'read-coding-system))
       (t (lambda (prompt &optional default-coding-system)
	    "Prompt the user for a coding system."
	    (gnus-completing-read
	     prompt (mapcar (lambda (s) (symbol-name (car s)))
			    mm-mime-mule-charset-alist))))))))

(defvar mm-coding-system-list nil)
(defun mm-get-coding-system-list ()
  "Get the coding system list."
  (or mm-coding-system-list
      (setq mm-coding-system-list (mm-coding-system-list))))

(defun mm-coding-system-p (cs)
  "Return non-nil if CS is a symbol naming a coding system.
In XEmacs, also return non-nil if CS is a coding system object.
If CS is available, return CS itself in Emacs, and return a coding
system object in XEmacs."
  (if (fboundp 'find-coding-system)
      (and cs (find-coding-system cs))
    (if (fboundp 'coding-system-p)
	(when (coding-system-p cs)
	  cs)
      ;; no-MULE XEmacs:
      (car (memq cs (mm-get-coding-system-list))))))

(defvar mm-charset-synonym-alist
  `(
    ;; Not in XEmacs, but it's not a proper MIME charset anyhow.
    ,@(unless (mm-coding-system-p 'x-ctext)
	'((x-ctext . ctext)))
    ;; ISO-8859-15 is very similar to ISO-8859-1.  But it's _different_ in 8
    ;; positions!
    ,@(unless (mm-coding-system-p 'iso-8859-15)
	'((iso-8859-15 . iso-8859-1)))
    ;; BIG-5HKSCS is similar to, but different than, BIG-5.
    ,@(unless (mm-coding-system-p 'big5-hkscs)
	'((big5-hkscs . big5)))
    ;; A Microsoft misunderstanding.
    ,@(when (and (not (mm-coding-system-p 'unicode))
		 (mm-coding-system-p 'utf-16-le))
	'((unicode . utf-16-le)))
    ;; A Microsoft misunderstanding.
    ,@(unless (mm-coding-system-p 'ks_c_5601-1987)
	(if (mm-coding-system-p 'cp949)
	    '((ks_c_5601-1987 . cp949))
	  '((ks_c_5601-1987 . euc-kr))))
    ;; Windows-31J is Windows Codepage 932.
    ,@(when (and (not (mm-coding-system-p 'windows-31j))
		 (mm-coding-system-p 'cp932))
	'((windows-31j . cp932)))
    ;; Charset name: GBK, Charset aliases: CP936, MS936, windows-936
    ;; http://www.iana.org/assignments/charset-reg/GBK
    ;; Emacs 22.1 has cp936, but not gbk, so we alias it:
    ,@(when (and (not (mm-coding-system-p 'gbk))
		 (mm-coding-system-p 'cp936))
	'((gbk . cp936)))
    ;; UTF8 is a bogus name for UTF-8
    ,@(when (and (not (mm-coding-system-p 'utf8))
		 (mm-coding-system-p 'utf-8))
	'((utf8 . utf-8)))
    ;; ISO8859-1 is a bogus name for ISO-8859-1
    ,@(when (and (not (mm-coding-system-p 'iso8859-1))
		 (mm-coding-system-p 'iso-8859-1))
	'((iso8859-1 . iso-8859-1)))
    ;; ISO_8859-1 is a bogus name for ISO-8859-1
    ,@(when (and (not (mm-coding-system-p 'iso_8859-1))
		 (mm-coding-system-p 'iso-8859-1))
	'((iso_8859-1 . iso-8859-1)))
    )
  "A mapping from unknown or invalid charset names to the real charset names.

See `mm-codepage-iso-8859-list' and `mm-codepage-ibm-list'.")

(defun mm-codepage-setup (number &optional alias)
  "Create a coding system cpNUMBER.
The coding system is created using `codepage-setup'.  If ALIAS is
non-nil, an alias is created and added to
`mm-charset-synonym-alist'.  If ALIAS is a string, it's used as
the alias.  Else windows-NUMBER is used."
  (interactive
   (let ((completion-ignore-case t)
	 (candidates (if (fboundp 'cp-supported-codepages)
			 (cp-supported-codepages)
		       ;; Removed in Emacs 23 (unicode), so signal an error:
		       (error "`codepage-setup' not present in this Emacs version"))))
     (list (gnus-completing-read "Setup DOS Codepage" candidates
                                 t nil nil "437"))))
  (when alias
    (setq alias (if (stringp alias)
		    (intern alias)
		  (intern (format "windows-%s" number)))))
  (let* ((cp (intern (format "cp%s" number))))
    (unless (mm-coding-system-p cp)
      (if (fboundp 'codepage-setup)	; silence compiler
	  (codepage-setup number)
	(error "`codepage-setup' not present in this Emacs version")))
    (when (and alias
	       ;; Don't add alias if setup of cp failed.
	       (mm-coding-system-p cp))
      (add-to-list 'mm-charset-synonym-alist (cons alias cp)))))

(defcustom mm-codepage-iso-8859-list
  (list 1250 ;; Windows-1250 is a variant of Latin-2 heavily used by Microsoft
	;; Outlook users in Czech republic.  Use this to allow reading of
	;; their e-mails.
	'(1252 . 1) ;; Windows-1252 is a superset of iso-8859-1 (West
	            ;; Europe).  See also `gnus-article-dumbquotes-map'.
	'(1254 . 9) ;; Windows-1254 is a superset of iso-8859-9 (Turkish).
	'(1255 . 8));; Windows-1255 is a superset of iso-8859-8 (Hebrew).
  "A list of Windows codepage numbers and iso-8859 charset numbers.

If an element is a number corresponding to a supported windows
codepage, appropriate entries to `mm-charset-synonym-alist' are
added by `mm-setup-codepage-iso-8859'.  An element may also be a
cons cell where the car is a codepage number and the cdr is the
corresponding number of an iso-8859 charset."
  :type '(list (set :inline t
		    (const 1250 :tag "Central and East European")
		    (const (1252 . 1) :tag "West European")
		    (const (1254 . 9) :tag "Turkish")
		    (const (1255 . 8) :tag "Hebrew"))
	       (repeat :inline t
		       :tag "Other options"
		       (choice
			(integer :tag "Windows codepage number")
			(cons (integer :tag "Windows codepage number")
			      (integer :tag "iso-8859 charset  number")))))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defcustom mm-codepage-ibm-list
  (list 437 ;; (US etc.)
	860 ;; (Portugal)
	861 ;; (Iceland)
	862 ;; (Israel)
	863 ;; (Canadian French)
	865 ;; (Nordic)
	852 ;;
	850 ;; (Latin 1)
	855 ;; (Cyrillic)
	866 ;; (Cyrillic - Russian)
	857 ;; (Turkish)
	864 ;; (Arabic)
	869 ;; (Greek)
	874);; (Thai)
  ;; In Emacs 23 (unicode), cp... and ibm... are aliases.
  ;; Cf. http://thread.gmane.org/v9lkng5nwy.fsf@marauder.physik.uni-ulm.de
  "List of IBM codepage numbers.

The codepage mappings slightly differ between IBM and other vendors.
See \"ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/IBM/README.TXT\".

If an element is a number corresponding to a supported windows
codepage, appropriate entries to `mm-charset-synonym-alist' are
added by `mm-setup-codepage-ibm'."
  :type '(list (set :inline t
		    (const 437 :tag "US etc.")
		    (const 860 :tag "Portugal")
		    (const 861 :tag "Iceland")
		    (const 862 :tag "Israel")
		    (const 863 :tag "Canadian French")
		    (const 865 :tag "Nordic")
		    (const 852)
		    (const 850 :tag "Latin 1")
		    (const 855 :tag "Cyrillic")
		    (const 866 :tag "Cyrillic - Russian")
		    (const 857 :tag "Turkish")
		    (const 864 :tag "Arabic")
		    (const 869 :tag "Greek")
		    (const 874 :tag "Thai"))
	       (repeat :inline t
		       :tag "Other options"
		       (integer :tag "Codepage number")))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defun mm-setup-codepage-iso-8859 (&optional list)
  "Add appropriate entries to `mm-charset-synonym-alist'.
Unless LIST is given, `mm-codepage-iso-8859-list' is used."
  (unless list
    (setq list mm-codepage-iso-8859-list))
  (dolist (i list)
    (let (cp windows iso)
      (if (consp i)
	  (setq cp (intern (format "cp%d" (car i)))
		windows (intern (format "windows-%d" (car i)))
		iso (intern (format "iso-8859-%d" (cdr i))))
	(setq cp (intern (format "cp%d" i))
	      windows (intern (format "windows-%d" i))))
      (unless (mm-coding-system-p windows)
	(if (mm-coding-system-p cp)
	    (add-to-list 'mm-charset-synonym-alist (cons windows cp))
	  (add-to-list 'mm-charset-synonym-alist (cons windows iso)))))))

(defun mm-setup-codepage-ibm (&optional list)
  "Add appropriate entries to `mm-charset-synonym-alist'.
Unless LIST is given, `mm-codepage-ibm-list' is used."
  (unless list
    (setq list mm-codepage-ibm-list))
  (dolist (number list)
    (let ((ibm (intern (format "ibm%d" number)))
	  (cp  (intern (format "cp%d" number))))
      (when (and (not (mm-coding-system-p ibm))
		 (mm-coding-system-p cp))
	(add-to-list 'mm-charset-synonym-alist (cons ibm cp))))))

;; Initialize:
(mm-setup-codepage-iso-8859)
(mm-setup-codepage-ibm)

;; Note: this has to be defined before `mm-charset-to-coding-system'.
(defcustom mm-charset-eval-alist
  (if (featurep 'xemacs)
      nil ;; I don't know what would be useful for XEmacs.
    '(;; Emacs 22 provides autoloads for 1250-1258
      ;; (i.e. `mm-codepage-setup' does nothing).
      (windows-1250 . (mm-codepage-setup 1250 t))
      (windows-1251 . (mm-codepage-setup 1251 t))
      (windows-1253 . (mm-codepage-setup 1253 t))
      (windows-1257 . (mm-codepage-setup 1257 t))))
  "An alist of (CHARSET . FORM) pairs.
If an article is encoded in an unknown CHARSET, FORM is
evaluated.  This allows to load additional libraries providing
charsets on demand.  If supported by your Emacs version, you
could use `autoload-coding-system' here."
  :version "22.1" ;; Gnus 5.10.9
  :type '(list (set :inline t
		    (const (windows-1250 . (mm-codepage-setup 1250 t)))
		    (const (windows-1251 . (mm-codepage-setup 1251 t)))
		    (const (windows-1253 . (mm-codepage-setup 1253 t)))
		    (const (windows-1257 . (mm-codepage-setup 1257 t)))
		    (const (cp850 . (mm-codepage-setup 850 nil))))
	       (repeat :inline t
		       :tag "Other options"
		       (cons (symbol :tag "charset")
			     (symbol :tag "form"))))
  :group 'mime)
(put 'mm-charset-eval-alist 'risky-local-variable t)

(defvar mm-charset-override-alist)

;; Note: this function has to be defined before `mm-charset-override-alist'
;; since it will use this function in order to determine its default value
;; when loading mm-util.elc.
(defun mm-charset-to-coding-system (charset &optional lbt
					    allow-override silent)
  "Return coding-system corresponding to CHARSET.
CHARSET is a symbol naming a MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as the line break code type of the coding system.

If ALLOW-OVERRIDE is given, use `mm-charset-override-alist' to
map undesired charset names to their replacement.  This should
only be used for decoding, not for encoding.

A non-nil value of SILENT means don't issue a warning even if CHARSET
is not available."
  ;; OVERRIDE is used (only) in `mm-decode-body' and `mm-decode-string'.
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (when lbt
    (setq charset (intern (format "%s-%s" charset lbt))))
  (cond
   ((null charset)
    charset)
   ;; Running in a non-MULE environment.
   ((or (null (mm-get-coding-system-list))
	(not (fboundp 'coding-system-get)))
    charset)
   ;; Check override list quite early.  Should only used for decoding, not for
   ;; encoding!
   ((and allow-override
	 (let ((cs (cdr (assq charset mm-charset-override-alist))))
	   (and cs (mm-coding-system-p cs) cs))))
   ;; ascii
   ((or (eq charset 'us-ascii)
	(string-match "ansi.x3.4" (symbol-name charset)))
    'ascii)
   ;; Check to see whether we can handle this charset.  (This depends
   ;; on there being some coding system matching each `mime-charset'
   ;; property defined, as there should be.)
   ((and (mm-coding-system-p charset)
;;; Doing this would potentially weed out incorrect charsets.
;;; 	 charset
;;; 	 (eq charset (coding-system-get charset 'mime-charset))
	 )
    charset)
   ;; Use coding system Emacs knows.
   ((and (fboundp 'coding-system-from-name)
	 (coding-system-from-name charset)))
   ;; Eval expressions from `mm-charset-eval-alist'
   ((let* ((el (assq charset mm-charset-eval-alist))
	   (cs (car el))
	   (form (cdr el)))
      (and cs
	   form
	   (prog2
	       ;; Avoid errors...
	       (condition-case nil (eval form) (error nil))
	       ;; (message "Failed to eval `%s'" form))
	       (mm-coding-system-p cs)
	     (message "Added charset `%s' via `mm-charset-eval-alist'" cs))
	   cs)))
   ;; Translate invalid charsets.
   ((let ((cs (cdr (assq charset mm-charset-synonym-alist))))
      (and cs
	   (mm-coding-system-p cs)
	   ;; (message
	   ;;  "Using synonym `%s' from `mm-charset-synonym-alist' for `%s'"
	   ;;  cs charset)
	   cs)))
   ;; Last resort: search the coding system list for entries which
   ;; have the right mime-charset in case the canonical name isn't
   ;; defined (though it should be).
   ((let (cs)
      ;; mm-get-coding-system-list returns a list of cs without lbt.
      ;; Do we need -lbt?
      (dolist (c (mm-get-coding-system-list))
	(if (and (null cs)
		 (eq charset (or (coding-system-get c :mime-charset)
				 (coding-system-get c 'mime-charset))))
	    (setq cs c)))
      (unless (or silent cs)
	;; Warn the user about unknown charset:
	(if (fboundp 'gnus-message)
	    (gnus-message 7 "Unknown charset: %s" charset)
	  (message "Unknown charset: %s" charset)))
      cs))))

;; Note: `mm-charset-to-coding-system' has to be defined before this.
(defcustom mm-charset-override-alist
  ;; Note: pairs that cannot be used in the Emacs version currently running
  ;; will be removed.
  '((gb2312 . gbk)
    (iso-8859-1 . windows-1252)
    (iso-8859-8 . windows-1255)
    (iso-8859-9 . windows-1254))
  "A mapping from undesired charset names to their replacement.

You may add pairs like (iso-8859-1 . windows-1252) here,
i.e. treat iso-8859-1 as windows-1252.  windows-1252 is a
superset of iso-8859-1."
  :type
  '(list
    :convert-widget
    (lambda (widget)
      (let ((defaults
	      (delq nil
		    (mapcar (lambda (pair)
			      (if (mm-charset-to-coding-system (cdr pair)
							       nil nil t)
				  pair))
			    '((gb2312 . gbk)
			      (iso-8859-1 . windows-1252)
			      (iso-8859-8 . windows-1255)
			      (iso-8859-9 . windows-1254)
			      (undecided  . windows-1252)))))
	    (val (copy-sequence (default-value 'mm-charset-override-alist)))
	    pair rest)
	(while val
	  (push (if (and (prog1
			     (setq pair (assq (caar val) defaults))
			   (setq defaults (delq pair defaults)))
			 (equal (car val) pair))
		    `(const ,pair)
		  `(cons :format "%v"
			 (const :format "(%v" ,(caar val))
			 (symbol :size 3 :format " . %v)\n" ,(cdar val))))
		rest)
	  (setq val (cdr val)))
	(while defaults
	  (push `(const ,(pop defaults)) rest))
	(widget-convert
	 'list
	 `(set :inline t :format "%v" ,@(nreverse rest))
	 `(repeat :inline t :tag "Other options"
		  (cons :format "%v"
			(symbol :size 3 :format "(%v")
			(symbol :size 3 :format " . %v)\n")))))))
  ;; Remove pairs that cannot be used in the Emacs version currently
  ;; running.  Note that this section will be evaluated when loading
  ;; mm-util.elc.
  :set (lambda (symbol value)
	 (custom-set-default
	  symbol (delq nil
		       (mapcar (lambda (pair)
				 (if (mm-charset-to-coding-system (cdr pair)
								  nil nil t)
				     pair))
			       value))))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defvar mm-binary-coding-system
  (cond
   ((mm-coding-system-p 'binary) 'binary)
   ((mm-coding-system-p 'no-conversion) 'no-conversion)
   (t nil))
  "100% binary coding system.")

(defvar mm-text-coding-system
  (or (if (memq system-type '(windows-nt ms-dos))
	  (and (mm-coding-system-p 'raw-text-dos) 'raw-text-dos)
	(and (mm-coding-system-p 'raw-text) 'raw-text))
      mm-binary-coding-system)
  "Text-safe coding system (For removing ^M).")

(defvar mm-text-coding-system-for-write nil
  "Text coding system for write.")

(defvar mm-auto-save-coding-system
  (cond
   ((mm-coding-system-p 'utf-8-emacs)	; Mule 7
    (if (memq system-type '(windows-nt ms-dos))
	(if (mm-coding-system-p 'utf-8-emacs-dos)
	    'utf-8-emacs-dos mm-binary-coding-system)
      'utf-8-emacs))
   ((mm-coding-system-p 'emacs-mule)
    (if (memq system-type '(windows-nt ms-dos))
	(if (mm-coding-system-p 'emacs-mule-dos)
	    'emacs-mule-dos mm-binary-coding-system)
      'emacs-mule))
   ((mm-coding-system-p 'escape-quoted) 'escape-quoted)
   (t mm-binary-coding-system))
  "Coding system of auto save file.")

(defvar mm-universal-coding-system mm-auto-save-coding-system
  "The universal coding system.")

;; Fixme: some of the cars here aren't valid MIME charsets.  That
;; should only matter with XEmacs, though.
(defvar mm-mime-mule-charset-alist
  `((us-ascii ascii)
    (iso-8859-1 latin-iso8859-1)
    (iso-8859-2 latin-iso8859-2)
    (iso-8859-3 latin-iso8859-3)
    (iso-8859-4 latin-iso8859-4)
    (iso-8859-5 cyrillic-iso8859-5)
    ;; Non-mule (X)Emacs uses the last mule-charset for 8bit characters.
    ;; The fake mule-charset, gnus-koi8-r, tells Gnus that the default
    ;; charset is koi8-r, not iso-8859-5.
    (koi8-r cyrillic-iso8859-5 gnus-koi8-r)
    (iso-8859-6 arabic-iso8859-6)
    (iso-8859-7 greek-iso8859-7)
    (iso-8859-8 hebrew-iso8859-8)
    (iso-8859-9 latin-iso8859-9)
    (iso-8859-14 latin-iso8859-14)
    (iso-8859-15 latin-iso8859-15)
    (viscii vietnamese-viscii-lower)
    (iso-2022-jp latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978)
    (euc-kr korean-ksc5601)
    (gb2312 chinese-gb2312)
    (gbk chinese-gbk)
    (gb18030 gb18030-2-byte
	     gb18030-4-byte-bmp gb18030-4-byte-smp
	     gb18030-4-byte-ext-1 gb18030-4-byte-ext-2)
    (big5 chinese-big5-1 chinese-big5-2)
    (tibetan tibetan)
    (thai-tis620 thai-tis620)
    (windows-1251 cyrillic-iso8859-5)
    (iso-2022-7bit ethiopic arabic-1-column arabic-2-column)
    (iso-2022-jp-2 latin-iso8859-1 greek-iso8859-7
		   latin-jisx0201 japanese-jisx0208-1978
		   chinese-gb2312 japanese-jisx0208
		   korean-ksc5601 japanese-jisx0212)
    (iso-2022-int-1 latin-iso8859-1 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2)
    (iso-2022-int-1 latin-iso8859-1 latin-iso8859-2
		    cyrillic-iso8859-5 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2
		    chinese-cns11643-3 chinese-cns11643-4
		    chinese-cns11643-5 chinese-cns11643-6
		    chinese-cns11643-7)
    (iso-2022-jp-3 latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
		   japanese-jisx0213-1 japanese-jisx0213-2)
    (shift_jis latin-jisx0201 katakana-jisx0201 japanese-jisx0208)
    ,(cond ((fboundp 'unicode-precedence-list)
	    (cons 'utf-8 (delq 'ascii (mapcar 'charset-name
					      (unicode-precedence-list)))))
	   ((or (not (fboundp 'charsetp)) ;; non-Mule case
		(charsetp 'unicode-a)
		(not (mm-coding-system-p 'mule-utf-8)))
	    '(utf-8 unicode-a unicode-b unicode-c unicode-d unicode-e))
	   (t ;; If we have utf-8 we're in Mule 5+.
	    (append '(utf-8)
		    (delete 'ascii
			    (coding-system-get 'mule-utf-8 'safe-charsets))))))
  "Alist of MIME-charset/MULE-charsets.")

(defun mm-enrich-utf-8-by-mule-ucs ()
  "Make the `utf-8' MIME charset usable by the Mule-UCS package.
This function will run when the `un-define' module is loaded under
XEmacs, and fill the `utf-8' entry in `mm-mime-mule-charset-alist'
with Mule charsets.  It is completely useless for Emacs."
  (when (boundp 'unicode-basic-translation-charset-order-list)
    (condition-case nil
	(let ((val (delq
		    'ascii
		    (copy-sequence
		     (symbol-value
		      'unicode-basic-translation-charset-order-list))))
	      (elem (assq 'utf-8 mm-mime-mule-charset-alist)))
	  (if elem
	      (setcdr elem val)
	    (setq mm-mime-mule-charset-alist
		  (nconc mm-mime-mule-charset-alist
			 (list (cons 'utf-8 val))))))
      (error))))

;; Correct by construction, but should be unnecessary for Emacs:
(if (featurep 'xemacs)
    (eval-after-load "un-define" '(mm-enrich-utf-8-by-mule-ucs))
  (when (and (fboundp 'coding-system-list)
	     (fboundp 'sort-coding-systems))
    (let ((css (sort-coding-systems (coding-system-list 'base-only)))
	  cs mime mule alist)
      (while css
	(setq cs (pop css)
	      mime (or (coding-system-get cs :mime-charset); Emacs 23 (unicode)
		       (coding-system-get cs 'mime-charset)))
	(when (and mime
		   (not (eq t (setq mule
				    (coding-system-get cs 'safe-charsets))))
		   (not (assq mime alist)))
	  (push (cons mime (delq 'ascii mule)) alist)))
      (setq mm-mime-mule-charset-alist (nreverse alist)))))

(defvar mm-hack-charsets '(iso-8859-15 iso-2022-jp-2)
  "A list of special charsets.
Valid elements include:
`iso-8859-15'    convert ISO-8859-1, -9 to ISO-8859-15 if ISO-8859-15 exists.
`iso-2022-jp-2'  convert ISO-2022-jp to ISO-2022-jp-2 if ISO-2022-jp-2 exists."
)

(defvar mm-iso-8859-15-compatible
  '((iso-8859-1 "\xA4\xA6\xA8\xB4\xB8\xBC\xBD\xBE")
    (iso-8859-9 "\xA4\xA6\xA8\xB4\xB8\xBC\xBD\xBE\xD0\xDD\xDE\xF0\xFD\xFE"))
  "ISO-8859-15 exchangeable coding systems and inconvertible characters.")

(defvar mm-iso-8859-x-to-15-table
  (and (fboundp 'coding-system-p)
       (mm-coding-system-p 'iso-8859-15)
       (mapcar
	(lambda (cs)
	  (if (mm-coding-system-p (car cs))
	      (let ((c (string-to-char
			(decode-coding-string "\341" (car cs)))))
		(cons (char-charset c)
		      (cons
		       (- (string-to-char
			   (decode-coding-string "\341" 'iso-8859-15)) c)
		       (string-to-list (decode-coding-string (car (cdr cs))
							     (car cs))))))
	    '(gnus-charset 0)))
	mm-iso-8859-15-compatible))
  "A table of the difference character between ISO-8859-X and ISO-8859-15.")

(defcustom mm-coding-system-priorities
  (let ((lang (if (boundp 'current-language-environment)
		  (symbol-value 'current-language-environment))))
    (cond (;; XEmacs without Mule but with `file-coding'.
	   (not lang) nil)
	  ;; In XEmacs 21.5 it may be the one like "Japanese (UTF-8)".
	  ((string-match "\\`Japanese" lang)
	   ;; Japanese users prefer iso-2022-jp to euc-japan or
	   ;; shift_jis, however iso-8859-1 should be used when
	   ;; there are only ASCII text and Latin-1 characters.
	   '(iso-8859-1 iso-2022-jp iso-2022-jp-2 shift_jis utf-8))))
  "Preferred coding systems for encoding outgoing messages.

More than one suitable coding system may be found for some text.
By default, the coding system with the highest priority is used
to encode outgoing messages (see `sort-coding-systems').  If this
variable is set, it overrides the default priority."
  :version "21.2"
  :type '(repeat (symbol :tag "Coding system"))
  :group 'mime)

;; ??
(defvar mm-use-find-coding-systems-region
  (fboundp 'find-coding-systems-region)
  "Use `find-coding-systems-region' to find proper coding systems.

Setting it to nil is useful on Emacsen supporting Unicode if sending
mail with multiple parts is preferred to sending a Unicode one.")

(defvar mm-extra-numeric-entities
  (mapcar
   (lambda (item)
     (cons (car item) (mm-ucs-to-char (cdr item))))
   '((#x80 . #x20AC) (#x82 . #x201A) (#x83 . #x0192) (#x84 . #x201E)
     (#x85 . #x2026) (#x86 . #x2020) (#x87 . #x2021) (#x88 . #x02C6)
     (#x89 . #x2030) (#x8A . #x0160) (#x8B . #x2039) (#x8C . #x0152)
     (#x8E . #x017D) (#x91 . #x2018) (#x92 . #x2019) (#x93 . #x201C)
     (#x94 . #x201D) (#x95 . #x2022) (#x96 . #x2013) (#x97 . #x2014)
     (#x98 . #x02DC) (#x99 . #x2122) (#x9A . #x0161) (#x9B . #x203A)
     (#x9C . #x0153) (#x9E . #x017E) (#x9F . #x0178)))
  "*Alist of extra numeric entities and characters other than ISO 10646.
This table is used for decoding extra numeric entities to characters,
like \"&#128;\" to the euro sign, mainly in html messages.")

;;; Internal variables:

;;; Functions:

(defun mm-mule-charset-to-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (and (fboundp 'find-coding-systems-for-charsets)
	   (fboundp 'sort-coding-systems))
      (let ((css (sort (sort-coding-systems
			(find-coding-systems-for-charsets (list charset)))
		       'mm-sort-coding-systems-predicate))
	    cs mime)
	(while (and (not mime)
		    css)
	  (when (setq cs (pop css))
	    (setq mime (or (coding-system-get cs :mime-charset)
			   (coding-system-get cs 'mime-charset)))))
	mime)
    (let ((alist (mapcar (lambda (cs)
			   (assq cs mm-mime-mule-charset-alist))
			 (sort (mapcar 'car mm-mime-mule-charset-alist)
			       'mm-sort-coding-systems-predicate)))
	  out)
      (while alist
	(when (memq charset (cdar alist))
	  (setq out (caar alist)
		alist nil))
	(pop alist))
      out)))

(eval-and-compile
  (if (featurep 'xemacs)
      (defalias 'mm-enable-multibyte 'ignore)
    (defun mm-enable-multibyte ()
      "Set the multibyte flag of the current buffer.
Only do this if the default value of `enable-multibyte-characters' is
non-nil.  This is a no-op in XEmacs."
      (set-buffer-multibyte 'to)))

  (if (featurep 'xemacs)
      (defalias 'mm-disable-multibyte 'ignore)
    (defun mm-disable-multibyte ()
      "Unset the multibyte flag of in the current buffer.
This is a no-op in XEmacs."
      (set-buffer-multibyte nil))))

(defun mm-preferred-coding-system (charset)
  ;; A typo in some Emacs versions.
  (or (get-charset-property charset 'preferred-coding-system)
      (get-charset-property charset 'prefered-coding-system)))

;; Mule charsets shouldn't be used.
(defsubst mm-guess-charset ()
  "Guess Mule charset from the language environment."
  (or
   mail-parse-mule-charset ;; cached mule-charset
   (progn
     (setq mail-parse-mule-charset
	   (and (boundp 'current-language-environment)
		(car (last
		      (assq 'charset
			    (assoc current-language-environment
				   language-info-alist))))))
     (if (or (not mail-parse-mule-charset)
	     (eq mail-parse-mule-charset 'ascii))
	 (setq mail-parse-mule-charset
	       (or (car (last (assq mail-parse-charset
				    mm-mime-mule-charset-alist)))
		   ;; default
		   'latin-iso8859-1)))
     mail-parse-mule-charset)))

(defun mm-charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defaults to the current point.
If POS is out of range, the value is nil.
If the charset is `composition', return the actual one."
  (let ((char (char-after pos)) charset)
    (if (< (mm-char-int char) 128)
	(setq charset 'ascii)
      ;; charset-after is fake in some Emacsen.
      (setq charset (and (fboundp 'char-charset) (char-charset char)))
      (if (eq charset 'composition)	; Mule 4
	  (let ((p (or pos (point))))
	    (cadr (find-charset-region p (1+ p))))
	(if (and charset (not (memq charset '(ascii eight-bit-control
						    eight-bit-graphic))))
	    charset
	  (mm-guess-charset))))))

(defun mm-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (eq charset 'unknown)
      (error "The message contains non-printable characters, please use attachment"))
  (if (and (fboundp 'coding-system-get) (fboundp 'get-charset-property))
      (or
       (and (mm-preferred-coding-system charset)
	    (or (coding-system-get
		 (mm-preferred-coding-system charset) :mime-charset)
		(coding-system-get
		 (mm-preferred-coding-system charset) 'mime-charset)))
       (and (eq charset 'ascii)
	    'us-ascii)
       (mm-preferred-coding-system charset)
       (mm-mule-charset-to-mime-charset charset))
    ;; This is for XEmacs.
    (mm-mule-charset-to-mime-charset charset)))

;; `delete-dups' is not available in XEmacs 21.4.
(if (fboundp 'delete-dups)
    (defalias 'mm-delete-duplicates 'delete-dups)
  (defun mm-delete-duplicates (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept.

This is a compatibility function for Emacsen without `delete-dups'."
    ;; Code from `subr.el' in Emacs 22:
    (let ((tail list))
      (while tail
	(setcdr tail (delete (car tail) (cdr tail)))
	(setq tail (cdr tail))))
    list))

;; Fixme:  This is used in places when it should be testing the
;; default multibyteness.  See mm-default-multibyte-p.
(eval-and-compile
  (if (and (not (featurep 'xemacs))
	   (boundp 'enable-multibyte-characters))
      (defun mm-multibyte-p ()
	"Non-nil if multibyte is enabled in the current buffer."
	enable-multibyte-characters)
    (defun mm-multibyte-p () (featurep 'mule))))

(defun mm-default-multibyte-p ()
  "Return non-nil if the session is multibyte.
This affects whether coding conversion should be attempted generally."
  (if (featurep 'mule)
      (if (boundp 'enable-multibyte-characters)
	  (default-value 'enable-multibyte-characters)
	t)))

(defun mm-iso-8859-x-to-15-region (&optional b e)
  (if (fboundp 'char-charset)
      (let (charset item c inconvertible)
	(save-restriction
	  (if e (narrow-to-region b e))
	  (goto-char (point-min))
	  (skip-chars-forward "\0-\177")
	  (while (not (eobp))
	    (cond
	     ((not (setq item (assq (char-charset (setq c (char-after)))
				    mm-iso-8859-x-to-15-table)))
	      (forward-char))
	     ((memq c (cdr (cdr item)))
	      (setq inconvertible t)
	      (forward-char))
	     (t
	      (insert-before-markers (prog1 (+ c (car (cdr item)))
				       (delete-char 1)))))
	    (skip-chars-forward "\0-\177")))
	(not inconvertible))))

(defun mm-sort-coding-systems-predicate (a b)
  (let ((priorities
	 (mapcar (lambda (cs)
		   ;; Note: invalid entries are dropped silently
		   (and (setq cs (mm-coding-system-p cs))
			(coding-system-base cs)))
		 mm-coding-system-priorities)))
    (and (setq a (mm-coding-system-p a))
	 (if (setq b (mm-coding-system-p b))
	     (> (length (memq (coding-system-base a) priorities))
		(length (memq (coding-system-base b) priorities)))
	   t))))

(eval-when-compile
  (autoload 'latin-unity-massage-name "latin-unity")
  (autoload 'latin-unity-maybe-remap "latin-unity")
  (autoload 'latin-unity-representations-feasible-region "latin-unity")
  (autoload 'latin-unity-representations-present-region "latin-unity"))

(defvar latin-unity-coding-systems)
(defvar latin-unity-ucs-list)

(defun mm-xemacs-find-mime-charset-1 (begin end)
  "Determine which MIME charset to use to send region as message.
This uses the XEmacs-specific latin-unity package to better handle the
case where identical characters from diverse ISO-8859-? character sets
can be encoded using a single one of the corresponding coding systems.

It treats `mm-coding-system-priorities' as the list of preferred
coding systems; a useful example setting for this list in Western
Europe would be '(iso-8859-1 iso-8859-15 utf-8), which would default
to the very standard Latin 1 coding system, and only move to coding
systems that are less supported as is necessary to encode the
characters that exist in the buffer.

Latin Unity doesn't know about those non-ASCII Roman characters that
are available in various East Asian character sets.  As such, its
behavior if you have a JIS 0212 LATIN SMALL LETTER A WITH ACUTE in a
buffer and it can otherwise be encoded as Latin 1, won't be ideal.
But this is very much a corner case, so don't worry about it."
  (let ((systems mm-coding-system-priorities) csets psets curset)

    ;; Load the Latin Unity library, if available.
    (when (and (not (featurep 'latin-unity)) (locate-library "latin-unity"))
      (require 'latin-unity))

    ;; Now, can we use it?
    (if (featurep 'latin-unity)
	(progn
	  (setq csets (latin-unity-representations-feasible-region begin end)
		psets (latin-unity-representations-present-region begin end))

	  (catch 'done

	    ;; Pass back the first coding system in the preferred list
	    ;; that can encode the whole region.
	    (dolist (curset systems)
	      (setq curset (latin-unity-massage-name 'buffer-default curset))

	      ;; If the coding system is a universal coding system, then
	      ;; it can certainly encode all the characters in the region.
	      (if (memq curset latin-unity-ucs-list)
		  (throw 'done (list curset)))

	      ;; If a coding system isn't universal, and isn't in
	      ;; the list that latin unity knows about, we can't
	      ;; decide whether to use it here. Leave that until later
	      ;; in `mm-find-mime-charset-region' function, whence we
	      ;; have been called.
	      (unless (memq curset latin-unity-coding-systems)
		(throw 'done nil))

	      ;; Right, we know about this coding system, and it may
	      ;; conceivably be able to encode all the characters in
	      ;; the region.
	      (if (latin-unity-maybe-remap begin end curset csets psets t)
		  (throw 'done (list curset))))

	    ;; Can't encode using anything from the
	    ;; `mm-coding-system-priorities' list.
	    ;; Leave `mm-find-mime-charset' to do most of the work.
	    nil))

      ;; Right, latin unity isn't available; let `mm-find-charset-region'
      ;; take its default action, which equally applies to GNU Emacs.
      nil)))

(defmacro mm-xemacs-find-mime-charset (begin end)
  (when (featurep 'xemacs)
    `(and (featurep 'mule) (mm-xemacs-find-mime-charset-1 ,begin ,end))))

(declare-function mm-delete-duplicates "mm-util" (list))

(defun mm-find-mime-charset-region (b e &optional hack-charsets)
  "Return the MIME charsets needed to encode the region between B and E.
nil means ASCII, a single-element list represents an appropriate MIME
charset, and a longer list means no appropriate charset."
  (let (charsets)
    ;; The return possibilities of this function are a mess...
    (or (and (mm-multibyte-p)
	     mm-use-find-coding-systems-region
	     ;; Find the mime-charset of the most preferred coding
	     ;; system that has one.
	     (let ((systems (find-coding-systems-region b e)))
	       (when mm-coding-system-priorities
		 (setq systems
		       (sort systems 'mm-sort-coding-systems-predicate)))
	       (setq systems (delq 'compound-text systems))
	       (unless (equal systems '(undecided))
		 (while systems
		   (let* ((head (pop systems))
			  (cs (or (coding-system-get head :mime-charset)
				  (coding-system-get head 'mime-charset))))
		     ;; The mime-charset (`x-ctext') of
		     ;; `compound-text' is not in the IANA list.  We
		     ;; shouldn't normally use anything here with a
		     ;; mime-charset having an `x-' prefix.
		     ;; Fixme:  Allow this to be overridden, since
		     ;; there is existing use of x-ctext.
		     ;; Also people apparently need the coding system
		     ;; `iso-2022-jp-3' (which Mule-UCS defines with
		     ;; mime-charset, though it's not valid).
		     (if (and cs
			      (not (string-match "^[Xx]-" (symbol-name cs)))
			      ;; UTF-16 of any variety is invalid for
			      ;; text parts and, unfortunately, has
			      ;; mime-charset defined both in Mule-UCS
			      ;; and versions of Emacs.  (The name
			      ;; might be `mule-utf-16...'  or
			      ;; `utf-16...'.)
			      (not (string-match "utf-16" (symbol-name cs))))
			 (setq systems nil
			       charsets (list cs))))))
	       charsets))
	;; If we're XEmacs, and some coding system is appropriate,
	;; mm-xemacs-find-mime-charset will return an appropriate list.
	;; Otherwise, we'll get nil, and the next setq will get invoked.
	(setq charsets (mm-xemacs-find-mime-charset b e))

	;; Fixme: won't work for unibyte Emacs 23:

	;; We're not multibyte, or a single coding system won't cover it.
	(setq charsets
	      (mm-delete-duplicates
	       (mapcar 'mm-mime-charset
		       (delq 'ascii
			     (mm-find-charset-region b e))))))
    (if (and (> (length charsets) 1)
	     (memq 'iso-8859-15 charsets)
	     (memq 'iso-8859-15 hack-charsets)
	     (save-excursion (mm-iso-8859-x-to-15-region b e)))
	(dolist (x mm-iso-8859-15-compatible)
	  (setq charsets (delq (car x) charsets))))
    (if (and (memq 'iso-2022-jp-2 charsets)
	     (memq 'iso-2022-jp-2 hack-charsets))
	(setq charsets (delq 'iso-2022-jp charsets)))
    ;; Attempt to reduce the number of charsets if utf-8 is available.
    (if (and (featurep 'xemacs)
	     (> (length charsets) 1)
	     (mm-coding-system-p 'utf-8))
	(let ((mm-coding-system-priorities
	       (cons 'utf-8 mm-coding-system-priorities)))
	  (setq charsets
		(mm-delete-duplicates
		 (mapcar 'mm-mime-charset
			 (delq 'ascii
			       (mm-find-charset-region b e)))))))
    charsets))

(defmacro mm-with-unibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
Use unibyte mode for this."
  `(with-temp-buffer
     (mm-disable-multibyte)
     ,@forms))
(put 'mm-with-unibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-buffer 'edebug-form-spec '(body))

(defmacro mm-with-multibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
Use multibyte mode for this."
  `(with-temp-buffer
     (mm-enable-multibyte)
     ,@forms))
(put 'mm-with-multibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-multibyte-buffer 'edebug-form-spec '(body))

(defmacro mm-with-unibyte-current-buffer (&rest forms)
  "Evaluate FORMS with current buffer temporarily made unibyte.
Equivalent to `progn' in XEmacs.

Note: We recommend not using this macro any more; there should be
better ways to do a similar thing.  The previous version of this macro
bound the default value of `enable-multibyte-characters' to nil while
evaluating FORMS but it is no longer done.  So, some programs assuming
it if any may malfunction."
  (if (featurep 'xemacs)
      `(progn ,@forms)
    (let ((multibyte (make-symbol "multibyte")))
      `(let ((,multibyte enable-multibyte-characters))
	 (when ,multibyte
	   (set-buffer-multibyte nil))
	 (prog1
	     (progn ,@forms)
	   (when ,multibyte
	     (set-buffer-multibyte t)))))))
(put 'mm-with-unibyte-current-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-current-buffer 'edebug-form-spec '(body))

(defun mm-find-charset-region (b e)
  "Return a list of Emacs charsets in the region B to E."
  (cond
   ((and (mm-multibyte-p)
	 (fboundp 'find-charset-region))
    ;; Remove composition since the base charsets have been included.
    ;; Remove eight-bit-*, treat them as ascii.
    (let ((css (find-charset-region b e)))
      (dolist (cs
	       '(composition eight-bit-control eight-bit-graphic control-1)
	       css)
	(setq css (delq cs css)))))
   (t
    ;; We are in a unibyte buffer or XEmacs non-mule, so we futz around a bit.
    (save-excursion
      (save-restriction
	(narrow-to-region b e)
	(goto-char (point-min))
	(skip-chars-forward "\0-\177")
	(if (eobp)
	    '(ascii)
	  (let (charset)
	    (setq charset
		  (and (boundp 'current-language-environment)
		       (car (last (assq 'charset
					(assoc current-language-environment
					       language-info-alist))))))
	    (if (eq charset 'ascii) (setq charset nil))
	    (or charset
		(setq charset
		      (car (last (assq mail-parse-charset
				       mm-mime-mule-charset-alist)))))
	    (list 'ascii (or charset 'latin-iso8859-1)))))))))

(defun mm-auto-mode-alist ()
  "Return an `auto-mode-alist' with only the .gz (etc) thingies."
  (let ((alist auto-mode-alist)
	out)
    (while alist
      (when (listp (cdar alist))
	(push (car alist) out))
      (pop alist))
    (nreverse out)))

(defvar mm-inhibit-file-name-handlers
  '(jka-compr-handler image-file-handler epa-file-handler)
  "A list of handlers doing (un)compression (etc) thingies.")

(defun mm-insert-file-contents (filename &optional visit beg end replace
					 inhibit)
  "Like `insert-file-contents', but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
`find-file-hooks', etc.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'.
  This function ensures that none of these modifications will take place."
  (letf* ((format-alist nil)
          (auto-mode-alist (if inhibit nil (mm-auto-mode-alist)))
          ((default-value 'major-mode) 'fundamental-mode)
          (enable-local-variables nil)
          (after-insert-file-functions nil)
          (enable-local-eval nil)
          (inhibit-file-name-operation (if inhibit
                                           'insert-file-contents
                                         inhibit-file-name-operation))
          (inhibit-file-name-handlers
           (if inhibit
               (append mm-inhibit-file-name-handlers
                       inhibit-file-name-handlers)
             inhibit-file-name-handlers))
          (ffh (if (boundp 'find-file-hook)
                   'find-file-hook
                 'find-file-hooks))
          (val (symbol-value ffh)))
    (set ffh nil)
    (unwind-protect
	(insert-file-contents filename visit beg end replace)
      (set ffh val))))

(defun mm-append-to-file (start end filename &optional codesys inhibit)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write.
Optional fourth argument specifies the coding system to use when
encoding the file.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'."
  (let ((coding-system-for-write
	 (or codesys mm-text-coding-system-for-write
	     mm-text-coding-system))
	(inhibit-file-name-operation (if inhibit
					 'append-to-file
				       inhibit-file-name-operation))
	(inhibit-file-name-handlers
	 (if inhibit
	     (append mm-inhibit-file-name-handlers
		     inhibit-file-name-handlers)
	   inhibit-file-name-handlers)))
    (write-region start end filename t 'no-message)
    (message "Appended to %s" filename)))

(defun mm-write-region (start end filename &optional append visit lockname
			      coding-system inhibit)

  "Like `write-region'.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'."
  (let ((coding-system-for-write
	 (or coding-system mm-text-coding-system-for-write
	     mm-text-coding-system))
	(inhibit-file-name-operation (if inhibit
					 'write-region
				       inhibit-file-name-operation))
	(inhibit-file-name-handlers
	 (if inhibit
	     (append mm-inhibit-file-name-handlers
		     inhibit-file-name-handlers)
	   inhibit-file-name-handlers)))
    (write-region start end filename append visit lockname)))

(autoload 'gmm-write-region "gmm-utils")

;; It is not a MIME function, but some MIME functions use it.
(if (and (fboundp 'make-temp-file)
	 (ignore-errors
	   (let ((def (symbol-function 'make-temp-file)))
	     (and (byte-code-function-p def)
		  (setq def (if (fboundp 'compiled-function-arglist)
				;; XEmacs
				(eval (list 'compiled-function-arglist def))
			      (aref def 0)))
		  (>= (length def) 4)
		  (eq (nth 3 def) 'suffix)))))
    (defalias 'mm-make-temp-file 'make-temp-file)
  ;; Stolen (and modified for XEmacs) from Emacs 22.
  (defun mm-make-temp-file (prefix &optional dir-flag suffix)
    "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
    (let ((umask (default-file-modes))
	  file)
      (unwind-protect
	  (progn
	    ;; Create temp files with strict access rights.  It's easy to
	    ;; loosen them later, whereas it's impossible to close the
	    ;; time-window of loose permissions otherwise.
	    (set-default-file-modes 448)
	    (while (condition-case err
		       (progn
			 (setq file
			       (make-temp-name
				(expand-file-name
				 prefix
				 (if (fboundp 'temp-directory)
				     ;; XEmacs
				     (temp-directory)
				   temporary-file-directory))))
			 (if suffix
			     (setq file (concat file suffix)))
			 (if dir-flag
			     (make-directory file)
			   ;; NOTE: This is unsafe if Emacs 20
			   ;; users and XEmacs users don't use
			   ;; a secure temp directory.
			   (gmm-write-region "" nil file nil 'silent
					     nil 'excl))
			 nil)
		     (file-already-exists t)
		     ;; The XEmacs version of `make-directory' issues
		     ;; `file-error'.
		     (file-error (or (and (featurep 'xemacs)
					  (file-exists-p file))
				     (signal (car err) (cdr err)))))
	      ;; the file was somehow created by someone else between
	      ;; `make-temp-name' and `write-region', let's try again.
	      nil)
	    file)
	;; Reset the umask.
	(set-default-file-modes umask)))))

(defvar mm-image-load-path-cache nil)

(defun mm-image-load-path (&optional package)
  (if (and mm-image-load-path-cache
	   (equal load-path (car mm-image-load-path-cache)))
      (cdr mm-image-load-path-cache)
    (let (dir result)
      (dolist (path load-path)
	(when (and path
		   (file-directory-p
		    (setq dir (concat (file-name-directory
				       (directory-file-name path))
				      "etc/images/" (or package "gnus/")))))
	  (push dir result)))
      (setq result (nreverse result)
	    mm-image-load-path-cache (cons load-path result))
      result)))

;; Fixme: This doesn't look useful where it's used.
(if (fboundp 'detect-coding-region)
    (defun mm-detect-coding-region (start end)
      "Like `detect-coding-region' except returning the best one."
      (let ((coding-systems
	     (detect-coding-region start end)))
	(or (car-safe coding-systems)
	    coding-systems)))
  (defun mm-detect-coding-region (start end)
    (let ((point (point)))
      (goto-char start)
      (skip-chars-forward "\0-\177" end)
      (prog1
	  (if (eq (point) end) 'ascii (mm-guess-charset))
	(goto-char point)))))

(declare-function mm-detect-coding-region "mm-util" (start end))

(if (fboundp 'coding-system-get)
    (defun mm-detect-mime-charset-region (start end)
      "Detect MIME charset of the text in the region between START and END."
      (let ((cs (mm-detect-coding-region start end)))
	(or (coding-system-get cs :mime-charset)
	    (coding-system-get cs 'mime-charset))))
  (defun mm-detect-mime-charset-region (start end)
    "Detect MIME charset of the text in the region between START and END."
    (let ((cs (mm-detect-coding-region start end)))
      cs)))

(eval-when-compile
  (unless (fboundp 'coding-system-to-mime-charset)
    (defalias 'coding-system-to-mime-charset 'ignore)))

(defun mm-coding-system-to-mime-charset (coding-system)
  "Return the MIME charset corresponding to CODING-SYSTEM.
To make this function work with XEmacs, the APEL package is required."
  (when coding-system
    (or (and (fboundp 'coding-system-get)
	     (or (coding-system-get coding-system :mime-charset)
		 (coding-system-get coding-system 'mime-charset)))
	(and (featurep 'xemacs)
	     (or (and (fboundp 'coding-system-to-mime-charset)
		      (not (eq (symbol-function 'coding-system-to-mime-charset)
			       'ignore)))
		 (and (condition-case nil
			  (require 'mcharset)
			(error nil))
		      (fboundp 'coding-system-to-mime-charset)))
	     (coding-system-to-mime-charset coding-system)))))

(eval-when-compile
  (require 'jka-compr))

(defun mm-decompress-buffer (filename &optional inplace force)
  "Decompress buffer's contents, depending on jka-compr.
Only when FORCE is t or `auto-compression-mode' is enabled and FILENAME
agrees with `jka-compr-compression-info-list', decompression is done.
Signal an error if FORCE is neither nil nor t and compressed data are
not decompressed because `auto-compression-mode' is disabled.
If INPLACE is nil, return decompressed data or nil without modifying
the buffer.  Otherwise, replace the buffer's contents with the
decompressed data.  The buffer's multibyteness must be turned off."
  (when (and filename
	     (if force
		 (prog1 t (require 'jka-compr))
	       (and (fboundp 'jka-compr-installed-p)
		    (jka-compr-installed-p))))
    (let ((info (jka-compr-get-compression-info filename)))
      (when info
	(unless (or (memq force (list nil t))
		    (jka-compr-installed-p))
	  (error ""))
	(let ((prog (jka-compr-info-uncompress-program info))
	      (args (jka-compr-info-uncompress-args info))
	      (msg (format "%s %s..."
			   (jka-compr-info-uncompress-message info)
			   filename))
	      (err-file (jka-compr-make-temp-name))
	      (cur (current-buffer))
	      (coding-system-for-read mm-binary-coding-system)
	      (coding-system-for-write mm-binary-coding-system)
	      retval err-msg)
	  (message "%s" msg)
	  (mm-with-unibyte-buffer
	    (insert-buffer-substring cur)
	    (condition-case err
		(progn
		  (unless (memq (apply 'call-process-region
				       (point-min) (point-max)
				       prog t (list t err-file) nil args)
				jka-compr-acceptable-retval-list)
		    (erase-buffer)
		    (insert (mapconcat 'identity
				       (split-string
					(prog2
					    (insert-file-contents err-file)
					    (buffer-string)
					  (erase-buffer)) t)
				       " ")
			    "\n")
		    (setq err-msg
			  (format "Error while executing \"%s %s < %s\""
				  prog (mapconcat 'identity args " ")
				  filename)))
		  (setq retval (buffer-string)))
	      (error
	       (setq err-msg (error-message-string err)))))
	  (when (file-exists-p err-file)
	    (ignore-errors (delete-file err-file)))
	  (when inplace
	    (unless err-msg
	      (delete-region (point-min) (point-max))
	      (insert retval))
	    (setq retval nil))
	  (message "%s" (or err-msg (concat msg "done")))
	  retval)))))

(eval-when-compile
  (unless (fboundp 'coding-system-name)
    (defalias 'coding-system-name 'ignore))
  (unless (fboundp 'find-file-coding-system-for-read-from-filename)
    (defalias 'find-file-coding-system-for-read-from-filename 'ignore))
  (unless (fboundp 'find-operation-coding-system)
    (defalias 'find-operation-coding-system 'ignore)))

(defun mm-find-buffer-file-coding-system (&optional filename)
  "Find coding system used to decode the contents of the current buffer.
This function looks for the coding system magic cookie or examines the
coding system specified by `file-coding-system-alist' being associated
with FILENAME which defaults to `buffer-file-name'.  Data compressed by
gzip, bzip2, etc. are allowed."
  (unless filename
    (setq filename buffer-file-name))
  (save-excursion
    (let ((decomp (unless ;; No worth to examine charset of tar files.
		      (and filename
			   (string-match
			    "\\.\\(?:tar\\.[^.]+\\|tbz\\|tgz\\)\\'"
			    filename))
		    (mm-decompress-buffer filename nil t))))
      (when decomp
	(set-buffer (generate-new-buffer " *temp*"))
        (mm-disable-multibyte)
	(insert decomp)
	(setq filename (file-name-sans-extension filename)))
      (goto-char (point-min))
      (unwind-protect
	  (cond
	   ((boundp 'set-auto-coding-function) ;; Emacs
	    (if filename
		(or (funcall (symbol-value 'set-auto-coding-function)
			     filename (- (point-max) (point-min)))
		    (car (find-operation-coding-system 'insert-file-contents
						       filename)))
	      (let (auto-coding-alist)
		(condition-case nil
		    (funcall (symbol-value 'set-auto-coding-function)
			     nil (- (point-max) (point-min)))
		  (error nil)))))
	   ((and (featurep 'xemacs) (featurep 'file-coding)) ;; XEmacs
	    (let ((case-fold-search t)
		  (end (point-at-eol))
		  codesys start)
	      (or
	       (and (re-search-forward "-\\*-+[\t ]*" end t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "[\t ]*-+\\*-" end t))
		    (progn
		      (setq end (match-beginning 0))
		      (goto-char start)
		      (or (looking-at "coding:[\t ]*\\([^\t ;]+\\)")
			  (re-search-forward
			   "[\t ;]+coding:[\t ]*\\([^\t ;]+\\)"
			   end t)))
		    (find-coding-system (setq codesys
					      (intern (match-string 1))))
		    codesys)
	       (and (re-search-forward "^[\t ]*;+[\t ]*Local[\t ]+Variables:"
				       nil t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "^[\t ]*;+[\t ]*End:" nil t))
		    (progn
		      (setq end (match-beginning 0))
		      (goto-char start)
		      (re-search-forward
		       "^[\t ]*;+[\t ]*coding:[\t ]*\\([^\t\n\r ]+\\)"
		       end t))
		    (find-coding-system (setq codesys
					      (intern (match-string 1))))
		    codesys)
	       (and (progn
		      (goto-char (point-min))
		      (setq case-fold-search nil)
		      (re-search-forward "^;;;coding system: "
					 ;;(+ (point-min) 3000) t))
					 nil t))
		    (looking-at "[^\t\n\r ]+")
		    (find-coding-system
		     (setq codesys (intern (match-string 0))))
		    codesys)
	       (and filename
		    (setq codesys
			  (find-file-coding-system-for-read-from-filename
			   filename))
		    (coding-system-name (coding-system-base codesys)))))))
	(when decomp
	  (kill-buffer (current-buffer)))))))

(provide 'mm-util)

;;; mm-util.el ends here
