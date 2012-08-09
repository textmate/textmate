;;; dns-mode.el --- a mode for viewing/editing Domain Name System master files

;; Copyright (C) 2000-2001, 2004-2012  Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: DNS master zone file SOA comm

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

;; Use M-x dns-mode RET to invoke in master files.
;;
;; C-c C-s  Increment SOA serial.
;;          Understands YYYYMMDDNN, Unix time, and serial number formats,
;;          and complains if it fail to find SOA serial.

;;; References:

;; RFC 1034, "DOMAIN NAMES - CONCEPTS AND FACILITIES", P. Mockapetris.
;; RFC 1035, "DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION", P. Mockapetris.

;;; Release history:

;; 2004-09-11  Posted on gnu.emacs.sources.
;; 2004-09-13  Ported to XEmacs.
;; 2004-09-14  Installed in Emacs CVS.

;;; Code:

(defgroup dns-mode nil
  "DNS master file mode configuration."
  :group 'data)

(defconst dns-mode-classes '("IN" "CS" "CH" "HS")
  "List of strings with known DNS classes.")

(defconst dns-mode-types '("A" "NS" "MD" "MF" "CNAME" "SOA" "MB" "MG" "MR"
			   "NULL" "WKS" "PTR" "HINFO" "MINFO" "MX" "TXT"
			   "RP" "AFSDB" "X25" "ISDN" "RT" "NSAP" "NSAP"
			   "SIG" "KEY" "PX" "GPOS" "AAAA" "LOC" "NXT"
			   "EID" "NIMLOC" "SRV" "ATMA" "NAPTR" "KX" "CERT"
			   "A6" "DNAME" "SINK" "OPT" "APL" "DS" "SSHFP"
			   "RRSIG" "NSEC" "DNSKEY" "UINFO" "UID" "GID"
			   "UNSPEC" "TKEY" "TSIG" "IXFR" "AXFR" "MAILB"
			   "MAILA")
  "List of strings with known DNS types.")

;; Font lock.

(defvar dns-mode-control-entity-face 'font-lock-keyword-face
  "Name of face used for control entities, e.g. $ORIGIN.")

(defvar dns-mode-bad-control-entity-face 'font-lock-warning-face
  "Name of face used for non-standard control entities, e.g. $FOO.")

(defvar dns-mode-type-face 'font-lock-type-face
  "Name of face used for DNS types, e.g., SOA.")

(defvar dns-mode-class-face 'font-lock-constant-face
  "Name of face used for DNS classes, e.g., IN.")

(defcustom dns-mode-font-lock-keywords
  `(("^$ORIGIN" 0 ,dns-mode-control-entity-face)
    ("^$INCLUDE" 0 ,dns-mode-control-entity-face)
    ("^$[a-z0-9A-Z]+" 0 ,dns-mode-bad-control-entity-face)
    (,(regexp-opt dns-mode-classes) 0 ,dns-mode-class-face)
    (,(regexp-opt dns-mode-types) 0 ,dns-mode-type-face))
  "Font lock keywords used to highlight text in DNS master file mode."
  :type 'sexp
  :group 'dns-mode)

(defcustom dns-mode-soa-auto-increment-serial t
  "Whether to increment the SOA serial number automatically.

If this variable is t, the serial number is incremented upon each save of
the file.  If it is `ask', Emacs asks for confirmation whether it should
increment the serial upon saving.  If nil, serials must be incremented
manually with \\[dns-mode-soa-increment-serial]."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Ask" ask)
		 (const :tag "Never" nil))
  :group 'dns-mode)

;; Syntax table.

(defvar dns-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<   " table)
    (modify-syntax-entry ?\n ">   " table)
    table)
  "Syntax table in use in DNS master file buffers.")

;; Keymap.

(defvar dns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'dns-mode-soa-increment-serial)
    map)
  "Keymap for DNS master file mode.")

;; Menu.

(defvar dns-mode-menu nil
  "Menubar used in DNS master file mode.")

(easy-menu-define dns-mode-menu dns-mode-map
  "DNS Menu."
  '("DNS"
    ["Increment SOA serial" dns-mode-soa-increment-serial t]))

;; Mode.

;;;###autoload
(define-derived-mode dns-mode text-mode "DNS"
  "Major mode for viewing and editing DNS master files.
This mode is inherited from text mode.  It add syntax
highlighting, and some commands for handling DNS master files.
Its keymap inherits from `text-mode' and it has the same
variables for customizing indentation.  It has its own abbrev
table and its own syntax table.

Turning on DNS mode runs `dns-mode-hook'."
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) ";+ *")
  (unless (featurep 'xemacs)
    (set (make-local-variable 'font-lock-defaults)
	 '(dns-mode-font-lock-keywords nil nil ((?_ . "w")))))
  (add-hook 'before-save-hook 'dns-mode-soa-maybe-increment-serial
	    nil t)
  (easy-menu-add dns-mode-menu dns-mode-map))

;;;###autoload (defalias 'zone-mode 'dns-mode)

;; Tools.

;;;###autoload
(defun dns-mode-soa-increment-serial ()
  "Locate SOA record and increment the serial field."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
	     (concat "^\\(\\(\\([^ \t]+[ \t]+\\)?[^ \t]+"
		     "[ \t]+\\)?[^ \t]+[ \t]+\\)?SOA") nil t)
      (error "Cannot locate SOA record"))
    (if (re-search-forward (concat "\\<\\("
				   ;; year
				   "\\(198\\|199\\|20[0-9]\\)[0-9]"
				   ;; month
				   "\\(0[0-9]\\|10\\|11\\|12\\)"
				   ;; day
				   "\\([012][0-9]\\|30\\|31\\)"
				   ;; counter
				   "\\([0-9]\\{1,3\\}\\)"
				   "\\)\\>")
			   nil t)
	;; YYYYMMDDIII format, one to three I's.
	(let* ((serial (match-string 1))
	       (counterstr (match-string 5))
	       (counter (string-to-number counterstr))
	       (now (format-time-string "%Y%m%d"))
	       (nowandoldserial (concat now counterstr)))
	  (if (string< serial nowandoldserial)
	      (let ((new (format "%s00" now)))
		(replace-match new nil nil nil 1)
		(message "Replaced old serial %s with %s" serial new))
	    (if (string= serial nowandoldserial)
		(let ((new (format (format "%%s%%0%dd" (length counterstr))
				   now (1+ counter))))
		  (replace-match new nil nil nil 1)
		  (message "Replaced old serial %s with %s" serial new))
	      (error "Current SOA serial is in the future"))))
      (if (re-search-forward "\\<\\([0-9]\\{9,10\\}\\)\\>" nil t)
	  ;; Unix time
	  (let* ((serial (match-string 1))
		 (new (format-time-string "%s")))
	    (if (not (string< serial new))
		(error "Current SOA serial is in the future")
	      (replace-match new nil nil nil 1)
	      (message "Replaced old serial %s with %s" serial new)))
	(if (re-search-forward "\\<\\([0-9]+\\)\\>" nil t)
	    ;; Just any serial number.
	    (let* ((serial (match-string 1))
		   (new (format "%d" (1+ (string-to-number serial)))))
	      (replace-match new nil nil nil 1)
	      (message "Replaced old serial %s with %s" serial new))
	  (error "Cannot locate serial number in SOA record"))))))

(defun dns-mode-soa-maybe-increment-serial ()
  "Increment SOA serial if needed.

This function is run from `before-save-hook'."
  (when (and (buffer-modified-p)
	     dns-mode-soa-auto-increment-serial
	     (or (eq dns-mode-soa-auto-increment-serial t)
		 (y-or-n-p "Increment SOA serial? ")))
    ;; If `dns-mode-soa-increment-serial' signals an error saving will
    ;; fail but that probably means that the serial should be fixed to
    ;; comply with the RFC anyway! -rfr
    (progn (dns-mode-soa-increment-serial)
	   ;; We return nil in case this is used in write-contents-functions.
	   nil)))

(provide 'dns-mode)

;;; dns-mode.el ends here
