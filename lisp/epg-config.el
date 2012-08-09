;;; epg-config.el --- configuration of the EasyPG Library

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Package: epg

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

;;; Code:

(defconst epg-package-name "epg"
  "Name of this package.")

(defconst epg-version-number "1.0.0"
  "Version number of this package.")

(defconst epg-bug-report-address "ueno@unixuser.org"
  "Report bugs to this address.")

(defgroup epg ()
  "The EasyPG library."
  :version "23.1"
  :group 'data)

(defcustom epg-gpg-program (or (executable-find "gpg")
			       (executable-find "gpg2")
			       "gpg")
  "The `gpg' executable."
  :group 'epg
  :type 'string)

(defcustom epg-gpgsm-program "gpgsm"
  "The `gpgsm' executable."
  :group 'epg
  :type 'string)

(defcustom epg-gpg-home-directory nil
  "The directory which contains the configuration files of `epg-gpg-program'."
  :group 'epg
  :type '(choice (const :tag "Default" nil) directory))

(defcustom epg-passphrase-coding-system nil
  "Coding system to use with messages from `epg-gpg-program'."
  :group 'epg
  :type 'symbol)

(defcustom epg-debug nil
  "If non-nil, debug output goes to the \" *epg-debug*\" buffer.
Note that the buffer name starts with a space."
  :group 'epg
  :type 'boolean)

(defconst epg-gpg-minimum-version "1.4.3")

;;;###autoload
(defun epg-configuration ()
  "Return a list of internal configuration parameters of `epg-gpg-program'."
  (let (config groups type args)
    (with-temp-buffer
      (apply #'call-process epg-gpg-program nil (list t nil) nil
	     (append (if epg-gpg-home-directory
			 (list "--homedir" epg-gpg-home-directory))
		     '("--with-colons" "--list-config")))
      (goto-char (point-min))
      (while (re-search-forward "^cfg:\\([^:]+\\):\\(.*\\)" nil t)
	(setq type (intern (match-string 1))
	      args (match-string 2))
	(cond
	 ((eq type 'group)
	  (if (string-match "\\`\\([^:]+\\):" args)
		  (setq groups
			(cons (cons (downcase (match-string 1 args))
				    (delete "" (split-string
						(substring args
							   (match-end 0))
						";")))
			      groups))
	    (if epg-debug
		(message "Invalid group configuration: %S" args))))
	 ((memq type '(pubkey cipher digest compress))
	  (if (string-match "\\`\\([0-9]+\\)\\(;[0-9]+\\)*" args)
	      (setq config
		    (cons (cons type
				(mapcar #'string-to-number
					(delete "" (split-string args ";"))))
			  config))
	    (if epg-debug
		(message "Invalid %S algorithm configuration: %S"
			 type args))))
	 (t
	  (setq config (cons (cons type args) config))))))
    (if groups
	(cons (cons 'groups groups) config)
      config)))

(defun epg-config--parse-version (string)
  (let ((index 0)
	version)
    (while (eq index (string-match "\\([0-9]+\\)\\.?" string index))
      (setq version (cons (string-to-number (match-string 1 string))
			  version)
	    index (match-end 0)))
    (nreverse version)))

(defun epg-config--compare-version (v1 v2)
  (while (and v1 v2 (= (car v1) (car v2)))
    (setq v1 (cdr v1) v2 (cdr v2)))
  (- (or (car v1) 0) (or (car v2) 0)))

;;;###autoload
(defun epg-check-configuration (config &optional minimum-version)
  "Verify that a sufficient version of GnuPG is installed."
  (let ((entry (assq 'version config))
	version)
    (unless (and entry
		 (stringp (cdr entry)))
      (error "Undetermined version: %S" entry))
    (setq version (epg-config--parse-version (cdr entry))
	  minimum-version (epg-config--parse-version
			   (or minimum-version
			       epg-gpg-minimum-version)))
    (unless (>= (epg-config--compare-version version minimum-version) 0)
      (error "Unsupported version: %s" (cdr entry)))))

;;;###autoload
(defun epg-expand-group (config group)
  "Look at CONFIG and try to expand GROUP."
  (let ((entry (assq 'groups config)))
    (if (and entry
	     (setq entry (assoc (downcase group) (cdr entry))))
	(cdr entry))))

(provide 'epg-config)

;;; epg-config.el ends here
