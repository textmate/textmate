;;; pgg-pgp5.el --- PGP 5.* support for PGG.

;; Copyright (C) 1999-2000, 2002-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/11/02
;; Keywords: PGP, OpenPGP
;; Package: pgg
;; Obsolete-since: 24.1

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

(eval-when-compile
  (require 'cl)				; for pgg macros
  (require 'pgg))

(defgroup pgg-pgp5 ()
  "PGP 5.* interface."
  :group 'pgg)

(defcustom pgg-pgp5-pgpe-program "pgpe"
  "PGP 5.* 'pgpe' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgps-program "pgps"
  "PGP 5.* 'pgps' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgpk-program "pgpk"
  "PGP 5.* 'pgpk' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgpv-program "pgpv"
  "PGP 5.* 'pgpv' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-shell-file-name "/bin/sh"
  "File name to load inferior shells from.
Bourne shell or its equivalent \(not tcsh) is needed for \"2>\"."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-extra-args nil
  "Extra arguments for every PGP 5.* invocation."
  :group 'pgg-pgp5
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Arguments")))

(defvar pgg-pgp5-user-id nil
  "PGP 5.* ID of your default identity.")

(defun pgg-pgp5-process-region (start end passphrase program args)
  (let* ((errors-file-name (pgg-make-temp-file "pgg-errors"))
	 (args
	  (append args
		  pgg-pgp5-extra-args
		  (list (concat "2>" errors-file-name))))
	 (shell-file-name pgg-pgp5-shell-file-name)
	 (shell-command-switch pgg-pgp5-shell-command-switch)
	 (process-environment process-environment)
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (process-connection-type nil)
	 process status exit-status)
    (with-current-buffer (get-buffer-create output-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (when passphrase
      (setenv "PGPPASSFD" "0"))
    (unwind-protect
	(progn
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary))
	    (setq process
		  (apply #'funcall
			 #'start-process-shell-command "*PGP*" output-buffer
			 program args)))
	  (set-process-sentinel process #'ignore)
	  (when passphrase
	    (process-send-string process (concat passphrase "\n")))
	  (process-send-region process start end)
	  (process-send-eof process)
	  (while (eq 'run (process-status process))
	    (accept-process-output process 5))
	  (setq status (process-status process)
		exit-status (process-exit-status process))
	  (delete-process process)
	  (with-current-buffer output-buffer
	    (pgg-convert-lbt-region (point-min)(point-max) 'LF)

	    (if (memq status '(stop signal))
		(error "%s exited abnormally: '%s'" program exit-status))
	    (if (= 127 exit-status)
		(error "%s could not be found" program))

	    (set-buffer (get-buffer-create errors-buffer))
	    (buffer-disable-undo)
	    (erase-buffer)
	    (insert-file-contents errors-file-name)))
      (if (and process (eq 'run (process-status process)))
	  (interrupt-process process))
      (condition-case nil
	  (delete-file errors-file-name)
	(file-error nil)))))

(defun pgg-pgp5-lookup-key (string &optional type)
  "Search keys associated with STRING."
  (let ((args (list "+language=en" "-l" string)))
    (with-current-buffer (get-buffer-create pgg-output-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (apply #'call-process pgg-pgp5-pgpk-program nil t nil args)
      (goto-char (point-min))
      (when (re-search-forward "^sec" nil t)
	(substring
	 (nth 2 (split-string
		 (buffer-substring (match-end 0)(progn (end-of-line)(point)))))
	 2)))))

(defun pgg-pgp5-encrypt-region (start end recipients &optional sign passphrase)
  "Encrypt the current region between START and END."
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (passphrase (or passphrase
			 (when sign
			   (pgg-read-passphrase
			    (format "PGP passphrase for %s: "
				    pgg-pgp5-user-id)
			    pgg-pgp5-user-id))))
	 (args
	  (append
	   `("+NoBatchInvalidKeys=off" "-fat" "+batchmode=1"
	     ,@(if (or recipients pgg-encrypt-for-me)
		   (apply #'append
			  (mapcar (lambda (rcpt)
				    (list "-r"
					  (concat "\"" rcpt "\"")))
				  (append recipients
					  (if pgg-encrypt-for-me
					      (list pgg-pgp5-user-id)))))))
	   (if sign '("-s" "-u" pgg-pgp5-user-id)))))
    (pgg-pgp5-process-region start end nil pgg-pgp5-pgpe-program args)
    (pgg-process-when-success nil)))

(defun pgg-pgp5-decrypt-region (start end &optional passphrase)
  "Decrypt the current region between START and END."
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (passphrase
	  (or passphrase
	      (pgg-read-passphrase
	       (format "PGP passphrase for %s: " pgg-pgp5-user-id)
	       (pgg-pgp5-lookup-key pgg-pgp5-user-id 'encrypt))))
	 (args
	  '("+verbose=1" "+batchmode=1" "+language=us" "-f")))
    (pgg-pgp5-process-region start end passphrase pgg-pgp5-pgpv-program args)
    (pgg-process-when-success nil)))

(defun pgg-pgp5-sign-region (start end &optional clearsign passphrase)
  "Make detached signature from text between START and END."
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (passphrase
	  (or passphrase
	      (pgg-read-passphrase
	       (format "PGP passphrase for %s: " pgg-pgp5-user-id)
	       (pgg-pgp5-lookup-key pgg-pgp5-user-id 'sign))))
	 (args
	  (list (if clearsign "-fat" "-fbat")
		"+verbose=1" "+language=us" "+batchmode=1"
		"-u" pgg-pgp5-user-id)))
    (pgg-pgp5-process-region start end passphrase pgg-pgp5-pgps-program args)
    (pgg-process-when-success
      (when (re-search-forward "^-+BEGIN PGP SIGNATURE" nil t);XXX
	(let ((packet
	       (cdr (assq 2 (pgg-parse-armor-region
			     (progn (beginning-of-line 2)
				    (point))
			     (point-max))))))
	  (if pgg-cache-passphrase
	      (pgg-add-passphrase-to-cache
	       (cdr (assq 'key-identifier packet))
	       passphrase)))))))

(defun pgg-pgp5-verify-region (start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE."
  (let ((orig-file (pgg-make-temp-file "pgg"))
	(args '("+verbose=1" "+batchmode=1" "+language=us"))
	(orig-mode (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (let ((coding-system-for-write 'binary)
		jka-compr-compression-info-list jam-zcat-filename-list)
	    (write-region start end orig-file)))
      (set-default-file-modes orig-mode))
    (when (stringp signature)
      (copy-file signature (setq signature (concat orig-file ".asc")))
      (setq args (append args (list signature))))
    (pgg-pgp5-process-region (point)(point) nil pgg-pgp5-pgpv-program args)
    (delete-file orig-file)
    (if signature (delete-file signature))
    (with-current-buffer pgg-errors-buffer
      (goto-char (point-min))
      (if (re-search-forward "^Good signature" nil t)
	  (progn
	    (set-buffer pgg-output-buffer)
	    (insert-buffer-substring pgg-errors-buffer)
	    t)
	nil))))

(defun pgg-pgp5-insert-key ()
  "Insert public key at point."
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (args
	  (list "+verbose=1" "+batchmode=1" "+language=us" "-x"
		(concat "\"" pgg-pgp5-user-id "\""))))
    (pgg-pgp5-process-region (point)(point) nil pgg-pgp5-pgpk-program args)
    (insert-buffer-substring pgg-output-buffer)))

(defun pgg-pgp5-snarf-keys-region (start end)
  "Add all public keys in region between START and END to the keyring."
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (key-file (pgg-make-temp-file "pgg"))
	 (args
	  (list "+verbose=1" "+batchmode=1" "+language=us" "-a"
		key-file)))
    (let ((coding-system-for-write 'raw-text-dos))
      (write-region start end key-file))
    (pgg-pgp5-process-region start end nil pgg-pgp5-pgpk-program args)
    (delete-file key-file)
    (pgg-process-when-success nil)))

(provide 'pgg-pgp5)

;;; pgg-pgp5.el ends here
