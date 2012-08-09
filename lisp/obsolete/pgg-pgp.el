;;; pgg-pgp.el --- PGP 2.* and 6.* support for PGG.

;; Copyright (C) 1999-2000, 2002-2012  Free Software Foundation, Inc.

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

(defgroup pgg-pgp ()
  "PGP 2.* and 6.* interface."
  :group 'pgg)

(defcustom pgg-pgp-program "pgp"
  "PGP 2.* and 6.* executable."
  :group 'pgg-pgp
  :type 'string)

(defcustom pgg-pgp-shell-file-name "/bin/sh"
  "File name to load inferior shells from.
Bourne shell or its equivalent \(not tcsh) is needed for \"2>\"."
  :group 'pgg-pgp
  :type 'string)

(defcustom pgg-pgp-shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument."
  :group 'pgg-pgp
  :type 'string)

(defcustom pgg-pgp-extra-args nil
  "Extra arguments for every PGP invocation."
  :group 'pgg-pgp
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Arguments")))

(defvar pgg-pgp-user-id nil
  "PGP ID of your default identity.")

(defun pgg-pgp-process-region (start end passphrase program args)
  (let* ((errors-file-name (pgg-make-temp-file "pgg-errors"))
	 (args
	  (concat args
		  pgg-pgp-extra-args
                  " 2>" (shell-quote-argument errors-file-name)))
	 (shell-file-name pgg-pgp-shell-file-name)
	 (shell-command-switch pgg-pgp-shell-command-switch)
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
		  (start-process-shell-command "*PGP*" output-buffer
                                               (concat program " " args))))
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

(defun pgg-pgp-lookup-key (string &optional type)
  "Search keys associated with STRING."
  (let ((args (list "+batchmode" "+language=en" "-kv" string)))
    (with-current-buffer (get-buffer-create pgg-output-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (apply #'call-process pgg-pgp-program nil t nil args)
      (goto-char (point-min))
      (cond
       ((re-search-forward "^pub\\s +[0-9]+/" nil t);PGP 2.*
	(buffer-substring (point)(+ 8 (point))))
       ((re-search-forward "^Type" nil t);PGP 6.*
	(beginning-of-line 2)
	(substring
	 (nth 2 (split-string
		 (buffer-substring (point)(progn (end-of-line) (point)))))
	 2))))))

(defun pgg-pgp-encrypt-region (start end recipients &optional sign passphrase)
  "Encrypt the current region between START and END."
  (let* ((pgg-pgp-user-id (or pgg-pgp-user-id pgg-default-user-id))
	 (passphrase (or passphrase
			 (when sign
			   (pgg-read-passphrase
			    (format "PGP passphrase for %s: "
				    pgg-pgp-user-id)
			    pgg-pgp-user-id))))
	 (args
	  (concat
	   "+encrypttoself=off +verbose=1 +batchmode +language=us -fate "
           (if (or recipients pgg-encrypt-for-me)
               (mapconcat 'shell-quote-argument
                          (append recipients
                                  (if pgg-encrypt-for-me
                                      (list pgg-pgp-user-id))) " "))
           (if sign (concat " -s -u " (shell-quote-argument pgg-pgp-user-id))))))
    (pgg-pgp-process-region start end nil pgg-pgp-program args)
    (pgg-process-when-success nil)))

(defun pgg-pgp-decrypt-region (start end &optional passphrase)
  "Decrypt the current region between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let* ((pgg-pgp-user-id (or pgg-pgp-user-id pgg-default-user-id))
	 (key (pgg-pgp-lookup-key pgg-pgp-user-id 'encrypt))
	 (passphrase
	  (or passphrase
	      (pgg-read-passphrase
	       (format "PGP passphrase for %s: " pgg-pgp-user-id) key)))
	 (args
	  "+verbose=1 +batchmode +language=us -f"))
    (pgg-pgp-process-region start end passphrase pgg-pgp-program args)
    (pgg-process-when-success
      (if pgg-cache-passphrase
	  (pgg-add-passphrase-to-cache key passphrase)))))

(defun pgg-pgp-sign-region (start end &optional clearsign passphrase)
  "Make detached signature from text between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let* ((pgg-pgp-user-id (or pgg-pgp-user-id pgg-default-user-id))
	 (passphrase
	  (or passphrase
	      (pgg-read-passphrase
	       (format "PGP passphrase for %s: " pgg-pgp-user-id)
	       (pgg-pgp-lookup-key pgg-pgp-user-id 'sign))))
	 (args
	  (concat (if clearsign "-fast" "-fbast")
		" +verbose=1 +language=us +batchmode"
		" -u " (shell-quote-argument pgg-pgp-user-id))))
    (pgg-pgp-process-region start end passphrase pgg-pgp-program args)
    (pgg-process-when-success
      (goto-char (point-min))
      (when (re-search-forward "^-+BEGIN PGP" nil t);XXX
	(let ((packet
	       (cdr (assq 2 (pgg-parse-armor-region
			     (progn (beginning-of-line 2)
				    (point))
			     (point-max))))))
	  (if pgg-cache-passphrase
	      (pgg-add-passphrase-to-cache
	       (cdr (assq 'key-identifier packet))
	       passphrase)))))))

(defun pgg-pgp-verify-region (start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE."
  (let* ((orig-file (pgg-make-temp-file "pgg"))
	 (args "+verbose=1 +batchmode +language=us")
	 (orig-mode (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (let ((coding-system-for-write 'binary)
		jka-compr-compression-info-list jam-zcat-filename-list)
	    (write-region start end orig-file)))
      (set-default-file-modes orig-mode))
    (if (stringp signature)
	(progn
	  (copy-file signature (setq signature (concat orig-file ".asc")))
	  (setq args (concat args " " (shell-quote-argument signature)))))
    (setq args (concat args " " (shell-quote-argument orig-file)))
    (pgg-pgp-process-region (point)(point) nil pgg-pgp-program args)
    (delete-file orig-file)
    (if signature (delete-file signature))
    (pgg-process-when-success
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^warning: " nil t)
	  (delete-region (match-beginning 0)
			 (progn (beginning-of-line 2) (point)))))
      (goto-char (point-min))
      (when (re-search-forward "^\\.$" nil t)
	(delete-region (point-min)
		       (progn (beginning-of-line 2)
			      (point)))))))

(defun pgg-pgp-insert-key ()
  "Insert public key at point."
  (let* ((pgg-pgp-user-id (or pgg-pgp-user-id pgg-default-user-id))
	 (args
	  (concat "+verbose=1 +batchmode +language=us -kxaf "
                  (shell-quote-argument pgg-pgp-user-id))))
    (pgg-pgp-process-region (point)(point) nil pgg-pgp-program args)
    (insert-buffer-substring pgg-output-buffer)))

(defun pgg-pgp-snarf-keys-region (start end)
  "Add all public keys in region between START and END to the keyring."
  (let* ((pgg-pgp-user-id (or pgg-pgp-user-id pgg-default-user-id))
	 (key-file (pgg-make-temp-file "pgg"))
	 (args
	  (concat "+verbose=1 +batchmode +language=us -kaf "
                  (shell-quote-argument key-file))))
    (let ((coding-system-for-write 'raw-text-dos))
      (write-region start end key-file))
    (pgg-pgp-process-region start end nil pgg-pgp-program args)
    (delete-file key-file)
    (pgg-process-when-success nil)))

(provide 'pgg-pgp)

;;; pgg-pgp.el ends here
