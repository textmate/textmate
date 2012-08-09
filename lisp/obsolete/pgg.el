;;; pgg.el --- glue for the various PGP implementations.

;; Copyright (C) 1999-2000, 2002-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Symmetric encryption added by: Sascha Wilde <wilde@sha-bang.de>
;; Created: 1999/10/28
;; Keywords: PGP
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

(require 'pgg-def)
(require 'pgg-parse)
(autoload 'run-at-time "timer")

;; Don't merge these two `eval-when-compile's.
(eval-when-compile
  ;; For Emacs <22.2 and XEmacs.
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))
  (require 'cl))

;;; @ utility functions
;;;

(eval-when-compile
  (when (featurep 'xemacs)
    (defmacro pgg-run-at-time-1 (time repeat function args)
      (if (condition-case nil
	      (let ((delete-itimer 'delete-itimer)
		    (itimer-driver-start 'itimer-driver-start)
		    (itimer-value 'itimer-value)
		    (start-itimer 'start-itimer))
		(unless (or (symbol-value 'itimer-process)
			    (symbol-value 'itimer-timer))
		  (funcall itimer-driver-start))
		;; Check whether there is a bug to which the difference of
		;; the present time and the time when the itimer driver was
		;; woken up is subtracted from the initial itimer value.
		(let* ((inhibit-quit t)
		       (ctime (current-time))
		       (itimer-timer-last-wakeup
			(prog1
			    ctime
			  (setcar ctime (1- (car ctime)))))
		       (itimer-list nil)
		       (itimer (funcall start-itimer "pgg-run-at-time"
					'ignore 5)))
		  (sleep-for 0.1) ;; Accept the timeout interrupt.
		  (prog1
		      (> (funcall itimer-value itimer) 0)
		    (funcall delete-itimer itimer))))
	    (error nil))
	  `(let ((time ,time))
	     (apply #'start-itimer "pgg-run-at-time"
		    ,function (if time (max time 1e-9) 1e-9)
		    ,repeat nil t ,args))
	`(let ((time ,time)
	       (itimers (list nil)))
	   (setcar
	    itimers
	    (apply #'start-itimer "pgg-run-at-time"
		   (lambda (itimers repeat function &rest args)
		     (let ((itimer (car itimers)))
		       (if repeat
			   (progn
			     (set-itimer-function
			      itimer
			      (lambda (itimer repeat function &rest args)
				(set-itimer-restart itimer repeat)
				(set-itimer-function itimer function)
				(set-itimer-function-arguments itimer args)
				(apply function args)))
			     (set-itimer-function-arguments
			      itimer
			      (append (list itimer repeat function) args)))
			 (set-itimer-function
			  itimer
			  (lambda (itimer function &rest args)
			    (delete-itimer itimer)
			    (apply function args)))
			 (set-itimer-function-arguments
			  itimer
			  (append (list itimer function) args)))))
		   1e-9 (if time (max time 1e-9) 1e-9)
		   nil t itimers ,repeat ,function ,args)))))))

(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defun pgg-run-at-time (time repeat function &rest args)
	  "Emulating function run as `run-at-time'.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
	  (pgg-run-at-time-1 time repeat function args))
	(defun pgg-cancel-timer (timer)
	  "Emulate cancel-timer for xemacs."
	  (let ((delete-itimer 'delete-itimer))
	    (funcall delete-itimer timer))))
    (defalias 'pgg-run-at-time 'run-at-time)
    (defalias 'pgg-cancel-timer 'cancel-timer)))

(defun pgg-invoke (func scheme &rest args)
  (progn
    (require (intern (format "pgg-%s" scheme)))
    (apply 'funcall (intern (format "pgg-%s-%s" scheme func)) args)))

(put 'pgg-save-coding-system 'lisp-indent-function 2)

(defmacro pgg-save-coding-system (start end &rest body)
  `(if (called-interactively-p 'interactive)
       (let ((buffer (current-buffer)))
	 (with-temp-buffer
	   (let (buffer-undo-list)
	     (insert-buffer-substring buffer ,start ,end)
	     (encode-coding-region (point-min)(point-max)
				   buffer-file-coding-system)
	     (prog1 (save-excursion ,@body)
	       (push nil buffer-undo-list)
	       (ignore-errors (undo))))))
     (save-restriction
       (narrow-to-region ,start ,end)
       ,@body)))

(defun pgg-temp-buffer-show-function (buffer)
  (let ((window (or (get-buffer-window buffer 'visible)
		    (split-window-vertically))))
    (set-window-buffer window buffer)
    (shrink-window-if-larger-than-buffer window)))

;; XXX `pgg-display-output-buffer' is a horrible name for this function.
;;     It should be something like `pgg-situate-output-or-display-error'.
(defun pgg-display-output-buffer (start end status)
  "Situate en/decryption results or pop up an error buffer.

Text from START to END is replaced by contents of output buffer if STATUS
is true, or else the output buffer is displayed."
  (if status
      (pgg-situate-output start end)
    (pgg-display-error-buffer)))

(defun pgg-situate-output (start end)
  "Place en/decryption result in place of current text from START to END."
  (delete-region start end)
  (insert-buffer-substring pgg-output-buffer)
  (decode-coding-region start (point) buffer-file-coding-system))

(defun pgg-display-error-buffer ()
  "Pop up an error buffer indicating the reason for an en/decryption failure."
  (let ((temp-buffer-show-function
         (function pgg-temp-buffer-show-function)))
    (with-output-to-temp-buffer pgg-echo-buffer
      (set-buffer standard-output)
      (insert-buffer-substring pgg-errors-buffer))))

(defvar pgg-passphrase-cache (make-vector 7 0))

(defvar pgg-pending-timers (make-vector 7 0)
  "Hash table for managing scheduled pgg cache management timers.

We associate key and timer, so the timer can be canceled if a new
timeout for the key is set while an old one is still pending.")

(defun pgg-read-passphrase (prompt &optional key notruncate)
  "Using PROMPT, obtain passphrase for KEY from cache or user.

Truncate the key to 8 trailing characters unless NOTRUNCATE is true
\(default false).

Custom variables `pgg-cache-passphrase' and `pgg-passphrase-cache-expiry'
regulate cache behavior."
  (or (pgg-read-passphrase-from-cache key notruncate)
      (read-passwd prompt)))

(defun pgg-read-passphrase-from-cache (key &optional notruncate)
  "Obtain passphrase for KEY from time-limited passphrase cache.

Truncate the key to 8 trailing characters unless NOTRUNCATE is true
\(default false).

Custom variables `pgg-cache-passphrase' and `pgg-passphrase-cache-expiry'
regulate cache behavior."
  (and pgg-cache-passphrase
       key (or notruncate
                (setq key (pgg-truncate-key-identifier key)))
       (symbol-value (intern-soft key pgg-passphrase-cache))))

(defun pgg-add-passphrase-to-cache (key passphrase &optional notruncate)
  "Associate KEY with PASSPHRASE in time-limited passphrase cache.

Truncate the key to 8 trailing characters unless NOTRUNCATE is true
\(default false).

Custom variables `pgg-cache-passphrase' and `pgg-passphrase-cache-expiry'
regulate cache behavior."

  (let* ((key (if notruncate key (pgg-truncate-key-identifier key)))
         (interned-timer-key (intern-soft key pgg-pending-timers))
         (old-timer (symbol-value interned-timer-key))
         new-timer)
    (when old-timer
        (cancel-timer old-timer)
        (unintern interned-timer-key pgg-pending-timers))
    (set (intern key pgg-passphrase-cache)
         passphrase)
    (set (intern key pgg-pending-timers)
         (pgg-run-at-time pgg-passphrase-cache-expiry nil
                           #'pgg-remove-passphrase-from-cache
                           key notruncate))))

(if (fboundp 'clear-string)
    (defalias 'pgg-clear-string 'clear-string)
  (defun pgg-clear-string (string)
    (fillarray string ?_)))

(declare-function pgg-clear-string "pgg" (string))

(defun pgg-remove-passphrase-from-cache (key &optional notruncate)
  "Omit passphrase associated with KEY in time-limited passphrase cache.

Truncate the key to 8 trailing characters unless NOTRUNCATE is true
\(default false).

This is a no-op if there is not entry for KEY (eg, it's already expired.

The memory for the passphrase is filled with underscores to clear any
references to it.

Custom variables `pgg-cache-passphrase' and `pgg-passphrase-cache-expiry'
regulate cache behavior."
  (let* ((passphrase (pgg-read-passphrase-from-cache key notruncate))
         (key (if notruncate key (pgg-truncate-key-identifier key)))
         (interned-timer-key (intern-soft key pgg-pending-timers))
         (old-timer (symbol-value interned-timer-key)))
    (when passphrase
      (pgg-clear-string passphrase)
      (unintern key pgg-passphrase-cache))
    (when old-timer
      (pgg-cancel-timer old-timer)
      (unintern interned-timer-key pgg-pending-timers))))

(defmacro pgg-convert-lbt-region (start end lbt)
  `(let ((pgg-conversion-end (set-marker (make-marker) ,end)))
     (goto-char ,start)
     (case ,lbt
       (CRLF
	(while (progn
		 (end-of-line)
		 (> (marker-position pgg-conversion-end) (point)))
	  (insert "\r")
	  (forward-line 1)))
       (LF
	(while (re-search-forward "\r$" pgg-conversion-end t)
	  (replace-match ""))))))

(put 'pgg-as-lbt 'lisp-indent-function 3)

(defmacro pgg-as-lbt (start end lbt &rest body)
  `(let ((inhibit-read-only t)
	 buffer-read-only
	 buffer-undo-list)
     (pgg-convert-lbt-region ,start ,end ,lbt)
     (let ((,end (point)))
       ,@body)
     (push nil buffer-undo-list)
     (ignore-errors (undo))))

(put 'pgg-process-when-success 'lisp-indent-function 0)

(defmacro pgg-process-when-success (&rest body)
  `(with-current-buffer pgg-output-buffer
     (if (zerop (buffer-size)) nil ,@body t)))

(defalias 'pgg-make-temp-file
  (if (fboundp 'make-temp-file)
      'make-temp-file
    (lambda (prefix &optional dir-flag)
      (let ((file (expand-file-name
		   (make-temp-name prefix)
		   (if (fboundp 'temp-directory)
		       (temp-directory)
		     temporary-file-directory))))
	(if dir-flag
	    (make-directory file))
	file))))

;;; @ interface functions
;;;

;;;###autoload
(defun pgg-encrypt-region (start end rcpts &optional sign passphrase)
  "Encrypt the current region between START and END for RCPTS.

If optional argument SIGN is non-nil, do a combined sign and encrypt.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive
   (list (region-beginning)(region-end)
	 (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let ((status
	 (pgg-save-coding-system start end
	   (pgg-invoke "encrypt-region" (or pgg-scheme pgg-default-scheme)
		       (point-min) (point-max) rcpts sign passphrase))))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-encrypt-symmetric-region (start end &optional passphrase)
  "Encrypt the current region between START and END symmetric with passphrase.

If optional PASSPHRASE is not specified, it will be obtained from the
cache or user."
  (interactive "r")
  (let ((status
	 (pgg-save-coding-system start end
	   (pgg-invoke "encrypt-symmetric-region"
		       (or pgg-scheme pgg-default-scheme)
		       (point-min) (point-max) passphrase))))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-encrypt-symmetric (&optional start end passphrase)
  "Encrypt the current buffer using a symmetric, rather than key-pair, cipher.

If optional arguments START and END are specified, only encrypt within
the region.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive)
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-encrypt-symmetric-region start end passphrase)))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-encrypt (rcpts &optional sign start end passphrase)
  "Encrypt the current buffer for RCPTS.

If optional argument SIGN is non-nil, do a combined sign and encrypt.

If optional arguments START and END are specified, only encrypt within
the region.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive (list (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-encrypt-region start end rcpts sign passphrase)))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-decrypt-region (start end &optional passphrase)
  "Decrypt the current region between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive "r")
  (let* ((buf (current-buffer))
	 (status
	  (pgg-save-coding-system start end
	    (pgg-invoke "decrypt-region" (or pgg-scheme pgg-default-scheme)
			(point-min) (point-max) passphrase))))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-decrypt (&optional start end passphrase)
  "Decrypt the current buffer.

If optional arguments START and END are specified, only decrypt within
the region.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-decrypt-region start end passphrase)))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-sign-region (start end &optional cleartext passphrase)
  "Make the signature from text between START and END.

If the optional 3rd argument CLEARTEXT is non-nil, it does not create
a detached signature.

If this function is called interactively, CLEARTEXT is enabled
and the output is displayed.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive "r")
  (let ((status (pgg-save-coding-system start end
		  (pgg-invoke "sign-region" (or pgg-scheme pgg-default-scheme)
			      (point-min) (point-max)
			      (or (called-interactively-p 'interactive)
				  cleartext)
                              passphrase))))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-sign (&optional cleartext start end passphrase)
  "Sign the current buffer.

If the optional argument CLEARTEXT is non-nil, it does not create a
detached signature.

If optional arguments START and END are specified, only sign data
within the region.

If this function is called interactively, CLEARTEXT is enabled
and the output is displayed.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-sign-region start end
                                  (or (called-interactively-p 'interactive)
				      cleartext)
                                  passphrase)))
    (when (called-interactively-p 'interactive)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-verify-region (start end &optional signature fetch)
  "Verify the current region between START and END.
If the optional 3rd argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.

If the optional 4th argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'."
  (interactive "r")
  (let* ((packet
	  (if (null signature) nil
	    (with-temp-buffer
	      (buffer-disable-undo)
	      (unless (featurep 'xemacs)
		(set-buffer-multibyte nil))
	      (insert-file-contents signature)
	      (cdr (assq 2 (pgg-decode-armor-region
			    (point-min)(point-max)))))))
	 (key (cdr (assq 'key-identifier packet)))
	 status keyserver)
    (and (stringp key)
	 pgg-query-keyserver
	 (setq key (concat "0x" (pgg-truncate-key-identifier key)))
	 (null (pgg-lookup-key key))
	 (or fetch (called-interactively-p 'interactive))
	 (y-or-n-p (format "Key %s not found; attempt to fetch? " key))
	 (setq keyserver
	       (or (cdr (assq 'preferred-key-server packet))
		   pgg-default-keyserver-address))
	 (pgg-fetch-key keyserver key))
    (setq status
	  (pgg-save-coding-system start end
	    (pgg-invoke "verify-region" (or pgg-scheme pgg-default-scheme)
			(point-min) (point-max) signature)))
    (when (called-interactively-p 'interactive)
      (let ((temp-buffer-show-function
	     (function pgg-temp-buffer-show-function)))
	(with-output-to-temp-buffer pgg-echo-buffer
	  (set-buffer standard-output)
	  (insert-buffer-substring (if status pgg-output-buffer
				     pgg-errors-buffer)))))
    status))

;;;###autoload
(defun pgg-verify (&optional signature fetch start end)
  "Verify the current buffer.
If the optional argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.
If the optional argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'.
If optional arguments START and END are specified, only verify data
within the region."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-verify-region start end signature fetch)))
    (when (called-interactively-p 'interactive)
      (let ((temp-buffer-show-function
	     (function pgg-temp-buffer-show-function)))
	(with-output-to-temp-buffer pgg-echo-buffer
	  (set-buffer standard-output)
	  (insert-buffer-substring (if status pgg-output-buffer
				     pgg-errors-buffer)))))
    status))

;;;###autoload
(defun pgg-insert-key ()
  "Insert the ASCII armored public key."
  (interactive)
  (pgg-invoke "insert-key" (or pgg-scheme pgg-default-scheme)))

;;;###autoload
(defun pgg-snarf-keys-region (start end)
  "Import public keys in the current region between START and END."
  (interactive "r")
  (pgg-save-coding-system start end
    (pgg-invoke "snarf-keys-region" (or pgg-scheme pgg-default-scheme)
		start end)))

;;;###autoload
(defun pgg-snarf-keys ()
  "Import public keys in the current buffer."
  (interactive "")
  (pgg-snarf-keys-region (point-min) (point-max)))

(defun pgg-lookup-key (string &optional type)
  (pgg-invoke "lookup-key" (or pgg-scheme pgg-default-scheme) string type))

(defvar pgg-insert-url-function  (function pgg-insert-url-with-w3))

(defun pgg-insert-url-with-w3 (url)
  (ignore-errors
    (require 'url)
    (let (buffer-file-name)
      (url-insert-file-contents url))))

(defvar pgg-insert-url-extra-arguments nil)
(defvar pgg-insert-url-program nil)

(defun pgg-insert-url-with-program (url)
  (let ((args (copy-sequence pgg-insert-url-extra-arguments))
	process)
    (insert
     (with-temp-buffer
       (setq process
	     (apply #'start-process " *PGG url*" (current-buffer)
		    pgg-insert-url-program (nconc args (list url))))
       (set-process-sentinel process #'ignore)
       (while (eq 'run (process-status process))
	 (accept-process-output process 5))
       (delete-process process)
       (if (and process (eq 'run (process-status process)))
	   (interrupt-process process))
       (buffer-string)))))

(defun pgg-fetch-key (keyserver key)
  "Attempt to fetch a KEY from KEYSERVER for addition to PGP or GnuPG keyring."
  (with-current-buffer (get-buffer-create pgg-output-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (let ((proto (if (string-match "^[a-zA-Z\\+\\.\\\\-]+:" keyserver)
		     (substring keyserver 0 (1- (match-end 0))))))
      (save-excursion
	(funcall pgg-insert-url-function
		 (if proto keyserver
		   (format "http://%s:11371/pks/lookup?op=get&search=%s"
			   keyserver key))))
      (when (re-search-forward "^-+BEGIN" nil 'last)
	(delete-region (point-min) (match-beginning 0))
	(when (re-search-forward "^-+END" nil t)
	  (delete-region (progn (end-of-line) (point))
			 (point-max)))
	(insert "\n")
	(with-temp-buffer
	  (insert-buffer-substring pgg-output-buffer)
	  (pgg-snarf-keys-region (point-min)(point-max)))))))


(provide 'pgg)

;;; pgg.el ends here
