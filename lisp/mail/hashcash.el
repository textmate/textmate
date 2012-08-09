;;; hashcash.el --- Add hashcash payments to email

;; Copyright (C) 2003-2005, 2007-2012  Free Software Foundation, Inc.

;; Written by: Paul Foley <mycroft@actrix.gen.nz> (1997-2002)
;; Maintainer: Paul Foley <mycroft@actrix.gen.nz>
;; Keywords: mail, hashcash

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

;; The hashcash binary is at http://www.hashcash.org/.
;;
;; Call mail-add-payment to add a hashcash payment to a mail message
;; in the current buffer.
;;
;; Call mail-add-payment-async after writing the addresses but before
;; writing the mail to start calculating the hashcash payment
;; asynchronously.
;;
;; The easiest way to do this automatically for all outgoing mail
;; is to set `message-generate-hashcash' to t.  If you want more
;; control, try the following hooks.
;;
;; To automatically add payments to all outgoing mail when sending:
;;    (add-hook 'message-send-hook 'mail-add-payment)
;;
;; To start calculations automatically when addresses are prefilled:
;;    (add-hook 'message-setup-hook 'mail-add-payment-async)
;;
;; To check whether calculations are done before sending:
;;    (add-hook 'message-send-hook 'hashcash-wait-or-cancel)

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))	; for case

(defgroup hashcash nil
  "Hashcash configuration."
  :group 'mail)

(defcustom hashcash-default-payment 20
  "*The default number of bits to pay to unknown users.
If this is zero, no payment header will be generated.
See `hashcash-payment-alist'."
  :type 'integer
  :group 'hashcash)

(defcustom hashcash-payment-alist '()
  "*An association list mapping email addresses to payment amounts.
Elements may consist of (ADDR AMOUNT) or (ADDR STRING AMOUNT), where
ADDR is the email address of the intended recipient and AMOUNT is
the value of hashcash payment to be made to that user.  STRING, if
present, is the string to be hashed; if not present ADDR will be used."
  :type '(repeat (choice (list :tag "Normal"
			       (string :name "Address")
			       (integer :name "Amount"))
			 (list :tag "Replace hash input"
			       (string :name "Address")
			       (string :name "Hash input")
			       (integer :name "Amount"))))
  :group 'hashcash)

(defcustom hashcash-default-accept-payment 20
  "*The default minimum number of bits to accept on incoming payments."
  :type 'integer
  :group 'hashcash)

(defcustom hashcash-accept-resources `((,user-mail-address nil))
  "*An association list mapping hashcash resources to payment amounts.
Resources named here are to be accepted in incoming payments.  If the
corresponding AMOUNT is NIL, the value of `hashcash-default-accept-payment'
is used instead."
  :group 'hashcash)

(defcustom hashcash-path (executable-find "hashcash")
  "*The path to the hashcash binary."
  :group 'hashcash)

(defcustom hashcash-extra-generate-parameters nil
  "*A list of parameter strings passed to `hashcash-path' when minting.
For example, you may want to set this to '(\"-Z2\") to reduce header length."
  :type '(repeat string)
  :group 'hashcash)

(defcustom hashcash-double-spend-database "hashcash.db"
  "*The path to the double-spending database."
  :group 'hashcash)

(defcustom hashcash-in-news nil
  "*Specifies whether or not hashcash payments should be made to newsgroups."
  :type 'boolean
  :group 'hashcash)

(defvar hashcash-process-alist nil
  "Alist of asynchronous hashcash processes and buffers.")

(require 'mail-utils)

(eval-and-compile
  (if (fboundp 'point-at-bol)
      (defalias 'hashcash-point-at-bol 'point-at-bol)
    (defalias 'hashcash-point-at-bol 'line-beginning-position))

  (if (fboundp 'point-at-eol)
      (defalias 'hashcash-point-at-eol 'point-at-eol)
    (defalias 'hashcash-point-at-eol 'line-end-position)))

(defun hashcash-strip-quoted-names (addr)
  (setq addr (mail-strip-quoted-names addr))
  (if (and addr (string-match "\\`\\([^+@]+\\)\\+[^@]*\\(@.+\\)" addr))
      (concat (match-string 1 addr) (match-string 2 addr))
    addr))

(declare-function message-narrow-to-headers-or-head "message" ())
(declare-function message-fetch-field "message" (header &optional not-all))
(declare-function message-goto-eoh "message" ())
(declare-function message-narrow-to-headers "message" ())

(defun hashcash-token-substring ()
  (save-excursion
    (let ((token ""))
      (loop
	(setq token
	  (concat token (buffer-substring (point) (hashcash-point-at-eol))))
	(goto-char (hashcash-point-at-eol))
	(forward-char 1)
	(unless (looking-at "[ \t]") (return token))
	(while (looking-at "[ \t]") (forward-char 1))))))

(defun hashcash-payment-required (addr)
  "Return the hashcash payment value required for the given address."
  (let ((val (assoc addr hashcash-payment-alist)))
    (or (nth 2 val) (nth 1 val) hashcash-default-payment)))

(defun hashcash-payment-to (addr)
  "Return the string with which hashcash payments should collide."
  (let ((val (assoc addr hashcash-payment-alist)))
    (or (nth 1 val) (nth 0 val) addr)))

(defun hashcash-generate-payment (str val)
  "Generate a hashcash payment by finding a VAL-bit collison on STR."
  (if (and (> val 0)
	   hashcash-path)
      (with-current-buffer (get-buffer-create " *hashcash*")
	(erase-buffer)
	(apply 'call-process hashcash-path nil t nil
	       "-m" "-q" "-b" (number-to-string val) str
	       hashcash-extra-generate-parameters)
	(goto-char (point-min))
	(hashcash-token-substring))
    (error "No `hashcash' binary found")))

(defun hashcash-generate-payment-async (str val callback)
  "Generate a hashcash payment by finding a VAL-bit collison on STR.
Return immediately.  Call CALLBACK with process and result when ready."
  (if (and (> val 0)
	   hashcash-path)
      (let ((process (apply 'start-process "hashcash" nil
			    hashcash-path "-m" "-q"
			    "-b" (number-to-string val) str
			    hashcash-extra-generate-parameters)))
	(setq hashcash-process-alist (cons
				      (cons process (current-buffer))
				      hashcash-process-alist))
	(set-process-filter process `(lambda (process output)
				       (funcall ,callback process output))))
    (funcall callback nil nil)))

(defun hashcash-check-payment (token str val)
  "Check the validity of a hashcash payment."
  (if hashcash-path
      (zerop (call-process hashcash-path nil nil nil "-c"
			   "-d" "-f" hashcash-double-spend-database
			   "-b" (number-to-string val)
			   "-r" str
			   token))
    (progn
      (message "No hashcash binary found")
      (sleep-for 1)
      nil)))

(defun hashcash-version (token)
  "Find the format version of a hashcash token."
  ;; Version 1.2 looks like n:yymmdd:rrrrr:xxxxxxxxxxxxxxxx
  ;;   This carries its own version number embedded in the token,
  ;;   so no further format number changes should be necessary
  ;;   in the X-Payment header.
  ;;
  ;; Version 1.1 looks like yymmdd:rrrrr:xxxxxxxxxxxxxxxx
  ;;   You need to upgrade your hashcash binary.
  ;;
  ;; Version 1.0 looked like nnnnnrrrrrxxxxxxxxxxxxxxxx
  ;;   This is no longer supported.
  (cond ((equal (aref token 1) ?:) 1.2)
	((equal (aref token 6) ?:) 1.1)
	(t (error "Unknown hashcash format version"))))

(defun hashcash-already-paid-p (recipient)
  "Check for hashcash token to RECIPIENT in current buffer."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (let ((token (message-fetch-field "x-hashcash"))
	    (case-fold-search t))
	(and (stringp token)
	     (string-match (regexp-quote recipient) token))))))

;;;###autoload
(defun hashcash-insert-payment (arg)
  "Insert X-Payment and X-Hashcash headers with a payment for ARG"
  (interactive "sPay to: ")
  (unless (hashcash-already-paid-p arg)
    (let ((pay (hashcash-generate-payment (hashcash-payment-to arg)
					  (hashcash-payment-required arg))))
      (when pay
	(insert-before-markers "X-Hashcash: " pay "\n")))))

;;;###autoload
(defun hashcash-insert-payment-async (arg)
  "Insert X-Payment and X-Hashcash headers with a payment for ARG
Only start calculation.  Results are inserted when ready."
  (interactive "sPay to: ")
  (unless (hashcash-already-paid-p arg)
    (hashcash-generate-payment-async
     (hashcash-payment-to arg)
     (hashcash-payment-required arg)
     `(lambda (process payment)
	(hashcash-insert-payment-async-2 ,(current-buffer) process payment)))))

(defun hashcash-insert-payment-async-2 (buffer process pay)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (setq hashcash-process-alist (delq
					(assq process hashcash-process-alist)
					hashcash-process-alist))
	  (message-goto-eoh)
	  (when pay
	    (insert-before-markers "X-Hashcash: " pay)))))))

(defun hashcash-cancel-async (&optional buffer)
  "Delete any hashcash processes associated with BUFFER.
BUFFER defaults to the current buffer."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let (entry)
    (while (setq entry (rassq buffer hashcash-process-alist))
      (delete-process (car entry))
      (setq hashcash-process-alist
	    (delq entry hashcash-process-alist)))))

(defun hashcash-wait-async (&optional buffer)
  "Wait for asynchronous hashcash processes in BUFFER to finish.
BUFFER defaults to the current buffer."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let (entry)
    (while (setq entry (rassq buffer hashcash-process-alist))
      (accept-process-output (car entry) 1))))

(defun hashcash-processes-running-p (buffer)
  "Return non-nil if hashcash processes in BUFFER are still running."
  (rassq buffer hashcash-process-alist))

(defun hashcash-wait-or-cancel ()
  "Ask user whether to wait for hashcash processes to finish."
  (interactive)
  (when (hashcash-processes-running-p (current-buffer))
    (if (y-or-n-p
	  "Hashcash process(es) still running; wait for them to finish? ")
	(hashcash-wait-async)
      (hashcash-cancel-async))))

;;;###autoload
(defun hashcash-verify-payment (token &optional resource amount)
  "Verify a hashcash payment"
  (let* ((split (split-string token ":"))
	 (key (if (< (hashcash-version token) 1.2)
		  (nth 1 split)
		  (case (string-to-number (nth 0 split))
		    (0 (nth 2 split))
		    (1 (nth 3 split))))))
    (cond ((null resource)
	   (let ((elt (assoc key hashcash-accept-resources)))
	     (and elt (hashcash-check-payment token (car elt)
			(or (cadr elt) hashcash-default-accept-payment)))))
	  ((equal token key)
	   (hashcash-check-payment token resource
				(or amount hashcash-default-accept-payment)))
	  (t nil))))

;;;###autoload
(defun mail-add-payment (&optional arg async)
  "Add X-Payment: and X-Hashcash: headers with a hashcash payment
for each recipient address.  Prefix arg sets default payment temporarily.
Set ASYNC to t to start asynchronous calculation.  (See
`mail-add-payment-async')."
  (interactive "P")
  (let ((hashcash-default-payment (if arg (prefix-numeric-value arg)
				    hashcash-default-payment))
	(addrlist nil))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(let ((to (hashcash-strip-quoted-names (mail-fetch-field "To" nil t)))
	      (cc (hashcash-strip-quoted-names (mail-fetch-field "Cc" nil t)))
	      (ng (hashcash-strip-quoted-names (mail-fetch-field "Newsgroups"
								 nil t))))
	  (when to
	    (setq addrlist (split-string to ",[ \t\n]*")))
	  (when cc
	    (setq addrlist (nconc addrlist (split-string cc ",[ \t\n]*"))))
	  (when (and hashcash-in-news ng)
	    (setq addrlist (nconc addrlist (split-string ng ",[ \t\n]*")))))
	(when addrlist
	  (mapc (if async
		    #'hashcash-insert-payment-async
		  #'hashcash-insert-payment)
		addrlist)))))
  t)

;;;###autoload
(defun mail-add-payment-async (&optional arg)
  "Add X-Payment: and X-Hashcash: headers with a hashcash payment
for each recipient address.  Prefix arg sets default payment temporarily.
Calculation is asynchronous."
  (interactive "P")
  (mail-add-payment arg t))

;;;###autoload
(defun mail-check-payment (&optional arg)
  "Look for a valid X-Payment: or X-Hashcash: header.
Prefix arg sets default accept amount temporarily."
  (interactive "P")
  (let ((hashcash-default-accept-payment (if arg (prefix-numeric-value arg)
					   hashcash-default-accept-payment))
	(version (hashcash-version (hashcash-generate-payment "x" 1))))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n\n")
      (beginning-of-line)
      (let ((end (point))
	    (ok nil))
	(goto-char (point-min))
	(while (and (not ok) (search-forward "X-Payment: hashcash " end t))
	  (let ((value (split-string (hashcash-token-substring) " ")))
	    (when (equal (car value) (number-to-string version))
	      (setq ok (hashcash-verify-payment (cadr value))))))
	(goto-char (point-min))
	(while (and (not ok) (search-forward "X-Hashcash: " end t))
	  (setq ok (hashcash-verify-payment (hashcash-token-substring))))
	(when ok
	  (message "Payment valid"))
	ok))))

(provide 'hashcash)

;;; hashcash.el ends here
