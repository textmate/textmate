;;; mailpost.el --- RMAIL coupler to /usr/uci/post mailer

;; This is in the public domain
;; since Delp distributed it in 1986 without a copyright notice.

;; This file is part of GNU Emacs.

;; Author: Gary Delp <delp@huey.Udel.Edu>
;; Maintainer: FSF
;; Created: 13 Jan 1986
;; Keywords: mail

;;; Commentary:

;; Yet another mail interface.  this for the rmail system to provide
;;  the missing sendmail interface on systems without /usr/lib/sendmail,
;;   but with /usr/uci/post.

;;; Code:

(require 'mailalias)
(require 'sendmail)

;; (setq send-mail-function 'post-mail-send-it)

(defun post-mail-send-it ()
  "The MH -post interface for `rmail-mail' to call.
To use it, include \"(setq send-mail-function 'post-mail-send-it)\" in
site-init."
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " post-mail errors")
		  0))
	temfile
	(tembuf (generate-new-buffer " post-mail temp"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(with-current-buffer tembuf
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what post-mail expects.
	  (mail-sendmail-undelimit-header)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  ;; Find and handle any FCC fields.
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    ;; If there is a From and no Sender, put it a Sender.
	    (goto-char (point-min))
	    (and (re-search-forward "^From:"  delimline t)
		 (not (save-excursion
			(goto-char (point-min))
			(re-search-forward "^Sender:" delimline t)))
		 (progn
		   (forward-line 1)
		   (insert "Sender: " (user-login-name) "\n")))
	    ;; don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer))))
	  (let ((m (default-file-modes)))
	    (unwind-protect
		(progn
		  (set-default-file-modes 384)
		  (setq temfile  (make-temp-file ",rpost")))
	      (set-default-file-modes m)))
	  (apply 'call-process
		 (append (list (if (boundp 'post-mail-program)
				   post-mail-program
				 "/usr/uci/lib/mh/post")
			       nil errbuf nil
			       "-nofilter" "-msgid")
			 (if mail-interactive '("-watch") '("-nowatch"))
			 (list temfile)))
	  (if mail-interactive
	      (with-current-buffer errbuf
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (switch-to-buffer errbuf)))))

(provide 'mailpost)

;;; mailpost.el ends here
