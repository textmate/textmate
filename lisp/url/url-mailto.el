;;; url-mail.el --- Mail Uniform Resource Locator retrieval code

;; Copyright (C) 1996-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
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

(require 'url-vars)
(require 'url-parse)
(require 'url-util)

;;;###autoload
(defun url-mail (&rest args)
  (interactive "P")
  (if (fboundp 'message-mail)
      (apply 'message-mail args)
    (or (apply 'mail args)
	(error "Mail aborted"))))

(defun url-mail-goto-field (field)
  (if (not field)
      (goto-char (point-max))
    (let ((dest nil)
	  (lim nil)
	  (case-fold-search t))
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote mail-header-separator) nil t)
	    (setq lim (match-beginning 0)))
	(goto-char (point-min))
	(if (re-search-forward (concat "^" (regexp-quote field) ":") lim t)
	    (setq dest (match-beginning 0))))
      (if dest
	  (progn
	    (goto-char dest)
	    (end-of-line))
	(goto-char lim)
	(insert (capitalize field) ": ")
	(save-excursion
	  (insert "\n"))))))

(declare-function mail-send-and-exit "sendmail")

;;;###autoload
(defun url-mailto (url)
  "Handle the mailto: URL syntax."
  (if (url-user url)
      ;; malformed mailto URL (mailto://wmperry@gnu.org) instead of
      ;; mailto:wmperry@gnu.org
      (setf (url-filename url) (concat (url-user url) "@" (url-filename url))))
  (setq url (url-filename url))
  (let (to args source-url subject func headers-start)
    (if (string-match (regexp-quote "?") url)
	(setq headers-start (match-end 0)
	      to (url-unhex-string (substring url 0 (match-beginning 0)))
	      args (url-parse-query-string
		    (substring url headers-start nil) t t))
      (setq to (url-unhex-string url)))
    (setq source-url (url-view-url t))
    (if (and url-request-data (not (assoc "subject" args)))
	(setq args (cons (list "subject"
			       (concat "Automatic submission from "
				       url-package-name "/"
				       url-package-version)) args)))
    (if (and source-url (not (assoc "x-url-from" args)))
	(setq args (cons (list "x-url-from" source-url) args)))

    (let ((tolist (assoc "to" args)))
      (if tolist
	  (if (not (string= to ""))
	      (setcdr tolist
		      (list (concat to ", " (cadr tolist)))))
	(setq args (cons (list "to" to) args))))

    (setq subject (cdr-safe (assoc "subject" args)))
    (if (eq url-mail-command 'compose-mail)
	(compose-mail nil nil nil 'new)
      (if (eq url-mail-command 'mail)
	  (mail 'new)
	(funcall url-mail-command)))
    (while args
      (if (string= (caar args) "body")
	  (progn
	    (goto-char (point-min))
	    (or (search-forward (concat "\n" mail-header-separator "\n") nil t)
	    	(goto-char (point-max)))
	    (insert (mapconcat 
		     #'(lambda (string)
			 (replace-regexp-in-string "\r\n" "\n" string))
		     (cdar args) "\n")))
	(url-mail-goto-field (caar args))
	(setq func (intern-soft (concat "mail-" (caar args))))
	(insert (mapconcat 'identity (cdar args) ", ")))
      (setq args (cdr args)))
    ;; (url-mail-goto-field "User-Agent")
;;     (insert url-package-name "/" url-package-version " URL/" url-version)
    (if (not url-request-data)
	(progn
	  (set-buffer-modified-p nil)
	  (if subject
	      (url-mail-goto-field nil)
	    (url-mail-goto-field "subject")))
      (if url-request-extra-headers
	  (mapconcat
	   (lambda (x)
	     (url-mail-goto-field (car x))
	     (insert (cdr x)))
	   url-request-extra-headers ""))
      (goto-char (point-max))
      (insert url-request-data)
      ;; It seems Microsoft-ish to send without warning.
      ;; Fixme: presumably this should depend on a privacy setting.
      (if (y-or-n-p "Send this auto-generated mail? ")
	  (let ((buffer (current-buffer)))
	    (cond ((eq url-mail-command 'compose-mail)
		   (funcall (get mail-user-agent 'sendfunc) nil))
		  ;; otherwise, we can't be sure
		  ((fboundp 'message-send-and-exit)
		   (message-send-and-exit))
		  (t (mail-send-and-exit nil)))
	    (kill-buffer buffer))))
    nil))

(provide 'url-mailto)

;;; url-mailto.el ends here
