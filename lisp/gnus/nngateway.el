;;; nngateway.el --- posting news via mail gateways

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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

(eval-when-compile (require 'cl))
(require 'nnoo)
(require 'message)

(nnoo-declare nngateway)

(defvoo nngateway-address nil
  "Address of the mail-to-news gateway.")

(defvoo nngateway-header-transformation 'nngateway-simple-header-transformation
  "Function to be called to rewrite the news headers into mail headers.
It is called narrowed to the headers to be transformed with one
parameter -- the gateway address.")

;;; Interface functions

(nnoo-define-basics nngateway)

(deffoo nngateway-open-server (server &optional defs)
  (if (nngateway-server-opened server)
      t
    (unless (assq 'nngateway-address defs)
      (setq defs (append defs (list (list 'nngateway-address server)))))
    (nnoo-change-server 'nngateway server defs)))

(deffoo nngateway-request-post (&optional server)
  (when (or (nngateway-server-opened server)
	    (nngateway-open-server server))
    ;; Rewrite the header.
    (let ((buf (current-buffer)))
      (with-temp-buffer
	(insert-buffer-substring buf)
	(message-narrow-to-head)
	(funcall nngateway-header-transformation nngateway-address)
	(goto-char (point-max))
	(insert mail-header-separator "\n")
	(widen)
	(let (message-required-mail-headers)
	  (funcall (or message-send-mail-real-function
		       message-send-mail-function)))
	t))))

;;; Internal functions

(defun nngateway-simple-header-transformation (gateway)
  "Transform the headers to use GATEWAY."
  (let ((newsgroups (mail-fetch-field "newsgroups")))
    (message-remove-header "to")
    (message-remove-header "cc")
    (goto-char (point-min))
    (insert "To: " (nnheader-replace-chars-in-string newsgroups ?. ?-)
	    "@" gateway "\n")))

(defun nngateway-mail2news-header-transformation (gateway)
  "Transform the headers for sending to a mail2news gateway."
  (message-remove-header "to")
  (message-remove-header "cc")
  (goto-char (point-min))
  (insert "To: " gateway "\n"))

(nnoo-define-skeleton nngateway)

(provide 'nngateway)

;;; nngateway.el ends here
