;;; url-imap.el --- IMAP retrieval routines

;; Copyright (C) 1999, 2004-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: comm, data, processes

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

;; Anyway, here's a teaser. It's quite broken in lots of regards, but at
;; least it seem to work. At least a little. At least when called
;; manually like this (I've no idea how it's supposed to be called):

;; (url-imap (url-generic-parse-url "imap://cyrus.andrew.cmu.edu/archive.c-client;UID=1021"))

;;; Code:

(require 'url-util)
(require 'url-parse)
(require 'nnimap)
(require 'mm-util)

(defconst url-imap-default-port 143 "Default IMAP port.")

(defun url-imap-open-host (host port user pass)
  ;; xxx use user and password
  (if (fboundp 'nnheader-init-server-buffer)
      (nnheader-init-server-buffer))
  (let ((imap-username user)
	(imap-password pass)
	(authenticator (if user 'login 'anonymous)))
    (nnimap-open-server host
			`((nnimap-server-port ,port)
			  (nnimap-stream 'network)
			  (nnimap-authenticator ,authenticator)))))

(defun url-imap (url)
  (unless (vectorp url)
    (signal 'wrong-type-error (list "Need a pre-parsed URL." url)))
  (with-current-buffer (generate-new-buffer " *url-imap*")
    (mm-disable-multibyte)
    (let* ((host (url-host url))
	   (port (url-port url))
	   ;; xxx decode mailbox (see rfc2192)
	   (mailbox (url-filename url))
	   (coding-system-for-read 'binary))
      (and (eq (string-to-char mailbox) ?/)
	   (setq mailbox (substring mailbox 1)))
      (url-imap-open-host host port (url-user url) (url-password url))
      (cond ((assoc "TYPE" (url-attributes url))
	     ;; xxx list mailboxes (start gnus?)
	     )
	    ((assoc "UID" (url-attributes url))
	     ;; fetch message part
	     ;; xxx handle partial fetches
	     (insert "Content-type: message/rfc822\n\n")
	     (nnimap-request-article (cdr (assoc "UID" (url-attributes url)))
				     mailbox host (current-buffer)))
	    (t
	     ;; xxx list messages in mailbox (start gnus?)
	     )))
    (current-buffer)))

;;; url-imap.el ends here
