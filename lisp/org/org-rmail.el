;;; org-rmail.el --- Support for links to Rmail messages from within Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Rmail messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Declare external functions and variables
(declare-function rmail-show-message "rmail" (&optional n no-summary))
(declare-function rmail-what-message "rmail" ())
(defvar rmail-current-message)

;; Install the link type
(org-add-link-type "rmail" 'org-rmail-open)
(add-hook 'org-store-link-functions 'org-rmail-store-link)

;; Implementation
(defun org-rmail-store-link ()
  "Store a link to an Rmail folder or message."
  (when (or (eq major-mode 'rmail-mode)
	    (eq major-mode 'rmail-summary-mode))
    (save-window-excursion
      (save-restriction
	(when (eq major-mode 'rmail-summary-mode)
	  (rmail-show-message rmail-current-message))
	(when (fboundp 'rmail-narrow-to-non-pruned-header)
	  (rmail-narrow-to-non-pruned-header))
	(let* ((folder buffer-file-name)
	       (message-id (mail-fetch-field "message-id"))
	       (from (mail-fetch-field "from"))
	       (to (mail-fetch-field "to"))
	       (subject (mail-fetch-field "subject"))
	       (date (mail-fetch-field "date"))
	       (date-ts (and date (format-time-string
				   (org-time-stamp-format t)
				   (date-to-time date))))
	       (date-ts-ia (and date (format-time-string
				      (org-time-stamp-format t t)
				      (date-to-time date))))
	       desc link)
	  (org-store-link-props
	   :type "rmail" :from from :to to
	   :subject subject :message-id message-id)
	  (when date
	    (org-add-link-props :date date :date-timestamp date-ts
				:date-timestamp-inactive date-ts-ia))
	  (setq message-id (org-remove-angle-brackets message-id))
	  (setq desc (org-email-link-description))
	  (setq link (org-make-link "rmail:" folder "#" message-id))
	  (org-add-link-props :link link :description desc)
	  (rmail-show-message rmail-current-message)
	  link)))))

(defun org-rmail-open (path)
  "Follow an Rmail message link to the specified PATH."
  (let (folder article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in Rmail link"))
    (setq folder (match-string 1 path)
	  article (match-string 3 path))
    (org-rmail-follow-link folder article)))

(defun org-rmail-follow-link (folder article)
  "Follow an Rmail link to FOLDER and ARTICLE."
  (require 'rmail)
  (setq article (org-add-angle-brackets article))
  (let (message-number)
    (save-excursion
      (save-window-excursion
	(rmail (if (string= folder "RMAIL") rmail-file-name folder))
	(setq message-number
	      (save-restriction
		(widen)
		(goto-char (point-max))
		(if (re-search-backward
		     (concat "^Message-ID:\\s-+" (regexp-quote
						  (or article "")))
		     nil t)
		    (rmail-what-message))))))
    (if message-number
	(progn
	  (rmail (if (string= folder "RMAIL") rmail-file-name folder))
	  (rmail-show-message message-number)
	  message-number)
      (error "Message not found"))))

(provide 'org-rmail)

;;; org-rmail.el ends here
