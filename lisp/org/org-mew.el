;;; org-mew.el --- Support for links to Mew messages from within Org-mode

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Tokuya Kameshima <kames at fa2 dot so-net dot ne dot jp>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Mew messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

(defgroup org-mew nil
  "Options concerning the Mew link."
  :tag "Org Startup"
  :group 'org-link)

(defcustom org-mew-link-to-refile-destination t
  "Create a link to the refile destination if the message is marked as refile."
  :group 'org-mew
  :type 'boolean)

;; Declare external functions and variables
(declare-function mew-cache-hit "ext:mew-cache" (fld msg &optional must-hit))
(declare-function mew-case-folder "ext:mew-func" (case folder))
(declare-function mew-header-get-value "ext:mew-header"
		  (field &optional as-list))
(declare-function mew-init "ext:mew" ())
(declare-function mew-refile-get "ext:mew-refile" (msg))
(declare-function mew-sinfo-get-case "ext:mew-summary" ())
(declare-function mew-summary-display "ext:mew-summary2" (&optional redisplay))
(declare-function mew-summary-folder-name "ext:mew-syntax" (&optional ext))
(declare-function mew-summary-get-mark "ext:mew-mark" ())
(declare-function mew-summary-message-number2 "ext:mew-syntax" ())
(declare-function mew-summary-pick-with-mewl "ext:mew-pick"
		  (pattern folder src-msgs))
(declare-function mew-summary-search-msg "ext:mew-const" (msg))
(declare-function mew-summary-set-message-buffer "ext:mew-summary3" (fld msg))
(declare-function mew-summary-visit-folder "ext:mew-summary4"
		  (folder &optional goend no-ls))
(declare-function mew-window-push "ext:mew" ())
(defvar mew-init-p)
(defvar mew-summary-goto-line-then-display)

;; Install the link type
(org-add-link-type "mew" 'org-mew-open)
(add-hook 'org-store-link-functions 'org-mew-store-link)

;; Implementation
(defun org-mew-store-link ()
  "Store a link to a Mew folder or message."
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
    (let* ((msgnum (mew-summary-message-number2))
	   (mark-info (mew-summary-get-mark))
	   (folder-name
	    (if (and org-mew-link-to-refile-destination
		     (eq mark-info ?o))	; marked as refile
		(mew-case-folder (mew-sinfo-get-case)
				 (nth 1 (mew-refile-get msgnum)))
	      (mew-summary-folder-name)))
	   message-id from to subject desc link date date-ts date-ts-ia)
      (save-window-excursion
	(if (fboundp 'mew-summary-set-message-buffer)
	    (mew-summary-set-message-buffer folder-name msgnum)
	  (set-buffer (mew-cache-hit folder-name msgnum t)))
	(setq message-id (mew-header-get-value "Message-Id:"))
	(setq from (mew-header-get-value "From:"))
	(setq to (mew-header-get-value "To:"))
	(setq date (mew-header-get-value "Date:"))
	(setq date-ts (and date (format-time-string
				 (org-time-stamp-format t)
				 (date-to-time date))))
	(setq date-ts-ia (and date (format-time-string
				    (org-time-stamp-format t t)
				    (date-to-time date))))
	(setq subject (mew-header-get-value "Subject:")))
      (org-store-link-props :type "mew" :from from :to to
			    :subject subject :message-id message-id)
      (when date
	(org-add-link-props :date date :date-timestamp date-ts
			    :date-timestamp-inactive date-ts-ia))
      (setq message-id (org-remove-angle-brackets message-id))
      (setq desc (org-email-link-description))
      (setq link (org-make-link "mew:" folder-name
				"#" message-id))
      (org-add-link-props :link link :description desc)
      link)))

(defun org-mew-open (path)
  "Follow the Mew message link specified by PATH."
  (let (folder msgnum)
    (cond ((string-match "\\`\\(+.*\\)+\\+\\([0-9]+\\)\\'" path) ; for Bastien's
	   (setq folder (match-string 1 path))
	   (setq msgnum (match-string 2 path)))
	  ((string-match "\\`\\(\\(%#\\)?[^#]+\\)\\(#\\(.*\\)\\)?" path)
	   (setq folder (match-string 1 path))
	   (setq msgnum (match-string 4 path)))
	  (t (error "Error in Mew link")))
    (require 'mew)
    (mew-window-push)
    (unless mew-init-p (mew-init))
    (mew-summary-visit-folder folder)
    (when msgnum
      (if (not (string-match "\\`[0-9]+\\'" msgnum))
	  (let* ((pattern (concat "message-id=" msgnum))
		 (msgs (mew-summary-pick-with-mewl pattern folder nil)))
	    (setq msgnum (car msgs))))
      (if (mew-summary-search-msg msgnum)
	  (if mew-summary-goto-line-then-display
	      (mew-summary-display))
	(error "Message not found")))))

(provide 'org-mew)

;;; org-mew.el ends here
