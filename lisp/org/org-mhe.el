;;; org-mhe.el --- Support for links to MH-E messages from within Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Thomas Baumann <thomas dot baumann at ch dot tum dot de>
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

;; This file implements links to MH-E messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Customization variables

(defcustom org-mhe-search-all-folders nil
  "Non-nil means the search for the mh-message may extend to all folders.
When non-nil, the search for a message will extend to all other
folders if it cannot be found in the folder given in the link.
Searching all folders may be slow with the default pick based
search but is very efficient with one of the other search engines
supported by MH-E."
  :group 'org-link-follow
  :type 'boolean)

;; Declare external functions and variables
(declare-function mh-display-msg "mh-show" (msg-num folder-name))
(declare-function mh-find-path "mh-utils" ())
(declare-function mh-get-header-field "mh-utils" (field))
(declare-function mh-get-msg-num "mh-utils" (error-if-no-message))
(declare-function mh-header-display "mh-show" ())
(declare-function mh-index-previous-folder "mh-search" ())
(declare-function mh-normalize-folder-name "mh-utils"
		  (folder &optional empty-string-okay dont-remove-trailing-slash
			  return-nil-if-folder-empty))
(declare-function mh-search "mh-search"
		  (folder search-regexp &optional redo-search-flag
			  window-config))
(declare-function mh-search-choose "mh-search" (&optional searcher))
(declare-function mh-show "mh-show" (&optional message redisplay-flag))
(declare-function mh-show-buffer-message-number "mh-comp" (&optional buffer))
(declare-function mh-show-header-display "mh-show" t t)
(declare-function mh-show-msg "mh-show" (msg))
(declare-function mh-show-show "mh-show" t t)
(declare-function mh-visit-folder "mh-folder" (folder &optional
						      range index-data))
(defvar mh-progs)
(defvar mh-current-folder)
(defvar mh-show-folder-buffer)
(defvar mh-index-folder)
(defvar mh-searcher)
(defvar mh-search-regexp-builder)

;; Install the link type
(org-add-link-type "mhe" 'org-mhe-open)
(add-hook 'org-store-link-functions 'org-mhe-store-link)

;; Implementation
(defun org-mhe-store-link ()
  "Store a link to an MH-E folder or message."
  (when (or (equal major-mode 'mh-folder-mode)
	    (equal major-mode 'mh-show-mode))
    (save-window-excursion
      (let* ((from (org-mhe-get-header "From:"))
	     (to (org-mhe-get-header "To:"))
	     (message-id (org-mhe-get-header "Message-Id:"))
	     (subject (org-mhe-get-header "Subject:"))
	     (date (org-mhe-get-header "Date:"))
	     (date-ts (and date (format-time-string
				 (org-time-stamp-format t) (date-to-time date))))
	     (date-ts-ia (and date (format-time-string
				    (org-time-stamp-format t t)
				    (date-to-time date))))
	     link desc)
	(org-store-link-props :type "mh" :from from :to to
			      :subject subject :message-id message-id)
	(when date
	  (org-add-link-props :date date :date-timestamp date-ts
			      :date-timestamp-inactive date-ts-ia))
	(setq desc (org-email-link-description))
	(setq link (org-make-link "mhe:" (org-mhe-get-message-real-folder) "#"
				  (org-remove-angle-brackets message-id)))
	(org-add-link-props :link link :description desc)
	link))))

(defun org-mhe-open (path)
  "Follow an MH-E message link specified by PATH."
  (let (folder article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in MH-E link"))
    (setq folder (match-string 1 path)
	  article (match-string 3 path))
    (org-mhe-follow-link folder article)))

;;; mh-e integration based on planner-mode
(defun org-mhe-get-message-real-folder ()
  "Return the name of the real folder for the current message.
So if you use sequences, it will now work."
  (save-excursion
    (let* ((folder
	    (if (equal major-mode 'mh-folder-mode)
		mh-current-folder
	      ;; Refer to the show buffer
	      mh-show-folder-buffer))
	   (end-index
	    (if (boundp 'mh-index-folder)
		(min (length mh-index-folder) (length folder))))
	   )
      ;; a simple test on mh-index-data does not work, because
      ;; mh-index-data is always nil in a show buffer.
      (if (and (boundp 'mh-index-folder)
	       (string= mh-index-folder (substring folder 0 end-index)))
	  (if (equal major-mode 'mh-show-mode)
	      (save-window-excursion
		(let (pop-up-frames)
		  (when (buffer-live-p (get-buffer folder))
		    (progn
		      (pop-to-buffer folder)
		      (org-mhe-get-message-folder-from-index)
		      )
		    )))
	    (org-mhe-get-message-folder-from-index)
	    )
	folder
	)
      )))

(defun org-mhe-get-message-folder-from-index ()
  "Return the name of the message folder in an index folder buffer."
  (save-excursion
    (mh-index-previous-folder)
    (if (re-search-forward "^\\(+.*\\)$" nil t)
	(message "%s" (match-string 1)))))

(defun org-mhe-get-message-folder ()
  "Return the name of the current message folder.
Be careful if you use sequences."
  (save-excursion
    (if (equal major-mode 'mh-folder-mode)
	mh-current-folder
      ;; Refer to the show buffer
      mh-show-folder-buffer)))

(defun org-mhe-get-message-num ()
  "Return the number of the current message.
Be careful if you use sequences."
  (save-excursion
    (if (equal major-mode 'mh-folder-mode)
	(mh-get-msg-num nil)
      ;; Refer to the show buffer
      (mh-show-buffer-message-number))))

(defun org-mhe-get-header (header)
  "Return the field for HEADER of the message in folder mode.
This will create a show buffer for the corresponding message.  If
you have a better idea of how to do this then please let us know."
  (let* ((folder (org-mhe-get-message-folder))
	 (num (org-mhe-get-message-num))
	 (buffer (get-buffer-create (concat "show-" folder)))
	 (header-field))
  (with-current-buffer buffer
    (mh-display-msg num folder)
    (if (equal major-mode 'mh-folder-mode)
	(mh-header-display)
      (mh-show-header-display))
    (set-buffer buffer)
    (setq header-field (mh-get-header-field header))
    (if (equal major-mode 'mh-folder-mode)
	(mh-show)
      (mh-show-show))
    (org-trim header-field))))

(defun org-mhe-follow-link (folder article)
  "Follow an MH-E link to FOLDER and ARTICLE.
If ARTICLE is nil FOLDER is shown.  If the configuration variable
`org-mhe-search-all-folders' is t and `mh-searcher' is pick,
ARTICLE is searched in all folders.  Indexed searches (swish++,
namazu, and others supported by MH-E) will always search in all
folders."
  (require 'mh-e)
  (require 'mh-search)
  (require 'mh-utils)
  (mh-find-path)
  (if (not article)
      (mh-visit-folder (mh-normalize-folder-name folder))
    (mh-search-choose)
    (if (equal mh-searcher 'pick)
	(progn
	  (setq article (org-add-angle-brackets article))
	  (mh-search folder (list "--message-id" article))
	  (when (and org-mhe-search-all-folders
		     (not (org-mhe-get-message-real-folder)))
	    (kill-this-buffer)
	    (mh-search "+" (list "--message-id" article))))
      (if mh-search-regexp-builder
	  (mh-search "+" (funcall mh-search-regexp-builder
				  (list (cons 'message-id article))))
	(mh-search "+" article)))
    (if (org-mhe-get-message-real-folder)
	(mh-show-msg 1)
      (kill-this-buffer)
      (error "Message not found"))))

(provide 'org-mhe)

;;; org-mhe.el ends here
