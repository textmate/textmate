;;; org-gnus.el --- Support for links to Gnus groups and messages from within Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;         Tassilo Horn <tassilo at member dot fsf dot org>
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

;; This file implements links to Gnus groups and messages from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)
(eval-when-compile (require 'gnus-sum))

;; Declare external functions and variables
(declare-function message-fetch-field "message" (header &optional not-all))
(declare-function message-narrow-to-head-1 "message" nil)
(declare-function nnimap-group-overview-filename "nnimap" (group server))
;; The following line suppresses a compiler warning stemming from gnus-sum.el
(declare-function gnus-summary-last-subject "gnus-sum" nil)
;; Customization variables

(when (fboundp 'defvaralias)
  (defvaralias 'org-usenet-links-prefer-google 'org-gnus-prefer-web-links))

(defcustom org-gnus-prefer-web-links nil
  "If non-nil, `org-store-link' creates web links to Google groups or Gmane.
When nil, Gnus will be used for such links.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type 'boolean)

(defcustom org-gnus-nnimap-query-article-no-from-file nil
  "If non-nil, `org-gnus-follow-link' will try to translate
Message-Ids to article numbers by querying the .overview file.
Normally, this translation is done by querying the IMAP server,
which is usually very fast.  Unfortunately, some (maybe badly
configured) IMAP servers don't support this operation quickly.
So if following a link to a Gnus article takes ages, try setting
this variable to `t'."
  :group 'org-link-store
  :version "24.1"
  :type 'boolean)


;; Install the link type
(org-add-link-type "gnus" 'org-gnus-open)
(add-hook 'org-store-link-functions 'org-gnus-store-link)

;; Implementation

(defun org-gnus-nnimap-cached-article-number (group server message-id)
  "Return cached article number (uid) of message in GROUP on SERVER.
MESSAGE-ID is the message-id header field that identifies the
message.  If the uid is not cached, return nil."
  (with-temp-buffer
    (let ((nov (nnimap-group-overview-filename group server)))
      (when (file-exists-p nov)
	(mm-insert-file-contents nov)
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(catch 'found
	  (while (search-forward message-id nil t)
	    (let ((hdr (split-string (thing-at-point 'line) "\t")))
	      (if (string= (nth 4 hdr) message-id)
		  (throw 'found (nth 0 hdr))))))))))

(defun org-gnus-group-link (group)
  "Create a link to the Gnus group GROUP.
If GROUP is a newsgroup and `org-gnus-prefer-web-links' is
non-nil, create a link to groups.google.com or gmane.org.
Otherwise create a link to the group inside Gnus.

If `org-store-link' was called with a prefix arg the meaning of
`org-gnus-prefer-web-links' is reversed."
  (let ((unprefixed-group (replace-regexp-in-string "^[^:]+:" "" group)))
    (if (and (string-match "^nntp" group) ;; Only for nntp groups
	     (org-xor current-prefix-arg
		      org-gnus-prefer-web-links))
	(org-make-link (if (string-match "gmane" unprefixed-group)
			   "http://news.gmane.org/"
			 "http://groups.google.com/group/")
		       unprefixed-group)
      (org-make-link "gnus:" group))))

(defun org-gnus-article-link (group newsgroups message-id x-no-archive)
  "Create a link to a Gnus article.
The article is specified by its MESSAGE-ID.  Additional
parameters are the Gnus GROUP, the NEWSGROUPS the article was
posted to and the X-NO-ARCHIVE header value of that article.

If GROUP is a newsgroup and `org-gnus-prefer-web-links' is
non-nil, create a link to groups.google.com or gmane.org.
Otherwise create a link to the article inside Gnus.

If `org-store-link' was called with a prefix arg the meaning of
`org-gnus-prefer-web-links' is reversed."
  (if (and (org-xor current-prefix-arg org-gnus-prefer-web-links)
	   newsgroups	  ;; Make web links only for nntp groups
	   (not x-no-archive)) ;; and if X-No-Archive isn't set.
      (format (if (string-match "gmane\\." newsgroups)
		  "http://mid.gmane.org/%s"
		"http://groups.google.com/groups/search?as_umsgid=%s")
	      (org-fixup-message-id-for-http message-id))
    (org-make-link "gnus:" group "#" message-id)))

(defun org-gnus-store-link ()
  "Store a link to a Gnus folder or message."
  (cond
   ((eq major-mode 'gnus-group-mode)
    (let* ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
			 (gnus-group-group-name))         ; version
			((fboundp 'gnus-group-name)
			 (gnus-group-name))
			(t "???")))
	   desc link)
      (when group
	(org-store-link-props :type "gnus" :group group)
	(setq desc (org-gnus-group-link group)
	      link desc)
	(org-add-link-props :link link :description desc)
	link)))

   ((memq major-mode '(gnus-summary-mode gnus-article-mode))
    (let* ((group gnus-newsgroup-name)
	   (header (with-current-buffer gnus-summary-buffer
		     (gnus-summary-article-header)))
	   (from (mail-header-from header))
	   (message-id (org-remove-angle-brackets (mail-header-id header)))
	   (date (org-trim (mail-header-date header)))
	   (date-ts (and date
			 (ignore-errors
			   (format-time-string
			    (org-time-stamp-format t)
			    (date-to-time date)))))
	   (date-ts-ia (and date
			    (ignore-errors
			      (format-time-string
			       (org-time-stamp-format t t)
			       (date-to-time date)))))
	   (subject (copy-sequence (mail-header-subject header)))
	   (to (cdr (assq 'To (mail-header-extra header))))
	   newsgroups x-no-archive desc link)
      ;; Remove text properties of subject string to avoid Emacs bug
      ;; #3506
      (set-text-properties 0 (length subject) nil subject)

      ;; Fetching an article is an expensive operation; newsgroup and
      ;; x-no-archive are only needed for web links.
      (when (org-xor current-prefix-arg org-gnus-prefer-web-links)
	;; Make sure the original article buffer is up-to-date
	(save-window-excursion (gnus-summary-select-article))
	(setq to (or to (gnus-fetch-original-field "To"))
	      newsgroups (gnus-fetch-original-field "Newsgroups")
	      x-no-archive (gnus-fetch-original-field "x-no-archive")))
      (org-store-link-props :type "gnus" :from from :subject subject
			    :message-id message-id :group group :to to)
      (when date
	(org-add-link-props :date date :date-timestamp date-ts
			    :date-timestamp-inactive date-ts-ia))
      (setq desc (org-email-link-description)
	    link (org-gnus-article-link
		  group	newsgroups message-id x-no-archive))
      (org-add-link-props :link link :description desc)
      link))
   ((eq major-mode 'message-mode)
    (setq org-store-link-plist nil)  ; reset
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (and (not (message-fetch-field "Message-ID"))
             (message-generate-headers '(Message-ID)))
        (goto-char (point-min))
        (re-search-forward "^Message-ID: *.*$" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'message-deletable nil)
        (let ((gcc (car (last
                         (message-unquote-tokens
                          (message-tokenize-header (mail-fetch-field "gcc" nil t) " ,")))))
              (id (org-remove-angle-brackets (mail-fetch-field "Message-ID")))
              (to (mail-fetch-field "To"))
              (from (mail-fetch-field "From"))
              (subject (mail-fetch-field "Subject"))
              desc link
              newsgroup xarchive)       ; those are always nil for gcc
          (and (not gcc)
               (error "Can not create link: No Gcc header found."))
          (org-store-link-props :type "gnus" :from from :subject subject
                                :message-id id :group gcc :to to)
          (setq desc (org-email-link-description)
                link (org-gnus-article-link
                      gcc newsgroup id xarchive))
          (org-add-link-props :link link :description desc)
          link))))))

(defun org-gnus-open-nntp (path)
  "Follow the nntp: link specified by PATH."
  (let* ((spec (split-string path "/"))
	 (server (split-string (nth 2 spec) "@"))
	 (group (nth 3 spec))
	 (article (nth 4 spec)))
    (org-gnus-follow-link
     (format "nntp+%s:%s" (or (cdr server) (car server)) group)
     article)))

(defun org-gnus-open (path)
  "Follow the Gnus message or folder link specified by PATH."
  (let (group article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in Gnus link"))
    (setq group (match-string 1 path)
	  article (match-string 3 path))
    (when group
      (setq group (org-substring-no-properties group)))
    (when article
      (setq article (org-substring-no-properties article)))
    (org-gnus-follow-link group article)))

(defun org-gnus-follow-link (&optional group article)
  "Follow a Gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))
  (when group
    (setq group (org-substring-no-properties group)))
  (when article
    (setq article (org-substring-no-properties article)))
  (cond ((and group article)
	 (gnus-activate-group group)
	 (condition-case nil
	     (let* ((method (gnus-find-method-for-group group))
		    (backend (car method))
		    (server (cadr method)))
	       (cond
		((eq backend 'nndoc)
		 (if (gnus-group-read-group t nil group)
		     (gnus-summary-goto-article article nil t)
		   (message "Couldn't follow gnus link.  %s"
			    "The summary couldn't be opened.")))
		(t
		 (let ((articles 1)
		       group-opened)
		   (when (and (eq backend 'nnimap)
			      org-gnus-nnimap-query-article-no-from-file)
		     (setq article
			   (or (org-gnus-nnimap-cached-article-number
				(nth 1 (split-string group ":"))
				server (concat "<" article ">")) article)))
		   (while (and (not group-opened)
			       ;; stop on integer overflows
			       (> articles 0))
		     (setq group-opened (gnus-group-read-group
					 articles nil group)
			   articles (if (< articles 16)
					(1+ articles)
				      (* articles 2))))
		   (if group-opened
		       (gnus-summary-goto-article article nil t)
		     (message "Couldn't follow gnus link.  %s"
			      "The summary couldn't be opened."))))))
	   (quit (message "Couldn't follow gnus link.  %s"
			  "The linked group is empty."))))
	(group (gnus-group-jump-to-group group))))

(defun org-gnus-no-new-news ()
  "Like `M-x gnus' but doesn't check for new news."
  (if (not (gnus-alive-p)) (gnus)))

(provide 'org-gnus)


;;; org-gnus.el ends here
