;;; nnagent.el --- offline backend for Gnus

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

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

(require 'nnheader)
(require 'nnoo)
(eval-when-compile (require 'cl))
(require 'gnus-agent)
(require 'nnml)

(nnoo-declare nnagent
  nnml)



(defconst nnagent-version "nnagent 1.0")

(defvoo nnagent-directory nil
  "Internal variable."
  nnml-directory)

(defvoo nnagent-active-file nil
  "Internal variable."
  nnml-active-file)

(defvoo nnagent-newsgroups-file nil
  "Internal variable."
  nnml-newsgroups-file)

(defvoo nnagent-get-new-mail nil
  "Internal variable."
  nnml-get-new-mail)

;;; Interface functions.

(nnoo-define-basics nnagent)

(defun nnagent-server (server)
  (and server (format "%s+%s" (car gnus-command-method) server)))

(deffoo nnagent-open-server (server &optional defs)
  (setq defs
	`((nnagent-directory ,(gnus-agent-directory))
	  (nnagent-active-file ,(gnus-agent-lib-file "active"))
	  (nnagent-newsgroups-file ,(gnus-agent-lib-file "newsgroups"))
	  (nnagent-get-new-mail nil)))
  (nnoo-change-server 'nnagent
		      (nnagent-server server)
		      defs)
  (let ((dir (gnus-agent-directory))
	err)
    (cond
     ((not (condition-case arg
	       (file-exists-p dir)
	     (ftp-error (setq err (format "%s" arg)))))
      (nnagent-close-server)
      (nnheader-report
       'nnagent (or err
		    (format "No such file or directory: %s" dir))))
     ((not (file-directory-p (file-truename dir)))
      (nnagent-close-server)
      (nnheader-report 'nnagent "Not a directory: %s" dir))
     (t
      (nnheader-report 'nnagent "Opened server %s using directory %s"
		       server dir)
      t))))

(deffoo nnagent-retrieve-groups (groups &optional server)
  (save-excursion
    (cond
     ((file-exists-p (gnus-agent-lib-file "groups"))
      (nnmail-find-file (gnus-agent-lib-file "groups"))
      'groups)
     ((file-exists-p (gnus-agent-lib-file "active"))
      (nnmail-find-file (gnus-agent-lib-file "active"))
      'active)
     (t nil))))

(defun nnagent-request-type (group article)
  (unless (stringp article)
    (let ((gnus-agent nil))
      (if (not (gnus-check-backend-function
		'request-type (car gnus-command-method)))
	  'unknown
	(funcall (gnus-get-function gnus-command-method 'request-type)
		 (gnus-group-real-name group) article)))))

(deffoo nnagent-request-newgroups (date server)
  nil)

(deffoo nnagent-request-update-info (group info &optional server)
  nil)

(deffoo nnagent-request-post (&optional server)
  (gnus-agent-insert-meta-information 'news gnus-command-method)
  (gnus-request-accept-article "nndraft:queue" nil t t))

(deffoo nnagent-request-set-mark (group action server)
  (mm-with-unibyte-buffer
    (insert "(gnus-agent-synchronize-group-flags \""
	    group
	    "\" '")
    (gnus-pp action)
    (insert " \""
	    (gnus-method-to-server gnus-command-method)
	    "\"")
    (insert ")\n")
    (let ((coding-system-for-write nnheader-file-coding-system))
      (write-region (point-min) (point-max) (gnus-agent-lib-file "flags")
		    t 'silent)))
  ;; Also set the marks for the original back end that keeps marks in
  ;; the local system.
  (let ((gnus-agent nil))
    (when (and (memq (car gnus-command-method) '(nntp))
	       (gnus-check-backend-function 'request-set-mark
					    (car gnus-command-method)))
      (funcall (gnus-get-function gnus-command-method 'request-set-mark)
	       group action server)))
  nil)

(deffoo nnagent-retrieve-headers (articles &optional group server fetch-old)
  (let ((file (gnus-agent-article-name ".overview" group))
	arts n first)
    (save-excursion
      (gnus-agent-load-alist group)
      (setq arts (gnus-sorted-difference
		  articles (mapcar 'car gnus-agent-article-alist)))
      ;; Assume that articles with smaller numbers than the first one
      ;; Agent knows are gone.
      (setq first (caar gnus-agent-article-alist))
      (when first
	(while (and arts (< (car arts) first))
	  (pop arts)))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (let ((file-name-coding-system nnmail-pathname-coding-system))
	(nnheader-insert-nov-file file (car articles)))
      (goto-char (point-min))
      (gnus-parse-without-error
	(while (and arts (not (eobp)))
	  (setq n (read (current-buffer)))
	  (when (> n (car arts))
	    (beginning-of-line))
	  (while (and arts (> n (car arts)))
	    (insert (format
		     "%d\t[Undownloaded article %d]\tGnus Agent\t\t\t\n"
		     (car arts) (car arts)))
	    (pop arts))
	  (when (and arts (= n (car arts)))
	    (pop arts))
	  (forward-line 1)))
      (while arts
	(insert (format
		 "%d\t[Undownloaded article %d]\tGnus Agent\t\t\t\n"
		 (car arts) (car arts)))
	(pop arts))
      (if (and fetch-old
	       (not (numberp fetch-old)))
	  t				; Don't remove anything.
	(nnheader-nov-delete-outside-range
	 (if fetch-old (max 1 (- (car articles) fetch-old))
	   (car articles))
	 (car (last articles)))
	t)
      'nov)))

(deffoo nnagent-request-expire-articles (articles group &optional server force)
  articles)

(deffoo nnagent-request-group (group &optional server dont-check info)
  (nnoo-parent-function 'nnagent 'nnml-request-group
			(list group (nnagent-server server) dont-check info)))

(deffoo nnagent-close-group (group &optional server)
  (nnoo-parent-function 'nnagent 'nnml-close-group
			(list group (nnagent-server server))))

(deffoo nnagent-request-accept-article (group &optional server last)
  (nnoo-parent-function 'nnagent 'nnml-request-accept-article
			(list group (nnagent-server server) last)))

(deffoo nnagent-request-article (id &optional group server buffer)
  (nnoo-parent-function 'nnagent 'nnml-request-article
			(list id group (nnagent-server server) buffer)))

(deffoo nnagent-request-create-group (group &optional server args)
  (nnoo-parent-function 'nnagent 'nnml-request-create-group
			(list group (nnagent-server server) args)))

(deffoo nnagent-request-delete-group (group &optional force server)
  (nnoo-parent-function 'nnagent 'nnml-request-delete-group
			(list group force (nnagent-server server))))

(deffoo nnagent-request-list (&optional server)
  (nnoo-parent-function 'nnagent 'nnml-request-list
			(list (nnagent-server server))))

(deffoo nnagent-request-list-newsgroups (&optional server)
  (nnoo-parent-function 'nnagent 'nnml-request-list-newsgroups
			(list (nnagent-server server))))

(deffoo nnagent-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (nnoo-parent-function 'nnagent 'nnml-request-move-article
			(list article group (nnagent-server server)
			      accept-form last move-is-internal)))

(deffoo nnagent-request-rename-group (group new-name &optional server)
  (nnoo-parent-function 'nnagent 'nnml-request-rename-group
			(list group new-name (nnagent-server server))))

(deffoo nnagent-request-scan (&optional group server)
  (nnoo-parent-function 'nnagent 'nnml-request-scan
			(list group (nnagent-server server))))

(deffoo nnagent-set-status (article name value &optional group server)
  (nnoo-parent-function 'nnagent 'nnml-set-status
			(list article name value group (nnagent-server server))))

(deffoo nnagent-server-opened (&optional server)
  (nnoo-parent-function 'nnagent 'nnml-server-opened
			(list (nnagent-server server))))

(deffoo nnagent-status-message (&optional server)
  (nnoo-parent-function 'nnagent 'nnml-status-message
			(list (nnagent-server server))))

(deffoo nnagent-request-regenerate (server)
  (nnoo-parent-function 'nnagent 'nnml-request-regenerate
			(list (nnagent-server server))))

(deffoo nnagent-retrieve-group-data-early (server infos)
  nil)

;; Use nnml functions for just about everything.
(nnoo-import nnagent
  (nnml))


;;; Internal functions.

(provide 'nnagent)

;;; nnagent.el ends here
