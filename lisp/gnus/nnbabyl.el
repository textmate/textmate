;;; nnbabyl.el --- rmail mbox access for Gnus

;; Copyright (C) 1995-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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

;; For an overview of what the interface functions do, please see the
;; Gnus sources.

;;; Code:

(require 'nnheader)
(condition-case nil
    (require 'rmail)
  (error (nnheader-message
      5 "Ignore rmail errors from this file, you don't have rmail")))
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnbabyl)

(defvoo nnbabyl-mbox-file (expand-file-name "~/RMAIL")
  "The name of the rmail box file in the users home directory.")

(defvoo nnbabyl-active-file (expand-file-name "~/.rmail-active")
  "The name of the active file for the rmail box.")

(defvoo nnbabyl-get-new-mail t
  "If non-nil, nnbabyl will check the incoming mail file and split the mail.")


(defvoo nnbabyl-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defvar nnbabyl-mail-delimiter "\^_")

(defconst nnbabyl-version "nnbabyl 1.0"
  "nnbabyl version.")

(defvoo nnbabyl-mbox-buffer nil)
(defvoo nnbabyl-current-group nil)
(defvoo nnbabyl-status-string "")
(defvoo nnbabyl-group-alist nil)
(defvoo nnbabyl-active-timestamp nil)

(defvoo nnbabyl-previous-buffer-mode nil)



;;; Interface functions

(nnoo-define-basics nnbabyl)

(deffoo nnbabyl-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let ((number (length articles))
	  (count 0)
	  (delim (concat "^" nnbabyl-mail-delimiter))
	  article art-string start stop)
      (nnbabyl-possibly-change-newsgroup group server)
      (while (setq article (pop articles))
	(setq art-string (nnbabyl-article-string article))
	(set-buffer nnbabyl-mbox-buffer)
	(end-of-line)
	(when (or (search-forward art-string nil t)
		  (search-backward art-string nil t))
	  (unless (re-search-backward delim nil t)
	    (goto-char (point-min)))
	  (while (and (not (looking-at ".+:"))
		      (zerop (forward-line 1))))
	  (setq start (point))
	  (search-forward "\n\n" nil t)
	  (setq stop (1- (point)))
	  (set-buffer nntp-server-buffer)
	  (insert "221 ")
	  (princ article (current-buffer))
	  (insert " Article retrieved.\n")
	  (insert-buffer-substring nnbabyl-mbox-buffer start stop)
	  (goto-char (point-max))
	  (insert ".\n"))
	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     (zerop (% (incf count) 20))
	     (nnheader-message 5 "nnbabyl: Receiving headers... %d%%"
			       (/ (* count 100) number))))

      (and (numberp nnmail-large-newsgroup)
	   (> number nnmail-large-newsgroup)
	   (nnheader-message 5 "nnbabyl: Receiving headers...done"))

      (set-buffer nntp-server-buffer)
      (nnheader-fold-continuation-lines)
      'headers)))

(deffoo nnbabyl-open-server (server &optional defs)
  (nnoo-change-server 'nnbabyl server defs)
  (nnbabyl-create-mbox)
  (cond
   ((not (file-exists-p nnbabyl-mbox-file))
    (nnbabyl-close-server)
    (nnheader-report 'nnbabyl "No such file: %s" nnbabyl-mbox-file))
   ((file-directory-p nnbabyl-mbox-file)
    (nnbabyl-close-server)
    (nnheader-report 'nnbabyl "Not a regular file: %s" nnbabyl-mbox-file))
   (t
    (nnheader-report 'nnbabyl "Opened server %s using mbox %s" server
		     nnbabyl-mbox-file)
    t)))

(deffoo nnbabyl-close-server (&optional server)
  ;; Restore buffer mode.
  (when (and (nnbabyl-server-opened)
	     nnbabyl-previous-buffer-mode)
    (with-current-buffer nnbabyl-mbox-buffer
      (narrow-to-region
       (caar nnbabyl-previous-buffer-mode)
       (cdar nnbabyl-previous-buffer-mode))
      (funcall (cdr nnbabyl-previous-buffer-mode))))
  (nnoo-close-server 'nnbabyl server)
  (setq nnbabyl-mbox-buffer nil)
  t)

(deffoo nnbabyl-server-opened (&optional server)
  (and (nnoo-current-server-p 'nnbabyl server)
       nnbabyl-mbox-buffer
       (buffer-name nnbabyl-mbox-buffer)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(deffoo nnbabyl-request-article (article &optional newsgroup server buffer)
  (nnbabyl-possibly-change-newsgroup newsgroup server)
  (with-current-buffer nnbabyl-mbox-buffer
    (goto-char (point-min))
    (when (search-forward (nnbabyl-article-string article) nil t)
      (let (start stop summary-line)
	(unless (re-search-backward (concat "^" nnbabyl-mail-delimiter) nil t)
	  (goto-char (point-min))
	  (end-of-line))
	(while (and (not (looking-at ".+:"))
		    (zerop (forward-line 1))))
	(setq start (point))
	(or (when (re-search-forward
		   (concat "^" nnbabyl-mail-delimiter) nil t)
	      (beginning-of-line)
	      t)
	    (goto-char (point-max)))
	(setq stop (point))
	(let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (insert-buffer-substring nnbabyl-mbox-buffer start stop)
	  (goto-char (point-min))
	  ;; If there is an EOOH header, then we have to remove some
	  ;; duplicated headers.
	  (setq summary-line (looking-at "Summary-line:"))
	  (when (search-forward "\n*** EOOH ***" nil t)
	    (if summary-line
		;; The headers to be deleted are located before the
		;; EOOH line...
		(delete-region (point-min) (progn (forward-line 1)
						  (point)))
	      ;; ...or after.
	      (delete-region (progn (beginning-of-line) (point))
			     (or (search-forward "\n\n" nil t)
				 (point)))))
	  (if (numberp article)
	      (cons nnbabyl-current-group article)
	    (nnbabyl-article-group-number)))))))

(deffoo nnbabyl-request-group (group &optional server dont-check info)
  (let ((active (cadr (assoc group nnbabyl-group-alist))))
    (save-excursion
      (cond
       ((or (null active)
	    (null (nnbabyl-possibly-change-newsgroup group server)))
	(nnheader-report 'nnbabyl "No such group: %s" group))
       (dont-check
	(nnheader-report 'nnbabyl "Selected group %s" group)
	(nnheader-insert ""))
       (t
	(nnheader-report 'nnbabyl "Selected group %s" group)
	(nnheader-insert "211 %d %d %d %s\n"
			 (1+ (- (cdr active) (car active)))
			 (car active) (cdr active) group))))))

(deffoo nnbabyl-request-scan (&optional group server)
  (nnbabyl-possibly-change-newsgroup group server)
  (nnbabyl-read-mbox)
  (nnmail-get-new-mail
   'nnbabyl
   (lambda ()
     (with-current-buffer nnbabyl-mbox-buffer
       (save-buffer)))
   (file-name-directory nnbabyl-mbox-file)
   group
   (lambda ()
     (save-excursion
       (let ((in-buf (current-buffer)))
	 (goto-char (point-min))
	 (while (search-forward "\n\^_\n" nil t)
	   (delete-char -1))
	 (set-buffer nnbabyl-mbox-buffer)
	 (goto-char (point-max))
	 (search-backward "\n\^_" nil t)
	 (goto-char (match-end 0))
	 (insert-buffer-substring in-buf)))
     (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file))))

(deffoo nnbabyl-close-group (group &optional server)
  t)

(deffoo nnbabyl-request-create-group (group &optional server args)
  (nnmail-activate 'nnbabyl)
  (unless (assoc group nnbabyl-group-alist)
    (push (list group (cons 1 0))
	  nnbabyl-group-alist)
    (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file))
  t)

(deffoo nnbabyl-request-list (&optional server)
  (save-excursion
    (nnmail-find-file nnbabyl-active-file)
    (setq nnbabyl-group-alist (nnmail-get-active))
    t))

(deffoo nnbabyl-request-newgroups (date &optional server)
  (nnbabyl-request-list server))

(deffoo nnbabyl-request-list-newsgroups (&optional server)
  (nnheader-report 'nnbabyl "nnbabyl: LIST NEWSGROUPS is not implemented."))

(deffoo nnbabyl-request-expire-articles
    (articles newsgroup &optional server force)
  (nnbabyl-possibly-change-newsgroup newsgroup server)
  (let* ((is-old t)
	 rest)
    (nnmail-activate 'nnbabyl)

    (with-current-buffer nnbabyl-mbox-buffer
      (set-text-properties (point-min) (point-max) nil)
      (while (and articles is-old)
	(goto-char (point-min))
	(when (search-forward (nnbabyl-article-string (car articles)) nil t)
	  (if (setq is-old
		    (nnmail-expired-article-p
		     newsgroup
		     (buffer-substring
		      (point) (progn (end-of-line) (point))) force))
	      (progn
		(unless (eq nnmail-expiry-target 'delete)
		  (with-temp-buffer
		    (nnbabyl-request-article (car articles)
					     newsgroup server
					     (current-buffer))
		    (let ((nnml-current-directory nil))
		      (nnmail-expiry-target-group
		       nnmail-expiry-target newsgroup)))
		  (nnbabyl-possibly-change-newsgroup newsgroup server))
		(nnheader-message 5 "Deleting article %d in %s..."
				  (car articles) newsgroup)
		(nnbabyl-delete-mail))
	    (push (car articles) rest)))
	(setq articles (cdr articles)))
      (save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnbabyl-group-alist))))
	(goto-char (point-min))
	(while (and (not (search-forward
			  (nnbabyl-article-string (car active)) nil t))
		    (<= (car active) (cdr active)))
	  (setcar active (1+ (car active)))
	  (goto-char (point-min))))
      (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
      (nconc rest articles))))

(deffoo nnbabyl-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (let ((buf (get-buffer-create " *nnbabyl move*"))
	result)
    (and
     (nnbabyl-request-article article group server)
     (with-current-buffer buf
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (while (re-search-forward
	       "^X-Gnus-Newsgroup:"
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (delete-region (point-at-bol) (progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (save-excursion
       (nnbabyl-possibly-change-newsgroup group server)
       (set-buffer nnbabyl-mbox-buffer)
       (goto-char (point-min))
       (if (search-forward (nnbabyl-article-string article) nil t)
	   (nnbabyl-delete-mail))
       (and last (save-buffer))))
    result))

(deffoo nnbabyl-request-accept-article (group &optional server last)
  (nnbabyl-possibly-change-newsgroup group server)
  (nnmail-check-syntax)
  (let ((buf (current-buffer))
	result beg)
    (and
     (nnmail-activate 'nnbabyl)
     (save-excursion
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (save-excursion
	 (while (re-search-backward "^X-Gnus-Newsgroup: " beg t)
	   (delete-region (point) (progn (forward-line 1) (point)))))
       (when nnmail-cache-accepted-message-ids
	 (nnmail-cache-insert (nnmail-fetch-field "message-id")
			      group
			      (nnmail-fetch-field "subject")
			      (nnmail-fetch-field "from")))
       (setq result
	     (if (stringp group)
		 (list (cons group (nnbabyl-active-number group)))
	       (nnmail-article-group 'nnbabyl-active-number)))
       (if (and (null result)
		(yes-or-no-p "Moved to `junk' group; delete article? "))
	   (setq result 'junk)
	 (setq result (car (nnbabyl-save-mail result))))
       (set-buffer nnbabyl-mbox-buffer)
       (goto-char (point-max))
       (search-backward "\n\^_")
       (goto-char (match-end 0))
       (insert-buffer-substring buf)
       (when last
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-insert (nnmail-fetch-field "message-id")
				group
				(nnmail-fetch-field "subject")
				(nnmail-fetch-field "from")))
	 (save-buffer)
	 (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file))
       result))))

(deffoo nnbabyl-request-replace-article (article group buffer)
  (nnbabyl-possibly-change-newsgroup group)
  (with-current-buffer nnbabyl-mbox-buffer
    (goto-char (point-min))
    (if (not (search-forward (nnbabyl-article-string article) nil t))
	nil
      (nnbabyl-delete-mail t t)
      (insert-buffer-substring buffer)
      (save-buffer)
      t)))

(deffoo nnbabyl-request-delete-group (group &optional force server)
  (nnbabyl-possibly-change-newsgroup group server)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    (with-current-buffer nnbabyl-mbox-buffer
      (goto-char (point-min))
      ;; Delete all articles in this group.
      (let ((ident (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":"))
	    found)
	(while (search-forward ident nil t)
	  (setq found t)
	  (nnbabyl-delete-mail))
	(when found
	  (save-buffer)))))
  ;; Remove the group from all structures.
  (setq nnbabyl-group-alist
	(delq (assoc group nnbabyl-group-alist) nnbabyl-group-alist)
	nnbabyl-current-group nil)
  ;; Save the active file.
  (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
  t)

(deffoo nnbabyl-request-rename-group (group new-name &optional server)
  (nnbabyl-possibly-change-newsgroup group server)
  (with-current-buffer nnbabyl-mbox-buffer
    (goto-char (point-min))
    (let ((ident (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":"))
	  (new-ident (concat "\nX-Gnus-Newsgroup: " new-name ":"))
	  found)
      (while (search-forward ident nil t)
	(replace-match new-ident t t)
	(setq found t))
      (when found
	(save-buffer))))
  (let ((entry (assoc group nnbabyl-group-alist)))
    (and entry (setcar entry new-name))
    (setq nnbabyl-current-group nil)
    ;; Save the new group alist.
    (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
    t))


;;; Internal functions.

;; If FORCE, delete article no matter how many X-Gnus-Newsgroup
;; headers there are.  If LEAVE-DELIM, don't delete the Unix mbox
;; delimiter line.
(defun nnbabyl-delete-mail (&optional force leave-delim)
  ;; Delete the current X-Gnus-Newsgroup line.
  (unless force
    (delete-region (point-at-bol) (progn (forward-line 1) (point))))
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region
       (save-excursion
	 (unless (re-search-backward (concat "^" nnbabyl-mail-delimiter) nil t)
	   (goto-char (point-min))
	   (end-of-line))
	 (if leave-delim (progn (forward-line 1) (point))
	   (match-beginning 0)))
       (progn
	 (forward-line 1)
	 (or (and (re-search-forward (concat "^" nnbabyl-mail-delimiter)
				     nil t)
		  (match-beginning 0))
	     (point-max))))
      (goto-char (point-min))
      ;; Only delete the article if no other groups owns it as well.
      (when (or force (not (re-search-forward "^X-Gnus-Newsgroup: " nil t)))
	(delete-region (point-min) (point-max))))))

(defun nnbabyl-possibly-change-newsgroup (newsgroup &optional server)
  (when (and server
	     (not (nnbabyl-server-opened server)))
    (nnbabyl-open-server server))
  (when (or (not nnbabyl-mbox-buffer)
	    (not (buffer-name nnbabyl-mbox-buffer)))
    (save-excursion (nnbabyl-read-mbox)))
  (unless nnbabyl-group-alist
    (nnmail-activate 'nnbabyl))
  (if newsgroup
      (if (assoc newsgroup nnbabyl-group-alist)
	  (setq nnbabyl-current-group newsgroup)
	(nnheader-report 'nnbabyl "No such group in file"))
    t))

(defun nnbabyl-article-string (article)
  (if (numberp article)
      (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":"
	      (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnbabyl-article-group-number ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^X-Gnus-Newsgroup: +\\([^:]+\\):\\([0-9]+\\) "
			     nil t)
      (cons (buffer-substring (match-beginning 1) (match-end 1))
	    (string-to-number
	     (buffer-substring (match-beginning 2) (match-end 2)))))))

(defun nnbabyl-insert-lines ()
  "Insert how many lines and chars there are in the body of the mail."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	;; There may be an EOOH line here...
	(when (looking-at "\\*\\*\\* EOOH \\*\\*\\*")
	  (search-forward "\n\n" nil t))
	(setq chars (- (point-max) (point))
	      lines (max (- (count-lines (point) (point-max)) 1) 0))
	;; Move back to the end of the headers.
	(goto-char (point-min))
	(search-forward "\n\n" nil t)
	(forward-char -1)
	(save-excursion
	  (when (re-search-backward "^Lines: " nil t)
	    (delete-region (point) (progn (forward-line 1) (point)))))
	(insert (format "Lines: %d\n" lines))
	chars))))

(defun nnbabyl-save-mail (group-art)
  ;; Called narrowed to an article.
  (nnbabyl-insert-lines)
  (nnmail-insert-xref group-art)
  (nnbabyl-insert-newsgroup-line group-art)
  (run-hooks 'nnbabyl-prepare-save-mail-hook)
  group-art)

(defun nnbabyl-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "Mail-from: From " t t)
      (forward-line 1))
    ;; If there is a C-l at the beginning of the narrowed region, this
    ;; isn't really a "save", but rather a "scan".
    (goto-char (point-min))
    (unless (looking-at "\^L")
      (save-excursion
	(insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	(goto-char (point-max))
	(insert "\^_\n")))
    (when (search-forward "\n\n" nil t)
      (forward-char -1)
      (while group-art
	(insert (format "X-Gnus-Newsgroup: %s:%d   %s\n"
			(caar group-art) (cdar group-art)
			(current-time-string)))
	(setq group-art (cdr group-art))))
    t))

(defun nnbabyl-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (cadr (assoc group nnbabyl-group-alist))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (push (list group (setq active (cons 1 1)))
	    nnbabyl-group-alist))
    (cdr active)))

(defun nnbabyl-create-mbox ()
  (unless (file-exists-p nnbabyl-mbox-file)
    ;; Create a new, empty RMAIL mbox file.
    (with-current-buffer (setq nnbabyl-mbox-buffer
			       (create-file-buffer nnbabyl-mbox-file))
      (setq buffer-file-name nnbabyl-mbox-file)
      (insert "BABYL OPTIONS:\n\n\^_")
      (nnmail-write-region
       (point-min) (point-max) nnbabyl-mbox-file t 'nomesg))))

(defun nnbabyl-read-mbox ()
  (nnmail-activate 'nnbabyl)
  (nnbabyl-create-mbox)

  (unless (and nnbabyl-mbox-buffer
	       (buffer-name nnbabyl-mbox-buffer)
	       (with-current-buffer nnbabyl-mbox-buffer
		 (= (buffer-size) (nnheader-file-size nnbabyl-mbox-file))))
    ;; This buffer has changed since we read it last.  Possibly.
    (save-excursion
      (let ((delim (concat "^" nnbabyl-mail-delimiter))
	    (alist nnbabyl-group-alist)
	    start end number)
	(set-buffer (setq nnbabyl-mbox-buffer
			  (nnheader-find-file-noselect
			   nnbabyl-mbox-file nil t)))
	;; Save previous buffer mode.
	(setq nnbabyl-previous-buffer-mode
	      (cons (cons (point-min) (point-max))
		    major-mode))

	(buffer-disable-undo)
	(widen)
	(setq buffer-read-only nil)
	(fundamental-mode)

	;; Go through the group alist and compare against
	;; the rmail file.
	(while alist
	  (goto-char (point-max))
	  (when (and (re-search-backward
		      (format "^X-Gnus-Newsgroup: %s:\\([0-9]+\\) "
			      (caar alist))
		      nil t)
		     (> (setq number
			      (string-to-number
			       (buffer-substring
				(match-beginning 1) (match-end 1))))
			(cdadar alist)))
	    (setcdr (cadar alist) number))
	  (setq alist (cdr alist)))

	;; We go through the mbox and make sure that each and
	;; every mail belongs to some group or other.
	(goto-char (point-min))
	(if (looking-at "\^L")
	    (setq start (point))
	  (re-search-forward delim nil t)
	  (setq start (match-end 0)))
	(while (re-search-forward delim nil t)
	  (setq end (match-end 0))
	  (unless (search-backward "\nX-Gnus-Newsgroup: " start t)
	    (goto-char end)
	    (save-excursion
	      (save-restriction
		(narrow-to-region (goto-char start) end)
		(nnbabyl-save-mail
		 (nnmail-article-group 'nnbabyl-active-number))
		(setq end (point-max)))))
	  (goto-char (setq start end)))
	(when (buffer-modified-p (current-buffer))
	  (save-buffer))
	(nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)))))

(defun nnbabyl-remove-incoming-delims ()
  (goto-char (point-min))
  (while (search-forward "\^_" nil t)
    (replace-match "?" t t)))

(defun nnbabyl-check-mbox ()
  "Go through the nnbabyl mbox and make sure that no article numbers are reused."
  (interactive)
  (let ((idents (make-vector 1000 0))
	id)
    (save-excursion
      (when (or (not nnbabyl-mbox-buffer)
		(not (buffer-name nnbabyl-mbox-buffer)))
	(nnbabyl-read-mbox))
      (set-buffer nnbabyl-mbox-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^X-Gnus-Newsgroup: \\([^ ]+\\) "  nil t)
	(if (intern-soft (setq id (match-string 1)) idents)
	    (progn
	      (delete-region (point-at-bol) (progn (forward-line 1) (point)))
	      (nnheader-message 7 "Moving %s..." id)
	      (nnbabyl-save-mail
	       (nnmail-article-group 'nnbabyl-active-number)))
	  (intern id idents)))
      (when (buffer-modified-p (current-buffer))
	(save-buffer))
      (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
      (nnheader-message 5 ""))))

(provide 'nnbabyl)

;;; nnbabyl.el ends here
