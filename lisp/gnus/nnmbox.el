;;; nnmbox.el --- mail mbox access for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

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
(require 'message)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-range)
(eval-when-compile (require 'cl))

(nnoo-declare nnmbox)

(defvoo nnmbox-mbox-file (expand-file-name "~/mbox")
  "The name of the mail box file in the user's home directory.")

(defvoo nnmbox-active-file (expand-file-name "~/.mbox-active")
  "The name of the active file for the mail box.")

(defvoo nnmbox-get-new-mail t
  "If non-nil, nnmbox will check the incoming mail file and split the mail.")

(defvoo nnmbox-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defconst nnmbox-version "nnmbox 1.0"
  "nnmbox version.")

(defvoo nnmbox-current-group nil
  "Current nnmbox news group directory.")

(defvar nnmbox-mbox-buffer nil)

(defvoo nnmbox-status-string "")

(defvoo nnmbox-group-alist nil)
(defvoo nnmbox-active-timestamp nil)

(defvoo nnmbox-file-coding-system mm-binary-coding-system)
(defvoo nnmbox-file-coding-system-for-write nil)
(defvoo nnmbox-active-file-coding-system mm-binary-coding-system)
(defvoo nnmbox-active-file-coding-system-for-write nil)

(defvar nnmbox-group-building-active-articles nil)
(defvar nnmbox-group-active-articles nil)


;;; Interface functions

(nnoo-define-basics nnmbox)

(deffoo nnmbox-retrieve-headers (sequence &optional newsgroup server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let ((number (length sequence))
	  (count 0)
	  article start stop)
      (nnmbox-possibly-change-newsgroup newsgroup server)
      (while sequence
	(setq article (car sequence))
	(set-buffer nnmbox-mbox-buffer)
	(when (nnmbox-find-article article)
	  (setq start
		(save-excursion
		  (re-search-backward
		   (concat "^" message-unix-mail-delimiter) nil t)
		  (point)))
	  (search-forward "\n\n" nil t)
	  (setq stop (1- (point)))
	  (set-buffer nntp-server-buffer)
	  (insert (format "221 %d Article retrieved.\n" article))
	  (insert-buffer-substring nnmbox-mbox-buffer start stop)
	  (goto-char (point-max))
	  (insert ".\n"))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     (zerop (% count 20))
	     (nnheader-message 5 "nnmbox: Receiving headers... %d%%"
			       (/ (* count 100) number))))

      (and (numberp nnmail-large-newsgroup)
	   (> number nnmail-large-newsgroup)
	   (nnheader-message 5 "nnmbox: Receiving headers...done"))

      (set-buffer nntp-server-buffer)
      (nnheader-fold-continuation-lines)
      'headers)))

(deffoo nnmbox-open-server (server &optional defs)
  (nnoo-change-server 'nnmbox server defs)
  (nnmbox-create-mbox)
  (cond
   ((not (file-exists-p nnmbox-mbox-file))
    (nnmbox-close-server)
    (nnheader-report 'nnmbox "No such file: %s" nnmbox-mbox-file))
   ((file-directory-p nnmbox-mbox-file)
    (nnmbox-close-server)
    (nnheader-report 'nnmbox "Not a regular file: %s" nnmbox-mbox-file))
   (t
    (nnheader-report 'nnmbox "Opened server %s using mbox %s" server
		     nnmbox-mbox-file)
    t)))

(deffoo nnmbox-close-server (&optional server)
  (when (and nnmbox-mbox-buffer
	     (buffer-name nnmbox-mbox-buffer))
    (kill-buffer nnmbox-mbox-buffer))
  (nnoo-close-server 'nnmbox server)
  t)

(deffoo nnmbox-server-opened (&optional server)
  (and (nnoo-current-server-p 'nnmbox server)
       nnmbox-mbox-buffer
       (buffer-name nnmbox-mbox-buffer)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(deffoo nnmbox-request-article (article &optional newsgroup server buffer)
  (nnmbox-possibly-change-newsgroup newsgroup server)
  (with-current-buffer nnmbox-mbox-buffer
    (when (nnmbox-find-article article)
      (let (start stop)
	(re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
	(setq start (point))
	(forward-line 1)
	(setq stop (if (re-search-forward (concat "^"
						  message-unix-mail-delimiter)
					  nil 'move)
		       (match-beginning 0)
		     (point)))
	(let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (insert-buffer-substring nnmbox-mbox-buffer start stop)
	  (goto-char (point-min))
	  (while (looking-at "From ")
	    (delete-char 5)
	    (insert "X-From-Line: ")
	    (forward-line 1))
	  (if (numberp article)
	      (cons nnmbox-current-group article)
	    (nnmbox-article-group-number nil)))))))

(deffoo nnmbox-request-group (group &optional server dont-check info)
  (nnmbox-possibly-change-newsgroup nil server)
  (let ((active (cadr (assoc group nnmbox-group-alist))))
    (cond
     ((or (null active)
	  (null (nnmbox-possibly-change-newsgroup group server)))
      (nnheader-report 'nnmbox "No such group: %s" group))
     (dont-check
      (nnheader-report 'nnmbox "Selected group %s" group)
      (nnheader-insert ""))
     (t
      (nnheader-report 'nnmbox "Selected group %s" group)
      (nnheader-insert "211 %d %d %d %s\n"
		       (1+ (- (cdr active) (car active)))
		       (car active) (cdr active) group)))))

(defun nnmbox-save-buffer ()
  (let ((coding-system-for-write
	 (or nnmbox-file-coding-system-for-write
	     nnmbox-file-coding-system)))
    (save-buffer)))

(defun nnmbox-save-active (group-alist active-file)
  (let ((nnmail-active-file-coding-system
	 (or nnmbox-active-file-coding-system-for-write
	     nnmbox-active-file-coding-system)))
    (nnmail-save-active group-alist active-file)))

(deffoo nnmbox-request-scan (&optional group server)
  (nnmbox-possibly-change-newsgroup group server)
  (nnmbox-read-mbox)
  (nnmail-get-new-mail
   'nnmbox
   (lambda ()
     (with-current-buffer nnmbox-mbox-buffer
       (nnmbox-save-buffer)))
   (file-name-directory nnmbox-mbox-file)
   group
   (lambda ()
     (save-excursion
       (let ((in-buf (current-buffer)))
	 (set-buffer nnmbox-mbox-buffer)
	 (goto-char (point-max))
	 (insert-buffer-substring in-buf)))
     (nnmbox-save-active nnmbox-group-alist nnmbox-active-file))))

(deffoo nnmbox-close-group (group &optional server)
  t)

(deffoo nnmbox-request-create-group (group &optional server args)
  (nnmail-activate 'nnmbox)
  (unless (assoc group nnmbox-group-alist)
    (push (list group (cons 1 0))
	  nnmbox-group-alist)
    (nnmbox-save-active nnmbox-group-alist nnmbox-active-file))
  t)

(deffoo nnmbox-request-list (&optional server)
  (save-excursion
    (let ((nnmail-file-coding-system
	   nnmbox-active-file-coding-system))
      (nnmail-find-file nnmbox-active-file))
    (setq nnmbox-group-alist (nnmail-get-active))
    t))

(deffoo nnmbox-request-newgroups (date &optional server)
  (nnmbox-request-list server))

(deffoo nnmbox-request-list-newsgroups (&optional server)
  (nnheader-report 'nnmbox "LIST NEWSGROUPS is not implemented."))

(deffoo nnmbox-request-expire-articles
    (articles newsgroup &optional server force)
  (nnmbox-possibly-change-newsgroup newsgroup server)
  (let* ((is-old t)
	 rest)
    (nnmail-activate 'nnmbox)

    (with-current-buffer nnmbox-mbox-buffer
      (while (and articles is-old)
	(when (nnmbox-find-article (car articles))
	  (if (setq is-old
		    (nnmail-expired-article-p
		     newsgroup
		     (buffer-substring
		      (point) (progn (end-of-line) (point))) force))
	      (progn
		(unless (eq nnmail-expiry-target 'delete)
		  (with-temp-buffer
		    (nnmbox-request-article (car articles)
					     newsgroup server
					     (current-buffer))
		    (let ((nnml-current-directory nil))
		      (nnmail-expiry-target-group
		       nnmail-expiry-target newsgroup)))
		  (nnmbox-possibly-change-newsgroup newsgroup server))
		(nnheader-message 5 "Deleting article %d in %s..."
				  (car articles) newsgroup)
		(nnmbox-delete-mail))
	    (push (car articles) rest)))
	(setq articles (cdr articles)))
      (nnmbox-save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnmbox-group-alist))))
	(while (and (not (nnmbox-find-article (car active)))
		    (<= (car active) (cdr active)))
	  (setcar active (1+ (car active)))))
      (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
      (nconc rest articles))))

(deffoo nnmbox-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (let ((buf (get-buffer-create " *nnmbox move*"))
	result)
    (and
     (nnmbox-request-article article group server)
     (with-current-buffer buf
       (erase-buffer)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (while (re-search-forward
	       "^X-Gnus-Newsgroup:"
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (gnus-delete-line))
       (setq result (eval accept-form))
       (kill-buffer buf)
       result)
     (save-excursion
       (nnmbox-possibly-change-newsgroup group server)
       (set-buffer nnmbox-mbox-buffer)
       (when (nnmbox-find-article article)
	 (nnmbox-delete-mail))
       (and last (nnmbox-save-buffer))))
    result))

(deffoo nnmbox-request-accept-article (group &optional server last)
  (nnmbox-possibly-change-newsgroup group server)
  (nnmail-check-syntax)
  (let ((buf (current-buffer))
	result cont)
    (and
     (nnmail-activate 'nnmbox)
     (with-temp-buffer
       (insert-buffer-substring buf)
       (goto-char (point-min))
       (cond (;; The From line may have been quoted by movemail.
	      (looking-at (concat ">" message-unix-mail-delimiter))
	      (delete-char 1)
	      (forward-line 1))
	     ((looking-at "X-From-Line: ")
	      (replace-match "From ")
	      (forward-line 1))
	     (t
	      (insert "From nobody " (current-time-string) "\n")))
       (narrow-to-region (point)
			 (if (search-forward "\n\n" nil 'move)
			     (1- (point))
			   (point)))
       (while (re-search-backward "^X-Gnus-Newsgroup: " nil t)
	 (delete-region (point) (progn (forward-line 1) (point))))
       (when nnmail-cache-accepted-message-ids
	 (nnmail-cache-insert (message-fetch-field "message-id")
			      group
			      (message-fetch-field "subject")
			      (message-fetch-field "from")))
       (widen)
       (setq result (if (stringp group)
			(list (cons group (nnmbox-active-number group)))
		      (nnmail-article-group 'nnmbox-active-number)))
       (prog1
	   (if (and (null result)
		    (yes-or-no-p "Moved to `junk' group; delete article? "))
	       (setq result 'junk)
	     (setq result (car (nnmbox-save-mail result))))
	 (setq cont (buffer-string))))
     (with-current-buffer nnmbox-mbox-buffer
       (goto-char (point-max))
       (insert cont)
       (when last
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-close))
	 (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
	 (nnmbox-save-buffer))))
    result))

(deffoo nnmbox-request-replace-article (article group buffer)
  (nnmbox-possibly-change-newsgroup group)
  (with-current-buffer nnmbox-mbox-buffer
    (if (not (nnmbox-find-article article))
	nil
      (nnmbox-delete-mail t t)
      (insert
       (with-temp-buffer
	 (insert-buffer-substring buffer)
	 (goto-char (point-min))
	 (when (looking-at "X-From-Line:")
	   (delete-region (point) (progn (forward-line 1) (point))))
	 (while (re-search-forward (concat "^" message-unix-mail-delimiter)
				   nil t)
	   (goto-char (match-beginning 0))
	   (insert ">"))
	 (goto-char (point-max))
	 (unless (bolp)
	   (insert "\n"))
	 (buffer-string)))
      (nnmbox-save-buffer)
      t)))

(deffoo nnmbox-request-delete-group (group &optional force server)
  (nnmbox-possibly-change-newsgroup group server)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    (with-current-buffer nnmbox-mbox-buffer
      (goto-char (point-min))
      ;; Delete all articles in this group.
      (let ((ident (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"))
	    found)
	(while (search-forward ident nil t)
	  (setq found t)
	  (nnmbox-delete-mail))
	(when found
	  (nnmbox-save-buffer)))))
  ;; Remove the group from all structures.
  (setq nnmbox-group-alist
	(delq (assoc group nnmbox-group-alist) nnmbox-group-alist)
	nnmbox-current-group nil)
  ;; Save the active file.
  (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
  t)

(deffoo nnmbox-request-rename-group (group new-name &optional server)
  (nnmbox-possibly-change-newsgroup group server)
  (with-current-buffer nnmbox-mbox-buffer
    (goto-char (point-min))
    (let ((ident (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"))
	  (new-ident (concat "\nX-Gnus-Newsgroup: " new-name ":"))
	  found)
      (while (search-forward ident nil t)
	(replace-match new-ident t t)
	(setq found t))
      (when found
	(nnmbox-save-buffer))))
  (let ((entry (assoc group nnmbox-group-active-articles)))
    (when entry
      (setcar entry new-name)))
  (let ((entry (assoc group nnmbox-group-alist)))
    (when entry
      (setcar entry new-name))
    (setq nnmbox-current-group nil)
    ;; Save the new group alist.
    (nnmbox-save-active nnmbox-group-alist nnmbox-active-file)
    t))


;;; Internal functions.

;; If FORCE, delete article no matter how many X-Gnus-Newsgroup
;; headers there are.  If LEAVE-DELIM, don't delete the Unix mbox
;; delimiter line.
(defun nnmbox-delete-mail (&optional force leave-delim)
  ;; Delete the current X-Gnus-Newsgroup line.
  ;; First delete record of active article, unless the article is being
  ;; replaced, indicated by FORCE being non-nil.
  (if (not force)
      (nnmbox-record-deleted-article (nnmbox-article-group-number t)))
  (or force
      (gnus-delete-line))
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (narrow-to-region
       (prog2
	   (re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
	   (if leave-delim (progn (forward-line 1) (point))
	     (match-beginning 0))
	 (forward-line 1))
       (or (and (re-search-forward (concat "^" message-unix-mail-delimiter)
				   nil t)
		(match-beginning 0))
	   (point-max)))
      (goto-char (point-min))
      ;; Only delete the article if no other group owns it as well.
      (when (or force
		(not (re-search-forward "^X-Gnus-Newsgroup: " nil t))
		(search-backward "\n\n" nil t))
	(delete-region (point-min) (point-max))))))

(defun nnmbox-possibly-change-newsgroup (newsgroup &optional server)
  (when (and server
	     (not (nnmbox-server-opened server)))
    (nnmbox-open-server server))
  (when (or (not nnmbox-mbox-buffer)
	    (not (buffer-name nnmbox-mbox-buffer)))
    (nnmbox-read-mbox))
  (when (not nnmbox-group-alist)
    (nnmail-activate 'nnmbox))
  (if newsgroup
      (when (assoc newsgroup nnmbox-group-alist)
	(setq nnmbox-current-group newsgroup))
    t))

(defun nnmbox-article-string (article)
  (if (numberp article)
      (concat "\nX-Gnus-Newsgroup: " nnmbox-current-group ":"
	      (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnmbox-article-group-number (this-line)
  (save-excursion
    (if this-line
	(beginning-of-line)
      (goto-char (point-min)))
    (when (re-search-forward "^X-Gnus-Newsgroup: +\\([^:]+\\):\\([0-9]+\\) "
			     nil t)
      (cons (buffer-substring (match-beginning 1) (match-end 1))
	    (string-to-number
	     (buffer-substring (match-beginning 2) (match-end 2)))))))

(defun nnmbox-in-header-p (pos)
  "Return non-nil if POS is in the header of an article."
  (save-excursion
    (goto-char pos)
    (re-search-backward (concat "^" message-unix-mail-delimiter) nil t)
    (search-forward "\n\n" nil t)
    (< pos (point))))

(defun nnmbox-find-article (article)
  "Leaves point on the relevant X-Gnus-Newsgroup line if found."
  ;; Check that article is in the active range first, to avoid an
  ;; expensive exhaustive search if it isn't.
  (if (and (numberp article)
	   (not (nnmbox-is-article-active-p article)))
      nil
    (let ((art-string (nnmbox-article-string article))
	  (found nil))
      ;; There is the possibility that the X-Gnus-Newsgroup line appears
      ;; in the body of an article (for instance, if an article has been
      ;; forwarded from someone using Gnus as their mailer), so check
      ;; that the line is actually part of the article header.
      (or (and (search-forward art-string nil t)
	       (nnmbox-in-header-p (point)))
	  (progn
	    (goto-char (point-min))
	    (while (and (not found)
			(search-forward art-string nil t))
	      (setq found (nnmbox-in-header-p (point))))
	    found)))))

(defun nnmbox-record-active-article (group-art)
  (let* ((group (car group-art))
	 (article (cdr group-art))
	 (entry
	  (or (assoc group nnmbox-group-active-articles)
	      (progn
		(push (list group)
		      nnmbox-group-active-articles)
		(car nnmbox-group-active-articles)))))
    ;; add article to index, either by building complete list
    ;; in reverse order, or as a list of ranges.
    (if (not nnmbox-group-building-active-articles)
	(setcdr entry (gnus-add-to-range (cdr entry) (list article)))
      (when (memq article (cdr entry))
	(switch-to-buffer nnmbox-mbox-buffer)
	(error "Article %s:%d already exists!" group article))
      (when (and (cadr entry) (< article (cadr entry)))
	(switch-to-buffer nnmbox-mbox-buffer)
	(error "Article %s:%d out of order" group article))
      (setcdr entry (cons article (cdr entry))))))

(defun nnmbox-record-deleted-article (group-art)
  (let* ((group (car group-art))
	 (article (cdr group-art))
	 (entry
	  (or (assoc group nnmbox-group-active-articles)
	      (progn
		(push (list group)
		      nnmbox-group-active-articles)
		(car nnmbox-group-active-articles)))))
    ;; remove article from index
    (setcdr entry (gnus-remove-from-range (cdr entry) (list article)))))

(defun nnmbox-is-article-active-p (article)
  (gnus-member-of-range
   article
   (cdr (assoc nnmbox-current-group
	       nnmbox-group-active-articles))))

(defun nnmbox-save-mail (group-art)
  "Called narrowed to an article."
  (let ((delim (concat "^" message-unix-mail-delimiter)))
    (goto-char (point-min))
    ;; This might come from somewhere else.
    (if (looking-at delim)
	(forward-line 1)
      (insert "From nobody " (current-time-string) "\n"))
    ;; Quote all "From " lines in the article.
    (while (re-search-forward delim nil t)
      (goto-char (match-beginning 0))
      (insert ">")))
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (nnmail-insert-lines)
  (nnmail-insert-xref group-art)
  (nnmbox-insert-newsgroup-line group-art)
  (let ((alist group-art))
    (while alist
      (nnmbox-record-active-article (car alist))
      (setq alist (cdr alist))))
  (run-hooks 'nnmail-prepare-save-mail-hook)
  (run-hooks 'nnmbox-prepare-save-mail-hook)
  group-art)

(defun nnmbox-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (forward-char -1)
      (while group-art
	(insert (format "X-Gnus-Newsgroup: %s:%d   %s\n"
			(caar group-art) (cdar group-art)
			(current-time-string)))
	(setq group-art (cdr group-art))))
    t))

(defun nnmbox-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (cadr (assoc group nnmbox-group-alist))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (push (list group (setq active (cons 1 1)))
	    nnmbox-group-alist))
    (cdr active)))

(defun nnmbox-create-mbox ()
  (when (not (file-exists-p nnmbox-mbox-file))
    (let ((nnmail-file-coding-system
	   (or nnmbox-file-coding-system-for-write
	       nnmbox-file-coding-system))
	  (dir (file-name-directory nnmbox-mbox-file)))
      (and dir (gnus-make-directory dir))
      (nnmail-write-region (point-min) (point-min)
			   nnmbox-mbox-file t 'nomesg))))

(defun nnmbox-read-mbox ()
  (nnmail-activate 'nnmbox)
  (nnmbox-create-mbox)
  (if (and nnmbox-mbox-buffer
	   (buffer-name nnmbox-mbox-buffer)
	   (with-current-buffer nnmbox-mbox-buffer
	     (= (buffer-size) (nnheader-file-size nnmbox-mbox-file))))
      ()
    (save-excursion
      (let ((delim (concat "^" message-unix-mail-delimiter))
	    (alist nnmbox-group-alist)
	    (nnmbox-group-building-active-articles t)
	    start end end-header number)
	(set-buffer (setq nnmbox-mbox-buffer
			  (let ((nnheader-file-coding-system
				 nnmbox-file-coding-system))
			    (nnheader-find-file-noselect
			     nnmbox-mbox-file t t))))
	(mm-enable-multibyte)
	(buffer-disable-undo)
	(gnus-add-buffer)

	;; Go through the group alist and compare against the mbox file.
	(while alist
	  (goto-char (point-max))
	  (when (and (re-search-backward
		      (format "^X-Gnus-Newsgroup: %s:\\([0-9]+\\) "
			      (caar alist)) nil t)
		     (> (setq number
			      (string-to-number
			       (buffer-substring
				(match-beginning 1) (match-end 1))))
			(cdadar alist)))
	    (setcdr (cadar alist) number))
	  (setq alist (cdr alist)))

	;; Examine all articles for our private X-Gnus-Newsgroup
	;; headers.  This is done primarily as a consistency check, but
	;; it is convenient for building an index of the articles
	;; present, to avoid costly searches for missing articles
	;; (eg. when expiring articles).
	(goto-char (point-min))
	(setq nnmbox-group-active-articles nil)
	(while (re-search-forward delim nil t)
	  (setq start (match-beginning 0))
	  (save-excursion
	    (search-forward "\n\n" nil t)
	    (setq end-header (point))
	    (setq end (or (and
			   (re-search-forward delim nil t)
			   (match-beginning 0))
			  (point-max))))
	  (if (search-forward "\nX-Gnus-Newsgroup: " end-header t)
	      ;; Build a list of articles in each group, remembering
	      ;; that each article may be in more than one group.
	      (progn
		(nnmbox-record-active-article (nnmbox-article-group-number t))
		(while (search-forward "\nX-Gnus-Newsgroup: " end-header t)
		  (nnmbox-record-active-article (nnmbox-article-group-number t))))
	    ;; The article is either new, or for some other reason
	    ;; hasn't got our private headers, so add them now.  The
	    ;; only situation I've encountered when the X-Gnus-Newsgroup
	    ;; header is missing is if the article contains a forwarded
	    ;; message which does contain that header line (earlier
	    ;; versions of Gnus didn't restrict their search to the
	    ;; headers).  In this case, there is an Xref line which
	    ;; provides the relevant information to construct the
	    ;; missing header(s).
	    (save-excursion
	      (save-restriction
		(narrow-to-region start end)
		(if (re-search-forward "\nXref: [^ ]+" end-header t)
		    ;; generate headers from Xref:
		    (let (alist)
		      (while (re-search-forward " \\([^:]+\\):\\([0-9]+\\)" end-header t)
			(push (cons (match-string 1)
				    (string-to-number (match-string 2))) alist))
		      (nnmbox-insert-newsgroup-line alist))
		  ;; this is really a new article
		  (nnmbox-save-mail
		   (nnmail-article-group 'nnmbox-active-number))))))
	  (goto-char end))
	;; put article lists in order
	(setq alist nnmbox-group-active-articles)
	(while alist
	  (setcdr (car alist) (gnus-compress-sequence (nreverse (cdar alist))))
	  (setq alist (cdr alist)))))))

(provide 'nnmbox)

;;; nnmbox.el ends here
