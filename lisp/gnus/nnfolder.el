;;; nnfolder.el --- mail folder access for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org> (adding MARKS)
;;      ShengHuo Zhu <zsh@cs.rochester.edu> (adding NOV)
;;      Scott Byer <byer@mv.us.adobe.com>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: mail

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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(require 'nnheader)
(require 'message)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))
(require 'gnus)
(require 'gnus-util)
(require 'gnus-range)

;; FIXME not explicitly used in this file.
(autoload 'gnus-article-unpropagatable-p "gnus-sum")

(nnoo-declare nnfolder)

(defvoo nnfolder-directory (expand-file-name message-directory)
  "The name of the nnfolder directory.")

(defvoo nnfolder-nov-directory nil
  "The name of the nnfolder NOV directory.
If nil, `nnfolder-directory' is used.")

(defvoo nnfolder-marks-directory nil
  "The name of the nnfolder MARKS directory.
If nil, `nnfolder-directory' is used.")

(defvoo nnfolder-active-file
    (nnheader-concat nnfolder-directory "active")
  "The name of the active file.")

;; I renamed this variable to something more in keeping with the general GNU
;; style. -SLB

(defvoo nnfolder-ignore-active-file nil
  "If non-nil, the active file is ignored.
This causes nnfolder to do some extra work in order to determine the
true active ranges of an mbox file.  Note that the active file is
still saved, but its values are not used.  This costs some extra time
when scanning an mbox when opening it.")

(defvoo nnfolder-distrust-mbox nil
  "If non-nil, the folder will be distrusted.
This means that nnfolder will not trust the user with respect to
inserting unaccounted for mail in the middle of an mbox file.  This
can greatly slow down scans, which now must scan the entire file for
unmarked messages.  When nil, scans occur forward from the last marked
message, a huge time saver for large mailboxes.")

(defvoo nnfolder-newsgroups-file
    (concat (file-name-as-directory nnfolder-directory) "newsgroups")
  "Mail newsgroups description file.")

(defvoo nnfolder-get-new-mail t
  "If non-nil, nnfolder will check the incoming mail file and split the mail.")

(defvoo nnfolder-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")

(defvoo nnfolder-save-buffer-hook nil
  "Hook run before saving the nnfolder mbox buffer.")


(defvoo nnfolder-inhibit-expiry nil
  "If non-nil, inhibit expiry.")



(defconst nnfolder-version "nnfolder 2.0"
  "nnfolder version.")

(defconst nnfolder-article-marker "X-Gnus-Article-Number: "
  "String used to demarcate what the article number for a message is.")

(defvoo nnfolder-current-group nil)
(defvoo nnfolder-current-buffer nil)
(defvoo nnfolder-status-string "")
(defvoo nnfolder-group-alist nil)
(defvoo nnfolder-buffer-alist nil)
(defvoo nnfolder-scantime-alist nil)
(defvoo nnfolder-active-timestamp nil)
(defvoo nnfolder-active-file-coding-system mm-text-coding-system)
(defvoo nnfolder-active-file-coding-system-for-write
    nnmail-active-file-coding-system)
(defvoo nnfolder-file-coding-system mm-text-coding-system)
(defvoo nnfolder-file-coding-system-for-write nnheader-file-coding-system
  "Coding system for save nnfolder file.
if nil, `nnfolder-file-coding-system' is used.") ; FIXME: fill-in the doc-string of this variable

(defvoo nnfolder-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much.  If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnfolder-generate-active-file' command.  The function will go
through all nnfolder directories and generate nov databases for them
all.  This may very well take some time.")

(defvoo nnfolder-nov-file-suffix ".nov")

(defvoo nnfolder-nov-buffer-alist nil)

(defvar nnfolder-nov-buffer-file-name nil)

(defvoo nnfolder-marks-is-evil nil
  "If non-nil, Gnus will never generate and use marks file for mail groups.
Using marks files makes it possible to backup and restore mail groups
separately from `.newsrc.eld'.  If you have, for some reason, set
this to t, and want to set it to nil again, you should always remove
the corresponding marks file (usually base nnfolder file name
concatenated with `.mrk', but see `nnfolder-marks-file-suffix') for
the group.  Then the marks file will be regenerated properly by Gnus.")

(defvoo nnfolder-marks nil)

(defvoo nnfolder-marks-file-suffix ".mrk")

(defvar nnfolder-marks-modtime (gnus-make-hashtable))



;;; Interface functions

(nnoo-define-basics nnfolder)

(deffoo nnfolder-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let (article start stop num)
      (nnfolder-possibly-change-group group server)
      (when nnfolder-current-buffer
	(set-buffer nnfolder-current-buffer)
	(goto-char (point-min))
	(if (stringp (car articles))
	    'headers
	  (if (nnfolder-retrieve-headers-with-nov articles fetch-old)
	      'nov
	    (setq articles (gnus-sorted-intersection
			    ;; Is ARTICLES sorted?
			    (sort articles '<)
			    (nnfolder-existing-articles)))
	    (while (setq article (pop articles))
	      (set-buffer nnfolder-current-buffer)
	      (cond ((nnfolder-goto-article article)
		     (setq start (point))
		     (setq stop (if (search-forward "\n\n" nil t)
				    (1- (point))
				  (point-max)))
		     (set-buffer nntp-server-buffer)
		     (insert (format "221 %d Article retrieved.\n" article))
		     (insert-buffer-substring nnfolder-current-buffer
					      start stop)
		     (goto-char (point-max))
		     (insert ".\n"))

		    ;; If we couldn't find this article, skip over ranges
		    ;; of missing articles so we don't search the whole file
		    ;; for each of them.
		    ((numberp article)
		     (setq start (point))
		     (and
		      ;; Check that we are either at BOF or after an
		      ;; article with a lower number.  We do this so we
		      ;; won't be confused by out-of-order article numbers,
		      ;; as caused by active file bogosity.
		      (cond
		       ((bobp))
		       ((search-backward (concat "\n" nnfolder-article-marker)
					 nil t)
			(goto-char (match-end 0))
			(setq num (string-to-number
				   (buffer-substring
				    (point) (point-at-eol))))
			(goto-char start)
			(< num article)))
		      ;; Check that we are before an article with a
		      ;; higher number.
		      (search-forward (concat "\n" nnfolder-article-marker)
				      nil t)
		      (progn
			(setq num (string-to-number
				   (buffer-substring
				    (point) (point-at-eol))))
			(> num article))
		      ;; Discard any article numbers before the one we're
		      ;; now looking at.
		      (while (and articles
				  (< (car articles) num))
			(setq articles (cdr articles))))
		     (goto-char start))))
	    (set-buffer nntp-server-buffer)
	    (nnheader-fold-continuation-lines)
	    'headers))))))

(deffoo nnfolder-open-server (server &optional defs)
  (nnoo-change-server 'nnfolder server defs)
  (nnmail-activate 'nnfolder t)
  (gnus-make-directory nnfolder-directory)
  (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
    (and nnfolder-nov-directory
	 (gnus-make-directory nnfolder-nov-directory)))
  (unless nnfolder-marks-is-evil
    (and nnfolder-marks-directory
	 (gnus-make-directory nnfolder-marks-directory)))
  (cond
   ((not (file-exists-p nnfolder-directory))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Couldn't create directory: %s"
		     nnfolder-directory))
   ((not (file-directory-p (file-truename nnfolder-directory)))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Not a directory: %s" nnfolder-directory))
   (t
    (nnmail-activate 'nnfolder)
    (nnheader-report 'nnfolder "Opened server %s using directory %s"
		     server nnfolder-directory)
    t)))

(deffoo nnfolder-request-close ()
  (let ((alist nnfolder-buffer-alist))
    (while alist
      (nnfolder-close-group (caar alist) nil t)
      (setq alist (cdr alist))))
  (nnoo-close-server 'nnfolder)
  (setq nnfolder-buffer-alist nil
	nnfolder-group-alist nil))

(deffoo nnfolder-request-article (article &optional group server buffer)
  (nnfolder-possibly-change-group group server)
  (with-current-buffer nnfolder-current-buffer
    (goto-char (point-min))
    (when (nnfolder-goto-article article)
      (let (start stop)
	(setq start (point))
	(forward-line 1)
	(unless (and (nnmail-search-unix-mail-delim)
		     (forward-line -1))
	  (goto-char (point-max)))
	(setq stop (point))
	(let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (insert-buffer-substring nnfolder-current-buffer start stop)
	  (goto-char (point-min))
	  (while (looking-at "From ")
	    (delete-char 5)
	    (insert "X-From-Line: ")
	    (forward-line 1))
	  (if (numberp article)
	      (cons nnfolder-current-group article)
	    (goto-char (point-min))
	    (cons nnfolder-current-group
		  (if (search-forward (concat "\n" nnfolder-article-marker)
				      nil t)
		      (string-to-number (buffer-substring
				      (point) (point-at-eol)))
		    -1))))))))

(deffoo nnfolder-request-group (group &optional server dont-check info)
  (nnfolder-possibly-change-group group server t)
  (save-excursion
    (cond ((not (assoc group nnfolder-group-alist))
	   (nnheader-report 'nnfolder "No such group: %s" group))
	  ((file-directory-p (nnfolder-group-pathname group))
	   (nnheader-report 'nnfolder "%s is a directory"
			    (file-name-as-directory
			     (let ((nnmail-pathname-coding-system nil))
			       (nnfolder-group-pathname group)))))
	  (dont-check
	   (nnheader-report 'nnfolder "Selected group %s" group)
	   t)
	  (t
	   (let* ((active (assoc group nnfolder-group-alist))
		  (group (car active))
		  (range (cadr active)))
	     (cond
	      ((null active)
	       (nnheader-report 'nnfolder "No such group: %s" group))
	      ((null nnfolder-current-group)
	       (nnheader-report 'nnfolder "Empty group: %s" group))
	      (t
	       (nnheader-report 'nnfolder "Selected group %s" group)
	       (nnheader-insert "211 %d %d %d %s\n"
				(1+ (- (cdr range) (car range)))
				(car range) (cdr range) group))))))))

(deffoo nnfolder-request-scan (&optional group server)
  (nnfolder-possibly-change-group nil server)
  (when nnfolder-get-new-mail
    (nnfolder-possibly-change-group group server)
    (nnmail-get-new-mail
     'nnfolder 'nnfolder-save-all-buffers
     nnfolder-directory group)))

(defun nnfolder-save-all-buffers ()
  (let ((bufs nnfolder-buffer-alist))
    (save-excursion
      (while bufs
	(if (not (gnus-buffer-live-p (nth 1 (car bufs))))
	    (setq nnfolder-buffer-alist
		  (delq (car bufs) nnfolder-buffer-alist))
	  (set-buffer (nth 1 (car bufs)))
	  (nnfolder-save-buffer)
	  (kill-buffer (current-buffer)))
	(setq bufs (cdr bufs))))))

;; Don't close the buffer if we're not shutting down the server.  This way,
;; we can keep the buffer in the group buffer cache, and not have to grovel
;; over the buffer again unless we add new mail to it or modify it in some
;; way.

(deffoo nnfolder-close-group (group &optional server force)
  ;; Make sure we _had_ the group open.
  (when (or (assoc group nnfolder-buffer-alist)
	    (equal group nnfolder-current-group))
    (let ((inf (assoc group nnfolder-buffer-alist)))
      (when inf
	(when (and nnfolder-current-group
		   nnfolder-current-buffer)
	  (push (list nnfolder-current-group nnfolder-current-buffer)
		nnfolder-buffer-alist))
	(setq nnfolder-buffer-alist
	      (delq inf nnfolder-buffer-alist))
	(setq nnfolder-current-buffer (cadr inf)
	      nnfolder-current-group (car inf))))
    (when (and nnfolder-current-buffer
	       (buffer-name nnfolder-current-buffer))
      (with-current-buffer nnfolder-current-buffer
	;; If the buffer was modified, write the file out now.
	(nnfolder-save-buffer)
	;; If we're shutting the server down, we need to kill the
	;; buffer and remove it from the open buffer list.  Or, of
	;; course, if we're trying to minimize our space impact.
	(kill-buffer (current-buffer))
	(setq nnfolder-buffer-alist (delq (assoc group nnfolder-buffer-alist)
					  nnfolder-buffer-alist)))))
  (setq nnfolder-current-group nil
	nnfolder-current-buffer nil)
  t)

(deffoo nnfolder-request-create-group (group &optional server args)
  (nnfolder-possibly-change-group nil server)
  (nnmail-activate 'nnfolder)
  (cond ((zerop (length group))
	 (nnheader-report 'nnfolder "Invalid (empty) group name"))
	((file-directory-p (nnfolder-group-pathname group))
	 (nnheader-report 'nnfolder "%s is a directory"
			  (file-name-as-directory
			   (let ((nnmail-pathname-coding-system nil))
			     (nnfolder-group-pathname group)))))
	((assoc group nnfolder-group-alist)
	 t)
	(t
	 (push (list group (cons 1 0)) nnfolder-group-alist)
	 (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)
	 (save-current-buffer
	   (nnfolder-read-folder group))
	 t)))

(deffoo nnfolder-request-list (&optional server)
  (nnfolder-possibly-change-group nil server)
  (save-excursion
    (let ((nnmail-file-coding-system nnfolder-active-file-coding-system))
      (nnmail-find-file nnfolder-active-file)
      (setq nnfolder-group-alist (nnmail-get-active)))
    t))

(deffoo nnfolder-request-newgroups (date &optional server)
  (nnfolder-possibly-change-group nil server)
  (nnfolder-request-list server))

(deffoo nnfolder-request-list-newsgroups (&optional server)
  (nnfolder-possibly-change-group nil server)
  (save-excursion
    (let ((nnmail-file-coding-system nnfolder-file-coding-system))
      (nnmail-find-file nnfolder-newsgroups-file))))

;; Return a list consisting of all article numbers existing in the
;; current folder.

(defun nnfolder-existing-articles ()
  (save-excursion
    (when nnfolder-current-buffer
      (set-buffer nnfolder-current-buffer)
      (goto-char (point-min))
      (let ((marker (concat "\n" nnfolder-article-marker))
	    (number "[0-9]+")
	    numbers)
	(while (and (search-forward marker nil t)
		    (re-search-forward number nil t))
	  (let ((newnum (string-to-number (match-string 0))))
	    (if (nnmail-within-headers-p)
		(push newnum numbers))))
      ;; The article numbers are increasing, so this result is sorted.
	(nreverse numbers)))))

(autoload 'gnus-request-group "gnus-int")
(declare-function gnus-request-create-group "gnus-int"
                  (group &optional gnus-command-method args))

(deffoo nnfolder-request-expire-articles (articles newsgroup
						   &optional server force)
  (nnfolder-possibly-change-group newsgroup server)
  (let ((is-old t)
	;; The articles we have deleted so far.
	(deleted-articles nil)
	;; The articles that really exist and will
	;; be expired if they are old enough.
	(maybe-expirable
	 (gnus-sorted-intersection articles (nnfolder-existing-articles)))
	target)
    (nnmail-activate 'nnfolder)

    (with-current-buffer nnfolder-current-buffer
      ;; Since messages are sorted in arrival order and expired in the
      ;; same order, we can stop as soon as we find a message that is
      ;; too old.
      (while (and maybe-expirable is-old)
	(goto-char (point-min))
	(when (and (nnfolder-goto-article (car maybe-expirable))
		   (search-forward (concat "\n" nnfolder-article-marker)
				   nil t))
	  (forward-sexp)
	  (when (setq is-old
		      (nnmail-expired-article-p
		       newsgroup
		       (buffer-substring
			(point) (progn (end-of-line) (point)))
		       force nnfolder-inhibit-expiry))
	    (setq target nnmail-expiry-target)
	    (unless (eq target 'delete)
	      (with-temp-buffer
		(nnfolder-request-article (car maybe-expirable)
					  newsgroup server (current-buffer))
		(let ((nnfolder-current-directory nil))
		  (when (functionp target)
		    (setq target (funcall target newsgroup)))
		  (when (and target (not (eq target 'delete)))
		    (if (or (gnus-request-group target)
			    (gnus-request-create-group target))
			(nnmail-expiry-target-group target newsgroup)
		      (setq target nil)))))
	      (nnfolder-possibly-change-group newsgroup server))
	    (when target
	      (nnheader-message 5 "Deleting article %d in %s..."
				(car maybe-expirable) newsgroup)
	      (nnfolder-delete-mail)
	      (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
		(nnfolder-nov-delete-article newsgroup (car maybe-expirable)))
	      ;; Must remember which articles were actually deleted
	      (push (car maybe-expirable) deleted-articles))))
	(setq maybe-expirable (cdr maybe-expirable)))
      (unless nnfolder-inhibit-expiry
	(nnheader-message 5 "Deleting articles...done"))
      (nnfolder-save-buffer)
      (nnfolder-adjust-min-active newsgroup)
      (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)
      (nnfolder-save-all-buffers)
      (gnus-sorted-difference articles (nreverse deleted-articles)))))

(deffoo nnfolder-request-move-article (article group server accept-form
					       &optional last move-is-internal)
  (save-excursion
    (let ((buf (get-buffer-create " *nnfolder move*"))
	  result)
      (and
       (nnfolder-request-article article group server)
       (with-current-buffer buf
	 (erase-buffer)
	 (insert-buffer-substring nntp-server-buffer)
	 (goto-char (point-min))
	 (while (re-search-forward
		 (concat "^" nnfolder-article-marker)
		 (save-excursion (and (search-forward "\n\n" nil t) (point)))
		 t)
	   (gnus-delete-line))
	 (setq result (eval accept-form))
	 (kill-buffer buf)
	 result)
       (save-excursion
	 (nnfolder-possibly-change-group group server)
	 (set-buffer nnfolder-current-buffer)
	 (goto-char (point-min))
	 (when (nnfolder-goto-article article)
	   (nnfolder-delete-mail))
	 (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
	   (nnfolder-nov-delete-article group article))
	 (when last
	   (nnfolder-save-buffer)
	   (nnfolder-adjust-min-active group)
	   (nnfolder-save-active nnfolder-group-alist nnfolder-active-file))))
      result)))

(deffoo nnfolder-request-accept-article (group &optional server last)
  (save-excursion
    (nnfolder-possibly-change-group group server)
    (nnmail-check-syntax)
    (let ((buf (current-buffer))
	  result art-group)
      (goto-char (point-min))
      (when (looking-at "X-From-Line: ")
	(replace-match "From ")
	(while (progn (forward-line) (looking-at "[ \t]"))
	  (delete-char -1)))
      (with-temp-buffer
	(let ((nnmail-file-coding-system nnfolder-active-file-coding-system)
	      (nntp-server-buffer (current-buffer)))
	  (nnmail-find-file nnfolder-active-file)
	  (setq nnfolder-group-alist (nnmail-parse-active))))
      (save-excursion
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (forward-line -1)
	  (goto-char (point-max)))
	(while (re-search-backward (concat "^" nnfolder-article-marker) nil t)
	  (delete-region (point) (progn (forward-line 1) (point))))
	(when nnmail-cache-accepted-message-ids
	  (nnmail-cache-insert (nnmail-fetch-field "message-id")
			       group
			       (nnmail-fetch-field "subject")
			       (nnmail-fetch-field "from")))
	(setq result (if (stringp group)
			 (list (cons group (nnfolder-active-number group)))
		       (setq art-group
			     (nnmail-article-group 'nnfolder-active-number))))
	(if (and (null result)
		 (yes-or-no-p "Moved to `junk' group; delete article? "))
	    (setq result 'junk)
	  (setq result
		(car (nnfolder-save-mail result)))))
      (when last
	(save-excursion
	  (nnfolder-possibly-change-folder (or (caar art-group) group))
	  (nnfolder-save-buffer)
	  (when nnmail-cache-accepted-message-ids
	    (nnmail-cache-close))))
      (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)
      (unless result
	(nnheader-report 'nnfolder "Couldn't store article"))
      result)))

(deffoo nnfolder-request-replace-article (article group buffer)
  (nnfolder-possibly-change-group group)
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (not (looking-at "X-From-Line: "))
	(insert "From nobody " (current-time-string) "\n")
      (replace-match "From ")
      (forward-line 1)
      (while (looking-at "[ \t]")
	(delete-char -1)
	(forward-line 1)))
    (nnfolder-normalize-buffer)
    (set-buffer nnfolder-current-buffer)
    (goto-char (point-min))
    (if (not (nnfolder-goto-article article))
	nil
      (nnfolder-delete-mail)
      (insert-buffer-substring buffer)
      (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
	(with-current-buffer buffer
	  (let ((headers (nnfolder-parse-head article
					      (point-min) (point-max))))
	    (with-current-buffer (nnfolder-open-nov group)
	      (if (nnheader-find-nov-line article)
		  (delete-region (point) (progn (forward-line 1) (point))))
	      (nnheader-insert-nov headers)))))
      (nnfolder-save-buffer)
      t)))

(deffoo nnfolder-request-delete-group (group &optional force server)
  (nnfolder-close-group group server t)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    ;; Delete the file that holds the group.
    (let ((data (nnfolder-group-pathname group))
	  (nov (nnfolder-group-nov-pathname group))
	  (mrk (nnfolder-group-marks-pathname group)))
      (ignore-errors (delete-file data))
      (ignore-errors (delete-file nov))
      (ignore-errors (delete-file mrk))))
  ;; Remove the group from all structures.
  (setq nnfolder-group-alist
	(delq (assoc group nnfolder-group-alist) nnfolder-group-alist)
	nnfolder-current-group nil
	nnfolder-current-buffer nil)
  ;; Save the active file.
  (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)
  t)

(deffoo nnfolder-request-rename-group (group new-name &optional server)
  (nnfolder-possibly-change-group group server)
  (with-current-buffer nnfolder-current-buffer
    (and (file-writable-p buffer-file-name)
	 (ignore-errors
	   (let ((new-file (nnfolder-group-pathname new-name)))
	     (gnus-make-directory (file-name-directory new-file))
	     (rename-file buffer-file-name new-file)
	     (when (file-exists-p (nnfolder-group-nov-pathname group))
	       (setq new-file (nnfolder-group-nov-pathname new-name))
	       (gnus-make-directory (file-name-directory new-file))
	       (rename-file (nnfolder-group-nov-pathname group) new-file))
	     (when (file-exists-p (nnfolder-group-marks-pathname group))
	       (setq new-file (nnfolder-group-marks-pathname new-name))
	       (gnus-make-directory (file-name-directory new-file))
	       (rename-file (nnfolder-group-marks-pathname group) new-file)))
	   t)
	 ;; That went ok, so we change the internal structures.
	 (let ((entry (assoc group nnfolder-group-alist)))
	   (and entry (setcar entry new-name))
	   (setq nnfolder-current-buffer nil
		 nnfolder-current-group nil)
	   ;; Save the new group alist.
	   (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)
	   ;; We kill the buffer instead of renaming it and stuff.
	   (kill-buffer (current-buffer))
	   t))))

(deffoo nnfolder-request-regenerate (server)
  (nnfolder-possibly-change-group nil server)
  (nnfolder-generate-active-file)
  t)


;;; Internal functions.

(defun nnfolder-adjust-min-active (group)
  ;; Find the lowest active article in this group.
  (let* ((active (cadr (assoc group nnfolder-group-alist)))
	 (marker (concat "\n" nnfolder-article-marker))
	 (number "[0-9]+")
	 (activemin (cdr active)))
    (with-current-buffer nnfolder-current-buffer
      (goto-char (point-min))
      (while (and (search-forward marker nil t)
		  (re-search-forward number nil t))
	(let ((newnum (string-to-number (match-string 0))))
	  (if (nnmail-within-headers-p)
	      (setq activemin (min activemin newnum)))))
      (setcar active activemin))))

(defun nnfolder-article-string (article)
  (if (numberp article)
      (concat "\n" nnfolder-article-marker (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnfolder-goto-article (article)
  "Place point at the start of the headers of ARTICLE.
ARTICLE can be an article number or a Message-ID.
Returns t if successful, nil otherwise."
  (let ((art-string (nnfolder-article-string article))
	start found)
    ;; It is likely that we are at or before the delimiter line.
    ;; We therefore go to the end of the previous line, and start
    ;; searching from there.
    (beginning-of-line)
    (unless (bobp)
      (forward-char -1))
    (setq start (point))
    ;; First search forward.
    (while (and (setq found (search-forward art-string nil t))
		(not (nnmail-within-headers-p))))
    ;; If unsuccessful, search backward from where we started,
    (unless found
      (goto-char start)
      (while (and (setq found (search-backward art-string nil t))
		  (not (nnmail-within-headers-p)))))
    (when found
      (nnmail-search-unix-mail-delim-backward))))

(defun nnfolder-delete-mail (&optional leave-delim)
  "Delete the message that point is in.
If optional argument LEAVE-DELIM is t, then mailbox delimiter is not
deleted.  Point is left where the deleted region was."
  (save-restriction
    (narrow-to-region
     (save-excursion
       ;; In case point is at the beginning of the message already.
       (forward-line 1)
       (nnmail-search-unix-mail-delim-backward)
       (if leave-delim (progn (forward-line 1) (point))
	 (point)))
     (progn
       (forward-line 1)
       (if (nnmail-search-unix-mail-delim)
	   (point)
	 (point-max))))
    (run-hooks 'nnfolder-delete-mail-hook)
    (delete-region (point-min) (point-max))))

(defun nnfolder-possibly-change-group (group &optional server dont-check)
  ;; Change servers.
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (unless (gnus-buffer-live-p nnfolder-current-buffer)
    (setq nnfolder-current-buffer nil
	  nnfolder-current-group nil))
  ;; Change group.
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (when (and group
	       (not (equal group nnfolder-current-group))
	       (progn
		 (nnmail-activate 'nnfolder)
		 (and (assoc group nnfolder-group-alist)
		      (file-exists-p (nnfolder-group-pathname group)))))
      (if dont-check
	  (setq nnfolder-current-group group
		nnfolder-current-buffer nil)
	(let (inf file)
	  ;; If we have to change groups, see if we don't already have
	  ;; the folder in memory.  If we do, verify the modtime and
	  ;; destroy the folder if needed so we can rescan it.
	  (setq nnfolder-current-buffer
		(nth 1 (assoc group nnfolder-buffer-alist)))

	  ;; If the buffer is not live, make sure it isn't in the
	  ;; alist.  If it is live, verify that nobody else has
	  ;; touched the file since last time.
	  (when (and nnfolder-current-buffer
		     (not (gnus-buffer-live-p nnfolder-current-buffer)))
	    (setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist)
		  nnfolder-current-buffer nil))

	  (setq nnfolder-current-group group)

	  (when (or (not nnfolder-current-buffer)
		    (not (verify-visited-file-modtime
			  nnfolder-current-buffer)))
	    (save-excursion
	      (setq file (nnfolder-group-pathname group))
	      ;; See whether we need to create the new file.
	      (unless (file-exists-p file)
		(gnus-make-directory (file-name-directory file))
		(let ((nnmail-file-coding-system
		       (or nnfolder-file-coding-system-for-write
			   nnfolder-file-coding-system-for-write)))
		  (nnmail-write-region (point-min) (point-min)
				       file t 'nomesg)))
	      (when (setq nnfolder-current-buffer (nnfolder-read-folder group))
		(set-buffer nnfolder-current-buffer)
		(push (list group nnfolder-current-buffer)
		      nnfolder-buffer-alist)))))))))

(defun nnfolder-save-mail (group-art-list)
  "Called narrowed to an article."
  (let* (save-list group-art)
    (goto-char (point-min))
    ;; The From line may have been quoted by movemail.
    (when (looking-at ">From")
      (delete-char 1))
    ;; This might come from somewhere else.
    (unless (looking-at "From ")
      (insert "From nobody " (current-time-string) "\n")
      (goto-char (point-min)))
    ;; Quote all "From " lines in the article.
    (forward-line 1)
    (let (case-fold-search)
      (while (re-search-forward "^From " nil t)
	(beginning-of-line)
	(insert "> ")))
    (setq save-list group-art-list)
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art-list)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnfolder-prepare-save-mail-hook)

    ;; Insert the mail into each of the destination groups.
    (while (setq group-art (pop group-art-list))
      ;; Kill any previous newsgroup markers.
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (forward-line -1)
	(goto-char (point-max)))
      (while (search-backward (concat "\n" nnfolder-article-marker) nil t)
	(delete-region (1+ (point)) (progn (forward-line 2) (point))))

      ;; Insert the new newsgroup marker.
      (nnfolder-insert-newsgroup-line group-art)

      (save-excursion
	(let ((beg (point-min))
	      (end (point-max))
	      (obuf (current-buffer)))
	  (nnfolder-possibly-change-folder (car group-art))
	  (let ((buffer-read-only nil))
	    (nnfolder-normalize-buffer)
	    (insert-buffer-substring obuf beg end))
	  (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
	    (set-buffer obuf)
	    (nnfolder-add-nov (car group-art) (cdr group-art)
			      (nnfolder-parse-head nil beg end))))))

    ;; Did we save it anywhere?
    save-list))

(defun nnfolder-normalize-buffer ()
  "Make sure there are two newlines at the end of the buffer."
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (delete-region (point) (point-max))
  (unless (bobp)
    (insert "\n\n")))

(defun nnfolder-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max))
      (insert "\n"))
    (forward-char -1)
    (insert (format (concat nnfolder-article-marker "%d   %s\n")
		    (cdr group-art) (message-make-date)))))

(defun nnfolder-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (cadr (assoc group nnfolder-group-alist))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (push (list group (setq active (cons 1 1)))
	    nnfolder-group-alist))
    (cdr active)))

(defun nnfolder-possibly-change-folder (group)
  (let ((inf (assoc group nnfolder-buffer-alist)))
    (if (and inf
	     (gnus-buffer-live-p (cadr inf)))
	(set-buffer (cadr inf))
      (when inf
	(setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist)))
      (when nnfolder-group-alist
	(nnfolder-save-active nnfolder-group-alist nnfolder-active-file))
      (push (list group (nnfolder-read-folder group))
	    nnfolder-buffer-alist))))

;; This method has a problem if you've accidentally let the active
;; list get out of sync with the files.  This could happen, say, if
;; you've accidentally gotten new mail with something other than Gnus
;; (but why would _that_ ever happen? :-).  In that case, we will be
;; in the middle of processing the file, ready to add new X-Gnus
;; article number markers, and we'll run across a message with no ID
;; yet - the active list _may_not_ be ready for us yet.

;; To handle this, I'm modifying this routine to maintain the maximum
;; ID seen so far, and when we hit a message with no ID, we will
;; _manually_ scan the rest of the message looking for any more,
;; possibly higher IDs.  We'll assume the maximum that we find is the
;; highest active.  Note that this shouldn't cost us much extra time
;; at all, but will be a lot less vulnerable to glitches between the
;; mbox and the active file.

(defun nnfolder-read-folder (group)
  (let* ((file (nnfolder-group-pathname group))
	 (nov  (nnfolder-group-nov-pathname group))
	 (buffer (set-buffer
		  (let ((nnheader-file-coding-system
			 nnfolder-file-coding-system))
		    (nnheader-find-file-noselect file t)))))
    (mm-enable-multibyte) ;; Use multibyte buffer for future copying.
    (buffer-disable-undo)
    (if (equal (cadr (assoc group nnfolder-scantime-alist))
	       (nth 5 (file-attributes file)))
	;; This looks up-to-date, so we don't do any scanning.
	(if (file-exists-p file)
	    buffer
	  (push (list group buffer) nnfolder-buffer-alist)
	  (set-buffer-modified-p t)
	  (nnfolder-save-buffer))
      ;; Parse the damn thing.
      (save-excursion
	(goto-char (point-min))
	;; Remove any blank lines at the start.
	(while (eq (following-char) ?\n)
	  (delete-char 1))
	(nnmail-activate 'nnfolder)
	;; Read in the file.
	(let ((delim "^From ")
	      (marker (concat "\n" nnfolder-article-marker))
	      (number "[0-9]+")
	      (active (or (cadr (assoc group nnfolder-group-alist))
			  (cons 1 0)))
	      (scantime (assoc group nnfolder-scantime-alist))
	      (minid (or (and (boundp 'most-positive-fixnum)
			      most-positive-fixnum)
			 (lsh -1 -1)))
	      maxid start end newscantime
	      novbuf articles newnum
	      buffer-read-only)
	  (setq maxid (cdr active))

	  (unless (or gnus-nov-is-evil nnfolder-nov-is-evil
		      (and (file-exists-p nov)
			   (file-newer-than-file-p nov file)))
	    (unless (file-exists-p nov)
	      (gnus-make-directory (file-name-directory nov)))
	    (with-current-buffer
		(setq novbuf (nnfolder-open-nov group))
	      (goto-char (point-min))
	      (while (not (eobp))
		(push (read novbuf) articles)
		(forward-line 1))
	      (setq articles (nreverse articles))))
	  (goto-char (point-min))

	  ;; Anytime the active number is 1 or 0, it is suspect.  In
	  ;; that case, search the file manually to find the active
	  ;; number.  Or, of course, if we're being paranoid.  (This
	  ;; would also be the place to build other lists from the
	  ;; header markers, such as expunge lists, etc., if we ever
	  ;; desired to abandon the active file entirely for mboxes.)
	  (when (or nnfolder-ignore-active-file
		    novbuf
		    (< maxid 2))
	    (while (and (search-forward marker nil t)
			(looking-at number))
	      (setq newnum (string-to-number (match-string 0)))
	      (when (nnmail-within-headers-p)
		(setq maxid (max maxid newnum)
		      minid (min minid newnum))
		(when novbuf
		  (if (memq newnum articles)
		      (setq articles (delq newnum articles))
		    (let ((headers (nnfolder-parse-head newnum)))
		      (with-current-buffer novbuf
			(nnheader-find-nov-line newnum)
			(nnheader-insert-nov headers)))))))
	    (when (and novbuf articles)
	      (with-current-buffer novbuf
		(dolist (article articles)
		  (when (nnheader-find-nov-line article)
		    (delete-region (point)
				   (progn (forward-line 1) (point)))))))
	    (setcar active (max 1 (min minid maxid)))
	    (setcdr active (max maxid (cdr active)))
	    (goto-char (point-min)))

	  ;; As long as we trust that the user will only insert
	  ;; unmarked mail at the end, go to the end and search
	  ;; backwards for the last marker.  Find the start of that
	  ;; message, and begin to search for unmarked messages from
	  ;; there.
	  (when (not (or nnfolder-distrust-mbox
			 (< maxid 2)))
	    (goto-char (point-max))
	    (unless (re-search-backward marker nil t)
	      (goto-char (point-min)))
	    ;;(when (nnmail-search-unix-mail-delim)
	    ;;  (goto-char (point-min)))
	    )

	  ;; Keep track of the active number on our own, and insert it
	  ;; back into the active list when we're done.  Also, prime
	  ;; the pump to cut down on the number of searches we do.
	  (unless (nnmail-search-unix-mail-delim)
	    (goto-char (point-max)))
	  (setq end (point-marker))
	  (while (not (= end (point-max)))
	    (setq start (marker-position end))
	    (goto-char end)
	   ;; There may be more than one "From " line, so we skip past
	    ;; them.
	    (while (looking-at delim)
	      (forward-line 1))
	    (set-marker end (if (nnmail-search-unix-mail-delim)
				(point)
			      (point-max)))
	    (goto-char start)
	    (when (not (search-forward marker end t))
	      (narrow-to-region start end)
	      (nnmail-insert-lines)
	      (nnfolder-insert-newsgroup-line
	       (cons nil
		     (setq newnum
			   (nnfolder-active-number group))))
	      (when novbuf
		(let ((headers (nnfolder-parse-head newnum (point-min)
						    (point-max))))
		  (with-current-buffer novbuf
		    (goto-char (point-max))
		    (nnheader-insert-nov headers))))
	      (widen)))

	  (set-marker end nil)
	  ;; Make absolutely sure that the active list reflects
	  ;; reality!
	  (nnfolder-save-active nnfolder-group-alist nnfolder-active-file)

	  ;; Set the scantime for this group.
	  (setq newscantime (visited-file-modtime))
	  (if scantime
	      (setcdr scantime (list newscantime))
	    (push (list group newscantime)
		  nnfolder-scantime-alist))
	  ;; Save nov.
	  (when novbuf
	    (nnfolder-save-nov))
	  (current-buffer))))))

;;;###autoload
(defun nnfolder-generate-active-file ()
  "Look for mbox folders in the nnfolder directory and make them into groups.
This command does not work if you use short group names."
  (interactive)
  (nnmail-activate 'nnfolder)
  (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
    (dolist (file (directory-files (or nnfolder-nov-directory
				       nnfolder-directory)
				   t
				   (concat
				    (regexp-quote nnfolder-nov-file-suffix)
				    "$")))
      (when (not (message-mail-file-mbox-p file))
	(ignore-errors
	  (delete-file file)))))
    (dolist (file (directory-files nnfolder-directory))
      (when (and (not (backup-file-name-p file))
		 (message-mail-file-mbox-p
		  (nnheader-concat nnfolder-directory file)))
	(let ((oldgroup (assoc file nnfolder-group-alist)))
	  (if oldgroup
	      (nnheader-message 5 "Refreshing group %s..." file)
	    (nnheader-message 5 "Adding group %s..." file))
	  (if oldgroup
	      (setq nnfolder-group-alist
		    (delq oldgroup (copy-sequence nnfolder-group-alist))))
	  (push (list file (cons 1 0)) nnfolder-group-alist)
	  (nnfolder-possibly-change-folder file)
	  (nnfolder-possibly-change-group file)
	  (nnfolder-close-group file))))
    (nnheader-message 5 ""))

(defun nnfolder-group-pathname (group)
  "Make file name for GROUP."
  (setq group
	(mm-encode-coding-string group nnmail-pathname-coding-system))
  (let ((dir (file-name-as-directory (expand-file-name nnfolder-directory))))
    ;; If this file exists, we use it directly.
    (if (or nnmail-use-long-file-names
	    (file-exists-p (concat dir group)))
	(concat dir group)
      ;; If not, we translate dots into slashes.
      (concat dir (nnheader-replace-chars-in-string group ?. ?/)))))

(defun nnfolder-group-nov-pathname (group)
  "Make pathname for GROUP NOV."
  (let ((nnfolder-directory
	 (or nnfolder-nov-directory nnfolder-directory)))
    (concat (nnfolder-group-pathname group) nnfolder-nov-file-suffix)))

(defvar copyright-update)

(defun nnfolder-save-buffer ()
  "Save the buffer."
  (when (buffer-modified-p)
    (run-hooks 'nnfolder-save-buffer-hook)
    (gnus-make-directory (file-name-directory (buffer-file-name)))
    (let ((coding-system-for-write
	   (or nnfolder-file-coding-system-for-write
	       nnfolder-file-coding-system)))
      (set (make-local-variable 'copyright-update) nil)
      (save-buffer)))
  (unless (or gnus-nov-is-evil nnfolder-nov-is-evil)
    (nnfolder-save-nov)))

(defun nnfolder-save-active (group-alist active-file)
  (let ((nnmail-active-file-coding-system
	 (or nnfolder-active-file-coding-system-for-write
	     nnfolder-active-file-coding-system)))
    (nnmail-save-active group-alist active-file)))

(defun nnfolder-open-nov (group)
  (or (cdr (assoc group nnfolder-nov-buffer-alist))
      (let ((buffer (get-buffer-create (format " *nnfolder overview %s*" group))))
	(with-current-buffer buffer
	  (set (make-local-variable 'nnfolder-nov-buffer-file-name)
	       (nnfolder-group-nov-pathname group))
	  (erase-buffer)
	  (when (file-exists-p nnfolder-nov-buffer-file-name)
	    (nnheader-insert-file-contents nnfolder-nov-buffer-file-name)))
	(push (cons group buffer) nnfolder-nov-buffer-alist)
	buffer)))

(defun nnfolder-save-nov ()
  (save-excursion
    (while nnfolder-nov-buffer-alist
      (when (buffer-name (cdar nnfolder-nov-buffer-alist))
	(set-buffer (cdar nnfolder-nov-buffer-alist))
	(when (buffer-modified-p)
	  (gnus-make-directory (file-name-directory
				nnfolder-nov-buffer-file-name))
	  (nnmail-write-region 1 (point-max) nnfolder-nov-buffer-file-name
			       nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnfolder-nov-buffer-alist (cdr nnfolder-nov-buffer-alist)))))

(defun nnfolder-nov-delete-article (group article)
  (with-current-buffer (nnfolder-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point))))
    t))

(defun nnfolder-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnfolder-nov-is-evil)
      nil
    (let ((nov (nnfolder-group-nov-pathname nnfolder-current-group)))
      (when (file-exists-p nov)
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (nnheader-insert-file-contents nov)
	  (if (and fetch-old
		   (not (numberp fetch-old)))
	      t				; Don't remove anything.
	    (nnheader-nov-delete-outside-range
	     (if fetch-old (max 1 (- (car articles) fetch-old))
	       (car articles))
	     (car (last articles)))
	    t))))))

(defun nnfolder-parse-head (&optional number b e)
  "Parse the head of the current buffer."
  (let ((buf (current-buffer))
	chars)
    (save-excursion
      (unless b
	(setq b (if (nnmail-search-unix-mail-delim-backward)
		    (point) (point-min)))
	(forward-line 1)
	(setq e (if (nnmail-search-unix-mail-delim)
		    (point) (point-max))))
      (setq chars (- e b))
      (unless (zerop chars)
	(goto-char b)
	(if (search-forward "\n\n" e t) (setq e (1- (point)))))
      (with-temp-buffer
	(insert-buffer-substring buf b e)
	(let ((headers (nnheader-parse-naked-head)))
	  (mail-header-set-chars headers chars)
	  (mail-header-set-number headers number)
	  headers)))))

(defun nnfolder-add-nov (group article headers)
  "Add a nov line for the GROUP base."
  (with-current-buffer (nnfolder-open-nov group)
    (goto-char (point-max))
    (mail-header-set-number headers article)
    (nnheader-insert-nov headers)))

(deffoo nnfolder-request-set-mark (group actions &optional server)
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (unless nnfolder-marks-is-evil
    (nnfolder-open-marks group server)
    (setq nnfolder-marks (nnheader-update-marks-actions nnfolder-marks actions))
    (nnfolder-save-marks group server))
  nil)

(deffoo nnfolder-request-marks (group info &optional server)
  ;; Change servers.
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (when (and (not nnfolder-marks-is-evil) (nnfolder-marks-changed-p group))
    (nnheader-message 8 "Updating marks for %s..." group)
    (nnfolder-open-marks group server)
    ;; Update info using `nnfolder-marks'.
    (mapc (lambda (pred)
	    (unless (memq (cdr pred) gnus-article-unpropagated-mark-lists)
	      (gnus-info-set-marks
	       info
	       (gnus-update-alist-soft
		(cdr pred)
		(cdr (assq (cdr pred) nnfolder-marks))
		(gnus-info-marks info))
	       t)))
	  gnus-article-mark-lists)
    (let ((seen (cdr (assq 'read nnfolder-marks))))
      (gnus-info-set-read info
			  (if (and (integerp (car seen))
				   (null (cdr seen)))
			      (list (cons (car seen) (car seen)))
			    seen)))
    (nnheader-message 8 "Updating marks for %s...done" group))
  info)

(defun nnfolder-group-marks-pathname (group)
  "Make pathname for GROUP NOV."
  (let ((nnfolder-directory (or nnfolder-marks-directory nnfolder-directory)))
    (concat (nnfolder-group-pathname group) nnfolder-marks-file-suffix)))

(defun nnfolder-marks-changed-p (group)
  (let ((file (nnfolder-group-marks-pathname group)))
    (if (null (gnus-gethash file nnfolder-marks-modtime))
	t ;; never looked at marks file, assume it has changed
      (not (equal (gnus-gethash file nnfolder-marks-modtime)
		  (nth 5 (file-attributes file)))))))

(defun nnfolder-save-marks (group server)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(file (nnfolder-group-marks-pathname group)))
    (condition-case err
	(progn
	  (with-temp-file file
	    (erase-buffer)
	    (gnus-prin1 nnfolder-marks)
	    (insert "\n"))
	  (gnus-sethash file
			(nth 5 (file-attributes file))
			nnfolder-marks-modtime))
      (error (or (gnus-yes-or-no-p
		  (format "Could not write to %s (%s).  Continue? " file err))
		 (error "Cannot write to %s (%s)" file err))))))

(defun nnfolder-open-marks (group server)
  (let ((file (nnfolder-group-marks-pathname group)))
    (if (file-exists-p file)
	(condition-case err
	    (with-temp-buffer
	      (gnus-sethash file (nth 5 (file-attributes file))
			    nnfolder-marks-modtime)
	      (nnheader-insert-file-contents file)
	      (setq nnfolder-marks (read (current-buffer)))
	      (dolist (el gnus-article-unpropagated-mark-lists)
		(setq nnfolder-marks (gnus-remassoc el nnfolder-marks))))
	  (error (or (gnus-yes-or-no-p
		      (format "Error reading nnfolder marks file %s (%s).  Continuing will use marks from .newsrc.eld.  Continue? " file err))
		     (error "Cannot read nnfolder marks file %s (%s)" file err))))
      ;; User didn't have a .marks file.  Probably first time
      ;; user of the .marks stuff.  Bootstrap it from .newsrc.eld.
      (let ((info (gnus-get-info
		   (gnus-group-prefixed-name
		    group
		    (gnus-server-to-method (format "nnfolder:%s" server))))))
	(nnheader-message 7 "Bootstrapping marks for %s..." group)
	(setq nnfolder-marks (gnus-info-marks info))
	(push (cons 'read (gnus-info-read info)) nnfolder-marks)
	(dolist (el gnus-article-unpropagated-mark-lists)
	  (setq nnfolder-marks (gnus-remassoc el nnfolder-marks)))
	(nnfolder-save-marks group server)
	(nnheader-message 7 "Bootstrapping marks for %s...done" group)))))

(provide 'nnfolder)

;;; nnfolder.el ends here
