;;; nnml.el --- mail spool access for Gnus

;; Copyright (C) 1995-2012 Free Software
;;   Foundation, Inc.

;; Authors: Didier Verna <didier@xemacs.org> (adding compaction)
;;	Simon Josefsson <simon@josefsson.org> (adding MARKS)
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.
;; For an overview of what the interface functions do, please see the
;; Gnus sources.

;;; Code:

(require 'gnus)
(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))

;; FIXME first is unused in this file.
(autoload 'gnus-article-unpropagatable-p "gnus-sum")
(autoload 'gnus-backlog-remove-article "gnus-bcklg")

(nnoo-declare nnml)

(defvoo nnml-directory message-directory
  "Spool directory for the nnml mail backend.")

(defvoo nnml-active-file
    (expand-file-name "active" nnml-directory)
  "Mail active file.")

(defvoo nnml-newsgroups-file
    (expand-file-name "newsgroups" nnml-directory)
  "Mail newsgroups description file.")

(defvoo nnml-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")

(defvoo nnml-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail spools.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much.  If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnml-generate-nov-databases' command.  The function will go
through all nnml directories and generate nov databases for them
all.  This may very well take some time.")

(defvoo nnml-marks-is-evil nil
  "If non-nil, Gnus will never generate and use marks file for mail spools.
Using marks files makes it possible to backup and restore mail groups
separately from `.newsrc.eld'.  If you have, for some reason, set this
to t, and want to set it to nil again, you should always remove the
corresponding marks file (usually named `.marks' in the nnml group
directory, but see `nnml-marks-file-name') for the group.  Then the
marks file will be regenerated properly by Gnus.")

(defvoo nnml-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")

(defvoo nnml-inhibit-expiry nil
  "If non-nil, inhibit expiry.")

(defvoo nnml-use-compressed-files nil
  "If non-nil, allow using compressed message files.

If it is a string, use it as the file extension which specifies
the compression program.  You can set it to \".bz2\" if your Emacs
supports auto-compression using the bzip2 program.  A value of t
is equivalent to \".gz\".")

(defvoo nnml-compressed-files-size-threshold 1000
  "Default size threshold for compressed message files.
Message files with bodies larger than that many characters will
be automatically compressed if `nnml-use-compressed-files' is
non-nil.")



(defconst nnml-version "nnml 1.0"
  "nnml version.")

(defvoo nnml-nov-file-name ".overview")
(defvoo nnml-marks-file-name ".marks")

(defvoo nnml-current-directory nil)
(defvoo nnml-current-group nil)
(defvoo nnml-status-string "")
(defvoo nnml-nov-buffer-alist nil)
(defvoo nnml-group-alist nil)
(defvoo nnml-active-timestamp nil)
(defvoo nnml-article-file-alist nil)

(defvoo nnml-generate-active-function 'nnml-generate-active-info)

(defvar nnml-nov-buffer-file-name nil)

(defvoo nnml-file-coding-system nnmail-file-coding-system)

(defvoo nnml-marks nil)

(defvar nnml-marks-modtime (gnus-make-hashtable))


;;; Interface functions.

(nnoo-define-basics nnml)

(eval-when-compile
  (defsubst nnml-group-name-charset (group server-or-method)
    (gnus-group-name-charset
     (if (stringp server-or-method)
	 (gnus-server-to-method
	  (if (string-match "\\+" server-or-method)
	      (concat (substring server-or-method 0 (match-beginning 0))
		      ":" (substring server-or-method (match-end 0)))
	    (concat "nnml:" server-or-method)))
       (or server-or-method gnus-command-method '(nnml "")))
     group)))

(defun nnml-decoded-group-name (group &optional server-or-method)
  "Return a decoded group name of GROUP on SERVER-OR-METHOD."
  (if nnmail-group-names-not-encoded-p
      group
    (mm-decode-coding-string
     group
     (nnml-group-name-charset group server-or-method))))

(defun nnml-encoded-group-name (group &optional server-or-method)
  "Return an encoded group name of GROUP on SERVER-OR-METHOD."
  (mm-encode-coding-string
   group
   (nnml-group-name-charset group server-or-method)))

(defun nnml-group-pathname (group &optional file server)
  "Return an absolute file name of FILE for GROUP on SERVER."
  (nnmail-group-pathname (inline (nnml-decoded-group-name group server))
			 nnml-directory file))

(deffoo nnml-retrieve-headers (sequence &optional group server fetch-old)
  (when (nnml-possibly-change-directory group server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (let* ((file nil)
	     (number (length sequence))
	     (count 0)
	     (file-name-coding-system nnmail-pathname-coding-system)
	     beg article)
	(if (stringp (car sequence))
	    'headers
	  (if (nnml-retrieve-headers-with-nov sequence fetch-old)
	      'nov
	    (while sequence
	      (setq article (car sequence))
	      (setq file (nnml-article-to-file article))
	      (when (and file
			 (file-exists-p file)
			 (not (file-directory-p file)))
		(insert (format "221 %d Article retrieved.\n" article))
		(setq beg (point))
		(nnheader-insert-head file)
		(goto-char beg)
		(if (re-search-forward "\n\r?\n" nil t)
		    (forward-char -1)
		  (goto-char (point-max))
		  (insert "\n\n"))
		(insert ".\n")
		(delete-region (point) (point-max)))
	      (setq sequence (cdr sequence))
	      (setq count (1+ count))
	      (and (numberp nnmail-large-newsgroup)
		   (> number nnmail-large-newsgroup)
		   (zerop (% count 20))
		   (nnheader-message 6 "nnml: Receiving headers... %d%%"
				     (/ (* count 100) number))))

	    (and (numberp nnmail-large-newsgroup)
		 (> number nnmail-large-newsgroup)
		 (nnheader-message 6 "nnml: Receiving headers...done"))

	    (nnheader-fold-continuation-lines)
	    'headers))))))

(deffoo nnml-open-server (server &optional defs)
  (nnoo-change-server 'nnml server defs)
  (when (not (file-exists-p nnml-directory))
    (ignore-errors (make-directory nnml-directory t)))
  (cond
   ((not (file-exists-p nnml-directory))
    (nnml-close-server)
    (nnheader-report 'nnml "Couldn't create directory: %s" nnml-directory))
   ((not (file-directory-p (file-truename nnml-directory)))
    (nnml-close-server)
    (nnheader-report 'nnml "Not a directory: %s" nnml-directory))
   (t
    (nnheader-report 'nnml "Opened server %s using directory %s"
		     server nnml-directory)
    t)))

(deffoo nnml-request-regenerate (server)
  (nnml-possibly-change-directory nil server)
  (nnml-generate-nov-databases server)
  t)

(deffoo nnml-request-article (id &optional group server buffer)
  (nnml-possibly-change-directory group server)
  (let* ((nntp-server-buffer (or buffer nntp-server-buffer))
	 (file-name-coding-system nnmail-pathname-coding-system)
	 path gpath group-num)
    (if (stringp id)
	(when (and (setq group-num (nnml-find-group-number id server))
		   (cdr
		    (assq (cdr group-num)
			  (nnheader-article-to-file-alist
			   (setq gpath (nnml-group-pathname (car group-num)
							    nil server))))))
	  (nnml-update-file-alist)
	  (setq path (concat gpath (if nnml-use-compressed-files
				       (cdr (assq (cdr group-num)
						  nnml-article-file-alist))
				     (number-to-string (cdr group-num))))))
      (setq path (nnml-article-to-file id)))
    (cond
     ((not path)
      (nnheader-report 'nnml "No such article: %s" id))
     ((not (file-exists-p path))
      (nnheader-report 'nnml "No such file: %s" path))
     ((file-directory-p path)
      (nnheader-report 'nnml "File is a directory: %s" path))
     ((not (save-excursion (let ((nnmail-file-coding-system
				  nnml-file-coding-system))
			     (nnmail-find-file path))))
      (nnheader-report 'nnml "Couldn't read file: %s" path))
     (t
      (nnheader-report 'nnml "Article %s retrieved" id)
      ;; We return the article number.
      (cons (if group-num (car group-num) group)
	    (string-to-number (file-name-nondirectory path)))))))

(deffoo nnml-request-group (group &optional server dont-check info)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(decoded (nnml-decoded-group-name group server)))
    (cond
     ((not (nnml-possibly-change-directory group server))
      (nnheader-report 'nnml "Invalid group (no such directory)"))
     ((not (file-exists-p nnml-current-directory))
      (nnheader-report 'nnml "Directory %s does not exist"
		       nnml-current-directory))
     ((not (file-directory-p nnml-current-directory))
      (nnheader-report 'nnml "%s is not a directory" nnml-current-directory))
     (dont-check
      (nnheader-report 'nnml "Group %s selected" decoded)
      t)
     (t
      (nnheader-re-read-dir nnml-current-directory)
      (nnmail-activate 'nnml)
      (let ((active (nth 1 (assoc group nnml-group-alist))))
	(if (not active)
	    (nnheader-report 'nnml "No such group: %s" decoded)
	  (nnheader-report 'nnml "Selected group %s" decoded)
	  (nnheader-insert "211 %d %d %d %s\n"
			   (max (1+ (- (cdr active) (car active))) 0)
			   (car active) (cdr active) group)))))))

(deffoo nnml-request-scan (&optional group server)
  (setq nnml-article-file-alist nil)
  (nnml-possibly-change-directory group server)
  (nnmail-get-new-mail 'nnml 'nnml-save-incremental-nov nnml-directory group))

(deffoo nnml-close-group (group &optional server)
  (setq nnml-article-file-alist nil)
  t)

(deffoo nnml-request-create-group (group &optional server args)
  (nnml-possibly-change-directory nil server)
  (nnmail-activate 'nnml)
  (cond
   ((let ((file (directory-file-name (nnml-group-pathname group nil server)))
	  (file-name-coding-system nnmail-pathname-coding-system))
      (and (file-exists-p file)
	   (not (file-directory-p file))))
    (nnheader-report 'nnml "%s is a file"
		     (directory-file-name (nnml-group-pathname group
							       nil server))))
   ((assoc group nnml-group-alist)
    t)
   (t
    (let (active)
      (push (list group (setq active (cons 1 0)))
	    nnml-group-alist)
      (nnml-possibly-create-directory group server)
      (nnml-possibly-change-directory group server)
      (let* ((file-name-coding-system nnmail-pathname-coding-system)
	     (articles (nnml-directory-articles nnml-current-directory)))
	(when articles
	  (setcar active (apply 'min articles))
	  (setcdr active (apply 'max articles))))
      (nnmail-save-active nnml-group-alist nnml-active-file)
      t))))

(deffoo nnml-request-list (&optional server)
  (save-excursion
    (let ((nnmail-file-coding-system nnmail-active-file-coding-system)
	  (file-name-coding-system nnmail-pathname-coding-system))
      (nnmail-find-file nnml-active-file))
    (setq nnml-group-alist (nnmail-get-active))
    t))

(deffoo nnml-request-newgroups (date &optional server)
  (nnml-request-list server))

(deffoo nnml-request-list-newsgroups (&optional server)
  (save-excursion
    (nnmail-find-file nnml-newsgroups-file)))

(deffoo nnml-request-expire-articles (articles group &optional server force)
  (nnml-possibly-change-directory group server)
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
	 (active-articles
	  (nnml-directory-articles nnml-current-directory))
	 (is-old t)
	 (decoded (nnml-decoded-group-name group server))
	 article rest mod-time number target)
    (nnmail-activate 'nnml)

    (setq active-articles (sort active-articles '<))
    ;; Articles not listed in active-articles are already gone,
    ;; so don't try to expire them.
    (setq articles (gnus-sorted-intersection articles active-articles))

    (while (and articles is-old)
      (if (and (setq article (nnml-article-to-file
			      (setq number (pop articles))))
	       (setq mod-time (nth 5 (file-attributes article)))
	       (nnml-deletable-article-p group number)
	       (setq is-old (nnmail-expired-article-p group mod-time force
						      nnml-inhibit-expiry)))
	  (progn
	    ;; Allow a special target group.
	    (setq target nnmail-expiry-target)
	    (unless (eq target 'delete)
	      (with-temp-buffer
		(nnml-request-article number group server (current-buffer))
		(let (nnml-current-directory
		      nnml-current-group
		      nnml-article-file-alist)
		  (when (functionp target)
		    (setq target (funcall target group)))
		  (when (and target (not (eq target 'delete)))
		    (if (or (gnus-request-group target)
			    (gnus-request-create-group target))
			(nnmail-expiry-target-group target group)
		      (setq target nil)))))
	      ;; Maybe directory is changed during nnmail-expiry-target-group.
	      (nnml-possibly-change-directory group server))
	    (if target
		(progn
		  (nnheader-message 5 "Deleting article %s in %s"
				    number decoded)
		  (condition-case ()
		      (funcall nnmail-delete-file-function article)
		    (file-error
		     (push number rest)))
		  (setq active-articles (delq number active-articles))
		  (nnml-nov-delete-article group number))
	      (push number rest)))
	(push number rest)))
    (let ((active (nth 1 (assoc group nnml-group-alist))))
      (when active
	(setcar active (or (and active-articles
				(apply 'min active-articles))
			   (1+ (cdr active)))))
      (nnmail-save-active nnml-group-alist nnml-active-file))
    (nnml-save-nov)
    (nconc rest articles)))

(deffoo nnml-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (let ((buf (get-buffer-create " *nnml move*"))
	(file-name-coding-system nnmail-pathname-coding-system)
	result)
    (nnml-possibly-change-directory group server)
    (nnml-update-file-alist)
    (and
     (nnml-deletable-article-p group article)
     (nnml-request-article article group server)
     (let (nnml-current-directory
	   nnml-current-group
	   nnml-article-file-alist)
       (with-current-buffer buf
	 (insert-buffer-substring nntp-server-buffer)
	 (setq result (eval accept-form))
	 (kill-buffer (current-buffer))
	 result))
     (progn
       (nnml-possibly-change-directory group server)
       (condition-case ()
	   (funcall nnmail-delete-file-function
		    (nnml-article-to-file  article))
	 (file-error nil))
       (nnml-nov-delete-article group article)
       (when last
	 (nnml-save-nov)
	 (nnmail-save-active nnml-group-alist nnml-active-file))))
    result))

(deffoo nnml-request-accept-article (group &optional server last)
  (nnml-possibly-change-directory group server)
  (nnmail-check-syntax)
  (let (result)
    (when nnmail-cache-accepted-message-ids
      (nnmail-cache-insert (nnmail-fetch-field "message-id")
			   group
			   (nnmail-fetch-field "subject")
			   (nnmail-fetch-field "from")))
    (if (stringp group)
	(and
	 (nnmail-activate 'nnml)
	 (setq result (car (nnml-save-mail
			    (list (cons group (nnml-active-number group
								  server)))
			    server t)))
	 (progn
	   (nnmail-save-active nnml-group-alist nnml-active-file)
	   (and last (nnml-save-nov))))
      (and
       (nnmail-activate 'nnml)
       (if (and (not (setq result (nnmail-article-group
				   `(lambda (group)
				      (nnml-active-number group ,server)))))
		(yes-or-no-p "Moved to `junk' group; delete article? "))
	   (setq result 'junk)
	 (setq result (car (nnml-save-mail result server t))))
       (when last
	 (nnmail-save-active nnml-group-alist nnml-active-file)
	 (when nnmail-cache-accepted-message-ids
	   (nnmail-cache-close))
	 (nnml-save-nov))))
    result))

(deffoo nnml-request-post (&optional server)
  (nnmail-do-request-post 'nnml-request-accept-article server))

(deffoo nnml-request-replace-article (article group buffer)
  (nnml-possibly-change-directory group)
  (with-current-buffer buffer
    (nnml-possibly-create-directory group)
    (let ((chars (nnmail-insert-lines))
	  (art (concat (int-to-string article) "\t"))
	  headers)
      (when (ignore-errors
	      (nnmail-write-region
	       (point-min) (point-max)
	       (or (nnml-article-to-file article)
		   (expand-file-name (int-to-string article)
				     nnml-current-directory))
	       nil (if (nnheader-be-verbose 5) nil 'nomesg))
	      t)
	(setq headers (nnml-parse-head chars article))
	;; Replace the NOV line in the NOV file.
	(with-current-buffer (nnml-open-nov group)
	  (goto-char (point-min))
	  (if (or (looking-at art)
		  (search-forward (concat "\n" art) nil t))
	      ;; Delete the old NOV line.
	      (gnus-delete-line)
	    ;; The line isn't here, so we have to find out where
	    ;; we should insert it.  (This situation should never
	    ;; occur, but one likes to make sure...)
	    (while (and (looking-at "[0-9]+\t")
			(< (string-to-number
			    (buffer-substring
			     (match-beginning 0) (match-end 0)))
			   article)
			(zerop (forward-line 1)))))
	  (beginning-of-line)
	  (nnheader-insert-nov headers)
	  (nnml-save-nov)
	  t)))))

(deffoo nnml-request-delete-group (group &optional force server)
  (nnml-possibly-change-directory group server)
  (let ((file (directory-file-name nnml-current-directory))
	(file-name-coding-system nnmail-pathname-coding-system))
    (if (file-exists-p file)
	(if (file-directory-p file)
	    (progn
	      (when force
		;; Delete all articles in GROUP.
		(let ((articles
		       (directory-files
			nnml-current-directory t
			(concat
			 nnheader-numerical-short-files
			 "\\|" (regexp-quote nnml-nov-file-name) "$"
			 "\\|" (regexp-quote nnml-marks-file-name) "$")))
		      (decoded (nnml-decoded-group-name group server)))
		  (dolist (article articles)
		    (when (file-writable-p article)
		      (nnheader-message 5 "Deleting article %s in %s..."
					(file-name-nondirectory article)
					decoded)
		      (funcall nnmail-delete-file-function article))))
		;; Try to delete the directory itself.
		(ignore-errors (delete-directory nnml-current-directory))))
	  (nnheader-report 'nnml "%s is not a directory" file))
      (nnheader-report 'nnml "No such directory: %s/" file))
    ;; Remove the group from all structures.
    (setq nnml-group-alist
	  (delq (assoc group nnml-group-alist) nnml-group-alist)
	  nnml-current-group nil
	  nnml-current-directory nil)
    ;; Save the active file.
    (nnmail-save-active nnml-group-alist nnml-active-file))
  t)

(deffoo nnml-request-rename-group (group new-name &optional server)
  (nnml-possibly-change-directory group server)
  (let ((new-dir (nnml-group-pathname new-name nil server))
	(old-dir (nnml-group-pathname group nil server))
	(file-name-coding-system nnmail-pathname-coding-system))
    (when (ignore-errors
	    (make-directory new-dir t)
	    t)
      ;; We move the articles file by file instead of renaming
      ;; the directory -- there may be subgroups in this group.
      ;; One might be more clever, I guess.
      (dolist (file (nnheader-article-to-file-alist old-dir))
	(rename-file
	 (concat old-dir (cdr file))
	 (concat new-dir (cdr file))))
      ;; Move .overview file.
      (let ((overview (concat old-dir nnml-nov-file-name)))
	(when (file-exists-p overview)
	  (rename-file overview (concat new-dir nnml-nov-file-name))))
      ;; Move .marks file.
      (let ((marks (concat old-dir nnml-marks-file-name)))
	(when (file-exists-p marks)
	  (rename-file marks (concat new-dir nnml-marks-file-name))))
      (when (<= (length (directory-files old-dir)) 2)
	(ignore-errors (delete-directory old-dir)))
      ;; That went ok, so we change the internal structures.
      (let ((entry (assoc group nnml-group-alist)))
	(when entry
	  (setcar entry new-name))
	(setq nnml-current-directory nil
	      nnml-current-group nil)
	;; Save the new group alist.
	(nnmail-save-active nnml-group-alist nnml-active-file)
	t))))

(deffoo nnml-set-status (article name value &optional group server)
  (nnml-possibly-change-directory group server)
  (let ((file (nnml-article-to-file article)))
    (cond
     ((not (file-exists-p file))
      (nnheader-report 'nnml "File %s does not exist" file))
     (t
      (with-temp-file file
	(nnheader-insert-file-contents file)
	(nnmail-replace-status name value))
      t))))


;;; Internal functions.

(defun nnml-article-to-file (article)
  (nnml-update-file-alist)
  (let (file)
    (if (setq file
	      (if nnml-use-compressed-files
		  (cdr (assq article nnml-article-file-alist))
		(number-to-string article)))
	(expand-file-name file nnml-current-directory)
      (when (not nnheader-directory-files-is-safe)
	;; Just to make sure nothing went wrong when reading over NFS --
	;; check once more.
	(when (file-exists-p
	       (setq file (expand-file-name (number-to-string article)
					    nnml-current-directory)))
	  (nnml-update-file-alist t)
	  file)))))

(defun nnml-deletable-article-p (group article)
  "Say whether ARTICLE in GROUP can be deleted."
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	path)
    (when (setq path (nnml-article-to-file article))
      (when (file-writable-p path)
	(or (not nnmail-keep-last-article)
	    (not (eq (cdr (nth 1 (assoc group nnml-group-alist)))
		     article)))))))

;; Find an article number in the current group given the Message-ID.
(defun nnml-find-group-number (id server)
  (with-current-buffer (get-buffer-create " *nnml id*")
    (let ((alist nnml-group-alist)
	  number)
      ;; We want to look through all .overview files, but we want to
      ;; start with the one in the current directory.  It seems most
      ;; likely that the article we are looking for is in that group.
      (if (setq number (nnml-find-id nnml-current-group id server))
	  (cons nnml-current-group number)
      ;; It wasn't there, so we look through the other groups as well.
	(while (and (not number)
		    alist)
	  (or (string= (caar alist) nnml-current-group)
	      (setq number (nnml-find-id (caar alist) id server)))
	  (or number
	      (setq alist (cdr alist))))
	(and number
	     (cons (caar alist) number))))))

(defun nnml-find-id (group id server)
  (erase-buffer)
  (let ((nov (nnml-group-pathname group nnml-nov-file-name server))
	number found)
    (when (file-exists-p nov)
      (nnheader-insert-file-contents nov)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (point-at-bol) t))))
	    (forward-line 1)
	  (beginning-of-line)
	  (setq found t)
	  ;; We return the article number.
	  (setq number
		(ignore-errors (read (current-buffer))))))
      number)))

(defun nnml-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnml-nov-is-evil)
      nil
    (let ((nov (expand-file-name nnml-nov-file-name nnml-current-directory)))
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

(defun nnml-possibly-change-directory (group &optional server)
  (when (and server
	     (not (nnml-server-opened server)))
    (nnml-open-server server))
  (if (not group)
      t
    (let ((pathname (nnml-group-pathname group nil server))
	  (file-name-coding-system nnmail-pathname-coding-system))
      (when (not (equal pathname nnml-current-directory))
	(setq nnml-current-directory pathname
	      nnml-current-group group
	      nnml-article-file-alist nil))
      (file-exists-p nnml-current-directory))))

(defun nnml-possibly-create-directory (group &optional server)
  (let ((dir (nnml-group-pathname group nil server))
	(file-name-coding-system nnmail-pathname-coding-system))
    (unless (file-exists-p dir)
      (make-directory (directory-file-name dir) t)
      (nnheader-message 5 "Creating mail directory %s" dir))))

(defun nnml-save-mail (group-art &optional server full-nov)
  "Save a mail into the groups GROUP-ART in the nnml server SERVER.
GROUP-ART is a list that each element is a cons of a group name and an
article number.  This function is called narrowed to an article."
  (let* ((chars (nnmail-insert-lines))
	 (extension (and nnml-use-compressed-files
			 (> chars nnml-compressed-files-size-threshold)
			 (if (stringp nnml-use-compressed-files)
			     nnml-use-compressed-files
			   ".gz")))
	 decoded dec file first headers)
    (when nnmail-group-names-not-encoded-p
      (dolist (ga (prog1 group-art (setq group-art nil)))
	(setq group-art (nconc group-art
			       (list (cons (nnml-encoded-group-name (car ga)
								    server)
					   (cdr ga))))
	      decoded (nconc decoded (list (car ga)))))
      (setq dec decoded))
    (nnmail-insert-xref group-art)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nnml-prepare-save-mail-hook)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the groups it belongs in.
    (dolist (ga group-art)
      (if nnmail-group-names-not-encoded-p
	  (progn
	    (nnml-possibly-create-directory (car decoded) server)
	    (setq file (nnmail-group-pathname
			(pop decoded) nnml-directory
			(concat (number-to-string (cdr ga)) extension))))
	(nnml-possibly-create-directory (car ga) server)
	(setq file (nnml-group-pathname
		    (car ga) (concat (number-to-string (cdr ga)) extension)
		    server)))
      (if first
	  ;; It was already saved, so we just make a hard link.
	  (let ((file-name-coding-system nnmail-pathname-coding-system))
	    (funcall nnmail-crosspost-link-function first file t))
	;; Save the article.
	(nnmail-write-region (point-min) (point-max) file nil
			     (if (nnheader-be-verbose 5) nil 'nomesg))
	(setq first file)))
    ;; Generate a nov line for this article.  We generate the nov
    ;; line after saving, because nov generation destroys the
    ;; header.
    (setq headers (nnml-parse-head chars))
    ;; Output the nov line to all nov databases that should have it.
    (let ((func (if full-nov
		    'nnml-add-nov
		  'nnml-add-incremental-nov)))
      (if nnmail-group-names-not-encoded-p
	  (dolist (ga group-art)
	    (funcall func (pop dec) (cdr ga) headers))
	(dolist (ga group-art)
	  (funcall func (car ga) (cdr ga) headers)))))
  group-art)

(defun nnml-active-number (group &optional server)
  "Compute the next article number in GROUP on SERVER."
  (let* ((encoded (if nnmail-group-names-not-encoded-p
		      (nnml-encoded-group-name group server)))
	 (active (cadr (assoc (or encoded group) nnml-group-alist))))
    ;; The group wasn't known to nnml, so we just create an active
    ;; entry for it.
    (unless active
      ;; Perhaps the active file was corrupt?  See whether
      ;; there are any articles in this group.
      (nnml-possibly-create-directory group server)
      (nnml-possibly-change-directory group server)
      (unless nnml-article-file-alist
	(setq nnml-article-file-alist
	      (sort
	       (nnml-current-group-article-to-file-alist)
	       'car-less-than-car)))
      (setq active
	    (if nnml-article-file-alist
		(cons (caar nnml-article-file-alist)
		      (caar (last nnml-article-file-alist)))
	      (cons 1 0)))
      (push (list (or encoded group) active) nnml-group-alist))
    (setcdr active (1+ (cdr active)))
    (while (file-exists-p
	    (nnml-group-pathname group (int-to-string (cdr active)) server))
      (setcdr active (1+ (cdr active))))
    (cdr active)))

(defvar nnml-incremental-nov-buffer-alist nil)

(defun nnml-save-incremental-nov ()
  (save-excursion
    (while nnml-incremental-nov-buffer-alist
      (when (buffer-name (cdar nnml-incremental-nov-buffer-alist))
	(set-buffer (cdar nnml-incremental-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region (point-min) (point-max)
			       nnml-nov-buffer-file-name t 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnml-incremental-nov-buffer-alist
	    (cdr nnml-incremental-nov-buffer-alist)))))

(defun nnml-open-incremental-nov (group)
  (or (cdr (assoc group nnml-incremental-nov-buffer-alist))
      (let ((buffer (nnml-get-nov-buffer group t)))
	(push (cons group buffer) nnml-incremental-nov-buffer-alist)
	buffer)))

(defun nnml-add-incremental-nov (group article headers)
  "Add a nov line for the GROUP nov headers, incrementally."
  (with-current-buffer (nnml-open-incremental-nov group)
    (goto-char (point-max))
    (mail-header-set-number headers article)
    (nnheader-insert-nov headers)))

(defun nnml-add-nov (group article headers)
  "Add a nov line for the GROUP base."
  (with-current-buffer (nnml-open-nov group)
    (goto-char (point-max))
    (mail-header-set-number headers article)
    (nnheader-insert-nov headers)))

(defsubst nnml-header-value ()
  (buffer-substring (match-end 0) (point-at-eol)))

(defun nnml-parse-head (chars &optional number)
  "Parse the head of the current buffer."
  (save-excursion
    (save-restriction
      (unless (zerop (buffer-size))
	(narrow-to-region
	 (goto-char (point-min))
	 (if (re-search-forward "\n\r?\n" nil t)
	     (1- (point))
	   (point-max))))
      (let ((headers (nnheader-parse-naked-head)))
	(mail-header-set-chars headers chars)
	(mail-header-set-number headers number)
	headers))))

(defun nnml-get-nov-buffer (group &optional incrementalp)
  (let* ((decoded (nnml-decoded-group-name group))
	 (buffer (get-buffer-create (format " *nnml %soverview %s*"
					    (if incrementalp
						"incremental "
					      "")
					    decoded)))
	 (file-name-coding-system nnmail-pathname-coding-system))
    (with-current-buffer buffer
      (set (make-local-variable 'nnml-nov-buffer-file-name)
	   (nnmail-group-pathname decoded nnml-directory nnml-nov-file-name))
      (erase-buffer)
      (when (and (not incrementalp)
		 (file-exists-p nnml-nov-buffer-file-name))
	(nnheader-insert-file-contents nnml-nov-buffer-file-name)))
    buffer))

(defun nnml-open-nov (group)
  (or (let ((buffer (cdr (assoc group nnml-nov-buffer-alist))))
	(and (buffer-name buffer)
	     buffer))
      (let ((buffer (nnml-get-nov-buffer group)))
	(push (cons group buffer) nnml-nov-buffer-alist)
	buffer)))

(defun nnml-save-nov ()
  (save-excursion
    (while nnml-nov-buffer-alist
      (when (buffer-name (cdar nnml-nov-buffer-alist))
	(set-buffer (cdar nnml-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region (point-min) (point-max)
			       nnml-nov-buffer-file-name nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnml-nov-buffer-alist (cdr nnml-nov-buffer-alist)))))

;;;###autoload
(defun nnml-generate-nov-databases (&optional server)
  "Generate NOV databases in all nnml directories."
  (interactive (list (or (nnoo-current-server 'nnml) "")))
  ;; Read the active file to make sure we don't re-use articles
  ;; numbers in empty groups.
  (nnmail-activate 'nnml)
  (unless (nnml-server-opened server)
    (nnml-open-server server))
  (setq nnml-directory (expand-file-name nnml-directory))
  ;; Recurse down the directories.
  (nnml-generate-nov-databases-directory nnml-directory nil t)
  ;; Save the active file.
  (nnmail-save-active nnml-group-alist nnml-active-file))

(defvar nnml-files)
(defun nnml-generate-nov-databases-directory (dir &optional seen no-active)
  "Regenerate the NOV database in DIR.

Unless no-active is non-nil, update the active file too."
  (interactive (list (let ((file-name-coding-system
			    nnmail-pathname-coding-system))
		       (read-directory-name "Regenerate NOV in: "
					    nnml-directory nil t))))
  (setq dir (file-name-as-directory dir))
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    ;; Only scan this sub-tree if we haven't been here yet.
    (unless (member (file-truename dir) seen)
      (push (file-truename dir) seen)
      ;; We descend recursively
      (dolist (dir (directory-files dir t nil t))
	(when (and (not (string-match "^\\." (file-name-nondirectory dir)))
		   (file-directory-p dir))
	  (nnml-generate-nov-databases-directory dir seen)))
      ;; Do this directory.
      (let ((nnml-files (sort (nnheader-article-to-file-alist dir)
			 'car-less-than-car)))
	(if (not nnml-files)
	    (let* ((group (nnheader-file-to-group
			   (directory-file-name dir) nnml-directory))
		   (info (cadr (assoc group nnml-group-alist))))
	      (when info
		(setcar info (1+ (cdr info)))))
	  (funcall nnml-generate-active-function dir)
	  ;; Generate the nov file.
	  (nnml-generate-nov-file dir nnml-files)
	  (unless no-active
	    (nnmail-save-active nnml-group-alist nnml-active-file)))))))

(defun nnml-generate-active-info (dir)
  ;; Update the active info for this group.
  (let ((group (directory-file-name dir))
	entry last)
    (setq group (nnheader-file-to-group (nnml-encoded-group-name group)
					nnml-directory)
	  entry (assoc group nnml-group-alist)
	  last (or (caadr entry) 0)
	  nnml-group-alist (delq entry nnml-group-alist))
    (push (list group
		(cons (or (caar nnml-files) (1+ last))
		      (max last
			   (or (caar (last nnml-files))
			       0))))
	  nnml-group-alist)))

(defun nnml-generate-nov-file (dir files)
  (let* ((dir (file-name-as-directory dir))
	 (nov (concat dir nnml-nov-file-name))
	 (nov-buffer (get-buffer-create " *nov*"))
	 chars file headers)
    (with-current-buffer nov-buffer
      ;; Init the nov buffer.
      (buffer-disable-undo)
      (erase-buffer)
      (set-buffer nntp-server-buffer)
      ;; Delete the old NOV file.
      (when (file-exists-p nov)
	(funcall nnmail-delete-file-function nov))
      (dolist (file files)
	(let ((path (concat dir (cdr file))))
	  (unless (file-directory-p path)
	    (erase-buffer)
	    (nnheader-insert-file-contents path)
	    (narrow-to-region
	     (goto-char (point-min))
	     (progn
	       (re-search-forward "\n\r?\n" nil t)
	       (setq chars (- (point-max) (point)))
	       (max (point-min) (1- (point)))))
	    (unless (zerop (buffer-size))
	      (goto-char (point-min))
	      (setq headers (nnml-parse-head chars (car file)))
	      (with-current-buffer nov-buffer
		(goto-char (point-max))
		(nnheader-insert-nov headers)))
	    (widen))))
      (with-current-buffer nov-buffer
	(nnmail-write-region (point-min) (point-max) nov nil 'nomesg)
	(kill-buffer (current-buffer))))))

(defun nnml-nov-delete-article (group article)
  (with-current-buffer (nnml-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (when (bobp)
	(let ((active (cadr (assoc group nnml-group-alist)))
	      num)
	  (when active
	    (if (eobp)
		(setf (car active) (1+ (cdr active)))
	      (when (and (setq num (ignore-errors (read (current-buffer))))
			 (numberp num))
		(setf (car active) num)))))))
    t))

(defun nnml-update-file-alist (&optional force)
  (when nnml-use-compressed-files
    (when (or (not nnml-article-file-alist)
	      force)
      (setq nnml-article-file-alist
	    (nnml-current-group-article-to-file-alist)))))

(defun nnml-directory-articles (dir)
  "Return a list of all article files in a directory.
Use the nov database for that directory if available."
  (if (or gnus-nov-is-evil nnml-nov-is-evil
	  (not (file-exists-p
		(expand-file-name nnml-nov-file-name dir))))
      (nnheader-directory-articles dir)
    ;; build list from .overview if available
    ;; We would use nnml-open-nov, except that nnml-nov-buffer-alist is
    ;; defvoo'd, and we might get called when it hasn't been swapped in.
    (with-current-buffer (nnml-get-nov-buffer nnml-current-group)
      (let ((list nil)
	    art)
	(goto-char (point-min))
	(while (not (eobp))
	  (setq art (read (current-buffer)))
	  (push art list)
	  (forward-line 1))
	list))))

(defun nnml-current-group-article-to-file-alist ()
  "Return an alist of article/file pairs in the current group.
Use the nov database for the current group if available."
  (if (or nnml-use-compressed-files
	  gnus-nov-is-evil
	  nnml-nov-is-evil
	  (not (file-exists-p
		(expand-file-name nnml-nov-file-name
				  nnml-current-directory))))
      (nnheader-article-to-file-alist nnml-current-directory)
    ;; build list from .overview if available
    (with-current-buffer (nnml-get-nov-buffer nnml-current-group)
      (let ((alist nil)
	    art)
	(goto-char (point-min))
	(while (not (eobp))
	  (setq art (read (current-buffer)))
	  ;; assume file name is unadorned (ie. not compressed etc)
	  (push (cons art (int-to-string art)) alist)
	  (forward-line 1))
	alist))))

(deffoo nnml-request-set-mark (group actions &optional server)
  (nnml-possibly-change-directory group server)
  (unless nnml-marks-is-evil
    (nnml-open-marks group server)
    (setq nnml-marks (nnheader-update-marks-actions nnml-marks actions))
    (nnml-save-marks group server))
  nil)

(deffoo nnml-request-marks (group info &optional server)
  (nnml-possibly-change-directory group server)
  (when (and (not nnml-marks-is-evil) (nnml-marks-changed-p group server))
    (nnheader-message 8 "Updating marks for %s..." group)
    (nnml-open-marks group server)
    ;; Update info using `nnml-marks'.
    (mapc (lambda (pred)
	    (unless (memq (cdr pred) gnus-article-unpropagated-mark-lists)
	      (gnus-info-set-marks
	       info
	       (gnus-update-alist-soft
		(cdr pred)
		(cdr (assq (cdr pred) nnml-marks))
		(gnus-info-marks info))
	       t)))
	  gnus-article-mark-lists)
    (let ((seen (cdr (assq 'read nnml-marks))))
      (gnus-info-set-read info
			  (if (and (integerp (car seen))
				   (null (cdr seen)))
			      (list (cons (car seen) (car seen)))
			    seen)))
    (nnheader-message 8 "Updating marks for %s...done" group))
  info)

(defun nnml-marks-changed-p (group server)
  (let ((file (nnml-group-pathname group nnml-marks-file-name server)))
    (if (null (gnus-gethash file nnml-marks-modtime))
	t ;; never looked at marks file, assume it has changed
      (not (equal (gnus-gethash file nnml-marks-modtime)
		  (nth 5 (file-attributes file)))))))

(defun nnml-save-marks (group server)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(file (nnml-group-pathname group nnml-marks-file-name server)))
    (condition-case err
	(progn
	  (nnml-possibly-create-directory group server)
	  (with-temp-file file
	    (erase-buffer)
	    (gnus-prin1 nnml-marks)
	    (insert "\n"))
	  (gnus-sethash file
			(nth 5 (file-attributes file))
			nnml-marks-modtime))
      (error (or (gnus-yes-or-no-p
		  (format "Could not write to %s (%s).  Continue? " file err))
		 (error "Cannot write to %s (%s)" file err))))))

(defun nnml-open-marks (group server)
  (let* ((decoded (nnml-decoded-group-name group server))
	 (file (nnmail-group-pathname decoded nnml-directory
				      nnml-marks-file-name))
	 (file-name-coding-system nnmail-pathname-coding-system))
    (if (file-exists-p file)
	(condition-case err
	    (with-temp-buffer
	      (gnus-sethash file (nth 5 (file-attributes file))
			    nnml-marks-modtime)
	      (nnheader-insert-file-contents file)
	      (setq nnml-marks (read (current-buffer)))
	      (dolist (el gnus-article-unpropagated-mark-lists)
		(setq nnml-marks (gnus-remassoc el nnml-marks))))
	  (error (or (gnus-yes-or-no-p
		      (format "Error reading nnml marks file %s (%s).  Continuing will use marks from .newsrc.eld.  Continue? " file err))
		     (error "Cannot read nnml marks file %s (%s)" file err))))
      ;; User didn't have a .marks file.  Probably first time
      ;; user of the .marks stuff.  Bootstrap it from .newsrc.eld.
      (let ((info (gnus-get-info
		   (gnus-group-prefixed-name
		    group
		    (gnus-server-to-method
		     (format "nnml:%s" (or server "")))))))
	(setq decoded (if (member server '(nil ""))
			  (concat "nnml:" decoded)
			(format "nnml+%s:%s" server decoded)))
	(nnheader-message 7 "Bootstrapping marks for %s..." decoded)
	(setq nnml-marks (gnus-info-marks info))
	(push (cons 'read (gnus-info-read info)) nnml-marks)
	(dolist (el gnus-article-unpropagated-mark-lists)
	  (setq nnml-marks (gnus-remassoc el nnml-marks)))
	(nnml-save-marks group server)
	(nnheader-message 7 "Bootstrapping marks for %s...done" decoded)))))


;;;
;;; Group and server compaction. -- dvl
;;;

;; #### FIXME: this function handles self Xref: entry correctly, but I don't
;; #### know how to handle external cross-references. I actually don't know if
;; #### this is handled correctly elsewhere. For instance, what happens if you
;; #### move all articles to a new group (that's what people do for manual
;; #### compaction) ?

;; #### NOTE: the function below handles the article backlog. This is
;; #### conceptually the wrong place to do it because the backend is at a
;; #### lower level. However, this is the only place where we have the needed
;; #### information to do the job. Ideally, this function should not handle
;; #### the backlog by itself, but return a list of moved groups / articles to
;; #### the caller. This will become important to avoid code duplication when
;; #### other backends get a compaction feature. Also, note that invalidating
;; #### the "original article buffer" is already done at an upper level.

;; Shouldn't `nnml-request-compact-group' be interactive? --rsteib

(defun nnml-request-compact-group (group &optional server save)
  (nnml-possibly-change-directory group server)
  (unless nnml-article-file-alist
    (setq nnml-article-file-alist
	  (sort (nnml-current-group-article-to-file-alist)
		'car-less-than-car)))
  (if (not nnml-article-file-alist)
      ;; The group is empty: do nothing but return t
      t
    ;; The group is not empty:
    (let* ((group-full-name
	    (gnus-group-prefixed-name
	     group
	     (gnus-server-to-method (format "nnml:%s" server))))
	   (info (gnus-get-info group-full-name))
	   (new-number 1)
	   compacted)
      (let ((articles nnml-article-file-alist)
	    article)
	(while (setq article (pop articles))
	  (let ((old-number (car article)))
	    (when (> old-number new-number)
	      ;; There is a gap here:
	      (let ((old-number-string (int-to-string old-number))
		    (new-number-string (int-to-string new-number)))
		(setq compacted t)
		;; #### NOTE: `nnml-article-to-file' calls
		;; #### `nnml-update-file-alist'  (which in turn calls
		;; #### `nnml-current-group-article-to-file-alist', which
		;; #### might use the NOV database). This might turn out to be
		;; #### inefficient. In that case, we will do the work
		;; #### manually.
		;; 1/ Move the article to a new file:
		(let* ((oldfile (nnml-article-to-file old-number))
		       (newfile
			(gnus-replace-in-string
			 oldfile
			 ;; nnml-use-compressed-files might be any string, but
			 ;; probably it's sufficient to take into account only
			 ;; "\\.[a-z0-9]+".  Note that we can't only use the
			 ;; value of nnml-use-compressed-files because old
			 ;; articles might have been saved with a different
			 ;; value.
			 (concat
			  "\\(" old-number-string "\\)\\(\\(\\.[a-z0-9]+\\)?\\)$")
			 (concat new-number-string "\\2"))))
		  (with-current-buffer nntp-server-buffer
		    (nnmail-find-file oldfile)
		    ;; Update the Xref header in the article itself:
		    (when (and (re-search-forward "^Xref: [^ ]+ " nil t)
			       (re-search-forward
				(concat "\\<"
					(regexp-quote
					 (concat group ":" old-number-string))
					"\\>")
				(point-at-eol) t))
		      (replace-match
		       (concat group ":" new-number-string)))
		    ;; Save to the new file:
		    (nnmail-write-region (point-min) (point-max) newfile))
		  (funcall nnmail-delete-file-function oldfile))
		;; 2/ Update all marks for this article:
		;; #### NOTE: it is possible that the new article number
		;; #### already belongs to a range, whereas the corresponding
		;; #### article doesn't exist (for example, if you delete an
		;; #### article). For that reason, it is important to update
		;; #### the ranges (meaning remove nonexistent articles) before
		;; #### doing anything on them.
		;; 2 a/ read articles:
		(let ((read (gnus-info-read info)))
		  (setq read (gnus-remove-from-range read (list new-number)))
		  (when (gnus-member-of-range old-number read)
		    (setq read (gnus-remove-from-range read (list old-number)))
		    (setq read (gnus-add-to-range read (list new-number))))
		  (gnus-info-set-read info read))
		;; 2 b/ marked articles:
		(let ((oldmarks (gnus-info-marks info))
		      mark newmarks)
		  (while (setq mark (pop oldmarks))
		    (setcdr mark (gnus-remove-from-range (cdr mark)
							 (list new-number)))
		    (when (gnus-member-of-range old-number (cdr mark))
		      (setcdr mark (gnus-remove-from-range (cdr mark)
							   (list old-number)))
		      (setcdr mark (gnus-add-to-range (cdr mark)
						      (list new-number))))
		    (push mark newmarks))
		  (gnus-info-set-marks info newmarks))
		;; 3/ Update the NOV entry for this article:
		(unless nnml-nov-is-evil
		  (with-current-buffer (nnml-open-nov group)
		    (when (nnheader-find-nov-line old-number)
		      ;; Replace the article number:
		      (looking-at old-number-string)
		      (replace-match new-number-string nil t)
		      ;; Update the Xref header:
		      (when (re-search-forward
			     (concat "\\(Xref:[^\t\n]* \\)\\<"
				     (regexp-quote
				      (concat group ":" old-number-string))
				     "\\>")
			     (point-at-eol) t)
			(replace-match
			 (concat "\\1" group ":" new-number-string))))))
		;; 4/ Possibly remove the article from the backlog:
		(when gnus-keep-backlog
		  ;; #### NOTE: instead of removing the article, we could
		  ;; #### modify the backlog to reflect the numbering change,
		  ;; #### but I don't think it's worth it.
		  (gnus-backlog-remove-article group-full-name old-number)
		  (gnus-backlog-remove-article group-full-name new-number))))
	    (setq new-number (1+ new-number)))))
      (if (not compacted)
	  ;; No compaction had to be done:
	  t
	;; Some articles have actually been renamed:
	;; 1/ Rebuild active information:
	(let ((entry (assoc group nnml-group-alist))
	      (active (cons 1 (1- new-number))))
	  (setq nnml-group-alist (delq entry nnml-group-alist))
	  (push (list group active) nnml-group-alist)
	  ;; Update the active hashtable to let the *Group* buffer display
	  ;; up-to-date lines. I don't think that either gnus-newsrc-hashtb or
	  ;; gnus-newwrc-alist are out of date, since all we did is to modify
	  ;; the info of the group internally.
	  (gnus-set-active group-full-name active))
	;; 1 bis/
	;; #### NOTE: normally, we should save the overview (NOV) file
	;; #### here, just like we save the marks file. However, there is no
	;; #### such function as nnml-save-nov for a single group. Only for
	;; #### all groups. Gnus inconsistency is getting worse every day...
	;; 2/ Rebuild marks file:
	(unless nnml-marks-is-evil
	  ;; #### NOTE: this constant use of global variables everywhere is
	  ;; #### truly disgusting. Gnus really needs a *major* cleanup.
	  (setq nnml-marks (gnus-info-marks info))
	  (push (cons 'read (gnus-info-read info)) nnml-marks)
	  (dolist (el gnus-article-unpropagated-mark-lists)
	    (setq nnml-marks (gnus-remassoc el nnml-marks)))
	  (nnml-save-marks group server))
	;; 3/ Save everything if this was not part of a bigger operation:
	(if (not save)
	    ;; Nothing to save (yet):
	    t
	  ;; Something to save:
	  ;; a/ Save the NOV databases:
	  ;; #### NOTE: this should be done directory per directory in 1bis
	  ;; #### above. See comment there.
	  (nnml-save-nov)
	  ;; b/ Save the active file:
	  (nnmail-save-active nnml-group-alist nnml-active-file)
	  (let ((marks (nnml-group-pathname group nnml-marks-file-name server)))
	    (when (file-exists-p marks)
	      (delete-file marks)))
	  t)))))

(defun nnml-request-compact (&optional server)
  "Request compaction of all SERVER nnml groups."
  (interactive (list (or (nnoo-current-server 'nnml) "")))
  (nnmail-activate 'nnml)
  (unless (nnml-server-opened server)
    (nnml-open-server server))
  (setq nnml-directory (expand-file-name nnml-directory))
  (let* ((groups (gnus-groups-from-server
		  (gnus-server-to-method (format "nnml:%s" server))))
	 (first (pop groups))
	 group)
    (when first
      (while (setq group (pop groups))
	(nnml-request-compact-group (gnus-group-real-name group) server))
      (nnml-request-compact-group (gnus-group-real-name first) server t))))


(provide 'nnml)

;;; nnml.el ends here
