;;; nndiary.el --- A diary back end for Gnus

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Fri Jul 16 18:55:42 1999
;; Keywords:      calendar mail news

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

;; Contents management by FCM version 0.1.

;; Description:
;; ===========

;; nndiary is a mail back end designed to handle mails as diary event
;; reminders. It is now fully documented in the Gnus manual.


;; Bugs / Todo:
;; ===========

;; * Respooling doesn't work because contrary to the request-scan function,
;;   Gnus won't allow me to override the split methods when calling the
;;   respooling back end functions.
;; * There's a bug in the time zone mechanism with variable TZ locations.
;; * We could allow a keyword like `ask' in X-Diary-* headers, that would mean
;;   "ask for value upon reception of the message".
;; * We could add an optional header X-Diary-Reminders to specify a special
;;   reminders value for this message. Suggested by Jody Klymak.
;; * We should check messages validity in other circumstances than just
;;   moving an article from somewhere else (request-accept). For instance,
;;   when editing / saving and so on.


;; Remarks:
;; =======

;; * nnoo. NNDiary is very similar to nnml. This makes the idea of using nnoo
;;   (to derive nndiary from nnml) natural. However, my experience with nnoo
;;   is that for reasonably complex back ends like this one, nnoo is a burden
;;   rather than an help. It's tricky to use, not everything can be inherited,
;;   what can be inherited and when is not very clear, and you've got to be
;;   very careful because a little mistake can fuck up your other back ends,
;;   especially because their variables will be use instead of your real ones.
;;   Finally, I found it easier to just clone the needed parts of nnml, and
;;   tracking nnml updates is not a big deal.

;;   IMHO, nnoo is actually badly designed.  A much simpler, and yet more
;;   powerful one would be to make *real* functions and variables for a new
;;   back end based on another. Lisp is a reflexive language so that's a very
;;   easy thing to do: inspect the function's form, replace occurrences of
;;   <nnfrom> (even in strings) with <nnto>, and you're done.

;; * nndiary-get-new-mail, nndiary-mail-source and nndiary-split-methods:
;;   NNDiary has some experimental parts, in the sense Gnus normally uses only
;;   one mail back ends for mail retrieval and splitting. This back end is
;;   also an attempt to make it behave differently. For Gnus developers: as
;;   you can see if you snarf into the code, that was not a very difficult
;;   thing to do. Something should be done about the respooling breakage
;;   though.


;;; Code:

(require 'nnoo)
(require 'nnheader)
(require 'nnmail)
(eval-when-compile (require 'cl))

(require 'gnus-start)
(require 'gnus-sum)

;; Compatibility Functions  =================================================

(eval-and-compile
  (if (fboundp 'signal-error)
      (defun nndiary-error (&rest args)
	(apply #'signal-error 'nndiary args))
    (defun nndiary-error (&rest args)
      (apply #'error args))))


;; Back End behavior customization ===========================================

(defgroup nndiary nil
  "The Gnus Diary back end."
  :version "22.1"
  :group 'gnus-diary)

(defcustom nndiary-mail-sources
  `((file :path ,(expand-file-name "~/.nndiary")))
  "*NNDiary specific mail sources.
This variable is used by nndiary in place of the standard `mail-sources'
variable when `nndiary-get-new-mail' is set to non-nil.  These sources
must contain diary messages ONLY."
  :group 'nndiary
  :group 'mail-source
  :type 'sexp)

(defcustom nndiary-split-methods '(("diary" ""))
  "*NNDiary specific split methods.
This variable is used by nndiary in place of the standard
`nnmail-split-methods' variable when `nndiary-get-new-mail' is set to
non-nil."
  :group 'nndiary
  :group 'nnmail-split
  :type '(choice (repeat :tag "Alist" (group (string :tag "Name") regexp))
		 (function-item nnmail-split-fancy)
		 (function :tag "Other")))


(defcustom nndiary-reminders '((0 . day))
  "*Different times when you want to be reminded of your appointments.
Diary articles will appear again, as if they'd been just received.

Entries look like (3 . day) which means something like \"Please
Hortense, would you be so kind as to remind me of my appointments 3 days
before the date, thank you very much. Anda, hmmm... by the way, are you
doing anything special tonight ?\".

The units of measure are 'minute 'hour 'day 'week 'month and 'year (no,
not 'century, sorry).

NOTE: the units of measure actually express dates, not durations: if you
use 'week, messages will pop up on Sundays at 00:00 (or Mondays if
`nndiary-week-starts-on-monday' is non-nil) and *not* 7 days before the
appointment, if you use 'month, messages will pop up on the first day of
each months, at 00:00 and so on.

If you really want to specify a duration (like 24 hours exactly), you can
use the equivalent in minutes (the smallest unit).  A fuzz of 60 seconds
maximum in the reminder is not that painful, I think.  Although this
scheme might appear somewhat weird at a first glance, it is very powerful.
In order to make this clear, here are some examples:

- '(0 . day): this is the default value of `nndiary-reminders'.  It means
  pop up the appointments of the day each morning at 00:00.

- '(1 . day): this means pop up the appointments the day before, at 00:00.

- '(6 . hour): for an appointment at 18:30, this would pop up the
  appointment message at 12:00.

- '(360 . minute): for an appointment at 18:30 and 15 seconds, this would
  pop up the appointment message at 12:30."
  :group 'nndiary
  :type '(repeat (cons :format "%v\n"
		       (integer :format "%v")
		       (choice :format "%[%v(s)%] before...\n"
			       :value day
			       (const :format "%v" minute)
			       (const :format "%v" hour)
			       (const :format "%v" day)
			       (const :format "%v" week)
			       (const :format "%v" month)
			       (const :format "%v" year)))))

(defcustom nndiary-week-starts-on-monday nil
  "*Whether a week starts on monday (otherwise, sunday)."
  :type 'boolean
  :group 'nndiary)


(defcustom nndiary-request-create-group-hooks nil
  "*Hooks to run after `nndiary-request-create-group' is executed.
The hooks will be called with the full group name as argument."
  :group 'nndiary
  :type 'hook)

(defcustom nndiary-request-update-info-hooks nil
  "*Hooks to run after `nndiary-request-update-info-group' is executed.
The hooks will be called with the full group name as argument."
  :group 'nndiary
  :type 'hook)

(defcustom nndiary-request-accept-article-hooks nil
  "*Hooks to run before accepting an article.
Executed near the beginning of `nndiary-request-accept-article'.
The hooks will be called with the article in the current buffer."
  :group 'nndiary
  :type 'hook)

(defcustom nndiary-check-directory-twice t
  "*If t, check directories twice to avoid NFS failures."
  :group 'nndiary
  :type 'boolean)


;; Back End declaration ======================================================

;; Well, most of this is nnml clonage.

(nnoo-declare nndiary)

(defvoo nndiary-directory (nnheader-concat gnus-directory "diary/")
  "Spool directory for the nndiary back end.")

(defvoo nndiary-active-file
    (expand-file-name "active" nndiary-directory)
  "Active file for the nndiary back end.")

(defvoo nndiary-newsgroups-file
    (expand-file-name "newsgroups" nndiary-directory)
  "Newsgroups description file for the nndiary back end.")

(defvoo nndiary-get-new-mail nil
  "Whether nndiary gets new mail and split it.
Contrary to traditional mail back ends, this variable can be set to t
even if your primary mail back end also retrieves mail. In such a case,
NDiary uses its own mail-sources and split-methods.")

(defvoo nndiary-nov-is-evil nil
  "If non-nil, Gnus will never use nov databases for nndiary groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much.  If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nndiary-generate-nov-databases' command.  The function will go
through all nnml directories and generate nov databases for them
all.  This may very well take some time.")

(defvoo nndiary-prepare-save-mail-hook nil
  "*Hook run narrowed to an article before saving.")

(defvoo nndiary-inhibit-expiry nil
  "If non-nil, inhibit expiry.")



(defconst nndiary-version "0.2-b14"
  "Current Diary back end version.")

(defun nndiary-version ()
  "Current Diary back end version."
  (interactive)
  (message "NNDiary version %s" nndiary-version))

(defvoo nndiary-nov-file-name ".overview")

(defvoo nndiary-current-directory nil)
(defvoo nndiary-current-group nil)
(defvoo nndiary-status-string "" )
(defvoo nndiary-nov-buffer-alist nil)
(defvoo nndiary-group-alist nil)
(defvoo nndiary-active-timestamp nil)
(defvoo nndiary-article-file-alist nil)

(defvoo nndiary-generate-active-function 'nndiary-generate-active-info)
(defvoo nndiary-nov-buffer-file-name nil)
(defvoo nndiary-file-coding-system nnmail-file-coding-system)

(defconst nndiary-headers
  '(("Minute" 0 59)
    ("Hour" 0 23)
    ("Dom" 1 31)
    ("Month" 1 12)
    ("Year" 1971)
    ("Dow" 0 6)
    ("Time-Zone" (("Y" -43200)

		  ("X" -39600)

		  ("W" -36000)

		  ("V" -32400)

		  ("U" -28800)
		  ("PST" -28800)

		  ("T"   -25200)
		  ("MST" -25200)
		  ("PDT" -25200)

		  ("S"   -21600)
		  ("CST" -21600)
		  ("MDT" -21600)

		  ("R"   -18000)
		  ("EST" -18000)
		  ("CDT" -18000)

		  ("Q"   -14400)
		  ("AST" -14400)
		  ("EDT" -14400)

		  ("P"   -10800)
		  ("ADT" -10800)

		  ("O" -7200)

		  ("N" -3600)

		  ("Z"   0)
		  ("GMT" 0)
		  ("UT"  0)
		  ("UTC" 0)
		  ("WET" 0)

		  ("A"    3600)
		  ("CET"  3600)
		  ("MET"  3600)
		  ("MEZ"  3600)
		  ("BST"  3600)
		  ("WEST" 3600)

		  ("B"    7200)
		  ("EET"  7200)
		  ("CEST" 7200)
		  ("MEST" 7200)
		  ("MESZ" 7200)

		  ("C" 10800)

		  ("D" 14400)

		  ("E" 18000)

		  ("F" 21600)

		  ("G" 25200)

		  ("H" 28800)

		  ("I"   32400)
		  ("JST" 32400)

		  ("K"   36000)
		  ("GST" 36000)

		  ("L" 39600)

		  ("M"    43200)
		  ("NZST" 43200)

		  ("NZDT" 46800))))
  ;; List of NNDiary headers that specify the time spec. Each header name is
  ;; followed by either two integers (specifying a range of possible values
  ;; for this header) or one list (specifying all the possible values for this
  ;; header). In the latter case, the list does NOT include the unspecified
  ;; spec (*).
  ;; For time zone values, we have symbolic time zone names associated with
  ;; the (relative) number of seconds ahead GMT.
  )

(defsubst nndiary-schedule ()
  (let (head)
    (condition-case arg
	(mapcar
	 (lambda (elt)
	   (setq head (nth 0 elt))
	   (nndiary-parse-schedule (nth 0 elt) (nth 1 elt) (nth 2 elt)))
	 nndiary-headers)
      (error
       (nnheader-report 'nndiary "X-Diary-%s header parse error: %s."
			head (cdr arg))
       nil))
    ))

;;; Interface functions =====================================================

(nnoo-define-basics nndiary)

(deffoo nndiary-retrieve-headers (sequence &optional group server fetch-old)
  (when (nndiary-possibly-change-directory group server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (let* ((file nil)
	     (number (length sequence))
	     (count 0)
	     (file-name-coding-system nnmail-pathname-coding-system)
	     beg article
	     (nndiary-check-directory-twice
	      (and nndiary-check-directory-twice
		   ;; To speed up, disable it in some case.
		   (or (not (numberp nnmail-large-newsgroup))
		       (<= number nnmail-large-newsgroup)))))
	(if (stringp (car sequence))
	    'headers
	  (if (nndiary-retrieve-headers-with-nov sequence fetch-old)
	      'nov
	    (while sequence
	      (setq article (car sequence))
	      (setq file (nndiary-article-to-file article))
	      (when (and file
			 (file-exists-p file)
			 (not (file-directory-p file)))
		(insert (format "221 %d Article retrieved.\n" article))
		(setq beg (point))
		(nnheader-insert-head file)
		(goto-char beg)
		(if (search-forward "\n\n" nil t)
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
		   (nnheader-message 6 "nndiary: Receiving headers... %d%%"
				     (/ (* count 100) number))))

	    (and (numberp nnmail-large-newsgroup)
		 (> number nnmail-large-newsgroup)
		 (nnheader-message 6 "nndiary: Receiving headers...done"))

	    (nnheader-fold-continuation-lines)
	    'headers))))))

(deffoo nndiary-open-server (server &optional defs)
  (nnoo-change-server 'nndiary server defs)
  (when (not (file-exists-p nndiary-directory))
    (ignore-errors (make-directory nndiary-directory t)))
  (cond
   ((not (file-exists-p nndiary-directory))
    (nndiary-close-server)
    (nnheader-report 'nndiary "Couldn't create directory: %s"
		     nndiary-directory))
   ((not (file-directory-p (file-truename nndiary-directory)))
    (nndiary-close-server)
    (nnheader-report 'nndiary "Not a directory: %s" nndiary-directory))
   (t
    (nnheader-report 'nndiary "Opened server %s using directory %s"
		     server nndiary-directory)
    t)))

(deffoo nndiary-request-regenerate (server)
  (nndiary-possibly-change-directory nil server)
  (nndiary-generate-nov-databases server)
  t)

(deffoo nndiary-request-article (id &optional group server buffer)
  (nndiary-possibly-change-directory group server)
  (let* ((nntp-server-buffer (or buffer nntp-server-buffer))
	 (file-name-coding-system nnmail-pathname-coding-system)
	 path gpath group-num)
    (if (stringp id)
	(when (and (setq group-num (nndiary-find-group-number id))
		   (cdr
		    (assq (cdr group-num)
			  (nnheader-article-to-file-alist
			   (setq gpath
				 (nnmail-group-pathname
				  (car group-num)
				  nndiary-directory))))))
	  (setq path (concat gpath (int-to-string (cdr group-num)))))
      (setq path (nndiary-article-to-file id)))
    (cond
     ((not path)
      (nnheader-report 'nndiary "No such article: %s" id))
     ((not (file-exists-p path))
      (nnheader-report 'nndiary "No such file: %s" path))
     ((file-directory-p path)
      (nnheader-report 'nndiary "File is a directory: %s" path))
     ((not (save-excursion (let ((nnmail-file-coding-system
				  nndiary-file-coding-system))
			     (nnmail-find-file path))))
      (nnheader-report 'nndiary "Couldn't read file: %s" path))
     (t
      (nnheader-report 'nndiary "Article %s retrieved" id)
      ;; We return the article number.
      (cons (if group-num (car group-num) group)
	    (string-to-number (file-name-nondirectory path)))))))

(deffoo nndiary-request-group (group &optional server dont-check info)
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (cond
     ((not (nndiary-possibly-change-directory group server))
      (nnheader-report 'nndiary "Invalid group (no such directory)"))
     ((not (file-exists-p nndiary-current-directory))
      (nnheader-report 'nndiary "Directory %s does not exist"
		       nndiary-current-directory))
     ((not (file-directory-p nndiary-current-directory))
      (nnheader-report 'nndiary "%s is not a directory"
		       nndiary-current-directory))
     (dont-check
      (nnheader-report 'nndiary "Group %s selected" group)
      t)
     (t
      (nnheader-re-read-dir nndiary-current-directory)
      (nnmail-activate 'nndiary)
      (let ((active (nth 1 (assoc group nndiary-group-alist))))
	(if (not active)
	    (nnheader-report 'nndiary "No such group: %s" group)
	  (nnheader-report 'nndiary "Selected group %s" group)
	  (nnheader-insert "211 %d %d %d %s\n"
			   (max (1+ (- (cdr active) (car active))) 0)
			   (car active) (cdr active) group)))))))

(deffoo nndiary-request-scan (&optional group server)
  ;; Use our own mail sources and split methods while Gnus doesn't let us have
  ;; multiple back ends for retrieving mail.
  (let ((mail-sources nndiary-mail-sources)
	(nnmail-split-methods nndiary-split-methods))
    (setq nndiary-article-file-alist nil)
    (nndiary-possibly-change-directory group server)
    (nnmail-get-new-mail 'nndiary 'nndiary-save-nov nndiary-directory group)))

(deffoo nndiary-close-group (group &optional server)
  (setq nndiary-article-file-alist nil)
  t)

(deffoo nndiary-request-create-group (group &optional server args)
  (nndiary-possibly-change-directory nil server)
  (nnmail-activate 'nndiary)
  (cond
   ((assoc group nndiary-group-alist)
    t)
   ((and (file-exists-p (nnmail-group-pathname group nndiary-directory))
	 (not (file-directory-p (nnmail-group-pathname
				 group nndiary-directory))))
    (nnheader-report 'nndiary "%s is a file"
		     (nnmail-group-pathname group nndiary-directory)))
   (t
    (let (active)
      (push (list group (setq active (cons 1 0)))
	    nndiary-group-alist)
      (nndiary-possibly-create-directory group)
      (nndiary-possibly-change-directory group server)
      (let ((articles (nnheader-directory-articles nndiary-current-directory)))
	(when articles
	  (setcar active (apply 'min articles))
	  (setcdr active (apply 'max articles))))
      (nnmail-save-active nndiary-group-alist nndiary-active-file)
      (run-hook-with-args 'nndiary-request-create-group-hooks
			  (gnus-group-prefixed-name group
						    (list "nndiary" server)))
      t))
   ))

(deffoo nndiary-request-list (&optional server)
  (save-excursion
    (let ((nnmail-file-coding-system nnmail-active-file-coding-system)
	  (file-name-coding-system nnmail-pathname-coding-system))
      (nnmail-find-file nndiary-active-file))
    (setq nndiary-group-alist (nnmail-get-active))
    t))

(deffoo nndiary-request-newgroups (date &optional server)
  (nndiary-request-list server))

(deffoo nndiary-request-list-newsgroups (&optional server)
  (save-excursion
    (nnmail-find-file nndiary-newsgroups-file)))

(deffoo nndiary-request-expire-articles (articles group &optional server force)
  (nndiary-possibly-change-directory group server)
  (let ((active-articles
	 (nnheader-directory-articles nndiary-current-directory))
	article rest number)
    (nnmail-activate 'nndiary)
    ;; Articles not listed in active-articles are already gone,
    ;; so don't try to expire them.
    (setq articles (gnus-intersection articles active-articles))
    (while articles
      (setq article (nndiary-article-to-file (setq number (pop articles))))
      (if (and (nndiary-deletable-article-p group number)
	       ;; Don't use nnmail-expired-article-p. Our notion of expiration
	       ;; is a bit peculiar ...
	       (or force (nndiary-expired-article-p article)))
	  (progn
	    ;; Allow a special target group.
	    (unless (eq nnmail-expiry-target 'delete)
	      (with-temp-buffer
		(nndiary-request-article number group server (current-buffer))
		(let ((nndiary-current-directory nil))
		  (nnmail-expiry-target-group nnmail-expiry-target group)))
	      (nndiary-possibly-change-directory group server))
	    (nnheader-message 5 "Deleting article %s in %s" number group)
	    (condition-case ()
		(funcall nnmail-delete-file-function article)
	      (file-error (push number rest)))
	    (setq active-articles (delq number active-articles))
	    (nndiary-nov-delete-article group number))
	(push number rest)))
    (let ((active (nth 1 (assoc group nndiary-group-alist))))
      (when active
	(setcar active (or (and active-articles
				(apply 'min active-articles))
			   (1+ (cdr active)))))
      (nnmail-save-active nndiary-group-alist nndiary-active-file))
    (nndiary-save-nov)
    (nconc rest articles)))

(deffoo nndiary-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (let ((buf (get-buffer-create " *nndiary move*"))
	result)
    (nndiary-possibly-change-directory group server)
    (nndiary-update-file-alist)
    (and
     (nndiary-deletable-article-p group article)
     (nndiary-request-article article group server)
     (let (nndiary-current-directory
	   nndiary-current-group
	   nndiary-article-file-alist)
       (with-current-buffer buf
	 (insert-buffer-substring nntp-server-buffer)
	 (setq result (eval accept-form))
	 (kill-buffer (current-buffer))
	 result))
     (progn
       (nndiary-possibly-change-directory group server)
       (condition-case ()
	   (funcall nnmail-delete-file-function
		    (nndiary-article-to-file  article))
	 (file-error nil))
       (nndiary-nov-delete-article group article)
       (when last
	 (nndiary-save-nov)
	 (nnmail-save-active nndiary-group-alist nndiary-active-file))))
    result))

(deffoo nndiary-request-accept-article (group &optional server last)
  (nndiary-possibly-change-directory group server)
  (nnmail-check-syntax)
  (run-hooks 'nndiary-request-accept-article-hooks)
  (when (nndiary-schedule)
    (let (result)
      (when nnmail-cache-accepted-message-ids
	(nnmail-cache-insert (nnmail-fetch-field "message-id")
			     group
			     (nnmail-fetch-field "subject")))
      (if (stringp group)
	  (and
	   (nnmail-activate 'nndiary)
	   (setq result
		 (car (nndiary-save-mail
		       (list (cons group (nndiary-active-number group))))))
	   (progn
	     (nnmail-save-active nndiary-group-alist nndiary-active-file)
	     (and last (nndiary-save-nov))))
	(and
	 (nnmail-activate 'nndiary)
	 (if (and (not (setq result
			     (nnmail-article-group 'nndiary-active-number)))
		  (yes-or-no-p "Moved to `junk' group; delete article? "))
	     (setq result 'junk)
	   (setq result (car (nndiary-save-mail result))))
	 (when last
	   (nnmail-save-active nndiary-group-alist nndiary-active-file)
	   (when nnmail-cache-accepted-message-ids
	     (nnmail-cache-close))
	   (nndiary-save-nov))))
      result))
  )

(deffoo nndiary-request-post (&optional server)
  (nnmail-do-request-post 'nndiary-request-accept-article server))

(deffoo nndiary-request-replace-article (article group buffer)
  (nndiary-possibly-change-directory group)
  (with-current-buffer buffer
    (nndiary-possibly-create-directory group)
    (let ((chars (nnmail-insert-lines))
	  (art (concat (int-to-string article) "\t"))
	  headers)
      (when (ignore-errors
	      (nnmail-write-region
	       (point-min) (point-max)
	       (or (nndiary-article-to-file article)
		   (expand-file-name (int-to-string article)
				     nndiary-current-directory))
	       nil (if (nnheader-be-verbose 5) nil 'nomesg))
	      t)
	(setq headers (nndiary-parse-head chars article))
	;; Replace the NOV line in the NOV file.
	(with-current-buffer (nndiary-open-nov group)
	  (goto-char (point-min))
	  (if (or (looking-at art)
		  (search-forward (concat "\n" art) nil t))
	      ;; Delete the old NOV line.
	      (delete-region (progn (beginning-of-line) (point))
			     (progn (forward-line 1) (point)))
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
	  (nndiary-save-nov)
	  t)))))

(deffoo nndiary-request-delete-group (group &optional force server)
  (nndiary-possibly-change-directory group server)
  (when force
    ;; Delete all articles in GROUP.
    (let ((articles
	   (directory-files
	    nndiary-current-directory t
	    (concat nnheader-numerical-short-files
		    "\\|" (regexp-quote nndiary-nov-file-name) "$")))
	  article)
      (while articles
	(setq article (pop articles))
	(when (file-writable-p article)
	  (nnheader-message 5 "Deleting article %s in %s..." article group)
	  (funcall nnmail-delete-file-function article))))
    ;; Try to delete the directory itself.
    (ignore-errors (delete-directory nndiary-current-directory)))
  ;; Remove the group from all structures.
  (setq nndiary-group-alist
	(delq (assoc group nndiary-group-alist) nndiary-group-alist)
	nndiary-current-group nil
	nndiary-current-directory nil)
  ;; Save the active file.
  (nnmail-save-active nndiary-group-alist nndiary-active-file)
  t)

(deffoo nndiary-request-rename-group (group new-name &optional server)
  (nndiary-possibly-change-directory group server)
  (let ((new-dir (nnmail-group-pathname new-name nndiary-directory))
	(old-dir (nnmail-group-pathname group nndiary-directory)))
    (when (ignore-errors
	    (make-directory new-dir t)
	    t)
      ;; We move the articles file by file instead of renaming
      ;; the directory -- there may be subgroups in this group.
      ;; One might be more clever, I guess.
      (let ((files (nnheader-article-to-file-alist old-dir)))
	(while files
	  (rename-file
	   (concat old-dir (cdar files))
	   (concat new-dir (cdar files)))
	  (pop files)))
      ;; Move .overview file.
      (let ((overview (concat old-dir nndiary-nov-file-name)))
	(when (file-exists-p overview)
	  (rename-file overview (concat new-dir nndiary-nov-file-name))))
      (when (<= (length (directory-files old-dir)) 2)
	(ignore-errors (delete-directory old-dir)))
      ;; That went ok, so we change the internal structures.
      (let ((entry (assoc group nndiary-group-alist)))
	(when entry
	  (setcar entry new-name))
	(setq nndiary-current-directory nil
	      nndiary-current-group nil)
	;; Save the new group alist.
	(nnmail-save-active nndiary-group-alist nndiary-active-file)
	t))))

(deffoo nndiary-set-status (article name value &optional group server)
  (nndiary-possibly-change-directory group server)
  (let ((file (nndiary-article-to-file article)))
    (cond
     ((not (file-exists-p file))
      (nnheader-report 'nndiary "File %s does not exist" file))
     (t
      (with-temp-file file
	(nnheader-insert-file-contents file)
	(nnmail-replace-status name value))
      t))))


;;; Interface optional functions ============================================

(deffoo nndiary-request-update-info (group info &optional server)
  (nndiary-possibly-change-directory group)
  (let ((timestamp (gnus-group-parameter-value (gnus-info-params info)
					       'timestamp t)))
    (if (not timestamp)
	(nnheader-report 'nndiary "Group %s doesn't have a timestamp" group)
      ;; else
      ;; Figure out which articles should be re-new'ed
      (let ((articles (nndiary-flatten (gnus-info-read info) 0))
	    article file unread buf)
	(save-excursion
	  (setq buf (nnheader-set-temp-buffer " *nndiary update*"))
	  (while (setq article (pop articles))
	    (setq file (concat nndiary-current-directory
			       (int-to-string article)))
	    (and (file-exists-p file)
		 (nndiary-renew-article-p file timestamp)
		 (push article unread)))
	  ;;(message "unread: %s" unread)
	  (sit-for 1)
	  (kill-buffer buf))
	(setq unread (sort unread '<))
	(and unread
	     (gnus-info-set-read info (gnus-update-read-articles
				       (gnus-info-group info) unread t)))
	))
    (run-hook-with-args 'nndiary-request-update-info-hooks
			(gnus-info-group info))
    t))



;;; Internal functions ======================================================

(defun nndiary-article-to-file (article)
  (nndiary-update-file-alist)
  (let (file)
    (if (setq file (cdr (assq article nndiary-article-file-alist)))
	(expand-file-name file nndiary-current-directory)
      ;; Just to make sure nothing went wrong when reading over NFS --
      ;; check once more.
      (if nndiary-check-directory-twice
	  (when (file-exists-p
		 (setq file (expand-file-name (number-to-string article)
					      nndiary-current-directory)))
	    (nndiary-update-file-alist t)
	    file)))))

(defun nndiary-deletable-article-p (group article)
  "Say whether ARTICLE in GROUP can be deleted."
  (let (path)
    (when (setq path (nndiary-article-to-file article))
      (when (file-writable-p path)
	(or (not nnmail-keep-last-article)
	    (not (eq (cdr (nth 1 (assoc group nndiary-group-alist)))
		     article)))))))

;; Find an article number in the current group given the Message-ID.
(defun nndiary-find-group-number (id)
  (with-current-buffer (get-buffer-create " *nndiary id*")
    (let ((alist nndiary-group-alist)
	  number)
      ;; We want to look through all .overview files, but we want to
      ;; start with the one in the current directory.  It seems most
      ;; likely that the article we are looking for is in that group.
      (if (setq number (nndiary-find-id nndiary-current-group id))
	  (cons nndiary-current-group number)
	;; It wasn't there, so we look through the other groups as well.
	(while (and (not number)
		    alist)
	  (or (string= (caar alist) nndiary-current-group)
	      (setq number (nndiary-find-id (caar alist) id)))
	  (or number
	      (setq alist (cdr alist))))
	(and number
	     (cons (caar alist) number))))))

(defun nndiary-find-id (group id)
  (erase-buffer)
  (let ((nov (expand-file-name nndiary-nov-file-name
			       (nnmail-group-pathname group
						      nndiary-directory)))
	number found)
    (when (file-exists-p nov)
      (nnheader-insert-file-contents nov)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward"\t" (point-at-bol) t))))
	    (forward-line 1)
	  (beginning-of-line)
	  (setq found t)
	  ;; We return the article number.
	  (setq number
		(ignore-errors (read (current-buffer))))))
      number)))

(defun nndiary-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nndiary-nov-is-evil)
      nil
    (let ((nov (expand-file-name nndiary-nov-file-name
				 nndiary-current-directory)))
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

(defun nndiary-possibly-change-directory (group &optional server)
  (when (and server
	     (not (nndiary-server-opened server)))
    (nndiary-open-server server))
  (if (not group)
      t
    (let ((pathname (nnmail-group-pathname group nndiary-directory))
	  (file-name-coding-system nnmail-pathname-coding-system))
      (when (not (equal pathname nndiary-current-directory))
	(setq nndiary-current-directory pathname
	      nndiary-current-group group
	      nndiary-article-file-alist nil))
      (file-exists-p nndiary-current-directory))))

(defun nndiary-possibly-create-directory (group)
  (let ((dir (nnmail-group-pathname group nndiary-directory)))
    (unless (file-exists-p dir)
      (make-directory (directory-file-name dir) t)
      (nnheader-message 5 "Creating mail directory %s" dir))))

(defun nndiary-save-mail (group-art)
  "Called narrowed to an article."
  (let (chars headers)
    (setq chars (nnmail-insert-lines))
    (nnmail-insert-xref group-art)
    (run-hooks 'nnmail-prepare-save-mail-hook)
    (run-hooks 'nndiary-prepare-save-mail-hook)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the groups it belongs in.
    (let ((ga group-art)
	  first)
      (while ga
	(nndiary-possibly-create-directory (caar ga))
	(let ((file (concat (nnmail-group-pathname
			     (caar ga) nndiary-directory)
			    (int-to-string (cdar ga)))))
	  (if first
	      ;; It was already saved, so we just make a hard link.
	      (funcall nnmail-crosspost-link-function first file t)
	    ;; Save the article.
	    (nnmail-write-region (point-min) (point-max) file nil
				 (if (nnheader-be-verbose 5) nil 'nomesg))
	    (setq first file)))
	(setq ga (cdr ga))))
    ;; Generate a nov line for this article.  We generate the nov
    ;; line after saving, because nov generation destroys the
    ;; header.
    (setq headers (nndiary-parse-head chars))
    ;; Output the nov line to all nov databases that should have it.
    (let ((ga group-art))
      (while ga
	(nndiary-add-nov (caar ga) (cdar ga) headers)
	(setq ga (cdr ga))))
    group-art))

(defun nndiary-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (cadr (assoc group nndiary-group-alist))))
    ;; The group wasn't known to nndiary, so we just create an active
    ;; entry for it.
    (unless active
      ;; Perhaps the active file was corrupt?  See whether
      ;; there are any articles in this group.
      (nndiary-possibly-create-directory group)
      (nndiary-possibly-change-directory group)
      (unless nndiary-article-file-alist
	(setq nndiary-article-file-alist
	      (sort
	       (nnheader-article-to-file-alist nndiary-current-directory)
	       'car-less-than-car)))
      (setq active
	    (if nndiary-article-file-alist
		(cons (caar nndiary-article-file-alist)
		      (caar (last nndiary-article-file-alist)))
	      (cons 1 0)))
      (push (list group active) nndiary-group-alist))
    (setcdr active (1+ (cdr active)))
    (while (file-exists-p
	    (expand-file-name (int-to-string (cdr active))
			      (nnmail-group-pathname group nndiary-directory)))
      (setcdr active (1+ (cdr active))))
    (cdr active)))

(defun nndiary-add-nov (group article headers)
  "Add a nov line for the GROUP base."
  (with-current-buffer (nndiary-open-nov group)
    (goto-char (point-max))
    (mail-header-set-number headers article)
    (nnheader-insert-nov headers)))

(defsubst nndiary-header-value ()
  (buffer-substring (match-end 0) (progn (end-of-line) (point))))

(defun nndiary-parse-head (chars &optional number)
  "Parse the head of the current buffer."
  (save-excursion
    (save-restriction
      (unless (zerop (buffer-size))
	(narrow-to-region
	 (goto-char (point-min))
	 (if (search-forward "\n\n" nil t) (1- (point)) (point-max))))
      (let ((headers (nnheader-parse-naked-head)))
	(mail-header-set-chars headers chars)
	(mail-header-set-number headers number)
	headers))))

(defun nndiary-open-nov (group)
  (or (cdr (assoc group nndiary-nov-buffer-alist))
      (let ((buffer (get-buffer-create (format " *nndiary overview %s*"
					       group))))
	(with-current-buffer buffer
	  (set (make-local-variable 'nndiary-nov-buffer-file-name)
	       (expand-file-name
		nndiary-nov-file-name
		(nnmail-group-pathname group nndiary-directory)))
	  (erase-buffer)
	  (when (file-exists-p nndiary-nov-buffer-file-name)
	    (nnheader-insert-file-contents nndiary-nov-buffer-file-name)))
	(push (cons group buffer) nndiary-nov-buffer-alist)
	buffer)))

(defun nndiary-save-nov ()
  (save-excursion
    (while nndiary-nov-buffer-alist
      (when (buffer-name (cdar nndiary-nov-buffer-alist))
	(set-buffer (cdar nndiary-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region 1 (point-max) nndiary-nov-buffer-file-name
			       nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nndiary-nov-buffer-alist (cdr nndiary-nov-buffer-alist)))))

;;;###autoload
(defun nndiary-generate-nov-databases (&optional server)
  "Generate NOV databases in all nndiary directories."
  (interactive (list (or (nnoo-current-server 'nndiary) "")))
  ;; Read the active file to make sure we don't re-use articles
  ;; numbers in empty groups.
  (nnmail-activate 'nndiary)
  (unless (nndiary-server-opened server)
    (nndiary-open-server server))
  (setq nndiary-directory (expand-file-name nndiary-directory))
  ;; Recurse down the directories.
  (nndiary-generate-nov-databases-1 nndiary-directory nil t)
  ;; Save the active file.
  (nnmail-save-active nndiary-group-alist nndiary-active-file))

(defun nndiary-generate-nov-databases-1 (dir &optional seen no-active)
  "Regenerate the NOV database in DIR."
  (interactive "DRegenerate NOV in: ")
  (setq dir (file-name-as-directory dir))
  ;; Only scan this sub-tree if we haven't been here yet.
  (unless (member (file-truename dir) seen)
    (push (file-truename dir) seen)
    ;; We descend recursively
    (let ((dirs (directory-files dir t nil t))
	  dir)
      (while (setq dir (pop dirs))
	(when (and (not (string-match "^\\." (file-name-nondirectory dir)))
		   (file-directory-p dir))
	  (nndiary-generate-nov-databases-1 dir seen))))
    ;; Do this directory.
    (let ((nndiary-files (sort (nnheader-article-to-file-alist dir)
		       'car-less-than-car)))
      (if (not nndiary-files)
	  (let* ((group (nnheader-file-to-group
			 (directory-file-name dir) nndiary-directory))
		 (info (cadr (assoc group nndiary-group-alist))))
	    (when info
	      (setcar info (1+ (cdr info)))))
	(funcall nndiary-generate-active-function dir)
	;; Generate the nov file.
	(nndiary-generate-nov-file dir nndiary-files)
	(unless no-active
	  (nnmail-save-active nndiary-group-alist nndiary-active-file))))))

(defvar nndiary-files) ; dynamically bound in nndiary-generate-nov-databases-1
(defun nndiary-generate-active-info (dir)
  ;; Update the active info for this group.
  (let* ((group (nnheader-file-to-group
		 (directory-file-name dir) nndiary-directory))
	 (entry (assoc group nndiary-group-alist))
	 (last (or (caadr entry) 0)))
    (setq nndiary-group-alist (delq entry nndiary-group-alist))
    (push (list group
		(cons (or (caar nndiary-files) (1+ last))
		      (max last
			   (or (caar (last nndiary-files))
			       0))))
	  nndiary-group-alist)))

(defun nndiary-generate-nov-file (dir files)
  (let* ((dir (file-name-as-directory dir))
	 (nov (concat dir nndiary-nov-file-name))
	 (nov-buffer (get-buffer-create " *nov*"))
	 chars file headers)
    ;; Init the nov buffer.
    (with-current-buffer nov-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (set-buffer nntp-server-buffer)
      ;; Delete the old NOV file.
      (when (file-exists-p nov)
	(funcall nnmail-delete-file-function nov))
      (while files
	(unless (file-directory-p (setq file (concat dir (cdar files))))
	  (erase-buffer)
	  (nnheader-insert-file-contents file)
	  (narrow-to-region
	   (goto-char (point-min))
	   (progn
	     (search-forward "\n\n" nil t)
	     (setq chars (- (point-max) (point)))
	     (max 1 (1- (point)))))
	  (unless (zerop (buffer-size))
	    (goto-char (point-min))
	    (setq headers (nndiary-parse-head chars (caar files)))
	    (with-current-buffer nov-buffer
	      (goto-char (point-max))
	      (nnheader-insert-nov headers)))
	  (widen))
	(setq files (cdr files)))
      (with-current-buffer nov-buffer
	(nnmail-write-region 1 (point-max) nov nil 'nomesg)
	(kill-buffer (current-buffer))))))

(defun nndiary-nov-delete-article (group article)
  (with-current-buffer (nndiary-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (when (bobp)
	(let ((active (cadr (assoc group nndiary-group-alist)))
	      num)
	  (when active
	    (if (eobp)
		(setf (car active) (1+ (cdr active)))
	      (when (and (setq num (ignore-errors (read (current-buffer))))
			 (numberp num))
		(setf (car active) num)))))))
    t))

(defun nndiary-update-file-alist (&optional force)
  (when (or (not nndiary-article-file-alist)
	    force)
    (setq nndiary-article-file-alist
	  (nnheader-article-to-file-alist nndiary-current-directory))))


(defun nndiary-string-to-number (str min &optional max)
  ;; Like `string-to-number' but barf if STR is not exactly an integer, and not
  ;; within the specified bounds.
  ;; Signals are caught by `nndiary-schedule'.
  (if (not (string-match "^[ \t]*[0-9]+[ \t]*$" str))
      (nndiary-error "not an integer value")
    ;; else
    (let ((val (string-to-number str)))
      (and (or (< val min)
	       (and max (> val max)))
	   (nndiary-error "value out of range"))
      val)))

(defun nndiary-parse-schedule-value (str min-or-values max)
  ;; Parse the schedule string STR, or signal an error.
  ;; Signals are caught by `nndiary-schedule'.
  (if (string-match "[ \t]*\\*[ \t]*" str)
      ;; unspecified
      nil
    ;; specified
    (if (listp min-or-values)
	;; min-or-values is values
	;; #### NOTE: this is actually only a hack for time zones.
	(let ((val (and (string-match "[ \t]*\\([^ \t]+\\)[ \t]*" str)
			(match-string 1 str))))
	  (if (and val (setq val (assoc val min-or-values)))
	      (list (cadr val))
	    (nndiary-error "invalid syntax")))
      ;; min-or-values is min
      (mapcar
       (lambda (val)
	 (let ((res (split-string val "-")))
	   (cond
	    ((= (length res) 1)
	     (nndiary-string-to-number (car res) min-or-values max))
	    ((= (length res) 2)
	     ;; don't know if crontab accepts this, but ensure
	     ;; that BEG is <= END
	     (let ((beg (nndiary-string-to-number (car res) min-or-values max))
		   (end (nndiary-string-to-number (cadr res) min-or-values max)))
	       (cond ((< beg end)
		      (cons beg end))
		     ((= beg end)
		      beg)
		     (t
		      (cons end beg)))))
	    (t
	     (nndiary-error "invalid syntax")))
	   ))
       (split-string str ",")))
    ))

;; ### FIXME: remove this function if it's used only once.
(defun nndiary-parse-schedule (head min-or-values max)
  ;; Parse the cron-like value of header X-Diary-HEAD in current buffer.
  ;; - Returns nil if `*'
  ;; - Otherwise returns a list of integers and/or ranges (BEG . END)
  ;; The exception is the Timze-Zone value which is always of the form (STR).
  ;; Signals are caught by `nndiary-schedule'.
  (let ((header (format "^X-Diary-%s: \\(.*\\)$" head)))
    (goto-char (point-min))
    (if (not (re-search-forward header nil t))
	(nndiary-error "header missing")
      ;; else
      (nndiary-parse-schedule-value (match-string 1) min-or-values max))
    ))

(defun nndiary-max (spec)
  ;; Returns the max of specification SPEC, or nil for permanent schedules.
  (unless (null spec)
    (let ((elts spec)
	  (max 0)
	  elt)
      (while (setq elt (pop elts))
	(if (integerp elt)
	    (and (> elt max) (setq max elt))
	  (and (> (cdr elt) max) (setq max (cdr elt)))))
      max)))

(defun nndiary-flatten (spec min &optional max)
  ;; flatten the spec by expanding ranges to all possible values.
  (let (flat n)
    (cond ((null spec)
	   ;; this happens when I flatten something else than one of my
	   ;; schedules (a list of read articles for instance).
	   (unless (null max)
	     (setq n min)
	     (while (<= n max)
	       (push n flat)
	       (setq n (1+ n)))))
	  (t
	   (let ((elts spec)
		 elt)
	     (while (setq elt (pop elts))
	       (if (integerp elt)
		   (push elt flat)
		 ;; else
		 (setq n (car elt))
		 (while (<= n (cdr elt))
		   (push n flat)
		   (setq n (1+ n))))))))
    flat))

(defun nndiary-unflatten (spec)
  ;; opposite of flatten: build ranges if possible
  (setq spec (sort spec '<))
  (let (min max res)
    (while (setq min (pop spec))
      (setq max min)
      (while (and (car spec) (= (car spec) (1+ max)))
	(setq max (1+ max))
	(pop spec))
      (if (= max min)
	  (setq res (append res (list min)))
	(setq res (append res (list (cons min max))))))
    res))

(defun nndiary-compute-reminders (date)
  ;; Returns a list of times corresponding to the reminders of date DATE.
  ;; See the comment in `nndiary-reminders' about rounding.
  (let* ((reminders nndiary-reminders)
	 (date-elts (decode-time date))
	 ;; ### NOTE: out-of-range values are accepted by encode-time. This
	 ;; makes our life easier.
	 (monday (- (nth 3 date-elts)
		    (if nndiary-week-starts-on-monday
			(if (zerop (nth 6 date-elts))
			    6
			  (- (nth 6 date-elts) 1))
		      (nth 6 date-elts))))
	 reminder res)
    ;; remove the DOW and DST entries
    (setcdr (nthcdr 5 date-elts) (nthcdr 8 date-elts))
    (while (setq reminder (pop reminders))
      (push
       (cond ((eq (cdr reminder) 'minute)
	      (subtract-time
	       (apply 'encode-time 0 (nthcdr 1 date-elts))
	       (seconds-to-time (* (car reminder) 60.0))))
	     ((eq (cdr reminder) 'hour)
	      (subtract-time
	       (apply 'encode-time 0 0 (nthcdr 2 date-elts))
	       (seconds-to-time (* (car reminder) 3600.0))))
	     ((eq (cdr reminder) 'day)
	      (subtract-time
	       (apply 'encode-time 0 0 0 (nthcdr 3 date-elts))
	       (seconds-to-time (* (car reminder) 86400.0))))
	     ((eq (cdr reminder) 'week)
	      (subtract-time
	       (apply 'encode-time 0 0 0 monday (nthcdr 4 date-elts))
	       (seconds-to-time (* (car reminder) 604800.0))))
	     ((eq (cdr reminder) 'month)
	      (subtract-time
	       (apply 'encode-time 0 0 0 1 (nthcdr 4 date-elts))
	       (seconds-to-time (* (car reminder) 18748800.0))))
	     ((eq (cdr reminder) 'year)
	      (subtract-time
	       (apply 'encode-time 0 0 0 1 1 (nthcdr 5 date-elts))
	       (seconds-to-time (* (car reminder) 400861056.0)))))
       res))
    (sort res 'time-less-p)))

(defun nndiary-last-occurence (sched)
  ;; Returns the last occurrence of schedule SCHED as an Emacs time struct, or
  ;; nil for permanent schedule or errors.
  (let ((minute (nndiary-max (nth 0 sched)))
	(hour (nndiary-max (nth 1 sched)))
	(year (nndiary-max (nth 4 sched)))
	(time-zone (or (and (nth 6 sched) (car (nth 6 sched)))
		       (current-time-zone))))
    (when year
      (or minute (setq minute 59))
      (or hour (setq hour 23))
      ;; I'll just compute all possible values and test them by decreasing
      ;; order until one succeeds. This is probably quite rude, but I got
      ;; bored in finding a good algorithm for doing that ;-)
      ;; ### FIXME: remove identical entries.
      (let ((dom-list (nth 2 sched))
	    (month-list (sort (nndiary-flatten (nth 3 sched) 1 12) '>))
	    (year-list (sort (nndiary-flatten (nth 4 sched) 1971) '>))
	    (dow-list (nth 5 sched)))
	;; Special case: an asterisk in one of the days specifications means
	;; that only the other should be taken into account. If both are
	;; unspecified, you would get all possible days in both.
	(cond ((null dow-list)
	       ;; this gets all days if dom-list is nil
	       (setq dom-list (nndiary-flatten dom-list 1 31)))
	      ((null dom-list)
	       ;; this also gets all days if dow-list is nil
	       (setq dow-list (nndiary-flatten dow-list 0 6)))
	      (t
	       (setq dom-list (nndiary-flatten dom-list 1 31))
	       (setq dow-list (nndiary-flatten dow-list 0 6))))
	(or
	 (catch 'found
	   (while (setq year (pop year-list))
	     (let ((months month-list)
		   month)
	       (while (setq month (pop months))
		 ;; Now we must merge the Dows with the Doms. To do that, we
		 ;; have to know which day is the 1st one for this month.
		 ;; Maybe there's simpler, but decode-time(encode-time) will
		 ;; give us the answer.
		 (let ((first (nth 6 (decode-time
				      (encode-time 0 0 0 1 month year
						   time-zone))))
		       (max (cond ((= month 2)
				   (if (date-leap-year-p year) 29 28))
				  ((<= month 7)
				   (if (zerop (% month 2)) 30 31))
				  (t
				   (if (zerop (% month 2)) 31 30))))
		       (doms dom-list)
		       (dows dow-list)
		       day days)
		   ;; first, review the doms to see if they are valid.
		   (while (setq day (pop doms))
		     (and (<= day max)
			  (push day days)))
		   ;; second add all possible dows
		   (while (setq day (pop dows))
		     ;; days start at 1.
		     (setq day (1+ (- day first)))
		     (and (< day 0) (setq day (+ 7 day)))
		     (while (<= day max)
		       (push day days)
		       (setq day (+ 7 day))))
		   ;; Finally, if we have some days, they are valid
		   (when days
		     (sort days '>)
		     (throw 'found
			    (encode-time 0 minute hour
					 (car days) month year time-zone)))
		   )))))
	 ;; There's an upper limit, but we didn't find any last occurrence.
	 ;; This means that the schedule is undecidable. This can happen if
	 ;; you happen to say something like "each Feb 31 until 2038".
	 (progn
	   (nnheader-report 'nndiary "Undecidable schedule")
	   nil))
	))))

(defun nndiary-next-occurence (sched now)
  ;; Returns the next occurrence of schedule SCHED, starting from time NOW.
  ;; If there's no next occurrence, returns the last one (if any) which is then
  ;; in the past.
  (let* ((today (decode-time now))
	 (this-minute (nth 1 today))
	 (this-hour (nth 2 today))
	 (this-day (nth 3 today))
	 (this-month (nth 4 today))
	 (this-year (nth 5 today))
	 (minute-list (sort (nndiary-flatten (nth 0 sched) 0 59) '<))
	 (hour-list (sort (nndiary-flatten (nth 1 sched) 0 23) '<))
	 (dom-list (nth 2 sched))
	 (month-list (sort (nndiary-flatten (nth 3 sched) 1 12) '<))
	 (years (if (nth 4 sched)
		    (sort (nndiary-flatten (nth 4 sched) 1971) '<)
		  t))
	 (dow-list (nth 5 sched))
	 (year (1- this-year))
	 (time-zone (or (and (nth 6 sched) (car (nth 6 sched)))
			(current-time-zone))))
    ;; Special case: an asterisk in one of the days specifications means that
    ;; only the other should be taken into account. If both are unspecified,
    ;; you would get all possible days in both.
    (cond ((null dow-list)
	   ;; this gets all days if dom-list is nil
	   (setq dom-list (nndiary-flatten dom-list 1 31)))
	  ((null dom-list)
	   ;; this also gets all days if dow-list is nil
	   (setq dow-list (nndiary-flatten dow-list 0 6)))
	  (t
	   (setq dom-list (nndiary-flatten dom-list 1 31))
	   (setq dow-list (nndiary-flatten dow-list 0 6))))
    ;; Remove past years.
    (unless (eq years t)
      (while (and (car years) (< (car years) this-year))
	(pop years)))
    (if years
	;; Because we might not be limited in years, we must guard against
	;; infinite loops. Appart from cases like Feb 31, there are probably
	;; other ones, (no monday XXX 2nd etc). I don't know any algorithm to
	;; decide this, so I assume that if we reach 10 years later, the
	;; schedule is undecidable.
	(or
	 (catch 'found
	   (while (if (eq years t)
		      (and (setq year (1+ year))
			   (<= year (+ 10 this-year)))
		    (setq year (pop years)))
	     (let ((months month-list)
		   month)
	       ;; Remove past months for this year.
	       (and (= year this-year)
		    (while (and (car months) (< (car months) this-month))
		      (pop months)))
	       (while (setq month (pop months))
		 ;; Now we must merge the Dows with the Doms. To do that, we
		 ;; have to know which day is the 1st one for this month.
		 ;; Maybe there's simpler, but decode-time(encode-time) will
		 ;; give us the answer.
		 (let ((first (nth 6 (decode-time
				      (encode-time 0 0 0 1 month year
						   time-zone))))
		       (max (cond ((= month 2)
				   (if (date-leap-year-p year) 29 28))
				  ((<= month 7)
				   (if (zerop (% month 2)) 30 31))
				  (t
				   (if (zerop (% month 2)) 31 30))))
		       (doms dom-list)
		       (dows dow-list)
		       day days)
		   ;; first, review the doms to see if they are valid.
		   (while (setq day (pop doms))
		     (and (<= day max)
			  (push day days)))
		   ;; second add all possible dows
		   (while (setq day (pop dows))
		     ;; days start at 1.
		     (setq day (1+ (- day first)))
		     (and (< day 0) (setq day (+ 7 day)))
		     (while (<= day max)
		       (push day days)
		       (setq day (+ 7 day))))
		   ;; Aaaaaaall right. Now we have a valid list of DAYS for
		   ;; this month and this year.
		   (when days
		     (setq days (sort days '<))
		     ;; Remove past days for this year and this month.
		     (and (= year this-year)
			  (= month this-month)
			  (while (and (car days) (< (car days) this-day))
			    (pop days)))
		     (while (setq day (pop days))
		       (let ((hours hour-list)
			     hour)
			 ;; Remove past hours for this year, this month and
			 ;; this day.
			 (and (= year this-year)
			      (= month this-month)
			      (= day this-day)
			      (while (and (car hours)
					  (< (car hours) this-hour))
				(pop hours)))
			 (while (setq hour (pop hours))
			   (let ((minutes minute-list)
				 minute)
			     ;; Remove past hours for this year, this month,
			     ;; this day and this hour.
			     (and (= year this-year)
				  (= month this-month)
				  (= day this-day)
				  (= hour this-hour)
				  (while (and (car minutes)
					      (< (car minutes) this-minute))
				    (pop minutes)))
			     (while (setq minute (pop minutes))
			       ;; Ouch! Here, we've got a complete valid
			       ;; schedule. It's a good one if it's in the
			       ;; future.
			       (let ((time (encode-time 0 minute hour day
							month year
							time-zone)))
				 (and (time-less-p now time)
				      (throw 'found time)))
			       ))))
		       ))
		   )))
	     ))
	 (nndiary-last-occurence sched))
      ;; else
      (nndiary-last-occurence sched))
    ))

(defun nndiary-expired-article-p (file)
  (with-temp-buffer
    (if (nnheader-insert-head file)
	(let ((sched (nndiary-schedule)))
	  ;; An article has expired if its last schedule (if any) is in the
	  ;; past. A permanent schedule never expires.
	  (and sched
	       (setq sched (nndiary-last-occurence sched))
	       (time-less-p sched (current-time))))
      ;; else
      (nnheader-report 'nndiary "Could not read file %s" file)
      nil)
    ))

(defun nndiary-renew-article-p (file timestamp)
  (erase-buffer)
  (if (nnheader-insert-head file)
      (let ((now (current-time))
	    (sched (nndiary-schedule)))
	;; The article should be re-considered as unread if there's a reminder
	;; between the group timestamp and the current time.
	(when (and sched (setq sched (nndiary-next-occurence sched now)))
	  (let ((reminders ;; add the next occurrence itself at the end.
		 (append (nndiary-compute-reminders sched) (list sched))))
	    (while (and reminders (time-less-p (car reminders) timestamp))
	      (pop reminders))
	    ;; The reminders might be empty if the last date is in the past,
	    ;; or we've got at least the next occurrence itself left. All past
	    ;; dates are renewed.
	    (or (not reminders)
		(time-less-p (car reminders) now)))
	  ))
    ;; else
    (nnheader-report 'nndiary "Could not read file %s" file)
    nil))

;; The end... ===============================================================

(dolist (header nndiary-headers)
  (setq header (intern (format "X-Diary-%s" (car header))))
  ;; Required for building NOV databases and some other stuff.
  (add-to-list 'gnus-extra-headers header)
  (add-to-list 'nnmail-extra-headers header))

(unless (assoc "nndiary" gnus-valid-select-methods)
  (gnus-declare-backend "nndiary" 'post-mail 'respool 'address))

(provide 'nndiary)

;;; nndiary.el ends here
