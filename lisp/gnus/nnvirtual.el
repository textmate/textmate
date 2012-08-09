;;; nnvirtual.el --- virtual newsgroups access for Gnus

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: David Moore <dmoore@ucsd.edu>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news

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

;; The other access methods (nntp, nnspool, etc) are general news
;; access methods.  This module relies on Gnus and can not be used
;; separately.

;;; Code:

(require 'nntp)
(require 'nnheader)
(require 'gnus)
(require 'nnoo)
(require 'gnus-util)
(require 'gnus-start)
(require 'gnus-sum)
(require 'gnus-msg)
(eval-when-compile (require 'cl))

(nnoo-declare nnvirtual)

(defvoo nnvirtual-always-rescan t
  "If non-nil, always scan groups for unread articles when entering a group.
If this variable is nil and you read articles in a component group
after the virtual group has been activated, the read articles from the
component group will show up when you enter the virtual group.")

(defvoo nnvirtual-component-regexp nil
  "Regexp to match component groups.")

(defvoo nnvirtual-component-groups nil
  "Component group in this nnvirtual group.")



(defconst nnvirtual-version "nnvirtual 1.1")

(defvoo nnvirtual-current-group nil)

(defvoo nnvirtual-mapping-table nil
  "Table of rules on how to map between component group and article number to virtual article number.")

(defvoo nnvirtual-mapping-offsets nil
  "Table indexed by component group to an offset to be applied to article numbers in that group.")

(defvoo nnvirtual-mapping-len 0
  "Number of articles in this virtual group.")

(defvoo nnvirtual-mapping-reads nil
  "Compressed sequence of read articles on the virtual group as computed from the unread status of individual component groups.")

(defvoo nnvirtual-mapping-marks nil
  "Compressed marks alist for the virtual group as computed from the marks of individual component groups.")

(defvoo nnvirtual-info-installed nil
  "T if we have already installed the group info for this group, and shouldn't blast over it again.")

(defvoo nnvirtual-status-string "")

(autoload 'gnus-cache-articles-in-group "gnus-cache")



;;; Interface functions.

(nnoo-define-basics nnvirtual)


(deffoo nnvirtual-retrieve-headers (articles &optional newsgroup
					     server fetch-old)
  (when (nnvirtual-possibly-change-server server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (if (stringp (car articles))
	  'headers
	(let ((vbuf (nnheader-set-temp-buffer
		     (get-buffer-create " *virtual headers*")))
	      (carticles (nnvirtual-partition-sequence articles))
	      (system-name (system-name))
	      cgroup carticle article result prefix)
	  (while carticles
	    (setq cgroup (caar carticles))
	    (setq articles (cdar carticles))
	    (pop carticles)
	    (when (and articles
		       (gnus-check-server
			(gnus-find-method-for-group cgroup) t)
		       (gnus-request-group cgroup t)
		       (setq prefix (gnus-group-real-prefix cgroup))
		       ;; FIX FIX FIX we want to check the cache!
		       ;; This is probably evil if people have set
		       ;; gnus-use-cache to nil themselves, but I
		       ;; have no way of finding the true value of it.
		       (let ((gnus-use-cache t))
			 (setq result (gnus-retrieve-headers
				       articles cgroup nil))))
	      (set-buffer nntp-server-buffer)
	      ;; If we got HEAD headers, we convert them into NOV
	      ;; headers.  This is slow, inefficient and, come to think
	      ;; of it, downright evil.  So sue me.  I couldn't be
	      ;; bothered to write a header parse routine that could
	      ;; parse a mixed HEAD/NOV buffer.
	      (when (eq result 'headers)
		(nnvirtual-convert-headers))
	      (goto-char (point-min))
	      (while (not (eobp))
		(delete-region (point)
			       (progn
				 (setq carticle (read nntp-server-buffer))
				 (point)))

		;; We remove this article from the articles list, if
		;; anything is left in the articles list after going through
		;; the entire buffer, then those articles have been
		;; expired or canceled, so we appropriately update the
		;; component group below.  They should be coming up
		;; generally in order, so this shouldn't be slow.
		(setq articles (delq carticle articles))

		(setq article (nnvirtual-reverse-map-article cgroup carticle))
		(if (null article)
		    ;; This line has no reverse mapping, that means it
		    ;; was an extra article reference returned by nntp.
		    (progn
		      (beginning-of-line)
		      (delete-region (point) (progn (forward-line 1) (point))))
		  ;; Otherwise insert the virtual article number,
		  ;; and clean up the xrefs.
		  (princ article nntp-server-buffer)
		  (nnvirtual-update-xref-header cgroup carticle
						prefix system-name)
		  (forward-line 1))
		)

	      (set-buffer vbuf)
	      (goto-char (point-max))
	      (insert-buffer-substring nntp-server-buffer))
	    ;; Anything left in articles is expired or canceled.
	    ;; Could be smart and not tell it about articles already known?
	    (when articles
	      (gnus-group-make-articles-read cgroup articles))
	    )

	  ;; The headers are ready for reading, so they are inserted into
	  ;; the nntp-server-buffer, which is where Gnus expects to find
	  ;; them.
	  (prog1
	      (with-current-buffer nntp-server-buffer
		(erase-buffer)
		(insert-buffer-substring vbuf)
		;; FIX FIX FIX, we should be able to sort faster than
		;; this if needed, since each cgroup is sorted, we just
		;; need to merge
		(sort-numeric-fields 1 (point-min) (point-max))
		'nov)
	    (kill-buffer vbuf)))))))


(defvoo nnvirtual-last-accessed-component-group nil)

(deffoo nnvirtual-request-article (article &optional group server buffer)
  (when (nnvirtual-possibly-change-server server)
    (if (stringp article)
	;; This is a fetch by Message-ID.
	(cond
	 ((not nnvirtual-last-accessed-component-group)
	  (nnheader-report
	   'nnvirtual "Don't know what server to request from"))
	 (t
	  (save-excursion
	    (when buffer
	      (set-buffer buffer))
	    (let* ((gnus-override-method nil)
		   (gnus-command-method
		    (gnus-find-method-for-group
		     nnvirtual-last-accessed-component-group)))
	      (funcall (gnus-get-function gnus-command-method 'request-article)
		       article nil (nth 1 gnus-command-method) buffer)))))
      ;; This is a fetch by number.
      (let* ((amap (nnvirtual-map-article article))
	     (cgroup (car amap)))
	(cond
	 ((not amap)
	  (nnheader-report 'nnvirtual "No such article: %s" article))
	 ((not (gnus-check-group cgroup))
	  (nnheader-report
	   'nnvirtual "Can't open server where %s exists" cgroup))
	 ((not (gnus-request-group cgroup t))
	  (nnheader-report 'nnvirtual "Can't open component group %s" cgroup))
	 (t
	  (setq nnvirtual-last-accessed-component-group cgroup)
	  (if buffer
	      (with-current-buffer buffer
		;; We bind this here to avoid double decoding.
		(let ((gnus-article-decode-hook nil))
		  (gnus-request-article-this-buffer (cdr amap) cgroup)))
	    (gnus-request-article (cdr amap) cgroup))))))))


(deffoo nnvirtual-open-server (server &optional defs)
  (unless (assq 'nnvirtual-component-regexp defs)
    (push `(nnvirtual-component-regexp ,server)
	  defs))
  (nnoo-change-server 'nnvirtual server defs)
  (if nnvirtual-component-groups
      t
    (setq nnvirtual-mapping-table nil
	  nnvirtual-mapping-offsets nil
	  nnvirtual-mapping-len 0
	  nnvirtual-mapping-reads nil
	  nnvirtual-mapping-marks nil
	  nnvirtual-info-installed nil)
    (when nnvirtual-component-regexp
      ;; Go through the newsrc alist and find all component groups.
      (let ((newsrc (cdr gnus-newsrc-alist))
	    group)
	(while (setq group (car (pop newsrc)))
	  (when (string-match nnvirtual-component-regexp group) ; Match
	    ;; Add this group to the list of component groups.
	    (setq nnvirtual-component-groups
		  (cons group (delete group nnvirtual-component-groups)))))))
    (if (not nnvirtual-component-groups)
	(nnheader-report 'nnvirtual "No component groups: %s" server)
      t)))


(deffoo nnvirtual-request-group (group &optional server dont-check info)
  (nnvirtual-possibly-change-server server)
  (setq nnvirtual-component-groups
	(delete (nnvirtual-current-group) nnvirtual-component-groups))
  (cond
   ((null nnvirtual-component-groups)
    (setq nnvirtual-current-group nil)
    (nnheader-report 'nnvirtual "No component groups in %s" group))
   (t
    (setq nnvirtual-current-group group)
    (nnvirtual-create-mapping dont-check)
    (when nnvirtual-always-rescan
      (nnvirtual-request-update-info
       (nnvirtual-current-group)
       (gnus-get-info (nnvirtual-current-group))))
    (nnheader-insert "211 %d 1 %d %s\n"
		     nnvirtual-mapping-len nnvirtual-mapping-len group))))


(deffoo nnvirtual-request-type (group &optional article)
  (if (not article)
      'unknown
    (if (numberp article)
	(let ((mart (nnvirtual-map-article article)))
	  (if mart
	      (gnus-request-type (car mart) (cdr mart))))
      (gnus-request-type
       nnvirtual-last-accessed-component-group nil))))

(deffoo nnvirtual-request-update-mark (group article mark)
  (let* ((nart (nnvirtual-map-article article))
	 (cgroup (car nart)))
    (when (and nart
	       (memq mark gnus-auto-expirable-marks)
	       ;; The component group might be a virtual group.
	       (= mark (gnus-request-update-mark cgroup (cdr nart) mark))
	       (gnus-group-auto-expirable-p cgroup))
      (setq mark gnus-expirable-mark)))
  mark)


(deffoo nnvirtual-close-group (group &optional server)
  (when (and (nnvirtual-possibly-change-server server)
	     (not (gnus-ephemeral-group-p (nnvirtual-current-group))))
    (nnvirtual-update-read-and-marked t t))
  t)


(deffoo nnvirtual-request-newgroups (date &optional server)
  (nnheader-report 'nnvirtual "NEWGROUPS is not supported."))


(deffoo nnvirtual-request-list-newsgroups (&optional server)
  (nnheader-report 'nnvirtual "LIST NEWSGROUPS is not implemented."))


(deffoo nnvirtual-request-update-info (group info &optional server)
  (when (and (nnvirtual-possibly-change-server server)
	     (not nnvirtual-info-installed))
    ;; Install the precomputed lists atomically, so the virtual group
    ;; is not left in a half-way state in case of C-g.
    (gnus-atomic-progn
      (setcar (cddr info) nnvirtual-mapping-reads)
      (if (nthcdr 3 info)
	  (setcar (nthcdr 3 info) nnvirtual-mapping-marks)
	(when nnvirtual-mapping-marks
	  (setcdr (nthcdr 2 info) (list nnvirtual-mapping-marks))))
      (setq nnvirtual-info-installed t))
    t))


(deffoo nnvirtual-catchup-group (group &optional server all)
  (when (and (nnvirtual-possibly-change-server server)
	     (not (gnus-ephemeral-group-p (nnvirtual-current-group))))
    ;; copy over existing marks first, in case they set anything
    (nnvirtual-update-read-and-marked nil nil)
    ;; do a catchup on all component groups
    (let ((gnus-group-marked (copy-sequence nnvirtual-component-groups))
	  (gnus-expert-user t))
      ;; Make sure all groups are activated.
      (mapc
       (lambda (g)
	 (when (not (numberp (gnus-group-unread g)))
	   (gnus-activate-group g)))
       nnvirtual-component-groups)
      (with-current-buffer gnus-group-buffer
	(gnus-group-catchup-current nil all)))))


(deffoo nnvirtual-find-group-art (group article)
  "Return the real group and article for virtual GROUP and ARTICLE."
  (nnvirtual-map-article article))


(deffoo nnvirtual-request-post (&optional server)
  (if (not gnus-message-group-art)
      (nnheader-report 'nnvirtual "Can't post to an nnvirtual group")
    (let ((group (car (nnvirtual-find-group-art
		       (car gnus-message-group-art)
		       (cdr gnus-message-group-art)))))
      (gnus-request-post (gnus-find-method-for-group group)))))


(deffoo nnvirtual-request-expire-articles (articles group
						    &optional server force)
  (nnvirtual-possibly-change-server server)
  (setq nnvirtual-component-groups
	(delete (nnvirtual-current-group) nnvirtual-component-groups))
  (let (unexpired)
    (dolist (group nnvirtual-component-groups)
      (setq unexpired (nconc unexpired
			     (mapcar
			      #'(lambda (article)
				  (nnvirtual-reverse-map-article
				   group article))
			      (gnus-uncompress-range
			       (gnus-group-expire-articles-1 group))))))
    (sort (delq nil unexpired) '<)))


;;; Internal functions.

(defun nnvirtual-convert-headers ()
  "Convert HEAD headers into NOV headers."
  (with-current-buffer nntp-server-buffer
    (let* ((dependencies (make-vector 100 0))
	   (headers (gnus-get-newsgroup-headers dependencies)))
      (erase-buffer)
      (mapc 'nnheader-insert-nov headers))))


(defun nnvirtual-update-xref-header (group article prefix system-name)
  "Edit current NOV header in current buffer to have an xref to the component group, and also server prefix any existing xref lines."
  ;; Move to beginning of Xref field, creating a slot if needed.
  (beginning-of-line)
  (looking-at
   "[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t")
  (goto-char (match-end 0))
  (unless (search-forward "\t" (point-at-eol) 'move)
    (insert "\t"))

  ;; Remove any spaces at the beginning of the Xref field.
  (while (eq (char-after (1- (point))) ? )
    (forward-char -1)
    (delete-char 1))

  (insert "Xref: " system-name " " group ":")
  (princ article (current-buffer))
  (insert " ")

  ;; If there were existing xref lines, clean them up to have the correct
  ;; component server prefix.
  (save-restriction
    (narrow-to-region (point)
		      (or (search-forward "\t" (point-at-eol) t)
			  (point-at-eol)))
    (goto-char (point-min))
    (when (re-search-forward "Xref: *[^\n:0-9 ]+ *" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (when (re-search-forward
	   (concat (regexp-quote (gnus-group-real-name group)) ":[0-9]+")
	   nil t)
      (replace-match "" t t))
    (unless (eobp)
      (insert " ")
      (when (not (string= "" prefix))
	(while (re-search-forward "[^ ]+:[0-9]+" nil t)
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (insert prefix))))))

  ;; Ensure a trailing \t.
  (end-of-line)
  (or (eq (char-after (1- (point))) ?\t)
      (insert ?\t)))


(defun nnvirtual-possibly-change-server (server)
  (or (not server)
      (nnoo-current-server-p 'nnvirtual server)
      (nnvirtual-open-server server)))


(defun nnvirtual-update-read-and-marked (read-p update-p)
  "Copy marks from the virtual group to the component groups.
If READ-P is not nil, update the (un)read status of the components.
If UPDATE-P is not nil, call gnus-group-update-group on the components."
  (when nnvirtual-current-group
    (let ((unreads (and read-p
			(nnvirtual-partition-sequence
			 (gnus-list-of-unread-articles
			  (nnvirtual-current-group)))))
	  (type-marks
	   (delq nil
		 (mapcar (lambda (ml)
			   (if (eq (car ml) 'score)
			       nil
			     (cons (car ml)
				   (nnvirtual-partition-sequence (cdr ml)))))
			 (gnus-info-marks (gnus-get-info
					   (nnvirtual-current-group))))))
	  type groups info)

      ;; Ok, atomically move all of the (un)read info, clear any old
      ;; marks, and move all of the current marks.  This way if someone
      ;; hits C-g, you won't leave the component groups in a half-way state.
      (progn
	;; move (un)read
	;; bind for workaround guns-update-read-articles
	(let ((gnus-newsgroup-active nil))
	  (dolist (entry unreads)
	    (gnus-update-read-articles (car entry) (cdr entry))))

	;; clear all existing marks on the component groups
	(dolist (group nnvirtual-component-groups)
	  (when (and (setq info (gnus-get-info group))
		     (gnus-info-marks info))
	    (gnus-info-set-marks
	     info
	     (if (assq 'score (gnus-info-marks info))
		 (list (assq 'score (gnus-info-marks info)))
	       nil))))

	;; Ok, currently type-marks is an assq list with keys of a mark type,
	;; with data of an assq list with keys of component group names
	;; and the articles which correspond to that key/group pair.
	(dolist (mark type-marks)
	  (setq type (car mark))
	  (setq groups (cdr mark))
	  (dolist (carticles groups)
	    (gnus-add-marked-articles (car carticles) type (cdr carticles)
				      nil t))))

      ;; possibly update the display, it is really slow
      (when update-p
	(dolist (group nnvirtual-component-groups)
	  (gnus-group-update-group group t))))))


(defun nnvirtual-current-group ()
  "Return the prefixed name of the current nnvirtual group."
  (concat "nnvirtual:" nnvirtual-current-group))



;;; This is currently O(kn^2) to merge n lists of length k.
;;; You could do it in O(knlogn), but we have a small n, and the
;;; overhead of the other approach is probably greater.
(defun nnvirtual-merge-sorted-lists (&rest lists)
  "Merge many sorted lists of numbers."
  (if (null (cdr lists))
      (car lists)
    (sort (apply 'nconc lists) '<)))


;;; We map between virtual articles and real articles in a manner
;;; which keeps the size of the virtual active list the same as the
;;; sum of the component active lists.

;;; To achieve fair mixing of the groups, the last article in each of
;;; N component groups will be in the last N articles in the virtual
;;; group.

;;; If you have 3 components A, B and C, with articles 1-8, 1-5, and
;;; 6-7 respectively, then the virtual article numbers look like:
;;;
;;;  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
;;;  A1 A2 A3 A4 B1 A5 B2 A6 B3 A7 B4 C6 A8 B5 C7

;;; To compute these mappings we generate a couple tables and then
;;; do some fast operations on them.  Tables for the example above:
;;;
;;; Offsets - [(A 0) (B -3) (C -1)]
;;;
;;;               a  b  c  d  e
;;; Mapping - ([  3  0  1  3  0 ]
;;;            [  6  3  2  9  3 ]
;;;            [  8  6  3 15  9 ])
;;;
;;; (note column 'e' is different in real algorithm, which is slightly
;;;  different than described here, but this gives you the methodology.)
;;;
;;; The basic idea is this, when going from component->virtual, apply
;;; the appropriate offset to the article number.  Then search the first
;;; column of the table for a row where 'a' is less than or equal to the
;;; modified number.  You can see that only group A can therefore go to
;;; the first row, groups A and B to the second, and all to the last.
;;; The third column of the table is telling us the number of groups
;;; which might be able to reach that row (it might increase by more than
;;; 1 if several groups have the same size).
;;; Then column 'b' provides an additional offset you apply when you have
;;; found the correct row.  You then multiply by 'c' and add on the groups
;;; _position_ in the offset table.  The basic idea here is that on
;;; any given row we are going to map back and forth using X'=X*c+Y and
;;; X=(X'/c), Y=(X' mod c).  Then once you've done this transformation,
;;; you apply a final offset from column 'e' to give the virtual article.
;;;
;;; Going the other direction, you instead search on column 'd' instead
;;; of 'a', and apply everything in reverse order.

;;; Convert component -> virtual:
;;; set num = num - Offset(group)
;;; find first row in Mapping where num <= 'a'
;;; num = (num-'b')*c + Position(group) + 'e'

;;; Convert virtual -> component:
;;; find first row in Mapping where num <= 'd'
;;; num = num - 'e'
;;; group_pos = num mod 'c'
;;; num = (num / 'c') + 'b' + Offset(group_pos)

;;; Easy no? :)
;;;
;;; Well actually, you need to keep column e offset smaller by the 'c'
;;; column for that line, and always add 1 more when going from
;;; component -> virtual.  Otherwise you run into a problem with
;;; unique reverse mapping.

(defun nnvirtual-map-article (article)
  "Return a cons of the component group and article corresponding to the given virtual ARTICLE."
  (let ((table nnvirtual-mapping-table)
	entry group-pos)
    (while (and table
		(> article (aref (car table) 3)))
      (setq table (cdr table)))
    (when (and table
	       (> article 0))
      (setq entry (car table))
      (setq article (- article (aref entry 4) 1))
      (setq group-pos (mod article (aref entry 2)))
      (cons (car (aref nnvirtual-mapping-offsets group-pos))
	    (+ (/ article (aref entry 2))
	       (aref entry 1)
	       (cdr (aref nnvirtual-mapping-offsets group-pos)))
	    ))
    ))



(defun nnvirtual-reverse-map-article (group article)
  "Return the virtual article number corresponding to the given component GROUP and ARTICLE."
  (when (numberp article)
    (let ((table nnvirtual-mapping-table)
	  (group-pos 0)
	  entry)
      (while (not (string= group (car (aref nnvirtual-mapping-offsets
					    group-pos))))
	(setq group-pos (1+ group-pos)))
      (setq article (- article (cdr (aref nnvirtual-mapping-offsets
					  group-pos))))
      (while (and table
		  (> article (aref (car table) 0)))
	(setq table (cdr table)))
      (setq entry (car table))
      (when (and entry
		 (> article 0)
		 (< group-pos (aref entry 2))) ; article not out of range below
	(+ (aref entry 4)
	   group-pos
	   (* (- article (aref entry 1))
	      (aref entry 2))
	   1))
      )))


(defsubst nnvirtual-reverse-map-sequence (group articles)
  "Return list of virtual article numbers for all ARTICLES in GROUP.
The ARTICLES should be sorted, and can be a compressed sequence.
If any of the article numbers has no corresponding virtual article,
then it is left out of the result."
  (when (numberp (cdr-safe articles))
    (setq articles (list articles)))
  (let (result a i j new-a)
    (while (setq a (pop articles))
      (if (atom a)
	  (setq i a
		j a)
	(setq i (car a)
	      j (cdr a)))
      (while (<= i j)
	;; If this is slow, you can optimize by moving article checking
	;; into here.  You don't have to recompute the group-pos,
	;; nor scan the table every time.
	(when (setq new-a (nnvirtual-reverse-map-article group i))
	  (push new-a result))
	(setq i (1+ i))))
    (nreverse result)))


(defun nnvirtual-partition-sequence (articles)
  "Return an association list of component article numbers.
These are indexed by elements of nnvirtual-component-groups, based on
the sequence ARTICLES of virtual article numbers.  ARTICLES should be
sorted, and can be a compressed sequence.  If any of the article
numbers has no corresponding component article, then it is left out of
the result."
  (when (numberp (cdr-safe articles))
    (setq articles (list articles)))
  (let ((carticles (mapcar 'list nnvirtual-component-groups))
	a i j article entry)
    (while (setq a (pop articles))
      (if (atom a)
	  (setq i a
		j a)
	(setq i (car a)
	      j (cdr a)))
      (while (<= i j)
	(when (setq article (nnvirtual-map-article i))
	  (setq entry (assoc (car article) carticles))
	  (setcdr entry (cons (cdr article) (cdr entry))))
	(setq i (1+ i))))
    (mapc (lambda (x) (setcdr x (nreverse (cdr x))))
	  carticles)
    carticles))


(defun nnvirtual-create-mapping (dont-check)
  "Build the tables necessary to map between component (group, article) to virtual article.
Generate the set of read messages and marks for the virtual group
based on the marks on the component groups."
  (let ((cnt 0)
	(tot 0)
	(M 0)
	(i 0)
	actives all-unreads all-marks
	active min max size unreads marks
	next-M next-tot
	reads beg)
    ;; Ok, we loop over all component groups and collect a lot of
    ;; information:
    ;; Into actives we place (g size max), where size is max-min+1.
    ;; Into all-unreads we put (g unreads).
    ;; Into all-marks we put (g marks).
    ;; We also increment cnt and tot here, and compute M (max of sizes).
    (mapc (lambda (g)
	    (setq active (or (and dont-check
				  (gnus-active g))
			     (gnus-activate-group g))
		  min (car active)
		  max (cdr active))
	    (when (and active (>= max min) (not (zerop max)))
	      ;; store active information
	      (push (list g (- max min -1) max) actives)
	      ;; collect unread/mark info for later
	      (setq unreads (gnus-list-of-unread-articles g))
	      (setq marks (gnus-info-marks (gnus-get-info g)))
	      (when gnus-use-cache
		(push (cons 'cache
			    (gnus-cache-articles-in-group g))
		      marks))
	      (push (cons g unreads) all-unreads)
	      (push (cons g marks) all-marks)
	      ;; count groups, total #articles, and max size
	      (setq size (- max min -1))
	      (setq cnt (1+ cnt)
		    tot (+ tot size)
		    M (max M size))))
	  nnvirtual-component-groups)

    ;; Number of articles in the virtual group.
    (setq nnvirtual-mapping-len tot)


    ;; We want the actives list sorted by size, to build the tables.
    (setq actives (sort actives (lambda (g1 g2) (< (nth 1 g1) (nth 1 g2)))))

    ;; Build the offset table.  Largest sized groups are at the front.
    (setq nnvirtual-mapping-offsets
	  (vconcat
	   (nreverse
	    (mapcar (lambda (entry)
		      (cons (nth 0 entry)
			    (- (nth 2 entry) M)))
		    actives))))

    ;; Build the mapping table.
    (setq nnvirtual-mapping-table nil)
    (setq actives (mapcar (lambda (entry) (nth 1 entry)) actives))
    (while actives
      (setq size (car actives))
      (setq next-M (- M size))
      (setq next-tot (- tot (* cnt size)))
      ;; make current row in table
      (push (vector M next-M cnt tot (- next-tot cnt))
	    nnvirtual-mapping-table)
      ;; update M and tot
      (setq M next-M)
      (setq tot next-tot)
      ;; subtract the current size from all entries.
      (setq actives (mapcar (lambda (x) (- x size)) actives))
      ;; remove anything that went to 0.
      (while (and actives
		  (= (car actives) 0))
	(pop actives)
	(setq cnt (- cnt 1))))


    ;; Now that the mapping tables are generated, we can convert
    ;; and combine the separate component unreads and marks lists
    ;; into single lists of virtual article numbers.
    (setq unreads (apply 'nnvirtual-merge-sorted-lists
			 (mapcar (lambda (x)
				   (nnvirtual-reverse-map-sequence
				    (car x) (cdr x)))
				 all-unreads)))
    (setq marks (mapcar
		 (lambda (type)
		   (cons (cdr type)
			 (gnus-compress-sequence
			  (apply
			   'nnvirtual-merge-sorted-lists
			   (mapcar (lambda (x)
				     (nnvirtual-reverse-map-sequence
				      (car x)
				      (cdr (assq (cdr type) (cdr x)))))
				   all-marks)))))
		 gnus-article-mark-lists))

    ;; Remove any empty marks lists, and store.
    (setq nnvirtual-mapping-marks nil)
    (dolist (mark marks)
      (when (cdr mark)
	(push mark nnvirtual-mapping-marks)))

    ;; We need to convert the unreads to reads.  We compress the
    ;; sequence as we go, otherwise it could be huge.
    (while (and (<= (incf i) nnvirtual-mapping-len)
		unreads)
      (if (= i (car unreads))
	  (setq unreads (cdr unreads))
	;; try to get a range.
	(setq beg i)
	(while (and (<= (incf i) nnvirtual-mapping-len)
		    (not (= i (car unreads)))))
	(setq i (- i 1))
	(if (= i beg)
	    (push i reads)
	  (push (cons beg i) reads))
	))
    (when (<= i nnvirtual-mapping-len)
      (if (= i nnvirtual-mapping-len)
	  (push i reads)
	(push (cons i nnvirtual-mapping-len) reads)))

    ;; Store the reads list for later use.
    (setq nnvirtual-mapping-reads (nreverse reads))

    ;; Throw flag to show we changed the info.
    (setq nnvirtual-info-installed nil)
    ))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
