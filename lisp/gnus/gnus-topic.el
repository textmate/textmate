;;; gnus-topic.el --- a folding minor mode for Gnus group buffers

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Ilja Weis <kult@uni-paderborn.de>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'gnus-util)

(defgroup gnus-topic nil
  "Group topics."
  :group 'gnus-group)

(defvar gnus-topic-mode nil
  "Minor mode for Gnus group buffers.")

(defcustom gnus-topic-mode-hook nil
  "Hook run in topic mode buffers."
  :type 'hook
  :group 'gnus-topic)

(when (featurep 'xemacs)
  (add-hook 'gnus-topic-mode-hook 'gnus-xmas-topic-menu-add))

(defcustom gnus-topic-line-format "%i[ %(%{%n%}%) -- %A ]%v\n"
  "Format of topic lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%i  Indentation based on topic level.
%n  Topic name.
%v  Nothing if the topic is visible, \"...\" otherwise.
%g  Number of groups in the topic.
%a  Number of unread articles in the groups in the topic.
%A  Number of unread articles in the groups in the topic and its subtopics.

General format specifiers can also be used.
See Info node `(gnus)Formatting Variables'."
  :link '(custom-manual "(gnus)Formatting Variables")
  :type 'string
  :group 'gnus-topic)

(defcustom gnus-topic-indent-level 2
  "*How much each subtopic should be indented."
  :type 'integer
  :group 'gnus-topic)

(defcustom gnus-topic-display-empty-topics t
  "*If non-nil, display the topic lines even of topics that have no unread articles."
  :type 'boolean
  :group 'gnus-topic)

;; Internal variables.

(defvar gnus-topic-active-topology nil)
(defvar gnus-topic-active-alist nil)
(defvar gnus-topic-unreads nil)

(defvar gnus-topology-checked-p nil
  "Whether the topology has been checked in this session.")

(defvar gnus-topic-killed-topics nil)
(defvar gnus-topic-inhibit-change-level nil)

(defconst gnus-topic-line-format-alist
  `((?n name ?s)
    (?v visible ?s)
    (?i indentation ?s)
    (?g number-of-groups ?d)
    (?a (gnus-topic-articles-in-topic entries) ?d)
    (?A total-number-of-articles ?d)
    (?l level ?d)))

(defvar gnus-topic-line-format-spec nil)

;;; Utility functions

(defun gnus-group-topic-name ()
  "The name of the topic on the current line."
  (let ((topic (get-text-property (point-at-bol) 'gnus-topic)))
    (and topic (symbol-name topic))))

(defun gnus-group-topic-level ()
  "The level of the topic on the current line."
  (get-text-property (point-at-bol) 'gnus-topic-level))

(defun gnus-group-topic-unread ()
  "The number of unread articles in topic on the current line."
  (get-text-property (point-at-bol) 'gnus-topic-unread))

(defun gnus-topic-unread (topic)
  "Return the number of unread articles in TOPIC."
  (or (cdr (assoc topic gnus-topic-unreads))
      0))

(defun gnus-group-topic-p ()
  "Return non-nil if the current line is a topic."
  (gnus-group-topic-name))

(defun gnus-topic-visible-p ()
  "Return non-nil if the current topic is visible."
  (get-text-property (point-at-bol) 'gnus-topic-visible))

(defun gnus-topic-articles-in-topic (entries)
  (let ((total 0)
	number)
    (while entries
      (when (numberp (setq number (car (pop entries))))
	(incf total number)))
    total))

(defun gnus-group-topic (group)
  "Return the topic GROUP is a member of."
  (let ((alist gnus-topic-alist)
	out)
    (while alist
      (when (member group (cdar alist))
	(setq out (caar alist)
	      alist nil))
      (setq alist (cdr alist)))
    out))

(defun gnus-group-parent-topic (group)
  "Return the topic GROUP is member of by looking at the group buffer."
  (with-current-buffer gnus-group-buffer
    (if (gnus-group-goto-group group)
	(gnus-current-topic)
      (gnus-group-topic group))))

(defun gnus-topic-goto-topic (topic)
  (when topic
    (gnus-goto-char (text-property-any (point-min) (point-max)
				       'gnus-topic (intern topic)))))

(defun gnus-topic-jump-to-topic (topic)
  "Go to TOPIC."
  (interactive
   (list (gnus-completing-read "Go to topic" (gnus-topic-list) t)))
  (let ((buffer-read-only nil))
    (dolist (topic (gnus-current-topics topic))
      (unless (gnus-topic-goto-topic topic)
	(gnus-topic-goto-missing-topic topic)
	(gnus-topic-display-missing-topic topic))))
  (gnus-topic-goto-topic topic))

(defun gnus-current-topic ()
  "Return the name of the current topic."
  (let ((result
	 (or (get-text-property (point) 'gnus-topic)
	     (save-excursion
	       (and (gnus-goto-char (previous-single-property-change
				     (point) 'gnus-topic))
		    (get-text-property (max (1- (point)) (point-min))
				       'gnus-topic))))))
    (when result
      (symbol-name result))))

(defun gnus-current-topics (&optional topic)
  "Return a list of all current topics, lowest in hierarchy first.
If TOPIC, start with that topic."
  (let ((topic (or topic (gnus-current-topic)))
	topics)
    (while topic
      (push topic topics)
      (setq topic (gnus-topic-parent-topic topic)))
    (nreverse topics)))

(defun gnus-group-active-topic-p ()
  "Say whether the current topic comes from the active topics."
  (get-text-property (point-at-bol) 'gnus-active))

(defun gnus-topic-find-groups (topic &optional level all lowest recursive)
  "Return entries for all visible groups in TOPIC.
If RECURSIVE is t, return groups in its subtopics too."
  (let ((groups (cdr (assoc topic gnus-topic-alist)))
	info clevel unread group params visible-groups entry active)
    (setq lowest (or lowest 1))
    (setq level (or level gnus-level-unsubscribed))
    ;; We go through the newsrc to look for matches.
    (while groups
      (when (setq group (pop groups))
	(setq entry (gnus-group-entry group)
	      info (nth 2 entry)
	      params (gnus-info-params info)
	      active (gnus-active group)
	      unread (or (car entry)
			 (and (not (equal group "dummy.group"))
			      active
			      (- (1+ (cdr active)) (car active))))
	      clevel (or (gnus-info-level info)
			 (if (member group gnus-zombie-list)
			     gnus-level-zombie gnus-level-killed))))
      (and
       info				; nil means that the group is dead.
       (<= clevel level)
       (>= clevel lowest)		; Is inside the level we want.
       (or all
	   (if (or (eq unread t)
		   (eq unread nil))
	       gnus-group-list-inactive-groups
	     (> unread 0))
	   (and gnus-list-groups-with-ticked-articles
		(cdr (assq 'tick (gnus-info-marks info))))
	   ;; Has right readedness.
	   ;; Check for permanent visibility.
	   (and gnus-permanently-visible-groups
		(string-match gnus-permanently-visible-groups group))
	   (memq 'visible params)
	   (cdr (assq 'visible params)))
       ;; Add this group to the list of visible groups.
       (push (or entry group) visible-groups)))
    (setq visible-groups (nreverse visible-groups))
    (when recursive
      (if (eq recursive t)
	  (setq recursive (cdr (gnus-topic-find-topology topic))))
      (dolist (topic-topology (cdr recursive))
	(setq visible-groups
	      (nconc visible-groups
		     (gnus-topic-find-groups
		      (caar topic-topology)
		      level all lowest topic-topology)))))
    visible-groups))

(defun gnus-topic-goto-previous-topic (n)
  "Go to the N'th previous topic."
  (interactive "p")
  (gnus-topic-goto-next-topic (- n)))

(defun gnus-topic-goto-next-topic (n)
  "Go to the N'th next topic."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n))
	(topic (gnus-current-topic)))
    (while (and (> n 0)
		(setq topic
		      (if backward
			  (gnus-topic-previous-topic topic)
			(gnus-topic-next-topic topic))))
      (gnus-topic-goto-topic topic)
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more topics"))
    n))

(defun gnus-topic-previous-topic (topic)
  "Return the previous topic on the same level as TOPIC."
  (let ((top (cddr (gnus-topic-find-topology
		    (gnus-topic-parent-topic topic)))))
    (unless (equal topic (caaar top))
      (while (and top (not (equal (caaadr top) topic)))
	(setq top (cdr top)))
      (caaar top))))

(defun gnus-topic-parent-topic (topic &optional topology)
  "Return the parent of TOPIC."
  (unless topology
    (setq topology gnus-topic-topology))
  (let ((parent (car (pop topology)))
	result found)
    (while (and topology
		(not (setq found (equal (caaar topology) topic)))
		(not (setq result (gnus-topic-parent-topic
				   topic (car topology)))))
      (setq topology (cdr topology)))
    (or result (and found parent))))

(defun gnus-topic-next-topic (topic &optional previous)
  "Return the next sibling of TOPIC."
  (let ((parentt (cddr (gnus-topic-find-topology
			(gnus-topic-parent-topic topic))))
	prev)
    (while (and parentt
		(not (equal (caaar parentt) topic)))
      (setq prev (caaar parentt)
	    parentt (cdr parentt)))
    (if previous
	prev
      (caaadr parentt))))

(defun gnus-topic-forward-topic (num)
  "Go to the next topic on the same level as the current one."
  (let* ((topic (gnus-current-topic))
	 (way (if (< num 0) 'gnus-topic-previous-topic
		'gnus-topic-next-topic))
	 (num (abs num)))
    (while (and (not (zerop num))
		(setq topic (funcall way topic)))
      (when (gnus-topic-goto-topic topic)
	(decf num)))
    (unless (zerop num)
      (goto-char (point-max)))
    num))

(defun gnus-topic-find-topology (topic &optional topology level remove)
  "Return the topology of TOPIC."
  (unless topology
    (setq topology gnus-topic-topology)
    (setq level 0))
  (let ((top topology)
	result)
    (if (equal (caar topology) topic)
	(progn
	  (when remove
	    (delq topology remove))
	  (cons level topology))
      (setq topology (cdr topology))
      (while (and topology
		  (not (setq result (gnus-topic-find-topology
				     topic (car topology) (1+ level)
				     (and remove top)))))
	(setq topology (cdr topology)))
      result)))

(defvar gnus-tmp-topics nil)
(defun gnus-topic-list (&optional topology)
  "Return a list of all topics in the topology."
  (unless topology
    (setq topology gnus-topic-topology
	  gnus-tmp-topics nil))
  (push (caar topology) gnus-tmp-topics)
  (mapc 'gnus-topic-list (cdr topology))
  gnus-tmp-topics)

;;; Topic parameter jazz

(defun gnus-topic-parameters (topic)
  "Return the parameters for TOPIC."
  (let ((top (gnus-topic-find-topology topic)))
    (when top
      (nth 3 (cadr top)))))

(defun gnus-topic-set-parameters (topic parameters)
  "Set the topic parameters of TOPIC to PARAMETERS."
  (let ((top (gnus-topic-find-topology topic)))
    (unless top
      (error "No such topic: %s" topic))
    ;; We may have to extend if there is no parameters here
    ;; to begin with.
    (unless (nthcdr 2 (cadr top))
      (nconc (cadr top) (list nil)))
    (unless (nthcdr 3 (cadr top))
      (nconc (cadr top) (list nil)))
    (setcar (nthcdr 3 (cadr top)) parameters)
    (gnus-dribble-enter
     (format "(gnus-topic-set-parameters %S '%S)" topic parameters))))

(defun gnus-group-topic-parameters (group)
  "Compute the group parameters for GROUP in topic mode.
Possibly inherit parameters from topics above GROUP."
  (let ((params-list (copy-sequence (gnus-group-get-parameter group))))
    (save-excursion
      (gnus-topic-hierarchical-parameters
       ;; First we try to go to the group within the group buffer and find the
       ;; topic for the group that way. This hopefully copes well with groups
       ;; that are in more than one topic. Failing that (i.e. when the group
       ;; isn't visible in the group buffer) we find a topic for the group via
       ;; gnus-group-topic.
       (or (and (gnus-group-goto-group group)
		(gnus-current-topic))
	   (gnus-group-topic group))
       params-list))))

(defun gnus-topic-hierarchical-parameters (topic &optional group-params-list)
  "Compute the topic parameters for TOPIC.
Possibly inherit parameters from topics above TOPIC.
If optional argument GROUP-PARAMS-LIST is non-nil, use it as the basis for
inheritance."
  (let ((params-list
	 ;; We probably have lots of nil elements here, so we remove them.
	 ;; Probably faster than doing this "properly".
	 (delq nil (cons group-params-list
			 (mapcar 'gnus-topic-parameters
				 (gnus-current-topics topic)))))
	param out params)
    ;; Now we have all the parameters, so we go through them
    ;; and do inheritance in the obvious way.
    (let (posting-style)
      (while (setq params (pop params-list))
	(while (setq param (pop params))
	  (when (atom param)
	    (setq param (cons param t)))
	  (cond ((eq (car param) 'posting-style)
		 (let ((param (cdr param))
		       elt)
		   (while (setq elt (pop param))
		     (unless (assoc (car elt) posting-style)
		       (push elt posting-style)))))
		(t
		 (unless (assq (car param) out)
		   (push param out))))))
      (and posting-style (push (cons 'posting-style posting-style) out)))
    ;; Return the resulting parameter list.
    out))

;;; General utility functions

(defun gnus-topic-enter-dribble ()
  (gnus-dribble-enter
   (format "(setq gnus-topic-topology '%S)" gnus-topic-topology)))

;;; Generating group buffers

(defun gnus-group-prepare-topics (level &optional predicate lowest
					regexp list-topic topic-level)
  "List all newsgroups with unread articles of level LEVEL or lower.
Use the `gnus-group-topics' to sort the groups.
If PREDICATE is a function, list groups that the function returns non-nil;
if it is t, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(lowest (or lowest 1))
	(not-in-list
	 (and gnus-group-listed-groups
	      (copy-sequence gnus-group-listed-groups))))

    (gnus-update-format-specifications nil 'topic)

    (when (or (not gnus-topic-alist)
	      (not gnus-topology-checked-p))
      (gnus-topic-check-topology))

    (unless list-topic
      (erase-buffer))

    ;; List dead groups?
    (when (or gnus-group-listed-groups
	      (and (>= level gnus-level-zombie)
		   (<= lowest gnus-level-zombie)))
      (gnus-group-prepare-flat-list-dead
       (setq gnus-zombie-list (sort gnus-zombie-list 'string<))
       gnus-level-zombie ?Z
       regexp))

    (when (or gnus-group-listed-groups
	       (and (>= level gnus-level-killed)
		    (<= lowest gnus-level-killed)))
      (gnus-group-prepare-flat-list-dead
       (setq gnus-killed-list (sort gnus-killed-list 'string<))
       gnus-level-killed ?K regexp)
      (when not-in-list
	(unless gnus-killed-hashtb
	  (gnus-make-hashtable-from-killed))
	(gnus-group-prepare-flat-list-dead
	 (gnus-remove-if (lambda (group)
			   (or (gnus-group-entry group)
			       (gnus-gethash group gnus-killed-hashtb)))
			 not-in-list)
	 gnus-level-killed ?K regexp)))

    ;; Use topics.
    (prog1
	(when (or (< lowest gnus-level-zombie)
		  gnus-group-listed-groups)
	  (if list-topic
	      (let ((top (gnus-topic-find-topology list-topic)))
		(gnus-topic-prepare-topic (cdr top) (car top)
					  (or topic-level level) predicate
					  nil lowest regexp))
	    (gnus-topic-prepare-topic gnus-topic-topology 0
				      (or topic-level level) predicate
				      nil lowest regexp)))
      (gnus-group-set-mode-line)
      (setq gnus-group-list-mode (cons level predicate))
      (gnus-run-hooks 'gnus-group-prepare-hook))))

(defun gnus-topic-prepare-topic (topicl level &optional list-level
					predicate silent
					lowest regexp)
  "Insert TOPIC into the group buffer.
If SILENT, don't insert anything.  Return the number of unread
articles in the topic and its subtopics."
  (let* ((type (pop topicl))
	 (entries (gnus-topic-find-groups
		   (car type)
		   (if gnus-group-listed-groups
		       gnus-level-killed
		     list-level)
		   (or predicate gnus-group-listed-groups
		       (cdr (assq 'visible
				  (gnus-topic-hierarchical-parameters
				   (car type)))))
		   (if gnus-group-listed-groups 0 lowest)))
	 (visiblep (and (eq (nth 1 type) 'visible) (not silent)))
	 (gnus-group-indentation
	  (make-string (* gnus-topic-indent-level level) ? ))
	 (beg (progn (beginning-of-line) (point)))
	 (topicl (reverse topicl))
	 (all-entries entries)
	 (point-max (point-max))
	 (unread 0)
	 (topic (car type))
	 info entry end active tick)
    ;; Insert any sub-topics.
    (while topicl
      (incf unread
	    (gnus-topic-prepare-topic
	     (pop topicl) (1+ level) list-level predicate
	     (not visiblep) lowest regexp)))
    (setq end (point))
    (goto-char beg)
    ;; Insert all the groups that belong in this topic.
    (while (setq entry (pop entries))
      (when (if (stringp entry)
		(gnus-group-prepare-logic
		 entry
		 (and
		  (or (not gnus-group-listed-groups)
		      (if (< list-level gnus-level-zombie) nil
			(let ((entry-level
			       (if (member entry gnus-zombie-list)
				   gnus-level-zombie gnus-level-killed)))
			  (and (<= entry-level list-level)
			       (>= entry-level lowest)))))
		  (cond
		   ((stringp regexp)
		    (string-match regexp entry))
		   ((functionp regexp)
		    (funcall regexp entry))
		   ((null regexp) t)
		   (t nil))))
	      (setq info (nth 2 entry))
	      (gnus-group-prepare-logic
	       (gnus-info-group info)
	       (and (or (not gnus-group-listed-groups)
			(let ((entry-level (gnus-info-level info)))
			  (and (<= entry-level list-level)
			       (>= entry-level lowest))))
		    (or (not (functionp predicate))
			(funcall predicate info))
		    (or (not (stringp regexp))
			(string-match regexp (gnus-info-group info))))))
	(when visiblep
	  (if (stringp entry)
	      ;; Dead groups.
	      (gnus-group-insert-group-line
	       entry (if (member entry gnus-zombie-list)
			 gnus-level-zombie gnus-level-killed)
	       nil (- (1+ (cdr (setq active (gnus-active entry))))
		      (car active))
	       nil)
	    ;; Living groups.
	    (when (setq info (nth 2 entry))
	      (gnus-group-insert-group-line
	       (gnus-info-group info)
	       (gnus-info-level info) (gnus-info-marks info)
	       (car entry) (gnus-info-method info)))))
	(when (and (listp entry)
		   (numberp (car entry)))
	  (incf unread (car entry)))
	(when (listp entry)
	  (setq tick t))))
    (goto-char beg)
    ;; Insert the topic line.
    (when (and (not silent)
	       (or gnus-topic-display-empty-topics ;We want empty topics
		   (not (zerop unread))	;Non-empty
		   tick			;Ticked articles
		   (/= point-max (point-max)))) ;Inactive groups
      (gnus-extent-start-open (point))
      (gnus-topic-insert-topic-line
       (car type) visiblep
       (not (eq (nth 2 type) 'hidden))
       level all-entries unread))
    (gnus-topic-update-unreads (car type) unread)
    (when gnus-group-update-tool-bar
      (gnus-put-text-property beg end 'point-entered
			      'gnus-tool-bar-update)
      (gnus-put-text-property beg end 'point-left
			      'gnus-tool-bar-update))
    (goto-char end)
    unread))

(defun gnus-topic-remove-topic (&optional insert total-remove hide in-level)
  "Remove the current topic."
  (let ((topic (gnus-group-topic-name))
	(level (gnus-group-topic-level))
	(beg (progn (beginning-of-line) (point)))
	buffer-read-only)
    (when topic
      (while (and (zerop (forward-line 1))
		  (> (or (gnus-group-topic-level) (1+ level)) level)))
      (delete-region beg (point))
      ;; Do the change in this rather odd manner because it has been
      ;; reported that some topics share parts of some lists, for some
      ;; reason.  I have been unable to determine why this is the
      ;; case, but this hack seems to take care of things.
      (let ((data (cadr (gnus-topic-find-topology topic))))
	(setcdr data
		(list (if insert 'visible 'invisible)
		      (caddr data)
		      (cadddr data))))
      (if total-remove
	  (setq gnus-topic-alist
		(delq (assoc topic gnus-topic-alist) gnus-topic-alist))
	(gnus-topic-insert-topic topic in-level)))))

(defun gnus-topic-insert-topic (topic &optional level)
  "Insert TOPIC."
  (gnus-group-prepare-topics
   (car gnus-group-list-mode) (cdr gnus-group-list-mode)
   nil nil topic level))

(defun gnus-topic-fold (&optional insert topic)
  "Remove/insert the current topic."
  (let ((topic (or topic (gnus-group-topic-name))))
    (when topic
      (save-excursion
	(if (not (gnus-group-active-topic-p))
	    (gnus-topic-remove-topic
	     (or insert (not (gnus-topic-visible-p))))
	  (let ((gnus-topic-topology gnus-topic-active-topology)
		(gnus-topic-alist gnus-topic-active-alist)
		(gnus-group-list-mode (cons 5 t)))
	    (gnus-topic-remove-topic
	     (or insert (not (gnus-topic-visible-p))) nil nil 9)
	    (gnus-topic-enter-dribble)))))))

(defun gnus-topic-insert-topic-line (name visiblep shownp level entries
					  &optional unread)
  (let* ((visible (if visiblep "" "..."))
	 (indentation (make-string (* gnus-topic-indent-level level) ? ))
	 (total-number-of-articles unread)
	 (number-of-groups (length entries))
	 (active-topic (eq gnus-topic-alist gnus-topic-active-alist))
	 gnus-tmp-header)
    (gnus-topic-update-unreads name unread)
    (beginning-of-line)
    ;; Insert the text.
    (if shownp
	(gnus-add-text-properties
	 (point)
	 (prog1 (1+ (point))
	   (eval gnus-topic-line-format-spec))
	 (list 'gnus-topic (intern name)
	       'gnus-topic-level level
	       'gnus-topic-unread unread
	       'gnus-active active-topic
	       'gnus-topic-visible visiblep)))))

(defun gnus-topic-update-unreads (topic unreads)
  (setq gnus-topic-unreads (delq (assoc topic gnus-topic-unreads)
				 gnus-topic-unreads))
  (push (cons topic unreads) gnus-topic-unreads))

(defun gnus-topic-update-topics-containing-group (group)
  "Update all topics that have GROUP as a member."
  (when (and (eq major-mode 'gnus-group-mode)
	     gnus-topic-mode)
    (save-excursion
      (let ((alist gnus-topic-alist))
	;; This is probably not entirely correct.  If a topic
	;; isn't shown, then it's not updated.  But the updating
	;; should be performed in any case, since the topic's
	;; parent should be updated.  Pfft.
	(while alist
	  (when (and (member group (cdar alist))
		     (gnus-topic-goto-topic (caar alist)))
	    (gnus-topic-update-topic-line (caar alist)))
	  (pop alist))))))

(defun gnus-topic-update-topic ()
  "Update all parent topics to the current group."
  (when (and (eq major-mode 'gnus-group-mode)
	     gnus-topic-mode)
    (let ((group (gnus-group-group-name))
	  (m (point-marker))
	  (buffer-read-only nil))
      (when (and group
		 (gnus-get-info group)
		 (gnus-topic-goto-topic (gnus-current-topic)))
	(gnus-topic-update-topic-line (gnus-group-topic-name))
	(goto-char m)
	(set-marker m nil)
	(gnus-group-position-point)))))

(defun gnus-topic-goto-missing-group (group)
  "Place point where GROUP is supposed to be inserted."
  (let* ((topic (gnus-group-topic group))
	 (groups (cdr (assoc topic gnus-topic-alist)))
	 (g (cdr (member group groups)))
	 (unfound t)
	 entry)
    ;; Try to jump to a visible group.
    (while (and g
		(not (gnus-group-goto-group (car g) t)))
      (pop g))
    ;; It wasn't visible, so we try to see where to insert it.
    (when (not g)
      (setq g (cdr (member group (reverse groups))))
      (while (and g unfound)
	(when (gnus-group-goto-group (pop g) t)
	  (forward-line 1)
	  (setq unfound nil)))
      (when (and unfound
		 topic
		 (not (gnus-topic-goto-missing-topic topic)))
	(gnus-topic-display-missing-topic topic)))))

(defun gnus-topic-display-missing-topic (topic)
  "Insert topic lines recursively for missing topics."
  (let ((parent (gnus-topic-find-topology
		 (gnus-topic-parent-topic topic))))
    (when (and parent
	       (not (gnus-topic-goto-missing-topic (caadr parent))))
      (gnus-topic-display-missing-topic (caadr parent))))
  (gnus-topic-goto-missing-topic topic)
  ;; Skip past all groups in the topic we're in.
  (while (gnus-group-group-name)
    (forward-line 1))
  (let* ((top (gnus-topic-find-topology topic))
	 (children (cddr top))
	 (type (cadr top))
	 (unread 0)
	 (entries (gnus-topic-find-groups
		   (car type) (car gnus-group-list-mode)
		   (cdr gnus-group-list-mode)))
	entry)
    (while children
      (incf unread (gnus-topic-unread (caar (pop children)))))
    (while (setq entry (pop entries))
      (when (numberp (car entry))
	(incf unread (car entry))))
    (gnus-topic-insert-topic-line
     topic t t (car (gnus-topic-find-topology topic)) nil unread)))

(defun gnus-topic-goto-missing-topic (topic)
  (if (gnus-topic-goto-topic topic)
      (forward-line 1)
    ;; Topic not displayed.
    (let* ((top (gnus-topic-find-topology
		 (gnus-topic-parent-topic topic)))
	   (tp (reverse (cddr top))))
      (if (not top)
	  (gnus-topic-insert-topic-line
	   topic t t (car (gnus-topic-find-topology topic)) nil 0)
	(while (not (equal (caaar tp) topic))
	  (setq tp (cdr tp)))
	(pop tp)
	(while (and tp
		    (not (gnus-topic-goto-topic (caaar tp))))
	  (pop tp))
	(if tp
	    (gnus-topic-forward-topic 1)
	  (gnus-topic-goto-missing-topic (caadr top)))))
    nil))

(defun gnus-topic-update-topic-line (topic-name &optional reads)
  (let* ((top (gnus-topic-find-topology topic-name))
	 (type (cadr top))
	 (children (cddr top))
	 (entries (gnus-topic-find-groups
		   (car type) (car gnus-group-list-mode)
		   (cdr gnus-group-list-mode)))
	 (parent (gnus-topic-parent-topic topic-name))
	 (all-entries entries)
	 (unread 0)
	 old-unread entry new-unread)
    (when (gnus-topic-goto-topic (car type))
      ;; Tally all the groups that belong in this topic.
      (if reads
	  (setq unread (- (gnus-group-topic-unread) reads))
	(while children
	  (incf unread (gnus-topic-unread (caar (pop children)))))
	(while (setq entry (pop entries))
	  (when (numberp (car entry))
	    (incf unread (car entry)))))
      (setq old-unread (gnus-group-topic-unread))
      ;; Insert the topic line.
      (gnus-topic-insert-topic-line
       (car type) (gnus-topic-visible-p)
       (not (eq (nth 2 type) 'hidden))
       (gnus-group-topic-level) all-entries unread)
      (gnus-delete-line)
      (forward-line -1)
      (setq new-unread (gnus-group-topic-unread)))
    (when parent
      (forward-line -1)
      (gnus-topic-update-topic-line
       parent
       (- (or old-unread 0) (or new-unread 0))))
    unread))

(defun gnus-topic-group-indentation ()
  (make-string
   (* gnus-topic-indent-level
      (or (save-excursion
	    (forward-line -1)
	    (gnus-topic-goto-topic (gnus-current-topic))
	    (gnus-group-topic-level))
	  0))
   ? ))

;;; Initialization

(gnus-add-shutdown 'gnus-topic-close 'gnus)

(defun gnus-topic-close ()
  (setq gnus-topic-active-topology nil
	gnus-topic-active-alist nil
	gnus-topic-killed-topics nil
	gnus-topology-checked-p nil))

(defun gnus-topic-check-topology ()
  ;; The first time we set the topology to whatever we have
  ;; gotten here, which can be rather random.
  (unless gnus-topic-alist
    (gnus-topic-init-alist))

  (setq gnus-topology-checked-p t)
  ;; Go through the topic alist and make sure that all topics
  ;; are in the topic topology.
  (let ((topics (gnus-topic-list))
	(alist gnus-topic-alist)
	changed)
    (while alist
      (unless (member (caar alist) topics)
	(nconc gnus-topic-topology
	       (list (list (list (caar alist) 'visible))))
	(setq changed t))
      (setq alist (cdr alist)))
    (when changed
      (gnus-topic-enter-dribble))
    ;; Conversely, go through the topology and make sure that all
    ;; topologies have alists.
    (while topics
      (unless (assoc (car topics) gnus-topic-alist)
	(push (list (car topics)) gnus-topic-alist))
      (pop topics)))
  ;; Go through all living groups and make sure that
  ;; they belong to some topic.
  (let* ((tgroups (apply 'append (mapcar 'cdr gnus-topic-alist)))
	 (entry (last (assoc (caar gnus-topic-topology) gnus-topic-alist)))
	 (newsrc (cdr gnus-newsrc-alist))
	 group)
    (while newsrc
      (unless (member (setq group (gnus-info-group (pop newsrc))) tgroups)
	(setcdr entry (list group))
	(setq entry (cdr entry)))))
  ;; Go through all topics and make sure they contain only living groups.
  (let ((alist gnus-topic-alist)
	topic)
    (while (setq topic (pop alist))
      (while (cdr topic)
	(if (and (cadr topic)
		 (gnus-group-entry (cadr topic)))
	    (setq topic (cdr topic))
	  (setcdr topic (cddr topic)))))))

(defun gnus-topic-init-alist ()
  "Initialize the topic structures."
  (setq gnus-topic-topology
	(cons (list "Gnus" 'visible)
	      (mapcar (lambda (topic)
			(list (list (car topic) 'visible)))
		      '(("misc")))))
  (setq gnus-topic-alist
	(list (cons "misc"
		    (mapcar (lambda (info) (gnus-info-group info))
			    (cdr gnus-newsrc-alist)))
	      (list "Gnus")))
  (gnus-topic-enter-dribble))

;;; Maintenance

(defun gnus-topic-clean-alist ()
  "Remove bogus groups from the topic alist."
  (let ((topic-alist gnus-topic-alist)
	result topic)
    (unless gnus-killed-hashtb
      (gnus-make-hashtable-from-killed))
    (while (setq topic (pop topic-alist))
      (let ((topic-name (pop topic))
	    group filtered-topic)
	(while (setq group (pop topic))
	  (when (and (or (gnus-active group)
			 (gnus-info-method (gnus-get-info group)))
		     (not (gnus-gethash group gnus-killed-hashtb)))
	    (push group filtered-topic)))
	(push (cons topic-name (nreverse filtered-topic)) result)))
    (setq gnus-topic-alist (nreverse result))))

(defun gnus-topic-change-level (group level oldlevel &optional previous)
  "Run when changing levels to enter/remove groups from topics."
  (with-current-buffer gnus-group-buffer
    (let ((buffer-read-only nil))
      (unless gnus-topic-inhibit-change-level
	(gnus-group-goto-group (or (car (nth 2 previous)) group))
	(when (and gnus-topic-mode
		   gnus-topic-alist
		   (not gnus-topic-inhibit-change-level))
	  ;; Remove the group from the topics.
	  (if (and (< oldlevel gnus-level-zombie)
		   (>= level gnus-level-zombie))
	      (let ((alist gnus-topic-alist))
		(while (gnus-group-goto-group group)
		  (gnus-delete-line))
		(while alist
		  (when (member group (car alist))
		    (setcdr (car alist) (delete group (cdar alist))))
		  (pop alist)))
	    ;; If the group is subscribed we enter it into the topics.
	    (when (and (< level gnus-level-zombie)
		       (>= oldlevel gnus-level-zombie))
	      (let* ((prev (gnus-group-group-name))
		     (gnus-topic-inhibit-change-level t)
		     (gnus-group-indentation
		      (make-string
		       (* gnus-topic-indent-level
			  (or (save-excursion
				(gnus-topic-goto-topic (gnus-current-topic))
				(gnus-group-topic-level))
			      0))
		       ? ))
		     (yanked (list group))
		     alist talist end)
		;; Then we enter the yanked groups into the topics
		;; they belong to.
		(when (setq alist (assoc (save-excursion
					   (forward-line -1)
					   (or
					    (gnus-current-topic)
					    (caar gnus-topic-topology)))
					 gnus-topic-alist))
		  (setq talist alist)
		  (when (stringp yanked)
		    (setq yanked (list yanked)))
		  (if (not prev)
		      (nconc alist yanked)
		    (if (not (cdr alist))
			(setcdr alist (nconc yanked (cdr alist)))
		      (while (and (not end) (cdr alist))
			(when (equal (cadr alist) prev)
			  (setcdr alist (nconc yanked (cdr alist)))
			  (setq end t))
			(setq alist (cdr alist)))
		      (unless end
			(nconc talist yanked))))))
	      (gnus-topic-update-topic))))))))

(defun gnus-topic-goto-next-group (group props)
  "Go to group or the next group after group."
  (if (not group)
      (if (not (memq 'gnus-topic props))
	  (goto-char (point-max))
	(let ((topic (symbol-name (cadr (memq 'gnus-topic props)))))
	  (or (gnus-topic-goto-topic topic)
	      (gnus-topic-goto-topic (gnus-topic-next-topic topic)))))
    (if (gnus-group-goto-group group)
	t
      ;; The group is no longer visible.
      (let* ((list (assoc (gnus-group-topic group) gnus-topic-alist))
	     (topic-visible (save-excursion (gnus-topic-goto-topic (car list))))
	     (after (and topic-visible (cdr (member group (cdr list))))))
	;; First try to put point on a group after the current one.
	(while (and after
		    (not (gnus-group-goto-group (car after))))
	  (setq after (cdr after)))
	;; Then try to put point on a group before point.
	(unless after
	  (setq after (cdr (member group (reverse (cdr list)))))
	  (while (and after
		      (not (gnus-group-goto-group (car after))))
	    (setq after (cdr after))))
	;; Finally, just put point on the topic.
	(if (not (car list))
	    (goto-char (point-min))
	  (unless after
	    (if topic-visible
		(gnus-goto-char topic-visible)
	      (gnus-topic-goto-topic (gnus-topic-next-topic (car list))))
	    (setq after nil)))
	t))))

;;; Topic-active functions

(defun gnus-topic-grok-active (&optional force)
  "Parse all active groups and create topic structures for them."
  ;; First we make sure that we have really read the active file.
  (when (or force
	    (not gnus-topic-active-alist))
    (let (groups)
      ;; Get a list of all groups available.
      (mapatoms (lambda (g) (when (symbol-value g)
			      (push (symbol-name g) groups)))
		gnus-active-hashtb)
      (setq groups (sort groups 'string<))
      ;; Init the variables.
      (setq gnus-topic-active-topology (list (list "" 'visible)))
      (setq gnus-topic-active-alist nil)
      ;; Descend the top-level hierarchy.
      (gnus-topic-grok-active-1 gnus-topic-active-topology groups)
      ;; Set the top-level topic names to something nice.
      (setcar (car gnus-topic-active-topology) "Gnus active")
      (setcar (car gnus-topic-active-alist) "Gnus active"))))

(defun gnus-topic-grok-active-1 (topology groups)
  (let* ((name (caar topology))
	 (prefix (concat "^" (regexp-quote name)))
	 tgroups ntopology group)
    (while (and groups
		(string-match prefix (setq group (car groups))))
      (if (not (string-match "\\." group (match-end 0)))
	  ;; There are no further hierarchies here, so we just
	  ;; enter this group into the list belonging to this
	  ;; topic.
	  (push (pop groups) tgroups)
	;; New sub-hierarchy, so we add it to the topology.
	(nconc topology (list (setq ntopology
				    (list (list (substring
						 group 0 (match-end 0))
						'invisible)))))
	;; Descend the hierarchy.
	(setq groups (gnus-topic-grok-active-1 ntopology groups))))
    ;; We remove the trailing "." from the topic name.
    (setq name
	  (if (string-match "\\.$" name)
	      (substring name 0 (match-beginning 0))
	    name))
    ;; Add this topic and its groups to the topic alist.
    (push (cons name (nreverse tgroups)) gnus-topic-active-alist)
    (setcar (car topology) name)
    ;; We return the rest of the groups that didn't belong
    ;; to this topic.
    groups))

;;; Topic mode, commands and keymap.

(defvar gnus-topic-mode-map nil)
(defvar gnus-group-topic-map nil)

(unless gnus-topic-mode-map
  (setq gnus-topic-mode-map (make-sparse-keymap))

  ;; Override certain group mode keys.
  (gnus-define-keys gnus-topic-mode-map
    "=" gnus-topic-select-group
    "\r" gnus-topic-select-group
    " " gnus-topic-read-group
    "\C-c\C-x" gnus-topic-expire-articles
    "c" gnus-topic-catchup-articles
    "\C-k" gnus-topic-kill-group
    "\C-y" gnus-topic-yank-group
    "\M-g" gnus-topic-get-new-news-this-topic
    "AT" gnus-topic-list-active
    "Gp" gnus-topic-edit-parameters
    "#" gnus-topic-mark-topic
    "\M-#" gnus-topic-unmark-topic
    [tab] gnus-topic-indent
    [(meta tab)] gnus-topic-unindent
    "\C-i" gnus-topic-indent
    "\M-\C-i" gnus-topic-unindent
    gnus-mouse-2 gnus-mouse-pick-topic)

  ;; Define a new submap.
  (gnus-define-keys (gnus-group-topic-map "T" gnus-group-mode-map)
    "#" gnus-topic-mark-topic
    "\M-#" gnus-topic-unmark-topic
    "n" gnus-topic-create-topic
    "m" gnus-topic-move-group
    "D" gnus-topic-remove-group
    "c" gnus-topic-copy-group
    "h" gnus-topic-hide-topic
    "s" gnus-topic-show-topic
    "j" gnus-topic-jump-to-topic
    "M" gnus-topic-move-matching
    "C" gnus-topic-copy-matching
    "\M-p" gnus-topic-goto-previous-topic
    "\M-n" gnus-topic-goto-next-topic
    "\C-i" gnus-topic-indent
    [tab] gnus-topic-indent
    "r" gnus-topic-rename
    "\177" gnus-topic-delete
    [delete] gnus-topic-delete
    "H" gnus-topic-toggle-display-empty-topics)

  (gnus-define-keys (gnus-topic-sort-map "S" gnus-group-topic-map)
    "s" gnus-topic-sort-groups
    "a" gnus-topic-sort-groups-by-alphabet
    "u" gnus-topic-sort-groups-by-unread
    "l" gnus-topic-sort-groups-by-level
    "e" gnus-topic-sort-groups-by-server
    "v" gnus-topic-sort-groups-by-score
    "r" gnus-topic-sort-groups-by-rank
    "m" gnus-topic-sort-groups-by-method))

(defun gnus-topic-make-menu-bar ()
  (unless (boundp 'gnus-topic-menu)
    (easy-menu-define
     gnus-topic-menu gnus-topic-mode-map ""
     '("Topics"
       ["Toggle topics" gnus-topic-mode t]
       ("Groups"
	["Copy..." gnus-topic-copy-group t]
	["Move..." gnus-topic-move-group t]
	["Remove" gnus-topic-remove-group t]
	["Copy matching..." gnus-topic-copy-matching t]
	["Move matching..." gnus-topic-move-matching t])
       ("Topics"
	["Goto..." gnus-topic-jump-to-topic t]
	["Show" gnus-topic-show-topic t]
	["Hide" gnus-topic-hide-topic t]
	["Delete" gnus-topic-delete t]
	["Rename..." gnus-topic-rename t]
	["Create..." gnus-topic-create-topic t]
	["Mark" gnus-topic-mark-topic t]
	["Indent" gnus-topic-indent t]
	["Sort" gnus-topic-sort-topics t]
	["Previous topic" gnus-topic-goto-previous-topic t]
	["Next topic" gnus-topic-goto-next-topic t]
	["Toggle hide empty" gnus-topic-toggle-display-empty-topics t]
	["Edit parameters" gnus-topic-edit-parameters t])
       ["List active" gnus-topic-list-active t]))))

(defun gnus-topic-mode (&optional arg redisplay)
  "Minor mode for topicsifying Gnus group buffers."
  ;; FIXME: Use define-minor-mode.
  (interactive (list current-prefix-arg t))
  (when (eq major-mode 'gnus-group-mode)
    (make-local-variable 'gnus-topic-mode)
    (setq gnus-topic-mode
	  (if (null arg) (not gnus-topic-mode)
	    (> (prefix-numeric-value arg) 0)))
    ;; Infest Gnus with topics.
    (if (not gnus-topic-mode)
	(setq gnus-goto-missing-group-function nil)
      (when (gnus-visual-p 'topic-menu 'menu)
	(gnus-topic-make-menu-bar))
      (gnus-set-format 'topic t)
      (add-minor-mode 'gnus-topic-mode " Topic" gnus-topic-mode-map)
      (add-hook 'gnus-group-catchup-group-hook 'gnus-topic-update-topic)
      (set (make-local-variable 'gnus-group-prepare-function)
	   'gnus-group-prepare-topics)
      (set (make-local-variable 'gnus-group-get-parameter-function)
	   'gnus-group-topic-parameters)
      (set (make-local-variable 'gnus-group-goto-next-group-function)
	   'gnus-topic-goto-next-group)
      (set (make-local-variable 'gnus-group-indentation-function)
	   'gnus-topic-group-indentation)
      (set (make-local-variable 'gnus-group-update-group-function)
	   'gnus-topic-update-topics-containing-group)
      (set (make-local-variable 'gnus-group-sort-alist-function)
	   'gnus-group-sort-topic)
      (setq gnus-group-change-level-function 'gnus-topic-change-level)
      (setq gnus-goto-missing-group-function 'gnus-topic-goto-missing-group)
      (gnus-make-local-hook 'gnus-check-bogus-groups-hook)
      (add-hook 'gnus-check-bogus-groups-hook 'gnus-topic-clean-alist
		nil 'local)
      (setq gnus-topology-checked-p nil)
      ;; We check the topology.
      (when gnus-newsrc-alist
	(gnus-topic-check-topology))
      (gnus-run-hooks 'gnus-topic-mode-hook))
    ;; Remove topic infestation.
    (unless gnus-topic-mode
      (remove-hook 'gnus-summary-exit-hook 'gnus-topic-update-topic)
      (setq gnus-group-change-level-function nil)
      (remove-hook 'gnus-check-bogus-groups-hook 'gnus-topic-clean-alist)
      (setq gnus-group-prepare-function 'gnus-group-prepare-flat)
      (setq gnus-group-sort-alist-function 'gnus-group-sort-flat))
    (when redisplay
      (gnus-group-list-groups))))

(defun gnus-topic-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If the group is opened, just switch the summary buffer.
If ALL is non-nil, already read articles become readable.

If ALL is a positive number, fetch this number of the latest
articles in the group.  If ALL is a negative number, fetch this
number of the earliest articles in the group.

If performed over a topic line, toggle folding the topic."
  (interactive "P")
  (when (and (eobp) (not (gnus-group-group-name)))
    (forward-line -1))
  (if (gnus-group-topic-p)
      (let ((gnus-group-list-mode
	     (if all (cons (if (numberp all) all 7) t) gnus-group-list-mode)))
	(gnus-topic-fold all)
	(gnus-dribble-touch))
    (gnus-group-select-group all)))

(defun gnus-mouse-pick-topic (e)
  "Select the group or topic under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (gnus-topic-read-group nil))

(defun gnus-topic-expire-articles (topic)
  "Expire articles in this topic or group."
  (interactive (list (gnus-group-topic-name)))
  (if (not topic)
      (call-interactively 'gnus-group-expire-articles)
    (save-excursion
      (gnus-message 5 "Expiring groups in %s..." topic)
      (let ((gnus-group-marked
	     (mapcar (lambda (entry) (car (nth 2 entry)))
		     (gnus-topic-find-groups topic gnus-level-killed t
					     nil t))))
	(gnus-group-expire-articles nil))
      (gnus-message 5 "Expiring groups in %s...done" topic))))

(defun gnus-topic-catchup-articles (topic)
  "Catchup this topic or group.
Also see `gnus-group-catchup'."
  (interactive (list (gnus-group-topic-name)))
  (if (not topic)
      (call-interactively 'gnus-group-catchup-current)
    (save-excursion
      (let* ((groups
	     (mapcar (lambda (entry) (car (nth 2 entry)))
		     (gnus-topic-find-groups topic gnus-level-killed t
					     nil t)))
	     (buffer-read-only nil)
	     (gnus-group-marked groups))
	(gnus-group-catchup-current)
	(mapcar 'gnus-topic-update-topics-containing-group groups)))))

(defun gnus-topic-read-group (&optional all no-article group)
  "Read news in this newsgroup.
If the prefix argument ALL is non-nil, already read articles become
readable.

If ALL is a positive number, fetch this number of the latest
articles in the group.  If ALL is a negative number, fetch this
number of the earliest articles in the group.

If the optional argument NO-ARTICLE is non-nil, no article will
be auto-selected upon group entry.  If GROUP is non-nil, fetch
that group.

If performed over a topic line, toggle folding the topic."
  (interactive "P")
  (when (and (eobp) (not (gnus-group-group-name)))
    (forward-line -1))
  (if (gnus-group-topic-p)
      (let ((gnus-group-list-mode
	     (if all (cons (if (numberp all) all 7) t) gnus-group-list-mode)))
	(gnus-topic-fold all))
    (gnus-group-read-group all no-article group)))

(defun gnus-topic-create-topic (topic parent &optional previous full-topic)
  "Create a new TOPIC under PARENT.
When used interactively, PARENT will be the topic under point."
  (interactive
   (list
    (read-string "New topic: ")
    (gnus-current-topic)))
  ;; Check whether this topic already exists.
  (when (gnus-topic-find-topology topic)
    (error "Topic already exists"))
  (unless parent
    (setq parent (caar gnus-topic-topology)))
  (let ((top (cdr (gnus-topic-find-topology parent)))
	(full-topic (or full-topic (list (list topic 'visible nil nil)))))
    (unless top
      (error "No such parent topic: %s" parent))
    (if previous
	(progn
	  (while (and (cdr top)
		      (not (equal (caaadr top) previous)))
	    (setq top (cdr top)))
	  (setcdr top (cons full-topic (cdr top))))
      (nconc top (list full-topic)))
    (unless (assoc topic gnus-topic-alist)
      (push (list topic) gnus-topic-alist)))
  (gnus-topic-enter-dribble)
  (gnus-group-list-groups)
  (gnus-topic-goto-topic topic))

;; FIXME:
;;  1. When the marked groups are overlapped with the process
;;     region, the behavior of move or remove is not right.
;;  2. Can't process on several marked groups with a same name,
;;     because gnus-group-marked only keeps one copy.

(defvar gnus-topic-history nil)

(defun gnus-topic-move-group (n topic &optional copyp)
  "Move the next N groups to TOPIC.
If COPYP, copy the groups instead."
  (interactive
   (list current-prefix-arg
	 (gnus-completing-read "Move to topic" (mapcar 'car gnus-topic-alist) t
			       nil 'gnus-topic-history)))
  (let ((use-marked (and (not n) (not (gnus-region-active-p))
			 gnus-group-marked t))
	(groups (gnus-group-process-prefix n))
	(topicl (assoc topic gnus-topic-alist))
	(start-topic (gnus-group-topic-name))
	(start-group (progn (forward-line 1) (gnus-group-group-name)))
	entry)
    (if (and (not groups) (not copyp) start-topic)
	(gnus-topic-move start-topic topic)
      (dolist (g groups)
	(gnus-group-remove-mark g use-marked)
	(when (and
	       (setq entry (assoc (gnus-current-topic) gnus-topic-alist))
	       (not copyp))
	  (setcdr entry (gnus-delete-first g (cdr entry))))
	(nconc topicl (list g)))
      (gnus-topic-enter-dribble)
      (if start-group
	  (gnus-group-goto-group start-group)
	(gnus-topic-goto-topic start-topic))
      (gnus-group-list-groups))))

(defun gnus-topic-remove-group (&optional n)
  "Remove the current group from the topic."
  (interactive "P")
  (let ((use-marked (and (not n) (not (gnus-region-active-p))
			 gnus-group-marked t))
	(groups (gnus-group-process-prefix n)))
    (mapc
     (lambda (group)
       (gnus-group-remove-mark group use-marked)
       (let ((topicl (assoc (gnus-current-topic) gnus-topic-alist))
	     (buffer-read-only nil))
	 (when (and topicl group)
	   (gnus-delete-line)
	   (gnus-delete-first group topicl))
	 (gnus-topic-update-topic)))
     groups)
    (gnus-topic-enter-dribble)
    (gnus-group-position-point)))

(defun gnus-topic-copy-group (n topic)
  "Copy the current group to a topic."
  (interactive
   (list current-prefix-arg
	 (gnus-completing-read
	  "Copy to topic" (mapcar 'car gnus-topic-alist) t)))
  (gnus-topic-move-group n topic t))

(defun gnus-topic-kill-group (&optional n discard)
  "Kill the next N groups."
  (interactive "P")
  (if (gnus-group-topic-p)
      (let ((topic (gnus-group-topic-name)))
	(push (cons
	       (gnus-topic-find-topology topic)
	       (assoc topic gnus-topic-alist))
	      gnus-topic-killed-topics)
	(gnus-topic-remove-topic nil t)
	(gnus-topic-find-topology topic nil nil gnus-topic-topology)
	(gnus-topic-enter-dribble))
    (gnus-group-kill-group n discard)
    (if (not (gnus-group-topic-p))
	(gnus-topic-update-topic)
      ;; Move up one line so that we update the right topic.
      (forward-line -1)
      (gnus-topic-update-topic)
      (forward-line 1))))

(defun gnus-topic-yank-group (&optional arg)
  "Yank the last topic."
  (interactive "p")
  (if gnus-topic-killed-topics
      (let* ((previous
	      (or (gnus-group-topic-name)
		  (gnus-topic-next-topic (gnus-current-topic))))
	     (data (pop gnus-topic-killed-topics))
	     (alist (cdr data))
	     (item (cdar data)))
	(push alist gnus-topic-alist)
	(gnus-topic-create-topic
	 (caar item) (gnus-topic-parent-topic previous) previous
	 item)
	(gnus-topic-enter-dribble)
	(gnus-topic-goto-topic (caar item)))
    (let* ((prev (gnus-group-group-name))
	   (gnus-topic-inhibit-change-level t)
	   (gnus-group-indentation
	    (make-string
	     (* gnus-topic-indent-level
		(or (save-excursion
		      (gnus-topic-goto-topic (gnus-current-topic))
		      (gnus-group-topic-level))
		    0))
	     ? ))
	   yanked alist)
      ;; We first yank the groups the normal way...
      (setq yanked (gnus-group-yank-group arg))
      ;; Then we enter the yanked groups into the topics they belong
      ;; to.
      (setq alist (assoc (save-excursion
			   (forward-line -1)
			   (gnus-current-topic))
			 gnus-topic-alist))
      (when (stringp yanked)
	(setq yanked (list yanked)))
      (if (not prev)
	  (nconc alist yanked)
	(if (not (cdr alist))
	    (setcdr alist (nconc yanked (cdr alist)))
	  (while (cdr alist)
	    (when (equal (cadr alist) prev)
	      (setcdr alist (nconc yanked (cdr alist)))
	      (setq alist nil))
	    (setq alist (cdr alist))))))
    (gnus-topic-update-topic)))

(defun gnus-topic-hide-topic (&optional permanent)
  "Hide the current topic.
If PERMANENT, make it stay hidden in subsequent sessions as well."
  (interactive "P")
  (when (gnus-current-topic)
    (gnus-topic-goto-topic (gnus-current-topic))
    (if permanent
	(setcar (cddr
		 (cadr
		  (gnus-topic-find-topology (gnus-current-topic))))
		'hidden))
    (gnus-topic-remove-topic nil nil)))

(defun gnus-topic-show-topic (&optional permanent)
  "Show the hidden topic.
If PERMANENT, make it stay shown in subsequent sessions as well."
  (interactive "P")
  (when (gnus-group-topic-p)
    (if (not permanent)
	(gnus-topic-remove-topic t nil)
      (let ((topic
	     (gnus-topic-find-topology
	      (gnus-completing-read "Show topic"
                                    (mapcar 'car gnus-topic-alist) t))))
	(setcar (cddr (cadr topic)) nil)
	(setcar (cdr (cadr topic)) 'visible)
	(gnus-group-list-groups)))))

(defun gnus-topic-mark-topic (topic &optional unmark non-recursive)
  "Mark all groups in the TOPIC with the process mark.
If NON-RECURSIVE (which is the prefix) is t, don't mark its subtopics."
  (interactive (list (gnus-group-topic-name)
		     nil
		     (and current-prefix-arg t)))
  (if (not topic)
      (call-interactively 'gnus-group-mark-group)
    (save-excursion
      (let ((groups (gnus-topic-find-groups topic gnus-level-killed t nil
					    (not non-recursive))))
	(while groups
	  (funcall (if unmark 'gnus-group-remove-mark 'gnus-group-set-mark)
		   (gnus-info-group (nth 2 (pop groups)))))))))

(defun gnus-topic-unmark-topic (topic &optional dummy non-recursive)
  "Remove the process mark from all groups in the TOPIC.
If NON-RECURSIVE (which is the prefix) is t, don't unmark its subtopics."
  (interactive (list (gnus-group-topic-name)
		     nil
		     (and current-prefix-arg t)))
  (if (not topic)
      (call-interactively 'gnus-group-unmark-group)
    (gnus-topic-mark-topic topic t non-recursive)))

(defun gnus-topic-get-new-news-this-topic (&optional n)
  "Check for new news in the current topic."
  (interactive "P")
  (if (not (gnus-group-topic-p))
      (gnus-group-get-new-news-this-group n)
    (let* ((topic (gnus-group-topic-name))
	   (data (cadr (gnus-topic-find-topology topic))))
      (save-excursion
	(gnus-topic-mark-topic topic nil (and n t))
	(gnus-group-get-new-news-this-group))
      (gnus-topic-remove-topic (eq 'visible (cadr data))))))

(defun gnus-topic-move-matching (regexp topic &optional copyp)
  "Move all groups that match REGEXP to some topic."
  (interactive
   (let (topic)
     (nreverse
      (list
       (setq topic (gnus-completing-read "Move to topic"
                                         (mapcar 'car gnus-topic-alist) t))
       (read-string (format "Move to %s (regexp): " topic))))))
  (gnus-group-mark-regexp regexp)
  (gnus-topic-move-group nil topic copyp))

(defun gnus-topic-copy-matching (regexp topic &optional copyp)
  "Copy all groups that match REGEXP to some topic."
  (interactive
   (let (topic)
     (nreverse
      (list
       (setq topic (gnus-completing-read "Copy to topic"
                                         (mapcar 'car gnus-topic-alist) t))
       (read-string (format "Copy to %s (regexp): " topic))))))
  (gnus-topic-move-matching regexp topic t))

(defun gnus-topic-delete (topic)
  "Delete a topic."
  (interactive (list (gnus-group-topic-name)))
  (unless topic
    (error "No topic to be deleted"))
  (let ((entry (assoc topic gnus-topic-alist))
	(buffer-read-only nil))
    (when (cdr entry)
      (error "Topic not empty"))
    ;; Delete if visible.
    (when (gnus-topic-goto-topic topic)
      (gnus-delete-line))
    ;; Remove from alist.
    (setq gnus-topic-alist (delq entry gnus-topic-alist))
    ;; Remove from topology.
    (gnus-topic-find-topology topic nil nil 'delete)
    (gnus-dribble-touch)))

(defun gnus-topic-rename (old-name new-name)
  "Rename a topic."
  (interactive
   (let ((topic (gnus-current-topic)))
     (list topic
	   (read-string (format "Rename %s to: " topic) topic))))
  ;; Check whether the new name exists.
  (when (gnus-topic-find-topology new-name)
    (error "Topic '%s' already exists" new-name))
  ;; "nil" is an invalid name, for reasons I'd rather not go
  ;; into here.  Trust me.
  (when (equal new-name "nil")
    (error "Invalid name: %s" nil))
  ;; Do the renaming.
  (let ((top (gnus-topic-find-topology old-name))
	(entry (assoc old-name gnus-topic-alist)))
    (when top
      (setcar (cadr top) new-name))
    (when entry
      (setcar entry new-name))
    (forward-line -1)
    (gnus-dribble-touch)
    (gnus-group-list-groups)
    (forward-line 1)))

(defun gnus-topic-indent (&optional unindent)
  "Indent a topic -- make it a sub-topic of the previous topic.
If UNINDENT, remove an indentation."
  (interactive "P")
  (if unindent
      (gnus-topic-unindent)
    (let* ((topic (gnus-current-topic))
	   (parent (gnus-topic-previous-topic topic))
	   (buffer-read-only nil))
      (unless parent
	(error "Nothing to indent %s into" topic))
      (when topic
	(gnus-topic-goto-topic topic)
	(gnus-topic-kill-group)
	(push (cdar gnus-topic-killed-topics) gnus-topic-alist)
	(gnus-topic-create-topic
	 topic parent nil (cdar (car gnus-topic-killed-topics)))
	(pop gnus-topic-killed-topics)
	(or (gnus-topic-goto-topic topic)
	    (gnus-topic-goto-topic parent))))))

(defun gnus-topic-unindent ()
  "Unindent a topic."
  (interactive)
  (let* ((topic (gnus-current-topic))
	 (parent (gnus-topic-parent-topic topic))
	 (grandparent (gnus-topic-parent-topic parent)))
    (unless grandparent
      (error "Nothing to indent %s into" topic))
    (when topic
      (gnus-topic-goto-topic topic)
      (gnus-topic-kill-group)
      (push (cdar gnus-topic-killed-topics) gnus-topic-alist)
      (gnus-topic-create-topic
       topic grandparent (gnus-topic-next-topic parent)
       (cdar (car gnus-topic-killed-topics)))
      (pop gnus-topic-killed-topics)
      (gnus-topic-goto-topic topic))))

(defun gnus-topic-list-active (&optional force)
  "List all groups that Gnus knows about in a topicsified fashion.
If FORCE, always re-read the active file."
  (interactive "P")
  (when force
    (gnus-get-killed-groups))
  (gnus-topic-grok-active force)
  (let ((gnus-topic-topology gnus-topic-active-topology)
	(gnus-topic-alist gnus-topic-active-alist)
	gnus-killed-list gnus-zombie-list)
    (gnus-group-list-groups gnus-level-killed nil 1)))

(defun gnus-topic-toggle-display-empty-topics ()
  "Show/hide topics that have no unread articles."
  (interactive)
  (setq gnus-topic-display-empty-topics
	(not gnus-topic-display-empty-topics))
  (gnus-group-list-groups)
  (message "%s empty topics"
	   (if gnus-topic-display-empty-topics
	       "Showing" "Hiding")))

;;; Topic sorting functions

(defun gnus-topic-edit-parameters (group)
  "Edit the group parameters of GROUP.
If performed on a topic, edit the topic parameters instead."
  (interactive (list (gnus-group-group-name)))
  (if group
      (gnus-group-edit-group-parameters group)
    (if (not (gnus-group-topic-p))
	(error "Nothing to edit on the current line")
      (let ((topic (gnus-group-topic-name)))
	(gnus-edit-form
	 (gnus-topic-parameters topic)
	 (format "Editing the topic parameters for `%s'."
		 (or group topic))
	 `(lambda (form)
	    (gnus-topic-set-parameters ,topic form)))))))

(defun gnus-group-sort-topic (func reverse)
  "Sort groups in the topics according to FUNC and REVERSE."
  (let ((alist gnus-topic-alist))
    (while alist
      ;; !!!Sometimes nil elements sneak into the alist,
      ;; for some reason or other.
      (setcar alist (delq nil (car alist)))
      (setcar alist (delete "dummy.group" (car alist)))
      (gnus-topic-sort-topic (pop alist) func reverse))))

(defun gnus-topic-sort-topic (topic func reverse)
  ;; Each topic only lists the name of the group, while
  ;; the sort predicates expect group infos as inputs.
  ;; So we first transform the group names into infos,
  ;; then sort, and then transform back into group names.
  (setcdr
   topic
   (mapcar
    (lambda (info) (gnus-info-group info))
    (sort
     (mapcar
      (lambda (group) (gnus-get-info group))
      (cdr topic))
     func)))
  ;; Do the reversal, if necessary.
  (when reverse
    (setcdr topic (nreverse (cdr topic)))))

(defun gnus-topic-sort-groups (func &optional reverse)
  "Sort the current topic according to FUNC.
If REVERSE, reverse the sorting order."
  (interactive (list gnus-group-sort-function current-prefix-arg))
  (let ((topic (assoc (gnus-current-topic) gnus-topic-alist)))
    (gnus-topic-sort-topic
     topic (gnus-make-sort-function func) reverse)
    (gnus-group-list-groups)))

(defun gnus-topic-sort-groups-by-alphabet (&optional reverse)
  "Sort the current topic alphabetically by group name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-alphabet reverse))

(defun gnus-topic-sort-groups-by-unread (&optional reverse)
  "Sort the current topic by number of unread articles.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-unread reverse))

(defun gnus-topic-sort-groups-by-level (&optional reverse)
  "Sort the current topic by group level.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-level reverse))

(defun gnus-topic-sort-groups-by-score (&optional reverse)
  "Sort the current topic by group score.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-score reverse))

(defun gnus-topic-sort-groups-by-rank (&optional reverse)
  "Sort the current topic by group rank.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-rank reverse))

(defun gnus-topic-sort-groups-by-method (&optional reverse)
  "Sort the current topic alphabetically by backend name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-method reverse))

(defun gnus-topic-sort-groups-by-server (&optional reverse)
  "Sort the current topic alphabetically by server name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-server reverse))

(defun gnus-topic-sort-topics-1 (top reverse)
  (if (cdr top)
      (let ((subtop
	     (mapcar (gnus-byte-compile
		      `(lambda (top)
			 (gnus-topic-sort-topics-1 top ,reverse)))
		     (sort (cdr top)
			   (lambda (t1 t2)
			     (string-lessp (caar t1) (caar t2)))))))
	(setcdr top (if reverse (reverse subtop) subtop))))
  top)

(defun gnus-topic-sort-topics (&optional topic reverse)
  "Sort topics in TOPIC alphabetically by topic name.
If REVERSE, reverse the sorting order."
  (interactive
   (list (gnus-completing-read "Sort topics in"
                               (mapcar 'car gnus-topic-alist) t
                               (gnus-current-topic))
	 current-prefix-arg))
  (let ((topic-topology (or (and topic (cdr (gnus-topic-find-topology topic)))
			    gnus-topic-topology)))
    (gnus-topic-sort-topics-1 topic-topology reverse)
    (gnus-topic-enter-dribble)
    (gnus-group-list-groups)
    (gnus-topic-goto-topic topic)))

(defun gnus-topic-move (current to)
  "Move the CURRENT topic to TO."
  (interactive
   (list
    (gnus-group-topic-name)
    (gnus-completing-read "Move to topic" (mapcar 'car gnus-topic-alist) t)))
  (unless (and current to)
    (error "Can't find topic"))
  (let ((current-top (cdr (gnus-topic-find-topology current)))
	(to-top (cdr (gnus-topic-find-topology to))))
    (unless current-top
      (error "Can't find topic `%s'" current))
    (unless to-top
      (error "Can't find topic `%s'" to))
    (if (gnus-topic-find-topology to current-top 0);; Don't care the level
	(error "Can't move `%s' to its sub-level" current))
    (gnus-topic-find-topology current nil nil 'delete)
    (setcdr (last to-top) (list current-top))
    (gnus-topic-enter-dribble)
    (gnus-group-list-groups)
    (gnus-topic-goto-topic current)))

(defun gnus-subscribe-topics (newsgroup)
  (catch 'end
    (let (match gnus-group-change-level-function)
      (dolist (topic (gnus-topic-list))
	(when (and (setq match (cdr (assq 'subscribe
					  (gnus-topic-parameters topic))))
		   (string-match match newsgroup))
	  ;; Just subscribe the group.
	  (gnus-subscribe-alphabetically newsgroup)
	  ;; Add the group to the topic.
	  (nconc (assoc topic gnus-topic-alist) (list newsgroup))
	  ;; if this topic specifies a default level, use it
	  (let ((subscribe-level (cdr (assq 'subscribe-level
					    (gnus-topic-parameters topic)))))
	    (when subscribe-level
		(gnus-group-change-level newsgroup subscribe-level
					 gnus-level-default-subscribed)))
	  (throw 'end t)))
      nil)))

(provide 'gnus-topic)

;;; gnus-topic.el ends here
