;;; org-archive.el --- Archiving for Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
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

;; This file contains the face definitions for Org.

;;; Code:

(require 'org)

(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())

(defcustom org-archive-default-command 'org-archive-subtree
  "The default archiving command."
  :group 'org-archive
  :type '(choice
	  (const org-archive-subtree)
	  (const org-archive-to-archive-sibling)
	  (const org-archive-set-tag)))

(defcustom org-archive-reversed-order nil
  "Non-nil means make the tree first child under the archive heading, not last."
  :group 'org-archive
  :version "24.1"
  :type 'boolean)

(defcustom org-archive-sibling-heading "Archive"
  "Name of the local archive sibling that is used to archive entries locally.
Locally means: in the tree, under a sibling.
See `org-archive-to-archive-sibling' for more information."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-mark-done nil
  "Non-nil means mark entries as DONE when they are moved to the archive file.
This can be a string to set the keyword to use.  When t, Org-mode will
use the first keyword in its list that means done."
  :group 'org-archive
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (string :tag "Use this keyword")))

(defcustom org-archive-stamp-time t
  "Non-nil means add a time stamp to entries moved to an archive file.
This variable is obsolete and has no effect anymore, instead add or remove
`time' from the variable `org-archive-save-context-info'."
  :group 'org-archive
  :type 'boolean)

(defcustom org-archive-subtree-add-inherited-tags 'infile
  "Non-nil means append inherited tags when archiving a subtree."
  :group 'org-archive
  :version "24.1"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When archiving a subtree to the same file" infile)
	  (const :tag "Always" t)))

(defcustom org-archive-save-context-info '(time file olpath category todo itags)
  "Parts of context info that should be stored as properties when archiving.
When a subtree is moved to an archive file, it loses information given by
context, like inherited tags, the category, and possibly also the TODO
state (depending on the variable `org-archive-mark-done').
This variable can be a list of any of the following symbols:

time       The time of archiving.
file       The file where the entry originates.
ltags      The local tags, in the headline of the subtree.
itags      The tags the subtree inherits from further up the hierarchy.
todo       The pre-archive TODO state.
category   The category, taken from file name or #+CATEGORY lines.
olpath     The outline path to the item.  These are all headlines above
           the current item, separated by /, like a file path.

For each symbol present in the list, a property will be created in
the archived entry, with a prefix \"ARCHIVE_\", to remember this
information."
  :group 'org-archive
  :type '(set :greedy t
	  (const :tag "Time" time)
	  (const :tag "File" file)
	  (const :tag "Category" category)
	  (const :tag "TODO state" todo)
	  (const :tag "Priority" priority)
	  (const :tag "Inherited tags" itags)
	  (const :tag "Outline path" olpath)
	  (const :tag "Local tags" ltags)))

(defun org-get-local-archive-location ()
  "Get the archive location applicable at point."
  (let ((re "^#\\+ARCHIVE:[ \t]+\\(\\S-.*\\S-\\)[ \t]*$")
	prop)
    (save-excursion
      (save-restriction
	(widen)
	(setq prop (org-entry-get nil "ARCHIVE" 'inherit))
	(cond
	 ((and prop (string-match "\\S-" prop))
	  prop)
	 ((or (re-search-backward re nil t)
	      (re-search-forward re nil t))
	  (match-string 1))
	 (t org-archive-location))))))

(defun org-add-archive-files (files)
  "Splice the archive files into the list of files.
This implies visiting all these files and finding out what the
archive file is."
  (org-uniquify
   (apply
    'append
    (mapcar
     (lambda (f)
       (if (not (file-exists-p f))
	   nil
	 (with-current-buffer (org-get-agenda-file-buffer f)
	   (cons f (org-all-archive-files)))))
     files))))

(defun org-all-archive-files ()
  "Get a list of all archive files used in the current buffer."
  (let (file files)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward
		"^\\(#\\+\\|[ \t]*:\\)ARCHIVE:[ \t]+\\(.*\\)"
		nil t)
	  (setq file (org-extract-archive-file
		      (org-match-string-no-properties 2)))
	  (and file (> (length file) 0) (file-exists-p file)
	       (add-to-list 'files file)))))
    (setq files (nreverse files))
    (setq file (org-extract-archive-file))
    (and file (> (length file) 0) (file-exists-p file)
	 (add-to-list 'files file))
    files))

(defun org-extract-archive-file (&optional location)
  "Extract and expand the file name from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (if (= (match-beginning 1) (match-end 1))
	  (buffer-file-name (buffer-base-buffer))
	(expand-file-name
	 (format (match-string 1 location)
		 (file-name-nondirectory
		  (buffer-file-name (buffer-base-buffer))))))))

(defun org-extract-archive-heading (&optional location)
  "Extract the heading from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (format (match-string 2 location)
	      (file-name-nondirectory
	       (buffer-file-name (buffer-base-buffer))))))

(defun org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current file, or in
a different file.  The tree will be moved to that location, the subtree
heading be marked DONE, and the current time will be added.

When called with prefix argument FIND-DONE, find whole trees without any
open TODO items and archive them (after getting confirmation from the user).
If the cursor is not at a headline when this command is called, try all level
1 trees.  If the cursor is on a headline, only try the direct children of
this heading."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
		 (org-archive-subtree ,find-done))
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (if find-done
	(org-archive-all-done)
      ;; Save all relevant TODO keyword-relatex variables
      (let ((tr-org-todo-line-regexp org-todo-line-regexp) ; keep despite compiler
	    (tr-org-todo-keywords-1 org-todo-keywords-1)
	    (tr-org-todo-kwd-alist org-todo-kwd-alist)
	    (tr-org-done-keywords org-done-keywords)
	    (tr-org-todo-regexp org-todo-regexp)
	    (tr-org-todo-line-regexp org-todo-line-regexp)
	    (tr-org-odd-levels-only org-odd-levels-only)
	    (this-buffer (current-buffer))
	    ;; start of variables that will be used for saving context
	    ;; The compiler complains about them - keep them anyway!
	    (file (abbreviate-file-name
		   (or (buffer-file-name (buffer-base-buffer))
		       (error "No file associated to buffer"))))
	    (olpath (mapconcat 'identity (org-get-outline-path) "/"))
	    (time (format-time-string
		   (substring (cdr org-time-stamp-formats) 1 -1)
		   (current-time)))
	    category todo priority ltags itags atags
	    ;; end of variables that will be used for saving context
	    location afile heading buffer level newfile-p infile-p visiting)

	;; Find the local archive location
	(setq location (org-get-local-archive-location)
	      afile (org-extract-archive-file location)
	      heading (org-extract-archive-heading location)
	      infile-p (equal file (abbreviate-file-name afile)))
	(unless afile
	  (error "Invalid `org-archive-location'"))

	(if (> (length afile) 0)
	    (setq newfile-p (not (file-exists-p afile))
		  visiting (find-buffer-visiting afile)
		  buffer (or visiting (find-file-noselect afile)))
	  (setq buffer (current-buffer)))
	(unless buffer
	  (error "Cannot access file \"%s\"" afile))
	(if (and (> (length heading) 0)
		 (string-match "^\\*+" heading))
	    (setq level (match-end 0))
	  (setq heading nil level 0))
	(save-excursion
	  (org-back-to-heading t)
	  ;; Get context information that will be lost by moving the tree
	  (setq category (org-get-category nil 'force-refresh)
		todo (and (looking-at org-todo-line-regexp)
			  (match-string 2))
		priority (org-get-priority
			  (if (match-end 3) (match-string 3) ""))
		ltags (org-get-tags)
		itags (org-delete-all ltags (org-get-tags-at))
		atags (org-get-tags-at))
	  (setq ltags (mapconcat 'identity ltags " ")
		itags (mapconcat 'identity itags " "))
	  ;; We first only copy, in case something goes wrong
	  ;; we need to protect `this-command', to avoid kill-region sets it,
	  ;; which would lead to duplication of subtrees
	  (let (this-command) (org-copy-subtree 1 nil t))
	  (set-buffer buffer)
	  ;; Enforce org-mode for the archive buffer
	  (if (not (eq major-mode 'org-mode))
	      ;; Force the mode for future visits.
	      (let ((org-insert-mode-line-in-empty-file t)
		    (org-inhibit-startup t))
		(call-interactively 'org-mode)))
	  (when newfile-p
	    (goto-char (point-max))
	    (insert (format "\nArchived entries from file %s\n\n"
			    (buffer-file-name this-buffer))))
	  ;; Force the TODO keywords of the original buffer
	  (let ((org-todo-line-regexp tr-org-todo-line-regexp)
		(org-todo-keywords-1 tr-org-todo-keywords-1)
		(org-todo-kwd-alist tr-org-todo-kwd-alist)
		(org-done-keywords tr-org-done-keywords)
		(org-todo-regexp tr-org-todo-regexp)
		(org-todo-line-regexp tr-org-todo-line-regexp)
		(org-odd-levels-only
		 (if (local-variable-p 'org-odd-levels-only (current-buffer))
		     org-odd-levels-only
		   tr-org-odd-levels-only)))
	    (goto-char (point-min))
	    (show-all)
	    (if heading
		(progn
		  (if (re-search-forward
		       (concat "^" (regexp-quote heading)
			       (org-re "[ \t]*\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\($\\|\r\\)"))
		       nil t)
		      (goto-char (match-end 0))
		    ;; Heading not found, just insert it at the end
		    (goto-char (point-max))
		    (or (bolp) (insert "\n"))
		    (insert "\n" heading "\n")
		    (end-of-line 0))
		  ;; Make the subtree visible
		  (show-subtree)
		  (if org-archive-reversed-order
		      (progn
			(org-back-to-heading t)
			(outline-next-heading))
		    (org-end-of-subtree t))
		  (skip-chars-backward " \t\r\n")
		  (and (looking-at "[ \t\r\n]*")
		       (replace-match "\n\n")))
	      ;; No specific heading, just go to end of file.
	      (goto-char (point-max)) (insert "\n"))
	    ;; Paste
	    (org-paste-subtree (org-get-valid-level level (and heading 1)))
	    ;; Shall we append inherited tags?
	    (and itags
		 (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
			  infile-p)
		     (eq org-archive-subtree-add-inherited-tags t))
		 (org-set-tags-to atags))
	    ;; Mark the entry as done
	    (when (and org-archive-mark-done
		       (looking-at org-todo-line-regexp)
		       (or (not (match-end 2))
			   (not (member (match-string 2) org-done-keywords))))
	      (let (org-log-done org-todo-log-states)
		(org-todo
		 (car (or (member org-archive-mark-done org-done-keywords)
			  org-done-keywords)))))

	    ;; Add the context info
	    (when org-archive-save-context-info
	      (let ((l org-archive-save-context-info) e n v)
		(while (setq e (pop l))
		  (when (and (setq v (symbol-value e))
			     (stringp v) (string-match "\\S-" v))
		    (setq n (concat "ARCHIVE_" (upcase (symbol-name e))))
		    (org-entry-put (point) n v)))))

	    ;; Save and kill the buffer, if it is not the same buffer.
	    (when (not (eq this-buffer buffer))
	      (save-buffer))))
	;; Here we are back in the original buffer.  Everything seems to have
	;; worked.  So now cut the tree and finish up.
	(let (this-command) (org-cut-subtree))
	(when (featurep 'org-inlinetask)
	  (org-inlinetask-remove-END-maybe))
	(setq org-markers-to-move nil)
	(message "Subtree archived %s"
		 (if (eq this-buffer buffer)
		     (concat "under heading: " heading)
		   (concat "in file: " (abbreviate-file-name afile))))))
    (org-reveal)
    (if (looking-at "^[ \t]*$")
	(outline-next-visible-heading 1))))

(defun org-archive-to-archive-sibling ()
  "Archive the current heading by moving it under the archive sibling.
The archive sibling is a sibling of the heading with the heading name
`org-archive-sibling-heading' and an `org-archive-tag' tag.  If this
sibling does not exist, it will be created at the end of the subtree."
  (interactive)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (when (eq org-loop-over-headlines-in-active-region 'start-level)
		  'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 '(progn (setq org-map-continue-from
		       (progn (org-back-to-heading)
			      (if (looking-at (concat "^.*:" org-archive-tag ":.*$"))
			      	  (org-end-of-subtree t)
				(point))))
		 (when (org-at-heading-p)
		   (org-archive-to-archive-sibling)))
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (save-restriction
      (widen)
      (let (b e pos leader level)
	(org-back-to-heading t)
	(looking-at org-outline-regexp)
	(setq leader (match-string 0)
	      level (funcall outline-level))
	(setq pos (point))
	(condition-case nil
	    (outline-up-heading 1 t)
	  (error (setq e (point-max)) (goto-char (point-min))))
	(setq b (point))
	(unless e
	  (condition-case nil
	      (org-end-of-subtree t t)
	    (error (goto-char (point-max))))
	  (setq e (point)))
	(goto-char b)
	(unless (re-search-forward
		 (concat "^" (regexp-quote leader)
			 "[ \t]*"
			 org-archive-sibling-heading
			 "[ \t]*:"
			 org-archive-tag ":") e t)
	  (goto-char e)
	  (or (bolp) (newline))
	  (insert leader org-archive-sibling-heading "\n")
	  (beginning-of-line 0)
	  (org-toggle-tag org-archive-tag 'on))
	(beginning-of-line 1)
	(if org-archive-reversed-order
	    (outline-next-heading)
	  (org-end-of-subtree t t))
	(save-excursion
	  (goto-char pos)
	  (let ((this-command this-command)) (org-cut-subtree)))
	(org-paste-subtree (org-get-valid-level level 1))
	(org-set-property
	 "ARCHIVE_TIME"
	 (format-time-string
	  (substring (cdr org-time-stamp-formats) 1 -1)
	  (current-time)))
	(outline-up-heading 1 t)
	(hide-subtree)
	(org-cycle-show-empty-lines 'folded)
	(goto-char pos)))
    (org-reveal)
    (if (looking-at "^[ \t]*$")
	(outline-next-visible-heading 1))))

(defun org-archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (let ((re org-not-done-heading-regexp) re1
	(rea (concat ".*:" org-archive-tag ":"))
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag (no open TODO items)? "
		    "Move subtree to archive (no open TODO items)? "))
	beg end (cntarch 0))
    (if (org-at-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (+ (- (match-end 0) (match-beginning 0) 1)
				     (if org-odd-levels-only 2 1))
				  ?*))
			    " "))
	  (move-marker begm (point))
	  (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
	(setq beg (match-beginning 0)
	      end (save-excursion (org-end-of-subtree t) (point)))
	(goto-char beg)
	(if (re-search-forward re end t)
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   (y-or-n-p question))
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

(defun org-toggle-archive-tag (&optional find-done)
  "Toggle the archive tag for the current headline.
With prefix ARG, check all children of current headline and offer tagging
the children that do not contain any open TODO items."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-toggle-archive-tag ,find-done)
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (if find-done
	(org-archive-all-done 'tag)
      (let (set)
	(save-excursion
	  (org-back-to-heading t)
	  (setq set (org-toggle-tag org-archive-tag))
	  (when set (hide-subtree)))
	(and set (beginning-of-line 1))
	(message "Subtree %s" (if set "archived" "unarchived"))))))

(defun org-archive-set-tag ()
  "Set the ARCHIVE tag."
  (interactive)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 'org-archive-set-tag
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (org-toggle-tag org-archive-tag 'on)))

;;;###autoload
(defun org-archive-subtree-default ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (call-interactively org-archive-default-command))

;;;###autoload
(defun org-archive-subtree-default-with-confirmation ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (if (y-or-n-p "Archive this subtree or entry? ")
      (call-interactively org-archive-default-command)
    (error "Abort")))

(provide 'org-archive)

;;; org-archive.el ends here
