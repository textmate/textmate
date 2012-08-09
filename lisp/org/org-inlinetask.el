;;; org-inlinetask.el --- Tasks independent of outline hierarchy

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This module implements inline tasks in Org-mode.  Inline tasks are
;; tasks that have all the properties of normal outline nodes, including
;; the ability to store meta data like scheduling dates, TODO state, tags
;; and properties.  However, these nodes are treated specially by the
;; visibility cycling and export commands.
;;
;; Visibility cycling exempts these nodes from cycling.  So whenever their
;; parent is opened, so are these tasks.  This will only work with
;; `org-cycle', so if you are also using other commands to show/hide
;; entries, you will occasionally find these tasks to behave like
;; all other outline nodes, seemingly splitting the text of the parent
;; into children.
;;
;; Export commands do not treat these nodes as part of the sectioning
;; structure, but as a special inline text that is either removed, or
;; formatted in some special way.  This in handled by
;; `org-inlinetask-export' and `org-inlinetask-export-templates'
;; variables.
;;
;; Special fontification of inline tasks, so that they can be immediately
;; recognized.  From the stars of the headline, only the first and the
;; last two will be visible, the others will be hidden using the
;; `org-hide' face.
;;
;; An inline task is identified solely by a minimum outline level, given
;; by the variable `org-inlinetask-min-level', default 15.
;;
;; If you need to have a time planning line (DEADLINE etc), drawers,
;; for example LOGBOOK of PROPERTIES, or even normal text as part of
;; the inline task, you must add an "END" headline with the same
;; number of stars.
;;
;; As an example, here are two valid inline tasks:
;;
;;    **************** TODO a small task
;;
;; and
;;
;;    **************** TODO another small task
;;                     DEADLINE: <2009-03-30 Mon>
;;                     :PROPERTIES:
;;                       :SOMETHING: or other
;;                     :END:
;;                     And here is some extra text
;;    **************** END
;;
;; Also, if you want to use refiling and archiving for inline tasks,
;; The END line must be present to make things work properly.
;;
;; This package installs one new command:
;;
;; C-c C-x t      Insert a new inline task with END line

;;; Code:

(require 'org)

(defgroup org-inlinetask nil
  "Options concerning inline tasks in Org mode."
  :tag "Org Inline Tasks"
  :group 'org-structure)

(defcustom org-inlinetask-min-level 15
  "Minimum level a headline must have before it is treated as an inline task.
It is strongly recommended that you set `org-cycle-max-level' not at all,
or to a number smaller than this one.  In fact, when `org-cycle-max-level' is
not set, it will be assumed to be one less than the value of smaller than
the value of this variable."
  :group 'org-inlinetask
  :type '(choice
	  (const :tag "Off" nil)
	  (integer)))

(defcustom org-inlinetask-export t
  "Non-nil means export inline tasks.
When nil, they will not be exported."
  :group 'org-inlinetask
  :type 'boolean)

(defvar org-inlinetask-export-templates
  '((html "<div class=\"inlinetask\"><b>%s%s</b><br />%s</div>"
	  '((unless (eq todo "")
	      (format "<span class=\"%s %s\">%s%s</span> "
		      class todo todo priority))
	    heading content))
    (odt "%s" '((org-odt-format-inlinetask heading content
					   todo priority tags)))

    (latex "\\begin\{description\}\n\\item[%s%s]~%s\\end\{description\}"
	   '((unless (eq todo "") (format "\\textsc\{%s%s\} " todo priority))
	     heading content))
    (ascii "     -- %s%s%s"
	   '((unless (eq todo "") (format "%s%s " todo priority))
	     heading
	     (unless (eq content "")
	       (format "\n         ¦ %s"
		       (mapconcat 'identity (org-split-string content "\n")
				  "\n         ¦ ")))))
    (docbook "<variablelist>
<varlistentry>
<term>%s%s</term>
<listitem><para>%s</para></listitem>
</varlistentry>
</variablelist>"
	     '((unless (eq todo "") (format "%s%s " todo priority))
	       heading content)))
  "Templates for inline tasks in various exporters.

This variable is an alist in the shape of \(BACKEND STRING OBJECTS\).

BACKEND is the name of the backend for the template \(ascii, html...\).

STRING is a format control string.

OBJECTS is a list of elements to be substituted into the format
string.  They can be of any type, from a string to a form
returning a value (thus allowing conditional insertion).  A nil
object will be substituted as the empty string.  Obviously, there
must be at least as many objects as %-sequences in the format
string.

Moreover, the following special keywords are provided: `todo',
`priority', `heading', `content', `tags'.  If some of them are not
defined in an inline task, their value is the empty string.

As an example, valid associations are:

\(html \"<ul><li>%s <p>%s</p></li></ul>\" \(heading content\)\)

or, with the additional package \"todonotes\" for LaTeX,

\(latex \"\\todo[inline]{\\textbf{\\textsf{%s %s}}\\linebreak{} %s}\"
       '\(\(unless \(eq todo \"\"\)
	   \(format \"\\textsc{%s%s}\" todo priority\)\)
	 heading content\)\)\)")

(defvar org-odd-levels-only)
(defvar org-keyword-time-regexp)
(defvar org-drawer-regexp)
(defvar org-complex-heading-regexp)
(defvar org-property-end-re)

(defcustom org-inlinetask-default-state nil
  "Non-nil means make inline tasks have a TODO keyword initially.
This should be the state `org-inlinetask-insert-task' should use by
default, or nil of no state should be assigned."
  :group 'org-inlinetask
  :version "24.1"
  :type '(choice
	  (const :tag "No state" nil)
	  (string :tag "Specific state")))

(defun org-inlinetask-insert-task (&optional no-state)
  "Insert an inline task.
If prefix arg NO-STATE is set, ignore `org-inlinetask-default-state'."
  (interactive "P")
  ;; Error when inside an inline task, except if point was at its very
  ;; beginning, in which case the new inline task will be inserted
  ;; before this one.
  (when (and (org-inlinetask-in-task-p)
	     (not (and (org-inlinetask-at-task-p) (bolp))))
    (error "Cannot nest inline tasks"))
  (or (bolp) (newline))
  (let* ((indent (if org-odd-levels-only
		     (1- (* 2 org-inlinetask-min-level))
		   org-inlinetask-min-level))
	 (indent-string (concat (make-string indent ?*) " ")))
    (insert indent-string
	    (if (or no-state (not org-inlinetask-default-state))
		"\n"
	      (concat org-inlinetask-default-state " \n"))
	    indent-string "END\n"))
  (end-of-line -1))
(define-key org-mode-map "\C-c\C-xt" 'org-inlinetask-insert-task)

(defun org-inlinetask-outline-regexp ()
  "Return string matching an inline task heading.
The number of levels is controlled by `org-inlinetask-min-level'."
  (let ((nstars (if org-odd-levels-only
		    (1- (* org-inlinetask-min-level 2))
		  org-inlinetask-min-level)))
    (format "^\\(\\*\\{%d,\\}\\)[ \t]+" nstars)))

(defun org-inlinetask-at-task-p ()
  "Return true if point is at beginning of an inline task."
  (save-excursion
    (beginning-of-line)
    (and (looking-at (concat (org-inlinetask-outline-regexp)  "\\(.*\\)"))
	 (not (string-match "^end[ \t]*$" (downcase (match-string 2)))))))

(defun org-inlinetask-in-task-p ()
  "Return true if point is inside an inline task."
  (save-excursion
    (beginning-of-line)
    (let* ((case-fold-search t)
	   (stars-re (org-inlinetask-outline-regexp))
	   (task-beg-re (concat stars-re "\\(?:.*\\)"))
	   (task-end-re (concat stars-re "END[ \t]*$")))
      (or (org-looking-at-p task-beg-re)
	  (and (re-search-forward "^\\*+[ \t]+" nil t)
	       (progn (beginning-of-line) (org-looking-at-p task-end-re)))))))

(defun org-inlinetask-goto-beginning ()
  "Go to the beginning of the inline task at point."
  (end-of-line)
  (let ((case-fold-search t)
	(inlinetask-re (org-inlinetask-outline-regexp)))
    (re-search-backward inlinetask-re nil t)
    (when (org-looking-at-p (concat inlinetask-re "END[ \t]*$"))
      (re-search-backward inlinetask-re nil t))))

(defun org-inlinetask-goto-end ()
  "Go to the end of the inline task at point.
Return point."
  (save-match-data
    (beginning-of-line)
    (let* ((case-fold-search t)
	   (inlinetask-re (org-inlinetask-outline-regexp))
	   (task-end-re (concat inlinetask-re "END[ \t]*$")))
      (cond
       ((looking-at task-end-re) (forward-line))
       ((looking-at inlinetask-re)
	(forward-line)
	(cond
	 ((looking-at task-end-re) (forward-line))
	 ((looking-at inlinetask-re))
	 ((org-inlinetask-in-task-p)
	  (re-search-forward inlinetask-re nil t)
	  (forward-line))))
       (t (re-search-forward inlinetask-re nil t)
	  (forward-line)))
      (point))))

(defun org-inlinetask-get-task-level ()
  "Get the level of the inline task around.
This assumes the point is inside an inline task."
  (save-excursion
    (end-of-line)
    (re-search-backward (org-inlinetask-outline-regexp) nil t)
    (- (match-end 1) (match-beginning 1))))

(defun org-inlinetask-promote ()
  "Promote the inline task at point.
If the task has an end part, promote it.  Also, prevents level from
going below `org-inlinetask-min-level'."
  (interactive)
  (if (not (org-inlinetask-in-task-p))
      (error "Not in an inline task")
    (save-excursion
      (let* ((lvl (org-inlinetask-get-task-level))
	     (next-lvl (org-get-valid-level lvl -1))
	     (diff (- next-lvl lvl))
	     (down-task (concat (make-string next-lvl ?*)))
	     beg)
	(if (< next-lvl org-inlinetask-min-level)
	    (error "Cannot promote an inline task at minimum level")
	  (org-inlinetask-goto-beginning)
	  (setq beg (point))
	  (replace-match down-task nil t nil 1)
	  (org-inlinetask-goto-end)
	  (if (eobp) (beginning-of-line) (forward-line -1))
	  (unless (= (point) beg)
	    (replace-match down-task nil t nil 1)
	    (when org-adapt-indentation
	      (goto-char beg)
	      (org-fixup-indentation diff))))))))

(defun org-inlinetask-demote ()
  "Demote the inline task at point.
If the task has an end part, also demote it."
  (interactive)
  (if (not (org-inlinetask-in-task-p))
      (error "Not in an inline task")
    (save-excursion
      (let* ((lvl (org-inlinetask-get-task-level))
	     (next-lvl (org-get-valid-level lvl 1))
	     (diff (- next-lvl lvl))
	     (down-task (concat (make-string next-lvl ?*)))
	     beg)
	(org-inlinetask-goto-beginning)
	(setq beg (point))
	(replace-match down-task nil t nil 1)
	(org-inlinetask-goto-end)
	(if (eobp) (beginning-of-line) (forward-line -1))
	(unless (= (point) beg)
	  (replace-match down-task nil t nil 1)
	  (when org-adapt-indentation
	    (goto-char beg)
	    (org-fixup-indentation diff)))))))

(defvar org-export-current-backend) ; dynamically bound in org-exp.el
(defun org-inlinetask-export-handler ()
  "Handle headlines with level larger or equal to `org-inlinetask-min-level'.
Either remove headline and meta data, or do special formatting."
  (goto-char (point-min))
  (let* ((keywords-re (concat "^[ \t]*" org-keyword-time-regexp))
	 (inline-re (concat (org-inlinetask-outline-regexp) ".*")))
    (while (re-search-forward inline-re nil t)
      (let ((headline (match-string 0))
	    (beg (point-at-bol))
	    (end (copy-marker (save-excursion
				(org-inlinetask-goto-end) (point))))
	    content)
      ;; Delete SCHEDULED, DEADLINE...
      (while (re-search-forward keywords-re end t)
	(delete-region (point-at-bol) (1+ (point-at-eol))))
      (goto-char beg)
      ;; Delete drawers
      (while (re-search-forward org-drawer-regexp end t)
	(when (save-excursion (re-search-forward org-property-end-re nil t))
	  (delete-region beg (1+ (match-end 0)))))
      ;; Get CONTENT, if any.
      (goto-char beg)
      (forward-line 1)
      (unless (= (point) end)
	(setq content (buffer-substring (point)
					(save-excursion (goto-char end)
							(forward-line -1)
							(point)))))
      ;; Remove the task.
      (goto-char beg)
      (delete-region beg end)
      (when (and org-inlinetask-export
		 (assq org-export-current-backend
		       org-inlinetask-export-templates))
	;; Format CONTENT, if appropriate.
	(setq content
	      (if (not (and content (string-match "\\S-" content)))
		  ""
		;; Ensure CONTENT has minimal indentation, a single
		;; newline character at its boundaries, and isn't
		;; protected.
		(when (string-match "\\`\\([ \t]*\n\\)+" content)
		  (setq content (substring content (match-end 0))))
		(when (string-match "[ \t\n]+\\'" content)
		  (setq content (substring content 0 (match-beginning 0))))
		(org-add-props
		    (concat "\n\n" (org-remove-indentation content) "\n\n")
		    '(org-protected nil org-native-text nil))))

	(when (string-match org-complex-heading-regexp headline)
	  (let* ((nil-to-str
		  (function
		   ;; Change nil arguments into empty strings.
		   (lambda (el) (or (eval el) ""))))
		 ;;  Set up keywords provided to templates.
		 (todo (or (match-string 2 headline) ""))
		 (class (or (and (eq "" todo) "")
			    (if (member todo org-done-keywords) "done" "todo")))
		 (priority (or (match-string 3 headline) ""))
		 (heading (or (match-string 4 headline) ""))
		 (tags (or (match-string 5 headline) ""))
		 ;; Read `org-inlinetask-export-templates'.
		 (backend-spec (assq org-export-current-backend
				     org-inlinetask-export-templates))
		 (format-str (org-add-props (nth 1 backend-spec)
				 '(org-protected t org-native-text t)))
		 (tokens (cadr (nth 2 backend-spec)))
		 ;; Build export string. Ensure it won't break
		 ;; surrounding lists by giving it arbitrary high
		 ;; indentation.
		 (export-str (org-add-props
				 (eval (append '(format format-str)
					       (mapcar nil-to-str tokens)))
				 '(original-indentation 1000))))
	    ;; Ensure task starts a new paragraph.
	    (unless (or (bobp)
			(save-excursion (forward-line -1)
					(looking-at "[ \t]*$")))
	      (insert "\n"))
	    (insert export-str)
	    (unless (bolp) (insert "\n")))))))))

(defun org-inlinetask-get-current-indentation ()
  "Get the indentation of the last non-while line above this one."
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-backward " \t\n")
    (beginning-of-line 1)
    (or (org-at-item-p)
	(looking-at "[ \t]*"))
    (goto-char (match-end 0))
    (current-column)))

(defvar org-indent-indentation-per-level) ; defined in org-indent.el

(defface org-inlinetask
  (org-compatible-face 'shadow '((t (:bold t))))
  "Face for inlinetask headlines."
  :group 'org-faces)

(defun org-inlinetask-fontify (limit)
  "Fontify the inline tasks down to LIMIT."
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re (concat "^\\(\\*\\)\\(\\*\\{"
		    (format "%d" (- nstars 3))
		    ",\\}\\)\\(\\*\\* .*\\)"))
	 ;; Virtual indentation will add the warning face on the first
	 ;; star. Thus, in that case, only hide it.
	 (start-face (if (and (org-bound-and-true-p org-indent-mode)
			      (> org-indent-indentation-per-level 1))
			 'org-hide
		       'org-warning)))
    (while (re-search-forward re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   `(face ,start-face font-lock-fontified t))
      (add-text-properties (match-beginning 2) (match-end 2)
			   '(face org-hide font-lock-fontified t))
      (add-text-properties (match-beginning 3) (match-end 3)
			   '(face org-inlinetask font-lock-fontified t)))))

(defun org-inlinetask-toggle-visibility ()
  "Toggle visibility of inline task at point."
  (let ((end (save-excursion
	       (org-inlinetask-goto-end)
	       (if (bolp) (1- (point)) (point))))
	(start (save-excursion
		 (org-inlinetask-goto-beginning)
		 (point-at-eol))))
    (cond
     ;; Nothing to show/hide.
     ((= end start))
     ;; Inlinetask was folded: expand it.
     ((get-char-property (1+ start) 'invisible)
      (outline-flag-region start end nil))
     (t (outline-flag-region start end t)))))

(defun org-inlinetask-remove-END-maybe ()
  "Remove an END line when present."
  (when (looking-at (format "\\([ \t]*\n\\)*\\*\\{%d,\\}[ \t]+END[ \t]*$"
			    org-inlinetask-min-level))
    (replace-match "")))

(eval-after-load "org-exp"
  '(add-hook 'org-export-preprocess-before-backend-specifics-hook
	     'org-inlinetask-export-handler))
(eval-after-load "org"
  '(add-hook 'org-font-lock-hook 'org-inlinetask-fontify))

(provide 'org-inlinetask)

;;; org-inlinetask.el ends here
