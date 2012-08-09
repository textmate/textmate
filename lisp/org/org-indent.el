;;; org-indent.el --- Dynamic indentation for  Org-mode
;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;;
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
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.
;;
;; The process is synchronous, toggled at every buffer modification.
;; Though, the initialization (indentation of text already in the
;; buffer), which can take a few seconds in large buffers, happens on
;; idle time.
;;
;;; Code:

(require 'org-macs)
(require 'org-compat)
(require 'org)

(eval-when-compile
  (require 'cl))

(declare-function org-inlinetask-get-task-level "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-list-item-body-column "org-list" (item))

(defgroup org-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Org Indent"
  :group 'org)

(defconst org-indent-max 40
  "Maximum indentation in characters.")
(defconst org-indent-max-levels 20
  "Maximum added level through virtual indentation, in characters.

It is computed by multiplying `org-indent-indentation-per-level'
minus one by actual level of the headline minus one.")

(defvar org-indent-strings nil
  "Vector with all indentation strings.
It will be set in `org-indent-initialize'.")
(defvar org-indent-stars nil
  "Vector with all indentation star strings.
It will be set in `org-indent-initialize'.")
(defvar org-indent-inlinetask-first-star (org-add-props "*" '(face org-warning))
  "First star of inline tasks, with correct face.")
(defvar org-indent-agent-timer nil
  "Timer running the initialize agent.")
(defvar org-indent-agentized-buffers nil
  "List of buffers watched by the initialize agent.")
(defvar org-indent-agent-resume-timer nil
  "Timer to reschedule agent after switching to other idle processes.")
(defvar org-indent-agent-active-delay '(0 2 0)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize is current.")
(defvar org-indent-agent-passive-delay '(0 0 400000)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize isn't current.")
(defvar org-indent-agent-resume-delay '(0 0 100000)
  "Minimal time for other idle processes before switching back to agent.")
(defvar org-indent-initial-marker nil
  "Position of initialization before interrupt.
This is used locally in each buffer being initialized.")
(defvar org-hide-leading-stars-before-indent-mode nil
  "Used locally.")
(defvar org-indent-modified-headline-flag nil
  "Non-nil means the last deletion operated on an headline.
It is modified by `org-indent-notify-modified-headline'.")


(defcustom org-indent-boundary-char ?\   ; comment to protect space char
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so.
This can be useful on a terminal window - under a windowing system,
it may be prettier to customize the org-indent face."
  :group 'org-indent
  :set (lambda (var val)
	 (set var val)
	 (and org-indent-strings (org-indent-initialize)))
  :type 'character)

(defcustom org-indent-mode-turns-off-org-adapt-indentation t
  "Non-nil means setting the variable `org-indent-mode' will \
turn off indentation adaptation.
For details see the variable `org-adapt-indentation'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-mode-turns-on-hiding-stars t
  "Non-nil means setting the variable `org-indent-mode' will \
turn on `org-hide-leading-stars'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :group 'org-indent
  :type 'integer)

(defface org-indent
  (org-compatible-face nil nil)
  "Face for outline indentation.
The default is to make it look like whitespace.  But you may find it
useful to make it ever so slightly different."
  :group 'org-faces)

(defun org-indent-initialize ()
  "Initialize the indentation strings."
  (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  (setq org-indent-stars (make-vector (1+ org-indent-max) nil))
  (aset org-indent-strings 0 nil)
  (aset org-indent-stars 0 nil)
  (loop for i from 1 to org-indent-max do
	(aset org-indent-strings i
	      (org-add-props
		  (concat (make-string (1- i) ?\ )
			  (char-to-string org-indent-boundary-char))
		  nil 'face 'org-indent)))
  (loop for i from 1 to org-indent-max-levels do
	(aset org-indent-stars i
	      (org-add-props (make-string i ?*)
		  nil 'face 'org-hide))))

(defsubst org-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

;;;###autoload
(define-minor-mode org-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time." nil " Ind" nil
  (cond
   ((org-bound-and-true-p org-inhibit-startup)
    (setq org-indent-mode nil))
   ((and org-indent-mode (featurep 'xemacs))
    (message "org-indent-mode does not work in XEmacs - refusing to turn it on")
    (setq org-indent-mode nil))
   ((and org-indent-mode
	 (not (org-version-check "23.1.50" "Org Indent mode" :predicate)))
    (message "org-indent-mode can crash Emacs 23.1 - refusing to turn it on!")
    (ding)
    (sit-for 1)
    (setq org-indent-mode nil))
   (org-indent-mode
    ;; mode was turned on.
    (org-set-local 'indent-tabs-mode nil)
    (or org-indent-strings (org-indent-initialize))
    (org-set-local 'org-indent-initial-marker (copy-marker 1))
    (when org-indent-mode-turns-off-org-adapt-indentation
      (org-set-local 'org-adapt-indentation nil))
    (when org-indent-mode-turns-on-hiding-stars
      (org-set-local 'org-hide-leading-stars-before-indent-mode
		     org-hide-leading-stars)
      (org-set-local 'org-hide-leading-stars t))
    (make-local-variable 'buffer-substring-filters)
    (add-to-list 'buffer-substring-filters
		 'org-indent-remove-properties-from-string)
    (org-add-hook 'after-change-functions 'org-indent-refresh-maybe nil 'local)
    (org-add-hook 'before-change-functions
		  'org-indent-notify-modified-headline nil 'local)
    (and font-lock-mode (org-restart-font-lock))
    (org-indent-remove-properties (point-min) (point-max))
    ;; Submit current buffer to initialize agent.  If it's the first
    ;; buffer submitted, also start the agent.  Current buffer is
    ;; pushed in both cases to avoid a race condition.
    (if org-indent-agentized-buffers
	(push (current-buffer) org-indent-agentized-buffers)
      (push (current-buffer) org-indent-agentized-buffers)
      (setq org-indent-agent-timer
	    (run-with-idle-timer 0.2 t #'org-indent-initialize-agent))))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (kill-local-variable 'org-adapt-indentation)
    (setq org-indent-agentized-buffers
	  (delq (current-buffer) org-indent-agentized-buffers))
    (when (markerp org-indent-initial-marker)
      (set-marker org-indent-initial-marker nil))
    (when (boundp 'org-hide-leading-stars-before-indent-mode)
      (org-set-local 'org-hide-leading-stars
		     org-hide-leading-stars-before-indent-mode))
    (setq buffer-substring-filters
	  (delq 'org-indent-remove-properties-from-string
		buffer-substring-filters))
    (remove-hook 'after-change-functions 'org-indent-refresh-maybe 'local)
    (remove-hook 'before-change-functions
		 'org-indent-notify-modified-headline 'local)
    (org-with-wide-buffer
     (org-indent-remove-properties (point-min) (point-max)))
    (and font-lock-mode (org-restart-font-lock))
    (redraw-display))))

(defun org-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (error "Not in Org mode")
    (message "Setting buffer indentation. It may take a few seconds...")
    (org-indent-remove-properties (point-min) (point-max))
    (org-indent-add-properties (point-min) (point-max))
    (message "Indentation of buffer set.")))

(defun org-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
			  '(line-prefix nil wrap-prefix nil) string)
  string)

(defun org-indent-initialize-agent ()
  "Start or resume current buffer initialization.
Only buffers in `org-indent-agentized-buffers' trigger an action.
When no more buffer is being watched, the agent suppress itself."
  (when org-indent-agent-resume-timer
    (cancel-timer org-indent-agent-resume-timer))
  (setq org-indent-agentized-buffers
	(org-remove-if-not #'buffer-live-p org-indent-agentized-buffers))
  (cond
   ;; Job done:  kill agent.
   ((not org-indent-agentized-buffers) (cancel-timer org-indent-agent-timer))
   ;; Current buffer is agentized: start/resume initialization
   ;; somewhat aggressively.
   ((memq (current-buffer) org-indent-agentized-buffers)
    (org-indent-initialize-buffer (current-buffer)
				  org-indent-agent-active-delay))
   ;; Else, start/resume initialization of the last agentized buffer,
   ;; softly.
   (t (org-indent-initialize-buffer (car org-indent-agentized-buffers)
				    org-indent-agent-passive-delay))))

(defun org-indent-initialize-buffer (buffer delay)
  "Set virtual indentation for the buffer BUFFER, asynchronously.
Give hand to other idle processes if it takes longer than DELAY,
a time value."
  (with-current-buffer buffer
    (when org-indent-mode
      (org-with-wide-buffer
       (let ((interruptp
	      ;; Always nil unless interrupted.
	      (catch 'interrupt
		(and org-indent-initial-marker
		     (marker-position org-indent-initial-marker)
		     (org-indent-add-properties org-indent-initial-marker
						(point-max)
						delay)
		     nil))))
	 (move-marker org-indent-initial-marker interruptp)
	 ;; Job is complete: un-agentize buffer.
	 (unless interruptp
	   (setq org-indent-agentized-buffers
		 (delq buffer org-indent-agentized-buffers))))))))

(defsubst org-indent-set-line-properties (l w h)
  "Set prefix properties on current line an move to next one.

Prefix properties `line-prefix' and `wrap-prefix' in current line
are set to, respectively, length L and W.

If H is non-nil, `line-prefix' will be starred.  If H is
`inline', the first star will have `org-warning' face.

Assume point is at beginning of line."
  (let ((line (cond
	       ((eq 'inline h)
		(let ((stars (aref org-indent-stars
				   (min l org-indent-max-levels))))
		  (and stars
		       (concat org-indent-inlinetask-first-star
			       (substring stars 1)))))
	       (h (aref org-indent-stars
			(min l org-indent-max-levels)))
	       (t (aref org-indent-strings
			(min l org-indent-max)))))
	(wrap (aref org-indent-strings (min w org-indent-max))))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (point) (min (1+ (point-at-eol)) (point-max))
			 `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line 1))

(defun org-indent-add-properties (beg end &optional delay)
  "Add indentation properties between BEG and END.

When DELAY is non-nil, it must be a time value.  In that case,
the process is asynchronous and can be interrupted, either by
user request, or after DELAY.  This is done by throwing the
`interrupt' tag along with the buffer position where the process
stopped."
  (save-match-data
    (org-with-wide-buffer
     (goto-char beg)
     (beginning-of-line)
     ;; 1. Initialize prefix at BEG.  This is done by storing two
     ;;    variables: INLINE-PF and PF, representing respectively
     ;;    length of current `line-prefix' when line is inside an
     ;;    inline task or not.
     (let* ((case-fold-search t)
	    (limited-re (org-get-limited-outline-regexp))
	    (added-ind-per-lvl (1- org-indent-indentation-per-level))
	    (pf (save-excursion
		  (and (ignore-errors (let ((outline-regexp limited-re))
					(org-back-to-heading t)))
		       (+ (* org-indent-indentation-per-level
			     (- (match-end 0) (match-beginning 0) 2)) 2))))
	    (pf-inline (and (featurep 'org-inlinetask)
			    (org-inlinetask-in-task-p)
			    (+ (* org-indent-indentation-per-level
				  (1- (org-inlinetask-get-task-level))) 2)))
	    (time-limit (and delay (time-add (current-time) delay))))
       ;; 2. For each line, set `line-prefix' and `wrap-prefix'
       ;;    properties depending on the type of line (headline,
       ;;    inline task, item or other).
       (with-silent-modifications
	 (while (and (<= (point) end) (not (eobp)))
	   (cond
	    ;; When in asynchronous mode, check if interrupt is
	    ;; required.
	    ((and delay (input-pending-p)) (throw 'interrupt (point)))
	    ;; In asynchronous mode, take a break of
	    ;; `org-indent-agent-resume-delay' every DELAY to avoid
	    ;; blocking any other idle timer or process output.
	    ((and delay (time-less-p time-limit (current-time)))
	     (setq org-indent-agent-resume-timer
		   (run-with-idle-timer
		    (time-add (current-idle-time)
			      org-indent-agent-resume-delay)
		    nil #'org-indent-initialize-agent))
	     (throw 'interrupt (point)))
	    ;; Headline or inline task.
	    ((looking-at org-outline-regexp)
	     (let* ((nstars (- (match-end 0) (match-beginning 0) 1))
		    (line (* added-ind-per-lvl (1- nstars)))
		    (wrap (+ line (1+ nstars))))
	       (cond
		;; Headline: new value for PF.
		((looking-at limited-re)
		 (org-indent-set-line-properties line wrap t)
		 (setq pf wrap))
		;; End of inline task: PF-INLINE is now nil.
		((looking-at "\\*+ end[ \t]*$")
		 (org-indent-set-line-properties line wrap 'inline)
		 (setq pf-inline nil))
		;; Start of inline task.  Determine if it contains
		;; text, or if it is only one line long.  Set
		;; PF-INLINE accordingly.
		(t (org-indent-set-line-properties line wrap 'inline)
		   (setq pf-inline (and (org-inlinetask-in-task-p) wrap))))))
	    ;; List item: `wrap-prefix' is set where body starts.
	    ((org-at-item-p)
	     (let* ((line (or pf-inline pf 0))
		    (wrap (+ (org-list-item-body-column (point)) line)))
	       (org-indent-set-line-properties line wrap nil)))
	    ;; Normal line: use PF-INLINE, PF or nil as prefixes.
	    (t (let* ((line (or pf-inline pf 0))
		      (wrap (+ line (org-get-indentation))))
		 (org-indent-set-line-properties line wrap nil))))))))))

(defun org-indent-notify-modified-headline (beg end)
  "Set `org-indent-modified-headline-flag' depending on context.

BEG and END are the positions of the beginning and end of the
range of deleted text.

This function is meant to be called by `before-change-functions'.
Flag will be non-nil if command is going to modify or delete an
headline."
  (when org-indent-mode
    (setq org-indent-modified-headline-flag
	  (save-excursion
	    (goto-char beg)
	    (save-match-data
	      (or (and (org-at-heading-p) (< beg (match-end 0)))
		  (re-search-forward org-outline-regexp-bol end t)))))))

(defun org-indent-refresh-maybe (beg end dummy)
  "Refresh indentation properties in an adequate portion of buffer.
BEG and END are the positions of the beginning and end of the
range of inserted text.  DUMMY is an unused argument.

This function is meant to be called by `after-change-functions'."
  (when org-indent-mode
    (save-match-data
      ;; If an headline was modified or inserted, set properties until
      ;; next headline.
      (if (or org-indent-modified-headline-flag
	      (save-excursion
		(goto-char beg)
		(beginning-of-line)
		(re-search-forward org-outline-regexp-bol end t)))
	(let ((end (save-excursion
		     (goto-char end)
		     (org-with-limited-levels (outline-next-heading))
		     (point))))
	  (setq org-indent-modified-headline-flag nil)
	  (org-indent-add-properties beg end))
	;; Otherwise, only set properties on modified area.
	(org-indent-add-properties beg end)))))

(provide 'org-indent)

;;; org-indent.el ends here
