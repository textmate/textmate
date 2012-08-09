;;; em-smart.el --- smart display of output

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; The best way to get a sense of what this code is trying to do is by
;; using it.  Basically, the philosophy represents a blend between the
;; ease of use of modern day shells, and the review-before-you-proceed
;; mentality of Plan 9's 9term.
;;
;; @ When you invoke a command, it is assumed that you want to read
;;   the output of that command.
;;
;; @ If the output is not what you wanted, it is assumed that you will
;;   want to edit, and then resubmit a refined version of that
;;   command.
;;
;; @ If the output is valid, pressing any self-inserting character key
;;   will jump to end of the buffer and insert that character, in
;;   order to begin entry of a new command.
;;
;; @ If you show an intention to edit the previous command -- by
;;   moving around within it -- then the next self-inserting
;;   characters will insert *there*, instead of at the bottom of the
;;   buffer.
;;
;; @ If you show an intention to review old commands, such as M-p or
;;   M-r, point will jump to the bottom of the buffer before invoking
;;   that command.
;;
;; @ If none of the above has happened yet (i.e., your point is just
;;   sitting on the previous command), you can use SPACE and BACKSPACE
;;   (or DELETE) to page forward and backward *through the output of
;;   the last command only*.  It will constrain the movement of the
;;   point and window so that the maximum amount of output is always
;;   displayed at all times.
;;
;; @ While output is being generated from a command, the window will
;;   be constantly reconfigured (until it would otherwise make no
;;   difference) in order to always show you the most output from the
;;   command possible.  This happens if you change window sizes,
;;   scroll, etc.
;;
;; @ Like I said, it's not really comprehensible until you try it! ;)
;;
;; One disadvantage of this module is that it increases Eshell's
;; memory consumption by a factor of two or more.  With small commands
;; (such as pwd), where the screen is mostly full, consumption can
;; increase by orders of magnitude.

;;; Code:

(eval-when-compile (require 'eshell))

;;;###autoload
(eshell-defgroup eshell-smart nil
  "This module combines the facility of normal, modern shells with
some of the edit/review concepts inherent in the design of Plan 9's
9term.  See the docs for more details.

Most likely you will have to turn this option on and play around with
it to get a real sense of how it works."
  :tag "Smart display of output"
  ;; :link '(info-link "(eshell)Smart display of output")
  :group 'eshell-module)

;;; User Variables:

(defcustom eshell-smart-load-hook nil
  "A list of functions to call when loading `eshell-smart'."
  :version "24.1"			; removed eshell-smart-initialize
  :type 'hook
  :group 'eshell-smart)

(defcustom eshell-smart-unload-hook
  (list
   (function
    (lambda ()
      (remove-hook 'window-configuration-change-hook
		   'eshell-refresh-windows))))
  "A hook that gets run when `eshell-smart' is unloaded."
  :type 'hook
  :group 'eshell-smart)

(defcustom eshell-review-quick-commands nil
  "If t, always review commands.
Reviewing means keeping point on the text of the command that was just
invoked, to allow corrections to be made easily.

If set to nil, quick commands won't be reviewed.  A quick command is a
command that produces no output, and exits successfully.

If set to `not-even-short-output', then the definition of \"quick
command\" is extended to include commands that produce output, if and
only if that output can be presented in its entirely in the Eshell window."
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (const :tag "Not even short output"
			not-even-short-output))
  :group 'eshell-smart)

(defcustom eshell-smart-display-navigate-list
  '(insert-parentheses
    mouse-yank-at-click
    mouse-yank-primary
    mouse-yank-secondary
    yank-pop
    yank-rectangle
    yank)
  "A list of commands which cause Eshell to jump to the end of buffer."
  :type '(repeat function)
  :group 'eshell-smart)

(defcustom eshell-smart-space-goes-to-end t
  "If non-nil, space will go to end of buffer when point-max is visible.
That is, if a command is running and the user presses SPACE at a time
when the end of the buffer is visible, point will go to the end of the
buffer and smart-display will be turned off (that is, subsequently
pressing backspace will not cause the buffer to scroll down).

This feature is provided to make it very easy to watch the output of a
long-running command, such as make, where it's more desirable to see
the output go by than to review it afterward.

Setting this variable to nil means that space and backspace will
always have a consistent behavior, which is to move back and forth
through displayed output.  But it also means that enabling output
tracking requires the user to manually move point to the end of the
buffer using \\[end-of-buffer]."
  :type 'boolean
  :group 'eshell-smart)

(defcustom eshell-where-to-jump 'begin
  "This variable indicates where point should jump to after a command.
The options are `begin', `after' or `end'."
  :type '(radio (const :tag "Beginning of command" begin)
		(const :tag "After command word" after)
		(const :tag "End of command" end))
  :group 'eshell-smart)

;;; Internal Variables:

(defvar eshell-smart-displayed nil)
(defvar eshell-smart-command-done nil)
(defvar eshell-currently-handling-window nil)

;;; Functions:

(defun eshell-smart-initialize ()
  "Setup Eshell smart display."
  (unless eshell-non-interactive-p
    ;; override a few variables, since they would interfere with the
    ;; smart display functionality.
    (set (make-local-variable 'eshell-scroll-to-bottom-on-output) nil)
    (set (make-local-variable 'eshell-scroll-to-bottom-on-input) nil)
    (set (make-local-variable 'eshell-scroll-show-maximum-output) t)

    (add-hook 'window-scroll-functions 'eshell-smart-scroll-window nil t)
    (add-hook 'window-configuration-change-hook 'eshell-refresh-windows)

    (add-hook 'eshell-output-filter-functions 'eshell-refresh-windows t t)

    (add-hook 'after-change-functions 'eshell-disable-after-change nil t)

    (add-hook 'eshell-input-filter-functions 'eshell-smart-display-setup nil t)

    (make-local-variable 'eshell-smart-command-done)
    (add-hook 'eshell-post-command-hook
	      (function
	       (lambda ()
		 (setq eshell-smart-command-done t))) t t)

    (unless (eq eshell-review-quick-commands t)
      (add-hook 'eshell-post-command-hook
		'eshell-smart-maybe-jump-to-end nil t))))

(defun eshell-smart-scroll-window (wind start)
  "Scroll the given Eshell window accordingly."
  (unless eshell-currently-handling-window
    (let ((inhibit-point-motion-hooks t)
	  (eshell-currently-handling-window t))
      (save-selected-window
	(select-window wind)
	(eshell-smart-redisplay)))))

(defun eshell-refresh-windows (&optional frame)
  "Refresh all visible Eshell buffers."
  (let (affected)
    (walk-windows
     (function
      (lambda (wind)
	(with-current-buffer (window-buffer wind)
	  (if eshell-mode
	      (let (window-scroll-functions)
		(eshell-smart-scroll-window wind (window-start))
		(setq affected t))))))
     0 frame)
    (if affected
	(let (window-scroll-functions)
	  (eshell-redisplay)))))

(defun eshell-smart-display-setup ()
  "Set the point to somewhere in the beginning of the last command."
  (cond
   ((eq eshell-where-to-jump 'begin)
    (goto-char eshell-last-input-start))
   ((eq eshell-where-to-jump 'after)
    (goto-char (next-single-property-change
		eshell-last-input-start 'arg-end))
    (if (= (point) (- eshell-last-input-end 2))
	(forward-char)))
   ((eq eshell-where-to-jump 'end)
    (goto-char (1- eshell-last-input-end)))
   (t
    (error "Invalid value for `eshell-where-to-jump'")))
  (setq eshell-smart-command-done nil)
  (add-hook 'pre-command-hook 'eshell-smart-display-move nil t)
  (eshell-refresh-windows))

(defun eshell-disable-after-change (b e l)
  "Disable smart display mode if the buffer changes in any way."
  (when eshell-smart-command-done
    (remove-hook 'pre-command-hook 'eshell-smart-display-move t)
    (setq eshell-smart-command-done nil)))

(defun eshell-smart-maybe-jump-to-end ()
  "Jump to the end of the input buffer.
This is done whenever a command exits successfully and both the command
and the end of the buffer are still visible."
  (when (and (= eshell-last-command-status 0)
	     (if (eq eshell-review-quick-commands 'not-even-short-output)
		 (and (pos-visible-in-window-p (point-max))
		      (pos-visible-in-window-p eshell-last-input-start))
	       (= (count-lines eshell-last-input-end
			       eshell-last-output-end) 0)))
    (goto-char (point-max))
    (remove-hook 'pre-command-hook 'eshell-smart-display-move t)))

(defun eshell-smart-redisplay ()
  "Display as much output as possible, smartly."
  (if (eobp)
      (save-excursion
	(recenter -1)
	;; trigger the redisplay now, so that we catch any attempted
	;; point motion; this is to cover for a redisplay bug
	(eshell-redisplay))
    (let ((top-point (point)))
      (and (memq 'eshell-smart-display-move pre-command-hook)
	   (>= (point) eshell-last-input-start)
	   (< (point) eshell-last-input-end)
	   (set-window-start (selected-window)
			     (line-beginning-position) t))
      (if (pos-visible-in-window-p (point-max))
	  (save-excursion
	    (goto-char (point-max))
	    (recenter -1)
	    (unless (pos-visible-in-window-p top-point)
	      (goto-char top-point)
	      (set-window-start (selected-window)
				(line-beginning-position) t)))))))

(defun eshell-smart-goto-end ()
  "Like `end-of-buffer', but do not push a mark."
  (interactive)
  (goto-char (point-max)))

(defun eshell-smart-display-move ()
  "Handle self-inserting or movement commands intelligently."
  (let (clear)
    (if (or current-prefix-arg
	    (and (> (point) eshell-last-input-start)
		 (< (point) eshell-last-input-end))
	    (>= (point) eshell-last-output-end))
	(setq clear t)
      (cond
       ((eq this-command 'self-insert-command)
	(if (eq last-command-event ? )
	    (if (and eshell-smart-space-goes-to-end
		     eshell-current-command)
		(if (not (pos-visible-in-window-p (point-max)))
		    (setq this-command 'scroll-up)
		  (setq this-command 'eshell-smart-goto-end))
	      (setq this-command 'scroll-up))
	  (setq clear t)
	  (goto-char (point-max))))
       ((eq this-command 'delete-backward-char)
	(setq this-command 'ignore)
	(if (< (point) eshell-last-input-start)
	    (eshell-show-output)
	  (if (pos-visible-in-window-p eshell-last-input-start)
	      (progn
		(ignore-errors
		  (scroll-down))
		(eshell-show-output))
	    (scroll-down)
	    (if (pos-visible-in-window-p eshell-last-input-end)
		(eshell-show-output)))))
       ((or (memq this-command eshell-smart-display-navigate-list)
	    (and (eq this-command 'eshell-send-input)
		 (not (and (>= (point) eshell-last-input-start)
			   (< (point) eshell-last-input-end)))))
	(setq clear t)
	(goto-char (point-max)))))
    (if clear
	(remove-hook 'pre-command-hook 'eshell-smart-display-move t))))

(provide 'em-smart)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-smart.el ends here
