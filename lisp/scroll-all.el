;;; scroll-all.el --- scroll all buffers together minor mode

;; Copyright (C) 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Gary D. Foster <Gary.Foster@corp.sun.com>
;; Keywords: scroll crisp brief lock

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
;;    This mode allows multiple buffers to be 'locked' so that scrolling
;;    up or down lines in any buffer causes all the buffers to mirror
;;    the scrolling.  It hooks into the post-command-hook to check for
;;    potential scrolling commands and if we're locked, mirrors them in all
;;    windows.  This allows us to grab line-at-a-time scrolling as well as
;;    screen-at-a-time scrolling, and doesn't remap any of the keyboard
;;    commands to do it.

;; You can enable and disable this mode with the 'scroll-all-mode' command.

;; Suggestions/ideas from:
;;    Rick Macdonald <rickm@vsl.com>
;;    Anders Lindgren <andersl@csd.uu.se>

;;; Code:

(defun scroll-all-function-all (func arg)
  "Apply function FUNC with argument ARG to all visible windows."
  (let ((num-windows (count-windows))
	(count 1))
    (when (> num-windows 1)
      (other-window 1)
      (while (< count num-windows)
	(condition-case nil
	    (funcall func arg)
	  ;; Ignore beginning- or end-of-buffer error in other windows.
	  (error nil)
	  )
	(other-window 1)
	(setq count (1+ count))))))

(defun scroll-all-scroll-down-all (arg)
  "Scroll down in all visible windows."
  (interactive "p")
  (scroll-all-function-all 'next-line arg))

(defun scroll-all-scroll-up-all (arg)
  "Scroll up in all visible windows."
  (interactive "p")
  (scroll-all-function-all 'previous-line arg))

(defun scroll-all-page-down-all (arg)
  "Page down in all visible windows."
  (interactive "P")
  (scroll-all-function-all 'scroll-up arg))

(defun scroll-all-page-up-all (arg)
  "Page up in all visible windows."
  (interactive "P")
  (scroll-all-function-all 'scroll-down arg))

(defun scroll-all-beginning-of-buffer-all (arg)
  "Go to the beginning of the buffer in all visible windows."
  (interactive "P")
  (scroll-all-function-all 'beginning-of-buffer arg))

(defun scroll-all-end-of-buffer-all (arg)
  "Go to the end of the buffer in all visible windows."
  (interactive "P")
  (scroll-all-function-all 'end-of-buffer arg))


(defun scroll-all-check-to-scroll ()
  "Check `this-command' to see if a scroll is to be done."
  (cond ((eq this-command 'next-line)
	 (call-interactively 'scroll-all-scroll-down-all))
	((eq this-command 'previous-line)
	 (call-interactively 'scroll-all-scroll-up-all))
	((memq this-command '(scroll-up scroll-up-command))
	 (call-interactively 'scroll-all-page-down-all))
	((memq this-command '(scroll-down scroll-down-command))
	 (call-interactively 'scroll-all-page-up-all))
	((eq this-command 'beginning-of-buffer)
	 (call-interactively 'scroll-all-beginning-of-buffer-all))
	((eq this-command 'end-of-buffer)
	 (call-interactively 'scroll-all-end-of-buffer-all))))


;;;###autoload
(define-minor-mode scroll-all-mode
  "Toggle shared scrolling in same-frame windows (Scroll-All mode).
With a prefix argument ARG, enable Scroll-All mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Scroll-All mode is enabled, scrolling commands invoked in
one window apply to all visible windows in the same frame."
  nil " *SL*" nil
  :global t
  :group 'windows
  :group 'scrolling
  (if scroll-all-mode
      (add-hook 'post-command-hook 'scroll-all-check-to-scroll)
    (remove-hook 'post-command-hook 'scroll-all-check-to-scroll)))

(provide 'scroll-all)

;;; scroll-all.el ends here
