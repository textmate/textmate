;;; calc-trail.el --- functions for manipulating the Calc "trail"

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Trail commands.

(defun calc-trail-in ()
  (interactive)
  (let ((win (get-buffer-window (calc-trail-display t))))
    (and win (select-window win))))

(defun calc-trail-out ()
  (interactive)
  (calc-select-buffer)
  (let ((win (get-buffer-window (current-buffer))))
    (if win
	(progn
	  (select-window win)
	  (calc-align-stack-window))
      (calc))))

(defun calc-trail-next (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line n)
   (calc-trail-here)))

(defun calc-trail-previous (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (- n))
   (calc-trail-here)))

(defun calc-trail-first (n)
  (interactive "p")
  (calc-with-trail-buffer
   (goto-char (point-min))
   (forward-line n)
   (calc-trail-here)))

(defun calc-trail-last (n)
  (interactive "p")
  (calc-with-trail-buffer
   (goto-char (point-max))
   (forward-line (- n))
   (calc-trail-here)))

(defun calc-trail-scroll-left (n)
  (interactive "P")
  (let ((curwin (selected-window)))
    (calc-with-trail-buffer
     (unwind-protect
	 (progn
	   (select-window (get-buffer-window (current-buffer)))
	   (calc-scroll-left n))
       (select-window curwin)))))

(defun calc-trail-scroll-right (n)
  (interactive "P")
  (let ((curwin (selected-window)))
    (calc-with-trail-buffer
     (unwind-protect
	 (progn
	   (select-window (get-buffer-window (current-buffer)))
	   (calc-scroll-right n))
       (select-window curwin)))))

(defun calc-trail-forward (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (* n (1- (window-height))))
   (calc-trail-here)))

(defun calc-trail-backward (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (- (* n (1- (window-height)))))
   (calc-trail-here)))

(defun calc-trail-isearch-forward ()
  (interactive)
  (calc-with-trail-buffer
   (let ((win (get-buffer-window (current-buffer)))
         pos)
     (save-window-excursion
       (select-window win)
       (isearch-forward)
       (setq pos (point)))
     (goto-char pos)
     (set-window-point win pos)
     (calc-trail-here))))

(defun calc-trail-isearch-backward ()
  (interactive)
  (calc-with-trail-buffer
   (let ((win (get-buffer-window (current-buffer)))
         pos)
     (save-window-excursion
       (select-window win)
       (isearch-backward)
       (setq pos (point)))
     (goto-char pos)
     (set-window-point win pos)
     (calc-trail-here))))

(defun calc-trail-yank (arg)
  (interactive "P")
  (calc-wrapper
   (or arg (calc-set-command-flag 'hold-trail))
   (calc-enter-result 0 "yank"
		      (calc-with-trail-buffer
		       (if arg
			   (forward-line (- (prefix-numeric-value arg))))
		       (if (or (looking-at "Emacs Calc")
			       (looking-at "----")
			       (looking-at " ? ? ?[^ \n]* *$")
			       (looking-at "..?.?$"))
			   (error "Can't yank that line"))
		       (if (looking-at ".*, \\.\\.\\., ")
			   (error "Can't yank (vector was abbreviated)"))
		       (forward-char 4)
		       (search-forward " ")
		       (let* ((next (save-excursion (forward-line 1) (point)))
			      (str (buffer-substring (point) (1- next)))
			      (val (with-current-buffer save-buf
				     (math-read-plain-expr str))))
			 (if (eq (car-safe val) 'error)
			     (error "Can't yank that line: %s" (nth 2 val))
			   val))))))

(defun calc-trail-marker (str)
  (interactive "sText to insert in trail: ")
  (calc-with-trail-buffer
   (forward-line 1)
   (let ((buffer-read-only nil))
     (insert "---- " str "\n"))
   (forward-line -1)
   (calc-trail-here)))

(defun calc-trail-kill (n)
  (interactive "p")
  (calc-with-trail-buffer
   (let ((buffer-read-only nil))
     (save-restriction
       (narrow-to-region   ; don't delete "Emacs Trail" header
	(save-excursion
	  (goto-char (point-min))
	  (forward-line 1)
	  (point))
	(point-max))
       (kill-line n)))
   (calc-trail-here)))

(provide 'calc-trail)

;;; calc-trail.el ends here
