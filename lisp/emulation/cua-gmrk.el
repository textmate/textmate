;;; cua-gmrk.el --- CUA unified global mark support

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard emulations convenience cua mark
;; Package: cua-base

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

(eval-when-compile
  (require 'cua-base)
  (require 'cua-rect)
  )

;;; Global Marker

;; Non-nil when global marker is active.
(defvar cua--global-mark-active nil)

;; Global mark position marker.
(defvar cua--global-mark-marker nil)

;; Overlay for global mark position.
(defvar cua--global-mark-overlay nil)

;; Initialize global mark things once...
(defvar cua--global-mark-initialized nil)

;; Saved configured blink-cursor-interval
(defvar cua--orig-blink-cursor-interval nil)

(defun cua--deactivate-global-mark (&optional msg)
  (when cua--global-mark-overlay
    (delete-overlay cua--global-mark-overlay)
    (setq cua--global-mark-overlay nil))
  (if (markerp cua--global-mark-marker)
      (move-marker cua--global-mark-marker nil))
  (if cua--orig-blink-cursor-interval
      (setq blink-cursor-interval cua--orig-blink-cursor-interval
	    cua--orig-blink-cursor-interval nil))
  (setq cua--global-mark-active nil)
  (if msg
      (message "Global Mark Cleared")))

(defun cua--activate-global-mark (&optional msg)
  (if (not (markerp cua--global-mark-marker))
      (setq cua--global-mark-marker (make-marker)))
  (when (eobp)
    (insert " ")
    (backward-char 1))
  (move-marker cua--global-mark-marker (point))
  (if (overlayp cua--global-mark-overlay)
      (move-overlay cua--global-mark-overlay (point) (1+ (point)))
    (setq cua--global-mark-overlay
	  (make-overlay (point) (1+ (point))))
    (overlay-put cua--global-mark-overlay 'face 'cua-global-mark))
  (if (and cua-global-mark-blink-cursor-interval
	   (not cua--orig-blink-cursor-interval))
      (setq cua--orig-blink-cursor-interval blink-cursor-interval
	    blink-cursor-interval cua-global-mark-blink-cursor-interval))
  (setq cua--global-mark-active t)
  (if msg
      (message "Global Mark Set")))

(defun cua--global-mark-active ()
  (if cua--global-mark-active
      (or (and (markerp cua--global-mark-marker)
	       (marker-buffer cua--global-mark-marker))
	  (and (cua--deactivate-global-mark nil)
	       nil))))

(defun cua-toggle-global-mark (stay)
  "Set or cancel the global marker.
When the global marker is set, CUA cut and copy commands will automatically
insert the deleted or copied text before the global marker, even when the
global marker is in another buffer.
If the global marker isn't set, set the global marker at point in the current
buffer.  Otherwise jump to the global marker position and cancel it.
With prefix argument, don't jump to global mark when canceling it."
  (interactive "P")
  (unless cua--global-mark-initialized
    (cua--init-global-mark))
  (if (not (cua--global-mark-active))
      (if (not buffer-read-only)
	  (cua--activate-global-mark t)
	(ding)
	(message "Cannot set global mark in read-only buffer"))
    (when (not stay)
      (pop-to-buffer (marker-buffer cua--global-mark-marker))
      (goto-char cua--global-mark-marker))
    (cua--deactivate-global-mark t)))

(defun cua--insert-at-global-mark (str &optional msg)
  ;; Insert string at global marker and move marker
  (with-current-buffer (marker-buffer cua--global-mark-marker)
    (goto-char (marker-position cua--global-mark-marker))
    (insert-for-yank str)
    (cua--activate-global-mark))
  (if msg
      (message "%s %d to global mark in %s:%d" msg
	       (length str)
	       (buffer-name (marker-buffer cua--global-mark-marker))
	       (marker-position cua--global-mark-marker))))

(defun cua--delete-at-global-mark (arg &optional msg)
  ;; Delete chars at global marker
  (with-current-buffer (marker-buffer cua--global-mark-marker)
    (goto-char (marker-position cua--global-mark-marker))
    (delete-char arg))
  (if msg
      (message "%s %d chars at global mark in %s:%d" msg arg
	       (buffer-name (marker-buffer cua--global-mark-marker))
	       (marker-position cua--global-mark-marker))))

(defun cua-copy-region-to-global-mark (start end)
  "Copy region to global mark buffer/position."
  (interactive "r")
  (if (cua--global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer cua--global-mark-marker) src-buf)
	      (let ((text (cua--filter-buffer-noprops start end)))
		(goto-char (marker-position cua--global-mark-marker))
		(insert text))
	    (set-buffer (marker-buffer cua--global-mark-marker))
	    (goto-char (marker-position cua--global-mark-marker))
	    (insert-buffer-substring-as-yank src-buf start end))
	  (cua--activate-global-mark)
	  (message "Copied %d to global mark in %s:%d"
		   (abs (- end start))
		   (buffer-name (marker-buffer cua--global-mark-marker))
		   (marker-position cua--global-mark-marker))))
    (cua--deactivate-global-mark)
    (message "No Global Mark")))

(defun cua-cut-region-to-global-mark (start end)
  "Move region to global buffer/position."
  (interactive "r")
  (if (cua--global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer cua--global-mark-marker) src-buf)
	      (if (and (< start (marker-position cua--global-mark-marker))
		       (< (marker-position cua--global-mark-marker) end))
		  (message "Can't move region into itself")
		(let ((text (cua--filter-buffer-noprops start end))
		      (p1 (copy-marker start))
		      (p2 (copy-marker end)))
		  (goto-char (marker-position cua--global-mark-marker))
		  (insert text)
		  (cua--activate-global-mark)
		  (delete-region (marker-position p1) (marker-position p2))
		  (move-marker p1 nil)
		  (move-marker p2 nil)))
	    (set-buffer (marker-buffer cua--global-mark-marker))
	    (goto-char (marker-position cua--global-mark-marker))
	    (insert-buffer-substring src-buf start end)
	    (message "Moved %d to global mark in %s:%d"
		     (abs (- end start))
		     (buffer-name (marker-buffer cua--global-mark-marker))
		     (marker-position cua--global-mark-marker))
	    (cua--activate-global-mark)
	    (set-buffer src-buf)
	    (delete-region start end))))
    (cua--deactivate-global-mark)
    (message "No Global Mark")))

(defun cua--copy-rectangle-to-global-mark (as-text)
  ;; Copy rectangle to global mark buffer/position.
  (if (cua--global-mark-active)
      (let ((src-buf (current-buffer))
	    (text (cua--extract-rectangle)))
	(with-current-buffer (marker-buffer cua--global-mark-marker)
	  (goto-char (marker-position cua--global-mark-marker))
	  (if as-text
	      (while text
		(insert-for-yank (car text))
		(if (setq text (cdr text))
		    (insert "\n")))
	    (cua--insert-rectangle text 'auto))
	  (cua--activate-global-mark)
	  (message "Copied rectangle to global mark in %s:%d"
		   (buffer-name (marker-buffer cua--global-mark-marker))
		   (marker-position cua--global-mark-marker))))
    (cua--deactivate-global-mark)
    (message "No Global Mark")))

(defun cua--cut-rectangle-to-global-mark (as-text)
  ;; Move rectangle to global buffer/position.
  (if (cua--global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer cua--global-mark-marker) src-buf)
	      (let ((olist (overlays-at (marker-position cua--global-mark-marker)))
		    in-rect)
		(while olist
		  (if (eq (overlay-get (car olist) 'face) 'cua-rectangle)
		      (setq in-rect t olist nil)
		    (setq olist (cdr olist))))
		(if in-rect
		    (message "Can't move rectangle into itself")
		  (let ((text (cua--extract-rectangle)))
		    (cua--delete-rectangle)
		    (goto-char (marker-position cua--global-mark-marker))
		    (if as-text
			(while text
			  (insert-for-yank (car text))
			  (if (setq text (cdr text))
			      (insert "\n")))
		      (cua--insert-rectangle text 'auto))
		    (cua--activate-global-mark))))
	    (let ((text (cua--extract-rectangle)))
	      (cua--delete-rectangle)
	      (set-buffer (marker-buffer cua--global-mark-marker))
	      (goto-char (marker-position cua--global-mark-marker))
	      (cua--insert-rectangle text 'auto))
	    (message "Moved rectangle to global mark in %s:%d"
		     (buffer-name (marker-buffer cua--global-mark-marker))
		     (marker-position cua--global-mark-marker))
	    (cua--activate-global-mark))))
    (cua--deactivate-global-mark)
    (message "No Global Mark")))

(defun cua-copy-to-global-mark ()
  "Copy active region/rectangle to global mark buffer/position."
  (interactive)
  (setq cua--last-killed-rectangle nil)
  (if cua--rectangle
      (cua--copy-rectangle-to-global-mark nil)
    (let ((start (mark)) (end (point)))
      (or (<= start end)
	  (setq start (prog1 end (setq end start))))
      (cua-copy-region-to-global-mark start end))))

(defun cua-copy-next-to-global-mark (n)
  "Copy the following N characters in buffer to global mark buffer/position."
  (interactive "p")
  (setq cua--last-killed-rectangle nil)
  (or (eobp)
      (let ((p (point)))
	(goto-char (+ p n))
	(cua-copy-region-to-global-mark p (point)))))

(defun cua-cut-to-global-mark ()
  "Move active region/rectangle to global mark buffer/position."
  (interactive)
  (if buffer-read-only
      (cua-copy-to-global-mark)
    (setq cua--last-killed-rectangle nil)
    (if cua--rectangle
	(cua--cut-rectangle-to-global-mark nil)
      (let ((start (mark)) (end (point)))
	(or (<= start end)
	    (setq start (prog1 end (setq end start))))
	(cua-cut-region-to-global-mark start end)))))

(defun cua-cut-next-to-global-mark (n)
  "Move the following N characters in buffer to global mark buffer/position."
  (interactive "p")
  (setq cua--last-killed-rectangle nil)
  (or (eobp)
      (let ((p (point)))
	(goto-char (+ p n))
	(cua-cut-region-to-global-mark p (point)))))

(defun cua-delete-char-at-global-mark (arg)
  "Delete character following the global mark position."
  (interactive "p")
  (cua--delete-at-global-mark arg "Deleted"))

(defun cua-delete-backward-char-at-global-mark (arg)
  "Delete character before the global mark position."
  (interactive "p")
  (cua--delete-at-global-mark (- arg) "Deleted backward"))

(defun cua-insert-char-at-global-mark ()
  "Insert the character you type at the global mark position."
  (interactive)
  (cua--insert-at-global-mark (char-to-string (aref (this-single-command-keys) 0)) "Inserted"))

(defun cua-insert-newline-at-global-mark ()
  "Insert a newline at the global mark position."
  (interactive)
  (cua--insert-at-global-mark "\n"))

(defun cua-indent-to-global-mark-column ()
  "Indent current line or rectangle to global mark column."
  (interactive "*")
  (if (cua--global-mark-active)
      (let (col)
	(with-current-buffer (marker-buffer cua--global-mark-marker)
	  (goto-char (marker-position cua--global-mark-marker))
	  (setq col (current-column)))
	(if cua--rectangle
	    (cua--indent-rectangle nil col t)
	  (indent-to col))
	(if (eq (current-buffer) (marker-buffer cua--global-mark-marker))
	    (save-excursion
	      (goto-char (marker-position cua--global-mark-marker))
	      (move-to-column col)
	      (move-marker cua--global-mark-marker (point))
	      (move-overlay cua--global-mark-overlay (point) (1+ (point))))))))


(defun cua-cancel-global-mark ()
  "Cancel the global mark."
  (interactive)
  (if mark-active
      (cua-cancel)
    (if (cua--global-mark-active)
	(cua--deactivate-global-mark t)))
  (cua--fallback))

;;; Post-command hook for global mark.

(defun cua--global-mark-post-command ()
  (when (and (cua--global-mark-active) ;; Updates cua--global-mark-active variable
	     cua-global-mark-keep-visible)
    ;; keep global mark position visible
    (sit-for 0)
    (if (or (not (eq (current-buffer) (marker-buffer cua--global-mark-marker)))
	    (not (pos-visible-in-window-p (marker-position cua--global-mark-marker))))
	(let ((w (selected-window)) (p (point)) h)
	  ;; The following code is an attempt to keep the global mark visible in
	  ;; other window -- but it doesn't work.
	  (switch-to-buffer-other-window (marker-buffer cua--global-mark-marker) t)
	  (goto-char (marker-position cua--global-mark-marker))
	  (if (not (pos-visible-in-window-p (marker-position cua--global-mark-marker)))
	      (recenter (if (> (setq h (- (window-height) 4)) 1) h '(4))))
	  (select-window w)
	  (goto-char p)))))

;;; Initialization

(defun cua--init-global-mark ()
  (define-key cua--global-mark-keymap [remap copy-region-as-kill]	'cua-copy-to-global-mark)
  (define-key cua--global-mark-keymap [remap kill-ring-save]		'cua-copy-to-global-mark)
  (define-key cua--global-mark-keymap [remap kill-region]		'cua-cut-to-global-mark)
  (define-key cua--global-mark-keymap [remap yank]			'cua-copy-next-to-global-mark)

  (define-key cua--global-mark-keymap [remap keyboard-escape-quit]	'cua-cancel-global-mark)
  (define-key cua--global-mark-keymap [remap keyboard-quit]		'cua-cancel-global-mark)

  (define-key cua--global-mark-keymap [(control ?d)]			'cua-cut-next-to-global-mark)
  (define-key cua--global-mark-keymap [remap delete-backward-char]	'cua-delete-backward-char-at-global-mark)
  (define-key cua--global-mark-keymap [remap backward-delete-char]	'cua-delete-backward-char-at-global-mark)
  (define-key cua--global-mark-keymap [remap backward-delete-char-untabify] 'cua-delete-backward-char-at-global-mark)
  (define-key cua--global-mark-keymap [remap self-insert-command]	'cua-insert-char-at-global-mark)
  (define-key cua--global-mark-keymap [remap self-insert-iso]		'cua-insert-char-at-global-mark)

  ;; Catch self-inserting characters which are "stolen" by other modes
  (define-key cua--global-mark-keymap [t]
    '(menu-item "sic" cua-insert-char-at-global-mark :filter cua--self-insert-char-p))

  (define-key cua--global-mark-keymap [remap newline]			'cua-insert-newline-at-global-mark)
  (define-key cua--global-mark-keymap [remap newline-and-indent]	'cua-insert-newline-at-global-mark)
  (define-key cua--global-mark-keymap "\r"				'cua-insert-newline-at-global-mark)

  (define-key cua--global-mark-keymap "\t"				'cua-indent-to-global-mark-column)

  (setq cua--global-mark-initialized t))

(provide 'cua-gmrk)

;;; cua-gmrk.el ends here
