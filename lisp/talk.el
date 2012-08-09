;;; talk.el --- allow several users to talk to each other through Emacs

;; Copyright (C) 1995, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: comm, frames

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

;; This is a multi-user talk package that runs in Emacs.
;; Use talk-connect to bring a new person into the conversation.

;;; Code:

(defvar talk-display-alist nil
  "Alist of displays on which Emacs talk is now running.
Each element has the form (DISPLAY FRAME BUFFER).")

;;;###autoload
(defun talk-connect (display)
  "Connect to display DISPLAY for the Emacs talk group."
  (interactive "sTalk to display: ")
  ;; Make sure we have an entry for the current display.
  (let ((mydisp (cdr (assq 'display (frame-parameters (selected-frame))))))
    (talk-add-display mydisp))
  ;; Make sure we have an entry for the specified display.
  (talk-add-display display)
  ;; Add the new buffers to all talk frames.
  (talk-update-buffers))

;;;###autoload
(defun talk ()
  "Connect to the Emacs talk group from the current X display or tty frame."
  (interactive)
  (let ((type (frame-live-p (selected-frame))))
    (if (or (eq type t) (eq type 'x))
	(talk-add-display
	 (terminal-name (frame-terminal (selected-frame))))
      (error "Unknown frame type")))
  (talk-update-buffers))

(defun talk-add-display (display)
  (let* ((elt (assoc display talk-display-alist))
	 (name (concat "*talk-" display "*"))
	 frame buffer)
    (if (and elt (frame-live-p (nth 1 elt)))
	(setq frame (nth 1 elt))
      (setq frame (make-frame-on-display display (list (cons 'name name)))))
    (if (not (and elt (buffer-name (get-buffer (setq buffer (nth 2 elt))))))
	(setq buffer (get-buffer-create name)))
    (add-to-list 'delete-frame-functions 'talk-handle-delete-frame)
    (setq talk-display-alist
	  (cons (list display frame buffer) (delq elt talk-display-alist)))))

(defun talk-handle-delete-frame (frame)
  (dolist (d talk-display-alist)
    (when (eq (nth 1 d) frame)
      (setq talk-display-alist (delq d talk-display-alist))
      (talk-update-buffers))))

(defun talk-disconnect ()
  "Disconnect this display from the Emacs talk group."
  (interactive)
  (let* ((mydisp (cdr (assq 'display (frame-parameters (selected-frame)))))
	 (elt (assoc mydisp talk-display-alist)))
    (delete-frame (nth 1 elt))
    (kill-buffer (nth 2 elt))
    (setq talk-display-alist (delq elt talk-display-alist))
    (talk-update-buffers)))

(defun talk-update-buffers ()
  "Update all the talk frames so that each shows all the talk buffers."
  (let ((tail talk-display-alist))
    (while tail
      (let ((frame (nth 1 (car tail)))
	    (this-buffer (nth 2 (car tail)))
	    (buffers
	     (mapcar (function (lambda (elt) (nth 2 elt)))
		     talk-display-alist)))
	;; Put this display's own talk buffer
	;; at the front of the list.
	(setq buffers (cons this-buffer (delq this-buffer buffers)))
	(talk-split-up-frame frame buffers))
      (setq tail (cdr tail)))))

(defun talk-split-up-frame (frame buffers)
  "Split FRAME into equal-sized windows displaying the buffers in BUFFERS.
Select the first of these windows, displaying the first of the buffers."
  (let ((lines-per-buffer (/ (frame-height frame) (length buffers)))
	(old-frame (selected-frame)))
    (unwind-protect
	(progn
	  (select-frame frame)
	  (select-window (frame-first-window frame))
	  (delete-other-windows)
	  (while (progn
		   (switch-to-buffer (car buffers))
		   (setq buffers (cdr buffers)))
	    (split-window-below lines-per-buffer)
	    (other-window 1))
	  (select-window (frame-first-window frame)))
      (select-frame old-frame))))

(provide 'talk)

;;; talk.el ends here
