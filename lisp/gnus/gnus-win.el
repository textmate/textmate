;;; gnus-win.el --- window configuration functions for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
(require 'gnus-util)

(defgroup gnus-windows nil
  "Window configuration."
  :group 'gnus)

(defcustom gnus-use-full-window t
  "*If non-nil, use the entire Emacs screen."
  :group 'gnus-windows
  :type 'boolean)

(defcustom gnus-window-min-width 2
  "*Minimum width of Gnus buffers."
  :group 'gnus-windows
  :type 'integer)

(defcustom gnus-window-min-height 1
  "*Minimum height of Gnus buffers."
  :group 'gnus-windows
  :type 'integer)

(defcustom gnus-always-force-window-configuration nil
  "*If non-nil, always force the Gnus window configurations."
  :group 'gnus-windows
  :type 'boolean)

(defcustom gnus-use-frames-on-any-display nil
  "*If non-nil, frames on all displays will be considered usable by Gnus.
When nil, only frames on the same display as the selected frame will be
used to display Gnus windows."
  :version "22.1"
  :group 'gnus-windows
  :type 'boolean)

(defvar gnus-buffer-configuration
  '((group
     (vertical 1.0
	       (group 1.0 point)))
    (summary
     (vertical 1.0
	       (summary 1.0 point)))
    (article
     (cond
      (gnus-use-trees
       '(vertical 1.0
		  (summary 0.25 point)
		  (tree 0.25)
		  (article 1.0)))
      (t
       '(vertical 1.0
		  (summary 0.25 point)
		  (article 1.0)))))
    (server
     (vertical 1.0
	       (server 1.0 point)))
    (browse
     (vertical 1.0
	       (browse 1.0 point)))
    (message
     (vertical 1.0
	       (message 1.0 point)))
    (pick
     (vertical 1.0
	       (article 1.0 point)))
    (info
     (vertical 1.0
	       (info 1.0 point)))
    (summary-faq
     (vertical 1.0
	       (summary 0.25)
	       (faq 1.0 point)))
    (only-article
     (vertical 1.0
	       (article 1.0 point)))
    (edit-article
     (vertical 1.0
	       (article 1.0 point)))
    (edit-form
     (vertical 1.0
	       (group 0.5)
	       (edit-form 1.0 point)))
    (edit-score
     (vertical 1.0
	       (summary 0.25)
	       (edit-score 1.0 point)))
    (edit-server
     (vertical 1.0
	       (server 0.5)
	       (edit-form 1.0 point)))
    (post
     (vertical 1.0
	       (post 1.0 point)))
    (reply
     (vertical 1.0
	       (article 0.5)
	       (message 1.0 point)))
    (forward
     (vertical 1.0
	       (message 1.0 point)))
    (reply-yank
     (vertical 1.0
	       (message 1.0 point)))
    (mail-bounce
     (vertical 1.0
	       (article 0.5)
	       (message 1.0 point)))
    (pipe
     (vertical 1.0
	       (summary 0.25 point)
	       ("*Shell Command Output*" 1.0)))
    (bug
     (vertical 1.0
	       (if gnus-bug-create-help-buffer '("*Gnus Help Bug*" 0.5))
	       ("*Gnus Bug*" 1.0 point)))
    (score-trace
     (vertical 1.0
	       (summary 0.5 point)
	       ("*Score Trace*" 1.0)))
    (score-words
     (vertical 1.0
	       (summary 0.5 point)
	       ("*Score Words*" 1.0)))
    (split-trace
     (vertical 1.0
	       (summary 0.5 point)
	       ("*Split Trace*" 1.0)))
    (category
     (vertical 1.0
	       (category 1.0)))
    (compose-bounce
     (vertical 1.0
	       (article 0.5)
	       (message 1.0 point)))
    (display-term
     (vertical 1.0
	       ("*display*" 1.0)))
    (mml-preview
     (vertical 1.0
	       (message 0.5)
	       (mml-preview 1.0 point))))
  "Window configuration for all possible Gnus buffers.
See the Gnus manual for an explanation of the syntax used.")

(defvar gnus-window-to-buffer
  '((group . gnus-group-buffer)
    (summary . gnus-summary-buffer)
    (article . gnus-article-buffer)
    (server . gnus-server-buffer)
    (browse . "*Gnus Browse Server*")
    (edit-group . gnus-group-edit-buffer)
    (edit-form . gnus-edit-form-buffer)
    (edit-server . gnus-server-edit-buffer)
    (edit-score . gnus-score-edit-buffer)
    (message . gnus-message-buffer)
    (mail . gnus-message-buffer)
    (post-news . gnus-message-buffer)
    (faq . gnus-faq-buffer)
    (tree . gnus-tree-buffer)
    (score-trace . "*Score Trace*")
    (split-trace . "*Split Trace*")
    (info . gnus-info-buffer)
    (category . gnus-category-buffer)
    (article-copy . gnus-article-copy)
    (draft . gnus-draft-buffer)
    (mml-preview . mml-preview-buffer))
  "Mapping from short symbols to buffer names or buffer variables.")

(defcustom gnus-configure-windows-hook nil
  "*A hook called when configuring windows."
  :version "22.1"
  :group 'gnus-windows
  :type 'hook)

;;; Internal variables.

(defvar gnus-current-window-configuration nil
  "The most recently set window configuration.")

(defvar gnus-created-frames nil)
(defvar gnus-window-frame-focus nil)

(defun gnus-kill-gnus-frames ()
  "Kill all frames Gnus has created."
  (while gnus-created-frames
    (when (frame-live-p (car gnus-created-frames))
      ;; We slap a condition-case around this `delete-frame' to ensure
      ;; against errors if we try do delete the single frame that's left.
      (ignore-errors
	(delete-frame (car gnus-created-frames))))
    (pop gnus-created-frames)))

;;;###autoload
(defun gnus-add-configuration (conf)
  "Add the window configuration CONF to `gnus-buffer-configuration'."
  (setq gnus-buffer-configuration
	(cons conf (delq (assq (car conf) gnus-buffer-configuration)
			 gnus-buffer-configuration))))

(defvar gnus-frame-list nil)

(defun gnus-window-to-buffer-helper (obj)
  (cond ((not (symbolp obj))
	 obj)
	((boundp obj)
	 (symbol-value obj))
	((fboundp obj)
	 (funcall obj))
	(t
	 nil)))

(defun gnus-configure-frame (split &optional window)
  "Split WINDOW according to SPLIT."
  (let* ((current-window (or (get-buffer-window (current-buffer)) (selected-window)))
         (window (or window current-window)))
    (select-window window)
    ;; The SPLIT might be something that is to be evalled to
    ;; return a new SPLIT.
    (while (and (not (assq (car split) gnus-window-to-buffer))
		(symbolp (car split)) (fboundp (car split)))
      (setq split (eval split)))
    (let* ((type (car split))
	   (subs (cddr split))
	   (len (if (eq type 'horizontal) (window-width) (window-height)))
	   (total 0)
	   (window-min-width (or gnus-window-min-width window-min-width))
	   (window-min-height (or gnus-window-min-height window-min-height))
	   s result new-win rest comp-subs size sub)
      (cond
       ;; Nothing to do here.
       ((null split))
       ;; Don't switch buffers.
       ((null type)
	(and (memq 'point split) window))
       ;; This is a buffer to be selected.
       ((not (memq type '(frame horizontal vertical)))
	(let ((buffer (cond ((stringp type) type)
			    (t (cdr (assq type gnus-window-to-buffer))))))
	  (unless buffer
	    (error "Invalid buffer type: %s" type))
	  (let ((buf (gnus-get-buffer-create
		      (gnus-window-to-buffer-helper buffer))))
	    (when (buffer-name buf)
	      (if (eq buf (window-buffer (selected-window)))
		  (set-buffer buf)
		(switch-to-buffer buf))))
	  (when (memq 'frame-focus split)
	    (setq gnus-window-frame-focus window))
	  ;; We return the window if it has the `point' spec.
	  (and (memq 'point split) window)))
       ;; This is a frame split.
       ((eq type 'frame)
	(unless gnus-frame-list
	  (setq gnus-frame-list (list (window-frame current-window))))
	(let ((i 0)
	      params frame fresult)
	  (while (< i (length subs))
	    ;; Frame parameter is gotten from the sub-split.
	    (setq params (cadr (elt subs i)))
	    ;; It should be a list.
	    (unless (listp params)
	      (setq params nil))
	    ;; Create a new frame?
	    (unless (setq frame (elt gnus-frame-list i))
	      (nconc gnus-frame-list (list (setq frame (make-frame params))))
	      (push frame gnus-created-frames))
	    ;; Is the old frame still alive?
	    (unless (frame-live-p frame)
	      (setcar (nthcdr i gnus-frame-list)
		      (setq frame (make-frame params))))
	    ;; Select the frame in question and do more splits there.
	    (select-frame frame)
	    (setq fresult (or (gnus-configure-frame (elt subs i)) fresult))
	    (incf i))
	  ;; Select the frame that has the selected buffer.
	  (when fresult
	    (select-frame (window-frame fresult)))))
       ;; This is a normal split.
       (t
	(when (> (length subs) 0)
	  ;; First we have to compute the sizes of all new windows.
	  (while subs
	    (setq sub (append (pop subs) nil))
	    (while (and (not (assq (car sub) gnus-window-to-buffer))
			(symbolp (car sub)) (fboundp (car sub)))
	      (setq sub (eval sub)))
	    (when sub
	      (push sub comp-subs)
	      (setq size (cadar comp-subs))
	      (cond ((equal size 1.0)
		     (setq rest (car comp-subs))
		     (setq s 0))
		    ((floatp size)
		     (setq s (floor (* size len))))
		    ((integerp size)
		     (setq s size))
		    (t
		     (error "Invalid size: %s" size)))
	      ;; Try to make sure that we are inside the safe limits.
	      (cond ((zerop s))
		    ((eq type 'horizontal)
		     (setq s (max s window-min-width)))
		    ((eq type 'vertical)
		     (setq s (max s window-min-height))))
	      (setcar (cdar comp-subs) s)
	      (incf total s)))
	  ;; Take care of the "1.0" spec.
	  (if rest
	      (setcar (cdr rest) (- len total))
	    (error "No 1.0 specs in %s" split))
	  ;; The we do the actual splitting in a nice recursive
	  ;; fashion.
	  (setq comp-subs (nreverse comp-subs))
	  (while comp-subs
	    (if (null (cdr comp-subs))
		(setq new-win window)
	      (setq new-win
		    (split-window window (cadar comp-subs)
				  (eq type 'horizontal))))
	    (setq result (or (gnus-configure-frame
			      (car comp-subs) window)
			     result))
	    (select-window new-win)
	    (setq window new-win)
	    (setq comp-subs (cdr comp-subs))))
	;; Return the proper window, if any.
	(when result
	  (select-window result)))))))

(defvar gnus-frame-split-p nil)

(defun gnus-configure-windows (setting &optional force)
  (cond
   ((null setting)
    ;; Do nothing.
    )
   ((window-configuration-p setting)
    (set-window-configuration setting))
   (t
    (setq gnus-current-window-configuration setting)
    (setq force (or force gnus-always-force-window-configuration))
    (let ((split (if (symbolp setting)
                     (cadr (assq setting gnus-buffer-configuration))
                   setting))
          all-visible)

      (setq gnus-frame-split-p nil)

      (unless split
        (error "No such setting in `gnus-buffer-configuration': %s" setting))

      (if (and (setq all-visible (gnus-all-windows-visible-p split))
               (not force))
          ;; All the windows mentioned are already visible, so we just
          ;; put point in the assigned buffer, and do not touch the
          ;; winconf.
          (select-window all-visible)

        ;; Make sure "the other" buffer, nntp-server-buffer, is live.
        (unless (gnus-buffer-live-p nntp-server-buffer)
          (nnheader-init-server-buffer))

        ;; Either remove all windows or just remove all Gnus windows.
        (let ((frame (selected-frame)))
          (unwind-protect
              (if gnus-use-full-window
                  ;; We want to remove all other windows.
                  (if (not gnus-frame-split-p)
                      ;; This is not a `frame' split, so we ignore the
                      ;; other frames.
                      (delete-other-windows)
                    ;; This is a `frame' split, so we delete all windows
                    ;; on all frames.
                    (gnus-delete-windows-in-gnusey-frames))
                ;; Just remove some windows.
                (gnus-remove-some-windows)
                (if (featurep 'xemacs)
                    (switch-to-buffer nntp-server-buffer)
                  (set-buffer nntp-server-buffer)))
            (select-frame frame)))

        (let (gnus-window-frame-focus)
          (if (featurep 'xemacs)
              (switch-to-buffer nntp-server-buffer)
            (set-buffer nntp-server-buffer))
          (gnus-configure-frame split)
          (run-hooks 'gnus-configure-windows-hook)
          (when gnus-window-frame-focus
            (gnus-select-frame-set-input-focus
             (window-frame gnus-window-frame-focus)))))))))

(defun gnus-delete-windows-in-gnusey-frames ()
  "Do a `delete-other-windows' in all frames that have Gnus windows."
  (let ((buffers (gnus-buffers)))
    (mapcar
     (lambda (frame)
       (unless (eq (cdr (assq 'minibuffer
			      (frame-parameters frame)))
		   'only)
	 (select-frame frame)
	 (let (do-delete)
	   (walk-windows
	    (lambda (window)
	      (when (memq (window-buffer window) buffers)
		(setq do-delete t))))
	   (when do-delete
	     (delete-other-windows)))))
     (frame-list))))

(defun gnus-all-windows-visible-p (split)
  "Say whether all buffers in SPLIT are currently visible.
In particular, the value returned will be the window that
should have point."
  (let ((stack (list split))
	(all-visible t)
	type buffer win buf)
    (while (and (setq split (pop stack))
		all-visible)
      (when (consp (car split))
	(push 1.0 split)
	(push 'vertical split))
      ;; The SPLIT might be something that is to be evalled to
      ;; return a new SPLIT.
      (while (and (not (assq (car split) gnus-window-to-buffer))
		  (symbolp (car split)) (fboundp (car split)))
	(setq split (eval split)))

      (setq type (elt split 0))
      (cond
       ;; Nothing here.
       ((null split) t)
       ;; A buffer.
       ((not (memq type '(horizontal vertical frame)))
	(setq buffer (cond ((stringp type) type)
			   (t (cdr (assq type gnus-window-to-buffer)))))
	(unless buffer
	  (error "Invalid buffer type: %s" type))
	(if (and (setq buf (get-buffer (gnus-window-to-buffer-helper buffer)))
		 (buffer-live-p buf)
		 (setq win (gnus-get-buffer-window buf t)))
	    (if (memq 'point split)
		(setq all-visible win))
	  (setq all-visible nil)))
       (t
	(when (eq type 'frame)
	  (setq gnus-frame-split-p t))
	(setq stack (append (cddr split) stack)))))
    (unless (eq all-visible t)
      all-visible)))

(defun gnus-window-top-edge (&optional window)
  "Return the top coordinate of WINDOW."
  (nth 1 (window-edges window)))

(defun gnus-remove-some-windows ()
  (let ((buffers (gnus-buffers))
	buf bufs lowest-buf lowest)
    (save-excursion
      ;; Remove windows on all known Gnus buffers.
      (while (setq buf (pop buffers))
	(when (get-buffer-window buf)
	  (push buf bufs)
	  (pop-to-buffer buf)
	  (when (or (not lowest)
		    (< (gnus-window-top-edge) lowest))
	    (setq lowest (gnus-window-top-edge)
		  lowest-buf buf))))
      (when lowest-buf
	(pop-to-buffer lowest-buf)
	(if (featurep 'xemacs)
	    (switch-to-buffer nntp-server-buffer)
	  (set-buffer nntp-server-buffer)))
      (mapcar (lambda (b) (delete-windows-on b t))
	      (delq lowest-buf bufs)))))

(eval-and-compile
  (cond
   ((fboundp 'frames-on-display-list)
    (defalias 'gnus-frames-on-display-list 'frames-on-display-list))
   ((and (featurep 'xemacs) (fboundp 'frame-device))
    (defun gnus-frames-on-display-list ()
      (apply 'filtered-frame-list 'identity (list (frame-device nil)))))
   (t
    (defalias 'gnus-frames-on-display-list 'frame-list))))

(defun gnus-get-buffer-window (buffer &optional frame)
  (cond ((and (null gnus-use-frames-on-any-display)
	      (memq frame '(t 0 visible)))
	 (car
	  (let ((frames (gnus-frames-on-display-list)))
	    (gnus-remove-if (lambda (win) (not (memq (window-frame win)
						     frames)))
			    (get-buffer-window-list buffer nil frame)))))
	(t
	 (get-buffer-window buffer frame))))

(provide 'gnus-win)

;;; gnus-win.el ends here
