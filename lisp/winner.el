;;; winner.el --- Restore old window configurations

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author: Ivar Rummelhoff <ivarru@math.uio.no>
;; Created: 27 Feb 1997
;; Keywords: convenience frames

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

;; Winner mode is a global minor mode that records the changes in the
;; window configuration (i.e. how the frames are partitioned into
;; windows) so that the changes can be "undone" using the command
;; `winner-undo'.  By default this one is bound to the key sequence
;; ctrl-c left.  If you change your mind (while undoing), you can
;; press ctrl-c right (calling `winner-redo').  Even though it uses
;; some features of Emacs20.3, winner.el should also work with
;; Emacs19.34 and XEmacs20, provided that the installed version of
;; custom is not obsolete.

;; Winner mode was improved August 1998.
;; Further improvements February 2002.

;;; Code:

(eval-when-compile
  (require 'cl))


(defmacro winner-active-region ()
  (if (boundp 'mark-active)
      'mark-active
    '(region-active-p)))

(defsetf winner-active-region () (store)
  (if (featurep 'xemacs)
      `(if ,store (zmacs-activate-region)
	 (zmacs-deactivate-region))
    `(setq mark-active ,store)))

(defalias 'winner-edges
  (if (featurep 'xemacs) 'window-pixel-edges 'window-edges))
(defalias 'winner-window-list
  (if (featurep 'xemacs)
      (lambda () (delq (minibuffer-window) (window-list nil 0)))
    (lambda () (window-list nil 0))))

(require 'ring)

(defgroup winner nil
  "Restoring window configurations."
  :group 'windows)

;;;###autoload
(defcustom winner-mode nil
  "Toggle Winner mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `winner-mode'."
  :set #'(lambda (symbol value) (funcall symbol (or value 0)))
  :initialize 'custom-initialize-default
  :type    'boolean
  :group   'winner
  :require 'winner)

(defcustom winner-dont-bind-my-keys nil
  "If non-nil: Do not use `winner-mode-map' in Winner mode."
  :type  'boolean
  :group 'winner)

(defcustom winner-ring-size 200
  "Maximum number of stored window configurations per frame."
  :type  'integer
  :group 'winner)

(defcustom winner-boring-buffers '("*Completions*")
  "`winner-undo' will not restore windows displaying any of these buffers.
You may want to include buffer names such as *Help*, *Apropos*,
*Buffer List*, *info* and *Compile-Log*."
  :type '(repeat string)
  :group 'winner)





;;;; Saving old configurations (internal variables and subroutines)


;;; Current configuration

;; List the windows according to their edges.
(defun winner-sorted-window-list ()
  (sort (winner-window-list)
        (lambda (x y)
          (loop for a in (winner-edges x)
                for b in (winner-edges y)
                while (= a b)
                finally return (< a b)))))

(defun winner-win-data ()
  ;; Essential properties of the windows in the selected frame.
  (loop for win in (winner-sorted-window-list)
        collect (cons (winner-edges win) (window-buffer win))))

;; This variable is updated with the current window configuration
;; every time it changes.
(defvar winner-currents nil)

;; The current configuration (+ the buffers involved).
(defsubst winner-conf ()
  (cons (current-window-configuration)
        (winner-win-data)))


;; Save current configuration.
;; (Called below by `winner-save-old-configurations').
(defun winner-remember ()
  (let ((entry (assq (selected-frame) winner-currents)))
    (if entry (setcdr entry (winner-conf))
      (push (cons (selected-frame) (winner-conf))
	    winner-currents))))

;; Consult `winner-currents'.
(defun winner-configuration (&optional frame)
  (or (cdr (assq (or frame (selected-frame)) winner-currents))
      (letf (((selected-frame) frame))
	(winner-conf))))



;;; Saved configurations

;; This variable contains the window configuration rings.
;; The key in this alist is the frame.
(defvar winner-ring-alist nil)

;; Find the right ring.  If it does not exist, create one.
(defsubst winner-ring (frame)
  (or (cdr (assq frame winner-ring-alist))
      (let ((ring (make-ring winner-ring-size)))
        (ring-insert ring (winner-configuration frame))
        (push (cons frame ring) winner-ring-alist)
        ring)))


;; If the same command is called several times in a row,
;; we only save one window configuration.
(defvar winner-last-command nil)

;; Frames affected by the previous command.
(defvar winner-last-frames nil)


(defsubst winner-equal (a b)
  "Check whether two Winner configurations (as produced by
`winner-conf') are equal."
  (equal (cdr a) (cdr b)))


;; Save the current window configuration, if it has changed.
;; If so return frame, otherwise return nil.
(defun winner-insert-if-new (frame)
  (unless (or (memq frame winner-last-frames)
	      (eq this-command 'winner-redo))
    (let ((conf (winner-configuration frame))
	  (ring (winner-ring frame)))
      (when (and (not (ring-empty-p ring))
		 (winner-equal conf (ring-ref ring 0)))
        ;; When the previous configuration was very similar,
        ;; keep only the latest.
	(ring-remove ring 0))
      (ring-insert ring conf)
      (push frame winner-last-frames)
      frame)))



;;; Hooks

;; Frames affected by the current command.
(defvar winner-modified-list nil)

;; Called whenever the window configuration changes
;; (a `window-configuration-change-hook').
(defun winner-change-fun ()
  (unless (or (memq (selected-frame) winner-modified-list)
              (/= 0 (minibuffer-depth)))
    (push (selected-frame) winner-modified-list)))

;; A `post-command-hook' for emacsen with
;; `window-configuration-change-hook'.
(defun winner-save-old-configurations ()
  (when (zerop (minibuffer-depth))
    (unless (eq this-command winner-last-command)
      (setq winner-last-frames nil)
      (setq winner-last-command this-command))
    (dolist (frame winner-modified-list)
      (winner-insert-if-new frame))
    (setq winner-modified-list nil)
    (winner-remember)))

;; A `minibuffer-setup-hook'.
(defun winner-save-unconditionally ()
  (unless (eq this-command winner-last-command)
    (setq winner-last-frames nil)
    (setq winner-last-command this-command))
  (winner-insert-if-new (selected-frame))
  (winner-remember))

;; A `post-command-hook' for other emacsen.
;; Also called by `winner-undo' before "undoing".
(defun winner-save-conditionally ()
  (when (zerop (minibuffer-depth))
    (winner-save-unconditionally)))




;;;; Restoring configurations

;; Works almost as `set-window-configuration',
;; but does not change the contents or the size of the minibuffer,
;; and tries to preserve the selected window.
(defun winner-set-conf (winconf)
  (let* ((miniwin  (minibuffer-window))
         (chosen   (selected-window))
         (minisize (window-height miniwin)))
    (letf (((window-buffer miniwin))
           ((window-point  miniwin)))
      (set-window-configuration winconf))
    (cond
     ((window-live-p chosen) (select-window chosen))
     ((window-minibuffer-p (selected-window))
      (other-window 1)))
    (when (/= minisize (window-height miniwin))
      (letf (((selected-window) miniwin) )
        (setf (window-height) minisize)))))



(defvar winner-point-alist nil)
;; `set-window-configuration' restores old points and marks.  This is
;; not what we want, so we make a list of the "real" (i.e. new) points
;; and marks before undoing window configurations.
;;
;; Format of entries: (buffer (mark . mark-active) (window . point) ..)

(defun winner-make-point-alist ()
  (letf (((current-buffer)))
    (loop with alist
	  for win in (winner-window-list)
	  for entry =
          (or (assq (window-buffer win) alist)
              (car (push (list (set-buffer (window-buffer win))
                               (cons (mark t) (winner-active-region)))
                         alist)))
	  do (push (cons win (window-point win))
                   (cddr entry))
	  finally return alist)))

(defun winner-get-point (buf win)
  ;; Consult (and possibly extend) `winner-point-alist'.
  ;; Returns nil if buf no longer exists.
  (when (buffer-name buf)
    (let ((entry (assq buf winner-point-alist)))
      (cond
       (entry
	(or (cdr (assq win (cddr entry)))
	    (cdr (assq nil (cddr entry)))
	    (letf (((current-buffer) buf))
	      (push (cons nil (point)) (cddr entry))
	      (point))))
       (t (letf (((current-buffer) buf))
	    (push (list buf
			(cons (mark t) (winner-active-region))
			(cons nil (point)))
		  winner-point-alist)
	    (point)))))))


;; Make sure point does not end up in the minibuffer and delete
;; windows displaying dead or boring buffers
;; (c.f. `winner-boring-buffers').  Return nil if all the windows
;; should be deleted.  Preserve correct points and marks.
(defun winner-set (conf)
  ;; For the format of `conf', see `winner-conf'.
  (let* ((buffers nil)
	 (alive
          ;; Possibly update `winner-point-alist'
	  (loop for buf in (mapcar 'cdr (cdr conf))
		for pos = (winner-get-point buf nil)
		if (and pos (not (memq buf buffers)))
		do (push buf buffers)
		collect pos)))
    (winner-set-conf (car conf))
    (let (xwins)                        ; to be deleted

      ;; Restore points
      (dolist (win (winner-sorted-window-list))
        (unless (and (pop alive)
                     (setf (window-point win)
                           (winner-get-point (window-buffer win) win))
                     (not (member (buffer-name (window-buffer win))
                                  winner-boring-buffers)))
          (push win xwins)))            ; delete this window

      ;; Restore marks
      (letf (((current-buffer)))
	(loop for buf in buffers
	      for entry = (cadr (assq buf winner-point-alist))
	      do (progn (set-buffer buf)
			(set-mark (car entry))
			(setf (winner-active-region) (cdr entry)))))
      ;; Delete windows, whose buffers are dead or boring.
      ;; Return t if this is still a possible configuration.
      (or (null xwins)
	  (progn
            (mapc 'delete-window (cdr xwins)) ; delete all but one
            (unless (one-window-p t)
              (delete-window (car xwins))
              t))))))



;;;; Winner mode  (a minor mode)

(defcustom winner-mode-hook nil
  "Functions to run whenever Winner mode is turned on."
  :type 'hook
  :group 'winner)

(defcustom winner-mode-leave-hook nil
  "Functions to run whenever Winner mode is turned off."
  :type 'hook
  :group 'winner)

(defvar winner-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) left] 'winner-undo)
    (define-key map [(control c) right] 'winner-redo)
    map)
  "Keymap for Winner mode.")

;; Check if `window-configuration-change-hook' is working.
(defun winner-hook-installed-p ()
  (save-window-excursion
    (let ((winner-var nil)
	  (window-configuration-change-hook
	   '((lambda () (setq winner-var t)))))
      (split-window)
      winner-var)))


;;;###autoload
(defun winner-mode (&optional arg)
  "Toggle Winner mode.
With arg, turn Winner mode on if and only if arg is positive."
  (interactive "P")
  (let ((on-p (if arg (> (prefix-numeric-value arg) 0)
		(not winner-mode))))
    (cond
     ;; Turn mode on
     (on-p
      (setq winner-mode t)
      (cond
       ((winner-hook-installed-p)
	(add-hook 'window-configuration-change-hook 'winner-change-fun)
      (add-hook 'post-command-hook 'winner-save-old-configurations))
       (t (add-hook 'post-command-hook 'winner-save-conditionally)))
      (add-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
      (setq winner-modified-list (frame-list))
      (winner-save-old-configurations)
      (run-hooks 'winner-mode-hook)
      (when (called-interactively-p 'interactive)
	(message "Winner mode enabled")))
     ;; Turn mode off
     (winner-mode
      (setq winner-mode nil)
      (remove-hook 'window-configuration-change-hook 'winner-change-fun)
      (remove-hook 'post-command-hook 'winner-save-old-configurations)
      (remove-hook 'post-command-hook 'winner-save-conditionally)
      (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
      (run-hooks 'winner-mode-leave-hook)
      (when (called-interactively-p 'interactive)
	(message "Winner mode disabled"))))))

;; Inspired by undo (simple.el)

(defvar winner-undo-frame nil)

(defvar winner-pending-undo-ring nil
  "The ring currently used by `winner-undo'.")
(defvar winner-undo-counter nil)
(defvar winner-undone-data  nil) ; There confs have been passed.

(defun winner-undo ()
  "Switch back to an earlier window configuration saved by Winner mode.
In other words, \"undo\" changes in window configuration."
  (interactive)
  (cond
   ((not winner-mode) (error "Winner mode is turned off"))
   (t (unless (and (eq last-command 'winner-undo)
 		   (eq winner-undo-frame (selected-frame)))
	(winner-save-conditionally)     ; current configuration->stack
 	(setq winner-undo-frame (selected-frame))
 	(setq winner-point-alist (winner-make-point-alist))
 	(setq winner-pending-undo-ring (winner-ring (selected-frame)))
 	(setq winner-undo-counter 0)
 	(setq winner-undone-data (list (winner-win-data))))
      (incf winner-undo-counter)	; starting at 1
      (when (and (winner-undo-this)
 		 (not (window-minibuffer-p (selected-window))))
 	(message "Winner undo (%d / %d)"
 		 winner-undo-counter
 		 (1- (ring-length winner-pending-undo-ring)))))))




(defun winner-undo-this ()		; The heart of winner undo.
  (loop
   (cond
    ((>= winner-undo-counter (ring-length winner-pending-undo-ring))
     (message "No further window configuration undo information")
     (return nil))

    ((and				; If possible configuration
      (winner-set (ring-ref winner-pending-undo-ring
 			    winner-undo-counter))
                                        ; .. and new configuration
      (let ((data (winner-win-data)))
 	(and (not (member data winner-undone-data))
 	     (push data winner-undone-data))))
     (return t))			; .. then everything is fine.
    (t ;; Otherwise, discharge it (and try the next one).
     (ring-remove winner-pending-undo-ring winner-undo-counter)))))


(defun winner-redo ()			; If you change your mind.
  "Restore a more recent window configuration saved by Winner mode."
  (interactive)
  (cond
   ((eq last-command 'winner-undo)
    (winner-set
     (if (zerop (minibuffer-depth))
         (ring-remove winner-pending-undo-ring 0)
       (ring-ref winner-pending-undo-ring 0)))
    (unless (eq (selected-window) (minibuffer-window))
      (message "Winner undid undo")))
   (t (error "Previous command was not a `winner-undo'"))))

;;; To be evaluated when the package is loaded:

(unless (or (assq 'winner-mode minor-mode-map-alist)
	    winner-dont-bind-my-keys)
  (push (cons 'winner-mode winner-mode-map)
	minor-mode-map-alist))

(provide 'winner)
;;; winner.el ends here
