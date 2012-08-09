;;; isearchb --- a marriage between iswitchb and isearch

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: FSF
;; Created: 16 Apr 2004
;; Version: 1.5
;; Keywords: lisp
;; X-URL: http://www.newartisans.com/johnw/emacs.html

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

;; This module allows you to switch to buffers even faster than with
;; iswitchb!  It is not intended to replace it, however, as it works
;; well only with buffers whose names don't typically overlap.  You'll
;; have to try it first, and see how your mileage varies.
;;
;; The first way to use isearchb is by holding down a modifier key, in
;; which case every letter you type while holding it searches for any
;; buffer matching what you're typing (using the same ordering scheme
;; employed by iswitchb).  To use it this way, add to your .emacs:
;;
;;   (isearchb-set-keybindings 'super)  ; s-x s-y s-z now finds "xyz"
;;
;; The other way is by using a command that puts you into "search"
;; mode, just like with isearch.  I use C-z for this.  The binding in
;; my .emacs looks like:
;;
;;   (define-key global-map [(control ?z)] 'isearchb-activate)
;;
;; Now, after pressing C-z (for example), each self-inserting
;; character thereafter will search for a buffer containing those
;; characters.  For instance, typing "C-z xyz" will switch to the
;; first buffer containing "xyz".  Once you press a non-self-inserting
;; character (such as any control key sequence), the search will end.
;;
;; C-z after C-z toggles between the previously selected buffer and
;; the current one.
;;
;; C-g aborts the search and returns you to your original buffer.
;;
;; TAB, after typing in a few characters (after C-z), will jump into
;; iswitchb, using the prefix you've typed so far.  This is handy when
;; you realize that isearchb is not powerful enough to find the buffer
;; you're looking for.
;;
;; C-s and C-r move forward and backward in the buffer list.  If
;; `isearchb-show-completions' is non-nil (the default), the list of
;; possible completions is shown in the minibuffer.
;;
;; If `isearchb-idle-timeout' is set to a number, isearchb will quit
;; after that many seconds of idle time.  I recommend trying it set to
;; one or two seconds.  Then, if you switch to a buffer and wait for
;; that amount of time, you can start typing without manually exiting
;; isearchb.

;; TODO:
;;   C-z C-z is broken
;;   killing iswitchb.el and then trying to switch back is broken
;;   make sure TAB isn't broken

(require 'iswitchb)

(defgroup isearchb nil
  "Switch between buffers using a mechanism like isearch."
  :group 'iswitchb)

(defcustom isearchb-idle-timeout nil
  "Number of idle seconds before isearchb turns itself off.
If nil, don't use a timeout."
  :type '(choice (integer :tag "Seconds")
		 (const :tag "Disable" nil))
  :group 'isearchb)

(defcustom isearchb-show-completions t
  "If non-nil, show possible completions in the minibuffer."
  :type 'boolean
  :group 'isearchb)

(defvar isearchb-start-buffer nil)
(defvar isearchb-last-buffer nil)
(defvar isearchb-idle-timer nil)

(defun isearchb-stop (&optional return-to-buffer ignore-command)
  "Called by isearchb to terminate a search in progress."
  (remove-hook 'pre-command-hook 'isearchb-follow-char)
  (if return-to-buffer
      (switch-to-buffer isearchb-start-buffer)
    (setq isearchb-last-buffer isearchb-start-buffer))
  (when isearchb-idle-timer
    (cancel-timer isearchb-idle-timer)
    (setq isearchb-idle-timer nil))
  (if ignore-command
      (setq this-command 'ignore
	    last-command 'ignore))
  (message nil))

(defun isearchb-iswitchb ()
  "isearchb's custom version of the `iswitchb' command.
Its purpose is to pass different call arguments to
`iswitchb-read-buffer'."
  (interactive)
  (let* ((prompt "iswitch ")
	 (iswitchb-method 'samewindow)
	 (buf (iswitchb-read-buffer prompt nil nil iswitchb-text t)))
    (if (eq iswitchb-exit 'findfile)
	(call-interactively 'find-file)
      (when buf
	(if (get-buffer buf)
	    ;; buffer exists, so view it and then exit
	    (iswitchb-visit-buffer buf)
	  ;; else buffer doesn't exist
	  (iswitchb-possible-new-buffer buf))))))

(defun isearchb ()
  "Switch to buffer matching a substring, based on chars typed."
  (interactive)
  (unless (eq last-command 'isearchb)
    (setq iswitchb-text nil))
  (unless iswitchb-text
    (setq iswitchb-text "")
    (iswitchb-make-buflist nil))
  (if last-command-event
      (setq iswitchb-rescan t
	    iswitchb-text (concat iswitchb-text
				  (char-to-string last-command-event))))
  (iswitchb-set-matches)
  (let* ((match (car iswitchb-matches))
	 (buf (and match (get-buffer match))))
    (if (null buf)
	(progn
	  (isearchb-stop t)
	  (isearchb-iswitchb))
      (switch-to-buffer buf)
      (if isearchb-show-completions
	  (message "isearchb: %s%s" iswitchb-text
		   (iswitchb-completions iswitchb-text))
	(if (= 1 (length iswitchb-matches))
	    (message "isearchb: %s (only match)" iswitchb-text)
	  (message "isearchb: %s" iswitchb-text))))))

(defun isearchb-set-keybindings (modifier)
  "Setup isearchb on the given MODIFIER."
  (dotimes (i 128)
    (if (eq 'self-insert-command
	    (lookup-key global-map (vector i)))
	(define-key global-map (vector (list modifier i)) 'isearchb))))

(defun isearchb-follow-char ()
  "Function added to `post-command-hook' to handle the isearchb \"mode\"."
  (let (keys)
    (if (not (and (memq last-command '(isearchb isearchb-activate))
		  (setq keys (this-command-keys))
		  (= 1 (length keys))))
	(isearchb-stop)
      (cond
       ((or (equal keys "\C-h") (equal keys "\C-?")
	    (equal keys [backspace]) (equal keys [delete]))
	(setq iswitchb-text
	      (substring iswitchb-text 0 (1- (length iswitchb-text))))
	(if (= 0 (length iswitchb-text))
	    (isearchb-stop t t)
	  (setq last-command-event nil)
	  (setq this-command 'isearchb)))
       ((or (equal keys "\C-i") (equal keys [tab]))
	(setq this-command 'isearchb-iswitchb))
       ((equal keys "\C-s")
	(iswitchb-next-match)
	(setq last-command-event nil)
	(setq this-command 'isearchb))
       ((equal keys "\C-r")
	(iswitchb-prev-match)
	(setq last-command-event nil)
	(setq this-command 'isearchb))
       ((equal keys "\C-g")
	(ding)
	(isearchb-stop t t))
       ((eq (lookup-key global-map keys) 'self-insert-command)
	(setq this-command 'isearchb)))
      (if (and isearchb-idle-timeout
	       (null isearchb-idle-timer))
	(setq isearchb-idle-timer
	      (run-with-idle-timer isearchb-idle-timeout nil
				   'isearchb-stop))))))

;;;###autoload
(defun isearchb-activate ()
  "Active isearchb mode for subsequent alphanumeric keystrokes.
Executing this command again will terminate the search; or, if
the search has not yet begun, will toggle to the last buffer
accessed via isearchb."
  (interactive)
  (cond
   ((eq last-command 'isearchb)
    (isearchb-stop nil t))
   ((eq last-command 'isearchb-activate)
    (if isearchb-last-buffer
	(switch-to-buffer isearchb-last-buffer)
      (error "isearchb: There is no previous buffer to toggle to"))
    (isearchb-stop nil t))
   (t
    (message "isearchb: ")
    (setq iswitchb-text nil
	  isearchb-start-buffer (current-buffer))
    (add-hook 'pre-command-hook 'isearchb-follow-char))))

(provide 'isearchb)

;;; isearchb.el ends here
