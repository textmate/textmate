;;; emacs-lock.el --- prevents you from exiting Emacs if a buffer is locked

;; Copyright (C) 1994, 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Tom Wurgler <twurgler@goodyear.com>
;; Created: 12/8/94
;; Keywords: extensions, processes
;; Obsolete-since: 24.1

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

;; This code sets a buffer-local variable to t if toggle-emacs-lock is run,
;; then if the user attempts to exit Emacs, the locked buffer name will be
;; displayed and the exit aborted.  This is just a way of protecting
;; yourself from yourself.  For example, if you have a shell running a big
;; program and exiting Emacs would abort that program, you may want to lock
;; that buffer, then if you forget about it after a while, you won't
;; accidentally exit Emacs.  To unlock the buffer, just goto the buffer and
;; run toggle-emacs-lock again.

;;; Code:

(defvar emacs-lock-from-exiting nil
  "Whether Emacs is locked to prevent exiting.  See `check-emacs-lock'.")
(make-variable-buffer-local 'emacs-lock-from-exiting)

(defvar emacs-lock-buffer-locked nil
  "Whether a shell or telnet buffer was locked when its process was killed.")
(make-variable-buffer-local 'emacs-lock-buffer-locked)
(put 'emacs-lock-buffer-locked 'permanent-local t)

(defun check-emacs-lock ()
  "Check if variable `emacs-lock-from-exiting' is t for any buffer.
If any locked buffer is found, signal error and display the buffer's name."
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when emacs-lock-from-exiting
	(error "Emacs is locked from exit due to buffer: %s" (buffer-name))))))

(defun toggle-emacs-lock ()
  "Toggle `emacs-lock-from-exiting' for the current buffer.
See `check-emacs-lock'."
  (interactive)
  (setq emacs-lock-from-exiting (not emacs-lock-from-exiting))
  (if emacs-lock-from-exiting
      (message "Buffer is now locked")
    (message "Buffer is now unlocked")))

(defun emacs-lock-check-buffer-lock ()
  "Check if variable `emacs-lock-from-exiting' is t for a buffer.
If the buffer is locked, signal error and display its name."
  (when emacs-lock-from-exiting
    (error "Buffer `%s' is locked, can't delete it" (buffer-name))))

; These next defuns make it so if you exit a shell that is locked,  the lock
; is shut off for that shell so you can exit Emacs.  Same for telnet.
; Also, if a shell or a telnet buffer was locked and the process killed,
; turn the lock back on again if the process is restarted.

(defun emacs-lock-shell-sentinel ()
  (set-process-sentinel
   (get-buffer-process (buffer-name)) (function emacs-lock-clear-sentinel)))

(defun emacs-lock-clear-sentinel (_proc _str)
  (if emacs-lock-from-exiting
      (progn
	(setq emacs-lock-from-exiting nil)
	(setq emacs-lock-buffer-locked t)
	(message "Buffer is now unlocked"))
    (setq emacs-lock-buffer-locked nil)))

(defun emacs-lock-was-buffer-locked ()
  (if emacs-lock-buffer-locked
      (setq emacs-lock-from-exiting t)))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'check-emacs-lock))
(add-hook 'kill-buffer-hook 'emacs-lock-check-buffer-lock)
(add-hook 'shell-mode-hook 'emacs-lock-was-buffer-locked)
(add-hook 'shell-mode-hook 'emacs-lock-shell-sentinel)
(add-hook 'telnet-mode-hook 'emacs-lock-was-buffer-locked)
(add-hook 'telnet-mode-hook 'emacs-lock-shell-sentinel)

(provide 'emacs-lock)

;;; emacs-lock.el ends here
