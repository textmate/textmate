;;; master.el --- make a buffer the master over another buffer

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.2
;; Keywords: comm

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

;; master-mode is a minor mode which enables you to scroll another
;; buffer (the slave) without leaving your current buffer (the master).

;; It can be used by sql.el, for example: The SQL buffer is the master
;; and its SQLi buffer is the slave.  This allows you to scroll the SQLi
;; buffer containing the output from the SQL buffer containing the
;; commands.
;;
;; This is how to use sql.el and master.el together: The variable
;; sql-buffer contains the slave buffer.  It is a local variable in the
;; SQL buffer.
;;
;; (add-hook 'sql-mode-hook
;;	   (function (lambda ()
;;		       (master-mode t)
;;		       (master-set-slave sql-buffer))))
;; (add-hook 'sql-set-sqli-hook
;;	   (function (lambda ()
;;		       (master-set-slave sql-buffer))))

;;; Thanks to all the people who helped me out:
;;
;; Rob Riepel <networking.stanford.edu>

;;; History:
;;

;;; Code:

(defgroup master nil
  "Support for master/slave relationships between buffers."
  :version "22.1"
  :group 'convenience)

;; Variables that don't need initialization.

(defvar master-of nil
  "Slave buffer of the current buffer.  See `master-mode'.
You can set this variable using `master-set-slave'.")

(defvar master-set-slave-hook nil
  "Hook run after the slave is changed using \\[master-set-slave].")

;;; Define master mode.

;;;###autoload
(define-minor-mode master-mode
  "Toggle Master mode.
With a prefix argument ARG, enable Master mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Master mode is enabled, you can scroll the slave buffer
using the following commands:

\\{master-mode-map}

The slave buffer is stored in the buffer-local variable `master-of'.
You can set this variable using `master-set-slave'.  You can show
yourself the value of `master-of' by calling `master-show-slave'."
  :group 'master
  :keymap
  '(("\C-c\C-n" . master-says-scroll-up)
    ("\C-c\C-p" . master-says-scroll-down)
    ("\C-c<" . master-says-beginning-of-buffer)
    ("\C-c>" . master-says-end-of-buffer)
    ("\C-c\C-l" . master-says-recenter)))

;; Initialize Master mode by setting a slave buffer.

(defun master-set-slave (buffer)
  "Makes BUFFER the slave of the current buffer.
Use \\[master-mode] to toggle control of the slave buffer."
  (interactive "bSlave: ")
  (make-local-variable 'master-of)
  (setq master-of buffer)
  (run-hooks 'master-set-slave-hook))

(defun master-show-slave ()
  "Displays a message with the name of the slave buffer."
  (interactive)
  (message "This buffer is the master of %s.  Master-mode is %S."
	   (or master-of "none")
	   master-mode))



;;; Functions that the master buffer can call for the slave buffer.

(defun master-says-scroll-up (&optional arg)
  "Display and scroll the slave buffer up.
See `scroll-up'."
  (interactive)
  (master-says 'scroll-up arg))

(defun master-says-scroll-down (&optional arg)
  "Display and scroll the slave buffer down.
See `scroll-down'."
  (interactive)
  (master-says 'scroll-down arg))

(defun master-says-beginning-of-buffer (&optional arg)
  "Display and move to the beginning of the slave buffer.
See `beginning-of-buffer'."
  (interactive)
  (master-says 'beginning-of-buffer arg))

(defun master-says-end-of-buffer (&optional arg)
  "Display and move to the end of the slave buffer.
See `end-of-buffer'."
  (interactive)
  (master-says 'end-of-buffer arg))

(defun master-says-recenter (&optional arg)
  "Recenter the slave buffer.
See `recenter'."
  (interactive)
  (master-says 'recenter arg))

;; The master function doing the stuff.

(defun master-says (&optional command arg)
  "Display slave buffer and execute COMMAND with ARG in its window."
  (interactive)
  (if (null (buffer-live-p (get-buffer master-of)))
      (error "Slave buffer has disappeared")
    (let ((window  (selected-window)))
      (if (not (eq (window-buffer window) (get-buffer master-of)))
	  (switch-to-buffer-other-window master-of))
      (if command (condition-case nil (apply command arg) (error nil)))
      (select-window window))))

(provide 'master)

;;; master.el ends here
