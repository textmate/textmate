;;; ledit.el --- Emacs side of ledit interface

;; Copyright (C) 1985, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: languages

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

;; This is a major mode for editing Liszt.  See etc/LEDIT for details.

;;; Code:

;;; To do:
;;; o lisp -> emacs side of things (grind-definition and find-definition)

(defvar ledit-mode-map nil)

(defconst ledit-zap-file
  (expand-file-name (concat (user-login-name) ".l1") temporary-file-directory)
  "File name for data sent to Lisp by Ledit.")
(defconst ledit-read-file
  (expand-file-name (concat (user-login-name) ".l2") temporary-file-directory)
  "File name for data sent to Ledit by Lisp.")
(defconst ledit-compile-file
  (expand-file-name (concat (user-login-name) ".l4") temporary-file-directory)
  "File name for data sent to Lisp compiler by Ledit.")
(defconst ledit-buffer "*LEDIT*"
  "Name of buffer in which Ledit accumulates data to send to Lisp.")

;;;###autoload
(defconst ledit-save-files t "\
*Non-nil means Ledit should save files before transferring to Lisp.")
;;;###autoload
(defconst ledit-go-to-lisp-string "%?lisp" "\
*Shell commands to execute to resume Lisp job.")
;;;###autoload
(defconst ledit-go-to-liszt-string "%?liszt" "\
*Shell commands to execute to resume Lisp compiler job.")

(defun ledit-save-defun ()
  "Save the current defun in the ledit buffer."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (append-to-buffer ledit-buffer (point) end))
   (message "Current defun saved for Lisp")))

(defun ledit-save-region (beg end)
  "Save the current region in the ledit buffer"
  (interactive "r")
  (append-to-buffer ledit-buffer beg end)
  (message "Region saved for Lisp"))

(defun ledit-zap-defun-to-lisp ()
  "Carry the current defun to Lisp."
  (interactive)
  (ledit-save-defun)
  (ledit-go-to-lisp))

(defun ledit-zap-defun-to-liszt ()
  "Carry the current defun to liszt."
  (interactive)
  (ledit-save-defun)
  (ledit-go-to-liszt))

(defun ledit-zap-region-to-lisp (beg end)
  "Carry the current region to Lisp."
  (interactive "r")
  (ledit-save-region beg end)
  (ledit-go-to-lisp))

(defun ledit-go-to-lisp ()
  "Suspend Emacs and restart a waiting Lisp job."
  (interactive)
  (if ledit-save-files
      (save-some-buffers))
  (if (get-buffer ledit-buffer)
      (with-current-buffer ledit-buffer
        (goto-char (point-min))
        (write-region (point-min) (point-max) ledit-zap-file)
        (erase-buffer)))
  (suspend-emacs ledit-go-to-lisp-string)
  (load ledit-read-file t t))

(defun ledit-go-to-liszt ()
  "Suspend Emacs and restart a waiting Liszt job."
  (interactive)
  (if ledit-save-files
      (save-some-buffers))
  (if (get-buffer ledit-buffer)
      (with-current-buffer ledit-buffer
        (goto-char (point-min))
        (insert "(declare (macros t))\n")
        (write-region (point-min) (point-max) ledit-compile-file)
        (erase-buffer)))
  (suspend-emacs ledit-go-to-liszt-string)
  (load ledit-read-file t t))

(defun ledit-setup ()
  "Set up key bindings for the Lisp/Emacs interface."
  (unless ledit-mode-map
    (setq ledit-mode-map (make-sparse-keymap))
    (set-keymap-parent ledit-mode-map lisp-mode-shared-map))
  (define-key ledit-mode-map "\e\^d" 'ledit-save-defun)
  (define-key ledit-mode-map "\e\^r" 'ledit-save-region)
  (define-key ledit-mode-map "\^xz" 'ledit-go-to-lisp)
  (define-key ledit-mode-map "\e\^c" 'ledit-go-to-liszt))

(ledit-setup)

;;;###autoload
(defun ledit-mode ()
  "\\<ledit-mode-map>Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  \\[ledit-save-defun]	-- record defun at or after point
	   for later transmission to Lisp job.
  \\[ledit-save-region] -- record region for later transmission to Lisp job.
  \\[ledit-go-to-lisp] -- transfer to Lisp job and transmit saved text.
  \\[ledit-go-to-liszt] -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.

\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)"
  (interactive)
  (delay-mode-hooks (lisp-mode))
  (ledit-from-lisp-mode))

;;;###autoload
(defun ledit-from-lisp-mode ()
  (use-local-map ledit-mode-map)
  (setq mode-name "Ledit")
  (setq major-mode 'ledit-mode)
  (run-mode-hooks 'ledit-mode-hook))

(provide 'ledit)

;;; ledit.el ends here
