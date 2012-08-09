;;; url-dired.el --- URL Dired minor mode

;; Copyright (C) 1996-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, files

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

;;; Code:

(autoload 'dired-get-filename "dired")

(defvar url-dired-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'url-dired-find-file)
    (define-key map [mouse-2] 'url-dired-find-file-mouse)
    map)
  "Keymap used when browsing directories.")

(defun url-dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (let ((filename (dired-get-filename)))
    (find-file filename)))

(defun url-dired-find-file-mouse (event)
  "In dired, visit the file or directory name you click on."
  (interactive "@e")
  (mouse-set-point event)
  (url-dired-find-file))

(define-minor-mode url-dired-minor-mode
  "Minor mode for directory browsing.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :lighter " URL" :keymap url-dired-minor-mode-map)

(defun url-find-file-dired (dir)
  "\"Edit\" directory DIR, but with additional URL-friendly bindings."
  (interactive "DURL Dired (directory): ")
  (find-file dir)
  (url-dired-minor-mode t))

(provide 'url-dired)

;;; url-dired.el ends here
