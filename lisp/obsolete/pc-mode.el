;;; pc-mode.el --- emulate certain key bindings used on PCs

;; Copyright (C) 1995, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: emulations
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

;;; Code:

;;;###autoload
(defun pc-bindings-mode ()
  "Set up certain key bindings for PC compatibility.
The keys affected are:
Delete (and its variants) delete forward instead of backward.
C-Backspace kills backward a word (as C-Delete normally would).
M-Backspace does undo.
Home and End move to beginning and end of line
C-Home and C-End move to beginning and end of buffer.
C-Escape does list-buffers."

  (interactive)
  (define-key function-key-map [delete] "\C-d")
  (define-key function-key-map [M-delete] [?\M-d])
  (define-key function-key-map [C-delete] [?\M-d])
  (global-set-key [C-M-delete] 'kill-sexp)
  (global-set-key [C-backspace] 'backward-kill-word)
  (global-set-key [M-backspace] 'undo)

  (global-set-key [C-escape] 'list-buffers)

  (global-set-key [home] 'beginning-of-line)
  (global-set-key [end] 'end-of-line)
  (global-set-key [C-home] 'beginning-of-buffer)
  (global-set-key [C-end] 'end-of-buffer))

(provide 'pc-mode)

;;; pc-mode.el ends here
