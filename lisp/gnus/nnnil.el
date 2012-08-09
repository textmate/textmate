;;; nnnil.el --- empty backend for Gnus

;; This file is in the public domain.

;; Author: Paul Jarc <prj@po.cwru.edu>

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

;; nnnil is a Gnus backend that provides no groups or articles.  It's useful
;; as a primary select method when you want all your real select methods to
;; be secondary or foreign.

;;; Code:

(eval-and-compile
  (require 'nnheader))

(defvar nnnil-status-string "")

(defun nnnil-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nnnil-open-server (server &optional definitions)
  t)

(defun nnnil-close-server (&optional server)
  t)

(defun nnnil-request-close ()
  t)

(defun nnnil-server-opened (&optional server)
  t)

(defun nnnil-status-message (&optional server)
  nnnil-status-string)

(defun nnnil-request-article (article &optional group server to-buffer)
  (setq nnnil-status-string "No such group")
  nil)

(defun nnnil-request-group (group &optional server fast info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "411 no such news group\n")))
  (setq nnnil-status-string "No such group")
  nil)

(defun nnnil-close-group (group &optional server)
  t)

(defun nnnil-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  t)

(defun nnnil-request-post (&optional server)
  (setq nnnil-status-string "Read-only server")
  nil)

(provide 'nnnil)

;;; nnnil.el ends here
