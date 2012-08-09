;;; epa-dired.el --- the EasyPG Assistant, dired extension -*- lexical-binding: t -*-
;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Package: epa

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

(require 'epa)
(require 'dired)

;;;###autoload
(defun epa-dired-do-decrypt ()
  "Decrypt marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-decrypt-file (expand-file-name (car file-list)))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

;;;###autoload
(defun epa-dired-do-verify ()
  "Verify marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-verify-file (expand-file-name (car file-list)))
      (setq file-list (cdr file-list)))))

;;;###autoload
(defun epa-dired-do-sign ()
  "Sign marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-sign-file
       (expand-file-name (car file-list))
       (epa-select-keys (epg-make-context) "Select keys for signing.
If no one is selected, default secret key is used.  "
			nil t)
       (y-or-n-p "Make a detached signature? "))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

;;;###autoload
(defun epa-dired-do-encrypt ()
  "Encrypt marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-encrypt-file
       (expand-file-name (car file-list))
       (epa-select-keys (epg-make-context) "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

(provide 'epa-dired)

;;; epa-dired.el ends here
