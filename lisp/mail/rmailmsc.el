;;; rmailmsc.el --- miscellaneous support functions for the RMAIL mail reader

;; Copyright (C) 1985, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail
;; Package: rmail

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

(require 'rmail)

;;;###autoload
(defun set-rmail-inbox-list (file-name)
  "Set the inbox list of the current RMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list.

This applies only to the current session."
  (interactive "sSet mailbox list to (comma-separated list of filenames): ")
  (unless (eq major-mode 'rmail-mode)
    (error "set-rmail-inbox-list works only for an Rmail file"))
  (let ((inbox-list
	 (with-temp-buffer
	   (insert file-name)
	   (goto-char (point-min))
	   ;; split-string does not remove leading/trailing whitespace.
	   (nreverse (mail-parse-comma-list)))))
    (when (or (not rmail-inbox-list)
	      (y-or-n-p (concat "Replace "
				(mapconcat 'identity
					   rmail-inbox-list
					   ", ")
				"? ")))
      (message "Setting the inbox list for %s for this session"
	       (file-name-nondirectory (buffer-file-name)))
      (setq rmail-inbox-list inbox-list)))
  (rmail-show-message-1 rmail-current-message))

;; Local Variables:
;; generated-autoload-file: "rmail.el"
;; End:

;;; rmailmsc.el ends here
