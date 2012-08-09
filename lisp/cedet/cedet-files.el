;;; cedet-files.el --- Common routines dealing with file names.

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; Package: cedet

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
;;
;; Various useful routines for dealing with file names in the tools
;; which are a part of CEDET.

;;; Code:

(defun cedet-directory-name-to-file-name (referencedir &optional testmode)
  "Convert the REFERENCEDIR (a full path name) into a filename.
Convert directory separation characters into ! characters.
Optional argument TESTMODE is used by tests to avoid conversion
to the file's truename, and dodging platform tricks."
  (let ((file referencedir))
    ;; Expand to full file name
    (when (not testmode)
      (setq file (file-truename file)))
    ;; If FILE is a directory, then force it to end in /.
    (when (file-directory-p file)
      (setq file (file-name-as-directory file)))
    ;; Handle Windows Special cases
    (when (or (memq system-type '(windows-nt ms-dos)) testmode)
      ;; Replace any invalid file-name characters (for the
      ;; case of backing up remote files).
      (when (not testmode)
	(setq file (expand-file-name (convert-standard-filename file))))
      ;; Normalize DOSish file names.
      (if (eq (aref file 1) ?:)
	  (setq file (concat "/"
			     "drive_"
			     (char-to-string (downcase (aref file 0)))
			     (if (eq (aref file 2) ?/)
				 ""
			       "/")
			     (substring file 2)))))
    ;; Make the name unique by substituting directory
    ;; separators.  It may not really be worth bothering about
    ;; doubling `!'s in the original name...
    (setq file (subst-char-in-string
		?/ ?!
		(replace-regexp-in-string "!" "!!" file)))
    file))

(defun cedet-file-name-to-directory-name (referencefile &optional testmode)
  "Reverse the process of `cedet-directory-name-to-file-name'.
Convert REFERENCEFILE to a directory name replacing ! with /.
Optional TESTMODE is used in tests to avoid doing some platform
specific conversions during tests."
  (let ((file referencefile))
    ;; Replace the ! with /
    (setq file (subst-char-in-string ?! ?/ file))
    ;; Occurrences of // meant there was once a single !.
    (setq file (replace-regexp-in-string "//" "!" file))

    ;; Handle Windows special cases
    (when (or (memq system-type '(windows-nt ms-dos)) testmode)

      ;; Handle drive letters from DOSish file names.
      (when (string-match "^/drive_\\([a-z]\\)/" file)
	(let ((driveletter (match-string 1 file))
	      )
	  (setq file (concat driveletter ":"
			     (substring file (match-end 1))))))

      ;; Handle the \\file\name nomenclature on some Windows boxes.
      (when (string-match "^!" file)
	(setq file (concat "//" (substring file 1)))))
    file))

(provide 'cedet-files)

;;; cedet-files.el ends here
