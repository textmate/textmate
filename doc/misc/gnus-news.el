;;; gnus-news.el --- a hack to create GNUS-NEWS from texinfo source
;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Reiner Steib  <Reiner.Steib@gmx.de>
;; Keywords: tools

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

(defvar gnus-news-header-disclaimer
"GNUS NEWS -- history of user-visible changes.

Copyright (C) 1999-2012  Free Software Foundation, Inc.
See the end of the file for license conditions.

Please send Gnus bug reports to bugs@gnus.org.
For older news, see Gnus info node \"New Features\".\n\n")

(defvar gnus-news-trailer
"
* For older news, see Gnus info node \"New Features\".

----------------------------------------------------------------------

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
\(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

\nLocal variables:\nmode: outline
paragraph-separate: \"[ 	]*$\"\nend:\n")

(defvar gnus-news-makeinfo-command "makeinfo")

(defvar gnus-news-fill-column 80)

(defvar gnus-news-makeinfo-switches
  (concat " --no-headers --paragraph-indent=0"
	  " --no-validate" ;; Allow unresolved references.
	  " --fill-column=" (number-to-string
			     (+ 3 ;; will strip leading spaces later
				(or gnus-news-fill-column 80)))))

(defun batch-gnus-news ()
  "Make GNUS-NEWS in batch mode."
  (let (infile outfile)
    (setq infile (car command-line-args-left)
	  command-line-args-left (cdr command-line-args-left)
	  outfile (car command-line-args-left)
	  command-line-args-left nil)
    (if (and infile outfile)
	(message "Creating `%s' from `%s'..." outfile infile)
      (error "Not enough files given."))
    (gnus-news-translate-file infile outfile)))

(defun gnus-news-translate-file (infile outfile)
  "Translate INFILE (texinfo) to OUTFILE (GNUS-NEWS)."
  (let* ((dir (concat (or (getenv "srcdir") ".") "/"))
	 (infile (concat dir infile))
	 (buffer (find-file-noselect (concat dir outfile))))
    (with-temp-buffer
      ;; Could be done using `texinfmt' stuff as in `infohack.el'.
      (insert
       (shell-command-to-string
	(concat gnus-news-makeinfo-command " "
		gnus-news-makeinfo-switches " " infile)))
      (goto-char (point-max))
      (delete-char -1)
      (goto-char (point-min))
      (save-excursion
	(while (re-search-forward "^   \\* " nil t)
	  (replace-match "\f\n* ")))
      (save-excursion
	(while (re-search-forward "^        \\* " nil t)
	  (replace-match "** ")))
      (save-excursion
	(while (re-search-forward "^     " nil t)
	  (replace-match "")))
      ;; Avoid `*' from @ref at beginning of line:
      (save-excursion
	(while (re-search-forward "^\\*Note" nil t)
	  (replace-match " \\&")))
      (goto-char (point-min))
      (insert gnus-news-header-disclaimer)
      (goto-char (point-max))
      (insert gnus-news-trailer)
      (write-region (point-min) (point-max) outfile))))

;;; gnus-news.el ends here
