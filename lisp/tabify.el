;;; tabify.el --- tab conversion commands for Emacs

;; Copyright (C) 1985, 1994, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Package: emacs

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

;; Commands to optimize spaces to tabs or expand tabs to spaces in a region
;; (`tabify' and `untabify').  The variable tab-width does the obvious.

;;; Code:

;;;###autoload
(defun untabify (start end)
  "Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (let ((c (current-column)))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (goto-char start)
        (while (search-forward "\t" nil t)      ; faster than re-search
          (forward-char -1)
          (let ((tab-beg (point))
                (indent-tabs-mode nil)
                column)
            (skip-chars-forward "\t")
            (setq column (current-column))
            (delete-region tab-beg (point))
            (indent-to column)))))
    (move-to-column c)))

(defvar tabify-regexp " [ \t]+"
  "Regexp matching whitespace that tabify should consider.
Usually this will be \" [ \\t]+\" to match a space followed by whitespace.
\"^\\t* [ \\t]+\" is also useful, for tabifying only initial whitespace.")

;;;###autoload
(defun tabify (start end)
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Include the beginning of the line in the narrowing
      ;; since otherwise it will throw off current-column.
      (goto-char start)
      (beginning-of-line)
      (narrow-to-region (point) end)
      (goto-char start)
      (let ((indent-tabs-mode t))
        (while (re-search-forward tabify-regexp nil t)
          ;; The region between (match-beginning 0) and (match-end 0) is just
          ;; spacing which we want to adjust to use TABs where possible.
          (let ((end-col (current-column))
                (beg-col (save-excursion (goto-char (match-beginning 0))
                                         (skip-chars-forward "\t")
                                         (current-column))))
            (if (= (/ end-col tab-width) (/ beg-col tab-width))
                ;; The spacing (after some leading TABs which we wouldn't
                ;; want to touch anyway) does not straddle a TAB boundary,
                ;; so it neither contains a TAB, nor will we be able to use
                ;; a TAB here anyway: there's nothing to do.
                nil
              (delete-region (match-beginning 0) (point))
              (indent-to end-col))))))))

(provide 'tabify)

;;; tabify.el ends here
