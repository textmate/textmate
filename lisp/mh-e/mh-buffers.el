;;; mh-buffers.el --- MH-E buffer constants and utilities

;; Copyright (C) 1993, 1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Change Log:

;;; Code:

;; The names of ephemeral buffers have a " *mh-" prefix (so that they
;; are hidden and can be programmatically removed in mh-quit), and the
;; variable names have the form mh-temp-.*-buffer.
(defconst mh-temp-buffer " *mh-temp*")  ;scratch
(defconst mh-temp-checksum-buffer " *mh-checksum*")
(defconst mh-temp-fetch-buffer " *mh-fetch*")  ;wget/curl/fetch output
(defconst mh-temp-index-buffer " *mh-index*")

;; The names of MH-E buffers that are not ephemeral and can be used by
;; the user (and deleted by the user when no longer needed) have a
;; "*MH-E " prefix (so they can be programmatically removed in
;; mh-quit), and the variable names have the form mh-.*-buffer.
;; Temporary buffers for search results
(defconst mh-aliases-buffer "*MH-E Aliases*") ;alias lookups
(defconst mh-folders-buffer "*MH-E Folders*") ;folder list
(defconst mh-help-buffer "*MH-E Help*") ;quick help
(defconst mh-info-buffer "*MH-E Info*") ;version information buffer
(defconst mh-log-buffer "*MH-E Log*") ;output of MH commands and so on
(defconst mh-mail-delivery-buffer "*MH-E Mail Delivery*") ;mail delivery log
(defconst mh-recipients-buffer "*MH-E Recipients*") ;killed when draft sent
(defconst mh-sequences-buffer "*MH-E Sequences*") ;sequences list

(defvar mh-log-buffer-lines 100
  "Number of lines to keep in `mh-log-buffer'.")



(defun mh-truncate-log-buffer ()
  "If `mh-log-buffer' is too big then truncate it.
If the number of lines in `mh-log-buffer' exceeds
`mh-log-buffer-lines' then keep only the last
`mh-log-buffer-lines'. As a side effect the point is set to the
end of the log buffer.

The function returns the size of the final size of the log buffer."
  (with-current-buffer (get-buffer-create mh-log-buffer)
    (goto-char (point-max))
    (save-excursion
      (when (equal (forward-line (- mh-log-buffer-lines)) 0)
        (delete-region (point-min) (point))))
    (unless (or (bobp)
                (save-excursion
                  (and (equal (forward-line -1) 0) (equal (char-after) ?))))
      (insert "\n\n"))
    (buffer-size)))

(provide 'mh-buffers)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-buffers.el ends here
