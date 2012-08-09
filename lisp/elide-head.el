;;; elide-head.el --- hide headers in files

;; Copyright (C) 1999, 2001-2012 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: outlines tools

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

;; Functionality for eliding boilerplate text (normally copyright
;; notices) in file headers to avoid clutter when you know what it
;; says.
;;
;; `elide-head-headers-to-hide' controls what is elided by the command
;; `elide-head'.  A buffer-local invisible overlay manages the
;; elision.

;; You might add `elide-head' to appropriate major mode hooks or to
;; `find-file-hook'.  Please do not do this in site init files.  If
;; you do, information may be hidden from users who don't know it
;; already.

;; Note that `hs-minor-mode' will do a similar job by default, but
;; it's not selective about what leading commentary it hides.

;; Inspired by jwz's hide-copyleft.el, for which we don't have an
;; assignment.

;;; Code:

(defgroup elide-head nil
  "Eliding copyright headers and the like in source files."
  :version "21.1"
  :prefix "elide-head"
  :group 'tools)

(defcustom elide-head-headers-to-hide
  '(("is free software[:;] you can redistribute it" . ; GNU boilerplate
     "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|\
If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
    ("The Regents of the University of California\\.  All rights reserved\\." .
     "SUCH DAMAGE\\.")				      ; BSD
    ("Permission is hereby granted, free of charge" . ; X11
     "authorization from the X Consortium\\."))
  "Alist of regexps defining start end end of text to elide.

The cars of elements of the list are searched for in order.  Text is
elided with an invisible overlay from the end of the line where the
first match is found to the end of the match for the corresponding
cdr."
  :group 'elide-head
  :type '(alist :key-type  (string :tag "Start regexp")
		:value-type (string :tag "End regexp")))

(defvar elide-head-overlay nil)
(make-variable-buffer-local 'elide-head-overlay)

;;;###autoload
(defun elide-head (&optional arg)
  "Hide header material in buffer according to `elide-head-headers-to-hide'.

The header is made invisible with an overlay.  With a prefix arg, show
an elided material again.

This is suitable as an entry on `find-file-hook' or appropriate mode hooks."
  (interactive "P")
  (if arg
      (elide-head-show)
    (save-excursion
      (save-restriction
	(let ((rest elide-head-headers-to-hide)
	      beg end)
	  (widen)
	  (goto-char (point-min))
	  (while rest
	    (save-excursion
	      (when (re-search-forward (caar rest) nil t)
		(setq beg (point))
		(when (re-search-forward (cdar rest) nil t)
		  (setq end (point-marker)
			rest nil))))
	    (if rest (setq rest (cdr rest))))
	  (if (not (and beg end))
	      (if (called-interactively-p 'interactive)
		  (message "No header found"))
	    (goto-char beg)
	    (end-of-line)
	    (if (overlayp elide-head-overlay)
		(move-overlay elide-head-overlay (point-marker) end)
	      (setq elide-head-overlay (make-overlay (point-marker) end)))
	    (overlay-put elide-head-overlay 'invisible t)
	    (overlay-put elide-head-overlay 'evaporate t)
	    (overlay-put elide-head-overlay 'after-string "...")))))))

(defun elide-head-show ()
  "Show a header elided current buffer by \\[elide-head]."
  (interactive)
  (if (and (overlayp elide-head-overlay)
	   (overlay-buffer elide-head-overlay))
      (delete-overlay elide-head-overlay)
    (if (called-interactively-p 'interactive)
	(message "No header hidden"))))

(provide 'elide-head)

;;; elide-head.el ends here
