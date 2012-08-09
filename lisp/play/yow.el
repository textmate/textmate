;;; yow.el --- quote random zippyisms

;; Copyright (C) 1993-1995, 2000-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Author: Richard Mlynarik
;; Keywords: games

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

;; Important pinheadery for GNU Emacs.
;;
;; See cookie1.el for implementation.  Note --- the `n' argument of yow
;; from the 18.xx implementation is no longer; we only support *random*
;; random access now.

;;; Code:

(require 'cookie1)

(defgroup yow nil
  "Quote random zippyisms."
  :prefix "yow-"
  :group 'games)

(defcustom yow-file (concat data-directory "yow.lines")
   "File containing pertinent pinhead phrases."
  :type 'file
  :group 'yow)

(defconst yow-load-message "Am I CONSING yet?...")
(defconst yow-after-load-message "I have SEEN the CONSING!!")

;;;###autoload
(defun yow (&optional insert display)
  "Return or display a random Zippy quotation.  With prefix arg, insert it."
  (interactive "P\np")
  (let ((yow (cookie yow-file yow-load-message yow-after-load-message)))
    (cond (insert
	   (insert yow))
	  ((not display)
	   yow)
	  (t
	   (message "%s" yow)))))

(defsubst read-zippyism (prompt &optional require-match)
  "Read a Zippyism from the minibuffer with completion, prompting with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (read-cookie prompt yow-file yow-load-message yow-after-load-message
	       require-match))

;;;###autoload
(defun insert-zippyism (&optional zippyism)
  "Prompt with completion for a known Zippy quotation, and insert it at point."
  (interactive (list (read-zippyism "Pinhead wisdom: " t)))
  (insert zippyism))

;;;###autoload
(defun apropos-zippy (regexp)
  "Return a list of all Zippy quotes matching REGEXP.
If called interactively, display a list of matches."
  (interactive "sApropos Zippy (regexp): ")
  ;; Make sure yows are loaded
  (cookie yow-file yow-load-message yow-after-load-message)
  (let* ((case-fold-search t)
         (cookie-table-symbol (intern yow-file cookie-cache))
         (string-table (symbol-value cookie-table-symbol))
         (matches nil)
         (len (length string-table))
         (i 0))
    (save-match-data
      (while (< i len)
        (and (string-match regexp (aref string-table i))
             (setq matches (cons (aref string-table i) matches)))
        (setq i (1+ i))))
    (and matches
         (setq matches (sort matches 'string-lessp)))
    (and (called-interactively-p 'interactive)
         (cond ((null matches)
                (message "No matches found."))
               (t
                (let ((l matches))
                  (with-output-to-temp-buffer "*Zippy Apropos*"
                    (while l
                      (princ (car l))
                      (setq l (cdr l))
                      (and l (princ "\n\n")))
		    (help-print-return-message))))))
    matches))


;; Yowza!! Feed zippy quotes to the doctor. Watch results.
;; fun, fun, fun. Entertainment for hours...
;;
;; written by Kayvan Aghaiepour

(declare-function doctor-ret-or-read "doctor" (arg))

;;;###autoload
(defun psychoanalyze-pinhead ()
  "Zippy goes to the analyst."
  (interactive)
  (doctor)				; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (insert (yow))
    (sit-for 0)
    (doctor-ret-or-read 1)
    (doctor-ret-or-read 1)))

(provide 'yow)

;;; yow.el ends here
