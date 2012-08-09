;;; flow-fill.el --- interpret RFC2646 "flowed" text

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

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

;; This implement decoding of RFC2646 formatted text, including the
;; quoted-depth wins rules.

;; Theory of operation: search for lines ending with SPC, save quote
;; length of line, remove SPC and concatenate line with the following
;; line if quote length of following line matches current line.

;; When no further concatenations are possible, we've found a
;; paragraph and we let `fill-region' fill the long line into several
;; lines with the quote prefix as `fill-prefix'.

;; Todo: implement basic `fill-region' (Emacs and XEmacs
;;       implementations differ..)

;;; History:

;; 2000-02-17  posted on ding mailing list
;; 2000-02-19  use `point-at-{b,e}ol' in XEmacs
;; 2000-03-11  no compile warnings for point-at-bol stuff
;; 2000-03-26  committed to gnus cvs
;; 2000-10-23  don't flow "-- " lines, make "quote-depth wins" rule
;;             work when first line is at level 0.
;; 2002-01-12  probably incomplete encoding support
;; 2003-12-08  started working on test harness.

;;; Code:

(eval-when-compile (require 'cl))

(defcustom fill-flowed-display-column 'fill-column
  "Column beyond which format=flowed lines are wrapped, when displayed.
This can be a Lisp expression or an integer."
  :version "22.1"
  :group 'mime-display
  :type '(choice (const :tag "Standard `fill-column'" fill-column)
		 (const :tag "Fit Window" (- (window-width) 5))
		 (sexp)
		 (integer)))

(defcustom fill-flowed-encode-column 66
  "Column beyond which format=flowed lines are wrapped, in outgoing messages.
This can be a Lisp expression or an integer.
RFC 2646 suggests 66 characters for readability."
  :version "22.1"
  :group 'mime-display
  :type '(choice (const :tag "Standard fill-column" fill-column)
		 (const :tag "RFC 2646 default (66)" 66)
		 (sexp)
		 (integer)))

;;;###autoload
(defun fill-flowed-encode (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    ;; No point in doing this unless hard newlines is used.
    (when use-hard-newlines
      (let ((start (point-min)) end)
	;; Go through each paragraph, filling it and adding SPC
	;; as the last character on each line.
	(while (setq end (text-property-any start (point-max) 'hard 't))
	  (save-restriction
	    (narrow-to-region start end)
	    (let ((fill-column (eval fill-flowed-encode-column)))
	      (fill-flowed-fill-buffer))
	    (goto-char (point-min))
	    (while (re-search-forward "\n" nil t)
	      (replace-match " \n" t t))
	    (goto-char (setq start (1+ (point-max)))))))
      t)))

(defun fill-flowed-fill-buffer ()
  (let ((prefix nil)
	(prev-prefix nil)
	(start (point-min)))
    (goto-char (point-min))
    (while (not (eobp))
      (setq prefix (and (looking-at "[> ]+")
			(match-string 0)))
      (if (equal prefix prev-prefix)
	  (forward-line 1)
	(save-restriction
	  (narrow-to-region start (point))
	  (let ((fill-prefix prev-prefix))
	    (fill-region (point-min) (point-max) t 'nosqueeze 'to-eop))
	  (goto-char (point-max)))
	(setq prev-prefix prefix
	      start (point))))
    (save-restriction
      (narrow-to-region start (point))
      (let ((fill-prefix prev-prefix))
	(fill-region (point-min) (point-max) t 'nosqueeze 'to-eop)))))

;;;###autoload
(defun fill-flowed (&optional buffer delete-space)
  (with-current-buffer (or (current-buffer) buffer)
    (goto-char (point-min))
    ;; Remove space stuffing.
    (while (re-search-forward "^\\( \\|>+ $\\)" nil t)
      (delete-char -1)
      (forward-line 1))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (when (save-excursion
	      (beginning-of-line)
	      (looking-at "^\\(>*\\)\\( ?\\)"))
	(let ((quote (match-string 1))
	      sig)
	  (if (string= quote "")
	      (setq quote nil))
	  (when (and quote (string= (match-string 2) ""))
	    (save-excursion
	      ;; insert SP after quote for pleasant reading of quoted lines
	      (beginning-of-line)
	      (when (> (skip-chars-forward ">") 0)
		(insert " "))))
	  ;; XXX slightly buggy handling of "-- "
	  (while (and (save-excursion
			(ignore-errors (backward-char 3))
			(setq sig (looking-at "-- "))
			(looking-at "[^-][^-] "))
		      (save-excursion
			(unless (eobp)
			  (forward-char 1)
			  (looking-at (format "^\\(%s\\)\\([^>\n\r]\\)"
					      (or quote " ?"))))))
	    (save-excursion
	      (replace-match (if (string= (match-string 2) " ")
				 "" "\\2")))
	    (backward-delete-char -1)
	    (when delete-space
	      (delete-char -1))
	    (end-of-line))
	  (unless sig
	    (condition-case nil
		(let ((fill-prefix (when quote (concat quote " ")))
		      (fill-column (eval fill-flowed-display-column))
		      filladapt-mode
		      adaptive-fill-mode)
		  (fill-region (point-at-bol)
			       (min (1+ (point-at-eol))
				    (point-max))
			       'left 'nosqueeze))
	      (error
	       (forward-line 1)
	       nil))))))))

;; Test vectors.

(defvar show-trailing-whitespace)

(defvar fill-flowed-encode-tests
  `(
    ;; The syntax of each list element is:
    ;; (INPUT . EXPECTED-OUTPUT)
    (,(concat
       "> Thou villainous ill-breeding spongy dizzy-eyed \n"
       "> reeky elf-skinned pigeon-egg! \n"
       ">> Thou artless swag-bellied milk-livered \n"
       ">> dismal-dreaming idle-headed scut!\n"
       ">>> Thou errant folly-fallen spleeny reeling-ripe \n"
       ">>> unmuzzled ratsbane!\n"
       ">>>> Henceforth, the coding style is to be strictly \n"
       ">>>> enforced, including the use of only upper case.\n"
       ">>>>> I've noticed a lack of adherence to the coding \n"
       ">>>>> styles, of late.\n"
       ">>>>>> Any complaints?")
     .
     ,(concat
       "> Thou villainous ill-breeding spongy dizzy-eyed reeky elf-skinned\n"
       "> pigeon-egg! \n"
       ">> Thou artless swag-bellied milk-livered dismal-dreaming idle-headed\n"
       ">> scut!\n"
       ">>> Thou errant folly-fallen spleeny reeling-ripe unmuzzled ratsbane!\n"
       ">>>> Henceforth, the coding style is to be strictly enforced,\n"
       ">>>> including the use of only upper case.\n"
       ">>>>> I've noticed a lack of adherence to the coding styles, of late.\n"
       ">>>>>> Any complaints?\n"
       ))
    ;; (,(concat
    ;;    "\n"
    ;;    "> foo\n"
    ;;    "> \n"
    ;;    "> \n"
    ;;    "> bar\n")
    ;;  .
    ;;  ,(concat
    ;;    "\n"
    ;;    "> foo bar\n"))
    ))

(defun fill-flowed-test ()
  (interactive "")
  (switch-to-buffer (get-buffer-create "*Format=Flowed test output*"))
  (erase-buffer)
  (setq show-trailing-whitespace t)
  (dolist (test fill-flowed-encode-tests)
    (let (start output)
      (insert "***** BEGIN TEST INPUT *****\n")
      (insert (car test))
      (insert "***** END TEST INPUT *****\n\n")
      (insert "***** BEGIN TEST OUTPUT *****\n")
      (setq start (point))
      (insert (car test))
      (save-restriction
	(narrow-to-region start (point))
	(fill-flowed))
      (setq output (buffer-substring start (point-max)))
      (insert "***** END TEST OUTPUT *****\n")
      (unless (string= output (cdr test))
	(insert "\n***** BEGIN TEST EXPECTED OUTPUT *****\n")
	(insert (cdr test))
	(insert "***** END TEST EXPECTED OUTPUT *****\n"))
      (insert "\n\n")))
  (goto-char (point-max)))

(provide 'flow-fill)

;;; flow-fill.el ends here
