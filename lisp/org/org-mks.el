;;; org-mks.el --- Multi-key-selection for Org-mode

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;

;;; Code:

(require 'org)
(eval-when-compile
  (require 'cl))

(defun org-mks (table title &optional prompt specials)
  "Select a member of an alist with multiple keys.
TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Selectable members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIAL is an alist with
also (\"key\" \"description\") entries.  When one of these is selection,
only the bare key is returned."
  (setq prompt (or prompt "Select: "))
  (let (tbl orig-table dkey ddesc des-keys allowed-keys
	    current prefix rtn re pressed buffer (inhibit-quit t))
    (save-window-excursion
      (setq buffer (org-switch-to-buffer-other-window "*Org Select*"))
      (setq orig-table table)
      (catch 'exit
	(while t
	  (erase-buffer)
	  (insert title "\n\n")
	  (setq tbl table
		des-keys nil
		allowed-keys nil)
	  (setq prefix (if current (concat current " ") ""))
	  (while tbl
	    (cond
	     ((and (= 2 (length (car tbl))) (= (length (caar tbl)) 1))
	      ;; This is a description on this level
	      (setq dkey (caar tbl) ddesc (cadar tbl))
	      (pop tbl)
	      (push dkey des-keys)
	      (push dkey allowed-keys)
	      (insert prefix "[" dkey "]" "..." "  " ddesc "..." "\n")
	      ;; Skip keys which are below this prefix
	      (setq re (concat "\\`" (regexp-quote dkey)))
	      (while (and tbl (string-match re (caar tbl))) (pop tbl)))
	     ((= 2 (length (car tbl)))
	      ;; Not yet a usable description, skip it
	      )
	     (t
	      ;; usable entry on this level
	      (insert prefix "[" (caar tbl) "]" "     " (nth 1 (car tbl)) "\n")
	      (push (caar tbl) allowed-keys)
	      (pop tbl))))
	  (when specials
	    (insert "-------------------------------------------------------------------------------\n")
	    (let ((sp specials))
	      (while sp
		(insert (format "[%s]     %s\n"
				(caar sp) (nth 1 (car sp))))
		(push (caar sp) allowed-keys)
		(pop sp))))
	  (push "\C-g" allowed-keys)
	  (goto-char (point-min))
	  (if (not (pos-visible-in-window-p (point-max)))
	      (org-fit-window-to-buffer))
	  (message prompt)
	  (setq pressed (char-to-string (read-char-exclusive)))
	  (while (not (member pressed allowed-keys))
	    (message "Invalid key `%s'" pressed) (sit-for 1)
	    (message prompt)
	    (setq pressed (char-to-string (read-char-exclusive))))
	  (when (equal pressed "\C-g")
	    (kill-buffer buffer)
	    (error "Abort"))
	  (when (and (not (assoc pressed table))
		     (not (member pressed des-keys))
		     (assoc pressed specials))
	    (throw 'exit (setq rtn pressed)))
	  (unless (member pressed des-keys)
	    (throw 'exit (setq rtn (rassoc (cdr (assoc pressed table))
					   orig-table))))
	  (setq current (concat current pressed))
	  (setq table (mapcar
		       (lambda (x)
			 (if (and (> (length (car x)) 1)
				  (equal (substring (car x) 0 1) pressed))
			     (cons (substring (car x) 1) (cdr x))
			   nil))
		       table))
	  (setq table (remove nil table)))))
    (when buffer (kill-buffer buffer))
    rtn))

(provide 'org-mks)

;;; org-mks.el ends here
