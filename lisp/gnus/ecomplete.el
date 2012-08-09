;;; ecomplete.el --- electric completion of addresses and the like

;; Copyright (C) 2006-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-when-compile
  (when (featurep 'xemacs)
    ;; The `kbd' macro requires that the `read-kbd-macro' macro is available.
    (require 'edmacro)))

(defgroup ecomplete nil
  "Electric completion of email addresses and the like."
  :group 'mail)

(defcustom ecomplete-database-file "~/.ecompleterc"
  "*The name of the file to store the ecomplete data."
  :group 'ecomplete
  :type 'file)

(defcustom ecomplete-database-file-coding-system 'iso-2022-7bit
  "Coding system used for writing the ecomplete database file."
  :type '(symbol :tag "Coding system")
  :group 'ecomplete)

;;; Internal variables.

(defvar ecomplete-database nil)

;;;###autoload
(defun ecomplete-setup ()
  (when (file-exists-p ecomplete-database-file)
    (with-temp-buffer
      (let ((coding-system-for-read ecomplete-database-file-coding-system))
	(insert-file-contents ecomplete-database-file)
	(setq ecomplete-database (read (current-buffer)))))))

(defun ecomplete-add-item (type key text)
  (let ((elems (assq type ecomplete-database))
	(now (string-to-number
	      (format "%.0f" (if (featurep 'emacs)
				 (float-time)
			       (require 'gnus-util)
			       (gnus-float-time)))))
	entry)
    (unless elems
      (push (setq elems (list type)) ecomplete-database))
    (if (setq entry (assoc key (cdr elems)))
	(setcdr entry (list (1+ (cadr entry)) now text))
      (nconc elems (list (list key 1 now text))))))

(defun ecomplete-get-item (type key)
  (assoc key (cdr (assq type ecomplete-database))))

(defun ecomplete-save ()
  (with-temp-buffer
    (let ((coding-system-for-write ecomplete-database-file-coding-system))
      (insert "(")
      (loop for (type . elems) in ecomplete-database
	    do
	    (insert (format "(%s\n" type))
	    (dolist (entry elems)
	      (prin1 entry (current-buffer))
	      (insert "\n"))
	    (insert ")\n"))
      (insert ")")
      (write-region (point-min) (point-max)
		    ecomplete-database-file nil 'silent))))

(defun ecomplete-get-matches (type match)
  (let* ((elems (cdr (assq type ecomplete-database)))
	 (match (regexp-quote match))
	 (candidates
	  (sort
	   (loop for (key count time text) in elems
		 when (string-match match text)
		 collect (list count time text))
	   (lambda (l1 l2)
	     (> (car l1) (car l2))))))
    (when (> (length candidates) 10)
      (setcdr (nthcdr 10 candidates) nil))
    (unless (zerop (length candidates))
      (with-temp-buffer
	(dolist (candidate candidates)
	  (insert (caddr candidate) "\n"))
	(goto-char (point-min))
	(put-text-property (point) (1+ (point)) 'ecomplete t)
	(while (re-search-forward match nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'isearch))
	(buffer-string)))))

(defun ecomplete-display-matches (type word &optional choose)
  (let* ((matches (ecomplete-get-matches type word))
	 (line 0)
	 (max-lines (when matches (- (length (split-string matches "\n")) 2)))
	 (message-log-max nil)
	 command highlight)
    (if (not matches)
	(progn
	  (message "No ecomplete matches")
	  nil)
      (if (not choose)
	  (progn
	    (message "%s" matches)
	    nil)
	(setq highlight (ecomplete-highlight-match-line matches line))
	(let ((local-map (make-sparse-keymap))
	      selected)
	  (define-key local-map (kbd "RET")
	    (lambda () (setq selected (nth line (split-string matches "\n")))))
	  (define-key local-map (kbd "M-n")
	    (lambda () (setq line (min (1+ line) max-lines))))
	  (define-key local-map (kbd "M-p")
	    (lambda () (setq line (max (1- line) 0))))
	  (let ((overriding-local-map local-map))
	    (while (and (null selected)
			(setq command (read-key-sequence highlight))
			(lookup-key local-map command))
	      (apply (key-binding command) nil)
	      (setq highlight (ecomplete-highlight-match-line matches line))))
	  (if selected
	      (message selected)
	    (message "Abort"))
	  selected)))))

(defun ecomplete-highlight-match-line (matches line)
  (with-temp-buffer
    (insert matches)
    (goto-char (point-min))
    (forward-line line)
    (save-restriction
      (narrow-to-region (point) (point-at-eol))
      (while (not (eobp))
	;; Put the 'region face on any characters on this line that
	;; aren't already highlighted.
	(unless (get-text-property (point) 'face)
	  (put-text-property (point) (1+ (point)) 'face 'highlight))
	(forward-char 1)))
    (buffer-string)))

(provide 'ecomplete)

;;; ecomplete.el ends here
