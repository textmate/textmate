;;; case-table.el --- code to extend the character set and support case tables

;; Copyright (C) 1988, 1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n
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

;; Written by:
;; TN/ETX/TX/UMG Howard Gayle        UUCP : seismo!enea!erix!howard
;; Telefonaktiebolaget L M Ericsson  Phone: +46 8 719 55 65
;; Ericsson Telecom     	     Telex: 14910 ERIC S
;; S-126 25 Stockholm                FAX  : +46 8 719 64 82
;; Sweden

;;; Code:

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((description (make-char-table 'case-table)))
    (map-char-table
     (function (lambda (key value)
		 (if (not (natnump value))
		     (if (consp key)
			 (set-char-table-range description key "case-invariant")
		       (aset description key "case-invariant"))
		   (let (from to)
		     (if (consp key)
			 (setq from (car key) to (cdr key))
		       (setq from (setq to key)))
		     (while (<= from to)
		       (aset
			description from
			(cond ((/= from (downcase from))
			       (concat "uppercase, matches "
				       (char-to-string (downcase from))))
			      ((/= from (upcase from))
			       (concat "lowercase, matches "
				       (char-to-string (upcase from))))
			      (t "case-invariant")))
		       (setq from (1+ from)))))))
     (current-case-table))
    (save-excursion
     (with-output-to-temp-buffer "*Help*"
       (set-buffer standard-output)
       (describe-vector description)
       (help-mode)))))

(defun get-upcase-table (case-table)
  "Return the upcase table of CASE-TABLE."
  (or (char-table-extra-slot case-table 0)
      ;; Setup all extra slots of CASE-TABLE by temporarily selecting
      ;; it as the standard case table.
      (let ((old (standard-case-table)))
	(unwind-protect
	    (progn
	      (set-standard-case-table case-table)
	      (char-table-extra-slot case-table 0))
	  (or (eq case-table old)
	      (set-standard-case-table old))))))

(defun copy-case-table (case-table)
  (let ((copy (copy-sequence case-table))
	(up (char-table-extra-slot case-table 0)))
    ;; Clear out the extra slots (except for upcase table) so that
    ;; they will be recomputed from the main (downcase) table.
    (if up
	(set-char-table-extra-slot copy 0 (copy-sequence up)))
    (set-char-table-extra-slot copy 1 nil)
    (set-char-table-extra-slot copy 2 nil)
    copy))

(defun set-case-syntax-delims (l r table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
This sets the entries for L and R in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to
indicate left and right delimiters."
  (aset table l l)
  (aset table r r)
  (let ((up (get-upcase-table table)))
    (aset up l l)
    (aset up r r))
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table and upcase table.
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table)))

(defun set-case-syntax-pair (uc lc table)
  "Make characters UC and LC a pair of inter-case-converting letters.
This sets the entries for characters UC and LC in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (get-upcase-table table)))
    (aset up uc uc)
    (aset up lc uc))
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table and upcase table.
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-upcase-syntax (uc lc table)
  "Make character UC an upcase of character LC.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (aset table lc lc)
  (let ((up (get-upcase-table table)))
    (aset up uc uc)
    (aset up lc uc))
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table and upcase table.
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-downcase-syntax (uc lc table)
  "Make character LC a downcase of character UC.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (get-upcase-table table)))
    (aset up uc uc))
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table and upcase table.
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-case-syntax (c syntax table)
  "Make character C case-invariant with syntax SYNTAX.
This sets the entry for character C in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table'.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset table c c)
  (let ((up (get-upcase-table table)))
    (aset up c c))
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table and upcase table.
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry c syntax (standard-syntax-table)))

(provide 'case-table)

;;; case-table.el ends here
