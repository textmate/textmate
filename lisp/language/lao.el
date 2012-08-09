;;; lao.el --- support for Lao -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Lao

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

(define-coding-system 'lao
  "8-bit encoding for ASCII (MSB=0) and LAO (MSB=1)."
  :coding-type 'charset
  :mnemonic ?L
  :charset-list '(lao))

(set-language-info-alist
 "Lao" '((charset lao)
	 (coding-system lao)
	 (coding-priority lao)
	 (input-method . "lao")
	 (unibyte-display . lao)
	 (features lao-util)
	 (documentation . t)))

(let ((consonant "ກ-ຮໜໝ")
      (tone "່-໌")
      (vowel-upper-lower "ັິ-ົໍ")
      (semivowel-lower "ຼ")
      (fallback-rule [nil 0 compose-gstring-for-graphic]))
  ;;            target characters    regexp
  ;;            -----------------    ------
  (dolist (l `((,vowel-upper-lower . "[c].[t]?")
	       (,tone .              "[c].")
	       (,semivowel-lower .   "[c].[v][t]?")
	       (,semivowel-lower .   "[c].[t]")))
    (let* ((chars (car l))
	   (len (length chars))
	   ;; Replace `c', `t', `v' to consonant, tone, and vowel.
	   (regexp (mapconcat #'(lambda (c)
				  (cond ((= c ?c) consonant)
					((= c ?t) tone)
					((= c ?v) vowel-upper-lower)
					(t (string c))))
			      (cdr l) ""))
	   ;; Element of composition-function-table.
	   (elt (list (vector regexp 1 'lao-composition-function)
		      fallback-rule))
	   ch)
      (dotimes (i len)
	(setq ch (aref chars i))
	(if (and (> i 1) (= (aref chars (1- i)) ?-))
	    ;; End of character range.
	    (set-char-table-range composition-function-table
				  (cons (aref chars (- i 2)) ch) elt)
	  (if (or (= (1+ i) len)
		  (and (/= ch ?-) (/= (aref chars (1+ i)) ?-)))
	      ;; A character not forming a range.
	      (set-char-table-range composition-function-table ch elt)))))))

(provide 'lao)

;;; lao.el ends here
