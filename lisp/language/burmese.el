;;; burmese.el --- support for Burmese -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Burma, i18n

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

;; Aung San Suu Kyi says to call her country "Burma".
;; The murderous generals say to call it "Myanmar".
;; We will call it "Burma". -- rms, Chief GNUisance.

;;; Code:

(set-language-info-alist
 "Burmese" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (sample-text . "Burmese (မ္ရန္‌မာ)	မင္‍ဂလာပာ")
	     (documentation . t)))

(defvar burmese-composable-pattern
  (let ((table
	 '(("K" . "[\u1004\u105A]\u103A\u1039") ; KINZI sequence
	   ("C" . "[\u1000-\u102A\u103F\u1041-\u1049\u104E\u105A-\u105D\u1061\u1065-\u1066\u106E\u1071\u1075\u1081\u108E\uAA60-\uAA6F\uAA71-\uAA76]") ; consonant and vowel letter
	   ("V" . "\u1039")					   ; VIRAMA
	   ("A" . "\u103A")					   ; ASAT
	   ("S" . "[\u1000-\u1019\u101C\u101E\u1020\u1021\u105A]") ; subscript
	   ("M" . "[\u103B-\u103E\105E-\1060]") ; medial
	   ("v" . "[\u102B-\u103A\u103C-\u103E\u1062-\u1064\u1067-\u106D\u1071-\u1074\u1082-\u108D\u108F\u109A\u109C\uAA70]"))) ; vowel sign, etc.
	(regexp "\\(K\\)?C\\(VS\\)?\\(VS\\)?A?M*v*"))
    (let ((case-fold-search nil))
      (dolist (elt table)
	(setq regexp (replace-regexp-in-string (car elt) (cdr elt)
					       regexp t t))))
    regexp))

(let ((elt (list (vector burmese-composable-pattern 0 'font-shape-gstring)
		 (vector "." 0 'font-shape-gstring))))
  (set-char-table-range composition-function-table '(#x1000 . #x107F) elt)
  (set-char-table-range composition-function-table '(#xAA60 . #xAA7B) elt))

