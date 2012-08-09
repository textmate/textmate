;;; arabic.el --- Quail package for inputting Arabic	-*- coding: utf-8;-*-

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: James Cloos <cloos@jhcloos.com>
;; Keywords: mule, input method, Arabic

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

(require 'quail)

(quail-define-package
 "arabic" "Arabic" "ع" nil "Arabic input method.

Based on Arabic table in X Keyboard Configuration DB.
" nil t t t t nil nil nil nil nil t)

;;  ذّ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9) 0( -_ =+
;;      ضَ صً ثُ قٌ فﻹ غإ ع` ه÷ خ× ح؛ ج< د> <>
;;       شِ سٍ ي] ب[ لﻷ اأ تـ ن، م/ ك: ط"
;;        ئ~ ءْ ؤ} ر{ ﻻﻵ ىآ ة' و, ز. ظ؟
;;

(quail-define-rules
 ("`" ?ذ)
 ("~" ?ّ)

 ("Q" ?َ)
 ("W" ?ً)
 ("E" ?ُ)
 ("R" ?ٌ)
 ("T" ["لإ"])
 ("Y" ?إ)
 ("U" ?`)
 ("I" ?÷)
 ("O" ?×)
 ("P" ?؛)
 ("{" ?<)
 ("}" ?>)

 ("A" ?ِ)
 ("S" ?ٍ)
 ("D" ?\])
 ("F" ?\[)
 ("G" ["لأ"])
 ("H" ?أ)
 ("J" ?ـ)
 ("K" ?،)
 ("L" ?/)

 ("Z" ?~)
 ("X" ?ْ)
 ("C" ?})
 ("V" ?{)
 ("B" ["لآ"])
 ("N" ?آ)
 ("M" ?')
 ("<" ?,)
 (">" ?.)
 ("?" ?؟)

 ("q" ?ض)
 ("w" ?ص)
 ("e" ?ث)
 ("r" ?ق)
 ("t" ?ف)
 ("y" ?غ)
 ("u" ?ع)
 ("i" ?ه)
 ("o" ?خ)
 ("p" ?ح)
 ("[" ?ج)
 ("]" ?د)

 ("a" ?ش)
 ("s" ?س)
 ("d" ?ي)
 ("f" ?ب)
 ("g" ?ل)
 ("h" ?ا)
 ("j" ?ت)
 ("k" ?ن)
 ("l" ?م)
 (";" ?ك)
 ("'" ?ط)

 ("z" ?ئ)
 ("x" ?ء)
 ("c" ?ؤ)
 ("v" ?ر)
 ("b" ["لا"])
 ("n" ?ى)
 ("m" ?ة)
 ("," ?و)
 ("." ?ز)
 ("/" ?ظ))

;;; arabic.el ends here
