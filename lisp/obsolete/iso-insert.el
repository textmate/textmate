;;; iso-insert.el --- insert functions for ISO 8859/1  -*- coding: iso-8859-1;-*-

;; Copyright (C) 1987, 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n
;; Obsolete-since: 22.1

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

;; Provides keys for inserting ISO Latin-1 characters.  They use the
;; prefix key C-x 8.  Type C-x 8 C-h for a list.

;;; Code:

(defun insert-no-break-space ()
   (interactive "*")
   (insert ?\ )
)

(defun insert-inverted-exclamation-mark ()
   (interactive "*")
   (insert ?\¡)
)

(defun insert-cent-sign ()
   (interactive "*")
   (insert ?\¢)
)

(defun insert-pound-sign ()
   (interactive "*")
   (insert ?\£)
)

(defun insert-general-currency-sign ()
   (interactive "*")
   (insert ?\¤)
)

(defun insert-yen-sign ()
   (interactive "*")
   (insert ?\¥)
)

(defun insert-broken-vertical-line ()
   (interactive "*")
   (insert ?\¦)
)

(defun insert-section-sign ()
   (interactive "*")
   (insert ?\§)
)

(defun insert-diaeresis ()
   (interactive "*")
   (insert ?\¨)
)

(defun insert-copyright-sign ()
   (interactive "*")
   (insert ?\©)
)

(defun insert-ordinal-indicator-feminine ()
   (interactive "*")
   (insert ?\ª)
)

(defun insert-angle-quotation-mark-left ()
   (interactive "*")
   (insert ?\«)
)

(defun insert-not-sign ()
   (interactive "*")
   (insert ?\¬)
)

(defun insert-soft-hyphen ()
   (interactive "*")
   (insert ?\­)
)

(defun insert-registered-sign ()
   (interactive "*")
   (insert ?\®)
)

(defun insert-macron ()
   (interactive "*")
   (insert ?\¯)
)

(defun insert-degree-sign ()
   (interactive "*")
   (insert ?\°)
)

(defun insert-plus-or-minus-sign ()
   (interactive "*")
   (insert ?\±)
)

(defun insert-superscript-two ()
   (interactive "*")
   (insert ?\²)
)

(defun insert-superscript-three ()
   (interactive "*")
   (insert ?\³)
)

(defun insert-acute-accent ()
   (interactive "*")
   (insert ?\´)
)

(defun insert-micro-sign ()
   (interactive "*")
   (insert ?\µ)
)

(defun insert-pilcrow ()
   (interactive "*")
   (insert ?\¶)
)

(defun insert-middle-dot ()
   (interactive "*")
   (insert ?\·)
)

(defun insert-cedilla ()
   (interactive "*")
   (insert ?\¸)
)

(defun insert-superscript-one ()
   (interactive "*")
   (insert ?\¹)
)

(defun insert-ordinal-indicator-masculine ()
   (interactive "*")
   (insert ?\º)
)

(defun insert-angle-quotation-mark-right ()
   (interactive "*")
   (insert ?\»)
)

(defun insert-fraction-one-quarter ()
   (interactive "*")
   (insert ?\¼)
)

(defun insert-fraction-one-half ()
   (interactive "*")
   (insert ?\½)
)

(defun insert-fraction-three-quarters ()
   (interactive "*")
   (insert ?\¾)
)

(defun insert-inverted-question-mark ()
   (interactive "*")
   (insert ?\¿)
)

(defun insert-A-grave ()
   (interactive "*")
   (insert ?\À)
)

(defun insert-A-acute ()
   (interactive "*")
   (insert ?\Á)
)

(defun insert-A-circumflex ()
   (interactive "*")
   (insert ?\Â)
)

(defun insert-A-tilde ()
   (interactive "*")
   (insert ?\Ã)
)

(defun insert-A-umlaut ()
   (interactive "*")
   (insert ?\Ä)
)

(defun insert-A-ring ()
   (interactive "*")
   (insert ?\Å)
)

(defun insert-AE ()
   (interactive "*")
   (insert ?\Æ)
)

(defun insert-C-cedilla ()
   (interactive "*")
   (insert ?\Ç)
)

(defun insert-E-grave ()
   (interactive "*")
   (insert ?\È)
)

(defun insert-E-acute ()
   (interactive "*")
   (insert ?\É)
)

(defun insert-E-circumflex ()
   (interactive "*")
   (insert ?\Ê)
)

(defun insert-E-umlaut ()
   (interactive "*")
   (insert ?\Ë)
)

(defun insert-I-grave ()
   (interactive "*")
   (insert ?\Ì)
)

(defun insert-I-acute ()
   (interactive "*")
   (insert ?\Í)
)

(defun insert-I-circumflex ()
   (interactive "*")
   (insert ?\Î)
)

(defun insert-I-umlaut ()
   (interactive "*")
   (insert ?\Ï)
)

(defun insert-D-stroke ()
   (interactive "*")
   (insert ?\Ğ)
)

(defun insert-N-tilde ()
   (interactive "*")
   (insert ?\Ñ)
)

(defun insert-O-grave ()
   (interactive "*")
   (insert ?\Ò)
)

(defun insert-O-acute ()
   (interactive "*")
   (insert ?\Ó)
)

(defun insert-O-circumflex ()
   (interactive "*")
   (insert ?\Ô)
)

(defun insert-O-tilde ()
   (interactive "*")
   (insert ?\Õ)
)

(defun insert-O-umlaut ()
   (interactive "*")
   (insert ?\Ö)
)

(defun insert-multiplication-sign ()
   (interactive "*")
   (insert ?\×)
)

(defun insert-O-slash ()
   (interactive "*")
   (insert ?\Ø)
)

(defun insert-U-grave ()
   (interactive "*")
   (insert ?\Ù)
)

(defun insert-U-acute ()
   (interactive "*")
   (insert ?\Ú)
)

(defun insert-U-circumflex ()
   (interactive "*")
   (insert ?\Û)
)

(defun insert-U-umlaut ()
   (interactive "*")
   (insert ?\Ü)
)

(defun insert-Y-acute ()
   (interactive "*")
   (insert ?\İ)
)

(defun insert-THORN ()
   (interactive "*")
   (insert ?\Ş)
)

(defun insert-ss ()
   (interactive "*")
   (insert ?\ß)
)

(defun insert-a-grave ()
   (interactive "*")
   (insert ?\à)
)

(defun insert-a-acute ()
   (interactive "*")
   (insert ?\á)
)

(defun insert-a-circumflex ()
   (interactive "*")
   (insert ?\â)
)

(defun insert-a-tilde ()
   (interactive "*")
   (insert ?\ã)
)

(defun insert-a-umlaut ()
   (interactive "*")
   (insert ?\ä)
)

(defun insert-a-ring ()
   (interactive "*")
   (insert ?\å)
)

(defun insert-ae ()
   (interactive "*")
   (insert ?\æ)
)

(defun insert-c-cedilla ()
   (interactive "*")
   (insert ?\ç)
)

(defun insert-e-grave ()
   (interactive "*")
   (insert ?\è)
)

(defun insert-e-acute ()
   (interactive "*")
   (insert ?\é)
)

(defun insert-e-circumflex ()
   (interactive "*")
   (insert ?\ê)
)

(defun insert-e-umlaut ()
   (interactive "*")
   (insert ?\ë)
)

(defun insert-i-grave ()
   (interactive "*")
   (insert ?\ì)
)

(defun insert-i-acute ()
   (interactive "*")
   (insert ?\í)
)

(defun insert-i-circumflex ()
   (interactive "*")
   (insert ?\î)
)

(defun insert-i-umlaut ()
   (interactive "*")
   (insert ?\ï)
)

(defun insert-d-stroke ()
   (interactive "*")
   (insert ?\ğ)
)

(defun insert-n-tilde ()
   (interactive "*")
   (insert ?\ñ)
)

(defun insert-o-grave ()
   (interactive "*")
   (insert ?\ò)
)

(defun insert-o-acute ()
   (interactive "*")
   (insert ?\ó)
)

(defun insert-o-circumflex ()
   (interactive "*")
   (insert ?\ô)
)

(defun insert-o-tilde ()
   (interactive "*")
   (insert ?\õ)
)

(defun insert-o-umlaut ()
   (interactive "*")
   (insert ?\ö)
)

(defun insert-division-sign ()
   (interactive "*")
   (insert ?\÷)
)

(defun insert-o-slash ()
   (interactive "*")
   (insert ?\ø)
)

(defun insert-u-grave ()
   (interactive "*")
   (insert ?\ù)
)

(defun insert-u-acute ()
   (interactive "*")
   (insert ?\ú)
)

(defun insert-u-circumflex ()
   (interactive "*")
   (insert ?\û)
)

(defun insert-u-umlaut ()
   (interactive "*")
   (insert ?\ü)
)

(defun insert-y-acute ()
   (interactive "*")
   (insert ?\ı)
)

(defun insert-thorn ()
   (interactive "*")
   (insert ?\ş)
)

(defun insert-y-umlaut ()
   (interactive "*")
   (insert ?\ÿ)
)

(defvar 8859-1-map nil "Keymap for ISO 8859/1 character insertion.")
(if 8859-1-map nil
   (setq 8859-1-map (make-keymap))
   (define-key 8859-1-map " "    'insert-no-break-space)
   (define-key 8859-1-map "!"    'insert-inverted-exclamation-mark)
   (define-key 8859-1-map "\""   (make-sparse-keymap))
   (define-key 8859-1-map "\"\"" 'insert-diaeresis)
   (define-key 8859-1-map "\"A"  'insert-A-umlaut)
   (define-key 8859-1-map "\"E"  'insert-E-umlaut)
   (define-key 8859-1-map "\"I"  'insert-I-umlaut)
   (define-key 8859-1-map "\"O"  'insert-O-umlaut)
   (define-key 8859-1-map "\"U"  'insert-U-umlaut)
   (define-key 8859-1-map "\"a"  'insert-a-umlaut)
   (define-key 8859-1-map "\"e"  'insert-e-umlaut)
   (define-key 8859-1-map "\"i"  'insert-i-umlaut)
   (define-key 8859-1-map "\"o"  'insert-o-umlaut)
   (define-key 8859-1-map "\"u"  'insert-u-umlaut)
   (define-key 8859-1-map "\"y"  'insert-y-umlaut)
   (define-key 8859-1-map "'"    (make-sparse-keymap))
   (define-key 8859-1-map "''"   'insert-acute-accent)
   (define-key 8859-1-map "'A"   'insert-A-acute)
   (define-key 8859-1-map "'E"   'insert-E-acute)
   (define-key 8859-1-map "'I"   'insert-I-acute)
   (define-key 8859-1-map "'O"   'insert-O-acute)
   (define-key 8859-1-map "'U"   'insert-U-acute)
   (define-key 8859-1-map "'Y"   'insert-Y-acute)
   (define-key 8859-1-map "'a"   'insert-a-acute)
   (define-key 8859-1-map "'e"   'insert-e-acute)
   (define-key 8859-1-map "'i"   'insert-i-acute)
   (define-key 8859-1-map "'o"   'insert-o-acute)
   (define-key 8859-1-map "'u"   'insert-u-acute)
   (define-key 8859-1-map "'y"   'insert-y-acute)
   (define-key 8859-1-map "$"    'insert-general-currency-sign)
   (define-key 8859-1-map "+"    'insert-plus-or-minus-sign)
   (define-key 8859-1-map ","    (make-sparse-keymap))
   (define-key 8859-1-map ",,"   'insert-cedilla)
   (define-key 8859-1-map ",C"   'insert-C-cedilla)
   (define-key 8859-1-map ",c"   'insert-c-cedilla)
   (define-key 8859-1-map "-"    'insert-soft-hyphen)
   (define-key 8859-1-map "."    'insert-middle-dot)
   (define-key 8859-1-map "/"    (make-sparse-keymap))
   (define-key 8859-1-map "//"   'insert-division-sign)
   (define-key 8859-1-map "/O"   'insert-O-slash)
   (define-key 8859-1-map "/o"   'insert-o-slash)
   (define-key 8859-1-map "1"    (make-sparse-keymap))
   (define-key 8859-1-map "1/"   (make-sparse-keymap))
   (define-key 8859-1-map "1/2"  'insert-fraction-one-half)
   (define-key 8859-1-map "1/4"  'insert-fraction-one-quarter)
   (define-key 8859-1-map "3"    (make-sparse-keymap))
   (define-key 8859-1-map "3/"   (make-sparse-keymap))
   (define-key 8859-1-map "3/4"  'insert-fraction-three-quarters)
   (define-key 8859-1-map "<"    'insert-angle-quotation-mark-left)
   (define-key 8859-1-map "="    'insert-macron)
   (define-key 8859-1-map ">"    'insert-angle-quotation-mark-right)
   (define-key 8859-1-map "?"    'insert-inverted-question-mark)
   (define-key 8859-1-map "A"    'insert-A-ring)
   (define-key 8859-1-map "E"    'insert-AE)
   (define-key 8859-1-map "C"    'insert-copyright-sign)
   (define-key 8859-1-map "D"    'insert-D-stroke)
   (define-key 8859-1-map "L"    'insert-pound-sign)
   (define-key 8859-1-map "P"    'insert-pilcrow)
   (define-key 8859-1-map "R"    'insert-registered-sign)
   (define-key 8859-1-map "S"    'insert-section-sign)
   (define-key 8859-1-map "T"    'insert-THORN)
   (define-key 8859-1-map "Y"    'insert-yen-sign)
   (define-key 8859-1-map "^"    (make-sparse-keymap))
   (define-key 8859-1-map "^1"   'insert-superscript-one)
   (define-key 8859-1-map "^2"   'insert-superscript-two)
   (define-key 8859-1-map "^3"   'insert-superscript-three)
   (define-key 8859-1-map "^A"   'insert-A-circumflex)
   (define-key 8859-1-map "^E"   'insert-E-circumflex)
   (define-key 8859-1-map "^I"   'insert-I-circumflex)
   (define-key 8859-1-map "^O"   'insert-O-circumflex)
   (define-key 8859-1-map "^U"   'insert-U-circumflex)
   (define-key 8859-1-map "^a"   'insert-a-circumflex)
   (define-key 8859-1-map "^e"   'insert-e-circumflex)
   (define-key 8859-1-map "^i"   'insert-i-circumflex)
   (define-key 8859-1-map "^o"   'insert-o-circumflex)
   (define-key 8859-1-map "^u"   'insert-u-circumflex)
   (define-key 8859-1-map "_"    (make-sparse-keymap))
   (define-key 8859-1-map "_a"   'insert-ordinal-indicator-feminine)
   (define-key 8859-1-map "_o"   'insert-ordinal-indicator-masculine)
   (define-key 8859-1-map "`"    (make-sparse-keymap))
   (define-key 8859-1-map "`A"   'insert-A-grave)
   (define-key 8859-1-map "`E"   'insert-E-grave)
   (define-key 8859-1-map "`I"   'insert-I-grave)
   (define-key 8859-1-map "`O"   'insert-O-grave)
   (define-key 8859-1-map "`U"   'insert-U-grave)
   (define-key 8859-1-map "`a"   'insert-a-grave)
   (define-key 8859-1-map "`e"   'insert-e-grave)
   (define-key 8859-1-map "`i"   'insert-i-grave)
   (define-key 8859-1-map "`o"   'insert-o-grave)
   (define-key 8859-1-map "`u"   'insert-u-grave)
   (define-key 8859-1-map "a"    'insert-a-ring)
   (define-key 8859-1-map "e"    'insert-ae)
   (define-key 8859-1-map "c"    'insert-cent-sign)
   (define-key 8859-1-map "d"    'insert-d-stroke)
   (define-key 8859-1-map "o"    'insert-degree-sign)
   (define-key 8859-1-map "s"    'insert-ss)
   (define-key 8859-1-map "t"    'insert-thorn)
   (define-key 8859-1-map "u"    'insert-micro-sign)
   (define-key 8859-1-map "x"    'insert-multiplication-sign)
   (define-key 8859-1-map "|"    'insert-broken-vertical-line)
   (define-key 8859-1-map "~"    (make-sparse-keymap))
   (define-key 8859-1-map "~A"   'insert-A-tilde)
   (define-key 8859-1-map "~N"   'insert-N-tilde)
   (define-key 8859-1-map "~O"   'insert-O-tilde)
   (define-key 8859-1-map "~a"   'insert-a-tilde)
   (define-key 8859-1-map "~n"   'insert-n-tilde)
   (define-key 8859-1-map "~o"   'insert-o-tilde)
   (define-key 8859-1-map "~~"   'insert-not-sign)
   (if (not (lookup-key global-map "\C-x8"))
      (define-key global-map "\C-x8" 8859-1-map))
)
(defalias '8859-1-map 8859-1-map)

(provide 'iso-insert)

;;; iso-insert.el ends here
