;; leim-ext.el -- extra leim configuration	-*- coding:utf-8; -*-

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

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

;; Makefile in this directory appends the contents of this file (only
;; such non-empty lines that don't begin with ';') to the generated
;; file leim-list.el.

;;; Code:

(eval-after-load "quail/PY-b5"
  '(quail-defrule "ling2" ?○ nil t))

;; Enable inputting full-width space (U+3000).
(eval-after-load "quail/Punct"
  '(quail-defrule " " ?　 nil t))
(eval-after-load "quail/Punct-b5"
  '(quail-defrule " " ?　 nil t))

(autoload 'ucs-input-activate "quail/uni-input"
  "Activate UCS input method.
With arg, activate UCS input method if and only if arg is positive.

While this input method is active, the variable
`input-method-function' is bound to the function `ucs-input-method'.")

(register-input-method "ucs" "UTF-8" 'ucs-input-activate "U+"
		       "Unicode input as hex in the form Uxxxx.")

(autoload 'hangul-input-method-activate "quail/hangul"
  "Activate Hangul input method INPUT-METHOD.
FUNC is a function to handle input key.
HELP-TEXT is a text set in `hangul-input-method-help-text'.")

(register-input-method
 "korean-hangul"
 "UTF-8"
 'hangul-input-method-activate
 "한2"
 "Hangul 2-Bulsik Input"
 'hangul2-input-method
 "Input method: korean-hangul2 (mode line indicator:한2)\n\nHangul 2-Bulsik input method.")

(register-input-method
 "korean-hangul3f"
 "UTF-8"
 'hangul-input-method-activate
 "한3f"
 "Hangul 3-Bulsik final Input"
 'hangul3-input-method
 "Input method: korean-hangul3 (mode line indicator:한3f)\n\nHangul 3-Bulsik final input method.")

(register-input-method
 "korean-hangul390"
 "UTF-8"
 'hangul-input-method-activate
 "한390"
 "Hangul 3-Bulsik 390 Input"
 'hangul390-input-method
 "Input method: korean-hangul390 (mode line indicator:한390)\n\nHangul 3-Bulsik 390 input method.")

(register-input-method
 "korean-hangul3"
 "UTF-8"
 'hangul-input-method-activate
 "한390"
 "Hangul 3-Bulsik 390 Input"
 'hangul390-input-method
 "Input method: korean-hangul390 (mode line indicator:한390)\n\nHangul 3-Bulsik 390 input method.")

